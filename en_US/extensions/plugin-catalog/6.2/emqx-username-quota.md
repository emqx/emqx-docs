# Per-username Session Quota

This plugin enforces a per-username session quota.

- Session counters are maintained per username and synchronized cluster-wide.
- Authentication is rejected with `quota_exceeded` when the configured quota is reached.
- Reconnects with an existing `clientid` do not consume additional quota.
- Per-username quota overrides allow custom limits, unlimited sessions, or connection blocking.

:::: tip Note
If your deployment can use the username as the namespace by setting `client_attrs.tns` in the `client_attrs_init` configuration, you can enforce this type of session limit through namespace-based limits.
Use this plugin only when namespace assignment follows a different scheme.
::::

## Configuration

| Field | Default | Validation | Description |
|-------|---------|------------|-------------|
| `max_sessions_per_username` | `100` | Must be a positive integer (`>= 1`). Values less than `1` or non-numeric values are rejected. | Default maximum concurrent sessions per username. Individual usernames can override this value through the overrides API. |
| `snapshot_min_age_ms` | `300000` | Must be within `120000` to `900000`. Values outside the range are clamped. | Minimum age, in milliseconds, of a snapshot before a rebuild is triggered. This prevents frequent rebuilds on large clusters. |
| `snapshot_request_timeout_ms` | `5000` | String values are accepted if they can be converted to positive integers. | Timeout budget for list API snapshot request handling. |

Update plugin config through the standard plugin config API:

`PUT /api/v5/plugins/<name-vsn>/config`

## Runtime API

The plugin exposes runtime APIs through plugin API gateway.

Base path: `/api/v5/plugin_api/emqx_username_quota`

### Session Queries

- `GET /quota/usernames`: List all usernames with active sessions.
- `GET /quota/usernames/:username`: Get details for a single username.
- `GET /metrics`: Export plugin metrics in Prometheus text format.
- `POST /kick/:username`: Kick all sessions for a username.

### Snapshot Management

- `DELETE /quota/snapshot`: Force snapshot rebuild.

### Quota Overrides

- `POST /quota/overrides`: Set per-username quota overrides.
- `DELETE /quota/overrides`: Delete per-username quota overrides.
- `GET /quota/overrides`: List all quota overrides.

### `GET /quota/usernames`

This endpoint serves results from a pre-built snapshot rather than scanning the live session data on every request.

A snapshot is a point-in-time copy of per-username session counts, sorted by count for efficient cursor-based pagination. Snapshots are built asynchronously in the background and cached. A new build is triggered only when the current snapshot is older than `snapshot_min_age_ms`.

When the first request arrives and no snapshot exists yet, the server waits for the in-progress build to complete. The wait time is up to the request deadline minus 1 second. If the build finishes in time, the endpoint returns a normal `200` response. If not, it returns `503` with partial data.

Query params:

- `limit`: positive integer, capped at `100` (default `100`)
- `used_gte`: **required** when no cursor is provided. This is the minimum session count filter. Only usernames with at least this many sessions are included. Must be a positive integer `>= 1`.
- `cursor`: optional opaque cursor returned by previous list call. If missing, the first page is returned.

Parameter rules:

- `used_gte` without `cursor`: OK (first page)
- `cursor` without `used_gte`: OK (`used_gte` is embedded in the cursor)
- Both `used_gte` and `cursor`: **400** `BAD_REQUEST`. The filter is locked in the cursor.
- Neither `used_gte` nor `cursor`: **400** `BAD_REQUEST`

Behavior:

- Results are always sorted by session count then username.
- Pagination is cursor-based. Omit `cursor` for the first page.
- Each item includes `username`, realtime `used`, and `limit` (effective quota).
- If realtime `used` differs from the snapshot count, `snapshot_used` is included so callers can see both the cached and current counts.

Successful response shape:

- `data`: username quota entries
- `meta.limit`: page size (pagination limit)
- `meta.count`: number of entries in this page
- `meta.total`: total entries in snapshot
- `meta.next_cursor`: cursor for next page (when available)
- `meta.snapshot`: snapshot metadata:
  - `node`
  - `generation` (incremental snapshot id)
  - `taken_at_ms` (snapshot timestamp in milliseconds)

Error responses:

- `400 BAD_REQUEST`: missing `used_gte`, or `used_gte` provided with cursor
- `400 INVALID_CURSOR`: cursor references an unavailable node or is malformed
- `503 SERVICE_UNAVAILABLE`: snapshot is being rebuilt
  - Body includes `snapshot_build_in_progress: true`, `data`, and `meta`
  - `data`: partial first page read from the in-progress snapshot (may be empty if the build just started)
  - `meta.count`: number of partial entries, `meta.partial: true`
  - Retry the same request with bounded backoff

### `DELETE /quota/snapshot`

Force an immediate snapshot rebuild. Returns `200` with `{"status": "ok"}` after initiating the rebuild asynchronously. The snapshot will be rebuilt in the background.

### `GET /quota/usernames/:username`

Returns details for a single username. Response fields: `username`, `used`, `limit`, `clientids`.

Returns `404 NOT_FOUND` if the username has no active sessions.

### `GET /metrics`

Returns Prometheus text format metrics for the plugin.
On replicant nodes, the request is forwarded to the snapshot owner core node.

Currently exported:

- `emqx_username_count`: Total number of usernames in the active snapshot.

### `POST /kick/:username`

Kicks all sessions for a username. Returns `{"kicked": N}` where N is the number of sessions kicked.

Returns `404 NOT_FOUND` if the username has no active sessions.

### `POST /quota/overrides`

Set per-username quota overrides. Body is a JSON array:

```json
[
  {"username": "user1", "quota": 1000},
  {"username": "vip", "quota": "nolimit"},
  {"username": "blocked", "quota": 0}
]
```

Override semantics:

| `quota` value    | Meaning                                        |
|------------------|------------------------------------------------|
| positive integer | Custom session limit for this username         |
| `"nolimit"`      | Unlimited sessions (no quota enforcement)      |
| `0`              | Ban: reject all new connections                |

Overrides are persisted to disk and replicated cluster-wide. When no override exists for a username, the global `max_sessions_per_username` config is used.

### `DELETE /quota/overrides`

Delete overrides by username. Body is a JSON array of username strings:

```json
["user1", "blocked"]
```

### `GET /quota/overrides`

List all overrides. Returns `{"data": [{"username": "...", "quota": ...}, ...]}`.

## How Snapshot-based Listing Works

This section explains how the plugin builds and serves snapshots for list-style APIs such as `GET /quota/usernames` and `GET /metrics`.

### Snapshot Owner Routing

Snapshots are built on core nodes. `GET /quota/usernames` and `GET /metrics` are routed to the snapshot owner core node, selected as the first node in the sorted running core node list.

### Blue/Green Snapshots

Two snapshot buffers (blue and green) are maintained. While one serves read requests, the other is used for building the next snapshot. Once a build completes, the roles are swapped. This eliminates data gaps during rebuilds because the old snapshot remains available until the new one is ready.

### Background Snapshot Build

Snapshot rebuilds run in a background process with yield-based throttling to avoid blocking the server. The list API remains responsive while a build is in progress.

## Operational Considerations and Limitations

This section describes runtime behaviors and limitations to consider when operating the plugin in production.

### Quota Overshoot During Connection Bursts

Quota decisions are made during authentication, while session counters are finalized on session lifecycle hooks. Under high concurrent connect bursts (especially in clusters), this creates a short synchronization window where the observed concurrent sessions for one username can temporarily exceed `max_sessions_per_username`.

Practical implication:

- This plugin provides cluster-wide quota enforcement with eventual consistency under burst load.
- Under extreme connection fan-in, the plugin does not guarantee strict per-connection quota enforcement at every instant.

### Bootstrap Behavior on Plugin Startup

When the plugin is installed on a running cluster, existing client sessions were established before hooks were registered. On startup, the plugin bootstraps quota state by traversing all local channels and registering each session.

To avoid overloading the Core nodes with a storm of DB write operations (especially when replicant nodes have a large number of existing connections), the bootstrap loop is throttled:

- Sessions are registered in batches of 100.
- After each batch, the bootstrap waits for the last written record to be replicated back to the local table before continuing. It polls every 10ms.
- If replication does not complete within 10 seconds, an error is logged and bootstrap is aborted with an `error` level log.
  Sessions registered before the timeout are retained; remaining sessions will be picked up naturally through subsequent hook-based registration on reconnect.

### Handling `503` Responses from List APIs

When the server is busy or building a snapshot, the list API returns `503`.

The `503` response body includes a `data` array with a partial first page read from the in-progress snapshot table. This gives callers best-effort data immediately rather than an empty response. The `meta.partial: true` flag indicates the data is incomplete. The partial page may be empty if the build has just started.

API Client Guidance:

- Inspect `data` for any partial results available immediately.
- Retry with bounded backoff.

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## Download

Tarballs for each EMQX release:

| EMQX Version | Plugin Version | Package |
|---|---|---|
| 6.2.0 | 1.2.0 | [emqx_username_quota-1.2.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.0/emqx_username_quota-1.2.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.0/emqx_username_quota-1.2.0.sha256)) |
| 6.2.1 | 1.2.1 | [emqx_username_quota-1.2.1.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.1/emqx_username_quota-1.2.1.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.1/emqx_username_quota-1.2.1.sha256)) |
| 6.2.2 | 1.2.2 | [emqx_username_quota-1.2.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.2/emqx_username_quota-1.2.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.2/emqx_username_quota-1.2.2.sha256)) |
| 6.2.3 | 1.2.2 | [emqx_username_quota-1.2.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_username_quota-1.2.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_username_quota-1.2.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
