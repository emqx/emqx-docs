# EMQX Enterprise Version 6

## 6.0.3

*Release Date: 2026-06-17*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.0.3.

### Enhancements

#### Security Hardening

- [#17040](https://github.com/emqx/emqx/pull/17040) Restricted API key access to Dashboard user-account management endpoints.

  Previously, an API key with the `administrator` role could call the Dashboard user management endpoints `POST/DELETE /users/:username/mfa` and `POST /users/:username/change_pwd` via HTTP Basic authentication. This meant an API key could reset or disable another Dashboard user's MFA, or change another Dashboard user's password, bypassing the intended separation between human Dashboard sessions and machine API keys.

  These endpoints now return `401 API_KEY_NOT_ALLOW` when accessed via an API key, consistent with the existing policy that blocks API key access to `/users`, `/users/:username`, `/logout`, and `/api_key`. Dashboard users can still manage their own MFA and password from the Dashboard UI using bearer-token (JWT) sessions.

- [#17065](https://github.com/emqx/emqx/pull/17065) Added SSRF protection for rule-engine-reachable connector and bridge configurations.

  When `rule_engine.ssrf.enable` is set to `true`, EMQX applies an outbound SSRF policy to connector, bridge, and action configurations. The policy evaluates each target as follows: exact matches in `rule_engine.ssrf.deny_hosts` are rejected immediately; resolved target IPs are then checked against `rule_engine.ssrf.allow_cidrs` before `rule_engine.ssrf.deny_cidrs`. The default denied ranges cover loopback, link-local (including cloud instance-metadata endpoints), RFC 1918, ULA, unspecified, and multicast ranges. The check runs at config-update time and covers HTTP `url` fields as well as `server`, `servers`, and `bootstrap_hosts` fields across all connector families.

  The feature is disabled by default to preserve compatibility with deployments whose connectors legitimately point at internal services. Operators in multi-tenant or externally-exposed setups are encouraged to enable it together with a network-layer egress firewall.

- [#17173](https://github.com/emqx/emqx/pull/17173) Restricted API keys from exporting or importing Dashboard accounts and API keys via the data backup endpoints.

  `POST /data/export` called with an API key now silently omits the `dashboard_users` and `api_keys` mnesia table sets from the resulting archive. `POST /data/import` called with an API key now returns `403 FORBIDDEN` when the uploaded backup contains either of those table sets.

  Dashboard bearer-token (login) callers are unaffected and continue to be able to back up and restore the full database, including Dashboard users and API keys.

  This closes a privilege-escalation gap. The existing `/users` and `/api_key` endpoints already deny API keys access to Dashboard login credentials and API key records, but an API key holder could bypass those restrictions by going through the data backup endpoints instead.

- [#17187](https://github.com/emqx/emqx/pull/17187) Removed the EMQX release version (`rel_vsn`) from the unauthenticated `GET /status?format=json` response to avoid disclosing the broker version to unauthenticated callers. The version remains available via the authenticated node-info APIs.

- [#17201](https://github.com/emqx/emqx/pull/17201) Hardened the plugin install endpoint against path traversal in uploaded tarballs and tightened the install allowlist.

  - The install path now refuses to extract any tarball whose entries would resolve outside the plugin install directory.
  - `emqx ctl plugins allow <name-vsn>` entries now expire 5 minutes after they are issued, and can be pinned to a SHA-256 hash of the package via `emqx ctl plugins allow <name-vsn> sha256:<HEX>`. Uploads whose contents do not match the pinned hash are rejected with `403 Forbidden`. When the optional `sha256:` argument is omitted, the previous behavior of accepting any payload named `<name-vsn>.tar.gz` is preserved.
  - A successful install via the HTTP plugin install endpoint (and the Dashboard upload that wraps it) immediately revokes the allow entry cluster-wide, preventing the same grant from being reused for a different tarball.

- [#17309](https://github.com/emqx/emqx/pull/17309) Sanitized PROXY-Protocol v2 SSL Common Name and Subject fields to prevent control characters from being smuggled into client identity.

  When a listener is configured with `proxy_protocol = true`, the broker now rejects connections whose PROXY-Protocol SSL TLV bytes contain ASCII control characters (the same byte class already rejected for MQTT-ingested `clientid`, `username`, and `password`). This blocks attacker-controlled bytes from reaching outbound HTTP authentication, authorization, or rule-engine header values via `${cert_common_name}` and `${cert_subject}` templates.

  The HTTP authentication and authorization clients also now refuse to send a request when a rendered header name or value contains a CR, LF, or NUL byte.

- [#17315](https://github.com/emqx/emqx/pull/17315) Extended the byte-class check applied to MQTT clientid / username / password to other fields that feed `ClientInfo` and HTTP request templating:

  - `peersni` (TLS Server Name Indication; also accepted from the PROXY-Protocol v2 `authority` TLV) is now validated at the connection ingestion boundary. Control characters cause the connection to be rejected and a warning logged.
  - Client attribute values produced by `mqtt.client_attrs_init` Variform expressions are dropped (with a warning) when they contain control characters, so templates such as `${client_attrs.tns}` cannot carry injected bytes downstream.
  - HTTP action / bridge connector header rendering now drops any header whose rendered name or value contains NUL, CR, or LF.

- [#17330](https://github.com/emqx/emqx/pull/17330) Hardened the PROXY Protocol v2 TLV parser on TCP and SSL listeners with `proxy_protocol` enabled. Previously, a TLV whose declared length overran the buffer caused the parser to silently truncate the TLV stream, dropping any trailing fields. The parser is now strict: malformed TLV streams cause the connection to be rejected with a warning log entry instead of being accepted with a partially parsed PROXY header.

- [#17440](https://github.com/emqx/emqx/pull/17440) Restricted `GET /api/v5/data/files/<filename>` (backup file download) to the global Dashboard administrator. Backup archives can contain Dashboard accounts (including password hashes and MFA/TOTP state) and API key records, so API key callers, Dashboard viewers, and namespaced administrators are no longer permitted to download them. Listing the backup directory (`GET /api/v5/data/files`) remains available to all roles that previously had access.

- [#17491](https://github.com/emqx/emqx/pull/17491) Fixed passwords and secrets being exposed in gateway authentication APIs, error paths, and debug logs. Gateway authentication API responses now redact secrets while preserving the raw configuration structure. The following log paths no longer print raw passwords or secrets: gateway authentication failures, listener start errors, ExProto authentication logs, CoAP token-required logs, and LwM2M invalid-register logs.

- [#17501](https://github.com/emqx/emqx/pull/17501) Blocked namespaced Dashboard users from reading MQTT message content across namespace boundaries.

  - The following endpoints now return `403 FORBIDDEN` for any non-global caller, because they can expose MQTT payloads outside the caller's namespace. Previously, a namespaced user could read or delete messages produced by other namespaces.

    - `GET /clients/:clientid/mqueue_messages`
    - `GET /clients/:clientid/inflight_messages`
    - `GET|DELETE /mqtt/retainer/messages`
    - `GET|DELETE /mqtt/retainer/message/:topic`
    - `GET /mqtt/delayed/messages`
    - `GET|DELETE /mqtt/delayed/messages/:node/:msgid`
    - `DELETE /mqtt/delayed/messages/:topic`

  - Trace APIs are now namespace-scoped: `GET /trace` lists only traces created by the caller's namespace. The per-trace endpoints (`/trace/:name`, `/trace/:name/download`, `/trace/:name/log`, `/trace/:name/log_detail`, `/trace/:name/stop`) return `404` when the trace belongs to a different namespace, preventing callers from discovering that other-namespace traces exist. The bulk `DELETE /trace` is reserved for the global administrator; namespaced callers receive `403`. Namespaced administrators retain full access to their own traces, including creating, listing, downloading, streaming, stopping, and deleting them.

#### Clustering

- [#17076](https://github.com/emqx/emqx/pull/17076) Introduced a new routing table synchronization mechanism. The routing table schema version has been stepped to `v3`, with backward compatibility for `v2` provided.

  With schema v3, each node (core or replicant) takes full ownership of the routing table entries pointing towards it, giving peer nodes only read-only access to these entries. This improves partition tolerance of the EMQX cluster, as peer nodes in a partitioned cluster cannot change the routing table on behalf of other nodes. It also improves `SUBACK` latency on replicant nodes.

  **Backward compatibility:** When a node supporting v3 joins a cluster of nodes that only support v2, it keeps using v2 for compatibility. To switch the cluster to v3, perform a full cluster restart after upgrade. To prevent the automatic switch, set `broker.routing.storage_schema` to `v2`.

  **Downgrade note:** After the cluster switches to v3, rolling downgrade is not possible.

  To check the current routing schema version on a node:

  ```
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17152](https://github.com/emqx/emqx/pull/17152), [#17181](https://github.com/emqx/emqx/pull/17181) Added support for configuring Erlang inet port options (both connect and listen) for the distribution port, with a default `buffer` size of 1 MB.

  Previously, the Erlang distribution port used an extremely small default port buffer (1460 bytes, or ~9 KB on some platforms), which caused performance bottlenecks even when the distribution port buffer (`+zdbbl`) was configured to a much larger value (e.g., 32 MB). This affected cluster communication reliability and could manifest as `erpc timeout` errors, Mnesia transaction congestion, and degraded multi-core node support.

- [#17221](https://github.com/emqx/emqx/pull/17221) Improved Cluster Linking diagnostics for MQTT message forwarding.

  When message forwarding connections experience connectivity issues, the link resource status and respective alarms now include the disconnect reason, making configuration problems easier to identify.

- [#17530](https://github.com/emqx/emqx/pull/17530) Cluster linking now requires a non-community license. Under the default community license, configured links stay inactive (no message forwarding or route replication) and the REST API rejects attempts to enable a link with a clear hint to load a non-community license. Disabling and deleting links remain available so that legacy configuration can be tidied up. After upgrading the license, links can be enabled from the Dashboard or REST API without restarting the node.

#### Observability

- [#16656](https://github.com/emqx/emqx/pull/16656) Made system monitor reports such as `busy_port` and `long_schedule` more informative by including process labels for easier troubleshooting.

- [#16744](https://github.com/emqx/emqx/pull/16744) Added support for end-to-end tracing of messages published via the HTTP API.

- [#16757](https://github.com/emqx/emqx/pull/16757) Set `os_mon` to collect only system-wide memory statistics by default, reducing per-process memory scanning overhead.

- [#16911](https://github.com/emqx/emqx/pull/16911) Reduced the overhead of Prometheus metrics collection by avoiding accidental repeated queries of Mria statistics.

- [#17018](https://github.com/emqx/emqx/pull/17018) Reduced the number of calls to other nodes performed when calling the Prometheus scraping API endpoint. This makes the API call return faster and reduces the chance of it timing out when the cluster is under strain.

  Specifically, `emqx_mria_lag` metric that is of interest to replicant nodes is now refreshed periodically (every 10 seconds by default) instead of refreshed on demand for each API call.

- [#17031](https://github.com/emqx/emqx/pull/17031) Added session high-watermark history for license usage auditing.

  EMQX now records the daily peak session count and retains at least 24 months of history. Operators can query this data via `emqx ctl license history` with optional `--period daily|monthly` and `--json` flags. A new `license.high_watermark_timezone` config controls the day boundary for bucketing.

- [#17162](https://github.com/emqx/emqx/pull/17162) Exposed per-node license info via Prometheus gauges (`emqx_license_max_sessions`, `emqx_license_expiry_at`, `emqx_license_issued_at`) so cluster-wide license consistency can be alerted on without per-node CLI checks.

  Operators can now alert on license inconsistencies across cluster nodes by comparing these gauges. The implementation fetches all three values from a single `emqx_license_checker:dump/0` gen_server call, eliminating a redundant round-trip on every Prometheus scrape.

- [#17176](https://github.com/emqx/emqx/pull/17176) Added `emqx_routes_count` and `emqx_routes_max` Prometheus metrics to export the number of route table entries per node.

- [#17329](https://github.com/emqx/emqx/pull/17329) Added two node-wide gauge metrics to the `/api/v5/prometheus/stats` endpoint:

  - `emqx_vm_uptime_ms` reports the EMQX node uptime in milliseconds.
  - `emqx_vm_max_fds` reports the maximum number of file descriptors available to the node.

- [#17558](https://github.com/emqx/emqx/pull/17558) Added two new metrics and corresponding rates to the `GET /monitor_current` HTTP API: `rules_matched` and `actions_executed`. They track the number of rules matched and the action execution rate (success + failure), respectively.

  Also fixed `actions.executed` undercounting action invocations in non-batch mode (`batch_size = 1`): the counter is now incremented once per action callback invocation, independently of the buffer-worker telemetry flush window.

#### Access Control

- [#16741](https://github.com/emqx/emqx/pull/16741) Added configuration options `idp_signs_envelopes` and `idp_signs_assertions` to SAML SSO backend to control signature verification behavior.

  Previously, SAML signature verification was not working correctly because the IdP certificate fingerprint was not being extracted from metadata and passed to esaml for verification.

  Both options default to `false` for backward compatibility with existing configurations. Users who want to enable signature verification should explicitly set these to `true` when their IdP is configured to sign SAML responses.

- [#16942](https://github.com/emqx/emqx/pull/16942), [#17235](https://github.com/emqx/emqx/pull/17235) Introduced fine-grained scope-based access control for both API keys and Dashboard login users.

  API keys now support an optional `scopes` field. When set, requests are authorized against a fixed catalog of management scopes in addition to the role check. The `publisher` API key role is constrained to the `publish` scope only.

  Dashboard login users also support `scopes`, layered on top of role-based checks. Four login-only scopes (`user_management`, `mfa_management`, `sso_management`, `api_key_management`) cover Dashboard-only endpoints. `user_management`, `sso_management`, and `api_key_management` are administrator-only; `mfa_management` may be held by any role for self-exemption from forced MFA. API keys cannot use these login-only scopes.

  New catalog endpoints `GET /api_key_scopes` and `GET /user_scopes` expose the scope vocabulary to bearer-authenticated callers. `GET /users`, `POST /users`, and `PUT /users/:username` now include `scopes` in their responses; when not explicitly set, the response shows the role-default scopes.

  Behavior changes that follow from the new scope model:

  - The `dashboard.default_username` user is now a protected break-glass account. It cannot be deleted, demoted from administrator, or assigned scopes; only its `description` may be changed. The existing last-administrator check still applies to other administrators.
  - Self-service updates now respect scopes, except for the dedicated change-password and MFA self endpoints. For example, a viewer without `user_management` can still change their own password and manage their own MFA, but cannot edit other profile fields.
  - `PUT /users/:username` and `PUT /api_key/:name` validate role changes against persisted scopes when the request omits `scopes`; incompatible demotions or role changes are rejected.
  - API key bootstrap files accept an optional scopes column (`key:secret:role:scopes`). Unknown or role-incompatible scopes are dropped with a warning, so existing three-column files remain loadable.
  - The SAML SP metadata endpoint (`GET /sso/saml/metadata`) is now reachable without authentication, matching `/sso/saml/acs`.

- [#16943](https://github.com/emqx/emqx/pull/16943), [#17361](https://github.com/emqx/emqx/pull/17361) Added per-backend `force_mfa` enforcement for SSO (LDAP, OIDC, and SAML).

  When enabled, SSO users must complete TOTP MFA setup or verification before receiving a Dashboard token, regardless of IDP-side MFA settings. New API endpoints `POST /sso/mfa/setup` and `POST /sso/mfa/verify` handle the MFA flow.

  Administrators can exempt or require existing users individually via `POST` / `DELETE` on `/users/:username/mfa`, and that decision overrides the live backend policy until the administrator changes it. SSO users on a `force_mfa = true` backend who disable their own MFA must set it up again on the next login; only an administrator-initiated disable exempts a user from the live policy.

- [#17178](https://github.com/emqx/emqx/pull/17178) The `emqx ctl api_keys add` CLI command now accepts a `--scopes <scope1,scope2,...>` option, matching the scope-based permission control already supported by the REST API.

#### Gateway

- [#16736](https://github.com/emqx/emqx/pull/16736) Improved the JT/T 808 gateway with protocol updates, encoding support, and message handling fixes.

  - Added JT/T 808 protocol 2019 support.
  - Added the `jt808.frame.parse_unknown_message` option to transparently forward unknown messages.
  - Added GBK string encoding support through the new `frame.string_encoding` option. The default `utf8` mode keeps the existing pass-through behavior, while `gbk` converts GBK-encoded strings from devices to UTF-8 for MQTT and converts UTF-8 strings from MQTT to GBK for devices. This applies to fields such as license plates, driver names, text messages, area names, and client parameters. MQTT payloads always use UTF-8 regardless of this setting.
  - Added support for custom `msg_sn` values in downlink messages. When a downlink MQTT payload contains `msg_sn` in the header, the gateway uses it instead of the auto-generated channel sequence number.
  - Fixed JT/T 808 gateway parameter setting (0x8103) and query response (0x0104) message handling for CAN bus ID parameters (0x0110~0x01FF), which should use BYTE[8] data type with base64 encoding in JSON instead of string type.
  - Fixed JT/T 808 0x0702 driver identity report message parsing.

- [#17013](https://github.com/emqx/emqx/pull/17013) Added GBT32960-2025 protocol support to the GBT32960 gateway.

  The gateway now automatically detects the protocol version by frame header (`##` for 2016, `$$` for 2025) and handles version-specific parsing and serialization, including:

  - New 2025 info types: Vehicle, DriveMotor, FuelCell, Engine, Location, Alarm, PowerBatteryVoltage/Temp, FuelCellStack, SuperCapacitor, SuperCapacitorExtreme, and Digital Signature.
  - New command: Activation (0x09/0x0A).
  - Version-aware parameter sizes for parameter query/setting (0x02/0x03: BYTE in 2025 vs WORD in 2016).
  - 2025 vehicle login with BMS battery pack encoding fields.

#### Data Integration

- [#16511](https://github.com/emqx/emqx/pull/16511) Added support for the IoTDB Table Model in data integration.

- [#16962](https://github.com/emqx/emqx/pull/16962) Improved Kafka source polling behavior by ensuring fetch requests wait briefly for data instead of returning empty batches immediately when no records are available. This reduces unnecessary polling delays and helps Kafka consumers receive new records more consistently.

- [#17025](https://github.com/emqx/emqx/pull/17025) The way the InfluxDB database performs health checks and credential verification has been changed.

  It no longer performs checks by executing `SHOW DATABASES`, which could be falsely flagged as a system penetration by some auditing systems.

  See also [emqx/influxdb-client-erl#54](https://github.com/emqx/influxdb-client-erl/pull/54).

- [#17089](https://github.com/emqx/emqx/pull/17089) MQTT ingress bridges now support consuming from remote message queues exposed as `$queue/{name}/{bind-filter}` when the remote broker supports MQTT 5 Subscription Identifiers. Queue subscriptions are rejected when Subscription Identifiers are unavailable, and regular topic subscriptions automatically retry without Subscription Identifiers if the remote broker does not accept them.

- [#17104](https://github.com/emqx/emqx/pull/17104) Added date-part placeholders to blob name templates in aggregated upload actions (Azure Blob Storage, Amazon S3, GCS, Snowflake, S3 Tables). Placeholders are rendered against the aggregation start time and default to UTC. This enables Hive-partitioned object layouts (e.g. `year=2025/month=04/day=22/hour=07/...`) directly consumable by Spark, Databricks, and Synapse.

  Supported placeholders:

  - `${datetime.YYYY}`
  - `${datetime.MM}`
  - `${datetime.DD}`
  - `${datetime.hh}`
  - `${datetime.mm}`
  - `${datetime.ss}`
  - `${datetime.DOY}` (day of year)

  Each placeholder can be prefixed with an explicit timezone:

  - `utc` (default): e.g. `${datetime.utc.YYYY}`
  - `local` (EMQX node's system timezone): e.g. `${datetime.local.YYYY}`

- [#17136](https://github.com/emqx/emqx/pull/17136) Added the `ping_with_auth` option for InfluxDB connectors. When enabled, health checks include the configured credentials for InfluxDB-compatible services that require authenticated health check requests. Also fixed the InfluxDB connector/action to preserve Unicode text when writing values from `write_syntax` literals or MQTT payloads.

- [#17165](https://github.com/emqx/emqx/pull/17165) Added the `resource_opts.dispatch_strategy` option for actions.

  The new option defaults to `per_clientid`, preserving the previous buffer worker dispatch behavior. Setting it to `random` makes queries without an explicit `pick_key` use a random dispatch key, which helps spread traffic across multiple buffer workers when a small number of clients publish a large amount of messages.

- [#17170](https://github.com/emqx/emqx/pull/17170) [#17282](https://github.com/emqx/emqx/pull/17282) [#17297](https://github.com/emqx/emqx/pull/17297) Added `tcp_opts` (`nodelay`, `sndbuf`, `recbuf`, `buffer`, `keepalive`, `delay_send`, `active_n`) to the MQTT bridge connector and Cluster Link configurations, so the outbound MQTT client TCP socket can be tuned per connection. Unset fields keep the operating system / `gen_tcp` defaults. `delay_send` (off by default) coalesces small writes for better throughput at the cost of a small latency increase.

- [#17474](https://github.com/emqx/emqx/pull/17474) Reduced the overhead of IoTDB REST API connector health checks by using a bounded version query instead of listing all databases on each check.

- [#17481](https://github.com/emqx/emqx/pull/17481) Added a `retain_as_published` option to MQTT bridge ingress (source) subscriptions. When the bridge connects to the remote broker using MQTT 5.0 and `retain_as_published = true`, the original `retain` flag on forwarded messages is preserved instead of being cleared, allowing the bridge to faithfully republish retained messages from upstream. The default is `false` to keep existing behavior. The option has no effect when `proto_ver` is `v3` or `v4`.

  Also, the connector now emits a warning log when `bridge_mode = true` is configured together with `proto_ver = v5`, since the legacy bridge-mode flag has no effect under MQTT 5.0; set `retain_as_published` on individual subscriptions instead.

- [#17508](https://github.com/emqx/emqx/pull/17508) Set the PostgreSQL `application_name` startup parameter to `emqx` for PostgreSQL and TimescaleDB connector connections.

  This makes EMQX database sessions easier to identify in PostgreSQL logs and views such as `pg_stat_activity`.

#### Plugins

- [#16735](https://github.com/emqx/emqx/pull/16735) EMQX now supports plugin-defined HTTP API callbacks under `/api/v5/plugin_api/{plugin}/...`.

  This allows plugin authors to expose plugin-specific API endpoints through the Dashboard API service, with consistent authentication and HTTP error handling.

- [#16849](https://github.com/emqx/emqx/pull/16849) Added cookie-based authentication fallback for plugin API endpoints.

  Plugin UI iframes served by the Dashboard can now authenticate via the `emqx_auth` cookie when no `Authorization` header is present. This only applies to `/api/v5/plugin_api/...` paths.

#### REST API

- [#16718](https://github.com/emqx/emqx/pull/16718) Improved the REST API Swagger specification.

  Previously, summaries and descriptions of specification fields were mixed together. Now, summaries are brief, simple, and punctuation-free, while descriptions provide the details.

- [#16958](https://github.com/emqx/emqx/pull/16958) Added focused `/api-spec` endpoints and a Dashboard API spec explorer page for easier browsing of EMQX HTTP API documentation.

  The Dashboard now serves tag-scoped and drill-down OpenAPI slices, and these endpoints are disabled together with Swagger when `dashboard.swagger_support` is set to `false`. Added `emqx ctl api_keys` CLI commands to list, show, add, delete, enable, and disable API keys from the command line.

#### Deployment

- [#17079](https://github.com/emqx/emqx/pull/17079) Added `service.wsEnabled` option to the Helm chart to suppress the ws/wss Service port entries when MQTT WebSocket listeners are disabled. Defaults to `true` to preserve existing behavior.

### Bug Fixes

#### Core MQTT Functionalities

- [#16651](https://github.com/emqx/emqx/pull/16651) Fixed a rare connection process crash during shutdown caused by operating on an already closed socket, typically under high system stress. Prior to this fix, this race condition typically resulted in an `error` level log saying `{badmatch,{ok,{sock_error,closed}...`.

- [#16675](https://github.com/emqx/emqx/pull/16675) Fixed timestamp ordering issue where `disconnected_at` could be later than `connected_at` during session takeover or discard scenarios.

  Previously, `disconnected_at` was recorded too late (in `ensure_disconnected`), after the new session's `connected_at` was already set. This caused a race condition where `disconnected_at > connected_at`, making it difficult to track client presence state externally.

  The fix records `disconnected_at` immediately when takeover begins or when discard is received, ensuring it's always earlier than the new session's `connected_at`. This ensures correct timestamp ordering for external presence state tracking systems.

- [#16684](https://github.com/emqx/emqx/pull/16684) Enabled `mqtt.client_attrs_init` expressions to use the password, for example by passing it to `jwt_value`, when initializing client attributes.

  Previously, `client_attrs_init` ran before password was added to the rendering context, so expressions depending on password could not be resolved.

- [#16715](https://github.com/emqx/emqx/pull/16715) Fixed an issue where retained `$SYS` messages (for example, broker/node identity topics) were stored without expiry, which could leave stale node identifiers visible in Dashboard views after StatefulSet rotation.

  Now, newly published retained `$SYS` messages include `Message-Expiry-Interval = 3600` (1 hour).

  For already existing stale retained `$SYS` entries created before this change, you can manually clear them by publishing an empty retained message to the stale topic:

  ```
  emqx eval 'emqx:publish(emqx_message:set_flag(retain, true, emqx_message:make(emqx_sys, <<"$SYS/brokers/emqx@127.0.0.1/sysdescr">>, <<>>))).'
  ```

  Replace the topic in the command with the stale `$SYS/...` topic you want to remove.

- [#16731](https://github.com/emqx/emqx/pull/16731) Fixed a crash in `emqx ctl subscriptions list` that could happen when shared subscriptions were present.

  Before this fix, listing subscriptions could fail for some clients and return no output.

  After this fix, `emqx ctl subscriptions list` works reliably with both regular and shared subscriptions.

- [#16779](https://github.com/emqx/emqx/pull/16779) Improved handling of malformed first packets by classifying them as invalid CONNECT packets and adding better protocol hints in logs.

- [#16781](https://github.com/emqx/emqx/pull/16781) Fixed CONNECT validation when retained messages are unavailable.

  When `mqtt.retain_available` is set to `false`, CONNECT packets with Will Retain set are now correctly rejected with CONNACK reason `Retain not supported (0x9A)`.

- [#16782](https://github.com/emqx/emqx/pull/16782) Fixed MQTT v5 protocol handling for invalid PUBLISH properties.

  If a client sends a PUBLISH packet containing `Subscription-Identifier`, EMQX now treats it as a protocol error and disconnects the client.

- [#16783](https://github.com/emqx/emqx/pull/16783) Fixed MQTT v5 SUBSCRIBE validation for `Subscription-Identifier` upper bound.

  EMQX now accepts `268435455` (0x0FFFFFFF), which is the maximum valid Subscription Identifier value defined by the MQTT spec.

- [#16956](https://github.com/emqx/emqx/pull/16956) Log client connection termination at warning level instead of info when the reason is `emsgsize` (received packet exceeds `mqtt.max_packet_size`).

- [#17139](https://github.com/emqx/emqx/pull/17139) Restored `retainer.enable` as a real runtime switch for the retainer subsystem.

  This allows deployments to keep MQTT retained-message protocol support enabled while disabling retained-message storage, instead of relying on `mqtt.retain_available`, which can reject retained publishes at the protocol layer.

- [#17172](https://github.com/emqx/emqx/pull/17172) Fixed an issue where MQTT packets (such as PUBACK) sent by a client right before disconnecting could be lost when the connection process had pending outbound messages in its mailbox. Now the connection process correctly drains its mailbox before shutting down, ensuring that inbound packets are processed even after the socket is closed.

- [#17353](https://github.com/emqx/emqx/pull/17353) Fixed an issue in the `socket` TCP backend where outbound MQTT packets could be sent in the wrong order when a client connection experienced repeated send congestion. This scenario was practically very unlikely to occur.

- [#17383](https://github.com/emqx/emqx/pull/17383) After a session takeover, the channel info reflected by the Dashboard and REST API (`mqueue_len`, `inflight_cnt`) now updates immediately after the takeover replay completes, rather than waiting for the next 15-second stats refresh tick.

- [#17515](https://github.com/emqx/emqx/pull/17515) Fixed an issue where Message Queue subscriptions using QoS 0 could stop receiving messages after the queue subscriber's local inflight window became full.

- [#17569](https://github.com/emqx/emqx/pull/17569) Reduced MQTT v5 user-property parsing cost from quadratic to linear.

  Previously a CONNECT, PUBLISH or SUBSCRIBE packet carrying many user-properties caused super-linear scheduler time on the owning connection process, because each parsed property was appended to the end of the accumulated list. Parsing now scales linearly with the number of entries while preserving their wire order.

#### Rule Engine

- [#16699](https://github.com/emqx/emqx/pull/16699) Previously, under certain race conditions, long and cryptic logs like the following could be printed:

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  Now, EMQX prints more meaningful information to help debug the issue.

- [#16847](https://github.com/emqx/emqx/pull/16847) Fixed a crash when non-ASCII unicode string is used in message transformation expression.

- [#17211](https://github.com/emqx/emqx/pull/17211) Added the `connected_at` field to the `$events/client/connack` Rule Event, which was stated in the documentation but missing from the actual data.

#### Data Integration

- [#16622](https://github.com/emqx/emqx/pull/16622) Fixed an issue where, if an Action used async query mode and its Connector became disconnected after multiple health checks, its Fallback Actions could be triggered twice.

- [#16659](https://github.com/emqx/emqx/pull/16659) When using an older MQTT Connector configuration with static clientids (from 5.10.0 and earlier) on later EMQX versions, the username and password at the root of the configuration were ignored. This could cause issues when upgrading while keeping the same configuration, as the MQTT clients would stop using the credentials.

  Now, if there are username and/or password fields in the root Connector, those credentials are merged with any specific ones specified per clientid, the latter taking precedence.

- [#16685](https://github.com/emqx/emqx/pull/16685) Fixed an issue where the Sparkplug B metrics alias mapping feature could fail after an EMQX node was upgraded from 5.10.x to 6.0.y.

  Due to differences in how Protobuf code was generated before 6.0.0, if an EMQX node had started on an older version (< 6.0.0) with the same OTP version as the newer version, the cached Protobuf code was kept but no longer matched the newer code's expectations. This caused Sparkplug B alias mapping to fail after upgrading from 5.10.x to 6.0.y.

- [#16723](https://github.com/emqx/emqx/pull/16723) Fixed an issue with RabbitMQ Connector/Action/Source where, if some connection or channel processes died unexpectedly, the Connector/Action/Source would be reported as disconnected and would not recover without being restarted.

- [#16742](https://github.com/emqx/emqx/pull/16742) Fixed GreptimeDB TLS connection failures.

- [#16796](https://github.com/emqx/emqx/pull/16796) Fixed handling of multiline SQL statements in connector actions.

- [#16863](https://github.com/emqx/emqx/pull/16863) Added a warning log when an async reply is received for an already-expired request.

- [#16890](https://github.com/emqx/emqx/pull/16890) Fixed an ExHook issue where successful reconnect reloads could duplicate the same server name in the running list and trigger repeated callback dispatches.

- [#16936](https://github.com/emqx/emqx/pull/16936) Fixed an issue where the health check of an Azure Blob Storage Action in aggregate mode could timeout if the container contained too many blobs.

- [#16955](https://github.com/emqx/emqx/pull/16955) Eliminated Kafka producer action false health check warning logs.

  Previously if Kafka producer is idling for too long, Kafka may close the connection (typically default is 10 minutes), if Kafka producer action health-checks happen to be performed around the same moment, there could be a false warning message with message "not_all_kafka_partitions_connected".

- [#16972](https://github.com/emqx/emqx/pull/16972) HTTP and GCP PubSub Actions were patched to treat transient connection errors with reason `closing` as recoverable errors, reducing log noise.

- [#17084](https://github.com/emqx/emqx/pull/17084) Fixed an issue with MQTT Sources in which, if its Connector used `clean_start = false` and reconnected to a broker with a session containing messages, those messages would not trigger rule actions.

- [#17111](https://github.com/emqx/emqx/pull/17111) Fixed query execution for PostgreSQL connectors in disable prepared statements mode. Previously, concurrent queries could interleave and produce errors.

- [#17113](https://github.com/emqx/emqx/pull/17113) Fixed RocketMQ connector isolation: a misconfigured or unreachable RocketMQ connector no longer destabilizes other RocketMQ connectors on the same node. Previously, one connector with an unreachable broker could stall the shared client supervisor for up to 60 seconds, causing sibling connectors to flap with `resource_health_check_timed_out` and for Dashboard operations on them to hang.

  The default TCP/TLS connect timeout is also lowered from 60 seconds to 10 seconds so a misconfigured server surfaces as failed quickly instead of appearing stuck.

- [#17180](https://github.com/emqx/emqx/pull/17180) Fixed an issue where, under heavy load, a timed out call to a MongoDB process would be interpreted as an unrecoverable error and wouldn't be retried. Now, the message will be retried on such events.

- [#17216](https://github.com/emqx/emqx/pull/17216) Fixed Timescale/PostgreSQL actions to report a structured bad parameter error instead of crashing the database connection process when a quoted JSON numeric string is mapped to a `FLOAT` column.

- [#17250](https://github.com/emqx/emqx/pull/17250) Fixed Redis Sentinel connectors to support separate authentication settings for Redis data nodes and Sentinel nodes.

- [#17293](https://github.com/emqx/emqx/pull/17293) Fixed an issue where, when writing a Parquet file with an object containing a required key but with an `undefined`/`null` value, a corrupt file would be written instead of raising an error.

- [#17303](https://github.com/emqx/emqx/pull/17303) Upgraded Kafka client libraries: `brod` from 4.5.2 to 4.5.4 and `wolff` from 4.1.9 to 4.1.10.

  Notable fixes picked up from upstream:

  - `brod`: fix a race condition during Kafka connection re-authentication (via `kafka_protocol` 4.3.4).
  - `wolff`: under high-memory load control (`drop_if_highmem`), keep a minimum buffer reserve so the producer is not starved of in-flight data; only bytes exceeding the reserve are dropped.

- [#17347](https://github.com/emqx/emqx/pull/17347) Upgraded the RocketMQ client dependency to `v0.7.2` to fix memory growth in async producer requests.

- [#17439](https://github.com/emqx/emqx/pull/17439) Fixed an issue where the health check of an Azure Blob Storage Connector could timeout, or generate large bandwidth costs, if the storage account contained too many containers. Companion fix to #16935.

- [#17450](https://github.com/emqx/emqx/pull/17450) Fixed an issue where the `/prometheus/data_integration` Prometheus endpoint could respond with a 500 status when using `mode=node`. This issue would only arise when the configuration for Actions and Connectors was manually edited and inconsistent, having an Action whose Connector does not exist.

- [#17568](https://github.com/emqx/emqx/pull/17568) Upgraded the Kafka client library `brod` to 4.5.5.

  Consumer group: respect the broker-assigned member ID when the join response carries the `member_id_required` error code (returned by older Kafka brokers, e.g. 2.2.0, that do not support static member instance IDs). Previously the member ID was discarded on error, preventing the retry from succeeding.

- [#17579](https://github.com/emqx/emqx/pull/17579) Fixed Redis Sentinel connectors to use isolated Sentinel managers per resource and clean them up when resources stop, avoiding shared Sentinel state across connectors.

#### Clustering

- [#16393](https://github.com/emqx/emqx/pull/16393) Improved the stability of the Cluster Link route replication under unstable network conditions.

- [#16739](https://github.com/emqx/emqx/pull/16739) Improved cluster recovery time after a simultaneous restart of all nodes.

  The built-in Mria database management system no longer waits for the full synchronization of an internal table used to generate transaction synchronization events.

- [#17132](https://github.com/emqx/emqx/pull/17132) Fixed an issue where adding or removing topic metrics could fail on a replicant node when its raw config or runtime state had drifted, raising a `cluster_rpc_apply_failed` alarm and stalling cluster RPC replication. Duplicate-add and missing-remove are now rejected on the initiator only, while replicants apply the change idempotently.

- [#17182](https://github.com/emqx/emqx/pull/17182) Bumped emqx-OTP to 27.3.4.2-8 for mria.

  Without this change, during EMQX startup, Mria app boot may get stuck if it's not connected to the cluster.

- [#17214](https://github.com/emqx/emqx/pull/17214) Removed cryptic error-level logging of disconnect events from Cluster Link message forwarding MQTT clients, in favor of more user-friendly messages with enough context for troubleshooting. Events similar to this one should no longer appear in the error logs:

  ```
  2026-05-06T03:00:48.738654+00:00 [error] [PoolWorker] unexpected info: {disconnected,141,#{}}
  ```

- [#17218](https://github.com/emqx/emqx/pull/17218) Avoid `bin/emqx` and `bin/emqx_ctl` invocations from triggering `nodeup`/`nodedown` events on the running broker, which previously surfaced as misleading `cm_registry_node_down` warnings in the broker log. The temporary helper nodes started by these scripts now register as hidden Erlang nodes, as intended.

- [#17269](https://github.com/emqx/emqx/pull/17269) Improved cluster recovery after a network partition.

  - Previously, part of the clients connected to the replicant nodes could be lost from the global registry. This could lead to inconsistent behavior during takeover and incorrect information displayed in the Dashboard.

    This fix adds a background process that re-registers the existing clients when network partition is healed. It also adds a new alarm: "Broker is recovering after a network partition", which is raised while the global registry is being rebuilt.

  - Introduced a new cluster auto-heal algorithm that can automatically recover overlapping network partitions.

- [#17343](https://github.com/emqx/emqx/pull/17343) Fixed a clustered-config replication bug where importing a data backup (or loading a HOCON config via `emqx ctl conf load` / `PUT /api/v5/configs`) that contained a `file`-type authorization source could leave peer nodes lagging with a `cluster_rpc_apply_failed` / `failed_to_read_acl_file` error.

  The importer used to write the ACL file locally and replace inline `rules` with a `path`, then ship the path-form config across the cluster. Peer nodes have no such file on disk and so could not apply the change. The config sent to the cluster now keeps `rules` inline, so each peer writes its own copy of the ACL file from the replicated content.

- [#17348](https://github.com/emqx/emqx/pull/17348) Fixed noisy and misleading `emqx ctl conf cluster_sync status` diagnostics when clustered nodes have the same effective checked configuration but different raw configuration representations.

  The command now suppresses raw-only representation differences that do not correspond to checked configuration changes, while still warning when checked configuration is inconsistent. It also avoids crashing when a raw configuration key exists on one node but is missing from another node.

  It also ignores timestamp-only metadata differences in `created_at` and `last_modified_at` for actions, sources, bridges, and rule metadata. Data import or boot-time configuration loading can refresh these generated timestamps on only some nodes even when the effective runtime configuration is otherwise identical.

- [#17349](https://github.com/emqx/emqx/pull/17349) Improved responsiveness of a Cluster Link in situations when route replication was stuck connecting to an unresponsive target cluster. Now, deleting such Cluster Link should finish slightly sooner.

- [#17382](https://github.com/emqx/emqx/pull/17382) Fixed corruption of global channel registry that may occur when cluster experiences a network partition.

- [#17424](https://github.com/emqx/emqx/pull/17424) Fixed a global session registry leak that could leave duplicate or stale entries for the same client ID after a network partition followed by Mnesia autoheal.

  Discard and takeover-kick RPC handlers now also remove the registry row when the target process is no longer alive, and the registration throttle on the connect path now recognizes tombstone rows (no local channel state) and reaps them instead of blocking new connections for the same client ID indefinitely.

- [#17432](https://github.com/emqx/emqx/pull/17432) Fixed an issue where concurrent Cluster Link API requests could return generic error responses, instead of returning either success or not found.

- [#17469](https://github.com/emqx/emqx/pull/17469) Fixed the issue where warnings similar to those below are emitted when enabling or disabling an active Cluster Link.

  ```
  [warning] tag: RESOURCE, msg: handle_resource_metrics_failed, reason: {badkey, matched}, event: matched, ...
  ```

#### Access Control

- [#16692](https://github.com/emqx/emqx/pull/16692) Fixed a CRL cache regression where `emqx_crl_cache:evict/1` did not fully clear internal URL state. After eviction, the same CRL URL now re-registers correctly on next use, restores its refresh timer, and avoids repeated HTTP fetches per connection.

- [#16780](https://github.com/emqx/emqx/pull/16780) Fixed an issue in authorization source validation where requests missing the `type` field could trigger an internal error.

  Now EMQX returns a clear `BAD_REQUEST` validation error for this case.

- [#16805](https://github.com/emqx/emqx/pull/16805) Added support for authz hook results to opt out of authorization cache storage for dynamic ACL decisions.

- [#16865](https://github.com/emqx/emqx/pull/16865) Added `cert_common_name` and `cert_subject` aliases for `mqtt.client_attrs_init` expressions, alongside the existing `cn` and `dn` variables.

- [#16868](https://github.com/emqx/emqx/pull/16868) Improved REST API authentication error messages to guide programmatic clients toward using API keys (Basic auth) instead of repeatedly logging in for bearer tokens. Error responses now mention the `api_key.bootstrap_file` configuration option and the `POST /api_key` endpoint for creating persistent API keys.

- [#16939](https://github.com/emqx/emqx/pull/16939) Fixed the built-in database authenticator so it no longer logs a warning when the default bootstrap file path is configured but the file does not exist.

- [#17045](https://github.com/emqx/emqx/pull/17045) Fixed password-based authentication backends to let the auth chain continue when the CONNECT packet has no password, instead of rejecting the connection immediately.

  Previously, if a client connected without a password, the first password-based authenticator (built-in database, MySQL, PostgreSQL, MongoDB, Redis, or LDAP) in the chain would return an error, blocking any subsequent authenticators from being tried.

- [#17100](https://github.com/emqx/emqx/pull/17100) Fixed OIDC SSO login failing with `provider_not_ready` when the identity provider returns a JWKS response whose `Content-Type` uses the `+json` structured syntax suffix (e.g. `application/jwk-set+json; charset=utf-8`). Such responses are now accepted as valid JWKS content.

- [#17122](https://github.com/emqx/emqx/pull/17122) Fixed Dashboard RBAC checks for SSO users with URL-encoded usernames such as email addresses, so viewer self-service MFA disable requests work correctly when `force_mfa` is disabled.

- [#17140](https://github.com/emqx/emqx/pull/17140) Fixed a silent failure when EMQX fetched a Certificate Revocation List (CRL) over HTTP from a server that returns a DER-encoded body (`Content-Type: application/pkix-crl`, the format mandated by RFC 5280 §5).

  Previously, EMQX only decoded PEM-encoded CRL bodies; a DER body was silently treated as zero CRLs and cached as an empty list, causing every TLS handshake on `enable_crl_check = true` listeners to fail with `bad_crls, no_relevant_crls` and no log line indicating what went wrong.

  EMQX now decodes both PEM and DER CRL bodies. When a fetched body is neither, a warning is logged with the URL so the misconfiguration is visible.

- [#17171](https://github.com/emqx/emqx/pull/17171) Fixed an RBAC issue that prevented namespaced Dashboard administrators from enabling or disabling MFA for their own account.

  Namespaced administrators remain restricted from managing MFA settings for other Dashboard users.

- [#17177](https://github.com/emqx/emqx/pull/17177) Dashboard-created REST API keys are now generated randomly instead of being derived from the API key name.

- [#17223](https://github.com/emqx/emqx/pull/17223) Fixed missing client certificate when a TCP-passthrough proxy (e.g. GCP TCP Proxy NLB, AWS NLB) is placed in front of an SSL listener with `proxy_protocol = true`. The TLS handshake at the listener was completing successfully and the client certificate was present, but it was not exposed to authentication or rule events. Functions, ACL rules, and authentication backends that depend on the client certificate (CN, subject, full PEM) now work correctly in this deployment shape.

- [#17428](https://github.com/emqx/emqx/pull/17428) Fixed a Dashboard OIDC SSO crash that prevented EMQX from completing the OpenID provider discovery when the provider's `.well-known/openid-configuration` response included a `Cache-Control` header such as `max-age=0` (observed with Kanidm). The crash caused the OIDC supervisor to exhaust its restart budget after a single failure, leaving SSO unable to recover without a config re-save. The cache-control parser is now tolerant of these values, the worker no longer hard-crashes on a bad expiry, and the OIDC supervisor allows several restarts within a minute so transient failures retry cleanly.

#### Gateway

- [#16603](https://github.com/emqx/emqx/pull/16603) Fixed the CoAP Gateway when running in DTLS connection mode.
- [#16670](https://github.com/emqx/emqx/pull/16670) NATS gateway now enforces the max publish payload, honors the `echo` option (no local delivery), and improves publish/subscribe subject handling and related error messages.
- [#17141](https://github.com/emqx/emqx/pull/17141) Fixed CoAP connection-mode token takeover so reconnecting UDP/DTLS clients can resume with a valid token while invalid token/clientid combinations are rejected. Also ensured required connection info fields are present before running CoAP takeover connected hooks.

- [#17258](https://github.com/emqx/emqx/pull/17258) Fixed an issue in the MQTT-SN gateway where a connected client sending a second CONNECT packet on the same session would crash its connection process. The gateway now responds with a DISCONNECT and closes the session gracefully.

- [#17287](https://github.com/emqx/emqx/pull/17287) Fixed MQTT-SN clients crash caused by packets received in unexpected connection or Will states, including `DISCONNECT` during connection setup, `REGISTER` before the Will handshake completes, and `WILLMSGUPD` before a Will topic exists.

#### Multi-tenancy

- [#17118](https://github.com/emqx/emqx/pull/17118) Improved pagination on multi-tenancy list endpoints (`/mt/ns_list`, `/mt/ns_list_details`, `/mt/managed_ns_list`, `/mt/managed_ns_list_details`, `/mt/ns/{ns}/client_list`):

  - Added an RFC 8288 `Link: <?...>; rel="next"` response header. When more pages are available the header carries the query-only URI-reference of the next page; when absent, the current response is the last page. This removes the prior ambiguity where a full page (`len(results) == limit`) could not be distinguished from the exact-boundary "no more data" case without an extra request.
  - Added inclusive keyset cursor query parameters (`first_ns`, `first_clientid`) alongside the existing exclusive cursors (`last_ns`, `last_clientid`). The inclusive form supports exact-match lookup (e.g. `?first_ns=foo&limit=1`) and is preserved across paginated Link headers when the caller opts in. The two forms are mutually exclusive on a single request; supplying both returns HTTP 400.

- [#17406](https://github.com/emqx/emqx/pull/17406) Now, events captured by a trace initiated by a namespaced admin are limited to the namespace of such admin, for traces of types topic, IP address, and clientid. Traces of type rule ID already had such behavior.

#### Plugins

- [#16784](https://github.com/emqx/emqx/pull/16784) Reduced noisy plugin startup warnings in single-node deployments.

  EMQX no longer tries to fetch plugin config from the local node during cluster config sync, avoiding repeated `config_not_found_on_node` warnings at startup.

- [#16823](https://github.com/emqx/emqx/pull/16823) Fixed a Dashboard plugin management issue for preinstalled plugins.

  When a plugin package is unpacked into `plugins/` before node startup, starting it from the Dashboard no longer causes `Plugin Config Not Found` on the plugin config page.

- [#16842](https://github.com/emqx/emqx/pull/16842) Reduced noisy plugin config warning logs when no peer node has the plugin config yet.

  Previously, when a node tried to fetch plugin config from peer nodes during startup, it would log a warning even when all peers simply didn't have the config (e.g., first node to load the plugin). Now this benign case is logged at debug level, and only genuine errors (RPC failures, timeouts) remain as warnings.

- [#16843](https://github.com/emqx/emqx/pull/16843) Fixed an issue where HTTP headers and query string parameters were not passed through to plugin API handlers, causing plugins to receive empty headers and missing query parameters.

- [#16904](https://github.com/emqx/emqx/pull/16904) Prevent enabling or starting multiple versions of the same plugin at once. When a newer version is enabled, older configured versions of that plugin are automatically disabled, and management API actions now return a clear error instead of reporting success while another version is still active.

- [#17247](https://github.com/emqx/emqx/pull/17247) When a plugin's REST API callback crashes or runs over its timeout budget, the broker now logs the failing API method and path together with the configured timeout, so the offending call is identifiable in mixed-traffic logs. A timeout is logged as a warning (not an error) and includes a hint pointing at `plugins.api_endpoint.timeout`, the config key to raise when a plugin callback legitimately needs more time.

- [#17473](https://github.com/emqx/emqx/pull/17473) Lowered the log level of `unabled_to_stop_plugin_apps` from warning to info when the plugin's Erlang applications cannot be stopped because other running applications still depend on them. This is an expected, non-actionable condition during plugin unload and no longer raises a warning.

- [#17575](https://github.com/emqx/emqx/pull/17575) Fixed a race condition in the emqx_username_quota plugin that could cause the per-username session counter to become inconsistent with the actual number of tracked client records. The counter could be decremented past zero and then be deleted while a concurrent session registration incremented it, losing the increment permanently.

#### REST API

- [#17002](https://github.com/emqx/emqx/pull/17002) Updated `minirest` library to version 1.4.12. This version fixes a bug that caused EMQX API to produce malformed API responses with `204 No Content` status line, emitting invalid `content-length` header.

- [#17054](https://github.com/emqx/emqx/pull/17054) Fixed `GET /api/v5/configs?key=...` returning incomplete data when `Accept: application/json` was set.

  Previously, the JSON response ignored the `key` query parameter and always returned a fixed subset of root configurations, which excluded keys like `multi_tenancy`. The endpoint now honors the `key` parameter in JSON responses consistently with the hocon (text/plain) response.

- [#17319](https://github.com/emqx/emqx/pull/17319) `GET /api/v5/schemas/{hotconf,actions,connectors}` now returns the response with `Content-Type: application/json`. Previously the response body was valid JSON but the header was `text/plain; charset=utf-8`, which broke clients that dispatch on the response content type.

#### Observability

- [#16661](https://github.com/emqx/emqx/pull/16661) Improved `topic_metrics` and `cluster_rpc` logging when an invalid topic is requested.
- [#16674](https://github.com/emqx/emqx/pull/16674) Ensured that the Erlang PID is printed as a log data field.
- [#16876](https://github.com/emqx/emqx/pull/16876) Changed log message `msg_publish_not_allowed` to `msg_not_routed_to_subscribers`.

- [#16879](https://github.com/emqx/emqx/pull/16879) Added `log.audit.cache_size` as the primary config key for the audit log DB cache size, while keeping `log.audit.max_filter_size` for backward compatibility.

- [#17513](https://github.com/emqx/emqx/pull/17513) Fixed Prometheus matched authorization allow/deny metrics so they reflect real matched authorization decisions.

#### Deployment

- [#16545](https://github.com/emqx/emqx/pull/16545) Fixed `node.cookie` handling of `#` character. Previously, if the cookie contained `#`, only the prefix before `#` would take effect. For example, if `abc#d` was configured, only `abc` was used as the cookie.

  Added validation to reject problematic characters: backslash, single quote, double quote, and space.

- [#16620](https://github.com/emqx/emqx/pull/16620) Fixed a CRC32C dynamic library loading issue on aarch64.

- [#16657](https://github.com/emqx/emqx/pull/16657) Fixed an issue where, when importing configuration from an older node version into a newer one, values would not be upgraded according to newer code, leading to strange behavior.

  One such example is importing an MQTT Connector with static clientids from 5.10.0 into 6.0.0. In 5.10.0, usernames and passwords could not be associated with particular static clientids, and this was represented internally in a certain way. Later versions added the capability to create those associations with a different internal representation. This subtle internal representation conversion was missing when importing such configurations in previous EMQX versions.

- [#17024](https://github.com/emqx/emqx/pull/17024) Dashboard HTTP listener now automatically uses IPv6 when the bind address is an IPv6 address, removing the need to explicitly set `inet6 = true`.

- [#17227](https://github.com/emqx/emqx/pull/17227) Cluster config file save errors now name the file and the underlying reason.

  When `cluster.hocon` (or its directory) is read-only, immutable, or otherwise unwritable (e.g. mounted read-only into a container), changing config via the Dashboard or REST API previously returned an opaque HTTP 400 with body `{config_update_crashed,{badmatch,{error,ebusy}}}` and only logged a badmatch crash that did not name the file.

  The error now:

  - Logs `failed_to_save_conf_file` with the actual file path and reason (`eacces`, `eperm`, `ebusy`, ...) plus a hint listing common operator-side causes.
  - Returns a structured HTTP 400 body that names both the file and the reason, so the cause is visible in the Dashboard without digging through node logs.

  Previously, when only the temporary file write failed (e.g. read-only directory), the API silently returned HTTP 200 even though the change was not persisted to disk. The API now correctly reports failure in this case as well.

- [#17246](https://github.com/emqx/emqx/pull/17246) Upgraded `jose` library from 1.11.10 to 1.11.12, picking up EC and EdDSA key fixes for newer OTP releases.

- [#17252](https://github.com/emqx/emqx/pull/17252) Published `.sha256` checksum sidecars alongside plugin packages on the official download site, allowing users to verify the integrity of downloaded plugin archives.

- [#17254](https://github.com/emqx/emqx/pull/17254) Improved memory-usage reporting inside containers. The broker now picks the most constraining memory reading among cgroup v2, cgroup v1, and the host's `/proc/meminfo` (smallest non-zero total wins, larger usage ratio breaks ties). Previously the reading could be misleading in two ways: on containers with a tight cgroup limit, the host view could indicate >70% while the cgroup limit was <10% (or the reverse); and on hosts where a cgroup is mounted with no memory limit set, the cgroup reading could collapse the reported usage ratio to ~0%. Overload-protection thresholds and the `Memory used` metric now reflect the limit that actually constrains the process.

- [#17271](https://github.com/emqx/emqx/pull/17271) Hardened the official EMQX Docker image to clear image-scanner findings:

  - Applied Debian security upgrades during the runtime image build, so the image picks up the latest patched `libssl3t64`.
  - Removed the unused `libgnutls30t64` package. EMQX talks TLS via OpenSSL through Erlang/OTP and never links GnuTLS, so it was only present as a transitive dependency of `curl` and showed up in scanner reports.
  - Replaced the Debian `curl` package with a statically-linked `curl` binary from [stunnel/static-curl](https://github.com/stunnel/static-curl) (OpenSSL, HTTP/2, HTTP/3; no RTMP, no GnuTLS). The Debian package would have transitively re-introduced `libgnutls30t64` via `librtmp1`; the static binary avoids this while keeping container health checks that call `curl` working unchanged.

- [#17311](https://github.com/emqx/emqx/pull/17311) Fixed Docker startup when the container hostname cannot be resolved. The entrypoint now falls back to the interface IP address before auto-generating the node name, and fails with a clear error if no node host can be determined.
- [#17342](https://github.com/emqx/emqx/pull/17342) Fixed cluster configuration import failing with a "required_field: node.cookie" schema check error when the exported `cluster.hocon` contained a partial `node` section. Read-only roots (`node`, `rpc`) are not part of the data import anyway, so they are now dropped from the imported config before the pre-flight schema check, letting the running node's own values be used for the validation.

- [#17369](https://github.com/emqx/emqx/pull/17369) Moved the Dashboard listener defaults (`http.bind` and the placeholder HTTPS `ssl_options`) from the user-editable `etc/emqx.conf` into the shipped `etc/base.hocon`. Previously, the hardcoded `emqx.conf` block silently reverted runtime updates to the default self-signed certificate on restart. Runtime updates made through the Dashboard, the REST API, or the `emqx_acme` plugin's automatic HTTPS configuration are now correctly preserved across restarts.

- [#17536](https://github.com/emqx/emqx/pull/17536) Documented the `file://` option in Dashboard tooltips for the SSL listener `password` and other secret-typed configuration fields (MQTT bridge password, cluster link password, Dashboard OIDC client secret, S3 secret access key, AI completion API key, Pulsar/RocketMQ credentials, etc.). The generic secret type description already mentioned this convention, but field-specific descriptions shadowed it in the Dashboard, causing users to assume the field accepted only literal values.

- [#17540](https://github.com/emqx/emqx/pull/17540) Fixed a bug where setting `password = "file://..."` on an SSL listener caused config validation to fail with `bad_password_or_invalid_keyfile` when the keyfile was encrypted. The `file://` reference is now resolved during validation, not only at runtime.

## 6.0.2

*Release Date: 2026-01-16*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.0.2.

### Enhancements

#### Security

- [#16461](https://github.com/emqx/emqx/pull/16461) EMQX now supports TLS 1.3 session resumption using stateless session tickets, allowing clients to resume TLS connections without requiring server-side session state.

  **Configuration**

  - **Node-level**: `node.tls_stateless_tickets_seed`

    Secret key seed used to generate TLS 1.3 stateless session tickets.

  - **Listener-level**: `listeners.ssl.<name>.ssl_options.session_tickets`

    Enables TLS 1.3 session resumption. Supported values:

    - `disabled` (default)
    - `stateless`
    - `stateless_with_cert` (includes certificate information in the ticket)

  **Notes**

  - Session tickets are generated only when `node.tls_stateless_tickets_seed` is configured (non-empty), and `session_tickets` is enabled in listener SSL options.
  - If `session_tickets` is enabled but `node.tls_stateless_tickets_seed` is empty, session tickets will not be generated and an error log will be emitted when starting the listener.

  This PR also included a fix for the TLS 1.2 session resumption configuration. Previously, the `reuse_sessions` option for SSL listener did not take effect, i.e. EMQX always tried to enable TLS 1.2 session resumption. It is now possible to turn it off. Please note that TLS 1.2 session resumption will be disabled by default starting version 6.2.0.

#### Rule Engine

- [#16524](https://github.com/emqx/emqx/pull/16524) Enhanced base64 encoding and decoding functions in rule engine SQL with support for padding and URL-safe options.

  The `base64_encode` and `base64_decode` functions now support optional parameters to control encoding behavior:

  - **`no_padding`**: Encode or decode without padding characters (`=`). Useful when you need to remove padding from encoded strings or decode strings that do not have padding.
  - **`urlsafe`**: Use URL-safe base64 encoding/decoding. Replaces `+` with `-` and `/` with `_`, making the encoded string safe to use in URLs without encoding.

  These options can be used individually or combined in any order.

  **Examples in rule SQL:**

  Encode without padding:
  ```sql
  SELECT base64_encode(payload, 'no_padding') as encoded FROM "t/#"
  ```

  Encode with URL-safe characters:
  ```sql
  SELECT base64_encode(payload, 'urlsafe') as encoded FROM "t/#"
  ```

  Encode with both options (no padding and URL-safe):
  ```sql
  SELECT base64_encode(payload, 'no_padding', 'urlsafe') as encoded FROM "t/#"
  ```

  Decode URL-safe base64:
  ```sql
  SELECT base64_decode(payload, 'urlsafe') as decoded FROM "t/#"
  ```

  Decode unpadded URL-safe base64:
  ```sql
  SELECT base64_decode(payload, 'urlsafe', 'no_padding') as decoded FROM "t/#"
  ```

- [#16533](https://github.com/emqx/emqx/pull/16533) Added two new variadic expression helper functions, `json_value` and `jwt_value`, for extracting values from JSON data and JWT tokens using dot-separated key paths.

  - `json_value` extracts values from JSON binary strings by navigating nested objects with a dot-separated key path.
  - `jwt_value` decodes the payload of a JWT and extracts claim values using the same dot-separated path syntax.

  **Examples**:

  - If `username` contains a JSON object, you can access a nested field with `json_value(username, 'shop.floor')`.
  - If `password` contains a JWT with a customized claim, you can access a nested value with `jwt_value(password, 'client_attrs.unitid')`.

- [#16539](https://github.com/emqx/emqx/pull/16539) Added support for tracking Sparkplug B metric aliases when using the `spb_decode` Rule Engine function.

  After a device or Edge of Network (EoN) node publishes its `NBIRTH` or `DBIRTH` messages, EMQX records the alias-to-name mappings defined in those messages. When `spb_decode` is later applied to `NDATA` or `DDATA` messages from the same session, the original metric names are automatically restored and included in the decoded output.

  Note: when executing fallback actions, the mapping is not available in the environment where they run. This means that, if a fallback action republishes the undecoded `DDATA`/`NDATA` payload to a Sparkplug B `DDATA`/`NDATA` topic, the metric `name` fields will not be populated by the alias mapping.

#### Durable Storage

- [#16136](https://github.com/emqx/emqx/pull/16136) Improved resource management and performance for durable storage.

  Introduced a concept of a durable storage database group. Certain resources (such as memtable size and disk usage quota) can be shared between the group members.

  Added the following new metrics (per DB group):

  - `emqx_ds_disk_usage`: Total size of SST files
  - `emqx_ds_write_buffer_memory_usage`: RocksDB memtable size
  - `emqx_ds_total_trash_size`: Disk usage by trash SST files

  Added the following group configurations:

  - `durable_storage.db_groups.<group>.storage_quota`: Soft quota for the SST files size
  - `durable_storage.db_groups.<group>.write_buffer_size`: Maximum memtable size
  - `durable_storage.db_groups.<group>.rocksdb_nthreads_high` and `durable_storage.db_groups.<group>.rocksdb_nthreads_low`: Size of RocksDB thread pools.

  Added a new alarm that is raised when the quota is exceeded: `db_storage_quota_exceeded:<DB>`. Please refer to the "Storage Quota" section of the documentation for more details.

  Default session checkpoint interval has been changed to 15s.

- [#16286](https://github.com/emqx/emqx/pull/16286) Optimized the default durable storage settings to reduce CPU load. This PR disables subscriptions for DBs that don't use them.

#### Performance

- [#16413](https://github.com/emqx/emqx/pull/16413) Improved subscription handling performance by reducing redundant monitoring of MQTT session processes.

### Bug Fixes

#### Core MQTT Functionalities

- [#16354](https://github.com/emqx/emqx/pull/16354) Fixed a crash in MQTT v5 connections caused by a type mismatch when processing the request-response-information property.

- [#16515](https://github.com/emqx/emqx/pull/16515) Fixed an issue where WebSocket connections could crash when the broker sent messages exceeding the client-advertised `Maximum-Packet-Size`.

- [#16569](https://github.com/emqx/emqx/pull/16569) Fixed a rare race condition that could cause the supporting `emqx_flapping` process for flapping detection to crash under high system load.

#### Data Integration

- [#16265](https://github.com/emqx/emqx/pull/16265) The health check now verifies leader connectivity only for the partitions assigned to the current EMQX node, preventing unnecessary idle connections and false alarms.

  Previously, the Kafka source connector checked leader connectivity for all partitions. In clustered deployments, each node owns only a subset of partitions, leaving connections to unassigned partition leaders idle. Because Kafka closes idle connections after a timeout (10 minutes by default), this could result in false connectivity alarms.

- [#16542](https://github.com/emqx/emqx/pull/16542) Fixed an issue where Kafka producer connections could disconnect prematurely when Kafka was overloaded, leading to excessive produce request retries.

  The produce request timeout is now automatically set to at least twice the metadata request timeout, with a minimum of 30 seconds. This reduces unnecessary reconnections and retries when metadata requests take longer than expected, especially when the metadata request timeout is configured to a small value.

- [#16352](https://github.com/emqx/emqx/pull/16352) Upgraded Apache Pulsar client to 2.1.2. When Pulsar producer action's `batch_size` is configured to `1`, the producer will now encode single messages instead of single-element batch. This should allow consumers to share load using Key Share strategy.

- [#16383](https://github.com/emqx/emqx/pull/16383) Improved the IoTDB Connector health check when using the REST API driver.

  Previously, client credentials were not validated during health checks. The health check now sends a lightweight no-op query, allowing misconfigured credentials to be detected early.

- [#16507](https://github.com/emqx/emqx/pull/16507) Fixed an issue where an MQTT Source would stop receiving messages after its Connector reconnected.

  Previously, when an MQTT Source’s Connector recovered from a connection loss, its topics were not re-subscribed, causing the Source to stop working until the Connector was restarted. The Source now automatically re-subscribes upon reconnect.


#### Clustering

- [#16269](https://github.com/emqx/emqx/pull/16269) Fixed an issue in the Cluster Linking route replication protocol recovery sequence where re-bootstrapping was incorrectly skipped even though the remote side needed it.

- [#16317](https://github.com/emqx/emqx/pull/16317) Fixed an issue in Cluster Linking garbage-collection logic that could incorrectly remove active routes from the internal routing table while cleaning up stale route replication state.

  This issue could occur only in setups with multiple independent Cluster Links, where some links remained down for extended periods.

- [#16465](https://github.com/emqx/emqx/pull/16465) Upgraded `gen_rpc` to `3.5.1`.

  Before the `gen_rpc` upgrade, EMQX may experience a long tail of crash logs due to a connect timeout if a peer node is unreachable. The new version of gen_rpc no longer has the long tail and has converted crash logs to more readable error logs. Additionally, the frequent log `"failed_to_connect_server"` is also throttled to avoid spamming.

- [#16544](https://github.com/emqx/emqx/pull/16544) Improved the robustness of the cluster autoclean procedure. Previously, if the autoclean feature was disabled during the initial startup of a node, it would not be activated after subsequent configuration changes.

#### Upgrade

- [#16308](https://github.com/emqx/emqx/pull/16308) Fixed an issue where Multi-Factor Authentication (MFA) could not be enabled after upgrading EMQX from versions earlier than 5.3.0 due to incompatible login-user database records.

#### Configuration Management

- [#16397](https://github.com/emqx/emqx/pull/16397) Added TLS certificate and key file validation before listener startup.

  EMQX now performs basic validation when parsing SSL listener configuration and emits error-level logs if invalid PEM files are detected (for example, `invalid_pem_file_ignored` and `bad_keyfile_ignored`). This makes troubleshooting easier as administrators can observe errors when starting/reconfiguring, instead of troubleshooting TLS handshake failures.

#### Access Control

- [#16423](https://github.com/emqx/emqx/pull/16423) Added support for verifying the JWT `aud` (audience) claim during authentication.

  When the `aud` claim is configured in `verify_claims`, the JWT must include a valid `aud` value. Both string and array formats are supported:

  - If `aud` is a string, it must exactly match the configured value.
  - If `aud` is an array, at least one element must match the configured value.
  - An empty string or empty array fails verification.
  - The verification also fails if the `aud` claim is missing when it is configured in `verify_claims`.

- [#16459](https://github.com/emqx/emqx/pull/16459) Fixed the issue in SCRAM authentication HTTP API. Previously, incorrect user ID was returned for the created user in the user creation API call.

#### Observability

- [#16417](https://github.com/emqx/emqx/pull/16417) Reduced log volume for `resource_exception` events. Logs generated when a resource exception occurs are now throttled, and potentially large terms are redacted to prevent excessive log output.

- [#16537](https://github.com/emqx/emqx/pull/16537) Fixed a formatter crash triggered by certain `gen_rpc` error messages.

  Previously, EMQX could crash with a “FORMATTER CRASH” error when `gen_rpc` logged specific errors (such as transmission timeouts). The formatter now safely handles these messages without crashing.

## 6.0.1

*Release Date: 2025-11-11*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.0.1.

### Enhancements

#### Message Queue

- [#16080](https://github.com/emqx/emqx/pull/16080) Added a configuration option to disable the Message Queues feature. Disabling Message Queues can slightly reduce the resource usage in the cluster. When Durable Sessions are also disabled, EMQX avoids maintaining Durable Storage, further reducing administrative overhead and improving performance.
- [#16096](https://github.com/emqx/emqx/pull/16096) Added support for automatic creation of message queues when clients subscribe to non-existent `$q/` topics. Now configuration options are available to enable auto-creation for both regular and last-value semantics queues.
- [#16097](https://github.com/emqx/emqx/pull/16097) Optimized message writing to regular message queues by replacing transactional appends with dirty append functions. For QoS 0 messages, asynchronous append operations are now used. These changes significantly improve the performance of message insertion into regular queues.
- [#16098](https://github.com/emqx/emqx/pull/16098) Added a maximum queue count configuration option to limit the total number of message queues in the system.
- [#16152](https://github.com/emqx/emqx/pull/16152) Introduced per-queue limits for maximum message count and total message size. Also added new metrics to monitor message append latency and help diagnose performance or queue-limiting issues.

#### Data Integration

- [#16121](https://github.com/emqx/emqx/pull/16121) Upgraded the GreptimeDB ingester client to [v0.2.3](https://github.com/GreptimeTeam/greptimedb-ingester-erl/releases/tag/v0.2.3), which fixes several bugs and introduces support for row-based gRPC protocol (the column-based protocol is now deprecated).

  Additionally, updated the CI image to the latest stable version of GreptimeDB.

- [#16127](https://github.com/emqx/emqx/pull/16127) Fixed an invalid string value issue in the GreptimeDB connector, following the changes introduced in [#16121](https://github.com/emqx/emqx/pull/16121).

#### Performance

- [#15949](https://github.com/emqx/emqx/pull/15949) Changed the default value of the `parse_unit` option in listener configuration from `chunk` to `frame`. This change can significantly reduce CPU usage when the payload size exceeds the socket buffer (default is 4 KB).

  **Note**: With `parse_unit = frame`, if a `PUBLISH` packet exceeds the maximum allowed size, EMQX will close the connection instead of sending a `DISCONNECT` packet.

- [#16165](https://github.com/emqx/emqx/pull/16165) Optimized the performance of the `GET /clients_v2` API. Previously, when the cluster had around 50,000 clients or more, API calls to retrieve the client list could be extremely slow or even time out.

### Bug Fixes

#### Core MQTT Functionalities

- [#15884](https://github.com/emqx/emqx/pull/15884) Resolve an issue where, in rare cases, the global routing table could indefinitely retain routing information for nodes that had long left the cluster.
- [#15518](https://github.com/emqx/emqx/pull/15518) Resolved a race condition that may lead to accumulating inconsistencies in the routing table and shared subscriptions state in the cluster when a large number of shared subscribers disconnect simultaneously.

#### Upgrade

- [#16047](https://github.com/emqx/emqx/pull/16047) Added support to perform rolling upgrade from EMQX Enterprise base version 5.8.0 and newer to 6.0. During the upgrade, legacy configurations are automatically migrated to the new format supported in 6.0. Specifically, the deprecated `bridges` configuration root is converted into the new `connectors`, `sources`, and `actions` roots.

  However, the GCP PubSub Consumer and Kafka Consumer sources will still require manual changes. If any source configuration still includes the deprecated `topic_mapping` field, it must be removed. Then, for each entry previously defined in `topic_mapping`, a separate "Source + Rule" pair must be created manually.


#### Security

- [#16156](https://github.com/emqx/emqx/pull/16156) Fixed an issue where some dependencies were missing default configurations compared to EMQX 5.10, potentially causing RSA signature verification failures. The missing defaults could lead to errors, such as the following log message:

  ```
  {sign_unsupported,[[{rsa_padding,rsa_pkcs1_padding}]]}, [{jose_jwa_unsupported,verify,5,[{file,"src/jwa/jose_jwa_unsupported.erl"},{line,55}]}
  ```

- [#16175](https://github.com/emqx/emqx/pull/16175) Fixed an issue with periodic TLS certificate garbage collection. Previously, the garbage collection process incorrectly deleted certificate files that were actively used by configurations in managed namespaces.

#### Access Control

- [#16081](https://github.com/emqx/emqx/pull/16081) Fixed an issue where clients using extended authentication and memory-based sessions could crash with a `session_stepdown_request_exception` caused by a `calling_self` error.

  <details> <summary>Example error log</summary>


  ```
  2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
  ```

  </details>

#### Clustering

- [#16123](https://github.com/emqx/emqx/pull/16123) Fix a bug in the component managing Mria replication that could cause cluster joins to hang or remain incomplete in core-replicant clusters.

  During cluster changes involving adding new core nodes, those new core nodes could sometimes fail to start replication-related processes required by replicants. As a result, upgraded or newly added replicants could hang during startup.

  In Kubernetes deployments, this often caused readiness probes to fail, leading the controller to repeatedly restart the affected replicant pods.

  This issue typically affected upgrade rollouts involving the addition of new core and replicant nodes. For example, adding two cores and two replicants (running a newer EMQX version) to an existing cluster with 2 cores and 2 replicants.

#### Rule Engine

- [#16028](https://github.com/emqx/emqx/pull/16028) Fixed rule engine `jq` function memory leak.

  Previously if `jq` built-in function `index` is used (e.g. `.key | index("name")`), it would result in memory leak.

#### Data Integration

- [#16010](https://github.com/emqx/emqx/pull/16010) Fixed an issue where a Republish Fallback Action could fail with a `function_clause` error if the originating rule's SQL did not include the `metadata` field from the rule environment.

  Example error log:

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16046](https://github.com/emqx/emqx/pull/16046) Fixed a potential out-of-memory (OOM) crash when loading or restarting a configuration containing a Connector with several hundred Actions.

- [#16140](https://github.com/emqx/emqx/pull/16140) Fix a Redis cluster failover issue that could cause the Connector to remain stuck in a "connecting" state.

  Previously, EMQX’s Redis cluster client only refreshed the cluster topology when regular queries (such as `GET`) failed. However, failures in periodic `PING` commands did not trigger a refresh. As a result, after a failover, the connector could continue using the outdated cluster topology if no other commands were issued, preventing recovery.

  With this fix, failed `PING` responses now trigger a cluster topology refresh, ensuring that the connector can detect failovers and recover promptly.

#### MQTT Durable Sessions

- [#16105](https://github.com/emqx/emqx/pull/16105) Durable storage performance optimization. In particular, this fix reduces the latency of `CONNACK` for clients using a durable session.
- [#16129](https://github.com/emqx/emqx/pull/16129) Durable storage transaction configuration can be changed in the runtime. Previously changing this configuration required a node restart.

#### Observability

- [#15963](https://github.com/emqx/emqx/pull/15963) Reduced excessive audit log entries generated during looped evaluations in the remote shell (`remsh`).

- [#15967](https://github.com/emqx/emqx/pull/15967) Fixed an issue where Mnesia transaction blocking during the cleanup of large volumes of audit logs could lead to rapid memory growth.

- [#16060](https://github.com/emqx/emqx/pull/16060) Fixed a logger formatter crash that could occur for some debug-level log messages containing deeply nested terms with non-ASCII characters.

  <details> <summary>Example error log</summary>


  ```
  2025-09-29T06:55:34.120640+00:00 debug: FORMATTER CRASH: {report,#{request => #{messages => [#{role => <<"user">>,content => <<"{\"msg\": \"hello\"}">>}],system => <<"将输入的 JSON 数据中，值为数字的 value 相加起来，并输出，只需返回输出结果。"/utf8>>,model => <<"claude-3-haiku-20240307">>,max_tokens => 100},msg => emqx_ai_completion_request}}
  2025-09-29T06:55:34.120780+00:00 [debug] formatter_crashed: emqx_logger_textfmt, config: #{time_offset => [],chars_limit => unlimited,depth => 100,single_line => true,template => ["[",level,"] ",msg,"\n"],with_mfa => false,timestamp_format => auto,payload_encode => text}, log_event: #{meta => #{line => 44,pid => <0.281254.0>,time => 1759128934120640,file => "emqx_ai_completion_anthropic.erl",gl => <0.4317.0>,mfa => {emqx_ai_completion_anthropic,call_completion,3},report_cb => fun logger:format_otp_report/1,matched => <<"t/1">>,namespace => global,clientid => <<"c_emqx">>,trigger => <<"t/1">>,rule_id => <<"r1sczoo0">>,rule_trigger_ts => [1759128934120]},msg => {report,#{request => #{messages => [#{role => <<"user">>,content => <<"{\"msg\": \"hello\"}">>}],system => <<"将输入的 JSON 数据中，值为数字的 value 相加起来，并输出，只需返回输出结果。"/utf8>>,model => <<"claude-3-haiku-20240307">>,max_tokens => 100},msg => emqx_ai_completion_request}},level => debug}, reason: {error,badarg,[{erlang,iolist_to_binary,[["[",[["messages",": ",[[91,[[35,123,[["role"," => ",[60,60,"\"user\"",62,62]],44,["content"," => ",[60,60,"\"{\\\"msg\\\": \\\"hello\\\"}\"",62,62]]],125]],93]]],", ",["system",": ","将输入的 JSON 数据中，值为数字的 value 相加起来，并输出，只需返回输出结果。"],", ",["model",": ","claude-3-haiku-20240307"],", ",["max_tokens",": ","100"]],"]"]],[{error_info,#{module => erl_erts_errors}}]},{emqx_trace_formatter,format_term,2,[{file,"emqx_trace_formatter.erl"},{line,126}]},{emqx_logger_textfmt,format_term,2,[{file,"emqx_logger_textfmt.erl"},{line,230}]},{emqx_logger_textfmt,try_encode_meta,4,[{file,"emqx_logger_textfmt.erl"},{line,206}]},{lists,foldl_1,3,[{file,"lists.erl"},{line,2151}]},{emqx_logger_textfmt,enrich_report,3,[{file,"emqx_logger_textfmt.erl"},{line,102}]},{emqx_logger_textfmt,format,2,[{file,"emqx_logger_textfmt.erl"},{line,24}]}]}
  ```

  </details>

- [#16134](https://github.com/emqx/emqx/pull/16134) Fixed a backward compatibility issue that could prevent new Log Traces from being created in some cases.

#### Rate Limit

- [#16160](https://github.com/emqx/emqx/pull/16160) Improved the rate limiting algorithm for individual client connections. Previously, clients could temporarily exceed their publish rate limits, particularly just after connecting or after periods of inactivity.

  This update makes the limiter behavior more predictable and consistent, ensuring rate limits are correctly enforced from the start of a connection.

## 6.0.0

*Release Date: 2025-09-30*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.0.0.

### Feature Highlights

EMQX Enterprise 6.0.0 is the first release of the EMQX Enterprise version 6 series, bringing significant architectural improvements and new capabilities.

#### Message Queue

The native Message Queue feature unifies real-time MQTT publish/subscribe with persistent asynchronous queuing. The server buffers messages that match a topic filter, retaining them even when subscribers are offline. Clients can consume these messages through the special `$q/{topic}` topic, ensuring reliable message delivery.

Message Queues support offline message storage, last-value retention, and flexible dispatch strategies, enhancing MQTT with both real-time and durable messaging capabilities.

#### Namespace

The Namespace feature improves multi-tenancy and observability with namespace-level roles in the Dashboard. Users are restricted to their own resources (e.g., Rules, Actions, and Connectors) with fine-grained permissions such as Administrator or Viewer, and roles can be managed via the Dashboard, API, or CLI, simplifying multi-tenant operations.

Session count tracking has also been optimized: counts refresh on demand when there are fewer than 1,000 connections, and every 5 seconds otherwise. During rolling upgrades from older versions, counts may temporarily appear inconsistent, but will stabilize once all nodes are updated.

#### MQTT Durable Sessions

Durable storage has been optimized by separating session data from the broker’s other metadata, significantly reducing RAM usage and improving storage efficiency.

New configuration options provide finer control over RocksDB memory usage and performance. In addition, the default serialization schema for stored messages has been updated to ASN.1, further enhancing efficiency.

#### New Data Integrations

- Google BigQuery
- AWS AlloyDB
- CockroachDB
- AWS Redshift

#### Enhanced Integration

- **AWS**:
  - Support for Instance Metadata Service v2 APIs from EC2 instances when using S3 or S3Tables data integration. This enables seamless access to S3 buckets without manual AWS credential configuration, leveraging IAM roles for better security.
  - Parquet format support for S3 Tables Action.

- **RabbitMQ**: Define custom Headers and Properties Templates in RabbitMQ Sink to enhance message routing and compatibility within RabbitMQ.
- **Snowflake**: Snowpipe Streaming upload mode for Snowflake Action (preview feature).
- **RocketMQ**: New `key` and `tag` template fields in Action, along with a `key_dispatch` option for the Produce Strategy, allowing greater customization of message metadata.

#### Elixir Support

All packages now ship with Elixir support through the Mix build system, opening EMQX to the Elixir community and enabling better tooling with IEx console.

#### Enhanced LDAP Support

LDAP authorization now supports extended ACL rules in JSON format, and LDAP authentication can fetch ACL rules directly from LDAP with client-side caching.

#### Improved Tracing

Configurable limits for maximum traces (`trace.max_traces`) and trace file sizes (`trace.max_file_size`).
After `max_file_size` is reached, the trace log will rotate to a new file instead of halting.

#### Cluster Management

New `cluster.description` configuration option allows users to set and display custom cluster descriptions in the EMQX Dashboard.

### Enhancements

#### Message Queue

- [#15789](https://github.com/emqx/emqx/pull/15789) Implemented Message Queues, which are collections of messages identified by `topic_filter`. Each queue has an explicit lifecycle and is automatically replenished with published messages matched with the queue's topic filter during the queue's lifetime. Clients can cooperatively consume messages from a queue by subscribing to a special topic in the format: `$q/{topic}`.

#### Core MQTT Functionalities

- [#15805](https://github.com/emqx/emqx/pull/15805) Introduced a dedicated worker pool for handling sharded fanout message delivery.
  Previously, the broker pool handled both subscription management and message dispatch, which could lead to scheduling contention. This change separates the fanout dispatch workload into its own pool to ensure more balanced and efficient handling of pub/sub operations.

#### Access Control

- [#15349](https://github.com/emqx/emqx/pull/15349) Optimize external resource management for authentication and authorization. Previously, EMQX could remain connected to a resource configured for a disabled authenticator or authorizer.

- [#15294](https://github.com/emqx/emqx/pull/15294) Enhanced LDAP authentication and authorization. LDAP authorization now supports extended ACL rules in JSON format. LDAP authentication can now fetch ACL rules from LDAP. These rules are cached in the client's metadata, so authorization is performed without additional LDAP queries.

- [#15730](https://github.com/emqx/emqx/pull/15730) Added support for overriding the client ID based on authentication results. If an authentication backend returns a `clientid_override` attribute upon successful authentication, it will replace the client’s original client ID.

  The following backends now support `clientid_override`:

  - HTTP
  - JWT
  - LDAP
  - MongoDB
  - MySQL
  - Postgres
  - Redis

- [#15820](https://github.com/emqx/emqx/pull/15820) Changed default value of config `authorization.no_match` from `allow` to `deny` for better security defaults.

#### Clustering

- [#15600](https://github.com/emqx/emqx/pull/15600) Introduced a new configuration option `cluster.description` that allows you to add a descriptive label to the EMQX cluster.  This description can be updated via `PUT /cluster`, and retrieved with the `GET /cluster` API.

#### LLM-Based MQTT Data Processing

- [#15467](https://github.com/emqx/emqx/pull/15467) Exposed transport configuration options for AI Completion Providers. Users can now configure connection timeouts and the maximum number of connections to AI Completion Providers. This helps prevent `checkout_timeout` errors when message throughput is high and the provider is under load.
- Flow designer supports integrating with the [Google Gemini model](https://docs.mqttce.com/en/emqx/v6.0/flow-designer/gemini-node-quick-start.html).

- [#15631](https://github.com/emqx/emqx/pull/15631) Added a new API endpoint to list all models available for an AI provider.
- [#15467](https://github.com/emqx/emqx/pull/15467) Exposed transport options for AI Completion Providers. These options allow configuring connection timeouts and maximum connections to an AI Completion Provider.
- [#15724](https://github.com/emqx/emqx/pull/15724) Introduced `openai_response` type for AI Completion Providers and completion profiles to use OpenAI's `response` API.

#### Data Integration

- [#15418](https://github.com/emqx/emqx/pull/15418) EMQX supports data integration with BigQuery.

- [#15401](https://github.com/emqx/emqx/pull/15401) Added support for the Snowpipe Streaming upload mode in the Snowflake Action.
  *Note: Snowpipe Streaming is currently a* [*preview feature*](https://docs.snowflake.com/en/release-notes/preview-features) *and is only available for Snowflake accounts hosted on AWS.*

- [#15387](https://github.com/emqx/emqx/pull/15387) Added rate limiting to Kinesis Producer Connector and Action health checks to comply with AWS API quotas and improve cluster behavior.

  - Health check calls to `ListStreams` and `DescribeStream` are now limited to 5/s and 10/s per Connector, respectively, matching AWS rate limits.
  - A distributed limiter is coordinated by a core node in the cluster to enforce these limits consistently.
  - If a health check is throttled or times out, the Connector or Action will now retain its previous status instead of being marked as disconnected.

  Also introduced a new `resource_opts.health_check_interval_jitter`, which adds a uniform random delay to `resource_opts.health_check_interval` to reduce the chance of multiple Actions under the same Connector running health checks at the same time.

- [#15176](https://github.com/emqx/emqx/pull/15176) Upgraded the GreptimeDB Connector client and supported an optional new parameter `ttl` to set the default time-to-live for automatically created tables.

- [#15649](https://github.com/emqx/emqx/pull/15649) EMQX supports data integration with AWS AlloyDB, CockroachDB, and AWS Redshift.

- [#15635](https://github.com/emqx/emqx/pull/15635) Added new `key` and `tag` template fields in the RocketMQ Action, allowing customization of the message's key and tag. Also, introduced a new `key_dispatch` option for the `Produce Strategy` field.

- [#15621](https://github.com/emqx/emqx/pull/15621) Now, `access_key_id` and `secret_access_key` are optional fields for the S3 Tables Connector.  If omitted, they'll be obtained from the Instance Metadata Service v2 APIs from the EC2 instance where EMQX is deployed.

- [#15628](https://github.com/emqx/emqx/pull/15628) Removed HStreamDB data integration.

- [#15544](https://github.com/emqx/emqx/pull/15544) Added Arrow Flight SQL NIF driver support for Datalayers Integration.

- [#15637](https://github.com/emqx/emqx/pull/15637) Added support for templating message headers and properties for the RabbitMQ Action.

- [#15864](https://github.com/emqx/emqx/pull/15864) Removed the deprecated "Bridges V1" APIs and configuration schemas. All endpoints under `/bridges/*` and configuration entries under the `bridges` root key are no longer available, as data integrations have fully migrated to the "Connectors/Actions/Sources" model.

- [#15583](https://github.com/emqx/emqx/pull/15583) Updated the `brod` client to version 4.4.4, expanding support for a wider range of Kafka APIs. This update addresses the deprecation of `JoinGroups` API versions `v0` - `v1`.

#### Smart Data Hub

- [#15525](https://github.com/emqx/emqx/pull/15525) Prevented deletion of internal schemas that are still in use. If a schema is referenced by a Schema Validation or Message Transformation, it can no longer be removed to avoid runtime errors and configuration inconsistencies.

#### Durable Storage

- [#15463](https://github.com/emqx/emqx/pull/15463) Improved durable storage RAM usage and storage efficiency.
  - Introduced the following configuration parameters for the durable storage to improve control over RocksDB memory usage and storage performance:
    - `durable_storage.messages.rocksdb.write_buffer_size`: RocksDB memtable size per shard.
    - `durable_storage.messages.rocksdb.cache_size`: RocksDB block size per shard.
    - `durable_storage.messages.rocksdb.max_open_files`: Limits the number of file descriptors used by RocksDB per shard.
    - `durable_storage.messages.layout.wildcard_thresholds`: Allows to tune wildcard thresholds for the `wildcard_optimized_v2` storage layout.
  - Additionally, the default `serialization_schema` for stored messages has been changed to `asn1`.

- [#16044](https://github.com/emqx/emqx/pull/16044) Some of config fields for durable sessions have been removed or renamed, and old values are marked as deprecated:

    - `durable_sessions.heartbeat_interval` has been renamed to `durable_sessions.checkpoint_interval`.
    - `durable_sessions.idle_poll_interval` and `durable_sessions.renew_streams_interval` have been removed, as sessions are now fully event-driven.
    - `durable_sessions.session_gc_interval` and `durable_sessions.session_gc_batch_size` have been removed as obsolete.

#### CLI

- [#15399](https://github.com/emqx/emqx/pull/15399) The `node_dump` tool now exports the current system configuration in HOCON format, with sensitive information (such as passwords and secrets) automatically redacted for security.

#### Namespace

- [#15841](https://github.com/emqx/emqx/pull/15841) Improved the refresh rate of the session count for namespaced sessions.

  - If a namespace has fewer than 1000 connections, its session count is now updated on demand.
  - For namespaces with 1000 or more connections, the count is updated every 5 seconds.

  During a rolling upgrade from versions prior to 6.0, session counts may appear inconsistent due to changes in the internal tracking tables. This is expected: as clients reconnect to upgraded nodes, the session counts will gradually stabilize and become accurate once all nodes are running version 6.0 or later.

#### Observability

- [#15594](https://github.com/emqx/emqx/pull/15594) Introduced a new configuration option `trace.max_traces` to control the maximum number of active cluster-wide traces. This limit does not apply to node-local traces managed using `emqx ctl trace`.

  This update also optimized tracing implementation to eliminate potential atom leaks per created trace.

- [#15556](https://github.com/emqx/emqx/pull/15556) Introduced a new configuration option `trace.max_file_size` to limit the maximum file size for each individual trace.

- [#15650](https://github.com/emqx/emqx/pull/15650) Implemented automatic trace log rotation.

  When a trace file size exceeds `trace.max_file_size`, EMQX no longer discards all subsequent events and emits an incomprehensible warning to `stderr`. Instead, portions of the oldest events are discarded while the most recent ones are retained.

  As such, this also implies that:

  * EMQX now maintains multiple trace log files per active trace. The layout of the trace directory has changed accordingly.
  * Trace API has been updated to reflect this behavior. The Log Stream API may return new errors, such as when a stream becomes stale due to a slow consumer.


- [#15904](https://github.com/emqx/emqx/pull/15904) Support viewing and updating of tracing configuration through Trace API.

#### Performance

- [#15451](https://github.com/emqx/emqx/pull/15451) Introduced an experimental `socket` backend for TCP listeners, aimed at improving message processing latency and reducing compute resource usage. The feature can be enabled with the new `tcp_backend` listener option.

#### Build and Tooling

- [#15484](https://github.com/emqx/emqx/pull/15484) Switched the build system to [Elixir](https://elixir-lang.org/)'s [Mix](https://hexdocs.pm/elixir/introduction-to-mix.html), enabling all packages to include native Elixir support. This change improves developer tooling, allows integration with Elixir dependencies when needed, and enables use of the [IEx](https://hexdocs.pm/iex/IEx.html) shell as a more powerful EMQX console.

#### License

- [#15921](https://github.com/emqx/emqx/pull/15921) Introduced a license alarm for cluster-wide maximum transactions per second (TPS).
  - Each node calculates TPS as the average number of MQTT messages sent and received over the past 10 seconds.
  - The total cluster TPS is aggregated every 5 seconds.
  - If the observed TPS exceeds the licensed limit, an alarm is triggered.
  - The alarm remains active until a license with a higher TPS allowance is applied.

#### MQTT over QUIC

- [#15997](https://github.com/emqx/emqx/pull/15997) Added support for disabling QUIC stack loading by setting the environment variable `QUICER_SKIP_NIF_LOAD=1.`

### Bug Fixes

#### Core MQTT Functionalities

- [#15396](https://github.com/emqx/emqx/pull/15396) Removed redundant cleanup operations for shared subscriptions of disconnected clients. These operations were prone to crashes under high disconnect volumes and could lead to inconsistencies in the global broker state.

- [#15361](https://github.com/emqx/emqx/pull/15361) Fixed a `function_clause` error when parsing a malformed `User-Property` pair with invalid (too short) length.

- [#15783](https://github.com/emqx/emqx/pull/15783) Ensure that any changes to connection rate limits take effect immediately after the listener update has completed. Previously, parts of internal limiter state were not directly affected by configuration changes. For example, after increasing the burst rate, the effective rate limit could appear stricter than expected.

#### Access Control

- [#15489](https://github.com/emqx/emqx/pull/15489) Fixed OIDC issuer URL validation in Single Sign-On (SSO) settings. Previously, issuer URLs containing a port number (for example,
  `https://xxxxxxxx:8443/webman/sso/.well-known/openid-configuration`) were rejected with a `bad_port_number` error. These URLs are now supported.

#### Rule Engine

- [#15569](https://github.com/emqx/emqx/pull/15569) Fixed an issue where a Republish Rule Action could fail if the `direct_dispatch` template was empty or resolved to a non-boolean value. In these cases, the default value `false` is now used.

#### Data Integration

- [#15522](https://github.com/emqx/emqx/pull/15522) Fixed an issue where Snowflake Connector would fail to start correctly if `username` was not provided.
- [#15476](https://github.com/emqx/emqx/pull/15476) Fixed a missing callback in `emqx_connector_aggreg_delivery` that caused a crash when formatting delivery process status for aggregated-mode Actions (e.g., Azure Blob Storage, Snowflake, S3 Tables).
  This occurred during failures or when inspecting delivery processes with `gen_server:format_status/1`. The issue is now resolved, and more detailed delivery status information will be logged.
- [#15394](https://github.com/emqx/emqx/pull/15394) Fixed a rare race condition where Action metrics could become inconsistent due to unexpected asynchronous replies.
- [#15647](https://github.com/emqx/emqx/pull/15647) Fixed an issue where a MongoDB Connector was marked as `Disconnected` if the MongoDB account specified in the connector configuration lacked privileges to perform `find` queries on the `foo` collection.
- [#15603](https://github.com/emqx/emqx/pull/15603) Fixed an issue in the MQTT bridge where a stale connection could be shown as `Connected` and would not automatically reconnect.
- [#15383](https://github.com/emqx/emqx/pull/15383) Fixed a potential resource leak in MQTT bridge. When a bridge failed to start, the topic index table was not properly cleaned up.
- [#15786](https://github.com/emqx/emqx/pull/15786) Fixed a potential atom leak when probing RocketMQ Connectors.
- [#15806](https://github.com/emqx/emqx/pull/15806) Improved validation for Oracle Actions during creation. Previously, in rare cases, an Action containing an invalid SQL statement could be added successfully.
- [#15848](https://github.com/emqx/emqx/pull/15848) Improved error reporting for the Oracle Connector. When the connector becomes disconnected, its status now includes a more specific reason, making diagnostics easier.
- [#15693](https://github.com/emqx/emqx/pull/15693) Fixed a resource leak in Postgres-based bridges. Under certain race conditions during pool initialization, deleting a Connector could leave its connection pool behind. This has been corrected to ensure connection pools are properly cleaned up.
- [#15543](https://github.com/emqx/emqx/pull/15543) Fixed an issue in HTTP Server data integration when sending large payloads. If the payload size was 10 MB or more, the HTTP request could fail.

#### Smart Data Hub

- [#15839](https://github.com/emqx/emqx/pull/15839) Fixed an encoding issue with Protobuf schemas that use `map<_, _>` fields.
  Previously, schemas containing `map<string, string>` fields could fail to encode valid payloads, resulting in cryptic runtime errors.

  Example schema:

  ```protobuf
  syntax = "proto3";

  message test {
  map<string, string> args = 1;
  }
  ```

  Example rule:

  ```sql
  SELECT
  schema_encode('xxx', json_decode(payload), 'test') as protobuf_test
  FROM
  "t/#"
  ```

  Example payload failed to be encoded:

  ```json
  {
  "args": {
  "env": "stag"
  }
  }
  ```

  Previous error similar to:

  ```
  2025-06-17T06:59:22.725785+00:00 [warning] tag: RULE_SQL_EXEC, clientid: c_emqx, msg: SELECT_clause_exception, reason: {error,{gpb_type_error,{bad_unicode_string,[{value,env},{path,"test.args.key"}]}},[{'$schema_parser_xxx',mk_type_error,3,[{file,"$schema_parser_xxx.erl"},{line,437}]},{'$schema_parser_xxx','-v_map<string,string>/3-lc$^0/1-0-',3,[{file,"$schema_parser_xxx.erl"},{line,429}]},{'$schema_parser_xxx','v_map<string,string>',3,[{file,"$schema_parser_xxx.erl"},{line,429}]},{'$schema_parser_xxx',v_msg_test,3,[{file,"$schema_parser_xxx.erl"},{line,404}]},{'$schema_parser_xxx',encode_msg,3,[{file,"$schema_parser_xxx.erl"},{line,73}]},{emqx_schema_registry_serde,with_serde,2,[{file,"emqx_schema_registry_serde.erl"},{line,212}]}...
  ```

#### Observability

- [#15931](https://github.com/emqx/emqx/pull/15931) Resolved a bug where spurious but harmless error logs could appear during node startup:
    ```
    [error] Generic event handler emqx_alarm_handler crashed ...
    Reason: {aborted,{no_exists,[emqx_activated_alarm,runq_overload]}}
    ```

- [#15973](https://github.com/emqx/emqx/pull/15973) Fixed a bug where an alarm activation timeout could crash the connection process under certain conditions.

#### MQTT over QUIC

- [#15614](https://github.com/emqx/emqx/pull/15614) QUIC Listener: When TLS key logging (`SSLKEYLOGFILE`) is enabled, EMQX now dumps TLS keys even if the handshake fails.

#### Clustering

- [#16021](https://github.com/emqx/emqx/pull/16021) Fixed issues that occasionally prevented the DS Raft backend from functioning correctly when an existing node joined a new cluster and subsequently became member of DS replica sets.

#### Cluster Linking

- [#15894](https://github.com/emqx/emqx/pull/15894) Previously, when listing all cluster links via `GET /cluster/links`, disabled links would be returned having an `inconsistent` status. Now they are returned as `disconnected`.

#### Performance

- [#15696](https://github.com/emqx/emqx/pull/15696) Added connection rate limiting support for WebSocket (WS) and WebSocket Secure (WSS) listeners.
  The `max_conn_rate` and `max_conn_burst` configuration options are now enforced: incoming connections exceeding the defined rate are immediately closed upon acceptance, consistent with existing TCP listener behavior.

  Additionally, the behavior of `max_connections` has been updated. When the connection limit is exceeded, WS/WSS listeners now close connections immediately before any HTTP handshake, resulting in an abrupt socket close instead of returning an HTTP 429 response.

- [#15854](https://github.com/emqx/emqx/pull/15854) Reduced the default `active_n` value from `100` to `10` to improve MQTT client responsiveness, especially under high message rates with small payloads.

  The lower `active_n` introduces more backpressure at the TCP layer, stricter than the default `Receive-Maximum` of `32`, which helps in the following scenarios:

  - The client process is blocked by external authorization checks
  - Data integration operations are delaying message handling
  - The system is under heavy load or nearing resource limits

- [#15981](https://github.com/emqx/emqx/pull/15981) Prevented excessive memory growth caused by Mnesia transaction blocking during cleanup of large volumes of audit logs. This improves system stability and memory efficiency during heavy audit log maintenance operations.
