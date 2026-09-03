# Flapping Detect Configuration

Flapping detection identifies excessive connection attempts to EMQX. Starting from EMQX 6.3.0, you can configure independent policies for client IDs, usernames, and source IP addresses.

## Detection Dimensions

The `flapping_detect` configuration contains the following dimensions. Each dimension is disabled by default.

| Configuration | Detection Key | Behavior |
| --- | --- | --- |
| `by_clientid` | Client ID | Counts connection attempts from the same client ID. |
| `by_username` | Username | Counts connection attempts from clients that share the same username. Connections without a username are not counted. |
| `by_peerhost` | Source IP address | Counts connection attempts from the same source IP address. |

A dimension is enabled when detection settings are configured for it. Set the dimension to `none` to disable it. You can enable any combination of the three dimensions, and each enabled dimension is counted independently.

The following detection settings are available for each dimension:

| Field | Description | Default |
| --- | --- | --- |
| `window_time` | Time window in which EMQX counts connection attempts. | `1m` |
| `max_count` | Number of connection attempts that triggers a ban within `window_time`. | `15` |
| `ban_time` | Duration for which EMQX bans the matching client ID, username, or source IP address. | `5m` |

When a client ID, username, or source IP address reaches its threshold, EMQX creates a temporary ban entry for that identifier or address. New matching connection attempts are rejected before authentication, while existing connections remain connected. Ban entries expire automatically and can be inspected or removed through the `/banned` REST API.

Detection counters are maintained separately on each node. EMQX does not combine connection attempts handled by different nodes. After a node detects flapping, the resulting ban entry is replicated across the cluster.

## Configuration Example

The following HOCON example configures different policies for client IDs and source IP addresses and disables detection by username:

```hocon
flapping_detect {
  by_clientid {
    window_time = 1m
    max_count = 15
    ban_time = 5m
  }

  by_username = none

  by_peerhost {
    window_time = 30s
    max_count = 100
    ban_time = 10m
  }
}
```

You can also configure these settings for individual zones. A partial dimension policy in a zone inherits unspecified fields from the corresponding global dimension policy.

## Compatibility with Configurations Created Before EMQX 6.3.0

Starting from EMQX 6.3.0, the following flat fields are deprecated:

- `flapping_detect.enable`
- `flapping_detect.window_time`
- `flapping_detect.max_count`
- `flapping_detect.ban_time`

EMQX continues to accept these fields for backward compatibility. When `enable = true`, EMQX maps the flat policy fields to `by_clientid`. When `enable = false`, or when flat policy fields are provided without an explicit `enable = true`, EMQX sets `by_clientid` to `none`.

The deprecated fields affect only client ID detection. `by_username` and `by_peerhost` remain `none` unless you explicitly configure them.

If a configuration contains both `by_clientid` and deprecated flat fields, `by_clientid` takes precedence, including when it is set to `none`.

EMQX provides additional configuration options for advanced customization. For details, see the [EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

You can also configure flapping detection in the EMQX Dashboard by clicking **Access Control** -> **Flapping Detect**. For the Dashboard procedure, see [Flapping Detect](../access-control/flapping-detect.md).
