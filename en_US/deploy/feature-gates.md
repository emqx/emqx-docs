# Feature Gates

Starting in EMQX 6.3.0, feature gates are deployment-time controls that enable or disable optional EMQX features. They are resolved only when EMQX starts and cannot be changed at runtime.

Feature gates are configured by the `EMQX_FEATURES` environment variable and are designed for deployment policy. They are not stored in HOCON configuration files, are not persisted to `cluster.hocon`, and cannot be changed from the Dashboard, REST API, or CLI. To change the enabled feature set, update `EMQX_FEATURES` in the deployment environment and restart EMQX.

## Configure Feature Gates

Set `EMQX_FEATURES` to one of the following values:

| Value | Description |
| --- | --- |
| Unset or empty | Uses the `FULL` preset. This preserves the default EMQX behavior. |
| `FULL` | Enables all optional features. |
| `ESSENTIAL` | Starts EMQX with only the core applications. All optional features are disabled. |
| Custom feature list | Enables the listed features and their dependencies. Use lowercase feature names separated by commas, spaces, or both. |

Examples:

```bash
export EMQX_FEATURES=FULL
export EMQX_FEATURES=ESSENTIAL
export EMQX_FEATURES=dashboard,plugins
export EMQX_FEATURES="dashboard plugins"
export EMQX_FEATURES=dashboard,data_integration,metrics,plugins
```

`dashboard,plugins` and `"dashboard plugins"` have the same effect. If you use spaces to separate feature names, wrap the whole value in quotes to prevent the shell from splitting it.

Do not mix a preset with feature names. For example, `EMQX_FEATURES=ESSENTIAL,metrics` is invalid.

::: warning
Invalid values prevent EMQX from starting. If a feature name is unknown, EMQX logs `invalid_feature_specification` with `reason` set to `unknown_feature` and exits with a non-zero status.
:::

## Available Features

The following optional features can be used in a custom `EMQX_FEATURES` list:

| Feature | Description |
| --- | --- |
| `dashboard` | Dashboard UI, REST API, Dashboard role-based access control, and Dashboard single sign-on. |
| `data_integration` | Rule engine, connectors, actions, sources, and data bridges. |
| `message_transformation` | Message transformation. |
| `schema_validation` | Schema validation. |
| `schema_registry` | Schema registry and schema definitions used by related features. |
| `gateways` | Non-MQTT protocol gateways. |
| `cluster_link` | Cluster linking. |
| `multi_tenancy` | Multi-tenancy and namespace management. |
| `ai` | AI features, including AI completion and Agent-to-Agent registry. |
| `metrics` | Prometheus metrics export. |
| `mqtt_extensions` | MQTT extensions such as delayed publish, topic rewrite, topic metrics, auto subscription, slow subscribers, MQTT Streams, and Message Queue. |
| `file_transfer` | File Transfer over MQTT. |
| `gcp_device` | Migration compatibility shim for Google IoT Core. |
| `exhook` | External gRPC hooks. |
| `opentelemetry` | OpenTelemetry exporter. |
| `plugins` | Plugin framework for installing and managing third-party plugins. |

## Feature Dependencies

Some features require other features to work. When you enable a feature, EMQX automatically enables its dependencies.

| Feature | Automatically Enabled Dependencies |
| --- | --- |
| `data_integration` | `schema_registry` |
| `message_transformation` | `schema_registry` |
| `schema_validation` | `schema_registry` |
| `ai` | `schema_registry` |
| `metrics` | `dashboard` |
| `opentelemetry` | `dashboard` |

For example, setting `EMQX_FEATURES=metrics` enables `metrics` and `dashboard`.

## Core Applications

The `ESSENTIAL` preset disables optional features, but EMQX still starts the core applications required for broker operation and management. Authentication and authorization are core capabilities and remain available with every `EMQX_FEATURES` setting. Other core applications include the MQTT broker, configuration system, CLI, license validation, durable storage, audit log, node rebalance, retainer, TLS PSK, outbound telemetry, and shared resource or bridge framework applications.

Existing feature-specific configuration sections can remain in configuration files when the corresponding feature gate is disabled. EMQX does not start the applications behind disabled features, so those configuration sections are not used until the feature is enabled again.

## Code Loading and Memory Use

When `EMQX_FEATURES=ESSENTIAL`, EMQX defaults `CODE_LOADING_MODE` to `interactive`. In interactive mode, Erlang modules are loaded on demand instead of all at startup. As a result, modules that belong to disabled features can remain unloaded, which reduces the resident memory footprint of an `ESSENTIAL` node.

To use eager code loading with the `ESSENTIAL` preset, set `CODE_LOADING_MODE=embedded` explicitly before starting EMQX:

```bash
export EMQX_FEATURES=ESSENTIAL
export CODE_LOADING_MODE=embedded
```

## Inspect Enabled Features

EMQX logs the resolved feature state during startup:

```text
feature_gates_resolved
```

The log entry includes the resolved `preset`, `enabled` feature list, and `disabled` feature list.

When the `dashboard` feature is enabled, you can also query the resolved state through the REST API:

```bash
curl -u <API_KEY>:<SECRET_KEY> http://localhost:18083/api/v5/features
```

Example response:

```json
{
  "preset": "custom",
  "enabled": ["dashboard", "metrics"],
  "disabled": ["ai", "cluster_link", "data_integration", "plugins"]
}
```

The exact lists depend on the EMQX edition, version, and configured feature set.

::: tip
`GET /api/v5/features` is served by the Dashboard and management API applications. If the `dashboard` feature is disabled, inspect the resolved feature set from the startup log instead.
:::

## Cluster Deployment Considerations

For operational consistency, configure the same `EMQX_FEATURES` value on all nodes in a cluster. A mixed-feature cluster can expose different REST APIs, background applications, and cross-node behavior on different nodes.

When you use Docker Compose, Kubernetes, or another orchestration system, set `EMQX_FEATURES` in the shared deployment manifest so added and restarted nodes receive the same value.

## Related Links

- [Install EMQX Using Docker](./install-docker.md)
- [Deploy EMQX on Kubernetes Using Helm Chart](./kubernetes/chart.md)
- [Configuration Files](../configuration/configuration.md)
- [REST API](../admin/api.md)
