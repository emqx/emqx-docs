# Telemetry

EMQ collects usage data for EMQX through its telemetry feature. This functionality helps us gain insights into how our user community interacts with EMQX, enabling us to understand usage patterns and continuously enhance our products. By sharing these metrics, you contribute to improving EMQX's performance and features.

We prioritize your privacy. Telemetry data is anonymized and does not include personal or identifiable information, such as server models, hardware IDs, or IP addresses. This data is never shared with third parties.

Telemetry is disabled by default for users with a Commercial License and enabled by default for the following license types:

- EMQX Community
- Education or Non-Profit
- Trial

These defaults can be overridden by configuring the `telemetry.enable` flag in the EMQX configuration file. For example, you can permanently disable the Telemetry through the configuration file:

```bash
telemetry.enable = false
```

Alternatively, you can control telemetry behavior at startup using an environment variable:

```bash
export EMQX_TELEMETRY__ENABLE=false && ./bin/emqx foreground
```

By customizing these settings, you have full control over whether telemetry data is collected.

## Telemetry Metrics

We collect Telemetry data that includes the following:

- Hardware specifications of the cluster (excluding hardware UUIDs)
- Cluster topology
- EMQX version
- The enabled state of certain configurations
- Feature enablement
- Plug-in usage
- Metrics

Telemetry data is reported through encrypted HTTP requests to `https://telemetry.emqx.io/api/telemetry`. The data is stored securely in our private environment and is not exposed to any public network.

For Telemetry data collection codes, please refer to [apps/emqx_telemetry/src/emqx_telemetry.erl](https://github.com/emqx/emqx/blob/master/apps/emqx_telemetry/src/emqx_telemetry.erl). If you have any questions, please click [Issues](http://github.com/emqx/emqx/issues) to contact us.

The data transferred example is as follows：

```json
{
  "emqx_version": "5.0.9",
  "license": {
    "edition": "opensource"
  },
  "os_name": "macOS",
  "os_version": "12.5",
  "otp_version": "24",
  "up_time": 181903,
  "uuid": "5EAAF3C2-6186-11ED-AD7C-D5AAB80CED2E",
  "cluster_uuid": "5EAAF818-6186-11ED-AC1D-3DFDC18ED1BB",
  "nodes_uuid": [],
  "active_plugins": [],
  "num_clients": 0,
  "messages_received": 0,
  "messages_sent": 0,
  "build_info": {
    "wordsize": 64,
    "relform": "tgz",
    "os": "macos11",
    "erlang": "24.2.1-1",
    "elixir": "none",
    "arch": "x86_64-apple-darwin20.6.0"
  },
  "vm_specs": {
    "num_cpus": 8,
    "total_memory": 8589934592
  },
  "mqtt_runtime_insights": {
    "num_topics": 0,
    "messages_sent_rate": 0,
    "messages_received_rate": 0
  },
  "advanced_mqtt_features": {
    "topic_rewrite": 0,
    "retained": 3,
    "delayed": 0,
    "auto_subscribe": 0
  },
  "authn_authz": {
    "authz": [
      "file"
    ],
    "authn_listener": {},
    "authn": []
  },
  "gateway": {},
  "rule_engine": {
    "num_rules": 1
  },
  "bridge": {
    "num_data_bridges": 1,
    "data_bridge": {
      "webhook": {
        "num_linked_by_rules": 1,
        "num": 1
      }
    }
  },
  "exhook": {
    "servers": [],
    "num_servers": 0
  }
}
```
