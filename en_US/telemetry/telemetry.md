# Telemetry

EMQ collects information about the usage of EMQX through telemetry. This function is designed to provide us with comprehensive information about users and communities, as well as an understanding of how EMQX is used. Sharing these metrics with us can help us better understand how you use our products and can continuously help us improve our products.

It is important to note that telemetry data cannot and will not be used to identify or associate you personally. These statistics do not contain individual data, such as server model, hardware number, IP address, and will never be shared with anyone else.

## Disabling Telemetry

Telemetry is enabled by default. Some people may feel uncomfortable collecting such data. You can disable it in the simplest way before startup and during operation.

### Disable before starting EMQX

You can permanently disable telemetry through the configuration file `etc/emqx.conf`.

```hocon
telemetry {
 enable = false
}
```

Or to disable telemetry in this boot via environment variables at startup: 

```bash
export EMQX_TELEMETRY__ENABLE=false && ./bin/emqx start
```

### Disable while running EMQX

From Dashboard: Open the System -> Settings page and turn off the "Enable telemetry" option to permanently disable telemetry.

## Telemetry Metrics

The telemetry data we collect includes:

1. hardware specifications of the cluster, not including hardware UUID
2. cluster topology
3. EMQX version
4. some configuration enable state
5. feature enablement
6. plug-in usage
7. metrics

Telemetry reports data to by encrypting HTTP requests [https://telemetry.emqx.io/api/telemetry](https://telemetry.emqx.io/api/telemetry)y. The data is stored securely in our private environment and is not exposed to any public network.

For telemetry data collection codes, please refer to [apps/emqx_modules/src/emqx_telemetry.erl](https://github.com/emqx/emqx/blob/master/apps/emqx_modules/src/emqx_telemetry.erl). If you have any questions, please click [Issues](http://github.com/emqx/emqx/issues) to contact us.

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
