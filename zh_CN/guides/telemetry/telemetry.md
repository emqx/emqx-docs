# 遥测

EMQ 通过其遥测功能收集 EMQX 的使用数据。此功能帮助我们了解用户社区如何与 EMQX 互动，使我们能够了解使用模式并不断提升我们的产品。通过共享这些指标，您有助于改善 EMQX 的性能和功能。

我们优先考虑您的隐私。遥测数据会被匿名化，并且不包括个人信息或可识别信息，如服务器型号、硬件 ID 或 IP 地址。这些数据永远不会与第三方共享。

对于持有商业 License 的用户，遥测功能默认禁用；对于以下 License 类型，遥测功能默认启用：

- EMQX 社区版
- 教育用途或非盈利
- 试用

这些默认设置可以通过在 EMQX 配置文件中配置 `telemetry.enable` 标志来覆盖。通过添加以下配置项永久性的禁用遥测：

```bash
telemetry.enable = false
```

或者启动时通过环境变量在本次启动中禁用遥测：

```bash
export EMQX_TELEMETRY__ENABLE=false && ./bin/emqx foreground
```

通过自定义这些设置，您可以控制是否收集遥测数据。

## 指标收集

我们收集的遥测数据主要包括：

- 集群相关硬件信息（仅包括硬件规格，不包含硬件唯一编码）
- 集群相关拓扑信息
- 集群相关软件版本信息
- 集群配置信息(仅包括配置项，不包含配置值)
- 集群节点的组件使用信息
- 集群节点的插件使用情况
- 集群使用指标信息

遥测通过加密 HTTP 请求将数据上报至 `https://telemetry.emqx.io/api/telemetry`，数据将安全存储在我们的私有环境中，不会暴露在任何共有网络环境或者云端。

遥测数据收集代码请参考 [apps/emqx_telemetry/src/emqx_telemetry.erl](https://github.com/emqx/emqx/blob/master/apps/emqx_telemetry/src/emqx_telemetry.erl)，如果您有任何疑问请通过 [Issues](http://github.com/emqx/emqx/issues)联系我们：

传输的数据示例如下：

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
