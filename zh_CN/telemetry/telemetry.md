# 遥测

EMQ 通过遥测收集有关 EMQX 使用情况的信息，这种功能旨在为我们提供有关用户、社区的综合信息，以及对 EMQX 使用方式的了解。与我们共享这些指标可以帮助我们更好地了解您如何使用我们的产品，并可以持续地帮助我们改进产品。

需要向您说明的是，遥测数据不能也不会用于识别或关联到您本人。即这些统计信息不包含可以标识个体的数据，如服务器型号、硬件编号、IP 地址。同时，统计数据数据永远不会与其他任何人共享。

## 禁用遥测

遥测**默认是启用的**，有些人可能对收集这样的数据感到不舒服，你在启动前、运行中都可以通过最简单的方式禁用它。

### 启动前禁用

您可以打开配置文件 `etc/emqx.conf`，通过添加以下配置项永久性的禁用遥测：

```
telemetry {
 enable = false
}
```

或者启动时通过环境变量在本次启动中禁用遥测：

```bash
export EMQX_TELEMETRY__ENABLE=false && ./bin/emqx start
```

### 运行中禁用

在 Dashboard 系统设置 -> 设置 页面，关闭“启动遥测”选项即可永久性禁用遥测。

## 指标收集

我们收集的遥测数据主要包括：

1. 集群相关硬件信息（仅包括硬件规格，不包含硬件唯一编码）
2. 集群相关拓扑信息
3. 集群相关软件版本信息
4. 集群配置信息(仅包括配置项，不包含配置值)
5. 集群节点的组件使用信息
6. 集群节点的插件使用情况
7. 集群使用指标信息

遥测通过加密 HTTP 请求将数据上报至 [https://telemetry.emqx.io/api/telemetry](https://telemetry.emqx.io/api/telemetry)，数据将安全存储在我们的私有环境中，不会暴露在任何共有网络环境或者云端。

遥测数据收集代码请参考 [apps/emqx_modules/src/emqx_telemetry.erl](https://github.com/emqx/emqx/blob/master/apps/emqx_modules/src/emqx_telemetry.erl)，如果您有任何疑问请通过 [Issues](http://github.com/emqx/emqx/issues)联系我们：

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
