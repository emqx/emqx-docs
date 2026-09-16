# Mria 日志与告警

本文档描述 Mria 数据库管理系统上报的日志消息与告警。目前涵盖网络分区事件，后续版本将补充更多错误类型。

## 网络分区

### 分区检测

当检测到网络分区时，所有节点（Core 或 Replicant）的日志中会出现以下消息：

```text
[error] ** Node 'emqx@remote.host' not responding **, ** Removing (timedout) connection **
...
[notice] msg: Remote RLOG agent died, reason: noconnection, repl_state: ...
```

### 分区恢复

当分区恢复后，所有 Core 节点会打印以下日志，表示 EMQX 检测到之前失联的对端节点重新连接：

```text
[error] Mnesia('emqx@local.host'): ** ERROR ** mnesia_event got {inconsistent_database, running_partitioned_network, 'emqx@remote.host'}

[critical] msg: Core cluster partition, context: running_partitioned_network, from: 'emqx@remote.host'
```

同时，系统会触发 `partition` 告警：

```text
[warning] msg: alarm_is_activated, message: <<"Partition occurs at node emqx@remote.host">>, name: partition
```

### Core 节点恢复

少数派分区中的 Core 节点会打印以下日志：

```text
[notice] msg: Mria is restarting to join the cluster, seed: 'emqx@remote.node'
[warning] msg: Stopping mria, reason: heal
[notice] msg: stopping_emqx_apps, ...
```

少数派节点重启完成后，会打印标准的 EMQX 启动消息：

```text
...
Listener tcp:default on 0.0.0.0:1883 started.
Listener ssl:default on 0.0.0.0:8883 started.
Listener ws:default on 0.0.0.0:8083 started.
Listener wss:default on 0.0.0.0:8084 started.
```

### Replicant 节点恢复

Replicant 节点的以下日志表示数据同步已完全恢复：

```text
[notice] msg: Shard fully up, node: 'emqx@remote.host', shard: ...
```

### Broker Heal 告警

分区恢复的另一个标志是所有节点上触发的 `broker_heal` 告警：

```text
[warning] msg: broker_heal_initiated, pid: <0.8705.0>, results: ...
```

该告警会自动清除：

```text
[warning] msg: alarm_is_deactivated, pid: <0.4506.0>, name: broker_heal
```
