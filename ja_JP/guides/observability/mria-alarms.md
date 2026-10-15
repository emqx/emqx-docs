# Mria ログとアラーム

本書は Mria データベース管理システムによって報告されるログメッセージとアラームについて説明します。現在はネットワークパーティションイベントを対象としており、今後の改訂で追加のエラータイプが含まれる可能性があります。

## ネットワークパーティション

### パーティション検出

ネットワークパーティションが検出されると、すべてのノード（Core または Replicant）で以下のログメッセージが表示されます。

```text
[error] ** Node 'emqx@remote.host' not responding **, ** Removing (timedout) connection **
...
[notice] msg: Remote RLOG agent died, reason: noconnection, repl_state: ...
```

### パーティション回復

パーティションが回復すると、EMQX が以前に切断されたピアの再接続を検知し、すべての Core ノードで以下のログが表示されます。

```text
[error] Mnesia('emqx@local.host'): ** ERROR ** mnesia_event got {inconsistent_database, running_partitioned_network, 'emqx@remote.host'}

[critical] msg: Core cluster partition, context: running_partitioned_network, from: 'emqx@remote.host'
```

また、`partition` アラームが発生します。

```text
[warning] msg: alarm_is_activated, message: <<"Partition occurs at node emqx@remote.host">>, name: partition
```

### Core ノードのリカバリー

マイノリティパーティションにある Core ノードでは、以下のログが表示されます。

```text
[notice] msg: Mria is restarting to join the cluster, seed: 'emqx@remote.node'
[warning] msg: Stopping mria, reason: heal
[notice] msg: stopping_emqx_apps, ...
```

マイノリティの再起動が完了すると、再起動した Core ノードは標準の EMQX の起動メッセージを出力します。

```text
...
Listener tcp:default on 0.0.0.0:1883 started.
Listener ssl:default on 0.0.0.0:8883 started.
Listener ws:default on 0.0.0.0:8083 started.
Listener wss:default on 0.0.0.0:8084 started。
```

### Replicant ノードのリカバリー

Replicant ノードでは、レプリケーションが完全に再開されたことを示す以下のログが表示されます。

```text
[notice] msg: Shard fully up, node: 'emqx@remote.host', shard: ...
```

### ブローカー回復アラーム

パーティション回復のもう一つの指標として、すべてのノードで `broker_heal` アラームが発生します。

```text
[warning] msg: broker_heal_initiated, pid: <0.8705.0>, results: ...
```

このアラームは自動的に解除されます。

```text
[warning] msg: alarm_is_deactivated, pid: <0.4506.0>, name: broker_heal
```
