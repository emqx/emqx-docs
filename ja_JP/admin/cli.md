# コマンドラインインターフェース

このページでは、EMQXがサポートする起動および管理コマンドの種類を紹介し、`ctl`管理コマンドについて詳細に説明します。

## 起動コマンド

EMQXは基本的な起動および管理コマンドをサポートしており、`emqx <command>`コマンドで実行できます。

よく使われる起動および管理コマンドは以下の通りです。

| コマンド    | 説明                                                         |
| ---------- | ------------------------------------------------------------ |
| start      | EMQXをデーモンモードで起動します。実行時に対話型シェルは不要です。 |
| console    | EMQXをErlangまたはElixirの対話型シェルで起動します。開発環境でのデバッグに使用し、EMQXとの対話が必要です。 |
| foreground | EMQXをフォアグラウンドモードで起動します。対話型シェルは使用しません。開発環境でバックグラウンド実行せずに起動する際に使用します。 |
| stop       | 実行中のEMQXノードを停止します。                           |
| ctl        | EMQXの管理および監視を行います。`emqx ctl help`で詳細情報を取得できます。 |

以下は開発やデバッグ向けの高度なコマンドで、通常のユーザーはあまり気にする必要はありません。

| コマンド        | 説明                                                         |
| -------------- | ------------------------------------------------------------ |
| remote_console | リモートのEMQXノードの対話型シェルに接続します。             |
| attach         | 実行中のEMQXノードにアタッチして対話操作を行います。         |
| ertspath       | EMQXのErlangライブラリのパスを取得します。                   |
| root_dir       | EMQXのルートディレクトリのパスを取得します。                 |
| pid            | 実行中のEMQXノードのプロセスIDを取得します。                 |
| ping           | EMQXノードが稼働しているか確認します。                       |
| check_config   | EMQXの設定ファイルが正しいか検証します。                     |
| console_clean  | 対話型シェルのコンソール出力をクリアします。                 |
| escript        | EMQXノード上でEscriptスクリプトを実行します。                |

## ctlコマンド

EMQXの`ctl`コマンドは、EMQXの管理および監視のための複数のサブコマンドを提供します。`ctl`コマンドはEMQXサービス起動後に実行する必要があります。

> EMQXは`emqx_ctl`コマンドも提供しており、これは`emqx ctl`のエイリアスです。  
> `ctl`コマンドは隠しErlangノードを起動して指定ノードにリモート接続し、Erlangのリモートコールを実行して結果を表示します。したがって、`ctl`コマンドの過剰な使用は避けることを推奨します。

以下に`ctl`コマンドの全サブコマンドと簡単な説明を示します。詳細なパラメータ情報は`help`コマンドで確認してください。

## status

ブローカーが稼働しているかどうかを簡単に確認するコマンドです。

```bash
$ emqx ctl status
Node 'emqx@127.0.0.1' 5.8.7 is started
```

## broker

ローカルブローカーの稼働状況、統計情報、メトリクスを確認するコマンドです。

```bash
$ emqx ctl broker
sysdescr  : EMQX Enterprise
version   : 5.8.7
datetime  : 2025-08-06T10:01:23.857678490+02:00
uptime    : 52 seconds
```

### broker stats

ローカルブローカーの統計情報を表示します。接続数、セッション数、サブスクリプション数、トピック数などのメトリクスを含みます。

```bash
$ emqx ctl broker stats
channels.count                : 0
channels.max                  : 0
cluster_sessions.count        : 0
cluster_sessions.max          : 0
connections.count             : 0
connections.max               : 0
delayed.count                 : 0
delayed.max                   : 0
durable_subscriptions.count   : 0
durable_subscriptions.max     : 0
live_connections.count        : 0
live_connections.max          : 0
retained.count                : 3
retained.max                  : 3
sessions.count                : 0
sessions.max                  : 0
suboptions.count              : 0
suboptions.max                : 0
subscribers.count             : 0
subscribers.max               : 0
subscriptions.count           : 0
subscriptions.max             : 0
subscriptions.shared.count    : 0
subscriptions.shared.max      : 0
topics.count                  : 0
topics.max                    : 0
```

### broker metrics

ローカルブローカーのメトリクスを表示します。認証、認可、メッセージ配信、パケット処理、オーバーロード保護などのメトリクスを含みます。

```bash
$ emqx ctl broker metrics
authentication.failure        : 0
authentication.success        : 0
authentication.success.anonymo: 0
authorization.allow           : 0
authorization.cache_hit       : 0
authorization.cache_miss      : 0
authorization.deny            : 0
authorization.matched.allow   : 0
authorization.matched.deny    : 0
authorization.nomatch         : 0
authorization.superuser       : 0
bytes.received                : 0
bytes.sent                    : 0
client.auth.anonymous         : 0
client.authenticate           : 0
client.authorize              : 0
client.connack                : 0
client.connect                : 0
client.connected              : 0
client.disconnected           : 0
client.subscribe              : 0
client.unsubscribe            : 0
delivery.dropped              : 0
delivery.dropped.expired      : 0
delivery.dropped.no_local     : 0
delivery.dropped.qos0_msg     : 0
delivery.dropped.queue_full   : 0
delivery.dropped.too_large    : 0
messages.acked                : 0
messages.delayed              : 0
messages.delivered            : 0
messages.dropped              : 0
messages.dropped.await_pubrel_: 0
messages.dropped.no_subscriber: 0
messages.forward              : 0
messages.persisted            : 0
messages.publish              : 0
messages.qos0.received        : 0
messages.qos0.sent            : 0
messages.qos1.received        : 0
messages.qos1.sent            : 0
messages.qos2.received        : 0
messages.qos2.sent            : 0
messages.received             : 0
messages.sent                 : 0
messages.transformation_failed: 0
messages.transformation_succee: 0
messages.validation_failed    : 0
messages.validation_succeeded : 0
overload_protection.delay.ok  : 0
overload_protection.delay.time: 0
overload_protection.gc        : 0
overload_protection.hibernatio: 0
overload_protection.new_conn  : 0
packets.auth.received         : 0
packets.auth.sent             : 0
packets.connack.auth_error    : 0
packets.connack.error         : 0
packets.connack.sent          : 0
packets.connect.received      : 0
packets.disconnect.received   : 0
packets.disconnect.sent       : 0
packets.pingreq.received      : 0
packets.pingresp.sent         : 0
packets.puback.inuse          : 0
packets.puback.missed         : 0
packets.puback.received       : 0
packets.puback.sent           : 0
packets.pubcomp.inuse         : 0
packets.pubcomp.missed        : 0
packets.pubcomp.received      : 0
packets.pubcomp.sent          : 0
packets.publish.auth_error    : 0
packets.publish.dropped       : 0
packets.publish.error         : 0
packets.publish.inuse         : 0
packets.publish.received      : 0
packets.publish.sent          : 0
packets.pubrec.inuse          : 0
packets.pubrec.missed         : 0
packets.pubrec.received       : 0
packets.pubrec.sent           : 0
packets.pubrel.missed         : 0
packets.pubrel.received       : 0
packets.pubrel.sent           : 0
packets.received              : 0
packets.sent                  : 0
packets.suback.sent           : 0
packets.subscribe.auth_error  : 0
packets.subscribe.error       : 0
packets.subscribe.received    : 0
packets.unsuback.sent         : 0
packets.unsubscribe.error     : 0
packets.unsubscribe.received  : 0
session.created               : 0
session.discarded             : 0
session.resumed               : 0
session.takenover             : 0
session.terminated            : 0
```

## cluster

ノードのクラスター状態を表示および管理するコマンドです。

EMQXの`join`コマンドは、指定したノードに対してクラスター参加の「リクエスト」を送るものであり、「招待」ではありません。つまり、`emqx ctl cluster join <OneOfTheClusteredNodes>`は、`<OneOfTheClusteredNodes>`のクラスターに参加するようリクエストを送るコマンドです。

### cluster join \<Node\>

指定したノードが所属するEMQXクラスターにノードを参加させます。

指定ノードが稼働中でアクセス可能であることを確認してください。

```bash
$ emqx ctl cluster join emqx2@127.0.0.1
Failed to join the cluster: {node_down,'emqx2@127.0.0.1'}
```

### cluster leave

現在のEMQXクラスターからノードを離脱させます。

```bash
$ emqx ctl cluster leave
Failed to leave the cluster: node_not_in_cluster
```

### cluster force-leave \<Node\>

指定したノードをクラスターから強制的に削除します。

::: tip 注意

この操作はクラスター状態の不整合を引き起こす可能性があるため、慎重に使用してください。

:::

```bash
$ emqx ctl cluster force-leave emqx2@127.0.0.1
Failed to remove the node from cluster: node_not_in_cluster
```

### cluster status [--json]

EMQXクラスターの状態を表示します。

オプションの`--json`を指定するとJSON形式で表示されます。

```bash
$ emqx ctl cluster status
Cluster status: #{running_nodes => ['emqx@127.0.0.1'],stopped_nodes => []}
```

```bash
$ emqx ctl cluster status --json
{
  "stopped_nodes" : [

  ],
  "running_nodes" : [
    "emqx@127.0.0.1"
  ]
}
```

### cluster discovery enable

自動クラスター検出を有効化し起動します（設定されている場合）。

```bash
$ emqx ctl cluster discovery enable
Automatic cluster discovery enabled.
```

## clients

接続中のクライアントを表示および管理するコマンドです。

### clients list

現在EMQXに接続中のすべてのクライアントを表示します。アクティブなクライアント数や接続数の監視に利用できます。

:::tip
大量のクライアントが接続している場合、`list`コマンドは時間がかかりリソースを多く消費する可能性があります。
:::

```bash
$ emqx ctl clients list
Client(emqx_c, username=undefined, peername=127.0.0.1:59441, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4530, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736435155, connected_at=1684736435155)
Client(emqx_a, username=undefined, peername=127.0.0.1:59444, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4588, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736441613, connected_at=1684736441613)
```

### clients show \<ClientId\>

特定クライアントの詳細な接続情報を表示します。

```bash
$ emqx ctl clients show emqx_c
Client(emqx_c, username=undefined, peername=127.0.0.1:59441, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4680, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736435155, connected_at=1684736435155)
```

### clients kick \<ClientId\>

指定したクライアントを強制切断します。

```bash
$ emqx ctl clients kick emqx_c
ok
```

### clients stats --file <path/to/file.csv>

クライアントごとの統計情報をCSVファイルに出力します。システム管理者がクライアントの活動状況を観察し、負荷の高いクライアントを特定するのに役立ちます。

```bash
$ emqx ctl clients stats path/to/file.csv
```

**引数:**

- 出力するCSVファイルのパス
- `--batch`オプションは一度に処理するクライアント数を制御します。小さい値はリソース消費を抑えますが処理時間が長くなります（デフォルトは`1000`）。
- `--sleep`オプションはバッチ処理間の待機時間（ミリ秒）を制御します。値を大きくするとシステムへの影響をさらに減らせますが、実行時間が長くなります（デフォルトは`10ms`）。

**出力フォーマット:**

生成されるCSVファイルは以下の列を含みます。

```sql
timestamp, clientid, recv_oct, recv_cnt, send_oct, send_cnt, subscriptions_cnt, awaiting_rel_cnt, mqueue_len, mqueue_dropped
```

各フィールドの説明:

- `timestamp`: データ収集時のUNIXタイムスタンプ（ミリ秒）
- `clientid`: MQTTクライアントID
- `recv_oct`: クライアントから受信した合計バイト数
- `recv_cnt`: 受信したメッセージ（またはメッセージ断片）の数
- `send_oct`: クライアントへ送信した合計バイト数
- `send_cnt`: 送信したMQTTパケット数
- `subscriptions_cnt`: クライアントが保持するサブスクリプション数
- `awaiting_rel_cnt`: PUBRELを待機中のQoS 2メッセージ数
- `mqueue_len`: クライアントのメモリ上のセッションメッセージキューの長さ
- `mqueue_dropped`: メモリ上のセッションメッセージキューからドロップされたメッセージ数

**注意:**

- このコマンドはリアルタイムのテレメトリではなく観測用です。
- パフォーマンス低下を避けるため、ETSスキャンを周期的にスリープ（例：1000件ごとに10ms）して制御しています。
- 生成されたCSVはオフライン分析や可視化、自動処理に利用可能です。

## topics

現在のシステムでサブスクライブされているすべてのトピックを表示するコマンドです。

### topics list

すべてのトピックを一覧表示します。トピック数や分布の監視に利用できます。

:::tip 注意

クラスター内に大量のトピックサブスクリプションがある場合、`list`コマンドは時間がかかりリソースを多く消費する可能性があります。
:::

```bash
$ emqx ctl topics list
t/1 -> emqx@127.0.0.1
```

### topics show \<Topic\>

特定のトピックの詳細情報を表示します。

```bash
$ emqx ctl topics show t/1
t/1 -> emqx@127.0.0.1
```

## subscriptions

クライアントのサブスクリプションを表示、追加、削除するコマンドです。

### subscriptions list

すべてのサブスクリプションを一覧表示します。

```bash
$ emqx ctl subscriptions list
emqx_a -> topic:t/1 qos:0 nl:0 rh:0 rap:0
emqx_c -> topic:t/1 qos:0 nl:0 rh:0 rap:0
```

### subscriptions show \<ClientId\>

特定クライアントのサブスクリプションを表示します。

```bash
$ emqx ctl subscriptions show emqx_a
emqx_a -> topic:t/1 qos:0 nl:0 rh:0 rap:0
```

### subscriptions add \<ClientId\> \<Topic\> \<QoS\>

手動でサブスクリプションを追加します。

```bash
$ emqx ctl subscriptions add emqx_a t/1 1
ok
```

### subscriptions del \<ClientId\> \<Topic\>

手動でサブスクリプションを削除します。

```bash
$ emqx ctl subscriptions del emqx_a t/1
ok
```

:::tip
システム内に大量のサブスクリプションがある場合、`list`コマンドは時間がかかりリソースを多く消費する可能性があります。
:::

## plugins

プラグインのインストール状況を表示および管理するコマンドです。

### plugins list

インストール済みプラグインを一覧表示します。

```bash
emqx ctl plugins list
[]
```

### plugins describe \<Name-Vsn\>

インストール済みプラグインの詳細情報を表示します。

```bash
emqx ctl plugins describe emqx_auth_mnesia-3.0.1
```

### plugins allow \<Name-Vsn\>

ダッシュボード経由で指定プラグインのインストールを許可します。

```bash
emqx ctl plugins allow emqx_auth_mnesia-3.0.1
{
  "result" : "ok",
  "name_vsn" : "emqx_auth_mnesia-3.0.1",
  "action" : "do_allow_installation"
}
```

### plugins disallow \<Name-Vsn\>

ダッシュボード経由で指定プラグインのインストールを禁止します。

```bash
emqx ctl plugins disallow emqx_auth_mnesia-3.0.1
{
  "result" : "ok",
  "name_vsn" : "emqx_auth_mnesia-3.0.1",
  "action" : "do_disallow_installation"
}
```

### plugins install \<Name-Vsn\> \[--cluster\]

プラグインインストールディレクトリにあるプラグインパッケージをインストールします。`--cluster`オプションを付けると、クラスター内の全稼働ノードに配布・インストールします。

```bash
emqx ctl plugins install emqx_auth_mnesia-3.0.1
emqx ctl plugins install emqx_auth_mnesia-3.0.1 --cluster
```

### plugins uninstall \<Name-Vsn\>

指定プラグインをアンインストールします。

```bash
emqx ctl plugins uninstall emqx_auth_mnesia-3.0.1
```

### plugins start \<Name-Vsn\>

指定プラグインを起動します。

```bash
emqx ctl plugins start emqx_auth_mnesia-3.0.1
```

### plugins stop \<Name-Vsn\>

指定プラグインを停止します。

```bash
emqx ctl plugins stop emqx_auth_mnesia-3.0.1
```

### plugins restart \<Name-Vsn\>

指定プラグインを再起動します。

```bash
emqx ctl plugins restart emqx_auth_mnesia-3.0.1
```

### plugins disable \<Name-Vsn\>

プラグインの自動起動を無効化します。

```bash
emqx ctl plugins disable emqx_auth_mnesia-3.0.1
```

### plugins enable \<Name-Vsn\> [Position]

プラグインの自動起動を有効化し、起動順序の位置を指定します。

```bash
emqx ctl plugins enable emqx_auth_mnesia-3.0.1 front
```

`front`、`rear`、または`before Other-Vsn`を使って起動順序の相対位置を指定できます。位置を指定しない場合、既存のプラグインの順序は変わらず、新しいプラグインは末尾に追加されます。

## vm

Erlang仮想マシンの統計データを確認します。

### vm all

Erlang VMのCPU負荷、メモリ使用量など全情報を表示します。

```bash
$ emqx ctl vm all
cpu/load1               : 13.16
cpu/load5               : 11.95
cpu/load15              : 9.75
memory/total            : 127648904
memory/processes        : 30427456
memory/processes_used   : 30426744
memory/system           : 97221448
memory/atom             : 2277809
memory/atom_used        : 2259843
memory/binary           : 668072
memory/code             : 48748792
memory/ets              : 10725432
process/limit           : 2097152
process/count           : 626
io/max_fds              : 8192
io/active_fds           : 0
ports/count             : 27
ports/limit             : 1048576
```

### vm load

Erlang VMの1分、5分、15分のCPU負荷平均を表示します。

```bash
$ emqx ctl vm load
cpu/load1               : 0.96
cpu/load5               : 1.03
cpu/load15              : 1.05
```

### vm memory

Erlang VMのメモリ使用状況（合計、プロセス、アトム、バイナリ、ETS）を表示します。

```bash
$ emqx ctl vm memory
memory/total            : 218672189
memory/processes        : 70762184
memory/processes_used   : 70760616
memory/system           : 147910005
memory/atom             : 3080769
memory/atom_used        : 3061022
memory/binary           : 1652808
memory/code             : 67620307
memory/ets              : 17414480
```

### vm process

Erlang VMのプロセス数とプロセス上限を表示します。

```bash
$ emqx ctl vm process
process/limit           : 2097152
process/count           : 870
```

### vm io

Erlang VMのI/O情報（最大ファイルディスクリプタ数、アクティブファイルディスクリプタ数）を表示します。

```bash
$ emqx ctl vm io
io/max_fds              : 1048576
io/active_fds           : 0
```

### vm ports

Erlang VMのポート数とポート上限を表示します。

```bash
$ emqx ctl vm ports
ports/count             : 12
ports/limit             : 1048576
```

## mnesia

組み込みデータベース（Mnesia）の稼働状況とメトリクスを表示します。

```bash
$ emqx ctl mnesia
===> System info in version "4.20.4.1", debug level = none <===
opt_disc. Directory "/Users/emqx/Downloads/emqx-503/data/mnesia/emqx@127.0.0.1" is used.
use fallback at restart = false
running db nodes   = ['emqx@127.0.0.1']
stopped db nodes   = []
master node tables = []
backend types      = null_copies    - mria_mnesia_null_storage
                     rocksdb_copies - mnesia_rocksdb
remote             = []
ram_copies         = [bpapi,emqx_channel_registry,
                      emqx_ee_schema_registry_serde_tab,
                      emqx_exclusive_subscription,
                      emqx_gateway_coap_channel_registry,emqx_retainer_index,
                      emqx_retainer_index_meta,emqx_retainer_message,
                      emqx_route,emqx_routing_node,emqx_shared_subscription,
                      emqx_trie,mria_schema]
disc_copies        = [cluster_rpc_commit,cluster_rpc_mfa,emqx_acl,
                      emqx_activated_alarm,emqx_admin,emqx_admin_jwt,emqx_app,
                      emqx_authn_mnesia,emqx_banned,emqx_dashboard_monitor,
                      emqx_deactivated_alarm,emqx_delayed,
                      emqx_enhanced_authn_scram_mnesia,emqx_psk,
                      emqx_telemetry,emqx_trace,schema]
disc_only_copies   = []
[{'emqx@127.0.0.1',disc_copies}] = [schema,emqx_psk,emqx_delayed,emqx_app,
                                    emqx_admin_jwt,emqx_dashboard_monitor,
                                    emqx_admin,cluster_rpc_mfa,
                                    cluster_rpc_commit,emqx_acl,
                                    emqx_enhanced_authn_scram_mnesia,
                                    emqx_authn_mnesia,emqx_banned,
                                    emqx_activated_alarm,
                                    emqx_deactivated_alarm,emqx_telemetry,
                                    emqx_trace]
[{'emqx@127.0.0.1',ram_copies}] = [mria_schema,emqx_trie,
                                   emqx_shared_subscription,emqx_routing_node,
                                   emqx_route,emqx_exclusive_subscription,
                                   bpapi,emqx_channel_registry,
                                   emqx_retainer_index_meta,
                                   emqx_retainer_message,emqx_retainer_index,
                                   emqx_ee_schema_registry_serde_tab,
                                   emqx_gateway_coap_channel_registry]
414 transactions committed, 32 aborted, 6 restarted, 250 logged to disc
0 held locks, 0 in queue; 0 local transactions, 0 remote
0 transactions waits for other nodes: []
```

## log

ログハンドラーの状態管理（ログレベル設定など）に使用します。

### log set-level \<Level\>

全体のログレベルを設定します。

```bash
$ emqx ctl log set-level debug
debug
```

### log primary-level

現在のプライマリログレベルを表示します。`primary-level`はEMQXの全体のデフォルトログレベルを示し、特定のログハンドラーが独自のレベルを持たない限り、すべてのログ出力に影響します。

```bash
$ emqx ctl log primary-level
debug
```

### log primary-level \<Level\>

プライマリログレベルを設定します。

```bash
$ emqx ctl log primary-level info
info
```

### log handlers list

ログハンドラーの一覧を表示します。ログハンドラーはログの処理方法や保存方法を個別に設定可能です。

```bash
$ emqx ctl log handlers list
LogHandler(id=ssl_handler, level=debug, destination=console, status=started)
LogHandler(id=console, level=debug, destination=console, status=started)
```

### log handlers start \<HandlerId\>

特定のログハンドラーを起動します。

```bash
$ emqx ctl log handlers start console
log handler console started
```

### log handlers stop \<HandlerId\>

特定のログハンドラーを停止します。

```bash
$ emqx ctl log handlers stop console
log handler console stopped
```

### log handlers set-level \<HandlerId\> \<Level\>

特定のログハンドラーのログレベルを設定します。

```bash
$ emqx ctl log handlers set-level console debug
debug
```

## trace

特定のクライアントやトピックなどのイベントをトレース（ログ出力）するコマンドです。

### trace list

ローカルノードで開始されているすべてのトレースを一覧表示します。

```bash
$ emqx ctl trace list
Trace(ip_address=127.0.0.1, level=debug, destination="trace.log")
```

### trace start client \<ClientId\> \<File\> [\<Level\>]

特定クライアントのトレースを開始します。

```bash
$ emqx ctl trace start client emqx_c trace.log debug
trace emqx_c CLI-emqx_c successfully
```

### trace stop client \<ClientId\>

特定クライアントのトレースを停止します。

```bash
$ emqx ctl trace stop client emqx_c
stop tracing clientid emqx_c successfully
```

### trace start topic \<Topic\> \<File\> [\<Level\>]

特定トピックのトレースを開始します。

```bash
$ emqx ctl trace start topic t/1 trace.log info
trace t/1 CLI-t/1 successfully
```

### trace stop topic \<Topic\>

特定トピックのトレースを停止します。

```bash
$ emqx ctl trace stop topic t/1
stop tracing topic t/1 successfully
```

### trace start ip_address \<IP\> \<File\> [\<Level\>]

特定クライアントIPアドレスのトレースを開始します。

```bash
$ emqx ctl trace start ip_address 127.0.0.1 trace.log debug
trace 127.0.0.1 CLI-127.0.0.1 successfully
```

### trace stop ip_address \<IP\>

特定クライアントIPアドレスのトレースを停止します。

```bash
$ emqx ctl trace stop ip_address 127.0.0.1
stop tracing ip_address 127.0.0.1 successfully
```

::: tip
コマンドラインからトレースログファイルを指定する際は絶対パスの使用を推奨します。  
例：`emqx ctl trace start client foobar /abs/path/to/trace.log debug`
:::

::: tip
トレースはダッシュボードUIからも管理可能です。詳細は[Log Trace](../observability/tracer.md)を参照してください。
:::

## traces

`trace`コマンドに似ていますが、クラスター全ノードでトレーサーを開始・停止します。

### traces list

クラスターで開始されているすべてのトレースを一覧表示します。

```bash
$ emqx ctl traces list
Trace(mytraces_ip: ip_address=127.0.0.1, waiting, LogSize:#{'emqx@127.0.0.1' => 0})
```

### traces start \<Name\> client \<ClientId\> [\<Duration\>]

クラスターで特定クライアントのトレースを開始します。

```bash
$ emqx ctl traces start mytraces client emqx_c 1200
cluster_trace clientid emqx_c mytraces successfully
```

### traces start \<Name\> topic \<Topic\> [\<Duration\>]

クラスターで特定トピックのトレースを開始します。

```bash
$ emqx ctl traces start mytraces_ip topic t/1 1200
cluster_trace topic t/1 mytraces_ip successfully
```

### traces start \<Name\> ip_address \<IPAddr\> [\<Duration\>]

クラスターで特定クライアントIPのトレースを開始します。

```bash
$ emqx ctl traces start mytraces_ip ip_address 127.0.0.1 1200
cluster_trace ip_address 127.0.0.1 mytraces_ip successfully
```

### traces stop \<Name\>

クラスターでトレースを停止します。

```bash
$ emqx ctl traces stop mytraces_ip
Stop cluster_trace mytraces_ip successfully
```

### traces delete \<Name\>

クラスターでトレースを削除します。

```bash
$ emqx ctl traces delete mytraces_ip
Del cluster_trace mytraces_ip successfully
```

## listeners

リスナーを管理するコマンドです。

### listeners

すべてのリスナー情報を一覧表示します。

```bash
$ emqx ctl listeners
ssl:default
  listen_on       : 0.0.0.0:8883
  acceptors       : 16
  proxy_protocol  : false
  running         : true
  current_conn    : 0
  max_conns       : 5000000
tcp:default
  listen_on       : 0.0.0.0:1883
  acceptors       : 16
  proxy_protocol  : false
  running         : true
  current_conn    : 12
  max_conns       : 5000000
  shutdown_count  : [{takenover,2},{discarded,1}]
ws:default
  listen_on       : 0.0.0.0:8083
  acceptors       : 16
  proxy_protocol  : false
  running         : true
  current_conn    : 0
  max_conns       : 5000000
wss:default
  listen_on       : 0.0.0.0:8084
  acceptors       : 16
  proxy_protocol  : false
  running         : true
  current_conn    : 0
  max_conns       : 5000000
```

#### 一般的なシャットダウン理由

TCPリスナーでは、`shutdown_count`フィールドがあり、理由別に切断されたクライアント数を記録しています。これにより、なぜクライアントが切断されたかを特定できます。

```bash
shutdown_count  : [{takenover,2},{discarded,1}]
```

上記例では：

- 2件は同じ`clientid`で新しいセッションが既存セッションを引き継いだため切断
- 1件は同じ`clientid`で新しいクリーンセッションが既存セッションを置き換えたため切断

以下は`shutdown_count`に現れる一般的な理由一覧です。

| 理由                         | 説明                                                         |
| ---------------------------- | ------------------------------------------------------------ |
| `banned`                     | ACL違反、レート制限、IP制限によりクライアントがブラックリスト入りした。 |
| `closed`                     | サーバまたはクライアントによって接続が閉じられた。           |
| `discarded`                  | 同じ`clientid`かつ`clean_start=true`の新クライアントが接続し、既存セッションがアクティブだった。 |
| `takenover`                  | 同じ`clientid`かつ`clean_start=false`の新クライアントが接続し、既存セッションがアクティブだった。 |
| `einval`                     | 無効な引数またはソケットエラー。通常、既に閉じられたソケットに書き込みを試みた競合状態。 |
| `frame_too_large`            | MQTTパケットが許容最大フレームサイズを超えた。               |
| `idle_timeout`               | TCP/SSL接続確立後、許容時間内に`CONNECT`パケットが受信されなかった。 |
| `invalid_proto_name`         | `CONNECT`パケットのプロトコル名が無効、または`"MQTT"`でない。 |
| `invalid_topic`              | クライアントが無効なトピックを使用（不正文字含む、ブローカーにより禁止など）。 |
| `keepalive_timeout`          | キープアライブ間隔内にパケット送信がなかった。               |
| `malformed_packet`           | MQTTパケットが破損しているかMQTT仕様に準拠していない。       |
| `not_authorized`             | クライアントが認可されていない操作を試み、ACLにより拒否された。 |
| `ssl_closed`                 | SSL/TLS接続がピアによって閉じられた。                         |
| `ssl_error`                  | SSL/TLSハンドシェイクまたはデータ送受信中にエラーが発生した。 |
| `ssl_upgrade_timeout`        | SSL/TLSハンドシェイクが許容時間内に完了しなかった。           |
| `unexpected_packet`          | 現在の接続状態で予期しないパケットが送信された。             |
| `zero_remaining_len`         | パケットの残り長がゼロであり、多くの文脈で無効。             |
| `bad_username_or_password`   | 認証に失敗（ユーザー名またはパスワード不正）。               |
| `client_identifier_not_valid`| 指定された`clientid`が無効、またはログイン中に他クライアントによりロックされている。 |
| `protocol_error`             | 一般的なMQTTプロトコル違反。                                 |
| `tcp_closed`                 | TCP接続がクライアント側またはネットワーク障害により閉じられた。 |
| `timeout`                    | 一般的なタイムアウト（認証など）。                           |

### listeners stop \<Identifier\>

リスナーを停止します。識別子は`{type}:{name}`形式（例：`tcp:default`）。一時的な効果で、EMQX再起動後に元に戻ります。

```bash
$ emqx ctl listeners stop tcp:default
Stop tcp:default listener successfully.
```

::: tip
リスナー停止により、接続中のすべてのクライアントが切断されます。
:::

### listeners start \<Identifier\>

リスナーを起動します（一時的な効果）。

```bash
$ emqx ctl listeners start tcp:default
Started tcp:default listener successfully.
```

### listeners restart \<Identifier\>

リスナーを再起動します。

```bash
$ emqx ctl listeners restart tcp:default
Restarted tcp:default listener successfully.
```

::: tip
リスナー再起動により、接続中のすべてのクライアントが切断されます。
:::

### listeners enable \<Identifier\> \<true/false\>

リスナーを有効または無効にします。設定に永続化され、恒久的に有効です。

```bash
$ emqx ctl listeners enable tcp:default true
Enabled tcp:default listener successfully.
```

```bash
$ emqx ctl listeners enable tcp:default false
Disabled tcp:default listener successfully.
```

## authz cache-clean

認可（ACL）キャッシュを強制的にクリアしたい場合に使用します。

### authz cache-clean all

すべてのノードの認可キャッシュをクリアします。

```bash
$ emqx ctl authz cache-clean all
Authorization cache drain started on all nodes OK
```

### authz cache-clean node \<Node\>

指定ノードの認可キャッシュをクリアします。

```bash
$ emqx ctl authz cache-clean node emqx@127.0.0.1
Authorization cache drain started on node emqx@127.0.0.1 OK
```

### authz cache-clean \<ClientId\>

指定クライアントの認可キャッシュをクリアします。

```bash
$ emqx ctl authz cache-clean mqttx_9502dc8a
Drain mqttx_9502dc8a authz cache OK
```

## pem_cache

更新されたpem（x509鍵・証明書）ファイルをEMQXに強制再読み込みさせるコマンドです。

### pem_cache clean all

すべてのノードのx509証明書キャッシュをクリアします。

```bash
$ emqx ctl pem_cache clean all
PEM cache clean OK
```

### pem_cache clean node \<Node\>

指定ノードのx509証明書キャッシュをクリアします。

```bash
$ emqx ctl pem_cache clean emqx@127.0.0.1
emqx@127.0.0.1 PEM cache clean OK
```

## olp

OLPはオーバーロード保護の略称です。  
`olp`コマンドはオーバーロード状態の確認や、システムのオーバーロード保護の有効/無効を切り替えます。

詳細は`overload_protection`設定ドキュメントを参照してください。

::: tip
`olp`はデフォルトで有効化されておらず、CLIからの有効化は設定に永続化されません。
:::

### olp status

システムがオーバーロード状態ならその状態を返し、そうでなければ「not overloaded」と報告します。

```bash
$ emqx ctl olp status
'emqx@172.17.0.3' is not overloaded
```

### olp enable

オーバーロード保護を有効化します。

```bash
$ emqx ctl olp enable
Enable overload protection 'emqx@127.0.0.1' : {ok,<0.5703.0>}
```

### olp disable

オーバーロード保護を無効化します。

```bash
$ emqx ctl olp disable
Disable overload protetion 'emqx@127.0.0.1' : ok
```

## data

ノードのデータをtarアーカイブファイルにエクスポート/インポートするコマンドです。

### data export

```bash
data export \
  [--root-keys key1,key2,key3] \
  [--table-sets set1,set2,set3] \
  [--dir out_dir]
```

EMQXノードのデータをtarアーカイブファイルにエクスポートします。バックアップやノード間のデータ転送に便利です。

含まれるデータ：

- クラスター設定
- EMQXデータディレクトリの追加ファイル（SSL証明書など）
- 組み込みデータベース

`--root-keys`や`--table-sets`オプションでエクスポート対象を指定可能。指定しなければすべてエクスポートします。

```bash
emqx ctl data export --root-keys listeners,connectors,actions,rule_engine --dir /tmp
Exporting data to "/tmp/emqx-export-2025-08-06-12-00-19.334.tar.gz"...
Exporting cluster configuration...
Exporting additional files from EMQX data_dir: "data"...
Exporting built-in database...
Exporting emqx_banned_rules database table...
Exporting emqx_banned database table...
Exporting emqx_psk database table...
Exporting emqx_authn_mnesia database table...
Exporting emqx_authn_scram_mnesia database table...
Exporting emqx_acl database table...
Exporting emqx_app database table...
Exporting emqx_mt_config database table...
Exporting emqx_admin database table...
Exporting emqx_retainer_message database table...
Data has been successfully exported to /tmp/emqx-export-2025-08-06-12-00-19.334.tar.gz.
```

### data import \<File\>

指定したtarアーカイブファイルからデータをインポートします。バックアップからの復元や新ノードへのデータ移行に使用します。

```bash
emqx ctl data import /tmp/emqx-export-2025-08-06-12-00-19.334.tar.gz
Importing data from "/tmp/emqx-export-2025-08-06-12-00-19.334.tar.gz"...
Importing cluster configuration for namespace global...
Importing built-in database...
Importing emqx_retainer_message database table...
Starting reindexing retained messages
Reindexed 3 messages
Reindexing retained messages finished
Importing emqx_admin database table...
Importing emqx_mt_config database table...
Importing emqx_app database table...
Importing emqx_acl database table...
Importing emqx_authn_scram_mnesia database table...
Importing emqx_authn_mnesia database table...
Importing emqx_psk database table...
Importing emqx_banned database table...
Importing emqx_banned_rules database table...
Data has been imported successfully.
```

## ds

Durable Storageを操作するコマンドです。

### ds info

組み込みDurable Storageの状態概要を表示します。

```bash
emqx ctl ds info
THIS SITE:
EFC84E67230295E2

SITES:
.------------------.----------------.--------.
: Site             : Node           : Status :
:------------------:----------------:--------:
: EFC84E67230295E2 : emqx@127.0.0.1 :     up :
 ------------------ ---------------- --------

SHARDS:
.----------.----------.-------------.
: DB/Shard : Replicas : Transitions :
:----------:----------:-------------:
 ---------- ---------- -------------
```

### ds set-replicas \<storage\> \<site1\> \<site2\> ...

Durable Storageのレプリカセットを変更します。

### ds join \<storage\> \<site\>

ストレージのレプリカセットにサイトを追加します。

### ds leave \<storage\> \<site\>

ストレージのレプリカセットからサイトを削除します。

### ds forget \<site\>

既知のサイト一覧からサイトを削除します。

## exclusive

現在のシステム内のすべての排他トピックを表示または削除するコマンドです。

### exclusive list

すべての排他トピックを一覧表示します。

```bash
$ emqx ctl exclusive list
t/1 -> client1
```

### exclusive delete \<Topic\>

排他トピックを削除します。

```bash
$ emqx ctl exclusive delete t/1
ok
```

## retainer

retainerコマンドは保持されたメッセージを検査・管理できます。`emqx ctl retainer reindex`コマンドで保持メッセージのインデックス作成・更新も可能です。

### retainer info

保持メッセージ数を表示します。

```bash
$ emqx ctl retainer info
Number of retained messages: 3
```

### retainer topics

保持メッセージがあるすべてのトピックを表示します。

```bash
$ emqx ctl retainer topics
$SYS/brokers
$SYS/brokers/emqx@127.0.0.1/sysdescr
$SYS/brokers/emqx@127.0.0.1/version
```

### retainer clean

すべての保持メッセージをクリアします。

```bash
emqx ctl retainer clean
```

### retainer clean \<Topic\>

特定のトピックフィルターに基づいて保持メッセージをクリアします。

```bash
emqx ctl retainer clean t/1
```

### retainer reindex status

インデックス再作成処理の状態を表示します。

```bash
$ emqx ctl retainer reindex status
Reindexing is not running
```

### retainer reindex start [force]

設定に基づき保持メッセージトピックの新しいインデックスを生成します。`true`を`<force>`パラメータに渡すと、既存の再インデックス処理を無視して強制実行します。

```bash
$ emqx ctl retainer reindex start true
Starting reindexing
Reindexed 0 messages
Reindexing finished
```

## observer

Erlang仮想マシンの状態をリアルタイムに監視する`top`コマンドのような機能を提供します。

### observer status

現在のコンソールでobserverを起動し、EMQXノードの状態や活動を監視・デバッグします。

```bash
$ emqx ctl observer status
```

### observer bin_leak

すべてのプロセスにガベージコレクションを強制実行し、最大のバイナリデータを解放した上位100プロセスを表示します。メモリリークの可能性を検出できます。

```bash
$ emqx ctl observer bin_leak
{<0.2140.0>,-48,
 [{current_function,{logger_std_h,file_ctrl_loop,1}},
  {initial_call,{erlang,apply,2}}]}
{<0.2093.0>,-29,
 [{current_function,{application_master,main_loop,2}},
  {initial_call,{proc_lib,init_p,5}}]}
{<0.2116.0>,-23,
 [user_drv,
  {current_function,{user_drv,server_loop,6}},
  {initial_call,{user_drv,server,2}}]}
...
```

### observer load Mod

EMQXクラスターの全ノードに指定モジュールをロードします。クラスター全体でモジュールを利用可能にする際に使用します。

```bash
$ emqx ctl observer load Mod
Loaded 'Mod' module on []: ok
```

## conf

EMQXクラスター設定の確認および変更に使用します。

### conf reload --replace|--merge

ローカルノードの`etc/emqx.conf`をリロードします。デフォルトは既存設定に新設定をマージします。`--replace`を使うと既存設定を新設定で置き換えます。

### conf show_keys

現在使用中の設定キーをすべて表示します。

### conf show [\<key\>]

指定キー以下の使用中設定（デフォルト値含む）を表示します。キー未指定時は全キーを表示します。

### conf load --replace|--merge \<path\>

HOCON形式の設定ファイルを読み込みます。デフォルトは既存設定にマージ。`--replace`で置換。現在ノードがクラスター全体の設定変更トランザクションを開始し、他ノードに同期します。

注意：ローリングアップグレード中のランタイム設定変更は避けてください。

## conf cluster_sync

クラスター内ノード間の設定同期に問題がある場合のトラブルシューティング用コマンドです。

::: tip

EMQX 5.0.xでは`cluster_call`という名前でした。5.1でも利用可能ですがヘルプには表示されません。

:::

EMQX HTTP APIやダッシュボードからの設定変更は、変更内容をまずローカルの`data/configs/cluster.hocon`に書き込み、データベースに記録し、非同期で他ノードに転送します。

何らかの理由で同期が失敗した場合、このコマンドで状態を確認・修正できます。

EMQXはクラスター内の設定変更に対し、増加するID（`tnx_id`）を生成し、変更を管理しています。

::: tip
`skip`や`fast_forward`コマンドはクラスター内で設定不整合を引き起こす可能性があります。
:::

### conf cluster_sync status

全ノードの設定同期状態を要約表示します。

```bash
$ emqx ctl conf cluster_sync status
-----------------------------------------------
All configuration synchronized(tnx_id=0) successfully
-----------------------------------------------
```

### conf cluster_sync inspect \<tnx_id\>

指定した`tnx_id`の設定変更トランザクションの詳細を表示します。

例：2番目の変更（`tnx_id=2`）でTLSリスナー有効化操作を確認。

```bash
$ emqx ctl conf cluster_sync inspect 2
{atomic,#{created_at => {{2022,6,21},{21,57,50}},
          initiator => 'emqx@127.0.0.1',
          mfa =>
              {emqx,update_config,
                    [[listeners,ssl,default],
                     {action,stop,#{<<"enabled">> => false}},
                     #{override_to => cluster,rawconf_with_defaults => true}]},
          tnx_id => 2}}
```

### conf cluster_sync skip [node]

指定ノードの（現在失敗中の）コミットをインクリメントします。

::: warning 注意

クラスター内の設定不整合を引き起こす可能性があります。

:::

### conf cluster_sync fast_forward [node] \<tnx_id\>

指定ノードの設定変更を指定`tnx_id`まで高速進行させます。

::: warning 注意

クラスター内の設定不整合を引き起こす可能性があります。

:::

### conf cluster_sync fix

最も包括的な設定を持つノード（通常は設定リーダーで最大`tnx_id`）と他ノードを同期します。

## eviction status

現在のノードのエビクション状態を取得します。

```bash
$ emqx ctl eviction
Eviction status: disabled
```

## rebalance

クラスター内の負荷を再分散するコマンドです。高負荷ノードから低負荷ノードへ接続やセッションを移行し、ノード間の負荷バランスを取ります。

### rebalance start --evacuation

現在ノードの退避を開始し、必要に応じて指定サーバーへのリダイレクトを行います。

```
rebalance start --evacuation \
  [--wait-health-check Secs] \
  [--redirect-to "Host1:Port1 Host2:Port2 .."] \
  [--conn-evict-rate CountPerSec] \
  [--migrate-to "node1@host1 node2@host2 .."] \
  [--wait-takeover Secs] \
  [--sess-evict-rate CountPerSec]
```

### rebalance start

現在ノードをコーディネーターとして指定ノードで再分散を開始します。

```
rebalance start \
  [--nodes "node1@host1 node2@host2 .."] \
  [--wait-health-check Secs] \
  [--conn-evict-rate ConnPerSec] \
  [--abs-conn-threshold Count] \
  [--rel-conn-threshold Fraction] \
  [--wait-takeover Secs] \
  [--sess-evict-rate CountPerSec] \
  [--abs-sess-threshold Count] \
  [--rel-sess-threshold Fraction]
```

### rebalance node-status

現在ノードの再分散状態を取得します。

### rebalance node-status "node1@host1"

リモートノードの再分散状態を取得します。

### rebalance status

クラスター全体の現在の再分散/退避処理の状態を取得します。

### rebalance stop

現在ノードの退避を停止します。

## gateway

ゲートウェイのロード状況や稼働状態を検査・管理するコマンドです。

### gateway list

すべてのゲートウェイ情報を一覧表示します。

```bash
$ emqx ctl gateway list
Gateway(name=coap, status=running, clients=0, started_at=2023-05-22T14:23:50.353+08:00)
Gateway(name=exproto, status=unloaded)
Gateway(name=lwm2m, status=unloaded)
Gateway(name=mqttsn, status=unloaded)
Gateway(name=stomp, status=unloaded)
```

### gateway lookup \<Name\>

特定ゲートウェイの詳細情報を照会します。

```bash
$ emqx ctl gateway lookup coap
name: coap
status: running
created_at: 2023-05-22T14:23:50.352+08:00
started_at: 2023-05-22T14:23:50.353+08:00
config: #{connection_required => false,enable => true,enable_stats => true,
          heartbeat => 30000,idle_timeout => 30000,
          listeners =>
              #{udp =>
                    #{default =>
                          #{access_rules => [],bind => 5683,enable => true,
                            enable_authn => true,max_conn_rate => 1000,
                            max_connections => 1024000,
                            udp_options =>
                                #{active_n => 100,reuseaddr => true}}}},
          mountpoint => <<>>,notify_type => qos,publish_qos => coap,
          subscribe_qos => coap}
```

### gateway load \<Name\> \<JsonConf\>

ゲートウェイをロードし、パラメータを設定します。

```bash
emqx ctl gateway load coap '{"type":"coap", ...}'
```

### gateway unload \<Name\>

ゲートウェイをアンロードします。

```bash
$ emqx ctl gateway unload coap
ok
```

### gateway stop \<Name\>

ゲートウェイを停止します。

```bash
$ emqx ctl gateway stop coap
ok
```

### gateway start \<Name\>

ゲートウェイを起動します。

```bash
$ emqx ctl gateway start coap
ok
```

## gateway-registry

`emqx ctl gateway-registry`

システムに登録されているゲートウェイを一覧表示します。

デフォルトでは以下5つのゲートウェイが登録されています：

* coap
* exproto
* lwm2m
* mqttsn
* stomp

EMQXはプラグイン可能な設計であり、さらに多くのゲートウェイをプラグインとしてインストールし、実行時にEMQXに登録できます。  
登録後は管理APIやCLI（`gateway`コマンド）で管理可能です。

## gateway-clients

ゲートウェイクライアントを検査するコマンドです。

### gateway-clients list \<Name\>

指定ゲートウェイのすべてのクライアントを一覧表示します。

### gateway-clients lookup \<Name\> \<ClientId\>

指定クライアントの情報を照会します。

### gateway-clients kick \<Name\> \<ClientId\>

指定クライアントをゲートウェイから強制切断します。

## gateway-metrics \<Name\>

指定ゲートウェイのすべてのメトリクスを一覧表示します。

## license

### license info

ライセンス情報を表示します。

```bash
$ emqx ctl license info
customer        : Developer
email           : contact@emqx.io
deployment      : Development
max_sessions    : 10000000
start_at        : 2025-03-02
expiry_at       : 2029-03-01
type            : community
customer_type   : 11
expiry          : false
```

### license update License

ライセンス情報を更新します。

```bash
emqx ctl license update <YOUR_LICENSE_STRING>
```

`YOUR_LICENSE_STRING`は実際のライセンス文字列に置き換えてください。

### license update default

デフォルトのCommunity Licenseに戻します。

```bash
emqx ctl license update default
```

### license history

セッションのピーク数履歴を表示します。EMQX Enterpriseは日次ピークセッション数を記録し、少なくとも24ヶ月分保持して課金監査に利用します。

```bash
emqx ctl license history [N] [--period daily|monthly] [--json]
```

- `N`: オプションの正の整数。返す行数の上限（デフォルトは月次で24行）
- `--period daily|monthly`: 集計粒度。`daily`は日次、`monthly`は日次ピークを月次最大に集約（デフォルトは`monthly`）
- `--json`: プレーンテキストではなくJSON形式で出力

**例：プレーンテキスト出力**

```bash
$ emqx ctl license history
period=2026-04 high_watermark=25000 observed_at=2026-04-18T13:53:05.000Z
period=2026-03 high_watermark=23500 observed_at=2026-03-31T22:10:42.000Z
```

**例：JSON出力**

```bash
$ emqx ctl license history --json
```

```json
{
  "period": "monthly",
  "count": 2,
  "data": [
    { "period": "2026-04", "high_watermark": 25000, "observed_at": "2026-04-18T13:53:05.000Z" },
    { "period": "2026-03", "high_watermark": 23500, "observed_at": "2026-03-31T22:10:42.000Z" }
  ]
}
```

まだデータが記録されていない場合、プレーンテキスト出力は以下を表示します。

```
No session high-watermark history recorded.
```

## mt

`mt`コマンドはEMQX Enterpriseのマルチテナンシー保守操作を提供します。

### mt purge_ns \<Namespace\>

EMQX Enterprise 6.1.4以降、このコマンドは指定したネームスペースを削除し、同期的にクリーンアップ処理を実行します。  
クリーンアップはネームスペース設定と組み込みデータベース内のネームスペーススコープデータ（パスワード認証ユーザー、SCRAMユーザー、認可ルールなど）を削除します。  
ネームスペースが存在しなくてもコマンドは実行されます。

ネームスペース状態が変わっていなければ冪等です。クリーンアップが完了しなかった場合は、同名のネームスペースが再作成されていなければ再実行可能です。

中断されたネームスペース削除の残骸を除去する最後の手段として使用してください。通常の削除はダッシュボードまたは`DELETE /mt/ns/<namespace>` REST APIを利用してください。

::: warning 重要

存在するネームスペースに対して実行すると、ネームスペースとそのデータが永久に削除されます。  
同名のネームスペースが再作成された後に再実行しないでください。

:::

例：`tenant-a`ネームスペースを削除

```bash
emqx ctl mt purge_ns tenant-a
```

すべてのクリーンアップが成功した場合、出力JSONに`"result": "ok"`が含まれます。

```json
{"namespace":"tenant-a","result":"ok"}
```

一部のクリーンアップが失敗した場合、出力JSONに`"error": "cleanup_incomplete"`が含まれます。

```json
{"error":"cleanup_incomplete","hint":"some cleanup steps failed; check logs and re-run the command to retry","namespace":"tenant-a"}
```

失敗したクリーンアップ手順はEMQXログを確認し、原因を解決してから再実行してください。

## admins

管理ユーザーを管理するコマンドです。

### admins add \<Username\> \<Password\> \<Description\>

ダッシュボードユーザーを追加します。

```bash
$ emqx ctl admins add emqx_u EMQemq@1172
ok
```

### admins passwd \<Username\> \<Password\>

特定ダッシュボードユーザーのパスワードをリセットします。

```bash
$ emqx ctl admins passwd emqx_u EMQemq@11721
ok
```

### admins del \<Username\>

特定ダッシュボードユーザーを削除します。

```bash
$ emqx ctl admins del emqx_u
ok
```

## api_keys

REST APIキーをコマンドラインで管理するコマンドです。ダッシュボードにログインせずAPIアクセスをブートストラップできます。

### api_keys list

すべてのAPIキーを一覧表示します。

```bash
$ emqx ctl api_keys list
[
  {
    "role" : "administrator",
    "name" : "my-key",
    "expired_at" : "infinity",
    "expired" : false,
    "enable" : true,
    "desc" : "",
    "api_key" : "admin"
  }
]
```

### api_keys show --name \<Name\>

特定APIキーの詳細を表示します。

```bash
$ emqx ctl api_keys show --name my-key
```

### api_keys add

新しいAPIキーを作成します。生成された`api_key`と`api_secret`が出力されます。`api_secret`は作成時のみ表示されます。

```bash
$ emqx ctl api_keys add --name my-key --role viewer --valid-days 30 --desc "My API key"
{
  "role" : "viewer",
  "name" : "my-key",
  "expired_at" : 1777201070,
  "expired" : false,
  "enable" : true,
  "desc" : "My API key",
  "api_secret" : "tEWX9APine9B9Bkk...",
  "api_key" : "CPKcoFpIkIlbaqhL"
}
```

オプション:

| オプション | 説明 |
| --- | --- |
| `--name <Name>` | 必須。APIキーを識別する名前。 |
| `--api-secret <Secret>` | 任意。シークレットキーを指定。省略時は自動生成。 |
| `--valid-days <infinity\|days>` | 任意。有効期限。`infinity`は無期限（デフォルト）、または日数指定。 |
| `--role <Role>` | 任意。`administrator`（デフォルト）、`viewer`、`publisher`のいずれか。詳細は[Roles and Permissions](./api.md#roles-and-permissions)参照。 |
| `--desc <Desc>` | 任意。APIキーの説明。 |

### api_keys enable --name \<Name\>

無効化されていたAPIキーを有効化します。

```bash
$ emqx ctl api_keys enable --name my-key
```

### api_keys disable --name \<Name\>

APIキーを削除せずに無効化します。

```bash
$ emqx ctl api_keys disable --name my-key
```

### api_keys del --name \<Name\>

APIキーを削除します。

```bash
$ emqx ctl api_keys del --name my-key
{
  "result" : "ok",
  "name" : "my-key"
}
```

## rules

ルールエンジンで作成されたルールを一覧表示するコマンドです。

### rules list

ルールID、名前などの情報を含むすべてのルールを一覧表示します。

```bash
$ emqx ctl rules list
Rule{id=my-rule, name=, enabled=true, descr=this is my rule}
```

### rules show \<RuleID\>

特定ルールの詳細情報を表示します。

```bash
$ emqx ctl rules show my-rule
Id:
  my-rule
Name:

Description:
  this is my rule
Enabled:
  true
SQL:
  SELECT
    *
  FROM
    "f/#"
Created at:
  2023-05-22T14:14:27.567+08:00
Updated at:
  2023-05-22T14:14:27.567+08:00
Actions:
  - Name:  republish
    Type:  function
    Args:  #{payload => <<>>,qos => 0,retain => false,topic => <<"t/1">>,
             user_properties => <<"${user_properties}">>}
```

CLIは参照のみ対応し、ルールやアクションの管理はダッシュボードから行います。
