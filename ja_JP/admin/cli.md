# コマンドラインインターフェース

このページでは、EMQXがサポートする起動および管理コマンドの種類を紹介し、ctl管理コマンドについて詳細に解説します。

## 起動コマンド

EMQXは基本的な起動および管理コマンドをサポートしており、`emqx <command>` コマンドで実行できます。

よく使われる起動および管理コマンドは以下の通りです：

| コマンド    | 説明                                                         |
| ---------- | ------------------------------------------------------------ |
| start      | EMQXをデーモンモードで起動します。実行時に対話型シェルは不要です。 |
| console    | EMQXをErlangまたはElixirの対話型シェルで起動します。開発環境でのデバッグに使用し、EMQXとの対話が必要です。 |
| foreground | EMQXをフォアグラウンドモードで起動します。対話型シェルは使用しません。開発環境でバックグラウンドでなく起動する際に使用します。 |
| stop       | 実行中のEMQXノードを停止します。                           |
| ctl        | EMQXの管理と監視を行います。`emqx ctl help`で詳細情報を取得できます。 |

以下は開発やデバッグ向けの高度なコマンドで、通常のユーザーはあまり意識する必要はありません：

| コマンド        | 説明                                                         |
| -------------- | ------------------------------------------------------------ |
| remote_console | リモートEMQXノードの対話型シェルに接続します。               |
| attach         | 実行中のEMQXノードにアタッチして対話操作を行います。         |
| ertspath       | EMQXのErlangライブラリのパスを取得します。                   |
| root_dir       | EMQXのルートディレクトリのパスを取得します。                 |
| pid            | 実行中のEMQXノードのプロセスIDを取得します。                 |
| ping           | EMQXノードが稼働中かどうかを確認します。                     |
| check_config   | EMQXの設定ファイルが正しいか検証します。                     |
| console_clean  | 対話型シェルのコンソール出力をクリアします。                 |
| escript        | EMQXノード上でEscriptスクリプトを実行します。                |

## ctlコマンド

EMQXの`ctl`コマンドは、EMQXの管理および監視のための複数のサブコマンドを提供します。`ctl`コマンドはEMQXサービス起動後に実行する必要があります。

> EMQXは`emqx_ctl`コマンドも提供しており、これは`emqx ctl`のエイリアスです。  
> `ctl`コマンドは隠れたErlangノードを起動して指定したEMQXノードにリモート接続し、Erlangのリモートコールを実行し、結果を表示します。したがって、`ctl`コマンドの過剰な使用は避けることが推奨されます。

以下に`ctl`コマンドの全サブコマンドと簡単な説明を示します。詳細なパラメータ情報は`help`コマンドで確認できます。

## status

ブローカーが起動しているかどうかを素早く確認するコマンドです。

```bash
$ emqx ctl status
Node 'emqx@127.0.0.1' 5.0.3 is started
```

## broker

ローカルブローカーの稼働状況、統計、メトリクスを確認するコマンドです。

```bash
$ emqx ctl broker
sysdescr  : EMQX Enterprise
version   : 5.0.3
datetime  : 2023-05-12T10:21:50.095047713+08:00
uptime    : 52 seconds
```

## observer

Erlang仮想マシンの情報を提供し、Linuxの`top`コマンドのようなリアルタイムビューを表示します。サブコマンドは以下の通りです：

| コマンド           | 説明                                                         |
| ----------------- | ------------------------------------------------------------ |
| observer status   | 現在のコンソールでobserverを起動し、EMQXノードの状態や活動を監視・デバッグします。 |
| observer bin_leak | 全プロセスにガベージコレクションを強制し、最大のバイナリデータを解放した上位100プロセスを表示し、メモリリークの可能性を調査します。 |
| observer load Mod | 指定したモジュールをEMQXクラスター内の全ノードにロードします。クラスター全体でモジュールを利用可能にする際に使用します。 |

### observer status

```bash
emqx ctl observer status
```

### observer bin_leak

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

```bash
$ emqx ctl observer load Mod
Loaded 'Mod' module on []: ok
```

## conf cluster_sync

このコマンドは、クラスター内のノード間で設定変更を同期するためのcluster-callsに問題がある場合のトラブルシューティングに主に使用します。

::: tip

EMQX 5.0.xではこのコマンドは`cluster_call`という名前でした。この古いコマンドはEMQX 5.1でも利用可能ですが、ヘルプ情報には表示されません。

:::

EMQXのHTTP APIは多くの設定変更に使用できます。API呼び出し（例えばダッシュボード操作）を受けたノードは、まず変更内容をローカルの`data/configs/cluster.hocon`に書き込みます。その後、同じ操作をデータベースに記録し、非同期でクラスター内の他ノードに転送します。

何らかの理由で、この複製がピアノードで適用できない場合、このコマンドで複製状況を確認し、修正して進めることが可能です。

EMQXはクラスター範囲内の各設定変更に対してID（tnxid）を生成します。このIDはクラスター内で厳密に増加し、ダッシュボードからの設定変更などすべての変更がデータベースに記録されます。以下の例は、tnxid=2の2番目の変更内容（TLSリスナーを有効化する操作）を表示しています。

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

::: tip
`skip`や`fast_forward`コマンドはクラスター内のノード間で設定が乖離する可能性があります。
:::

## admins

`admins`コマンドは管理ユーザーの作成、更新、削除に使用します。サブコマンドは以下の通りです：

| コマンド                                        | 説明                                   |
| ---------------------------------------------- | ------------------------------------- |
| admins add \<Username> \<Password> \<Description> | ダッシュボードユーザーを追加します。 |
| admins passwd \<Username> \<Password>            | 指定したダッシュボードユーザーのパスワードをリセットします。 |
| admins del \<Username>                          | 指定したダッシュボードユーザーを削除します。 |

### admins add \<Username> \<Password> \<Description>

```bash
$ emqx ctl admins add emqx_u EMQemq@1172
ok
```

### admins passwd \<Username> \<Password>

```bash
$ emqx ctl admins passwd emqx_u EMQemq@11721
ok
```

### admins del \<Username>

```bash
$ emqx ctl admins del emqx_u
ok
```

## retainer

`retainer`コマンドは保持されたメッセージの確認や管理に使用します。`emqx ctl retainer reindex`コマンドで保持メッセージのインデックス作成や更新も可能です。

| コマンド                        | 説明                                                         |
| ------------------------------ | ------------------------------------------------------------ |
| retainer info                  | 保持メッセージの数を表示します。                             |
| retainer topics                | 保持メッセージがあるすべてのトピックを表示します。           |
| retainer clean                 | すべての保持メッセージをクリアします。                       |
| retainer clean \<Topic>         | 指定したトピックフィルターに従って保持メッセージをクリアします。 |
| retainer reindex status        | インデックス作成処理の状態を表示します。                     |
| retainer reindex start [force] | 設定に基づき保持メッセージトピックの新しいインデックスを生成します。`<force>`に`true`を渡すと、既に開始されたインデックス作成を無視します。 |

### retainer info

```bash
$ emqx ctl retainer info
Number of retained messages: 3
```

### retainer topics

```bash
$ emqx ctl retainer topics
$SYS/brokers
$SYS/brokers/emqx@127.0.0.1/sysdescr
$SYS/brokers/emqx@127.0.0.1/version
```

### retainer clean

```bash
emqx ctl retainer clean
```

### retainer clean \<Topic>

```bash
emqx ctl retainer clean t/1
```

### retainer reindex status

```bash
$ emqx ctl retainer reindex status
Reindexing is not running
```

### retainer reindex start [force]

```bash
$ emqx ctl retainer reindex start true
Starting reindexing
Reindexed 0 messages
Reindexing finished
```

## cluster

クラスター内のノードの状態を確認・管理するコマンドです。

EMQXの`join`コマンドはクラスター参加の「リクエスト」を指定ノードに送るもので、「招待」ではありません。つまり、`emqx ctl cluster join <OneOfTheClusteredNodes>`は、指定ノードのクラスターに参加するリクエストを送るコマンドであり、指定ノード自身が自分のクラスターに参加するわけではありません。

| コマンド                      | 説明                                   | 利用例・注意点                                               |
| ---------------------------- | ------------------------------------ | ------------------------------------------------------------ |
| emqx ctl cluster             | EMQXのクラスター制御コマンドです。    |                                                              |
| cluster join \<Node\>        | クラスターに参加します。               | - 指定ノードが存在しアクセス可能であることを確認してください。<br />- 指定ノードのクラスターに参加するために使用します。 |
| cluster leave                | クラスターから離脱します。             | - 現在のクラスターからノードを除外するために使用します。     |
| cluster force-leave \<Node\> | 強制的にノードをクラスターから除外します。 | - 指定ノードを強制的にクラスターから除外します。<br />- クラスター状態の不整合を招く可能性があるため注意して使用してください。 |
| cluster status [--json]      | クラスターの状態を確認します。         | - クラスターの状態を表示します。<br />- `--json`オプションでJSON形式で表示可能です。<br />- クラスターの監視やデバッグに役立ちます。 |

### cluster join \<Node\>

```bash
$ emqx ctl cluster join emqx2@127.0.0.1
Failed to join the cluster: {node_down,'emqx2@127.0.0.1'}
```

### cluster leave

```bash
$ emqx ctl cluster leave
Failed to leave the cluster: node_not_in_cluster
```

### cluster force-leave \<Node\>

```bash
$ emqx ctl cluster force-leave emqx2@127.0.0.1
Failed to remove the node from cluster: node_not_in_cluster
```

### cluster status [--json]

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

## clients

接続中のクライアントを確認・管理するコマンドです。

| コマンド                   | 説明                                                         |
| ------------------------- | ------------------------------------------------------------ |
| clients list              | 現在EMQXに接続中のすべてのクライアントを表示します。アクティブなクライアントや接続数の監視に使用します。 |
| clients show \<ClientId\> | 特定クライアントの詳細な接続情報を表示します。               |
| clients kick \<ClientId\> | 指定したクライアントを強制切断します。                       |

### emqx ctl clients list

```bash
$ emqx ctl clients list
Client(emqx_c, username=undefined, peername=127.0.0.1:59441, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4530, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736435155, connected_at=1684736435155)
Client(emqx_a, username=undefined, peername=127.0.0.1:59444, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4588, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736441613, connected_at=1684736441613)
```

### emqx ctl clients show \<ClientId\>

```bash
$ emqx ctl clients show emqx_c
Client(emqx_c, username=undefined, peername=127.0.0.1:59441, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4680, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736435155, connected_at=1684736435155)
```

### emqx ctl clients kick \<ClientId\>

```bash
$ emqx ctl clients kick emqx_c
ok
```

:::tip
多数のクライアントが接続している場合、`list`コマンドは時間がかかりリソースを多く消費する可能性があります。
:::

## topics

現在のシステムでサブスクライブされているすべてのトピックを確認するコマンドです。

| コマンド               | 説明                                                         |
| --------------------- | ------------------------------------------------------------ |
| topics list           | すべてのトピックを一覧表示します。トピック数や分布の監視に使用します。 |
| topics show \<Topic\> | 特定トピックの詳細情報を表示します。                         |

### topics list

```bash
$ emqx ctl topics list
t/1 -> emqx@127.0.0.1
```

### topics show \<Topic\>

```bash
$ emqx ctl topics show t/1
t/1 -> emqx@127.0.0.1
```

:::tip
クラスター内に多数のトピックサブスクリプションがある場合、`list`コマンドは時間がかかりリソースを多く消費する可能性があります。
:::

## exclusive

現在のシステム内のすべての排他トピックを確認したり、排他トピックを削除したりするコマンドです。

| コマンド                    | 説明                     |
| -------------------------- | ------------------------ |
| exclusive list             | すべての排他トピックを一覧表示します。 |
| exclusive delete \<Topic\> | 排他トピックを削除します。 |

### exclusive list

```bash
$ emqx ctl exclusive list
t/1 -> client1
```

### exclusive delete \<Topic\>

```bash
$ emqx ctl exclusive delete t/1
ok
```

## subscriptions

クライアントのサブスクリプションを確認、追加、削除するコマンドです。

| コマンド                                          | 説明                               |
| ------------------------------------------------ | --------------------------------- |
| subscriptions list                               | すべてのサブスクリプションを一覧表示します。 |
| subscriptions show \<ClientId\>                  | 特定クライアントのサブスクリプションを表示します。 |
| subscriptions add \<ClientId\> \<Topic\> \<QoS\> | 手動でサブスクリプションを追加します。 |
| subscriptions del \<ClientId\> \<Topic\>          | 手動でサブスクリプションを削除します。 |

### subscriptions list

```bash
$ emqx ctl subscriptions list
emqx_a -> topic:t/1 qos:0 nl:0 rh:0 rap:0
emqx_c -> topic:t/1 qos:0 nl:0 rh:0 rap:0
```

### subscriptions show \<ClientId\>

```bash
$ emqx ctl subscriptions show emqx_a
emqx_a -> topic:t/1 qos:0 nl:0 rh:0 rap:0
```

### subscriptions add \<ClientId\> \<Topic\> \<QoS\>

```bash
$ emqx ctl subscriptions add emqx_a t/1 1
ok
```

### subscriptions del \<ClientId\> \<Topic\>

```bash
$ emqx ctl subscriptions del emqx_a t/1
ok
```

:::tip
システム内に多数のサブスクリプションがある場合、`list`コマンドは時間がかかりリソースを多く消費する可能性があります。
:::

## plugins

プラグインのインストール状況を確認・管理するコマンドです。

| コマンド                                  | 説明                                                         |
| ---------------------------------------- | ------------------------------------------------------------ |
| plugins list                             | インストール済みのプラグインを一覧表示します。               |
| plugins describe \<Name-Vsn\>            | インストール済みプラグインの詳細情報を表示します。           |
| plugins allow \<Name-Vsn\>               | ダッシュボード経由で指定プラグインのインストールを許可します。 |
| plugins install \<Name-Vsn\>             | プラグインインストールディレクトリにあるプラグインパッケージをインストールします。 |
| plugins uninstall \<Name-Vsn\>           | 指定プラグインをアンインストールします。                     |
| plugins start \<Name-Vsn\>               | 指定プラグインを起動します。                                 |
| plugins stop \<Name-Vsn\>                | 指定プラグインを停止します。                                 |
| plugins restart \<Name-Vsn\>             | 指定プラグインを再起動します。                               |
| plugins disable \<Name-Vsn\>             | プラグインの自動起動を無効化します。                         |
| plugins enable \<Name-Vsn\> \[Position\] | プラグインの自動起動を有効化し、起動順序の位置を指定します。 |

### plugins list

```bash
emqx ctl plugins list
```

### plugins describe \<Name-Vsn\>

```bash
emqx ctl plugins describe emqx_auth_mnesia-3.0.1
```

### plugins allow \<Name-Vsn\>

```bash
emqx ctl plugins allow emqx_auth_mnesia-3.0.1
```

### plugins install \<Name-Vsn\>

```bash
emqx ctl plugins install emqx_auth_mnesia-3.0.1
```

### plugins uninstall \<Name-Vsn\>

```bash
emqx ctl plugins uninstall emqx_auth_mnesia-3.0.1
```

### plugins start \<Name-Vsn\>

```bash
emqx ctl plugins start emqx_auth_mnesia-3.0.1
```

### plugins stop \<Name-Vsn\>

```bash
emqx ctl plugins stop emqx_auth_mnesia-3.0.1
```

### plugins restart \<Name-Vsn\>

```bash
emqx ctl plugins restart emqx_auth_mnesia-3.0.1
```

### plugins disable \<Name-Vsn\>

```bash
emqx ctl plugins disable emqx_auth_mnesia-3.0.1
```

### plugins enable \<Name-Vsn\> \[Position\]

```bash
emqx ctl plugins enable emqx_auth_mnesia-3.0.1 front
```

`front`、`rear`、または`before Other-Vsn`を使って起動順序の相対位置を指定できます。位置を指定しない場合は、既存のプラグインの順序は変わらず、新しいプラグインは末尾に追加されます。

## vm

Erlang仮想マシンから収集した統計データを確認します。

```bash
$ emqx ctl vm
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

## mnesia

組み込みデータベース（Mnesia）の稼働状況とメトリクスを確認するコマンドです。

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

ログハンドラーの状態管理（ログレベル設定など）に使用するコマンドです。

| コマンド                                        | 説明                                                         |
| ---------------------------------------------- | ------------------------------------------------------------ |
| log set-level \<Level\>                        | 全体のログレベルを設定します。                              |
| log primary-level                              | 現在のプライマリログレベルを表示します。`primary-level`はEMQXの全体のデフォルトログレベルを示し、特定のハンドラーが独自のレベルを持たない限りすべてに影響します。 |
| log primary-level \<Level\>                    | プライマリログレベルを設定します。                          |
| log handlers list                              | ログハンドラー一覧を表示します。ハンドラーはログの処理・保存方法を定義し、独自のログレベルを設定できます。 |
| log handlers start \<HandlerId\>               | 指定したハンドラーを起動します。                            |
| log handlers stop \<HandlerId\>                | 指定したハンドラーを停止します。                            |
| log handlers set-level \<HandlerId\> \<Level\> | 指定したハンドラーのログレベルを設定します。                |

### log set-level \<Level\>

```bash
$ emqx ctl log set-level debug
debug
```

### log primary-level

```bash
$ emqx ctl log primary-level
debug
```

### log primary-level \<Level\>

```bash
$ emqx ctl log primary-level info
info
```

### log handlers list

```bash
$ emqx ctl log handlers list
LogHandler(id=ssl_handler, level=debug, destination=console, status=started)
LogHandler(id=console, level=debug, destination=console, status=started)
```

### log handlers start \<HandlerId\>

```bash
$ emqx ctl log handlers start console
log handler console started
```

### log handlers stop \<HandlerId\>

```bash
$ emqx ctl log handlers stop console
log handler console stopped
```

### log handlers set-level \<HandlerId\> \<Level>

```bash
$ emqx ctl log handlers set-level console debug
debug
```

## trace

特定のクライアントやトピックなどのイベントをトレース（ログ記録）するコマンドです。

| コマンド                                              | 説明                                   |
| ---------------------------------------------------- | ------------------------------------- |
| trace list                                           | ローカルノードで開始されたすべてのトレースを一覧表示します。 |
| trace start client \<ClientId\> \<File\> [\<Level\>] | 特定クライアントのトレースを開始します。 |
| trace stop client \<ClientId\>                       | 特定クライアントのトレースを停止します。 |
| trace start topic \<Topic\> \<File\> [\<Level\>]     | 特定トピックのトレースを開始します。   |
| trace stop topic \<Topic\>                           | 特定トピックのトレースを停止します。   |
| trace start ip_address \<IP\> \<File\> [\<Level\>]   | 特定クライアントIPアドレスのトレースを開始します。 |
| trace stop ip_address \<IP\>                         | 特定クライアントIPアドレスのトレースを停止します。 |

### trace list

```bash
$ emqx ctl trace list
Trace(ip_address=127.0.0.1, level=debug, destination="trace.log")
```

### trace start client \<ClientId\> \<File\> [\<Level\>]

```bash
$ emqx ctl trace start client emqx_c trace.log debug
trace emqx_c CLI-emqx_c successfully
```

### trace stop client \<ClientId\>

```bash
$ emqx ctl trace stop client emqx_c
stop tracing clientid emqx_c successfully
```

### trace start topic \<Topic> \<File> [\<Level\>]

```bash
$ emqx ctl trace start topic t/1 trace.log info
trace t/1 CLI-t/1 successfully
```

### trace stop topic \<Topic>

```bash
$ emqx ctl trace stop topic t/1
stop tracing topic t/1 successfully
```

### trace start ip_address \<IP> \<File> [\<Level\>]

```bash
$ emqx ctl trace start ip_address 127.0.0.1 trace.log debug
trace 127.0.0.1 CLI-127.0.0.1 successfully
```

### trace stop ip_address \<IP>

```bash
$ emqx ctl trace stop ip_address 127.0.0.1
stop tracing ip_address 127.0.0.1 successfully
```

::: tip
コマンドラインから開始する場合は、トレースログファイルに絶対パスを使用することを推奨します。  
例：`emqx ctl trace start client foobar /abs/path/to/trace.log debug`
:::

::: tip
トレースはダッシュボードUIからも管理可能です。詳細は[tracer](../observability/tracer.md)を参照してください。
:::

`emqx ctl traces`

このコマンドは`trace`コマンドに似ていますが、クラスター内の全ノードに適用されます。

## traces

クラスター内の全ノードでトレーサーを開始・停止するコマンドです。

| コマンド                                                 | 説明                             |
| ------------------------------------------------------- | -------------------------------- |
| traces list                                             | クラスターで開始されたすべてのトレースを一覧表示します。 |
| traces start \<Name> client \<ClientId> [\<Duration>]   | クラスター内のクライアントのトレースを開始します。       |
| traces start \<Name> topic \<Topic> [\<Duration>]       | クラスター内のトピックのトレースを開始します。           |
| traces start \<Name> ip_address \<IPAddr> [\<Duration>] | クラスター内のクライアントIPのトレースを開始します。     |
| traces stop \<Name>                                     | クラスター内のトレースを停止します。                     |
| traces delete \<Name>                                   | クラスター内のトレースを削除します。                     |

### traces list

```bash
$ emqx ctl traces list
Trace(mytraces_ip: ip_address=127.0.0.1, waiting, LogSize:#{'emqx@127.0.0.1' => 0})
```

### traces start \<Name> client \<ClientId> [\<Duration>]

```bash
$ emqx ctl traces start mytraces client emqx_c 1200
cluster_trace clientid emqx_c mytraces successfully
```

### traces start \<Name> topic \<Topic> [\<Duration>]

```bash
$ emqx ctl traces start mytraces_ip topic t/1 1200
cluster_trace topic t/1 mytraces_ip successfully
```

### traces start \<Name> ip_address \<IPAddr> [\<Duration>]

```bash
$ emqx ctl traces start mytraces_ip ip_address 127.0.0.1 1200
cluster_trace ip_address 127.0.0.1 mytraces_ip successfully
```

### traces stop \<Name>

```bash
$ emqx ctl traces stop mytraces_ip
Stop cluster_trace mytraces_ip successfully
```

### traces delete \<Name>

```bash
$ emqx ctl traces delete mytraces_ip
Del cluster_trace mytraces_ip successfully
```

## listeners

リスナーの管理に使用するコマンドです。

| コマンド                                      | 説明                                                         |
| -------------------------------------------- | ------------------------------------------------------------ |
| listeners                                    | すべてのリスナー情報を一覧表示します。                       |
| listeners stop \<Identifier\>                | リスナーを停止します。Identifierは`{type}:{name}`形式（例：`tcp:default`）。一時的な効果で、EMQX再起動後に元の状態に戻ります。 |
| listeners start \<Identifier\>               | リスナーを起動します。一時的な効果で、EMQX再起動後に元の状態に戻ります。 |
| listeners restart \<Identifier\>             | リスナーを再起動します。                                     |
| listeners enable \<Identifier\> <true/false> | リスナーの有効・無効を設定します。設定は永続化され、永続的に有効です。 |

### listeners

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

### listeners stop \<Identifier\>

```bash
$ emqx ctl listeners stop tcp:default
Stop tcp:default listener successfully.
```

::: tip
リスナーを停止すると、接続中のすべてのクライアントが切断されます。
:::

### listeners start \<Identifier\>

```bash
$ emqx ctl listeners start tcp:default
Started tcp:default listener successfully.
```

### listeners restart \<Identifier\>

```bash
$ emqx ctl listeners restart tcp:default
Restarted tcp:default listener successfully.
```

::: tip
リスナーを再起動すると、接続中のすべてのクライアントが切断されます。
:::

### listeners enable \<Identifier\> <true/false> 

```bash
$ emqx ctl listeners enable tcp:default true
Enabled tcp:default listener successfully.
```

```bash
$ emqx ctl listeners enable tcp:default false
Disabled tcp:default listener successfully.
```

## authz cache-clean

`emqx ctl authz cache-clean`

キャッシュされた認可（ACL）データを強制的に削除したい場合に便利なコマンドです。

## pem_cache

`emqx ctl pem_cache`

証明書更新などの後に、pem（x509キーおよび証明書）ファイルをEMQXに再読み込みさせるためのコマンドです。

## olp

`emqx ctl olp`

OLPはオーバーロードプロテクション（過負荷保護）を意味します。  
`olp`コマンドは過負荷状態の確認や、システムのオーバーロード保護の有効・無効を管理します。

詳細は`overload_protection`設定ドキュメントを参照してください。

::: tip
`olp`はデフォルトで有効ではありません。CLIから有効化しても設定に永続化されません。
:::

## gateway-registry

`emqx ctl gateway-registry`

システムに登録されているゲートウェイを一覧表示します。

デフォルトで以下の5つのゲートウェイが登録されています：

* coap
* lwm2m
* mqttsn
* stomp

EMQXはプラグイン可能な設計であり、より多くのゲートウェイをプラグインとしてインストールし、ランタイムにEMQXに登録できます。  
登録されたゲートウェイは管理APIやCLI（以下の`gateway`コマンド参照）で管理可能です。

## gateway

ゲートウェイの読み込みや稼働状態を確認・管理するコマンドです。

| コマンド                            | 説明                                                         |
| ---------------------------------- | ------------------------------------------------------------ |
| gateway list                       | すべてのゲートウェイ情報を一覧表示します。                   |
| gateway lookup \<Name\>            | 特定ゲートウェイの詳細情報を確認します。                     |
| gateway load \<Name\> \<JsonConf\> | ゲートウェイを読み込み、設定パラメータを指定します。          |
| gateway unload \<Name\>            | ゲートウェイをアンロードします。                             |
| gateway stop \<Name\>              | ゲートウェイを停止します。                                   |
| gateway start \<Name\>             | ゲートウェイを起動します。                                   |

### gateway list

```bash
$ emqx ctl gateway list
Gateway(name=coap, status=running, clients=0, started_at=2023-05-22T14:23:50.353+08:00)
Gateway(name=lwm2m, status=unloaded)
Gateway(name=mqttsn, status=unloaded)
Gateway(name=stomp, status=unloaded)
```

### gateway lookup \<Name\>

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

```bash
emqx ctl gateway load coap '{"type":"coap", ...}'
```

### gateway unload \<Name\>

```bash
$ emqx ctl gateway unload coap
ok
```

### gateway stop \<Name\>

```bash
$ emqx ctl gateway stop coap
ok
```

### gateway start \<Name\>

```bash
$ emqx ctl gateway start coap
ok
```

## gateway-metrics

ゲートウェイのメトリクスを確認します。

## rules

ルールエンジンで作成されたルールを一覧表示するコマンドです。

| コマンド             | 説明                                                         |
| ------------------- | ------------------------------------------------------------ |
| rules list          | ルールID、名前などの情報を含むすべてのルールを一覧表示します。 |
| rules show \<RuleID> | 特定ルールの詳細情報を表示します。                           |

以下はルールの実行例です：

### rules list

```bash
$ emqx ctl rules list
Rule{id=my-rule, name=, enabled=true, descr=this is my rule}
```

### rules show \<RuleID>

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

CLIは確認用であり、ルールやアクションの管理はダッシュボードで行います。

## license

| コマンド                | 説明                             |
| ---------------------- | -------------------------------- |
| license info           | ライセンス情報を表示します。     |
| license update License | ライセンス情報を更新します。     |
| license update default | デフォルトのCommunity Licenseに戻します。 |

### license info

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

```bash
emqx ctl license update <YOUR_LICENSE_STRING>
```

`YOUR_LICENSE_STRING`は実際のライセンス文字列に置き換えてください。

### license update default

```bash
emqx ctl license update default
```
