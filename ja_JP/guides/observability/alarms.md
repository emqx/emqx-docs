# アラーム

EMQX は、CPU 使用率、システムおよびプロセスメモリ使用率、プロセス数、ルールエンジンのリソース状態、クラスターのパーティションや修復など、内部状態の変化を監視するための組み込みの監視およびアラーム機能を提供しています。EMQX は、これらの変化が閾値を超えたり期待値から逸脱した場合にアラームを発動・記録し、正常に戻った場合はリストから削除します。

本ページでは、EMQX が提供するアラーム情報、詳細なアラーム情報の取得・確認方法、および EMQX におけるアラーム設定と閾値の構成方法を紹介します。監視およびアラーム機能により、運用中の潜在的な問題を通知し、適切な閾値を設定してアラームを構成することで、EMQX の安全性、安定性、信頼性を確保できます。

## アラーム一覧

以下の表は、システム監視中に潜在的な問題を示すために発動可能なアラームを示しています。

::: tip

アラームは、システムへの影響度や重大度に応じて3つのレベルに分類されます。

- **Error（エラー）**: ユーザー設定によるエラー。クライアントはエラーを検知し再試行可能です。

- **Warning（警告）**: 時折発生するエラー。頻発する場合は注意が必要です。

- **Critical（重大）**: クライアントとサーバー間で不可逆なデータ損失が発生し、通信や業務が中断されます。

これらのレベルは開発視点で定義されており、あくまで推奨です。業務ニーズに応じて独自のアラームレベルを定義可能です。

:::

| **アラーム名**               | レベル    | 説明                                                         | **詳細**                                    | **閾値**                                                     |
| :-------------------------- | -------- | :----------------------------------------------------------- | :------------------------------------------ | :----------------------------------------------------------- |
| high_system_memory_usage    | Warning  | システムメモリ使用率が高い                                   | システムメモリ使用率が約~p%を超えている     | `os_mon.sysmem_high_watermark = 70%`                         |
| high_process_memory_usage   | Warning  | 単一のErlangプロセスメモリ使用率が高い（システムメモリ使用率の割合） | プロセスメモリ使用率が約~p%を超えている     | `os_mon.procmem_high_watermark = 5%`                         |
| high_cpu_usage              | Warning  | CPU使用率が高い                                              | 約~p%のCPU使用率                            | `os_mon.cpu_high_watermark = 80%` `os_mon.cpu_low_watermark = 60%` |
| too_many_processes          | Warning  | プロセス数が多すぎる                                        | 約~p%のプロセス使用率                        | `vm_mon.process_high_watermark = 80%` `vm_mon.process_low_watermark = 60%` |
| license_quota               | Warning  | ライセンスの接続数が上限を超えている                         | ライセンス：接続数が%を超過                  | `license.connection_high_watermark_alarm = 80%` `license.connection_low_watermark_alarm = 75%` |
| license_expiry              | Critical | ライセンスが期限切れ                                         | ライセンスは%に期限切れとなる予定             | -                                                            |
| license_tps                 | Warning  | TPS使用率がライセンス上限を超えている                        | ライセンス：TPS上限（例：10）を超過          | -                                                            |
| partition                   | Critical | ノードでパーティションが発生                                 | ノード~sでパーティションが発生                | -                                                            |
| resource                    | Critical | リソースが切断されている                                    | リソース~s(~s)がダウンしている               | -                                                            |
| conn_congestion             | Critical | 接続プロセスの輻輳                                        | 接続が輻輳している                            | -                                                            |

## アラームの取得

EMQX では、アラームの取得および詳細情報の確認に複数の方法を提供しています。ひとつは EMQX ダッシュボードを利用する方法で、アクティブなアラームと履歴アラームの両方をユーザーフレンドリーなインターフェースで閲覧できます。これにより、発動したアラームの概要を一元的に確認可能です。

また、MQTT のシステムトピックをサブスクライブしてリアルタイムにシステムアラームの通知を受け取る方法もあります。さらに Webhook 連携により、アラームイベントを外部のHTTPサービスに送信して処理することも可能です。ログや REST API からアラーム情報を取得することもできます。

### ダッシュボードでアラームを確認する

EMQX ダッシュボードで、**Monitoring** -> **Alarms** をクリックします。次に **Active** または **History** タブを選択すると、現在アクティブなアラームや過去のアラーム一覧が表示されます。

EMQX ダッシュボードでのアラーム管理の詳細は、[アラーム](../dashboard/alarm_dashboard.md)をご参照ください。

<img src="./assets/view-alarms.png" alt="アラームの表示" style="zoom:50%;" />

### システムトピックでアラームを取得する

アラームが発動または解除されると、EMQX は MQTT メッセージをシステムトピック `$SYS/brokers/<Node>/alarms/activate` または `$SYS/brokers/<Node>/alarms/deactivate` にパブリッシュします。ユーザーはこれらのトピックをサブスクライブしてアラーム通知を受け取れます。

アラーム通知メッセージのペイロードは JSON 形式で、以下のフィールドを含みます。

| フィールド名       | 型               | 説明                                                         |
| ------------------ | ---------------- | ------------------------------------------------------------ |
| `name`             | string           | アラーム名                                                   |
| `details`          | object           | アラームの詳細                                               |
| `message`          | string           | 人間が読みやすいアラームの説明文                             |
| `activate_at`      | integer          | アラーム発動時刻をマイクロ秒単位の UNIX タイムスタンプで表現 |
| `deactivate_at`    | integer / string | アラーム解除時刻をマイクロ秒単位の UNIX タイムスタンプで表現。発動中のアラームは `infinity` となる。 |
| `activated`        | boolean          | アラームが発動中かどうか                                     |

例えば、システムメモリ使用率が高いアラームの場合、以下のようなメッセージを受け取ります。

<img src="./assets/alarm_activate_msg.png" alt="アラームメッセージ" style="zoom:50%;" />

同じ種類のアラームは繰り返し報告されません。例えば高CPU使用率のアラームが発動中の場合、同種の新たなアラームは生成されません。監視対象の指標が正常に戻ると自動的にアラームは解除されるか、手動で解除可能です。

### ログからアラームを取得する

アラームの発動・解除はログ（コンソールまたはファイル）に記録されます。メッセージ送信やイベント処理の失敗時に詳細情報をログに残せるほか、ログ解析を通じてアラートを検知することも可能です。以下の例はログに出力された詳細なアラーム情報を示しています。ログレベルは `warning`、`msg` フィールドは `alarm_is_activated` および `alarm_is_deactivated` です。

<img src="./assets/view-alarms-log.png" alt="ログでのアラーム表示" style="zoom:50%;" />

### REST API でアラームを取得する

API を通じてアラームの照会や管理が可能です。UI の左ナビゲーションメニューで **Alarms** をクリックすると、この API リクエストが実行されます。EMQX API の利用方法は [REST API](../api.md) をご参照ください。

<img src="./assets/view-alarms-api.png" alt="APIでのアラーム表示" style="zoom:45%;" />

### Webhook 連携でアラームイベントを送信する

EMQX バージョン 5.8.5 以降、ルールエンジンは以下の2つの新しいアラームイベントをサポートしています。

- [$events/sys/alarm_activated](../../develop/data-integration/rule-sql-events-and-fields.md#system-alarm-activated-event-events-sys-alarm-activated)
- [$events/sys/alarm_deactivated](../../develop/data-integration/rule-sql-events-and-fields.md#system-alarm-deactivated-event-events-sys-alarm-deactivated)

これらのイベントにより、Webhook 連携を通じて外部 HTTP サービスへアラームの発動・解除通知を受け取れます。

Webhook 連携の設定手順：

1. EMQX ダッシュボードで **Monitoring** -> **Alarms** に移動します。
2. 右上の **Set Up Webhook** ボタンをクリックし、Webhook 連携設定ページを開きます。
3. Webhook 連携の名前と任意のメモを入力します。**Trigger** フィールドには `Alarm Activated` と `Alarm Deactivated` があらかじめ選択されています。
4. 通知を送信したい Webhook URL を入力します。
5. 詳細な設定は [Webhook 作成](../../develop/data-integration/webhook.md) を参照してください。
6. 設定完了後、**Save** をクリックします。

![alarm_webhook_setup](./assets/alarm_webhook_setup.png)

## アラーム設定

アラーム設定には、アラームの動作設定と閾値設定が含まれます。動作設定はアラームメッセージの表示や保存方法を決定し、閾値設定は潜在的な問題を検知してアラームを発動するための基準値を定めます。これにより、業務ニーズに応じてアラームの動作や閾値をカスタマイズ可能です。

### アラーム動作設定の構成

アラームの動作設定は、設定ファイル内の設定項目を変更することでのみ構成可能です。以下の表は、アラーム動作設定に利用可能な設定項目を示しています。

| 設定項目               | 説明                                                         | デフォルト値          | 選択可能な値      |
| ---------------------- | ------------------------------------------------------------ | -------------------- | ----------------- |
| alarm.actions          | アラーム発動・解除時にログ（コンソールまたはファイル）への書き込みと、システムトピック `$SYS/brokers/<node_name>/alarms/activate` および `$SYS/brokers/<node_name>/alarms/deactivate` への MQTT メッセージのパブリッシュを行うアクション。 | `["log", "publish"]` | -                 |
| alarm.size_limit       | 履歴として保持する解除済みアラームの最大件数。この上限を超えると最も古い解除済みアラームから削除される。 | `1000`               | `1-3000`          |
| alarm.validity_period  | 解除済みアラームの保持期間。解除直後に削除されず、一定期間保持される。 | `24h`                | -                 |

### ダッシュボードでアラーム閾値を設定する

EMQX ダッシュボードでアラーム閾値を設定可能です。閾値設定用の **Monitoring** ページを開く方法は2通りあります。

1. **Alarms** ページで **Setting** ボタンをクリックすると、**Monitoring** ページに遷移します。
2. 左ナビゲーションメニューから **Management** -> **Monitoring** をクリックします。

**Monitoring** -> **System** タブの **Erlang VM** タブでは、Erlang 仮想マシンのシステムパフォーマンスに関する以下の項目を設定できます。

<img src="./assets/monitoring-system-ee.png" alt="Erlang VMの監視設定" style="zoom:40%;" />

- **Process limit check interval**: プロセス数の定期チェック間隔（秒）。デフォルトは `30` 秒。
- **Process high watermark**: ローカルノードで同時に存在可能なプロセス数の閾値（割合）。この値を超えるとアラームが発動。デフォルトは `80` %。
- **Process low watermark**: プロセス数がこの値まで下がるとアラームが解除される閾値（割合）。デフォルトは `60` %。
- **Enable Long GC monitoring**: デフォルトは無効。有効化すると、Erlang プロセスが長時間ガベージコレクションを行うと警告ログ `long_gc` を出力し、システムトピック `$SYS/sysmon/long_gc` に MQTT メッセージをパブリッシュ。
- **Enable Long Schedule monitoring**: デフォルトは有効。Erlang VM が長時間スケジュールされたタスクを検知すると警告ログ `long_schedule` を出力。タスクの適切なスケジュール時間はテキストボックスで設定可能。デフォルトは `240` ミリ秒。
- **Enable Large Heap monitoring**: デフォルトは有効。Erlang プロセスが大きなヒープ領域を消費すると警告ログ `large_heap` を出力し、システムトピック `$SYS/sysmon/large_heap` に MQTT メッセージをパブリッシュ。ヒープサイズの閾値はテキストボックスで設定可能。デフォルトは `32` MB。
- **Enable Busy Distribution Port monitoring**: デフォルトは有効。クラスター内の他ノードと通信するための RPC 接続が過負荷になると警告ログ `busy_dis_port` を出力し、システムトピック `$SYS/sysmon/busy_dist_port` に MQTT メッセージをパブリッシュ。
- **Enable Busy Port monitoring**: デフォルトは有効。ポートが過負荷になると警告ログ `busy_port` を出力し、システムトピック `$SYS/sysmon/busy_port` に MQTT メッセージをパブリッシュ。

設定完了後、**Save Changes** をクリックしてください。

**Operating System** タブでは、システムパフォーマンスに関する以下の項目を設定できます。

<img src="./assets/monitoring-operating-system-ee.png" alt="OSの監視設定" style="zoom:40%;" />

- **The time interval of the periodic CPU check**: CPU 使用率の定期チェック間隔（秒）。デフォルトは `60` 秒。
- **CPU high watermark**: システム CPU 使用率の上限閾値。これを超えるとアラームが発動。デフォルトは `80` %。
- **CPU low watermark**: システム CPU 使用率の下限閾値。これを下回るとアラームが解除。デフォルトは `60` %。
- **Mem check interval**: メモリ使用率の定期チェック間隔。デフォルトは有効で `60` 秒。
- **SysMem high watermark**: システムメモリ使用率の上限閾値。これを超えるとアラームが発動。デフォルトは `70` %。
- **ProcMem high watermark**: 単一の Erlang プロセスによるメモリ使用率の上限閾値。これを超えるとアラームが発動。デフォルトは `5` %。

設定完了後、**Save Changes** をクリックしてください。

### 設定ファイルでアラーム閾値を設定する

設定ファイルの設定項目を変更してアラーム閾値を設定することも可能です。現在変更可能な設定項目は以下の通りです。

| 設定項目                        | 説明                                                         | デフォルト値   |
| ------------------------------ | ------------------------------------------------------------ | ------------- |
| sysmon.os.cpu_check_interval    | CPU 使用率のチェック間隔                                     | `60s`         |
| sysmon.os.cpu_high_watermark    | CPU 使用率の上限閾値。これを超えるとアラームが発動。          | `80%`         |
| sysmon.os.cpu_low_watermark     | CPU 使用率の下限閾値。これを下回るとアラームが解除。          | `60%`         |
| sysmon.os.mem_check_interval    | メモリ使用率のチェック間隔                                   | `60s`         |
| sysmon.os.sysmem_high_watermark | システムメモリ使用率の上限閾値。これを超えるとアラームが発動。 | `70%`         |
| sysmon.os.procmem_high_watermark| 単一プロセスのメモリ使用率の上限閾値。これを超えるとアラームが発動。 | `5%`          |
| sysmon.vm.process_check_interval| プロセス数のチェック間隔                                     | `30s`         |
| sysmon.vm.process_high_watermark| プロセス占有率の上限閾値。これを超えるとアラームが発動。作成済みプロセス数/最大数の比率で測定。 | `80%`         |
| sysmon.vm.process_low_watermark | プロセス占有率の下限閾値。これを下回るとアラームが解除。作成済みプロセス数/最大数の比率で測定。 | `60%`         |
| sysmon.vm.long_gc               | Long GC 監視の有効化設定                                    | `disabled`    |
| sysmon.vm.long_schedule         | Long Schedule 監視の有効化設定                              | `disabled`    |
| sysmon.vm.large_heap            | Large Heap 監視の有効化設定                                 | `disabled`    |
| sysmon.vm.busy_dist_port        | Busy Distribution Port 監視の有効化設定                     | `true`        |
| sysmon.vm.busy_port             | Busy Port 監視の有効化設定                                  | `true`        |
| sysmon.top.num_items            | 監視グループごとのトッププロセス数                          | `10`          |
| sysmon.top.sample_interval      | トッププロセスのチェック間隔                                | `2s`          |
| sysmon.top.max_procs            | VM 内のプロセス数がこの値を超えた場合、データ収集を停止       | `1000000`     |

EMQX Enterprise では、ライセンスの有効期限が30日未満になるか、接続数が上限を超えた場合にアラームを発動します。接続数の上限・下限閾値は設定ファイルの以下の項目で調整可能です。ライセンス設定の詳細は [License](../configuration/license.md) をご参照ください。

| 設定項目                                | 説明                                                         | デフォルト値   |
| ------------------------------------- | ------------------------------------------------------------ | ------------- |
| license.connection_high_watermark_alarm | ライセンスがサポートする最大接続数の上限閾値。これを超えるとアラームが発動。アクティブ接続数/最大接続数の比率で測定。 | `80%`         |
| license.connection_low_watermark_alarm  | ライセンスがサポートする最大接続数の下限閾値。これを下回るとアラームが解除。アクティブ接続数/最大接続数の比率で測定。 | `75%`         |
