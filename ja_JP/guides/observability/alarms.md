# アラーム

EMQXは、CPU使用率、システムおよびプロセスメモリ使用率、プロセス数、ルールエンジンのリソース状態、クラスターのパーティションや修復状況など、内部状態の変化を監視するための組み込みの監視およびアラーム機能を提供しています。EMQXは、これらの状態が閾値を超えたり期待値から逸脱した際にアラームを発動・記録し、状態が回復するとリストから削除します。

本ページでは、EMQXが提供するアラーム情報の概要、詳細なアラーム情報の取得・確認方法、およびEMQXでのアラーム設定や閾値の構成方法について紹介します。監視およびアラーム機能により、運用中の潜在的な問題を通知し続けることが可能です。適切な閾値を設定してアラームを構成することで、EMQXの安全性、安定性、信頼性を確保できます。

## アラーム一覧

以下の表は、システム監視中に潜在的な問題を示すために発動可能なアラームを示しています。

::: tip

アラームは、システムへの影響度や重大度に応じて3つのレベルがあります：

- **Error（エラー）**：ユーザー設定によるエラー。クライアントはエラーを認識しリトライ可能です。

- **Warning（警告）**：発生頻度が高い場合は注意が必要な一時的なエラー。

- **Critical（重大）**：クライアントとサーバー間で回復不能なデータ損失が発生し、通信や業務が中断される状態。

これらのレベルは開発視点で定義されており、あくまで推奨です。ビジネスニーズに応じて独自のアラームレベルを定義できます。

:::

| **アラーム**                  | レベル     | 説明                                                         | **詳細**                                    | **閾値**                                                     |
| :--------------------------- | ---------- | :------------------------------------------------------------ | :------------------------------------------ | :------------------------------------------------------------ |
| high_system_memory_usage      | Warning    | システムメモリ使用率が高すぎる                               | システムメモリ使用率が約〜p%を超えている       | `os_mon.sysmem_high_watermark = 70%`                         |
| high_process_memory_usage     | Warning    | 単一のErlangプロセスメモリ使用率が高すぎる（システムメモリ使用率の割合） | プロセスメモリ使用率が約〜p%を超えている       | `os_mon.procmem_high_watermark = 5%`                         |
| high_cpu_usage                | Warning    | CPU使用率が高すぎる                                         | 約〜p%のCPU使用率                            | `os_mon.cpu_high_watermark = 80%` `os_mon.cpu_low_watermark = 60%` |
| too_many_processes            | Warning    | プロセス数が多すぎる                                       | 約〜p%のプロセス使用率                        | `vm_mon.process_high_watermark = 80%` `vm_mon.process_low_watermark = 60%` |
| license_quota                 | Warning    | ライセンスの接続数が上限を超えている                         | ライセンス：接続数が%を超えている             | `license.connection_high_watermark_alarm = 80%` `license.connection_low_watermark_alarm = 75%` |
| license_expiry                | Critical   | ライセンスが期限切れ                                         | ライセンスの有効期限が%に切れる予定            | -                                                            |
| license_tps                  | Warning    | TPS使用率がライセンス上限を超えている                        | ライセンス：TPS上限（例：10）を超えている      | -                                                            |
| partition                    | Critical   | ノードでパーティションが発生                                 | ノード〜sでパーティションが発生                | -                                                            |
| resource                     | Critical   | リソースが切断されている                                    | リソース〜s（〜s）がダウンしている             | -                                                            |
| conn_congestion              | Critical   | 接続プロセスの輻輳                                         | 接続が輻輳している                            | -                                                            |

## アラームの取得

EMQXは、アラームを取得して詳細情報を確認するための複数の方法を提供しています。1つはEMQXダッシュボードを通じて、アクティブなアラームと履歴アラームの両方をユーザーフレンドリーなインターフェースで閲覧できます。これにより、発動したアラームの概要を一元的に把握できます。

また、MQTTのシステムトピックをサブスクライブしてリアルタイムにシステムアラームの通知を受け取る方法もあります。さらにWebhook連携により、アラームイベントを外部HTTPサービスに送信して処理することも可能です。アラームはログやREST API経由でも取得可能です。

### ダッシュボードでのアラーム確認

EMQXダッシュボードで、**Monitoring** -> **Alarms** をクリックします。次に、**Active** または **History** タブを選択すると、現在発動中のアラームや過去のアラーム一覧を確認できます。

EMQXダッシュボードでのアラーム管理の詳細は、[Alarms](../dashboard/alarm_dashboard.md) を参照してください。

<img src="./assets/view-alarms.png" alt="アラームの表示" style="zoom:50%;" />

### システムトピック経由でのアラーム取得

アラームが発動または解除されると、EMQXはMQTTメッセージをシステムトピック `$SYS/brokers/<Node>/alarms/activate` または `$SYS/brokers/<Node>/alarms/deactivate` にパブリッシュします。ユーザーはこれらのトピックをサブスクライブしてアラーム通知を受け取れます。

アラーム通知メッセージのペイロードはJSON形式で、以下のフィールドを含みます：

| フィールド名         | 型               | 説明                                                         |
| -------------------- | ---------------- | ------------------------------------------------------------ |
| `name`               | string           | アラーム名                                                  |
| `details`            | object           | アラームの詳細情報                                         |
| `message`            | string           | 人間が読みやすいアラームの説明                             |
| `activate_at`        | integer          | アラーム発動時刻をマイクロ秒単位のUNIXタイムスタンプで表現    |
| `deactivate_at`      | integer / string | アラーム解除時刻をマイクロ秒単位のUNIXタイムスタンプで表現。発動中のアラームは `infinity` となる。 |
| `activated`          | boolean          | アラームが発動中かどうか                                   |

システムメモリ使用率が高い場合のアラーム例は以下のようなメッセージを受け取ります：

<img src="./assets/alarm_activate_msg.png" alt="アラームメッセージ" style="zoom:50%;" />

同じ種類のアラームは繰り返し報告されません。例えばCPU使用率の高いアラームが発動中の場合、同じ種類のアラームは再度発生しません。監視対象の指標が正常に戻ると自動的にアラームは解除されるか、手動で解除することも可能です。

### ログからのアラーム取得

アラームの発動・解除はログ（コンソールまたはファイル）に記録されます。メッセージ送信やイベント処理で障害が発生した場合、詳細情報がログに出力され、ログ解析を通じてアラートを捕捉することも可能です。以下はログに出力されるアラーム詳細の例です：

ログレベルは `warning` で、`msg` フィールドは `alarm_is_activated` または `alarm_is_deactivated` となります。

<img src="./assets/view-alarms-log.png" alt="ログでのアラーム表示" style="zoom:50%;" />

### REST API経由でのアラーム取得

APIを通じてアラームの照会や管理が可能です。UIの左側ナビゲーションメニューで **Alarms** をクリックすると、このAPIリクエストが実行されます。EMQX APIの利用方法は [REST API](../api.md) を参照してください。

<img src="./assets/view-alarms-api.png" alt="APIでのアラーム表示" style="zoom:45%;" />

### Webhook連携によるアラームイベント送信

EMQXバージョン5.8.5以降、ルールエンジンは以下の2つの新しいアラームイベントをサポートしています：

- [$events/sys/alarm_activated](../../develop/data-integration/rule-sql-events-and-fields.md#system-alarm-activated-event-events-sys-alarm-activated)
- [$events/sys/alarm_deactivated](../../develop/data-integration/rule-sql-events-and-fields.md#system-alarm-deactivated-event-events-sys-alarm-deactivated)

これらのイベントにより、Webhook連携を通じて外部HTTPサービスへアラームの発動・解除通知を受け取れます。

Webhook連携の設定手順：

1. EMQXダッシュボードで **Monitoring** -> **Alarms** に移動します。
2. 右上の **Set Up Webhook** ボタンをクリックし、Webhook連携設定ページを開きます。
3. Webhook連携の名前と任意のメモを入力します。**Trigger** フィールドには `Alarm Activated` と `Alarm Deactivated` があらかじめ選択されています。
4. 通知を送信するWebhookのURLを入力します。
5. 詳細な設定は [Create Webhook](../../develop/data-integration/webhook.md) を参照してください。
6. 設定が完了したら **Save** をクリックします。

![alarm_webhook_setup](./assets/alarm_webhook_setup.png)

## アラーム設定

アラーム設定には、アラームの動作設定と閾値設定があります。アラームの動作設定はアラームメッセージの表示や保存方法を決定し、閾値設定は潜在的な問題を検知してアラームを発動するための限界値や条件を定めます。これにより、ビジネスニーズに応じてアラームの動作や閾値をカスタマイズできます。

### アラーム動作設定

アラームの動作設定は、設定ファイル内の設定項目を変更することでのみ構成可能です。以下の表は、アラーム動作設定に利用できる設定項目を示しています。

| 設定項目              | 説明                                                         | デフォルト値           | 選択可能な値      |
| --------------------- | ------------------------------------------------------------ | --------------------- | ----------------- |
| alarm.actions         | アラーム発動・解除時に、ログ（コンソールまたはファイル）への書き込みと、システムトピック `$SYS/brokers/<node_name>/alarms/activate` および `$SYS/brokers/<node_name>/alarms/deactivate` へのMQTTメッセージのパブリッシュを行うアクション。 | `["log", "publish"]`  | -                 |
| alarm.size_limit      | 履歴として保持する解除済みアラームの最大総数。この上限を超えると最も古い解除済みアラームから削除される。 | `1000`                | `1-3000`          |
| alarm.validity_period | 解除済みアラームの保持期間。解除直後に削除せず、一定期間経過後に削除する。 | `24h`                 | -                 |

### ダッシュボードでのアラーム閾値設定

EMQXダッシュボードでアラーム閾値を設定できます。閾値設定用の **Monitoring** ページを開く方法は2通りあります：

1. **Alarms** ページで **Setting** ボタンをクリックすると、**Monitoring** ページに遷移します。
2. 左側ナビゲーションメニューから **Management** -> **Monitoring** を選択します。

**Monitoring** -> **System** タブの中の **Erlang VM** タブでは、Erlang仮想マシンのシステムパフォーマンスに関する以下の項目を設定できます：

<img src="./assets/monitoring-system-ee.png" alt="システム監視設定" style="zoom:40%;" />

- **Process limit check interval**：プロセス数の定期チェック間隔（秒）。デフォルトは `30` 秒。
- **Process high watermark**：ローカルノードで同時に存在可能なプロセス数の閾値。割合がこの値を超えるとアラーム発動。デフォルトは `80` パーセント。
- **Process low watermark**：ローカルノードで同時に存在可能なプロセス数の閾値。割合がこの値以下になるとアラーム解除。デフォルトは `60` パーセント。
- **Enable Long GC monitoring**：デフォルトは無効。Erlangプロセスが長時間ガベージコレクションを行うと、警告レベルのログ `long_gc` を出力し、システムトピック `$SYS/sysmon/long_gc` にMQTTメッセージをパブリッシュします。
- **Enable Long Schedule monitoring**：デフォルトは有効。Erlang VMが長時間スケジュールされたタスクを検出すると、警告レベルのログ `long_schedule` を出力します。タスクの適切なスケジュール時間をミリ秒単位で設定可能。デフォルトは `240` ミリ秒。
- **Enable Large Heap monitoring**：デフォルトは有効。Erlangプロセスが大容量のヒープメモリを消費すると、警告レベルのログ `large_heap` を出力し、システムトピック `$SYS/sysmon/large_heap` にMQTTメッセージをパブリッシュします。メモリ容量の閾値をバイト単位で設定可能。デフォルトは `32` MB。
- **Enable Busy Distribution Port monitoring**：デフォルトは有効。クラスター内の他ノードとの通信に使われるRPC接続が過負荷になると、警告レベルのログ `busy_dis_port` を出力し、システムトピック `$SYS/sysmon/busy_dist_port` にMQTTメッセージをパブリッシュします。
- **Enable Busy Port monitoring**：デフォルトは有効。ポートが過負荷になると、警告レベルのログ `busy_port` を出力し、システムトピック `$SYS/sysmon/busy_port` にMQTTメッセージをパブリッシュします。

設定完了後、**Save Changes** をクリックしてください。

**Operating System** タブでは、システムパフォーマンスに関する以下の項目を設定できます：

<img src="./assets/monitoring-operating-system-ee.png" alt="OS監視設定" style="zoom:40%;" />

- **The time interval of the periodic CPU check**：CPU使用率の定期チェック間隔（秒）。デフォルトは `60` 秒。
- **CPU high watermark**：システムCPU使用率の閾値。割合がこの値を超えるとアラーム発動。デフォルトは `80` パーセント。
- **CPU low watermark**：システムCPU使用率の閾値。割合がこの値以下になるとアラーム解除。デフォルトは `60` パーセント。
- **Mem check interval**：メモリ使用率の定期チェック間隔（秒）。デフォルトは `60` 秒。デフォルトで有効。
- **SysMem high watermark**：システムメモリ使用率の閾値。割合がこの値を超えるとアラーム発動。デフォルトは `70` %。
- **ProcMem high watermark**：単一Erlangプロセスメモリ使用率の閾値。割合がこの値を超えるとアラーム発動。デフォルトは `5` %。

設定完了後、**Save Changes** をクリックしてください。

### 設定ファイルによるアラーム閾値設定

設定ファイル内の設定項目を変更することでもアラーム閾値を設定できます。現在変更可能な設定項目は以下の通りです：

| 設定項目                         | 説明                                                         | デフォルト値    |
| -------------------------------- | ------------------------------------------------------------ | -------------- |
| sysmon.os.cpu_check_interval      | CPU使用率のチェック間隔                                      | `60s`          |
| sysmon.os.cpu_high_watermark      | CPU使用率の高水準閾値。これを超えるとアラーム発動。          | `80%`          |
| sysmon.os.cpu_low_watermark       | CPU使用率の低水準閾値。これを下回るとアラーム解除。          | `60%`          |
| sysmon.os.mem_check_interval      | メモリ使用率のチェック間隔                                  | `60s`          |
| sysmon.os.sysmem_high_watermark   | システムメモリ使用率の高水準閾値。これを超えるとアラーム発動。 | `70%`          |
| sysmon.os.procmem_high_watermark  | 単一プロセスメモリ使用率の高水準閾値。これを超えるとアラーム発動。 | `5%`           |
| sysmonn.vm.process_check_interval | プロセス数のチェック間隔                                    | `30s`          |
| sysmon.vm.process_high_watermark  | プロセス占有率の高水準閾値。これを超えるとアラーム発動。作成済みプロセス数／最大数の比率で計測。 | `80%`          |
| sysmon.vm.process_low_watermark   | プロセス占有率の低水準閾値。これを下回るとアラーム解除。作成済みプロセス数／最大数の比率で計測。 | `60%`          |
| sysmonn.vm.long_gc                | Long GC監視の有効化                                         | `disabled`     |
| sysmon.vm.long_schedule           | Long Schedule監視の有効化                                  | `disabled`     |
| sysmon.vm.large_heap              | Large Heap監視の有効化                                     | `disabled`     |
| sysmon.vm.busy_dist_port          | Busy Distribution Port監視の有効化                         | `true`        |
| sysmon.vm.busy_port               | Busy Port監視の有効化                                      | `true`        |
| sysmonn.top.num_items             | 監視グループごとのトッププロセス数                         | `10`           |
| sysmon.top.sample_interlval       | トッププロセスのチェック間隔                              | `2s`           |
| sysmon.top.max_procs              | VM内のプロセス数がこの値を超えるとデータ収集を停止          | `1000000`      |

EMQX Enterpriseでは、ライセンスの有効期限が30日未満になるか、接続数が高水準閾値を超えた場合にアラームを発動します。接続数の高水準・低水準閾値は、設定ファイルの以下の設定項目を変更して調整可能です。ライセンス設定の詳細は [License](../configuration/license.md) を参照してください。

| 設定項目                              | 説明                                                         | デフォルト値    |
| ------------------------------------- | ------------------------------------------------------------ | -------------- |
| license.connection_high_watermark_alarm | ライセンスがサポートする最大接続数の高水準閾値。これを超えるとアラーム発動。アクティブ接続数／最大接続数の比率で計測。 | `80%`          |
| license.connection_low_watermark_alarm  | ライセンスがサポートする最大接続数の低水準閾値。これを下回るとアラーム解除。アクティブ接続数／最大接続数の比率で計測。 | `75%`          |
