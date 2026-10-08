# アラーム

EMQXは、CPU使用率、システムおよびプロセスメモリ使用率、プロセス数、ルールエンジンのリソース状態、クラスターのパーティションおよび修復など、内部状態の変化を監視するための組み込みの監視およびアラーム機能を提供しています。EMQXは、これらの変化が閾値を超えたり期待値から逸脱した場合にアラームを発動・記録し、状態が回復するとリストから削除します。

本ページでは、EMQXが提供するアラーム情報の概要、詳細なアラーム情報の取得・確認方法、およびEMQXにおけるアラーム設定と閾値の構成方法を紹介します。監視およびアラーム機能により、運用中の潜在的な問題を通知し続けます。適切な閾値を設定してアラームを構成することで、EMQXの安全性、安定性、信頼性を確保できます。

## アラーム一覧

以下の表は、システム監視中に潜在的な問題を示すために発動可能なアラームを示しています。

::: tip

アラームは、システムへの影響度や重大度に応じて3つのレベルに分類されます。

- **Error（エラー）**: ユーザー設定によるエラー。クライアントはエラーを認識し再試行可能です。

- **Warning（警告）**: 時折発生するエラー。頻発する場合は注意が必要です。

- **Critical（重大）**: クライアントとサーバー間で不可逆的なデータ損失が発生し、通信や業務に支障をきたします。

これらのレベルは開発視点で定義された推奨値であり、ビジネスニーズに応じて独自のアラームレベルを定義可能です。

:::

| **アラーム名**               | レベル    | 説明                                                         | **詳細**                                    | **閾値**                                                    |
| :-------------------------- | -------- | :------------------------------------------------------------ | :------------------------------------------ | :---------------------------------------------------------- |
| high_system_memory_usage    | Warning  | システムメモリ使用率が高い                                   | システムメモリ使用率が約~p%を超えている     | `os_mon.sysmem_high_watermark = 70%`                        |
| high_process_memory_usage   | Warning  | 単一のErlangプロセスのメモリ使用率が高い（システムメモリ使用率の割合） | プロセスメモリ使用率が約~p%を超えている     | `os_mon.procmem_high_watermark = 5%`                        |
| high_cpu_usage              | Warning  | CPU使用率が高い                                              | 約~p%のCPU使用率                            | `os_mon.cpu_high_watermark = 80%` `os_mon.cpu_low_watermark = 60%` |
| too_many_processes          | Warning  | プロセス数が多すぎる                                        | 約~p%のプロセス使用率                        | `vm_mon.process_high_watermark = 80%` `vm_mon.process_low_watermark = 60%` |
| license_quota               | Warning  | ライセンスの接続数が上限を超えている                         | ライセンス：接続数が%を超えている            | `license.connection_high_watermark_alarm = 80%` `license.connection_low_watermark_alarm = 75%` |
| license_expiry              | Critical | ライセンスの期限が切れそう、または期限切れ                     | `ライセンスはYYYY-MM-DDに期限切れです。`, `ライセンスは本日YYYY-MM-DDに期限切れです。`, または `ライセンスはYYYY-MM-DDに期限切れました。` | 残り30日未満、または期限切れ                                |
| partition                  | Critical | ノードでパーティションが発生                                 | ノード~sでパーティションが発生               | -                                                           |
| resource                   | Critical | リソースが切断されている                                    | リソース~s(~s)がダウンしている               | -                                                           |
| conn_congestion            | Critical | 接続プロセスの輻輳                                         | 接続が輻輳している                           | -                                                           |

## アラームの取得

EMQXは、アラームの取得および詳細情報の確認に複数の方法を提供しています。1つはEMQXダッシュボードを介して、アクティブおよび履歴のアラームをユーザーフレンドリーなインターフェースで閲覧する方法です。ここは発動したアラームの概要を簡単に確認できる中央拠点となります。

さらに、MQTTのシステムトピックをサブスクライブしてリアルタイムにシステムアラームの通知を受け取る方法もあります。Webhook連携を利用して、アラームイベントを外部HTTPサービスに送信することも可能です。また、ログやREST APIを通じてアラーム情報にアクセスすることもできます。

### ダッシュボードでアラームを確認する

EMQXダッシュボードで、**Monitoring** -> **Alarms** をクリックします。次に、**Active** タブまたは **History** タブを選択すると、現在アクティブなアラームや過去のアラーム一覧を確認できます。

EMQXダッシュボードでのアラーム管理の詳細は、[アラーム](../dashboard/alarm_dashboard.md)をご覧ください。

<img src="./assets/view-alarms.png" alt="アラームの表示" style="zoom:50%;" />

### システムトピック経由でアラームを取得する

アラームが発動または解除されると、EMQXはMQTTメッセージをシステムトピック `$SYS/brokers/<Node>/alarms/activate` または `$SYS/brokers/<Node>/alarms/deactivate` にパブリッシュします。ユーザーはこれらのトピックをサブスクライブしてアラーム通知を受け取れます。

アラーム通知メッセージのペイロードはJSON形式で、以下のフィールドを含みます。

| フィールド名         | 型                | 説明                                                         |
| -------------------- | ----------------- | ------------------------------------------------------------ |
| `name`               | string            | アラーム名                                                   |
| `details`            | object            | アラームの詳細                                               |
| `message`            | string            | 人間が読みやすいアラームの説明                              |
| `activate_at`        | integer           | アラーム発動時刻をマイクロ秒単位のUNIXタイムスタンプで表現  |
| `deactivate_at`      | integer / string  | アラーム解除時刻をマイクロ秒単位のUNIXタイムスタンプで表現。発動中のアラームは `infinity` となる。 |
| `activated`          | boolean           | アラームが発動中かどうか                                    |

システムメモリ使用率が高いアラームの例を挙げると、以下のようなアラームメッセージを受け取ります。

<img src="./assets/alarm_activate_msg.png" alt="アラームメッセージ" style="zoom:50%;" />

アラームは重複して報告されません。例えば高CPU使用率のアラームが発動中の場合、同じ種類のアラームは再度発生しません。監視対象の指標が正常に戻ると自動的にアラームは解除されますが、手動で解除することも可能です。

### ログからアラームを取得する

アラームの発動・解除はログ（コンソールまたはファイル）に記録できます。メッセージ送信やイベント処理の失敗時に詳細情報をログに出力し、ログ解析によるアラート検知にも利用可能です。以下はログに出力された詳細なアラーム情報の例です。ログレベルは `warning` で、`msg` フィールドは `alarm_is_activated` または `alarm_is_deactivated` となっています。

<img src="./assets/view-alarms-log.png" alt="ログでのアラーム表示" style="zoom:50%;" />

### REST APIでアラームを取得する

APIを通じてアラームの照会や管理が可能です。UIの左側ナビゲーションメニューで **Alarms** をクリックすると、このAPIリクエストを実行できます。EMQX APIの利用方法は[REST API](../api.md)をご参照ください。

<img src="./assets/view-alarms-api.png" alt="APIでのアラーム表示" style="zoom:45%;" />

### Webhook連携によるアラームイベント送信

EMQXバージョン5.8.5以降、ルールエンジンは以下の2つの新しいアラームイベントをサポートしています。

- [$events/sys/alarm_activated](../../develop/data-integration/rule-sql-events-and-fields.md#system-alarm-activated-event-events-sys-alarm-activated)
- [$events/sys/alarm_deactivated](../../develop/data-integration/rule-sql-events-and-fields.md#system-alarm-deactivated-event-events-sys-alarm-deactivated)

これらのイベントにより、Webhook連携を通じて外部HTTPサービスへアラームの発動・解除通知を受け取れます。

Webhook連携の設定方法：

1. EMQXダッシュボードで **Monitoring** -> **Alarms** に移動します。
2. 右上の **Set Up Webhook** ボタンをクリックし、Webhook連携設定ページを開きます。
3. Webhook連携の名前と（任意で）説明を入力します。**Trigger** フィールドには `Alarm Activated` と `Alarm Deactivated` が事前選択されています。
4. 通知を送信するWebhookのURLを入力します。
5. 詳細な設定は[Webhookの作成](../../develop/data-integration/webhook.md)を参照してください。
6. 設定完了後、**Save** をクリックします。

![alarm_webhook_setup](./assets/alarm_webhook_setup.png)

## アラーム設定

アラーム設定には、アラームの動作設定と閾値設定が含まれます。アラームの動作設定はアラームメッセージの表示や保存方法を決定し、閾値設定は潜在的な問題を検知してアラームを発動させるための制限値や数値を定めます。これにより、ビジネスニーズに応じてアラームの動作や閾値をカスタマイズ可能です。

### アラーム動作設定

アラームの動作設定は設定ファイル内の設定項目を変更することでのみ構成できます。以下の表はアラーム動作設定に利用可能な設定項目です。

| 設定項目               | 説明                                                         | デフォルト値          | オプション値       |
| ---------------------- | ------------------------------------------------------------ | -------------------- | ------------------ |
| alarm.actions          | アラーム発動・解除時に、ログ（コンソールまたはファイル）への書き込みおよびシステムトピック `$SYS/brokers/<node_name>/alarms/activate` と `$SYS/brokers/<node_name>/alarms/deactivate` へのMQTTメッセージのパブリッシュを行うアクション。 | `["log", "publish"]` | -                  |
| alarm.size_limit       | 履歴として保持する解除済みアラームの最大数。上限を超えると最も古い解除済みアラームから削除される。 | `1000`               | `1-3000`           |
| alarm.validity_period  | 解除済みアラームの保持期間。解除直後に削除されず、一定期間経過後に削除される。 | `24h`                | -                  |

### ダッシュボードでのアラーム閾値設定

EMQXダッシュボードでアラーム閾値を設定できます。閾値設定用の **Monitoring** ページを起動する方法は2通りあります。

1. **Alarms** ページで **Setting** ボタンをクリックすると **Monitoring** ページに遷移します。
2. 左側ナビゲーションメニューから **Management** -> **Monitoring** をクリックします。

**Monitoring** -> **System** タブの **Erlang VM** タブを開くと、Erlang仮想マシンのシステムパフォーマンスに関する以下の項目を設定できます。

<img src="./assets/monitoring-system-ee.png" alt="システム監視設定" style="zoom:40%;" />

- **Process limit check interval**: 定期的にプロセス数の制限をチェックする間隔（秒）。デフォルトは `30` 秒です。
- **Process high watermark**: ローカルノードで同時に存在可能なプロセス数の閾値。割合がこの値を超えるとアラームが発動します。デフォルトは `80` パーセントです。
- **Process low watermark**: ローカルノードで同時に存在可能なプロセス数の解除閾値。割合がこの値まで下がるとアラームが解除されます。デフォルトは `60` パーセントです。
- **Enable Long GC monitoring**: デフォルトは無効。有効にすると、Erlangプロセスが長時間ガベージコレクションを行うと警告レベルのログ `long_gc` が出力され、システムトピック `$SYS/sysmon/long_gc` にMQTTメッセージがパブリッシュされます。
- **Enable Long Schedule monitoring**: デフォルトは有効。Erlang VMが長時間スケジュールされたタスクを検出すると警告レベルのログ `long_schedule` が出力されます。タスクの適切なスケジュール時間をテキストボックスで設定可能です。デフォルトは `240` ミリ秒です。
- **Enable Large Heap monitoring**: デフォルトは有効。Erlangプロセスが大きなヒープ領域を消費すると警告レベルのログ `large_heap` が出力され、システムトピック `$SYS/sysmon/large_heap` にMQTTメッセージがパブリッシュされます。ヒープサイズの上限をテキストボックスで設定可能です。デフォルトは `32` MBです。
- **Enable Busy Distribution Port monitoring**: デフォルトは有効。クラスター内の他ノードとの通信に使うRPC接続が過負荷状態になると警告レベルのログ `busy_dis_port` が出力され、システムトピック `$SYS/sysmon/busy_dist_port` にMQTTメッセージがパブリッシュされます。
- **Enable Busy Port monitoring**: デフォルトは有効。ポートが過負荷状態になると警告レベルのログ `busy_port` が出力され、システムトピック `$SYS/sysmon/busy_port` にMQTTメッセージがパブリッシュされます。

設定完了後、**Save Changes** をクリックしてください。

**Operating System** タブをクリックすると、システムパフォーマンスに関する以下の項目を設定できます。

<img src="./assets/monitoring-operating-system-ee.png" alt="OS監視設定" style="zoom:40%;" />

- **The time interval of the periodic CPU check**: CPU使用率を定期的にチェックする間隔（秒）。デフォルトは `60` 秒です。
- **CPU high watermark**: システムCPU使用率の上限閾値。割合がこの値を超えるとアラームが発動します。デフォルトは `80` パーセントです。
- **CPU low watermark**: システムCPU使用率の解除閾値。割合がこの値まで下がるとアラームが解除されます。デフォルトは `60` パーセントです。
- **Mem check interval**: メモリ使用率を定期的にチェックする間隔（秒）。デフォルトは `60` 秒で有効です。
- **SysMem high watermark**: システムメモリ使用率の上限閾値。割合がこの値を超えるとアラームが発動します。デフォルトは `70`% です。
- **ProcMem high watermark**: 単一Erlangプロセスのメモリ使用率の上限閾値。割合がこの値を超えるとアラームが発動します。デフォルトは `5`% です。

設定完了後、**Save Changes** をクリックしてください。

### 設定ファイルでのアラーム閾値設定

設定ファイル内のアラーム閾値設定項目を変更することでも閾値を設定可能です。現在変更可能な設定項目は以下の通りです。

| 設定項目                          | 説明                                                         | デフォルト値    |
| -------------------------------- | ------------------------------------------------------------ | -------------- |
| sysmon.os.cpu_check_interval      | CPU使用率のチェック間隔                                      | `60s`          |
| sysmon.os.cpu_high_watermark      | CPU使用率の高水準閾値。これを超えるとアラームが発動する。    | `80%`          |
| sysmon.os.cpu_low_watermark       | CPU使用率の低水準閾値。これを下回るとアラームが解除される。  | `60%`          |
| sysmon.os.mem_check_interval      | メモリ使用率のチェック間隔                                  | `60s`          |
| sysmon.os.sysmem_high_watermark   | システムメモリ使用率の高水準閾値。これを超えるとアラームが発動する。 | `70%`          |
| sysmon.os.procmem_high_watermark  | プロセスメモリ使用率の高水準閾値。単一プロセスの使用率がこれを超えるとアラームが発動する。 | `5%`           |
| sysmon.vm.process_check_interval  | プロセス数のチェック間隔                                    | `30s`          |
| sysmon.vm.process_high_watermark  | プロセス占有率の高水準閾値。これを超えるとアラームが発動する。作成済みプロセス数/最大数の比率で測定。 | `80%`          |
| sysmon.vm.process_low_watermark   | プロセス占有率の低水準閾値。これを下回るとアラームが解除される。作成済みプロセス数/最大数の比率で測定。 | `60%`          |
| sysmon.vm.long_gc                 | Long GC監視の有効化設定                                    | `disabled`     |
| sysmon.vm.long_schedule           | Long Schedule監視の有効化設定                              | `disabled`     |
| sysmon.vm.large_heap              | Large Heap監視の有効化設定                                 | `disabled`     |
| sysmon.vm.busy_dist_port          | Busy Distribution Port監視の有効化設定                     | `true`        |
| sysmon.vm.busy_port               | Busy Port監視の有効化設定                                  | `true`        |
| sysmon.top.num_items              | 監視グループごとの上位プロセス数                           | `10`           |
| sysmon.top.sample_interval        | 上位プロセスのチェック間隔                                | `2s`           |
| sysmon.top.max_procs              | VM内のプロセス数がこの値を超えるとデータ収集を停止する。     | `1000000`      |

EMQX Enterpriseは、ライセンスの期限が30日未満になるか、接続数が高水準閾値を超えた場合にアラームを発動します。接続数の高水準・低水準閾値は設定ファイルの以下の項目を変更して調整可能です。ライセンス設定の詳細は[ライセンス](../configuration/license.md)をご参照ください。

| 設定項目                                | 説明                                                         | デフォルト値    |
| --------------------------------------- | ------------------------------------------------------------ | -------------- |
| license.connection_high_watermark_alarm | ライセンスがサポートする最大接続数の高水準閾値。これを超えるとアラームが発動する。アクティブ接続数/最大接続数の比率で測定。 | `80%`          |
| license.connection_low_watermark_alarm  | ライセンスがサポートする最大接続数の低水準閾値。これを下回るとアラームが解除される。アクティブ接続数/最大接続数の比率で測定。 | `75%`          |
