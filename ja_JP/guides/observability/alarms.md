# アラーム

EMQX は、CPU 使用率、システムおよびプロセスメモリ使用率、プロセス数、ルールエンジンのリソース状況、クラスターのパーティションおよび修復など、内部状態の変化を監視するための組み込みの監視およびアラーム機能を提供しています。これらの変化が閾値を超えたり期待値から逸脱した場合に EMQX はアラームをトリガーして記録し、状態が復旧するとリストから削除します。

本ページでは、EMQX が提供するアラーム情報、詳細なアラーム情報の取得および確認方法、そして EMQX におけるアラーム設定および閾値の設定方法について紹介します。監視およびアラーム機能により、運用中の潜在的な問題を通知し続けることが可能です。適切な閾値を設定してアラームを構成することで、EMQX の安全性、安定性、信頼性を確保できます。

## アラーム一覧

以下の表は、システム監視中に潜在的な問題を示すためにトリガーされる可能性のあるアラームを示しています。

::: tip

アラームはシステムへの影響度や重大度に応じて3つのレベルに分かれます：

- **Error（エラー）**：ユーザー設定によるエラー。クライアントはエラーを認識しリトライ可能です。

- **Warning（警告）**：発生頻度が高い場合は注意が必要な一時的なエラー。

- **Critical（重大）**：クライアントとサーバー間で不可逆なデータ損失が発生し、通信や業務に支障をきたします。

これらのレベルは開発視点で定義されており、あくまで推奨です。ビジネスニーズに応じて独自のアラームレベルを定義可能です。

:::

| **アラーム**               | レベル    | 説明                                                         | **詳細**                                    | **閾値**                                                    |
| :------------------------ | -------- | :----------------------------------------------------------- | :------------------------------------------- | :----------------------------------------------------------- |
| high_system_memory_usage  | Warning  | システムメモリ使用率が高すぎる                              | システムメモリ使用率が約 ~p% を超えている    | `os_mon.sysmem_high_watermark = 70%`                         |
| high_process_memory_usage | Warning  | 単一の Erlang プロセスのメモリ使用率が高すぎる（システムメモリ使用率の割合） | プロセスメモリ使用率が約 ~p% を超えている    | `os_mon.procmem_high_watermark = 5%`                         |
| high_cpu_usage            | Warning  | CPU 使用率が高すぎる                                        | 約 ~p% の CPU 使用率                         | `os_mon.cpu_high_watermark = 80%` `os_mon.cpu_low_watermark = 60%` |
| too_many_processes        | Warning  | プロセス数が多すぎる                                       | 約 ~p% のプロセス使用率                      | `vm_mon.process_high_watermark = 80%` `vm_mon.process_low_watermark = 60%` |
| license_quota             | Warning  | ライセンスの接続数がクォータを超過している                  | ライセンス：接続数が % を超過している         | `license.connection_high_watermark_alarm = 80%` `license.connection_low_watermark_alarm = 75%` |
| license_expiry            | Critical | ライセンスがまもなく期限切れ、または期限切れである           | `ライセンスは YYYY-MM-DD に期限切れです。`、`ライセンスは本日 YYYY-MM-DD に期限切れです。`、または `ライセンスは YYYY-MM-DD に期限切れました。` | 30日未満で期限切れ、または既に期限切れ                        |
| license_tps               | Warning  | TPS 使用率がライセンス上限を超過している                    | ライセンス：TPS 上限（例：10）を超過         | -                                                            |
| partition                 | Critical | ノードでパーティションが発生している                         | ノード ~s でパーティションが発生している      | -                                                            |
| resource                  | Critical | リソースが切断されている                                    | リソース ~s(~s) がダウンしている             | -                                                            |
| conn_congestion           | Critical | 接続プロセスの輻輳が発生している                            | 接続が輻輳している                           | -                                                            |

## アラームの取得

EMQX はアラームの取得および詳細情報の確認に複数の方法を提供しています。1つは EMQX ダッシュボードを通じて、アクティブなアラームと履歴アラームの両方をユーザーフレンドリーなインターフェースで閲覧できる方法です。これにより、トリガーされたアラームの概要を簡単に把握できます。

また、MQTT のシステムトピックをサブスクライブしてリアルタイムにシステムアラームの通知を受け取る方法もあります。さらに、Webhook 統合を利用してアラームイベントを外部の HTTP サービスに送信し、追加処理を行うことも可能です。アラームはログや REST API からもアクセスできます。

### ダッシュボードでアラームを確認する

EMQX ダッシュボードで **Monitoring** -> **Alarms** をクリックし、**Active** または **History** タブを選択すると、現在アクティブなアラームや履歴アラームの一覧を表示できます。

EMQX ダッシュボードでのアラーム管理の完全なガイドは [Alarms](../dashboard/alarm_dashboard.md) を参照してください。

<img src="./assets/view-alarms.png" alt="アラームの表示" style="zoom:50%;" />

### システムトピック経由でアラームを取得する

アラームがトリガーまたは解除されると、EMQX は MQTT メッセージをシステムトピック `$SYS/brokers/<Node>/alarms/activate` または `$SYS/brokers/<Node>/alarms/deactivate` にパブリッシュします。ユーザーはこれらのトピックをサブスクライブしてアラーム通知を受け取れます。

アラーム通知メッセージのペイロードは JSON 形式で、以下のフィールドを含みます：

| フィールド名       | 型               | 説明                                                         |
| ------------------ | ---------------- | ------------------------------------------------------------ |
| `name`             | string           | アラーム名                                                   |
| `details`          | object           | アラームの詳細情報                                          |
| `message`          | string           | 人間が読みやすいアラームの説明                              |
| `activate_at`      | integer          | アラームが有効化された時刻をマイクロ秒単位の UNIX タイムスタンプで表現 |
| `deactivate_at`    | integer / string | アラームが無効化された時刻をマイクロ秒単位の UNIX タイムスタンプで表現。有効化中のアラームは `infinity` の値を持つ。 |
| `activated`        | boolean          | アラームが有効かどうか                                      |

システムメモリ使用率が高いアラームの例では、以下のようなアラームメッセージを受け取ります：

<img src="./assets/alarm_activate_msg.png" alt="アラームメッセージ" style="zoom:50%;" />

アラームは繰り返し報告されません。つまり、高 CPU 使用率のアラームが一度有効化されると、同じ種類のアラームは再度生成されません。監視対象の指標が正常に戻ると自動的にアラームは無効化されるか、手動で無効化することも可能です。

### ログからアラームを取得する

アラームの有効化および無効化はログ（コンソールまたはファイル）に書き込まれます。メッセージ送信やイベント処理中に障害が発生した場合、詳細情報がログに記録され、ログ解析を通じてアラートを検知することも可能です。以下の例はログに出力された詳細なアラーム情報を示しています：

ログレベルは `warning` で、`msg` フィールドは `alarm_is_activated` および `alarm_is_deactivated` です。

<img src="./assets/view-alarms-log.png" alt="ログでのアラーム表示" style="zoom:50%;" />

### REST API でアラームを取得する

API を通じてアラームの照会および管理が可能です。UI の左側ナビゲーションメニューの **Alarms** をクリックするとこの API リクエストが実行されます。EMQX API の利用方法は [REST API](../api.md) を参照してください。

<img src="./assets/view-alarms-api.png" alt="APIでのアラーム表示" style="zoom:45%;" />

### Webhook 統合によるアラームイベント送信

EMQX バージョン 5.8.5 以降、ルールエンジンは以下の2つの新しいアラームイベントをサポートしています：

- [$events/sys/alarm_activated](../../develop/data-integration/rule-sql-events-and-fields.md#system-alarm-activated-event-events-sys-alarm-activated)
- [$events/sys/alarm_deactivated](../../develop/data-integration/rule-sql-events-and-fields.md#system-alarm-deactivated-event-events-sys-alarm-deactivated)

これらのイベントを利用すると、Webhook 統合を通じて外部 HTTP サービスにアラームの発生・解除通知を受け取ることができます。

Webhook 統合の設定方法：

1. EMQX ダッシュボードで **Monitoring** -> **Alarms** に移動します。
2. 右上の **Set Up Webhook** ボタンをクリックして Webhook 統合設定ページを開きます。
3. Webhook 統合の名前とメモ（任意）を入力します。**Trigger** フィールドには `Alarm Activated` と `Alarm Deactivated` が事前選択されています。
4. 通知を送信したい Webhook URL を入力します。
5. 詳細な設定については [Create Webhook](../../develop/data-integration/webhook.md) を参照してください。
6. 設定が完了したら **Save** をクリックします。

![alarm_webhook_setup](./assets/alarm_webhook_setup.png)

## アラーム設定

アラーム設定には、アラームの動作設定と閾値設定が含まれます。アラームの動作設定はアラームメッセージの表示や保存方法を決定し、閾値設定は潜在的な問題を検知してアラームをトリガーするための制限値や基準を定めます。これにより、ビジネスニーズに合わせてアラームの設定や閾値をカスタマイズできます。

### アラーム動作設定の構成

アラームの動作設定は設定ファイルの設定項目を変更することでのみ構成可能です。以下の表はアラーム動作設定に利用できる設定項目を示しています。

| 設定項目              | 説明                                                         | デフォルト値          | 選択可能な値       |
| --------------------- | ------------------------------------------------------------ | -------------------- | ------------------ |
| alarm.actions         | アラームが有効化または無効化された際に、ログ（コンソールまたはファイル）への書き込みと、システムトピック `$SYS/brokers/<node_name>/alarms/activate` および `$SYS/brokers/<node_name>/alarms/deactivate` への MQTT メッセージパブリッシュを行うアクション。 | `["log", "publish"]` | -                  |
| alarm.size_limit      | 無効化されたアラームの履歴として保持する最大数。上限を超えると最も古い無効化アラームから削除される。 | `1000`               | `1-3000`           |
| alarm.validity_period | 無効化されたアラームの保持期間。無効化直後に削除されず、一定期間経過後に削除される。 | `24h`                | -                  |

### ダッシュボードでアラーム閾値を設定する

アラーム閾値は EMQX ダッシュボード上で設定可能です。閾値設定用の **Monitoring** ページを起動する方法は2通りあります：

1. **Alarms** ページで **Setting** ボタンをクリックすると **Monitoring** ページに遷移します。
2. 左側ナビゲーションメニューから **Management** -> **Monitoring** をクリックします。

**Monitoring** -> **System** タブの **Erlang VM** タブでは、Erlang 仮想マシンのシステムパフォーマンスに関する以下の項目を設定できます：

<img src="./assets/monitoring-system-ee.png" alt="システム監視設定" style="zoom:40%;" />

- **Process limit check interval**：プロセス数の定期チェック間隔（秒）。デフォルトは `30` 秒。
- **Process high watermark**：ローカルノードで同時に存在可能なプロセス数の閾値。割合がこの値を超えるとアラームが発生。デフォルトは `80%`。
- **Process low watermark**：プロセス数がこの割合まで下がるとアラームが解除される閾値。デフォルトは `60%`。
- **Enable Long GC monitoring**：デフォルト無効。有効化すると Erlang プロセスの長時間ガベージコレクション時に警告ログ `long_gc` を出力し、システムトピック `$SYS/sysmon/long_gc` に MQTT メッセージをパブリッシュ。
- **Enable Long Schedule monitoring**：デフォルト有効。Erlang VM が長時間スケジュールされたタスクを検知すると警告ログ `long_schedule` を出力。タスクの許容時間はテキストボックスで設定可能。デフォルトは `240` ミリ秒。
- **Enable Large Heap monitoring**：デフォルト有効。Erlang プロセスのヒープサイズが大きい場合に警告ログ `large_heap` を出力し、システムトピック `$SYS/sysmon/large_heap` に MQTT メッセージをパブリッシュ。ヒープサイズの閾値はテキストボックスで設定可能。デフォルトは `32` MB。
- **Enable Busy Distribution Port monitoring**：デフォルト有効。クラスター内の他ノードと通信するための RPC 接続が過負荷になると警告ログ `busy_dis_port` を出力し、システムトピック `$SYS/sysmon/busy_dist_port` に MQTT メッセージをパブリッシュ。
- **Enable Busy Port monitoring**：デフォルト有効。ポートが過負荷になると警告ログ `busy_port` を出力し、システムトピック `$SYS/sysmon/busy_port` に MQTT メッセージをパブリッシュ。

設定完了後、**Save Changes** をクリックしてください。

**Operating System** タブでは、システムパフォーマンスに関する以下の項目を設定できます：

<img src="./assets/monitoring-operating-system-ee.png" alt="OS監視設定" style="zoom:40%;" />

- **The time interval of the periodic CPU check**：CPU 使用率の定期チェック間隔（秒）。デフォルトは `60` 秒。
- **CPU high watermark**：システム CPU 使用率の上限閾値。割合が超えるとアラームが発生。デフォルトは `80%`。
- **CPU low watermark**：システム CPU 使用率の下限閾値。割合が下がるとアラームが解除。デフォルトは `60%`。
- **Mem check interval**：メモリ使用率の定期チェック間隔。デフォルト有効で、デフォルト値は `60` 秒。
- **SysMem high watermark**：システムメモリ使用率の上限閾値。割合が超えるとアラームが発生。デフォルトは `70%`。
- **ProcMem high watermark**：単一の Erlang プロセスによるメモリ使用率の上限閾値。割合が超えるとアラームが発生。デフォルトは `5%`。

設定完了後、**Save Changes** をクリックしてください。

### 設定ファイルでアラーム閾値を設定する

設定ファイルの設定項目を変更してアラーム閾値を設定することも可能です。現在変更可能な設定項目は以下の通りです：

| 設定項目                          | 説明                                                         | デフォルト値    |
| --------------------------------- | ------------------------------------------------------------ | -------------- |
| sysmon.os.cpu_check_interval      | CPU 使用率のチェック間隔                                      | `60s`          |
| sysmon.os.cpu_high_watermark      | CPU 使用率の高水準閾値。これを超えるとアラームが発生。        | `80%`          |
| sysmon.os.cpu_low_watermark       | CPU 使用率の低水準閾値。これを下回るとアラームが解除。        | `60%`          |
| sysmon.os.mem_check_interval      | メモリ使用率のチェック間隔                                    | `60s`          |
| sysmon.os.sysmem_high_watermark   | システムメモリ使用率の高水準閾値。これを超えるとアラームが発生。 | `70%`          |
| sysmon.os.procmem_high_watermark  | プロセスメモリ使用率の高水準閾値。単一プロセスの使用率がこれを超えるとアラームが発生。 | `5%`           |
| sysmon.vm.process_check_interval  | プロセス数のチェック間隔                                      | `30s`          |
| sysmon.vm.process_high_watermark  | プロセス占有率の高水準閾値。これを超えるとアラームが発生。作成済みプロセス数/最大数の比率で測定。 | `80%`          |
| sysmon.vm.process_low_watermark   | プロセス占有率の低水準閾値。これを下回るとアラームが解除。作成済みプロセス数/最大数の比率で測定。 | `60%`          |
| sysmon.vm.long_gc                 | Long GC 監視の有効化設定                                     | `disabled`     |
| sysmon.vm.long_schedule           | Long Schedule 監視の有効化設定                               | `disabled`     |
| sysmon.vm.large_heap              | Large Heap 監視の有効化設定                                  | `disabled`     |
| sysmon.vm.busy_dist_port          | Busy Distribution Port 監視の有効化設定                      | `true`        |
| sysmon.vm.busy_port               | Busy Port 監視の有効化設定                                   | `true`        |
| sysmon.top.num_items              | 監視グループごとのトッププロセス数                           | `10`           |
| sysmon.top.sample_interval        | トッププロセスのチェック間隔                                | `2s`           |
| sysmon.top.max_procs              | VM 内のプロセス数がこの値を超えるとデータ収集を停止          | `1000000`      |

EMQX Enterprise はライセンスの期限が30日未満になるか、接続数が高水準閾値を超えるとアラームを発生させます。接続数の高水準／低水準閾値は設定ファイルの以下の設定項目を変更して調整可能です。ライセンス設定の詳細は [License](../configuration/license.md) を参照してください。

| 設定項目                               | 説明                                                         | デフォルト値    |
| ------------------------------------- | ------------------------------------------------------------ | -------------- |
| license.connection_high_watermark_alarm | ライセンスがサポートする最大接続数の高水準閾値。これを超えるとアラームが発生。アクティブ接続数/最大接続数の比率で測定。 | `80%`          |
| license.connection_low_watermark_alarm  | ライセンスがサポートする最大接続数の低水準閾値。これを下回るとアラームが解除。アクティブ接続数/最大接続数の比率で測定。 | `75%`          |
