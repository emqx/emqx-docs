# アラーム

EMQX は、CPU 使用率、システムおよびプロセスメモリ使用率、プロセス数、ルールエンジンのリソース状態、クラスターのパーティションおよび修復など、内部状態の変化を監視するための組み込みの監視およびアラーム機能を提供しています。EMQX は、これらの変化が閾値を超えたり期待値から逸脱した場合にアラームを発生・記録し、正常に戻った場合にはリストから削除します。

本ページでは、EMQX が提供するアラーム情報、詳細なアラーム情報の取得および確認方法、EMQX におけるアラーム設定および閾値の設定方法について紹介します。監視およびアラーム機能により、運用中の潜在的な問題を通知し続けることが可能です。適切な閾値を設定しアラームを構成することで、EMQX の安全性、安定性、信頼性を確保できます。

## アラーム一覧

以下の表は、システム監視中に潜在的な問題を示すためにトリガーされる可能性のあるアラームを示しています。

::: tip

アラームは、システムへの影響度や重大度に応じて3つのレベルに分類されます。

- **Error（エラー）**: ユーザーの設定ミスによるエラー。クライアントはエラーを認識し再試行可能。
- **Warning（警告）**: 断続的なエラー。頻発する場合は注意が必要。
- **Critical（重大）**: クライアントとサーバー間で不可逆的なデータ損失が発生し、通信や業務に支障をきたす。

これらのレベルは開発視点で定義されており、あくまで推奨です。ビジネスニーズに応じて独自のアラームレベルを定義可能です。

:::

| **アラーム名**               | レベル     | 説明                                                        | **詳細**                                     | **閾値**                                                     |
| :-------------------------- | --------- | :---------------------------------------------------------- | :------------------------------------------- | :----------------------------------------------------------- |
| high_system_memory_usage    | Warning   | システムメモリ使用率が高い                                  | システムメモリ使用率が約 ~p% を超えている     | `os_mon.sysmem_high_watermark = 70%`                         |
| high_process_memory_usage   | Warning   | 単一の Erlang プロセスメモリ使用率が高い（システムメモリ使用率の割合） | プロセスメモリ使用率が約 ~p% を超えている     | `os_mon.procmem_high_watermark = 5%`                         |
| high_cpu_usage              | Warning   | CPU 使用率が高い                                            | 約 ~p% の CPU 使用率                          | `os_mon.cpu_high_watermark = 80%` `os_mon.cpu_low_watermark = 60%` |
| too_many_processes          | Warning   | プロセス数が多すぎる                                       | 約 ~p% のプロセス使用率                       | `vm_mon.process_high_watermark = 80%` `vm_mon.process_low_watermark = 60%` |
| license_quota               | Warning   | ライセンスの接続数が上限を超えている                       | ライセンス：接続数が % を超えている            | `license.connection_high_watermark_alarm = 80%` `license.connection_low_watermark_alarm = 75%` |
| license_expiry              | Critical  | ライセンスがまもなく期限切れ、または期限切れである         | `ライセンスは YYYY-MM-DD に期限切れです。`、`ライセンスは本日 YYYY-MM-DD に期限切れです。`、または `ライセンスは YYYY-MM-DD に期限切れました。` | 残り30日未満、または期限切れ                                   |
| license_tps                 | Warning   | TPS 使用率がライセンス上限を超えている                      | ライセンス：TPS 上限（例：10）を超過           | -                                                            |
| partition                   | Critical  | ノードでパーティションが発生している                         | ノード ~s でパーティションが発生               | -                                                            |
| resource                    | Critical  | リソースが切断されている                                   | リソース ~s(~s) がダウンしている               | -                                                            |
| conn_congestion             | Critical  | 接続プロセスの輻輳が発生している                            | 接続が輻輳している                             | -                                                            |

## アラームの取得

EMQX では、アラームを取得し詳細情報を確認するための複数の方法を提供しています。1つは EMQX ダッシュボードを通じて、アクティブおよび履歴のアラームをユーザーフレンドリーなインターフェースで確認する方法です。ここではトリガーされたアラームの概要を一元的に把握できます。

また、MQTT のシステムトピックをサブスクライブしてリアルタイムにシステムアラームの通知を受け取ることも可能です。さらに Webhook 統合を利用して、アラームイベントを外部の HTTP サービスに送信し、追加処理を行うこともできます。アラームはログや REST API を通じて取得することも可能です。

### ダッシュボードでアラームを確認する

EMQX ダッシュボードで、**Monitoring** -> **Alarms** をクリックします。次に、**Active** または **History** タブを選択して、現在アクティブなアラームや履歴アラームの一覧を表示します。

EMQX ダッシュボードでのアラーム管理の詳細な手順については、[アラーム](../dashboard/alarm_dashboard.md) を参照してください。

<img src="./assets/view-alarms.png" alt="アラームの表示" style="zoom:50%;" />

### システムトピック経由でアラームを取得する

アラームが発生または解除されると、EMQX はシステムトピック `$SYS/brokers/<Node>/alarms/activate` または `$SYS/brokers/<Node>/alarms/deactivate` に MQTT メッセージをパブリッシュします。ユーザーはこれらのトピックをサブスクライブしてアラーム通知を受け取れます。

アラーム通知メッセージのペイロードは JSON 形式で、以下のフィールドを含みます。

| フィールド名         | 型               | 説明                                                        |
| -------------------- | ---------------- | ----------------------------------------------------------- |
| `name`               | string           | アラーム名                                                 |
| `details`            | object           | アラームの詳細情報                                         |
| `message`            | string           | 人間が読みやすいアラームの説明文                           |
| `activate_at`        | integer          | アラームが発生した時刻をマイクロ秒単位の UNIX タイムスタンプで表現 |
| `deactivate_at`      | integer / string | アラームが解除された時刻をマイクロ秒単位の UNIX タイムスタンプで表現。アクティブなアラームの場合は `infinity`。 |
| `activated`          | boolean          | アラームが発生中かどうか                                   |

高いシステムメモリ使用率のアラームを例にすると、以下のようなアラームメッセージを受け取ります。

<img src="./assets/alarm_activate_msg.png" alt="アラームメッセージ" style="zoom:50%;" />

同じ種類のアラームは繰り返し通知されません。例えば高 CPU 使用率のアラームが発生すると、同じタイプのアラームは追加で発生しません。監視対象の指標が正常に戻ると自動的にアラームは解除されますが、手動で解除することも可能です。

### ログからアラームを取得する

アラームの発生および解除はログ（コンソールまたはファイル）に記録されます。メッセージ送信やイベント処理で障害が発生した場合、詳細情報がログに記録され、ログ解析を通じてアラートを検知することも可能です。以下はログに出力された詳細なアラーム情報の例です。ログレベルは `warning` で、`msg` フィールドは `alarm_is_activated` または `alarm_is_deactivated` となっています。

<img src="./assets/view-alarms-log.png" alt="ログでのアラーム表示" style="zoom:50%;" />

### REST API でアラームを取得する

API を通じてアラームの照会や管理が可能です。UI の左側ナビゲーションメニューで **Alarms** をクリックすると、この API リクエストが実行されます。EMQX API の利用方法については [REST API](../api.md) を参照してください。

<img src="./assets/view-alarms-api.png" alt="APIでのアラーム表示" style="zoom:45%;" />

### Webhook 統合によるアラームイベント送信

EMQX バージョン 5.8.5 以降、ルールエンジンは以下の2つの新しいアラームイベントをサポートしています。

- [$events/sys/alarm_activated](../../develop/data-integration/rule-sql-events-and-fields.md#system-alarm-activated-event-events-sys-alarm-activated)
- [$events/sys/alarm_deactivated](../../develop/data-integration/rule-sql-events-and-fields.md#system-alarm-deactivated-event-events-sys-alarm-deactivated)

これらのイベントにより、Webhook 統合を通じて外部 HTTP サービスへアラームの発生・解除通知を送信できます。

Webhook 統合の設定手順は以下の通りです。

1. EMQX ダッシュボードで **Monitoring** -> **Alarms** に移動します。
2. 右上の **Set Up Webhook** ボタンをクリックして Webhook 統合設定ページを開きます。
3. Webhook 統合の名前とメモ（任意）を入力します。**Trigger** フィールドには `Alarm Activated` と `Alarm Deactivated` が事前選択されています。
4. 通知を送信したい Webhook URL を入力します。
5. 詳細な設定については [Create Webhook](../../develop/data-integration/webhook.md) を参照してください。
6. 設定が完了したら **Save** をクリックします。

![alarm_webhook_setup](./assets/alarm_webhook_setup.png)

## アラーム設定

アラーム設定は、アラームの動作設定と閾値設定に分かれます。アラームの動作設定はアラームメッセージの表示や保存方法を決定し、閾値設定は潜在的な問題が検出された際にアラームを発生させるための制限値や値を定めます。これにより、ビジネスニーズに合わせてアラームの設定や閾値をカスタマイズ可能です。

### アラーム動作設定の構成

アラームの動作設定は、設定ファイル内の設定項目を変更することでのみ構成可能です。以下の表は、アラーム動作設定に利用できる設定項目を示しています。

| 設定項目              | 説明                                                        | デフォルト値          | 選択可能な値       |
| --------------------- | ----------------------------------------------------------- | -------------------- | ------------------ |
| alarm.actions         | アラーム発生・解除時にログ（コンソールまたはファイル）への書き込みおよびシステムトピック `$SYS/brokers/<node_name>/alarms/activate` と `$SYS/brokers/<node_name>/alarms/deactivate` への MQTT メッセージパブリッシュを行うアクション。 | `["log", "publish"]` | -                  |
| alarm.size_limit      | 履歴として保持する解除済みアラームの最大数。上限を超えると最も古い解除済みアラームから削除される。 | `1000`               | `1-3000`           |
| alarm.validity_period | 解除済みアラームの保持期間。解除直後に削除せず一定期間保持する。 | `24h`                | -                  |

### ダッシュボードでアラーム閾値を設定する

アラーム閾値は EMQX ダッシュボード上で設定可能です。閾値設定用の **Monitoring** ページを起動する方法は2通りあります。

1. **Alarms** ページで **Setting** ボタンをクリックすると、**Monitoring** ページに遷移します。
2. 左側ナビゲーションメニューから **Management** -> **Monitoring** をクリックします。

**Monitoring** -> **System** タブの中の **Erlang VM** タブでは、Erlang 仮想マシンのシステムパフォーマンスに関する以下の項目を設定できます。

<img src="./assets/monitoring-system-ee.png" alt="Erlang VM の監視設定" style="zoom:40%;" />

- **Process limit check interval**: プロセス数の定期チェック間隔（秒）。デフォルトは `30` 秒。
- **Process high watermark**: ローカルノードで同時に存在可能なプロセス数の閾値（割合）。この値を超えるとアラームが発生。デフォルトは `80%`。
- **Process low watermark**: ローカルノードで同時に存在可能なプロセス数の閾値（割合）。この値まで下がるとアラームが解除。デフォルトは `60%`。
- **Enable Long GC monitoring**: デフォルトは無効。有効にすると、Erlang プロセスが長時間ガベージコレクションを行った場合に警告ログ `long_gc` を出力し、システムトピック `$SYS/sysmon/long_gc` に MQTT メッセージをパブリッシュ。
- **Enable Long Schedule monitoring**: デフォルトで有効。Erlang VM が長時間スケジュールされたタスクを検出すると警告ログ `long_schedule` を出力。タスクの適切なスケジュール時間をテキストボックスで設定可能。デフォルトは `240` ミリ秒。
- **Enable Large Heap monitoring**: デフォルトで有効。Erlang プロセスが大量のヒープメモリを消費した場合に警告ログ `large_heap` を出力し、システムトピック `$SYS/sysmon/large_heap` に MQTT メッセージをパブリッシュ。ヒープサイズの上限をテキストボックスで設定可能。デフォルトは `32` MB。
- **Enable Busy Distribution Port monitoring**: デフォルトで有効。クラスター内の他ノードと通信するための RPC 接続が過負荷になると警告ログ `busy_dis_port` を出力し、システムトピック `$SYS/sysmon/busy_dist_port` に MQTT メッセージをパブリッシュ。
- **Enable Busy Port monitoring**: デフォルトで有効。ポートが過負荷になると警告ログ `busy_port` を出力し、システムトピック `$SYS/sysmon/busy_port` に MQTT メッセージをパブリッシュ。

設定完了後、**Save Changes** をクリックします。

**Operating System** タブでは、システムパフォーマンスに関する以下の項目を設定できます。

<img src="./assets/monitoring-operating-system-ee.png" alt="OS の監視設定" style="zoom:40%;" />

- **The time interval of the periodic CPU check**: CPU 使用率の定期チェック間隔（秒）。デフォルトは `60` 秒。
- **CPU high watermark**: システム CPU 使用率の上限閾値。これを超えるとアラームが発生。デフォルトは `80%`。
- **CPU low watermark**: システム CPU 使用率の下限閾値。これを下回るとアラームが解除。デフォルトは `60%`。
- **Mem check interval**: メモリ使用率の定期チェック間隔（秒）。デフォルトで有効。デフォルト値は `60` 秒。
- **SysMem high watermark**: システム全体のメモリ使用率の上限閾値。これを超えるとアラームが発生。デフォルトは `70%`。
- **ProcMem high watermark**: 単一の Erlang プロセスが使用するメモリ使用率の上限閾値。これを超えるとアラームが発生。デフォルトは `5%`。

設定完了後、**Save Changes** をクリックします。

### 設定ファイルでアラーム閾値を設定する

設定ファイル内のアラーム閾値設定項目を変更することでも、アラーム閾値を構成できます。現在変更可能な設定項目は以下の通りです。

| 設定項目                         | 説明                                                        | デフォルト値    |
| -------------------------------- | ----------------------------------------------------------- | -------------- |
| sysmon.os.cpu_check_interval      | CPU 使用率のチェック間隔                                    | `60s`          |
| sysmon.os.cpu_high_watermark      | CPU 使用率の高水準閾値。これを超えるとアラームが発生。      | `80%`          |
| sysmon.os.cpu_low_watermark       | CPU 使用率の低水準閾値。これを下回るとアラームが解除。      | `60%`          |
| sysmon.os.mem_check_interval      | メモリ使用率のチェック間隔                                  | `60s`          |
| sysmon.os.sysmem_high_watermark   | システムメモリ使用率の高水準閾値。これを超えるとアラームが発生。 | `70%`          |
| sysmon.os.procmem_high_watermark  | プロセスメモリ使用率の高水準閾値。単一プロセスの使用率がこれを超えるとアラームが発生。 | `5%`           |
| sysmon.vm.process_check_interval  | プロセス数のチェック間隔                                   | `30s`          |
| sysmon.vm.process_high_watermark  | プロセス占有率の高水準閾値。これを超えるとアラームが発生。作成済みプロセス数／最大数の比率で測定。 | `80%`          |
| sysmon.vm.process_low_watermark   | プロセス占有率の低水準閾値。これを下回るとアラームが解除。作成済みプロセス数／最大数の比率で測定。 | `60%`          |
| sysmon.vm.long_gc                 | Long GC 監視の有効化設定                                   | `disabled`     |
| sysmon.vm.long_schedule           | Long Schedule 監視の有効化設定                             | `disabled`     |
| sysmon.vm.large_heap              | Large Heap 監視の有効化設定                                | `disabled`     |
| sysmon.vm.busy_dist_port          | Busy Distribution Port 監視の有効化設定                    | `true`        |
| sysmon.vm.busy_port               | Busy Port 監視の有効化設定                                 | `true`        |
| sysmon.top.num_items              | 監視グループごとのトッププロセス数                         | `10`           |
| sysmon.top.sample_interval        | トッププロセスのチェック間隔                               | `2s`           |
| sysmon.top.max_procs              | VM 内のプロセス数がこの値を超えた場合、データ収集を停止    | `1000000`      |

EMQX Enterprise では、ライセンスの期限が30日未満になるか、接続数が上限を超えるとアラームが発生します。接続数の高水準・低水準閾値は設定ファイル内の以下の設定項目を変更して調整可能です。ライセンス設定の詳細は [License](../configuration/license.md) を参照してください。

| 設定項目                                | 説明                                                        | デフォルト値    |
| --------------------------------------- | ----------------------------------------------------------- | -------------- |
| license.connection_high_watermark_alarm | ライセンスがサポートする最大接続数の高水準閾値。これを超えるとアラームが発生。アクティブ接続数／最大接続数の比率で測定。 | `80%`          |
| license.connection_low_watermark_alarm  | ライセンスがサポートする最大接続数の低水準閾値。これを下回るとアラームが解除。アクティブ接続数／最大接続数の比率で測定。 | `75%`          |
