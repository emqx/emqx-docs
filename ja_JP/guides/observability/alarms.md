# アラーム

EMQX は、CPU 使用率、システムおよびプロセスメモリ使用率、プロセス数、ルールエンジンのリソース状態、クラスターのパーティションおよび修復など、内部状態の変化を監視するための組み込みの監視およびアラーム機能を提供しています。EMQX は、これらの変化が閾値を超えたり期待値から逸脱した場合にアラームを発動し記録し、状態が回復するとリストから削除します。

本ページでは、EMQX が提供するアラーム情報、詳細なアラーム情報の取得および確認方法、ならびに EMQX におけるアラーム設定および閾値の設定方法について紹介します。監視およびアラーム機能により、運用中の潜在的な問題を通知し、適切な閾値を設定することで、EMQX の安全性、安定性、信頼性を確保できます。

## アラーム一覧

以下の表は、システム監視中に潜在的な問題を示すために発動される可能性のあるアラームを一覧にしたものです。

::: tip

アラームは、システムへの影響度や重大度に応じて以下の3つのレベルがあります。

- **Error（エラー）**: ユーザー設定によるエラー。クライアントはエラーを認識し再試行可能。
- **Warning（警告）**: 断続的に発生するエラー。頻発する場合は注意が必要。
- **Critical（重大）**: クライアントとサーバー間で不可逆的なデータ損失が発生し、通信や業務に支障をきたす。

これらのレベルは開発視点で定義されており、あくまで推奨です。ビジネスニーズに応じて独自のアラームレベルを定義可能です。

:::

| **アラーム**                        | レベル    | 説明                                                         | **詳細**                                    | **閾値**                                                      |
| :--------------------------------- | -------- | :------------------------------------------------------------ | :------------------------------------------ | :------------------------------------------------------------ |
| high_system_memory_usage            | Warning  | システムメモリ使用率が高い                                   | システムメモリ使用率が約 ~p% を超えています | `os_mon.sysmem_high_watermark = 70%`                         |
| high_process_memory_usage           | Warning  | 単一の Erlang プロセスのメモリ使用率が高い（システムメモリ使用率の割合） | プロセスメモリ使用率が約 ~p% を超えています | `os_mon.procmem_high_watermark = 5%`                         |
| high_cpu_usage                      | Warning  | CPU 使用率が高い                                            | 約 ~p% の CPU 使用率                        | `os_mon.cpu_high_watermark = 80%` `os_mon.cpu_low_watermark = 60%` |
| too_many_processes                  | Warning  | プロセス数が多すぎる                                       | 約 ~p% のプロセス使用率                     | `vm_mon.process_high_watermark = 80%` `vm_mon.process_low_watermark = 60%` |
| license_quota                       | Warning  | ライセンスの接続数が上限を超えている                       | ライセンス：接続数が % を超えています        | `license.connection_high_watermark_alarm = 80%` `license.connection_low_watermark_alarm = 75%` |
| license_expiry                      | Critical | ライセンスが期限切れ                                         | ライセンスは % に期限切れになります          | -                                                            |
| mnesia_transaction_manager_overload | Warning  | mnesia が過負荷状態。メールボックスサイズ：N               | メールボックスサイズ = N                     | `sysmon.mnesia_tm_mailbox_threshold = 500`                   |
| broker_pool_overload                | Warning  | ブローカープールが過負荷状態。メールボックスサイズ：N       | メールボックスサイズ = N                     | `sysmon.broker_pool_mailbox_threshold = 500`                 |
| partition                           | Critical | ノードでパーティションが発生                                 | ノード ~s でパーティションが発生しています  | -                                                            |
| resource                            | Critical | リソースが切断されている                                   | リソース ~s(~s) がダウンしています           | -                                                            |
| conn_congestion                     | Critical | 接続プロセスが輻輳している                                 | 接続が輻輳しています                         | -                                                            |

## アラームの取得

EMQX では、アラームを取得し詳細情報を確認するための複数の方法を提供しています。1つは EMQX ダッシュボードを使い、アクティブおよび履歴のアラームをユーザーフレンドリーなインターフェースで閲覧する方法です。ここは発動されたアラームの概要を簡単に確認できる中心的な場所となります。

また、MQTT のシステムトピックをサブスクライブしてリアルタイムにシステムアラームの通知を受け取る方法もあります。さらに Webhook 統合を利用して、アラームイベントを外部の HTTP サービスに送信することも可能です。アラームはログや REST API からもアクセスできます。

### ダッシュボードでアラームを確認する

EMQX ダッシュボードで、**Monitoring** -> **Alarms** をクリックします。次に、**Active** または **History** タブを選択すると、現在アクティブなアラームおよび過去のアラームの一覧を確認できます。

<img src="./assets/view-alarms.png" alt="アラームの表示" style="zoom:50%;" />

### システムトピックでアラームを取得する

アラームが発動または解除されると、EMQX は MQTT メッセージをシステムトピック `$SYS/brokers/<Node>/alarms/activate` または `$SYS/brokers/<Node>/alarms/deactivate` にパブリッシュします。ユーザーはこれらのトピックをサブスクライブしてアラーム通知を受け取れます。

アラーム通知メッセージのペイロードは JSON 形式で、以下のフィールドを含みます。

| フィールド名         | 型               | 説明                                                         |
| -------------------- | ---------------- | ------------------------------------------------------------ |
| `name`               | string           | アラーム名                                                   |
| `details`            | object           | アラームの詳細                                               |
| `message`            | string           | 人間が読みやすいアラームの説明                              |
| `activate_at`        | integer          | アラーム発動時刻をマイクロ秒単位の UNIX タイムスタンプで表現 |
| `deactivate_at`      | integer / string | アラーム解除時刻をマイクロ秒単位の UNIX タイムスタンプで表現。発動中のアラームは `infinity` となる。 |
| `activated`          | boolean          | アラームが発動中かどうか                                    |

システムメモリ使用率が高いアラームの例を挙げると、以下のようなアラームメッセージを受け取ります。

<img src="./assets/alarm_activate_msg.png" alt="アラームメッセージ" style="zoom:50%;" />

同じ種類のアラームは繰り返し発報されません。例えば高 CPU 使用率のアラームが発動中の場合、同種のアラームは再度発生しません。監視対象の指標が正常に戻ると自動的にアラームは解除されるか、手動で解除することも可能です。

### ログからアラームを取得する

アラームの発動および解除はログ（コンソールまたはファイル）に記録されます。メッセージ送信やイベント処理で障害が発生した場合、詳細情報がログに記録され、ログ解析を通じてアラートを捕捉することも可能です。以下の例は、ログに出力された詳細なアラーム情報を示しています。ログレベルは `warning` で、`msg` フィールドはそれぞれ `alarm_is_activated` と `alarm_is_deactivated` となっています。

<img src="./assets/view-alarms-log.png" alt="ログでのアラーム表示" style="zoom:50%;" />

### REST API でアラームを取得する

API を通じてアラームの照会および管理が可能です。UI の左側ナビゲーションメニューで **Alarms** をクリックすると、この API リクエストを実行できます。EMQX API の利用方法については [REST API](../../guides/api.md) を参照してください。

<img src="./assets/view-alarms-api.png" alt="APIでのアラーム表示" style="zoom:45%;" />

### Webhook 統合によるアラームイベント送信

EMQX バージョン 5.8.5 以降、ルールエンジンは以下の2つの新しいアラームイベントをサポートしています。

- [$events/sys/alarm_activated](../../develop/data-integration/rule-sql-events-and-fields.md#system-alarm-activated-event-events-sys-alarm-activated)
- [$events/sys/alarm_deactivated](../../develop/data-integration/rule-sql-events-and-fields.md#system-alarm-deactivated-event-events-sys-alarm-deactivated)

これらのイベントにより、Webhook 統合を通じて外部 HTTP サービスへアラームの発動・解除通知を受け取ることが可能です。

Webhook 統合の設定手順は以下の通りです。

1. EMQX ダッシュボードで、**Monitoring** -> **Alarms** に移動します。
2. 右上の **Set Up Webhook** ボタンをクリックして、Webhook 統合設定ページを開きます。
3. Webhook 統合の名前とメモ（任意）を入力します。**Trigger** フィールドには `Alarm Activated` と `Alarm Deactivated` が事前選択されています。
4. 通知を送信したい Webhook URL を入力します。
5. 詳細な設定については [Create Webhook](../../develop/data-integration/webhook.md) を参照してください。
6. 設定が完了したら **Save** をクリックします。

![alarm_webhook_setup](./assets/alarm_webhook_setup.png)

## アラーム設定

アラーム設定には、アラームの表示・保存方法を決めるアラーム設定と、アラームを発動させる閾値を決めるアラーム閾値の設定があります。これにより、ビジネスニーズに応じてアラームの動作をカスタマイズできます。

### アラーム設定の構成

アラームの設定は設定ファイル内の設定項目を修正することでのみ変更可能です。以下の表はアラーム設定に利用できる設定項目を示しています。

| 設定項目              | 説明                                                         | デフォルト値          | 選択可能な値      |
| --------------------- | ------------------------------------------------------------ | -------------------- | ----------------- |
| alarm.actions         | アラーム発動・解除時にログ（コンソールまたはファイル）へ書き込み、MQTT メッセージをシステムトピック `$SYS/brokers/<node_name>/alarms/activate` および `$SYS/brokers/<node_name>/alarms/deactivate` にパブリッシュするアクション。 | `["log", "publish"]` | -                 |
| alarm.size_limit      | 履歴として保持する解除済みアラームの最大件数。上限を超えると古いアラームから削除される。 | `1000`               | `1-3000`          |
| alarm.validity_period | 解除済みアラームの保持期間。解除直後に削除せず、一定期間経過後に削除される。 | `24h`                | -                 |

### ダッシュボードでアラーム閾値を設定する

アラーム閾値は EMQX ダッシュボード上で設定可能です。閾値設定用の **Monitoring** ページを開く方法は2通りあります。

1. **Alarms** ページで **Setting** ボタンをクリックすると **Monitoring** ページに遷移します。
2. 左側ナビゲーションメニューから **Management** -> **Monitoring** をクリックします。

**Monitoring** -> **System** タブの中の **Erlang VM** タブでは、Erlang 仮想マシンのシステムパフォーマンスに関する以下の項目を設定できます。

<img src="./assets/monitoring-system-ee.png" alt="Erlang VM の監視設定" style="zoom:40%;" />

- **Process limit check interval**: プロセス数の定期チェック間隔（秒）。デフォルトは `30` 秒。
- **Process high watermark**: ローカルノードで同時に存在可能なプロセス数の閾値。割合がこの値を超えるとアラームが発動。デフォルトは `80` %。
- **Process low watermark**: プロセス数がこの割合まで下がるとアラームが解除される。デフォルトは `60` %。
- **Enable Long GC monitoring**: デフォルトは無効。有効にすると、Erlang プロセスが長時間ガベージコレクションを行うと警告ログ `long_gc` を出力し、システムトピック `$SYS/sysmon/long_gc` に MQTT メッセージをパブリッシュ。
- **Enable Long Schedule monitoring**: デフォルトで有効。Erlang VM が長時間スケジュールされたタスクを検出すると警告ログ `long_schedule` を出力。タスクの適切なスケジュール時間をミリ秒単位で設定可能。デフォルトは `240` ミリ秒。
- **Enable Large Heap monitoring**: デフォルトで有効。Erlang プロセスのヒープ領域が大きくなると警告ログ `large_heap` を出力し、システムトピック `$SYS/sysmon/large_heap` に MQTT メッセージをパブリッシュ。ヒープサイズの閾値をバイト単位で設定可能。デフォルトは `32` MB。
- **Enable Busy Distribution Port monitoring**: デフォルトで有効。クラスター内の他ノードと通信する RPC 接続が過負荷になると警告ログ `busy_dis_port` を出力し、システムトピック `$SYS/sysmon/busy_dist_port` に MQTT メッセージをパブリッシュ。
- **Enable Busy Port monitoring**: デフォルトで有効。ポートが過負荷になると警告ログ `busy_port` を出力し、システムトピック `$SYS/sysmon/busy_port` に MQTT メッセージをパブリッシュ。

設定完了後、**Save Changes** をクリックしてください。

**Operating System** タブでは、システムパフォーマンスに関する以下の項目を設定できます。

<img src="./assets/monitoring-operating-system-ee.png" alt="OSの監視設定" style="zoom:40%;" />

- **The time interval of the periodic CPU check**: CPU 使用率の定期チェック間隔（秒）。デフォルトは `60` 秒。
- **CPU high watermark**: システム CPU 使用率の閾値。割合がこの値を超えるとアラームが発動。デフォルトは `80` %。
- **CPU low watermark**: CPU 使用率がこの割合まで下がるとアラームが解除される。デフォルトは `60` %。
- **Mem check interval**: メモリ使用率の定期チェック間隔（秒）。デフォルトで有効。デフォルト値は `60` 秒。
- **SysMem high watermark**: システムメモリ使用率の閾値。割合がこの値を超えるとアラームが発動。デフォルトは `70` %。
- **ProcMem high watermark**: 単一の Erlang プロセスによるメモリ使用率の閾値。割合がこの値を超えるとアラームが発動。デフォルトは `5` %。

設定完了後、**Save Changes** をクリックしてください。

### 設定項目でアラーム閾値を設定する

設定ファイル内のアラーム閾値用設定項目を修正して設定することも可能です。現在変更可能な設定項目は以下の通りです。

| 設定項目                          | 説明                                                         | デフォルト値   |
| -------------------------------- | ------------------------------------------------------------ | ------------- |
| sysmon.os.cpu_check_interval      | CPU 使用率のチェック間隔                                      | `60s`         |
| sysmon.os.cpu_high_watermark      | CPU 使用率の高水準閾値。これを超えるとアラームが発動する。   | `80%`         |
| sysmon.os.cpu_low_watermark       | CPU 使用率の低水準閾値。これを下回るとアラームが解除される。 | `60%`         |
| sysmon.os.mem_check_interval      | メモリ使用率のチェック間隔                                    | `60s`         |
| sysmon.os.sysmem_high_watermark   | システムメモリ使用率の高水準閾値。これを超えるとアラームが発動。 | `70%`         |
| sysmon.os.procmem_high_watermark  | プロセスメモリ使用率の高水準閾値。これを超えるとアラームが発動。 | `5%`          |
| sysmonn.vm.process_check_interval | プロセス数のチェック間隔                                      | `30s`         |
| sysmon.vm.process_high_watermark  | プロセス占有率の高水準閾値。これを超えるとアラームが発動。   | `80%`         |
| sysmon.vm.process_low_watermark   | プロセス占有率の低水準閾値。これを下回るとアラームが解除される。 | `60%`         |
| sysmonn.vm.long_gc                | Long GC 監視の有効化設定                                     | `disabled`    |
| sysmon.vm.long_schedule           | Long Schedule 監視の有効化設定                              | `disabled`    |
| sysmon.vm.large_heap              | Large Heap 監視の有効化設定                                 | `disabled`    |
| sysmon.vm.busy_dist_port          | Busy Distribution Port 監視の有効化設定                     | `true`        |
| sysmon.vm.busy_port               | Busy Port 監視の有効化設定                                  | `true`        |
| sysmonn.top.num_items             | 監視グループごとの上位プロセス数                             | `10`          |
| sysmon.top.sample_interlval       | 上位プロセスのチェック間隔                                  | `2s`          |
| sysmon.top.max_procs              | VM 内のプロセス数がこの値を超えるとデータ収集を停止          | `1000000`     |

EMQX Enterprise は、ライセンスの有効期限が30日未満になるか、接続数が高水準閾値を超えた場合にアラームを発動します。接続数の高水準・低水準閾値は設定ファイル内の以下の設定項目を修正して調整可能です。ライセンス設定の詳細は [License](../configuration/license.md) を参照してください。

| 設定項目                                | 説明                                                         | デフォルト値   |
| --------------------------------------- | ------------------------------------------------------------ | ------------- |
| license.connection_high_watermark_alarm | ライセンスがサポートする最大接続数の高水準閾値。これを超えるとアラームが発動。アクティブ接続数／最大接続数の割合で測定。 | `80%`         |
| license.connection_low_watermark_alarm  | ライセンスがサポートする最大接続数の低水準閾値。これを下回るとアラームが解除。アクティブ接続数／最大接続数の割合で測定。 | `75%`         |
