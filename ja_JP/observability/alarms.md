# アラーム

EMQX は、CPU 使用率、システムおよびプロセスメモリ使用率、プロセス数、ルールエンジンのリソース状態、クラスターのパーティションおよび修復など、内部状態の変化を監視するための組み込みの監視およびアラーム機能を提供しています。EMQX は、これらの状態が閾値を超えたり期待値から逸脱した場合にアラームを発生・記録し、正常に戻った際にはリストから削除します。

本ページでは、EMQX が提供するアラーム情報、その詳細なアラーム情報の取得および確認方法、さらにアラーム設定や閾値の設定方法について紹介します。監視およびアラーム機能により、運用中の潜在的な問題を通知し続けます。適切な閾値を設定してアラームを構成することで、EMQX の安全性、安定性、信頼性を確保できます。

## アラーム一覧

以下の表は、システム監視中に潜在的な問題を示すためにトリガーされる可能性のあるアラームを示しています。

::: tip

アラームの重大度やシステムへの影響に応じて、アラームには3つのレベルがあります：

- **Error（エラー）**：ユーザー設定によるエラー。クライアントはエラーを認識し、再試行可能です。

- **Warning（警告）**：時折発生するエラー。頻発する場合は注意が必要です。

- **Critical（重大）**：クライアントとサーバー間で不可逆的なデータ損失が発生し、通信や業務に支障をきたす状態。

これらのレベルは開発視点で定義されており、あくまで推奨です。ビジネスニーズに応じて独自のアラームレベルを定義可能です。

:::

| **アラーム**               | レベル    | 説明                                                        | **詳細**                                    | **閾値**                                                      |
| :------------------------ | -------- | :----------------------------------------------------------- | :------------------------------------------- | :----------------------------------------------------------- |
| high_system_memory_usage  | Warning  | システムメモリ使用率が高すぎる                              | システムメモリ使用率が約 ~p% を超えている   | `os_mon.sysmem_high_watermark = 70%`                         |
| high_process_memory_usage | Warning  | 単一の Erlang プロセスメモリ使用率が高すぎる（システムメモリ使用率の割合） | プロセスメモリ使用率が約 ~p% を超えている   | `os_mon.procmem_high_watermark = 5%`                         |
| high_cpu_usage            | Warning  | CPU 使用率が高すぎる                                        | 約 ~p% の CPU 使用率                         | `os_mon.cpu_high_watermark = 80%` `os_mon.cpu_low_watermark = 60%` |
| too_many_processes        | Warning  | プロセス数が多すぎる                                       | 約 ~p% のプロセス使用率                      | `vm_mon.process_high_watermark = 80%` `vm_mon.process_low_watermark = 60%` |
| license_quota             | Warning  | ライセンスの接続数が上限を超えている                        | ライセンス：接続数が % を超えている          | `license.connection_high_watermark_alarm = 80%` `license.connection_low_watermark_alarm = 75%` |
| license_expiry            | Critical | ライセンスが期限切れ                                        | ライセンスの期限切れが % に迫っている        | -                                                            |
| license_tps               | Warning  | TPS 使用率がライセンス上限を超えている                      | ライセンス：TPS 上限（例：10）を超過         | -                                                            |
| partition                 | Critical | ノードでパーティションが発生                                | ノード ~s でパーティションが発生             | -                                                            |
| resource                  | Critical | リソースが切断されている                                   | リソース ~s(~s) がダウン                      | -                                                            |
| conn_congestion           | Critical | 接続プロセスが混雑している                                 | 接続が混雑している                            | -                                                            |

## アラームの取得

EMQX では、アラームを取得して詳細情報を確認するための複数の方法を提供しています。1つは EMQX ダッシュボードを利用する方法で、アクティブなアラームおよび履歴アラームをユーザーフレンドリーなインターフェースで閲覧できます。これにより、発生したアラームの概要を一元的に把握できます。

また、MQTT のシステムトピックをサブスクライブしてリアルタイムにシステムアラームの通知を受け取る方法もあります。さらに、Webhook 連携によりアラームイベントを外部の HTTP サービスに送信して処理することも可能です。アラームはログや REST API からもアクセスできます。

### ダッシュボードでアラームを確認する

EMQX ダッシュボードで、**Monitoring** -> **Alarms** をクリックします。次に、**Active** または **History** タブを選択すると、現在アクティブなアラームや履歴アラームの一覧が表示されます。

EMQX ダッシュボードでのアラーム管理の詳細は、[アラーム](../dashboard/alarm_dashboard.md) を参照してください。

<img src="./assets/view-alarms.png" alt="アラームの表示" style="zoom:50%;" />

### システムトピック経由でアラームを取得する

アラームが発生または解除されると、EMQX は MQTT メッセージをシステムトピック `$SYS/brokers/<Node>/alarms/activate` または `$SYS/brokers/<Node>/alarms/deactivate` にパブリッシュします。ユーザーはこれらのトピックをサブスクライブしてアラーム通知を受け取れます。

アラーム通知メッセージのペイロードは JSON 形式で、以下のフィールドを含みます：

| フィールド名       | 型               | 説明                                                        |
| ------------------ | ---------------- | ----------------------------------------------------------- |
| `name`             | string           | アラーム名                                                  |
| `details`          | object           | アラームの詳細                                              |
| `message`          | string           | 人間が読みやすいアラームの説明                              |
| `activate_at`      | integer          | アラーム発生時刻をマイクロ秒単位の UNIX タイムスタンプで表現 |
| `deactivate_at`    | integer / string | アラーム解除時刻をマイクロ秒単位の UNIX タイムスタンプで表現。アクティブなアラームの値は `infinity` です。 |
| `activated`        | boolean          | アラームが発生中かどうか                                   |

システムメモリ使用率が高いアラームの例では、以下のようなアラームメッセージを受け取ります：

<img src="./assets/alarm_activate_msg.png" alt="アラームメッセージ" style="zoom:50%;" />

同じ種類のアラームは繰り返し報告されません。例えば、高 CPU 使用率のアラームが発生中の場合、同じタイプのアラームは新たに生成されません。監視対象の指標が正常に戻るとアラームは自動的に解除されるか、手動で解除することも可能です。

### ログからアラームを取得する

アラームの発生および解除はログ（コンソールまたはファイル）に記録されます。メッセージ送信やイベント処理中に障害が発生した場合、詳細情報をログに記録でき、ログ解析によってアラートを検知することも可能です。以下の例は、ログに出力された詳細なアラーム情報を示しています。ログレベルは `warning` で、`msg` フィールドは `alarm_is_activated` および `alarm_is_deactivated` です。

<img src="./assets/view-alarms-log.png" alt="ログでのアラーム表示" style="zoom:50%;" />

### REST API でアラームを取得する

API を通じてアラームの照会および管理が可能です。UI の左側ナビゲーションメニューで **Alarms** をクリックすると、この API リクエストが実行されます。EMQX API の利用方法は [REST API](../admin/api.md) を参照してください。

<img src="./assets/view-alarms-api.png" alt="APIでのアラーム表示" style="zoom:45%;" />

### Webhook 連携でアラームイベントを送信する

EMQX バージョン 5.8.5 以降、ルールエンジンは以下の2つの新しいアラームイベントをサポートしています：

- [$events/sys/alarm_activated](../data-integration/rule-sql-events-and-fields.md#system-alarm-activated-event-events-sys-alarm-activated)
- [$events/sys/alarm_deactivated](../data-integration/rule-sql-events-and-fields.md#system-alarm-deactivated-event-events-sys-alarm-deactivated)

これらのイベントにより、Webhook 連携を通じて外部 HTTP サービスへアラーム発生・解除の通知を受け取れます。

Webhook 連携の設定手順：

1. EMQX ダッシュボードで **Monitoring** -> **Alarms** に移動します。
2. 右上の **Set Up Webhook** ボタンをクリックして、Webhook 連携設定ページを開きます。
3. Webhook 連携の名前と（任意で）メモを入力します。**Trigger** フィールドには `Alarm Activated` と `Alarm Deactivated` が事前選択されています。
4. 通知を送信する Webhook URL を入力します。
5. 詳細な設定オプションは [Webhook 作成](../data-integration/webhook.md) を参照してください。
6. 設定が完了したら **Save** をクリックします。

![alarm_webhook_setup](./assets/alarm_webhook_setup.png)

## アラーム設定

アラーム設定には、アラームの動作設定と閾値設定が含まれます。動作設定はアラームメッセージの表示や保存方法を決定し、閾値設定は潜在的な問題を検知してアラームを発生させるための限界値や値を定めます。アラーム設定機能により、ビジネスニーズに合わせてアラームの動作や閾値をカスタマイズできます。

### アラーム動作設定の構成

アラームの動作設定は、設定ファイル内の設定項目を変更することでのみ構成可能です。以下の表は、アラーム動作設定に利用できる設定項目を示しています。

| 設定項目              | 説明                                                        | デフォルト値          | 選択可能値       |
| --------------------- | ----------------------------------------------------------- | -------------------- | --------------- |
| alarm.actions         | アラーム発生・解除時にログ（コンソールまたはファイル）への書き込みおよびシステムトピック `$SYS/brokers/<node_name>/alarms/activate`、`$SYS/brokers/<node_name>/alarms/deactivate` への MQTT メッセージパブリッシュを行うアクション。 | `["log", "publish"]` | -               |
| alarm.size_limit      | 履歴として保持する解除済みアラームの最大数。この上限を超えると古いアラームから削除される。 | `1000`               | `1-3000`        |
| alarm.validity_period | 解除済みアラームの保持期間。解除直後に削除せず、一定期間保持する。 | `24h`                | -               |

### ダッシュボードでアラーム閾値を設定する

アラーム閾値は EMQX ダッシュボードで設定可能です。閾値設定用の **Monitoring** ページを開く方法は2通りあります：

1. **Alarms** ページで **Setting** ボタンをクリックすると、**Monitoring** ページに遷移します。
2. 左側ナビゲーションメニューから **Management** -> **Monitoring** をクリックします。

**Monitoring** -> **System** タブの中の **Erlang VM** タブでは、Erlang 仮想マシンのシステムパフォーマンスに関する以下の項目を設定できます：

<img src="./assets/monitoring-system-ee.png" alt="Erlang VM の監視設定" style="zoom:40%;" />

- **Process limit check interval**：プロセス数の定期チェック間隔を秒単位で指定します。デフォルトは `30` 秒です。
- **Process high watermark**：ローカルノードに同時存在可能なプロセス数の閾値（割合）を指定します。指定値を超えるとアラームが発生します。デフォルトは `80` パーセントです。
- **Process low watermark**：プロセス数がこの割合まで減少するとアラームが解除されます。デフォルトは `60` パーセントです。
- **Enable Long GC monitoring**：デフォルトで無効。有効化すると、Erlang プロセスが長時間ガベージコレクションを行うと警告レベルのログ `long_gc` が出力され、システムトピック `$SYS/sysmon/long_gc` に MQTT メッセージがパブリッシュされます。
- **Enable Long Schedule monitoring**：デフォルトで有効。Erlang VM が長時間スケジュールされたタスクを検知すると警告レベルのログ `long_schedule` が出力されます。タスクの適切なスケジュール時間をミリ秒単位で設定可能です。デフォルトは `240` ミリ秒です。
- **Enable Large Heap monitoring**：デフォルトで有効。Erlang プロセスが大きなヒープ領域を消費すると警告レベルのログ `large_heap` が出力され、システムトピック `$SYS/sysmon/large_heap` に MQTT メッセージがパブリッシュされます。ヒープ領域のサイズ制限を設定可能です。デフォルトは `32` MB です。
- **Enable Busy Distribution Port monitoring**：デフォルトで有効。クラスター内の他ノードと通信するための RPC 接続が過負荷になると警告レベルのログ `busy_dis_port` が出力され、システムトピック `$SYS/sysmon/busy_dist_port` に MQTT メッセージがパブリッシュされます。
- **Enable Busy Port monitoring**：デフォルトで有効。ポートが過負荷になると警告レベルのログ `busy_port` が出力され、システムトピック `$SYS/sysmon/busy_port` に MQTT メッセージがパブリッシュされます。

設定完了後、**Save Changes** をクリックしてください。

**Operating System** タブでは、システムパフォーマンスに関する以下の項目を設定できます：

<img src="./assets/monitoring-operating-system-ee.png" alt="OS の監視設定" style="zoom:40%;" />

- **The time interval of the periodic CPU check**：CPU 使用率の定期チェック間隔を秒単位で指定します。デフォルトは `60` 秒です。
- **CPU high watermark**：システム CPU 使用率の閾値を指定します。指定値を超えるとアラームが発生します。デフォルトは `80` パーセントです。
- **CPU low watermark**：CPU 使用率がこの値まで下がるとアラームが解除されます。デフォルトは `60` パーセントです。
- **Mem check interval**：メモリ使用率の定期チェック間隔を秒単位で指定します。デフォルトは `60` 秒で有効です。
- **SysMem high watermark**：システムメモリ使用率の閾値を指定します。指定値を超えるとアラームが発生します。デフォルトは `70%` です。
- **ProcMem high watermark**：単一の Erlang プロセスメモリ使用率の閾値を指定します。指定値を超えるとアラームが発生します。デフォルトは `5%` です。

設定完了後、**Save Changes** をクリックしてください。

### 設定ファイルでアラーム閾値を設定する

設定ファイル内の設定項目を変更することで、アラーム閾値を設定することも可能です。現在変更可能な設定項目は以下の通りです：

| 設定項目                      | 説明                                                        | デフォルト値   |
| ----------------------------- | ----------------------------------------------------------- | ------------- |
| sysmon.os.cpu_check_interval  | CPU 使用率のチェック間隔                                    | `60s`         |
| sysmon.os.cpu_high_watermark  | CPU 使用率の高水準閾値。これを超えるとアラームが発生する。 | `80%`         |
| sysmon.os.cpu_low_watermark   | CPU 使用率の低水準閾値。これを下回るとアラームが解除される。 | `60%`         |
| sysmon.os.mem_check_interval  | メモリ使用率のチェック間隔                                  | `60s`         |
| sysmon.os.sysmem_high_watermark | システムメモリ使用率の高水準閾値。これを超えるとアラームが発生する。 | `70%`         |
| sysmon.os.procmem_high_watermark | 単一プロセスメモリ使用率の高水準閾値。これを超えるとアラームが発生する。 | `5%`          |
| sysmon.vm.process_check_interval | プロセス数のチェック間隔                                  | `30s`         |
| sysmon.vm.process_high_watermark | プロセス使用率の高水準閾値。これを超えるとアラームが発生する。作成済みプロセス数／最大数の割合で測定。 | `80%`         |
| sysmon.vm.process_low_watermark | プロセス使用率の低水準閾値。これを下回るとアラームが解除される。作成済みプロセス数／最大数の割合で測定。 | `60%`         |
| sysmon.vm.long_gc             | Long GC 監視を有効にするかどうか                          | `disabled`    |
| sysmon.vm.long_schedule       | Long Schedule 監視を有効にするかどうか                    | `disabled`    |
| sysmon.vm.large_heap          | Large Heap 監視を有効にするかどうか                       | `disabled`    |
| sysmon.vm.busy_dist_port      | Busy Distribution Port 監視を有効にするかどうか           | `true`        |
| sysmon.vm.busy_port           | Busy Port 監視を有効にするかどうか                        | `true`        |
| sysmon.top.num_items          | 監視グループごとのトッププロセス数                        | `10`          |
| sysmon.top.sample_interval    | トッププロセスのチェック間隔                              | `2s`          |
| sysmon.top.max_procs          | VM 内のプロセス数がこの値を超えるとデータ収集を停止する。  | `1000000`     |

EMQX Enterprise では、ライセンスの期限が30日未満になるか、接続数が高水準閾値を超えた場合にアラームが発生します。接続数の高水準・低水準閾値は設定ファイルの以下の項目を変更して調整可能です。ライセンス設定の詳細は [ライセンス](../configuration/license.md) を参照してください。

| 設定項目                              | 説明                                                        | デフォルト値   |
| ------------------------------------ | ----------------------------------------------------------- | ------------- |
| license.connection_high_watermark_alarm | ライセンスがサポートする最大接続数の高水準閾値。これを超えるとアラームが発生。アクティブ接続数／最大接続数の割合で測定。 | `80%`         |
| license.connection_low_watermark_alarm  | ライセンスがサポートする最大接続数の低水準閾値。これを下回るとアラームが解除。アクティブ接続数／最大接続数の割合で測定。 | `75%`         |
