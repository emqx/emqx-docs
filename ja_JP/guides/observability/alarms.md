# アラーム

EMQX は、CPU 使用率、システムおよびプロセスメモリ使用率、プロセス数、ルールエンジンのリソース状態、クラスターのパーティションや修復状況など、内部状態の変化を監視するための組み込みの監視およびアラーム機能を提供しています。EMQX は、これらの状態が閾値を超えたり期待値から逸脱した場合にアラームを発動・記録し、状態が回復するとリストから削除します。

本ページでは、EMQX が提供するアラーム情報、詳細なアラーム情報の取得・確認方法、および EMQX におけるアラーム設定と閾値の構成方法について紹介します。監視およびアラーム機能により、運用中の潜在的な問題を通知し、適切な閾値設定により EMQX の安全性、安定性、信頼性を確保できます。

## アラーム一覧

以下の表は、システム監視中に潜在的な問題を示すために発動される可能性のあるアラームを示しています。

::: tip

アラームはシステムへの影響度や重大度に応じて3つのレベルに分類されます。

- **Error（エラー）**: ユーザー設定によるエラー。クライアントはエラーを認識し再試行可能。
- **Warning（警告）**: 発生頻度が高い場合は注意が必要な一時的なエラー。
- **Critical（重大）**: クライアントとサーバー間での不可逆的なデータ損失を伴い、通信や業務に支障をきたす。

これらのレベルは開発視点で定義されており、あくまで推奨です。ビジネスニーズに応じて独自のアラームレベルを定義可能です。

:::

| **アラーム**               | レベル     | 説明                                                        | **詳細**                                    | **閾値**                                                      |
| :------------------------ | --------- | :---------------------------------------------------------- | :------------------------------------------ | :------------------------------------------------------------ |
| high_system_memory_usage  | Warning   | システムメモリ使用率が高い                                  | システムメモリ使用率が約 ~p% を超えている  | `os_mon.sysmem_high_watermark = 70%`                         |
| high_process_memory_usage | Warning   | 単一の Erlang プロセスのメモリ使用率が高い（システムメモリ使用率の割合） | プロセスメモリ使用率が約 ~p% を超えている  | `os_mon.procmem_high_watermark = 5%`                         |
| high_cpu_usage            | Warning   | CPU 使用率が高い                                            | 約 ~p% の CPU 使用率                        | `os_mon.cpu_high_watermark = 80%` `os_mon.cpu_low_watermark = 60%` |
| too_many_processes        | Warning   | プロセス数が多すぎる                                        | 約 ~p% のプロセス使用率                      | `vm_mon.process_high_watermark = 80%` `vm_mon.process_low_watermark = 60%` |
| license_quota             | Warning   | ライセンスの接続数が上限を超えている                        | ライセンス：接続数が % を超えている          | `license.connection_high_watermark_alarm = 80%` `license.connection_low_watermark_alarm = 75%` |
| license_expiry            | Critical  | ライセンスが期限切れ                                        | ライセンスは % に期限切れとなる予定          | -                                                            |
| license_tps               | Warning   | TPS 使用率がライセンス上限を超えている                      | ライセンス：TPS 上限（例：10）を超過         | -                                                            |
| partition                 | Critical  | ノードでパーティションが発生                                | ノード ~s でパーティションが発生            | -                                                            |
| resource                  | Critical  | リソースが切断されている                                    | リソース ~s(~s) がダウン                      | -                                                            |
| conn_congestion           | Critical  | 接続プロセスの輻輳                                        | 接続が輻輳している                            | -                                                            |

## アラームの取得

EMQX では、アラームの取得および詳細情報の確認方法として複数の手段を提供しています。ひとつは EMQX ダッシュボードで、アクティブなアラームと履歴アラームの両方をユーザーフレンドリーなインターフェースで閲覧できます。ここがアラームの概要を簡単に確認できる中心的な場所となります。

また、MQTT のシステムトピックをサブスクライブしてリアルタイムにシステムアラームの通知を受け取る方法もあります。さらに Webhook 統合を利用して、アラームイベントを外部の HTTP サービスに送信し、追加処理を行うことも可能です。ログや REST API からもアラーム情報にアクセスできます。

### ダッシュボードでアラームを確認する

EMQX ダッシュボードで、**Monitoring** -> **Alarms** をクリックします。次に、**Active** または **History** タブを選択すると、現在アクティブなアラームと履歴アラームの一覧が表示されます。

EMQX ダッシュボードでのアラーム管理の詳細は、[アラーム](../dashboard/alarm_dashboard.md) を参照してください。

<img src="./assets/view-alarms.png" alt="アラーム表示" style="zoom:50%;" />

### システムトピック経由でアラームを取得する

アラームが発動または解除されると、EMQX は MQTT メッセージをシステムトピック `$SYS/brokers/<Node>/alarms/activate` または `$SYS/brokers/<Node>/alarms/deactivate` にパブリッシュします。ユーザーはこれらのトピックをサブスクライブしてアラーム通知を受け取れます。

アラーム通知メッセージのペイロードは JSON 形式で、以下のフィールドを含みます。

| フィールド名       | 型               | 説明                                                        |
| ------------------ | ---------------- | ----------------------------------------------------------- |
| `name`             | string           | アラーム名                                                  |
| `details`          | object           | アラームの詳細情報                                          |
| `message`          | string           | 人間が読みやすいアラームの説明                              |
| `activate_at`      | integer          | アラーム発動時刻をマイクロ秒単位の UNIX タイムスタンプで表現 |
| `deactivate_at`    | integer / string | アラーム解除時刻をマイクロ秒単位の UNIX タイムスタンプで表現。アクティブなアラームは `infinity` となる。 |
| `activated`        | boolean          | アラームが発動中かどうか                                    |

例えば、システムメモリ使用率が高いアラームの場合、以下のようなアラームメッセージを受信します。

<img src="./assets/alarm_activate_msg.png" alt="アラームメッセージ" style="zoom:50%;" />

同じ種類のアラームは繰り返し発報されません。例えば高 CPU 使用率のアラームが発動中は、同じアラームは再度発生しません。監視対象の指標が正常に戻ると自動的にアラームは解除されますが、手動で解除することも可能です。

### ログからアラームを取得する

アラームの発動および解除はログ（コンソールまたはファイル）に記録されます。メッセージ送信やイベント処理で障害が発生した場合、詳細情報がログに記録され、ログ解析を通じてアラートを検出することも可能です。以下はログに出力された詳細なアラーム情報の例です。ログレベルは `warning`、`msg` フィールドは `alarm_is_activated` および `alarm_is_deactivated` となっています。

<img src="./assets/view-alarms-log.png" alt="ログでアラームを確認" style="zoom:50%;" />

### REST API でアラームを取得する

API を通じてアラームの照会や管理が可能です。UI の左側ナビゲーションメニューで **Alarms** をクリックすると、この API リクエストが実行されます。EMQX API の使い方については [REST API](../api.md) を参照してください。

<img src="./assets/view-alarms-api.png" alt="APIでアラームを確認" style="zoom:45%;" />

### Webhook 統合によるアラームイベント送信

EMQX バージョン 5.8.5 以降、ルールエンジンは以下の2つの新しいアラームイベントをサポートしています。

- [$events/sys/alarm_activated](../../develop/data-integration/rule-sql-events-and-fields.md#system-alarm-activated-event-events-sys-alarm-activated)
- [$events/sys/alarm_deactivated](../../develop/data-integration/rule-sql-events-and-fields.md#system-alarm-deactivated-event-events-sys-alarm-deactivated)

これらのイベントにより、Webhook 統合を通じて外部 HTTP サービスへアラームの発動・解除通知を受け取れます。

Webhook 統合の設定手順は以下の通りです。

1. EMQX ダッシュボードで **Monitoring** -> **Alarms** に移動します。
2. 右上の **Set Up Webhook** ボタンをクリックし、Webhook 統合設定ページを開きます。
3. Webhook 統合の名前と任意のメモを入力します。**Trigger** フィールドには `Alarm Activated` と `Alarm Deactivated` が事前選択されています。
4. 通知を送信したい Webhook URL を入力します。
5. 詳細な設定オプションについては [Create Webhook](../../develop/data-integration/webhook.md) を参照してください。
6. 設定が完了したら **Save** をクリックします。

![alarm_webhook_setup](./assets/alarm_webhook_setup.png)

## アラーム設定

アラーム設定は、アラームの表示・保存方法を決めるアラーム設定と、アラームを発動させる閾値を決めるアラーム閾値の設定に分かれます。これにより、ビジネスニーズに合わせてアラームの動作をカスタマイズできます。

### アラーム設定の構成

アラームの設定は、設定ファイル内の設定項目を変更することでのみ構成可能です。以下の表はアラーム設定に利用できる設定項目です。

| 設定項目               | 説明                                                        | デフォルト値          | 選択肢           |
| ---------------------- | ----------------------------------------------------------- | -------------------- | ---------------- |
| alarm.actions          | アラーム発動・解除時にログ（コンソールまたはファイル）への書き込みおよび MQTT メッセージをシステムトピック `$SYS/brokers/<node_name>/alarms/activate` と `$SYS/brokers/<node_name>/alarms/deactivate` にパブリッシュするアクション。 | `["log", "publish"]` | -                |
| alarm.size_limit       | 履歴として保持する解除済みアラームの最大総数。この上限を超えると最も古い解除済みアラームから削除される。 | `1000`               | `1-3000`         |
| alarm.validity_period  | 解除済みアラームの保持期間。解除直後に削除されず、一定期間経過後に削除される。 | `24h`                | -                |

### ダッシュボードでアラーム閾値を設定する

アラーム閾値は EMQX ダッシュボードで設定可能です。閾値設定用の **Monitoring** ページを開く方法は2通りあります。

1. **Alarms** ページで **Setting** ボタンをクリックすると **Monitoring** ページに遷移します。
2. 左側ナビゲーションメニューから **Management** -> **Monitoring** をクリックします。

**Monitoring** -> **System** タブの中の **Erlang VM** タブでは、Erlang 仮想マシンのシステムパフォーマンスに関する以下の項目を設定できます。

<img src="./assets/monitoring-system-ee.png" alt="システム監視設定" style="zoom:40%;" />

- **Process limit check interval**: プロセス数の定期チェック間隔（秒）。デフォルトは `30` 秒。
- **Process high watermark**: ローカルノードで同時に存在可能なプロセス数の閾値。割合がこの値を超えるとアラーム発動。デフォルトは `80` %。
- **Process low watermark**: ローカルノードで同時に存在可能なプロセス数の閾値。割合がこの値以下になるとアラーム解除。デフォルトは `60` %。
- **Enable Long GC monitoring**: デフォルトは無効。有効化すると、Erlang プロセスが長時間ガベージコレクションを行った場合に警告ログ `long_gc` を出力し、システムトピック `$SYS/sysmon/long_gc` に MQTT メッセージをパブリッシュ。
- **Enable Long Schedule monitoring**: デフォルトは有効。Erlang VM が長時間スケジューリングされたタスクを検知すると警告ログ `long_schedule` を出力。タスクの適切なスケジュール時間をテキストボックスで設定可能。デフォルトは `240` ミリ秒。
- **Enable Large Heap monitoring**: デフォルトは有効。Erlang プロセスが大きなヒープ領域を消費した場合に警告ログ `large_heap` を出力し、システムトピック `$SYS/sysmon/large_heap` に MQTT メッセージをパブリッシュ。ヒープのバイトサイズ制限をテキストボックスで設定可能。デフォルトは `32` MB。
- **Enable Busy Distribution Port monitoring**: デフォルトは有効。クラスター内の他ノードと通信するための RPC 接続が過負荷になると警告ログ `busy_dis_port` を出力し、システムトピック `$SYS/sysmon/busy_dist_port` に MQTT メッセージをパブリッシュ。
- **Enable Busy Port monitoring**: デフォルトは有効。ポートが過負荷になると警告ログ `busy_port` を出力し、システムトピック `$SYS/sysmon/busy_port` に MQTT メッセージをパブリッシュ。

設定完了後、**Save Changes** をクリックしてください。

**Operating System** タブでは、システムパフォーマンスに関する以下の項目を設定できます。

<img src="./assets/monitoring-operating-system-ee.png" alt="OS監視設定" style="zoom:40%;" />

- **The time interval of the periodic CPU check**: CPU 使用率の定期チェック間隔（秒）。デフォルトは `60` 秒。
- **CPU high watermark**: システム CPU 使用率の閾値。割合がこの値を超えるとアラーム発動。デフォルトは `80` %。
- **CPU low watermark**: システム CPU 使用率の閾値。割合がこの値以下になるとアラーム解除。デフォルトは `60` %。
- **Mem check interval**: メモリ使用率の定期チェック間隔（秒）。デフォルトは `60` 秒で有効。
- **SysMem high watermark**: システムメモリ使用率の閾値。割合がこの値を超えるとアラーム発動。デフォルトは `70` %。
- **ProcMem high watermark**: 単一の Erlang プロセスによるメモリ使用率の閾値。割合がこの値を超えるとアラーム発動。デフォルトは `5` %。

設定完了後、**Save Changes** をクリックしてください。

### 設定項目でアラーム閾値を構成する

設定ファイルのアラーム閾値用設定項目を変更して閾値を構成することも可能です。現在変更可能な設定項目は以下の通りです。

| 設定項目                        | 説明                                                        | デフォルト値   |
| ------------------------------ | ----------------------------------------------------------- | ------------- |
| sysmon.os.cpu_check_interval    | CPU 使用率のチェック間隔                                    | `60s`         |
| sysmon.os.cpu_high_watermark    | CPU 使用率の高水準閾値。アラーム発動の閾値。               | `80%`         |
| sysmon.os.cpu_low_watermark     | CPU 使用率の低水準閾値。アラーム解除の閾値。               | `60%`         |
| sysmon.os.mem_check_interval    | メモリ使用率のチェック間隔                                  | `60s`         |
| sysmon.os.sysmem_high_watermark | システムメモリ使用率の高水準閾値。合計使用率がこの値に達するとアラーム発動。 | `70%`         |
| sysmon.os.procmem_high_watermark| プロセスメモリ使用率の高水準閾値。単一プロセスの使用率がこの値に達するとアラーム発動。 | `5%`          |
| sysmon.vm.process_check_interval| プロセス数のチェック間隔                                    | `30s`         |
| sysmon.vm.process_high_watermark| プロセス占有率の高水準閾値。作成済みプロセス数/最大数の比率で測定。アラーム発動の閾値。 | `80%`         |
| sysmon.vm.process_low_watermark | プロセス占有率の低水準閾値。作成済みプロセス数/最大数の比率で測定。アラーム解除の閾値。 | `60%`         |
| sysmon.vm.long_gc               | Long GC 監視の有効化設定                                   | `disabled`    |
| sysmon.vm.long_schedule         | Long Schedule 監視の有効化設定                             | `disabled`    |
| sysmon.vm.large_heap            | Large Heap 監視の有効化設定                                | `disabled`    |
| sysmon.vm.busy_dist_port        | Busy Distribution Port 監視の有効化設定                    | `true`        |
| sysmon.vm.busy_port             | Busy Port 監視の有効化設定                                 | `true`        |
| sysmon.top.num_items            | 監視グループごとのトッププロセス数                         | `10`          |
| sysmon.top.sample_interval      | トッププロセスのチェック間隔                               | `2s`          |
| sysmon.top.max_procs            | VM 内のプロセス数がこの値を超えた場合、データ収集を停止    | `1000000`     |

EMQX Enterprise では、ライセンスの有効期限が30日未満になるか、接続数が高水準閾値を超えるとアラームが発動します。接続数の高/低水準閾値は設定ファイルの以下の設定項目を変更して調整可能です。ライセンス設定の詳細は [License](../configuration/license.md) を参照してください。

| 設定項目                                | 説明                                                        | デフォルト値   |
| ------------------------------------- | ----------------------------------------------------------- | ------------- |
| license.connection_high_watermark_alarm | ライセンスがサポートする最大接続数の高水準閾値。アクティブ接続数/最大接続数の比率で測定。閾値を超えるとアラーム発動。 | `80%`         |
| license.connection_low_watermark_alarm  | ライセンスがサポートする最大接続数の低水準閾値。アクティブ接続数/最大接続数の比率で測定。閾値以下になるとアラーム解除。 | `75%`         |
