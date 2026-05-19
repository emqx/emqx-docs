# ログ

ログはトラブルシューティングやシステムパフォーマンスの最適化に役立つ信頼できる情報源です。EMQXのログからアクセス状況や動作、ネットワークの問題に関する記録を確認できます。

EMQXはコンソールログとファイルログの両方をサポートしています。ログデータの出力方法は2種類あり、必要に応じて出力方法を選択するか、両方を有効にすることも可能です。コンソールログはログデータをコンソールやコマンドラインインターフェースに出力することを指し、開発やデバッグ時にリアルタイムでログを素早く確認するために使われます。ファイルログはログデータをファイルに出力するもので、主に本番環境でログデータを長期間保存し、分析やトラブルシューティングに利用されます。

システムのデフォルトのログ処理動作は環境変数 `EMQX_DEFAULT_LOG_HANDLER` により設定可能で、以下の設定を受け付けます。

- `file`: ログ出力をファイルに向ける。
- `console`: ログ出力をコンソールに向ける。

環境変数 `EMQX_DEFAULT_LOG_HANDLER` のデフォルトは `console` ですが、systemdの `emqx.service` ファイル経由でEMQXを起動する場合は明示的に `file` に設定されます。

ログデータが多すぎる場合やログ書き込みが遅い場合など、システム運用への影響を最小限に抑えるために、EMQXはデフォルトで過負荷保護機構を有効にし、ユーザーにより良いサービスを提供しています。

## ログレベル

EMQXのログレベルはRFC 5424に準拠した8段階のうち6段階をサポートしており、デフォルトは `warning` です。低い順に以下のレベルがあります。

```bash
debug < info < notice < warning < error < critical
```

以下の表は各ログレベルの意味と出力内容の例を示しています。

| ログレベル | 意味                                                         | 出力例                                                        |
| --------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| debug     | プログラム内部の詳細情報で、コードのデバッグや診断に役立ちます。<br />本番環境で直接このレベルのログを出力することは推奨されません。代わりに特定クライアント向けに[Log Trace](./tracer.md)を有効にしてください。 | 変数の値、関数呼び出しスタックなどの詳細なデバッグデータ。 |
| info      | debugレベルより一般的な有用情報。                              | 認可拒否などの軽微な異常や、設定変更成功などの管理操作結果。 |
| notice    | イベント発生を示す重要なシステム情報で、特に対応は不要。         | ダッシュボードやCLIからのリクエストによるコンポーネントの再起動。 |
| warning   | 対応が必要な潜在的な問題やエラー。クリティカルな問題になる前の監視に使われます。 | 切断、接続タイムアウト、認証失敗などの類似イベント。         |
| error     | エラー発生を示し、管理者が迅速に問題を検出・解決できるようにします。 | 外部データベース接続失敗、存在しないトピックへのサブスクライブ失敗、設定ファイル解析失敗など。 |
| critical  | システムクラッシュや機能停止を引き起こす重大なエラー。管理者が即時対応すべき問題を示します。 | 設定ミスによりコンポーネントが起動・正常動作できない場合。 |

## ダッシュボードによるログ設定

このセクションでは主にEMQXダッシュボードでのログ設定方法を説明します。変更はノード再起動なしに即時反映されます。

EMQXダッシュボードにアクセスし、左メニューの **Management** -> **Logging** をクリックします。コンソールログまたはファイルログの設定はそれぞれ対応するタブを選択して行います。

### コンソールログの設定

**Logging** ページで **Console Log** タブを選択します。

<img src="./assets/config-console-log-1-ee.png" alt="コンソールログ設定画面" style="zoom:67%;" />

コンソールログハンドラーの設定項目は以下の通りです。

- **Enable Log Handler**: トグルスイッチをクリックしてコンソールログハンドラーを有効化します。

- **Log Level**: ドロップダウンリストから使用するログレベルを選択します。デフォルトは `warning` です。

- **Log Formatter**: ログのフォーマットをドロップダウンリストから選択します。選択肢は `text` と `JSON` で、デフォルトは `text` です。

- **Timestamp Format**: ログのタイムスタンプ形式を選択します。選択肢は以下の通りです。
  - `auto`: 使用しているログフォーマッターに応じて自動的に形式を決定します。textフォーマッターの場合は `rfc3339`、JSONフォーマッターの場合は `epoch` 形式を使用します。
  - `epoch`: マイクロ秒精度のUnixエポック形式でタイムスタンプを表します。
  - `rfc3339`: RFC3339準拠の日時文字列形式。例：`2024-03-26T11:52:19.777087+00:00`

- **Time Offset**: ログのタイムスタンプに対するUTCからの時差を指定します。デフォルトはシステムに従い、値は `system` です。

設定が完了したら **Save Changes** をクリックしてください。

### ファイルログの設定

**Logging** ページで **File Log** タブを選択します。

<img src="./assets/config-file-log-1-ee.png" alt="ファイルログ設定画面" style="zoom:67%;" />

ファイルログハンドラーの設定項目は以下の通りです。

- **Enable Log Handler**: トグルスイッチをクリックしてファイルログハンドラーを有効化します。

- **Log File Name**: ログファイル名を入力します。デフォルトは `log/emqx.log` です。

- **Max Log Files Number**: ローテーションされるログファイルの最大数を指定します。デフォルトは `10` です。

- **Rotation Size**: ログファイルが指定サイズに達した際にローテーションされます。デフォルトで有効です。テキストボックスに具体的なサイズを入力できます。無効にすると値は `infinity` となり、ログファイルは無制限に成長します。

- **Log Level**: 使用するログレベルをドロップダウンリストから選択します。選択肢は `debug`、`info`、`notice`、`warning`、`error`、`critical` で、デフォルトは `warning` です。

- **Log Formatter**: ログフォーマットをドロップダウンリストから選択します。選択肢は `text` と `JSON` で、デフォルトは `text` です。

- **Timestamp Format**: ログのタイムスタンプ形式を選択します。選択肢は以下の通りです。

  - `auto`: 使用しているログフォーマッターに応じて自動的に形式を決定します。textフォーマッターの場合は `rfc3339`、JSONフォーマッターの場合は `epoch` 形式を使用します。

  - `epoch`: マイクロ秒精度のUnixエポック形式でタイムスタンプを表します。

  - `rfc3339`: RFC3339準拠の日時文字列形式。例：`2024-03-26T11:52:19.777087+00:00`

- **Time Offset**: ログのタイムスタンプに対するUTCからの時差を指定します。デフォルトはシステムに従い、値は `system` です。

設定が完了したら **Save Changes** をクリックしてください。

ファイルログが有効な場合（`log.to = file` または両方）、ログディレクトリには以下のファイルが生成されます。

- **emqx.log.N:** `emqx.log` を接頭辞としたログファイルで、EMQXの全ログメッセージを含みます。例：`emqx.log.1`、`emqx.log.2` など。
- **emqx.log.siz` と `emqx.log.idx:** ログローテーション情報を記録するシステムファイルです。**手動で変更しないでください**。

## 設定ファイルによるログ設定

EMQXのログは設定ファイルからも設定可能です。例えば警告レベルのログをファイルに出力したい場合やコンソールに出力したい場合は、`base.hocon` の `log` 以下の設定項目を以下のように変更します。設定はノード再起動後に反映されます。設定ファイルによるログ設定の詳細は[Configuration - Logs](../configuration/logs.md)を参照してください。

```bash
log {
  file {
    default {
      enable = true
      formatter = text
      level = warning
      path = "/Users/emqx/Downloads/emqx-560/log/emqx.log"
      rotation_count = 10
      rotation_size = 50MB
      time_offset = system
      timestamp_format = auto
  }
  console {
    formatter = json
    level = debug
    time_offset = system
    timestamp_format = auto
  }
}
```

## ログフォーマット

ログメッセージのフォーマット（各フィールドはスペースで区切られます）は以下の通りです。

```
**timestamp level tag clientid msg peername username ...**
```

各フィールドの意味は以下の通りです。

- **timestamp:** ログエントリ作成時刻を示すRFC-3339形式のタイムスタンプ。
- **level:** ログの重大度レベル。角括弧で囲まれた形式 `[level]` で、`info`、`warning`、`error` などの標準ログレベルが入ります。
- **tag:** ログの分類に使われるすべて大文字の単語。検索や分析を容易にするためのもの。例：MQTT、AUTHN、AUTHZ。
- **clientid:** 特定クライアントに関するログの場合のみ含まれ、そのクライアントを識別します。
- **msg:** ログメッセージの内容。検索性と可読性を高めるため、多くは `snake_case` 形式で記述されます（例：`mqtt_packet_received`）。ただしすべてのメッセージがこの形式とは限りません。
- **peername:** クライアントの接続元IPアドレスとポート番号を `IP:port` 形式で示します。
- **username:** 指定された非空のユーザー名を持つクライアントに関連するログの場合に含まれ、そのユーザー名を示します。
- **...:** msgフィールドの後に任意の追加フィールドが続くことがあります。

### ログメッセージ例

```bash
2024-03-20T11:08:39.568980+01:00 [warning] tag: AUTHZ, clientid: client1, msg: cannot_publish_to_topic_due_to_not_authorized, peername: 127.0.0.1:47860, username: user1, topic: republish-event/1, reason: not_authorized
```

## ログスロットリング

ログスロットリングは、指定された時間ウィンドウ内で繰り返し発生するイベントのログ出力を制限し、ログの氾濫を防ぐ機能です。最初のイベントのみをログに記録し、その後の同一イベントは抑制することで、観測性を損なわずにログ管理の効率化を図ります。

ダッシュボードからは **Management** -> **Logging** を選択し、**Throttling** タブをクリックしてスロットリングの時間ウィンドウを設定できます。デフォルトは1分で、最小設定値は1秒です。

<img src="./assets/log_throttling-ee.png" alt="ログスロットリング設定画面" style="zoom:67%;" />

設定ファイルで直接時間ウィンドウを指定する場合は以下のようにします。

```
log {
  throttling {
    time_window = "5m"
  }
}
```

ログスロットリングはデフォルトで有効で、認証失敗やメッセージキューのオーバーフローなど特定のログイベントに適用されます。ただし、`console` または `file` のログレベルが `debug` に設定されている場合は、詳細ログを確実に取得するためスロットリングは無効になります。

スロットリングが適用されるログイベントは以下の通りです。

- "authentication_failure"
- "authorization_permission_denied"
- "cannot_publish_to_topic_due_to_not_authorized"
- "cannot_publish_to_topic_due_to_quota_exceeded"
- "connection_rejected_due_to_license_limit_reached"
- "data_bridge_buffer_overflow"
- "dropped_msg_due_to_mqueue_is_full"
- "dropped_qos0_msg"
- "external_broker_crashed"
- "failed_to_fetch_crl"
- "failed_to_retain_message"
- "handle_resource_metrics_failed"
- "retain_failed_for_payload_size_exceeded_limit"
- "retain_failed_for_rate_exceeded_limit"
- "retained_delete_failed_for_rate_exceeded_limit"
- "socket_receive_paused_by_rate_limit"
- "transformation_failed"
- "unrecoverable_resource_error"
- "validation_failed"

::: tip 注意
スロットリング対象のイベントリストは随時更新される可能性があります。
:::

時間ウィンドウ内でイベントがスロットリングされた場合、各タイプのドロップされたイベント数を集計した警告メッセージがログに記録されます。例えば、1分間に5回の認可拒否が発生した場合、以下のようにログが記録されます。

```
2024-03-13T15:45:11.707574+02:00 [warning] clientid: test, msg: authorization_permission_denied, peername: 127.0.0.1:54870, username: test, topic: t/#, action: SUBSCRIBE(Q0), source: file
2024-03-13T15:45:53.634909+02:00 [warning] msg: log_events_throttled_during_last_period, period: 1 minutes, 0 seconds, dropped: #{authorization_permission_denied => 4}
```

ご覧の通り、最初の "authorization_permission_denied" イベントは完全にログに記録され、続く4件の同様のイベントは抑制されますが、その数は "log_events_throttled_during_last_period" の統計として記録されます。
