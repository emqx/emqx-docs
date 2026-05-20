# ログ

ログはトラブルシューティングやシステムパフォーマンスの最適化において信頼できる情報源を提供します。EMQXのログからアクセス状況、動作、ネットワークの問題に関する記録を確認できます。

EMQXはコンソールログとファイルログの両方をサポートしています。ログデータの出力方法は2通りあり、必要に応じて出力方法を選択するか、両方を併用できます。コンソールログはログデータをコンソールやコマンドラインインターフェースに出力することを指し、開発やデバッグ時にリアルタイムでログを素早く確認するために使われます。ファイルログはログデータをファイルに出力するもので、主に本番環境でログデータを長期間保存し、分析やトラブルシューティングに利用されます。

システムのデフォルトのログ処理動作は環境変数 `EMQX_DEFAULT_LOG_HANDLER` により設定可能で、以下の値を受け付けます。

- `file`: ログ出力をファイルに向ける。
- `console`: ログ出力をコンソールに向ける。

環境変数 `EMQX_DEFAULT_LOG_HANDLER` のデフォルトは `console` ですが、systemdの `emqx.service` ファイル経由でEMQXを起動する場合は明示的に `file` に設定されます。

ログデータが多すぎる場合やログ書き込みが遅い場合など、ログがシステム動作に与える影響を最小化するために、EMQXはデフォルトでオーバーロード保護機構を有効にし、ユーザーにより良いサービスを提供しています。

## ログレベル

EMQXのログレベルは8段階中6段階を採用しており（[RFC 5424](https://www.ietf.org/rfc/rfc5424.txt)準拠）、デフォルトは `warning` です。低い順に以下の通りです。

```bash
debug < info < notice < warning < error < critical
```

以下の表は各ログレベルの意味と出力内容の例を示しています。

| ログレベル | 意味                                                         | 出力例                                                         |
| ---------- | ------------------------------------------------------------ | -------------------------------------------------------------- |
| debug      | プログラム内部の詳細な動作情報で、コードのデバッグや問題診断に役立ちます。<br />本番環境で直接出力することは推奨されません。代わりに特定のクライアントに対して[Log Trace](./tracer.md)を有効にしてください。 | 変数の値、関数呼び出しスタック、その他詳細なデバッグデータ。   |
| info       | debugより一般的な有用情報。                                   | 認可拒否などの軽微な異常や、設定変更成功などの管理操作結果。    |
| notice     | イベント発生を示す重要なシステム情報で、特に対応は不要。       | ダッシュボードやCLIからの要求によるコンポーネントの再起動。     |
| warning    | 対応が必要な潜在的な問題やエラー。重大問題になる前の監視に使われます。 | 切断、接続タイムアウト、認証失敗などの類似イベント。           |
| error      | エラー発生を示し、管理者が迅速に検知・対応できるようにします。 | 外部データベース接続失敗、存在しないトピックへのサブスクライブ失敗、設定ファイル解析失敗など。 |
| critical   | システムクラッシュや機能停止を引き起こす重大エラー。管理者が即時対応すべき問題を示します。 | 設定ミスによりコンポーネントが起動・正常動作できない場合。     |

## ダッシュボードによるログ設定

このセクションでは主にEMQXダッシュボードを使ったログ設定方法を説明します。設定変更はノードの再起動なしに即時反映されます。

EMQXダッシュボードにアクセスし、左側のナビゲーションメニューから **Management** -> **Logging** をクリックします。コンソールログまたはファイルログの設定はそれぞれ対応するタブを選択して行います。

### コンソールログの設定

**Logging** ページで **Console Log** タブを選択します。

<img src="./assets/config-console-log-1-ee.png" alt="コンソールログ設定画面" style="zoom:67%;" />

コンソールログハンドラーの以下の設定を行います。

- **Enable Log Handler**: トグルスイッチをクリックしてコンソールログハンドラーを有効化します。

- **Log Level**: ドロップダウンリストから使用するログレベルを選択します。デフォルトは `warning` です。

- **Log Formatter**: ログフォーマットをドロップダウンリストから選択します。選択肢は `text` と `JSON` で、デフォルトは `text` です。

- **Timestamp Format**: ログのタイムスタンプ形式を選択します。選択肢は以下の通りです。
  - `auto`: 使用しているログフォーマッターに応じて自動判別します。textフォーマッターの場合は `rfc3339`、JSONフォーマッターの場合は `epoch` 形式を利用します。
  - `epoch`: マイクロ秒精度のUnixエポック形式タイムスタンプ。
  - `rfc3339`: RFC3339準拠の日付時刻文字列形式。例：`2024-03-26T11:52:19.777087+00:00`。

- **Time Offset**: ログのUTCに対する時差を定義します。デフォルトはシステムに従い、値は `system` です。

設定が完了したら **Save Changes** をクリックします。

### ファイルログの設定

**Logging** ページで **File Log** タブを選択します。

<img src="./assets/config-file-log-1-ee.png" alt="ファイルログ設定画面" style="zoom:67%;" />

ファイルログハンドラーの以下の設定を行います。

- **Enable Log Handler**: トグルスイッチをクリックしてファイルログハンドラーを有効化します。

- **Log File Name**: ログファイル名を入力します。デフォルトは `log/emqx.log` です。

- **Max Log Files Number**: ローテーションするログファイルの最大数を指定します。デフォルトは `10` です。

- **Rotation Size**: ログファイルが指定サイズに達したらローテーションします。デフォルトで有効です。具体的な値をテキストボックスに入力してください。無効にすると値は `infinity` となり、ログファイルは無制限に成長します。

- **Log Level**: 使用するログレベルをドロップダウンリストから選択します。選択肢は `debug`、`info`、`notice`、`warning`、`error`、`critical` で、デフォルトは `warning` です。

- **Log Formatter**: ログフォーマットをドロップダウンリストから選択します。選択肢は `text` と `JSON` で、デフォルトは `text` です。

- **Timestamp Format**: ログのタイムスタンプ形式を選択します。選択肢は以下の通りです。

  - `auto`: 使用しているログフォーマッターに応じて自動判別します。textフォーマッターの場合は `rfc3339`、JSONフォーマッターの場合は `epoch` 形式を利用します。

  - `epoch`: マイクロ秒精度のUnixエポック形式タイムスタンプ。

  - `rfc3339`: RFC3339準拠の日付時刻文字列形式。例：`2024-03-26T11:52:19.777087+00:00`。

- **Time Offset**: ログのUTCに対する時差を定義します。デフォルトはシステムに従い、値は `system` です。

設定が完了したら **Save Changes** をクリックします。

ファイルログが有効（log.to = file または both）になると、ログディレクトリに以下のファイルが出現します。

- **emqx.log.N:** `emqx.log` を接頭辞とするログファイルで、EMQXの全ログメッセージを含みます。例：`emqx.log.1`、`emqx.log.2` など。
- **emqx.log.siz と emqx.log.idx:** ログローテーション情報を記録するシステムファイルです。**手動で変更しないでください**。

## 設定ファイルによるログ設定

EMQXのログは設定ファイルからも設定可能です。例えば、警告レベルのログをファイルに出力する場合やコンソール出力する場合は、`base.hocon` の `log` 以下の設定項目を以下のように変更します。設定はノード再起動後に反映されます。設定ファイルによるログ設定の詳細は [Configuration - Logs](../configuration/logs.md) を参照してください。

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
- **level:** ログの重大度レベル。ブラケットで囲まれた形式 `[level]`。`info`、`warning`、`error` などの標準ログレベル。
- **tag:** ログのカテゴリを示す全大文字の単語。検索や分析を容易にするために使用。例：MQTT、AUTHN、AUTHZ。
- **clientid:** 特定のクライアントに関するログの場合に含まれ、そのクライアントを識別します。
- **msg:** ログメッセージの内容。検索性と可読性向上のため、多くは `snake_case` 形式（例：`mqtt_packet_received`）を採用しています。すべてのメッセージがこの形式とは限りません。
- **peername:** クライアントの接続元IPアドレスとポート番号を `IP:port` 形式で示します。
- **username:** 非空のユーザー名が指定されたクライアントに関連するログにのみ含まれます。該当クライアントのユーザー名を示します。
- **...:** msgフィールドの後に任意の追加フィールドが続くことがあります。

### ログメッセージ例

```bash
2024-03-20T11:08:39.568980+01:00 [warning] tag: AUTHZ, clientid: client1, msg: cannot_publish_to_topic_due_to_not_authorized, peername: 127.0.0.1:47860, username: user1, topic: republish-event/1, reason: not_authorized
```

## ログスロットリング

ログスロットリングは、指定した時間ウィンドウ内で繰り返される同一イベントのログ出力を制限し、ログの洪水を防ぐ機能です。最初のイベントのみをログに記録し、その後の同一イベントを抑制することで、ログ管理の効率化と可観測性の維持を両立します。

ダッシュボードからは、**Management** -> **Logging** を選択し、**Throttling** タブをクリックしてスロットリングの時間ウィンドウを設定できます。デフォルトは1分で、最小設定値は1秒です。

<img src="./assets/log_throttling-ee.png" alt="ログスロットリング設定画面" style="zoom:67%;" />

設定ファイルで直接時間ウィンドウを設定する場合は以下のように記述します。

```
log {
  throttling {
    time_window = "5m"
  }
}
```

ログスロットリングはデフォルトで有効で、認証失敗やメッセージキューオーバーフローなど特定のログイベントに適用されます。ただし、`console` または `file` のログレベルが `debug` に設定されている場合は、詳細ログ出力のためスロットリングは無効になります。

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
スロットリング対象イベントのリストは更新される可能性があります。
:::

時間ウィンドウ内でイベントがスロットリングされた場合、各種イベントのドロップ数を集計した警告メッセージがログに記録されます。例えば、1分間に5回の認可拒否が発生した場合、以下のようにログが出力されます。

```
2024-03-13T15:45:11.707574+02:00 [warning] clientid: test, msg: authorization_permission_denied, peername: 127.0.0.1:54870, username: test, topic: t/#, action: SUBSCRIBE(Q0), source: file
2024-03-13T15:45:53.634909+02:00 [warning] msg: log_events_throttled_during_last_period, period: 1 minutes, 0 seconds, dropped: #{authorization_permission_denied => 4}
```

上記の例では、最初の1件は完全にログ出力され、残り4件はスロットリングにより抑制されましたが、その数は `log_events_throttled_during_last_period` の統計に記録されています。
