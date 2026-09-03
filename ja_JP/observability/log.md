# ログ

ログはトラブルシューティングやシステムパフォーマンスの最適化において信頼できる情報源を提供します。EMQXのログからアクセス状況、動作状況、ネットワークの問題に関する記録を確認できます。

EMQXはコンソールログとファイルログの両方をサポートしています。ログデータの出力方法は2通りあり、必要に応じて出力方法を選択するか、両方を有効にすることも可能です。コンソールログはログデータをコンソールやコマンドラインインターフェースに出力することを指し、主に開発やデバッグ時にリアルタイムでログを確認するために使われます。ファイルログはログデータをファイルに出力するもので、長期的な分析やトラブルシューティングのためにログを永続化する必要がある本番環境で一般的に使用されます。

システムのデフォルトのログ出力は環境変数 `EMQX_DEFAULT_LOG_HANDLER` により設定可能で、以下の設定を受け付けます。

- `file`: ログ出力をファイルに向ける
- `console`: ログ出力をコンソールに向ける

環境変数 `EMQX_DEFAULT_LOG_HANDLER` のデフォルトは `console` ですが、systemdの `emqx.service` ファイル経由でEMQXを起動すると明示的に `file` に設定されます。

ログデータが多すぎる場合やログ書き込みが遅い場合など、システム運用への影響を最小限に抑えるために、EMQXはデフォルトで過負荷保護機構を有効化し、ユーザーにより良いサービスを提供しています。

## ログレベル

EMQXのログレベルは8段階中6段階をサポートしており（[RFC 5424](https://www.ietf.org/rfc/rfc5424.txt)準拠）、デフォルトは `warning` です。低い順に以下のようになります。

```bash
debug < info < notice < warning < error < critical
```

以下の表は各ログレベルの意味と出力内容の例を示しています。

| ログレベル | 意味                                                         | 出力例                                                         |
| ---------- | ------------------------------------------------------------ | -------------------------------------------------------------- |
| debug      | プログラム内部の詳細情報で、コードのデバッグや診断に役立ちます。<br />本番環境で直接出力することは推奨されません。代わりに特定クライアント向けに[Log Trace](./tracer.md)を有効化してください。 | 変数の値、関数呼び出しスタックなどの詳細なデバッグ情報。       |
| info       | debugレベルより一般的な有用情報。                             | 認可拒否などの軽微な異常や、設定変更成功などの管理操作結果。     |
| notice     | イベント発生を示す重要なシステム情報で、特に対応は不要。       | ダッシュボードやCLIからのリクエストによるコンポーネント再起動。 |
| warning    | 対応が必要な潜在的な問題やエラー。重大問題になる前の監視に使用。 | 切断、接続タイムアウト、認証失敗などのイベント。               |
| error      | エラー発生を示し、管理者が迅速に問題を検知・解決できるようにする。 | 外部データベース接続失敗、存在しないトピックのサブスクライブ失敗、設定ファイル解析失敗など。 |
| critical   | システムクラッシュや機能停止を引き起こす重大なエラー。即時対応が必要。 | 設定ミスによりコンポーネントが起動・正常動作できない場合。       |

::: warning 重要なお知らせ

接続およびパーサーエラーのログに含まれる生のMQTTパケットデータはデフォルトでマスクされています。トラブルシューティングのために一時的に生パケットデータをログに記録したい場合は、リスナーの `allow_log_packet_data_from` オプションに信頼できるクライアントのIPアドレスまたはCIDRレンジを追加してください。このオプションは信頼できるクライアントに対してのみ、かつ診断時のみ有効にしてください。生パケットデータには認証情報などの機密情報が含まれる可能性があります。

:::

## ダッシュボードによるログ設定

このセクションでは主にEMQXダッシュボードでのログ設定方法を説明します。設定変更はノードの再起動なしに即時反映されます。

EMQXダッシュボードにアクセスし、左側メニューの **Management** -> **Logging** をクリックします。コンソールログまたはファイルログの設定はそれぞれ対応するタブを選択してください。

### コンソールログの設定

**Logging** ページで **Console Log** タブを選択します。

<img src="./assets/config-console-log-1-ee.png" alt="コンソールログ設定画面" style="zoom:67%;" />

コンソールログ出力の設定項目は以下の通りです。

- **Enable Log Output**: トグルスイッチをクリックしてコンソールログ出力を有効化します。

- **Log Level**: 記録する最小ログレベルを選択します。選択肢は `debug`、`info`、`notice`、`warning`、`error`、`critical`、`alert`、`emergency` です。デフォルトは `warning`。

- **Log Formatter**: ログのフォーマットを選択します。`text`（自由形式テキスト）または `json`（構造化ログ）から選べます。デフォルトは `text`。

- **Timestamp Format**: ログのタイムスタンプ形式を選択します。以下から選択可能です。
  - `auto`: ログフォーマッターに応じて自動判別。テキストフォーマッターは `rfc3339`、JSONフォーマッターは `epoch` 形式を使用。
  - `epoch`: マイクロ秒精度のUnixエポック形式。
  - `rfc3339`: RFC3339準拠の日時文字列（例：`2024-03-26T11:52:19.777087+00:00`）。

- **Time Offset**: ログのタイムスタンプに使用する時刻オフセットを設定します。`system`（ローカルシステムのオフセット）、`utc`（UTC）、または `+-[hh]:[mm]` 形式の固定オフセット（例：`-02:00`、`+00:00`）を入力します。デフォルトは `system`。JSONログはUnixエポック形式のため影響を受けません。

- **Payload Encode**: ログエントリ内のペイロードデータのエンコード方法を選択します。以下の値があります。
  - `text`: テキストエンコード。テキストベースのプロトコルやJSONエンコードされたペイロードに推奨。
  - `hex`: 16進数エンコード。カスタムバイナリプロトコルに推奨。
  - `hidden`: ペイロードを `******` に置き換え。

  デフォルトは `text`。

設定が完了したら **Save Changes** をクリックしてください。

### ファイルログの設定

**Logging** ページで **File Log** タブを選択します。

<img src="./assets/config-file-log-1-ee.png" alt="ファイルログ設定画面" style="zoom:67%;" />

ファイルログ出力の設定項目は以下の通りです。

- **Enable Log Output**: トグルスイッチをクリックしてファイルログ出力を有効化します。

- **Log File Name**: ログファイルのパスとファイル名を入力します。デフォルトは `${EMQX_LOG_DIR}/emqx.log` で、`${EMQX_LOG_DIR}` はEMQXのログディレクトリです。

- **Max Log Files Number**: ローテーションする最大ログファイル数を指定します。デフォルトは `10`。

- **Rotation Size**: ログファイルの最大サイズ（ローテーション前）を設定します。値を入力し、`KB`、`MB`、`GB` を選択します。デフォルトは `50 MB`。トグルをオフにすると `infinity` となり、サイズによるローテーションは行われません。

- **Log Level**: 記録する最小ログレベルを選択します。選択肢は `debug`、`info`、`notice`、`warning`、`error`、`critical`、`alert`、`emergency` です。デフォルトは `warning`。

- **Log Formatter**: ログのフォーマットを選択します。`text`（自由形式テキスト）または `json`（構造化ログ）から選べます。デフォルトは `text`。

- **Timestamp Format**: ログのタイムスタンプ形式を選択します。以下から選択可能です。
  - `auto`: ログフォーマッターに応じて自動判別。テキストフォーマッターは `rfc3339`、JSONフォーマッターは `epoch` 形式を使用。
  - `epoch`: マイクロ秒精度のUnixエポック形式。
  - `rfc3339`: RFC3339準拠の日時文字列（例：`2024-03-26T11:52:19.777087+00:00`）。

- **Time Offset**: ログのタイムスタンプに使用する時刻オフセットを設定します。`system`（ローカルシステムのオフセット）、`utc`（UTC）、または `+-[hh]:[mm]` 形式の固定オフセット（例：`-02:00`、`+00:00`）を入力します。デフォルトは `system`。JSONログはUnixエポック形式のため影響を受けません。

- **Payload Encode**: ログエントリ内のペイロードデータのエンコード方法を選択します。以下の値があります。
  - `text`: テキストエンコード。テキストベースのプロトコルやJSONエンコードされたペイロードに推奨。
  - `hex`: 16進数エンコード。カスタムバイナリプロトコルに推奨。
  - `hidden`: ペイロードを `******` に置き換え。

  デフォルトは `text`。

設定が完了したら **Save Changes** をクリックしてください。

ファイルログが有効化されている場合（`log.to = file` または両方）、ログディレクトリに以下のファイルが生成されます。

- **emqx.log.N:** `emqx.log` を接頭辞としたログファイルで、EMQXの全ログメッセージを含みます。例：`emqx.log.1`、`emqx.log.2` など。
- **emqx.log.siz` と `emqx.log.idx:** ログローテーション情報を記録するシステムファイルです。**手動で変更しないでください**。

## 設定ファイルによるログ設定

設定ファイルを使ってEMQXのログ設定を行うことも可能です。例えば、警告レベルのログをファイルに出力したい場合やコンソールに出力したい場合は、`base.hocon` の `log` 以下の設定項目を以下のように変更します。設定はノード再起動後に反映されます。設定ファイルによるログ設定の詳細は [Configuration - Logs](../configuration/logs.md) を参照してください。

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
- **level:** ログの重要度レベル。角括弧で囲まれた形式 `[level]` で、`info`、`warning`、`error` などの標準ログレベルが入ります。
- **tag:** ログの分類に使われる全大文字の単語。検索や分析を容易にするためのもの。例：MQTT、AUTHN、AUTHZ
- **clientid:** 特定のクライアントに関するログの場合のみ含まれ、ログに関連するクライアントを識別します。
- **msg:** ログメッセージの内容。検索性と可読性を高めるため、多くはスネークケース形式（例：`mqtt_packet_received`）を採用しています。ただし全てのメッセージがこの形式とは限りません。
- **peername:** クライアントの送信元IPアドレスとポート番号（`IP:port`形式）で、接続元を示します。
- **username:** クライアントに指定された空でないユーザー名がある場合に含まれ、そのユーザー名を示します。
- **...:** msgフィールドの後に任意の追加フィールドが続くことがあります。

### ログメッセージ例

```bash
2024-03-20T11:08:39.568980+01:00 [warning] tag: AUTHZ, clientid: client1, msg: cannot_publish_to_topic_due_to_not_authorized, peername: 127.0.0.1:47860, username: user1, topic: republish-event/1, reason: not_authorized
```

## ログスロットリング

ログスロットリングは、指定した時間枠内で繰り返される同一イベントのログ記録を制限し、ログの氾濫を防ぐ機能です。最初のイベントのみをログに記録し、同一イベントの後続ログは抑制することで、観測性を損なわずにログ管理を効率化します。

ダッシュボードで **Management** -> **Logging** を選択し、**Throttling** タブをクリックするとスロットリングの時間枠を設定できます。デフォルトは1分、最小設定は1秒です。

<img src="./assets/log_throttling-ee.png" alt="ログスロットリング設定画面" style="zoom:67%;" />

設定ファイルで直接時間枠を指定することも可能です。

```
log {
  throttling {
    time_window = "5m"
  }
}
```

ログスロットリングはデフォルトで有効化されており、認証失敗やメッセージキューのオーバーフローなど特定のログイベントに適用されます。ただし、`console` または `file` のログレベルが `debug` に設定されている場合は、詳細ログを確保するためスロットリングは無効になります。

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

時間枠内でイベントがスロットリングされた場合、各イベントタイプごとに抑制された件数を集約した警告メッセージがログに記録されます。例えば、1分間に5回の認可拒否が発生した場合、以下のようにログが出力されます。

```
2024-03-13T15:45:11.707574+02:00 [warning] clientid: test, msg: authorization_permission_denied, peername: 127.0.0.1:54870, username: test, topic: t/#, action: SUBSCRIBE(Q0), source: file
2024-03-13T15:45:53.634909+02:00 [warning] msg: log_events_throttled_during_last_period, period: 1 minutes, 0 seconds, dropped: #{authorization_permission_denied => 4}
```

最初の1件は完全にログに記録され、残り4件は抑制されますが、抑制件数は `log_events_throttled_during_last_period` に記録されます。

## 本番環境でのログ集中管理

本番環境では、各EMQXノードのログをEMQXクラスター外の中央システムに送信してください。ブローカーのホストにのみログを保持すると、ノードやストレージの障害時にログが利用できなくなる恐れがあります。中央集約により、CoreノードとReplicantノード間のイベント相関や、メトリクスや組み込みアラームに現れない状態のアラート化も可能になります。

### 収集方法の選択

以下の収集パターンのいずれかを利用してください。

- Kubernetesなどのコンテナ化環境では、JSONログをコンソールに出力し、プラットフォームのログエージェントでコンテナ出力を収集する。
- ファイルログの場合は、`emqx.log.N` ファイルを収集し、ローテーションを重複なく処理し、構造化フィールドを保持するログエージェントを使用する。
- [OpenTelemetryログハンドラー](./opentelemetry/logs.md)を使い、OpenTelemetry Collectorおよび対応バックエンドにログをエクスポートする。

### コンテキストの追加とログ保護

収集パイプラインでクラスター、ノード、ノードロール、EMQXバージョン、アベイラビリティゾーンなどのデプロイ情報を付加してください。

集中管理されたログは運用データとして保護してください。ログフィールドにはクライアントID、ユーザー名、トピック、ピアアドレス、エラー詳細などが含まれる場合があります。

### 収集パイプラインの監視

コレクターやトランスポートのヘルス指標、またはアプリケーションログ量に依存しない明示的なハートビートを使って収集経路を監視してください。以下の状態に対してアラートを設定します。

- コレクターまたはトランスポートが異常状態である。
- コレクターまたはトランスポートがレコードを拒否または破棄している。
- 中央バックエンドのストレージ容量が逼迫している。

単に到達可能なEMQXノードがログを生成していないだけでアラートを出すべきではありません。アイドル状態や正常なノードは設定された重要度で報告すべきログがない場合があります。

### ログアラートポリシーの定義

安定した構造化フィールド（例：`level`、`msg`）にマッチするログベースのアラートを選択的に作成してください。

- **Warningイベント:** 早期警告として有用ですが、クライアントの通常動作によるものもあるため、個別イベントでの対応は不要な場合が多いです。レートや通常基準からの逸脱を使って監視してください。
- **ErrorまたはCriticalイベント:** レプリケーション喪失、設定同期失敗、リスナー起動失敗、永続ストレージ障害などは通常即時アラートが必要です。

推奨されるメトリクスおよびログベースのアラートセット（Mriaレプリケーション信号を含む）は [Production Monitoring Best Practices](./monitoring-best-practices.md#centralize-logs-and-alert-selectively) を参照してください。
