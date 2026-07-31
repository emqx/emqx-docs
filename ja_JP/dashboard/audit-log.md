# 監査ログ

監査ログ機能は、EMQXクラスターにおける重要な運用変更をリアルタイムで追跡することを可能にします。監査ログを通じて、エンタープライズユーザーは誰がどの重要な操作をどのように、いつ行ったかを簡単に把握できます。これは、エンタープライズユーザーが規制要件に準拠し、運用中のデータセキュリティ監査を確実に行うための重要なツールです。

EMQX監査ログは、[ダッシュボード](../dashboard/introduction.md)、[REST API](../admin/api.md)、および[CLI](../admin/cli.md)からの変更に関連する操作を記録することをサポートしています。例えば、ダッシュボードのユーザーログインやクライアント、アクセス制御、データ統合の変更などです。ただし、メトリクス取得やクライアントリストの照会などの読み取り専用操作は記録されません。

EMQXは、監査ログの管理を支援するためにダッシュボードビューとログシステムとの統合を提供しています。これらの方法を通じて、EMQXは柔軟かつ包括的な監査ログのサポートを実現し、エンタープライズユーザーがニーズに応じて最適な方法で監査ログを管理・閲覧できるようにしています。

## 監査ログの有効化

監査ログ機能は、ダッシュボードおよび設定ファイルの両方から有効化および設定パラメータの調整が可能です。

### ダッシュボードでの監査ログ有効化

監査ログを有効化し、設定パラメータを変更するには、ダッシュボードの **Management** -> **Logging** -> **Audit Log** または **System** -> **Audit Log** にアクセスしてください。

<img src="./assets/audit_log_config.png" alt="監査ログ設定" style="zoom:50%;" />

監査ログに対して以下のオプションを設定できます。

- **Enable Log Handler**: 監査ログ処理プロセスの有効化または無効化。デフォルトで有効です。
- **Audit Log File Name**: 監査ログファイルのパスとファイル名を指定します。デフォルト値は `${EMQX_LOG_DIR}/audit.log` で、`${EMQX_LOG_DIR}` は変数であり、デフォルトは `./log` です。つまり最終的には `./log/audit.log.1` に保存されます。
- **Maximum Log Files Number**: ローテーションされるログファイルの最大数。デフォルトは `10` です。
- **Rotation Size**: ログファイルのサイズを設定し、指定サイズに達するとログファイルがローテーションされます。無効にするとログファイルは無制限に成長します。テキストボックスに値を入力し、ドロップダウンリストから `MB`、`GB`、`KB` などの単位を選択できます。デフォルトは `50MB` です。
- **Cache Size**: データベースに保存される最大レコード数を決定し、ダッシュボードおよび `/audit` API からアクセス・取得可能です。デフォルトは `5000` です。

  ::: tip 注意
  `log.audit.max_filter_size` は後方互換性のためエイリアスとして残されています。
  :::

- **Ignore High Frequency Request**: パブリッシュ／サブスクライブやクライアントのキックアウトなどの高頻度リクエストを無視し、監査ログの過剰な記録を防ぐかどうかを制御します。デフォルトで有効です。
- **Timestamp Format**: ログエントリのタイムスタンプ形式。選択肢は以下の通りです。
  - `auto`: ログフォーマッターに基づき最適な形式を自動選択。JSONは `epoch`、テキストは `rfc3339`。
  - `epoch`: マイクロ秒単位のUnixエポック時間。
  - `rfc3339`: RFC3339形式。
- **Time Offset**: ログエントリのタイムスタンプをフォーマットする際の時刻オフセット。選択肢は以下の通りです。
  - `system`: ローカルシステムの時刻オフセット。
  - `utc`: UTC時刻オフセット。
  - `+-[hh]:[mm]`: ユーザー指定の時刻オフセット（例：`"-02:00"`、`"+00:00"`）。

  デフォルトは `system` です。
- **Payload Encode**: ログエントリ内のペイロードデータのエンコード方法。`text`、`hex`、`hidden` から選択可能。デフォルトは `text` です。

### 設定ファイルでの監査ログ有効化

`base.hocon` ファイルの `log.audit` セクションで監査ログの有効化および設定変更も可能です。以下は例です。

```hocon
log.audit {
  path = "./log/audit.log"
  rotation_count = 10
  rotation_size = 50MB
  cache_size = 5000
  ignore_high_frequency_request = true
  timestamp_format = auto
  time_offset = system
  payload_encode = text
}
```

## ダッシュボードでの監査ログ閲覧

監査ログが有効化されると、ダッシュボードの **System** -> **Audit Log** から監査ログの内容を閲覧できます。

![image-20231214143911786](./assets/audit_log_list.png)

### 検索フィルター

以下の検索キーワードでログ操作をフィルタリング・検索できます。

- **開始時間** - **終了時間**: 操作が行われた時間範囲。
- **ソースタイプ**: 操作が行われた方法。選択肢は `Dashboard`、`REST API`、`CLI`、`Erlang Console`。ここで `Erlang Console` は通常、EMQによるオンサイト技術サポート時に使用されるErlang Shellコンソールを指します。
- **オペレーター**: ダッシュボードのユーザー名またはREST API呼び出しに使用されたキー名。操作方法がダッシュボードまたはREST APIの場合のみ有効です。
- **IP**: ダッシュボードユーザーまたはREST APIを呼び出したクライアントの送信元IP。操作方法がダッシュボードまたはREST APIの場合のみ表示されます。
- **操作名**: 監査ログでサポートされている操作名のドロップダウンリストから選択。
- **操作結果**: `成功` または `失敗` のドロップダウンリストから選択。

### リストの説明

表示される監査ログリストの各列の説明は以下の通りです。

- **操作時間**: 操作が行われた時間。
- **情報**:
  - ダッシュボードまたはREST APIの場合は操作名を表示。
  - CLIおよびコンソールの場合は実行されたコマンドを記録。
- **オペレーター**: 操作方法と対応するオペレーターを含みます。CLIおよびコンソール操作の場合、オペレーターはコマンドが実行されたEMQXノード名です。
- **IP**: ダッシュボードユーザーまたはREST APIを呼び出したクライアントの送信元IP。操作方法がダッシュボードまたはREST APIの場合のみ表示されます。
- **操作結果**: `成功` または `失敗`。失敗にはフォーム検証エラーやリソース削除不可などのシナリオが含まれます。ダッシュボードまたはREST APIの操作方法にのみ表示され、CLIおよびコンソールは操作結果を記録できません。

## ログファイルでの監査ログ閲覧

EMQXで監査ログが有効化されると、変更に関連する操作は `./log/audit.log.1` ファイルにログ形式で保存されます。エンタープライズユーザーは監査記録の詳細分析や既存のログ管理システムへの統合を容易に行え、コンプライアンスおよびデータセキュリティ要件を満たせます。

::: warning 注意

コマンドライン操作の監査ログには機密情報が含まれる可能性があるため、ログコレクターに送信する際は注意が必要です。ログ内容のフィルタリングや暗号化伝送の利用を推奨し、不正な情報漏洩を防止してください。

:::

監査ログに含まれるフィールドは、操作記録のソースによって異なります。

### ダッシュボードまたはREST APIからの操作記録

ダッシュボードまたはREST APIの操作を記録する監査ログには、操作ユーザー、操作対象、操作結果の情報が含まれます。ログメッセージのフォーマット例は以下の通りです。

```bash
{"time":1702604675872987,"level":"info","source_ip":"127.0.0.1","operation_type":"mqtt","operation_result":"success","http_status_code":204,"http_method":"delete","operation_id":"/mqtt/retainer/message/:topic","duration_ms":4,"auth_type":"jwt_token","query_string":{},"from":"dashboard","source":"admin","node":"emqx@127.0.0.1","http_request":{"method":"delete","headers":{"user-agent":"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36","sec-fetch-site":"same-origin","sec-fetch-mode":"cors","sec-fetch-dest":"empty","sec-ch-ua-platform":"\"macOS\"","sec-ch-ua-mobile":"?0","sec-ch-ua":"\"Google Chrome\";v=\"119\", \"Chromium\";v=\"119\", \"Not?A_Brand\";v=\"24\"","referer":"http://localhost:18083/","origin":"http://localhost:18083","host":"localhost:18083","connection":"keep-alive","authorization":"******","accept-language":"zh-CN,zh;q=0.9,zh-TW;q=0.8,en;q=0.7","accept-encoding":"gzip, deflate, br","accept":"*/*"},"body":{},"bindings":{"topic":"$SYS/brokers/emqx@127.0.0.1/version"}}}
```

上記ログメッセージサンプルに含まれるフィールドは以下の通りです。

| フィールド名         | 型       | 説明                                                      |
| -------------------- | -------- | --------------------------------------------------------- |
| time                 | 整数     | ログ記録の時間をマイクロ秒単位で表したタイムスタンプ。    |
| level                | 文字列   | ログレベル。                                              |
| source_ip            | 文字列   | 操作の送信元IPアドレス。                                  |
| operation_type       | 文字列   | 操作の機能モジュール。REST APIのタグに対応。              |
| operation_result     | 文字列   | 操作結果。`success` は成功、`failure` は失敗を示す。      |
| http_status_code     | 文字列   | HTTPレスポンスステータスコード。                          |
| http_method          | 文字列   | HTTPリクエストメソッド。                                  |
| duration_ms          | 整数     | 操作実行時間（ミリ秒単位）。                              |
| auth_type            | 文字列   | 認証タイプ。認証に使用された方法や仕組みを示し、ダッシュボードは `jwt_token`、REST APIは `api_key` に固定。 |
| query_string         | オブジェクト | HTTPリクエストのURLクエリパラメータ。                    |
| from                 | 文字列   | リクエストの送信元。`dashboard` はダッシュボード、`rest_api` はREST APIを示す。`cli`、`erlang_console` の場合はCLIまたはErlang Shellからの操作であり、このログ構造は該当しません。 |
| source               | 文字列   | 操作を行ったダッシュボードのユーザー名またはAPIキー名。  |
| node                 | 文字列   | 操作が実行されたノード名またはサーバー名。                |
| method               | 文字列   | HTTPリクエストメソッド。`post`、`put`、`delete` はそれぞれ作成、更新、削除操作に対応。 |
| operate_id           | 文字列   | リクエストのREST APIパス。詳細は[REST API](../admin/api.md)を参照。 |

### CLIまたはErlangコンソールからの操作記録

CLIまたはErlangコンソールの操作を記録する監査ログには、実行されたコマンドや呼び出しパラメータなどの情報が含まれます。ログメッセージのフォーマット例は以下の通りです。

```bash
{"time":1695866030977555,"level":"info","msg":"from_cli","from": "cli","node":"emqx@127.0.0.1","duration_ms":0,"cmd":"retainer","args":["clean", "t/1"]}
```

上記ログメッセージサンプルに含まれるフィールドは以下の通りです。

| フィールド名  | 型       | 説明                                                      |
| ------------ | -------- | --------------------------------------------------------- |
| time         | 整数     | ログ記録の時間をマイクロ秒単位で表したタイムスタンプ。    |
| level        | 文字列   | ログレベル。                                              |
| msg          | 文字列   | 操作の説明。                                              |
| from         | 文字列   | リクエストの送信元。`cli` はCLI、`erlang_console` はErlang Shellを示す。`dashboard`、`rest_api` の場合はダッシュボードまたはREST APIからの操作であり、このログ構造は該当しません。 |
| node         | 文字列   | 操作が実行されたノード名またはサーバー名。                |
| duration_ms  | 整数     | 操作の実行時間（ミリ秒単位）。                            |
| cmd          | 文字列   | 実行された具体的なコマンド操作。対応するコマンドは[CLI](../admin/cli.md)を参照。 |
| args         | 配列     | コマンドに付随する追加パラメータ。複数パラメータは配列で区切られる。 |
