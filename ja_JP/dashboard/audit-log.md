# 監査ログ

監査ログ機能は、EMQXクラスターにおける重要な運用変更をリアルタイムで追跡することを可能にします。監査ログを通じて、エンタープライズユーザーは誰がどの重要な操作を、どのように、いつ実行したかを容易に把握できます。これは、エンタープライズユーザーが規制要件を遵守し、運用中のデータセキュリティ監査を確実に行うための重要なツールです。

EMQX監査ログは、[ダッシュボード](../dashboard/introduction.md)、[REST API](../admin/api.md)、および[CLI](../admin/cli.md)からの変更に関連する操作を記録します。例えば、ダッシュボードのユーザーログインやクライアント、アクセス制御、データ統合の変更などです。ダッシュボードおよびREST APIにおいては、メトリクス取得やクライアントリスト照会などの読み取り専用操作は記録されません。CLIコマンドは、データを変更するかどうかに関わらず記録されますが、[CLIまたはErlangコンソールからの操作記録](#operation-records-from-cli-or-erlang-console)に記載の例外があります。

EMQXは、監査ログの管理を支援するためにダッシュボードビューとログシステムとの統合を提供しています。これらの方法を通じて、EMQXは監査ログに対して柔軟かつ包括的なサポートを提供し、エンタープライズユーザーがニーズに応じて最適な管理・閲覧方法を選択できるようにしています。

## 監査ログの有効化

監査ログ機能は、ダッシュボードおよび設定ファイルの両方から有効化および設定パラメータの調整が可能です。

### ダッシュボードでの監査ログ有効化

監査ログを有効化し、設定パラメータを変更するには、ダッシュボードの **Management** -> **Logging** -> **Audit Log**、または **System** -> **Audit Log** にアクセスしてください。

<img src="./assets/audit_log_config.png" alt="監査ログ設定" style="zoom:50%;" />

監査ログに対して以下のオプションを設定できます。

- **Enable Log Handler**：監査ログ処理プロセスを有効または無効にします。デフォルトで有効です。
- **Audit Log File Name**：監査ログファイルのパスと名前を指定します。デフォルト値は `${EMQX_LOG_DIR}/audit.log` で、`${EMQX_LOG_DIR}` は変数でありデフォルトは `./log`、つまり最終的に `./log/audit.log.1` に保存されます。
- **Maximum Log Files Number**：ローテーションされるログファイルの最大数です。デフォルトは `10` です。
- **Rotation Size**：ログファイルのサイズを設定し、指定サイズに達するとログファイルをローテーションします。無効にするとログファイルは無制限に増加します。テキストボックスに値を入力し、ドロップダウンリストから `MB`、`GB`、`KB` などの単位を選択できます。デフォルトは `50MB` です。
- **Cache Size**：データベースに保存される最大レコード数を決定し、ダッシュボードおよび `/audit` API を通じてアクセス・取得可能です。デフォルトは `5000` です。

  ::: tip 注意
  `log.audit.max_filter_size` は後方互換性のためエイリアスとして残されています。
  :::

- **Ignore High Frequency Request**：パブリッシュ／サブスクライブやクライアントのキックアウトなどの高頻度リクエストを無視し、監査ログの過剰な記録を防ぐかどうかを制御します。デフォルトで有効です。
- **Timestamp Format**：ログエントリのタイムスタンプ形式を指定します。選択肢は以下の通りです。
  - `auto`：ログフォーマッターに基づき最適な形式を自動選択。JSONは `epoch`、テキストは `rfc3339`。
  - `epoch`：マイクロ秒単位のUnixエポック時間。
  - `rfc3339`：RFC3339形式。
- **Time Offset**：ログエントリのタイムスタンプの時間オフセットを指定します。選択肢は以下の通りです。
  - `system`：ローカルシステムの時間オフセット。
  - `utc`：UTC時間オフセット。
  - `+-[hh]:[mm]`：ユーザー指定の時間オフセット（例：`"-02:00"`、`"+00:00"`）。

  デフォルトは `system` です。
- **Payload Encode**：ログエントリ内のペイロードデータのエンコード方式。`text`、`hex`、`hidden` から選択可能。デフォルトは `text` です。

### 設定ファイルでの監査ログ有効化

`base.hocon` ファイルの `log.audit` セクションで監査ログを有効化および設定変更も可能です。以下は例です。

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

監査ログが有効化されると、ダッシュボードの **System** -> **Audit Log** で監査ログの内容を閲覧できます。

![image-20231214143911786](./assets/audit_log_list.png)

### 検索フィルター

ログ操作をフィルタリング・検索可能で、サポートされる検索キーワードは以下の通りです。

- **開始時間** - **終了時間**：操作が発生した時間範囲。
- **ソースタイプ**：操作が実行された方法。`Dashboard`、`REST API`、`CLI`、`Erlang Console` の選択肢があります。ここでの `Erlang Console` は通常、EMQのオンサイト技術サポート時に使用されるErlang Shellコンソールを指します。
- **オペレーター**：ダッシュボードのユーザー名、またはREST API呼び出しに使用されたキー名。操作方法がダッシュボードまたはREST APIの場合のみ有効です。
- **IP**：ダッシュボードユーザーまたはREST APIを呼び出したクライアントの送信元IP。操作方法がダッシュボードまたはREST APIの場合のみ表示されます。
- **操作名**：監査ログでサポートされている操作名のドロップダウンリストから選択。
- **操作結果**：`Success` または `Failure` のドロップダウンリストから選択。

### リストの説明

表示される監査ログリストの各列の説明は以下の通りです。

- **操作時間**：操作が行われた時間。
- **情報**：
  - ダッシュボードまたはREST APIの場合、操作名を表示。
  - CLIおよびコンソールの場合、実行されたコマンドを記録。
- **オペレーター**：操作方法と対応するオペレーターを含みます。CLIおよびコンソール操作の場合、コマンドが実行されたEMQXノードの名前です。
- **IP**：ダッシュボードユーザーまたはREST APIを呼び出したクライアントの送信元IP。操作方法がダッシュボードまたはREST APIの場合のみ表示されます。
- **操作結果**：`Success` または `Failure`。失敗はフォーム検証エラーやリソース削除不可などのケースを含みます。ダッシュボードまたはREST API操作のみ表示され、CLIおよびコンソールは操作結果を記録できません。

## ログファイルでの監査ログ閲覧

監査ログがEMQXで有効化されると、変更に関する操作はログ形式で `./log/audit.log.1` ファイルに保存されます。エンタープライズユーザーは監査記録を詳細に分析し、既存のログ管理システムに統合することが容易になり、コンプライアンスおよびデータセキュリティ要件を満たせます。

::: warning 注意

コマンドライン操作の監査ログには機密情報が含まれる可能性があるため、ログコレクターに送信する際は注意が必要です。ログ内容のフィルタリングや暗号化伝送の利用など、不正な情報漏洩を防ぐ対策を推奨します。

:::

監査ログに含まれるフィールドは、操作記録のソースにより異なります。

### ダッシュボードまたはREST APIからの操作記録

ダッシュボードまたはREST APIの操作を記録する監査ログには、操作ユーザー、操作対象、操作結果の情報が含まれます。ログメッセージのフォーマット例は以下の通りです。

```bash
{"time":1702604675872987,"level":"info","source_ip":"127.0.0.1","operation_type":"mqtt","operation_result":"success","http_status_code":204,"http_method":"delete","operation_id":"/mqtt/retainer/message/:topic","duration_ms":4,"auth_type":"jwt_token","query_string":{},"from":"dashboard","source":"admin","node":"emqx@127.0.0.1","http_request":{"method":"delete","headers":{"user-agent":"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36","sec-fetch-site":"same-origin","sec-fetch-mode":"cors","sec-fetch-dest":"empty","sec-ch-ua-platform":"\"macOS\"","sec-ch-ua-mobile":"?0","sec-ch-ua":"\"Google Chrome\";v=\"119\", \"Chromium\";v=\"119\", \"Not?A_Brand\";v=\"24\"","referer":"http://localhost:18083/","origin":"http://localhost:18083","host":"localhost:18083","connection":"keep-alive","authorization":"******","accept-language":"zh-CN,zh;q=0.9,zh-TW;q=0.8,en;q=0.7","accept-encoding":"gzip, deflate, br","accept":"*/*"},"body":{},"bindings":{"topic":"$SYS/brokers/emqx@127.0.0.1/version"}}}
```

上記ログメッセージ例に含まれるフィールドは以下の通りです。

| フィールド名         | 型       | 説明                                                         |
| -------------------- | -------- | ------------------------------------------------------------ |
| time                 | Integer  | ログ記録のタイムスタンプ（マイクロ秒単位）                   |
| level                | String   | ログレベル                                                   |
| source_ip            | String   | 操作の送信元IPアドレス                                       |
| operation_type       | String   | 操作の機能モジュール。REST APIのタグに対応                   |
| operation_result     | String   | 操作結果。`success` または `failure` で成功・失敗を示す      |
| http_status_code     | String   | HTTPレスポンスステータスコード                               |
| http_method          | String   | HTTPリクエストメソッド                                       |
| duration_ms          | Integer  | 操作実行時間（ミリ秒単位）                                   |
| auth_type            | String   | 認証タイプ。認証に使用された方式やメカニズムを示し、ダッシュボードは `jwt_token`、REST APIは `api_key` に固定 |
| query_string         | Object   | HTTPリクエストのURLクエリパラメータ                           |
| from                 | String   | リクエストの発信元。`dashboard`、`rest_api` はそれぞれダッシュボード、REST APIを示す。`cli`、`erlang_console` はCLIまたはErlang Shellからの操作であり、このログ構造は該当しない。 |
| source               | String   | 操作を実行したダッシュボードのユーザー名またはAPIキー名      |
| node                 | String   | 操作が実行されたノード名（ノードまたはサーバー）             |
| method               | String   | HTTPリクエストメソッド。`post`、`put`、`delete` はそれぞれ作成、更新、削除操作に対応 |
| operate_id           | String   | リクエストのREST APIパス。詳細は[REST API](../admin/api.md)を参照 |

### CLIまたはErlangコンソールからの操作記録

すべてのCLIコマンドは監査ログに記録されます。読み取り専用コマンド（例：`emqx ctl status`）も含まれます。ただし、例外として、トップレベルの使用法一覧表示は記録されません。具体的には、コマンドなしで `emqx ctl` を実行した場合や、認識されないコマンドを実行した場合は、利用可能なコマンド一覧が表示されますが、これらは記録されません。無効な引数を受け取り自身の使用法メッセージを表示するコマンド（例：`emqx ctl status bad-arg`）は、そのコマンドの呼び出しとして記録されます。

CLIまたはErlangコンソールの操作を記録する監査ログには、実行されたコマンド、呼び出しパラメータなどの情報が含まれます。ログメッセージのフォーマット例は以下の通りです。

```bash
{"time":1695866030977555,"level":"info","msg":"from_cli","from": "cli","node":"emqx@127.0.0.1","duration_ms":0,"cmd":"retainer","args":["clean", "t/1"]}
```

上記ログメッセージ例に含まれるフィールドは以下の通りです。

| フィールド名  | 型       | 説明                                                         |
| ------------ | -------- | ------------------------------------------------------------ |
| time         | Integer  | ログ記録のタイムスタンプ（マイクロ秒単位）                   |
| level        | String   | ログレベル                                                   |
| msg          | String   | 操作の説明                                                   |
| from         | String   | リクエストの発信元。`cli`、`erlang_console` はそれぞれCLI、Erlang Shellを示す。`dashboard`、`rest_api` はダッシュボードまたはREST APIの操作であり、このログ構造は該当しない。 |
| node         | String   | 操作が実行されたノード名（ノードまたはサーバー）             |
| duration_ms  | Integer  | 操作の実行時間（ミリ秒単位）                                 |
| cmd          | String   | 実行された具体的なコマンド操作。対応コマンドは[CLI](../admin/cli.md)を参照 |
| args         | Array    | コマンドに付随する追加パラメータ。複数のパラメータは配列で区切られる |
