# Audit Log

Audit Log機能は、EMQXクラスターにおける重要な運用変更をリアルタイムで追跡することを可能にします。Audit Logを通じて、エンタープライズユーザーは誰がどの重要な操作を、どのように、いつ実行したかを簡単に確認できます。これは、エンタープライズユーザーが規制要件を遵守し、運用中のデータセキュリティ監査を確実に行うための重要なツールです。

EMQX Audit Logは、[ダッシュボード](./introduction.md)、[REST API](../api.md)、および[CLI](../cli.md)からの変更関連操作の記録をサポートしています。例えば、ダッシュボードのユーザーログインやクライアント、アクセス制御、データ統合の変更などです。ダッシュボードおよびREST APIでは、メトリクス取得やクライアント一覧照会といった読み取り専用操作は記録されません。CLIコマンドは、データを変更するかどうかに関わらず記録されますが、[CLIまたはErlangコンソールからの操作記録](#operation-records-from-cli-or-erlang-console)で説明する例外があります。

EMQXは、ダッシュボードビューとログシステムとの連携を提供し、エンタープライズがAudit Logを管理しやすい環境を整えています。これらの方法により、EMQXは柔軟かつ包括的なAudit Logのサポートを提供し、エンタープライズユーザーがニーズに応じて最適な管理・閲覧方法を選択できるようにしています。

## Audit Logの有効化

Audit Log機能は、ダッシュボードおよび設定ファイルの両方から有効化および設定パラメータの調整が可能です。

### ダッシュボードでのAudit Log有効化

ダッシュボードでAudit Logを有効化し、設定パラメータを変更するには、**管理** -> **ログ** -> **Audit Log**、または**システム** -> **Audit Log**に移動します。

<img src="./assets/audit_log_config.png" alt="Audit Logの設定" style="zoom:50%;" />

Audit Logに対して以下のオプションを設定できます：

- **ログハンドラーの有効化**：Audit Log処理プロセスの有効化・無効化。デフォルトで有効です。
- **Audit Logファイル名**：Audit Logファイルのパスと名前を指定します。デフォルトは`${EMQX_LOG_DIR}/audit.log`で、`${EMQX_LOG_DIR}`は変数であり、デフォルトは`./log`です。つまり最終的には`./log/audit.log.1`に保存されます。
- **最大ログファイル数**：ローテーションされるログファイルの最大数。デフォルトは`10`です。
- **ローテーションサイズ**：ログファイルのサイズを設定し、指定サイズに達するとログファイルがローテーションされます。無効にするとログファイルは無制限に成長します。テキストボックスに値を入力し、ドロップダウンリストから`MB`、`GB`、`KB`などの単位を選択できます。デフォルトは`50MB`です。
- **キャッシュサイズ**：データベースに保存される最大レコード数を決定し、ダッシュボードおよび`/audit` APIからアクセス・取得可能です。デフォルトは`5000`です。

  ::: tip 注意
  `log.audit.max_filter_size`は後方互換のためのエイリアスとして残されています。
  :::

- **高頻度リクエストの無視**：パブリッシュ／サブスクライブやクライアントのキックアウトなど、高頻度リクエストを無視してAudit Logの洪水を防ぐかどうかを制御します。デフォルトで有効です。
- **タイムスタンプ形式**：ログエントリのタイムスタンプに使用する形式。選択肢は以下の通りです：
  - `auto`：ログフォーマッターに基づき最適な形式を自動選択。JSONは`epoch`、テキストは`rfc3339`。
  - `epoch`：マイクロ秒単位のUnixエポック時間。
  - `rfc3339`：RFC3339形式。
- **タイムオフセット**：ログエントリのタイムスタンプをフォーマットする際の時間オフセット。選択肢は以下の通りです：
  - `system`：ローカルシステムの時間オフセット。
  - `utc`：UTCの時間オフセット。
  - `+-[hh]:[mm]`：ユーザー指定の時間オフセット（例：`"-02:00"`や`"+00:00"`）。

  デフォルトは`system`です。
- **ペイロードエンコード**：ログエントリ内のペイロードデータのエンコード方法。`text`、`hex`、`hidden`から選択可能。デフォルトは`text`です。

### 設定ファイルでのAudit Log有効化

`base.hocon`ファイルの`log.audit`以下に設定オプションを記述して、Audit Logを有効化および設定変更も可能です。例は以下の通りです。

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

## ダッシュボードでのAudit Log閲覧

Audit Logを有効化すると、ダッシュボードの**システム** -> **Audit Log**でAudit Logの内容を閲覧できます。

![image-20231214143911786](./assets/audit_log_list.png)

### 検索フィルター

ログ操作のフィルタリングおよび検索が可能で、サポートされる検索キーワードは以下の通りです：

- **開始時間** - **終了時間**：操作が発生した時間範囲。
- **ソースタイプ**：操作を実行した方法。選択肢は`Dashboard`、`REST API`、`CLI`、`Erlang Console`です。ここで`Erlang Console`は通常EMQの現地技術サポート時に使用されるErlang Shellコンソールを指します。
- **オペレーター**：ダッシュボードのユーザー名またはREST API呼び出しに使用されたキー名。操作方法がDashboardまたはREST APIの場合のみ有効です。
- **IP**：ダッシュボードユーザーまたはREST APIを呼び出したクライアントの送信元IP。操作方法がDashboardまたはREST APIの場合のみ表示されます。
- **操作名**：Audit Logでサポートされる操作名のドロップダウンリストから選択。
- **操作結果**：`成功`または`失敗`から選択。

### リストの説明

表示されるAudit Logリストの各列の説明は以下の通りです：

- **操作時間**：操作が行われた時間。
- **情報**：
  - DashboardまたはREST APIの場合は操作名を表示。
  - CLIおよびコンソールの場合は実行されたコマンドを記録。
- **オペレーター**：操作方法および対応するオペレーター。CLIおよびコンソール操作の場合、オペレーターはコマンドが実行されたEMQXノード名です。
- **IP**：ダッシュボードユーザーまたはREST APIを呼び出したクライアントの送信元IP。操作方法がDashboardまたはREST APIの場合のみ表示されます。
- **操作結果**：`成功`または`失敗`。失敗にはフォーム検証失敗やリソース削除不可などのケースが含まれます。DashboardまたはREST APIの操作方法のみ表示され、CLIおよびコンソールは操作結果を記録できません。

## ログファイルでのAudit Log閲覧

Audit LogがEMQXで有効化されている場合、変更関連操作は`./log/audit.log.1`ファイルにログ形式で保存されます。これによりエンタープライズユーザーはAudit Logの詳細分析や既存のログ管理システムへの統合を容易に行え、コンプライアンスやデータセキュリティ要件を満たせます。

::: warning 注意

コマンドライン操作のAudit Logには機密情報が含まれる可能性があるため、ログコレクターに送信する際は注意が必要です。ログ内容のフィルタリングや暗号化通信の利用など、不正な情報漏洩を防ぐ対策を推奨します。

:::

Audit Logに含まれるフィールドは、操作記録のソースによって異なります。

### ダッシュボードまたはREST APIからの操作記録

ダッシュボードまたはREST API操作を記録するAudit Logには、操作ユーザー、操作対象、操作結果の情報が含まれます。ログメッセージのフォーマット例は以下の通りです。

```bash
{"time":1702604675872987,"level":"info","source_ip":"127.0.0.1","operation_type":"mqtt","operation_result":"success","http_status_code":204,"http_method":"delete","operation_id":"/mqtt/retainer/message/:topic","duration_ms":4,"auth_type":"jwt_token","from":"dashboard","source":"admin","node":"emqx@127.0.0.1","http_request":{"method":"delete","headers":{"user-agent":"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36","sec-fetch-site":"same-origin","sec-fetch-mode":"cors","sec-fetch-dest":"empty","sec-ch-ua-platform":"\"macOS\"","sec-ch-ua-mobile":"?0","sec-ch-ua":"\"Google Chrome\";v=\"119\", \"Chromium\";v=\"119\", \"Not?A_Brand\";v=\"24\"","referer":"http://localhost:18083/","origin":"http://localhost:18083","host":"localhost:18083","connection":"keep-alive","authorization":"******","accept-language":"zh-CN,zh;q=0.9,zh-TW;q=0.8,en;q=0.7","accept-encoding":"gzip, deflate, br","accept":"*/*"},"body":{},"bindings":{"topic":"$SYS/brokers/emqx@127.0.0.1/version"}}}
```

以下の表は、ダッシュボードまたはREST API操作によって生成されるAudit Logエントリに現れる可能性のあるフィールドを説明しています。

| フィールド名           | 型       | 説明                                                                                      |
| ---------------------- | -------- | ----------------------------------------------------------------------------------------- |
| time                   | 整数     | ログ記録のタイムスタンプ（マイクロ秒単位）。                                             |
| level                  | 文字列   | ログレベル。                                                                             |
| source_ip              | 文字列   | 操作の送信元IPアドレス。                                                                 |
| operation_type         | 文字列   | 操作の機能モジュール。REST APIのタグに対応。                                             |
| operation_result       | 文字列   | 操作結果。`success`は成功、`failure`は失敗を示します。                                   |
| http_status_code       | 整数     | HTTPレスポンスのステータスコード。                                                       |
| http_method            | 文字列   | HTTPリクエストメソッド。                                                                 |
| duration_ms            | 整数     | 操作の実行時間（ミリ秒単位）。                                                           |
| auth_type              | 文字列   | 認証タイプ。認証に使用された方法やメカニズムを示し、`jwt_token`（ダッシュボード）または`api_key`（REST API）で固定。 |
| from                   | 文字列   | リクエストの送信元。`dashboard`、`rest_api`はそれぞれダッシュボード、REST APIを示します。`cli`、`erlang_console`の場合はCLIまたはErlang Shellからの操作であり、このログ構造は該当しません。 |
| source                 | 文字列   | 操作を実行したダッシュボードのユーザー名またはAPIキー名。                               |
| node                   | 文字列   | ノード名。操作が実行されたノードまたはサーバーを示します。                               |
| operation_id           | 文字列   | リクエストのREST APIパス。[REST API](../api.md)を参照してください。                      |
| http_request           | オブジェクト | HTTPリクエストの詳細。                                                                   |
| http_request.method    | 文字列   | HTTPリクエストメソッド。`post`、`put`、`delete`のいずれか。                             |
| http_request.bindings  | オブジェクト | `operation_id`内のプレースホルダーに対応するパスパラメータの値。                         |
| http_request.headers   | オブジェクト | HTTPリクエストヘッダー。機密情報と認識されるキーの値はマスクされています。               |
| http_request.body      | オブジェクト | HTTPリクエストボディ。機密情報と認識されるキーの値はマスクされています。                 |
| http_request.query_string | オブジェクト | 任意。解析済みのURLクエリパラメータ。クエリパラメータがない場合は省略。機密情報はマスク。 |
| http_request.namespace | 文字列   | 任意。操作対象として解決されたネームスペース。グローバルネームスペースの場合は`global`。 |

#### ネームスペースとクエリパラメータ

EMQX 6.3.1以降、監査対象のダッシュボードおよびREST APIリクエストの非空クエリパラメータは`http_request.query_string`に含まれます。ターゲットネームスペースを解決する操作では、`ns`または`namespace`クエリパラメータの有無にかかわらず、解決済みのネームスペースが`http_request.namespace`に記録されます。以下はグローバル管理者が明示的に`ns2`を対象とした例です。

```json
{
  "http_request": {
    "method": "put",
    "bindings": {"name": "test"},
    "query_string": {"ns": "ns2"},
    "namespace": "ns2"
  }
}
```

ネームスペース管理者が自身のネームスペース内のリソースをクエリパラメータなしで対象とした場合、`http_request.query_string`は省略されますが、`http_request.namespace`には解決済みネームスペースが含まれます。

認証、認可、コネクター、ブリッジ、ルール、トレース、データバックアップ、A2Aレジストリ、トピックメトリクスに関わるネームスペース操作では、解決済みターゲットネームスペースが記録されます。その他のネームスペースを解決しないエンドポイントでは`http_request.namespace`は省略されます。

### CLIまたはErlangコンソールからの操作記録

すべてのCLIコマンドはAudit Logに記録されます。読み取り専用コマンド（例：`emqx ctl status`）も含まれます。ただし例外として、トップレベルの使用方法表示は記録されません。つまり、コマンドなしで`emqx ctl`を実行した場合や認識されないコマンドを実行した場合は、利用可能なコマンド一覧が表示されますが記録されません。無効な引数を受け取り自身の使用方法を表示するコマンド（例：`emqx ctl status bad-arg`）は、そのコマンドの呼び出しとして記録されます。

CLIまたはErlangコンソール操作を記録するAudit Logには、実行されたコマンド、呼び出しパラメータなどの情報が含まれます。ログメッセージのフォーマット例は以下の通りです。

```bash
{"time":1695866030977555,"level":"info","msg":"from_cli","from": "cli","node":"emqx@127.0.0.1","duration_ms":0,"cmd":"retainer","args":["clean", "t/1"]}
```

以下の表は上記ログメッセージサンプルに含まれるフィールドを示します。

| フィールド名  | 型       | 説明                                                                                      |
| ------------ | -------- | ----------------------------------------------------------------------------------------- |
| time         | 整数     | ログ記録のタイムスタンプ（マイクロ秒単位）。                                             |
| level        | 文字列   | ログレベル。                                                                             |
| msg          | 文字列   | 操作の説明。                                                                             |
| from         | 文字列   | リクエストの送信元。`cli`、`erlang_console`はそれぞれCLI、Erlang Shellを示します。`dashboard`、`rest_api`の場合はダッシュボードまたはREST API操作であり、このログ構造は該当しません。 |
| node         | 文字列   | 操作が実行されたノードまたはサーバーのノード名。                                         |
| duration_ms  | 整数     | 操作の実行時間（ミリ秒単位）。                                                           |
| cmd          | 文字列   | 実行された具体的なコマンド操作。対応コマンドは[CLI](../cli.md)を参照してください。       |
| args         | 配列     | コマンドに付随する追加パラメータ。複数パラメータは配列で区切られます。                   |
