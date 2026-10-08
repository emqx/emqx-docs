# 監査ログ

監査ログ機能は、EMQXクラスターにおける重要な運用変更をリアルタイムで追跡できるようにします。監査ログを通じて、エンタープライズユーザーは誰がどの重要な操作をどのように、いつ実行したかを簡単に確認できます。これは、エンタープライズユーザーが規制要件に準拠し、運用時のデータセキュリティ監査を確実に行うための重要なツールです。

EMQX監査ログは、[ダッシュボード](./introduction.md)、[REST API](../api.md)、および[CLI](../cli.md)からの変更に関連する操作を記録します。たとえば、ダッシュボードのユーザーログインやクライアント、アクセス制御、データ統合の変更などです。ただし、メトリクス取得やクライアントリストの照会などの読み取り専用操作は記録されません。

EMQXは、ダッシュボードビューとログシステムとの統合を提供し、エンタープライズが監査ログを管理しやすくしています。これらの方法を通じて、EMQXは柔軟かつ包括的な監査ログのサポートを提供し、エンタープライズユーザーがニーズに応じて最適な監査ログの管理・閲覧方法を選択できるようにします。

## 監査ログへのアクセス

EMQX 6.0.4以降、クラスター全体の監査ログをダッシュボードで閲覧したり、`GET /api/v5/audit`で読み取ったりできるのはグローバル管理者およびグローバル閲覧者のみです。監査ログにはすべてのネームスペースの操作が含まれ、呼び出し元のネームスペースによるフィルタリングは行われません。ネームスペース付きのダッシュボードユーザーやAPIキーからのリクエストは、役割や割り当てられたスコープに関わらずHTTP `403`と`UNAUTHORIZED_ROLE`エラーコードを返します。[`audit`スコープ](../api.md#built-in-api-key-scopes)はこの制限を上書きしません。

## 監査ログの有効化

監査ログ機能は、ダッシュボードおよび設定ファイルの両方から有効化および設定パラメータの調整が可能です。

### ダッシュボードから監査ログを有効化

監査ログを有効にし、設定パラメータを変更するには、ダッシュボードの **Management** -> **Logging** -> **Audit Log**、または **System** -> **Audit Log** に移動します。

<img src="../assets/audit_log_config.png" alt="監査ログ設定" style="zoom:50%;" />

監査ログに対して以下のオプションを設定できます。

- **Enable Log Handler**：監査ログ処理プロセスの有効化・無効化。デフォルトで有効です。
- **Audit Log File Name**：監査ログファイルのパスと名前を指定します。デフォルトは`${EMQX_LOG_DIR}/audit.log`で、`${EMQX_LOG_DIR}`は変数であり、デフォルトは`./log`です。最終的に`./log/audit.log.1`に保存されます。
- **Maximum Log Files Number**：ローテーションされるログファイルの最大数。デフォルトは`10`です。
- **Rotation Size**：ログファイルのサイズを設定し、指定サイズに達するとログファイルがローテーションされます。無効にするとログファイルは無制限に増加します。テキストボックスに値を入力し、ドロップダウンリストから`MB`、`GB`、`KB`などの単位を選択できます。デフォルトは`50MB`です。
- **Cache Size**：データベースに保存される最大レコード数を決定します。ダッシュボードおよび`/audit` APIを通じてアクセス・取得可能です。デフォルトは`5000`です。

  ::: tip 注意
  `log.audit.max_filter_size`は後方互換性のためエイリアスとして残されています。
  :::

- **Ignore High Frequency Request**：高頻度リクエストを無視して監査ログへの記録過多を防ぐかどうかを制御します。パブリッシュ／サブスクライブやクライアントのキックアウト関連のリクエストなどが該当します。デフォルトで有効です。
- **Timestamp Format**：ログエントリのタイムスタンプのフォーマット。以下のオプションがあります。
  - `auto`：ログフォーマッターに基づき最適な形式を自動選択。JSONは`epoch`、テキストは`rfc3339`。
  - `epoch`：マイクロ秒単位のUnixエポック時間。
  - `rfc3339`：RFC3339形式。
- **Time Offset**：ログエントリのタイムスタンプをフォーマットする際の時間オフセット。以下のオプションがあります。
  - `system`：ローカルシステムの時間オフセット。
  - `utc`：UTC時間オフセット。
  - `+-[hh]:[mm]`：ユーザー指定の時間オフセット（例：`"-02:00"`や`"+00:00"`）。

  デフォルトは`system`です。
- **Payload Encode**：ログエントリ内のペイロードデータのエンコード方法。`text`、`hex`、`hidden`のいずれか。デフォルトは`text`です。

### 設定ファイルから監査ログを有効化

`base.hocon`ファイルの`log.audit`セクションで監査ログを有効化し、設定オプションを変更することも可能です。例は以下の通りです。

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

監査ログを有効にすると、ダッシュボードの **System** -> **Audit Log** でログエントリを閲覧できます。

![image-20231214143911786](./assets/audit_log_list.png)

### 検索フィルター

ログ操作をフィルター・検索可能で、サポートされる検索キーワードは以下の通りです。

- **開始時間** - **終了時間**：操作が発生した時間範囲。
- **ソースタイプ**：操作実行方法。`Dashboard`、`REST API`、`CLI`、`Erlang Console`が選択可能です。`Erlang Console`はEMQが提供するオンサイト技術サポート時に使用されるErlang Shellコンソールを指します。
- **オペレーター**：ダッシュボードのユーザー名またはREST API呼び出しに使用されたキー名。操作方法がダッシュボードまたはREST APIの場合に有効です。
- **IP**：ダッシュボードユーザーまたはREST APIを呼び出したクライアントの送信元IP。操作方法がダッシュボードまたはREST APIの場合に表示されます。
- **操作名**：監査ログでサポートされる操作名のドロップダウンリストから選択。
- **操作結果**：`Success`または`Failure`のドロップダウンリストから選択。

### リストの説明

表示される監査ログリストの各列の説明は以下の通りです。

- **操作時間**：操作が行われた時間。
- **情報**：
  - ダッシュボードまたはREST APIの場合、操作名を表示。
  - CLIおよびコンソールの場合、実行されたコマンドを記録。
- **オペレーター**：操作方法および対応するオペレーターを含みます。CLIおよびコンソールの操作では、コマンドが実行されたEMQXノード名がオペレーターとなります。
- **IP**：ダッシュボードユーザーまたはREST APIを呼び出したクライアントの送信元IP。操作方法がダッシュボードまたはREST APIの場合に表示されます。
- **操作結果**：`Success`または`Failure`。失敗にはフォーム検証失敗やリソース削除不可などが含まれます。ダッシュボードまたはREST APIの操作のみ表示され、CLIおよびコンソールは操作結果を記録できません。

## ログファイルでの監査ログ閲覧

監査ログがEMQXで有効化されると、変更に関する操作は`./log/audit.log.1`ファイルにログ形式で保存されます。エンタープライズユーザーは監査記録を詳細に分析し、既存のログ管理システムに統合することが容易になり、コンプライアンスおよびデータセキュリティ要件を満たせます。

::: warning 注意

コマンドライン操作の監査ログには機密情報が含まれる可能性があるため、ログコレクターに送信する際は注意してください。ログ内容のフィルタリングや暗号化伝送の利用など、不正な情報漏洩防止策を推奨します。

:::

監査ログに含まれるフィールドは、操作記録のソースによって異なります。

### ダッシュボードまたはREST APIからの操作記録

ダッシュボードまたはREST APIの操作を記録する監査ログには、操作ユーザー、操作対象オブジェクト、操作結果の情報が含まれます。ログメッセージの形式例は以下の通りです。

```bash
{"time":1702604675872987,"level":"info","source_ip":"127.0.0.1","operation_type":"mqtt","operation_result":"success","http_status_code":204,"http_method":"delete","operation_id":"/mqtt/retainer/message/:topic","duration_ms":4,"auth_type":"jwt_token","from":"dashboard","source":"admin","node":"emqx@127.0.0.1","http_request":{"method":"delete","headers":{"user-agent":"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36","sec-fetch-site":"same-origin","sec-fetch-mode":"cors","sec-fetch-dest":"empty","sec-ch-ua-platform":"\"macOS\"","sec-ch-ua-mobile":"?0","sec-ch-ua":"\"Google Chrome\";v=\"119\", \"Chromium\";v=\"119\", \"Not?A_Brand\";v=\"24\"","referer":"http://localhost:18083/","origin":"http://localhost:18083","host":"localhost:18083","connection":"keep-alive","authorization":"******","accept-language":"zh-CN,zh;q=0.9,zh-TW;q=0.8,en;q=0.7","accept-encoding":"gzip, deflate, br","accept":"*/*"},"body":{},"bindings":{"topic":"$SYS/brokers/emqx@127.0.0.1/version"}}}
```

以下の表は、ダッシュボードまたはREST API操作によって生成される監査ログエントリに含まれるフィールドを説明しています。

| フィールド名          | 型       | 説明                                                         |
| --------------------- | -------- | ------------------------------------------------------------ |
| time                  | Integer  | ログレコードのタイムスタンプ（マイクロ秒単位）               |
| level                 | String   | ログレベル                                                   |
| source_ip             | String   | 操作の送信元IPアドレス                                       |
| operation_type        | String   | 操作の機能モジュール。REST APIのタグに対応                   |
| operation_result      | String   | 操作結果。`success`は成功、`failure`は失敗を示す             |
| http_status_code      | Integer  | HTTPレスポンスのステータスコード                             |
| http_method           | String   | HTTPリクエストメソッド                                       |
| duration_ms           | Integer  | 操作実行時間（ミリ秒単位）                                   |
| auth_type             | String   | 認証タイプ。認証に使用された方法や仕組みを示し、`jwt_token`（ダッシュボード）または`api_key`（REST API）で固定 |
| from                  | String   | リクエストの送信元。`dashboard`、`rest_api`はそれぞれダッシュボード、REST APIを示す。`cli`、`erlang_console`の場合はCLIまたはErlang Shellからの操作であり、このログ構造は適用されない |
| source                | String   | 操作を行ったダッシュボードのユーザー名またはAPIキー名       |
| node                  | String   | 操作が実行されたノード名（ノードまたはサーバー）             |
| operation_id          | String   | リクエストのREST APIパス。詳細は[REST API](../api.md)参照    |
| http_request          | Object   | HTTPリクエストの詳細                                         |
| http_request.method   | String   | HTTPリクエストメソッド                                       |
| http_request.bindings | Object   | `operation_id`内のプレースホルダーに対応するパスパラメータの値 |
| http_request.headers  | Object   | HTTPリクエストヘッダー。機密と認識されるキーの値はマスクされる |
| http_request.body     | Object   | HTTPリクエストボディ。機密と認識されるキーの値はマスクされる |
| http_request.query_string | Object | 任意。解析済みのURLクエリパラメータ。クエリパラメータがない場合は省略。機密と認識されるキーの値はマスクされる |
| http_request.namespace | String  | 任意。操作対象として解決されたネームスペース。グローバルネームスペースの場合は`global` |

#### ネームスペースとクエリパラメータ

EMQX 6.0.4以降、監査対象のダッシュボードおよびREST APIリクエストの非空クエリパラメータは`http_request.query_string`に含まれます。データバックアップ操作では、`http_request.namespace`にリクエストに`namespace`クエリパラメータが含まれているかに関わらず解決された対象ネームスペースが記録されます。以下はグローバル管理者が`ns2`にバックアップをインポートする例です。

```json
{
  "http_request": {
    "method": "post",
    "body": {"filename": "emqx-export.zip"},
    "query_string": {"namespace": "ns2"},
    "namespace": "ns2"
  }
}
```

ネームスペース管理者が自身のネームスペースでクエリパラメータなしにデータバックアップ操作を行う場合、`http_request.query_string`は省略されますが、解決されたネームスペースは`http_request.namespace`に含まれます。

解決された対象ネームスペースは、バックアップファイルのエクスポート、インポート、アップロード、削除を行うデータバックアップ操作で記録されます。対象ネームスペースを解決しない他のエンドポイントでは`http_request.namespace`は省略されます。

### CLIまたはErlangコンソールからの操作記録

CLIまたはErlangコンソール操作を記録する監査ログには、実行されたコマンド、呼び出しパラメータなどの情報が含まれます。ログメッセージの形式例は以下の通りです。

```bash
{"time":1695866030977555,"level":"info","msg":"from_cli","from": "cli","node":"emqx@127.0.0.1","duration_ms":0,"cmd":"retainer","args":["clean", "t/1"]}
```

以下の表は上記ログメッセージに含まれるフィールドを示します。

| フィールド名  | 型       | 説明                                                         |
| ------------ | -------- | ------------------------------------------------------------ |
| time         | Integer  | ログレコードのタイムスタンプ（マイクロ秒単位）               |
| level        | String   | ログレベル                                                   |
| msg          | String   | 操作の説明                                                 |
| from         | String   | リクエストの送信元。`cli`、`erlang_console`はそれぞれCLI、Erlang Shellを示す。`dashboard`、`rest_api`の場合はダッシュボードまたはREST APIからの操作であり、このログ構造は適用されない |
| node         | String   | 操作が実行されたノード名（ノードまたはサーバー）             |
| duration_ms  | Integer  | 操作の実行時間（ミリ秒単位）                                 |
| cmd          | String   | 実行された具体的なコマンド操作。対応コマンドは[CLI](../cli.md)を参照 |
| args         | Array    | コマンドに付随する追加パラメータ。複数パラメータは配列で区切られる |
