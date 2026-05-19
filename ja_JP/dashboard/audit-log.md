# Audit Log

Audit Log機能は、EMQXクラスターにおける重要な運用変更をリアルタイムで追跡することを可能にします。Audit Logを通じて、エンタープライズユーザーは誰がどの重要な操作をいつどのように行ったかを簡単に確認できます。これは、エンタープライズユーザーが規制要件を遵守し、運用時のデータセキュリティ監査を確実に行うための重要なツールです。

EMQX Audit Logは、[ダッシュボード](../dashboard/introduction.md)、[REST API](../admin/api.md)、および[CLI](../admin/cli.md)からの変更に関する操作を記録することをサポートしています。例えば、ダッシュボードのユーザーログインやクライアント、アクセス制御、データ統合の変更などです。ただし、メトリクス取得やクライアントリストの照会などの読み取り専用操作は記録されません。

EMQXは、エンタープライズがAudit Logを管理しやすいように、ダッシュボードビューとログシステムとの連携を提供しています。これらの方法を通じて、EMQXは柔軟かつ包括的にAudit Logをサポートし、エンタープライズユーザーがニーズに応じて最適な方法でAudit Logを管理・閲覧できるようにしています。

## Audit Logの有効化

Audit Log機能は、ダッシュボードおよび設定ファイルの両方から有効化および設定パラメータの調整が可能です。

### ダッシュボードからAudit Logを有効化

ダッシュボードでAudit Logを有効化し、設定パラメータを変更するには、**管理** -> **ログ** -> **Audit Log**、または**システム** -> **Audit Log**に移動します。

<img src="./assets/audit_log_config.png" alt="Audit Log設定" style="zoom:50%;" />

Audit Logには以下のオプションを設定できます。

- **ログハンドラーを有効化**：Audit Log処理プロセスを有効または無効にします。デフォルトで有効です。
- **Audit Logファイル名**：Audit Logファイルのパスと名前を指定します。デフォルト値は`${EMQX_LOG_DIR}/audit.log`で、`${EMQX_LOG_DIR}`は変数でありデフォルトは`./log`です。つまり最終的には`./log/audit.log.1`に保存されます。
- **最大ログファイル数**：ローテーションされるログファイルの最大数です。デフォルトは`10`です。
- **ローテーションサイズ**：ログファイルのサイズを設定し、指定サイズに達するとログファイルがローテーションされます。無効にするとログファイルは無制限に増加します。テキストボックスに値を入力し、ドロップダウンリストから`MB`、`GB`、`KB`などの単位を選択できます。デフォルトは`50MB`です。
- **キャッシュサイズ**：データベースに保存される最大レコード数を決定し、ダッシュボードおよび`/audit` APIを通じてアクセス・取得可能です。デフォルトは`5000`です。

  ::: tip 注意
  `log.audit.max_filter_size`は後方互換性のためエイリアスとして残されています。
  :::

- **高頻度リクエストを無視**：パブリッシュ／サブスクライブやクライアントのキックアウトなどの高頻度リクエストを無視してAudit Logの洪水を防ぐかどうかを制御します。デフォルトで有効です。
- **タイムスタンプ形式**：ログエントリのタイムスタンプに使用される形式です。選択肢は以下の通りです。
  - `auto`：ログフォーマッターに基づき最適な形式を自動選択（JSONは`epoch`、テキストは`rfc3339`）。
  - `epoch`：マイクロ秒単位のUnixエポック時間。
  - `rfc3339`：RFC3339形式。
- **時間オフセット**：ログエントリのタイムスタンプをフォーマットする際の時間オフセットです。選択肢は以下の通りです。
  - `system`：ローカルシステムの時間オフセット。
  - `utc`：UTC時間オフセット。
  - `+-[hh]:[mm]`：ユーザー指定の時間オフセット（例：`"-02:00"`や`"+00:00"`）。

  デフォルトは`system`です。
- **ペイロードエンコード**：ログエントリ内のペイロードデータのエンコード方式です。`text`、`hex`、`hidden`から選択可能で、デフォルトは`text`です。

### 設定ファイルからAudit Logを有効化

`base.hocon`ファイル内の`log.audit`セクションでAudit Logを有効化し、設定オプションを変更することも可能です。以下は例です。

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

## ダッシュボードでAudit Logを閲覧

Audit Logを有効化すると、ダッシュボードの**システム** -> **Audit Log**でAudit Logの内容を閲覧できます。

![image-20231214143911786](./assets/audit_log_list.png)

### 検索フィルター

以下の検索キーワードで操作ログをフィルターおよび検索できます。

- **開始時間** - **終了時間**：操作が行われた時間範囲。
- **ソースタイプ**：操作が行われた方法。`Dashboard`、`REST API`、`CLI`、`Erlang Console`の選択肢があります。ここで`Erlang Console`はEMQによる現地技術サポート時に使用されるErlang Shellコンソールを指します。
- **オペレーター**：Dashboardのユーザー名またはREST API呼び出しに使用されたキー名です。操作方法がDashboardまたはREST APIの場合にのみ有効です。
- **IP**：DashboardユーザーまたはREST APIを呼び出したクライアントの送信元IPです。操作方法がDashboardまたはREST APIの場合にのみ表示されます。
- **操作名**：Audit Logでサポートされている操作名のドロップダウンリストから選択します。
- **操作結果**：`成功`または`失敗`から選択します。

### リストの説明

表示されるAudit Logリストの各列の説明は以下の通りです。

- **操作時間**：操作が行われた時間。
- **情報**：
  - DashboardまたはREST APIの場合、操作名を表示します。
  - CLIおよびConsoleの場合は実行されたコマンドを記録します。
- **オペレーター**：操作方法と対応するオペレーターを含みます。CLIおよびConsole操作の場合、オペレーターはコマンドが実行されたEMQXノードの名前です。
- **IP**：DashboardユーザーまたはREST APIを呼び出したクライアントの送信元IPです。操作方法がDashboardまたはREST APIの場合にのみ表示されます。
- **操作結果**：`成功`または`失敗`。失敗はフォーム検証失敗やリソース削除不可などのケースを含みます。DashboardまたはREST APIの場合のみ表示され、CLIおよびConsoleでは操作結果は記録されません。

## ログファイルでAudit Logを閲覧

Audit LogがEMQXで有効化されると、変更に関する操作は`./log/audit.log.1`ファイルにログ形式で保存されます。エンタープライズユーザーはAudit Logの詳細な分析を行い、既存のログ管理システムに統合することで、コンプライアンスやデータセキュリティ要件を満たすことが容易になります。

::: warning 注意

コマンドライン操作のAudit Logには機密情報が含まれる可能性があるため、ログコレクターに送信する際は注意が必要です。ログ内容のフィルタリングや暗号化伝送の利用など、不正な情報漏洩を防ぐ対策を推奨します。

:::

Audit Logに含まれるフィールドは、操作記録のソースによって異なります。

### ダッシュボードまたはREST APIからの操作記録

DashboardまたはREST APIの操作を記録するAudit Logには、操作ユーザー、操作対象、操作結果の情報が含まれます。ログメッセージのフォーマット例は以下の通りです。

```bash
{"time":1702604675872987,"level":"info","source_ip":"127.0.0.1","operation_type":"mqtt","operation_result":"success","http_status_code":204,"http_method":"delete","operation_id":"/mqtt/retainer/message/:topic","duration_ms":4,"auth_type":"jwt_token","query_string":{},"from":"dashboard","source":"admin","node":"emqx@127.0.0.1","http_request":{"method":"delete","headers":{"user-agent":"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36","sec-fetch-site":"same-origin","sec-fetch-mode":"cors","sec-fetch-dest":"empty","sec-ch-ua-platform":"\"macOS\"","sec-ch-ua-mobile":"?0","sec-ch-ua":"\"Google Chrome\";v=\"119\", \"Chromium\";v=\"119\", \"Not?A_Brand\";v=\"24\"","referer":"http://localhost:18083/","origin":"http://localhost:18083","host":"localhost:18083","connection":"keep-alive","authorization":"******","accept-language":"zh-CN,zh;q=0.9,zh-TW;q=0.8,en;q=0.7","accept-encoding":"gzip, deflate, br","accept":"*/*"},"body":{},"bindings":{"topic":"$SYS/brokers/emqx@127.0.0.1/version"}}}
```

以下の表は上記ログメッセージサンプルに含まれるフィールドの説明です。

| フィールド名        | 型       | 説明                                                          |
| ------------------- | -------- | ------------------------------------------------------------- |
| time                | 整数     | ログ記録のタイムスタンプ（マイクロ秒単位）                   |
| level               | 文字列   | ログレベル                                                    |
| source_ip           | 文字列   | 操作の送信元IPアドレス                                       |
| operation_type      | 文字列   | 操作の機能モジュール。REST APIのタグに対応                    |
| operation_result    | 文字列   | 操作結果。`success`は成功、`failure`は失敗を示す             |
| http_status_code    | 文字列   | HTTPレスポンスステータスコード                                |
| http_method         | 文字列   | HTTPリクエストメソッド                                        |
| duration_ms         | 整数     | 操作実行時間（ミリ秒単位）                                   |
| auth_type           | 文字列   | 認証タイプ。認証に使用された方法や仕組みを示し、Dashboardは`jwt_token`、REST APIは`api_key`で固定 |
| query_string        | オブジェクト | HTTPリクエストのURLクエリパラメータ                          |
| from                | 文字列   | リクエストの送信元。`dashboard`、`rest_api`はそれぞれダッシュボード、REST APIを示す。`cli`、`erlang_console`はCLIまたはErlang Shellからの操作で、このログ構造は該当しない。 |
| source              | 文字列   | 操作を行ったDashboardのユーザー名またはAPIキー名             |
| node                | 文字列   | 操作が実行されたノード名またはサーバー名                      |
| method              | 文字列   | HTTPリクエストメソッド。`post`、`put`、`delete`はそれぞれ作成、更新、削除操作に対応 |
| operate_id          | 文字列   | リクエストのREST APIパス。詳細は[REST API](../admin/api.md)を参照 |

### CLIまたはErlang Consoleからの操作記録

CLIまたはErlang Consoleの操作を記録するAudit Logには、実行されたコマンドや呼び出されたパラメータなどの情報が含まれます。ログメッセージのフォーマット例は以下の通りです。

```bash
{"time":1695866030977555,"level":"info","msg":"from_cli","from": "cli","node":"emqx@127.0.0.1","duration_ms":0,"cmd":"retainer","args":["clean", "t/1"]}
```

以下の表は上記ログメッセージサンプルに含まれるフィールドの説明です。

| フィールド名  | 型       | 説明                                                          |
| ------------ | -------- | ------------------------------------------------------------- |
| time         | 整数     | ログ記録のタイムスタンプ（マイクロ秒単位）                   |
| level        | 文字列   | ログレベル                                                    |
| msg          | 文字列   | 操作の説明                                                    |
| from         | 文字列   | リクエストの送信元。`cli`、`erlang_console`はそれぞれCLI、Erlang Shellを示す。`dashboard`、`rest_api`はダッシュボードまたはREST APIからの操作で、このログ構造は該当しない。 |
| node         | 文字列   | 操作が実行されたノード名またはサーバー名                      |
| duration_ms  | 整数     | 操作の実行時間（ミリ秒単位）                                 |
| cmd          | 文字列   | 実行された具体的なコマンド操作。対応コマンドは[CLI](../admin/cli.md)を参照 |
| args         | 配列     | コマンドに付随する追加パラメータ。複数パラメータは配列で区切られる |
