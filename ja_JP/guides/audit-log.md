# Audit Log

::: tip

Audit Log機能はEMQX Enterpriseエディションでのみ利用可能です。

:::

Audit Log機能は、EMQXクラスター内の重要な操作変更をリアルタイムで追跡することを可能にします。Audit Logを通じて、エンタープライズユーザーは誰がどの重要な操作を、どのように、いつ実行したかを簡単に確認できます。これは、エンタープライズユーザーが規制要件に準拠し、運用中のデータセキュリティ監査を確実に行うための重要なツールです。

EMQX Audit Logは、[ダッシュボード](./dashboard/introduction.md)、[REST API](../guides/api.md)、および[CLI](./cli.md)からの変更関連操作の記録をサポートしており、ダッシュボードのユーザーログインやクライアント、アクセス制御、データ統合の変更などが含まれます。ただし、メトリクス取得やクライアントリストの照会などの読み取り専用操作は記録されません。

EMQXは、ダッシュボードビューとログシステムとの統合を提供し、エンタープライズがAudit Logを管理しやすくしています。これらの方法により、EMQXはAudit Logに対して柔軟かつ包括的なサポートを提供し、エンタープライズユーザーがニーズに応じて最適な管理・閲覧方法を選択できるようにしています。

## Audit Logの有効化

Audit Log機能は、ダッシュボードおよび設定ファイルの両方から有効化および設定パラメータの調整が可能です。

### ダッシュボードからAudit Logを有効化

ダッシュボードの **Management** -> **Logging** -> **Audit Log** に移動すると、Audit Logの有効化および設定パラメータの変更ができます。

<img src="./assets/audit_log_config.png" alt="Audit Logの設定" style="zoom:50%;" />

Audit Logに対して以下のオプションを設定できます：

- **Enable Log Handler**：Audit Log処理プロセスの有効化・無効化。デフォルトで有効です。
- **Audit Log File Name**：Audit Logファイルのパスとファイル名を指定します。デフォルト値は`${EMQX_LOG_DIR}/audit.log`で、`${EMQX_LOG_DIR}`は変数でありデフォルトは`./log`です。つまり最終的に`./log/audit.log.1`に保存されます。
- **Maximum Log Files Number**：ローテーションされるログファイルの最大数。デフォルトは`10`です。
- **Rotation Size**：ログファイルのサイズを設定し、指定サイズに達するとログファイルがローテーションされます。無効にするとログファイルは無制限に増加します。テキストボックスに値を入力し、`MB`、`GB`、`KB`などの単位をドロップダウンリストから選択できます。デフォルトは`50MB`です。
- **Max Dashboard Record Size**：データベースに保存される最大レコード数を設定します。ダッシュボードおよび`/audit` APIからアクセス・取得可能です。デフォルトは`5000`です。
- **Ignore High Frequency Request**：高頻度リクエストを無視するかどうかを制御します。これにより、パブリッシュ／サブスクライブやクライアント強制切断などのリクエストによるAudit Logの過剰な記録を防ぎます。デフォルトで有効です。
- **Time Offset**：ログのタイムスタンプのフォーマットを定義します。例として "-02:00" や "+00:00" があります。デフォルトは`system`です。

### 設定ファイルからAudit Logを有効化

`emqx.conf`ファイルの`log.audit`セクションでAudit Logを有効化し、設定オプションを変更することも可能です。以下は例です。

```bash
log.audit {
  path = "./log/audit.log"
  rotation_count = 10
  rotation_size = 50MB
  time_offset = system
  ignore_high_frequency_requst = true
  max_filter_size = 5000
}
```

## ダッシュボードでAudit Logを閲覧

Audit Logが有効になると、ダッシュボードの **System** -> **Audit Log** でAudit Logの内容を閲覧できます。

![image-20231214143911786](./assets/image-20231214143911786.png)

### 検索フィルター

ログ操作をフィルターおよび検索できます。サポートされる検索キーワードは以下の通りです：

- **Start Time** - **End Time**：操作が発生した時間範囲。
- **Source Type**：操作を実行した方法。`Dashboard`、`REST API`、`CLI`、`Erlang Console`の選択肢があります。ここで`Erlang Console`はErlang Shellコンソールを指し、通常はEMQの現地技術サポート時に使用されます。
- **Operator**：ダッシュボードのユーザー名またはREST API呼び出し時に使用されたキー名です。操作方法がDashboardまたはREST APIの場合にのみ有効です。
- **IP**：ダッシュボードユーザーまたはREST APIを呼び出したクライアントの送信元IPです。操作方法がDashboardまたはREST APIの場合にのみ表示されます。
- **Operation Name**：Audit Logでサポートされている操作名のドロップダウンリストから選択します。
- **Operation Result**：`Success`または`Failure`のドロップダウンリストから選択します。

### リストの説明

表示されるAudit Logリストの各列の説明は以下の通りです：

- **Operation Time**：操作が行われた時間。
- **Info**：
  - DashboardまたはREST APIの場合、この列は操作名を表示します。
  - CLIおよびConsoleの場合、この列は実行されたコマンドを記録します。
- **Operator**：操作方法と対応するオペレーターを含みます。CLIおよびConsole操作の場合、オペレーターはコマンドが実行されたEMQXノード名です。
- **IP**：ダッシュボードユーザーまたはREST APIを呼び出したクライアントの送信元IPです。操作方法がDashboardまたはREST APIの場合にのみ表示されます。
- **Operation Result**：`Success`または`Failure`。失敗にはフォーム検証失敗やリソース削除不可などのシナリオが含まれます。DashboardまたはREST APIの操作方法の場合にのみ表示され、CLIおよびConsoleは操作結果を記録できません。

## ログファイルでAudit Logを閲覧

Audit LogがEMQXで有効になると、変更関連操作は`./log/audit.log.1`ファイルにログ形式で保存されます。エンタープライズユーザーはAudit記録を詳細に分析し、既存のログ管理システムに統合しやすくなり、コンプライアンスおよびデータセキュリティ要件を満たせます。

::: warning Notice

コマンドライン操作のAudit Logには機密情報が含まれる可能性があるため、ログコレクターに送信する際は注意が必要です。ログ内容のフィルタリングや暗号化伝送の利用を推奨し、不正な情報漏洩を防いでください。

:::

Audit Logに含まれるフィールドは、操作記録のソースによって異なります。

### ダッシュボードまたはREST APIからの操作記録

ダッシュボードまたはREST API操作を記録するAudit Logには、操作ユーザー、操作対象、操作結果の情報が含まれます。ログメッセージのフォーマット例は以下の通りです。

```bash
{"time":1702604675872987,"level":"info","source_ip":"127.0.0.1","operation_type":"mqtt","operation_result":"success","http_status_code":204,"http_method":"delete","operation_id":"/mqtt/retainer/message/:topic","duration_ms":4,"auth_type":"jwt_token","query_string":{},"from":"dashboard","source":"admin","node":"emqx@127.0.0.1","http_request":{"method":"delete","headers":{"user-agent":"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36","sec-fetch-site":"same-origin","sec-fetch-mode":"cors","sec-fetch-dest":"empty","sec-ch-ua-platform":"\"macOS\"","sec-ch-ua-mobile":"?0","sec-ch-ua":"\"Google Chrome\";v=\"119\", \"Chromium\";v=\"119\", \"Not?A_Brand\";v=\"24\"","referer":"http://localhost:18083/","origin":"http://localhost:18083","host":"localhost:18083","connection":"keep-alive","authorization":"******","accept-language":"zh-CN,zh;q=0.9,zh-TW;q=0.8,en;q=0.7","accept-encoding":"gzip, deflate, br","accept":"*/*"},"body":{},"bindings":{"topic":"$SYS/brokers/emqx@127.0.0.1/version"}}}
```

以下の表は上記ログメッセージサンプルに含まれるフィールドを示します。

| フィールド名         | 型       | 説明                                                         |
| -------------------- | -------- | ------------------------------------------------------------ |
| time                 | Integer  | ログ記録のタイムスタンプ（マイクロ秒単位）                   |
| level                | String   | ログレベル                                                   |
| source_ip            | String   | 操作の送信元IPアドレス                                       |
| operation_type       | String   | 操作の機能モジュール。REST APIのTagに対応                     |
| operation_result     | String   | 操作結果。`success`または`failure`で操作の成功・失敗を示す   |
| http_status_code     | String   | HTTPレスポンスステータスコード                                |
| http_method          | String   | HTTPリクエストメソッド                                       |
| duration_ms          | Integer  | 操作実行時間（ミリ秒単位）                                   |
| auth_type            | String   | 認証タイプ。認証に使用された方法や仕組みを示し、`jwt_token`（Dashboard）または`api_key`（REST API）で固定 |
| query_string         | Object   | HTTPリクエストのURLクエリパラメータ                           |
| from                 | String   | リクエストの送信元。`dashboard`、`rest_api`はそれぞれダッシュボード、REST APIを示す。`cli`、`erlang_console`の場合はCLIまたはErlang Shellからの操作であり、このログ構造は該当しない。 |
| source               | String   | 操作を実行したダッシュボードのユーザー名またはAPIキー名       |
| node                 | String   | 操作が実行されたノード名またはサーバー名                     |
| method               | String   | HTTPリクエストメソッド。`post`、`put`、`delete`はそれぞれ作成、更新、削除操作に対応 |
| operate_id           | String   | リクエストのREST APIパス。詳細は[REST API](../guides/api.md)を参照 |

### CLIまたはErlang Consoleからの操作記録

CLIまたはErlang Consoleの操作を記録するAudit Logには、実行されたコマンド、呼び出されたパラメータなどの情報が含まれます。ログメッセージのフォーマット例は以下の通りです。

```bash
{"time":1695866030977555,"level":"info","msg":"from_cli","from": "cli","node":"emqx@127.0.0.1","duration_ms":0,"cmd":"retainer","args":["clean", "t/1"]}
```

以下の表は上記ログメッセージサンプルに含まれるフィールドを示します。

| フィールド名  | 型       | 説明                                                         |
| ------------ | -------- | ------------------------------------------------------------ |
| time         | Integer  | ログ記録のタイムスタンプ（マイクロ秒単位）                   |
| level        | String   | ログレベル                                                   |
| msg          | String   | 操作の説明                                                   |
| from         | String   | リクエストの送信元。`cli`、`erlang_console`はそれぞれCLI、Erlang Shellからの操作を示す。`dashboard`、`rest_api`の場合はダッシュボードまたはREST APIからの操作であり、このログ構造は該当しない。 |
| node         | String   | 操作が実行されたノード名またはサーバー名                     |
| duration_ms  | Integer  | 操作の実行時間（ミリ秒単位）                                 |
| cmd          | String   | 実行された具体的なコマンド操作。対応コマンドは[CLI](./cli.md)を参照 |
| args         | Array    | コマンドに付随する追加パラメータ。複数パラメータは配列で区切られる |
