# MQTT Agent

MQTT Agentは、EMQXをMQTTインフラストラクチャからMQTTネイティブなAIオーケストレーションプラットフォームへと変換します。

MQTT Agentは、EMQXの接続機能を活用してクライアントイベントに反応するイベント駆動型のAI自動化を実行可能にします。

一般的な人間向けエージェントとは異なり、主にチャットインターフェースではありません。
多くのデバイス、多数の同時ワークフロー、外部システムへのアクセス制限、監査可能なツール利用を前提とした人間なしのAI自動化のために設計されています。

ブローカー、APIゲートウェイ、サーバレスランタイム、AIサービス、ワークフローエンジン、統合プラットフォームを個別に連携させる代わりに、MQTT Agentはこれらの基本要素を1つのMQTTネイティブなランタイムに統合します。

このプラグインは、MQTTトピックを介して利用可能な3つの合成可能なプリミティブを中心に構成されています：

- **Tools（ツール）**：MQTTパブリッシュ、MQTTリクエスト／リプライ、HTTPコール、データベースクエリなどの再利用可能でスキーマ検証済みの機能。
- **Sessions（セッション）**：MQTTトピック上でルーティングされるアドレス指定可能なLLM会話。セッションはコンテキストの保持者であり、会話履歴、保留中イベント、キューイングされたリクエスト、ツール呼び出し状態、使用カウンターを所有します。
- **Pipelines（パイプライン）**：MQTTイベントを処理するためにツールとセッションの呼び出しをオーケストレーションするイベントトリガー型のワークフローインスタンス。

これにより、EMQXは接続されたデバイスのイベントが安全なAIワークフローを直接トリガーできる場となります。LLMは承認されたツールのみを参照し、ツールはトピックやリソースの境界を強制し、セッションは使用状況を追跡し、パイプラインはOTPフォールトアイソレーションを伴いEMQXのスケールで動作可能です。

## できること

- **AIをMQTT運用に導入**：接続されたデバイスのイベントがモデル支援の意思決定、データ強化、検査、分類、フォローアップアクションを直接トリガー可能。
- **自動化をブローカー近くに保持**：ワークフローはMQTT接続、ルーティング、認可境界、運用テレメトリが既に存在する場所で実行。
- **AIの動作を制限**：LLMは承認されたツールのみを受け取り、各ツールは特定のトピック、エンドポイント、データベース、ストリームにスコープ可能。
- **機械規模のイベントフローを処理**：自動化は多数のデバイスと多数の同時ワークフロー向けに設計されており、単一の人間チャットセッション向けではありません。

## MQTT Agentインターフェース

MQTT AgentはMQTTトピックを使用して機能を提供します。エージェントトピックは`$`プレフィックスを持ち、MQTTシステムトピックとして通常の`#`サブスクリプションにはマッチしません。

## Tools（ツール）

ツールはパイプラインステップで直接使用されるか、パイプラインのLLMステップに提供される制限付きアクションです。

ツールは`type@id`でアドレス指定されます。`type`はツールの実装タイプ（例：HTTPリクエスト、データベースクエリ）を識別し、`id`はツールの設定オプションと制限のセットを識別します。

### ツールトピック

ツール呼び出しはMQTTのリクエスト／レスポンス交換です。呼び出し元はツールインスタンスのリクエストトピック（`$cap/<type>/<tool_id>/request/<req_id>`）にJSONリクエストをパブリッシュし、対応するレスポンストピック（`$cap/<type>/<tool_id>/response/<req_id>`）でJSONレスポンスを待ちます。

ツールはリクエストペイロードをデコードし、`args`フィールドをツール入力スキーマに対して検証し、アクションを実行し、同じ`req_id`のレスポンストピックに結果をパブリッシュします。

例えば、`message__publish@alerts`をリクエストID `req-42`で呼び出す場合：

- リクエストトピックは`$cap/message__publish/alerts/request/req-42`
- レスポンストピックは`$cap/message__publish/alerts/response/req-42`

`alerts`インスタンスがトピックプレフィックス`factory/line-1/alerts/`を持ち、以下のオブジェクトを受け入れるペイロードスキーマを持つとします。呼び出し元はこのリクエストペイロードをリクエストトピックにパブリッシュします：

```jsonc
// PUBLISH $cap/message__publish/alerts/request/req-42
{
  "args": {
    "topic": "temperature",
    "payload": {"severity": "warning", "reason": "temperature_high"}
  },
  "iid": "pipeline-instance-id",
  "sid": "session-id",
  "trace_id": "trace-id"
}
```

MQTTメッセージのパブリッシュ後、ツールは以下のレスポンスペイロードをレスポンストピックにパブリッシュします：

```jsonc
// PUBLISH $cap/message__publish/alerts/response/req-42
{
  "req_id": "req-42",
  "trace_id": "trace-id",
  "iid": "pipeline-instance-id",
  "sid": "session-id",
  "tool": {"type": "message__publish", "id": "alerts"},
  "response": {
    "status": "ok",
    "result": {"topic": "factory/line-1/alerts/temperature"}
  }
}
```

### タイプ、インスタンス、コンテキスト

ツールタイプは汎用実装を指します。ツールIDはその実装の設定済みインスタンスです。インスタンスは呼び出し時に変更できない固定設定（コンテキスト）を持ちます。

例えば、`postgresql__query`は汎用のPostgreSQLクエリエグゼキューターです。単体ではSQLパラメータのレンダリング、EMQXのPostgreSQL接続経由でのプリペアドクエリ実行、行の返却のみを行います。設定済みインスタンスはより限定的な機能を持ちます：

```json
{
  "type": "postgresql__query",
  "id": "orders_by_device",
  "desc": "Read recent orders for one device",
  "resource": "pg-main",
  "query": "select id, status, created_at from orders where device_id = ${device_id} order by created_at desc limit 10"
}
```

これにより`postgresql__query@orders_by_device`というツール参照が作成されます。パイプラインやLLMステップは`{"device_id": "dev-001"}`で呼び出せますが、別のデータベース接続を選択したり、任意のSQLを実行したり、`where`句を削除したり、制限を変更したりはできません。これらの固定部分は`orders_by_device`に関連付けられたインスタンスコンテキストに存在します。

同様のパターンは他のツールタイプにも適用されます。`message__publish`インスタンスはパブリッシュ境界を固定し、`http`インスタンスはエンドポイントの形状を固定し、ストリームやKVインスタンスはストレージターゲットを固定します。

### 組み込みツールタイプ

| ツールタイプ | 用途 |
|---|---|
| `message__publish` | 設定済みトピックプレフィックス下でMQTTメッセージをパブリッシュ。 |
| `message__request` | MQTT 5のリクエスト／リプライメッセージを送信し、レスポンスを待機。 |
| `http` | スキーマ定義された入力で外部HTTPエンドポイントを呼び出し。 |
| `postgresql__query` | 設定済み接続経由でパラメータ化されたPostgreSQLクエリを実行。 |
| `stream__write` | EMQXストリームにキー付きデータを書き込み。 |
| `stream__read` | EMQXストリームからキー付きデータを読み込み。 |
| `stream__del` | キー付きデータを削除、またはEMQXストリームをクリア。 |
| `kv__write` | 最終値EMQXストリームにキー・バリューエントリを書き込み。 |
| `kv__read` | 最終値EMQXストリームからキー・バリューエントリを読み込み。 |
| `kv__read_all` | 最終値EMQXストリームからすべてのキー・バリューエントリを読み込み。 |
| `kv__del` | 最終値EMQXストリームから1つのキー・バリューエントリを削除。 |
| `kv__clear` | 最終値EMQXストリームのすべてのキー・バリューエントリをクリア。 |

### 画像処理機構

`http`および`message__request`ツールは、ツールレスポンスから画像を抽出し、マルチモーダルデータを安全にLLMに渡せるようにします。OpenAI互換APIはツールレスポンスメッセージに直接埋め込まれた画像を受け付けないため、Agentはペイロード内の抽出画像を`Image <id>`プレースホルダーに置換し、画像データを別添として返します。

画像抽出は以下の2モードをサポートします：

- `autodiscover_images`：レスポンスペイロード内の`data:image/...;base64,...`値をスキャン。
- `images`：`.image_url`や`.`（ルート値）などのパスで画像位置を明示的に指定。

HTTPツールが`payload_type: "binary"`を使用する場合、レスポンスのContent-Typeが`image/png`などの画像メディアタイプであれば、バイナリ画像レスポンスも抽出可能です。

#### 自動検出例

HTTPツールがインラインのデータURIを含むJSONを返すとします：

```json
{
  "inspection_status": "accepted",
  "image_url": "data:image/png;base64,iVBORw0KGgoAAA...",
  "comment": "front camera frame"
}
```

`autodiscover_images`が有効な場合、ツールの`response`オブジェクトはサニタイズ済みの結果と抽出された添付ファイルを含みます：

```json
{
  "status": "ok",
  "result": {
    "body": {
      "inspection_status": "accepted",
      "image_url": "Image .image_url",
      "comment": "front camera frame"
    },
    "status_code": 200,
    "headers": {"content-type": "application/json"}
  },
  "attachments": [
    {
      "id": ".image_url",
      "type": "image",
      "mime_type": "image/png",
      "data": "iVBORw0KGgoAAA..."
    }
  ]
}
```

`attachments`なしのレスポンスはツールレスポンスとしてLLMに渡され、`attachments`は追加のマルチモーダルデータとして渡されます。

#### 明示的パス例

レスポンスに複数の画像類似フィールドがある場合、モデルが検査すべきものだけを指定します：

```json
{
  "autodiscover_images": false,
  "images": [".inspection.photo"]
}
```

このレスポンスに対して：

```json
{
  "inspection": {
    "photo": "data:image/jpeg;base64,/9j/4AAQSk...",
    "thumbnail": "data:image/jpeg;base64,/9j/2wBD..."
  }
}
```

`.inspection.photo`のみが抽出され、`thumbnail`は通常のペイロードデータのままです。完全な`response`オブジェクトは以下の通りです：

```json
{
  "status": "ok",
  "result": {
    "body": {
      "inspection": {
        "photo": "Image .inspection.photo",
        "thumbnail": "data:image/jpeg;base64,/9j/2wBD..."
      }
    },
    "status_code": 200,
    "headers": {"content-type": "application/json"}
  },
  "attachments": [
    {
      "id": ".inspection.photo",
      "type": "image",
      "mime_type": "image/jpeg",
      "data": "/9j/4AAQSk..."
    }
  ]
}
```

#### バイナリレスポンス例

`payload_type: "binary"`で設定されたHTTPツールが、`Content-Type: image/png`の生PNGバイト列を受信した場合、バイナリはルートの「値」として扱われます：

```text
Content-Type: image/png

<raw PNG bytes>
```

ルートペイロードは`Image .`として表現され、PNGバイトは別添として添付されます：

```json
{
  "status": "ok",
  "result": {
    "body": "Image .",
    "status_code": 200,
    "headers": {"content-type": "image/png"}
  },
  "attachments": [
    {
      "id": ".",
      "type": "image",
      "mime_type": "image/png",
      "data": "iVBORw0KGgoAAA..."
    }
  ]
}
```

### メタツール

メタツールはAgent設定を変更するパイプラインを構築可能にします。通常のツールですが、信頼されたビルダーのワークフローにのみ公開されることが多いです。

- `agent__create_tool`
- `agent__update_tool`
- `agent__delete_tool`
- `agent__query_tools`
- `agent__create_pipeline`
- `agent__update_pipeline`
- `agent__delete_pipeline`
- `agent__query_pipelines`
- `agent__insert_pipeline_step`
- `agent__update_pipeline_step`
- `agent__delete_pipeline_step`
- `agent__query_providers`
- `agent__query_connections`

## Sessions（セッション）

セッションはMQTTトピック上でルーティングされるアドレス指定可能なLLM状態機械です。セッションは会話履歴、保留中イベント、キューイングされたリクエスト、ツール呼び出し状態、使用カウンターを所有します。

セッションのトラフィックは2つのトピックスキーマを使用します：

- `$sess/in/<sid>` — セッションへのインバウンドフレーム。
- `$sess/out/<sid>` — セッションからのアウトバウンドフレーム。

各セッションは`sid`（セッションID）で識別されます。永続的なセッションIDはステップのキー式から派生し、非永続的なセッションIDはパイプラインインスタンスとステップから派生します。

`$sess/in/<sid>`のインバウンドフレーム：

| フレームタイプ | 目的 |
|---|---|
| `request` | プロバイダー、モデル、指示、入力、ツール、永続化設定でLLM作業を開始。 |
| `tool_result` | セッションから要求されたツール呼び出しの結果を返す。 |
| `event` | 次のLLMターンに新しいイベントコンテキストを追加。 |
| `stop` | セッションを明示的に終了。 |

`$sess/out/<sid>`のアウトバウンドフレーム：

| フレームタイプ | 目的 |
|---|---|
| `intermediate` | ターン完了前に中間モデルチャンクをストリーム。`chunk_type`（例：`content`）とチャンクバイトを含む。 |
| `tool_request` | 待機中のパイプラインに`$cap/...`経由でツール呼び出しを依頼。 |
| `final` | 現在のLLMターンを終了し、結果と使用カウンターを返す。 |
| `error` | プロバイダー利用不可や履歴圧縮エラーなど、セッション側の障害を報告。 |

すべてのアウトバウンドフレームには`sid`、`iid`、`trace_id`、累積`usage`が含まれます。モデルの推論／思考チャンクは公開も保持もされず、コンテンツチャンクは`intermediate`フレームとして公開されます。

永続化が有効な場合、`final`公開後もセッションは停止せず存続し、さらなるリクエストを受けてマルチターン会話を形成します。

## Pipelines（パイプライン）

パイプライン定義はID、MQTTトリガー、順序付けられたステップを含みます。受信MQTTメッセージがトリガートピックフィルターにマッチすると、MQTT Agentは1つのパイプラインインスタンスを開始し、メッセージはパイプラインコンテキストの`$.event`として利用可能になります。

パイプライントリガートピックは通常のMQTTトピックフィルターで、`$evt/...`イベントトピックにマッチします。例：

```text
$evt/device/+/done
```

パイプラインのライフサイクルイベントはJSONで以下にパブリッシュされます：

```text
$pipe/<pipeline_id>/inst/<iid>/events
```

サポートされるステップタイプ：

- `call_tool`：MQTTパブリッシュ、HTTP、PostgreSQL、KV、ストリームストレージなどのツールを呼び出し、その結果をコンテキストに書き込む。
- `llm_loop`：セッションに作業を送信し、選択されたツールをLLMツールとして公開し、セッションの応答時に最終または構造化結果を保存。
- `break`：コンテキスト値に基づきパイプラインを早期停止。

### パイプラインコンテキスト

パイプラインコンテキストは1つのパイプラインインスタンス内のすべてのステップで共有されるバイナリキー付きマップです。初期値は`{event: event_payload}`です。ステップ入力は`$.event.device_id`や`$.inspection.status`のようなJSONPath風文字列で前の値を参照でき、ステップ出力はステップの`result_path`（例：`$.inspection`）に書き込まれます。

### パイプラインロジック

パイプラインは単一ターンのハンドラーです。1つのトリガーイベントが1つのパイプラインインスタンスを作成し、そのインスタンスはイベントに対する順序付けられた作業を調整し、完了または失敗をパブリッシュして終了します。長時間実行される人間向けエージェントループではありません。

これは意図的な設計です。人間向けエージェントはしばしば人間がフィードバックを伴ってプロンプトをメインセッションに戻すことでターンベースの対話を形成しますが、MQTT Agentは人間なしのインタラクションを対象としています。イベントはデバイス、ブローカーフック、サブスクリプション、ルール、外部システム、その他の自動化ソースから到着し、単一の人間対話は存在しません。代わりに、パイプラインは個別イベントを処理し、セッションは必要に応じてLLMの継続性を提供し、`kv_*`や`stream_*`ツールはイベント間の明示的なワークフローメモリを提供します。

パイプラインはアクティブまたはドラフト状態であり、ドラフトパイプラインは保存されますが有効化されるまで実行されません。

### LLMステップのキー式

マルチターン対話をエミュレートするために、永続的なLLMセッションを持つ`llm_loop`ステップを使用することも可能です。この場合、各パイプラインインスタンスのこのステップはステップのキー式によってセッション識別子を使用します。異なるキー式を使うことで、`clientid`やトピックなどの基準で単一セッションを持つことができます。

## 管理画面

メインの管理UIはプラグインAPIゲートウェイ経由で提供されます：

```text
/api/v5/plugin_api/emqx_agent/ui
```

追加ページ：

```text
/api/v5/plugin_api/emqx_agent/builder/ui
/api/v5/plugin_api/emqx_agent/apple-box/ui
```

同じ管理画面は`/api/v5/plugin_api/emqx_agent`以下のプラグインAPIパスでも利用可能です：

| パス | 用途 |
|---|---|
| `/tools` | ツールの一覧表示および作成。 |
| `/tools/:type/:id` | ツールの取得、更新、削除。 |
| `/tools/statuses` | ランタイムのツール調整状況の確認。 |
| `/connections` | ツール接続の一覧表示および作成。 |
| `/connections/:id` | 接続の取得、更新、削除。 |
| `/connections/:id/start` | 接続の有効化および調整。 |
| `/connections/:id/stop` | 接続の無効化および調整。 |
| `/connections/statuses` | ランタイムの接続状況の確認。 |
| `/providers` | 設定済みAIプロバイダーの一覧。 |
| `/pipelines` | パイプライン定義の一覧表示および作成。 |
| `/pipelines/:id` | パイプラインの取得、更新、削除。 |

## デモページ

プラグインには2つのブラウザデモが含まれます：

- **Pipeline Builder**（`/builder/ui`）：イベント駆動型AIワークフローを構築するチャットスタイルインターフェース。
- **Apple Box Conveyor**（`/apple-box/ui`）：リンゴ箱の検査をシミュレートするMQTT/AIワークフローデモ。

EMQXがプラグイン有効状態で起動している状態でリポジトリルートからデモリソースをプロビジョニングします。両デモともOpenAI互換APIキーが必要です：

```bash
export OPENAI_API_KEY='sk-...'
```

オプションの環境変数：

| 変数名 | デフォルト | 用途 |
|---|---|---|
| `EMQX_BASE_URL` | `http://localhost:18083/api/v5/plugin_api/emqx_agent` | MQTT AgentプラグインAPIのベースURL。 |
| `EMQX_CORE_BASE_URL` | `http://localhost:18083/api/v5` | AIプロバイダー管理用のEMQXコアAPIベースURL。 |
| `EMQX_API_CREDS` | `key:secret` | ベーシック認証APIクレデンシャル。 |
| `OPENAI_BASE_URL` | `https://api.openai.com/v1` | OpenAI互換APIのベースURL。 |
| `OPENAI_MODEL` | スクリプト固有のデフォルト | デモパイプラインで使用するモデル。 |
| `PGHOST`, `PGPORT`, `PGDATABASE`, `PGUSER`, `PGPASSWORD` | `pgsql`, `5432`, `mqtt`, `root`, `public` | デモツールで使用するPostgreSQL接続情報。 |

Apple Box Conveyorデモのプロビジョニング：

```bash
python3 plugins/emqx_agent/demo_apple_box_init.py
```

このスクリプトは`apple-inspector` AIプロバイダー、PostgreSQL接続、apple-boxツール、データベーステーブル、アクティブな`apple-box-inspection`パイプラインを作成します。UIは以下で開けます：

```text
/api/v5/plugin_api/emqx_agent/apple-box/ui
```

Pipeline Builderデモのプロビジョニング：

```bash
python3 plugins/emqx_agent/demo_builder_init.py
```

このスクリプトはbuilder AIプロバイダー、PostgreSQL接続、builderメタツール、replyツール、データベーステーブル、アクティブな`pipeline-builder`パイプラインを作成します。UIは以下で開けます：

```text
/api/v5/plugin_api/emqx_agent/builder/ui
```

Apple Box初期化スクリプトは名前付きデモ資産を再作成します。Pipeline Builder初期化スクリプトと終了スクリプトは、デモに関連しないリソースを含むすべてのAgentパイプライン、ツール、接続を削除します。終了スクリプトを実行するには：

```bash
python3 plugins/emqx_agent/demo_teardown.py
```

## ビルドとテスト

リポジトリルートからプラグインをビルド：

```bash
make plugin-emqx_agent
```

このプラグインのCommon Testスイートを実行：

```bash
./scripts/ct/run.sh --app plugins/emqx_agent
```

上記コマンドはホストのAPIキー環境変数を渡さないため、LLM対応のデモスイートはスキップされます。デフォルトプロバイダーで実行するには、`OPENAI_API_KEY`をコンテナコマンドに渡します：

```bash
./scripts/ct/run.sh --app plugins/emqx_agent -- env OPENAI_API_KEY="$OPENAI_API_KEY" make plugins/emqx_agent-ct
```

別のプロバイダーを使う場合は、`EMQX_AGENT_TEST_LLM_PROVIDER`とそのAPIキーも渡してください。

## 開発

プラグインをビルド、インストール、有効化、起動するには：

```bash
plugins/emqx_agent/script/start_dev.sh
```

管理UIはプラグインAPIゲートウェイ経由で利用可能です：

```text
/api/v5/plugin_api/emqx_agent/ui
```

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリース向けのtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.3.0 | 1.0.0 | [emqx_agent-1.0.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_agent-1.0.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_agent-1.0.0.sha256)) |
| 6.3.1 | 1.0.0 | [emqx_agent-1.0.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.1/emqx_agent-1.0.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.1/emqx_agent-1.0.0.sha256)) |
| 6.3.2 | 1.0.0 | [emqx_agent-1.0.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.2/emqx_agent-1.0.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.2/emqx_agent-1.0.0.sha256)) |
| 6.3.3 | 1.0.0 | [emqx_agent-1.0.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.3/emqx_agent-1.0.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.3/emqx_agent-1.0.0.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
