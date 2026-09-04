# MQTT Agent

MQTT Agentは、EMQXをMQTTインフラストラクチャからMQTTネイティブなAIオーケストレーションプラットフォームへと変換します。

MQTT Agentにより、EMQXはクライアントイベントに反応するイベント駆動型のAI自動化を、EMQXの接続機能を活用して実行できるようになります。

一般的な人間向けエージェントとは異なり、主にチャットインターフェースではありません。  
これは人間を介さないAI自動化のために設計されており、多数のデバイス、多数の同時ワークフロー、外部システムへのアクセス制限、監査可能なツール利用を想定しています。

ブローカー、APIゲートウェイ、サーバレスランタイム、AIサービス、ワークフローエンジン、統合プラットフォームを個別に組み合わせる代わりに、MQTT Agentはこれらの基本要素を1つのMQTTネイティブなランタイムに統合します。

このプラグインは、MQTTトピックを介して利用可能な3つの合成可能な基本要素を中心に構成されています。

- **ツール**: MQTTパブリッシュ、MQTTリクエスト／リプライ、HTTPコール、データベースクエリなどの再利用可能でスキーマ検証済みの機能。  
- **セッション**: MQTTトピック上でルーティングされるアドレス指定可能なLLM会話。セッションはコンテキスト管理者であり、会話履歴、保留中イベント、キューイングされたリクエスト、ツール呼び出し状態、使用カウンターを所有します。  
- **パイプライン**: MQTTイベントを処理するためにツールとセッション呼び出しをオーケストレーションするイベントトリガー型のワークフローインスタンス。

これにより、EMQXは接続されたデバイスのイベントが直接安全なAIワークフローをトリガーできる場所になります。LLMは承認されたツールのみを参照し、ツールはトピックやリソースの境界を強制し、セッションは使用状況を追跡し、パイプラインはEMQXのスケールでOTPフォールト分離と共に実行可能です。

## 実現できること

- **MQTT運用にAIを導入**: 接続されたデバイスのイベントがモデル支援の意思決定、情報付加、検査、分類、フォローアップアクションを直接トリガー可能。  
- **自動化をブローカー近くに保持**: ワークフローはMQTT接続性、ルーティング、認可境界、運用テレメトリが既に存在する場所で実行。  
- **AIの動作を制限**: LLMは承認されたツールのみを受け取り、各ツールは特定のトピック、エンドポイント、データベース、ストリームにスコープ可能。  
- **機械規模のイベントフローを処理**: 自動化は多数のデバイスと多数の同時ワークフロー向けに構築されており、単一の人間チャットセッション向けではありません。

## MQTT Agentインターフェース

MQTT AgentはMQTTトピックを使って機能を提供します。エージェントのトピックは`$`プレフィックスを使用し、MQTTのシステムトピックとして通常の`#`サブスクリプションではマッチしません。

## ツール

ツールはパイプラインステップで直接使用されるか、パイプラインのLLMステップに提供される制限付きアクションです。

ツールはタイプとIDで指定します：`type@id`。タイプはツールの実装を識別し、例としてHTTPリクエストやデータベースクエリがあります。`id`はツールの設定オプションと制限のセットを識別します。

### ツールトピック

ツール呼び出しはMQTTのリクエスト／レスポンス交換です。呼び出し元はツールインスタンスのリクエストトピック（`$cap/<type>/<tool_id>/request/<req_id>`）にJSONリクエストをパブリッシュし、対応するレスポンストピック（`$cap/<type>/<tool_id>/response/<req_id>`）でJSONレスポンスを待ちます。

ツールはリクエストペイロードをデコードし、`args`フィールドをツール入力スキーマに対して検証し、アクションを実行して、同じ`req_id`のレスポンストピックに結果をパブリッシュします。

例えば、`message__publish@alerts`をリクエストID `req-42`で呼び出す場合：

- リクエストトピックは `$cap/message__publish/alerts/request/req-42`  
- レスポンストピックは `$cap/message__publish/alerts/response/req-42`

呼び出し元は以下のリクエストペイロードをリクエストトピックにパブリッシュします。

```json
// PUBLISH $cap/message__publish/alerts/request/req-42
{
  "args": {
    "topic": "factory/line-1/alerts",
    "payload": {"severity": "warning", "reason": "temperature_high"}
  },
  "iid": "pipeline-instance-id",
  "trace_id": "trace-id"
}
```

MQTTメッセージをパブリッシュした後、ツールは以下のレスポンスペイロードをレスポンストピックにパブリッシュします。

```json
// PUBLISH $cap/message__publish/alerts/response/req-42
{
  "status": "ok",
  "result": {"published": true}
}
```

### タイプ、インスタンス、コンテキスト

ツールタイプは汎用的な実装です。ツールIDはその実装の設定済みインスタンスです。インスタンスは呼び出し時に変更できない固定設定（コンテキスト）を持ちます。

例えば、`postgresql__query`は汎用のPostgreSQLクエリ実行ツールです。単体ではSQLパラメータのレンダリング、EMQXのPostgreSQL接続経由での準備済みクエリ実行、行の返却のみを行います。設定済みインスタンスはより限定的な機能を持ちます。

```json
{
  "type": "postgresql__query",
  "id": "orders_by_device",
  "desc": "Read recent orders for one device",
  "resource": "pg-main",
  "query": "select id, status, created_at from orders where device_id = ${device_id} order by created_at desc limit 10"
}
```

これにより、`postgresql__query@orders_by_device`というツール参照が作成されます。パイプラインやLLMステップは`{"device_id": "dev-001"}`で呼び出せますが、別のデータベース接続を選んだり、任意のSQLを実行したり、`where`句を削除したり、制限を変更したりはできません。これらの固定部分は`orders_by_device`に関連付けられたインスタンスコンテキストに存在します。

同様のパターンは他のツールタイプにも適用されます。`message__publish`インスタンスはパブリッシュ境界を固定し、`http`インスタンスはエンドポイント形状を固定し、ストリームやKVインスタンスはストレージターゲットを固定します。

### 組み込みツールタイプ

| ツールタイプ | 用途 |
|---|---|
| `message__publish` | 設定済みトピックプレフィックスの下でMQTTメッセージをパブリッシュします。 |
| `message__request` | MQTT 5のリクエスト／リプライメッセージを送信し、レスポンスを待ちます。 |
| `http` | スキーマ定義された入力で外部HTTPエンドポイントを呼び出します。 |
| `postgresql__query` | 設定済み接続を通じてパラメータ化されたPostgreSQLクエリを実行します。 |
| `stream__write` | EMQXストリームにキー付きデータを書き込みます。 |
| `stream__read` | EMQXストリームからキー付きデータを読み取ります。 |
| `stream__del` | キー付きデータを削除するかEMQXストリームをクリアします。 |
| `kv__write` | 最終値EMQXストリームにキー・バリューエントリを書き込みます。 |
| `kv__read` | 最終値EMQXストリームからキー・バリューエントリを読み取ります。 |
| `kv__read_all` | 最終値EMQXストリームからすべてのキー・バリューエントリを読み取ります。 |
| `kv__del` | 最終値EMQXストリームから1つのキー・バリューエントリを削除します。 |
| `kv__clear` | 最終値EMQXストリームのすべてのキー・バリューエントリをクリアします。 |

### 画像処理機構

`http`および`message__request`ツールは、ツールレスポンスから画像を抽出し、マルチモーダルデータを安全にLLMに渡せるようにします。OpenAI互換APIはツールレスポンスメッセージに直接埋め込まれた画像を受け付けないため、Agentはペイロード内の抽出された画像を`Image <id>`プレースホルダーに置き換え、画像データを別添として返します。

画像抽出は以下の2つのモードをサポートします。

- `autodiscover_images`: レスポンスペイロード内の`data:image/...;base64,...`値をスキャンします。  
- `images`: `.image_url`や`.`（ルート値）などのパスで明示的に画像位置を指定します。

レスポンスのコンテンツタイプが`image/png`などの画像メディアタイプの場合、バイナリ画像レスポンスも抽出可能です。

#### 自動検出の例

HTTPツールがインラインのデータURIを含むJSONを返すと仮定します。

```json
{
  "inspection_status": "accepted",
  "image_url": "data:image/png;base64,iVBORw0KGgoAAA...",
  "comment": "front camera frame"
}
```

`autodiscover_images`が有効な場合、ツールレスポンスはサニタイズされた結果と抽出された添付ファイルを含みます。

```json
{
  "status": "ok",
  "result": {
    "inspection_status": "accepted",
    "image_url": "Image .image_url",
    "comment": "front camera frame"
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

`result`フィールドはツールレスポンスとしてLLMに渡され、`attachments`は追加のマルチモーダルデータとして渡されます。

#### 明示的パスの例

レスポンスに複数の画像類似フィールドがある場合、モデルが検査すべき1つだけを指定します。

```json
{
  "autodiscover_images": false,
  "images": [".inspection.photo"]
}
```

このレスポンスの場合：

```json
{
  "inspection": {
    "photo": "data:image/jpeg;base64,/9j/4AAQSk...",
    "thumbnail": "data:image/jpeg;base64,/9j/2wBD..."
  }
}
```

`.inspection.photo`のみが抽出され、`thumbnail`は通常のペイロードデータのままです。完全なツールレスポンスは以下のようになります。

```json
{
  "status": "ok",
  "result": {
    "inspection": {
      "photo": "Image .inspection.photo",
      "thumbnail": "data:image/jpeg;base64,/9j/2wBD..."
    }
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

#### バイナリレスポンスの例

HTTPエンドポイントが`Content-Type: image/png`で生のPNGバイトを返す場合、バイナリはルートの「値」として扱われます。

```text
Content-Type: image/png

<raw PNG bytes>
```

ルートペイロードは`Image .`として表現され、PNGバイトは別添として添付されます。

```json
{
  "status": "ok",
  "result": "Image .",
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

メタツールはAgentの設定を変更するパイプライン構築を可能にします。通常のツールですが、信頼されたビルダー用ワークフローのみに公開されることが多いです。

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

## セッション

セッションはMQTTトピック上でルーティングされるアドレス指定可能なLLM状態機械です。セッションは会話履歴、保留中イベント、キューイングされたリクエスト、ツール呼び出し状態、使用カウンターを所有します。

セッションのトラフィックは2つのトピックスキーマを使用します。

- `$sess/in/<sid>` -- セッションへのインバウンドフレーム。  
- `$sess/out/<sid>` -- セッションからのアウトバウンドフレーム。

各セッションはクラスター内で一意な`s id`（セッションID）で識別されます。

`$sess/in/<sid>`のインバウンドフレーム：

| フレームタイプ | 用途 |
|---|---|
| `request` | プロバイダー、モデル、指示、入力、ツール、パーシステンス設定を指定してLLM処理を開始。 |
| `tool_result` | セッションから要求されたツール呼び出しの結果を返す。 |
| `event` | 次のLLMターンに新しいイベントコンテキストを追加。 |
| `stop` | セッションを明示的に終了。 |

`$sess/out/<sid>`のアウトバウンドフレーム：

| フレームタイプ | 用途 |
|---|---|
| `intermediate` | ターン完了前に中間モデルチャンクをストリーム。`chunk_type`（例：`content`）と`chunk`内のチャンクバイトを含む。 |
| `tool_request` | 待機中のパイプラインにツール呼び出しを`$cap/...`経由で依頼。 |
| `final` | 現在のLLMターンを終了し、結果と使用カウンターを返す。 |
| `error` | 利用不可プロバイダーや履歴圧縮エラーなど、セッション側の障害を報告。 |

すべてのアウトバウンドフレームには`sid`、`iid`、`trace_id`、累積された`usage`が含まれます。モデルの推論／思考チャンクは現在セッション内に保持され、公開されるストリームチャンクのみが`intermediate`フレームとして現れます。

パーシステンスが有効な場合、`final`公開後もセッションは停止せず存続し、さらにリクエストを受けてマルチターン会話を形成できます。

## パイプライン

パイプライン定義はID、MQTTトリガー、順序付けられたステップを含みます。受信MQTTメッセージがトリガートピックフィルターにマッチすると、MQTT Agentは1つのパイプラインインスタンスを開始し、メッセージをパイプラインコンテキストの`$.event`として利用可能にします。

パイプライントリガートピックは通常のMQTTトピックフィルターで、`$evt/...`イベントトピックにマッチします。例：

```text
$evt/device/+/done
```

パイプラインのライフサイクルイベントはJSONで以下にパブリッシュされます。

```text
$pipe/<pipeline_id>/inst/<iid>/events
```

サポートされるステップタイプ：

- `call_tool`: MQTTパブリッシュ、HTTP、PostgreSQL、KV、ストリームストレージなどのツールを呼び出し、その結果をコンテキストに書き込みます。  
- `llm_loop`: セッションに処理を送り、選択したツールをLLMツールとして公開し、セッションの応答時に最終または構造化結果を保存します。  
- `break`: コンテキスト値に基づきパイプラインを早期停止します。

### パイプラインコンテキスト

パイプラインコンテキストは1つのパイプラインインスタンス内のすべてのステップで共有されるバイナリキー付きマップです。初期値は`{event: event_payload}`です。ステップの入力は`$.event.device_id`や`$.inspection.status`のようなJSONPath風文字列で前の値を参照でき、ステップの出力は`result_path`で指定された場所に書き込まれます（例：`$.inspection`）。

### パイプラインロジック

パイプラインはシングルターンハンドラーです。1つのトリガーイベントが1つのパイプラインインスタンスを作成し、そのインスタンスはイベントに対する順序付けられた処理を調整し、完了または失敗をパブリッシュして終了します。長時間実行される人間向けエージェントループではありません。

これは意図的な設計です。人間向けエージェントはしばしば人間がフィードバックを含むプロンプトをメインセッションに戻すことでターンベースの対話を形成しますが、MQTT Agentは人間を介さないインタラクションを対象としています。イベントはデバイス、ブローカーフック、サブスクリプション、ルール、外部システム、その他自動化されたソースから到着し、単一の人間対話は存在しません。代わりに、パイプラインは個々のイベントを処理し、セッションは必要に応じてLLMの連続性を提供し、`kv_*`や`stream_*`ツールはイベント間の明示的なワークフローメモリを提供します。

パイプラインはアクティブまたはドラフト状態があり、ドラフトパイプラインは保存されますが有効化されるまで実行されません。

### LLMステップのキー式

マルチターン対話を模倣するために、永続的なLLMセッションを用いる`llm_loop`ステップを使用することも可能です。この場合、パイプラインインスタンスごとにステップのキー式でセッション識別子を使用します。異なるキー式を使うことで、`clientid`やトピックなどの基準ごとに単一セッションを持つことができます。

## 管理画面

メインの管理UIはプラグインAPIゲートウェイを通じて提供されます。

```text
/api/v5/plugin_api/emqx_agent/ui
```

追加ページ：

```text
/api/v5/plugin_api/emqx_agent/builder/ui
/api/v5/plugin_api/emqx_agent/apple-box/ui
```

同じ管理画面は`/api/v5/plugin_api/emqx_agent`以下のプラグインAPIパスからも利用可能です。

| パス | 用途 |
|---|---|
| `/tools` | ツールの一覧表示と作成。 |
| `/tools/:type/:id` | ツールの取得、更新、削除。 |
| `/tools/statuses` | ランタイムのツール調整状態の確認。 |
| `/connections` | ツール接続の一覧表示と作成。 |
| `/connections/:id` | 接続の取得、更新、削除。 |
| `/connections/:id/start` | 接続の有効化と調整。 |
| `/connections/:id/stop` | 接続の無効化と調整。 |
| `/connections/statuses` | ランタイムの接続状態の確認。 |
| `/providers` | 設定済みAIプロバイダーの一覧。 |
| `/pipelines` | パイプライン定義の一覧表示と作成。 |
| `/pipelines/:id` | パイプラインの取得、更新、削除。 |

## デモページ

プラグインには2つのブラウザデモが含まれます。

- **Pipeline Builder** (`/builder/ui`): イベント駆動型AIワークフローを構築するチャットスタイルインターフェース。  
- **Apple Box Conveyor** (`/apple-box/ui`): りんご箱の検査をシミュレートするMQTT/AIワークフローデモ。

EMQXがプラグイン有効化済みで起動している状態で、リポジトリルートからデモリソースをプロビジョニングします。両デモともOpenAI互換APIキーが必要です。

```bash
export OPENAI_API_KEY='sk-...'
```

オプションの環境変数：

| 変数名 | デフォルト | 用途 |
|---|---|---|
| `EMQX_BASE_URL` | `http://localhost:18083/api/v5/plugin_api/emqx_agent` | MQTT AgentプラグインAPIのベースURL。 |
| `EMQX_CORE_BASE_URL` | `http://localhost:18083/api/v5` | AIプロバイダー管理用のEMQXコアAPIベースURL。 |
| `EMQX_API_CREDS` | `key:secret` | ベーシック認証APIクレデンシャル。 |
| `OPENAI_BASE_URL` | `https://api.openai.com/v1` | OpenAI互換APIベースURL。 |
| `OPENAI_MODEL` | スクリプト固有のデフォルト | デモパイプラインで使用するモデル。 |
| `PGHOST`, `PGPORT`, `PGDATABASE`, `PGUSER`, `PGPASSWORD` | `pgsql`, `5432`, `mqtt`, `root`, `public` | デモツールで使用するPostgreSQL接続情報。 |

Apple Box Conveyorデモのプロビジョニング：

```bash
python3 plugins/emqx_agent/demo_apple_box_init.py
```

このスクリプトは`apple-inspector` AIプロバイダー、PostgreSQL接続、apple-boxツール、データベーステーブル、アクティブな`apple-box-inspection`パイプラインを作成します。UIは以下で開けます。

```text
/api/v5/plugin_api/emqx_agent/apple-box/ui
```

Pipeline Builderデモのプロビジョニング：

```bash
python3 plugins/emqx_agent/demo_builder_init.py
```

このスクリプトはビルダーAIプロバイダー、PostgreSQL接続、ビルダーメタツール、リプライツール、データベーステーブル、アクティブな`pipeline-builder`パイプラインを作成します。UIは以下で開けます。

```text
/api/v5/plugin_api/emqx_agent/builder/ui
```

両スクリプトはデモ資産を再作成し、プロビジョニング前に既存のAgentデモリソースを削除することがあります。デモリソースを明示的に削除するには以下を実行してください。

```bash
python3 plugins/emqx_agent/demo_teardown.py
```

## ビルドとテスト

リポジトリルートからプラグインをビルドします。

```bash
make plugin-emqx_agent
```

このプラグインのCommon Testスイートを実行します。

```bash
make plugins/emqx_agent-ct
```

LLM対応のデモスイートは有能なLLMが必要なため、`OPENAI_API_KEY`が設定されている場合のみ実行され、そうでなければスキップされます。

## 開発

プラグインをビルド、インストール、有効化し、そのノードで起動します。

```bash
plugins/emqx_agent/script/start_dev.sh
```

管理UIはプラグインAPIゲートウェイ経由で利用可能です。

```text
/api/v5/plugin_api/emqx_agent/ui
```

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリース向けのtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.3.0 | 1.0.0 | [emqx_agent-1.0.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_agent-1.0.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_agent-1.0.0.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
