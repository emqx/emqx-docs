# Sync Request

`emqx_sync_request` プラグインは、HTTP サービスが EMQX REST API を通じて MQTT リクエストを送信し、同一の HTTP リクエスト内で最初に一致した MQTT レスポンスを受け取ることを可能にします。このプラグインは EMQX Enterprise 6.0.4 以降の 6.0 系リリースで利用可能です。

バックエンドサービスが接続された MQTT クライアントにコマンドやクエリを送信する必要がある場合に、このプラグインを使用してください。標準のパブリッシュ API と異なり、このプラグインはクライアントのレスポンスを待機して相関付けを行い、タイムアウト処理や同時リクエストの管理も行います。HTTP サービスは MQTT クライアントを実行したり、リクエストとレスポンスのペアを追跡したりする必要がありません。

API を使用する前に、[プラグイン管理](../../plugin-management.md) に記載の手順で `emqx_sync_request` をインストールして起動してください。プラグインが起動している間のみエンドポイントが利用可能です。

## 動作概要

リクエストとレスポンスのフローは以下の通りです：

1. HTTP 呼び出し元が MQTT リクエストトピック、レスポンストピック、`request_id`、ペイロードを含む HTTP リクエストを API に送信します。
2. プラグインはリクエストトピックにサブスクライブしている MQTT クライアントを特定し、そのクライアントに直接リクエストを配信します。
3. MQTT クライアントはリクエストを処理し、レスポンストピックにレスポンスをパブリッシュします。MQTT 5 では、プラグインがリクエストメッセージに `request_id` を相関データとして含めます。
4. MQTT 5 クライアントがこの相関データを返す場合、プラグインはレスポンストピックと相関データでレスポンスを照合します。相関データが含まれない場合は、レスポンストピックに対する最も古い保留中リクエストにマッチさせます。このフォールバックは MQTT 3 のレスポンスおよび相関データを省略した MQTT 5 のレスポンスに適用されます。同じレスポンストピックを複数のリクエストが共有する場合、MQTT 5 クライアントは各レスポンスに相関データを返して、各レスポンスが意図したリクエストにマッチするようにしてください。
5. プラグインは最初にマッチした MQTT レスポンスを HTTP 呼び出し元に返します。タイムアウトまでにマッチするレスポンスが到着しない場合、API は `504 TIMEOUT` を返します。

リクエストトピックはオンラインの非共有サブスクライバーに正確に一致する必要があります：

- ワイルドカードトピックフィルターはリクエスト受信者としてマッチしません。
- 共有サブスクリプションはリクエスト受信者として受け付けられません。
- 正確に一致するサブスクライバーがオンラインにいない場合、API は `404 NO_SUBSCRIBERS` を返します。
- リクエストトピックに共有サブスクリプションがあるか、正確に一致するサブスクライバーが複数いる場合、API は `409 CONFLICT` を返します。

## リクエスト配信とレスポンス処理

プラグインはインフライトリクエストをローカルノードのメモリにのみ保持します。リクエストを永続化したり、レスポンストピックにサブスクライブしたり、MQTT ペイロードを変更したりはしません。

EMQX は各リクエストを通常の MQTT パブリッシュパイプラインを経由させず、選択されたクライアントに直接配信します。そのため、リクエストはルールエンジン、スキーマ検証、メッセージ変換、保持メッセージ処理、遅延パブリッシュの対象にならず、汎用の `/publish` API も使用しません。

リクエストを別ノードに転送して MQTT レスポンスを待つ場合、HTTP タイムアウトは転送時間とレスポンス待機時間の合計になります。転送時間がレスポンス待機時間を減らします。

レスポンスはリクエストを配信したノードに接続されたクライアントからパブリッシュされる必要があります。通常はリクエストを受信したのと同じ接続経由です。別ノード経由のレスポンスはマッチしません。

## プラグイン設定

これらの設定はプラグイン全体のタイムアウトや各ノードのリソース制限を制御します。リクエスト固有のパラメータは [リクエストボディ](#request-body) を参照してください。

| フィールド | デフォルト | 説明 |
| --- | --- | --- |
| `default_timeout` | `10s` | リクエストボディで `timeout` が省略された場合のデフォルト HTTP 待機タイムアウト。 |
| `max_timeout` | `60s` | リクエストごとに許容される最大 `timeout`。 |
| `max_inflight_requests` | `10000` | 1 ノードあたりレスポンス待ちのローカル HTTP リクエストの最大数。 |
| `max_payload_size` | `64KB` | MQTT リクエストおよびレスポンスのペイロード最大サイズ。 |

設定例：

```hocon
default_timeout = "10s"
max_timeout = "60s"
max_inflight_requests = 10000
max_payload_size = "64KB"
```

標準のプラグイン設定 API でプラグイン設定を更新します：

```http
PUT /api/v5/plugins/<name-vsn>/config
```

## 同期リクエスト API

以下のエンドポイントを呼び出して MQTT リクエストを送信し、レスポンスを待機します：

```http
POST /api/v5/plugin_api/emqx_sync_request/request
```

他の EMQX 管理 API と同様の認証方法を使用します。ダッシュボードログインで取得したベアラートークンが利用可能です。API キーは HTTP Basic 認証で送信し、`publish` スコープが必要です。

### リクエストボディ

```json
{
  "timeout": "5s",
  "request": {
    "topic": "devices/1001/request",
    "response_topic": "devices/1001/response",
    "request_id": "request-id-1",
    "qos": 0,
    "payload_encoding": "plain",
    "payload": "{\"cmd\":\"reboot\"}",
    "content_type": "application/json"
  }
}
```

| フィールド | 型 | 必須 | デフォルト | 説明 |
| --- | --- | --- | --- | --- |
| `timeout` | duration 文字列 | いいえ | `default_timeout` | マッチする MQTT レスポンスを待つ最大時間。`0` より大きく、`max_timeout` 以下である必要があります。例：`100ms`、`5s`、`1m`。 |
| `request` | オブジェクト | はい | - | MQTT リクエストのパラメータ。 |

`request` オブジェクトのフィールド：

| フィールド | 型 | 必須 | デフォルト | 説明 |
| --- | --- | --- | --- | --- |
| `topic` | 文字列 | はい | - | MQTT リクエストトピック。トピックフィルターではなくトピック名である必要があり、`+` と `#` は許可されません。このトピックに対して正確に 1 つの非共有サブスクライバーがオンラインである必要があります。 |
| `response_topic` | 文字列 | はい | - | MQTT レスポンストピック。こちらも `+` と `#` を含まないトピック名である必要があります。 |
| `request_id` | 文字列 | はい | - | MQTT 5 の相関データとして使用され、HTTP レスポンスにエコーバックされるプレーン文字列。最大長は 128 バイトです。 |
| `qos` | 整数 | いいえ | `0` | リクエストの MQTT QoS。許容値は `0`、`1`、`2` です。 |
| `payload_encoding` | 文字列 | いいえ | `plain` | リクエストペイロードのエンコーディング。許容値は `plain` と `base64` です。 |
| `payload` | 文字列 | はい | - | リクエストペイロード。`plain` の場合は文字列のバイト列が MQTT ペイロードとして使われます。`base64` の場合は有効な base64 エンコード文字列で、デコードしたバイト列が MQTT ペイロードになります。MQTT ペイロードは `max_payload_size` を超えてはいけません。 |
| `content_type` | 文字列 | いいえ | - | MQTT 5 のリクエストの Content Type。MQTT 3 クライアントには送信されません。 |

### 成功レスポンス

成功したリクエストは HTTP `200` を返します。MQTT レスポンスのペイロードは常に base64 エンコードで返されます。

```json
{
  "code": "OK",
  "message": "OK",
  "response": {
    "topic": "devices/1001/response",
    "request_id": "request-id-1",
    "payload_encoding": "base64",
    "payload": "eyJyZXN1bHQiOiJvayJ9",
    "content_type": "application/json"
  }
}
```

| フィールド | 説明 |
| --- | --- |
| `code` | 常に `OK`。 |
| `message` | 常に `OK`。 |
| `response.topic` | MQTT レスポンストピック。 |
| `response.request_id` | HTTP リクエストの `request_id`。 |
| `response.payload_encoding` | 常に `base64`。 |
| `response.payload` | Base64 エンコードされた MQTT レスポンスペイロード。 |
| `response.content_type` | 任意。レスポンス PUBLISH の MQTT 5 Content Type。レスポンダーが送信しない場合（MQTT 3 レスポンダーを含む）は省略されます。 |

### エラーレスポンス

エラーは他の EMQX 管理 API と同様の `code` と `message` の形で返されます。

| HTTP ステータス | コード | 意味 |
| --- | --- | --- |
| `400` | `BAD_REQUEST` | 無効な JSON ボディ、無効なフィールド値、リクエストペイロードが大きすぎる、または MQTT レスポンスペイロードが大きすぎる。 |
| `401` | `BAD_API_KEY_OR_SECRET` | API キー認証失敗。EMQX 管理 API の認証で返されます。 |
| `403` | `UNAUTHORIZED_ROLE` | API キーにこの API を呼び出す権限がありません。EMQX 管理 API の認可で返されます。 |
| `404` | `NO_SUBSCRIBERS` | リクエストトピックに正確に一致する非共有サブスクライバーがオンラインにいません。ワイルドカードサブスクライバーは無視されます。 |
| `409` | `CONFLICT` | リクエストトピックに共有サブスクリプションがあるか、正確に一致するサブスクライバーが複数います。 |
| `429` | `TOO_MANY_REQUESTS` | ローカルノードで既に `max_inflight_requests` の HTTP リクエストがレスポンス待ちです。 |
| `503` | `SERVICE_UNAVAILABLE` | リクエストをサブスクライバーノードにディスパッチできませんでした。 |
| `504` | `TIMEOUT` | マッチする MQTT レスポンスの待機中にタイムアウトしました。 |
| `500` | `INTERNAL_ERROR` | 予期しないサーバー側エラー。 |

## 運用診断

プラグインはノードローカルの診断用 CLI コマンドを提供します：

```bash
emqx ctl sync_request status
```

出力例：

```text
Counters since plugin start:
sync_request.requests.total: 42
sync_request.requests.succeeded: 39
sync_request.requests.failed: 3
sync_request.requests.bad_request: 1
sync_request.requests.no_subscribers: 1
sync_request.requests.conflict: 0
sync_request.requests.too_many_requests: 0
sync_request.requests.dispatch_failed: 0
sync_request.requests.timeout: 1
sync_request.requests.internal_error: 0

Current gauges:
sync_request.inflight_requests: 0
sync_request.pending_responses: 0
```

これらの値はクラスター全体の集計ではありません。コマンドは実行したノードのみの情報を読み取ります。クラスター環境では、HTTP リクエストを受け取るか MQTT レスポンスを配信する可能性のある各ノードで実行してください。

プラグインハンドラーに到達したリクエストのみがカウントされます。管理 API の認証および認可失敗はプラグイン実行前に EMQX が処理します。

| メトリクス | 種類 | 説明 |
| --- | --- | --- |
| `sync_request.requests.total` | カウンター | 処理された HTTP 同期リクエストの試行回数。 |
| `sync_request.requests.succeeded` | カウンター | HTTP `200` を返したリクエスト数。 |
| `sync_request.requests.failed` | カウンター | HTTP `200` 以外のステータスを返したリクエスト数。 |
| `sync_request.requests.bad_request` | カウンター | `400 BAD_REQUEST` で拒否されたリクエスト数。 |
| `sync_request.requests.no_subscribers` | カウンター | 正確に一致する非共有サブスクライバーがオンラインにいないため拒否されたリクエスト数。 |
| `sync_request.requests.conflict` | カウンター | リクエストトピックが複数または共有サブスクライバーにマッチしたため拒否されたリクエスト数。 |
| `sync_request.requests.too_many_requests` | カウンター | `max_inflight_requests` に達したため拒否されたリクエスト数。 |
| `sync_request.requests.dispatch_failed` | カウンター | サブスクライバーノードへのディスパッチに失敗したリクエスト数。 |
| `sync_request.requests.timeout` | カウンター | マッチする MQTT レスポンス待機中にタイムアウトしたリクエスト数。 |
| `sync_request.requests.internal_error` | カウンター | 予期しない内部エラーで失敗したリクエスト数。 |
| `sync_request.inflight_requests` | ゲージ | 現在 MQTT レスポンス待ちの HTTP リクエスト数。 |
| `sync_request.pending_responses` | ゲージ | リクエスト配信後に作成された保留中レスポンス登録数。 |
