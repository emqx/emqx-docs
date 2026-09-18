# Sync Request

`emqx_sync_request` プラグインは、HTTPサービスがEMQX REST APIを通じてMQTTリクエストを送信し、同一のHTTPリクエスト内で最初に一致したMQTTレスポンスを受信できるようにします。このプラグインはEMQX Enterprise 5.10.5以降で利用可能です。

バックエンドサービスが接続されたMQTTクライアントにコマンドやクエリを送信する必要がある場合に、このプラグインを使用してください。標準のパブリッシュAPIとは異なり、このプラグインはクライアントのレスポンスを待機して相関付けを行い、タイムアウトを処理し、同時リクエストを管理します。HTTPサービスはMQTTクライアントを実行したり、リクエストとレスポンスのペアを追跡したりする必要がありません。

APIを使用する前に、[プラグイン管理](../plugin-management.md)に記載の手順で`emqx_sync_request`をインストールして起動してください。プラグインが起動している間のみエンドポイントが利用可能です。

## 動作の仕組み

リクエストとレスポンスのフローは以下の通りです：

1. HTTP呼び出し元が、MQTTリクエストのトピック、レスポンストピック、`request_id`、およびペイロードを含むHTTPリクエストをAPIに送信します。
2. プラグインはリクエストトピックにサブスクライブしているMQTTクライアントを特定し、そのクライアントに直接リクエストを配信します。
3. MQTTクライアントはリクエストを処理し、レスポンストピックにレスポンスをパブリッシュします。MQTT 5の場合、プラグインはリクエストメッセージの相関データとして`request_id`を含めます。
4. MQTT 5クライアントがこの相関データを返す場合、プラグインはレスポンストピックと相関データでレスポンスを照合します。相関データが含まれない場合、プラグインはレスポンストピックに対する最も古い保留中リクエストにマッチさせます。このフォールバックはMQTT 3のレスポンスおよび相関データを省略したMQTT 5レスポンスに適用されます。同じレスポンストピックを使用する同時リクエストでは、MQTT 5クライアントが相関データを返すことで各レスポンスが意図したリクエストに正しくマッチします。
5. プラグインは最初にマッチしたMQTTレスポンスをHTTP呼び出し元に返します。タイムアウトまでにマッチするレスポンスが届かない場合、APIは`504 TIMEOUT`を返します。

リクエストトピックは、オンラインかつ非共有のサブスクライバーに正確に一致する必要があります：

- ワイルドカードトピックフィルターはリクエスト受信者としてマッチしません。
- 共有サブスクリプションはリクエスト受信者として受け付けません。
- 正確なサブスクライバーがオンラインにいない場合、APIは`404 NO_SUBSCRIBERS`を返します。
- リクエストトピックに共有サブスクリプションがあるか、正確なサブスクライバーが複数いる場合、APIは`409 CONFLICT`を返します。

## リクエスト配信とレスポンス処理

プラグインはインフライトリクエストをローカルノードのメモリにのみ保持します。リクエストを永続化したり、レスポンストピックにサブスクライブしたり、MQTTペイロードを変更したりしません。

EMQXは各リクエストを通常のMQTTパブリッシュパイプラインを経由せず、選択されたクライアントに直接配信します。そのため、リクエストはルールエンジン、スキーマ検証、メッセージ変換、保持メッセージ処理、遅延パブリッシュの対象外であり、汎用の`/publish` APIも使用しません。

リクエストを別ノードに転送してMQTTレスポンスを待つ場合、HTTPタイムアウトは共有されます。転送時間がレスポンス待機時間を減少させます。

レスポンスはリクエストを配信したノードに接続されたクライアントによってパブリッシュされる必要があります。通常は同じ接続を通じて受信したクライアントです。別ノードを経由したレスポンスはマッチしません。

## プラグイン設定

これらの設定はプラグイン全体のタイムアウトや各ノードのリソース制限を制御します。リクエスト固有のパラメータは[リクエストボディ](#request-body)で説明します。

| フィールド | デフォルト | 説明 |
| --- | --- | --- |
| `default_timeout` | `10s` | リクエストボディに`timeout`がない場合のデフォルトHTTP待機タイムアウト。 |
| `max_timeout` | `60s` | リクエストごとに許可される最大`timeout`。 |
| `max_inflight_requests` | `10000` | 1ノードあたりレスポンス待ちのローカルHTTPリクエスト最大数。 |
| `max_payload_size` | `64KB` | MQTTリクエストおよびレスポンスの最大ペイロードサイズ。 |

設定例：

```hocon
default_timeout = "10s"
max_timeout = "60s"
max_inflight_requests = 10000
max_payload_size = "64KB"
```

標準のプラグイン設定APIを通じて設定を更新します：

```http
PUT /api/v5/plugins/<name-vsn>/config
```

## 同期リクエストAPI

以下のエンドポイントを呼び出してMQTTリクエストを送信し、そのレスポンスを待機します：

```http
POST /api/v5/plugin_api/emqx_sync_request/request
```

他のEMQX管理APIと同様の認証方法を使用します。ダッシュボードログインで取得したベアラートークンが利用可能です。APIキーはHTTP Basic認証で送信し、`publish`スコープが必要です。

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
| `timeout` | 期間文字列 | いいえ | `default_timeout` | マッチするMQTTレスポンスを待つ最大時間。`0`より大きく、`max_timeout`以下である必要があります。例：`100ms`、`5s`、`1m`。 |
| `request` | オブジェクト | はい | - | MQTTリクエストのパラメータ。 |

`request`オブジェクトのフィールド：

| フィールド | 型 | 必須 | デフォルト | 説明 |
| --- | --- | --- | --- | --- |
| `topic` | 文字列 | はい | - | MQTTリクエストトピック。トピックフィルターではなくトピック名である必要があり、`+`や`#`は使用できません。このトピックに対して正確に1つの非共有サブスクライバーがオンラインである必要があります。 |
| `response_topic` | 文字列 | はい | - | MQTTレスポンストピック。こちらも`+`や`#`を含まないトピック名である必要があります。 |
| `request_id` | 文字列 | はい | - | MQTT 5相関データとして使用され、HTTPレスポンスにエコーバックされるプレーン文字列。最大長は128バイトです。 |
| `qos` | 整数 | いいえ | `0` | リクエストのMQTT QoS。許容値は`0`、`1`、`2`です。 |
| `payload_encoding` | 文字列 | いいえ | `plain` | リクエストペイロードのエンコーディング。許容値は`plain`と`base64`です。 |
| `payload` | 文字列 | はい | - | リクエストペイロード。`plain`の場合は文字列のバイト列がMQTTペイロードとして使われます。`base64`の場合は有効なbase64文字列で、デコード後のバイト列がMQTTペイロードになります。MQTTペイロードは`max_payload_size`を超えてはいけません。 |
| `content_type` | 文字列 | いいえ | - | MQTT 5のリクエスト用Content Type。MQTT 3クライアントには送信されません。 |

### 成功レスポンス

成功したリクエストはHTTP `200`を返します。MQTTレスポンスのペイロードは常にbase64で返されます。

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
| `code` | 常に`OK`。 |
| `message` | 常に`OK`。 |
| `response.topic` | MQTTレスポンストピック。 |
| `response.request_id` | HTTPリクエストの`request_id`。 |
| `response.payload_encoding` | 常に`base64`。 |
| `response.payload` | base64エンコードされたMQTTレスポンスペイロード。 |
| `response.content_type` | 任意。レスポンスPUBLISHのMQTT 5 Content Type。レスポンダーが送信しない場合（MQTT 3レスポンダーを含む）は省略されます。 |

### エラーレスポンス

エラーは他のEMQX管理APIと同様の`code`と`message`の形で返されます。

| HTTPステータス | コード | 意味 |
| --- | --- | --- |
| `400` | `BAD_REQUEST` | 無効なJSONボディ、無効なフィールド値、リクエストペイロードが大きすぎる、またはMQTTレスポンスペイロードが大きすぎる。 |
| `401` | `BAD_API_KEY_OR_SECRET` | APIキー認証失敗。EMQX管理API認証による返却。 |
| `403` | `UNAUTHORIZED_ROLE` | APIキーにこのAPIを呼び出す権限がない。EMQX管理API認可による返却。 |
| `404` | `NO_SUBSCRIBERS` | リクエストトピックに正確かつ非共有のサブスクライバーがオンラインにいない。ワイルドカードサブスクライバーは無視されます。 |
| `409` | `CONFLICT` | リクエストトピックに共有サブスクリプションがあるか、正確なサブスクライバーが複数いる。 |
| `429` | `TOO_MANY_REQUESTS` | ローカルノードで`max_inflight_requests`のHTTPリクエストが既にレスポンス待ち。 |
| `503` | `SERVICE_UNAVAILABLE` | リクエストをサブスクライバーノードにディスパッチできなかった。 |
| `504` | `TIMEOUT` | マッチするMQTTレスポンスの待機でタイムアウト。 |
| `500` | `INTERNAL_ERROR` | 予期しないサーバー内部エラー。 |

## 運用診断

プラグインはノードローカルの診断CLIコマンドを提供します：

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

これらの値はクラスター全体の集計ではありません。コマンドは実行したノードのみを読み取ります。クラスター環境では、HTTPリクエストを受け取るかMQTTレスポンスを配信する可能性のある各ノードで実行してください。

プラグインハンドラに到達したリクエストのみがカウントされます。管理APIの認証・認可失敗はプラグイン実行前にEMQXが処理します。

| メトリクス | 種類 | 説明 |
| --- | --- | --- |
| `sync_request.requests.total` | カウンター | 処理したHTTP同期リクエストの試行回数。 |
| `sync_request.requests.succeeded` | カウンター | HTTP `200`を返したリクエスト数。 |
| `sync_request.requests.failed` | カウンター | HTTP `200`以外のステータスを返したリクエスト数。 |
| `sync_request.requests.bad_request` | カウンター | `400 BAD_REQUEST`で拒否されたリクエスト数。 |
| `sync_request.requests.no_subscribers` | カウンター | 正確かつ非共有のサブスクライバーがオンラインにいないため拒否されたリクエスト数。 |
| `sync_request.requests.conflict` | カウンター | リクエストトピックが複数または共有サブスクライバーにマッチしたため拒否されたリクエスト数。 |
| `sync_request.requests.too_many_requests` | カウンター | `max_inflight_requests`に達したため拒否されたリクエスト数。 |
| `sync_request.requests.dispatch_failed` | カウンター | サブスクライバーノードへのディスパッチに失敗したリクエスト数。 |
| `sync_request.requests.timeout` | カウンター | マッチするMQTTレスポンスの待機でタイムアウトしたリクエスト数。 |
| `sync_request.requests.internal_error` | カウンター | 予期しない内部エラーで失敗したリクエスト数。 |
| `sync_request.inflight_requests` | ゲージ | 現在MQTTレスポンス待ちのHTTPリクエスト数。 |
| `sync_request.pending_responses` | ゲージ | リクエスト配信後に作成された保留中レスポンス登録数。 |
