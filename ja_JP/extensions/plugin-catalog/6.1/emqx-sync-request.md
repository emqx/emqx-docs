# Sync Request

`emqx_sync_request`プラグインは、HTTP呼び出し元がEMQX REST APIを介して1つのMQTTリクエストをパブリッシュし、最初に一致するMQTTレスポンスを同期的に待機できる機能を提供します。

HTTPベースのバックエンドサービスが接続されたMQTTクライアントにコマンドやクエリを送信し、同じHTTPリクエスト内で結果を受け取りたい場合にこのプラグインを使用します。プラグインはリクエストの配信、レスポンスの相関、タイムアウト処理、および同時進行中のリクエスト管理を行うため、HTTP呼び出し元は独自にMQTTクライアントを実行したり、MQTTリクエスト/レスポンスの追跡を実装する必要がありません。

## 動作概要

プラグインはプラグインAPIゲートウェイを通じてランタイムAPIを公開します：

```http
POST /api/v5/plugin_api/emqx_sync_request/request
```

EMQXがHTTPリクエストを受信すると、プラグインはリクエストトピックに対するオンラインのMQTTサブスクライバーを特定し、そのサブスクライバーに直接MQTTリクエストを配信し、一致するレスポンスメッセージを待機します。MQTT 5のレスポンダーの場合、レスポンスはレスポンストピックと`request_id`を値とするCorrelation Dataの両方に一致する必要があります。Correlation DataをサポートしないMQTT 3のレスポンダーでは、レスポンスはリクエストの順序に従いレスポンストピックでマッチングされます。

リクエストトピックは、オンラインでかつ非共有のサブスクライバーに正確に一致する必要があります：

- ワイルドカードトピックフィルターはリクエスト受信者としてマッチしません。
- 共有サブスクリプションはリクエスト受信者として受け付けません。
- 正確なサブスクライバーがオンラインに存在しない場合、APIは`404 NO_SUBSCRIBERS`を返します。
- リクエストトピックに共有サブスクリプションがあるか、正確なサブスクライバーが複数いる場合、APIは`409 CONFLICT`を返します。

## 配信セマンティクス

プラグインはインフライトリクエストをローカルノードのメモリにのみ保存します。リクエストを永続化せず、レスポンストピックへのサブスクライブも行わず、MQTTペイロードの変更も行いません。

リクエストメッセージは単一の正確なサブスクライバーに対して直接セッション配信されます。通常のMQTTパブリッシュパイプラインは通過しません。そのため、リクエストメッセージはルールエンジン、スキーマ検証、メッセージ変換、保持メッセージ処理、遅延パブリッシュの対象外であり、一般的な`/publish`パスも使用しません。

HTTPの待機タイムアウトは、リモートディスパッチとMQTTレスポンスのローカル待機の両方に共有される単一の期限です。リモートディスパッチ時間も同じタイムアウトにカウントされ、別途待機時間が加算されることはありません。

一致するレスポンスは、リクエストを配信したノードのブローカー`message.publish`フックを通じて監視されます。レスポンダーは通常、リクエストを受信したのと同じ接続のクライアントからレスポンスをパブリッシュすべきです。別のノードからパブリッシュされたレスポンスはマッチしません。

## 設定

| フィールド | デフォルト | 説明 |
| --- | --- | --- |
| `default_timeout` | `10s` | リクエストボディに`timeout`がない場合のデフォルトHTTP待機タイムアウト。 |
| `max_timeout` | `60s` | リクエストごとに許容される最大`timeout`。 |
| `max_inflight_requests` | `10000` | 1ノードあたりレスポンス待機中のローカルHTTPリクエストの最大数。 |
| `max_payload_size` | `64KB` | MQTTリクエストおよびレスポンスの最大ペイロードサイズ。 |

設定例：

```hocon
default_timeout = "10s"
max_timeout = "60s"
max_inflight_requests = 10000
max_payload_size = "64KB"
```

標準のプラグイン設定APIを通じてプラグイン設定を更新します：

```http
PUT /api/v5/plugins/<name-vsn>/config
```

## HTTP API

他のEMQX管理APIと同じ認証方法を使用します。ダッシュボードログインで取得したBearerトークンが利用可能です。APIキーはHTTP Basic認証で送信し、`publish`スコープが必要です。

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
| `timeout` | duration文字列 | いいえ | `default_timeout` | 一致するMQTTレスポンスを待つ最大時間。`0`より大きく、`max_timeout`以下でなければなりません。例：`100ms`、`5s`、`1m`。 |
| `request` | オブジェクト | はい | - | MQTTリクエストパラメータ。 |

`request`オブジェクトのフィールド：

| フィールド | 型 | 必須 | デフォルト | 説明 |
| --- | --- | --- | --- | --- |
| `topic` | 文字列 | はい | - | MQTTリクエストトピック。トピックフィルターではなくトピック名である必要があり、`+`や`#`は使用できません。このトピックに対して正確に1つの非共有サブスクライバーがオンラインでなければなりません。 |
| `response_topic` | 文字列 | はい | - | MQTTレスポンストピック。こちらも`+`や`#`を含まないトピック名である必要があります。 |
| `request_id` | 文字列 | はい | - | MQTT 5のCorrelation Dataとして使用され、HTTPレスポンスにも反映されるプレーン文字列。最大長は128バイトです。 |
| `qos` | 整数 | いいえ | `0` | リクエストのMQTT QoS。許容値は`0`、`1`、`2`です。 |
| `payload_encoding` | 文字列 | いいえ | `plain` | リクエストペイロードのエンコーディング。許容値は`plain`と`base64`です。 |
| `payload` | 文字列 | はい | - | リクエストペイロード。`plain`の場合は文字列のバイト列がMQTTペイロードとして使用されます。`base64`の場合は有効なbase64文字列でなければならず、デコード後のバイト列がMQTTペイロードとして使用されます。MQTTペイロードは`max_payload_size`を超えてはいけません。 |
| `content_type` | 文字列 | いいえ | - | MQTT 5のリクエスト用Content Type。MQTT 3クライアントはこのプロパティを受け取りません。 |

### 成功レスポンス

成功したリクエストはHTTP `200`を返します。MQTTレスポンスペイロードは常にbase64で返されます。

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

エラーは他のEMQX管理APIと同様の`code`と`message`のレスポンス形式を使用します。

| HTTPステータス | コード | 意味 |
| --- | --- | --- |
| `400` | `BAD_REQUEST` | 無効なJSONボディ、無効なフィールド値、リクエストペイロードが大きすぎる、またはMQTTレスポンスペイロードが大きすぎる。 |
| `401` | `BAD_API_KEY_OR_SECRET` | APIキー認証失敗。EMQX管理API認証による返却。 |
| `403` | `UNAUTHORIZED_ROLE` | APIキーにこのAPIを呼び出す権限がない。EMQX管理API認可による返却。 |
| `404` | `NO_SUBSCRIBERS` | リクエストトピックに対して正確な非共有サブスクライバーがオンラインにいない。ワイルドカードサブスクライバーは無視されます。 |
| `409` | `CONFLICT` | リクエストトピックに共有サブスクリプションがあるか、正確なサブスクライバーが複数いる。 |
| `429` | `TOO_MANY_REQUESTS` | このノードで既に`max_inflight_requests`のHTTPリクエストがレスポンス待機中。 |
| `503` | `SERVICE_UNAVAILABLE` | リクエストをサブスクライバーノードにディスパッチできなかった。 |
| `504` | `TIMEOUT` | 一致するMQTTレスポンスの待機がタイムアウトした。 |
| `500` | `INTERNAL_ERROR` | 予期しないサーバー側エラー。 |

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

これらの値はクラスター全体の集計ではありません。コマンドは実行したノードの情報のみを読み取ります。クラスター環境では、HTTPリクエストを受信またはMQTTレスポンスを配信する可能性のある各ノードで実行してください。

プラグインハンドラーに到達したリクエストのみがカウントされます。管理APIの認証・認可失敗はプラグイン実行前にEMQXが処理します。

| メトリクス | 種類 | スコープ | 説明 |
| --- | --- | --- | --- |
| `sync_request.requests.total` | カウンター | ノードローカル | このノードで処理されたHTTP同期リクエストの試行回数。 |
| `sync_request.requests.succeeded` | カウンター | ノードローカル | HTTP `200`を返したリクエスト数。 |
| `sync_request.requests.failed` | カウンター | ノードローカル | HTTP `200`以外のステータスを返したリクエスト数。 |
| `sync_request.requests.bad_request` | カウンター | ノードローカル | `400 BAD_REQUEST`で拒否されたリクエスト数。 |
| `sync_request.requests.no_subscribers` | カウンター | ノードローカル | 正確な非共有サブスクライバーがオンラインにいないため拒否されたリクエスト数。 |
| `sync_request.requests.conflict` | カウンター | ノードローカル | リクエストトピックが複数または共有サブスクライバーにマッチしたため拒否されたリクエスト数。 |
| `sync_request.requests.too_many_requests` | カウンター | ノードローカル | このノードで`max_inflight_requests`に達したため拒否されたリクエスト数。 |
| `sync_request.requests.dispatch_failed` | カウンター | ノードローカル | サブスクライバーノードへのディスパッチに失敗したリクエスト数。 |
| `sync_request.requests.timeout` | カウンター | ノードローカル | 一致するMQTTレスポンスの待機がタイムアウトしたリクエスト数。 |
| `sync_request.requests.internal_error` | カウンター | ノードローカル | 予期しない内部エラーで失敗したリクエスト数。 |
| `sync_request.inflight_requests` | ゲージ | ノードローカル | このノードでMQTTレスポンス待機中のHTTPリクエスト数。 |
| `sync_request.pending_responses` | ゲージ | ノードローカル | リクエスト配信後に作成されたローカルの保留中レスポンス登録数。 |

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリースに対応するプラグインパッケージ:

| EMQX バージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.1.4 | 0.1.0 | [emqx_sync_request-0.1.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.4/emqx_sync_request-0.1.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.4/emqx_sync_request-0.1.0.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
