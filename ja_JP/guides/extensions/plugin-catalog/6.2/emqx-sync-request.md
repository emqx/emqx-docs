# Sync Request

`emqx_sync_request` プラグインは、HTTP 呼び出し元が EMQX REST API を通じて 1 つの MQTT リクエストをパブリッシュし、最初に一致する MQTT レスポンスを同期的に待機できるようにします。

HTTP ベースのバックエンドサービスが接続された MQTT クライアントにコマンドやクエリを送信し、同じ HTTP リクエスト内で結果を受け取りたい場合にこのプラグインを使用します。プラグインはリクエストの配信、レスポンスの相関、タイムアウト処理、および同時進行中のリクエスト管理を行うため、HTTP 呼び出し元は独自に MQTT クライアントを実行したり、MQTT リクエスト／レスポンスの追跡を実装したりする必要がありません。

## 動作原理

プラグインはプラグイン API ゲートウェイを通じてランタイム API を公開します：

```http
POST /api/v5/plugin_api/emqx_sync_request/request
```

EMQX がこの HTTP リクエストを受信すると、プラグインはリクエストトピックに対してオンラインの MQTT サブスクライバーを特定し、そのサブスクライバーに直接 MQTT リクエストを配信し、一致するレスポンスメッセージを待機します。MQTT 5 のレスポンダーの場合、レスポンスはレスポンストピックと `request_id` を値とする Correlation Data の両方に一致する必要があります。Correlation Data をサポートしない MQTT 3 のレスポンダーの場合は、レスポンスはリクエストの順序に従ってレスポンストピックで一致します。

リクエストトピックは、オンラインの非共有サブスクライバーと完全に一致しなければなりません：

- ワイルドカードトピックフィルターはリクエスト受信者としてマッチしません。
- 共有サブスクリプションはリクエスト受信者として受け付けません。
- 正確に一致するサブスクライバーがオンラインにいない場合、API は `404 NO_SUBSCRIBERS` を返します。
- リクエストトピックに共有サブスクリプションがあるか、正確に一致するサブスクライバーが複数いる場合、API は `409 CONFLICT` を返します。

## 配信セマンティクス

プラグインはインフライトリクエストをローカルノードのメモリにのみ保存します。リクエストを永続化したり、レスポンストピックをサブスクライブしたり、MQTT ペイロードを変更したりしません。

リクエストメッセージは単一の正確なサブスクライバーに対して直接セッション配信されます。通常の MQTT パブリッシュパイプラインは通過しません。そのため、リクエストメッセージはルールエンジン、スキーマ検証、メッセージ変換、保持メッセージ処理、遅延パブリッシュの対象外であり、汎用の `/publish` パスも使用しません。

HTTP の待機タイムアウトはリモートディスパッチとローカルでの MQTT レスポンス待機に共通の単一の期限です。リモートディスパッチ時間も同じタイムアウトにカウントされ、別途の待機時間は加算されません。

一致するレスポンスは、リクエストを配信したノード上のブローカーの `message.publish` フックを通じて観測されます。レスポンダーは通常、リクエストを受信したのと同じ接続のクライアントから同じノード上でレスポンスをパブリッシュする必要があります。別ノードからパブリッシュされたレスポンスは一致しません。

## 設定

| フィールド | デフォルト | 説明 |
| --- | --- | --- |
| `default_timeout` | `10s` | リクエストボディに `timeout` が省略された場合のデフォルト HTTP 待機タイムアウト。 |
| `max_timeout` | `60s` | リクエストごとに許容される最大 `timeout`。 |
| `max_inflight_requests` | `10000` | 1 ノードあたりレスポンス待機中のローカル HTTP リクエストの最大数。 |
| `max_payload_size` | `64KB` | MQTT リクエストおよびレスポンスのペイロード最大サイズ。 |

設定例：

```hocon
default_timeout = "10s"
max_timeout = "60s"
max_inflight_requests = 10000
max_payload_size = "64KB"
```

プラグイン設定は標準のプラグイン設定 API を通じて更新します：

```http
PUT /api/v5/plugins/<name-vsn>/config
```

## HTTP API

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
| `timeout` | 期間文字列 | いいえ | `default_timeout` | 一致する MQTT レスポンスを待機する最大時間。`0` より大きく、`max_timeout` 以下でなければなりません。例：`100ms`、`5s`、`1m`。 |
| `request` | オブジェクト | はい | - | MQTT リクエストパラメータ。 |

`request` オブジェクトのフィールド：

| フィールド | 型 | 必須 | デフォルト | 説明 |
| --- | --- | --- | --- | --- |
| `topic` | 文字列 | はい | - | MQTT リクエストトピック。トピックフィルターではなくトピック名でなければならず、`+` と `#` は許可されません。このトピックに対して正確に 1 つの非共有サブスクライバーがオンラインである必要があります。 |
| `response_topic` | 文字列 | はい | - | MQTT レスポンストピック。こちらも `+` や `#` を含まないトピック名でなければなりません。 |
| `request_id` | 文字列 | はい | - | MQTT 5 の Correlation Data として使用され、HTTP レスポンスにエコーバックされるプレーン文字列。最大長は 128 バイトです。 |
| `qos` | 整数 | いいえ | `0` | リクエストの MQTT QoS。許容値は `0`、`1`、`2`。 |
| `payload_encoding` | 文字列 | いいえ | `plain` | リクエストペイロードのエンコーディング。許容値は `plain` と `base64`。 |
| `payload` | 文字列 | はい | - | リクエストペイロード。`plain` の場合は文字列のバイト列が MQTT ペイロードとして使われます。`base64` の場合は有効な base64 文字列で、デコード後のバイト列が MQTT ペイロードとなります。MQTT ペイロードは `max_payload_size` を超えてはなりません。 |
| `content_type` | 文字列 | いいえ | - | MQTT 5 のリクエスト用 Content Type。MQTT 3 クライアントには送信されません。 |

### 成功レスポンス

成功したリクエストは HTTP `200` を返します。MQTT レスポンスペイロードは常に base64 で返されます。

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

エラーは他の EMQX 管理 API と同様の `code` と `message` のレスポンス形式を使用します。

| HTTP ステータス | コード | 意味 |
| --- | --- | --- |
| `400` | `BAD_REQUEST` | 不正な JSON ボディ、無効なフィールド値、リクエストペイロードが大きすぎる、または MQTT レスポンスペイロードが大きすぎる。 |
| `401` | `BAD_API_KEY_OR_SECRET` | API キー認証失敗。EMQX 管理 API 認証による返却。 |
| `403` | `UNAUTHORIZED_ROLE` | API キーにこの API を呼び出す権限がない。EMQX 管理 API 認可による返却。 |
| `404` | `NO_SUBSCRIBERS` | リクエストトピックに対して正確な非共有サブスクライバーがオンラインにいない。ワイルドカードサブスクライバーは無視されます。 |
| `409` | `CONFLICT` | リクエストトピックに共有サブスクリプションがあるか、正確に一致するサブスクライバーが複数いる。 |
| `429` | `TOO_MANY_REQUESTS` | このノードで既に `max_inflight_requests` の HTTP リクエストがレスポンス待機中。 |
| `503` | `SERVICE_UNAVAILABLE` | サブスクライバーノードへのリクエストディスパッチに失敗。 |
| `504` | `TIMEOUT` | 一致する MQTT レスポンスの待機でタイムアウト。 |
| `500` | `INTERNAL_ERROR` | 想定外のサーバー側エラー。 |

## 運用診断

プラグインはノードローカルの診断 CLI コマンドを提供します：

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

これらの値はクラスター全体の集計ではありません。コマンドは実行したノードのみを読み取ります。クラスター環境では、HTTP リクエストを受け取ったり MQTT レスポンスを配信したりする可能性のある各ノードで実行してください。

プラグインハンドラーに到達したリクエストのみがカウントされます。管理 API の認証および認可失敗はプラグイン実行前に EMQX が処理します。

| メトリクス | 種類 | スコープ | 説明 |
| --- | --- | --- | --- |
| `sync_request.requests.total` | カウンター | ノードローカル | このノードで処理された HTTP 同期リクエスト試行数。 |
| `sync_request.requests.succeeded` | カウンター | ノードローカル | HTTP `200` を返したリクエスト数。 |
| `sync_request.requests.failed` | カウンター | ノードローカル | HTTP ステータスが `200` 以外で返されたリクエスト数。 |
| `sync_request.requests.bad_request` | カウンター | ノードローカル | `400 BAD_REQUEST` で拒否されたリクエスト数。 |
| `sync_request.requests.no_subscribers` | カウンター | ノードローカル | 正確な非共有サブスクライバーがオンラインにいなかったため拒否されたリクエスト数。 |
| `sync_request.requests.conflict` | カウンター | ノードローカル | リクエストトピックが複数または共有サブスクライバーにマッチしたため拒否されたリクエスト数。 |
| `sync_request.requests.too_many_requests` | カウンター | ノードローカル | このノードで `max_inflight_requests` に達したため拒否されたリクエスト数。 |
| `sync_request.requests.dispatch_failed` | カウンター | ノードローカル | サブスクライバーノードへのディスパッチに失敗したリクエスト数。 |
| `sync_request.requests.timeout` | カウンター | ノードローカル | 一致する MQTT レスポンスの待機でタイムアウトしたリクエスト数。 |
| `sync_request.requests.internal_error` | カウンター | ノードローカル | 想定外の内部エラーで失敗したリクエスト数。 |
| `sync_request.inflight_requests` | ゲージ | ノードローカル | このノードで MQTT レスポンス待機中の HTTP リクエストの現在数。 |
| `sync_request.pending_responses` | ゲージ | ノードローカル | リクエスト配信後に作成されたローカルの保留中レスポンス登録の現在数。 |

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリース向けの tarball：

| EMQX バージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.2.3 | 0.1.1 | [emqx_sync_request-0.1.1.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_sync_request-0.1.1.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_sync_request-0.1.1.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
