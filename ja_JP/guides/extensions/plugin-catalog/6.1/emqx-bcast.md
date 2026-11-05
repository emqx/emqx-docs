# Device Message Broadcasting (Bcast)

`emqx_bcast` プラグインは、バックエンドアプリケーションが HTTP 経由で IoT デバイスにコマンド、通知、その他のメッセージを配信できるようにします。製品全体へのブロードキャスト、ターゲットを絞ったバッチ配信、QoS 1 のオフライン再生をサポートしているため、アプリケーションは MQTT 接続を維持したり、独自の一時的なオフライン配信キューや MQTT PUBACK トラッキングシステムを構築したりする必要がありません。アプリケーションはペイロードを一度登録し、後のバッチリクエストでそのメッセージ ID を再利用することも可能です。

このプラグインは EMQX Enterprise 6.1.6 以降の 6.1 系リリースで利用可能です。

典型的なユースケースとしては、製品内のデバイスへのサービス通知や設定更新信号のブロードキャスト、選択したデバイスへのコマンドや設定更新の送信、一時的にオフラインだったデバイスが再接続した際の QoS 1 コマンドの配信などがあります。

## クイックスタート

この例では、製品 `P1` のオンラインデバイスに `Hello` をブロードキャストします。EMQX Enterprise 6.1.6 以降の 6.1 リリースが稼働しており、[MQTTX CLI](https://mqttx.app/cli) がデバイス接続用に利用可能であることを前提としています。

### プラグインのインストールと起動

このクイックスタートではクラスター管理プラグインモードを使用します。[Download](#download) に記載のパッケージをダウンロードし、EMQX のプラグインディレクトリにコピーして、`<version>` をダウンロードしたプラグインのバージョンに置き換えます。

```bash
cp emqx_bcast-<version>.tar.gz "$EMQX_HOME/plugins/"
emqx ctl plugins install emqx_bcast-<version> --cluster
emqx ctl plugins start emqx_bcast-<version>
```

シングルノード展開の場合は `--cluster` を省略してください。プラグインはダッシュボードからもインストールおよび起動可能です。ダッシュボードからのインストールに必要な認可を含む両方の方法の詳細は、[Install and Manage Plugins](../../plugin-management.md#install-and-manage-plugins) を参照してください。

### デバイス識別マッピングの設定

プラグインは各 MQTT クライアントを以下の識別子にマッピングします。

- `ProductKey`: クライアントの `client_attrs.tns` ネームスペース属性
- `DeviceName`: MQTT クライアント ID

ダッシュボードで **Management** -> **MQTT Settings** -> **Client Attributes** に移動し、属性 `tns` を追加して、その式を `nth(1, tokens(username, '-'))` に設定します。これは、`ProductKey-DeviceName` 形式のユーザー名の最初のセグメントからネームスペースを導出する設定に相当します。

```hocon
mqtt.client_attrs_init = [
  { expression = "nth(1, tokens(username, '-'))", set_as_attr = tns }
]
```

この設定により、ユーザー名 `P1-device001`、クライアント ID `device001` で接続するクライアントは、`ProductKey` が `P1`、`DeviceName` が `device001` となります。

クライアント属性の設定方法の詳細は、[Set Client Attributes](../../../../develop/client-attributes/client-attributes.md#set-client-attributes) を参照してください。

この設定を適用した後、クライアントを再接続して EMQX が接続時に属性を初期化するようにしてください。

### API キーの作成

ダッシュボードで **System** -> **API Keys** に移動し、**Administrator** ロールの API キーを作成します。**Namespace** は無効のままにしてグローバルキーを作成してください。**Custom Restricted Permissions** を選択し、`plugin_api` スコープを選択します。API キーとシークレットはすぐに保存してください。詳細な手順は [Create API Keys](../../../api.md#create-api-keys) を参照してください。

`plugin_api` スコープは `/api/v5/plugin_api/emqx_bcast/*` 以下の Bcast ランタイムエンドポイントへの認可を付与します。プラグイン管理エンドポイント `/api/v5/plugins*` への認可は含まれず、こちらは `system` スコープが必要です。

### デバイスの接続とサブスクライブ

以下のコマンドを実行して、`device001` をユーザー名 `P1-device001` で接続し、製品 `P1` のデフォルトブロードキャストトピックをサブスクライブします。

```bash
mqttx sub -h 127.0.0.1 -p 1883 \
  -i device001 \
  -u P1-device001 \
  -t '/sys/broadcast/P1' \
  -q 1 -v
```

MQTT リスナーが認証を必要とする場合は、対応するパスワードや TLS オプションを追加してください。デフォルトトピックで `BatchPub` メッセージを受信するには、代わりに `/P1/device001/user/get` をサブスクライブしてください。

ターゲットクライアントは配信トピックをサブスクライブしている必要があります。クライアントが接続されているだけではプラグインはメッセージを配信しません。

### ブロードキャストの送信と検証

MQTTX クライアントを起動したまま、別のターミナルで `<api_key>` と `<api_secret>` を前の手順で作成した認証情報に置き換え、以下を実行します。

```bash
curl -u '<api_key>:<api_secret>' \
  -X POST 'http://127.0.0.1:18083/api/v5/plugin_api/emqx_bcast/pub' \
  -H 'Content-Type: application/json' \
  -d '{"Action":"PubBroadcast","ProductKey":"P1","MessageContent":"SGVsbG8="}'
```

`MessageContent` は base64 エンコード済みである必要があります。この例の `SGVsbG8=` は `Hello` にデコードされます。

EMQX は HTTP `200` を返し、以下のようなレスポンスを返します。

```json
{
  "Success": true,
  "RequestId": "550e8400-e29b-41d4-a716-446655440000",
  "MessageId": "550e8400-e29b-41d4-a716-446655440000"
}
```

MQTTX クライアントはトピック `/sys/broadcast/P1` でペイロード `Hello` を受信します。

HTTP `200` は EMQX がリクエストを受理したことを意味し、配信を保証するものではありません。各 QoS レベルの配信保証については [Delivery Semantics](#delivery-semantics) を参照してください。

::: warning 重要なお知らせ

Bcast はメッセージをクライアントチャネルに直接送信します。これらのメッセージはパブリッシュ認可チェックや通常の MQTT パブリッシュパイプライン（ルールエンジン、スキーマ検証、メッセージ変換、保持メッセージ、遅延パブリッシュを含む）をバイパスします。

Bcast API へのアクセスは信頼できる呼び出し元にのみ許可し、呼び出しアプリケーション側でペイロードおよびターゲット識別子の検証を行ってください。

:::

## メッセージアクション

すべてのメッセージアクションは以下のエンドポイントを使用します。

```http
POST /api/v5/plugin_api/emqx_bcast/pub
```

JSON リクエストボディの `Action` フィールドで操作を選択します。`MessageContent` は base64 エンコード済みである必要があります。

### オンラインデバイスへのブロードキャスト

`PubBroadcast` は、製品内の配信トピックにサブスクライブしているすべてのオンラインデバイスにメッセージを送信します。メッセージは保存されないため、オフラインデバイスには届きません。

```json
{
  "Action": "PubBroadcast",
  "ProductKey": "P1",
  "MessageContent": "SGVsbG8="
}
```

プラグインは設定された `broadcast_topic` テンプレート（デフォルトは `/sys/broadcast/${productKey}`）を展開します。`TopicFullName` に具体的なカスタムトピックを設定することも可能です。`TopicFullName` にはワイルドカードやテンプレートプレースホルダーは使用できません。

デフォルトの最大デコード済みペイロードサイズは 64 KiB です。

### デバイスリストへの送信

`BatchPub` は明示的なデバイスリストにメッセージを送信します。1 リクエストで最大 `max_device_count` 個のユニークなデバイス名を指定可能です。デフォルトは `10,000` です。

```json
{
  "Action": "BatchPub",
  "ProductKey": "P1",
  "DeviceName": ["device001", "device002"],
  "MessageContent": "SGVsbG8=",
  "Qos": 1
}
```

以下のペイロードフィールドのうち、いずれか一つを正確に指定してください。

- `MessageContent`: インラインの base64 エンコード済みペイロード。デフォルトの最大デコード済みペイロードサイズは 10 KiB です。
- `MessageId`: `RegisterMessage` または以前の QoS 1 バッチリクエストで作成されたペイロードの ID。

`BatchPub` の配信トピックは以下の順で選択されます。

1. `TopicTemplateName`（`${productKey}` と `${deviceName}` を展開）
2. `TopicShortName`（`/${productKey}/${deviceName}/user/${TopicShortName}` として展開）
3. 設定された `batch_topic`（デフォルトは `/${productKey}/${deviceName}/user/get`）

### メッセージの登録と再利用

`RegisterMessage` は後続の `BatchPub` リクエストで再利用可能なペイロードを保存します。

```json
{
  "Action": "RegisterMessage",
  "MessageContent": "SGVsbG8="
}
```

レスポンスには `MessageId` が含まれます。同一内容の登録は同じ ID を返し、TTL を更新します。保存されたペイロードは不変です。

既存メッセージの TTL を更新するには、内容の代わりに ID を送信します。

```json
{
  "Action": "RegisterMessage",
  "MessageId": "550e8400-e29b-41d4-a716-446655440000"
}
```

`MessageContent` と `MessageId` は相互排他です。

## 配信セマンティクス

`BatchPub` の `Qos` フィールドは `0` または `1` を受け付け、デフォルトは `0` です。

| 要求された QoS | オンラインデバイス | オフラインデバイス | ストレージと確認 |
| --- | --- | --- | --- |
| `0` | 対象がオンラインかつサブスクライブしていれば配信される。 | スキップされる。 | ストレージもアック追跡もなし。 |
| `1` | サブスクライブ QoS が 1 の場合、配信され PUBACK まで追跡される。 | デバイスが再接続しサブスクライブ後に保存され再生される。 | 保留中配信は `msg_ttl`（デフォルト 15 日）後に期限切れ。 |

実際の配信 QoS は要求 QoS と対象サブスクライブ QoS の低い方です。QoS 1 のリクエストが QoS 0 のサブスクライブに届いた場合、プラグインはクライアントチャネルにメッセージを渡した時点で配信完了とみなします。

PUBACK は MQTT プロトコルレベルでの受信確認です。デバイスアプリケーションがメッセージを処理したりコマンドを実行したことを保証しません。

QoS 1 のバッチ配信は非同期で少なくとも一回配信です。クライアントは特に再接続やセッション引き継ぎ時に重複メッセージを許容する必要があります。HTTP `200` の成功応答は EMQX が非同期処理のためリクエストを受理したことを意味し、メッセージが保存または配信されたことを意味しません。受理ノードが保存完了前に障害を起こすとリクエストが失われる可能性があります。

保留中の QoS 1 状態はコアノード間でメモリ内レプリケーションされます。1 つのコアノード障害には耐えられますが、クラスター全体の再起動には耐えられません。プロデューサーはクラスター完全再起動後にメッセージを再送できる必要があります。

QoS 0 メッセージもセッション引き継ぎウィンドウ中に重複する可能性があります。

## プラグインの設定

以下の表は有効な設定項目とそのデフォルト値です。

| 設定項目 | デフォルト | 説明 |
| --- | --- | --- |
| `broadcast_topic` | `/sys/broadcast/${productKey}` | `PubBroadcast` 用のトピックテンプレート。 |
| `batch_topic` | `/${productKey}/${deviceName}/user/get` | `BatchPub` のデフォルトトピックテンプレート。 |
| `msg_ttl` | `15d` | 登録メッセージおよび保留中 QoS 1 配信の TTL。更新は更新後に作成されたレコードにのみ適用。 |
| `cleanup_interval` | `60s` | 期限切れレコード削除の間隔。 |
| `max_device_count` | `10000` | 1 回の `BatchPub` リクエストで指定可能な最大デバイス数。 |
| `max_message_size_broadcast` | `65536` | ブロードキャストの最大デコード済みペイロードサイズ（バイト）。 |
| `max_message_size_batch` | `10240` | バッチの最大デコード済みペイロードサイズ（バイト）。 |
| `max_pending_deliveries` | `10000000` | クラスター全体の保留中 QoS 1 配信の上限。 |
| `max_pending_deliveries_per_device` | `100` | デバイスごとの保留中 QoS 1 配信の上限。値は 10 から 200 の範囲に制限される。起動時やフェイルオーバー時にベストエフォートで適用。 |
| `delivery_pool_size` | `0` | 配信プールごとのワーカー数。`0` は Erlang スケジューラー数に合わせて自動設定。変更時にプールが再起動。 |

設定例:

```hocon
broadcast_topic = "/sys/broadcast/${productKey}"
batch_topic = "/${productKey}/${deviceName}/user/get"
msg_ttl = 15d
cleanup_interval = 60s
max_device_count = 10000
max_message_size_broadcast = 65536
max_message_size_batch = 10240
max_pending_deliveries = 10000000
max_pending_deliveries_per_device = 100
delivery_pool_size = 0
```

これらの設定は標準のプラグイン設定 API で更新可能です。

```http
PUT /api/v5/plugins/<name-vsn>/config
```

インストール、起動、設定手順の詳細は [Install and Manage Plugins](../../plugin-management.md#install-and-manage-plugins) を参照してください。`/api/v5/plugins*` 以下の REST API 操作は `system` スコープが必要です。ノードピン留めプラグインの場合、設定更新はサポートされますが、REST API によるライフサイクル管理はできません。

## API の呼び出し

EMQX API キーとシークレットを使った HTTP Basic 認証を使用します。

```bash
curl -u '<api_key>:<api_secret>' \
  -X POST 'http://127.0.0.1:18083/api/v5/plugin_api/emqx_bcast/pub' \
  -H 'Content-Type: application/json' \
  -d '{"Action":"PubBroadcast","ProductKey":"P1","MessageContent":"SGVsbG8="}'
```

ダッシュボードユーザーまたは API キーは `plugin_api` または `system` スコープを持ち、ロールとネームスペースの権限が要求された HTTP メソッドを許可している必要があります。プラグイン API ゲートウェイは `system` スコープを受け入れますが、Bcast ランタイム呼び出しには制限された `plugin_api` スコープを推奨します。`plugin_api` スコープはプラグインライフサイクルエンドポイント `/api/v5/plugins*` への認可を含みません。

`plugin_api` スコープはプラグイン API ゲートウェイ経由で公開されるすべてのエンドポイントへのアクセスを許可します。Bcast ではメッセージや配信削除、メトリクスリセットのエンドポイントも含まれるため、付与前にすべての公開操作を確認してください。

成功したリクエストは HTTP `200` を返します。

```json
{
  "Success": true,
  "RequestId": "550e8400-e29b-41d4-a716-446655440000",
  "MessageId": "550e8400-e29b-41d4-a716-446655440000"
}
```

エラー応答は以下の形式です。

```json
{
  "Success": false,
  "RequestId": "550e8400-e29b-41d4-a716-446655440000",
  "Code": "InvalidBase64",
  "ErrorMessage": "Invalid Base64 encoding"
}
```

よくあるエラーは以下の通りです。

| HTTP ステータス | コード | 原因 |
| --- | --- | --- |
| `400` | `InvalidProductKey`, `InvalidDeviceName`, `DuplicateDeviceName` | 製品またはデバイス識別子が欠落または無効、またはデバイスリストに重複がある。 |
| `400` | `DeviceCountExceeded`, `MessageTooLarge`, `InvalidBase64` | リクエストが設定上限を超過、または無効な base64 を含む。 |
| `400` | `InvalidTopicTemplate`, `InvalidQos` | トピックフィールドが無効、または `Qos` が `0` または `1` でない。 |
| `400` | `MessageNotFound`, `MessageIdContentConflict`, `InvalidMessageId` | 参照されたメッセージが存在しない、ペイロードフィールドが競合、または ID が無効。 |
| `429` | `QuotaExceeded` | グローバルまたはデバイスごとの保留中配信上限に達した。 |
| `429` | `Busy` | ローカルの QoS 1 受け入れキューが満杯。後で再試行。 |

## メッセージと配信の管理

管理エンドポイントはメタデータを公開し、メッセージペイロードの内容は返しません。

| メソッド | エンドポイント | 説明 |
| --- | --- | --- |
| `GET` | `/api/v5/plugin_api/emqx_bcast/messages?limit=100&cursor=<cursor>` | 登録済みメッセージの一覧。`limit` は 1 から 1000 の範囲。 |
| `GET` | `/api/v5/plugin_api/emqx_bcast/messages/:messageId` | メッセージのメタデータと未配信数を取得。 |
| `DELETE` | `/api/v5/plugin_api/emqx_bcast/messages/:messageId` | メッセージと関連する保留中配信を削除。 |
| `GET` | `/api/v5/plugin_api/emqx_bcast/deliveries?product_key=<key>&device_name=<name>` | 1 デバイスの保留中配信一覧。両方のクエリパラメータが必須。 |
| `GET` | `/api/v5/plugin_api/emqx_bcast/deliveries/:deliveryId` | 1 件の保留中配信を取得。 |
| `DELETE` | `/api/v5/plugin_api/emqx_bcast/deliveries/:deliveryId` | 1 件の保留中配信を削除。 |

削除はすでに送信中の配信を取り消せません。メッセージ削除はすべての稼働中コアノードの参加が必要です。コアノードが利用不可または非互換の場合、`500 InternalError` を返し、いかなるレコードも削除されません。

## Bcast の監視

プラグインは Prometheus テキスト形式のメトリクスを以下で公開します。

```http
GET /api/v5/plugin_api/emqx_bcast/metrics
```

メトリクスはノードローカルです。すべての EMQX ノードをスクレイプし、クラスター全体で集計してください。QoS 1 カウンターは最終的に以下の関係を満たします。

```text
wanted = acked + auto_acked + ttl_expired + canceled + queued + inflight
```

対応するメトリクス名は `bcast_batch_pub_qos1_` プレフィックスを使用します。ライブのバックログは `queued` と `inflight` ゲージを監視し、重複配信試行は `redelivered` カウンターを監視してください。

すべての稼働ノードでメトリクスをリセットするエンドポイントは以下です。

```http
POST /api/v5/plugin_api/emqx_bcast/metrics/reset
```

メンテナンスやテストウィンドウ中のみ使用してください。いずれかのノードにキューまたは送信中の配信がある場合、`409 PendingDeliveries` を返します。リセットに失敗し、一部のノードのみがリセットされた場合は `500 PartialReset` を返し、影響を受けたノードを特定します。

## 制限事項

- Bcast はリスナーのマウントポイントや `namespace_as_mountpoint` をサポートしません。これらの機能は保存されたサブスクリプションフィルターにプレフィックスを付与するため、Bcast のアンマウント配信トピックとマッチしなくなります。
- ローリングアップグレード中、プラグインが稼働していないノードに接続されたクライアントは保留中の Bcast 配信を受信できません。プラグインを起動し、`GET /api/v5/plugin_api/emqx_bcast/metrics` が `200` を返すことを確認してから、そのノードに MQTT または Bcast API トラフィックを送ってください。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリースの tarball:

| EMQX バージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.1.6 | 0.4.3 | [emqx_bcast-0.4.3.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.6/emqx_bcast-0.4.3.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.6/emqx_bcast-0.4.3.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
