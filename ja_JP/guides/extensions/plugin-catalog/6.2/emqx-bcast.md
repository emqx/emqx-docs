# デバイスメッセージブロードキャスト（Bcast）

`emqx_bcast` プラグインは、バックエンドアプリケーションがHTTPを介してIoTデバイスにコマンド、通知、その他のメッセージを配信できるようにします。製品全体へのブロードキャスト、ターゲットを絞ったバッチ配信、QoS 1のオフライン再生をサポートしているため、アプリケーションはMQTT接続を維持したり、一時的なオフライン配信キューやMQTTのPUBACK追跡システムを独自に構築する必要がありません。アプリケーションはペイロードを一度登録し、そのメッセージIDを後のバッチリクエストで再利用することも可能です。

このプラグインはEMQX Enterprise 6.2.4以降で利用可能です。

典型的なユースケースとしては、製品内のデバイスにサービス通知や設定更新信号をブロードキャストすること、選択したデバイスにコマンドや設定更新を送信すること、一時的にオフラインのデバイスが再接続した際にQoS 1のコマンドを配信することなどがあります。

## クイックスタート

この例では、製品 `P1` のオンラインデバイスに `Hello` をブロードキャストします。EMQX Enterprise 6.2.4以降が稼働しており、デバイス接続に [MQTTX CLI](https://mqttx.app/cli) が利用可能であることを前提としています。

### プラグインのインストールと起動

このクイックスタートではクラスター管理プラグインモードを使用します。[Download](#download) に記載のパッケージをダウンロードし、EMQXのプラグインディレクトリにコピーして、`<version>` をダウンロードしたプラグインのバージョンに置き換えます。

```bash
cp emqx_bcast-<version>.tar.gz "$EMQX_HOME/plugins/"
emqx ctl plugins install emqx_bcast-<version> --cluster
emqx ctl plugins start emqx_bcast-<version>
```

シングルノード展開の場合は `--cluster` を省略してください。プラグインはダッシュボードからもインストールおよび起動可能です。ダッシュボードでのインストールに必要な認可を含む両方の方法の詳細は [Install and Manage Plugins](../../plugin-management.md#install-and-manage-plugins) を参照してください。

### デバイス識別マッピングの設定

プラグインは各MQTTクライアントを以下の識別子にマッピングします：

- `ProductKey`：クライアントの `client_attrs.tns` ネームスペース属性
- `DeviceName`：MQTTクライアントID

ダッシュボードで **Management** -> **MQTT Settings** -> **Client Attributes** に移動し、属性 `tns` を追加し、その式を `nth(1, tokens(username, '-'))` に設定します。これは `ProductKey-DeviceName` 形式のユーザー名の最初のセグメントからネームスペースを導出する設定に相当します。

```hocon
mqtt.client_attrs_init = [
  { expression = "nth(1, tokens(username, '-'))", set_as_attr = tns }
]
```

この設定により、ユーザー名が `P1-device001`、クライアントIDが `device001` のクライアントは、`ProductKey` が `P1`、`DeviceName` が `device001` と認識されます。

クライアント属性の設定方法の詳細は [Set Client Attributes](../../../../develop/client-attributes/client-attributes.md#set-client-attributes) を参照してください。

設定適用後はクライアントを再接続し、EMQXがクライアント接続時に属性を初期化するようにしてください。

### APIキーの作成

ダッシュボードで **System** -> **API Keys** に移動し、**Administrator** ロールのAPIキーを作成します。**Namespace** は無効のままにしてグローバルキーを作成してください。**Custom Restricted Permissions** を選択し、`plugin_api` スコープを選択します。APIキーとシークレットはすぐに保存してください。詳細な手順は [Create API Keys](../../../api.md#create-api-keys) を参照してください。

`plugin_api` スコープは `/api/v5/plugin_api/emqx_bcast/*` 以下のBcastランタイムエンドポイントへの認可を付与します。プラグイン管理エンドポイント `/api/v5/plugins*` には `system` スコープが必要です。

### デバイスの接続とサブスクライブ

以下のコマンドを実行し、`device001` をユーザー名 `P1-device001` で接続し、製品 `P1` のデフォルトブロードキャストトピックにサブスクライブします：

```bash
mqttx sub -h 127.0.0.1 -p 1883 \
  -i device001 \
  -u P1-device001 \
  -t '/sys/broadcast/P1' \
  -q 1 -v
```

MQTTリスナーが認証を要求する場合は、対応するパスワードやTLSオプションを追加してください。デフォルトトピックで `BatchPub` メッセージを受信するには、代わりに `/P1/device001/user/get` にサブスクライブしてください。

対象クライアントは配信トピックにサブスクライブしている必要があります。クライアントが接続しているだけではプラグインはメッセージを配信しません。

### ブロードキャストの送信と確認

MQTTXクライアントを起動したまま、別のターミナルで `<api_key>` と `<api_secret>` を先ほど作成した認証情報に置き換えて以下を実行します：

```bash
curl -u '<api_key>:<api_secret>' \
  -X POST 'http://127.0.0.1:18083/api/v5/plugin_api/emqx_bcast/pub' \
  -H 'Content-Type: application/json' \
  -d '{"Action":"PubBroadcast","ProductKey":"P1","MessageContent":"SGVsbG8="}'
```

`MessageContent` はbase64エンコード済みである必要があります。この例の `SGVsbG8=` は `Hello` にデコードされます。

EMQXはHTTP `200` を返し、以下のようなレスポンスを返します：

```json
{
  "Success": true,
  "RequestId": "550e8400-e29b-41d4-a716-446655440000",
  "MessageId": "550e8400-e29b-41d4-a716-446655440000"
}
```

MQTTXクライアントはトピック `/sys/broadcast/P1` でペイロード `Hello` を受信します。

HTTP `200` はEMQXがリクエストを受理したことを示します。配信が確実に行われたことを意味しません。各QoSレベルの配信保証については [Delivery Semantics](#delivery-semantics) を参照してください。

::: warning 重要なお知らせ

Bcastはメッセージをクライアントチャネルに直接送信します。これらのメッセージはパブリッシュ認可チェックや通常のMQTTパブリッシュパイプライン（ルールエンジン、スキーマ検証、メッセージ変換、保持メッセージ、遅延パブリッシュを含む）をバイパスします。

Bcast APIへのアクセスは信頼できる呼び出し元にのみ許可し、呼び出しアプリケーション側でペイロードやターゲット識別子を検証してください。

:::

## メッセージアクション

すべてのメッセージアクションは以下のエンドポイントを使用します：

```http
POST /api/v5/plugin_api/emqx_bcast/pub
```

JSONリクエストボディの `Action` フィールドで操作を指定します。`MessageContent` はbase64エンコード済みである必要があります。

### オンラインデバイスへのブロードキャスト

`PubBroadcast` は製品内の配信トピックにサブスクライブしているすべてのオンラインサブスクライバーにメッセージを送信します。メッセージは保存されないため、オフラインデバイスは受信しません。

```json
{
  "Action": "PubBroadcast",
  "ProductKey": "P1",
  "MessageContent": "SGVsbG8="
}
```

プラグインはデフォルト値 `/sys/broadcast/${productKey}` の `broadcast_topic` テンプレートを展開します。`TopicFullName` に具体的なカスタムトピックを設定することも可能です。`TopicFullName` にはワイルドカードやテンプレートプレースホルダーは使用できません。

デフォルトの最大デコード済みペイロードサイズは64KiBです。

### デバイスリストへの送信

`BatchPub` は明示的なデバイスリストにメッセージを送信します。リクエストは最大 `max_device_count` 個のユニークなデバイス名を指定可能です。デフォルトは10,000です。

```json
{
  "Action": "BatchPub",
  "ProductKey": "P1",
  "DeviceName": ["device001", "device002"],
  "MessageContent": "SGVsbG8=",
  "Qos": 1
}
```

以下のペイロードフィールドのうちいずれか一方を指定してください：

- `MessageContent`：インラインのbase64エンコード済みペイロード。デフォルトの最大デコード済みペイロードサイズは10KiBです。
- `MessageId`：`RegisterMessage` または以前のQoS 1バッチリクエストで作成されたペイロードのID。

`BatchPub` の配信トピックは以下の順で選択されます：

1. `TopicTemplateName`（`${productKey}` と `${deviceName}` を展開）
2. `TopicShortName`（`/${productKey}/${deviceName}/user/${TopicShortName}` に展開）
3. 設定済みの `batch_topic`（デフォルトは `/${productKey}/${deviceName}/user/get`）

### メッセージの登録と再利用

`RegisterMessage` は後の `BatchPub` リクエストで再利用可能なペイロードを保存します：

```json
{
  "Action": "RegisterMessage",
  "MessageContent": "SGVsbG8="
}
```

レスポンスに `MessageId` が含まれます。同一内容の登録は同じIDを返し、TTLを更新します。保存されたペイロードは不変です。

既存メッセージのTTLを更新するには、ペイロードの代わりにIDを送信してください：

```json
{
  "Action": "RegisterMessage",
  "MessageId": "550e8400-e29b-41d4-a716-446655440000"
}
```

`MessageContent` と `MessageId` は相互排他的です。

## 配信セマンティクス

`BatchPub` の `Qos` フィールドは `0` または `1` を受け付け、デフォルトは `0` です。

| 要求されたQoS | オンラインデバイス | オフラインデバイス | 保存と確認 |
| --- | --- | --- | --- |
| `0` | ターゲットがオンラインかつサブスクライブしている場合に配信 | スキップ | 保存やアック追跡なし |
| `1` | サブスクライブQoSが1の場合に配信・PUBACKまで追跡 | デバイス再接続後に保存・再生 | 保留中配信は `msg_ttl`（デフォルト15日）後に期限切れ |

実際の配信QoSは要求QoSとターゲットのサブスクライブQoSの低い方です。QoS 1リクエストがQoS 0サブスクライブに到達した場合、プラグインはクライアントチャネルに渡した時点で配信完了とみなします。

PUBACKはMQTTプロトコルレベルでの受信確認であり、デバイスアプリケーションがメッセージを処理したりコマンドを実行したことを保証しません。

QoS 1のバッチ配信は非同期かつ少なくとも一回配信です。クライアントは特に再接続やセッション引き継ぎ時の重複メッセージに耐えられる必要があります。HTTP `200` の成功応答はEMQXが非同期処理のためにリクエストを受理したことを示しますが、メッセージが保存または配信されたことを意味しません。受理ノードが保存完了前に障害を起こすとリクエストが失われる可能性があります。

保留中のQoS 1状態はコアノード間でメモリ内に複製されます。1台のコアノード障害は耐えられますが、クラスター全体の再起動には耐えられません。プロデューサーは完全なクラスター再起動後にメッセージを再送できる必要があります。

QoS 0メッセージもセッション引き継ぎウィンドウ中に重複する可能性があります。

## プラグインの設定

以下の表は有効な設定項目とデフォルト値を示します：

| 設定項目 | デフォルト | 説明 |
| --- | --- | --- |
| `broadcast_topic` | `/sys/broadcast/${productKey}` | `PubBroadcast` 用のトピックテンプレート |
| `batch_topic` | `/${productKey}/${deviceName}/user/get` | `BatchPub` のデフォルトトピックテンプレート |
| `msg_ttl` | `15d` | 登録メッセージと保留中QoS 1配信のTTL。変更は更新後に作成されたレコードにのみ適用 |
| `cleanup_interval` | `60s` | 期限切れレコード削除の間隔 |
| `max_device_count` | `10000` | 1回の `BatchPub` リクエストで指定可能な最大デバイス数 |
| `max_message_size_broadcast` | `65536` | ブロードキャストペイロードの最大デコード済みサイズ（バイト） |
| `max_message_size_batch` | `10240` | バッチペイロードの最大デコード済みサイズ（バイト） |
| `max_pending_deliveries` | `10000000` | クラスター全体の保留中QoS 1配信の上限 |
| `max_pending_deliveries_per_device` | `100` | デバイスごとの保留中QoS 1配信の上限。値は10～200の範囲に制限。起動時やフェイルオーバー時にベストエフォートで適用 |
| `delivery_pool_size` | `0` | 配信プールごとのワーカー数。`0` はErlangスケジューラ数に合わせて自動設定。変更時にプールが再起動 |

設定例：

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

これらの設定は標準のプラグイン設定APIを使って更新します：

```http
PUT /api/v5/plugins/<name-vsn>/config
```

インストール、起動、設定手順の詳細は [Install and Manage Plugins](../../plugin-management.md#install-and-manage-plugins) を参照してください。`/api/v5/plugins*` 以下のREST API操作には `system` スコープが必要です。ノードピン留めプラグインの場合、設定更新は可能ですがREST APIでライフサイクル管理はできません。

## APIの呼び出し

EMQXのAPIキーとシークレットを使ったHTTP Basic認証を利用します：

```bash
curl -u '<api_key>:<api_secret>' \
  -X POST 'http://127.0.0.1:18083/api/v5/plugin_api/emqx_bcast/pub' \
  -H 'Content-Type: application/json' \
  -d '{"Action":"PubBroadcast","ProductKey":"P1","MessageContent":"SGVsbG8="}'
```

ダッシュボードユーザーまたはAPIキーは `plugin_api` または `system` スコープを持ち、かつロールとネームスペースの権限が要求されたHTTPメソッドを許可している必要があります。プラグインAPIゲートウェイは `system` スコープを受け入れますが、Bcastのランタイム呼び出しには制限付きの `plugin_api` スコープを推奨します。`plugin_api` スコープは `/api/v5/plugins*` 以下のプラグインライフサイクルエンドポイントを認可しません。

`plugin_api` スコープはプラグインAPIゲートウェイ経由で公開されるすべてのエンドポイントへのアクセスを付与します。Bcastの場合、メッセージや配信の削除、メトリクスリセットエンドポイントも含まれるため、付与前にすべての公開操作を確認してください。

成功したリクエストはHTTP `200` を返します：

```json
{
  "Success": true,
  "RequestId": "550e8400-e29b-41d4-a716-446655440000",
  "MessageId": "550e8400-e29b-41d4-a716-446655440000"
}
```

エラー応答は以下の形式を取ります：

```json
{
  "Success": false,
  "RequestId": "550e8400-e29b-41d4-a716-446655440000",
  "Code": "InvalidBase64",
  "ErrorMessage": "Invalid Base64 encoding"
}
```

よくあるエラー：

| HTTPステータス | コード | 原因 |
| --- | --- | --- |
| `400` | `InvalidProductKey`, `InvalidDeviceName`, `DuplicateDeviceName` | 製品またはデバイス識別子が欠落または無効、またはデバイスリストに重複がある |
| `400` | `DeviceCountExceeded`, `MessageTooLarge`, `InvalidBase64` | リクエストが設定上限を超過、またはbase64が無効 |
| `400` | `InvalidTopicTemplate`, `InvalidQos` | トピックフィールドが無効、または `Qos` が `0` または `1` でない |
| `400` | `MessageNotFound`, `MessageIdContentConflict`, `InvalidMessageId` | 参照メッセージが存在しない、ペイロードフィールドが競合、またはIDが無効 |
| `429` | `QuotaExceeded` | グローバルまたはデバイスごとの保留中配信上限に達した |
| `429` | `Busy` | ローカルQoS 1取り込みキューが満杯。後で再試行 |

## メッセージと配信の管理

管理エンドポイントはメタデータを公開し、メッセージペイロードの内容は返しません。

| メソッド | エンドポイント | 説明 |
| --- | --- | --- |
| `GET` | `/api/v5/plugin_api/emqx_bcast/messages?limit=100&cursor=<cursor>` | 登録済みメッセージの一覧。`limit` は1～1000の範囲で指定 |
| `GET` | `/api/v5/plugin_api/emqx_bcast/messages/:messageId` | メッセージメタデータと未配信数の取得 |
| `DELETE` | `/api/v5/plugin_api/emqx_bcast/messages/:messageId` | メッセージと関連する保留中配信の削除 |
| `GET` | `/api/v5/plugin_api/emqx_bcast/deliveries?product_key=<key>&device_name=<name>` | 1デバイスの保留中配信一覧。両クエリパラメータ必須 |
| `GET` | `/api/v5/plugin_api/emqx_bcast/deliveries/:deliveryId` | 1件の保留中配信取得 |
| `DELETE` | `/api/v5/plugin_api/emqx_bcast/deliveries/:deliveryId` | 1件の保留中配信削除 |

削除はすでに配信中の配信を取り消せません。メッセージ削除はすべての稼働中コアノードの参加が必要です。コアが利用不可または非互換の場合、`500 InternalError` を返し、レコードは削除されません。

## Bcastの監視

プラグインはPrometheus形式のテキストメトリクスを以下で公開します：

```http
GET /api/v5/plugin_api/emqx_bcast/metrics
```

メトリクスはノードローカルです。すべてのEMQXノードをスクレイプし、クラスター全体で集計してください。QoS 1カウンターは以下の関係を満たします：

```text
wanted = acked + auto_acked + ttl_expired + canceled + queued + inflight
```

対応するメトリクス名は `bcast_batch_pub_qos1_` プレフィックスを使用します。`queued` と `inflight` ゲージはライブのバックログを示し、`redelivered` カウンターは重複配信試行を示します。

すべての稼働ノードでメトリクスをリセットするエンドポイント：

```http
POST /api/v5/plugin_api/emqx_bcast/metrics/reset
```

メンテナンスやテストウィンドウ時のみ使用してください。いずれかのノードに保留中または配信中の配信がある場合は `409 PendingDeliveries` を返します。リセットに失敗し一部ノードのみリセットされた場合は `500 PartialReset` を返し、影響を受けたノードを特定します。

## 制限事項

- Bcastはリスナーマウントポイントや `namespace_as_mountpoint` をサポートしません。これらは保存されたサブスクリプションフィルターにプレフィックスを付与するため、Bcastのマウントされていない配信トピックと一致しません。
- ローリングアップグレード中、プラグインが稼働していないノードに接続しているクライアントは保留中のBcast配信を受信できません。プラグインを起動し、`GET /api/v5/plugin_api/emqx_bcast/metrics` が `200` を返すことを確認してから、そのノードにMQTTまたはBcast APIトラフィックを送信してください。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリース用のtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.2.4 | 0.4.4 | [emqx_bcast-0.4.4.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.4/emqx_bcast-0.4.4.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.4/emqx_bcast-0.4.4.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
