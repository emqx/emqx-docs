# デバイスメッセージブロードキャスト（Bcast）

`emqx_bcast` プラグインは、バックエンドアプリケーションがHTTP経由でIoTデバイスにコマンド、通知、その他のメッセージを配信できるようにします。製品全体へのブロードキャスト、ターゲットを絞ったバッチ配信、QoS 1のオフライン再生をサポートしており、アプリケーションはMQTT接続を維持したり、一時的なオフライン配信キューやMQTT PUBACKトラッキングシステムを独自に構築する必要がありません。アプリケーションはペイロードを一度登録し、そのメッセージIDを後のバッチリクエストで再利用することも可能です。

このプラグインはEMQX Enterprise 6.3.3以降で利用可能です。

典型的なユースケースは、製品内のデバイスにサービス通知や設定更新信号をブロードキャストしたり、選択したデバイスにコマンドや設定更新を送信したり、一時的にオフラインだったデバイスが再接続した際にQoS 1コマンドを配信することです。

## クイックスタート

この例では、製品 `P1` のオンラインデバイスに `Hello` をブロードキャストします。EMQX Enterprise 6.3.3以降が稼働しており、[MQTTX CLI](https://mqttx.app/cli) がデバイス接続に利用可能であることを前提としています。

### プラグインのインストールと起動

このクイックスタートではクラスター管理プラグインモードを使用します。[Download](#download) に記載のパッケージをダウンロードし、EMQXのプラグインディレクトリにコピーし、`<version>` をダウンロードしたプラグインのバージョンに置き換えます。

```bash
cp emqx_bcast-<version>.tar.gz "$EMQX_HOME/plugins/"
emqx ctl plugins install emqx_bcast-<version> --cluster
emqx ctl plugins start emqx_bcast-<version>
```

シングルノード構成の場合は `--cluster` を省略してください。プラグインのインストールと起動はダッシュボードからも可能です。ダッシュボードでのインストールに必要な認可を含む両手順の詳細は、[Install and Manage Plugins](../../plugin-management.md#install-and-manage-plugins) を参照してください。

### デバイス識別マッピングの設定

プラグインは各MQTTクライアントを以下の識別子にマッピングします。

- `ProductKey`：クライアントの `client_attrs.tns` ネームスペース属性
- `DeviceName`：MQTTクライアントID

ダッシュボードで **Management** -> **MQTT Settings** -> **Client Attributes** に移動し、属性 `tns` を追加し、式を `nth(1, tokens(username, '-'))` に設定します。これは `ProductKey-DeviceName` 形式のユーザー名の最初のセグメントからネームスペースを導出する設定に相当します。

```hocon
mqtt.client_attrs_init = [
  { expression = "nth(1, tokens(username, '-'))", set_as_attr = tns }
]
```

この設定により、ユーザー名 `P1-device001`、クライアントID `device001` で接続したクライアントは、`ProductKey` が `P1`、`DeviceName` が `device001` となります。

クライアント属性の設定方法の詳細は、[Set Client Attributes](../../../../develop/client-attributes/client-attributes.md#set-client-attributes) を参照してください。

設定適用後はクライアントを再接続し、EMQXが接続時に属性を初期化するようにしてください。

### APIキーの作成

ダッシュボードで **System** -> **API Keys** に移動し、**Administrator** ロールでAPIキーを作成します。**Namespace** は無効のままにしてグローバルキーを作成してください。**Custom Restricted Permissions** を選択し、`plugin_api` スコープを選択します。APIキーとシークレットは作成後すぐに保存してください。詳細な手順は [Create API Keys](../../../api.md#create-api-keys) を参照してください。

`plugin_api` スコープは `/api/v5/plugin_api/emqx_bcast/*` 以下のBcastランタイムエンドポイントへの認可を付与します。`/api/v5/plugins*` 以下のプラグイン管理エンドポイントは認可しません。これらは `system` スコープを必要とします。

### デバイスの接続とサブスクライブ

以下のコマンドを実行し、`device001` をユーザー名 `P1-device001` で接続し、製品 `P1` のデフォルトブロードキャストトピックをサブスクライブします。

```bash
mqttx sub -h 127.0.0.1 -p 1883 \
  -i device001 \
  -u P1-device001 \
  -t '/sys/broadcast/P1' \
  -q 1 -v
```

MQTTリスナーが認証を要求する場合は、対応するパスワードやTLSオプションを追加してください。デフォルトトピックで `BatchPub` メッセージを受信するには、代わりに `/P1/device001/user/get` をサブスクライブしてください。

対象クライアントは配信トピックをサブスクライブしている必要があります。クライアントが接続しているだけではプラグインはメッセージを配信しません。

### ブロードキャストの送信と検証

MQTTXクライアントを起動したまま、別のターミナルで `<api_key>` と `<api_secret>` を前のステップで作成した認証情報に置き換え、以下を実行します。

```bash
curl -u '<api_key>:<api_secret>' \
  -X POST 'http://127.0.0.1:18083/api/v5/plugin_api/emqx_bcast/pub' \
  -H 'Content-Type: application/json' \
  -d '{"Action":"PubBroadcast","ProductKey":"P1","MessageContent":"SGVsbG8="}'
```

`MessageContent` はBase64エンコード済みでなければなりません。この例では `SGVsbG8=` は `Hello` にデコードされます。

EMQXはHTTP `200` を返し、以下のようなレスポンスを返します。

```json
{
  "Success": true,
  "RequestId": "550e8400-e29b-41d4-a716-446655440000",
  "MessageId": "550e8400-e29b-41d4-a716-446655440000"
}
```

MQTTXクライアントはトピック `/sys/broadcast/P1` でペイロード `Hello` を受信します。

HTTP `200` はEMQXがリクエストを受理したことを示します。配信の確約ではありません。各QoSレベルの配信保証については [Delivery Semantics](#delivery-semantics) を参照してください。

::: warning 重要なお知らせ

Bcastはメッセージをクライアントチャネルに直接送信します。これらのメッセージはパブリッシュ認可チェックや通常のMQTTパブリッシュパイプライン（ルールエンジン、スキーマ検証、メッセージ変換、保持メッセージ、遅延パブリッシュを含む）をバイパスします。

Bcast APIへのアクセスは信頼できる呼び出し元のみに限定し、呼び出しアプリケーション側でペイロードとターゲット識別子の検証を行ってください。

:::

## メッセージアクション

すべてのメッセージアクションは以下のエンドポイントを使用します。

```http
POST /api/v5/plugin_api/emqx_bcast/pub
```

JSONリクエストボディの `Action` フィールドで操作を選択します。`MessageContent` はBase64エンコード済みである必要があります。

### オンラインデバイスへのブロードキャスト

`PubBroadcast` は、製品内の配信トピックをサブスクライブしているすべてのオンラインサブスクライバーにメッセージを送信します。メッセージは保存されないため、オフラインのデバイスには届きません。

```json
{
  "Action": "PubBroadcast",
  "ProductKey": "P1",
  "MessageContent": "SGVsbG8="
}
```

プラグインは設定された `broadcast_topic` テンプレート（デフォルトは `/sys/broadcast/${productKey}`）を展開します。`TopicFullName` に具体的なカスタムトピックを設定することも可能です。`TopicFullName` にはワイルドカードやテンプレートプレースホルダーは使用できません。

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

以下のペイロードフィールドのうち、どちらか一方を正確に指定してください。

- `MessageContent`：インラインのBase64エンコード済みペイロード。デフォルトの最大デコード済みペイロードサイズは10KiBです。
- `MessageId`：`RegisterMessage` または以前のQoS 1バッチリクエストで作成されたペイロードのID。

`BatchPub` の配信トピックは以下の順で選択されます。

1. `TopicTemplateName`（`${productKey}` と `${deviceName}` を展開）
2. `TopicShortName`（`/${productKey}/${deviceName}/user/${TopicShortName}` に展開）
3. 設定された `batch_topic`（デフォルトは `/${productKey}/${deviceName}/user/get`）

### メッセージの登録と再利用

`RegisterMessage` は後続の `BatchPub` リクエストで再利用可能なペイロードを保存します。

```json
{
  "Action": "RegisterMessage",
  "MessageContent": "SGVsbG8="
}
```

レスポンスには `MessageId` が含まれます。同一内容の登録は同じIDを返し、TTLを更新します。保存されたペイロードは不変です。

既存メッセージのTTLを更新するには、内容ではなくIDを送信してください。

```json
{
  "Action": "RegisterMessage",
  "MessageId": "550e8400-e29b-41d4-a716-446655440000"
}
```

`MessageContent` と `MessageId` は相互排他的です。

## 配信セマンティクス

`BatchPub` の `Qos` フィールドは `0` または `1` を受け付け、デフォルトは `0` です。

| 要求QoS | オンラインデバイス | オフラインデバイス | 保存と確認 |
| --- | --- | --- | --- |
| `0` | 対象がオンラインかつサブスクライブ中の場合に配信 | スキップ | 保存もアック追跡もなし |
| `1` | サブスクライブQoSが1の場合に配信し、PUBACKまで追跡 | デバイス再接続後に保存・再生 | 保留中配信は `msg_ttl`（デフォルト15日）後に期限切れ |

実際の配信QoSは要求QoSと対象サブスクライブQoSの低い方です。QoS 1リクエストがQoS 0サブスクライブに到達した場合、プラグインはクライアントチャネルに渡した時点で配信完了とみなします。

PUBACKはMQTTプロトコルレベルでの受信確認であり、デバイスアプリケーションがメッセージを処理したりコマンドを実行したことを保証しません。

QoS 1バッチ配信は非同期かつ少なくとも一回配信です。クライアントは特に再接続やセッション引き継ぎ時に重複メッセージを許容する必要があります。HTTP `200` の成功応答はEMQXが非同期処理のためリクエストを受理したことを示しますが、メッセージが保存または配信されたことを意味しません。受理ノードが保存完了前に障害発生した場合、リクエストは失われる可能性があります。

保留中のQoS 1状態はコアノード間でメモリ上にレプリケートされます。1台のコアノード障害には耐えられますが、クラスター全体の再起動には耐えられません。プロデューサーはクラスター再起動後にメッセージを再送できる必要があります。

QoS 0メッセージもセッション引き継ぎウィンドウ中に重複する可能性があります。

## プラグインの設定

以下の表は有効な設定とそのデフォルト値を示します。

| 設定 | デフォルト | 説明 |
| --- | --- | --- |
| `broadcast_topic` | `/sys/broadcast/${productKey}` | `PubBroadcast` 用のトピックテンプレート |
| `batch_topic` | `/${productKey}/${deviceName}/user/get` | `BatchPub` のデフォルトトピックテンプレート |
| `msg_ttl` | `15d` | 登録メッセージと保留中QoS 1配信のTTL。更新は更新後に作成されたレコードにのみ適用 |
| `cleanup_interval` | `60s` | 期限切れレコード削除の間隔 |
| `max_device_count` | `10000` | 1回の `BatchPub` リクエストで指定可能な最大デバイス数 |
| `max_message_size_broadcast` | `65536` | ブロードキャストの最大デコード済みペイロードサイズ（バイト） |
| `max_message_size_batch` | `10240` | バッチの最大デコード済みペイロードサイズ（バイト） |
| `max_pending_deliveries` | `10000000` | クラスター全体の保留中QoS 1配信の上限 |
| `max_pending_deliveries_per_device` | `100` | デバイスごとの保留中QoS 1配信の上限。値は10〜200にクランプ。起動時やフェイルオーバー時にベストエフォートで適用 |
| `delivery_pool_size` | `0` | 配信プールごとのワーカー数。`0` はErlangスケジューラ数に自動設定。変更時はプールが再起動 |

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

これらの設定は標準プラグイン設定APIで更新可能です。

```http
PUT /api/v5/plugins/<name-vsn>/config
```

インストール、起動、設定手順の詳細は [Install and Manage Plugins](../../plugin-management.md#install-and-manage-plugins) を参照してください。`/api/v5/plugins*` 以下のREST API操作は `system` スコープを必要とします。ノードピン留めプラグインの場合、設定更新はサポートされますがREST APIでのライフサイクル管理はできません。

## APIの呼び出し

EMQX APIキーとシークレットを用いたHTTP Basic認証を使用します。

```bash
curl -u '<api_key>:<api_secret>' \
  -X POST 'http://127.0.0.1:18083/api/v5/plugin_api/emqx_bcast/pub' \
  -H 'Content-Type: application/json' \
  -d '{"Action":"PubBroadcast","ProductKey":"P1","MessageContent":"SGVsbG8="}'
```

ダッシュボードユーザーまたはAPIキーは `plugin_api` または `system` スコープを持ち、ロールとネームスペースの権限が要求HTTPメソッドを許可している必要があります。プラグインAPIゲートウェイは `system` を受け入れますが、Bcastランタイム呼び出しには制限された `plugin_api` スコープを推奨します。`plugin_api` スコープはプラグインライフサイクルエンドポイント `/api/v5/plugins*` を認可しません。

`plugin_api` スコープはプラグインAPIゲートウェイで公開されるすべてのエンドポイントへのアクセスを許可します。Bcastではメッセージ・配信削除やメトリクスリセットエンドポイントも含まれるため、付与前に公開される操作を十分に確認してください。

成功したリクエストはHTTP `200` を返します。

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

主なエラー例：

| HTTPステータス | コード | 原因 |
| --- | --- | --- |
| `400` | `InvalidProductKey`, `InvalidDeviceName`, `DuplicateDeviceName` | 製品またはデバイス識別子が欠落または無効、またはデバイスリストに重複あり |
| `400` | `DeviceCountExceeded`, `MessageTooLarge`, `InvalidBase64` | リクエストが設定上限を超過、または無効なBase64 |
| `400` | `InvalidTopicTemplate`, `InvalidQos` | トピックフィールドが無効、または `Qos` が `0` または `1` でない |
| `400` | `MessageNotFound`, `MessageIdContentConflict`, `InvalidMessageId` | 参照メッセージが存在しない、ペイロードフィールドが競合、またはIDが無効 |
| `429` | `QuotaExceeded` | グローバルまたはデバイスごとの保留配信上限に達した |
| `429` | `Busy` | ローカルQoS 1受け入れキューが満杯。後で再試行 |

## メッセージと配信の管理

管理エンドポイントはメタデータを公開し、メッセージペイロードの内容は返しません。

| メソッド | エンドポイント | 説明 |
| --- | --- | --- |
| `GET` | `/api/v5/plugin_api/emqx_bcast/messages?limit=100&cursor=<cursor>` | 登録済みメッセージの一覧。`limit` は1〜1000の範囲。 |
| `GET` | `/api/v5/plugin_api/emqx_bcast/messages/:messageId` | メッセージメタデータと未配信数を取得。 |
| `DELETE` | `/api/v5/plugin_api/emqx_bcast/messages/:messageId` | メッセージと関連する保留中配信を削除。 |
| `GET` | `/api/v5/plugin_api/emqx_bcast/deliveries?product_key=<key>&device_name=<name>` | 1デバイスの保留中配信一覧。両クエリパラメータ必須。 |
| `GET` | `/api/v5/plugin_api/emqx_bcast/deliveries/:deliveryId` | 1件の保留中配信を取得。 |
| `DELETE` | `/api/v5/plugin_api/emqx_bcast/deliveries/:deliveryId` | 1件の保留中配信を削除。 |

削除はすでに配信中の配信を取り消しません。メッセージ削除はすべての稼働中コアノードの参加が必要です。コアノードが利用不可または互換性がない場合、リクエストは `500 InternalError` を返し、いかなるレコードも削除しません。

## Bcastのモニタリング

プラグインはPrometheusテキスト形式のメトリクスを以下で公開します。

```http
GET /api/v5/plugin_api/emqx_bcast/metrics
```

メトリクスはノードローカルです。すべてのEMQXノードをスクレイプし、クラスター全体で集計してください。QoS 1カウンターは最終的に以下の関係を満たします。

```text
wanted = acked + auto_acked + ttl_expired + canceled + queued + inflight
```

対応するメトリクス名は `bcast_batch_pub_qos1_` プレフィックスを使用します。`queued` と `inflight` ゲージはライブのバックログを示し、`redelivered` カウンターは重複配信試行を示します。

以下のエンドポイントは稼働中のすべてのノードでメトリクスをリセットします。

```http
POST /api/v5/plugin_api/emqx_bcast/metrics/reset
```

メンテナンスやテストウィンドウ中のみ使用してください。いずれかのノードにキューまたは配信中の配信がある場合は `409 PendingDeliveries` を返します。リセットが一部のノードで成功し他で失敗した場合は `500 PartialReset` を返し、影響を受けたノードを特定します。

## 制限事項

- Bcastはリスナーのマウントポイントや `namespace_as_mountpoint` をサポートしません。これらの機能は保存されたサブスクリプションフィルターにプレフィックスを付与し、Bcastのマウントされていない配信トピックとマッチしなくなります。
- ローリングアップグレード中、プラグインが稼働していないノードに接続しているクライアントは保留中のBcast配信を受信できません。プラグインを起動し、`GET /api/v5/plugin_api/emqx_bcast/metrics` が `200` を返すことを確認してから、そのノードにMQTTまたはBcast APIトラフィックを送ってください。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリース向けのtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.3.3 | 0.4.4 | [emqx_bcast-0.4.4.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.3/emqx_bcast-0.4.4.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.3/emqx_bcast-0.4.4.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
