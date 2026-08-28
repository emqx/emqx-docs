# REST APIベースの MQTT 5.0 SCRAM 認証

EMQX は REST API を使用した MQTT 5.0 強化認証をサポートしており、[Salted Challenge Response Authentication Mechanism (SCRAM)](https://en.wikipedia.org/wiki/Salted_Challenge_Response_Authentication_Mechanism) を実装しています。この SCRAM 認証機構は、外部のウェブリソースから認証に必要なデータを取得する仕組みを採用しています。有効化されている場合、クライアントが SCRAM で接続要求を開始すると、EMQX は提供されたユーザー名を使って外部サービスへ HTTP リクエストを構築し、認証プロセスに必要な認証データを取得します。

SCRAM は本来軽量でシンプルな認証方式ですが、本実装では外部 REST API と連携することで機能を拡張しています。これにより、EMQX は様々な外部システムから安全かつ効率的に認証データを取得でき、より複雑な認証シナリオに対応可能となっています。

::: tip 前提条件

- [EMQX の基本的な認証概念](./authn.md)に関する理解
- SCRAM 認証機構は MQTT 5.0 接続のみ対応
- 本認証機構は RFC 7804: [Salted Challenge Response HTTP Authentication Mechanism](https://datatracker.ietf.org/doc/html/rfc7804) の実装ではありません

:::

## HTTP リクエストとレスポンス

認証プロセスは HTTP API コールに類似しています。EMQX はクライアントとして動作し、外部 HTTP サービスへ HTTP リクエストを構築・送信します。サービスは `username` に対応する認証データを含むレスポンスを返します。

### レスポンス形式の要件

認証を正常に行うため、HTTP レスポンスは以下の条件を満たす必要があります。

- **Content-Type**: レスポンスは `application/json` でエンコードされていること
- **認証データ**: `stored_key`、`server_key`、`salt` を含み、すべて16進数でエンコードされていること
- **スーパーユーザー指標**: `is_superuser` フィールドを使用し、値は `true` または `false`
- **クライアント属性**: 任意で `client_attrs` フィールドを指定可能。キーと値は両方とも文字列である必要があります。詳細は[クライアント属性](../../client-attributes/client-attributes.md)を参照してください
- **アクセス制御リスト (ACL)**: 任意で `acl` フィールドを含めてクライアントの権限を定義可能です。詳細は[アクセス制御リスト](./jwt.md#access-control-list-optional)を参照してください
- **有効期限**: 任意で `expire_at` フィールドを設定可能。クライアントの認証有効期限を Unix タイムスタンプ（秒単位）で指定し、期限切れ後はクライアントは切断して再認証が必要です
- **HTTP ステータスコード**: HTTP レスポンスは `200 OK` を返す必要があります。`4xx` または `5xx` のステータスコードは `ignore` と解釈され、この認証機構をスキップして認証チェーンが続行されます

### HTTP レスポンス例

以下は期待される HTTP レスポンスの構造と内容の例です。

```json
HTTP/1.1 200 OK
Headers: Content-Type: application/json
...
Body:
{
    "stored_key": "008F5E0CC6316BB172F511E93E4756EEA876B5B5125F1CD2FD69A2C30F9A0D73",
    "server_key": "81466E185EC642AFAE1EFA75953735D6C0934D099149AAAB601D59F8F8162580",
    "salt": "6633653634383437393466356532333165656435346432393464366165393137",
    "is_superuser": true, // オプション: true | false、デフォルトは false
    "client_attrs": { // オプション
        "role": "admin",
        "sn": "10c61f1a1f47"
    },
    "expire_at": 1654254601, // オプション
    "acl": // オプション
    [
        {
            "permission": "allow",
            "action": "subscribe",
            "topic": "eq t/1/#",
            "qos": [1]
        },
        {
            "permission": "deny",
            "action": "all",
            "topic": "t/3"
        }
    ]
}
```

## ダッシュボードでの認証機構設定

EMQX ダッシュボードから SCRAM 認証機構を設定できます。

1. EMQX ダッシュボードにログインします。

2. 左のナビゲーションメニューで **Access Control** -> **Authentication** をクリックし、**Authentication** ページを開きます。

3. 右上の **Create** をクリックします。

4. **Mechanism** に **SCRAM** を、**Backend** に **HTTP Server** を選択します。**Next** をクリックすると、以下のような **Configuration** ステップの画面に進みます。

   ![authn-scram-http](./assets/authn-scram-restapi.png)

5. バックエンドの設定を以下のように行います。

   - **Method**: HTTP リクエストメソッドを選択します（`GET` または `POST`）。

     ::: tip

     `POST` メソッドは、パスワードなどの機密情報がサーバーログに露出しないよう推奨されます。信頼できない環境では HTTPS を使用してください。

     :::

   - **URL**: HTTP サービスの URL を入力します。

   - **Precondition**: [Variform 式](../../configuration/configuration.md#variform-expressions)を使用して、この HTTP Server 認証機構をクライアント接続に適用するか制御します。式はクライアントの属性（`username`、`clientid`、`listener` など）に対して評価され、結果が文字列 `"true"` の場合のみ認証機構が呼び出されます。そうでなければスキップされます。詳細は[認証の前提条件](./authn.md#authentication-preconditions)を参照してください。

   - **Headers**（任意）: 追加の HTTP リクエストヘッダーを指定します。

   - **Authentication Configuration**:

     - **Password Hash**: パスワードハッシュアルゴリズムを選択します（`sha256` または `sha512`）。
     - **Enable TLS**: スイッチを切り替えて TLS を有効化します。TLS 有効化の詳細は[外部リソースアクセスの TLS](../../network/overview.md#tls-for-external-resource-access)を参照してください。
     - **Body**: リクエストテンプレートを定義します。`POST` リクエストの場合は JSON 形式でリクエストボディに送信され、`GET` リクエストの場合は URL のクエリ文字列としてエンコードされます。[プレースホルダー](./authn.md#authentication-placeholders)を使ってキーと値をマッピングしてください。

   - **Advanced Settings**:

     - **Connection Pool size**（任意）: EMQX ノードから HTTP サーバーへの同時接続数（整数値）を設定します。デフォルトは `8`。
     - **Connect Timeout**（任意）: EMQX が接続タイムアウトと判断するまでの待機時間を指定します。サポートされる単位は `milliseconds`、`second`、`minute`、`hour` です。
     - **HTTP Pipelining**（任意）: レスポンスを待たずに送信可能な最大 HTTP リクエスト数を正の整数で指定します。デフォルトは `100`。
     - **Request Timeout**（任意）: EMQX がリクエストタイムアウトと判断するまでの待機時間を指定します。サポートされる単位は `milliseconds`、`second`、`minute`、`hour` です。
     - **Iteration Count**（任意）: SCRAM のイテレーション回数を設定します。デフォルトは `4096`。

6. 設定が完了したら、**Create** をクリックして設定を確定します。
