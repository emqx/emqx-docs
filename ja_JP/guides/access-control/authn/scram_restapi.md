# REST APIベースのMQTT 5.0 SCRAM認証

EMQXはREST APIを使用したMQTT 5.0の拡張認証をサポートしており、[Salted Challenge Response Authentication Mechanism（SCRAM）](https://en.wikipedia.org/wiki/Salted_Challenge_Response_Authentication_Mechanism)を実装しています。このSCRAM認証機構は、認証に必要なデータを取得するために外部のWebリソースを利用します。有効化されている場合、クライアントがSCRAMで接続要求を開始すると、EMQXは提供されたユーザー名を用いて外部サービスへHTTPリクエストを構築し、認証プロセスに必要な認証データを取得します。

SCRAM自体は軽量かつシンプルな認証方式ですが、本実装では外部REST APIとの連携により機能を拡張しています。これによりEMQXは様々な外部システムから安全かつ効率的に認証データを取得でき、より複雑な認証シナリオに対応可能です。

::: tip 前提条件

- [EMQXの基本的な認証概念](./authn.md)に関する理解
- SCRAM認証機構はMQTT 5.0接続のみ対応
- 本認証機構はRFC 7804の[Salted Challenge Response HTTP Authentication Mechanism](https://datatracker.ietf.org/doc/html/rfc7804)の実装ではありません

:::

## HTTPリクエストとレスポンス

認証プロセスはHTTP APIコールに類似しています。EMQXはクライアントとして動作し、外部HTTPサービスへHTTPリクエストを構築・送信します。サービスは`username`に対応する認証データを含むレスポンスを返します。

### レスポンスフォーマット要件

認証成功のため、HTTPレスポンスは以下の条件を満たす必要があります。

- **Content-Type**: レスポンスは`application/json`でエンコードされていること。
- **認証データ**: `stored_key`、`server_key`、`salt`を含み、すべて16進数でエンコードされていること。
- **スーパーユーザー指標**: `is_superuser`フィールドを使用し、値は`true`または`false`。
- **クライアント属性**: 任意で`client_attrs`フィールドに[クライアント属性](../../../develop/client-attributes/client-attributes.md)を指定可能。キー・値は文字列である必要があります。
- **アクセス制御リスト（ACL）**: 任意で`acl`フィールドにクライアントの権限を定義可能。詳細は[アクセス制御リスト](./jwt.md#access-control-list-optional)を参照してください。
- **有効期限**: 任意で`expire_at`フィールドに認証の有効期限（Unixタイムスタンプ秒単位）を設定可能。期限切れ後はクライアントは切断され再認証が必要です。
- **HTTPステータスコード**: HTTPレスポンスは`200 OK`である必要があります。`4xx`または`5xx`のステータスコードは`ignore`として扱われ、この認証機構はスキップされます。

### HTTPレスポンス例

以下は期待されるHTTPレスポンスの構造と内容の例です。

```json
HTTP/1.1 200 OK
Headers: Content-Type: application/json
...
Body:
{
    "stored_key": "008F5E0CC6316BB172F511E93E4756EEA876B5B5125F1CD2FD69A2C30F9A0D73",
    "server_key": "81466E185EC642AFAE1EFA75953735D6C0934D099149AAAB601D59F8F8162580",
    "salt": "6633653634383437393466356532333165656435346432393464366165393137",
    "is_superuser": true, // 値の選択肢: true | false、デフォルト: false
    "client_attrs": { // 任意
        "role": "admin",
        "sn": "10c61f1a1f47"
    },
    "expire_at": 1654254601, // 任意
    "acl": // 任意
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

EMQXダッシュボードからSCRAM認証機構を設定できます。

1. EMQXダッシュボードにログインします。

2. 左のナビゲーションメニューで **アクセス制御** -> **認証** をクリックし、**認証**ページを開きます。

3. 右上の **作成** をクリックします。

4. **メカニズム**に **SCRAM** を、**バックエンド**に **HTTP Server** を選択します。**次へ**をクリックすると、以下のような**設定**ステップのページに進みます。

   ![authn-scram-http](./assets/authn-scram-restapi.png)

5. バックエンドの設定を行います。

   - **Method**: HTTPリクエストメソッドを選択（`GET`または`POST`）。

     ::: tip

     `POST`メソッドはパスワードなどの機密情報がサーバーログに露出するのを防ぐため推奨されます。信頼できない環境ではHTTPSを使用してください。

     :::

   - **URL**: HTTPサービスのURLを入力します。URLのホスト部分はプレースホルダーをサポートしていません。固定のホスト名またはIPアドレスを使用してください。

   - **Precondition**: [Variform式](../../configuration/configuration.md#variform-expressions)で、クライアント接続に対してこのHTTP Server認証機構を適用するか制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列の`"true"`の場合のみ認証機構が呼び出されます。詳細は[認証の前提条件](./authn.md#authentication-preconditions)を参照してください。

   - **Headers**（任意）: 追加のHTTPリクエストヘッダーを指定します。

   - **認証設定**:

     - **Password Hash**: パスワードハッシュアルゴリズムを選択（`sha256`または`sha512`）。
     - **TLSを有効化**: スイッチを切り替えてTLSを有効化します。TLS有効化の詳細は[外部リソースアクセスのTLS](../../network/overview.md#tls-for-external-resource-access)を参照してください。
     - **Body**: リクエストテンプレートを定義します。`POST`リクエストの場合はJSON形式でリクエストボディに送信され、`GET`リクエストの場合はURLのクエリ文字列としてエンコードされます。[プレースホルダー](./authn.md#authentication-placeholders)を使ってキーと値をマッピングしてください。

   - **詳細設定**:

     - **接続プールサイズ**（任意）: EMQXノードからHTTPサーバーへの同時接続数（整数値）を設定します。デフォルトは`8`です。
     - **接続タイムアウト**（任意）: EMQXが接続タイムアウトと判断するまでの待機時間を指定します。サポートされる単位は`milliseconds`、`second`、`minute`、`hour`です。
     - **HTTPパイプライニング**（任意）: レスポンスを待たずに送信可能なHTTPリクエストの最大数を正の整数で指定します。デフォルトは`100`です。
     - **リクエストタイムアウト**（任意）: EMQXがリクエストタイムアウトと判断するまでの待機時間を指定します。サポートされる単位は`milliseconds`、`second`、`minute`、`hour`です。
     - **イテレーション回数**（任意）: SCRAMのイテレーション回数を設定します。デフォルトは`4096`です。

6. 設定が完了したら、**作成**をクリックして設定を確定します。
