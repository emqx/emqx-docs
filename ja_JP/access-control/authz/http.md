# HTTPサービスの利用

::: tip
EMQX v5.8.0以降、HTTP認証機能はレスポンスボディにACLルールを含めてクライアントの権限を事前設定できるようになりました。より良いパフォーマンスのために新しいフォーマットの利用を推奨します。詳細は[HTTP認証](../authn/http.md)をご参照ください。
:::

EMQXはHTTPサービスに基づく認可をサポートしています。ユーザーは外部HTTPアプリケーションをデータソースとして自ら構築する必要があります。EMQXはHTTPサービスにリクエストを送り、HTTP APIから返されるデータに基づいて認可結果を判定し、複雑な認可ロジックを実現します。

::: tip ヒント

[EMQX認可の基本概念](./authz.md)の知識

:::

## HTTPリクエストとレスポンス

クライアントがサブスクライブまたはパブリッシュ操作を開始すると、HTTP認可者は設定されたリクエストテンプレートに基づいてリクエストを構築し送信します。ユーザーは認可サービス内で認可ロジックを実装し、以下の要件に従って結果を返す必要があります。

### リクエスト

リクエストはJSON形式を利用でき、URLやリクエストボディ内に以下のプレースホルダーを使用できます：

- `${clientid}`：クライアントID
- `${username}`：クライアントがログイン時に使用したユーザー名
- `${client_attrs.NAME}`：クライアント属性。`NAME`は実行時に事前設定された属性名に置き換えられます。クライアント属性の詳細は[MQTTクライアント属性](../../client-attributes/client-attributes.md)をご参照ください。
- `${peerhost}`：クライアントの送信元IPアドレス
- `${proto_name}`：クライアントが使用するプロトコル名（例：`MQTT`、`CoAP`）
- `${mountpoint}`：ゲートウェイリスナーのマウントポイント（トピックプレフィックス）
- `${action}`：要求されているアクション（例：`publish`、`subscribe`）
- `${topic}`：現在のリクエストでパブリッシュまたはサブスクライブされるトピック（またはトピックフィルター）
- `${qos}`：現在のリクエストでパブリッシュまたはサブスクライブされるメッセージのQoS
- `${retain}`：現在のリクエストでパブリッシュされるメッセージがリテインドメッセージかどうか
- `${zone}`：実行時のクライアントのゾーン。ゾーンはクライアントの論理的な分類（地域や環境など）で、クライアント設定に基づき動的に適用されます。

### レスポンス

認可サービスは以下のフォーマットでレスポンスを返す必要があります：

- レスポンスの`content-type`は`application/json`であること。
- HTTPステータスコードが`200`の場合、認可結果はHTTPボディの`result`フィールドの値に依存します：
  - `allow`：パブリッシュまたはサブスクライブを許可
  - `deny`：パブリッシュまたはサブスクライブを拒否
  - `ignore`：このリクエストを無視し、次の認可者に処理を委ねる
- HTTPステータスコードが`204`の場合、このパブリッシュまたはサブスクライブリクエストは許可されたことを意味します。
- `200`および`204`以外のHTTPステータスコードは「無視」を意味します。例えば、HTTPサービスが利用不可の場合などです。

<!--- 注意：コードは`application/x-www-form-urlencoded`もサポートしていますが、将来的な拡張が難しいためドキュメントには記載していません -->

レスポンス例：

```json
HTTP/1.1 200 OK
Headers: Content-Type: application/json
...
Body:
{
    "result": "allow" | "deny" | "ignore" // デフォルトは `"ignore"`
}
```

::: tip EMQX 4.xとの互換性について

4.x系ではHTTP APIのステータスコードのみを利用し、内容は破棄していました。例えば`200`は`allow`、`403`は`deny`を示します。より詳細な情報を提供するため、EMQX 5.0でリクエスト内容の返却を追加しました。

:::

::: tip

`POST`メソッドの利用を推奨します。`GET`メソッド使用時はHTTPサーバーログにより一部の機密情報が露出する可能性があります。

信頼できない環境ではHTTPSの利用を推奨します。

:::

## ダッシュボードでの設定

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)で、左側ナビゲーションツリーの**アクセス制御** -> **認可**をクリックし、**認可**ページに入ります。

2. 右上の**作成**をクリックし、**バックエンド**に**HTTPサーバー**を選択して、**次へ**をクリックし、**設定**ステップに進みます。

   <img src="./assets/authz-http_ee.png" alt="authz-http_ee" style="zoom:67%;" />

3. 以下の指示に従って設定を行います。

   - **メソッド**：HTTPリクエストメソッドを選択します。選択肢は`GET`、`POST`です。
   - **URL**：HTTPアプリケーションのIPアドレスを入力します。
   - **ヘッダー**（任意）：HTTPリクエストヘッダーを設定します。キーと値は[プレースホルダー](./authz.md#authorization-placeholders)を利用可能です。
   - **OAuth2クライアント認証**：トグルスイッチをオンにすると、EMQXはアクセストークンを取得し、外部HTTP認可サービスへのリクエストに付加します。詳細は[OAuth2クライアント認証の設定](#configure-oauth2-client-credentials)をご参照ください。
   - **TLSを有効にする**：トグルスイッチをオンにすると、外部HTTP認可サービスへの接続にTLSを有効にします。この設定はOAuth2トークンエンドポイントのTLS設定とは独立しています。
   - **ボディ**：HTTPリクエストボディを設定します。キーと値は[プレースホルダー](./authz.md#authorization-placeholders)を利用可能です。
   - **詳細設定**：同時接続数、接続タイムアウト、最大HTTPリクエスト数、リクエストタイムアウトを設定します。
     - **プールサイズ**（任意）：EMQXノードから外部HTTPサーバーへの同時接続数を整数で指定します。デフォルトは`8`です。
     - **接続タイムアウト**（任意）：接続タイムアウトの待機時間を入力します。単位は**時間**、**分**、**秒**、**ミリ秒**が指定可能です。
     - **HTTPパイプライン**（任意）：正の整数で、レスポンスを待たずに送信可能な最大HTTPリクエスト数を指定します。デフォルトは`100`です。
     - **リクエストタイムアウト**（任意）：リクエストタイムアウトの待機時間を入力します。単位は**時間**、**分**、**秒**、**ミリ秒**が指定可能です。

4. **作成**をクリックして設定を完了します。

### OAuth2クライアント認証の設定

EMQX 6.0.4以降、HTTP認可者はOAuth 2.0クライアントクレデンシャルズグラントをサポートします。OAuth2を有効にすると、EMQXは設定されたトークンエンドポイントからアクセストークンを取得・キャッシュし、自動更新します。外部HTTP認可サービス呼び出し時には、`Authorization: Bearer <access_token>`ヘッダーにトークンを付加し、外部サービス側でEMQXを認証可能にします。

**OAuth2クライアント認証**をオンにし、以下の設定を行います：

| ダッシュボード設定 | 説明 |
| --- | --- |
| **トークンエンドポイント** | 必須。アクセストークン取得に使用するOAuth2認可サーバーのエンドポイント。URLはHTTPまたはHTTPSで、ユーザー情報を含んではいけません。 |
| **クライアントID** | 必須。アクセストークン取得に使用するOAuth2クライアントID。 |
| **クライアントシークレット** | 必須。アクセストークン取得に使用するOAuth2クライアントシークレット。 |
| **スコープ** | 任意。アクセストークン取得時に要求するOAuth2スコープ。 |
| **トークンリクエストタイムアウト** | トークンエンドポイントへのHTTPリクエストのタイムアウト。デフォルトは`5`秒。 |
| **TLSを有効にする** | トグルスイッチをオンにするとトークンエンドポイントへのTLSを有効にします。この設定は外部HTTP認可サービスのTLS設定とは独立しています。 |

EMQXは`application/x-www-form-urlencoded`のコンテンツタイプで`POST`リクエストをトークンエンドポイントに送信します。リクエストボディには`grant_type`、`client_id`、`client_secret`、および任意の`scope`が含まれます。トークンエンドポイントは`200`レスポンスと共に`access_token`を含むJSONボディを返す必要があります。`token_type`と`expires_in`も返せます。存在する場合、`token_type`は`Bearer`、`expires_in`は正の整数でなければなりません。

::: warning 重要なお知らせ

- OAuth2を有効にしている場合、HTTP認可者に`Authorization`ヘッダーを設定しないでください。EMQXは自動生成されるBearer認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントはクライアントIDとクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic認証ヘッダーによる認証はサポートしていません。

:::

## 設定項目による設定

HTTP認可は`type=http`で設定します。

HTTPの`POST`および`GET`リクエストをサポートし、それぞれ固有のオプションがあります。 <!--詳細は[authz:http_post](../../configuration/configuration-manual.html#authz:http_post)および[authz:http_get](../../configuration/configuration-manual.html#authz:http_get)をご参照ください。-->

`POST`リクエストで設定したHTTP認可者の例：

```bash
{
    type = http

    method = post
    url = "http://127.0.0.1:32333/authz/${peercert}?clientid=${clientid}"
    body {
        username = "${username}"
        topic = "${topic}"
        action = "${action}"
    }
    headers {
        "Content-Type" = "application/json"
        "X-Request-Source" = "EMQX"
    }
}
```

`GET`リクエストで設定したHTTP認可者の例：

```bash
{
    type = http

    method = get
    url = "http://127.0.0.1:32333/authz"
    body {
        username = "${username}"
        topic = "${topic}"
        action = "${action}"
    }
    headers {
        "X-Request-Source" = "EMQX"
    }
}
```

### OAuth2クライアント認証の設定

EMQX 6.0.4以降、HTTP認可者設定に`oauth2`ブロックを追加してOAuth2クライアント認証を有効にできます。`method`、`url`、`body`、`headers`と同じ階層に配置します：

```hocon
oauth2 {
    enable = true
    grant_type = client_credentials
    token_endpoint = "https://auth.example.com/oauth/token"
    client_id = "emqx-client"
    client_secret = "emqx-client-secret"
    scope = "authorization.check"
    timeout = 5s
    ssl {
        enable = true
    }
}
```

認可サーバーがスコープを要求しない場合は`scope`を省略してください。リクエスト形式や制限については[OAuth2クライアント認証の設定](#configure-oauth2-client-credentials)をご参照ください。
