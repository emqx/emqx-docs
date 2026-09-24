# HTTPサービスの利用

::: tip
EMQX v5.8.0以降、HTTP認証機能はレスポンスボディにACLルールを含めることをサポートし、クライアントの権限を事前設定できます。より良いパフォーマンスのために新しいフォーマットの使用を推奨します。詳細は[HTTP認証](../authn/http.md)をご参照ください。
:::

EMQXはHTTPサービスに基づく認可をサポートしています。ユーザーは外部HTTPアプリケーションをデータソースとして自ら構築する必要があります。EMQXはHTTPサービスにリクエストを送り、HTTP APIから返されたデータに基づいて認可結果を判断し、複雑な認可ロジックを実現します。

::: tip Tip

[基本的なEMQX認可の概念](./authz.md)の知識

:::

## HTTPリクエストとレスポンス

クライアントがサブスクライブまたはパブリッシュ操作を開始すると、HTTP認可機能は設定されたリクエストテンプレートに基づいてリクエストを構築し送信します。ユーザーは認可サービス内で認可ロジックを実装し、以下の要件に従って結果を返す必要があります。

### リクエスト

リクエストはJSON形式を使用でき、URLおよびリクエストボディ内に以下のプレースホルダーを使用可能です：

- `${clientid}`: クライアントID
- `${username}`: クライアントがログイン時に使用したユーザー名
- `${client_attrs.NAME}`: クライアント属性。`NAME`は実行時に事前設定された属性名に置き換えられます。クライアント属性の詳細は[MQTTクライアント属性](../../../develop/client-attributes/client-attributes.md)をご覧ください。
- `${peerhost}`: クライアントの送信元IPアドレス。[Proxy Protocol](http://www.haproxy.org/download/1.8/doc/proxy-protocol.txt)が有効な場合は、プロキシが報告する送信元IPアドレスを使用します。
- `${peerport}`: EMQX 6.0.4以降のクライアント送信元ポート番号（例：`51544`）。Proxy Protocolが有効な場合は、プロキシが報告する送信元ポートを使用します。
- `${peername}`: EMQX 6.0.4以降のクライアント送信元IPアドレスとポート。IPv4の場合は`IP:port`形式（例：`192.168.0.1:51544`）、IPv6の場合は角括弧なしの形式（例：`2001:db8::1:51544`）。Proxy Protocolが有効な場合は、プロキシが報告するIPアドレスとポートを使用します。
- `${proto_name}`: クライアントが使用するプロトコル名（例：`MQTT`、`CoAP`）
- `${mountpoint}`: ゲートウェイリスナーのマウントポイント（トピックプレフィックス）
- `${action}`: 要求されているアクション（例：`publish`、`subscribe`）
- `${topic}`: 現在のリクエストでパブリッシュまたはサブスクライブされるトピック（またはトピックフィルター）
- `${qos}`: 現在のリクエストでパブリッシュまたはサブスクライブされるメッセージのQoS
- `${retain}`: 現在のリクエストでパブリッシュされるメッセージがリテインドメッセージかどうか
- `${zone}`: 実行時のクライアントのゾーン。ゾーンはクライアントの論理的分類（地域や環境など）で、クライアントの設定に基づき動的に適用されます。

::: tip
`${peerhost}`および`${peerport}`プレースホルダーは非推奨です。後方互換性のためにサポートは継続していますが、新しいテンプレートでは対応可能な場合`${peername}`の使用を推奨します。
:::

### レスポンス

認可サービスはチェック後、以下のフォーマットでレスポンスを返す必要があります：

- レスポンスの`content-type`は`application/json`であること。
- HTTPステータスコードが`200`の場合、HTTPボディの`result`フィールドの値により認可結果を判断します：
  - `allow`: パブリッシュまたはサブスクライブを許可
  - `deny`: パブリッシュまたはサブスクライブを拒否
  - `ignore`: このリクエストを無視し、次の認可機能に委譲
- HTTPステータスコードが`204`の場合、このパブリッシュまたはサブスクライブリクエストは許可されたものとみなします。
- `200`および`204`以外のHTTPステータスコードは「ignore」とみなします。例えば、HTTPサービスが利用不可の場合など。

<!--- 注意：コードは`application/x-www-form-urlencoded`もサポートしていますが、将来的な拡張が難しいためドキュメントには記載していません -->

レスポンス例：

```json
HTTP/1.1 200 OK
Headers: Content-Type: application/json
...
Body:
{
    "result": "allow" | "deny" | "ignore" // デフォルトは "ignore"
}
```

::: tip EMQX 4.xとの互換性について

4.xバージョンでは、EMQXはHTTP APIのステータスコードのみを使用し、コンテンツは破棄していました。例えば、`200`は`allow`、`403`は`deny`を意味していました。より詳細な情報をユーザーに提供するため、EMQX 5.0でリクエストコンテンツの返却を追加しました。

:::

::: tip

`POST`メソッドの使用を推奨します。`GET`メソッドを使用すると、一部の機密情報がHTTPサーバーログに露出する可能性があります。

信頼できない環境ではHTTPSの使用を推奨します。

:::

## ダッシュボードでの設定

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左ナビゲーションツリーで**アクセス制御** -> **認可**をクリックし、**認可**ページに入ります。

2. 右上の**作成**をクリックし、**バックエンド**として**HTTPサーバー**を選択し、**次へ**をクリックして**設定**ステップに進みます。

   <img src="./assets/authz-http_ee.png" alt="authz-http_ee" style="zoom:67%;" />

3. 以下の指示に従って設定を行います。

   - **メソッド**: HTTPリクエストメソッドを選択します。選択肢は`GET`または`POST`です。
   - **URL**: HTTPアプリケーションのIPアドレスを入力します。
   - **ヘッダー**（任意）: HTTPリクエストヘッダーを設定します。キーと値には[プレースホルダー](./authz.md#authorization-placeholders)が使用可能です。
   - **OAuth2クライアント認証情報**: トグルをオンにすると、EMQXはアクセストークンを取得し、外部HTTP認可サービスへのリクエストに追加します。詳細は[OAuth2クライアント認証情報の設定](#configure-oauth2-client-credentials)をご覧ください。
   - **TLSを有効化**: トグルをオンにすると、外部HTTP認可サービスへの接続にTLSを有効化します。この設定はOAuth2トークンエンドポイントのTLS設定とは独立しています。
   - **ボディ**: HTTPリクエストボディを設定します。キーと値には[プレースホルダー](./authz.md#authorization-placeholders)が使用可能です。
   - **詳細設定**: 同時接続数、接続タイムアウト、最大HTTPリクエスト数、リクエストタイムアウトを設定します。
     - **プールサイズ**（任意）: EMQXノードから外部HTTPサーバーへの同時接続数を整数で指定します。デフォルトは`8`です。
     - **接続タイムアウト**（任意）: 接続タイムアウトまでの待機時間を入力します。単位は**時間**、**分**、**秒**、**ミリ秒**が使用可能です。
     - **HTTPパイプライニング**（任意）: 正の整数で、レスポンスを待たずに送信可能な最大HTTPリクエスト数を指定します。デフォルトは`100`です。
     - **リクエストタイムアウト**（任意）: リクエストタイムアウトまでの待機時間を入力します。単位は**時間**、**分**、**秒**、**ミリ秒**が使用可能です。

4. **作成**をクリックして設定を完了します。

### OAuth2クライアント認証情報の設定

EMQX 6.0.4以降、HTTP認可機能はOAuth 2.0クライアントクレデンシャルズグラントをサポートします。OAuth2を有効にすると、EMQXは設定されたトークンエンドポイントからアクセストークンを取得・キャッシュし、自動的に更新します。外部HTTP認可サービスを呼び出す際には、`Authorization: Bearer <access_token>`リクエストヘッダーにトークンを送信し、外部サービスがEMQXを認証できるようにします。

**OAuth2クライアント認証情報**をオンにし、以下の設定を行います：

| ダッシュボード設定 | 説明 |
| --- | --- |
| **トークンエンドポイント** | 必須。アクセストークンをリクエストするOAuth2認証サーバーのエンドポイント。URLはHTTPまたはHTTPSを使用し、ユーザー情報を含めてはいけません。 |
| **クライアントID** | 必須。アクセストークンをリクエストするOAuth2クライアントID。 |
| **クライアントシークレット** | 必須。アクセストークンをリクエストするOAuth2クライアントシークレット。 |
| **スコープ** | 任意。アクセストークンに要求するOAuth2スコープ。 |
| **トークンリクエストタイムアウト** | トークンエンドポイントへのHTTPリクエストのタイムアウト。デフォルトは`5`秒。 |
| **TLSを有効化** | トグルをオンにするとトークンエンドポイントへのTLSを有効化します。この設定は外部HTTP認可サービスのTLS設定とは独立しています。 |

EMQXは`application/x-www-form-urlencoded`コンテンツタイプで`POST`リクエストをトークンエンドポイントに送信します。リクエストボディには`grant_type`、`client_id`、`client_secret`、および任意の`scope`が含まれます。トークンエンドポイントは`200`レスポンスで`access_token`を含むJSONボディを返す必要があります。`token_type`および`expires_in`も返すことができ、存在する場合は`token_type`は`Bearer`、`expires_in`は正の整数でなければなりません。

::: warning 重要なお知らせ

- OAuth2を有効にした場合、HTTP認可機能に`Authorization`ヘッダーを設定しないでください。EMQXは自動生成されるBearer認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントはクライアントIDとクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic認証ヘッダーによる認証はサポートされていません。

:::

## 設定ファイルによる設定

HTTP認可は`type=http`で設定します。

HTTPの`POST`および`GET`リクエストをサポートし、それぞれに特有のオプションがあります。<!--詳細は[authz:http_post](../../configuration/configuration-manual.html#authz:http_post)および[authz:http_get](../../configuration/configuration-manual.html#authz:http_get)をご覧ください。-->

`POST`リクエストで設定したHTTP認可機能の例：

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

`GET`リクエストで設定したHTTP認可機能の例：

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

### OAuth2クライアント認証情報の設定

EMQX 6.0.4以降、HTTP認可機能に`oauth2`ブロックを追加してOAuth2クライアントクレデンシャルズを有効にできます。`method`、`url`、`body`、`headers`と同じ階層に配置してください：

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

認証サーバーがスコープを要求しない場合は`scope`を省略してください。リクエスト形式や制限事項は[OAuth2クライアント認証情報の設定](#configure-oauth2-client-credentials)をご参照ください。
