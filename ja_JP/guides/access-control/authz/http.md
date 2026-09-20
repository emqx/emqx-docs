# HTTPサービスの利用

::: tip
EMQX v5.8.0以降、HTTP認証機能はレスポンスボディにACLルールを含めてクライアントの権限を事前設定できるようになりました。より高いパフォーマンスのために新しいフォーマットの使用を推奨します。詳細は[HTTP認証](../authn/http.md)をご参照ください。
:::

EMQXはHTTPサービスに基づく認可をサポートしています。ユーザーは外部のHTTPアプリケーションをデータソースとして自ら構築する必要があります。EMQXはHTTPサービスにリクエストを送り、HTTP APIから返されたデータに基づいて認可結果を判定することで、複雑な認可ロジックを実現します。

::: tip ヒント

[基本的なEMQX認可の概念](./authz.md)についての知識

:::

## HTTPリクエストとレスポンス

クライアントがサブスクライブやパブリッシュ操作を開始すると、HTTP認可機能は設定されたリクエストテンプレートに基づいてリクエストを構築し送信します。ユーザーは認可サービス内で認可ロジックを実装し、以下の要件に従って結果を返す必要があります。

### リクエスト

リクエストはJSON形式を利用でき、URLやリクエストボディ内で以下のプレースホルダーが使用可能です：

- `${clientid}`：クライアントID
- `${username}`：クライアントがログイン時に使用したユーザー名
- `${client_attrs.NAME}`：クライアント属性。`NAME`は実行時に事前設定された属性名に置き換えられます。クライアント属性の詳細は[MQTTクライアント属性](../../../develop/client-attributes/client-attributes.md)をご参照ください。
- `${peerhost}`：クライアントの送信元IPアドレス
- `${proto_name}`：クライアントが使用するプロトコル名（例：`MQTT`、`CoAP`）
- `${mountpoint}`：ゲートウェイリスナーのマウントポイント（トピックプレフィックス）
- `${action}`：要求されているアクション（例：`publish`、`subscribe`）
- `${topic}`：現在のリクエストでパブリッシュまたはサブスクライブされるトピック（またはトピックフィルター）
- `${qos}`：現在のリクエストでパブリッシュまたはサブスクライブされるメッセージのQoS（サービス品質）
- `${retain}`：現在のリクエストでパブリッシュされるメッセージがリテインドメッセージかどうか
- `${zone}`：実行時のクライアントのゾーン。ゾーンはクライアントの論理的分類（地域や環境など）で、クライアントの設定に基づき動的に適用されます。

### レスポンス

認可サービスは以下の形式でレスポンスを返す必要があります：

- レスポンスの`content-type`は`application/json`でなければなりません。
- HTTPステータスコードが`200`の場合、HTTPボディの`result`フィールドの値により認可結果が決まります：
  - `allow`：パブリッシュまたはサブスクライブを許可
  - `deny`：パブリッシュまたはサブスクライブを拒否
  - `ignore`：このリクエストを無視し、次の認可機能に処理を委ねる
- HTTPステータスコードが`204`の場合、このパブリッシュまたはサブスクライブリクエストは許可されたことを意味します。
- `200`および`204`以外のHTTPステータスコードは「無視」を意味します。例えば、HTTPサービスが利用不可の場合などです。

<!--- 注意：コードは`application/x-www-form-urlencoded`もサポートしていますが、将来的な拡張性の観点からドキュメントには記載していません -->

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

4.xバージョンでは、EMQXはHTTP APIのステータスコードのみを利用し、コンテンツは破棄していました。例えば`200`は`allow`、`403`は`deny`を意味していました。より詳細な情報提供のため、EMQX 5.0でリクエストコンテンツの返却を追加しました。

:::

::: tip

`POST`メソッドの使用を推奨します。`GET`メソッドを使用すると、HTTPサーバーログにより一部の機密情報が露出する可能性があります。

信頼できない環境ではHTTPSの利用を推奨します。

:::

## 動的ホスト名解決の設定

デフォルトでは、HTTP認可機能は作成時に`url`内のホスト名を解決し、永続的なコネクションプールを使用します。認可リクエストごとにホスト名を解決するには、`hostname_resolution`を`dynamic`に設定してください。

動的ホスト名解決では、`url`のホスト部分にプレースホルダーを使用できます。例えば、以下の設定はクライアントの`tenant`属性に応じて認可リクエストを異なるエンドポイントにルーティングします：

```hocon
{
    type = http
    method = post
    url = "https://${client_attrs.tenant}.auth.example.com/authz"
    hostname_resolution = dynamic
    allowed_hosts = ["*.auth.example.com"]
    pool_size = 8
    headers {
        "Content-Type" = "application/json"
    }
    body {
        username = "${username}"
        topic = "${topic}"
        action = "${action}"
    }
    ssl {
        enable = true
    }
}
```

動的ホスト名解決を設定する際の注意点：

- `hostname_resolution`は`static`または`dynamic`を受け付けます。デフォルトは`static`です。リテラルホスト名に対しても`dynamic`を指定すると、リクエストごとにホスト名を解決します。
- URLのホストにプレースホルダーが含まれる場合、`hostname_resolution`は`dynamic`でなければならず、`allowed_hosts`には少なくとも1つのエントリが必要です。
- `allowed_hosts`の各エントリは、`auth.example.com`のような正確なホスト名か、`*.auth.example.com`のようなワイルドカードパターンでなければなりません。ワイルドカードは指定されたサフィックス以下のホスト名にマッチしますが、サフィックス自体にはマッチしません。URLがリテラルホスト名の場合、`allowed_hosts`は効果を持ちません。
- URLの権限部分ではホストのみがプレースホルダーを含められます。スキームは`http`または`https`でなければならず、ポートが指定されている場合はリテラルの整数でなければなりません。URLのユーザー情報やフラグメントはサポートされません。URLパスやクエリのプレースホルダーは引き続きサポートされます。
- EMQXが有効なホスト名をレンダリングできない場合や、レンダリングされたホスト名が`allowed_hosts`にマッチしない場合、HTTPリクエストは送信されず認可チェックは失敗します。
- `dynamic`モードでは、レンダリングされたすべてのホストへのリクエストが単一のコネクションプールを共有します。`pool_size`はプールが保持可能なアイドル接続数の上限を制限します。`0`に設定すると接続の再利用を無効化します。`enable_pipelining`および`max_inactive`はこのモードでは適用されません。
- `dynamic`モードのHTTPSリクエストでは、EMQXは設定されたTLSオプションをレンダリングされたホストに適用します。SNI（Server Name Indication）が明示的に設定されていない限り、EMQXはレンダリングされたホスト名からSNIを導出します。
- `hostname_resolution`が`dynamic`の場合、OAuth2はサポートされません。

## ダッシュボードでの設定

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左ナビゲーションツリーで**アクセス制御** -> **認可**をクリックし、**認可**ページに入ります。

2. 右上の**作成**をクリックし、**バックエンド**として**HTTPサーバー**を選択して、**次へ**をクリックし設定画面に進みます。

   <img src="./assets/authz-http_ee.png" alt="authz-http_ee" style="zoom:67%;" />

3. 以下の手順に従い設定を行います。

   - **メソッド**：HTTPリクエストメソッドを選択します。選択肢は`GET`、`POST`です。
   - **URL**：HTTPアプリケーションのURLを入力します。ホスト部分は**ホスト名解決**が`Dynamic`の場合、[認可プレースホルダー](./authz.md#authorization-placeholders)を含めることができます。
   - **ホスト名解決**：認可機能作成時に固定ホスト名を解決する`Static`か、リクエストごとにホスト名を解決する`Dynamic`を選択します。デフォルトは`Static`です。詳細は[動的ホスト名解決の設定](#configure-dynamic-hostname-resolution)をご参照ください。
   - **許可ホスト**：URLホストにプレースホルダーが含まれる場合、レンダリングされたホスト名がマッチを許可される正確なホスト名またはワイルドカードパターンを入力します。
   - **前提条件**：省略可能なVariform式を入力します。式が`true`と評価された場合にのみこの認可機能が呼び出されます。詳細は[認可機能の前提条件](./authz.md#authorizer-preconditions)をご参照ください。
   - **ヘッダー**（省略可能）：HTTPリクエストヘッダーを設定します。キーと値は[プレースホルダー](./authz.md#authorization-placeholders)を使用可能です。
   - **OAuth2クライアント認証**：トグルをオンにすると、EMQXはアクセストークンを取得し、外部HTTP認可サービスへのリクエストに追加します。詳細は[OAuth2クライアント認証の設定](#configure-oauth2-client-credentials)をご参照ください。
   - **TLSを有効化**：トグルをオンにすると、外部HTTP認可サービスへの接続にTLSを有効化します。この設定はOAuth2トークンエンドポイントのTLS設定とは独立しています。
   - **ボディ**：HTTPリクエストボディを設定します。キーと値は[プレースホルダー](./authz.md#authorization-placeholders)を使用可能です。
   - **詳細設定**：同時接続数、接続タイムアウト、最大HTTPリクエスト数、リクエストタイムアウトを設定します。
     - **プールサイズ**（省略可能）：`Static`モードでは永続的なコネクションプールのサイズを指定します。値は最低`1`以上でなければなりません。`Dynamic`モードではリクエスト間で再利用可能な接続数を指定し、`0`に設定すると接続再利用が無効になります。デフォルトは`8`です。
     - **接続タイムアウト**（省略可能）：接続タイムアウトの待機時間を単位付き（時間、分、秒、ミリ秒）で入力します。
     - **HTTPパイプライニング**（省略可能）：正の整数で、レスポンスを待たずに送信可能な最大HTTPリクエスト数を指定します。デフォルトは`100`です。**ホスト名解決**が`Dynamic`の場合、この設定は無効です。
     - **リクエストタイムアウト**（省略可能）：リクエストタイムアウトの待機時間を単位付き（時間、分、秒、ミリ秒）で入力します。

4. **作成**をクリックして設定を完了します。

### OAuth2クライアント認証の設定

EMQX 6.0.4以降、HTTP認可機能はOAuth 2.0クライアントクレデンシャルズグラントをサポートします。OAuth2を有効にすると、EMQXは設定されたトークンエンドポイントからアクセストークンを取得・キャッシュ・自動更新します。EMQXが外部HTTP認可サービスを呼び出す際、`Authorization: Bearer <access_token>`リクエストヘッダーにトークンを付与し、外部サービスがEMQXを認証できるようにします。

**OAuth2クライアント認証**をオンにし、以下の設定を行います：

| ダッシュボード設定 | 説明 |
| --- | --- |
| **トークンエンドポイント** | 必須。アクセストークン取得に使用するOAuth2認可サーバーのエンドポイント。URLはHTTPまたはHTTPSで、ユーザー情報を含んではいけません。 |
| **クライアントID** | 必須。アクセストークン取得に使用するOAuth2クライアントID。 |
| **クライアントシークレット** | 必須。アクセストークン取得に使用するOAuth2クライアントシークレット。 |
| **スコープ** | 省略可能。アクセストークンに要求するOAuth2スコープ。 |
| **トークンリクエストタイムアウト** | トークンエンドポイントへのHTTPリクエストのタイムアウト。デフォルトは`5`秒。 |
| **TLSを有効化** | トグルをオンにするとトークンエンドポイントへのTLSを有効化します。この設定は外部HTTP認可サービスのTLS設定とは独立しています。 |

EMQXは`application/x-www-form-urlencoded`コンテンツタイプの`POST`リクエストをトークンエンドポイントに送信します。リクエストボディには`grant_type`、`client_id`、`client_secret`、および省略可能な`scope`が含まれます。トークンエンドポイントは`200`レスポンスとともに`access_token`を含むJSONボディを返す必要があります。`token_type`と`expires_in`も返せます。存在する場合、`token_type`は`Bearer`でなければならず、`expires_in`は正の整数でなければなりません。

::: warning 重要なお知らせ

- OAuth2を有効にした場合、HTTP認可機能に`Authorization`ヘッダーを設定しないでください。EMQXは自動生成されるBearer認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントはクライアントIDとクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic認証ヘッダーによる認証はサポートされていません。

:::

## 設定項目による設定

HTTP認可は`type=http`で設定します。

省略可能な`precondition`設定項目はVariform式を受け付けます。式が`true`と評価された場合にのみこの認可機能が呼び出されます。`precondition`を省略または空にした場合は前提条件は適用されません。詳細は[認可機能の前提条件](./authz.md#authorizer-preconditions)をご参照ください。

HTTPの`POST`および`GET`リクエストをサポートしています。それぞれ固有のオプションがあります。<!--詳細は[authz:http_post](../../configuration/configuration-manual.html#authz:http_post)および[authz:http_get](../../configuration/configuration-manual.html#authz:http_get)をご参照ください。-->

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

### OAuth2クライアント認証の設定

EMQX 6.0.4以降、HTTP認可機能の設定に`oauth2`ブロックを追加し、OAuth2クライアント認証を有効にできます。`method`、`url`、`body`、`headers`と同じ階層に配置してください：

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

認可サーバーがスコープを要求しない場合は`scope`を省略してください。リクエスト形式や制限事項の詳細は[OAuth2クライアント認証の設定](#configure-oauth2-client-credentials)をご参照ください。
