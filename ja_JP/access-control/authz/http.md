# HTTPサービスの利用

::: tip
EMQX v5.8.0以降、HTTP認証機能はレスポンスボディにACLルールを含めてクライアントの権限を事前設定できるようになりました。パフォーマンス向上のため、新しいフォーマットの利用を推奨します。詳細は[HTTP認証](../authn/http.md)をご参照ください。
:::

EMQXはHTTPサービスに基づく認可をサポートしています。ユーザーは外部のHTTPアプリケーションをデータソースとして自ら構築する必要があります。EMQXはHTTPサービスにリクエストを送り、HTTP APIから返されるデータに基づいて認可結果を判定し、複雑な認可ロジックを実現します。

::: tip ヒント

[基本的なEMQX認可の概念](./authz.md)の知識が必要です。

:::

## HTTPリクエストとレスポンス

クライアントがサブスクライブやパブリッシュ操作を開始すると、HTTP認可者は設定されたリクエストテンプレートに基づきリクエストを構築して送信します。ユーザーは認可サービス内で認可ロジックを実装し、以下の要件に従って結果を返す必要があります。

### リクエスト

リクエストはJSON形式を利用可能で、URLやリクエストボディ内に以下のプレースホルダーを使用できます：

- `${clientid}`：クライアントID
- `${username}`：クライアントがログイン時に使用したユーザー名
- `${client_attrs.NAME}`：クライアント属性。`NAME`は実行時に事前設定された属性名に置き換わります。クライアント属性の詳細は[MQTTクライアント属性](../../client-attributes/client-attributes.md)を参照してください。
- `${peerhost}`：クライアントの送信元IPアドレス
- `${proto_name}`：クライアントが使用するプロトコル名（例：`MQTT`、`CoAP`）
- `${mountpoint}`：ゲートウェイリスナーのマウントポイント（トピックプレフィックス）
- `${action}`：要求されているアクション（例：`publish`、`subscribe`）
- `${topic}`：現在のリクエストでパブリッシュまたはサブスクライブされるトピック（またはトピックフィルター）
- `${qos}`：現在のリクエストでパブリッシュまたはサブスクライブされるメッセージのQoS
- `${retain}`：現在のリクエストでパブリッシュされるメッセージがリテインメッセージかどうか
- `${zone}`：実行時のクライアントのZone。Zoneはクライアントの論理的分類（地域や環境など）で、クライアントの設定に基づき動的に適用可能です。

### レスポンス

認可サービスは以下の形式でレスポンスを返す必要があります：

- レスポンスの`content-type`は`application/json`でなければなりません。
- HTTPステータスコードが`200`の場合、認可結果はHTTPボディの`result`フィールドの値によって決まります：
  - `allow`：パブリッシュまたはサブスクライブを許可
  - `deny`：パブリッシュまたはサブスクライブを拒否
  - `ignore`：このリクエストを無視し、次の認可者に処理を委譲
- HTTPステータスコードが`204`の場合、このパブリッシュまたはサブスクライブリクエストは許可されたことを意味します。
- `200`および`204`以外のHTTPステータスコードは「無視」を意味します。例えば、HTTPサービスが利用不可の場合などです。

<!--- 注意：コードは`application/x-www-form-urlencoded`もサポートしますが、将来的な拡張が難しいためドキュメントには記載していません -->

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

バージョン4.xではHTTP APIの返すステータスコードのみを利用し、内容は破棄していました。例えば`200`は許可、`403`は拒否を意味していました。より詳細な情報を提供するため、EMQX 5.0でリクエスト内容の返却を追加しました。

:::

::: tip

`POST`メソッドの利用を推奨します。`GET`メソッド使用時はHTTPサーバーログに機密情報が露出する可能性があります。

信頼できない環境ではHTTPSの利用を推奨します。

:::

## 動的ホスト名解決の設定

デフォルトでは、HTTP認可者は作成時に`url`のホスト名を解決し、永続的なコネクションプールを使用します。認可リクエストごとにホスト名を解決するには、`hostname_resolution`を`dynamic`に設定してください。

動的ホスト名解決では、`url`のホスト部分にプレースホルダーを含めることも可能です。例えば、以下の設定はクライアントの`tenant`属性に応じて認可リクエストを異なるエンドポイントにルーティングします：

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

- `hostname_resolution`は`static`または`dynamic`を受け付けます。デフォルトは`static`です。リテラルホスト名に対しても`dynamic`を指定するとリクエストごとに解決されます。
- URLのホストにプレースホルダーが含まれる場合、`hostname_resolution`は`dynamic`でなければならず、`allowed_hosts`には少なくとも1つのエントリが必要です。
- `allowed_hosts`の各エントリは正確なホスト名（例：`auth.example.com`）またはワイルドカードパターン（例：`*.auth.example.com`）でなければなりません。ワイルドカードは指定されたサフィックスの下位ホスト名にマッチしますが、サフィックス自体にはマッチしません。URLがリテラルホスト名の場合、`allowed_hosts`は効果を持ちません。
- URLの権限部分（authority）ではホストのみがプレースホルダーを含められます。スキームは`http`または`https`でなければならず、ポートが指定されている場合はリテラルの整数でなければなりません。URLのユーザー情報やフラグメントはサポートされません。URLパスやクエリ内のプレースホルダーは引き続きサポートされます。
- EMQXが有効なホスト名を生成できない場合や、生成されたホスト名が`allowed_hosts`にマッチしない場合、HTTPリクエストは送信されず認可チェックは失敗します。
- `dynamic`モードでは、すべての生成されたホストへのリクエストが単一のコネクションプールを共有します。`pool_size`はプールが保持可能なアイドル接続数の上限を指定します。`0`に設定すると接続再利用を無効化します。`enable_pipelining`や`max_inactive`はこのモードでは適用されません。
- `dynamic`モードのHTTPSリクエストでは、EMQXは設定されたTLSオプションを生成されたホストに適用します。SNI（Server Name Indication）が明示的に設定されていない場合、EMQXは生成されたホスト名からSNIを導出します。
- `hostname_resolution`が`dynamic`の場合、OAuth2はサポートされません。

## ダッシュボードでの設定方法

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)にアクセスし、左側ナビゲーションツリーの**アクセス制御** -> **認可**をクリックして**認可**ページに入ります。

2. 右上の**作成**をクリックし、**バックエンド**として**HTTPサーバー**を選択し、**次へ**をクリックして**設定**ステップに進みます。

   <img src="./assets/authz-http_ee.png" alt="authz-http_ee" style="zoom:67%;" />

3. 以下の指示に従い設定を行います。

   - **メソッド**：HTTPリクエストメソッドを選択します。選択肢は`GET`、`POST`です。
   - **URL**：HTTPアプリケーションのURLを入力します。ホスト部分は**ホスト名解決**が`Dynamic`の場合、[認可プレースホルダー](./authz.md#authorization-placeholders)を含めることが可能です。
   - **ホスト名解決**：認可者作成時に固定ホスト名を解決する`Static`か、リクエストごとにホスト名を解決する`Dynamic`を選択します。デフォルトは`Static`です。詳細は[動的ホスト名解決の設定](#configure-dynamic-hostname-resolution)を参照してください。
   - **許可ホスト**：URLホストにプレースホルダーが含まれる場合、生成されるホスト名がマッチ可能な正確なホスト名またはワイルドカードパターンを入力します。
   - **前提条件**：任意のVariform式を入力します。この式が`true`評価のときのみEMQXはこの認可者を呼び出します。詳細は[認可者の前提条件](./authz.md#authorizer-preconditions)を参照してください。
   - **ヘッダー**（任意）：HTTPリクエストヘッダーを設定します。キーと値は[プレースホルダー](./authz.md#authorization-placeholders)を利用可能です。
   - **OAuth2クライアント認証**：トグルをオンにすると、EMQXはアクセストークンを取得し、外部HTTP認可サービスへのリクエストに追加します。詳細は[OAuth2クライアント認証の設定](#configure-oauth2-client-credentials)を参照してください。
   - **TLSを有効化**：トグルをオンにすると外部HTTP認可サービスへの接続にTLSを有効化します。この設定はOAuth2トークンエンドポイントのTLS設定とは独立しています。
   - **ボディ**：HTTPリクエストボディを設定します。キーと値は[プレースホルダー](./authz.md#authorization-placeholders)を利用可能です。
   - **詳細設定**：同時接続数、接続タイムアウト、最大HTTPリクエスト数、リクエストタイムアウトを設定します。
     - **プールサイズ**（任意）：`Static`モードでは永続的なコネクションプールのサイズを指定します。値は最低`1`以上でなければなりません。`Dynamic`モードではリクエスト間で再利用可能な接続数を指定し、`0`に設定すると接続再利用を無効化します。デフォルトは`8`です。
     - **接続タイムアウト**（任意）：接続タイムアウトの待機時間を入力します。単位は**時間**、**分**、**秒**、**ミリ秒**が指定可能です。
     - **HTTPパイプライニング**（任意）：正の整数で、レスポンスを待たずに送信可能な最大HTTPリクエスト数を指定します。デフォルトは`100`です。**ホスト名解決**が`Dynamic`の場合は適用されません。
     - **リクエストタイムアウト**（任意）：リクエストタイムアウトの待機時間を入力します。単位は**時間**、**分**、**秒**、**ミリ秒**が指定可能です。

4. **作成**をクリックして設定を完了します。

### OAuth2クライアント認証の設定

EMQX 6.0.4以降、HTTP認可者はOAuth 2.0クライアントクレデンシャルズグラントをサポートしています。OAuth2を有効にすると、EMQXは設定されたトークンエンドポイントからアクセストークンを取得・キャッシュ・自動更新します。外部HTTP認可サービスを呼び出す際、`Authorization: Bearer <access_token>`ヘッダーにトークンを付与し、外部サービス側でEMQXの認証を行います。

**OAuth2クライアント認証**をオンにし、以下の設定を行います：

| ダッシュボード設定 | 説明 |
| --- | --- |
| **トークンエンドポイント** | 必須。アクセストークンを要求するOAuth2認証サーバーのエンドポイント。URLはHTTPまたはHTTPSで、ユーザー情報を含んではいけません。 |
| **クライアントID** | 必須。アクセストークンを要求するOAuth2クライアントID。 |
| **クライアントシークレット** | 必須。アクセストークンを要求するOAuth2クライアントシークレット。 |
| **スコープ** | 任意。アクセストークンに要求するOAuth2スコープ。 |
| **トークンリクエストタイムアウト** | トークンエンドポイントへのHTTPリクエストのタイムアウト。デフォルトは`5`秒。 |
| **TLSを有効化** | トグルをオンにするとトークンエンドポイントへの接続にTLSを有効化します。この設定は外部HTTP認可サービスのTLS設定とは独立しています。 |

EMQXは`application/x-www-form-urlencoded`のコンテンツタイプで`POST`リクエストをトークンエンドポイントに送信します。リクエストボディには`grant_type`、`client_id`、`client_secret`、任意の`scope`が含まれます。トークンエンドポイントは`200`レスポンスでJSONボディに`access_token`を返す必要があります。`token_type`と`expires_in`も返すことができ、存在する場合、`token_type`は`Bearer`、`expires_in`は正の整数でなければなりません。

::: warning 重要なお知らせ

- OAuth2を有効にした場合、HTTP認可者の`Authorization`ヘッダーを設定しないでください。EMQXは自動生成されるBearer認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントはクライアントIDとクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic認証ヘッダーによる認証はサポートされません。

:::

## 設定ファイルによる設定方法

HTTP認可は`type=http`で設定します。

任意の`precondition`設定項目はVariform式を受け付けます。EMQXはこの式が`true`評価のときのみ認可者を呼び出します。`precondition`が省略または空の場合、前提条件は適用されません。詳細は[認可者の前提条件](./authz.md#authorizer-preconditions)を参照してください。

HTTPの`POST`および`GET`リクエストをサポートしています。それぞれに特有のオプションがあります。<!--詳細は[authz:http_post](../../configuration/configuration-manual.html#authz:http_post)および[authz:http_get](../../configuration/configuration-manual.html#authz:http_get)を参照してください。-->

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

EMQX 6.0.4以降、HTTP認可者設定に`oauth2`ブロックを追加することでOAuth2クライアント認証を有効化できます。`method`、`url`、`body`、`headers`と同じ階層に配置してください：

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

認可サーバーがスコープを要求しない場合は`scope`を省略してください。リクエスト形式や制限事項の詳細は[OAuth2クライアント認証の設定](#configure-oauth2-client-credentials)を参照してください。
