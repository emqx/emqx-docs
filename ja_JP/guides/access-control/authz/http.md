# HTTPサービスの利用

::: tip
EMQX v5.8.0以降、HTTP認証機能はレスポンスボディにACLルールを含めてクライアントの権限を事前設定できるようになりました。より高いパフォーマンスのために新しいフォーマットの利用を推奨します。詳細は[HTTP認証](../authn/http.md)をご参照ください。
:::

EMQXはHTTPサービスに基づく認可をサポートしています。ユーザーは外部HTTPアプリケーションをデータソースとして自ら構築する必要があります。EMQXはHTTPサービスにリクエストを送り、HTTP APIから返されたデータに基づいて認可結果を判定し、複雑な認可ロジックを実現します。

::: tip ヒント

[EMQX認可の基本概念](./authz.md)の知識を推奨します。

:::

## HTTPリクエストとレスポンス

クライアントがサブスクライブまたはパブリッシュ操作を開始すると、HTTP認可者は設定されたリクエストテンプレートに基づきリクエストを構築して送信します。ユーザーは認可サービス内で認可ロジックを実装し、以下の要件に従って結果を返す必要があります。

### リクエスト

リクエストはJSON形式で、URLおよびリクエストボディ内に以下のプレースホルダーを使用できます。

- `${clientid}`: クライアントID
- `${username}`: クライアントがログイン時に使用したユーザー名
- `${client_attrs.NAME}`: クライアント属性。`NAME`は実行時に事前設定された属性名に置き換わります。クライアント属性の詳細は[MQTTクライアント属性](../../../develop/client-attributes/client-attributes.md)をご参照ください。
- `${peerhost}`: クライアントの送信元IPアドレス
- `${proto_name}`: クライアントが使用するプロトコル名（例：`MQTT`、`CoAP`）
- `${mountpoint}`: ゲートウェイリスナーのマウントポイント（トピックプレフィックス）
- `${action}`: リクエストされているアクション（例：`publish`、`subscribe`）
- `${topic}`: 現在のリクエストでパブリッシュまたはサブスクライブされるトピック（またはトピックフィルター）
- `${qos}`: 現在のリクエストでパブリッシュまたはサブスクライブされるメッセージのQoS
- `${retain}`: 現在のリクエストでパブリッシュされるメッセージがリテインメッセージかどうか
- `${zone}`: 実行時のクライアントのゾーン。ゾーンはクライアントの論理的分類（例：地域や環境）で、クライアント設定に基づき動的に適用されます。

### レスポンス

認可サービスはチェック後、以下の形式でレスポンスを返す必要があります。

- レスポンスの`content-type`は`application/json`でなければなりません。
- HTTPステータスコードが`200`の場合、認可結果はHTTPボディの`result`フィールドの値により判定します。
  - `allow`: パブリッシュまたはサブスクライブを許可
  - `deny`: パブリッシュまたはサブスクライブを拒否
  - `ignore`: このリクエストを無視し、次の認可者に処理を委譲
- HTTPステータスコードが`204`の場合、このパブリッシュまたはサブスクライブリクエストは許可されたとみなします。
- `200`および`204`以外のHTTPステータスコードは「ignore」とみなし、例えばHTTPサービスが利用不可の場合などに該当します。

<!--- 注意: コードは`application/x-www-form-urlencoded`もサポートしていますが、将来的な拡張性の観点からドキュメントには記載していません -->

レスポンス例:

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

4.x系ではHTTP APIのステータスコードのみが認可判定に使われ、コンテンツは破棄されていました。例えば`200`は許可、`403`は拒否を意味していました。ユーザーにより多くの情報を提供するため、EMQX 5.0でリクエスト内容の返却を追加しました。

:::

::: tip

`POST`メソッドの利用を推奨します。`GET`メソッドを使用すると、HTTPサーバーログに機密情報が露出する可能性があります。

信頼できない環境ではHTTPSを使用してください。

:::

## ダッシュボードでの設定

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左ナビゲーションツリーで**アクセス制御** -> **認可**をクリックし、**認可**ページに入ります。

2. 右上の**作成**をクリックし、**バックエンド**に**HTTPサーバー**を選択して**次へ**をクリックし、**設定**ステップに進みます。

   <img src="./assets/authz-http_ee.png" alt="authz-http_ee" style="zoom:67%;" />

3. 以下の指示に従って設定を行います。

   - **メソッド**: HTTPリクエストメソッドを選択します。選択肢は`GET`、`POST`です。
   - **URL**: HTTPアプリケーションのIPアドレスを入力します。
   - **ヘッダー**（任意）: HTTPリクエストヘッダーを設定します。キーと値は[プレースホルダー](./authz.md#authorization-placeholders)を使用可能です。
   - **OAuth2クライアント認証情報**: トグルスイッチをオンにすると、EMQXがアクセストークンを取得し、外部HTTP認可サービスへのリクエストに付加します。詳細は[OAuth2クライアント認証情報の設定](#configure-oauth2-client-credentials)をご覧ください。
   - **TLSを有効化**: トグルスイッチをオンにすると、外部HTTP認可サービスへの接続にTLSを有効化します。この設定はOAuth2トークンエンドポイントのTLS設定とは独立しています。
   - **ボディ**: HTTPリクエストボディを設定します。キーと値は[プレースホルダー](./authz.md#authorization-placeholders)を使用可能です。
   - **詳細設定**: 同時接続数、接続タイムアウト、最大HTTPリクエスト数、リクエストタイムアウトを設定します。
     - **プールサイズ**（任意）: EMQXノードから外部HTTPサーバーへの同時接続数を整数で指定します。デフォルトは`8`です。
     - **接続タイムアウト**（任意）: 接続タイムアウトの待機時間を入力します。単位は**時間**、**分**、**秒**、**ミリ秒**が指定可能です。
     - **HTTPパイプライニング**（任意）: 正の整数で、レスポンスを待たずに送信可能な最大HTTPリクエスト数を指定します。デフォルトは`100`です。
     - **リクエストタイムアウト**（任意）: リクエストタイムアウトの待機時間を入力します。単位は**時間**、**分**、**秒**、**ミリ秒**が指定可能です。

4. **作成**をクリックして設定を完了します。

### OAuth2クライアント認証情報の設定

EMQX 6.0.4以降、HTTP認可者はOAuth 2.0クライアントクレデンシャルズグラントをサポートします。OAuth2を有効にすると、EMQXは設定されたトークンエンドポイントからアクセストークンを取得・キャッシュし、自動的に更新します。EMQXが外部HTTP認可サービスを呼び出す際、`Authorization: Bearer <access_token>`リクエストヘッダーにトークンを付加し、外部サービスはEMQXを認証できます。

**OAuth2クライアント認証情報**をオンにし、以下の設定を行います。

| ダッシュボード設定 | 説明 |
| --- | --- |
| **トークンエンドポイント** | 必須。アクセストークンを要求するOAuth2認可サーバーのエンドポイント。URLはHTTPまたはHTTPSで、ユーザー情報を含んではいけません。 |
| **クライアントID** | 必須。アクセストークンを要求するOAuth2クライアントID。 |
| **クライアントシークレット** | 必須。アクセストークンを要求するOAuth2クライアントシークレット。 |
| **スコープ** | 任意。アクセストークンに要求するOAuth2スコープ。 |
| **トークンリクエストタイムアウト** | トークンエンドポイントへのHTTPリクエストのタイムアウト。デフォルトは`5`秒です。 |
| **TLSを有効化** | トグルスイッチをオンにするとトークンエンドポイントへのTLSを有効化します。この設定は外部HTTP認可サービスのTLS設定とは独立しています。 |

EMQXは`application/x-www-form-urlencoded`コンテンツタイプで`POST`リクエストをトークンエンドポイントに送信します。リクエストボディには`grant_type`、`client_id`、`client_secret`、任意で`scope`が含まれます。トークンエンドポイントは`200`レスポンスでJSONボディに`access_token`を返す必要があります。`token_type`と`expires_in`も返せます。存在する場合、`token_type`は`Bearer`でなければならず、`expires_in`は正の整数でなければなりません。

::: warning 重要なお知らせ

- OAuth2を有効にした場合、HTTP認可者の`Authorization`ヘッダーは設定しないでください。EMQXは自動生成されるBearer認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントはクライアントIDとクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic認証ヘッダーによる認証はサポートされていません。

:::

## 設定項目による設定

HTTP認可は`type=http`で設定します。

HTTPの`POST`および`GET`リクエストをサポートし、それぞれに特有のオプションがあります。<!--詳細は[authz:http_post](../../configuration/configuration-manual.html#authz:http_post)および[authz:http_get](../../configuration/configuration-manual.html#authz:http_get)をご参照ください。-->

`POST`リクエストで設定したHTTP認可者の例:

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

`GET`リクエストで設定したHTTP認可者の例:

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

EMQX 6.0.4以降、HTTP認可者の設定に`oauth2`ブロックを追加してOAuth2クライアント認証情報を有効化できます。`method`、`url`、`body`、`headers`と同じ階層に配置してください。

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

認可サーバーがスコープを要求しない場合は`scope`を省略してください。リクエスト形式や制約については[OAuth2クライアント認証情報の設定](#configure-oauth2-client-credentials)をご参照ください。
