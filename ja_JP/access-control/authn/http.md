# HTTPサービスの利用

EMQXは、パスワード認証に外部HTTPサービスを利用することをサポートしています。有効化すると、クライアントが接続リクエストを開始した際に、EMQXは受け取った情報をもとにHTTPリクエストを構築し、クエリ結果に基づいてリクエストの受け入れ可否を判断することで、複雑な認証ロジックを実現します。

::: tip 前提条件

[EMQX認証の基本概念](../authn/authn.md)の知識が必要です。

:::

## HTTPリクエストとレスポンス

認証プロセスはHTTP API呼び出しに似ており、EMQXはリクエストクライアントとして「API」が要求する形式でHTTPサービスにリクエストを構築・送信し、HTTPサービスは「クライアント」が要求する形式で結果を返します。

- レスポンスのエンコード形式 `content-type` は `application/json` である必要があります。
- 認証結果はボディ内の `result` で示し、値は `allow`、`deny`、`ignore` のいずれかです。
- スーパーユーザーはボディ内の `is_superuser` で示し、値は `true` または `false` です。
- EMQX v5.7.0以降、オプションの `client_attrs` フィールドを使って[クライアント属性](../../client-attributes/client-attributes.md)を設定できます。キーと値はどちらも文字列である必要があります。
- EMQX v5.8.0以降、レスポンスボディにオプションの `acl` フィールドを設定してクライアントの権限を指定できます。詳細は[アクセスコントロールリスト（ACL）](./acl.md)を参照してください。
- EMQX v5.8.0以降、レスポンスボディにオプションの `expire_at` フィールドを設定してクライアントの認証有効期限を指定できます。これによりクライアントは切断され、再接続時に再認証が必要になります。値は秒単位のUnixタイムスタンプです。
- HTTPレスポンスのステータスコードは `200` または `204` が望ましく、`4xx/5xx` のステータスコードが返された場合はボディを無視し、結果を `ignore` として認証チェーンを継続します。

レスポンス例:

```js
HTTP/1.1 200 OK
Headers: Content-Type: application/json
...
Body:
{
    "result": "allow", // "allow" | "deny" | "ignore"
    "is_superuser": false, // オプション値: true | false、デフォルトは false
    "client_attrs": { // オプション（v5.7.0以降）
        "role": "admin",
        "sn": "10c61f1a1f47"
    }
    "expire_at": 1654254601, // オプション（v5.8.0以降）
    "acl": // オプション（v5.8.0以降）
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

::: tip EMQX 4.x互換性について

EMQX 4.xではHTTPステータスコードのみが使用され、ボディは破棄されます。例えば、`200` は `allow`、`403` は `deny` を意味します。
表現力が不足しているため、HTTPボディを利用する形に再設計されており、EMQX 5.0とは互換性がありません。

:::

## ダッシュボードでの設定

EMQXダッシュボードを使って関連設定を行えます。

1. EMQXダッシュボードの左ナビゲーションメニューから **Access Control** -> **Authentication** をクリックします。

2. **Authentication** ページの右上にある **Create** をクリックします。

3. **Mechanism** に **Password-Based** を選択し、**Backend** に **HTTP Server** を選択して **Configuration** ステップに進みます。

   <img src="./assets/authn-http.png" alt="HTTP" style="zoom:67%;" />

4. 以下の指示に従い認証バックエンドを設定します：

   - **Method**: HTTPリクエストメソッドを選択します。選択肢は `get`、`post` です。

     :::tip

     `POST` メソッドを推奨します。`GET` メソッドを使用すると、平文パスワードなどの機密情報がHTTPサーバーログに露出する可能性があります。また、信頼できない環境ではHTTPSを使用してください。

     :::

   - **URL**: HTTPサービスのURLアドレスを入力します。
   - **Precondition**: [Variform式](../../configuration/configuration.md#variform-expressions)で、このHTTPサーバー認証器をクライアント接続に適用するかどうかを制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証器が呼び出されます。詳細は[認証器の前提条件](./authn.md#authenticator-preconditions)を参照してください。
   - **Headers**（オプション）: HTTPリクエストヘッダーです。複数のヘッダーを追加可能で、キーと値に[プレースホルダー](./authn.md#authentication-placeholders)を使用できます。
   - **OAuth2 Client Credentials**: トグルスイッチをオンにすると、EMQXがアクセストークンを取得し、外部HTTP認証サービスへのリクエストにトークンを追加します。詳細は[OAuth2クライアント認証情報の設定](#configure-oauth2-client-credentials)を参照してください。
   - **Enable TLS**: トグルスイッチをオンにすると、外部HTTP認証サービスへの接続にTLSを有効化します。この設定はOAuth2トークンエンドポイントのTLS設定とは独立しています。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md)を参照してください。
   - **Body**: リクエストテンプレートです。`POST` リクエストの場合はJSONとしてリクエストボディに送信され、`GET` リクエストの場合はURLのクエリ文字列としてエンコードされます。マッピングのキーと値には[プレースホルダー](./authn.md#authentication-placeholders)を使用できます。
   - **Advanced Settings**:
     - **Pool size**（オプション）: EMQXノードからHTTPサーバーへの同時接続数を整数で指定します。デフォルトは `8` です。
     - **Connect Timeout**（オプション）: EMQXが接続タイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。
     - **HTTP Pipelining**（オプション）: 応答を待たずに送信可能なHTTPリクエストの最大数を正の整数で指定します。デフォルトは `100` です。
     - **Request Timeout**（オプション）: EMQXがリクエストタイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。

5. 設定が完了したら **Create** をクリックします。

### OAuth2クライアント認証情報の設定

EMQX 6.0.4以降、HTTP認証器はOAuth 2.0のクライアントクレデンシャルズグラントをサポートしています。OAuth2を有効にすると、EMQXは設定されたトークンエンドポイントからアクセストークンを取得・キャッシュ・自動更新します。外部HTTP認証サービス呼び出し時には、`Authorization: Bearer <access_token>` ヘッダーにトークンを付与し、外部サービスはこれによりEMQXを認証できます。

**OAuth2 Client Credentials** をオンにし、以下の設定を行います：

| ダッシュボード設定 | 説明 |
| --- | --- |
| **Token Endpoint** | 必須。アクセストークンを要求するOAuth2認可サーバーのエンドポイント。URLはHTTPまたはHTTPSで、ユーザー情報を含まない必要があります。 |
| **Client ID** | 必須。アクセストークンを要求するOAuth2クライアントID。 |
| **Client Secret** | 必須。アクセストークンを要求するOAuth2クライアントシークレット。 |
| **Scope** | オプション。アクセストークンに要求するOAuth2スコープ。 |
| **Token Request Timeout** | トークンエンドポイントへのHTTPリクエストのタイムアウト。デフォルトは `5` 秒です。 |
| **Enable TLS** | トグルスイッチをオンにするとトークンエンドポイントへのTLSを有効化します。この設定は外部HTTP認証サービスのTLS設定とは独立しています。 |

EMQXは `application/x-www-form-urlencoded` コンテンツタイプで `POST` リクエストをトークンエンドポイントに送信します。リクエストボディには `grant_type`、`client_id`、`client_secret`、およびオプションの `scope` が含まれます。トークンエンドポイントは `200` レスポンスでJSONボディに `access_token` を返す必要があります。`token_type` と `expires_in` も返すことができ、存在する場合は `token_type` は `Bearer`、`expires_in` は正の整数である必要があります。例：

```json
{
  "access_token": "eyJhbGciOi...",
  "token_type": "Bearer",
  "expires_in": 3600
}
```

::: warning 重要なお知らせ

- OAuth2を有効にしている場合、HTTP認証器の設定で `Authorization` ヘッダーを設定しないでください。EMQXは自動生成されるBearer認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントはクライアントIDとクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic認証ヘッダーによる認証はサポートされていません。

:::

## 設定項目による設定

EMQXの設定項目を使ってHTTP認証器を設定できます。 <!--詳細は[authn-http:post](../../configuration/configuration-manual.html#authn-http:post)および[authn-http:get](../../configuration/configuration-manual.html#authn-http:get)を参照してください。-->

以下はHTTPの `POST` と `GET` リクエストの例です：

:::: tabs type:card

::: tab POSTリクエスト

```hcl
{
    mechanism = password_based
    backend = http

    method = post
    url = "http://127.0.0.1:8080/auth?clientid=${clientid}"
    body {
        username = "${username}"
        password = "${password}"
    }
    headers {
        "Content-Type" = "application/json"
        "X-Request-Source" = "EMQX"
    }
}
```

:::

::: tab GETリクエスト

注：「body」はクエリ文字列に変換されます。

```hcl
{
    mechanism = password_based
    backend = http

    method = get
    url = "http://127.0.0.1:32333/auth"
    body {
        username = "${username}"
        password = "${password}"
    }
    headers {
        "X-Request-Source" = "EMQX"
    }
}
```

:::

::::

### OAuth2クライアント認証情報の設定

EMQX 6.0.4以降、HTTP認証器の設定に `oauth2` ブロックを追加してOAuth2クライアント認証情報を有効にできます。`method`、`url`、`body`、`headers` と同じ階層に配置します：

```hocon
oauth2 {
    enable = true
    grant_type = client_credentials
    token_endpoint = "https://auth.example.com/oauth/token"
    client_id = "emqx-client"
    client_secret = "emqx-client-secret"
    scope = "device.read device.write"
    timeout = 5s
    ssl {
        enable = true
    }
}
```

認可サーバーがスコープを要求しない場合は `scope` を省略してください。リクエスト形式や制約については[OAuth2クライアント認証情報の設定](#configure-oauth2-client-credentials)を参照してください。
