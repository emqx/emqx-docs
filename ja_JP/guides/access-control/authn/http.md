# HTTPサービスの利用

EMQXは、パスワード認証に外部HTTPサービスを利用することをサポートしています。有効化すると、クライアントが接続要求を開始した際に、EMQXは受け取った情報を用いてHTTPリクエストを構築し、クエリ結果に基づいて接続要求の受け入れ可否を判断し、複雑な認証ロジックを実現します。

::: tip 前提条件

[EMQX認証の基本概念](./authn.md)の知識が必要です。

:::

## HTTPリクエストとレスポンス

認証プロセスはHTTP API呼び出しに似ており、EMQXはリクエストクライアントとして「API」が要求する形式でHTTPサービスへリクエストを構築・送信し、HTTPサービスは「クライアント」が要求する形式で結果を返します。

- レスポンスのエンコーディング形式 `content-type` は `application/json` である必要があります。
- 認証結果はボディ内の `result` で示し、値は `allow`、`deny`、`ignore` のいずれかです。
- スーパーユーザーはボディ内の `is_superuser` で示し、値は `true` または `false` です。
- EMQX v5.7.0以降、オプションの `client_attrs` フィールドを使用して[クライアント属性](../../../develop/client-attributes/client-attributes.md)を設定できます。キーと値は両方とも文字列である必要があります。
- EMQX v5.8.0以降、レスポンスボディにオプションの `acl` フィールドを設定してクライアントの権限を指定できます。詳細は[アクセスコントロールリスト（ACL）](./acl.md)を参照してください。
- EMQX v5.8.0以降、レスポンスボディにオプションの `expire_at` フィールドを設定してクライアントの認証有効期限を指定できます。これにより、クライアントは切断され、再接続時に再認証が強制されます。値は秒単位のUnixタイムスタンプです。
- HTTPレスポンスのステータスコードは `200` または `204` が望ましく、`4xx/5xx` のステータスコードが返された場合はボディを無視し、結果を `ignore` として認証チェーンを継続します。

レスポンス例：

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

::: tip EMQX 4.x 互換性について

EMQX 4.xではHTTPステータスコードのみを使用し、ボディは破棄されます。例えば、`200` は `allow`、`403` は `deny` を示します。
表現力の不足から、HTTPボディを利用する形に再設計されており、EMQX 5.0とは互換性がありません。

:::

## 動的ホスト名解決の設定

デフォルトでは、HTTP認証機は作成時に `url` のホスト名を解決し、永続的なコネクションプールを使用します。認証リクエストごとにホスト名を解決するには、`hostname_resolution` を `dynamic` に設定します。

動的ホスト名解決は、`url` のホスト部分にプレースホルダーを使用することも可能です。例えば、以下の設定はクライアントの `tenant` 属性に応じて認証リクエストを異なるエンドポイントにルーティングします。

```hocon
{
    mechanism = password_based
    backend = http
    method = post
    url = "https://${client_attrs.tenant}.auth.example.com/authn"
    hostname_resolution = dynamic
    allowed_hosts = ["*.auth.example.com"]
    pool_size = 8
    headers {
        "Content-Type" = "application/json"
    }
    body {
        username = "${username}"
        password = "${password}"
    }
    ssl {
        enable = true
    }
}
```

動的ホスト名解決の設定時には以下に注意してください：

- `hostname_resolution` は `static` または `dynamic` を受け付け、デフォルトは `static` です。リテラルホスト名に対しても `dynamic` を指定すると、リクエストごとにホスト名を解決します。
- URLのホストにプレースホルダーが含まれる場合、`hostname_resolution` は `dynamic` でなければならず、`allowed_hosts` に少なくとも1つのエントリが必要です。
- `allowed_hosts` の各エントリは正確なホスト名（例：`auth.example.com`）またはワイルドカードパターン（例：`*.auth.example.com`）でなければなりません。ワイルドカードは指定されたサフィックス以下のホスト名にマッチしますが、サフィックス自体にはマッチしません。URLがリテラルホスト名の場合、`allowed_hosts` は無効です。
- URLの権限部分では、ホストのみがプレースホルダーを含めることができます。スキームは `http` または `https` でなければならず、ポートが指定されている場合はリテラルの整数でなければなりません。URLのユーザー情報やフラグメントはサポートされません。URLのパスやクエリ内のプレースホルダーは引き続きサポートされます。
- EMQXが有効なホスト名をレンダリングできない場合、またはレンダリングされたホスト名が `allowed_hosts` に一致しない場合、HTTPリクエストは送信されず認証は失敗します。
- `dynamic` モードでは、レンダリングされたすべてのホストへのリクエストが同じコネクションプールを共有します。`pool_size` はプールが再利用のために保持できるアイドル接続数を制限します。`0` に設定すると接続再利用が無効になります。`enable_pipelining` と `max_inactive` はこのモードでは適用されません。
- `dynamic` モードのHTTPSリクエストでは、EMQXは設定されたTLSオプションをレンダリングされたホストに適用します。SNI（Server Name Indication）が明示的に設定されていない限り、EMQXはレンダリングされたホスト名からSNIを導出します。
- `hostname_resolution` が `dynamic` の場合、OAuth2はサポートされません。

## ダッシュボードでの設定

EMQXダッシュボードを使って関連設定を完了できます。

1. EMQXダッシュボードの左ナビゲーションメニューから **Access Control** -> **Authentication** をクリックします。

2. **Authentication** ページの右上にある **Create** をクリックします。

3. **Mechanism** に **Password-Based** を選択し、**Backend** に **HTTP Server** を選択して **Configuration** ステップへ進みます。

   <img src="./assets/authn-http.png" alt="HTTP" style="zoom:67%;" />

4. 以下の指示に従い認証バックエンドを設定してください：

   - **Method**：HTTPリクエストメソッドを選択します。選択肢は `get`、`post` です。

     :::tip

     `POST` メソッドの使用を推奨します。`GET` メソッドを使用すると、平文パスワードなどの機密情報がHTTPサーバーログに露出する可能性があります。また、信頼できない環境ではHTTPSを使用してください。

     :::

   - **URL**：HTTPサービスのURLアドレスを入力します。ホスト部分は**Hostname Resolution**が `Dynamic` の場合、[認証プレースホルダー](./authn.md#authentication-placeholders)を含めることができます。
   - **Hostname Resolution**：認証機作成時に固定ホスト名を解決する `Static`、またはリクエストごとにホスト名を解決する `Dynamic` を選択します。デフォルトは `Static` です。詳細は[動的ホスト名解決の設定](#configure-dynamic-hostname-resolution)を参照してください。
   - **Allowed Hosts**：URLホストにプレースホルダーが含まれる場合、レンダリングされたホスト名が一致可能な正確なホスト名またはワイルドカードパターンを入力します。
   - **Precondition**：[Variform式](../../configuration/configuration.md#variform-expressions)で、このHTTPサーバー認証機をクライアント接続に適用するか制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証機が呼び出されます。詳細は[認証機の前提条件](./authn.md#authenticator-preconditions)を参照してください。
   - **Headers**（オプション）：HTTPリクエストヘッダー。複数追加可能で、キーと値は[プレースホルダー](./authn.md#authentication-placeholders)を使用できます。
   - **OAuth2 Client Credentials**：トグルをオンにすると、EMQXがアクセストークンを取得し、外部HTTP認証サービスへのリクエストに追加します。詳細は[OAuth2クライアント認証情報の設定](#configure-oauth2-client-credentials)を参照してください。
   - **Enable TLS**：トグルをオンにすると、外部HTTP認証サービスへの接続でTLSを有効にします。この設定はOAuth2トークンエンドポイントのTLS設定とは独立しています。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md)を参照してください。
   - **Body**：リクエストテンプレート。`POST` リクエストの場合はJSONとしてリクエストボディに送信され、`GET` リクエストの場合はURLのクエリ文字列にエンコードされます。マッピングのキーと値は[プレースホルダー](./authn.md#authentication-placeholders)を使用可能です。
   - **Advanced Settings**：
     - **Pool size**（オプション）：`Static` モードで永続的なコネクションプールのサイズを指定します。値は最低 `1` 以上でなければなりません。`Dynamic` モードでは、リクエスト間で再利用可能な接続数を指定し、`0` に設定すると接続再利用が無効になります。デフォルトは `8` です。

     - **Connect Timeout**（オプション）：EMQXが接続タイムアウトとみなすまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。

     - **HTTP Pipelining**（オプション）：レスポンスを待たずに送信可能な最大HTTPリクエスト数を正の整数で指定します。デフォルトは `100` です。**Hostname Resolution** が `Dynamic` の場合はこの設定は適用されません。

     - **Request Timeout**（オプション）：EMQXがリクエストタイムアウトとみなすまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。

5. 設定が完了したら、**Create** をクリックします。

### OAuth2クライアント認証情報の設定

EMQX 6.0.4以降、HTTP認証機はOAuth 2.0のクライアントクレデンシャルズグラントをサポートしています。OAuth2を有効にすると、EMQXは設定されたトークンエンドポイントからアクセストークンを取得・キャッシュ・自動更新します。外部HTTP認証サービスを呼び出す際、`Authorization: Bearer <access_token>` ヘッダーにトークンを付与し、外部サービスがEMQXを認証できるようにします。

**OAuth2 Client Credentials** をオンにし、以下の設定を行います：

| ダッシュボード設定 | 説明 |
| --- | --- |
| **Token Endpoint** | 必須。アクセストークンを要求するOAuth2認可サーバーのエンドポイント。URLはHTTPまたはHTTPSで、ユーザー情報を含んではいけません。 |
| **Client ID** | 必須。アクセストークンを要求するOAuth2クライアントID。 |
| **Client Secret** | 必須。アクセストークンを要求するOAuth2クライアントシークレット。 |
| **Scope** | オプション。アクセストークンに要求するOAuth2スコープ。 |
| **Token Request Timeout** | トークンエンドポイントへのHTTPリクエストのタイムアウト。デフォルトは `5` 秒です。 |
| **Enable TLS** | トグルをオンにするとトークンエンドポイントへのTLSを有効にします。この設定は外部HTTP認証サービスのTLS設定とは独立しています。 |

EMQXは `application/x-www-form-urlencoded` コンテンツタイプの `POST` リクエストをトークンエンドポイントに送信します。リクエストボディには `grant_type`、`client_id`、`client_secret`、およびオプションの `scope` が含まれます。トークンエンドポイントはJSONボディを含む `200` レスポンスを返す必要があります。JSONには `access_token` が含まれ、`token_type` と `expires_in` を含めることもできます。存在する場合、`token_type` は `Bearer`、`expires_in` は正の整数でなければなりません。例：

```json
{
  "access_token": "eyJhbGciOi...",
  "token_type": "Bearer",
  "expires_in": 3600
}
```

::: warning 重要なお知らせ

- OAuth2を有効にしている場合、HTTP認証機に `Authorization` ヘッダーを設定しないでください。EMQXは自動生成されるBearer認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントはクライアントIDとクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic認証ヘッダーによるトークンエンドポイント認証はサポートされていません。

:::

## 設定項目による設定

EMQXの設定項目でHTTP認証機を設定できます。<!--詳細は[authn-http:post](../../configuration/configuration-manual.html#authn-http:post)および[authn-http:get](../../configuration/configuration-manual.html#authn-http:get)を参照してください。-->

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

EMQX 6.0.4以降、HTTP認証機の設定に `oauth2` ブロックを追加してOAuth2クライアント認証情報を有効にできます。`method`、`url`、`body`、`headers` と同じ階層に配置します：

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

認可サーバーがスコープを要求しない場合は `scope` を省略してください。リクエスト形式や制限については[OAuth2クライアント認証情報の設定](#configure-oauth2-client-credentials)を参照してください。
