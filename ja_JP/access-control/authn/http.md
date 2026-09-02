# HTTP サービスの利用

EMQX は、パスワード認証に外部の HTTP サービスを利用することをサポートしています。有効化すると、クライアントが接続要求を開始した際に、EMQX は受け取った情報をもとに HTTP リクエストを構築し、クエリ結果に基づいて要求を受け入れるかどうかを判断し、複雑な認証ロジックを実現します。

::: tip 前提条件

[EMQX 認証の基本概念](../authn/authn.md)の知識が必要です。

:::

## HTTP リクエストとレスポンス

認証プロセスは HTTP API コールに似ており、EMQX はリクエストクライアントとして「API」が要求する形式で HTTP サービスへリクエストを構築・送信し、HTTP サービスは「クライアント」が要求する形式で結果を返します。

- レスポンスのエンコード形式 `content-type` は `application/json` である必要があります。
- 認証結果はボディの `result` で示し、値は `allow`、`deny`、`ignore` のいずれかです。
- スーパーユーザーはボディの `is_superuser` で示し、値は `true` または `false` です。
- EMQX v5.7.0 以降、オプションの `client_attrs` フィールドで[クライアント属性](../../client-attributes/client-attributes.md)を設定可能です。キーと値はどちらも文字列である必要があります。
- EMQX v5.8.0 以降、レスポンスボディにオプションの `acl` フィールドを設定してクライアントの権限を指定できます。詳細は[アクセスコントロールリスト（ACL）](./acl.md)を参照してください。
- EMQX v5.8.0 以降、レスポンスボディにオプションの `expire_at` フィールドを設定してクライアントの認証有効期限を指定できます。これによりクライアントは切断され、再接続時に再認証が強制されます。値は秒単位の Unix タイムスタンプです。
- HTTP レスポンスのステータスコードは `200` または `204` であるべきです。`4xx` または `5xx` のステータスコードが返された場合はボディを無視し、結果を `ignore` と判断して認証チェーンを継続します。

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

EMQX 4.x では HTTP ステータスコードのみを利用し、ボディは破棄されます。例えば `200` は `allow`、`403` は `deny` を意味します。
表現力不足のため、HTTP ボディを利用する形に再設計されており、EMQX 5.0 とは互換性がありません。

:::

## 動的ホスト名解決の設定

デフォルトでは、HTTP 認証機構は認証機構作成時に `url` のホスト名を解決し、永続的なコネクションプールを使用します。認証リクエストごとにホスト名を解決したい場合は、`hostname_resolution` を `dynamic` に設定します。

動的ホスト名解決では、`url` のホスト部分にプレースホルダーを使用できます。例えば、以下の設定はクライアントの `tenant` 属性に応じて認証リクエストを異なるエンドポイントにルーティングします。

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

動的ホスト名解決を設定する際は以下に注意してください：

- `hostname_resolution` は `static` または `dynamic` を受け付け、デフォルトは `static` です。リテラルホスト名に対しても `dynamic` を指定すると、リクエストごとにそのホスト名を解決します。
- URL ホストにプレースホルダーが含まれる場合、`hostname_resolution` は必ず `dynamic` であり、`allowed_hosts` に少なくとも1つのエントリが必要です。
- `allowed_hosts` の各エントリは正確なホスト名（例：`auth.example.com`）またはワイルドカードパターン（例：`*.auth.example.com`）である必要があります。ワイルドカードは指定されたサフィックス以下のホスト名にマッチしますが、サフィックス自体にはマッチしません。URL がリテラルホスト名の場合、`allowed_hosts` は無効です。
- URL の権限部（authority）内でプレースホルダーを含められるのはホストのみです。スキームは `http` または `https` でなければならず、ポートが指定されている場合はリテラルの整数でなければなりません。URL のユーザー情報やフラグメントはサポートされません。URL パスやクエリのプレースホルダーは引き続きサポートされます。
- EMQX が有効なホスト名をレンダリングできないか、レンダリングされたホスト名が `allowed_hosts` にマッチしない場合、HTTP リクエストは送信されず認証は失敗します。
- `dynamic` モードでは、レンダリングされたすべてのホストへのリクエストが同じコネクションプールを共有します。`pool_size` はプールが保持できるアイドル接続数の上限を指定し、`0` に設定すると接続再利用を無効化します。`enable_pipelining` と `max_inactive` はこのモードでは無効です。
- `dynamic` モードの HTTPS リクエストでは、EMQX は設定された TLS オプションをレンダリングされたホストに適用します。SNI（Server Name Indication）が明示的に設定されていない場合、EMQX はレンダリングされたホスト名から自動的に派生します。
- `hostname_resolution` が `dynamic` の場合、OAuth2 はサポートされません。

## ダッシュボードでの設定

EMQX ダッシュボードを使って関連設定を完了できます。

1. EMQX ダッシュボードの左側ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。

2. **認証** ページの右上にある **作成** をクリックします。

3. **メカニズム** に **パスワードベース**、**バックエンド** に **HTTP サーバー** を選択し、**設定** ステップに進みます。

   <img src="./assets/authn-http.png" alt="HTTP" style="zoom:67%;" />

4. 以下の指示に従って認証バックエンドを設定してください：

   - **メソッド**：HTTP リクエストメソッドを選択します。選択肢は `get`、`post` です。

     :::tip

     `POST` メソッドの使用を推奨します。`GET` メソッドを使用すると、HTTP サーバーログに平文パスワードなどの機密情報が露出する可能性があります。また、信頼できない環境では HTTPS を使用してください。

     :::

   - **URL**：HTTP サービスの URL アドレスを入力します。ホスト部分には **ホスト名解決** が `Dynamic` の場合、[認証プレースホルダー](./authn.md#authentication-placeholders)を含めることができます。
   - **ホスト名解決**：認証機構作成時に固定ホスト名を解決する場合は `Static`、リクエストごとにホスト名を解決する場合は `Dynamic` を選択します。デフォルトは `Static` です。詳細は[動的ホスト名解決の設定](#configure-dynamic-hostname-resolution)を参照してください。
   - **許可ホスト**：URL ホストにプレースホルダーが含まれる場合、レンダリングされたホスト名がマッチ可能な正確なホスト名またはワイルドカードパターンを入力します。
   - **前提条件**：[Variform 式](../../configuration/configuration.md#variform-expressions)で、HTTP サーバー認証機構をクライアント接続に適用するか制御します。式はクライアントの属性（`username`、`clientid`、`listener` など）に対して評価され、文字列 `"true"` の場合のみ認証機構が呼び出されます。そうでなければスキップされます。詳細は[認証機構の前提条件](./authn.md#authenticator-preconditions)を参照してください。
   - **ヘッダー**（オプション）：HTTP リクエストヘッダー。複数のヘッダーを追加可能です。キーと値は[プレースホルダー](./authn.md#authentication-placeholders)を使用できます。
   - **OAuth2 クライアント認証情報**：トグルをオンにすると、EMQX がアクセストークンを取得し、外部 HTTP 認証サービスへ送信するリクエストに追加します。詳細は[OAuth2 クライアント認証情報の設定](#configure-oauth2-client-credentials)を参照してください。
   - **TLS を有効化**：トグルをオンにすると、外部 HTTP 認証サービスへの接続に TLS を有効化します。この設定は OAuth2 トークンエンドポイントの TLS 設定とは独立しています。TLS 有効化の詳細は[ネットワークと TLS](../../network/overview.md)を参照してください。
   - **ボディ**：リクエストテンプレート。`POST` リクエストの場合は JSON ボディとして送信され、`GET` リクエストの場合は URL のクエリ文字列にエンコードされます。マッピングのキーと値は[プレースホルダー](./authn.md#authentication-placeholders)を使用できます。
   - **詳細設定**：
     - **プールサイズ**（オプション）：`Static` モードでは永続的コネクションプールのサイズを指定します。値は最低 `1` である必要があります。`Dynamic` モードではリクエスト間で再利用可能な接続数を指定し、`0` に設定すると接続再利用を無効化します。デフォルトは `8` です。

     - **接続タイムアウト**（オプション）：EMQX が接続タイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。

     - **HTTP パイプライニング**（オプション）：レスポンスを待たずに送信可能な最大 HTTP リクエスト数を正の整数で指定します。デフォルトは `100` です。**ホスト名解決**が `Dynamic` の場合は適用されません。

     - **リクエストタイムアウト**（オプション）：EMQX がリクエストタイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。

5. 設定が完了したら、**作成** をクリックします。

### OAuth2 クライアント認証情報の設定

EMQX 6.0.4 以降、HTTP 認証機構は OAuth 2.0 クライアントクレデンシャルズグラントをサポートします。OAuth2 を有効化すると、EMQX は設定されたトークンエンドポイントからアクセストークンを取得・キャッシュ・自動更新します。EMQX が外部 HTTP 認証サービスを呼び出す際、`Authorization: Bearer <access_token>` ヘッダーにトークンを付加し、外部サービスはこれを使って EMQX を認証します。

**OAuth2 クライアント認証情報**をオンにし、以下の設定を行います：

| ダッシュボード設定項目 | 説明 |
| --- | --- |
| **トークンエンドポイント** | 必須。アクセストークンをリクエストする OAuth2 認可サーバーのエンドポイント。URL は HTTP または HTTPS を使用し、ユーザー情報を含んではいけません。 |
| **クライアント ID** | 必須。アクセストークン取得に使用する OAuth2 クライアント ID。 |
| **クライアントシークレット** | 必須。アクセストークン取得に使用する OAuth2 クライアントシークレット。 |
| **スコープ** | オプション。アクセストークンに要求する OAuth2 スコープ。 |
| **トークンリクエストタイムアウト** | トークンエンドポイントへの HTTP リクエストのタイムアウト。デフォルトは `5` 秒。 |
| **TLS を有効化** | トークンエンドポイントへの接続に TLS を有効化します。この設定は外部 HTTP 認証サービスの TLS 設定とは独立しています。 |

EMQX は `application/x-www-form-urlencoded` コンテンツタイプで `POST` リクエストをトークンエンドポイントに送信します。リクエストボディには `grant_type`、`client_id`、`client_secret`、およびオプションの `scope` が含まれます。トークンエンドポイントは `200` レスポンスで JSON ボディに `access_token` を返す必要があります。`token_type` と `expires_in` も返すことができます。存在する場合、`token_type` は `Bearer`、`expires_in` は正の整数でなければなりません。例：

```json
{
  "access_token": "eyJhbGciOi...",
  "token_type": "Bearer",
  "expires_in": 3600
}
```

::: warning 重要なお知らせ

- OAuth2 を有効にしている場合、HTTP 認証機構の設定で `Authorization` ヘッダーを設定しないでください。EMQX は自動生成される Bearer 認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントはクライアント ID とクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic 認証ヘッダーによるトークンエンドポイント認証はサポートされていません。

:::

## 設定項目による設定

EMQX の設定項目で HTTP 認証機構を設定できます。<!--詳細は [authn-http:post](../../configuration/configuration-manual.html#authn-http:post) と [authn-http:get](../../configuration/configuration-manual.html#authn-http:get) を参照してください。-->

以下は HTTP の `POST` および `GET` リクエストの例です：

:::: tabs type:card

::: tab POST リクエスト

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

::: tab GET リクエスト

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

### OAuth2 クライアント認証情報の設定

EMQX 6.0.4 以降、HTTP 認証機構の設定に `oauth2` ブロックを追加して OAuth2 クライアント認証情報を有効化できます。`method`、`url`、`body`、`headers` と同じ階層に配置してください：

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

認可サーバーがスコープを要求しない場合は `scope` を省略してください。リクエスト形式と制約については[OAuth2 クライアント認証情報の設定](#configure-oauth2-client-credentials)を参照してください。
