# HTTPサービスの利用

EMQXは、外部HTTPサービスを用いたパスワード認証をサポートしています。有効化すると、クライアントが接続リクエストを開始した際に、EMQXは受け取った情報を使ってHTTPリクエストを構築し、クエリ結果に基づいてリクエストの受け入れ可否を判断することで、複雑な認証ロジックを実現します。

::: tip 前提条件

[EMQXの基本的な認証概念](../authn/authn.md)の知識

:::

## HTTPリクエストとレスポンス

認証プロセスはHTTP APIコールに似ており、EMQXはリクエストクライアントとして「API」が要求する形式でHTTPサービスへリクエストを構築・送信し、HTTPサービスは「クライアント」が要求する形式で結果を返します。

- レスポンスのエンコード形式 `content-type` は `application/json` である必要があります。
- 認証結果はボディ内の `result` で示し、値は `allow`、`deny`、`ignore` のいずれかです。
- スーパーユーザーはボディ内の `is_superuser` で示し、値は `true` または `false` です。
- EMQX v5.7.0以降、オプションの `client_attrs` フィールドで[クライアント属性](../../client-attributes/client-attributes.md)を設定可能です。キーと値は両方とも文字列である必要があります。
- EMQX v5.8.0以降、レスポンスボディにオプションの `acl` フィールドを設定してクライアントの権限を指定可能です。詳細は[アクセスコントロールリスト（ACL）](./acl.md)を参照してください。
- EMQX v5.8.0以降、レスポンスボディにオプションの `expire_at` フィールドを設定してクライアント認証の有効期限を指定可能です。再接続時にクライアントを強制切断し再認証を促します。値は秒単位のUnixタイムスタンプです。
- HTTPレスポンスのステータスコードは `200` または `204` であるべきです。`4xx/5xx` のステータスコードが返された場合、ボディは無視され、結果は `ignore` と判定され認証チェーンは継続されます。

レスポンス例:

```js
HTTP/1.1 200 OK
Headers: Content-Type: application/json
...
Body:
{
    "result": "allow", // "allow" | "deny" | "ignore"
    "is_superuser": false, // オプション値: true | false、デフォルト: false
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

EMQX 4.xではHTTPステータスコードのみが使用され、ボディは破棄されます。例えば、`200` は `allow`、`403` は `deny` を意味します。
表現力不足のため、HTTPボディを利用する形に再設計されており、EMQX 5.0とは互換性がありません。

:::

## ダッシュボードによる設定

EMQXダッシュボードを使って関連設定を完了できます。

1. EMQXダッシュボードの左ナビゲーションメニューから **Access Control** -> **Authentication** をクリックします。
2. **Authentication** ページの右上にある **Create** をクリックします。
3. **Mechanism** に **Password-Based** を選択し、**Backend** に **HTTP Server** を選択すると、以下のように **Configuration** タブに遷移します。

<img src="./assets/authn-http.png" alt="HTTP" style="zoom:67%;" />

4. 以下の指示に従い認証バックエンドを設定します：

   - **Method**：HTTPリクエストメソッドを選択します。選択肢は `get`、`post` です。

     :::tip

     `POST` メソッドの利用を推奨します。`GET` メソッド使用時は、パスワードなどの機密情報がHTTPサーバーログに平文で記録される可能性があります。また、信頼できない環境ではHTTPSの利用を推奨します。

     :::

   - **URL**：HTTPサービスのURLアドレスを入力します。
   - **Precondition**：このHTTPサーバー認証器をクライアント接続に適用するかを制御するための[Variform式](../../configuration/configuration.md#variform-expressions)です。`username`、`clientid`、`listener`などクライアント属性に対して評価され、式の評価結果が文字列の `"true"` の場合のみ認証器が呼び出されます。詳細は[Authenticator Preconditions](./authn.md#authenticator-preconditions)を参照してください。
   - **Headers**（オプション）：HTTPリクエストヘッダーです。複数追加可能で、キーと値には[プレースホルダー](./authn.md#authentication-placeholders)が使用できます。
   - **Enable TLS**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[Network and TLS](../../network/overview.md)を参照してください。
   - **Body**：リクエストテンプレートです。`POST` リクエストの場合はJSON形式でリクエストボディに送信され、`GET` リクエストの場合はURLのクエリ文字列としてエンコードされます。マッピングのキーと値には[プレースホルダー](./authn.md#authentication-placeholders)が使用可能です。
   - **Advanced Settings**：
     - **Pool size**（オプション）：EMQXノードからHTTPサーバーへの同時接続数を整数値で指定します。デフォルトは `8` です。

     - **Connect Timeout**（オプション）：EMQXが接続タイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間がサポートされています。

     - **HTTP Pipelining**（オプション）：レスポンスを待たずに送信可能な最大HTTPリクエスト数を正の整数で指定します。デフォルトは `100` です。

     - **Request Timeout**（オプション）：EMQXがリクエストタイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間がサポートされています。

5. 設定が完了したら **Create** をクリックします。

## 設定項目による設定

EMQXの設定項目を使ってHTTP認証器を設定可能です。<!--詳細は[authn-http:post](../../configuration/configuration-manual.html#authn-http:post)および[authn-http:get](../../configuration/configuration-manual.html#authn-http:get)を参照してください。-->

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

注：`body` はクエリ文字列に変換されます。

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
