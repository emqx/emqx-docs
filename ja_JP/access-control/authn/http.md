# HTTPサービスの利用

EMQXは、パスワード認証に外部HTTPサービスを利用することをサポートしています。有効化すると、クライアントが接続要求を開始した際に、EMQXは受け取った情報を使ってHTTPリクエストを構築し、クエリ結果に基づいて接続要求の受け入れを判断することで、複雑な認証ロジックを実現します。

::: tip 前提条件

[基本的なEMQX認証の概念](../authn/authn.md)についての知識

:::

## HTTPリクエストとレスポンス

認証プロセスはHTTP APIコールに似ており、EMQXはリクエストクライアントとして「API」が要求する形式でHTTPサービスにリクエストを構築・送信し、HTTPサービスは「クライアント」が要求する形式で結果を返します。

- レスポンスのエンコード形式 `content-type` は `application/json` でなければなりません。
- 認証結果はボディ内の `result` で示し、値は `allow`、`deny`、`ignore` のいずれかです。
- スーパーユーザーはボディ内の `is_superuser` で示し、値は `true` または `false` です。
- EMQX v5.7.0以降、オプションの `client_attrs` フィールドで[クライアント属性](../../client-attributes/client-attributes.md)を設定できます。キーと値は両方とも文字列である必要があります。
- EMQX v5.8.0以降、レスポンスボディにオプションの `acl` フィールドを設定してクライアントの権限を指定できます。詳細は[アクセスコントロールリスト（ACL）](./acl.md)を参照してください。
- EMQX v5.8.0以降、レスポンスボディにオプションの `expire_at` フィールドを設定してクライアント認証の有効期限を指定できます。これによりクライアントは切断され、再接続時に再認証が強制されます。値は秒単位のUnixタイムスタンプです。
- HTTPレスポンスのステータスコードは `200` または `204` が望ましく、`4xx/5xx` のステータスコードが返された場合はボディを無視し、結果を `ignore` と判断して認証チェーンを継続します。

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

::: tip EMQX 4.x 互換性について

EMQX 4.xではHTTPステータスコードのみが使用され、ボディは破棄されます。例えば、`200` は `allow`、`403` は `deny` を意味します。
表現力の不足から、HTTPボディを利用する形に再設計されており、EMQX 5.0とは互換性がありません。

:::

## ダッシュボードでの設定

EMQXダッシュボードを使って関連設定を完了できます。

1. EMQXダッシュボードの左ナビゲーションメニューから **アクセスコントロール** -> **認証** をクリックします。
2. **認証** ページで右上の **作成** をクリックします。
3. **メカニズム** に **パスワードベース** を選択し、**バックエンド** に **HTTPサーバー** を選択して、以下のように **設定** タブに移動します。

<img src="./assets/authn-http.png" alt="HTTP" style="zoom:67%;" />

4. 以下の指示に従い認証バックエンドを設定します：

   - **メソッド**：HTTPリクエストメソッドを選択します。選択肢は `get`、`post` です。

     :::tip

     `POST` メソッドの使用を推奨します。`GET` メソッドを使用すると、平文パスワードなどの機密情報がHTTPサーバーログに露出する可能性があります。また、信頼できない環境ではHTTPSを使用してください。
      :::

   - **URL**：HTTPサービスのURLアドレスを入力します。
   - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)で、このHTTPサーバー認証器をクライアント接続に適用するかどうかを制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証器が呼び出されます。それ以外の場合はスキップされます。詳細は[認証器の前提条件](./authn.md#authenticator-preconditions)を参照してください。
   - **ヘッダー**（オプション）：HTTPリクエストヘッダー。複数追加可能です。キーと値は[プレースホルダー](./authn.md#authentication-placeholders)を使用できます。
   - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md)を参照してください。
   - **ボディ**：リクエストテンプレート。`POST` リクエストの場合はJSON形式でリクエストボディに送信され、`GET` リクエストの場合はURLのクエリ文字列としてエンコードされます。キーと値は[プレースホルダー](./authn.md#authentication-placeholders)を使用可能です。
   - **詳細設定**：
     - **プールサイズ**（オプション）：EMQXノードからHTTPサーバーへの同時接続数を整数で指定します。デフォルトは `8` です。

     - **接続タイムアウト**（オプション）：EMQXが接続タイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。

     - **HTTPパイプライン**（オプション）：レスポンスを待たずに送信可能な最大HTTPリクエスト数を正の整数で指定します。デフォルトは `100` です。

     - **リクエストタイムアウト**（オプション）：EMQXがリクエストタイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。

5. 設定が完了したら **作成** をクリックします。

## 設定項目による設定

EMQXの設定項目を使ってHTTP認証器を設定できます。 <!--詳細は[authn-http:post](../../configuration/configuration-manual.html#authn-http:post)および[authn-http:get](../../configuration/configuration-manual.html#authn-http:get)を参照してください。-->

以下はHTTPの `POST` と `GET` リクエスト例です：

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
