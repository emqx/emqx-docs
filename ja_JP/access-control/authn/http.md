# HTTPサービスの利用

EMQXは、外部HTTPサービスを用いたパスワード認証をサポートしています。有効化すると、クライアントが接続リクエストを開始した際に、EMQXは受け取った情報をもとにHTTPリクエストを構築し、クエリ結果に基づいてリクエストを受け入れるかどうかを判定し、複雑な認証ロジックを実現します。

::: tip 前提条件

[EMQX認証の基本概念](../authn/authn.md)の知識が必要です。

:::

## HTTPリクエストとレスポンス

認証プロセスはHTTP APIコールに似ており、EMQXはリクエストクライアントとして「API」が要求する形式でHTTPサービスへリクエストを構築・送信し、HTTPサービスは「クライアント」が要求する形式で結果を返します。

- レスポンスのエンコード形式 `content-type` は `application/json` でなければなりません。
- 認証結果はボディ内の `result` で示し、値は `allow`、`deny`、`ignore` のいずれかです。
- スーパーユーザーはボディ内の `is_superuser` で示し、値は `true` または `false` です。
- EMQX v5.7.0以降、オプションの `client_attrs` フィールドで[クライアント属性](../../client-attributes/client-attributes.md)を設定可能です。キーと値は両方とも文字列である必要があります。
- EMQX v5.8.0以降、レスポンスボディにオプションの `acl` フィールドを設定してクライアントの権限を指定できます。詳細は[アクセス制御リスト（ACL）](./acl.md)を参照してください。
- EMQX v5.8.0以降、レスポンスボディにオプションの `expire_at` フィールドを設定してクライアント認証の有効期限を指定できます。これによりクライアントは切断され、再接続時に再認証が強制されます。値は秒単位のUnixタイムスタンプです。
- HTTPレスポンスのステータスコードは `200` または `204` が望ましく、`4xx` / `5xx` のステータスコードが返された場合はボディを無視し、結果を `ignore` として認証チェーンを継続します。

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

::: tip EMQX 4.x互換性について

EMQX 4.xではHTTPステータスコードのみを使用し、ボディは破棄されます。例えば `200` は `allow`、`403` は `deny` を意味します。
表現力が不足していたため、HTTPボディを利用する形に再設計されており、EMQX 5.0とは互換性がありません。

:::

## ダッシュボードでの設定

EMQXダッシュボードを使って関連設定を完了できます。

1. EMQXダッシュボードの左ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム** に **パスワードベース** を、**バックエンド** に **HTTPサーバー** を選択し、**設定** タブに移動します。以下のように表示されます。

<img src="./assets/authn-http.png" alt="HTTP" style="zoom:67%;" />

4. 以下の指示に従って認証バックエンドを設定します：

   - **メソッド**：HTTPリクエストメソッドを選択します。選択肢は `get`、`post` です。

     :::tip

     `POST` メソッドの使用を推奨します。`GET` メソッドを使用すると、パスワードなどの機密情報がHTTPサーバーログに平文で記録される可能性があります。また、信頼できない環境ではHTTPSを使用してください。

     :::

   - **URL**：HTTPサービスのURLアドレスを入力します。
   - **前提条件**：このHTTPサーバー認証器をクライアント接続に適用するか制御するための[Variform式](../../configuration/configuration.md#variform-expressions)です。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価されます。式の評価結果が文字列 `"true"` の場合のみ認証器が呼び出され、それ以外はスキップされます。詳細は[認証器の前提条件](./authn.md#authenticator-preconditions)を参照してください。
   - **ヘッダー**（オプション）：HTTPリクエストヘッダーです。複数追加可能で、キーと値には[プレースホルダー](./authn.md#authentication-placeholders)が利用できます。
   - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md)を参照してください。
   - **ボディ**：リクエストテンプレートです。`POST` リクエストの場合はJSON形式でリクエストボディに送信され、`GET` リクエストの場合はURLのクエリ文字列にエンコードされます。キーと値には[プレースホルダー](./authn.md#authentication-placeholders)が利用可能です。
   - **詳細設定**：
     - **プールサイズ**（オプション）：EMQXノードからHTTPサーバーへの同時接続数を整数で指定します。デフォルトは `8` です。

     - **接続タイムアウト**（オプション）：EMQXが接続タイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。

     - **HTTPパイプライニング**（オプション）：レスポンスを待たずに送信可能な最大HTTPリクエスト数を正の整数で指定します。デフォルトは `100` です。

     - **リクエストタイムアウト**（オプション）：EMQXがリクエストタイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。

5. 設定が完了したら **作成** をクリックします。

## 設定項目による設定

EMQXの設定項目でHTTP認証器を設定できます。<!--詳細は[authn-http:post](../../configuration/configuration-manual.html#authn-http:post)および[authn-http:get](../../configuration/configuration-manual.html#authn-http:get)を参照してください。-->

以下はHTTPの `POST` および `GET` リクエストの例です：

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
