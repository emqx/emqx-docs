# HTTP サービスの利用

EMQX は、パスワード認証に外部の HTTP サービスを利用することをサポートしています。有効化すると、クライアントが接続リクエストを開始した際に、EMQX は受け取った情報を基に HTTP リクエストを構築し、クエリ結果に基づいてリクエストの受け入れ可否を判断し、複雑な認証ロジックを実現します。

::: tip 前提条件

[EMQX 認証の基本概念](../authn/authn.md)の知識が必要です。

:::

## HTTP リクエストとレスポンス

認証プロセスは HTTP API コールに似ており、EMQX はリクエストクライアントとして「API」が要求する形式で HTTP サービスにリクエストを構築・送信し、HTTP サービスは「クライアント」が要求する形式で結果を返します。

- レスポンスのエンコード形式 `content-type` は `application/json` である必要があります。
- 認証結果はボディ内の `result` で示し、オプション値は `allow`、`deny`、`ignore` です。
- スーパーユーザーはボディ内の `is_superuser` で示し、オプション値は `true`、`false` です。
- EMQX v5.7.0 以降では、オプションの `client_attrs` フィールドを使って[クライアント属性](../../client-attributes/client-attributes.md)を設定できます。キーと値は両方とも文字列である必要があります。
- EMQX v5.8.0 以降では、レスポンスボディにオプションの `acl` フィールドを設定してクライアントの権限を指定できます。詳細は[アクセスコントロールリスト（ACL）](./acl.md)を参照してください。
- EMQX v5.8.0 以降では、レスポンスボディにオプションの `expire_at` フィールドを設定してクライアント認証の有効期限を指定できます。これによりクライアントは切断され、再接続時に再認証が必要になります。値は秒単位の Unix タイムスタンプです。
- HTTP レスポンスのステータスコードは `200` または `204` であるべきです。`4xx/5xx` のステータスコードが返された場合はボディを無視し、結果は `ignore` と判定され認証チェーンは継続されます。

レスポンス例：

```js
HTTP/1.1 200 OK
Headers: Content-Type: application/json
...
Body:
{
    "result": "allow", // "allow" | "deny" | "ignore"
    "is_superuser": false, // オプション値: true | false、デフォルトは false
    "client_attrs": { // オプション (v5.7.0 以降)
        "role": "admin",
        "sn": "10c61f1a1f47"
    }
    "expire_at": 1654254601, // オプション (v5.8.0 以降)
    "acl": // オプション (v5.8.0 以降)
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

EMQX 4.x では HTTP ステータスコードのみを使用し、ボディは破棄されます。例えば `200` は `allow`、`403` は `deny` を意味します。
表現力の不足により、HTTP ボディを活用する形に再設計されており、EMQX 5.0 とは互換性がありません。

:::

## ダッシュボードでの設定

EMQX ダッシュボードを使って関連設定を行うことができます。

1. EMQX ダッシュボードの左ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページで右上の **作成** をクリックします。
3. **メカニズム** に **パスワードベース** を、**バックエンド** に **HTTP サーバー** を選択し、**設定** タブに移動します。以下のように表示されます。

<img src="./assets/authn-http.png" alt="HTTP" style="zoom:67%;" />

4. 以下の指示に従って認証バックエンドを設定します：

   - **メソッド**：HTTP リクエストメソッドを選択します。選択肢は `get`、`post` です。

     :::tip

     `POST` メソッドの利用を推奨します。`GET` メソッドを使用すると、パスワードの平文などの機密情報が HTTP サーバーログに露出する可能性があります。また、信頼できない環境では HTTPS の利用を推奨します。

     :::

   - **URL**：HTTP サービスの URL アドレスを入力します。
   - **前提条件**：[Variform 式](../../configuration/configuration.md#variform-expressions)で、この HTTP サーバー認証器をクライアント接続に適用するかを制御します。式はクライアントの属性（`username`、`clientid`、`listener` など）に対して評価され、結果が文字列 `"true"` の場合のみ認証器が呼び出されます。そうでなければスキップされます。詳細は[認証器の前提条件](./authn.md#authenticator-preconditions)を参照してください。
   - **ヘッダー**（オプション）：HTTP リクエストヘッダー。複数のヘッダーを追加可能です。キーと値は[プレースホルダー](./authn.md#authentication-placeholders)を利用できます。
   - **TLS を有効化**：TLS を有効にする場合はトグルスイッチをオンにします。TLS 有効化の詳細は[ネットワークと TLS](../../network/overview.md)を参照してください。
   - **ボディ**：リクエストテンプレート。`POST` リクエストの場合は JSON 形式でリクエストボディに送信され、`GET` リクエストの場合は URL のクエリ文字列にエンコードされます。マッピングのキーと値は[プレースホルダー](./authn.md#authentication-placeholders)を利用可能です。
   - **詳細設定**：
     - **プールサイズ**（オプション）：EMQX ノードから HTTP サーバーへの同時接続数を整数で指定します。デフォルトは `8`。
     - **接続タイムアウト**（オプション）：EMQX が接続タイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。
     - **HTTP パイプライニング**（オプション）：レスポンスを待たずに送信可能な最大 HTTP リクエスト数を正の整数で指定します。デフォルト値は `100`。
     - **リクエストタイムアウト**（オプション）：EMQX がリクエストタイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。

5. 設定が完了したら **作成** をクリックします。

## 設定ファイルによる設定

EMQX の設定項目を使って HTTP 認証器を設定できます。 <!--詳細は [authn-http:post](../../configuration/configuration-manual.html#authn-http:post) と [authn-http:get](../../configuration/configuration-manual.html#authn-http:get) を参照してください。-->

以下は HTTP の `POST` と `GET` リクエストの例です：

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
