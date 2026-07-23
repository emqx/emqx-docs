# Client-Info 認証

Client-Info 認証（`cinfo` タイプ）は、クライアントのプロパティや属性をユーザー定義のルールに基づいて検証する軽量な認証機構です。これらのルールは Variform 式を用いてマッチング条件を定義し、条件に一致した場合の認証結果を決定します。例えば、ユーザー名が空のクライアントを素早くブロックするには、`str_eq(username, '')` という条件を使い、結果を `deny` に設定します。

## ダッシュボードでの Client-Info 認証の設定

1. EMQX ダッシュボードの左メニューから **アクセス制御** -> **認証** に移動し、**認証** ページを開きます。
2. 右上の **作成** をクリックし、**メカニズム** にて **Client Info** を選択します。Client-Info 認証はバックエンドの選択が不要なため、**次へ** をクリックして **パラメータの設定** に進みます。
3. 以下の手順でバックエンドを設定します：
   - **前提条件**：この Client Info 認証器をクライアント接続に適用するかどうかを制御するための [Variform 式](../../configuration/configuration.md#variform-expressions) です。この式はクライアントの属性（`username`、`clientid`、`listener` など）に対して評価されます。式の評価結果が文字列 `"true"` の場合のみ認証器が呼び出され、それ以外はスキップされます。前提条件の詳細は [Authentication Preconditions](./authn.md#authentication-preconditions) を参照してください。
   - **Checks** で **追加** をクリックします。
     - **Match Conditions** 入力欄にクライアント情報をマッチングするための Variform 式を入力します。複数の式がある場合は改行してそれぞれ入力してください。すべての式が `true` を返した場合に認証器は該当する結果を返し、それ以外は現在のチェックをスキップします。式で使用可能な変数は以下の通りです：
       - `username`：ユーザー名
       - `clientid`：クライアントID
       - `client_attrs.*`：クライアント属性
       - `peerhost`：クライアントのIPアドレス
       - `cert_subject`：TLS証明書のサブジェクト
       - `cert_common_name`：TLS証明書のコモンネーム
     - **Result** のドロップダウンメニューから、マッチ条件が真の場合に返す結果を選択します：
       - `allow`：クライアントの接続を許可します。
       - `ignore`：認証をチェーン内の次の認証器に委ねます。
       - `deny`：クライアントの接続を拒否します。
4. **作成** をクリックして認証設定を完了します。

## 設定ファイルでの Client-Info 認証の設定例

以下は Client-Info 認証器の設定例です：

```bash
authentication = [
  {
    mechanism = cinfo
    checks = [
      # ユーザー名が 'super-' で始まるクライアントを許可
      {
        is_match = "regex_match(username, '^super-.+$')"
        result = allow
      },
      # ユーザー名が空でクライアントIDが 'v1-' で始まるクライアントを拒否
      {
        # is_match が配列の場合、すべてのチェックが 'true' のときに 'true' を返す
        is_match = ["str_eq(username, '')", "str_eq(nth(1,tokens(clientid,'-')), 'v1')"]
        result = deny
      }
      # すべてのチェックを試しても 'allow' または 'deny' が返らない場合は次の認証器へ進む
    ]
  },
  # ... 他の認証器 ...
  # ...
  # すべての認証器を試しても 'allow' または 'deny' が返らない場合はクライアントを拒否
]
```

その他のマッチ式の例：

- すべてのクライアントにマッチ：`true`
- TLS証明書のコモンネームがユーザー名と一致するクライアントにマッチ：`str_eq(cert_common_name, username)`
- パスワードが環境変数 `EMQXVAR_SECRET` とクライアントIDを連結した文字列の `sha1` ハッシュと一致するクライアントにマッチ：
  `str_eq(password, hash(sha1, concat([clientid, getenv('SECRET')])))`
- 属性 `client_attrs.group` が `g0` でないクライアントにマッチ：
  `str_neq(client_attrs.group, 'g0')`
- クライアントIDがゾーン名で始まるクライアントにマッチ：
  `regex_match(clientid, concat(['^', zone, '.+$']))`
