# Client-Info 認証

Client-Info 認証（`cinfo` タイプ）は、クライアントのプロパティや属性をユーザー定義のルールに基づいて検証する軽量な認証方式です。これらのルールは Variform 式を用いてマッチング条件を定義し、条件に合致した場合の認証結果を決定します。例えば、ユーザー名が空のクライアントを素早くブロックするには、条件として `str_eq(username, '')` を使用し、結果を `deny` に設定します。

## ダッシュボードでの Client-Info 認証の設定

1. EMQX ダッシュボードの左メニューで **アクセス制御** -> **認証** に移動し、**認証** ページを開きます。
2. 右上の **作成** をクリックし、**メカニズム** に **Client Info** を選択します。Client-Info 認証はバックエンドの選択を必要としないため、**次へ** をクリックして **パラメータの設定** ステップに進みます。
3. 以下の手順に従ってバックエンドを設定します：
   - **前提条件**：この Client Info 認証器をクライアント接続に適用するかどうかを制御するための [Variform 式](../../configuration/configuration.md#variform-expressions) です。この式はクライアントの属性（`username`、`clientid`、`listener` など）に対して評価されます。式が文字列 `"true"` を返す場合にのみ認証器が呼び出され、それ以外の場合はスキップされます。前提条件の詳細は [認証の前提条件](./authn.md#authentication-preconditions) を参照してください。
   - **チェック** の **追加** をクリックします。
     - **マッチ条件** 入力欄に、クライアント情報とマッチングするための Variform 式を入力します。複数の式がある場合は、それぞれを改行して入力してください。すべての式が `true` を返すと認証器は該当する結果を返し、そうでなければ現在のチェックはスキップされます。式で使用可能な変数は以下の通りです：
       - `username`：ユーザー名
       - `clientid`：クライアントID
       - `client_attrs.*`：クライアント属性
       - `peerhost`：クライアントのIPアドレス
       - `cert_subject`：TLS証明書のサブジェクト
       - `cert_common_name`：TLS証明書のコモンネーム
     - **結果** のドロップダウンメニューから、マッチ条件が真の場合に返す結果を選択します：
       - `allow`：クライアントの接続を許可します。
       - `ignore`：認証をチェーン内の次の認証器に委ねます。
       - `deny`：クライアントの接続を拒否します。
4. **作成** をクリックして認証設定を完了します。

## 設定項目による Client-Info 認証の設定

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
        # is_match が配列の場合、すべてのチェックが 'true' の場合に 'true' を返す
        is_match = ["str_eq(username, '')", "str_eq(nth(1,tokens(clientid,'-')), 'v1')"]
        result = deny
      }
      # すべてのチェックが 'allow' または 'deny' を返さなかった場合は次の認証器へ進む
    ]
  },
  # ... 他の認証器 ...
  # ...
  # すべての認証器が 'allow' または 'deny' を返さなかった場合、クライアントは拒否される
]
```

その他のマッチ式の例：

- すべてのクライアントにマッチ：`true`
- TLS証明書のコモンネームがユーザー名と一致するクライアントにマッチ：`str_eq(cert_common_name, username)`
- パスワードが環境変数 `EMQXVAR_SECRET` とクライアントIDを連結した文字列の `sha1` ハッシュと一致するクライアントにマッチ：`str_eq(password, hash(sha1, concat([clientid, getenv('SECRET')])))`
- 属性 `client_attrs.group` が `g0` でないクライアントにマッチ：`str_neq(client_attrs.group, 'g0')`
- クライアントIDがゾーン名で始まるクライアントにマッチ：`regex_match(clientid, concat(['^', zone, '.+$']))`
