# PostgreSQLとの統合

EMQXはパスワード認証のためにPostgreSQLとの統合をサポートしています。

::: tip

[基本的なEMQX認証の概念](../authn/authn.md)についての知識

:::

## データスキーマとクエリ文

EMQXのPostgreSQL認証機能はほぼあらゆるストレージスキーマに対応しています。ビジネスニーズに応じて、資格情報の保存方法やアクセス方法を自由に決めることができます。たとえば、1つまたは複数のテーブルやビューを使用するなどが可能です。

ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認してください。

- `password_hash`：必須。データベースに保存されているパスワード（平文またはハッシュ化されたもの）。
- `salt`：任意。`salt = ""` またはこのフィールドを削除すると、ソルト値が追加されないことを示します。
- `is_superuser`：任意。現在のクライアントがスーパーユーザーかどうかを示すフラグ。デフォルトは `false`。

資格情報を保存するためのテーブル構造の例：

```sql
CREATE TABLE mqtt_user (
    id serial PRIMARY KEY,
    username text NOT NULL UNIQUE,
    password_hash  text NOT NULL,
    salt text NOT NULL,
    is_superuser boolean DEFAULT false,
    created timestamp with time zone DEFAULT NOW()
);
```

::: tip
上記の例では、クエリに役立つ暗黙の `UNIQUE` インデックスフィールド（username）が作成されています。
システム内に多数のユーザーが存在する場合は、クエリの応答時間を短縮しEMQXの負荷を軽減するために、事前にテーブルの最適化とインデックス付けを行ってください。
:::

このテーブルでは、MQTTユーザーは `username` によって識別されます。

たとえば、スーパーユーザー（`is_superuser`: `true`）でユーザー名が `user123`、パスワードが `secret`、ソルトが `salt` のドキュメントを追加したい場合、クエリ文は以下のようになります。

```bash
INSERT INTO mqtt_user(username, password_hash, salt, is_superuser) VALUES ('user123', 'f84fa2149dbb62ed4e0cf1f550d2949b33a6513d3a7707e08502511c79ccb0ee', 'salt', true);
INSERT 0 1
```

対応する設定パラメータは以下の通りです。

- password_hash_algorithm: `sha256`
- salt_position: `suffix`

SQL:

```sql
query = "SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} LIMIT 1"
```

## ダッシュボードでの設定

EMQXダッシュボードを使って、PostgreSQLをパスワード認証に利用する方法を設定できます。

1. EMQXダッシュボードの左側ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム** に **パスワードベース** を選択し、**バックエンド** に **PostgreSQL** を選択すると、以下のように **設定** タブに移動します。

<img src="./assets/authn-postgresql.png" alt="PostgreSQLによる認証" style="zoom:67%;" />

4. 以下の手順に従って認証バックエンドを設定します。
   - PostgreSQLへの接続情報を入力します。

     - **サーバー**：EMQXが接続するサーバーアドレス（`host:port`）を指定します。
     - **データベース**：PostgreSQLのデータベース名。
     - **ユーザー名**：ユーザー名を指定します。
     - **パスワード**：ユーザーパスワードを指定します。
   - 認証に関する設定を行います。
     - **パスワードハッシュ**：平文パスワードに適用され、結果がデータベースに保存されるハッシュアルゴリズムを選択します。選択肢は `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります。
       - `md5`、`sha`、`sha256`、`sha512` の場合：
         - **ソルト位置**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`、`prefix`、`disable` のいずれかです。外部ストレージからEMQX組み込みデータベースにユーザー資格情報を移行しない限り、デフォルト値のままで問題ありません。
         - 結果のハッシュは16進数文字列で表され、大文字小文字を区別せずに保存された資格情報と比較されます。
       - `plain` の場合：
         - **ソルト位置**：`disable` に設定してください。
       - `bcrypt` の場合：
         - **ソルトラウンド**：ハッシュ関数が適用される回数を定義します。値は _2のソルトラウンド乗_（コストファクター）で表されます。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ強化のためにはより高い値が推奨されます。注意：コストファクターを1増やすごとに認証にかかる時間は倍増します。
       - `pbkdf2` の場合：
         - **疑似乱数関数**：キー生成に使うハッシュ関数を選択します（例：`sha256`）。
         - **反復回数**：ハッシュ関数の実行回数を設定します。デフォルトは `4096`。
         - **派生キー長**（任意）：生成されるキーのバイト長を指定します。空欄の場合は選択した疑似乱数関数のデフォルト長になります。
         - 結果のハッシュは16進数文字列で表され、大文字小文字を区別せずに保存された資格情報と比較されます。
   - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)で、このPostgreSQL認証機能をクライアント接続に適用するかどうかを制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証機能が呼び出されます。それ以外の場合はスキップされます。詳細は[認証機能の前提条件](./authn.md#authenticator-preconditions)を参照してください。
   - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md#tls-for-external-resource-access)を参照してください。
   - **詳細設定**：接続プール、タイムアウト、プリペアドステートメントの動作を設定します。
     - **接続プールサイズ**（任意）：EMQXノードからPostgreSQLサーバーへの同時接続数を指定します。デフォルトは `8`。
     - **クエリタイムアウト**（任意）：EMQXがクエリのタイムアウトと判断するまでの待機時間を指定します。ミリ秒、秒、分、時間の単位が利用可能です。デフォルトは `5` 秒。
     - **接続タイムアウト**（任意）：EMQXが接続試行のタイムアウトと判断するまでの待機時間を指定します。ミリ秒、秒、分、時間の単位が利用可能です。デフォルトは `15` 秒。
     - **プリペアドステートメントを無効化**（任意）：データベースクエリでプリペアドステートメントの使用を無効にします。PostgreSQLのプロキシやミドルウェア（例：PGBouncerやSupabaseのトランザクションモード）がセッションレベルの機能（プリペアドステートメントなど）をサポートしていない場合に有効にしてください。デフォルトは無効。
   - **SQL**：データスキーマに従ってクエリ文を入力します。詳細は[SQLデータスキーマとクエリ文](#データスキーマとクエリ文)を参照してください。

設定が完了したら、**作成** をクリックします。

## 設定項目による設定

EMQXの設定項目を使ってPostgreSQL認証機能を設定することもできます。設定パラメータの完全な一覧は[EMQX Enterprise設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)を参照してください。

PostgreSQL認証は `mechanism = password_based` と `backend = postgresql` で識別されます。

設定例：

```bash
{
  mechanism = password_based
  backend = postgresql

  password_hash_algorithm {
    name = sha256
    salt_position = suffix
  }

  database = mqtt
  username = postgres
  password = public
  server = "127.0.0.1:5432"
  query = "SELECT password_hash, salt, is_superuser FROM users where username = ${username} LIMIT 1"
  query_timeout = "5s"
  connect_timeout = "15s"
  disable_prepared_statements = false
}
```
