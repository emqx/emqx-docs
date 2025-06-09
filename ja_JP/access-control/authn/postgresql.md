# PostgreSQLとの統合

EMQXはパスワード認証のためにPostgreSQLとの統合をサポートしています。

::: tip

[EMQX認証の基本概念](../authn/authn.md)の知識が必要です。

:::

## データスキーマとクエリ文

EMQXのPostgreSQL認証機能はほぼあらゆるストレージスキーマに対応しています。  
認証情報の保存方法やアクセス方法はビジネス要件に応じて自由に設計可能で、単一または複数のテーブルやビューなどを利用できます。

ユーザーはクエリ文のテンプレートを用意し、以下のフィールドを含める必要があります。

- `password_hash`：必須。データベースに保存されているパスワード（プレーンテキストまたはハッシュ値）。  
- `salt`：任意。`salt = ""` またはこのフィールドを省略すると、ソルト値が付加されないことを示します。  
- `is_superuser`：任意。現在のクライアントがスーパーユーザーかどうかのフラグ。デフォルトは `false`。

認証情報を保存するためのテーブル構造の例：

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
上記の例では、クエリに役立つ暗黙の `UNIQUE` インデックス（username）が作成されています。  
システム内のユーザー数が多い場合は、クエリの応答時間を短縮しEMQXへの負荷を軽減するために、事前にテーブルの最適化とインデックス設定を行ってください。
:::

このテーブルでは、MQTTユーザーは `username` で識別されます。

例えば、スーパーユーザー（`is_superuser`: `true`）として、ユーザー名 `user123`、パスワード `secret`、ソルト `salt` を持つレコードを追加する場合、クエリ文は以下のようになります。

```bash
INSERT INTO mqtt_user(username, password_hash, salt, is_superuser) VALUES ('user123', 'f84fa2149dbb62ed4e0cf1f550d2949b33a6513d3a7707e08502511c79ccb0ee', 'salt', true);
INSERT 0 1
```

対応する設定パラメータは以下の通りです。

- password_hash_algorithm: `sha256`  
- salt_position: `suffix`

SQL例：

```sql
query = "SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} LIMIT 1"
```

## ダッシュボードでの設定

EMQXダッシュボードを使って、PostgreSQLをパスワード認証に利用する設定が可能です。

1. EMQXダッシュボードの左側ナビゲーションメニューから **Access Control** -> **Authentication** をクリックします。  
2. **Authentication** ページの右上にある **Create** をクリックします。  
3. **Mechanism** に **Password-Based** を選択し、**Backend** に **PostgreSQL** を選択すると、以下のように **Configuration** タブが表示されます。

<img src="./assets/authn-postgresql.png" alt="PostgreSQLによる認証" style="zoom:67%;" />

4. 以下の手順に従い認証バックエンドを設定します。  
   - PostgreSQLへの接続情報を入力します。

     - **Server**：EMQXが接続するサーバーのアドレス（`host:port`）を指定します。  
     - **Database**：PostgreSQLのデータベース名。  
     - **Username**：ユーザー名を指定します。  
     - **Password**：ユーザーパスワードを指定します。  
   - 認証に関する設定を行います。  
     - **Password Hash**：プレーンテキストのパスワードに適用され、データベースに保存される前のハッシュアルゴリズムを選択します。選択肢は `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります。  
       - `md5`、`sha`、`sha256`、`sha512` の場合：  
         - **Salt Position**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`、`prefix`、`disable` のいずれかです。外部ストレージからEMQX内蔵データベースへユーザー認証情報を移行する場合を除き、デフォルト値のままで問題ありません。  
         - ハッシュ結果は16進数文字列で表現され、大文字小文字を区別せずに保存済みの認証情報と比較されます。  
       - `plain` の場合：  
         - **Salt Position** は `disable` に設定してください。  
       - `bcrypt` の場合：  
         - **Salt Rounds**：ハッシュ関数を適用する回数を2のべき乗で表す「コストファクター」です。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ強化のためにはより高い値を推奨します。コストファクターを1増やすと認証にかかる時間が約2倍になります。  
       - `pbkdf2` の場合：  
         - **Pseudorandom Function**：鍵生成に使うハッシュ関数を選択します（例：`sha256`）。  
         - **Iteration Count**：ハッシュ関数の実行回数。デフォルトは `4096`。  
         - **Derived Key Length**（任意）：生成される鍵のバイト長。空欄の場合は選択した擬似乱数関数により決定されます。  
         - ハッシュ結果は16進数文字列で表現され、大文字小文字を区別せずに保存済みの認証情報と比較されます。  
   - **Precondition**：[Variform式](../../configuration/configuration.md#variform-expressions)で記述し、このPostgreSQL認証機能をクライアント接続に適用するか制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証処理が実行されます。それ以外の場合はスキップされます。詳細は[認証の事前条件](./authn.md#authentication-preconditions)をご覧ください。  
   - **Enable TLS**：TLSを有効にする場合はスイッチをオンにします。TLSの有効化については[ネットワークとTLS](../../network/overview.md)を参照してください。  
   - **Advanced Settings**：  
     - **Connection Pool size**（任意）：EMQXノードからPostgreSQLサーバーへの同時接続数を指定します。デフォルトは `8`。  
     - **Disable Prepared Statements**（任意）：PGBouncerのトランザクションモードやSupabaseなど、プリペアドステートメントをサポートしないPostgreSQLサービスを利用する場合に有効にします。このオプションはEMQX v5.7.1で追加されました。  
   - **SQL**：データスキーマに合わせてクエリ文を入力します。詳細は[SQLデータスキーマとクエリ文](#データスキーマとクエリ文)をご参照ください。

設定が完了したら、**Create** をクリックしてください。

## 設定項目による設定

EMQXの設定項目を使ってPostgreSQL認証を設定することも可能です。  
<!-- 詳細な操作手順は[authn-postgresql:authentication](../../configuration/configuration-manual.html#authn-postgresql:authentication)をご覧ください。 -->

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
}
```
