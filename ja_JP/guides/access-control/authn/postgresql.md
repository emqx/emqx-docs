# PostgreSQLとの統合

EMQXはパスワード認証のためにPostgreSQLとの統合をサポートしています。

::: tip

[基本的なEMQX認証の概念](../authn/authn.md)の知識が必要です。

:::

## データスキーマとクエリ文

EMQXのPostgreSQL認証機能はほぼすべてのストレージスキーマに対応しています。ビジネス要件に応じて、認証情報の保存方法やアクセス方法を自由に決定できます。例えば、1つまたは複数のテーブルやビューを使用することが可能です。

ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認する必要があります。

- `password_hash`：必須。データベースに保存されているパスワード（平文またはハッシュ化済み）。
- `salt`：任意。`salt = ""` またはこのフィールドを削除すると、ソルト値が追加されないことを示します。
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
上記の例では、暗黙の`UNIQUE`インデックス（username）が作成されており、クエリの効率化に役立ちます。
システム内のユーザー数が多い場合は、クエリ応答時間の短縮とEMQXの負荷軽減のために、事前にテーブルの最適化やインデックス作成を行ってください。
:::

このテーブルでは、MQTTユーザーは`username`で識別されます。

例えば、ユーザー名`user123`、パスワード`secret`、ソルト`salt`を持つスーパーユーザー（`is_superuser`: `true`）のドキュメントを追加したい場合、クエリ文は以下のようになります。

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

EMQXダッシュボードを使って、PostgreSQLをパスワード認証に利用する設定が可能です。

1. EMQXダッシュボードの左側ナビゲーションメニューで **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム** に **パスワードベース** を、**バックエンド** に **PostgreSQL** を選択し、**設定** タブに進みます。以下のように表示されます。

<img src="./assets/authn-postgresql.png" alt="PostgreSQLによる認証" style="zoom:67%;" />

4. 以下の手順に従って認証バックエンドを設定します。
   - PostgreSQLへの接続情報を入力します。

     - **サーバー**：EMQXが接続するサーバーのアドレス（`host:port`）を指定します。
     - **データベース**：PostgreSQLのデータベース名。
     - **ユーザー名**：ユーザー名を指定します。
     - **パスワード**：ユーザーパスワードを指定します。
   - 認証に関する設定を行います。
     - **パスワードハッシュ**：平文パスワードに適用し、結果をデータベースに保存するハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります。
       - `md5`、`sha`、`sha256`、`sha512` の場合：
         - **ソルト位置**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`、`prefix`、`disable` のいずれかです。外部ストレージからEMQX組み込みデータベースへユーザー認証情報を移行しない限り、デフォルト値のままで問題ありません。
         - ハッシュ結果は16進数文字列で表され、大文字・小文字を区別せずに保存された認証情報と比較されます。
       - `plain` の場合：
         - **ソルト位置** は `disable` に設定してください。
       - `bcrypt` の場合：
         - **ソルトラウンド**：ハッシュ関数を適用する回数を2の累乗で表した「コストファクター」です。デフォルトは`10`で、許容範囲は`5`から`10`です。セキュリティ強化のためには高い値を推奨します。注：コストファクターを1増やすと認証にかかる時間が倍増します。
       - `pbkdf2` の場合：
         - **疑似乱数関数**：鍵生成に使うハッシュ関数を選択します（例：`sha256`）。
         - **反復回数**：ハッシュ関数を実行する回数。デフォルトは`4096`です。
         - **派生鍵長**（任意）：生成される鍵のバイト長を指定します。空欄の場合は選択した疑似乱数関数のデフォルト長になります。
         - ハッシュ結果は16進数文字列で表され、大文字・小文字を区別せずに保存された認証情報と比較されます。
   - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)で、このPostgreSQL認証機能をクライアント接続に適用するか制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列の `"true"` の場合のみ認証機能が呼び出されます。それ以外の場合はスキップされます。詳細は[認証機能の前提条件](./authn.md#authenticator-preconditions)をご覧ください。
   - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md)をご参照ください。
   - **詳細設定**：
     - **コネクションプールサイズ**（任意）：EMQXノードからPostgreSQLサーバーへの同時接続数を指定します。デフォルトは`8`です。
     - **プリペアドステートメント無効化**（任意）：PGBouncerのトランザクションモードやSupabaseなど、プリペアドステートメントをサポートしないPostgreSQLサービスを利用する場合はこのオプションを有効にしてください。このオプションはEMQX v5.7.1で追加されました。
   - **SQL**：データスキーマに応じたクエリ文を入力します。詳細は[SQLデータスキーマとクエリ文](#sql-table-structure-and-query-statement)をご覧ください。

設定が完了したら、**作成** をクリックします。

## 設定項目による構成

EMQXの設定項目を使ってPostgreSQL認証機能を構成することも可能です。<!-- 詳細な操作手順は[authn-postgresql:authentication](../../configuration/configuration-manual.html#authn-postgresql:authentication)をご参照ください。 -->

PostgreSQL認証は `mechanism = password_based` および `backend = postgresql` で識別されます。

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
