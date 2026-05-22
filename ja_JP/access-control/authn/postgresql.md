# PostgreSQLとの連携

EMQXは、パスワード認証のためにPostgreSQLとの連携をサポートしています。

::: tip

<<<<<<< HEAD
[基本的なEMQX認証の概念](../authn/authn.md)についての知識
=======
[EMQX認証の基本概念](../authn/authn.md)についての知識
>>>>>>> origin/release-6.1

:::

## データスキーマとクエリ文

<<<<<<< HEAD
EMQXのPostgreSQL認証機能はほぼあらゆるストレージスキーマに対応しています。認証情報の保存方法やアクセス方法は、ビジネスニーズに応じて、単一または複数のテーブル、ビューなどを利用して自由に決定できます。

ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認する必要があります。

- `password_hash`：必須。データベースに保存されているパスワード（平文またはハッシュ化されたもの）。
- `salt`：任意。`salt = ""` またはこのフィールドを削除すると、ソルト値が追加されないことを示します。
- `is_superuser`：任意。現在のクライアントがスーパーユーザーかどうかを示すフラグ。デフォルトは `false`。
=======
EMQXのPostgreSQL認証機能はほぼあらゆるストレージスキーマに対応しています。ビジネスニーズに応じて、認証情報の保存方法やアクセス方法を自由に決めることができます。例えば、1つまたは複数のテーブルやビューを使用することが可能です。

ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを保証する必要があります。

- `password_hash`：必須。データベースに保存されているパスワード（プレーンテキストまたはハッシュ化されたもの）。
- `salt`：任意。`salt = ""` またはこのフィールドを削除するとソルト値が追加されないことを示します。
- `is_superuser`：任意。現在のクライアントがスーパーユーザーかどうかのフラグ。デフォルトは `false`。
>>>>>>> origin/release-6.1

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
<<<<<<< HEAD
上記の例では、クエリに役立つ暗黙の `UNIQUE` インデックス（username）が作成されています。
システム内のユーザー数が多い場合は、クエリ応答時間を短縮しEMQXの負荷を軽減するために、事前にテーブルの最適化やインデックス設定を行ってください。
=======
上記の例では、クエリに役立つ暗黙の`UNIQUE`インデックス（username）が作成されています。
システム内のユーザー数が多い場合は、クエリ応答時間を短縮しEMQXの負荷を軽減するために、事前にテーブルの最適化とインデックス作成を行ってください。
>>>>>>> origin/release-6.1
:::

このテーブルでは、MQTTユーザーは`username`で識別されます。

<<<<<<< HEAD
例えば、スーパーユーザー（`is_superuser`: `true`）で、ユーザー名が `user123`、パスワードが `secret`、ソルトが `salt` のドキュメントを追加したい場合、クエリ文は以下のようになります。
=======
例えば、スーパーユーザー（`is_superuser`: `true`）として、ユーザー名が`user123`、パスワードが`secret`、ソルトが`salt`のドキュメントを追加したい場合、クエリ文は以下のようになります。
>>>>>>> origin/release-6.1

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

1. EMQXダッシュボードの左側ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
<<<<<<< HEAD
3. **メカニズム** に **パスワードベース** を選択し、**バックエンド** に **PostgreSQL** を選択すると、以下のように **設定** タブが表示されます。

<img src="./assets/authn-postgresql.png" alt="PostgreSQLによる認証" style="zoom:67%;" />

4. 以下の手順に従って認証バックエンドを設定します。
=======
3. **メカニズム**に **パスワードベース**、**バックエンド**に **PostgreSQL** を選択し、**設定** タブに進みます。以下のように表示されます。

<img src="./assets/authn-postgresql.png" alt="PostgreSQLによる認証" style="zoom:67%;" />

4. 以下の手順で認証バックエンドを設定します。
>>>>>>> origin/release-6.1
   - PostgreSQLへの接続情報を入力します。

     - **サーバー**：EMQXが接続するサーバーアドレス（`host:port`）を指定します。
     - **データベース**：PostgreSQLのデータベース名。
     - **ユーザー名**：ユーザー名を指定します。
     - **パスワード**：ユーザーパスワードを指定します。
<<<<<<< HEAD
   - 認証に関する設定を行います。
     - **パスワードハッシュ**：平文パスワードに適用され、データベースに保存される前のハッシュアルゴリズムを選択します。選択肢は `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定が必要です。
       - `md5`、`sha`、`sha256`、`sha512` の場合：
         - **ソルト位置**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`、`prefix`、`disable` のいずれかです。外部ストレージからEMQX組み込みデータベースへユーザー認証情報を移行しない限り、デフォルト値のままで問題ありません。
         - ハッシュ結果は16進数文字列として表現され、大文字小文字を区別せずに保存された認証情報と比較されます。
       - `plain` の場合：
         - **ソルト位置** は `disable` に設定してください。
       - `bcrypt` の場合：
         - **ソルトラウンド数**：ハッシュ関数の適用回数を定義します。値は _2のソルトラウンド数乗_ で表され、「コストファクター」とも呼ばれます。デフォルトは `10`、許容範囲は `5` ～ `10` です。セキュリティ強化のためには高い値が推奨されます。注：コストファクターを1増やすと認証にかかる時間が倍増します。
       - `pbkdf2` の場合：
         - **疑似乱数関数**：鍵生成に使用するハッシュ関数を選択します（例：`sha256`）。
         - **反復回数**：ハッシュ関数を実行する回数を設定します。デフォルトは `4096`。
         - **導出鍵長**（任意）：生成される鍵のバイト長を指定します。空欄の場合は選択した疑似乱数関数のデフォルト長になります。
         - ハッシュ結果は16進数文字列として表現され、大文字小文字を区別せずに保存された認証情報と比較されます。
   - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)で、このPostgreSQL認証機能をクライアント接続に適用するか制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証機能が呼び出されます。それ以外の場合はスキップされます。詳細は[認証機能の前提条件](./authn.md#authenticator-preconditions)を参照してください。
   - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md#tls-for-external-resource-access)を参照してください。
   - **詳細設定**：接続プール、タイムアウト、プリペアドステートメントの挙動を設定します。
     - **接続プールサイズ**（任意）：EMQXノードからPostgreSQLサーバーへの同時接続数を指定します。デフォルトは `8`。
     - **クエリタイムアウト**（任意）：EMQXがクエリのタイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。デフォルトは `5` 秒。
     - **接続タイムアウト**（任意）：EMQXが接続試行のタイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。デフォルトは `15` 秒。
     - **プリペアドステートメントを無効化**（任意）：データベースクエリにプリペアドステートメントを使用しないようにします。PostgreSQLのプロキシやミドルウェア（例：PGBouncerやSupabaseのトランザクションモード）がセッションレベルのプリペアドステートメントをサポートしていない場合に有効にしてください。デフォルトは無効。
   - **SQL**：データスキーマに応じたクエリ文を入力します。詳細は[SQLデータスキーマとクエリ文](#データスキーマとクエリ文)を参照してください。

設定が完了したら、**作成** をクリックしてください。
=======
   - 認証に関連する設定を行います。
     - **パスワードハッシュ**：プレーンテキストのパスワードに適用し、結果をデータベースに保存する際のハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります。
       - `md5`、`sha`、`sha256`、`sha512` の場合：
         - **ソルト位置**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`、`prefix`、`disable` のいずれかです。外部ストレージからEMQX内蔵データベースにユーザー認証情報を移行しない限り、デフォルト値のままで問題ありません。
         - ハッシュ結果は16進数文字列で表現され、大文字小文字を区別せずに保存された認証情報と比較されます。
       - `plain` の場合：
         - **ソルト位置**は `disable` に設定してください。
       - `bcrypt` の場合：
         - **ソルトラウンド**：ハッシュ関数を適用する回数を定義します。値は _2のソルトラウンド乗_ で表され、「コストファクター」とも呼ばれます。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ強化のために高い値が推奨されます。注：コストファクターを1増やすと認証に必要な時間が2倍になります。
       - `pbkdf2` の場合：
         - **疑似乱数関数**：キー生成に使うハッシュ関数を選択します（例：`sha256`）。
         - **イテレーション回数**：ハッシュ関数を実行する回数を設定します。デフォルトは `4096` です。
         - **派生キー長**（任意）：生成されるキーのバイト長を指定します。空欄の場合は選択した疑似乱数関数のデフォルト長になります。
         - ハッシュ結果は16進数文字列で表現され、大文字小文字を区別せずに保存された認証情報と比較されます。
   - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)で記述し、このPostgreSQL認証機能をクライアント接続に適用するかどうかを制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合にのみ認証機能が呼び出されます。詳細は[認証機能の前提条件](./authn.md#authenticator-preconditions)を参照してください。
   - **TLSを有効にする**：TLSを有効にする場合はトグルスイッチをオンにします。TLSの有効化については[ネットワークとTLS](../../network/overview.md)を参照してください。
   - **詳細設定**：
     - **コネクションプールサイズ**（任意）：EMQXノードからPostgreSQLサーバーへの同時接続数を指定します。デフォルトは `8` です。
     - **プリペアドステートメントを無効化**（任意）：トランザクションモードのPGBouncerやSupabaseなど、プリペアドステートメントをサポートしないPostgreSQLサービスを利用している場合に有効にします。このオプションはEMQX v5.7.1で導入されました。
   - **SQL**：データスキーマに合わせてクエリ文を入力します。詳細は[SQLデータスキーマとクエリ文](#データスキーマとクエリ文)を参照してください。

設定が完了したら、**作成** をクリックします。
>>>>>>> origin/release-6.1

## 設定項目による構成

<<<<<<< HEAD
EMQXの設定項目を使ってPostgreSQL認証機能を設定することも可能です。設定パラメータの完全な一覧は[EMQX Enterprise設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)を参照してください。

PostgreSQL認証は `mechanism = password_based` および `backend = postgresql` で識別されます。
=======
EMQXの設定項目を使ってPostgreSQL認証機能を設定することも可能です。<!-- 詳細な操作手順は[authn-postgresql:authentication](../../configuration/configuration-manual.html#authn-postgresql:authentication)を参照してください。 -->

PostgreSQL認証は、`mechanism = password_based` および `backend = postgresql` で識別されます。
>>>>>>> origin/release-6.1

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
