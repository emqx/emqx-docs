# PostgreSQLとの統合

EMQXはパスワード認証のためにPostgreSQLとの統合をサポートしています。

::: tip

[EMQX認証の基本概念](../authn/authn.md)についての知識

:::

## データスキーマとクエリ文

EMQXのPostgreSQL認証機能はほぼあらゆるストレージスキーマに対応しています。ビジネス要件に応じて、認証情報の保存方法やアクセス方法を自由に決定できます。例えば、1つまたは複数のテーブルやビューを使用することが可能です。

ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを保証する必要があります。

- `password_hash`：必須。データベースに保存されているパスワード（プレーンテキストまたはハッシュ化されたもの）。
- `salt`：任意。`salt = ""` またはこのフィールドを削除することで、ソルト値が追加されないことを示します。
- `is_superuser`：任意。現在のクライアントがスーパーユーザーかどうかを示すフラグ。デフォルトは `false`。

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
上記の例では、クエリの効率化に役立つ暗黙の `UNIQUE` インデックス（username）が作成されています。
システム内のユーザー数が多い場合は、クエリ応答時間を短縮しEMQXの負荷を軽減するために、事前にテーブルの最適化とインデックス作成を行ってください。
:::

このテーブルでは、MQTTユーザーは `username` で識別されます。

例えば、スーパーユーザー（`is_superuser`: `true`）でユーザー名が `user123`、パスワードが `secret`、ソルトが `salt` のドキュメントを追加したい場合、クエリ文は以下のようになります。

```bash
INSERT INTO mqtt_user(username, password_hash, salt, is_superuser) VALUES ('user123', 'f84fa2149dbb62ed4e0cf1f550d2949b33a6513d3a7707e08502511c79ccb0ee', 'salt', true);
INSERT 0 1
```

対応する設定パラメータは以下の通りです。

- password_hash_algorithm: `sha256`
- salt_position: `suffix`

SQL例:

```sql
query = "SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} LIMIT 1"
```

## ダッシュボードでの設定

EMQXダッシュボードを使って、PostgreSQLをパスワード認証に利用する設定が可能です。

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
     - **パスワードハッシュ**：プレーンテキストのパスワードがデータベースに保存される前に適用されるハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります。
       - `md5`、`sha`、`sha256`、`sha512` の場合：
         - **ソルト位置**：ソルト（ランダムデータ）がパスワードにどのように混ぜられるかを指定します。`suffix`、`prefix`、`disable` のいずれかです。外部ストレージからEMQX組み込みデータベースにユーザー認証情報を移行しない限り、デフォルト値のままで問題ありません。
         - ハッシュ結果は16進数文字列で表され、大文字小文字を区別せずに保存された認証情報と比較されます。
       - `plain` の場合：
         - **ソルト位置** は `disable` に設定してください。
       - `bcrypt` の場合：
         - **ソルトラウンド数**：ハッシュ関数の適用回数を2の累乗で表す「コストファクター」です。デフォルトは `10`、許容範囲は `5`～`10` です。セキュリティ強化のためには高い値を推奨します。コストファクターを1増やすごとに認証にかかる時間が倍増します。
       - `pbkdf2` の場合：
         - **疑似乱数関数**：鍵生成に使用するハッシュ関数を選択します（例：`sha256`）。
         - **反復回数**：ハッシュ関数の実行回数。デフォルトは `4096`。
         - **派生鍵長**（任意）：生成される鍵のバイト長。未指定の場合は疑似乱数関数により決定される長さになります。
         - ハッシュ結果は16進数文字列で表され、大文字小文字を区別せずに保存された認証情報と比較されます。
   - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)で記述し、このPostgreSQL認証機能をクライアント接続に適用するか制御します。この式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合にのみ認証機能が呼び出されます。詳細は[認証機能の前提条件](./authn.md#authenticator-preconditions)を参照してください。
   - **TLSを有効化**：TLSを有効にする場合はスイッチをオンにします。TLSの有効化については[ネットワークとTLS](../../network/overview.md)を参照してください。
   - **詳細設定**：
     - **接続プールサイズ**（任意）：EMQXノードからPostgreSQLサーバーへの同時接続数を指定します。デフォルトは `8` です。
     - **プリペアドステートメントを無効化**（任意）：トランザクションモードのPGBouncerやSupabaseなど、プリペアドステートメントをサポートしないPostgreSQLサービスを利用する場合に有効にします。このオプションはEMQX v5.7.1で追加されました。
   - **SQL**：データスキーマに合わせてクエリ文を入力します。詳細は[SQLデータスキーマとクエリ文](#データスキーマとクエリ文)を参照してください。

設定が完了したら、**作成** をクリックしてください。

## 設定項目による構成

EMQXの設定項目を使ってPostgreSQL認証機能を構成することも可能です。<!-- 詳細な操作手順は[authn-postgresql:authentication](../../configuration/configuration-manual.html#authn-postgresql:authentication)を参照してください。 -->

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
