# MySQLとの統合

EMQXはパスワード認証のためにMySQLとの統合をサポートしています。

::: tip

[EMQX認証の基本概念](./authn.md)の知識

:::

## データスキーマとクエリ文

MySQL認証機能はほぼすべてのMySQLストレージスキーマに対応しています。認証情報の保存方法やアクセス方法は、ビジネスニーズに応じて、単一または複数のテーブルやビューなどを利用して自由に設計できます。

ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認する必要があります。

- `password_hash`：必須。データベースに保存されているパスワード（平文またはハッシュ化済み）；
- `salt`：任意。`salt = ""` またはこのフィールドを削除すると、ソルト値が追加されないことを示します；
- `is_superuser`：任意。現在のクライアントがスーパーユーザーかどうかを示すフラグ。デフォルトは `false`。

認証情報を保存するためのテーブル構造の例：

```sql
CREATE TABLE `mqtt_user` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(100) DEFAULT NULL,
  `password_hash` varchar(100) DEFAULT NULL,
  `salt` varchar(35) DEFAULT NULL,
  `is_superuser` tinyint(1) DEFAULT 0,
  `created` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `mqtt_username` (`username`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
```

::: tip
上記の例では、クエリに役立つ暗黙の`UNIQUE`インデックスフィールド（username）が作成されています。
システム内のユーザー数が多い場合は、クエリ応答時間を短縮しEMQXの負荷を軽減するために、事前にテーブルの最適化とインデックス設定を行ってください。
:::

このテーブルでは、MQTTユーザーは`username`で識別されます。

例えば、スーパーユーザー（`is_superuser`: `true`）として、ユーザー名`emqx_u`、パスワード`public`、サフィックスソルト`salt_foo123`、パスワードハッシュ`sha256`のレコードを追加する場合、クエリ文は以下のようになります。

```bash
mysql> INSERT INTO mqtt_user(username, password_hash, salt, is_superuser) VALUES ('emqx_u', SHA2(concat('public', 'salt_foo123'), 256), 'salt_foo123', 1);
Query OK, 1 row affected (0,01 sec)
```

対応する設定パラメータは以下の通りです。

```sql
password_hash_algorithm {
    name = sha256
    salt_position = suffix
}

query = "SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} LIMIT 1"
```

::: tip 読み取り専用のデータベース権限を付与する

EMQXはクライアント認証時にデータベースへの読み取りアクセスのみを必要とします。`query`は`SELECT`文として設定してください。EMQXはプリペアドステートメントとしてクエリを実行します。

クエリ対象テーブルに対して`SELECT`権限のみを持つ専用のデータベースアカウントをEMQXに設定してください。書き込みや管理権限は付与しないでください。

```sql
GRANT SELECT ON mqtt.mqtt_user TO 'emqx_auth'@'%';
```

テーブル作成やユーザー管理は別の管理用アカウントで行ってください。EMQX設定内のデータベース認証情報が漏洩しても、読み取り専用アカウントでは認証データの変更や削除はできません。
:::

## ダッシュボードでの設定

EMQXダッシュボードを使って、MySQLをパスワード認証に利用する設定が可能です。

1. EMQXダッシュボードの左側ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム**で **パスワードベース** を選択し、**バックエンド**で **MySQL** を選択すると、以下のように **設定** タブが表示されます。

<img src="./assets/authn-mysql.png" alt="MySQLによる認証" style="zoom:67%;" />

4. 以下の手順に従い認証バックエンドを設定してください。

   - **接続**：MySQLへの接続情報を入力します。

     - **サーバー**：EMQXが接続するMySQLサーバーのアドレス（`host:port`）を指定します。

     - **データベース**：MySQLのデータベース名。

     - **ユーザー名**：ユーザー名を指定します。

     - **パスワード**：ユーザーパスワードを指定します。

      - **認証設定**：認証に関する設定を行います。
        - **パスワードハッシュ**：平文パスワードに適用され、データベースに保存されるハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります。
          - `md5`、`sha`、`sha256`、`sha512`の場合：
            - **ソルト位置**：ソルト（ランダムデータ）をパスワードにどのように付加するかを指定します。`suffix`（後置）、`prefix`（前置）、`disable`（無効）のいずれかです。外部ストレージからEMQX組み込みデータベースへユーザー認証情報を移行しない限り、デフォルト値のままで問題ありません。
            - ハッシュ結果は16進数文字列として表現され、大文字小文字を区別せずに保存された認証情報と比較されます。
          - `plain`の場合：
            - **ソルト位置**は`disable`に設定してください。
          - `bcrypt`の場合：
            - **ソルトラウンド数**：ハッシュ関数の適用回数を2のべき乗で表す「コストファクター」です。デフォルトは`10`で、許容範囲は`5`から`10`です。セキュリティ向上のためには高い値を推奨します。コストファクターを1増やすと認証に要する時間が倍増します。
          - `pbkdf2`の場合：
            - **疑似乱数関数**：鍵生成に用いるハッシュ関数を選択します（例：`sha256`）。
            - **繰り返し回数**：ハッシュ関数の実行回数を設定します。デフォルトは`4096`です。
            - **生成鍵の長さ**（任意）：生成される鍵のバイト数を指定します。空欄の場合は選択した疑似乱数関数のデフォルト長になります。
            - ハッシュ結果は16進数文字列として表現され、大文字小文字を区別せずに保存された認証情報と比較されます。

      - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)を使い、このMySQL認証機能をクライアント接続に適用するかどうかを制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列の `"true"` の場合のみ認証機能が呼び出されます。そうでなければスキップされます。詳細は[認証機能の前提条件](./authn.md#authenticator-preconditions)を参照してください。

      - **TLSを有効化**：TLSを有効にする場合はスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md)を参照してください。

      - **SQL**：データスキーマに従ってクエリ文を入力します。詳細は[SQLデータスキーマとクエリ文](#sql-table-structure-and-query-statement)を参照してください。

      - **詳細設定**：同時接続数や接続タイムアウトまでの待機時間を設定します。
        - **コネクションプールサイズ**（任意）：EMQXノードからMySQLへの同時接続数を整数で指定します。デフォルトは`8`です。
        - **クエリタイムアウト**（任意）：EMQXが接続タイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。デフォルトは`5`秒です。

5. 設定が完了したら、**作成**をクリックしてください。

## 設定項目による設定

EMQXの設定項目を使ってMySQL認証機能を設定することも可能です。

MySQL認証は`mechanism = password_based`かつ`backend = mysql`で識別されます。

設定例：

```bash
{
  backend = "mysql"
  mechanism = "password_based"

  server = "127.0.0.1:3306"
  username = "root"
  database = "mqtt_user"
  password = ""
  pool_size = 8

  password_hash_algorithm {name = "sha256", salt_position = "suffix"}
  query = "SELECT password_hash, salt FROM mqtt_user where username = ${username} LIMIT 1"
  query_timeout = "5s"
}
```
