# MySQLとの統合

EMQXはパスワード認証のためにMySQLとの統合をサポートしています。

::: tip

[基本的なEMQX認証の概念](../authn/authn.md)についての知識

:::

## データスキーマとクエリ文

MySQL認証機能はほぼすべてのMySQLストレージスキーマをサポートしています。認証情報の保存方法やアクセス方法は、ビジネスニーズに応じて、単一または複数のテーブル、ビューなどを使用して自由に決定できます。

ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認する必要があります。

- `password_hash`：必須。データベースに保存されているパスワード（プレーンテキストまたはハッシュ化されたもの）。
- `salt`：任意。`salt = ""` またはこのフィールドを削除すると、ソルト値が追加されないことを示します。
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
上記の例では、クエリに役立つ暗黙の `UNIQUE` インデックスフィールド（username）が作成されています。
システム内のユーザー数が多い場合は、クエリの応答時間を短縮し、EMQXの負荷を軽減するために、事前にテーブルの最適化およびインデックス付けを行ってください。
:::

このテーブルでは、MQTTユーザーは `username` で識別されます。

例えば、スーパーユーザー（`is_superuser`: `true`）として、ユーザー名 `emqx_u`、パスワード `public`、サフィックスソルト `slat_foo123`、パスワードハッシュ `sha256` のドキュメントを追加したい場合、クエリ文は以下のようになります。

```bash
mysql> INSERT INTO mqtt_user(username, password_hash, salt, is_superuser) VALUES ('emqx_u', SHA2(concat('public', 'slat_foo123'), 256), 'slat_foo123', 1);
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

## ダッシュボードでの設定

EMQXダッシュボードを使って、MySQLをパスワード認証に利用する設定が可能です。

1. EMQXダッシュボードの左側ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム** に **パスワードベース** を選択し、**バックエンド** に **MySQL** を選択すると、以下のように **設定** タブが表示されます。

<img src="./assets/authn-mysql.png" alt="MySQLによる認証" style="zoom:67%;" />

4. 以下の指示に従って認証バックエンドを設定してください。

   - **接続**：MySQLへの接続情報を入力します。

     - **サーバー**：EMQXが接続するサーバーアドレス（`host:port`）を指定します。

     - **データベース**：MySQLのデータベース名。

     - **ユーザー名**：ユーザー名を指定します。

     - **パスワード**：ユーザーパスワードを指定します。

   
      - **認証設定**：認証に関する設定を行います。
        - **パスワードハッシュ**：プレーンテキストのパスワードに適用され、結果がデータベースに保存されるハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります。
          - `md5`、`sha`、`sha256`、`sha512` の場合：
            - **ソルト位置**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`、`prefix`、`disable` のいずれかを選択します。外部ストレージからEMQX組み込みデータベースにユーザー認証情報を移行する場合を除き、デフォルト値のままで問題ありません。
            - 結果のハッシュは16進数文字列で表され、大文字小文字を区別せずに保存された認証情報と比較されます。
          - `plain` の場合：
            - **ソルト位置** は `disable` に設定してください。
          - `bcrypt` の場合：
            - **ソルトラウンド**：ハッシュ関数が適用される回数を定義します。値は _2のソルトラウンド乗_ で表され、「コストファクター」とも呼ばれます。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ向上のためには高い値が推奨されます。注意：コストファクターを1増やすと認証にかかる時間が倍増します。
          - `pbkdf2` の場合：
            - **疑似乱数関数**：キー生成に使用するハッシュ関数を選択します（例：`sha256`）。
            - **反復回数**：ハッシュ関数を実行する回数を設定します。デフォルトは `4096`。
            - **導出キー長**（任意）：生成されるキーのバイト長を指定します。空欄の場合は選択された疑似乱数関数のデフォルト長が使用されます。
            - 結果のハッシュは16進数文字列で表され、大文字小文字を区別せずに保存された認証情報と比較されます。
   
      - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)を使って、このMySQL認証機能をクライアント接続に適用するかどうかを制御します。この式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証機能が呼び出されます。それ以外の場合はスキップされます。詳細は[認証機能の前提条件](./authn.md#authenticator-preconditions)を参照してください。
   
   
      - **TLSを有効にする**：TLSを有効にする場合はトグルスイッチをオンにします。TLSの有効化については[ネットワークとTLS](../../network/overview.md#tls-for-external-resource-access)を参照してください。
   
   
      - **SQL**：データスキーマに従ってクエリ文を入力します。詳細は[SQLデータスキーマとクエリ文](#データスキーマとクエリ文)を参照してください。
   
   
      - **詳細設定**：接続プール、タイムアウト、プリペアドステートメントの動作を設定します。
        - **接続プールサイズ**（任意）：EMQXノードからMySQLへの同時接続数を整数で指定します。デフォルトは `8`。
        - **クエリタイムアウト**（任意）：EMQXがクエリのタイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。デフォルトは `5` 秒。
        - **接続タイムアウト**（任意）：EMQXが接続試行のタイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。デフォルトは `15` 秒。
        - **プリペアドステートメントを無効化**（任意）：データベースクエリにプリペアドステートメントを使用しないようにします。MySQLのプロキシやミドルウェア（例：PGBouncerやトランザクションモードのSupabase）がセッションレベルの機能（プリペアドステートメントなど）をサポートしていない場合に有効にしてください。デフォルトは無効です。
   
5. 設定が完了したら、**作成** をクリックします。

## 設定項目による設定

EMQXの設定項目を使ってMySQL認証機能を設定することも可能です。設定パラメータの全リストは[EMQX Enterprise設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)を参照してください。

MySQL認証は `mechanism = password_based` および `backend = mysql` で識別されます。

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
  connect_timeout = "15s"
  disable_prepared_statements = false
}
```
