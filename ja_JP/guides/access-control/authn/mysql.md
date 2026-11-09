# MySQLとの統合

EMQXはパスワード認証のためにMySQLとの統合をサポートしています。

::: tip

[基本的なEMQX認証の概念](../authn/authn.md)についての知識

:::

## データスキーマとクエリ文

MySQL認証機能はほぼすべてのMySQLストレージスキーマをサポートしています。認証情報の保存方法やアクセス方法は、ビジネスニーズに応じて、単一または複数のテーブル、ビューなどを使用して決定できます。

ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認する必要があります：

- `password_hash`：必須。データベースに保存されているパスワード（プレーンテキストまたはハッシュ済み）。
- `salt`：任意。`salt = ""` またはこのフィールドを削除すると、ソルト値が追加されないことを示します。
- `is_superuser`：任意。現在のクライアントがスーパーユーザーかどうかのフラグ。デフォルトは `false`。

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
システム内のユーザー数が多い場合は、クエリの応答時間を短縮し、EMQXの負荷を軽減するために、事前にテーブルの最適化とインデックス作成を行ってください。
:::

このテーブルでは、MQTTユーザーは`username`で識別されます。

例えば、スーパーユーザー（`is_superuser`: `true`）として、ユーザー名`emqx_u`、パスワード`public`、サフィックスソルト`salt_foo123`、およびパスワードハッシュ`sha256`を追加する場合、クエリ文は以下のようになります：

```bash
mysql> INSERT INTO mqtt_user(username, password_hash, salt, is_superuser) VALUES ('emqx_u', SHA2(concat('public', 'salt_foo123'), 256), 'salt_foo123', 1);
Query OK, 1 row affected (0,01 sec)
```

対応する設定パラメータは以下の通りです：

```sql
password_hash_algorithm {
    name = sha256
    salt_position = suffix
}

query = "SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} LIMIT 1"
```

::: tip 読み取り専用データベース権限の付与

EMQXはクライアント認証時にデータベースへの読み取りアクセスのみが必要です。`query`は`SELECT`文として設定してください。EMQXはこのクエリをプリペアドステートメントとして実行します。

EMQXが使用するデータベースアカウントには、クエリが読み取るテーブルに対する`SELECT`権限のみを付与し、書き込みや管理権限は付与しないでください。

```sql
GRANT SELECT ON mqtt.mqtt_user TO 'emqx_auth'@'%';
```

テーブルの作成やユーザー管理には別の管理用アカウントを使用してください。EMQX設定内のデータベース認証情報が漏洩した場合でも、読み取り専用アカウントでは認証データの変更や削除はできません。
:::

## ダッシュボードでの設定

EMQXダッシュボードを使って、MySQLをパスワード認証に利用する設定が可能です。

1. EMQXダッシュボードの左ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム**として **パスワードベース** を選択し、**バックエンド**として **MySQL** を選択すると、以下のような **設定** タブに移動します。

<img src="./assets/authn-mysql.png" alt="MySQLによる認証" style="zoom:67%;" />

4. 以下の指示に従って認証バックエンドを設定してください：

   - **接続**：MySQLへの接続情報を入力します。

     - **サーバー**：EMQXが接続するサーバーアドレス（`host:port`）を指定します。

     - **データベース**：MySQLのデータベース名。

     - **ユーザー名**：ユーザー名を指定します。

     - **パスワード**：ユーザーパスワードを指定します。
   
   
      - **認証設定**：認証に関する設定を行います：
        - **パスワードハッシュ**：プレーンテキストのパスワードに適用され、データベースに保存される前のハッシュアルゴリズムを選択します。選択肢は `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定が必要です：
          - `md5`、`sha`、`sha256`、`sha512` の場合：
            - **ソルト位置**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`（後置）、`prefix`（前置）、`disable`（無効）のいずれかです。外部ストレージからEMQX組み込みデータベースにユーザー認証情報を移行する場合を除き、デフォルト値のままで問題ありません。
            - 結果のハッシュは16進数文字列で表され、大文字小文字を区別せずに保存された認証情報と比較されます。
          - `plain` の場合：
            - **ソルト位置**は `disable` にしてください。
          - `bcrypt` の場合：
            - **ソルトラウンド**：ハッシュ関数が適用される回数を定義します。値は _2のソルトラウンド乗_ で表され、「コストファクター」とも呼ばれます。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ向上のために高い値が推奨されます。コストファクターを1増やすと認証にかかる時間が倍増します。
          - `pbkdf2` の場合：
            - **疑似乱数関数**：キー生成に使用するハッシュ関数を選択します（例：`sha256`）。
            - **繰り返し回数**：ハッシュ関数の実行回数を設定します。デフォルトは `4096`。
            - **生成キー長**（任意）：生成されるキーのバイト長を指定します。空欄の場合は選択した疑似乱数関数のデフォルト長になります。
            - 結果のハッシュは16進数文字列で表され、大文字小文字を区別せずに保存された認証情報と比較されます。
   
      - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)を使い、このMySQL認証機能をクライアント接続に適用するか制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列の `"true"` の場合のみ認証機能が呼び出されます。そうでなければスキップされます。詳細は[認証機能の前提条件](./authn.md#authenticator-preconditions)を参照してください。
   
   
      - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにしてください。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md#tls-for-external-resource-access)を参照してください。
   
   
      - **SQL**：データスキーマに従ってクエリ文を入力してください。詳細は[SQLデータスキーマとクエリ文](#sql-table-structure-and-query-statement)を参照してください。
   
   
      - **詳細設定**：接続プール、タイムアウト、プリペアドステートメントの動作を設定します。
        - **接続プールサイズ**（任意）：EMQXノードからMySQLへの同時接続数を整数で指定します。デフォルトは `8`。
        - **クエリタイムアウト**（任意）：EMQXがクエリのタイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。デフォルトは `5` 秒。
        - **接続タイムアウト**（任意）：EMQXが接続試行のタイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。デフォルトは `15` 秒。
        - **プリペアドステートメント無効化**（任意）：データベースクエリでプリペアドステートメントの使用を無効にします。MySQLプロキシやミドルウェア（例：PGBouncerやトランザクションモードのSupabase）がセッションレベルの機能（プリペアドステートメントなど）をサポートしない場合に有効にしてください。デフォルトは無効。
   
5. 設定が完了したら、**作成** をクリックしてください。

## 設定項目による設定

EMQXの設定項目を使ってMySQL認証機能を設定できます。設定パラメータの完全な一覧は[EMQX Enterprise設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)を参照してください。

MySQL認証は `mechanism = password_based` と `backend = mysql` で識別されます。

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
