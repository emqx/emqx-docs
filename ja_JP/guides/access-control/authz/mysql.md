# MySQLとの統合

このオーソライザーは、MySQLデータベースに格納されたルールのリストとパブリッシュ／サブスクリプション要求を照合することで認可チェックを実装します。

::: tip 前提条件

[基本的なEMQX認可の概念](./authz.md)の知識が必要です。

:::

## データスキーマとクエリ文

MySQLオーソライザーはほぼすべてのストレージスキーマをサポートします。認証情報の保存方法やアクセス方法はビジネスニーズに応じて自由に決められます。例えば、1つまたは複数のテーブルやビューを使用することが可能です。

ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認してください：
* `permission` はルールが一致した場合に適用されるアクションを指定します。`deny` または `allow` のいずれかである必要があります。
* `action` はルールが関連するリクエストを指定します。`publish`、`subscribe`、または `all` のいずれかである必要があります。
* `topic` はルールに関連するトピックのフィルターを指定します。ワイルドカードおよび[トピックプレースホルダー](./authz.md#topic-placeholders)をサポートする文字列である必要があります。
* `qos`（任意）はルールが適用されるQoSレベルを指定します。値は `0`、`1`、`2` のいずれか、または複数のQoSレベルを指定するためにカンマ区切りの文字列（例：`0,1`）も可能です。デフォルトはすべてのQoSレベルです。
* `retain`（任意）は現在のルールが保持メッセージをサポートするかどうかを指定します。値は `0` または `1` で、デフォルトは保持メッセージを許可します。

認証情報を格納するためのテーブル構造の例：

```sql
CREATE TABLE `mqtt_acl` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `ipaddress` VARCHAR(60) NOT NULL DEFAULT '',
  `username` VARCHAR(255) NOT NULL DEFAULT '',
  `clientid` VARCHAR(255) NOT NULL DEFAULT '',
  `action` ENUM('publish', 'subscribe', 'all') NOT NULL,
  `permission` ENUM('allow', 'deny') NOT NULL,
  `topic` VARCHAR(255) NOT NULL DEFAULT '',
  `qos` tinyint(1),
  `retain` tinyint(1),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
```

::: tip
システム内のユーザー数が多い場合は、クエリ応答時間を短縮しEMQXの負荷を軽減するために、事前にテーブルの最適化とインデックス作成を行ってください。
:::

このテーブルでは、MQTTユーザーは `username` で識別されます。

例えば、ユーザー `user123` に対してトピック `data/user123/#` のパブリッシュを許可する認可ルールを追加したい場合、クエリ文は以下のようになります：

```bash
mysql> INSERT INTO mqtt_acl(username, permission, action, topic, ipaddress) VALUES ('user123', 'allow', 'publish', 'data/user123/#', '127.0.0.1');
Query OK, 1 row affected (0,01 sec)
```

対応する設定パラメータは以下の通りです：

```bash
query = "SELECT action, permission, topic, ipaddress, qos, retain FROM mqtt_acl where username = ${username} and ipaddress = ${peerhost}"
```

::: tip 読み取り専用のデータベース権限を付与する

EMQXはパブリッシュまたはサブスクライブ操作の認可時にデータベースへの読み取りアクセスのみを必要とします。`query` は `SELECT` 文として設定してください。EMQXはプリペアドステートメントとしてクエリを実行します。

EMQXが使用するデータベースアカウントには、クエリが参照するテーブルに対する `SELECT` 権限のみを付与し、書き込みや管理権限は付与しないでください：

```sql
GRANT SELECT ON mqtt_user.mqtt_acl TO 'emqx_auth'@'%';
```

テーブル作成や認可ルール管理は別の管理用アカウントで行ってください。EMQX設定内のデータベース認証情報が漏洩しても、読み取り専用アカウントでは認可ルールの変更や削除はできません。
:::

## ダッシュボードでの設定

EMQXダッシュボードを使ってMySQLをユーザー認可に利用する設定が可能です。

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左ナビゲーションツリーで **アクセス制御** -> **認可** をクリックし、**認可** ページに入ります。

2. 右上の **作成** をクリックし、**バックエンド** で **MySQL** を選択して **次へ** をクリックします。以下のように **設定** タブが表示されます。

   <img src="./assets/authz-MySQL_ee.png" alt="authz-MySQL_ee" style="zoom:67%;" />

3. 以下の指示に従って認可バックエンドを設定します：

   - MySQL接続情報を入力します。

     - **サーバー**：EMQXが接続するサーバーのアドレス（`host:port`）を指定します。
     - **データベース**：MySQLのデータベース名。
     - **ユーザー名**：ユーザー名を指定します。
     - **パスワード**：ユーザーパスワードを指定します。

   - **前提条件**：任意のVariform式を入力します。式が `true` と評価される場合にのみEMQXはこのオーソライザーを呼び出します。詳細は[オーソライザーの前提条件](./authz.md#authorizer-preconditions)を参照してください。

   - **TLSを有効にする**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md#tls-for-external-resource-access)を参照してください。

   - **SQL**：データスキーマに従ってクエリ文を入力します。詳細は[データスキーマとクエリ文](#データスキーマとクエリ文)を参照してください。

   - **詳細設定**：接続プール、タイムアウト、プリペアドステートメントの動作を設定します。
     - **接続プールサイズ**（任意）：EMQXノードからMySQLへの同時接続数を整数値で指定します。デフォルトは `8`。
     - **接続タイムアウト**（任意）：EMQXが接続試行をタイムアウトと判断するまでの待機時間を指定します。ミリ秒、秒、分、時間の単位が利用可能です。デフォルトは `15` 秒。
     - **プリペアドステートメントを無効化**（任意）：データベースクエリにプリペアドステートメントを使用しないようにします。MySQLのプロキシやミドルウェア（例：PGBouncerやTransactionモードのSupabase）がセッションレベルの機能（プリペアドステートメントなど）をサポートしない場合に有効にしてください。デフォルトは無効。

4. **作成** をクリックして設定を完了します。

## 設定項目による設定

EMQXの設定項目を使ってMySQLオーソライザーを設定できます。

MySQLオーソライザーは `type` が `mysql` で識別されます。設定パラメータの全リストは[EMQX Enterprise設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)を参照してください。

任意の `precondition` 設定項目はVariform式を受け付けます。式が `true` と評価される場合にのみEMQXはこのオーソライザーを呼び出します。`precondition` が省略または空の場合は前提条件は適用されません。詳細は[オーソライザーの前提条件](./authz.md#authorizer-preconditions)を参照してください。

設定例：

```bash
{
  type = mysql

  database = "mqtt"
  username = "root"
  password = "public"
  server = "127.0.0.1:3306"
  query = "SELECT permission, action, topic FROM mqtt_acl WHERE username = ${username}"
  connect_timeout = "15s"
  disable_prepared_statements = false
}
```
