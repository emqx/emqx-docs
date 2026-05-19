# MySQLとの連携

このオーソライザーは、MySQLデータベースに保存されたルールのリストとパブリッシュ／サブスクリプション要求を照合することで認可チェックを実装しています。

::: tip 前提条件

[EMQX認可の基本概念](./authz.md)についての知識

:::

## データスキーマとクエリ文

MySQLオーソライザーはほぼあらゆるストレージスキーマをサポートしています。認証情報の保存方法やアクセス方法は、ビジネスニーズに応じて、単一または複数のテーブル、ビューなどを利用して自由に決めることができます。

ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認してください：
* `permission` はルールがマッチした場合に適用されるアクションを指定します。`deny` または `allow` のいずれかである必要があります。
* `action` はルールが関連するリクエストを指定します。`publish`、`subscribe`、または `all` のいずれかである必要があります。
* `topic` はルールに関連するトピックフィルターを指定します。ワイルドカードおよび[トピックプレースホルダー](./authz.md#topic-placeholders)をサポートする文字列である必要があります。
* `qos`（オプション）はルールが適用されるQoSレベルを指定します。値は `0`、`1`、`2` のいずれか、または複数のQoSレベルを指定する場合はカンマ区切りの文字列（例：`0,1`）です。デフォルトはすべてのQoSレベルです。
* `retain`（オプション）は現在のルールがリテインドメッセージをサポートするかどうかを指定します。値は `0` または `1` で、デフォルトはリテインドメッセージを許可します。

認証情報を保存するためのテーブル構造の例：

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
システム内のユーザー数が多い場合は、クエリの応答時間を短縮しEMQXの負荷を軽減するために、事前にテーブルの最適化およびインデックス作成を行ってください。
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

## ダッシュボードでの設定

EMQXダッシュボードを使って、MySQLをユーザー認可に利用する設定が可能です。

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左側ナビゲーションツリーで **アクセス制御** -> **認可** をクリックし、**認可** ページに入ります。

2. 右上の **作成** をクリックし、次に **バックエンド** から **MySQL** を選択します。次へをクリックすると、以下の **設定** タブが表示されます。

   <img src="./assets/authz-MySQL_ee.png" alt="MySQL認可設定画面" style="zoom:67%;" />

3. 以下の指示に従い認可バックエンドを設定します：

   - MySQL接続情報を入力します。

     - **サーバー**：EMQXが接続するサーバーアドレス（`host:port`）を指定します。
     - **データベース**：MySQLのデータベース名を指定します。
     - **ユーザー名**：ユーザー名を指定します。
     - **パスワード**：ユーザーパスワードを指定します。

   - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md#tls-for-external-resource-access)を参照してください。

   - **SQL**：データスキーマに合わせてクエリ文を入力します。詳細は[データスキーマとクエリ文](#データスキーマとクエリ文)を参照してください。

   - **詳細設定**：接続プール、タイムアウト、プリペアドステートメントの動作を設定します。
     - **接続プールサイズ**（任意）：EMQXノードからMySQLへの同時接続数を整数値で指定します。デフォルトは `8`。
     - **接続タイムアウト**（任意）：EMQXが接続試行をタイムアウトとみなすまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。デフォルトは `15` 秒。
     - **プリペアドステートメントを無効化**（任意）：データベースクエリでプリペアドステートメントの使用を無効にします。MySQLプロキシやミドルウェア（例：PGBouncerやSupabaseのトランザクションモード）がセッションレベルの機能（プリペアドステートメントなど）をサポートしない場合に有効にしてください。デフォルトは無効。

4. **作成** をクリックして設定を完了します。

## 設定ファイルでの設定

EMQXの設定ファイルでMySQLオーソライザーを設定することも可能です。

MySQLオーソライザーはタイプ `mysql` で識別されます。設定パラメータの全リストは[EMQX Enterprise設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)を参照してください。

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
