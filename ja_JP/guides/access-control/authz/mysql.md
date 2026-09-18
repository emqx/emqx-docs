# MySQLとの統合

このオーソライザーは、MySQLデータベースに格納されたルールのリストとパブリッシュ／サブスクリプション要求を照合することで認可チェックを実装します。

::: tip 前提条件

[基本的なEMQX認可の概念](./authz.md)についての知識

:::

## データスキーマとクエリ文

MySQLオーソライザーはほぼあらゆるストレージスキーマをサポートします。認証情報の保存方法やアクセス方法は、ビジネスニーズに応じて1つまたは複数のテーブル、ビューなどを使用して自由に決めることができます。

ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認する必要があります：
* `permission` はルールが一致した場合に適用されるアクションを指定します。`deny` または `allow` のいずれかである必要があります。
* `action` はルールが関連するリクエストを指定します。`publish`、`subscribe`、または `all` のいずれかである必要があります。
* `topic` はルールに関連するトピックフィルターを指定します。ワイルドカードおよび[トピックプレースホルダー](./authz.md#topic-placeholders)をサポートする文字列である必要があります。
* `qos`（オプション）はルールが適用されるQoS（サービス品質）レベルを指定します。値は `0`、`1`、`2` のいずれか、または複数のQoSレベルを指定するためにカンマ区切りの文字列（例：`0,1`）も可能です。デフォルトはすべてのQoSレベルです。
* `retain`（オプション）は現在のルールが保持メッセージをサポートするかどうかを指定します。値は `0` または `1` で、デフォルトは保持メッセージを許可します。

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

例えば、ユーザー `user123` に対して `data/user123/#` トピックのパブリッシュを許可する認可ルールを追加したい場合、クエリ文は以下のようになります：

```bash
mysql> INSERT INTO mqtt_acl(username, permission, action, topic, ipaddress) VALUES ('user123', 'allow', 'publish', 'data/user123/#', '127.0.0.1');
Query OK, 1 row affected (0,01 sec)
```

対応する設定パラメータは以下の通りです：
```bash
query = "SELECT action, permission, topic, ipaddress, qos, retain FROM mqtt_acl where username = ${username} and ipaddress = ${peerhost}"
```

## ダッシュボードでの設定

EMQXダッシュボードを使ってMySQLをユーザー認可に利用する設定が可能です。

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)にアクセスし、左のナビゲーションツリーから **アクセス制御** -> **認可** をクリックして **認可** ページに入ります。

2. 右上の **作成** をクリックし、次に **バックエンド** で **MySQL** を選択します。**次へ** をクリックすると、以下の **設定** タブが表示されます。

   <img src="./assets/authz-MySQL_ee.png" alt="authz-MySQL_ee" style="zoom:67%;" />

3. 以下の指示に従って設定を行います。

   **接続**：MySQL接続に必要な情報を入力します。

   - **サーバー**：EMQXが接続するサーバーのアドレス（`host:port`）を指定します。
   - **データベース**：MySQLのデータベース名。
   - **ユーザー名**：ユーザー名を指定します。
   - **パスワード**：ユーザーパスワードを指定します。

   **TLS設定**：TLSを有効にする場合はトグルスイッチをオンにします。

   **接続設定**：同時接続数と接続タイムアウトまでの待機時間を設定します。

   - **プールサイズ**（任意）：EMQXノードからMySQLへの同時接続数を整数で指定します。デフォルトは **8** です。

   **認可設定**：認可に関連する設定を入力します。

   - **SQL**：データスキーマに基づいてクエリ文を入力します。詳細は[データスキーマとクエリ文](#データスキーマとクエリ文)を参照してください。

4. **作成** をクリックして設定を完了します。

## 設定項目による設定

EMQXの設定項目を使ってMySQLオーソライザーを設定することも可能です。

MySQLオーソライザーはタイプ `mysql` で識別されます。 <!--詳細な設定は[authz:mysql](../../configuration/configuration-manual.html#authz:mysql)を参照してください。-->

設定例：

```bash
{
  type = mysql

  database = "mqtt"
  username = "root"
  password = "public"
  server = "127.0.0.1:3306"
  query = "SELECT permission, action, topic FROM mqtt_acl WHERE username = ${username}"
}
```
