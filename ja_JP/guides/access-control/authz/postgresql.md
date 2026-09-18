# PostgreSQLとの統合

このオーソライザーは、PostgreSQLデータベースに保存されたルールのリストとパブリッシュ／サブスクリプション要求を照合することで認可チェックを実装します。

::: tip 前提条件

[基本的なEMQX認可の概念](./authz.md)についての知識

:::

## データスキーマとクエリ文

PostgreSQLオーソライザーはほぼあらゆるストレージスキーマをサポートします。ACLルールの保存方法やアクセス方法（単一または複数のテーブル、ビューなど）はユーザーの判断に委ねられます。

ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを保証する必要があります：
* `permission` はルールが一致した場合に適用されるアクションを指定します。`deny` または `allow` のいずれかである必要があります。
* `action` はルールが関連するリクエストを指定します。`publish`、`subscribe`、または `all` のいずれかである必要があります。
* `topic` はルールに関連するトピックフィルターを指定します。ワイルドカードや[トピックプレースホルダー](./authz.md#topic-placeholders)をサポートする文字列である必要があります。
* `qos`（任意）はルールが適用されるQoSレベルを指定します。値は `0`、`1`、`2` のいずれか、またはカンマ区切りの文字列（例：`0,1`）で複数指定可能です。デフォルトはすべてのQoSレベルです。
* `retain`（任意）は現在のルールが保持メッセージをサポートするかどうかを指定します。値は `0` または `1` です。デフォルトは保持メッセージを許可します。

認証情報を保存するためのテーブル構造の例：

```sql
CREATE TABLE mqtt_acl(
  id serial PRIMARY KEY,
  username text NOT NULL,
  permission text NOT NULL,
  action text NOT NULL,
  topic text NOT NULL,
  qos smallint,
  retain smallint
);
CREATE INDEX mqtt_acl_username_idx ON mqtt_acl(username);
```

このテーブルでは、MQTTユーザーは `username` で識別されます。

例えば、ユーザー `user123` に対してトピック `data/user123/#` のパブリッシュを許可する認可ルールを追加したい場合、クエリ文は以下のようになります：

```bash
postgres=# INSERT INTO mqtt_acl(username, permission, action, topic, ipaddress) VALUES ('user123', 'allow', 'publish', 'data/user123/#', '127.0.0.1');
INSERT 0 1
```

対応する設定パラメータは以下の通りです：
```bash
query = "SELECT permission, action, topic, ipaddress, qos, retain FROM mqtt_acl WHERE username = ${username} and ipaddress = ${peerhost}"
```

## ダッシュボードでの設定

EMQXダッシュボードを使用して、PostgreSQLをユーザー認可に利用する設定が可能です。

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左側ナビゲーションツリーで **Access Control** -> **Authorization** をクリックし、**Authorization** ページに入ります。

2. 右上の **Create** をクリックし、**Backend** で **PostgreSQL** を選択します。次に **Next** をクリックします。以下のような **Configuration** タブが表示されます。

   <img src="./assets/authz-PostgreSQL_ee.png" alt="authz-PostgreSQL_ee" style="zoom:67%;" />

3. 以下の指示に従い設定を行います：

   - **Server**：EMQXが接続するサーバーアドレス（`host:port`）を指定します。
   - **Database**：PostgreSQLのデータベース名を指定します。
   - **Username**：ユーザー名を指定します。
   - **Password**：ユーザーパスワードを指定します。
   - **Enable TLS**：TLSを有効にする場合はトグルスイッチをオンにします。

   - **Connection Pool size**（任意）：EMQXノードからPostgreSQLへの同時接続数を整数で指定します。デフォルトは **8** です。
   - **Disable Prepared Statements**（任意）：トランザクションモードのPGBouncerやSupabaseなど、プリペアドステートメントをサポートしないPostgreSQLサービスを使用する場合はこのオプションを有効にします。このオプションはEMQX v5.7.1で導入されました。

   - **SQL**：データスキーマに応じたクエリ文を記入します。詳細は[データスキーマとクエリ文](#データスキーマとクエリ文)を参照してください。

4. **Create** をクリックして設定を完了します。

## 設定項目による設定

EMQXの設定項目を使ってPostgreSQLオーソライザーを設定することも可能です。

PostgreSQLオーソライザーは `postgresql` タイプで識別されます。 <!--詳細な設定は[authz:postgresql](../../configuration/configuration-manual.html#authz:postgresql)を参照してください。-->

設定例：

```bash
{
  type = postgresql

  database = "mqtt"
  username = "postgres"
  password = "public"
  server = "127.0.0.1:5432"
  query = "SELECT permission, action, topic FROM mqtt_acl WHERE username = ${username}"
}
```
