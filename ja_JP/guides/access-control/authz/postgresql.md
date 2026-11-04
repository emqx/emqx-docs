# PostgreSQLとの統合

このオーソライザーは、パブリッシュ／サブスクリプションリクエストをPostgreSQLデータベースに保存されたルールリストと照合することで認可チェックを実装します。

::: tip 前提条件

[EMQX認可の基本概念](./authz.md)の知識が必要です。

:::

## データスキーマとクエリ文

PostgreSQLオーソライザーはほぼあらゆるストレージスキーマをサポートします。ACLルールの保存方法やアクセス方法は、ユーザーがテーブルやビューなどを使って自由に決められます。

ユーザーはクエリ文のテンプレートを用意し、以下のフィールドが含まれていることを確認してください：
* `permission` はルールが一致した場合に適用されるアクションを指定します。`deny` または `allow` のいずれかである必要があります。
* `action` はルールが関連するリクエストの種類を指定します。`publish`、`subscribe`、または `all` のいずれかである必要があります。
* `topic` はルールに関連するトピックフィルターを指定します。ワイルドカードや[トピックプレースホルダー](./authz.md#topic-placeholders)をサポートする文字列である必要があります。
* `qos`（オプション）はルールが適用されるQoSレベルを指定します。値は `0`、`1`、`2` のいずれか、または複数のQoSレベルをカンマ区切りで指定した文字列（例：`0,1`）です。デフォルトはすべてのQoSレベルです。
* `retain`（オプション）は現在のルールが保持メッセージをサポートするかどうかを指定します。値は `0` または `1` です。デフォルトは保持メッセージを許可します。

認証情報を保存するためのテーブル構造例：

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

::: tip 読み取り専用データベース権限の付与

EMQXはパブリッシュやサブスクライブ操作の認可時にデータベースへの読み取りアクセスのみを必要とします。`query` は `SELECT` 文として設定してください。EMQXはこのクエリをプリペアドステートメントとして実行します。

EMQXが使用するデータベースアカウントには、クエリが参照するテーブルに対する `SELECT` 権限のみを付与し、書き込みや管理権限は付与しないでください：

```sql
GRANT USAGE ON SCHEMA public TO emqx_auth;
GRANT SELECT ON mqtt_acl TO emqx_auth;
```

テーブル作成や認可ルール管理は別の管理者アカウントで行ってください。EMQX設定のデータベース認証情報が漏洩しても、読み取り専用アカウントでは認可ルールの変更や削除はできません。
:::

## ダッシュボードでの設定

EMQXダッシュボードを使って、PostgreSQLをユーザー認可に利用する設定ができます。

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)にアクセスし、左のナビゲーションツリーから **アクセス制御** -> **認可** をクリックして **認可** ページに入ります。

2. 右上の **作成** をクリックし、バックエンドとして **PostgreSQL** を選択してから **次へ** をクリックします。以下のように **設定** タブが表示されます。

   <img src="./assets/authz-PostgreSQL_ee.png" alt="authz-PostgreSQL_ee" style="zoom:67%;" />

3. 以下の指示に従って設定を行ってください：

   - **サーバー**：EMQXが接続するサーバーアドレスを指定します（`host:port`）。
   - **データベース**：PostgreSQLのデータベース名。
   - **ユーザー名**：ユーザー名を指定します。
   - **パスワード**：ユーザーパスワードを指定します。
   - **TLSを有効にする**：TLSを有効にする場合はトグルスイッチをオンにします。
   
   - **接続プールサイズ**（任意）：EMQXノードからPostgreSQLへの同時接続数を整数で指定します。デフォルトは **8** です。
   - **プリペアドステートメントを無効にする**（任意）：PGBouncerのトランザクションモードやSupabaseなど、プリペアドステートメントをサポートしないPostgreSQLサービスを利用する場合はこのオプションを有効にしてください。このオプションはEMQX v5.7.1で追加されました。
   
   - **SQL**：データスキーマに応じたクエリ文を入力してください。詳細は[データスキーマとクエリ文](#データスキーマとクエリ文)を参照してください。
   
4. **作成** をクリックして設定を完了します。

## 設定項目による設定

EMQXの設定項目を使ってPostgreSQLオーソライザーを設定することも可能です。

PostgreSQLオーソライザーはタイプ `postgresql` で識別されます。 <!--詳細な設定は[authz:postgresql](../../configuration/configuration-manual.html#authz:postgresql)を参照してください。-->

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
