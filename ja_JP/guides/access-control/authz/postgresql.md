# PostgreSQLとの統合

このオーソライザーは、PostgreSQLデータベースに保存されたルールリストとパブリッシュ／サブスクリプション要求を照合することで認可チェックを実装しています。

::: tip 前提条件

[EMQXの基本的な認可概念](./authz.md)の知識が必要です。

:::

## データスキーマとクエリ文

PostgreSQLオーソライザーはほぼあらゆるストレージスキーマをサポートします。ACLルールの保存方法やアクセス方法（単一または複数のテーブル、ビューなど）はユーザーの判断に委ねられます。

ユーザーはクエリ文のテンプレートを用意し、以下のフィールドが含まれていることを確認する必要があります：
* `permission` の値はルールがマッチした場合に適用されるアクションを指定します。`deny` または `allow` のいずれかである必要があります。
* `action` の値はルールが関連するリクエストを指定します。`publish`、`subscribe`、または `all` のいずれかである必要があります。
* `topic` の値はルールが関連するトピックのフィルターを指定します。ワイルドカードや[トピックプレースホルダー](./authz.md#topic-placeholders)をサポートする文字列である必要があります。
* `qos`（任意）はルールが適用されるQoSレベルを指定します。値は `0`、`1`、`2` のいずれかです。複数のQoSレベルを指定する場合はカンマ区切りの文字列（例：`0,1`）も可能です。デフォルトはすべてのQoSレベルです。
* `retain`（任意）は現在のルールがリテインメッセージをサポートするかどうかを指定します。値は `0` または `1` で、デフォルトはリテインメッセージを許可します。

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

このテーブルでは、MQTTユーザーは `username` によって識別されます。

例えば、ユーザー `user123` に対してトピック `data/user123/#` へのパブリッシュを許可する認可ルールを追加したい場合、クエリ文は以下のようになります：

```bash
postgres=# INSERT INTO mqtt_acl(username, permission, action, topic, ipaddress) VALUES ('user123', 'allow', 'publish', 'data/user123/#', '127.0.0.1');
INSERT 0 1
```

対応する設定パラメータは以下の通りです：

```bash
query = "SELECT permission, action, topic, ipaddress, qos, retain FROM mqtt_acl WHERE username = ${username} and ipaddress = ${peerhost}"
```

## ダッシュボードでの設定

EMQXダッシュボードを使って、PostgreSQLをユーザー認可に利用する設定が可能です。

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左ナビゲーションツリーで **アクセス制御** -> **認可** をクリックし、**認可** ページに入ります。

2. 右上の **作成** をクリックし、**バックエンド** で **PostgreSQL** を選択します。次に **次へ** をクリックします。以下のように **設定** タブが表示されます。

   <img src="./assets/authz-PostgreSQL_ee.png" alt="PostgreSQL認可設定画面" style="zoom:67%;" />

3. 以下の指示に従って設定を行います：

   - **サーバー**：EMQXが接続するサーバーのアドレスを指定します（`host:port`）。
   - **データベース**：PostgreSQLのデータベース名。
   - **ユーザー名**：ユーザー名を指定します。
   - **パスワード**：ユーザーパスワードを指定します。
   - **TLSを有効にする**：TLSを有効にする場合はトグルをオンにします。

   - **接続プールサイズ**（任意）：EMQXノードからPostgreSQLへの同時接続数を整数で指定します。デフォルトは **8**。
   - **プリペアドステートメントを無効化**（任意）：PGBouncerのトランザクションモードやSupabaseなど、プリペアドステートメントをサポートしないPostgreSQLサービスを利用する場合に有効にします。このオプションはEMQX v5.7.1で追加されました。

   - **SQL**：データスキーマに合わせてクエリ文を入力します。詳細は[データスキーマとクエリ文](#データスキーマとクエリ文)を参照してください。

4. **作成** をクリックして設定を完了します。

## 設定項目による設定

EMQXの設定項目でPostgreSQLオーソライザーを設定することも可能です。

PostgreSQLオーソライザーは `postgresql` タイプで識別されます。
<!--詳細な設定は [authz:postgresql](../../configuration/configuration-manual.html#authz:postgresql) を参照してください。-->

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
