# PostgreSQLとの連携

このオーソライザーは、PostgreSQLデータベースに格納されたルールリストとパブリッシュ／サブスクリプション要求を照合することで認可チェックを実装しています。

::: tip 前提条件

[EMQX認可の基本概念](./authz.md)についての知識

:::

## データスキーマとクエリ文

PostgreSQLオーソライザーはほぼあらゆるストレージスキーマをサポートしています。ACLルールの保存方法やアクセス方法は、ユーザーが単一または複数のテーブルやビューなどを用いて自由に決定できます。

ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを保証する必要があります。
* `permission` はルールがマッチした場合に適用されるアクションを指定します。`deny` または `allow` のいずれかである必要があります。
* `action` はルールが関連するリクエストを指定します。`publish`、`subscribe`、または `all` のいずれかである必要があります。
* `topic` はルールに関連するトピックのフィルターを指定します。ワイルドカードや[トピックプレースホルダー](./authz.md#topic-placeholders)をサポートする文字列である必要があります。
* `qos`（省略可能）はルールが適用されるQoSレベルを指定します。`0`、`1`、`2` のいずれか、またはカンマ区切りで複数指定可能（例：`0,1`）。デフォルトはすべてのQoSレベルです。
* `retain`（省略可能）は現在のルールがリテインドメッセージをサポートするかどうかを指定します。値は `0` または `1`。デフォルトはリテインドメッセージを許可します。

認証情報を格納するテーブルの例：

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

例えば、ユーザー `user123` に対してトピック `data/user123/#` のパブリッシュを許可する認可ルールを追加したい場合、クエリ文は以下のようになります。

```bash
postgres=# INSERT INTO mqtt_acl(username, permission, action, topic, ipaddress) VALUES ('user123', 'allow', 'publish', 'data/user123/#', '127.0.0.1');
INSERT 0 1
```

対応する設定パラメータは以下の通りです。

```bash
query = "SELECT permission, action, topic, ipaddress, qos, retain FROM mqtt_acl WHERE username = ${username} and ipaddress = ${peerhost}"
```

## ダッシュボードでの設定

EMQXダッシュボードを使って、PostgreSQLをユーザー認可に利用する設定が可能です。

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左側ナビゲーションツリーで **アクセス制御** -> **認可** をクリックし、**認可** ページに入ります。

2. 右上の **作成** をクリックし、**バックエンド** に **PostgreSQL** を選択して **次へ** をクリックします。以下のように **設定** タブが表示されます。

   <img src="./assets/authz-PostgreSQL_ee.png" alt="PostgreSQL認可設定画面" style="zoom:67%;" />

3. 以下の指示に従い認可バックエンドを設定します。

   - PostgreSQLへの接続情報を入力します。

     - **サーバー**：EMQXが接続するサーバーアドレス（`host:port`）を指定します。
     - **データベース**：PostgreSQLのデータベース名。
     - **ユーザー名**：ユーザー名を指定します。
     - **パスワード**：ユーザーパスワードを指定します。

   - **TLSを有効にする**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md#tls-for-external-resource-access)を参照してください。

   - **SQL**：データスキーマに応じてクエリ文を入力します。詳細は[データスキーマとクエリ文](#データスキーマとクエリ文)を参照してください。

   - **詳細設定**：接続プール、タイムアウト、プリペアドステートメントの動作を設定します。
     - **接続プールサイズ**（省略可能）：EMQXノードからPostgreSQLへの同時接続数を整数で指定します。デフォルトは `8`。
     - **接続タイムアウト**（省略可能）：接続試行がタイムアウトとみなされるまでの待機時間を指定します。ミリ秒、秒、分、時間単位が利用可能です。デフォルトは `15` 秒。
     - **プリペアドステートメントを無効化**（省略可能）：データベースクエリでプリペアドステートメントの利用を無効にします。PostgreSQLのプロキシやミドルウェア（例：PGBouncerやSupabaseのトランザクションモード）がセッションレベルの機能（プリペアドステートメントなど）をサポートしない場合に有効にしてください。デフォルトは無効。

4. **作成** をクリックして設定を完了します。

## 設定項目による設定

EMQXの設定項目を使ってPostgreSQLオーソライザーを設定することも可能です。

PostgreSQLオーソライザーは `postgresql` タイプで識別されます。設定パラメータの全リストは[EMQX Enterprise設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)を参照してください。

設定例：

```bash
{
  type = postgresql

  database = "mqtt"
  username = "postgres"
  password = "public"
  server = "127.0.0.1:5432"
  query = "SELECT permission, action, topic FROM mqtt_acl WHERE username = ${username}"
  connect_timeout = "15s"
  disable_prepared_statements = false
}
```
