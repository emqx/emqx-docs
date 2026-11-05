# MongoDBとの統合

このオーソライザーは、MongoDBデータベースに保存されたルールのリストとパブリッシュ／サブスクライブ要求を照合することで認可チェックを実装しています。

::: tip 前提条件

[EMQX認可の基本概念](./authz.md)に関する知識

:::

## データスキーマとクエリ文

MongoDBオーソライザーは、認可ルールをMongoDBドキュメントとして保存することをサポートしています。ユーザーは、結果に以下のフィールドが含まれるようにクエリテンプレートを提供する必要があります。

* `permission`: ルールがマッチした場合に適用されるアクションを指定します。利用可能な値は `deny` または `allow` です。
* `action`: ルールが関連するリクエストを指定します。可能な値は `publish`、`subscribe`、または `all` です。
* `topic` / `topics`: ルールが適用されるトピックまたはトピックのリストを指定します。トピックフィルターおよび[トピックプレースホルダー](./authz.md#topic-placeholders)をサポートします。
* `qos`（オプション）: 現在のルールが適用されるQoSレベルを指定します。値は `0`、`1`、`2` のいずれか、または複数のQoSレベルを指定する数値配列です。デフォルトはすべてのQoSレベルです。
* `retain`（オプション）: ルールが保持メッセージのパブリッシュを許可するかどうかを示します。値は `0`、`1`、または `true`、`false` です。デフォルトでは保持メッセージは許可されています。

ユーザー名 `emqx_u` のクライアントがトピック `t/1` にQoS 1でパブリッシュすることを拒否する例：

```js
> db.mqtt_acl.insertOne(
  {
      "username": "emqx_u",
      "clientid": "emqx_c",
      "ipaddress": "127.0.0.1",
      "permission": "deny",
      "action": "publish",
      "qos": 1,
      "topics": ["t/1"]
  }
);
{
  acknowledged: true,
  insertedId: ObjectId("62b4a1a0e693ae0233bc3e98")
}
```

対応する設定パラメータは以下の通りです：
```bash
collection = "mqtt_acl"
filter { username = "${username}" }
```

::: tip
システム内のユーザー数が多い場合は、クエリの応答時間を短縮しEMQXの負荷を軽減するために、事前にコレクションの最適化とインデックス作成を行ってください。
:::

このMongoDBデータスキーマに対する対応するダッシュボードの設定パラメータは **Filter**：`{ username = "${username}" }` です。

::: tip 読み取り専用のデータベース権限を付与する

EMQXはパブリッシュまたはサブスクライブ操作の認可時にMongoDBへの読み取りアクセスのみを必要とします。設定されたコレクションに対して `find` クエリを実行し、ドキュメントの挿入、更新、削除は行いません。

MongoDBに対して組み込みの `read` ロールを持つ専用のデータベースユーザーを使用するようにEMQXを設定してください。書き込みや管理者ロールは付与しないでください：

```js
db.getSiblingDB("mqtt").createUser({
  user: "emqx_auth",
  pwd: "PASSWORD",
  roles: [ { role: "read", db: "mqtt" } ]
})
```

コレクションの作成や認可ルールの管理には別の管理ユーザーを使用してください。EMQX設定内のデータベース認証情報が漏洩しても、読み取り専用ユーザーでは認可ルールの変更や削除はできません。
:::

## ダッシュボードでの設定

EMQXダッシュボードを使ってMongoDBをユーザー認可に利用する設定が可能です。

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左ナビゲーションツリーで **アクセス制御** -> **認可** をクリックし、**認可** ページに入ります。

2. 右上の **作成** をクリックし、**バックエンド**として **MongoDB** を選択してから **次へ** をクリックします。以下のように **設定** タブが表示されます。

   <img src="./assets/authz-MongoDB_ee.png" alt="authz-MongoDB_ee" style="zoom:67%;" />

3. 以下の指示に従って設定を行います。

   **接続**: MongoDB接続に必要な情報を入力します。

   - **MongoDBモード**: MongoDBのデプロイ方法を選択します。`Single`、`Replica Set`、`Sharding` があります。
   - **サーバー**: EMQXが接続するサーバーアドレスを指定します（`host:port`）。
   - **データベース**: MongoDBのデータベース名。
   - **コレクション**: 認可ルールが保存されているMongoDBコレクション名（文字列型）。
   - **ユーザー名**: MongoDBのユーザー名を指定します。
   - **パスワード**: MongoDBのユーザーパスワードを指定します。

   **TLS設定**: TLSを有効にする場合はトグルスイッチをオンにします。

   **Filter**: 認証情報検索用のMongoDBセレクターとして解釈されるマップ。 [プレースホルダー](./authz.md#authorization-placeholders)をサポートします。

   **詳細設定**:

   - **Auth Source**: MongoDB接続時に使用する認証ソースを指定します。特定のデータベースやユーザー認証を管理するMongoDB認証データベースを指定可能です。
   - **Use Legacy Protocol**: MongoDBとの通信にレガシープロトコルを使用するか選択します。`auto`、`true`、`false` のいずれか。デフォルトは `auto` で、新しいプロトコルのサポートを自動判定します。
   - **Record Limit**: MongoDBから取得する認可レコードの上限数を制限します。
   - **Skip**: レコード取得時にスキップする認可レコード数を設定します。

   - **プールサイズ**（オプション）: EMQXノードからMongoDBへの同時接続数を整数で指定します。デフォルトは `8`。
   - **接続タイムアウト**（オプション）: EMQXが接続タイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間に対応。

4. **作成** をクリックして設定を完了します。

## 設定項目による設定

EMQXの設定項目を使ってMongoDBオーソライザーを設定できます。

MongoDBオーソライザーはタイプ `mongodb` で識別されます。オーソライザーは3種類のデプロイモードで稼働するMongoDBへの接続をサポートします。 <!---詳細な設定情報は以下を参照してください：[authz:mongo_single](../../configuration/configuration-manual.html#authz:mongo_single)、[authz:mongo_sharded](../../configuration/configuration-manual.html#authz:mongo_sharded)、[authz:mongo_rs](../../configuration/configuration-manual.html#authz:mongo_rs)-->

設定例：

:::: tabs type:card

::: tab Single 

```bash
{
  type = mongodb

  collection = "mqtt_user"
  filter { username = "${username}" }

  mongo_type = single
  server = "127.0.0.1:27017"

  database = "mqtt"
  username = "emqx"
  password = "secret"
}
```

:::

::: tab Replica set

```bash
{
  type = mongodb

  collection = "mqtt_user"
  filter { username = "${username}" }

  mongo_type = rs
  servers = "10.123.12.10:27017,10.123.12.11:27017,10.123.12.12:27017"
  replica_set_name = "rs0"

  database = "mqtt"
  username = "emqx"
  password = "secret"
}
```
:::

::: tab Sharding

```bash
{
  type = mongodb
  enable = true

  collection = "mqtt_user"
  filter { username = "${username}" }

  mongo_type = sharded
  servers = "10.123.12.10:27017,10.123.12.11:27017,10.123.12.12:27017"

  database = "mqtt"
  username = "emqx"
  password = "secret"
}
```

:::

::::
