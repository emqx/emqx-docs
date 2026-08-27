# MongoDBとの統合

このオーソライザーは、MongoDBデータベースに保存されたルールのリストとパブリッシュ／サブスクライブ要求を照合することで認可チェックを実装しています。

::: tip 前提条件

[基本的なEMQX認可の概念](./authz.md)の知識が必要です。

:::

## データスキーマとクエリ文

MongoDBオーソライザーは、認可ルールをMongoDBドキュメントとして保存することをサポートしています。ユーザーは、結果に以下のフィールドが含まれることを保証するクエリテンプレートを提供する必要があります。

* `permission`：ルールが一致した場合に適用されるアクションを指定します。利用可能な値は `deny` または `allow` です。
* `action`：ルールが関連するリクエストを指定します。可能な値は `publish`、`subscribe`、または `all` です。
* `topic` / `topics`：ルールが適用されるトピックまたはトピックのリストを指定します。トピックフィルターおよび[トピックプレースホルダー](./authz.md#topic-placeholders)をサポートします。
* `qos`（オプション）：現在のルールが適用されるQoSレベルを指定します。値の選択肢は `0`、`1`、`2` です。複数のQoSレベルを指定する場合は数値の配列も可能です。デフォルトはすべてのQoSレベルです。
* `retain`（オプション）：ルールがリテインドメッセージのパブリッシュを許可するかどうかを示します。値は `0`、`1`、または `true`、`false` です。デフォルトではリテインドメッセージは許可されています。

ユーザー名 `emqx_u` のクライアントがQoS 1でトピック `t/1` にパブリッシュすることを拒否する例：

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

このMongoDBデータスキーマに対応するダッシュボードの設定パラメータは **Filter**：`{ username = "${username}" }` です。

## ダッシュボードでの設定

EMQXダッシュボードを使用して、MongoDBをユーザー認可に利用する設定ができます。

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左ナビゲーションツリーで **Access Control** -> **Authorization** をクリックし、**Authorization** ページに入ります。

2. 右上の **Create** をクリックし、次に **Backend** で **MongoDB** を選択してから **Next** をクリックします。**Configuration** タブが表示されます。

   <img src="./assets/authz-MongoDB_ee.png" alt="authz-MongoDB_ee" style="zoom:67%;" />

3. 以下の指示に従って設定を行います。

   **Connect**：MongoDBに接続するための情報を入力します。

   - **MongoDB Mode**：MongoDBのデプロイ方法を選択します。`Single`、`Replica Set`、`Sharding` があります。
   - **Server**：EMQXが接続するサーバーのアドレスを指定します（`host:port`）。
   - **Database**：MongoDBのデータベース名。
   - **Collection**：認可ルールが保存されているMongoDBコレクションの名前。データ型は文字列です。
   - **Username**：MongoDBのユーザー名を指定します。
   - **Password**：MongoDBのユーザーパスワードを指定します。

   **Precondition**：任意のVariform式を入力します。式が `true` と評価された場合のみEMQXはこのオーソライザーを呼び出します。詳細は[オーソライザープリコンディション](./authz.md#authorizer-preconditions)を参照してください。

   **TLS Configuration**：TLSを有効にする場合はトグルスイッチをオンにします。

   **Filter**：MongoDBのクレデンシャル検索用セレクターとして解釈されるマップです。[プレースホルダー](./authz.md#authorization-placeholders)をサポートします。

   **Advanced Settings**：
   
   - **Auth Source**：MongoDB接続時に使用する認証ソースを指定します。特定のデータベースやユーザー認証情報を管理するMongoDB認証データベースを指定できます。
   - **Use Legacy Protocol**：MongoDBとの通信にレガシープロトコルを使用するかどうかを選択します。`auto`、`true`、`false` のいずれかです。デフォルトは `auto` で、新しいプロトコルのサポート有無を自動判定します。
   - **Record Limit**：MongoDBから取得する認可レコードの最大数を制限します。
   - **Skip**：認可レコードのリスト取得時にスキップするレコード数を設定します。
   
   - **Pool size**（オプション）：EMQXノードからMongoDBへの同時接続数を整数で指定します。デフォルトは `8`。
   - **Connect Timeout**（オプション）：EMQXが接続タイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。

4. **Create** をクリックして設定を完了します。

## 設定項目による構成

EMQXの設定項目を使ってMongoDBオーソライザーを構成できます。

MongoDBオーソライザーは `mongodb` タイプで識別されます。オーソライザーは3種類のデプロイモードで稼働するMongoDBへの接続をサポートしています。<!---詳細な設定情報は以下を参照してください：[authz:mongo_single](../../configuration/configuration-manual.html#authz:mongo_single)、[authz:mongo_sharded](../../configuration/configuration-manual.html#authz:mongo_sharded)、[authz:mongo_rs](../../configuration/configuration-manual.html#authz:mongo_rs)-->

オプションの `precondition` 設定項目はVariform式を受け付けます。式が `true` と評価された場合のみEMQXはこのオーソライザーを呼び出します。`precondition` が省略または空の場合はプリコンディションは適用されません。詳細は[オーソライザープリコンディション](./authz.md#authorizer-preconditions)を参照してください。

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
