# MongoDBとの連携

このオーソライザーは、MongoDBデータベースに格納されたルールのリストとパブリッシュ／サブスクライブ要求を照合することで認可チェックを実装します。

::: tip 前提条件

[基本的なEMQX認可の概念](./authz.md)の知識が必要です。

:::

## データスキーマとクエリ文

MongoDBオーソライザーは、認可ルールをMongoDBドキュメントとして保存することをサポートしています。ユーザーは、結果に以下のフィールドが含まれることを保証するためのクエリテンプレートを提供する必要があります。

* `permission`：ルールが一致した場合に適用されるアクションを指定します。利用可能な値は `deny` または `allow` です。
* `action`：ルールが関連するリクエストを指定します。可能な値は `publish`、`subscribe`、または `all` です。
* `topic` / `topics`：ルールが適用されるトピックまたはトピックのリストを指定します。トピックフィルターおよび[トピックプレースホルダー](./authz.md#topic-placeholders)をサポートします。
* `qos`（オプション）：現在のルールが適用されるQoSレベルを指定します。値の選択肢は `0`、`1`、`2` です。複数のQoSレベルを指定する場合は数値の配列も可能です。デフォルトはすべてのQoSレベルです。
* `retain`（オプション）：ルールが保持メッセージのパブリッシュを許可するかどうかを示します。値の選択肢は `0`、`1`、または `true`、`false` です。デフォルトでは保持メッセージは許可されています。

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
システム内に多数のユーザーが存在する場合は、クエリ応答時間を短縮しEMQXの負荷を軽減するために、事前にコレクションの最適化とインデックス作成を行ってください。
:::

このMongoDBデータスキーマに対応するダッシュボードの設定パラメータは **Filter**：`{ username = "${username}" }` です。

## ダッシュボードでの設定

EMQXダッシュボードを使用して、MongoDBをユーザー認可に利用する方法を設定できます。

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左側ナビゲーションツリーで **アクセス制御** -> **認可** をクリックし、**認可** ページに入ります。

2. 右上の **作成** をクリックし、**バックエンド**として **MongoDB** を選択します。次に **次へ** をクリックします。以下のように **設定** タブが表示されます。

   <img src="./assets/authz-MongoDB_ee.png" alt="authz-MongoDB_ee" style="zoom:67%;" />

3. 以下の指示に従って設定を行います。

   **接続**：MongoDBに接続するための情報を入力します。

   - **MongoDBモード**：MongoDBのデプロイ方法を選択します。`Single`、`Replica Set`、`Sharding` があります。
   - **サーバー**：EMQXが接続するサーバーアドレスを指定します（`host:port`）。
   - **データベース**：MongoDBのデータベース名。
   - **コレクション**：認可ルールが保存されているMongoDBコレクション名。データ型は文字列です。
   - **ユーザー名**：MongoDBのユーザー名を指定します。
   - **パスワード**：MongoDBのユーザーパスワードを指定します。

   **TLS設定**：TLSを有効にする場合はトグルスイッチをオンにします。

   **Filter**：認証情報検索のためのMongoDBセレクターとして解釈されるマップ。 [プレースホルダー](./authz.md#authorization-placeholders)をサポートします。

   **詳細設定**：
   
   - **認証ソース**：MongoDBに接続する際に使用する認証ソースを指定します。特定のデータベースやユーザー認証情報を管理するMongoDB認証データベースを指定できます。
   - **レガシープロトコルを使用**：MongoDBとの通信にレガシープロトコルを使用するかどうかを選択します。`auto`、`true`、`false` の選択肢があり、デフォルトは `auto` で、新しいプロトコルがサポートされているか自動判定します。
   - **レコード制限**：MongoDBから取得する認可レコードの最大数を制限します。
   - **スキップ**：レコードリスト取得時にスキップする認可レコードの数を設定します。
   
   - **プールサイズ**（オプション）：EMQXノードからMongoDBへの同時接続数を整数値で指定します。デフォルトは `8` です。
   - **接続タイムアウト**（オプション）：EMQXが接続タイムアウトと判断するまでの待機時間を指定します。ミリ秒、秒、分、時間単位がサポートされています。

4. **作成** をクリックして設定を完了します。

## 設定項目による構成

EMQXの設定項目を使ってMongoDBオーソライザーを構成できます。

MongoDBオーソライザーはタイプ `mongodb` で識別されます。オーソライザーは3種類のMongoDBデプロイモードに対応しています。  
<!---詳細な設定情報は以下を参照してください：[authz:mongo_single](../../configuration/configuration-manual.html#authz:mongo_single)、[authz:mongo_sharded](../../configuration/configuration-manual.html#authz:mongo_sharded)、[authz:mongo_rs](../../configuration/configuration-manual.html#authz:mongo_rs)-->

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
