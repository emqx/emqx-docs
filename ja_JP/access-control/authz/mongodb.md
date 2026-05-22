# MongoDBとの連携

このオーソライザーは、MongoDBデータベースに保存されたルールリストとパブリッシュ／サブスクライブ要求を照合することで認可チェックを実装します。

::: tip 前提条件

<<<<<<< HEAD
[EMQX認可の基本概念](./authz.md)の知識が必要です。
=======
[EMQX認可の基本概念](./authz.md)についての知識
>>>>>>> origin/release-6.1

:::

## データスキーマとクエリ文

<<<<<<< HEAD
MongoDBオーソライザーは、認可ルールをMongoDBドキュメントとして保存することをサポートしています。ユーザーは結果に以下のフィールドが含まれるようにクエリテンプレートを提供する必要があります。

* `permission`：ルールが一致した場合に適用されるアクションを指定します。利用可能な値は `deny` または `allow` です。
* `action`：ルールが関連するリクエストを指定します。可能な値は `publish`、`subscribe`、または `all` です。
* `topic` / `topics`：ルールが適用されるトピックまたはトピックのリストを指定します。トピックフィルターおよび[トピックプレースホルダー](./authz.md#topic-placeholders)をサポートします。
* `qos`（任意）：現在のルールが適用されるQoSレベルを指定します。値は `0`、`1`、`2` のいずれか、または複数のQoSレベルを指定する数値配列も可能です。デフォルトはすべてのQoSレベルです。
* `retain`（任意）：ルールが保持メッセージのパブリッシュを許可するかどうかを示します。値は `0`、`1`、または `true`、`false` のいずれかです。デフォルトでは保持メッセージは許可されています。
=======
MongoDBオーソライザーは、認可ルールをMongoDBドキュメントとして保存することをサポートしています。ユーザーは、結果に以下のフィールドが含まれることを保証するためにクエリテンプレートを提供する必要があります。

* `permission`：ルールがマッチした場合に適用されるアクションを指定します。利用可能な値は `deny` または `allow` です。
* `action`：ルールが関連するリクエストを指定します。可能な値は `publish`、`subscribe`、または `all` です。
* `topic` / `topics`：ルールが適用されるトピックまたはトピックのリストを指定します。トピックフィルターおよび[トピックプレースホルダー](./authz.md#topic-placeholders)をサポートします。
* `qos`（オプション）：現在のルールが適用されるQoSレベルを指定します。値の選択肢は `0`、`1`、`2` です。複数のQoSレベルを指定する場合は数値の配列も可能です。デフォルトはすべてのQoSレベルです。
* `retain`（オプション）：ルールがリテインドメッセージのパブリッシュを許可するかどうかを示します。値の選択肢は `0`、`1`、または `true`、`false` です。デフォルトではリテインドメッセージは許可されています。
>>>>>>> origin/release-6.1

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
システム内のユーザー数が多い場合は、クエリ応答時間を短縮しEMQXの負荷を軽減するために、事前にコレクションの最適化およびインデックス作成を行ってください。
:::

このMongoDBデータスキーマに対応するダッシュボードの設定パラメータは **Filter**：`{ username = "${username}" }` です。

## ダッシュボードでの設定

<<<<<<< HEAD
EMQXダッシュボードを使用して、MongoDBをユーザー認可に利用する設定ができます。

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左側ナビゲーションツリーで **アクセス制御** -> **認可** をクリックし、**認可** ページに入ります。

2. 右上の **作成** をクリックし、続いて **バックエンド** で **MongoDB** を選択します。次に **次へ** をクリックします。以下のように **設定** タブが表示されます。
=======
EMQXダッシュボードを使用して、MongoDBをユーザー認可に利用する方法を設定できます。

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左側ナビゲーションツリーで **Access Control** -> **Authorization** をクリックし、**Authorization** ページに入ります。

2. 右上の **Create** をクリックし、次に **Backend** として **MongoDB** を選択します。**Next** をクリックすると、**Configuration** タブが表示されます。
>>>>>>> origin/release-6.1

   <img src="./assets/authz-MongoDB_ee.png" alt="authz-MongoDB_ee" style="zoom:67%;" />

3. 以下の指示に従って設定を行います。

<<<<<<< HEAD
   **接続**：MongoDB接続に必要な情報を入力します。

   - **MongoDBモード**：MongoDBのデプロイ形態を選択します。`Single`、`Replica Set`、`Sharding` があります。
   - **サーバー**：EMQXが接続するサーバーアドレスを指定します（`host:port`）。
   - **データベース**：MongoDBのデータベース名。
   - **コレクション**：認可ルールが保存されているMongoDBコレクション名（文字列型）。
   - **ユーザー名**：MongoDBのユーザー名を指定します。
   - **パスワード**：MongoDBユーザーパスワードを指定します。

   **TLS設定**：TLSを有効にする場合はトグルスイッチをオンにします。

   **Filter**：認証情報検索のためのMongoDBセレクターとして解釈されるマップです。[プレースホルダー](./authz.md#authorization-placeholders)をサポートします。

   **詳細設定**：

   - **認証ソース**：MongoDB接続時に使用する認証ソースを指定します。特定のデータベースやユーザー認証を管理するMongoDB認証データベースを指定できます。
   - **レガシープロトコル使用**：MongoDBとの通信にレガシープロトコルを使用するかどうかを選択します。`auto`、`true`、`false` のいずれかで、デフォルトは `auto` です。自動的に新しいプロトコルがサポートされているか判定します。
   - **レコード制限**：MongoDBから取得する認可レコードの最大数を制限します。
   - **スキップ**：レコード一覧取得時にスキップする認可レコード数を設定します。

   - **プールサイズ**（任意）：EMQXノードからMongoDBへの同時接続数を整数で指定します。デフォルトは `8` です。
   - **接続タイムアウト**（任意）：EMQXが接続タイムアウトと判断するまでの待機時間を指定します。ミリ秒、秒、分、時間の単位が使用可能です。

4. **作成** をクリックして設定を完了します。
=======
   **Connect**：MongoDBへの接続に必要な情報を入力します。

   - **MongoDB Mode**：MongoDBのデプロイ形態を選択します。`Single`、`Replica Set`、`Sharding` があります。
   - **Server**：EMQXが接続するサーバーアドレスを指定します（`host:port`）。
   - **Database**：MongoDBのデータベース名。
   - **Collection**：認可ルールが保存されているMongoDBコレクション名。データ型は文字列です。
   - **Username**：MongoDBのユーザー名を指定します。
   - **Password**：MongoDBのユーザーパスワードを指定します。

   **TLS Configuration**：TLSを有効にする場合はトグルスイッチをオンにします。

   **Filter**：クレデンシャル検索のためのMongoDBセレクターとして解釈されるマップです。[プレースホルダー](./authz.md#authorization-placeholders)がサポートされています。

   **Advanced Settings**：

   - **Auth Source**：MongoDB接続時に使用する認証ソースを指定します。特定のデータベースやユーザー認証を管理するMongoDB認証データベースを指定可能です。
   - **Use Legacy Protocol**：MongoDBとの通信にレガシープロトコルを使用するかどうかを選択します。`auto`、`true`、`false` の選択肢があり、デフォルトは `auto` で、新しいプロトコルがサポートされているか自動判別します。
   - **Record Limit**：MongoDBから取得する認可レコードの最大数を制限します。
   - **Skip**：認可レコードのリスト取得時にスキップするレコード数を設定します。
>>>>>>> origin/release-6.1

   - **Pool size**（オプション）：EMQXノードからMongoDBへの同時接続数を整数で指定します。デフォルトは `8` です。
   - **Connect Timeout**（オプション）：EMQXが接続タイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。

<<<<<<< HEAD
EMQXの設定項目でMongoDBオーソライザーを設定することも可能です。

MongoDBオーソライザーはタイプ `mongodb` で識別されます。このオーソライザーは3種類のMongoDBデプロイメントモードに対応しています。  
<!---詳細な設定情報は以下を参照してください：[authz:mongo_single](../../configuration/configuration-manual.html#authz:mongo_single)、[authz:mongo_sharded](../../configuration/configuration-manual.html#authz:mongo_sharded)、[authz:mongo_rs](../../configuration/configuration-manual.html#authz:mongo_rs)-->
=======
4. **作成** をクリックして設定を完了します。

## 設定項目による構成

EMQXの設定項目を使ってMongoDBオーソライザーを設定することも可能です。

MongoDBオーソライザーはタイプ `mongodb` で識別されます。オーソライザーは3種類のMongoDBデプロイモードに対応しています。 <!---詳細な設定情報は以下を参照してください：[authz:mongo_single](../../configuration/configuration-manual.html#authz:mongo_single)、[authz:mongo_sharded](../../configuration/configuration-manual.html#authz:mongo_sharded)、[authz:mongo_rs](../../configuration/configuration-manual.html#authz:mongo_rs)-->
>>>>>>> origin/release-6.1

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
