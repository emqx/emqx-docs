# MongoDBとの統合

このオーソライザーは、MongoDBデータベースに格納されたルールリストとパブリッシュ／サブスクライブ要求を照合することで認可チェックを実装しています。

::: tip 前提条件

[EMQX認可の基本概念](./authz.md)の知識が必要です。

:::

## データスキーマとクエリ文

MongoDBオーソライザーは、認可ルールをMongoDBドキュメントとして保存することをサポートしています。ユーザーは、結果に以下のフィールドが含まれることを保証するクエリテンプレートを提供する必要があります。

* `permission`：ルールが一致した場合に適用されるアクションを指定します。利用可能な値は`deny`または`allow`です。
* `action`：ルールが関連するリクエストを指定します。可能な値は`publish`、`subscribe`、または`all`です。
* `topic` / `topics`：ルールが適用されるトピックまたはトピックのリストを指定します。トピックフィルターおよび[トピックプレースホルダー](./authz.md#topic-placeholders)をサポートします。
* `qos`（オプション）：現在のルールが適用されるQoSレベルを指定します。値は`0`、`1`、`2`のいずれか、または複数のQoSレベルを指定する数値配列も可能です。デフォルトはすべてのQoSレベルです。
* `retain`（オプション）：ルールが保持メッセージのパブリッシュを許可するかどうかを示します。値は`0`、`1`、または`true`、`false`です。デフォルトでは保持メッセージは許可されています。

ユーザー名`emqx_u`のクライアントがQoS 1でトピック`t/1`にパブリッシュすることを拒否する例：

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
システム内に多数のユーザーが存在する場合は、クエリ応答時間を短縮しEMQXの負荷を軽減するために、事前にコレクションを最適化およびインデックス化してください。
:::

このMongoDBデータスキーマに対するダッシュボードの対応設定パラメータは**Filter**：`{ username = "${username}" }`です。

::: tip 読み取り専用のデータベース権限を付与する

EMQXはパブリッシュまたはサブスクライブ操作を認可する際にMongoDBへの読み取りアクセスのみを必要とします。設定されたコレクションに対して`find`クエリを実行し、ドキュメントの挿入、更新、削除は行いません。

MongoDBに対して組み込みの`read`ロールを持つ専用のデータベースユーザーを使用するようにEMQXを設定してください。書き込みや管理者ロールは割り当てないでください：

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

EMQXダッシュボードを使用してMongoDBをユーザー認可に利用する設定が可能です。

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左ナビゲーションツリーで**アクセス制御** -> **認可**をクリックし、**認可**ページに入ります。

2. 右上の**作成**をクリックし、**バックエンド**として**MongoDB**を選択します。**次へ**をクリックすると、以下の**設定**タブが表示されます。

   <img src="./assets/authz-MongoDB_ee.png" alt="authz-MongoDB_ee" style="zoom:67%;" />

3. 以下の指示に従って設定を行います。

   **接続**：MongoDBに接続するための情報を入力します。

   - **MongoDBモード**：MongoDBの展開形態を選択します。`Single`、`Replica Set`、`Sharding`が選べます。
   - **サーバー**：EMQXが接続するサーバーアドレス（`host:port`）を指定します。
   - **データベース**：MongoDBのデータベース名を指定します。
   - **コレクション**：認可ルールが保存されているMongoDBコレクション名を指定します。データ型は文字列です。
   - **ユーザー名**：MongoDBのユーザー名を指定します。
   - **パスワード**：MongoDBユーザーのパスワードを指定します。

   **前提条件**：任意のVariform式を入力します。式が`true`と評価された場合のみEMQXはこのオーソライザーを呼び出します。詳細は[オーソライザの前提条件](./authz.md#authorizer-preconditions)を参照してください。

   **TLS設定**：TLSを有効にする場合はトグルスイッチをオンにします。

   **Filter**：認証情報検索のためのMongoDBセレクターとして解釈されるマップです。[プレースホルダー](./authz.md#authorization-placeholders)をサポートします。

   **詳細設定**：

   - **認証ソース**：MongoDB接続時に使用する認証ソースを指定します。特定のデータベースやユーザー認証を管理するMongoDB認証データベースを指定可能です。
   - **レガシープロトコルを使用**：MongoDBとの通信にレガシープロトコルを使用するかどうかを選択します。`auto`、`true`、`false`の選択肢があり、デフォルトは`auto`で新しいプロトコルのサポート有無を自動判別します。
   - **レコード制限**：MongoDBから取得する認可レコードの最大数を制限します。
   - **スキップ**：認可レコードの取得時にスキップするレコード数を設定します。
   
   - **プールサイズ**（オプション）：EMQXノードからMongoDBへの同時接続数を整数で指定します。デフォルトは`8`です。
   - **接続タイムアウト**（オプション）：EMQXが接続タイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が使用可能です。

4. **作成**をクリックして設定を完了します。

## 設定項目による設定

EMQXの設定項目を使ってMongoDBオーソライザーを設定できます。

MongoDBオーソライザーは`mongodb`タイプで識別されます。オーソライザーはMongoDBの3種類の展開モードに対応しています。 <!---詳細な設定情報は以下を参照してください：[authz:mongo_single](../../configuration/configuration-manual.html#authz:mongo_single)、[authz:mongo_sharded](../../configuration/configuration-manual.html#authz:mongo_sharded)、[authz:mongo_rs](../../configuration/configuration-manual.html#authz:mongo_rs)-->

オプションの`precondition`設定項目はVariform式を受け入れます。式が`true`と評価された場合のみEMQXはこのオーソライザーを呼び出します。`precondition`が省略または空の場合は前提条件は適用されません。詳細は[オーソライザの前提条件](./authz.md#authorizer-preconditions)を参照してください。

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
