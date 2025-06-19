# MongoDBとの統合

EMQXはパスワード認証のためにMongoDBとの統合をサポートしています。EMQXのMongoDB認証機能は、現在、Single、[Replica Set](https://www.mongodb.com/docs/manual/reference/replica-configuration/)、および[Sharding](https://www.mongodb.com/docs/manual/sharding/)の3つの異なるモードで稼働するMongoDBへの接続をサポートしています。本ページでは、サポートされているデータスキーマの詳細と、EMQXダッシュボードおよび設定ファイルでの設定方法について説明します。

::: tip

[EMQX認証の基本概念](../authn/authn.md)についての知識

:::

## データスキーマとクエリ文

EMQXのMongoDB認証機能は、認証データをMongoDBのドキュメントとして保存することをサポートしています。ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認する必要があります。

- `password_hash`: 必須。データベースに保存されるパスワード（プレーンテキストまたはハッシュ済み）。このフィールドは名前変更が可能です。
- `salt`: 任意。`salt = ""` またはこのフィールドを削除すると、ソルト値が追加されないことを示します。このフィールドも名前変更が可能です。
- `is_superuser`: 任意。現在のクライアントがスーパーユーザーかどうかのフラグ。デフォルトは `false`。このフィールドも名前変更が可能です。

例えば、ユーザー名が `user123`、パスワードが `secret`、サフィックスとしてのソルトが `salt_foo123`、パスワードハッシュが `sha256` のスーパーユーザー（`is_superuser`: `true`）のドキュメントを追加したい場合、クエリ文は以下のようになります。

```
> db.mqtt_user.insertOne(
  {
      "username": "emqx_u",
      "salt": "slat_foo123",
      "is_superuser": true,
      "password_hash": "44edc2d57cde8d79c98145003e105b90a14f1460b79186ea9cfe83942fc5abb5"
  }
);
{
  "acknowledged" : true,
  "insertedId" : ObjectId("631989e20a33e26b05b15abe")
}
```

:::tip

システム内のユーザー数が多い場合は、クエリの応答時間を短縮しEMQXへの負荷を軽減するために、事前にテーブルの最適化とインデックス作成を行ってください。

:::

このMongoDBデータスキーマに対応するダッシュボードの設定パラメータは以下の通りです。

- **Password Hash**: `sha256`
- **Salt Position**: `suffix`
- **Collection**: `mqtt_user`
- **Filter**: `{ username = "${username}" }`
- **Password Hash field**: `password_hash`
- **Salt Field**: `salt`
- **is_superuser Field**： `is_superuser`

## ダッシュボードでの設定

EMQXダッシュボードを使用して、MongoDBをパスワード認証に利用する設定が可能です。

1. EMQXダッシュボードの左側ナビゲーションメニューから **Access Control** -> **Authentication** をクリックします。
2. **Authentication** ページの右上にある **Create** をクリックします。
3. **Mechanism** に **Password-Based** を選択し、**Backend** に **MongoDB** を選択すると、以下のように **Configuration** タブに遷移します。

![authn-MongoDB_ee](./assets/authn-MongoDB_ee.png)

4. 以下の手順に従って認証バックエンドを設定します。

   - MongoDBへの接続情報を入力します。
     - **MongoDB Mode**: MongoDBのデプロイ方法を選択します。`Single`、`Replica Set`、`Sharding` のいずれかです。
     - **Server**: EMQXが接続するMongoDBサーバーのアドレスを指定します。**MongoDB Mode** が `Replica Set` または `Sharding` の場合は、接続するすべてのMongoDBサーバーをカンマ（`,`）区切りで入力してください。
     - **Replica Set Name**: Replica Setの名前を指定します。文字列型。**MongoDB Mode** が `Replica Set` の場合のみ必要です。
     - **Database**: MongoDBのデータベース名。文字列型。
     - **Collection**: 認証ルールを保存するMongoDBコレクション名。文字列型。
     - **Username**: MongoDBのユーザー名を指定します。
     - **Password**: MongoDBのユーザーパスワードを指定します。
     - **Read Mode**（任意）: **MongoDB Mode** が `Replica Set` の場合のみ必要です。デフォルトは `master`。選択肢は `master`、`slave_ok`。
       - **master**: 各クエリは最新のデータ（マスター／プライマリサーバー）からのみ読み取ります。接続先がマスターでない場合、最初の読み取りは失敗し、その後の操作は中止されます。
       - **slave_ok**: セカンダリ／スレーブサーバーからの古いデータまたはマスターからの最新データの読み取りを許可します。
     - **Write Mode**（任意）: **MongoDB Mode** が `Replica Set` の場合のみ必要です。選択肢は `unsafe`、`safe`。デフォルトは `safe`。

   - 認証に関する設定を行います。
     - **Password Hash Field**: パスワードのフィールド名を指定します。
     - **Password Hash**: プレーンテキストパスワードに適用され、データベースに保存される前のパスワードハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります。
       - `md5`、`sha`、`sha256`、`sha512` の場合:
         - **Salt Position**: ソルト（ランダムデータ）をパスワードにどのように混ぜるかを決定します。`suffix`、`prefix`、`disable` のいずれかです。外部ストレージからEMQX組み込みデータベースにユーザー認証情報を移行しない限り、デフォルト値のままで問題ありません。
         - 結果のハッシュは16進数文字列で表され、大文字・小文字を区別せずに保存された認証情報と比較されます。
       - `plain` の場合:
         - **Salt Position** は `disable` に設定してください。
       - `bcrypt` の場合:
         - **Salt Rounds**: ハッシュ関数の適用回数を定義します。2のべき乗で表され、"コストファクター"とも呼ばれます。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ向上のためには高い値が推奨されます。注：コストファクターを1増やすと認証に必要な時間が倍増します。
       - `pbkdf2` の場合:
         - **Pseudorandom Function**: キー生成に使用するハッシュ関数を選択します（例：`sha256`）。
         - **Iteration Count**: ハッシュ関数の実行回数を設定します。デフォルトは `4096`。
         - **Derived Key Length**（任意）: 生成されるキーのバイト長を指定します。空欄の場合は選択した疑似乱数関数により決定されます。
         - 結果のハッシュは16進数文字列で表され、大文字・小文字を区別せずに保存された認証情報と比較されます。
     - **Salt Field**: MongoDBのソルトフィールドを指定します。
     - **is_superuser Field**: ユーザーがスーパーユーザーかどうかを判定します。
     - **Precondition**: クライアント接続に対してこのMongoDB認証機能を適用するかどうかを制御するための[Variform式](../../configuration/configuration.md#variform-expressions)です。この式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証機能が呼び出されます。それ以外の場合はスキップされます。詳細は[認証の前提条件](./authn.md#authentication-preconditions)を参照してください。
     - **Enable TLS**: TLSを有効にする場合はトグルスイッチをオンにします。TLSの有効化については[ネットワークとTLS](../../network/overview.md)を参照してください。
     - **Filter**: 認証情報検索に使われるMongoDBセレクターとして解釈されるマップです。[プレースホルダー](./authn.md#authentication-placeholders)がサポートされています。
     - **Advanced Settings**: 同時接続数や接続タイムアウトまでの待機時間を設定します。
       - **Connection Pool size**（任意）: EMQXノードからMongoDBサーバーへの同時接続数を指定します。デフォルトは `8`。
       - **Connect Timeout**（任意）: 接続がタイムアウトとみなされるまでの待機時間を指定します。サポートされる単位はミリ秒、秒、分、時間です。デフォルトは `20` 秒。

5. 設定が完了したら、**Create** をクリックします。

## 設定ファイルでの設定

EMQXのMongoDB認証機能は、EMQXの設定項目で構成することも可能です。  
<!-- 詳細な操作手順は [authn-mongodb:standalone](../../configuration/configuration-manual.html#authn-mongodb:standalone)、[authn-mongodb:sharded-cluster](../../configuration/configuration-manual.html#authn-mongodb:sharded-cluster)、および [authn-mongodb:replica-set](../../configuration/configuration-manual.html#authn-mongodb:replica-set) を参照してください。 -->

以下は参考となるコード例です。

:::: tabs type:card

::: tab Single mode

```bash
{
  mechanism = password_based
  backend = mongodb

  password_hash_algorithm {
    name = sha256
    salt_position = suffix
  }

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
  mechanism = password_based
  backend = mongodb

  password_hash_algorithm {
    name = sha256
    salt_position = suffix
  }

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
  mechanism = password_based
  backend = mongodb
  enable = true

  password_hash_algorithm {
    name = sha256
    salt_position = suffix
  }

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
