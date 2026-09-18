# MongoDBとの統合

EMQXはパスワード認証のためにMongoDBとの統合をサポートしています。EMQXのMongoDB認証機能は、現在、Single、[レプリカセット](https://www.mongodb.com/docs/manual/reference/replica-configuration/)、および[シャーディング](https://www.mongodb.com/docs/manual/sharding/)の3つの異なるモードで動作するMongoDBへの接続をサポートしています。本ページでは、サポートされるデータスキーマの詳細と、EMQXダッシュボードおよび設定ファイルでの設定方法について説明します。

::: tip

[基本的なEMQX認証の概念](../authn/authn.md)についての知識

:::

## データスキーマとクエリ文

EMQXのMongoDB認証は、認証データをMongoDBドキュメントとして保存することをサポートしています。ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認する必要があります。

- `password_hash`：必須。データベースに保存されるパスワード（プレーンテキストまたはハッシュ済み）。このフィールドは名前の変更が可能です。
- `salt`：任意。`salt = ""` またはこのフィールドを削除すると、ソルト値が追加されないことを示します。このフィールドは名前の変更が可能です。
- `is_superuser`：任意。現在のクライアントがスーパーユーザーかどうかを示すフラグ。デフォルトは `false`。このフィールドは名前の変更が可能です。

例えば、ユーザー名が `user123`、パスワードが `secret`、接尾辞としてのソルトが `salt_foo123`、パスワードハッシュが `sha256` のスーパーユーザー（`is_superuser`: `true`）のドキュメントを追加する場合、クエリ文は以下のようになります。

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

システム内に多数のユーザーが存在する場合は、クエリの応答時間を短縮し、EMQXの負荷を軽減するために、事前にテーブルの最適化およびインデックスの設定を行ってください。

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

EMQXダッシュボードを使って、MongoDBをパスワード認証に利用する設定が可能です。

1. EMQXダッシュボードの左側ナビゲーションメニューから **Access Control** -> **Authentication** をクリックします。
2. **Authentication** ページの右上にある **Create** をクリックします。
3. **Mechanism** に **Password-Based** を選択し、**Backend** に **MongoDB** を選択すると、以下のように **Configuration** タブに移動します。

![authn-MongoDB_ee](./assets/authn-MongoDB_ee.png)

4. 以下の指示に従って認証バックエンドを設定してください。

   - MongoDB接続情報の入力：
     - **MongoDB Mode**：MongoDBの展開形態を選択します。`Single`、`Replica Set`、`Sharding` のいずれかです。
     - **Server**：EMQXが接続するMongoDBサーバーのアドレスを指定します。**MongoDB Mode** が `Replica Set` または `Sharding` の場合は、接続するすべてのMongoDBサーバーをカンマ区切りで入力してください。
     - **Replica Set Name**：レプリカセット名を指定します。文字列型。**MongoDB Mode** が `Replica Set` の場合のみ必要です。
     - **Database**：MongoDBのデータベース名。文字列型。
     - **Collection**：認証ルールが保存されているMongoDBコレクション名。文字列型。
     - **Username**：MongoDBのユーザー名を指定します。
     - **Password**：MongoDBのユーザーパスワードを指定します。
     - **Read Mode**（任意）：**MongoDB Mode** が `Replica Set` の場合のみ必要。デフォルトは `master`。選択肢は `master`、`slave_ok`。
       - **master**：各クエリは必ず最新のデータ（マスター/プライマリサーバー）から読み取る必要があります。接続先がマスターでない場合、最初の読み取りは失敗し、その後の操作は中止されます。
       - **slave_ok**：セカンダリ/スレーブサーバーから古いデータを読み取ることや、マスターから最新データを読み取ることを許可します。
     - **Write Mode**（任意）：**MongoDB Mode** が `Replica Set` の場合のみ必要。選択肢は `unsafe`、`safe`。デフォルトは `safe`。

   - 認証に関する設定：
     - **Password Hash Field**：パスワードのフィールド名を指定します。
     - **Password Hash**：プレーンテキストパスワードに適用し、データベースに保存する前のハッシュアルゴリズムを選択します。選択肢は `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります。
       - `md5`、`sha`、`sha256`、`sha512` の場合：
         - **Salt Position**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`、`prefix`、`disable` のいずれかです。外部ストレージからユーザー資格情報をEMQX組み込みデータベースに移行する場合を除き、デフォルト値のままで問題ありません。
         - ハッシュ結果は16進数文字列で表現され、大文字小文字を区別せずに保存された資格情報と比較されます。
       - `plain` の場合：
         - **Salt Position** は `disable` にしてください。
       - `bcrypt` の場合：
         - **Salt Rounds**：ハッシュ関数を適用する回数を定義します。2のべき乗で表現され、"コストファクター"とも呼ばれます。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ強化のためには高い値を推奨します。注：コストファクターを1増やすと認証にかかる時間がほぼ倍増します。
       - `pbkdf2` の場合：
         - **Pseudorandom Function**：鍵生成に使うハッシュ関数を選択します（例：`sha256`）。
         - **Iteration Count**：ハッシュ関数の実行回数を設定します。デフォルトは `4096`。
         - **Derived Key Length**（任意）：生成される鍵のバイト長を指定します。空欄の場合は選択した擬似乱数関数により決定されます。
         - ハッシュ結果は16進数文字列で表現され、大文字小文字を区別せずに保存された資格情報と比較されます。
     - **Salt Field**：MongoDB内のソルトフィールドを指定します。
     - **is_superuser Field**：ユーザーがスーパーユーザーかどうかを判定するフィールドを指定します。
     - **Precondition**：[Variform式](../../configuration/configuration.md#variform-expressions)で、MongoDB認証をクライアント接続に適用するかどうかを制御します。この式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証が実行されます。詳細は[Authenticator Preconditions](./authn.md#authenticator-preconditions)を参照してください。
     - **Enable TLS**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[Network and TLS](../../network/overview.md)を参照してください。
     - **Filter**：資格情報検索のためのMongoDBセレクターとして解釈されるマップ。[プレースホルダー](./authn.md#authentication-placeholders)がサポートされています。
     - **Advanced Settings**：同時接続数や接続タイムアウトまでの待機時間を設定します。
       - **Connection Pool size**（任意）：EMQXノードからMongoDBサーバーへの同時接続数を指定します。デフォルトは `8`。
       - **Connect Timeout**（任意）：接続がタイムアウトとみなされるまでの待機時間を設定します。単位はミリ秒、秒、分、時間がサポートされます。デフォルトは `20` 秒です。

5. 設定が完了したら、**Create** をクリックしてください。

## 設定項目による設定

EMQXのMongoDB認証は設定項目でも構成可能です。 <!--詳細な操作手順は [authn-mongodb:standalone](../../configuration/configuration-manual.html#authn-mongodb:standalone)、[authn-mongodb:sharded-cluster](../../configuration/configuration-manual.html#authn-mongodb:sharded-cluster)、[authn-mongodb:replica-set](../../configuration/configuration-manual.html#authn-mongodb:replica-set) を参照してください。-->

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
