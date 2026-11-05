# MongoDBとの統合

EMQXはパスワード認証のためにMongoDBとの統合をサポートしています。EMQXのMongoDB認証機能は、現在、Single、[レプリカセット](https://www.mongodb.com/docs/manual/reference/replica-configuration/)、および[シャーディング](https://www.mongodb.com/docs/manual/sharding/)の3つの異なるモードで稼働するMongoDBに接続することが可能です。本ページでは、サポートされるデータスキーマの詳細と、EMQXダッシュボードおよび設定ファイルでの設定方法について説明します。

::: tip

[基本的なEMQX認証の概念](./authn.md)についての知識

:::

## データスキーマとクエリ文

EMQXのMongoDB認証は、認証データをMongoDBのドキュメントとして保存することをサポートしています。ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認する必要があります。

- `password_hash`：必須；データベースに保存されるパスワード（プレーンテキストまたはハッシュ化されたもの）；このフィールドは名前変更が可能です。
- `salt`：任意；`salt = ""` またはこのフィールドを削除すると、ソルト値が追加されないことを示します；このフィールドは名前変更が可能です。
- `is_superuser`：任意；現在のクライアントがスーパーユーザーかどうかを示すフラグ；デフォルトは `false`；このフィールドは名前変更が可能です。

例えば、ユーザー名が `user123`、パスワードが `secret`、接尾辞としてのソルトが `salt_foo123`、パスワードハッシュが `sha256` のスーパーユーザー（`is_superuser`: `true`）のドキュメントを追加したい場合、クエリ文は以下のようになります。

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

システム内に多数のユーザーが存在する場合は、クエリの応答時間を短縮し、EMQXの負荷を軽減するために、事前にテーブルの最適化およびインデックス作成を行ってください。

:::

このMongoDBデータスキーマに対応するダッシュボードの設定パラメータは以下の通りです。

- **Password Hash**: `sha256`
- **Salt Position**: `suffix`
- **Collection**: `mqtt_user`
- **Filter**: `{ username = "${username}" }`
- **Password Hash field**: `password_hash`
- **Salt Field**: `salt`
- **is_superuser Field**： `is_superuser`

::: tip 読み取り専用データベース権限の付与

EMQXはクライアント認証時にMongoDBへの読み取りアクセスのみが必要です。設定されたコレクションに対して1回の`find_one`クエリを実行し、ドキュメントの挿入、更新、削除は行いません。

MongoDBには、該当データベースに対して組み込みの`read`ロールを持つ専用のデータベースユーザーを作成し、EMQXにそのユーザーを使用させてください。書き込みや管理者権限は付与しないでください。

```js
db.getSiblingDB("mqtt").createUser({
  user: "emqx_auth",
  pwd: "PASSWORD",
  roles: [ { role: "read", db: "mqtt" } ]
})
```

コレクションの作成やユーザーレコードの管理は、別の管理者ユーザーを使用してください。EMQX設定のデータベース認証情報が漏洩した場合でも、読み取り専用ユーザーでは認証データの変更や削除はできません。
:::

## ダッシュボードでの設定

EMQXダッシュボードを使って、MongoDBをパスワード認証に利用する設定が可能です。

1. EMQXダッシュボードの左側ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム**に **パスワードベース** を選択し、**バックエンド**に **MongoDB** を選択すると、以下のように **設定** タブが表示されます。

![authn-MongoDB_ee](./assets/authn-MongoDB_ee.png)

4. 以下の手順に従って認証バックエンドを設定します。

   - MongoDBへの接続情報を入力します：
     - **MongoDBモード**：MongoDBの展開方式を選択します。`Single`、`Replica Set`、`Sharding`のいずれかです。
     - **サーバー**：EMQXが接続するMongoDBサーバーのアドレスを指定します。**MongoDBモード**が`Replica Set`または`Sharding`の場合は、接続するすべてのMongoDBサーバーをカンマ区切りで入力してください。
     - **レプリカセット名**：`Replica Set`モードの場合に指定するレプリカセット名（文字列）。
     - **データベース**：MongoDBのデータベース名（文字列）。
     - **コレクション**：認証ルールが保存されているMongoDBコレクション名（文字列）。
     - **ユーザー名**：MongoDBのユーザー名。
     - **パスワード**：MongoDBのユーザーパスワード。
     - **読み取りモード**（任意）：`Replica Set`モードの場合のみ必要。デフォルトは`master`。選択肢は`master`、`slave_ok`。
       - **master**：クエリはすべてマスター（プライマリ）サーバーから最新データを読み取る必要があります。接続先がマスターでなければ最初の読み取りが失敗し、その後の操作は中止されます。
       - **slave_ok**：セカンダリ（スレーブ）サーバーの古いデータまたはマスターの最新データの読み取りを許可します。
     - **書き込みモード**（任意）：`Replica Set`モードの場合のみ必要。選択肢は`unsafe`、`safe`。デフォルトは`safe`。

   - 認証に関する設定を行います：
     - **パスワードハッシュフィールド**：パスワードのフィールド名を指定します。
     - **パスワードハッシュ**：プレーンテキストパスワードに適用し、結果をデータベースに保存するハッシュアルゴリズムを選択します。選択肢は`plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2`です。選択したアルゴリズムに応じて追加設定があります。
       - `md5`、`sha`、`sha256`、`sha512`の場合：
         - **ソルト位置**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`、`prefix`、`disable`のいずれかです。外部ストレージからEMQX組み込みデータベースにユーザー資格情報を移行する場合を除き、デフォルト値のままで問題ありません。
         - ハッシュ結果は16進数文字列で表現され、大文字小文字を区別せずに保存された資格情報と比較されます。
       - `plain`の場合：
         - **ソルト位置**は`disable`に設定してください。
       - `bcrypt`の場合：
         - **ソルトラウンド数**：ハッシュ関数を適用する回数を定義します。`2^Salt Rounds`として表現され、「コストファクター」とも呼ばれます。デフォルトは`10`で、許容範囲は`5`から`10`です。セキュリティ強化のためにはより高い値が推奨されます。注意：コストファクターを1増やすと認証に必要な時間が倍増します。
       - `pbkdf2`の場合：
         - **疑似乱数関数**：キー生成に使用するハッシュ関数を選択します（例：`sha256`）。
         - **反復回数**：ハッシュ関数を実行する回数を設定します。デフォルトは`4096`です。
         - **生成キー長**（任意）：生成されるキーのバイト長を指定します。空欄の場合は疑似乱数関数により決定される長さになります。
         - ハッシュ結果は16進数文字列で表現され、大文字小文字を区別せずに保存された資格情報と比較されます。
     - **ソルトフィールド**：MongoDB内のソルトフィールドを指定します。
     - **is_superuserフィールド**：ユーザーがスーパーユーザーかどうかを判定します。
     - **クライアントID上書きフィールド**：MongoDB認証結果のフィールド名を指定し、接続時にクライアントから提供されたClient IDを上書きできます。これにより認証データに基づいてユニークなClient IDを割り当て、多重テナントなどのセッション競合を防止できます。
     - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)を用いて、このMongoDB認証器をクライアント接続に適用するかどうかを制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列の `"true"` の場合のみ認証器が呼び出されます。そうでなければスキップされます。詳細は[認証器の前提条件](./authn.md#authenticator-preconditions)を参照してください。
     - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md)を参照してください。
     - **フィルター**：資格情報検索のためのMongoDBセレクターとして解釈されるマップ。[プレースホルダー](./authn.md#authentication-placeholders)がサポートされています。
     - **詳細設定**：同時接続数や接続タイムアウトまでの待機時間を設定します。
       - **接続プールサイズ**（任意）：EMQXノードからMongoDBサーバーへの同時接続数を指定します。デフォルトは`8`です。
       - **接続タイムアウト**（任意）：接続がタイムアウトとみなされるまでの待機時間を指定します。単位はミリ秒、秒、分、時間がサポートされます。デフォルトは`20`秒です。

5. 設定が完了したら、**作成**をクリックします。

## 設定ファイルでの設定

EMQXのMongoDB認証器は設定ファイルの設定項目でも構成可能です。<!--詳細な操作手順は[authn-mongodb:standalone](../../configuration/configuration-manual.html#authn-mongodb:standalone)、[authn-mongodb:sharded-cluster](../../configuration/configuration-manual.html#authn-mongodb:sharded-cluster)、[authn-mongodb:replica-set](../../configuration/configuration-manual.html#authn-mongodb:replica-set)を参照してください。-->

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
