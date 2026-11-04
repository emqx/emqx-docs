# MongoDBとの統合

EMQXはパスワード認証のためにMongoDBとの統合をサポートしています。EMQXのMongoDB認証機能は、現在、Single、[レプリカセット](https://www.mongodb.com/docs/manual/reference/replica-configuration/)、および[シャーディング](https://www.mongodb.com/docs/manual/sharding/)の3つの異なるモードで稼働しているMongoDBへの接続をサポートしています。本ページでは、サポートされているデータスキーマの詳細と、EMQXダッシュボードおよび設定ファイルでの設定方法について説明します。

::: tip

[基本的なEMQX認証の概念](./authn.md)についての知識

:::

## データスキーマとクエリ文

EMQX MongoDB認証機能は、認証データをMongoDBドキュメントとして保存することをサポートしています。ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認する必要があります。

- `password_hash`：必須。データベースに保存されているパスワード（プレーンテキストまたはハッシュ化済み）。このフィールドは名前の変更が可能です。
- `salt`：任意。`salt = ""` またはこのフィールドを削除すると、ソルト値が追加されないことを示します。このフィールドは名前の変更が可能です。
- `is_superuser`：任意。現在のクライアントがスーパーユーザーであるかを示すフラグ。デフォルトは `false`。このフィールドは名前の変更が可能です。

例えば、ユーザー名が `user123`、パスワードが `secret`、サフィックス付きソルトが `salt_foo123`、パスワードハッシュが `sha256` のスーパーユーザー（`is_superuser`: `true`）のドキュメントを追加したい場合、クエリ文は以下のようになります。

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

システム内のユーザー数が多い場合は、クエリ応答時間を短縮しEMQXの負荷を軽減するために、事前に対象テーブルの最適化とインデックス作成を行ってください。

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

EMQXはクライアント認証時にMongoDBへの読み取りアクセスのみを必要とします。設定されたコレクションに対して `find_one` クエリを1回実行し、ドキュメントの挿入、更新、削除は行いません。

EMQXには、データベースに組み込みの `read` ロールを持つ専用のデータベースユーザーを使用するよう設定してください。ユーザーに書き込みや管理者権限を付与しないでください。

```js
db.getSiblingDB("mqtt").createUser({
  user: "emqx_auth",
  pwd: "PASSWORD",
  roles: [ { role: "read", db: "mqtt" } ]
})
```

コレクションの作成やユーザーレコードの管理は別の管理者ユーザーで行ってください。EMQX設定内のデータベース認証情報が漏洩した場合でも、読み取り専用ユーザーでは認証データの変更や削除はできません。

:::

## ダッシュボードでの設定

EMQXダッシュボードを使ってMongoDBをパスワード認証に利用する設定が可能です。

1. EMQXダッシュボードの左側ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム** に **パスワードベース** を、**バックエンド** に **MongoDB** を選択し、**設定** タブに移動します。以下のように表示されます。

![authn-MongoDB_ee](./assets/authn-MongoDB_ee.png)

4. 以下の手順に従い認証バックエンドを設定します。

   - MongoDB接続情報を入力します：
     - **MongoDBモード**：MongoDBの展開形態を選択します。`Single`、`Replica Set`、`Sharding` のいずれかです。
     - **サーバー**：EMQXが接続するMongoDBサーバーのアドレスを指定します。**MongoDBモード**が `Replica Set` または `Sharding` の場合は、接続するすべてのMongoDBサーバーをカンマ（`,`）区切りで入力してください。
     - **レプリカセット名**：`Replica Set` モードの場合に指定するレプリカセット名。文字列型です。
     - **データベース**：MongoDBのデータベース名。文字列型です。
     - **コレクション**：認証ルールが格納されているMongoDBコレクション名。文字列型です。
     - **ユーザー名**：MongoDBのユーザー名を指定します。
     - **パスワード**：MongoDBユーザーのパスワードを指定します。
     - **読み取りモード**（任意）：`Replica Set` モードの場合のみ必要です。デフォルトは `master`。選択肢は `master`、`slave_ok` です。
       - **master**：クエリはすべてマスター（プライマリ）サーバーから最新データのみを読み取ります。接続先がマスターでない場合、最初の読み取りは失敗し、その後の操作は中止されます。
       - **slave_ok**：セカンダリ（スレーブ）サーバーから古いデータを、マスターからは最新データを読み取ることを許可します。
     - **書き込みモード**（任意）：`Replica Set` モードの場合のみ必要です。選択肢は `unsafe`、`safe`。デフォルトは `safe`。

   - 認証に関する設定を行います：
     - **パスワードハッシュフィールド**：パスワードのフィールド名を指定します。
     - **パスワードハッシュ**：プレーンテキストパスワードに適用し、データベースに保存される前のハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります。
       - `md5`、`sha`、`sha256`、`sha512` の場合：
         - **ソルト位置**：ソルト（ランダムデータ）をパスワードに混ぜる方法を指定します。`suffix`、`prefix`、`disable` のいずれかです。外部ストレージからユーザー認証情報をEMQX組み込みデータベースに移行する場合を除き、デフォルト値のままで問題ありません。
         - ハッシュ結果は16進数文字列で表現され、大文字小文字を区別せずに保存された認証情報と比較されます。
       - `plain` の場合：
         - **ソルト位置**：`disable` に設定してください。
       - `bcrypt` の場合：
         - **ソルトラウンド数**：ハッシュ関数の適用回数を定義します。値は _2^Salt Rounds_ として表され、「コストファクター」とも呼ばれます。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ強化のためにはより高い値が推奨されます。コストファクターを1増やすごとに認証にかかる時間は約2倍になります。
       - `pbkdf2` の場合：
         - **疑似乱数関数**：鍵生成に使用するハッシュ関数を選択します（例：`sha256`）。
         - **反復回数**：ハッシュ関数の実行回数を設定します。デフォルトは `4096`。
         - **派生鍵長**（任意）：生成される鍵のバイト長を指定します。空欄の場合は疑似乱数関数により決定される長さになります。
         - ハッシュ結果は16進数文字列で表現され、大文字小文字を区別せずに保存された認証情報と比較されます。
     - **ソルトフィールド**：MongoDB内のソルトフィールドを指定します。
     - **is_superuserフィールド**：ユーザーがスーパーユーザーかどうかを判定します。
     - **クライアントID上書きフィールド**：MongoDB認証結果内のフィールド名を指定し、接続時にクライアントから提供されたクライアントIDを上書きできます。これにより認証データに基づいてユニークなクライアントIDを割り当て、多重テナントなどのセッション競合を防止できます。
     - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)で、MongoDB認証機能をクライアント接続に適用するかどうかを制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証機能が呼び出されます。それ以外の場合はスキップされます。前提条件の詳細は[認証機能の前提条件](./authn.md#authenticator-preconditions)を参照してください。
     - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md)を参照してください。
     - **フィルター**：認証情報検索のためのMongoDBセレクターとして解釈されるマップ。[プレースホルダー](./authn.md#authentication-placeholders)がサポートされています。
     - **詳細設定**：同時接続数と接続タイムアウトまでの待機時間を設定します。
       - **コネクションプールサイズ**（任意）：EMQXノードからMongoDBサーバーへの同時接続数を指定します。デフォルトは `8`。
       - **接続タイムアウト**（任意）：接続がタイムアウトとみなされるまでの待機時間を指定します。単位はミリ秒、秒、分、時間がサポートされています。デフォルトは `20` 秒。

5. 設定が完了したら **作成** をクリックします。

## 設定ファイルでの設定

EMQXの設定項目を使ってMongoDB認証機能を設定できます。  
<!--詳細な操作手順は [authn-mongodb:standalone](../../configuration/configuration-manual.html#authn-mongodb:standalone)、[authn-mongodb:sharded-cluster](../../configuration/configuration-manual.html#authn-mongodb:sharded-cluster)、[authn-mongodb:replica-set](../../configuration/configuration-manual.html#authn-mongodb:replica-set) を参照してください。-->

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
