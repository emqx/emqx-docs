# MongoDBとの連携

EMQXはパスワード認証のためにMongoDBとの連携をサポートしています。EMQX MongoDB認証機能は、現在、Single、[レプリカセット](https://www.mongodb.com/docs/manual/reference/replica-configuration/)および[シャーディング](https://www.mongodb.com/docs/manual/sharding/)の3つの異なるモードで稼働するMongoDBへの接続に対応しています。本ページでは、サポートされるデータスキーマの詳細と、EMQXダッシュボードおよび設定ファイルでの設定方法について説明します。

::: tip

[基本的なEMQX認証の概念](./authn.md)についての知識

:::

## データスキーマとクエリ文

EMQX MongoDB認証機能は、認証データをMongoDBドキュメントとして保存することをサポートしています。ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認する必要があります。

- `password_hash`：必須。データベースに保存されるパスワード（プレーンテキストまたはハッシュ化済み）。このフィールドは名前変更が可能です。
- `salt`：任意。`salt = ""` またはこのフィールドを削除すると、ソルト値が追加されないことを示します。このフィールドは名前変更が可能です。
- `is_superuser`：任意。現在のクライアントがスーパーユーザーかどうかのフラグ。デフォルトは `false`。このフィールドは名前変更が可能です。

例えば、ユーザー名が `user123`、パスワードが `secret`、サフィックスとしてソルト `salt_foo123` を付加し、パスワードハッシュが `sha256` のスーパーユーザー（`is_superuser`: `true`）のドキュメントを追加したい場合、クエリ文は以下のようになります。

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

システム内のユーザー数が多い場合は、クエリ応答時間を短縮しEMQXの負荷を軽減するために、事前にテーブルの最適化やインデックス作成を行ってください。

 :::

このMongoDBデータスキーマに対応するダッシュボードの設定パラメータは以下の通りです。

- **Password Hash**：`sha256`
- **Salt Position**：`suffix`
- **Collection**：`mqtt_user`
- **Filter**：`{ username = "${username}" }`
- **Password Hash field**：`password_hash`
- **Salt Field**：`salt`
- **is_superuser Field**：`is_superuser`

::: tip 読み取り専用データベース権限の付与

EMQXはクライアント認証時にMongoDBへの読み取りアクセスのみを必要とします。設定されたコレクションに対して `find_one` クエリを1回実行し、ドキュメントの挿入、更新、削除は行いません。

MongoDBには、データベースに組み込みの `read` ロールを持つ専用のデータベースユーザーを作成し、EMQXにそのユーザーを使用させてください。書き込みや管理者権限は付与しないでください。

```js
db.getSiblingDB("mqtt").createUser({
  user: "emqx_auth",
  pwd: "PASSWORD",
  roles: [ { role: "read", db: "mqtt" } ]
})
```

コレクションの作成やユーザーレコードの管理は別の管理者ユーザーで行ってください。EMQX設定内のデータベース認証情報が漏洩しても、読み取り専用ユーザーでは認証データの変更や削除はできません。
:::

## ダッシュボードでの設定

EMQXダッシュボードを使って、MongoDBをパスワード認証に利用する設定が可能です。

1. EMQXダッシュボードの左側ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム**で **Password-Based** を選択し、**バックエンド**で **MongoDB** を選択すると、以下のように **設定** タブに遷移します。

![authn-MongoDB_ee](./assets/authn-MongoDB_ee.png)

4. 以下の手順に従い認証バックエンドを設定してください。

   - MongoDB接続情報を入力します：
     - **MongoDBモード**：MongoDBの展開形態を選択します。`Single`、`Replica Set`、`Sharding` から選択可能です。
     - **サーバー**：EMQXが接続するMongoDBサーバーのアドレスを指定します。**MongoDBモード**が `Replica Set` または `Sharding` の場合は、接続する全てのMongoDBサーバーをカンマ区切りで入力してください。
     - **レプリカセット名**：`Replica Set` モードの場合に指定するレプリカセット名。文字列型です。
     - **データベース**：MongoDBのデータベース名。文字列型です。
     - **コレクション**：認証ルールが格納されているMongoDBコレクション名。文字列型です。
     - **ユーザー名**：MongoDBのユーザー名を指定します。
     - **パスワード**：MongoDBユーザーのパスワードを指定します。
     - **読み取りモード**（任意）：`Replica Set` モードの場合にのみ必要です。デフォルトは `master`。選択肢は `master` と `slave_ok` です。
       - **master**：各クエリはマスター（プライマリ）サーバーから最新のデータのみを読み取ります。接続先がマスターでない場合、最初の読み取りが失敗し、その後の操作は中止されます。
       - **slave_ok**：セカンダリ（スレーブ）サーバーからの古いデータまたはマスターからの最新データの読み取りを許可します。
     - **書き込みモード**（任意）：`Replica Set` モードの場合にのみ必要です。選択肢は `unsafe` と `safe`。デフォルトは `safe`。

   - 認証に関する設定を行います：
     - **パスワードハッシュフィールド**：パスワードのフィールド名を指定します。
     - **パスワードハッシュ**：プレーンテキストのパスワードに適用し、データベースに格納する前のハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定が必要です。
       - `md5`、`sha`、`sha256`、`sha512` の場合：
         - **ソルト位置**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`、`prefix`、`disable` のいずれかを選択します。外部ストレージからEMQX組み込みデータベースへユーザー認証情報を移行しない限り、デフォルト値のままで問題ありません。
         - ハッシュ結果は16進数文字列で表現され、大文字小文字を区別せずに保存された認証情報と比較されます。
       - `plain` の場合：
         - **ソルト位置**は `disable` にしてください。
       - `bcrypt` の場合：
         - **ソルトラウンド数**：ハッシュ関数を適用する回数を定義します。値は _2^ソルトラウンド数_ で表され、コストファクターとも呼ばれます。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ強化のためには高い値を推奨します。コストファクターを1増やすごとに認証に必要な時間は倍増します。
       - `pbkdf2` の場合：
         - **疑似乱数関数**：キー生成に用いるハッシュ関数を選択します（例：`sha256`）。
         - **繰り返し回数**：ハッシュ関数の実行回数を設定します。デフォルトは `4096`。
         - **派生キー長**（任意）：生成されるキーのバイト長を指定します。空欄の場合は疑似乱数関数により決定される長さになります。
         - ハッシュ結果は16進数文字列で表現され、大文字小文字を区別せずに保存された認証情報と比較されます。
     - **ソルトフィールド**：MongoDB内のソルトフィールドを指定します。
     - **is_superuserフィールド**：ユーザーがスーパーユーザーかどうかを判定するフィールドを指定します。
     - **クライアントID上書きフィールド**：MongoDB認証結果の中で、MQTTクライアント接続のクライアントIDを上書きするために使用するフィールド名を指定します。これにより認証データに基づいて一意のクライアントIDを割り当てることができ、マルチテナントなどのセッション競合を防止できます。
     - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)を用いて、このMongoDB認証機能をクライアント接続に適用するかを制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証機能が呼び出されます。それ以外の場合はスキップされます。詳細は[認証機能の前提条件](./authn.md#authenticator-preconditions)を参照してください。
     - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md)を参照してください。
     - **フィルター**：認証情報の検索に使われるMongoDBセレクターとして解釈されるマップ。[プレースホルダー](./authn.md#authentication-placeholders)が利用可能です。
     - **詳細設定**：以下の任意の接続設定を行います。
       - **認証ソース**：ユーザー認証情報が保存されているMongoDBデータベースを指定します。例えば、ユーザーが `admin` データベースで作成されている場合は `admin` と入力します。
       - **レガシープロトコルの使用**：EMQXがMongoDBのレガシープロトコルをどのように使用するかを選択します。`auto` はサーバーがサポートするプロトコルを自動検出し、`true` はレガシープロトコルを使用、`false` は使用しません。デフォルトは `auto` です。
       - **コネクションプールサイズ**：EMQXノードからMongoDBサーバーへの同時接続数を指定します。デフォルトは `8` です。
       - **接続タイムアウト**：接続確立までの待機時間を指定します。単位はミリ秒、秒、分、時間がサポートされています。デフォルトは `20` 秒です。

5. 設定が完了したら、**作成** をクリックしてください。

## 設定項目による設定

EMQXの設定項目を使ってMongoDB認証機能を設定することも可能です。<!--詳細な操作手順は [authn-mongodb:standalone](../../configuration/configuration-manual.html#authn-mongodb:standalone)、[authn-mongodb:sharded-cluster](../../configuration/configuration-manual.html#authn-mongodb:sharded-cluster)、[authn-mongodb:replica-set](../../configuration/configuration-manual.html#authn-mongodb:replica-set) を参照してください。-->

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
