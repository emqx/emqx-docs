# MongoDBとの統合

<<<<<<< HEAD
EMQXはパスワード認証のためにMongoDBとの統合をサポートしています。EMQXのMongoDB認証機能は、現在、Single、[レプリカセット](https://www.mongodb.com/docs/manual/reference/replica-configuration/)、および[シャーディング](https://www.mongodb.com/docs/manual/sharding/)の3つの異なるモードで動作するMongoDBへの接続をサポートしています。本ページでは、サポートされるデータスキーマの詳細と、EMQXダッシュボードおよび設定ファイルでの設定方法について説明します。

::: tip

[基本的なEMQX認証の概念](../authn/authn.md)についての知識
=======
EMQXはパスワード認証のためにMongoDBとの統合をサポートしています。EMQXのMongoDB認証機能は現在、Single、[レプリカセット](https://www.mongodb.com/docs/manual/reference/replica-configuration/)、および[シャーディング](https://www.mongodb.com/docs/manual/sharding/)の3つの異なるモードで稼働するMongoDBへの接続をサポートしています。本ページでは、サポートされているデータスキーマの詳細と、EMQXダッシュボードおよび設定ファイルでの設定方法について説明します。

::: tip

[基本的なEMQX認証の概念](../authn/authn.md)の知識を推奨します。
>>>>>>> origin/release-6.1

:::

## データスキーマとクエリ文

<<<<<<< HEAD
EMQX MongoDB認証機能は、認証データをMongoDBドキュメントとして保存することをサポートしています。ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認する必要があります。

- `password_hash`：必須。データベースに保存されるパスワード（平文またはハッシュ化済み）。このフィールドは名前変更が可能です。
- `salt`：任意。`salt = ""` またはこのフィールドを削除すると、ソルト値が追加されないことを示します。このフィールドは名前変更が可能です。
- `is_superuser`：任意。現在のクライアントがスーパーユーザーかどうかを示すフラグ。デフォルトは `false`。このフィールドは名前変更が可能です。

例えば、ユーザー名が `user123`、パスワードが `secret`、サフィックスとしてソルト `salt_foo123` を付与し、パスワードハッシュが `sha256` のスーパーユーザー（`is_superuser`：`true`）のドキュメントを追加する場合、クエリ文は以下のようになります。
=======
EMQXのMongoDB認証機能は、認証データをMongoDBドキュメントとして保存することをサポートしています。ユーザーはクエリ文のテンプレートを提供し、以下のフィールドが含まれていることを確認する必要があります。

- `password_hash`：必須。データベースに保存されるパスワード（プレーンテキストまたはハッシュ済み）。このフィールドは名前変更が可能です。
- `salt`：任意。`salt = ""` またはこのフィールドを削除すると、ソルト値が追加されないことを示します。このフィールドは名前変更が可能です。
- `is_superuser`：任意。現在のクライアントがスーパーユーザーかどうかのフラグ。デフォルトは `false`。このフィールドは名前変更が可能です。

例えば、ユーザー名が `user123`、パスワードが `secret`、サフィックスとして `salt_foo123` のソルトを付与し、パスワードハッシュが `sha256` のスーパーユーザー（`is_superuser`: `true`）のドキュメントを追加したい場合、クエリ文は以下のようになります。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
システム内のユーザー数が多い場合は、クエリ応答時間を短縮しEMQXの負荷を軽減するために、事前にテーブルの最適化およびインデックス作成を行ってください。
=======
システム内のユーザー数が多い場合は、クエリ応答時間を短縮しEMQXの負荷を軽減するために、事前にテーブルの最適化およびインデックスの設定を行ってください。
>>>>>>> origin/release-6.1

 :::

このMongoDBデータスキーマに対応するダッシュボードの設定パラメータは以下の通りです：

- **Password Hash**：`sha256`
- **Salt Position**：`suffix`
- **Collection**：`mqtt_user`
- **Filter**：`{ username = "${username}" }`
- **Password Hash field**：`password_hash`
- **Salt Field**：`salt`
- **is_superuser Field**：`is_superuser`

## ダッシュボードでの設定

EMQXダッシュボードを使ってMongoDBをパスワード認証に利用する設定が可能です。

<<<<<<< HEAD
1. EMQXダッシュボードの左側ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム**に **Password-Based** を、**バックエンド**に **MongoDB** を選択し、**設定**タブに進みます。以下のように表示されます。

![authn-MongoDB_ee](./assets/authn-MongoDB_ee.png)

4. 以下の指示に従って認証バックエンドを設定します。

   - MongoDBへの接続情報を入力します：
     - **MongoDBモード**：MongoDBの展開形態を選択します。`Single`、`Replica Set`、`Sharding` のいずれかです。
     - **サーバー**：EMQXが接続するMongoDBサーバーのアドレスを指定します。**MongoDBモード**が `Replica Set` または `Sharding` の場合は、接続するすべてのMongoDBサーバーをカンマ（`,`）区切りで入力してください。
     - **レプリカセット名**：使用するレプリカセット名を指定します。文字列型。**MongoDBモード**が `Replica Set` の場合のみ必要です。
     - **データベース**：MongoDBのデータベース名。文字列型。
     - **コレクション**：認証ルールが保存されているMongoDBコレクション名。文字列型。
     - **ユーザー名**：MongoDBのユーザー名を指定します。
     - **パスワード**：MongoDBのユーザーパスワードを指定します。
     - **読み取りモード**（任意）：**MongoDBモード**が `Replica Set` の場合のみ必要。デフォルトは `master`。選択肢は `master`、`slave_ok`。
       - **master**：各クエリはシーケンス内で常に最新のデータ（マスター/プライマリサーバーから）を読み取る必要があります。接続先がマスターでない場合、最初の読み取りは失敗し、その後の操作は中止されます。
       - **slave_ok**：セカンダリ/スレーブサーバーからの古いデータ、またはマスターからの最新データの読み取りを許可します。
     - **書き込みモード**（任意）：**MongoDBモード**が `Replica Set` の場合のみ必要。選択肢は `unsafe`、`safe`。デフォルトは `safe`。

   - 認証に関する設定を行います：
     - **Password Hash Field**：パスワードのフィールド名を指定します。
     - **Password Hash**：平文パスワードに適用し、データベースに保存する前のハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定が必要です。
       - `md5`、`sha`、`sha256`、`sha512` の場合：
         - **Salt Position**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`、`prefix`、`disable` のいずれかです。外部ストレージからEMQX内蔵データベースにユーザー資格情報を移行する場合を除き、デフォルト値のままで問題ありません。
         - ハッシュ結果は16進数文字列で表現され、大文字小文字を区別せずに保存された資格情報と比較されます。
       - `plain` の場合：
         - **Salt Position** は `disable` に設定してください。
       - `bcrypt` の場合：
         - **Salt Rounds**：ハッシュ関数が適用される回数を定義します。値は _2^Salt Rounds_ で表され、「コストファクター」とも呼ばれます。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ強化のために高い値が推奨されます。注：コストファクターを1増やすと認証にかかる時間が倍増します。
       - `pbkdf2` の場合：
         - **疑似乱数関数**：キー生成に用いるハッシュ関数を選択します（例：`sha256`）。
         - **反復回数**：ハッシュ関数の実行回数を設定します。デフォルトは `4096`。
         - **派生キー長**（任意）：生成されるキーのバイト長を指定します。空欄の場合は選択した疑似乱数関数のデフォルト長になります。
         - ハッシュ結果は16進数文字列で表現され、大文字小文字を区別せずに保存された資格情報と比較されます。
     - **Salt Field**：MongoDB内のソルトフィールドを指定します。
     - **is_superuser Field**：ユーザーがスーパーユーザーかどうかを判定するフィールドを指定します。
     - **Client ID Override Field**：MongoDB認証結果内のフィールド名を指定し、接続時にクライアントが提供したClient IDを上書きできます。これにより認証データに基づいて一意のClient IDを割り当て、多重テナントなどのセッション競合を防止できます。
     - **Precondition**：[Variform式](../../configuration/configuration.md#variform-expressions)を使用して、このMongoDB認証機能をクライアント接続に適用するかどうかを制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証機能が呼び出されます。それ以外の場合はスキップされます。詳細は[認証機能の前提条件](./authn.md#authenticator-preconditions)を参照してください。
     - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md)を参照してください。
     - **Filter**：資格情報検索のためのMongoDBセレクターとして解釈されるマップ。[プレースホルダー](./authn.md#authentication-placeholders)がサポートされています。
     - **詳細設定**：同時接続数および接続タイムアウトまでの待機時間を設定します。
       - **コネクションプールサイズ**（任意）：EMQXノードからMongoDBサーバーへの同時接続数を指定します。デフォルトは `8`。
       - **接続タイムアウト**（任意）：接続がタイムアウトと見なされるまでの待機時間を指定します。単位はミリ秒、秒、分、時間がサポートされます。デフォルトは `20` 秒。

5. 設定が完了したら、**作成** をクリックします。

## 設定項目による設定

EMQXの設定項目を使ってMongoDB認証機能を設定することも可能です。<!--詳細な操作手順は [authn-mongodb:standalone](../../configuration/configuration-manual.html#authn-mongodb:standalone)、[authn-mongodb:sharded-cluster](../../configuration/configuration-manual.html#authn-mongodb:sharded-cluster)、および [authn-mongodb:replica-set](../../configuration/configuration-manual.html#authn-mongodb:replica-set) を参照してください。-->
=======
1. EMQXダッシュボードの左側ナビゲーションメニューから **Access Control** -> **Authentication** をクリックします。
2. **Authentication** ページの右上にある **Create** をクリックします。
3. **Mechanism** に **Password-Based** を選択し、**Backend** に **MongoDB** を選択すると、以下のように **Configuration** タブに移動します。

![authn-MongoDB_ee](./assets/authn-MongoDB_ee.png)

4. 以下の手順に従い認証バックエンドの設定を行います：

   - MongoDBへの接続情報を入力します：
     - **MongoDB Mode**：MongoDBの展開形態を選択します。`Single`、`Replica Set`、`Sharding` のいずれかです。
     - **Server**：EMQXが接続するMongoDBサーバーのアドレスを指定します。**MongoDB Mode** が `Replica Set` または `Sharding` の場合は、接続するすべてのMongoDBサーバーをカンマ（`,`）で区切って入力してください。
     - **Replica Set Name**：使用するレプリカセット名を指定します。文字列型で、**MongoDB Mode** が `Replica Set` の場合のみ必要です。
     - **Database**：MongoDBのデータベース名。文字列型です。
     - **Collection**：認証ルールが保存されているMongoDBコレクション名。文字列型です。
     - **Username**：MongoDBのユーザー名を指定します。
     - **Password**：MongoDBのユーザーパスワードを指定します。
     - **Read Mode**（任意）：**MongoDB Mode** が `Replica Set` の場合のみ必要です。デフォルトは `master`。選択肢は `master`、`slave_ok` です。
       - **master**：クエリはすべて最新のデータ（マスター／プライマリサーバー）からのみ読み取ります。接続先がマスターでない場合、最初の読み取りは失敗し、その後の操作は中止されます。
       - **slave_ok**：クエリはセカンダリ／スレーブサーバーからの古いデータまたはマスターからの最新データを読み取ることができます。
     - **Write Mode**（任意）：**MongoDB Mode** が `Replica Set` の場合のみ必要です。選択肢は `unsafe`、`safe` で、デフォルトは `safe` です。

   - 認証に関する設定を行います：
     - **Password Hash Field**：パスワードのフィールド名を指定します。
     - **Password Hash**：プレーンテキストのパスワードに適用され、データベースに保存される前のハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定が必要です。
       - `md5`、`sha`、`sha256`、`sha512` の場合：
         - **Salt Position**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`、`prefix`、`disable` のいずれかです。外部ストレージからユーザー認証情報を移行する場合を除き、デフォルト値のままで問題ありません。
         - ハッシュ結果は16進数文字列で表され、大文字小文字を区別せずに保存された認証情報と比較されます。
       - `plain` の場合：
         - **Salt Position** は `disable` に設定してください。
       - `bcrypt` の場合：
         - **Salt Rounds**：ハッシュ関数の適用回数を定義します。2のべき乗（_2^Salt Rounds_）で表され、「コストファクター」とも呼ばれます。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ強化のためにはより高い値を推奨します。注：コストファクターを1増やすごとに認証に必要な時間が倍増します。
       - `pbkdf2` の場合：
         - **Pseudorandom Function**：キー生成に用いるハッシュ関数を選択します（例：`sha256`）。
         - **Iteration Count**：ハッシュ関数の実行回数を設定します。デフォルトは `4096` です。
         - **Derived Key Length**（任意）：生成されるキーのバイト長を指定します。空欄の場合は選択された擬似乱数関数に基づく長さになります。
         - ハッシュ結果は16進数文字列で表され、大文字小文字を区別せずに保存された認証情報と比較されます。
     - **Salt Field**：MongoDBのソルトフィールドを指定します。
     - **is_superuser Field**：ユーザーがスーパーユーザーかどうかを判定するフィールドを指定します。
     - **Client ID Override Field**：MongoDB認証結果のフィールド名を指定し、接続時にクライアントが提供したClient IDを上書き可能にします。これにより認証データに基づいて一意のClient IDを割り当て、多重テナントなどのシナリオでセッション競合を防止できます。
     - **Precondition**：[Variform式](../../configuration/configuration.md#variform-expressions)を用いて、このMongoDB認証機能をクライアント接続に適用するかどうかを制御します。この式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証機能が呼び出されます。それ以外の場合はスキップされます。詳細は[Authenticator Preconditions](./authn.md#authenticator-preconditions)を参照してください。
     - **Enable TLS**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[Network and TLS](../../network/overview.md)を参照してください。
     - **Filter**：MongoDBのセレクターとして解釈されるマップ。認証プレースホルダーがサポートされています。
     - **Advanced Settings**：同時接続数と接続タイムアウトまでの待機時間を設定します。
       - **Connection Pool size**（任意）：EMQXノードからMongoDBサーバーへの同時接続数を指定します。デフォルトは `8` です。
       - **Connect Timeout**（任意）：接続がタイムアウトと見なされるまでの待機時間を指定します。単位はミリ秒、秒、分、時間に対応。デフォルトは `20` 秒です。

5. 設定が完了したら、**Create** をクリックしてください。

## 設定項目による設定

EMQXのMongoDB認証機能は設定ファイルでも設定可能です。  
<!--詳細な操作手順は [authn-mongodb:standalone](../../configuration/configuration-manual.html#authn-mongodb:standalone)、[authn-mongodb:sharded-cluster](../../configuration/configuration-manual.html#authn-mongodb:sharded-cluster)、[authn-mongodb:replica-set](../../configuration/configuration-manual.html#authn-mongodb:replica-set) を参照してください。-->
>>>>>>> origin/release-6.1

以下は参考となるコード例です：

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
