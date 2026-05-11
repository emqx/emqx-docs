# Redisとの統合

EMQXはパスワード認証のためにRedisとの統合をサポートしています。EMQXのRedis認証機能は、現在シングルモード、[Redis Sentinel](https://redis.io/docs/manual/sentinel/)、および[Redis Cluster](https://redis.io/docs/manual/scaling/)の3つの異なるモードで稼働しているRedisへの接続をサポートしています。本章では、サポートされているデータスキーマの詳細と、EMQXダッシュボードおよび設定ファイルでの設定方法について説明します。

::: tip 前提条件：

[EMQX認証の基本概念](../authn/authn.md)の知識

:::

## データスキーマとクエリ文

Redis認証は、あらかじめ定義されたフィールド名を持つ[Redisハッシュ](https://redis.io/docs/manual/data-types/#hashes)として保存された認証情報を使用します。

- `password_hash`: 必須；データベースに保存されるパスワード（プレーンテキストまたはハッシュ化済み）  
- `salt`: 任意；`salt = ""` またはこのフィールドを削除するとソルト値なしを示す  
- `is_superuser`: 任意；現在のクライアントがスーパーユーザーかどうかのフラグ；デフォルトは `false`

例えば、ユーザー名 `user123` のスーパーユーザー（`is_superuser`: `true`）を追加し、パスワードを `secret`、プレフィックスのソルトを `salt`、パスワードハッシュを `sha256` で保存する場合、クエリ文は以下のようになります。

```bash
>redis-cli
127.0.0.1:6379> HSET mqtt:user123 is_superuser 1 salt salt password_hash ac63a624e7074776d677dd61a003b8c803eb11db004d0ec6ae032a5d7c9c5caf
(integer) 3
```

対応する設定パラメータは以下の通りです。

```
password_hash_algorithm {
    name = sha256
    salt_position = prefix
}

cmd = "HMGET mqtt:${username} password_hash salt is_superuser"
```

::: tip
`password_hash` という名前はハッシュ化されたパスワードを保存することを意図しています。しかし、RedisにはMySQLのような `as` 構文がないため、EMQX 5.0ではEMQX 4.xで使われていた `password` フィールドとの互換性を保っています。

そのため、`cmd` を `HMGET mqtt:${username} password salt is_superuser` と設定することも可能です。
:::

## ダッシュボードでの設定

EMQXダッシュボードを使って、Redisをパスワード認証に利用する設定ができます。

1. EMQXダッシュボードの左側ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。  
2. **認証** ページの右上にある **作成** をクリックします。  
3. **メカニズム** に **パスワードベース** を選択し、**バックエンド** に **Redis** を選択すると、以下のように **設定** タブが表示されます。

<img src="./assets/authn-redis.png" alt="Redisによる認証" style="zoom:67%;" />

4. 以下の手順に従って認証バックエンドを設定します。

   - Redisへの接続情報を入力します。

     - **Redisモード**：Redisの展開形態を選択します。`Single`、`Sentinel`、`Cluster` のいずれかです。  
     - **サーバー**：EMQXが接続するRedisサーバーのアドレスを指定します。**Redisモード**が `Sentinel` または `Cluster` の場合は、接続するすべてのRedisサーバーをカンマ区切りで入力してください。  
     - **Sentinel名**：使用する名前を指定します。文字列型で、**Redisモード**が `Sentinel` の場合のみ必要です。  
     - **データベース**：Redisのデータベース名。文字列型です。  
     - **パスワード**：Redisユーザーのパスワードを指定します。  
   - 認証に関する設定を行います。

     - **パスワードハッシュ**：プレーンテキストのパスワードに適用し、結果をデータベースに保存する前に使うハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります。  
       - `md5`、`sha`、`sha256`、`sha512` の場合：  
         - **ソルト位置**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`（後置）、`prefix`（前置）、`disable`（無効）のいずれかです。外部ストレージからユーザー認証情報を移行する場合を除き、デフォルト値のままで問題ありません。  
         - ハッシュ結果は16進数文字列で表され、大文字小文字を区別せずに保存された認証情報と比較されます。  
       - `plain` の場合：  
         - **ソルト位置** は `disable` に設定してください。  
       - `bcrypt` の場合：  
         - **ソルトラウンド**：ハッシュ関数を適用する回数を定義します。2のべき乗で表される「コストファクター」とも呼ばれます。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ強化のためには高い値が推奨されます。注：コストファクターを1増やすごとに認証にかかる時間が倍増します。  
       - `pbkdf2` の場合：  
         - **疑似乱数関数**：キー生成に使うハッシュ関数を選択します（例：`sha256`）。  
         - **反復回数**：ハッシュ関数を実行する回数を設定します。デフォルトは `4096` です。  
         - **派生キー長**（任意）：生成されるキーのバイト長を指定します。空欄の場合は疑似乱数関数により決定される長さになります。  
         - ハッシュ結果は16進数文字列で表され、大文字小文字を区別せずに保存された認証情報と比較されます。  
   - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)を使って、このRedis認証をクライアント接続に適用するかどうかを制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証処理が実行されます。詳細は[認証の前提条件](./authn.md#authentication-preconditions)を参照してください。  
   - **TLSを有効化**：TLSを有効にしたい場合はスイッチをオンにします。TLSの有効化については[ネットワークとTLS](../../network/overview.md)を参照してください。  
   - **CMD**：Redisのクエリコマンドを指定します。  
   - **詳細設定**：  
     - **プールサイズ**（任意）：EMQXノードからRedisサーバーへの同時接続数を指定します。デフォルトは `8` です。  

5. 設定が完了したら、**作成** をクリックしてください。

## 設定ファイルでの設定

EMQXの設定項目を使ってRedis認証を設定することも可能です。  

Redis認証は `mechanism = password_based` と `backend = redis` で識別されます。

EMQXは3種類のRedis展開形態に対応しています。

:::: tabs type:card

::: tab シングルRedis

```bash
{
  mechanism = password_based
  backend = redis

  redis_type = single
  server = "127.0.0.1:6379"

  password_hash_algorithm {
      name = sha256
      salt_position = suffix
  }

  cmd = "HMGET mqtt_user:${username} password_hash salt is_superuser"
  database = 1
  password = "public"
  auto_reconnect = true
}
```

:::

::: tab Redis Sentinel

```bash
{
  mechanism = password_based
  backend = redis

  redis_type = sentinel
  servers = "10.123.13.11:6379,10.123.13.12:6379"
  sentinel = "mymaster"

  password_hash_algorithm {
      name = sha256
      salt_position = suffix
  }

  cmd = "HMGET mqtt_user:${username} password_hash salt is_superuser"
  database = 1
  password = "public"
  auto_reconnect = true
}
```

:::

::: tab Redis Cluster

```bash
{
  mechanism = password_based
  backend = redis

  redis_type = cluster
  servers = "10.123.13.11:6379,10.123.13.12:6379"

  password_hash_algorithm {
      name = sha256
      salt_position = suffix
  }

  cmd = "HMGET mqtt_user:${username} password_hash salt is_superuser"
  password = "public"
  auto_reconnect = true
}
```

:::

::::
