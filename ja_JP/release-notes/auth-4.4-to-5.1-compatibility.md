# EMQX 4.4 と EMQX 5.1 間の認証／認可の互換性問題

本ページでは、EMQX 4.4 と EMQX 5.1 間の認証および認可設定の互換性情報を紹介します。

## 共通の互換性変更点

### SSL オプション

EMQX 5.1 では、MySQL、PostgreSQL、MongoDB、Redis などの外部リソースにアクセスする際や、HTTPS 経由でのパスワード認証を行う場合に TLS を有効にするオプションが提供されています。詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。

### プレースホルダー

MySQL、PostgreSQL、MongoDB、Redis、外部リクエスト用の HTTP、JWT のデータ補間をサポートするバックエンドは、従来の `%X` 形式のプレースホルダーから `${variable}` 形式に変更されました。

## 認証

### 共通の変更点（すべての認証ソース）

#### パスワードハッシュ

組み込みデータベース、MySQL、PostgreSQL、MongoDB、Redis などのパスワードベースのプロバイダーはすべて、同じ `password_hash` オプションを持ち、同様に設定されます。詳細は [Password Hashing](../access-control/authn/authn.md#password-hashing) を参照してください。

#### リスナーごとの認証

EMQX 5.1 では、4.4 と異なり、各 MQTT リスナーごとに独自の認証設定が可能です。さらに、`enable_authn` リスナーオプションが追加されました：

- `enable_authn=true`（デフォルト）は認証チェーンに認証を委譲します。
- `enable_authn=false` はリスナーの認証を完全に無効化します。
- `enable_authn=quick_deny_anonymous` は `true` と同様ですが、認証情報なしで接続するクライアントを即座に拒否します。

#### 匿名アクセス機構の削除

EMQX 5.1 では明示的な `allow_anonymous` 設定はなくなりました。すべてのクライアントはデフォルトで接続が許可されます。認証器を追加して有効化すると、EMQX はクライアントの認証を試みます。匿名アクセスを許可するには、グローバルまたはリスナー固有の認証チェーンからすべての認証器を削除または無効化してください。

設定された認証チェーンを通過しても、いずれの認証器もクライアントの接続許可を判断できなければ、接続は拒否されます。

`bypass_auth_plugins` 設定も削除されました。すべてのクライアントを認証なしで接続許可したい場合は、`listeners.{type}.{name}.enable_authn = false` を設定してください。

### 組み込みデータベース（Mnesia）

- Mnesia は「組み込み」データベースと呼ばれ、設定内にユーザーレコードはありません。
- `password_hash` は `password_hash_algorithm` に変更され、`{name = Algo, salt_position = prefix}` の形式です。詳細は [Password Hashing](../access-control/authn/authn.md#password-hashing) を参照してください。
- `user_id_type` は MQTT ユーザー識別子として `clientid` または `username` のどちらを使うかを指定します。混在は許されません。
- 認証データレコード管理用の REST API が変更されました。詳細は `POST /authentication/{id}/users` の API ドキュメントを参照してください。
- 古いバージョンからのデータインポート用 API も利用可能です。詳細は `POST /authentication/{id}/import_users` を参照してください。

#### 例

EMQX 4.4

```
auth.mnesia.password_hash = sha256
```

EMQX 5.1

```
authentication {
   backend = built_in_database
   mechanism = password_based
   password_hash_algorithm {
      name = sha256
      salt_position = prefix
   }
   user_id_type = username
   enable = true
}
```

### 組み込みデータベース（強化認証）

- EMQX 4.4 で使われていた SHA1 ハッシュはサポートされません。`algorithm` パラメータで `sha512` または `sha256` を選択してください。
- `iteration_count` でハッシュ関数の計算回数を指定可能です（4.4 では暗黙的に 4096 回）。

#### 例

EMQX 4.4

```
# 設定なし
```

EMQX 5.1

```
{
    mechanism = scram
    backend = built_in_database
    enable = true

    algorithm = sha512
    iteration_count = 4096
}
```

### Redis

```
  mechanism = password_based
  backend = redis
```

- `type` は `redis_type` に変更されました。
  - `single` タイプの場合、`server` は `servers` に変更。
  - `sentinel` タイプの場合も同様に `server` は `servers` に変更。
  - `cluster` タイプでは `database` オプションは廃止。
- `database` は `cluster` タイプ以外では引き続き使用可能。
- `pool` は `pool_size` に変更。
- `password` はそのまま。
- `query_timeout` は廃止。
- `ssl.*` オプションは共通の SSL オプションに変更。詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。
- `auth_cmd` は `cmd` に変更。Redis ハッシュデータ構造と `HGET`、`HMGET` コマンドのみサポート。コマンド内で `${var}` 形式のプレースホルダーを使用可能。コマンドは少なくとも `password`（4.x 互換）または `password_hash` フィールドを取得し、オプションで `salt` と `is_superuser` フィールドも取得してください。
- `super_cmd` は廃止。代わりに `cmd` 内で `is_superuser` フィールドを提供してください。クライアントにスーパーユーザー権限を与える場合は、Redis クエリコマンドに `is_superuser` フィールドを追加してください。

  ::: details
  
  ```shell
  # NG
  GET emqx_user:${username}
  # NG
  HMGET emqx_user:${username} passwd
  
  # OK
  HMGET emqx_user:${username} password_hash
  
  # OK
  HMGET emqx_user:${username} password_hash is_superuser
  ```
  
  :::

- `password_hash` は共通の `password_hash_algorithm` パラメータを使用。

障害時に Redis に自動再接続するには `auto_reconnect` を使用できます。

#### 例

EMQX 4.4

```
auth.redis.type = single
auth.redis.server = 127.0.0.1:6379
auth.redis.pool = 8
auth.redis.database = 0
auth.redis.password = pass
password salt
auth.redis.auth_cmd = HMGET mqtt_user:%u password salt

auth.redis.password_hash = salt,sha256

auth.redis.ssl = on
auth.redis.ssl.cacertfile = path/to/your/cafile.pem
auth.redis.ssl.certfile = path/to/your/certfile
auth.redis.ssl.keyfile = path/to/your/keyfile
auth.redis.ssl.verify = true
auth.redis.ssl.server_name_indication = myredis
```

EMQX 5.1

```
authentication {
  mechanism = password_based
  backend = redis
  enable = true

  redis_type = single
  server = "127.0.0.1:6379"

  password_hash_algorithm {
      name = sha256
      salt_position = prefix
  }

  cmd = "HMGET mqtt_user:${username} password salt"
  database = 0
  password = "pass"
  auto_reconnect = true
  ssl {
    enable = true
    verify = verify_peer
   
    keyfile = path/to/your/keyfile
    certfile = path/to/your/certfile
    cacertfile = path/to/your/cafile.pem
    
    server_name_indication = myredis
  }
}
```

### MySQL

```
  backend = mysql
  mechanism = password_based
```

- `server`、`username`、`password`、`database`、`query_timeout` は引き続き使用可能。
- `pool` は `pool_size` に変更。
- `ssl.*` は共通の SSL オプションに変更。詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。
- `password_hash` は共通の `password_hash_algorithm` パラメータに変更。
- `auth_query` は `query` に変更。`${var}` 形式のプレースホルダーを使用。クエリは少なくとも `password` または `password_hash` カラムを取得し、オプションで `salt` と `is_superuser` カラムを取得してください。
- `super_query` は廃止。`is_superuser` カラムは `query` 内で提供してください。クライアントにスーパーユーザー権限を与える場合は、認証 SQL の結果に `is_superuser` フィールドを含めてください。

  ```sql
  SELECT
    password as password_hash,
    salt,
    is_superuser
  FROM mqtt_user
    where username = ${username} LIMIT 1
  ```

障害時に MySQL に自動再接続するには `auto_reconnect` を使用可能。

#### 例

EMQX 4.4

```
auth.mysql.server = 127.0.0.1:3306
auth.mysql.pool = 8
auth.mysql.username = dbuser
auth.mysql.database = mqtt

auth.mysql.query_timeout = 5s

auth.mysql.auth_query = select password_hash as password from mqtt where username = '%u' limit 1
auth.mysql.super_query = select is_superuser from mqtt where username = '%u' limit 1

auth.mysql.ssl = on
auth.mysql.ssl.cacertfile = path/to/your/cafile.pem
auth.mysql.ssl.certfile = path/to/your/certfile
auth.mysql.ssl.keyfile = path/to/your/keyfile
auth.mysql.ssl.verify = true
auth.mysql.ssl.server_name_indication = mymysql
```

EMQX 5.1

```
authentication {
  backend = mysql
  mechanism = password_based
  
  enable = true

  server = "127.0.0.1:3306"
  username = "dbuser"
  database = "mqtt"
  password = "dbpass"
  pool_size = 8

  password_hash_algorithm {
      name = sha256
      salt_position = prefix
  }

  query = "SELECT password_hash, salt, is_superuser FROM mqtt where username = ${username} LIMIT 1"
  query_timeout = "5s"
  
  auto_reconnect = true
  ssl {
    enable = true
    verify = verify_peer
   
    keyfile = path/to/your/keyfile
    certfile = path/to/your/certfile
    cacertfile = path/to/your/cafile.pem
    
    server_name_indication = mymysql
  }
}
```

### PostgreSQL

```
  mechanism = password_based
  backend = postgresql
```

- `server`、`username`、`password`、`database` は引き続き使用可能。
- `query_timeout` は廃止。
- `encoding` は廃止。
- `pool` は `pool_size` に変更。
- `ssl.*` は共通の SSL オプションに変更。詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。
- `password_hash` は共通の `password_hash_algorithm` パラメータに変更。
- `auth_query` は `query` に変更。`${var}` 形式のプレースホルダーを使用。クエリは少なくとも `password` または `password_hash` カラムを取得し、オプションで `salt` と `is_superuser` カラムを取得してください。
- `super_query` は廃止。`is_superuser` カラムは `query` 内で提供してください。クライアントにスーパーユーザー権限を与える場合は、認証 SQL の結果に `is_superuser` フィールドを含めてください。

  ```sql
  SELECT
    password as password_hash,
    salt,
    is_superuser
  FROM mqtt_user
    where username = ${username} LIMIT 1
  ```

#### 例

EMQX 4.4

```
auth.pgsql.server = 127.0.0.1:5432
auth.pgsql.pool = 8
auth.pgsql.username = root
auth.pgsql.password = dbpass

auth.pgsql.database = mqtt
auth.pgsql.encoding = utf8

auth.pgsql.auth_query = select password, salt from mqtt_user where username = '%u' limit 1
auth.pgsql.password_hash = salt,sha256
auth.pgsql.super_query = select is_superuser from mqtt_user where username = '%u' limit 1

auth.pgsql.ssl = on
auth.pgsql.ssl.cacertfile = path/to/your/cafile.pem
auth.pgsql.ssl.certfile = path/to/your/certfile
auth.pgsql.ssl.keyfile = path/to/your/keyfile
auth.pgsql.ssl.verify = true
auth.pgsql.ssl.server_name_indication = mypgsql
```

EMQX 5.1

```
authentication {
  backend = postgresql
  mechanism = password_based
  
  enable = true

  server = "127.0.0.1:5432"
  username = "root"
  database = "mqtt"
  password = "dbpass"
  pool_size = 8

  password_hash_algorithm {
      name = sha256
      salt_position = prefix
  }

  query = "SELECT password_hash, salt, is_superuser FROM mqtt_user where username = ${username} LIMIT 1"
  
  auto_reconnect = true
  ssl {
    enable = true
    verify = verify_peer
   
    keyfile = path/to/your/keyfile
    certfile = path/to/your/certfile
    cacertfile = path/to/your/cafile.pem
    
    server_name_indication = mypgsql
  }
}
```

### MongoDB

```
mechanism = password_based
backend = mongodb
```

- `type` は `mongo_type` に変更。値は `single`、`rs`、`sharded` のいずれか。未知の値は廃止。
- `server`
  - `rs`、`sharded` タイプは `servers`
  - `single` タイプは `server`
- `srv_record`、`username`、`password`、`auth_source`、`database`、`w_mode`、`topology`、`collection` は引き続き使用可能。
- `r_mode` は `rs` タイプのみ使用可能。
- `pool` は `pool_size` に変更。
- `ssl.*` は共通の SSL オプションに変更。詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。
- `auth_query.selector` は `filter` に変更。文字列ではなく、セレクターのデータ構造全体を指定します。`${var}` 形式のプレースホルダーはセレクター値で使用可能。
- `auth_query.salt_field` は `salt_field` に変更。
- `auth_query.super_field` は `is_superuser_field` に変更。
- `super_query` は廃止。`filter` で取得するドキュメントに `is_superuser_field` を含めてください。

  ::: details

  ```shell
  authentication = [
    {
      ...
      mechanism = "password_based"
      backend = "mongodb"
      # is_superuser_field = "is_superuser"
    }
  ]
  ```

  ::: 

- `password_hash` は共通の `password_hash_algorithm` パラメータに変更。
- `query_timeout` は廃止。

#### 例

EMQX 4.4

```
auth.mongo.type = single
auth.mongo.srv_record = false
auth.mongo.server = 127.0.0.1:27017
auth.mongo.pool = 8
auth.mongo.username = user
auth.mongo.password = pass
auth.mongo.auth_source = admin
auth.mongo.database = mqtt
auth.mongo.query_timeout = 5s

auth.mongo.ssl = on
auth.mongo.ssl.cacertfile = path/to/your/cafile.pem
auth.mongo.ssl.certfile = path/to/your/certfile
auth.mongo.ssl.keyfile = path/to/your/keyfile
auth.mongo.ssl.verify = true
auth.mongo.ssl.server_name_indication = mymongo

auth.mongo.w_mode = unsafe

auth.mongo.topology.pool_size = 1
auth.mongo.topology.max_overflow = 0

## auth.mongo.auth_query.password_hash = salt,sha256

auth.mongo.auth_query.collection = mqtt_user
auth.mongo.auth_query.password_field = password_hash

auth.mongo.auth_query.selector = username=%u, clientid=%c

auth.mongo.super_query.collection = mqtt_user
auth.mongo.super_query.super_field = is_superuser
auth.mongo.super_query.selector = username=%u, clientid=%c
```

EMQX 5.1

```
authentication {
  mechanism = password_based
  backend = mongodb
  enable = true

  password_hash_algorithm {
    name = sha256
    salt_position = prefix
  }

  collection = "mqtt_user"
  filter { username = "${username}", clientid = "${clientid}" }
  
  password_hash_field = "password_hash"
  salt_field = "salt"
  is_superuser_field = "is_superuser"

  mongo_type = single
  server = "127.0.0.1:27017"

  database = "mqtt"
  username = "emqx"
  password = "pass"
  
  ssl {
    enable = true
    verify = verify_peer
   
    keyfile = path/to/your/keyfile
    certfile = path/to/your/certfile
    cacertfile = path/to/your/cafile.pem
    
    server_name_indication = mymongo
  }
  
  topology {
    pool_size = 1
    max_overflow = 0
  }
}
```

### JWT

```
mechanism = jwt
```

- `secret`、`from`、`verify_claims`、`acl_claim_name`、`refresh_interval` は引き続き使用可能。
- `pubkey` は `public_key` に変更。
- `jwks` は `endpoint` に変更。
- `signature_format` は廃止。

`verify_claims` 内のセレクター値では `%X` 形式ではなく `${var}` 形式のプレースホルダー（`${username}`、`${clientid}`）を使用してください。

追加パラメータ：

- `use_jwks`: JWKS からキーを取得するかどうか
- `algorithm`: `public-key` または `hmac-based` の署名検証タイプ
- `secret_base64_encoded`: シークレットのフォーマット指定
- `pool_size`: JWKS サーバーへの接続数
- `ssl`: JWKS サーバー接続用の SSL オプション

すべてのパラメータセットが許可されているわけではありません。

EMQX 4.x は `public key`、`hmac secret`、`jwks` を同時にサポートしていましたが、EMQX 5.1 では一度に一つのアルゴリズムのみ使用し、グローバル設定で指定します。`use_jwks` と `algorithm` で利用可能なパラメータセットが決まります：

- `use_jwks=true` かつ `algorithm=public-key`：`endpoint`、`pool_size`、`refresh_interval`、`ssl`
- `use_jwks=false` かつ `algorithm=public-key`：`public_key`
- `use_jwks=false` かつ `algorithm=hmac-based`：`secret`、`secret_base64_encoded`

`use_jwks=true` と `algorithm=hmac-based` の組み合わせは無効です。

#### 例

EMQX 4.4

```
auth.jwt.jwks = https://127.0.0.1:8080/jwks
auth.jwt.jwks.refresh_interval = 5m
auth.jwt.from = password

auth.jwt.verify_claims = on

auth.jwt.verify_claims.username = %u

auth.jwt.acl_claim_name = acl
```

EMQX 5.1

```
{
  mechanism = jwt
  from = password,
  acl_claim_name = acl
  use_jwks = true
  algorithm = "public-key"
  verify_claims = {
    username = "${username}
  }
  
  ssl {
    enable = true
  }
  
  endpoint = "https://127.0.0.1:8080/jwks"
}
```

### HTTP

```
mechanism = password_based
backend = http
```

- `method`、`pool_size`、`connect_timeout`、`enable_pipelining` は引き続き使用可能。
- `auth_req.url` は `url` に変更。
- `auth_req.headers` は `headers` に変更。
- `auth_req.params` は `body` に変更。
- `timeout` は `request_timeout` に変更。
- `ssl.*` は共通の SSL オプションに変更。詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。
- `super_req` は廃止。代わりにサービスレスポンスに `is_superuser` フィールドを含めてください。

4.4 と異なり、`url`、`headers`、`body` パラメータでプレースホルダーが使用可能です。5.1 では `body` は文字列ではなくマップで、POST リクエストでは JSON または X-WWW-Form-Urlencoded 形式でシリアライズされ、GET リクエストではクエリパラメータとして送信されます。

4.4 と異なり、HTTP 認証は成功した HTTP ステータスコード（2XX）のレスポンスのみを有効とし、レスポンスボディ内の JSON フィールド（`result`）で認証結果を判断します。

::: details

**成功レスポンスステータスコード:**

```shell
200 または 204
```

リクエスト失敗や他のステータスコードの場合、認証器は無視されます。

**成功レスポンスボディ（JSON）:**

| 名前         | 型       | 必須 | 説明                   |
| ------------ | -------- | ---- | ---------------------- |
| result       | Enum     | 必須 | `allow | deny | ignore` |
| is_superuser | Boolean  | 任意 |                         |

```json
{
  "result": "allow",
  "is_superuser": true
}
```

:::

#### 例

EMQX 4.4

```
auth.http.auth_req.url = http://127.0.0.1:80/mqtt/auth
auth.http.auth_req.method = post
auth.http.auth_req.headers.content_type = application/x-www-form-urlencoded

auth.http.auth_req.params = clientid=%c,username=%u,password=%P

auth.http.timeout = 5s

auth.http.connect_timeout = 5s
auth.http.pool_size = 32

auth.http.enable_pipelining = 100

auth.http.ssl = on
auth.http.ssl.cacertfile = path/to/your/cafile.pem
auth.http.ssl.certfile = path/to/your/certfile
auth.http.ssl.keyfile = path/to/your/keyfile
auth.http.ssl.verify = true
auth.http.ssl.server_name_indication = myhttp
```

EMQX 5.1

```
{
    mechanism = password_based
    backend = http
    enable = true

    method = post
    url = "http://127.0.0.1:80/mqtt/auth"
    body {
        username = "${username}"
        clientid = "${clientid}"
        password = "${password}"
    }
    headers {
        "Content-Type" = "application/x-www-form-urlencoded"
    }
    request_timeout = "5s"
    connect_timeout = "5s"
    pool_size = 32
    
    enable_pipelining = 100
    
    ssl {
      enable = true
      verify = verify_peer
    
      keyfile = path/to/your/keyfile
      certfile = path/to/your/certfile
      cacertfile = path/to/your/cafile.pem
      
      server_name_indication = myhttp
    } 
}
```

## 認可

### ACL ファイル

1. `acl_file` 設定は削除されました。ファイルベースの ACL（acl.conf）はデフォルトで EMQX の認可ソースの一つとして使用されます。
2. `acl.conf` のデータファイル構文が変更されました。
3. DSL 内の `pubsub` は `all` に名称変更されました。

| 4.x    | 5.1      | 互換性       |
| ------ | -------- | ------------ |
| user   | username | あり         |
| client | clientid | あり         |
| pubsub | all      | なし         |

#### 例

EMQX 4.3

```bash
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

{allow, all}.
```

EMQX 5.1

```bash
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

{allow, {ipaddr, "127.0.0.1"}, all, ["$SYS/#", "#"]}.

{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

{allow, all}.
```

### 組み込みデータベース

- Mnesia は組み込みデータベースに名称変更。
- データ形式と REST API が変更されました。4.x の ACL データは `./bin/emqx_ctl data export` コマンドでエクスポート可能。ユーザーは 5.x 形式に変換し、対応する REST API `/authorization/sources/built_in_database/rules/{clients,users}` でインポートしてください。

#### 例

EMQX 4.4

```
# 設定なし
```

EMQX 5.1

```
{
  type = built_in_database
  enable = true
}
```

### JWT

JWT 認証時に取得したクレームに基づく暗黙の ACL です。ACL クレーム内では `%X` 形式ではなく `${variable}` 形式のプレースホルダーを使用してください。

### HTTP

```
type = http
```

- `method`、`pool_size`、`connect_timeout`、`enable_pipelining` は引き続き使用可能。
- `acl_req.url` は `url` に変更。
- `acl_req.headers` は `headers` に変更。
- `acl_req.params` は `body` に変更。
- `timeout` は `request_timeout` に変更。
- `ssl.*` は共通の SSL オプションに変更。詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。

4.4 と異なり、`url`、`headers`、`body` パラメータでプレースホルダーが使用可能です。5.1 では `body` は文字列ではなくマップで、POST リクエストでは JSON または X-WWW-Form-Urlencoded 形式でシリアライズされ、GET リクエストではクエリパラメータとして送信されます。

4.4 と異なり、認可結果は HTTP ステータスコードではなくレスポンスボディ内の JSON フィールド（`result`）で判断されます。HTTP 認可は成功した HTTP ステータスコード（2XX）のレスポンスのみを有効とします。

::: details

**成功レスポンスステータスコード:**

```shell
200 または 204
```

その他のステータスコードやリクエスト失敗は `ignore` として扱われます。

**成功レスポンスボディ（JSON）:**

| 名前   | 型    | 必須 | 説明                   |
| ------ | ----- | ---- | ---------------------- |
| result | Enum  | 必須 | `allow | deny | ignore` |

```json
{
  "result": "deny"
}
```

:::

#### 例

EMQX 4.4

```
auth.http.acl_req.url = http://127.0.0.1:80/mqtt/acl
auth.http.acl_req.method = post
auth.http.acl_req.headers.content_type = application/x-www-form-urlencoded

auth.http.acl_req.params = clientid=%c,username=%u,password=%P

auth.http.timeout = 5s

auth.http.connect_timeout = 5s
auth.http.pool_size = 32

auth.http.enable_pipelining = 100

auth.http.ssl = on
auth.http.ssl.cacertfile = path/to/your/cafile.pem
auth.http.ssl.certfile = path/to/your/certfile
auth.http.ssl.keyfile = path/to/your/keyfile
auth.http.ssl.verify = true
auth.http.ssl.server_name_indication = myhttp
```

EMQX 5.1

```
{
    type = http

    method = post
    url = "http://127.0.0.1:80/mqtt/acl"
    body {
        username = "${username}"
        clientid = "${clientid}"
        password = "${password}"
    }
    headers {
        "Content-Type" = "application/x-www-form-urlencoded"
    }
    request_timeout = "5s"
    connect_timeout = "5s"
    pool_size = 32
    
    enable_pipelining = 100
    
    ssl {
      enable = true
      verify = verify_peer
    
      keyfile = path/to/your/keyfile
      certfile = path/to/your/certfile
      cacertfile = path/to/your/cafile.pem
      
      server_name_indication = myhttp
    } 
}
```

### Redis

```
  type = redis
```

障害時に Redis に自動再接続するには `auto_reconnect` を使用可能。

- `type` は `redis_type` に変更。
  - `single` タイプは `server` が `servers` に変更。
  - `sentinel` タイプも同様に `server` が `servers` に変更。
  - `cluster` タイプでは `database` オプションは廃止。
- `database` は `cluster` タイプ以外で使用可能。
- `pool` は `pool_size` に変更。
- `password` はそのまま。
- `query_timeout` は廃止。
- `ssl.*` は共通の SSL オプションに変更。詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。
- `auth_cmd` は `cmd` に変更。`${var}` 形式のプレースホルダーをコマンド内で使用可能。
- Redis データソースは引き続きホワイトリストモードのみサポートし、`acl_nomatch = deny` の設定が必要です。
- `access` フィールド名は `action` に変更され、値は数値からアクション文字列に変更されました。

4.x のデータを継続利用する場合は、手動でマイグレーションしてください。

::: details 4.x と 5.x の対応表

| 4.x  | 5.x       | action              |
| ---- | --------- | ------------------- |
| 1    | subscribe | subscribe           |
| 2    | publish   | publish             |
| 3    | all       | subscribe & publish |

**5.x のデータ例**

```
HSET mqtt_acl:emqx_u t/# subscribe
HSET mqtt_acl:emqx_u # all
HSET mqtt_acl:emqx_u a/1 publish
```

:::

#### 例

EMQX 4.4

```
auth.redis.type = single
auth.redis.server = 127.0.0.1:6379
auth.redis.pool = 8
auth.redis.database = 0
auth.redis.password = pass
assword salt
auth.redis.acl_cmd = HGETALL mqtt_user:%u

auth.redis.password_hash = salt,sha256

auth.redis.ssl = on
auth.redis.ssl.cacertfile = path/to/your/cafile.pem
auth.redis.ssl.certfile = path/to/your/certfile
auth.redis.ssl.keyfile = path/to/your/keyfile
auth.redis.ssl.verify = true
auth.redis.ssl.server_name_indication = myredis
```

EMQX 5.1

```
{
  type = redis
  enable = true

  redis_type = single
  server = "127.0.0.1:6379"

  cmd = "HMGET mqtt_user:${username}"
  database = 0
  password = "pass"
  auto_reconnect = true
  ssl {
    enable = true
    verify = verify_peer
   
    keyfile = path/to/your/keyfile
    certfile = path/to/your/certfile
    cacertfile = path/to/your/cafile.pem
    
    server_name_indication = myredis
  }
}
```

### MySQL

```
  type = mysql
```

- クエリ結果に `ipaddr`、`username`、`clientid` フィールドは不要になりました。
- `access` フィールド名は `action` に変更され、整数から文字列または列挙型に変更されました。
- `allow` フィールド名は `permission` に変更され、整数から文字列または列挙型に変更されました。

  ::: details 4.x の整数値と 5.x の文字列／列挙型の対応

  **Access/action フィールド対応**

  | 4.x (int) | 5.x (varchar/enum) | action              |
  | --------- | ------------------ | ------------------- |
  | 1         | subscribe          | subscribe           |
  | 2         | publish            | publish             |
  | 3         | all                | subscribe & publish |

  **Allow/permission フィールド対応**

  | 4.x (int) | 5.x (varchar/enum) | permission |
  | --------- | ------------------ | ---------- |
  | 0         | deny               | deny       |
  | 1         | allow              | allow      |

  :::

- `server`、`username`、`password`、`database`、`query_timeout` は引き続き使用可能。
- `pool` は `pool_size` に変更。
- `ssl.*` は共通の SSL オプションに変更。詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。
- `acl_query` は `query` に変更。`${var}` 形式のプレースホルダーを使用してください。

障害時に MySQL に自動再接続するには `auto_reconnect` を使用可能。

ストレージスキーマが変更されました。

EMQX 4.4 では、クエリは `[Allow, IpAddr, Username, ClientId, Access, Topic]` の順でカラムを取得する必要がありました。

EMQX 5.1 では、クエリは `permission, action, topic` のカラムを任意の順序で取得すればよいですが、カラム名は厳密にこれらである必要があります。`IpAddr, Username, ClientId` はクエリ内で指定することが推奨されます。

#### 例

EMQX 4.4

```
auth.mysql.server = 127.0.0.1:3306
auth.mysql.pool = 8
auth.mysql.username = dbuser
auth.mysql.database = mqtt

auth.mysql.query_timeout = 5s

auth.mysql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where username = '%u'

auth.mysql.ssl = on
auth.mysql.ssl.cacertfile = path/to/your/cafile.pem
auth.mysql.ssl.certfile = path/to/your/certfile
auth.mysql.ssl.keyfile = path/to/your/keyfile
auth.mysql.ssl.verify = true
auth.mysql.ssl.server_name_indication = mymysql
```

EMQX 5.1

```
{
  type = mysql
  enable = true

  server = "127.0.0.1:3306"
  username = "dbuser"
  database = "mqtt"
  password = "dbpass"
  pool_size = 8


  query = "select allow as permission, access as action, topic from mqtt_acl where username = ${username} and ipaddr = ${peerhost} and clientid = ${clientid}"
  query_timeout = "5s"
  
  auto_reconnect = true
  ssl {
    enable = true
    verify = verify_peer
   
    keyfile = path/to/your/keyfile
    certfile = path/to/your/certfile
    cacertfile = path/to/your/cafile.pem
    
    server_name_indication = mymysql
  }
}
```

### PostgreSQL

```
type = postgresql
```

- クエリ結果に `ipaddr`、`username`、`clientid` フィールドは不要になりました。
- `access` フィールド名は `action` に変更され、整数から文字列または列挙型に変更されました。
- `allow` フィールド名は `permission` に変更され、整数から文字列または列挙型に変更されました。

::: details 4.x の整数値と 5.x の文字列／列挙型の対応

**Access/action フィールド対応**

| 4.x (int) | 5.x (varchar/enum) | action              |
| --------- | ------------------ | ------------------- |
| 1         | subscribe          | subscribe           |
| 2         | publish            | publish             |
| 3         | all                | subscribe & publish |

**Allow/permission フィールド対応**

| 4.x (int) | 5.x (varchar/enum) | permission |
| --------- | ------------------ | ---------- |
| 0         | deny               | deny       |
| 1         | allow              | allow      |

:::

- `server`、`username`、`password`、`database` は引き続き使用可能。
- `query_timeout` は廃止。
- `encoding` は廃止。
- `pool` は `pool_size` に変更。
- `ssl.*` は共通の SSL オプションに変更。詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。
- `acl_query` は `query` に変更。`${var}` 形式のプレースホルダーを使用してください。

ストレージスキーマが変更されました。

EMQX 4.4 では、クエリは `[Allow, IpAddr, Username, ClientId, Access, Topic]` の順でカラムを取得する必要がありました。

EMQX 5.1 では、クエリは `permission, action, topic` のカラムを任意の順序で取得すればよいですが、カラム名は厳密にこれらである必要があります。`IpAddr, Username, ClientId` はクエリ内で指定することが推奨されます。

#### 例

EMQX 4.4

```
auth.pgsql.server = 127.0.0.1:5432
auth.pgsql.pool = 8
auth.pgsql.username = root
auth.pgsql.password = dbpass

auth.pgsql.database = mqtt
auth.pgsql.encoding = utf8

auth.pgsql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where username = '%u'

auth.pgsql.ssl = on
auth.pgsql.ssl.cacertfile = path/to/your/cafile.pem
auth.pgsql.ssl.certfile = path/to/your/certfile
auth.pgsql.ssl.keyfile = path/to/your/keyfile
auth.pgsql.ssl.verify = true
auth.pgsql.ssl.server_name_indication = mypgsql
```

EMQX 5.1

```
{
  type = postgresql
  
  enable = true

  server = "127.0.0.1:5432"
  username = "root"
  database = "mqtt"
  password = "dbpass"
  pool_size = 8

  query = "select allow as permission, access as action, topic from mqtt_acl where username = ${username} and ipaddr = ${peerhost} and clientid = ${clientid}"
  
  auto_reconnect = true
  ssl {
    enable = true
    verify = verify_peer
   
    keyfile = path/to/your/keyfile
    certfile = path/to/your/certfile
    cacertfile = path/to/your/cafile.pem
    
    server_name_indication = mypgsql
  }
}
```

### MongoDB

```
type = mongodb
```

- `type` は `mongo_type` に変更。値は `single`、`rs`、`sharded` のいずれか。未知の値は廃止。
- `server`
  - `rs`、`sharded` タイプは `servers`
  - `single` タイプは `server`
- `srv_record`、`username`、`password`、`auth_source`、`database`、`w_mode`、`topology`、`collection` は引き続き使用可能。
- `r_mode` は `rs` タイプのみ使用可能。
- `pool` は `pool_size` に変更。
- `ssl.*` は共通の SSL オプションに変更。詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。
- `auth_query.selector` は `filter` に変更。文字列ではなく、セレクターのデータ構造全体を指定します。`${var}` 形式のプレースホルダーはセレクター値で使用可能。
- `query_timeout` は廃止。

ストレージスキーマが変更されました。

EMQX 4.4 では、結果ドキュメントは Redis や JWT と同様にアクションキーごとにトピックリストを含む必要がありました：

```
{
  "publish": ["t1", "t2"],
  "subscribe": ["t3", "t4"],
  "pubsub": ["t5", "t6"]
}
```

EMQX 5.1 では、MongoDB データソースは許可ルールと拒否ルールの両方に使用可能です。以前はホワイトリストモードのみサポートし、`acl_nomatch = deny` の設定が必要でした。ドキュメントは `permission`、`action`、`topics` フィールドを個別に持つ必要があります。`topics` はトピックの配列です。詳細は [AuthZ-MongoDB](../access-control/authz/mongodb.md) を参照してください。

4.x のデータを継続利用する場合は、手動でマイグレーションしてください。

::: details 5.x のデータ例

```json
[
  {
      "username": "emqx_u",
      "clientid": "emqx_c",
      "ipaddress": "127.0.0.1",
      "permission": "allow",
      "action": "all",
      "topics": ["#"]
  }
]
```

:::

#### 例

EMQX 4.4

```
auth.mongo.type = single
auth.mongo.srv_record = false
auth.mongo.server = 127.0.0.1:27017
auth.mongo.pool = 8
auth.mongo.username = user
auth.mongo.password = pass
auth.mongo.auth_source = admin
auth.mongo.database = mqtt
auth.mongo.query_timeout = 5s

auth.mongo.ssl = on
auth.mongo.ssl.cacertfile = path/to/your/cafile.pem
auth.mongo.ssl.certfile = path/to/your/certfile
auth.mongo.ssl.keyfile = path/to/your/keyfile
auth.mongo.ssl.verify = true
auth.mongo.ssl.server_name_indication = mymongo

auth.mongo.w_mode = unsafe

auth.mongo.topology.pool_size = 1
auth.mongo.topology.max_overflow = 0


auth.mongo.acl_query.collection = mqtt_user
auth.mongo.acl_query.selector = username=%u, clientid=%c
```

EMQX 5.1

```
{
  type = mongodb
  enable = true

  collection = "mqtt_user"
  filter { username = "${username}", clientid = "${clientid}" }
  
  mongo_type = single
  server = "127.0.0.1:27017"

  database = "mqtt"
  username = "emqx"
  password = "pass"
  
  ssl {
    enable = true
    verify = verify_peer
   
    keyfile = path/to/your/keyfile
    certfile = path/to/your/certfile
    cacertfile = path/to/your/cafile.pem
    
    server_name_indication = mymongo
  }
  
  topology {
    pool_size = 1
    max_overflow = 0
  }
}
```
