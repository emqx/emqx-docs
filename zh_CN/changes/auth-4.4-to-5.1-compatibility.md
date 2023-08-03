# 从 EMQX 4.4 到 EMQX 5.1 认证 / 授权的不兼容变更

本页介绍了认证和授权功能的配置在 EMQX 4.4 升级到 EMQX 5.1 之后的兼容性。

## 通用不兼容变更 

### SSL选项

EMQX 5.1 提供了当需要访问外部资源时可以启用 TLS 的选项，例如连接到数据库（MySQL、PostgreSQL、MongoDB、Redis）进行身份验证，或者通过 HTTPS 访问 Web 服务器进行基于密码的身份验证。更多信息，请参阅[启用 TLS 加密访问外部资源](../network/overview.md#tls-for-external-resource-access)。

### 占位符

现在支持某种数据插值的后端（例如：MySQL、PostgreSQL、MongoDB、Redis、用于外部请求的 HTTP、用于数据插值的 JWT）使用 `${variable}` 占位符代替 `%X`。

## 认证配置的不兼容变更

### 通用变更 (针对所有认证数据源)

#### 密码散列

所有使用密码认证的数据源（内置数据库、MySQL、PostgreSQL、MongoDB、Redis）现在都具有相同的 `password_hash` 选项，并以相同的方式配置。详情请参阅[密码散列](../access-control/authn/authn.html#密码散列)。

#### 监听器认证

与 4.4 版本不同，EMQX 5.1 中的每个 MQTT 监听器可以有自己的身份认证配置。此外，提供了 `enable_authn` 监听器选项：

- `enable_authn=true` 是默认值，将身份认证委托给身份认证链。
- `enable_authn=false ` 完全禁用监听器的身份认证。
- `enable_authn=quick_deny_anonymous  `类似于 `true`，但还会立即拒绝没有凭证的连接客户端。

#### 移除匿名认证机制

EMQX 5.1 不再有显式的 `allow_anonymous` 设置。所有客户端默认允许连接。如果**添加并启用**任何身份验证器，EMQX 将尝试对客户端进行身份验证。要允许匿名访问，请移除或禁用全局或特定监听器链中的所有身份认证器。

在遍历配置的身份认证链后，如果链中没有身份认证器决定允许此客户端连接，则拒绝连接。

同时也删除了`bypass_auth_plugins  ` 配置。当希望允许所有客户端在没有身份认证的情况下连接时，可以设置 `listeners.{type}.{name}.enable_authn = false`。

### 内置数据库（Mnesia）

- Mnesia 现在称为"内置"数据库；不在配置中保留用户记录。
- 将 `password_hash` 更改为 `password_hash_algorithm`：{name = Algo, salt_position = prefix}。有关详情，请参阅[密码散列](../access-control/authn/authn.html#密码散列)。
- `user_id_type` 用于标识是使用 `clientid` 还是 `username` 作为 MQTT 用户标识符。不允许混合类型的记录。
- 用于管理身份认证数据记录的 REST API 已更改。有关更多信息，请参阅 `POST /authentication/{id}/users` 的API文档。
- 用户可以使用数据导入 API 将数据从旧版本导入到 EMQX 5.x，请参阅 `POST /authentication/{id}/import_users` 了解详情。

#### 示例

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

### 内置数据库 (增强认证)

- EMQX 4.4 中使用的 SHA1哈希支持已不再可用。现在可以使用 `algorithm` 参数来选择 `sha512` 和 `sha256` 算法。
- 现在可以通过配置迭代次数选项 `iteration_count` 指定散列次数（在 EMQX 4.4 中隐式使用了4096）。

#### 示例

EMQX 4.4

```
# No configuration
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

- `type` 已更改为 `redis_type`。
  
  - 对于类型为 `single`，`server `已更改为 `servers`。
  - 对于类型为 `sentinel`，`server `已更改为 `servers`。
  - 对于类型为 `cluster`，`database` 选项不再可用。
  
- `database` 仍为 `database`（对于 `cluster` 类型除外）；此选项不再适用于集群。

- `pool` 已更改为 `pool_size`。

- `password  ` 仍为 `password`。

- `query_timeout   `不再使用。

- `ssl.*` 选项已更改为通用的 SSL 选项。请参阅[启用 TLS 加密访问外部资源](../network/overview.md#tls-for-external-resource-access)。

- `auth_cmd` 已更改为 `cmd`。仅支持 [Redis Hashes](https://redis.io/docs/manual/data-types/#hashes) 数据结构和 `HGET` 和 `HMGET` 查询命令。在命令中使用 `${var}` 样式的[占位符](https://chat.openai.com/access-control/authn/authn.md#authentication-placeholders)。命令应至少获取 `password`（与 4.x 兼容）或 `password_hash` 字段，以及可选的 `salt` 和 `is_superuser` 字段。

- 不再使用 `super_cmd`。请在 `cmd` 中提供 `is_superuser` 字段。如果需要给客户端提供超级用户权限，请将 `is_superuser` 字段添加到 Redis 查询命令中。

  ::: details

  ```shell
  # bad
  GET emqx_user:${username}
  # bad
  HMGET emqx_user:${username} passwd
  
  # good
  HMGET emqx_user:${username} password_hash
  
  # good
  HMGET emqx_user:${username} password_hash is_superuser
  ```

  :::

- `password_hash` 现在使用通用的 `password_hash_algorithm` 参数。

- 当与数据库连接失败时，您可以使用 `auto_reconnect` 配置来自动重连。

#### 示例

EMQX 4.4

```
auth.redis.type = single
auth.redis.server = 127.0.0.1:6379
auth.redis.pool = 8
auth.redis.database = 0
auth.redis.password = pass
assword salt
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

- `server`、`username`、`password`、`database`、`query_timeout ` 保持不变。

- `pool   `已更改为 `pool_size`。

- `ssl.*    `选项已更改为通用的 SSL 选项。请参阅[启用 TLS 加密访问外部资源](../network/overview.md#tls-for-external-resource-access)。

- `password_hash` 已更改为 `common password_hash_algorithm` 参数。

- `auth_query ` 已更改为 `query`。应使用 `${var}` 样式的[占位符](../access-control/authn/authn.md#认证占位符)。查询应至少获取 `password` 或 `password_hash` 列，并可选地获取`salt `和 `is_superuser` 列。

- 不再使用` `，请在查询中提供 `is_superuser` 列。如果需要给客户端提供超级用户权限，请确保认证 SQL 结果包含 `is_superuser` 字段。

  ```sql
  SELECT
    password as password_hash,
    salt,
    is_superuser
  FROM mqtt_user
    where username = ${username} LIMIT 1
  ```

- 当与数据库连接失败时，您可以使用 `auto_reconnect` 配置来自动重连。


#### 示例

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

- `server`、`username`、`password`、`database` 保持不变。

- 不再使用 `query_timeout`。

- 不再使用 `encoding`。

- `pool ` 已更改为 `pool_size`。

- `ssl.* ` 已更改为通用的 SSL 选项。请参阅[启用 TLS 加密访问外部资源](../network/overview.md#tls-for-external-resource-access)。

- `password_hash ` 已更改为通用的 `password_hash_algorithm` 参数。

- `auth_query ` 已更改为 `query`。应使用 `${var}` 样式的[占位符](../access-control/authn/authn.md#认证占位符)。查询应至少获取 `password` 或 `password_hash` 列，并可选地获取`salt` 和 `is_superuser` 列。

- 不再使用 `super_query`，请在查询中提供 `is_superuser` 列。如果需要给客户端提供超级用户权限，请确保认证 SQL 结果包含 `is_superuser` 字段。

  ```sql
  SELECT
    password as password_hash,
    salt,
    is_superuser
  FROM mqtt_user
    where username = ${username} LIMIT 1
  ```

#### 示例

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

- `type   `已更改为 `mongo_type` 字段。可能的值为 `single`、`rs`、`sharded`。不再支持未知值。

- `server`

  - 对于 `rs`、`sharded  `类型，更改为 `servers`
  - 对于 `single` 类型，更改为 `server`

- `srv_record`, `username`, `password`, `auth_source`, `database`, `w_mode`, `topology`, `collection` 保持不变。

- `r_mode` 仅适用于 `rs` 类型。

- `pool` 更改为 `pool_size`.

- `ssl.* ` 已更改为通用的 SSL 选项。请参阅[启用 TLS 加密访问外部资源](../network/overview.md#tls-for-external-resource-access)。

- `auth_query.selector` 更改为 `filter`。查询 Filter 应该不再是一个字符串，而是整个 selector 数据结构。在 selector 值中可使用  `${var}` 格式的[占位符](../access-control/authn/authn.md#认证占位符)。

- `auth_query.salt_field` 更改为 `salt_field`。

- `auth_query.super_field` 更改为 `is_superuser_field`。

- `super_query` 不再使用。 在使用 `filter` 获取的文档中提供 `is_superuser_field` 字段，同时设置 `is_superuser_field`。

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

- `password_hash` 更改为 `common password_hash_algorithm` 参数。

- `query_timeout` 不再使用。

#### 示例

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

-  `secret`、`from`、`verify_claims`、`acl_claim_name` 和 `refresh_interval` 保持不变。
- `pubkey ` 更改为 `public_key`。
- `jwks` 更改为 `endpoint`。
- `signature_format` 已删除。

在 `verifiy_claims` 配置中，在 selector 值中应使用 `${var}` 格式的占位符 (``${username}`` 和 `${clientid}`) 而不是 `%X` 。

更多参数：

- `use_jwks`：是否从 JWKS 获取 key
- `algorithm`：使用哪种签名来验证 `public-key|hmac-based` 
- `secret_base64_encoded`： 指定密钥格式
- `pool_size`： 连接到 JWKS 服务器的客户端连接个数
- `ssl`：用于连接 JWKS 服务器的 SSL 选项

注意：不是所有参数集都被允许。

EMQX 4.x 同时支持 `public key`、`hmac secret` 算法以及 `jwks`。与 EMQX 4.4 不同，(`secret`, `secret_base64_encoded`, `public_key`, `endpoint`, `pool_size`, `refresh_interval`, `ssl`) 参数的所有组合并非都被允许。EMQX 5.1 只能一次使用一种算法，并且在全局配置中设置。`use_jwks` 和 `algorithm` 标识了可用的参数集：

使用 `use_jwks=true` 和 `algorithm=public-key`：`endpoint`、`pool_size`、`refresh_interval`、`ssl`

使用 `use_jwks=false` 和 `algorithm=public-key`：`public_key`

使用 `use_jwks=false` 和 `algorithm=hmac-based`：`secret`、`secret_base64_encoded`

`use_jwks=true` 和 `algorithm=hmac-based` 的组合是无效的。

#### 示例

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

- `method`、`pool_size`、`connect_timeout` 和 `enable_pipelining` 保持不变。
- `auth_req.url` 已更改为 `url`。
- `auth_req.headers` 已更改为 `headers`。
- `auth_req.params` 已更改为 `body`。
- `timeout` 已更改为 `request_timeout`。
- `ssl.*` 已更改为通用的 SSL 选项。请参阅[启用 TLS 加密访问外部资源](../network/overview.md#tls-for-external-resource-access)。
- `super_req` 不再可用。客户端的超级用户身份通过认证响应中的 body 设置，需要在服务响应中提供 `is_superuser` 字段。

与 4.4 版本不同，`url`、`headers` 和 `body` 参数允许使用占位符。在 5.1 版本中，`body` 不再是一个字符串，而是一个映射。它将使用 JSON 或 X-WWW-Form-Urlencoded 格式进行序列化（用于 POST 请求），或作为查询参数（用于 GET 请求）。

与 4.4 版本不同，在 5.1 版本中，认证结果现在通过响应 body 内的 JSON 字段来确定，而不再使用 HTTP 响应状态码。HTTP 认证仅会关注返回成功的 HTTP 状态码（2XX），并从响应 body 中提取 `result` 字段作为解析依据。

::: details

**成功响应状态码：**

```shell
200 or 204
```

如果请求失败或返回其他状态码，认证器将被忽略。

**成功响应 body  (JSON)：**

| 名称          | 类型    | 是否必须 | 描述                    |
| ------------- | ------- | -------- | ----------------------- |
| result        | Enum    | 是       | `allow | deny | ignore` |
| is_supseruser | Boolean | 否       | 默认为 `false`          |

```json
{
  "result": "allow",
  "is_supseruser": true
}
```

:::

#### 示例

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

## 授权配置的不兼容变更

### ACL 文件

- 移除了 `acl_file` 配置。基于文件的 ACL（acl.conf）将作为授权检查数据源之一，并默认添加到 EMQX 中。
- `acl.conf` 数据文件的语法已经发生了变化。
- 在 dsl 中，`pubsub` 被重命名为 `all`。

| 4.x    | 5.1      | 兼容性 |
| ------ | -------- | ------ |
| user   | username | 是     |
| client | clientid | 是     |
| pubsub | all      | 否     |

#### 示例

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

### 内置数据库（Mnesia）

- 为方便理解，该数据源由 Mnesia 更名为内置数据库 (Built-in Database)。
- 数据格式与数据操作 REST API 有变动，4.x 版本中的 ACL 数据可以通过 `./bin/emqx_ctl data export` 命令导出。用户可以将数据转换为符合 5.1 版本的格式，并通过相应的 REST API 导入。新的 API 是 `/authorization/sources/built_in_database/rules{/clients,/users}`。

#### 示例

EMQX 4.4

```
# No settings
```

EMQX 5.1

```json
{
  type = built_in_database
  enable = true
}
```

### JWT

这是隐式的 ACL，基于 JWT 认证期间提取的声明 。在 ACL 声明中，应使用 `${variable}` 占位符，而不是 `%X` 占位符。

### HTTP

```
type = http
```

- `method`、`pool_size`、`connect_timeout` 和 `enable_pipelining` 参数保留不变。
- `acl_req.url` 已更改为 `url`。
- `acl_req.headers` 已更改为 `headers`。
- `acl_req.params` 已更改为 `body`。
- `timeout` 已更改为 `request_timeout`。
- `ssl.*` 已更改为通用的 SSL 选项。请参阅[启用 TLS 加密访问外部资源](../network/overview.md#tls-for-external-resource-access)。

与 4.4 版本不同，`url`、`headers` 和 `body` 参数允许使用占位符。

在 5.1 版本中，`body` 不再是一个字符串，而是一个映射。它将使用 JSON 或 X-WWW-Form-Urlencoded 格式进行序列化（用于 POST 请求），或作为查询参数（用于 GET 请求）。

与 4.4 版本不同，授权结果现在通过响应 body 内的 JSON 字段来确定，而不再使用 HTTP 响应状态码。HTTP 授权仅关注返回成功的 HTTP 状态码（2XX），并从响应 body 中提取 `result` 字段作为解析依据。

::: details

**成功响应状态码**

```shell
200 or 204
```

如果请求失败或返回其他状态码，授权检查器将被忽略。

**成功响应 body (JSON):**

| Name   | 类型 | 是否必须 | 描述                    |
| ------ | ---- | -------- | ----------------------- |
| result | Enum | 是       | `allow | deny | ignore` |

```json
{
  "result": "deny"
}
```

:::

#### 示例

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

- 当与数据库连接失败时，可以使用 `auto_reconnect` 配置进行自动重连。
- `type ` 已更改为 `redis_type`。
  - 对于类型 `single`，`server` 已更改为 `servers`。
  - 对于类型 `sentinel`，`server` 已更改为 `servers`。
  - 对于类型 `cluster`，`database` 选项不再可用。
- `database` 仍为 `database`，但不包括 `cluster` 类型；该选项不再适用于集群）。
- `pool` 已更改为 `pool_size`。
- `password` 仍为 `password`。
- 不再使用 `query_timeout` 选项。
- `ssl.*` 选项已更改为通用的 SSL 选项。请参阅[启用 TLS 加密访问外部资源](../network/overview.md#tls-for-external-resource-access)。
- `auth_cmd` 已更改为 `cmd`。应在命令中使用 `${var}` 样式的[占位符](../access-control/authn/authn.md#认证占位符)。
- Redis 数据源仍然仅支持白名单模式，需要设置 `acl_nomatch = deny`；
- `access` 字段名称更改为 `action`，数据由数字变为动作全称字符串，对应关系见下表。

如果您想继续使用 4.x 中的数据，请手动进行必要的迁移。

::: details  4.x 与 5.x 版本的数据对应关系

| 4.x  | 5.x       | 对应动作 |
| ---- | --------- | -------- |
| 1    | subscribe | 订阅     |
| 2    | publish   | 发布     |
| 3    | all       | 发布订阅 |

**5.1 版本中数据示例**

```
HSET mqtt_acl:emqx_u t/# subscribe
HSET mqtt_acl:emqx_u # all
HSET mqtt_acl:emqx_u a/1 publish
```

:::

#### 示例

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

- 查询结果中不再需要 `ipaddr/username/clientid` 字段，需要调整查询 SQL。

- `access` 字段名变为 `action`，数据类型由整型变为字符或字符枚举，数值对应关系见下表。

- `allow` 字段名变为 `permission`，数据类型由整型变为字符或字符枚举，数值对应关系见下表。

  ::: details 4.x 的整数值与 5.x 的字符/字符枚举值之间的对应关系

  **Access/action 字段数据类型映射**

  | 4.x (int) | 5.x (varchar/enum) | 对应动作 |
  | --------- | ------------------ | -------- |
  | 1         | subscribe          | 订阅     |
  | 2         | publish            | 发布     |
  | 3         | all                | 发布订阅 |

  **Allow/permission 字段数据类型映射**

  | 4.x (int) | 5.x (varchar/enum) | 对应行为 |
  | --------- | ------------------ | -------- |
  | 0         | deny               | 拒绝     |
  | 1         | allow              | 允许     |

  :::

- `server`、`username`、`password`、`database`、`query_timeout` 参数保留不变。
- `pool` 已更改为 `pool_size`。
- `ssl.*` 选项已更改为通用的 SSL 选项。请参阅[启用 TLS 加密访问外部资源](../network/overview.md#tls-for-external-resource-access)。
- `acl_query` 已更改为 `query`。应使用 `${var}` 样式的[占位符](../access-control/authn/authn.md#认证占位符)。
- 您可以使用 `auto_reconnect` 在 MySQL 连接失败时自动重新连接。

存储模式已更改。

在 EMQX 4.4 中，查询应准确按照 `[Allow, IpAddr, Username, ClientId, Access, Topic]` 列的顺序提取行，列名可以是任意名称。

在 EMQX 5.1 中，查询应该按照任意顺序提取具有 `permission, action, topic` 列的行，但列名必须是这些名称。 "who" 部分（`IpAddr, Username, ClientId`）现在建议作为查询的一部分。

#### 示例

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

- 查询结果中不再需要 `ipaddr/username/clientid` 字段，需要调整查询 SQL。
- `access` 字段名变为 `action`，数据类型由整型变为字符或字符枚举，数值对应关系见下表。
- `allow` 字段名变为 `permission`，数据类型由整型变为字符或字符枚举，数值对应关系见下表。

::: details 4.x 的整数值与 5.x 的字符/字符枚举值之间的对应关系

**Access/action 字段数据类型映射**

| 4.x (int) | 5.x (varchar/enum) | 对应动作 |
| --------- | ------------------ | -------- |
| 1         | subscribe          | 订阅     |
| 2         | publish            | 发布     |
| 3         | all                | 发布订阅 |

**Allow/permission 字段数据类型映射**

| 4.x (int) | 5.x (varchar/enum) | 对应行为 |
| --------- | ------------------ | -------- |
| 0         | deny               | 拒绝     |
| 1         | allow              | 允许     |

:::

- `server`、`username`、`password`、`database`、`query_timeout` 参数保留不变。
- `query_timeout` 不再使用。
- `encoding` 不再使用。
- `pool` 已更改为 `pool_size`。
- `ssl.*` 选项已更改为通用的 SSL 选项。请参阅[启用 TLS 加密访问外部资源](../network/overview.md#tls-for-external-resource-access)。
- `acl_query` 已更改为 `query`。应使用 `${var}` 样式的[占位符](../access-control/authn/authn.md#认证占位符)。

存储模式已更改。

在 EMQX 4.4 中，查询应准确按照 `[Allow, IpAddr, Username, ClientId, Access, Topic]` 列的顺序提取行，列名可以是任意名称。

在 EMQX 5.1 中，查询应该按照任意顺序提取具有 `permission, action, topic` 列的行，但列名必须是这些名称。 "who" 部分（`IpAddr, Username, ClientId`）现在建议作为查询的一部分。

### 示例

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

- `type` 已更改为 `mongo_type` 字段。可能的取值为 `single`、`rs` 和 `sharded`。不再允许未知的值。
- `server`
  - 对于 `rs` 和 `sharded` 类型，`server` 已更改为 `servers`。
  - 对于 `single` 类型，`server` 已更改为 `server`。
- `srv_record`、`username`、`password`、`auth_source`、`database`、`w_mode`、`topology`、`collection` 参数保留不变。
- `r_mode` 仅适用于 `rs` 类型。
- `pool` 已更改为 `pool_size`。
- `ssl.*` 选项已更改为通用的 SSL 选项。请参阅[启用 TLS 加密访问外部资源](../network/overview.md#tls-for-external-resource-access)。
- `auth_query.selector` 已更改为 `filter`。过滤器不应该是一个字符串，而是整个选择器数据结构。应在选择器值中使用 `${var}` 样式的[占位符](../access-control/authn/authn.md#认证占位符)。
- 不再使用 `query_timeout`。

存储模式已更改。在 EMQX 4.4 中，生成的数据存储文档应该按照 action 键包含主题列表，类似于 Redis 或 JWT：

```
{
  "publish": ["t1", "t2"],
  "subscribe": ["t3", "t4"],
  "pubsub": ["t5", "t6"]
}
```

在 EMQX 5.1 中, MongoDB 数据源可用于黑/白名单模式下，此前仅支持白名单模式，并要求设置 `acl_nomatch = deny`。 MongoDB 数据存储文档中应包含 `action` `permission` `topics` 字段的单独规则，`topic` 字段应该是一个主题数组。具体使用方式详见 [基于 MongoDB 进行授权](../access-control/authz/mongodb.md)。如需继续使用 4.x 中的数据，请手动迁移适配。

::: details 5.1 版本中数据示例

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

#### 示例

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

