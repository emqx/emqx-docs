# Authentication / Authorization Incompatibility Between EMQX 4.4 and EMQX 5.1 

This page presents the compatibility information for authentication and authorization configurations between EMQX 4.4 and EMQX 5.1.

## Commo Incompatibility Changes 

### SSL Options

EMQX 5.1 provides the option of enabling TLS when there is a need to access external resources, such as connecting to a database (MySQL, PostgreSQL, MongoDB, Redis) for authentication, or using password-based authentication with access to a web server via HTTPS.  For more information, refer to [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access).

### Placeholders

Backends that support some kind of data interpolation (MySQL, PostgreSQL, MongoDB, Redis, HTTP for external requests, JWT for data interpolation) now use '${variable}' placeholders instead of '%X' ones.

## Authentication

### Common Changes (All Authentication Sources)

#### Password Hashing

All password-based providers (Built-in database, MySQL, PostgreSQL, MongoDB, Redis) now have the same password hashing options, configured in the same way. For details, refer to [Password Hashing](../access-control/authn/authn.md#password-hashing).

#### Per-Listener Authentication

Unlike in version 4.4, each MQTT listener in EMQX 5.1 may have its own authentication configuration. Additionally, the `enable_authn` listener option is available:

- `enable_authn=true` is the default, delegating authentication to the authentication chain.
- `enable_authn=false` completely disables authentication for the listener.
- `enable_authn=quick_deny_anonymous` is similar to 'true', but also immediately rejects connecting clients without credentials.

#### Remove the Anonymous Mechanism

EMQX 5.1 no longer has explicit `allow_anonymous` settings. All clients are allowed to connect by default. If you **add and enable** any authenticator, EMQX will try to authenticate the clients. To allow anonymous access, remove or disable all authenticators in the global or listener-specific chain.

After traversing the configured authentication chain, if none of the authenticator in the chain can decide that this client is allowed to connect, then the connection is rejected.

The `bypass_auth_plugins` configuration is also deleted. When you wants to allow all clients to connect without authentication, you can set `listeners.{type}.{name}.enable_authn = false`.

### Built-in Database (Mnesia)

- Mnesia is now referred to as the "built-in" database; No user records in the configuration.
- Change `password_hash` to `password_hash_algorithm`: {name = Algo, salt_position = prefix}. For details, refer to [Password Hashing](../access-control/authn/authn.md#password-hashing).
- `user_id_type` is used to identify whether the `clientid` or `username` should be used as MQTT user identifiers. Mixed types of records are not allowed.
- The REST APIs to manage the authentication data records are changed. For more information, refer to the API doc for `POST /authentication/{id}/users`.
- Users can use the data import API to import data from older versions into EMQX 5.x, see `POST /authentication/{id}/import_users` for details.

#### Example

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

### Built-in Database (Enhanced Authentication)

- SHA1 hashing support used in EMQX 4.4 is no longer available. Use the `algorithm` parameter to choose between `sha512'`and `sha256` algorithms.
- `iteration_count` can now be configured (4096 was implicitly used in EMQX 4.4).

#### Example

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

- `type` is changed to `redis_type`.
  - For type `single`,  `server` is changed to `servers`.
  - For type `sentinel`,  `server` is changed to `servers`.
  - For type `cluster`,  `database` option is no longer available.
  
- `database` is changed to `database` (except for the `cluster` type; this option is not available for clusters anymore).

- `pool` is changed to `pool_size`.

- `password` is changed to `password`.

- `query_timeout` is no longer used.

- `ssl.*` options are changed to common SSL options. Refer to [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access).

- `auth_cmd` is changed to `cmd`. Only supports [Redis Hashes](https://redis.io/docs/manual/data-types/#hashes) data structure and `HGET` and `HMGET` query commands. Use `${var}`-style [placeholders](../access-control/authn/authn.md#authentication-placeholders) in the command. The command should fetch at least the `password` (compatible with 4.x) or `password_hash` field and optionally the `salt` and `is_superuser` fields.

- `super_cmd` is no longer used. Provide the `is_superuser` field in `cmd` instead. If you need to give clients super-user permissions, please add the `is_superuser` field to the Redis query command.

  ~~~shell
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
  
  ::: details
  ~~~

- `password_hash` now uses common `password_hash_algorithm` parameters.

You can use `auto_reconnect` to automatically reconnect to Redis on failure.

#### Example

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

- `server`, `username`, `password`, `database`, `query_timeout` are retained.

- `pool` is changed to `pool_size`.

- `ssl.*` options are changed to common SSL options. Refer to [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access).

- `password_hash` is changed to`common password_hash_algorithm` parameters.

- `auth_query` is changed to `query`.  `${var}`-style [placeholders](../access-control/authn/authn.md#authentication-placeholders) should be used. Query should fetch at least `password` or `password_hash` column and optionally `salt` and `is_superuser` columns.

- `super_query` is not used anymore, `is_superuser` column is provided in query instead. If you need to give clients super-user permissions, please ensure that the authentication SQL result contains the `is_superuser` field.

  ```sql
  SELECT
    password as password_hash,
    salt,
    is_superuser
  FROM mqtt_user
    where username = ${username} LIMIT 1
  ```

You can use `auto_reconnect`  to reconnect to MySQL automatically on failure.

#### Example

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

- `server`, `username`, `password`, `database` are retained.

- `query_timeout` is not used anymore.

- `encoding` is not used anymore.

- `pool` is changed to `pool_size`.

- `ssl.*` is changed to common SSL options. Refer to [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access).

- `password_hash` is changed to common `password_hash_algorithm` parameters.

- `auth_query` is changed to `query`.  `${var}`-style [placeholders](../access-control/authn/authn.md#authentication-placeholders) should be used. Query should fetch at least `password` or `password_hash` column and optionally `salt` and `is_superuser` columns.

- `super_query` is not used anymore, `is_superuser` column is proviced in query instead. If you need to give clients super-user permissions, please ensure that the authentication SQL result contains the `is_superuser` field.

  ```sql
  SELECT
    password as password_hash,
    salt,
    is_superuser
  FROM mqtt_user
    where username = ${username} LIMIT 1
  ```

#### Example

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

- `type` is changed to `mongo_type` field. Possible values are `single`, `rs`, `sharded`. Unknown value is not available anymore.

- `server`
  - For `rs`, `sharded` to `servers`
  - For `single` to `server`
  
- `srv_record`, `username`, `password`, `auth_source`, `database`, `w_mode`, `topology`, `collection` are retained.

- `r_mode` is availalable only for `rs` type.

- `pool` is changed to `pool_size`.

- `ssl.*` is changed to common SSL options. Refer to [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access).

- `auth_query.selector` is changed to `filter`. The filter should not be a string, but the whole selector data structure.  `${var`}-style [placeholders](../access-control/authn/authn.md#authentication-placeholders) may be used in selector values.

- `auth_query.salt_field` is changed to `salt_field`.

- `auth_query.super_field` is changed to `is_superuser_field`.

- `super_query` is not used anymore. Provide `is_superuser_field` field in the documents fetched with `filter` together with `is_superuser_field` setting.

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

- `password_hash` is changed to `common password_hash_algorithm` parameters.

- `query_timeout` is not used.

#### Example

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

-  `secret`, `from`, `verify_claims`, `acl_claim_name`, `refresh_interval` are retained.
- `pubkey ` is changed to `public_key`.
- `jwks` is changed to `endpoint`.
- `signature_format` is no more available.

In `verifiy_claims`, `${var}`-style placeholders (``${username}`` and `${clientid}`) should be used in selector values instead of `%X` ones.

Additional parameters:

- `use_jwks`: whether to fetch keys from JWKS
- `algorithm`:  `public-key|hmac-based` which type of signature to verify.
- `secret_base64_encoded`: specifies secret format
- `pool_size`: number of connections to JWKS server
- `ssl` SSL options for connecting to JWKS server

Not all sets of parameters are allowed. 

EMQX 4.x supports `public key` and `hmac secret` algorithms, as well as `jwks` at the same time. Unlike EMQX 4.4, not all combinations of  (`secret`,` secret_base64_encoded`,`public_key`,` endpoint`, `pool_size`, `refresh_interval` , `ssl` ) parameters are allowed. EMQX 5.1 uses one algorithm at a time only, which is set in the global config. `use_jwks` and `algorithm` identify the available sets:

With `use_jwks=true` and `algorithm=public-key`:  `endpoint`, `pool_size`,``refresh_interval`, `ssl`

With `use_jwks=false` and a`lgorithm=public-key`:  `public_key`

With `use_jwks=false` and `algorithm=hmac-based`: `secret`, `secret_base64_encoded`

Combination `use_jwks=true` and `algorithm=hmac-based` is invalid.

#### Example

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

-  `method`, `pool_size`, `connect_timeout`, and `enable_pipelining` are retained.
-  `auth_req.url` is changed to `url`.
-  `auth_req.headers` is changed to `headers`.
-  `auth_req.params` is changed with `body`.
-  `timeout` is changed to `request_timeout`.
- `ssl.*` is changed to common SSL options. Refer to [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access).
- `super_req` is not available. Provide `is_superuser` field in the service response instead.

Unlike version 4.4,  `url`, `headers`, and `body` parameters allow placeholders. In version 5.1, `body` is not a string, but a map. It is serialized using JSON or X-WWW-Form-Urlencoded format (for post requests) or as query params (for get requests).

Unlike version 4.4, HTTP authentication only respects responses with successful HTTP code (2XX) and takes the resolution from the response body (from `result`) field. In version 5.1, the authentication result is now determined through JSON fields within the response body, rather than utilizing HTTP response status codes.

::: details

**Success response status code:**

```shell
200 or 204
```

The authenticator will be ignored if the request fails or returns another status code.

**Success Response Body (JSON):**

| Name          | Type    | Required | Description             |
| ------------- | ------- | -------- | ----------------------- |
| result        | Enum    | true     | `allow | deny | ignore` |
| is_supseruser | Boolean | false    |                         |

```json
{
  "result": "allow",
  "is_supseruser": true
}
```

:::

#### Example

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

## Authorization

### ACL File

1. Removed the `acl_file` configuration. The file-based ACL (acl.conf) will be used as one of the authorization sources and added to EMQX by default.
2. `acl.conf` data file syntax has been changed.

| 4.x    | 5.x      | Compatibility |
| ------ | -------- | ------------- |
| user   | username | Yes           |
| client | clientid | Yes           |
| pubsub | all      | No            |

::: details Example

```bash
# 4.x
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.
{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

# 5.x
{allow, {username, {re, "^dashboard$"}}, subscribe, ["$SYS/#"]}.
{allow, {ipaddr, "127.0.0.1"}, all, ["$SYS/#", "#"]}.
```

:::

### File-Based

In dsl, `pubsub` is renamed to `all`.

#### Example

EMQX 4.3

```
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

{allow, all}.
```

EMQX 5.1

```
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

{allow, {ipaddr, "127.0.0.1"}, all, ["$SYS/#", "#"]}.

{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

{allow, all}.
```

### Built-in Database

- Mnesia renamed to the Built-in Database.
- The data format and REST API have changed. For more information, please refer to `/authorization/sources/built_in_database/rules/{clients,users}`.

4.x ACL data can be exported with `./bin/emqx_ctl data export` command. Users may convert the data into 5.x format and import it through the corresponding REST API.

#### Example

EMQX 4.4

```
# No settings
```

EMQX 5.1

```
{
  type = built_in_database
  enable = true
}
```

### JWT

This is the implicit ACL based on claims fetched during JWT authentication. In ACL claims, `${variable}` placeholders instead of `%X` ones should be used.

### HTTP

```
type = http
```

- `method`, `pool_size`, `connect_timeout`, `enable_pipelining` are retained.
- `acl_req.url` is changed to `url`. 
- `acl_req.headers` is changed to `headers`. 
- `acl_req.params` is changed to `body`. 
- `timeout` to `request_timeout`.
- `ssl.*` is changed to common SSL options. Refer to [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access).

Unlike 4.4,  `url`, `headers`, and `body` parameters allow placeholders.   

In 5.1, `body` is not a string, but a map. It is serialized using JSON or X-WWW-Form-Urlencoded format (for post requests) or as query params (for get requests).

Unlike 4.4, HTTP authorization only respects responses with successful HTTP code (2XX) and takes the resolution from the response body (from `result`) field. The authorization result is now determined through JSON fields within the response body, rather than utilizing HTTP response status codes.

::: details

**Success response status code:**

```shell
200 or 204
```

Other status codes or request failure will be treated as `ignore`.

**Success Response Body (JSON):**

| Name   | Type | Required | Description             |
| ------ | ---- | -------- | ----------------------- |
| result | Enum | true     | `allow | deny | ignore` |

```json
{
  "result": "deny"
}
```

:::

#### Example

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

auto_reconnect may be used to reconnect to Redis automatically on failure.

- `type` is changed to `redis_type`.
  - For type `single`,  `server` is changed to `servers`.
  - For type `sentinel`,  `server` is changed to `servers`.
  - For type `cluster`,  `database` option is no longer available.
- `database` is changed to `database` (except for the `cluster` type; this option is not available for clusters anymore).
- `pool` is changed to `pool_size`.
- `password` is changed to `password`.
- `query_timeout` is no longer used.
- `ssl.*` options are changed to common SSL options. Refer to [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access).
- `auth_cmd` is changed to `cmd`.  `${var}`-style [placeholders](../access-control/authn/authn.md#authentication-placeholders) should be used in the command. 
- Redis data source still only supports white list mode, which requires setting `acl_nomatch = deny`;
- The `access` field name changes to `action`, and the data changes from numbers to action strings.

If you want to continue using the data from in 4.x, please make necessary migrations manually.

::: details The correspondence between 4.x and 5.x data

| 4.x  | 5.x       | action              |
| ---- | --------- | ------------------- |
| 1    | subscribe | subscribe           |
| 2    | publish   | publish             |
| 3    | all       | subscribe & publish |

**Data example in 5.x**

```
HSET mqtt_acl:emqx_u t/# subscribe
HSET mqtt_acl:emqx_u # all
HSET mqtt_acl:emqx_u a/1 publish
```

:::

#### Example

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

- The `ipaddr/username/clientid` field is no longer required in the query result.

- The `access` field name is changed to `action`, and its data type is changed from integer to character or character enumeration.

- The `allow` field name is changed to `permission`, and its data type is changed from integer to character or character enumeration.

  ::: details The correspondence between 4.x integer values and 5.x character/enumeration values

  **Access/action field mapping**

  | 4.x (int) | 5.x (varchar/enum) | action              |
  | --------- | ------------------ | ------------------- |
  | 1         | subscribe          | subscribe           |
  | 2         | publish            | publish             |
  | 3         | all                | subscribe & publish |

  **Allow/permission field mapping**

  | 4.x (int) | 5.x (varchar/enum) | permission |
  | --------- | ------------------ | ---------- |
  | 0         | deny               | deny       |
  | 1         | allow              | allow      |

  :::

- `server`, `username`, `password`, `database`, `query_timeout` are retained.

- `pool` is changed to `pool_size`.

- `ssl.*` options are changed to common SSL options. Refer to [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access).

- `acl_query` is changed to query.  `${var}`-style [placeholders](../access-control/authn/authn.md#authentication-placeholders) should be used.

You can use `auto_reconnect` to reconnect to MySQL automatically on failure.

Storage schema is changed.

In EMQX 4.4, the query should fetch rows with columns `[Allow, IpAddr, Username, ClientId, Access, Topic]` under any name exactly in this order.

In EMQX 5.1, the query should fetch rows with columns `permission, action, topic` in any order but under exactly these names. The “who“ part (`IpAddr, Username, ClientId`) is now suggested to be a part of the query.

#### Example

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

- The `ipaddr/username/clientid` field is no longer required in the query result.
- The `access` field name is changed to `action`, and its data type is changed from integer to character or character enumeration.
- The `allow` field name is changed to `permission`, and its data type is changed from integer to character or character enumeration.

::: details The correspondence between 4.x integer values and 5.x character/enumeration values

**Access/action field mapping**

| 4.x (int) | 5.x (varchar/enum) | action              |
| --------- | ------------------ | ------------------- |
| 1         | subscribe          | subscribe           |
| 2         | publish            | publish             |
| 3         | all                | subscribe & publish |

**Allow/permission field mapping**

| 4.x (int) | 5.x (varchar/enum) | permission |
| --------- | ------------------ | ---------- |
| 0         | deny               | deny       |
| 1         | allow              | allow      |

:::

- `server`, `username`, `password`, `database` are retained.
- `query_timeout` is not used anymore.
- `encoding` is not used anymore.
- `poo`l is changed to `pool_size`.
- `ssl.*` options are changed to common SSL options. Refer to [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access).
- `acl_query` is changed to `query`.  `${var}`-style [placeholders](../access-control/authn/authn.md#authentication-placeholders) should be used. 

Storage schema is changed.

In EMQX 4.4, the query should fetch rows with columns `[Allow, IpAddr, Username, ClientId, Access, Topic]` under any name exactly in this order.

In EMQX 5.1, the query should fetch rows with columns `permission, action, topic` in any order but under exactly these names. The “who“ part `(IpAddr, Username, ClientId)` is now suggested to be a part of the query.

### Example

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

- `type` is changed to `mongo_type` field. Possible values are `single`, `rs`, `sharded`. Unknown value is not available anymore.
- `server`
  - For `rs`, `sharded` to `servers`
  - For `single` to `server`
- `srv_record`, `username`, `password`, `auth_source`, `database`, `w_mode`, `topology`, `collection` are retained.
- `r_mode` is availalable only for `rs` type.
- `pool` is changed to `pool_size`.
- `ssl.*` is changed to common SSL options. Refer to [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access).
- `auth_query.selector` is changed to `filter`. The filter should not be a string, but the whole selector data structure.  `${var`}-style [placeholders](../access-control/authn/authn.md#authentication-placeholders) may be used in selector values.
- `query_timeout` is not used.

Storage schema is changed.

In EMQX 4.4, the resulting documents should contain topics lists by action key, like in Redis or JWT:

```
{
  "publish": ["t1", "t2"],
  "subscribe": ["t3", "t4"],
  "pubsub": ["t5", "t6"]
}
```

In EMQX 5.5, MongoDB data source can be used for both allow and deny rules. Previously, only white list mode was supported, and it was required to set `acl_nomatch = deny`. The documents should contain individual rules with `permission`, `action`, `topics` fields. Note that `topics` should be an array of topics. For details, see [AuthZ-MongoDB](../access-control/authz/mongodb.md).

If you want to continue using the data from in 4.x, please make the necessary migrations manually.

::: details Data example in 5.x

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

#### Example

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

