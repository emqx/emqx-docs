# Authentication / Authorization Compatibility between v4.4 and v5.1 

## Common Changes 

### SSL options

All providers using external connections (MySQL, PostgreSQL, MongoDB, Redis, HTTP) have the same options for establishing secure connection via TLS under ssl key: https://www.emqx.io/docs/en/v5.0/network/overview.html#tls-for-external-resource-access.

### Placeholders

Backends supporting some kind of data interpolation (MySQL, PostgreSQL, MongoDB, Redis, HTTP for external requests, JWT for data interpolation) support ${variable} placeholders instead of %X ones. 

## Authentication

### Common Changes (All Authentication Sources)

#### Password Hashing

All password-based providers (Built-in database, MySQL, PostgreSQL, MongoDB, Redis) have the same password hashing options, configured in the same way: https://www.emqx.io/docs/en/v5.0/access-control/authn/authn.html#password-hashing 

#### Per-Listener Authentication

Unlike 4.4, each MQTT listener may have its own authentication. Also, enable_authn listener option is available:

- enable_authn=true is the default, when authentication is delegated to the authentication chain;
- enable_authn=false switches off authentication completely for the listener;
- enable_authn=quick_deny_anonymous like true, but also immediately rejects connecting clients without credentials.

#### Defaults

EMQX 5.1 does not have explicit allow_anonymous settings. One should remove or disable all authenticators in the global or listener-specific chain to allow anonymous access.

### Built-in Database

- No user records in the config
- password_hash to password_hash_algorithm {name = Algo, salt_position = prefix} (see https://www.emqx.io/docs/en/v5.0/access-control/authn/authn.html#password-hashing )
- user_id_type is used to identify whether clientid or username should be used as MQTT user identifiers. Mixed types of records are not allowed.

#### Example

4.4

```
auth.mnesia.password_hash = sha256
```

5.1

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

- No support for SHA1 hashing (used in 4.4). Use algorithm to choose between sha512 and sha256 algorithms.
- `iteration_count` can be configured (4096 is implicitly used in 4.4). 

#### Example

4.4

```
# No configuration
```

5.1

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

- type to redis_type 
  - for type single:
    - server to server;
  - for type sentine
    - server to servers;
  - for type sentinel:
    - server to servers;
- database to database (except cluster type, there is no this option for cluster anymore);
- pool to pool_size;
- password to password;
- query_timeout is not used anymore;
- ssl.* to common SSL options https://www.emqx.io/docs/en/v5.0/network/overview.html#tls-for-external-resource-access ;
- auth_cmd to cmd. Ony HGET and HMGET commands are supported. ${var}-style [placeholders](https://www.emqx.io/docs/en/v5.0/access-control/authn/authn.html#authentication-placeholders) should be used.  Command should fetch at least password or password_hash field and additionally salt and is_superuser fields;
- super_cmd is not used anymore, provide is_superuser field in cmd instead.
- password_hash to common password_hash_algorithm parameters.

auto_reconnect may be used to reconnect to Redis automatically on failure.

#### Example

4.4

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

5.1

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

- server, username, password, database, query_timeout to themselves
- pool to pool_size;
- ssl.* to common SSL options https://www.emqx.io/docs/en/v5.0/network/overview.html#tls-for-external-resource-access ;
- password_hash to common password_hash_algorithm parameters.
- auth_query to query.  ${var}-style [placeholders](https://www.emqx.io/docs/en/v5.0/access-control/authn/authn.html#authentication-placeholders) should be used. Query should fetch at least password or password_hash column and additionally salt and is_superuser columns;
- super_query is not used anymore, provide is_superuser column in query instead.

auto_reconnect may be used to reconnect to MySQL automatically on failure.

#### Example

4.4

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

5.1

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

- server, username, password, database.
- query_timeout is not used anymore.
- encoding is not used anymore.
- pool to pool_size;
- ssl.* to common SSL options https://www.emqx.io/docs/en/v5.0/network/overview.html#tls-for-external-resource-access ;
- password_hash to common password_hash_algorithm parameters.
- auth_query to query.  ${var}-style [placeholders](https://www.emqx.io/docs/en/v5.0/access-control/authn/authn.html#authentication-placeholders) should be used. Query should fetch at least password or password_hash column and additionally salt and is_superuser columns;
- super_query is not used anymore, provide is_superuser column in query instead.

#### Example

4.4

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

5.1

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

- type to mongo_type field; possible values are single, rs, sharded . unknown value is not available anymore.
- server
  - for rs, sharded to servers
  - for single to server
- srv_record, username, password, auth_source, database, w_mode, topology, collection to themselves;
- r_mode is availalable only for rs type.
- pool to pool_size;
- ssl.* to common SSL options https://www.emqx.io/docs/en/v5.0/network/overview.html#tls-for-external-resource-access ;
- auth_query.selector to filter. The filter should be not a string, but the whole selector data structure,  ${var}-style [placeholders](https://www.emqx.io/docs/en/v5.0/access-control/authn/authn.html#authentication-placeholders) may be used in selector values.
- auth_query.salt_field to salt_field
- auth_query.super_field to is_superuser_field
- super_query is not used anymore. Provide is_superuser_field field in the documents fetched with filter together with is_superuser_field setting.
- password_hash to common password_hash_algorithm parameters.
- query_timeout is not used.

#### Example

4.4

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

5.1

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

- secret, from, verify_claims, acl_claim_name, refresh_interval  to themselves
- pubkey to public_key
- jwks to endpoint
- signature_format is no more available

In verifiy_claims, ${var}-style placeholders (${username} and ${clientid}) should be used in selector values instead of %X ones.

Additional parameters:

- use_jwks whether to fetch keys from JWKS
- algorithm — public-key|hmac-based which type of signature to verify.
- secret_base64_encoded — specifies secret format
- pool_size — number of connections to JWKS server
- ssl SSL options for connecting to JWKS server

Not all sets of parameters are allowed. 

Unlike 4.4, not all combinations of  (secret,secret_base64_encoded,public_key,endpoint, pool_size,refresh_interval,ssl ) parameters are allowed. use_jwks and algorithm identify the available sets:

With use_jwks=true and algorithm=public-key:  endpoint, pool_size,refresh_interval,ssl

With use_jwks=false and algorithm=public-key:  public_key

With use_jwks=false and algorithm=hmac-based: secret,secret_base64_encoded

Combination use_jwks=true and algorithm=hmac-based is invalid.

#### Example

4.4

```
auth.jwt.jwks = https://127.0.0.1:8080/jwks
auth.jwt.jwks.refresh_interval = 5m
auth.jwt.from = password

auth.jwt.verify_claims = on

auth.jwt.verify_claims.username = %u

auth.jwt.acl_claim_name = acl
```

5.1

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

- method, pool_size, connect_timeout, enable_pipelining to themselves
- auth_req.url to url 
- auth_req.headers to headers 
- auth_req.params to body 
- timeout to request_timeout
- ssl.* to common SSL options https://www.emqx.io/docs/en/v5.0/network/overview.html#tls-for-external-resource-access;
- super_req is not available. Provide is_superuser field in the service response instead.

Unlike 4.4 url, headers and body parameters allow placeholders.   

In 5.1, body is not a string, but a map. It is serialized using JSON or X-WWW-Form-Urlencoded format (for post requests) or as query params (for get requests).

Unlike 4.4, HTTP authentication only respects responses with successful HTTP code (2XX) and takes the resolution from the response body (from result) field.  

#### Example

4.4

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

5.1

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

### File-based

In dsl, pubsub is renamed to all.

#### Example

4.3

```
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

{allow, all}.
```

5.1

```
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

{allow, {ipaddr, "127.0.0.1"}, all, ["$SYS/#", "#"]}.

{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

{allow, all}.
```

### Built-in Database

#### Example

4.4

```
# No settings
```

5.1

```
{
  type = built_in_database
  enable = true
}
```

### JWT

This is the implicit ACL based on claims fetched during JWT authentication. In ACL claims, ${variable} placeholders instead of %X ones should be used.

### HTTP

```
type = http
```

- method, pool_size, connect_timeout, enable_pipelining to themselves
- acl_req.url to url 
- acl_req.headers to headers 
- acl_req.params to body 
- timeout to request_timeout
- ssl.* to common SSL options https://www.emqx.io/docs/en/v5.0/network/overview.html#tls-for-external-resource-access;

Unlike 4.4 url, headers and body parameters allow placeholders.   

In 5.1, body is not a string, but a map. It is serialized using JSON or X-WWW-Form-Urlencoded format (for post requests) or as query params (for get requests).

Unlike 4.4, HTTP authorization only respects responses with successful HTTP code (2XX) and takes the resolution from the response body (from result) field.  

#### Example

4.4

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

5.1

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

- type to redis_type 
  - for type single:
    - server to server;
  - for type sentine
    - server to servers;
  - for type sentinel:
    - server to servers;
- database to database (except cluster type, there is no this option for cluster anymore);
- pool to pool_size;
- password to password;
- query_timeout is not used anymore;
- ssl.* to common SSL options https://www.emqx.io/docs/en/v5.0/network/overview.html#tls-for-external-resource-access ;
- acl_cmd to cmd. ${var}-style [placeholders](https://www.emqx.io/docs/en/v5.0/access-control/authn/authn.html#authentication-placeholders) should be used.

auto_reconnect may be used to reconnect to Redis automatically on failure.

#### Example

4.4

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

5.1

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

- server, username, password, database, query_timeout to themselves
- pool to pool_size;
- ssl.* to common SSL options https://www.emqx.io/docs/en/v5.0/network/overview.html#tls-for-external-resource-access ;
- acl_query to query.  ${var}-style [placeholders](https://www.emqx.io/docs/en/v5.0/access-control/authn/authn.html#authentication-placeholders) should be used

auto_reconnect may be used to reconnect to MySQL automatically on failure.

Storage schema is changed.

In 4.4, the query should fetch rows with columns [Allow, IpAddr, Username, ClientId, Access, Topic] under any name exactly in this order.

In 5.1, the query should fetch rows with columns permission, action, topic in any order but under exactly these names. The “who“ part (IpAddr, Username, ClientId) is now suggested to pe a part of the query.

#### Example

4.4

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

5.1

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

- server, username, password, database to themselves.
- query_timeout is not used anymore.
- encoding is not used anymore.
- pool to pool_size;
- ssl.* to common SSL options https://www.emqx.io/docs/en/v5.0/network/overview.html#tls-for-external-resource-access ;
- acl_query to query.  ${var}-style [placeholders](https://www.emqx.io/docs/en/v5.0/access-control/authn/authn.html#authentication-placeholders) should be used. 

Storage schema is changed.

In 4.4, the query should fetch rows with columns [Allow, IpAddr, Username, ClientId, Access, Topic] under any name exactly in this order.

In 5.1, the query should fetch rows with columns permission, action, topic in any order but under exactly these names. The “who“ part (IpAddr, Username, ClientId) is now suggested to pe a part of the query.

### Example

4.4

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

5.1

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

- type to mongo_type field; possible values are single, rs, sharded . unknown value is not available anymore.
- server
  - for rs, sharded to servers
  - for single to server
- srv_record, username, password, auth_source, database, w_mode, topology, collection to themselves;
- r_mode is availalable only for rs type.
- pool to pool_size;
- ssl.* to common SSL options https://www.emqx.io/docs/en/v5.0/network/overview.html#tls-for-external-resource-access ;
- acl_query.selector to filter. The filter should be not a string, but the whole selector data structure,  ${var}-style [placeholders](https://www.emqx.io/docs/en/v5.0/access-control/authn/authn.html#authentication-placeholders) may be used in selector values.
- query_timeout is not used.

Storage schema is changed.

In 4.4, the resulting documents should contain topics lists by action key, like in Redis or JWT:

```
{
  "publish": ["t1", "t2"],
  "subscribe": ["t3", "t4"],
  "pubsub": ["t5", "t6"]
}
```

In 5.5, the documents should contain individual rules with permission, action, topics fields. Note that topics should be an array of topics.

#### Example

4.4

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

5.1

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