# Introduction

Authentication is an important part of most applications. MQTT protocol supports username/password authentication as
well as some enhanced methods, like SCRAM authentication. Enabling authentication can effectively prevent illegal client connections.

Authentication in EMQX Broker means that when a client connects to EMQX Broker, the server configuration is used to control the client's permission to connect to the server.

EMQX Broker's authentication support includes two levels:

- The MQTT protocol itself specifies authentication primitives. EMQX Broker supports multiple variants of MQTT-level authentication:
  * username/password authentication with various backends (MongoDB, MySQL, PostgreSQL, Redis and built-in database);
  * SCRAM authentication with the built-in database;
  * JWT authentication;
  * authentication via custom HTTP API.
- At the transport layer, TLS guarantees client-to-server authentication using client certificates and ensures that the server verifies the server certificate to the client. PSK-based TLS/DTLS authentication is also supported.

In this document, we describe EMQX authentication and its configuration concepts.

## Authentication sources

_Authentication source_ (or simply _authenticator_) is an EMQX module that implements MQTT authentication. The following authenticators are
available by default:

| mechanism        | backend            | description                                                                   |
| ----             | ------------------ | ----------------------------------------------------------------------------- |
| password_based   | built_in_database  | [Authenticaton with Mnesia database as credential storage](./mnesia.md)       |
| password_based   | mysql              | [Authenticaton with MySQL database as credential storage](mysql.md)           |
| password_based   | postgresql         | [Authenticaton with PostgreSQL database as credential storage](postgresql.md) |
| password_based   | mongodb            | [Authenticaton with MongoDB database as credential storage](./mongodb.md)     |
| password_based   | redis              | [Authenticaton with Redis database as credential storage](./redis.md)         |
| password_based   | http               | [Authenticaton using external HTTP API for credential verification](./http.md)|
| jwt              |                    | [Authenticaton using JWT](./jwt.md)                                           |
| scram            | built_in_database  | [Authenticaton using SCRAM](./scram.md)                                       |

Each authenticator has its own configuration options.

Example:

```hocon
{
  mechanism = password_based
  password_hash_algorithm {
    name = plain
    salt_position = suffix
  }
  enable = true
  backend = redis
  cmd = "HMGET mqtt_user:${username} password_hash salt is_superuser"
  database = 1
  password = public
  redis_type = single
  server = "127.0.0.1:6379"
}
```

When a client connects, the configured authenticators perform identity verification of the client by checking
whether its `username`/`clientid` and `password` are consistent with the data pertaining to the authenticator.

## Authentication chains

When authenticating a client, EMQX may try to perform identity verification using several authenticators
sequentially. Each authenticator may either return authentication success/failure or pass verification to
the next authenticator in the sequence. Such sequences are called _authentication chains_.

Conditions on which verification is passed further in the chain depend on the authenticator and described
in its documentation.

Each authentication chain can contain only one authenticator of each type.

Configured authentication chains can be updated or rearranged dynamically. Each authenticator
can be _enabled_ or _disabled_.

Example:

```hocon
authentication = [
  {
    mechanism = password_based
    password_hash_algorithm {
      name = plain
      salt_position = suffix
    }
    enable = true
    backend = redis
    cmd = "HMGET mqtt_user:${username} password_hash salt is_superuser"
    database = 1
    password = public
    redis_type = single
    server = "127.0.0.1:6379"
  },
  {
    mechanism = password_based
    backend = built_in_database
    user_id_type = username
    password_hash_algorithm {
        name = sha256
        salt_position = suffix
    }
    enable = false
  }
]

```

An authentication chain without any enabled authenticators allows anonymous access.

Authentication chains can be configured globally per protocol as well as per individual protocol listeners:

```hocon
# emqx.conf

# Global chain for MQTT protocol
authentication = [
  ...
]

listeners.quic.default {
  ...
  # Specific chain for `quic.default` MQTT listener
  authentication = [
    ...
  ]
}

gateway.stomp {
  ...

  # Global chain for STOMP protocol
  authentication = [
    ...
  ]

  listeners.tcp.default {
    ...
    # Specific chain for `tcp.default` STOMP listener
    authentication = [
      ...
    ]
  }
}

```

When a client connects to a listener it is authenticated with the listener-specific chain. If there is no
chain specified for the listener, then the global chain for the listener protocol is used.

If a chain contains a single authenticator, its configuration can be used as chain configuration.
I.e. `[ ]` brackets may be omitted:

```hocon
authentication {
    mechanism = password_based
    backend = built_in_database
    user_id_type = username
    password_hash_algorithm {
        name = sha256
        salt_position = suffix
    }
    enable = false
}
```

## Password hashing

Password-based authenticators with a database backend (`built_in_database`, `mysql`, `mongodb`, `redis`, `postgresql`)
support multiple password hashing algorithms.

The algorithm for password verification is the following:
* Authenticator extracts hashed password and salt from the database using provided queries/selectors.
* Hashes password provided by the client with the configured hashing algorithm and fetched salt.
* Securely compares the resulting hash with the hash extracted from the database.

The following password hashing algorithms are supported:

```hocon
# simple algorithms:
password_hash_algorithm {
  name = sha256             # plain, md5, sha, sha512
  salt_position = suffix    # prefix, disable
}

# bcrypt
password_hash_algorithm {
  name = bcrypt
}

# pbkdf2
password_hash_algorithm {
  name = pbkdf2
  mac_fun = sha256          # md4, md5, ripemd160, sha, sha224, sha384, sha512
  iterations = 4096
  dk_length = 256           # optional
}
```

For password-based authenticators that allow user creation through EMQX API (`built_in_database`)
there are additional parameters required for hash creation:

```hocon
# bcrypt
password_hash_algorithm {
  name = bcrypt
  salt_rounds = 10          # used for user creation
}
```

## HTTP API

Authentication API allows to manipulate authentication chains and
concrete authenticators.

Authenticators are identified by their id formed as:
```
<mechanism>:<backend>
```
or just
```
<mechanism>
```
if there is no backend.

For example:

```
password_based:built_in_database
jwt
scram:built_in_database
```

::: tip
When used in URLs, authenticator ids should be URL-encoded: `password_based%3Abuilt_in_database`.
:::

### Global chain API

Global chain API operates with MQTT global (default) authentication chain.

#### GET /api/v5/authentication

Get the global MQTT authentication chain.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X GET \
http://localhost:18083/api/v5/authentication

## Response
[
  {
    "backend":"built_in_database",
    "enable":true,
    "id":"password_based:built_in_database",
    "mechanism":"password_based",
    "password_hash_algorithm":{
      "name":"sha256",
      "salt_position":"suffix"
    },
    "user_id_type":"username"
  }
]
```

#### POST /api/v5/authentication

Add authenticator to the global MQTT authentication chain.
Documentation for concrete authenticator fields can be found on its own
documentation page.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X POST \
-H "Content-Type: application/json" \
http://localhost:18083/api/v5/authentication \
-d @- <<'EOF'
{
  "mechanism": "password_based",
  "backend": "built_in_database",
  "user_id_type": "username",
  "password_hash_algorithm": {
    "name": "sha256",
    "salt_position": "suffix"
  }
}
EOF

## Response
{
  "backend":"built_in_database",
  "enable":true,
  "id":"password_based:built_in_database",
  "mechanism":"password_based",
  "password_hash_algorithm":{
    "name":"sha256",
    "salt_position":"suffix"
  },
  "user_id_type":"username"
}
```

#### DELETE /api/v5/authentication/{id}

Delete an authenticator from the global MQTT authentication chain.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X GET \
http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database

## Response
{
  "backend":"built_in_database",
  "enable":true,
  "id":"password_based:built_in_database",
  "mechanism":"password_based",
  "password_hash_algorithm":{
    "name":"sha256",
    "salt_position":"suffix"
  },
  "user_id_type":"username"
}
```

#### DELETE /api/v5/authentication/{id}

Delete authenticator from the global MQTT authentication chain.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X DELETE \
http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database

## Response
## 204 No Content
```

#### PUT /api/v5/authentication/{id}

Update configuration of an authenticator from the global MQTT authentication chain.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X PUT \
-H "Content-Type: application/json" \
http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database \
-d @- <<'EOF'
{
  "mechanism": "password_based",
  "backend": "built_in_database",
  "user_id_type": "clientid",
  "password_hash_algorithm": {
    "name": "sha512",
    "salt_position": "prefix"
  }
}
EOF

## Response
{
  "backend":"built_in_database",
  "enable":true,
  "id":"password_based:built_in_database",
  "mechanism":"password_based",
  "password_hash_algorithm":{
    "name":"sha512",
    "salt_position":"prefix"
  },"
  user_id_type":"clientid"
}
```

#### GET /api/v5/authentication/{id}/status

Get statistics of an authenticator from the global MQTT authentication chain.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X GET \
http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database/status

## Response
{
  "metrics": {
    "failed": 0,
    "nomatch": 0,
    "rate": 0,
    "rate_last5m": 0,
    "rate_max": 0,
    "success": 0,
    "total": 0
  },
  "node_error": [],
  "node_metrics": [
    {
      "metrics": {
        "failed": 0,
        "nomatch": 0,
        "rate": 0,
        "rate_last5m": 0,
        "rate_max": 0,
        "success": 0,
        "total": 0
      },
      "node": "emqx@127.0.0.1"
    }
  ],
  "node_resource_metrics": [
    {
      "metrics": {},
      "node": "emqx@127.0.0.1"
    }
  ],
  "node_status": [
    {
      "node": "emqx@127.0.0.1",
      "status": "connected"
    }
  ],
  "resource_metrics": {},
  "status": "connected"
}
```

#### POST /api/v5/authentication/{id}/move

Move an authenticator within the global chain.

The following move commands are available:

* Move an authenticator to the beginning of the chain (it will be applied first during authentication):
  ```json
  {"position": "front"}
  ```
* Move to the end of the chain (it will be applied last during authentication):
  ```json
  {"position": "rear"}
  ```
* Place at the position preceding some other authenticator:
  ```json
  {"position": "before:password_based:built_in_database"}
  ```

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X POST \
-H "Content-Type: application/json" \
http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database/move \
-d @- <<'EOF'
{"position": "front"}
EOF

## Response
## 204 No Content
```

### Listener chain API

Listener chain API allows operating with authentication chains of specific MQTT listeners by
listener ids.

Listener ids name convention is the following:
```
<transport_protocol>:<name>
```
For example, `listener_id` for the listener
```hocon
listeners.quic.default {
  ...
}
```
is `quic:default`.

::: tip
When used in URLs, listener ids should be URL-encoded: `quic%3Adefault`.
:::


#### GET /api/v5/listeners/{listener_id}/authentication

Get an MQTT listener authentication chain.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X GET \
http://localhost:18083/api/v5/listeners/tcp%3Adefault/authentication

## Response
[
  {
    "backend":"built_in_database",
    "enable":true,
    "id":"password_based:built_in_database",
    "mechanism":"password_based",
    "password_hash_algorithm":{
      "name":"sha256",
      "salt_position":"suffix"
    },
    "user_id_type":"username"
  }
]
```

#### POST /api/v5/listeners/{listener_id}/authentication

Add authenticator to an MQTT listener authentication chain.
Documentation for concrete authenticator fields can be found on its own
documentation page.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X POST \
-H "Content-Type: application/json" \
http://localhost:18083/api/v5/listeners/tcp%3Adefault/authentication \
-d @- <<'EOF'
{
  "mechanism": "password_based",
  "backend": "built_in_database",
  "user_id_type": "username",
  "password_hash_algorithm": {
    "name": "sha256",
    "salt_position": "suffix"
  }
}
EOF

## Response
{
  "backend":"built_in_database",
  "enable":true,
  "id":"password_based:built_in_database",
  "mechanism":"password_based",
  "password_hash_algorithm":{
    "name":"sha256",
    "salt_position":"suffix"
  },
  "user_id_type":"username"
}
```

#### GET /api/v5/listeners/{listener_id}/authentication/{id}

Get the configuration of an authenticator from an MQTT listener authentication chain.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X GET \
http://localhost:18083/api/v5/listeners/tcp%3Adefault/authentication/password_based%3Abuilt_in_database

## Response
{
  "backend":"built_in_database",
  "enable":true,
  "id":"password_based:built_in_database",
  "mechanism":"password_based",
  "password_hash_algorithm":{
    "name":"sha256",
    "salt_position":"suffix"
  },
  "user_id_type":"username"
}
```

#### DELETE /api/v5/listeners/{listener_id}/authentication/{id}

Delete an authenticator from an MQTT listener authentication chain.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X DELETE \
http://localhost:18083/api/v5/listeners/tcp%3Adefault/authentication/password_based%3Abuilt_in_database

## Response
## 204 No Content
```

#### PUT /api/v5/listeners/{listener_id}/authentication/{id}

Update the configuration of an authenticator from an MQTT listener authentication chain.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X PUT \
-H "Content-Type: application/json" \
http://localhost:18083/api/v5/listeners/tcp%3Adefault/authentication/password_based%3Abuilt_in_database \
-d @- <<'EOF'
{
  "mechanism": "password_based",
  "backend": "built_in_database",
  "user_id_type": "clientid",
  "password_hash_algorithm": {
    "name": "sha512",
    "salt_position": "prefix"
  }
}
EOF

## Response
{
  "backend":"built_in_database",
  "enable":true,
  "id":"password_based:built_in_database",
  "mechanism":"password_based",
  "password_hash_algorithm":{
    "name":"sha512",
    "salt_position":"prefix"
  },"
  user_id_type":"clientid"
}
```

#### GET /api/v5/listeners/{listener_id}/authentication/{id}/status

Get statistics of an authenticator from an MQTT listener authentication chain.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X GET \
http://localhost:18083/api/v5/listeners/tcp%3Adefault/authentication/password_based%3Abuilt_in_database/status

## Response
{
  "metrics": {
    "failed": 0,
    "nomatch": 0,
    "rate": 0,
    "rate_last5m": 0,
    "rate_max": 0,
    "success": 0,
    "total": 0
  },
  "node_error": [],
  "node_metrics": [
    {
      "metrics": {
        "failed": 0,
        "nomatch": 0,
        "rate": 0,
        "rate_last5m": 0,
        "rate_max": 0,
        "success": 0,
        "total": 0
      },
      "node": "emqx@127.0.0.1"
    }
  ],
  "node_resource_metrics": [
    {
      "metrics": {},
      "node": "emqx@127.0.0.1"
    }
  ],
  "node_status": [
    {
      "node": "emqx@127.0.0.1",
      "status": "connected"
    }
  ],
  "resource_metrics": {},
  "status": "connected"
}
```

#### POST /api/v5/listeners/{listener_id}/authentication/{id}/move

Move an authenticator within an MQTT listener authentication chain.

The following move commands are available:

* Move an authenticator to the beginning of the chain (it will be applied first during authentication):
  ```json
  {"position": "front"}
  ```
* Move to the end of the chain (it will be applied last during authentication):
  ```json
  {"position": "rear"}
  ```
* Place at the position preceding some other authenticator:
  ```json
  {"position": "before:password_based:built_in_database"}
  ```

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X POST \
-H "Content-Type: application/json" \
http://localhost:18083/api/v5/listeners/tcp%3Adefault/authentication/password_based%3Abuilt_in_database/move \
-d @- <<'EOF'
{"position": "front"}
EOF

## Response
## 204 No Content
```

## TLS authentication

To enable TLS authentication for clients, one may add an `ssl` listener
with `verify_peer` verify option set.
The default `ssl` MQTT listener is run on the 8883 port:

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"

  access_rules = [
    "allow all"
  ]

  proxy_protocol = false
  proxy_protocol_timeout = 3s

  ssl.keyfile = "etc/certs/key.pem"
  ssl.certfile = "etc/certs/cert.pem"
  ssl.cacertfile = "etc/certs/cacert.pem"

  # to verify client certs
  ssl.verify = verify_peer

  ssl.versions = ["tlsv1.3", "tlsv1.2", "tlsv1.1", "tlsv1"]
  tcp.backlog = 1024
  tcp.buffer = 4KB
}
```

Note that the `key.pem`,` cert.pem`, and `cacert.pem` under the default directory of `etc/certs` are self-signed certificates generated by EMQX Broker. Therefore, when testing with a client that supports TLS, you need to configure the above CA certificate `etc/certs/cacert.pem` to the client. For production use, securely issued certificates must be used.

## PSK authentication

To enable PSK authentication, one should enable `psk_authentication` section in `emqx.conf`:

```hocon
psk_authentication {
    ## Whether to enable the PSK feature.
    enable = true

    ## If init file is specified, emqx will import PSKs from the file
    ## into the built-in database at startup for use by the runtime.
    ##
    ## The file has to be structured line-by-line, each line must be in
    ## the format: <PSKIdentity>:<SharedSecret>
    init_file = "data/init.psk"

    ## Specifies the separator for PSKIdentity and SharedSecret in the init file.
    ## The default is colon (:)
    separator = ":"

    ## The size of each chunk used to import to the built-in database from psk file
    ## chunk_size = 50
}
```

File with psk identities and keys (`data/init.psk`) should be created:

```
myclient1:8c701116e9127c57a99d5563709af3deaca75563e2c4dd0865701ae839fb6d79
myclient2:d1e617d3b963757bfc21dad3fea169716c3a2f053f23decaea5cdfaabd04bfc4
...
```

`ssl` listener should be configured to use PSK ciphers:

```
listeners.ssl.default {
  ...
  ssl.versions = ["tlsv1.2"]
  ssl.ciphers = "RSA-PSK-AES256-CBC-SHA384,RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,RSA-PSK-RC4-SHA,RSA-PSK-DES-CBC3-SHA"
  ...
}

```

