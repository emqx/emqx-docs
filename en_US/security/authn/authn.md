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
- At the transport layer, TLS guarantees client-to-server authentication using client certificates and ensures that the server verifies the server certificate to the client. PSK-based TLS/DTLS authentication is also supported. For transport
layer security see [SSL documentation](../ssl.md).

In this document, we describe EMQX authentication and its configuration concepts.

## Authentication sources

_Authentication source_ (or simply _authenticator_) is an EMQX module that implements MQTT authentication.
Authenticators are identified by their _mechanism_ (i.e. algorithm of authentication) and _backend_ (data source
for credentials).

The following authenticators are available by default:

| mechanism        | backend            | description                                                                   |
| ----             | ------------------ | ----------------------------------------------------------------------------- |
| password_based   | built_in_database  | [Authentication with Mnesia database as credential storage](./mnesia.md)       |
| password_based   | mysql              | [Authentication with MySQL database as credential storage](mysql.md)           |
| password_based   | PostgreSQL         | [Authentication with PostgreSQL database as credential storage](postgresql.md) |
| password_based   | MongoDB            | [Authentication with MongoDB database as credential storage](./mongodb.md)     |
| password_based   | redis              | [Authentication with Redis database as credential storage](./redis.md)         |
| password_based   | http               | [Authentication using external HTTP API for credential verification](./http.md)|
| jwt              |                    | [Authentication using JWT](./jwt.md)                                           |
| scram            | built_in_database  | [Authentication using SCRAM](./scram.md)                                       |

Each authenticator has its own configuration options.

Example:

```
{
  mechanism = password_based
  backend = redis
  enable = true

  password_hash_algorithm {
    name = plain
    salt_position = suffix
  }

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

```
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

```
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
  authentication = {
    ...
  }

  listeners.tcp.default {
    ...
    # Specific chain for `tcp.default` STOMP listener
    authentication = {
      ...
    }
  }
}

```

Listener-specific chains completely override the global chains, i.e. if a listener has its own chain, then the global chain
isn't used at all for clients connecting to the listener.

When a client connects to a listener it is authenticated with the listener-specific chain. If there is no
chain specified for the listener, then the global chain for the listener protocol is used.

If a chain contains a single authenticator, its configuration can be used as chain configuration.
I.e. `[ ]` brackets may be omitted:

```
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

::: warning
Please note, that gateway listeners can have only a single authenticator in their chains.
:::

## Password hashing

Password-based authenticators with a database backend (`built_in_database`, `mysql`, `mongodb`, `redis`, `postgresql`)
support multiple password hashing algorithms.

The algorithm for password verification is the following:
* Authenticator extracts hashed password and salt from the database using provided queries/selectors.
* Hashes password provided by the client with the configured hashing algorithm and fetched salt.
* Securely compares the resulting hash with the hash extracted from the database.

The following password hashing algorithms are supported:

```
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

For password-based authenticators that allow user creation through EMQX API (`password_based:built_in_database`)
there are additional parameters required for hash creation:

```
# bcrypt
password_hash_algorithm {
  name = bcrypt
  salt_rounds = 10          # used for user creation
}
```

## Authentication placeholders

Many authenticators allow using _placeholders_ in their configuration.
Placeholders work as template variables that are filled with the corresponding
client's information when performing authentication.

The following placeholders are available:
* `${clientid}` — Client ID of the connecting client, either passed explicitly by the client or automatically generated.
* `${username}` — `username` value of `CONNECT` MQTT packet.
* `${password}` — `password` value of `CONNECT` MQTT packet.
* `${peerhost}` — Client IP address. EMQX supports [Proxy protocol](http://www.haproxy.org/download/1.8/doc/proxy-protocol.txt), so a user can make use of this value even if EMQX is behind some TCP proxy or load balancer.
* `${cert_subject}` — subject of client's TLS certificate, valid only for TLS connections.
* `${cert_common_name}` common name of client's TLS certificate, valid only for TLS connections.

Example of use (in Redis password-based authenticator):

```
{
  mechanism = password_based
  backend = redis

  ... other parameters ...

  cmd = "HMGET users:${clientid} password_hash salt is_superuser"
}
```

When a client with clientid `id2718` tries to connect, a Redis query
```
HMGET users:id2718 password_hash salt is_superuser
```
is performed to search credentials.

## REST API

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

For managing authenticators of the global MQTT chain, the endpoint is `/api/v5/authentication`.

For managing authenticators of the global chains of the other protocols (handled by `emqx_gateway`)
, the endpoint is `/api/v5/gateway/{protocol}/authentication`.

For managing authenticators of a concrete MQTT listener chain, the endpoint is `/api/v5/listeners/{listener_id}/authentication`.

For managing authenticators of a concrete listener chain of the other protocols (handled by `emqx_gateway`), the endpoint is `/api/v5/gateway/{protocol}/listeners/{listener_id}/authentication`.

Listener id name convention for MQTT chains is the following:
```
<transport_protocol>:<name>
```
For example, `listener_id` for the listener
```
listeners.quic.default {
  ...
}
```
is `quic:default`.

Listener id name convention for gateway chains is the following:
```
<protocol>:<transport_protocol>:<name>
```
For example, `listener_id` for the listener
```
gateway.stomp {
  ...
  listeners.tcp.def {
    ...
  }
}
```
is `stomp:tcp:def`.

::: tip
When used in URLs, listener ids should be URL-encoded: `quic%3Adefault`.
:::
