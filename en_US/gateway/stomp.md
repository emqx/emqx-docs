# Stomp Gateway

## Introduction

The Stomp gateway is based on the [Stomp v1.2](https://stomp.github.io/stomp-specification-1.2.html) and is compatible with the Stomp v1.0 and v1.1 specification.

## Quick Start

In EMQX 5.0, Stomp gateways can be configured and enabled through the Dashboard.

It can also be enabled via the HTTP API or emqx.conf,  e.g:

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'POST' 'http://127.0.0.1:18083/api/v5/gateway' \
  -u admin:public \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "stomp",
  "enable": true,
  "mountpoint": "stomp/",
  "listeners": [
    {
      "type": "tcp",
      "name": "default",
      "bind": "61613",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'
```

:::

::: tab Configuration

```properties
gateway.stomp {

  mountpoint = "stomp/"

  listeners.tcp.default {
    bind = 61613
    acceptors = 16
    max_connections = 1024000
    max_conn_rate = 1000
  }
}
```

:::

::::


::: tip
Configuring the gateway via emqx.conf requires changes on a per-node basis, but configuring it via Dashboard or the HTTP API will take effect across the cluster.
:::

The Stomp gateway only supports TCP and SSL type listeners, for a complete list of configurable parameters refer to: [Gateway Configuration - Listeners](../configuration/configuration-manual.md)

## Authentication

As the concept of username and password is already defined in the connection message of the Stomp protocol,
it supports a variety of authenticator types, such as:

- [Built-in Database Authentication](../access-control/authn/mnesia.md)
- [MySQL Authentication](../access-control/authn/mysql.md)
- [MongoDB Authentication](../access-control/authn/mongodb.md)
- [PostgreSQL Authentication](../access-control/authn/postgresql.md)
- [Redis Authentication](../access-control/authn/redis.md)
- [HTTP Server Authentication](../access-control/authn/http.md)
- [JWT Authentication](../access-control/authn/jwt.md)

Stomp gateway uses the information in the CONNECT or STOMP message of STOMP protocol to generate the authentication fields for the client:

- Client ID: is a randomly generated string.
- Username: is the value of the `login` field in the CONNECT or STOMP message headers.
- Password: The value of the `passcode` field in the CONNECT or STOMP message headers.

For example, to create a built-in database authentication for a Stomp gateway via HTTP API or emqx.conf:

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'POST' \
  'http://127.0.0.1:18083/api/v5/gateway/stomp/authentication' \
  -u admin:public
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{
  "backend": "built_in_database",
  "mechanism": "password_based",
  "password_hash_algorithm": {
    "name": "sha256",
    "salt_position": "suffix"
  },
  "user_id_type": "username"
}'
```

:::

::: tab Configuration

```properties
gateway.stomp {

  authentication {
    backend = built_in_database
    mechanism = password_based
    password_hash_algorithm {
      name = sha256
      salt_position = suffix
    }
    user_id_type = username
  }
}
```
:::

::::


Unlike the MQTT protocol, **the gateway only supports the creation of an authenticator, not a list of authenticators (or an authentication chain)**. When no authenticator is enabled, it means that all Stomp clients are allowed to log in.

For the configuration format of other types of authenticators refer to: [Security - Authenticator](../access-control/authn/authn.md)

## Publish/Subscribe

The Stomp protocol is fully compatible with the PUB/SUB messaging model, and the Stomp gateway uses:
- The SEND message of the Stomp protocol is used as a message publishing. The topic is the `destination` field in the SEND message, the message content is the message body content of the SEND message, and the QoS is fixed to 0.
- The SUBSCRIBE message of the Stomp protocol is used as a subscribing request. The topic is the `destination` field of the SUBSCRIBE message, the QoS is fixed to 0, and the wildcards defined in the MQTT protocol are supported.
- The UNSUBSCRIBE message of the Stomp protocol is used as an unsubscribe request. The topic is the `destination` field in the UNSUBSCRIBE message.

There is no special authorization configurations within Stomp gateway, and its permission control for topics needs to be configured [Authorization](../access-control/authz/authz.md).

## User Interfaces

- Detailed confguration options: [Configuration - Stomp Gateway](../configuration/configuration-manual.md)
- Detailed HTTP APIs description: [HTTP API - Gateway](../admin/api.md)

## Client libraries

- [erlang-stomp-client](https://github.com/KodiEhf/erlang-stomp-client)
- [stomp.py](https://github.com/jasonrbriggs/stomp.py)
