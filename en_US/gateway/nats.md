# NATS Protocol Gateway

EMQX 5.10 introduces the NATS gateway, which is implemented based on the [NATS Protocol](https://docs.nats.io/reference/reference-protocols/nats-protocol). It supports accepting connections from NATS clients and enables interoperability with MQTT publish/subscribe. Currently supported features include:

- Complete protocol message support, such as INFO, CONNECT, PUB/HPUB, SUB/UNSUB, MSG/HMsg, PING/PONG, +OK/-ERR.
- Support for CONNECT messages carrying `verbose=true` to enable message acknowledgment.
- Support for TCP, TLS, WebSocket, and WebSocket over TLS listeners.
- Support for NATS client publish/subscribe and wildcard subscriptions with MQTT publish/subscribe interoperability.
- Support for Queue Group shared subscriptions.
- Support for Request/Reply pattern, including fast failure response to requesting clients when there are no subscribers for the requested topic.

## Quick Start

In EMQX 5.0, you can configure and quickly enable the NATS gateway through the Dashboard.

You can also enable it through HTTP API or emqx.conf, for example:

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateway/nats' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "nats",
  "enable": true,
  "mountpoint": "nats/",
  "listeners": [
    {
      "type": "tcp",
      "name": "default",
      "bind": "4222",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'

```
:::

::: tab Configuration

```properties
gateway.nats {

  mountpoint = "nats/"

  listeners.tcp.default {
    bind = 4222
    acceptors = 16
    max_connections = 1024000
    max_conn_rate = 1000
  }
}
```
:::

::::

::: tip
Configuring the gateway through configuration files requires configuration on each node; managing through Dashboard or HTTP API will take effect across the entire cluster.
:::

The NATS gateway supports TCP/SSL/WS/WSS type listeners. For the complete list of configurable parameters, refer to the gateway configuration - listeners section in the [EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

## Authentication

The NATS protocol supports multiple authentication methods, including username/password and token authentication. The NATS gateway supports the following authenticator types:
- [Built-in Database Authentication](../access-control/authn/mnesia.md)
- [MySQL Authentication](../access-control/authn/mysql.md)
- [MongoDB Authentication](../access-control/authn/mongodb.md)
- [PostgreSQL Authentication](../access-control/authn/postgresql.md)
- [Redis Authentication](../access-control/authn/redis.md)
- [HTTP Server Authentication](../access-control/authn/http.md)
- [JWT Authentication](../access-control/authn/jwt.md)
- [LDAP Authentication](../access-control/authn/ldap.md)

The NATS gateway uses information from the CONNECT message of the NATS protocol to generate client authentication information. By default:

- Client ID: A randomly generated string.
- Username: The value of the `user` field in the CONNECT message.
- Password: The value of the `pass` field in the CONNECT message.

For example, create a built-in database authenticator for the NATS gateway through HTTP API or emqx.conf:

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'POST' \
  'http://127.0.0.1:18083/api/v5/gateway/nats/authentication' \
  -u <your-application-key>:<your-security-key> \
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
gateway.nats {

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

Unlike the MQTT protocol, **the gateway only supports creating one authenticator, not a list of authenticators (or authentication chain)**. When no authenticator is enabled, it means all NATS clients are allowed to connect.

For configuration formats of other types of authenticators, refer to: [Security - Authenticators](../access-control/authn/authn.md).

## Publish/Subscribe

The NATS protocol is fully compatible with the publish/subscribe messaging pattern and interacts with MQTT publish/subscribe. The NATS gateway conversion rules are:

- NATS protocol PUB and HPUB messages are used for message publishing.
  * The topic is the `subject` field in the PUB message. For example, a Subject of `t.a` will be converted by the NATS gateway to the MQTT topic `t/a` for publishing.
  * The message content is the message body content of the PUB message.
  * When the client connection CONNECT message has `verbose=1`, the converted message QoS is fixed at 1; otherwise, it's 0.
- NATS protocol SUB messages are used as subscription requests.
  * The topic is the `subject` field in the SUB message. For example, a Subject of `t.a` will be converted by the NATS gateway to the MQTT topic `t/a` for subscription.
  * When the client connection CONNECT message has `verbose=1`, the converted subscription QoS is fixed at 1; otherwise, it's 0.
  * Wildcards are supported, for example, `*.b.>` will be converted to `+/b/#`.
  * Shared subscriptions are supported. The Queue Group in the SUB message will be converted to the group name of MQTT shared subscriptions.
- NATS protocol UNSUB messages are used as unsubscription requests. The topic is the subscription ID corresponding to the UNSUB message.

The gateway has no independent publish/subscribe permission control. Topic permission control needs to be managed uniformly in [Authorization](../access-control/authz/authz.md).

## User Interface

- For detailed configuration instructions, refer to: [Gateway Configuration - NATS Gateway](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)
- For detailed HTTP API interface reference: [HTTP API - Gateway](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs)

## Limitations

Currently, in EMQX 5.10, there are the following implementation limitations:

- Since the current gateway listener does not support upgrading from TCP to TLS connections, clients connecting with `tls_handshake_first=false` are not currently supported.
- When no authenticator is configured, NATS clients that do not send CONNECT messages are supported for publish/subscribe, but managing anonymous clients is not currently supported. 