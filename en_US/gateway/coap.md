# CoAP Gateway

## Introduction

The CoAP gateway implements publish, subscribe, and receive messages as standard with [Publish-Subscribe Broker for the CoAP](https://datatracker.ietf.org/doc/html/draft-ietf-core-coap-pubsub-09).

For security reasons, the CoAP gateway implements **Connection Mode** to provide client access authentication to restrict unauthorized CoAP clients.


## Quick Start

In EMQX 5.0, CoAP gateways can be configured and enabled through the Dashboard.

It can also be enabled via the HTTP API, and emqx.conf e.g:

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateway/coap' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "coap",
  "enable": true,
  "mountpoint": "coap/",
  "connection_required": false,
  "listeners": [
    {
      "type": "udp",
      "name": "default",
      "bind": "5683",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'
```
:::

::: tab Configuration

```properties
gateway.coap {

  mountpoint = "coap/"

  connection_required = false

  listeners.udp.default {
    bind = "5683"
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

The CoAP gateway only supports UDP and DTLS type listeners, for a complete list of configurable parameters refer to: [Gateway Configuration - Listeners](../configuration/configuration-manual.md)


## Working Mode

CoAP gateway support two working mode:
- `Connectionless Mode`: This mode fully follows the [Publish-Subscribe Broker for the CoAP](https://datatracker.ietf.org/doc/html/draft-ietf-core-coap-pubsub-09) standard. In this mode there is no need for connection authentication, session and heartbeat maintenance. Only supports:
  * Message Publish
  * Topic Subscribe
  * Topic Unsubscribe

- `Connection Mode`: This model defines the concepts of connection authentication, session, and heartbeat maintenance. Client needs to create a connection before publishing or subscribing. Then it uses the token returned by the connection to invoke message publishing and topic subscription. It supports:
  * Create connection
  * Close connection
  * Heartbeat
  * Authentication

Enabling `Connection Mode` is determined by the `connection_required` option:

```properties
# emqx.conf
gateway.coap {

  ## true: Enabling connection mode
  ## false: Using connectionless mode
  connection_required = true
}
```

:::tip
`Connection mode` only brings new connection management related interfaces. You can still perform publish/subscribe and unsubscribe operations in this mode, but you need to carry ClientId and Token for each request.
:::


## Authentication

Only available in `Connection Mode`.

The client ID, username, and password are provided by the client's [Create Connection](#create-connection) request. The CoAP gateway supports the following authenticator types:

- [Built-in Database Authentication](../access-control/authn/mnesia.md)
- [MySQL Authentication](../access-control/authn/mysql.md)
- [MongoDB Authentication](../access-control/authn/mongodb.md)
- [PostgreSQL Authentication](../access-control/authn/postgresql.md)
- [Redis Authentication](../access-control/authn/redis.md)
- [HTTP Server Authentication](../access-control/authn/http.md)
- [JWT Authentication](../access-control/authn/jwt.md)

For example, to create a built-in database authentication for CoAP gateway via HTTP API, or emqx.conf:

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'POST' \
  'http://127.0.0.1:18083/api/v5/gateway/coap/authentication' \
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
gateway.coap {

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


Unlike the MQTT protocol, **the gateway only supports the creation of an authenticator, not a list of authenticators (or an authentication chain)**. When no authenticator is enabled, it means that all CoAP clients are allowed to log in.

For the configuration format of other types of authenticators refer to: [Security - Authenticator](../access-control/authn/authn.md)


## Publish/Subscribe

The CoAP gateway uses the URI path and methods defined in the [Publish-Subscribe Broker for the CoAP](https://datatracker.ietf.org/doc/html/draft-ietf-core-coap-pubsub-09) standard.

Detailed parameters refer to [Message Publish](#message-publish), [Topic Subscribe](#topic-subscribe), [Topic Unsubscribe](#topic-unsubscribe).

There is no special authorization configurations within CoAP gateway, and its permission control for topics needs to be configured [Authorization](../access-control/authz/authz.md).


## User Interfaces

- Detailed confguration options: [Configuration - CoAP Gateway](../configuration/configuration-manual.md)
- Detailed HTTP APIs description: [HTTP API - Gateway](../admin/api.md)


## Client libraries

- [libcoap](https://github.com/obgm/libcoap)
- [californium](https://github.com/eclipse/californium)


## Appendix: Client Side Interfaces

### Create Connection

Only available in `Connection Mode`.

This interface is used to create a client connection to the CoAP gateway.
When the authentication of the CoAP gateway is enabled, the gateway will
verify the `clientid`, `username`, `password` provided by this request to prevent illegal users.

**Request Parameters:**
- Method: `POST`
- URI: `mqtt/connection{?QueryString*}`, the `QueryString` is:
  - `clientid`: required parameter, UTF-8 string. The gateway uses this string as a unique identifier for the connection
  - `username`: optional parameter, UTF-8 string. Used for connection authentication.
  - `password`: optional parameter, UTF-8 string. Used for connection authentication.
- Payload: empty

**Response:**
- Return Code:
  - `2.01`: Connection created successfully. Token string for this connection will be returned in the message body.
  - `4.00`: Bad request. Detailed error information will be returned in the message body.
  - `4.01`: Not authorized. The request format is validated, but authorization falied.
- Payload: When the return code is `2.01`, the message body is `Token`, otherwise it is `ErrorMessage`.
  - `Token`: The token string to be used for subsequent requests.
  - `ErrorMessage`: the error description messages.

Take `libcoap` as an example:

```bash
# Initiate a connection request with clientid 123 and username and password admin/public.
# Returned token is 3404490787
coap-client -m post -e "" "coap://127.0.0.1/mqtt/connection?clientid=123&username=admin&password=public"

3404490787
```

:::tip
After the connection is successfully created, you can use Dashboard, HTTP API or CLI to check the client list in the CoAP gateway.
:::


### Close Connnection

Only available in `Connection Mode`.

This interface is used to close the CoAP connection.

**Request Parameters:**
- Method: `DELETE`
- URI: `mqtt/connection{?QueryString*}`, the `QueryString` is:
  - `clientid`: required parameter, UTF-8 string. The gateway uses this string as a unique identifier for the connection
  - `token`: required parameter, Using the token string returned by Create Connetion request
- Payload: empty

**Response:**
- Return Code:
  - `2.01`: Close connection successfully.
  - `4.00`: Bad request. Detailed error information will be returned in the message body.
  - `4.01`: Not authorized. The request format is validated, but authorization falied.
- Payload: When the return code is `2.01`, the message body is `Token`, otherwise it is `ErrorMessage`.

For example:

```bash
coap-client -m delete -e "" "coap://127.0.0.1/mqtt/connection?clientid=123&token=3404490787"
```

### Hearbeat

Only available in `Connection Mode`.

This interface is used to maintain the connection between the CoAP client and the gateway. When the heartbeat expires, the gateway deletes the session, subscription, and releases all resources for that client.

**Request Parameters:**
- Method: `PUT`
- URI: `mqtt/connection{?QueryString*}`, the `QueryString` is:
  - `clientid`: required parameter, UTF-8 string. The gateway uses this string as a unique identifier for the connection
  - `token`: required parameter, Using the token string returned by Create Connetion request
- Payload: empty

**Response:**
- Return Code:
  - `2.01`: Close connection successfully.
  - `4.00`: Bad request. Detailed error information will be returned in the message body.
  - `4.01`: Not authorized. The request format is validated, but authorization falied.
- Payload: When the return code is `2.01`, the message body is `Token`, otherwise it is `ErrorMessage`.

For example:

```bash
coap-client -m put -e "" "coap://127.0.0.1/mqtt/connection?clientid=123&token=3404490787"
```

:::tip
The heartbeat interval is determined by the `hearbeat` option of CoAP gateway, defaults to 30 seconds.
:::


### Message Publish

This interface is used by the CoAP client to send messages to specified topic. Additional identity information needs to be carried if the `Connection Mode` enabled.

**Request Parameters:**
- Method: `POST`
- URI: `mqtt/{+topic}{?QueryString*}`
  -  `{+topic}` is the topic of publish message, i.e. the URI is `ps/coap/test` if publish message to `coap/test`.
  - `{?QueryString}` is request parameters:
    - `clientid`: required in `Connection Mode` and optional in `Connectionless Mode`.
    - `token`: `Connection Mode` only, required parameter.
    - `retain`: Whether to publish as a retained message. boolean, optional parameter, default is `false`.
    - `qos`: Message QoS, which identifies the QoS level of the message and affects only how the MQTT client receives the message. Enum with `0`, `1`, `2`.
    - `expiry`: Message expiry interval in seconds, default is 0 which means never expire

- Payload: Message payload

**Response:**
- Return Code:
  - `2.04`: Publish successfully
  - `4.00`: Bad request. Detailed error information will be returned in the message body.
  - `4.01`: Not authorized. The request format is validated, but authorization falied.
- Payload: When the return code is `2.04`, the message body is empty, otherwise it is `ErrorMessage`.

For example, publish a message in `Connectionless Mode`:
```bash
coap-client -m post -e "Hi, this is libcoap" "coap://127.0.0.1/ps/coap/test"
```

Or carry `clientid` and `token` in `Connection Mode`:

```bash
coap-client -m post -e "Hi, this is libcoap" "coap://127.0.0.1/ps/coap/test&clientid=123&token=3404490787"
```


### Topic Subscribe

This interface is used by the CoAP client to subscribe a topic.
Additional identity information needs to be carried if the `Connection Mode` enabled.

**Request Parameters:**
- Method: `POST`
- Options: Set `observer` to `0`
- URI: `mqtt/{+topic}{?QueryString*}`
  -  `{+topic}` is the topic need to subscribe, i.e. the URI is `ps/coap/test` if subscribe to `coap/test`.
  - `{?QueryString}` is request parameters:
    - `clientid`: required in `Connection Mode` and optional in `Connectionless Mode`.
    - `token`: `Connection Mode` only, required parameter.
    - `qos`: Subscription QoS to indicate which the MessageType(`CON` or `NON`) used by gateway to deliver messages to CoAP client. Enumerated with:
      - `0`: Using NON message to delivery
      - `1` or `2`: Using CON  message to delivery

- Payload: empty

**Response:**
- Return Code:
  - `2.05`: Subscribe successfully
  - `4.00`: Bad request. Detailed error information will be returned in the message body.
  - `4.01`: Not authorized. The request format is validated, but authorization falied.
- Payload: When the return code is `2.05`, the message body is empty, otherwise it is `ErrorMessage`.

For example，subscribe `coap/test` in `Connectionless Mode`:

```bash
coap-client -m get -s 60 -O 6,0x00 -o - -T "obstoken" "coap://127.0.0.1/ps/coap/test"
```

Or, carry `clientid` and `token` to subscribe in `Connection Mode`:

```bash
coap-client -m post -e "Hi, this is libcoap" "coap://127.0.0.1/ps/coap/test&clientid=123&token=3404490787"
```


### Topic Unsubscribe

This interface is used by the CoAP client to unsubscribe a topic.
Additional identity information needs to be carried if the `Connection Mode` enabled.

**Request Parameters:**
- Method: `GET`
- URI: `mqtt/{+topic}{?QueryString*}`
  -  `{+topic}` is the topic need to unsubscribe, i.e. the URI is `ps/coap/test` if subscribe to `coap/test`.
  - `{?QueryString}` is request parameters:
    - `clientid`: required in `Connection Mode` and optional in `Connectionless Mode`.
    - `token`: `Connection Mode` only, required parameter.

- Payload: empty

**Response:**
- Return Code:
  - `2.07`: Unsubscribe successfully
  - `4.00`: Bad request. Detailed error information will be returned in the message body.
  - `4.01`: Not authorized. The request format is validated, but authorization falied.
- Payload: When the return code is `2.07`, the message body is empty, otherwise it is `ErrorMessage`.
