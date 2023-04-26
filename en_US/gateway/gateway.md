# Multi-Protocol Gateway

EMQX Multi-Protocol Gateway enables the handling of all non-MQTT protocol connections, authentication, and message sending and receiving. It provides a unified conceptual model for various protocols.

Prior to EMQX 5.0, non-MQTT protocol access was implemented by separate protocol plugins. These plugins had different design and implementation differences, making it challenging to use them.

Starting from 5.0, EMQX offers the Multi-Protocol Gateway defines a unified conceptual and operational model to make it easier to use. 

The Multi-Protocol Gateway supports the MQTT-SN, STOMP, CoAP, and LwM2M protocols. It can be enabled and configured directly in the Dashboard, or managed using the HTTP API or `emqx.conf`. On how to enable these gateways and how to customize the settings to better suit your business needs, you can click the link below for details. 

- [MQTT-SN](./mqttsn.md)
- [STOMP](./stomp.md)
- [CoAP](./coap.md)
- [LwM2M](./lwm2m.md)

## How the Multi-Protocol Gateway Works

EMQX Multi-Protocol Gateway defines a unified conceptual and operational model for several key components, including listeners, connections/sessions, publish/subscribe, and authentication, and authorization. 

<img src="./assets/gateway_struct.png" alt="gateway_struct" style="zoom:50%;" />

Here's a brief overview of each component:

- **Listener**: Support listener types: TCP, SSL, UDP, DTLS. Each gateway can create multiple listeners.
- **Connection/Session**: Gateway creates a session for each accepted client connection, which manages the subscription list, deliver/receive queue, and retransmission logic of client messages.
- **Publish/Subscribe**: Each type of gateway defines how to adapt to the PUB/SUB message model of the MQTT protocol. Non-PUB/SUB protocols require configuring message topics and payloads, and each type of gateway may use a different message format.

- **Authentication**: Each gateway can be configured with authenticators to use the client information for login authorization.

## Key Features

### Message format

To ensure compatibility with the PUB/SUB messaging model, each gateway type must adapt to the presence or absence of a PUB/SUB concept in its underlying protocol.

For protocols with a PUB/SUB concept, like [MQTT-SN](./mqttsn.md) and [Stomp](./stomp.md), gateways define topics and message payloads. In these cases, compatibility is achieved by using the client-sent topic and payload, and no message format conversion is needed.

For protocols without a PUB/SUB concept, such as [CoAP](./coap.md) and [LwM2M](./lwm2m.md), there are no definitions for topics, publishing, or subscribing. Here, the gateway must design the message content format, with each type potentially using a distinct format.

- **CoAP**: The CoAP gateway uses the URI path and methods defined in the [Publish-Subscribe Broker for the CoAP](https://datatracker.ietf.org/doc/html/draft-ietf-core-coap-pubsub-09) standard. For details, see [Message Publish](./coap.md#message-publish), [Topic Subscribe](./coap.md#topic-subscribe), [Topic Unsubscribe](./coap.md#topic-unsubscribe).
- **LwM2M**: The Messaging model of LwM2M protocol is based on [Resources Model and Operations](https://technical.openmobilealliance.org/OMNA/LwM2M/LwM2MRegistry.html). This is completely different from the Publish/Subscribe model of the MQTT protocol. For details, see [LwM2M Gateway - Message Format](./lwm2m.md#message-format).

### Authentications

Authentication is the process of verifying the identity of a client attempting to connect to a system. Starting from version 5.0, the gateway supports authenticators for login authorization. 

Different gateways may support different types of authenticators, but all gateways support HTTP-based authentication. [HTTP-based authentication](../access-control/authn/http.md). 

Note: If no authenticator is configured, any client is allowed to log in. 

#### How Authentication Works on the Gateway

The EMQX Multi-Protocol Gateway is responsible for authenticating clients that connect to it. This is accomplished through the creation of a `ClientInfo` for each connection.

The `ClientInfo` includes generic fields such as `Username` and `Password`, which are commonly used for authentication purposes. Additionally, each gateway has its own specific client information fields, such as `Endpoint Name` and `Life Time` for LwM2M, which may also be used for authentication.

When an authenticator is configured, the gateway compares the client's Username and Password fields with those stored in its database. If they match, the client is authenticated and granted access to the gateway.

::: tip

Client ID for different gateways can be duplicated, but when a duplicated Client ID logs in to a gateway, it will terminate the existing session associated with that Client ID.

:::

#### Listener Level Authentication

Each gateway can have its own authenticator configuration, and the data is kept separate between them. Different listening ports can be set up for different authentication methods and data sources. 

This can be achieved by configuring `mountpoint` and `authentication` for different listeners to override the gateway-level settings.

**Example Code:**

```properties
gateway.stomp {

  listeners.tcp.default {
    bind = 61613
    ## e.g, to configure the built-in database based authenticator for port 61613
    authentication {
      mechanism = password_based
      backend = built_in_database
      user_id_type = username
    }
  }

  listeners.tcp.default2 {
    bind = 61614
    ## e.g, to configure the HTTP Server-based authenticator for port 61614
    authentication {
      mechanism = password_based
      backend = http
      method = post
      url = "http://127.0.0.1:9000/stomp/auth"
      headers {
        content-type = "application/json"
      }
      body {
        username = "${username}"
        password = "${password}"
      }
    }
  }
}
```

::: tip
"Applying different authenticators to each listener is currently only supported in the configuration file `emqx.conf` and is not yet available in the HTTP API and Dashboard."
:::

### <!--Authorization-->

<!--this part should be rewritten, not sure what should be here-->

<!--In order to adapt the PUB/SUB messaging model, each type of gateway has to fulfill the compatibility with it.-->

<!--For protocol gateways that already have a PUB/SUB concept, such as MQTT-SN, Stomp usually defines the concept of topics and message payloads.-->
- <!--The gateway is compatible with the PUB/SUB messaging model by using the topic and payload sent by client.-->
- <!--Needs to configure the topic authorization rules in [Authorization](../access-control/authz/authz.md).-->

<!--However, for non-PUB/SUB concept protocols, it lacks the definition of topic, publish, and subscribe, so:-->
- <!--it is necessary to configure the topics of different messages. i.e, in the LwM2M gateway, we need to configure the topics for each type of message.-->
- <!--it is up to the gateway to design the format of the message content. Each type of gateway may use a different message format.-->
- <!--Needs to configure the topic authorization rules in [Authorization](../access-control/authz/authz.md).-->

<!--::: tip-->
<!--The authorization rules apply to all listeners and topics connected to the gateway.-->
<!--:::-->

## Integration with External Systems

For better integration with external systems, the gateway also supports hooks defined in the EMQX.

Due to the heterogeneity of semantics between gateways, only some of the core hooks are available.

Client connection-related hooks with the following supportability:

For improved interoperability with external systems, the gateway is designed to support hooks as defined in EMQX.

However, due to the differences in semantics among various gateways, only a subset of the core hooks can be utilized, see the table below for the client connection-related hooks supported: <!--I think we should make a more clear table, like a hook support -->

| Name                  | Required or Not | Description                                                  | Supported Protocols |
| --------------------- | --------------- | ------------------------------------------------------------ | ------------------- |
| `client.connect`      | Optional        | Originally used for MQTT protocol <!--not sure about the why we have it here--> | partial gateways    |
| `client.connack`      | Optional        | Originally used for MQTT protocol                            | partial gateways    |
| `client.authenticate` | Required        | Client authentication request                                | All gateways        |
| `client.connected`    | Required        | Client connected successfully                                | All gateways        |
| `client.disconnected` | Required        | Client disconnected                                          | All gateways        |
| `client.authorize`    | Required        | Client publish/subscribe authorization request               | All gateways        |
| `client.subscribe`    | Optional        | Originally used for MQTT protocol                            | partial gateways    |
| `client.unsubscribe`  | Optional        | Originally used for MQTT protocol                            | partial gateways    |

Session and message-related hooks have no heterogeneity issues between protocols, so these hooks are fully supported for each type of gateway.

