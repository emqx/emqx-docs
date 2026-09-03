# Listener Configuration

In EMQX, listener is configured to receive requests from MQTT clients. EMQX supports the following message transfer protocols, including:

- TCP: port  `1883`
- SSL: port `8883`
- Websocket listener: `8083`
- Secure websocket listener: `8084`

::: tip

You can also configure listeners via Dashboard by clicking **Management** -> **Listeners** on the left navigation menu of the Dashboard.
If you want to configure listeners from config files, it is recommended to use `base.hocon` instead of `emqx.conf`.
This is because if the configuration is set in emqx.conf, any changes made through the Dashboard will only be temporary and will be lost upon EMQX restart.

:::

::: tip

EMQX offers more configuration items to better serve customized needs. For details, see the [EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

:::

## Configure TCP Listener

TCP listener is a network service that listens for incoming TCP connections on a specific network port. It plays an essential role in establishing and managing connections between clients and EMQX over TCP/IP networks. 

To configure the TCP listener in EMQX, you can add the `listeners.tcp` configuration items in the `base.hocon` file within the `etc` folder of the EMQX installation directory.

For example, to enable the TCP listener on port `1883`, with a maximum 1,024,000 of concurrent connections allowed by the listener, you can work the code below:

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
}
```

where, 

- `listeners.tcp.default` is to enable the listener, and here `default` is the name of the listener, you can change it to your own listener name. 

- `bind` is to set the IP address and port of the listener, here it will listen to all incoming traffic from any IP address on port `1883`. 
- `max_connection` is to set the maximum number of concurrent connections allowed by the listener; default value: `infinity`.

## Configure SSL Listener

SSL listener is a network service that listens for incoming SSL (Secure Sockets Layer) connections. In EMQX, it is used to secure network traffic between a client and EMQX by encrypting the data that is transmitted between them.

To configure the SSL listener in EMQX, you can add the `listeners.ssl` configuration items in the `base.hocon` file within the `etc` folder of the EMQX installation directory.

For example, to enable the SSL listener on port `8883`, with a maximum 1,024,000 of concurrent connections allowed by the listener:

```bash
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  max_connections = 1024000
  ssl_options {
    cacertfile = "etc/certs/cacert.pem"
    certfile = "etc/certs/cert.pem"
    keyfile = "etc/certs/key.pem"
    verify = verify_none
    fail_if_no_peer_cert = false
  }
}
```

where:

- `listeners.ssl.default` is to enable the listener. 

- `bind` is the IP address and port of the listener, here it will listen to all incoming traffic from any IP address on port `8883`. 
- `max_connection` is the maximum number of concurrent connections allowed by the listener, default value: `infinity`.
- `ssl_options` is the SSL/TLS configuration option for the listener, it has three properties:
  - `cacertfile`: PEM file containing the trusted CA (certificate authority) certificates that the listener uses to verify the authenticity of the client certificates.
  - `certfile`: PEM file containing the SSL/TLS certificate chain for the listener. If the certificate is not directly issued by a root CA, the intermediate CA certificates should be appended after the listener certificate to form a chain.
  - `keyfile`: PEM file containing the private key corresponding to the SSL/TLS certificate.
  - `verify`:  Set 'verify_peer' to verify the authenticity of the clients' certificates, otherwise 'verify_none'.
  - `fail_if_no_peer_cert`: If set to `true`, the server fails if the client does not have a certificate to send, that is, sends an empty certificate. If set to false, it fails only if the client sends an invalid certificate (an empty certificate is considered valid).

## Configure WebSocket Listener

A WebSocket listener is a network service that receives and processes messages over WebSocket. WebSocket support in EMQX allows clients to use the WebSocket protocol to connect to EMQX and exchange data in real-time.

For an overview of how MQTT over WebSocket works and typical usage scenarios, see [MQTT over WebSocket](../connect-emqx/mqtt-over-websocket.md).

To configure the WebSocket listener in EMQX, you can add the `listeners.ws` configuration items in the `base.hocon` file within the `etc` folder of the EMQX installation directory.

For example, to enable the WebSocket listener on port `8083`, with a maximum 1,024,000 of concurrent connections allowed by the listener:

```bash
listeners.ws.default {
  bind = "0.0.0.0:8083"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
}
```

where:

- `listeners.ws.default` is to enable the listener. 

- `bind` is the IP address and port of the listener, here it will listen to all incoming traffic from any IP address on port `8083`. 
- `max_connection` is the maximum number of concurrent connections allowed by the listener, default value: `infinity`.
- `websocket.mqtt_path` is to set the path to the WebSocket’s MQTT protocol, which is `/mqtt` by default. 

## Configure Secure WebSocket Listener

A secure WebSocket listener is a WebSocket listener that uses the Secure Sockets Layer (SSL) or Transport Layer Security (TLS) protocol to encrypt the data exchanged between a WebSocket client and the broker. In EMQX, the secure WebSocket listener is an important security measure to protect sensitive data exchanged between WebSocket clients and EMQX> 

To configure the secure WebSocket listener in EMQX, you can add the `listeners.wss` configuration items in the `base.hocon` file within the `etc` folder of the EMQX installation directory.

For example, to enable the Secure WebSocket listener on port `8084`, with a maximum 1,024,000 of concurrent connections allowed by the listener:

```bash
listeners.wss.default {
  bind = "0.0.0.0:8084"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
  ssl_options {
    cacertfile = "etc/certs/cacert.pem"
    certfile = "etc/certs/cert.pem"
    keyfile = "etc/certs/key.pem"
  }
}
```

where:

- `listeners.wss.default` is to enable the listener. 

- `bind` is the IP address and port of the listener, here it will listen to all incoming traffic from any IP address on port `8084`. 
- `max_connection` is the maximum number of concurrent connections allowed by the listener, default value: `infinity`.
- `websocket.mqtt_path` is to set the path to the WebSocket’s MQTT protocol, which is `/mqtt` by default. 
- `ssl_options` is the SSL/TLS configuration option for the listener, it has three properties:
  - `cacertfile`: PEM file containing the trusted CA (certificate authority) certificates that the listener uses to verify the authenticity of the client certificates.
  - `certfile`: PEM file containing the SSL/TLS certificate chain for the listener. If the certificate is not directly issued by a root CA, the intermediate CA certificates should be appended after the listener certificate to form a chain.
  - `keyfile`: PEM file containing the private key corresponding to the SSL/TLS certificate.

## Forwarded Client Address (WebSocket Listeners)

WebSocket and secure WebSocket listeners have two options that control how EMQX determines a client's source address when the listener sits behind a proxy or load balancer:

- `websocket.proxy_address_header`: Specifies the HTTP header that carries the client IP address.
- `websocket.proxy_port_header`: Specifies the HTTP header that carries the client port.

Starting from EMQX 6.3.0, both options default to `""`. EMQX uses the corresponding TCP peer address or port for any option left empty. To obtain either value from a trusted proxy, explicitly configure the corresponding header name, such as `x-forwarded-for` or `x-forwarded-port`.

When the configured header is present on the WebSocket upgrade request, EMQX uses the first (leftmost) entry of the header value as the client's source IP address (or port) instead of the address of the real TCP peer. The derived address is what IP-based authorization rules, banned clients, flapping detection, and audit and trace logs see as the client's source IP. Configured header names are matched case-insensitively.

::: warning Trust Forwarded Address Headers Only Behind a Trusted Proxy

The header value determines the client source IP that EMQX uses, so it must be honored only when a trusted proxy sets it:

- If the listener is directly reachable by clients (no proxy in front), keep `proxy_address_header` and `proxy_port_header` empty so that EMQX always uses the real TCP peer address.
- If there is a proxy but it **appends** its observation to an inbound `X-Forwarded-For` header instead of overwriting or stripping it (appending is the default behavior of most proxies, for example NGINX's `$proxy_add_x_forwarded_for`), the leftmost entry that EMQX reads is still the one supplied by the client, so the source IP can still be spoofed. Configure the proxy to overwrite the header with the address it observed, use the [PROXY protocol](../deploy/cluster/lb.md) instead, or set the options to `""`.
- Do not try to disable the mechanism by pointing the option at an unused header name: a client can send a header by any name. The empty string is the only value a client can never supply.

When `proxy_protocol = true` is set on the listener, the client address comes from the PROXY protocol handshake, and these headers are not consulted.
:::

<!--To add QUIC-->

<!--To add code sample for adding multiple listeners.-->

## Link Listener to a Configuration Zone

Each listener in EMQX is associated with a zone, which by default is set to a logical zone named `default`.

When a listener is linked to a specific zone, MQTT clients connected to that listener inherit the settings from that zone.

For more information, see the [Zone Override](./configuration.md#zone-override) section in the configuration documentation.

## Mountpoint

Each listener can be configured with a `mountpoint`: a topic prefix that EMQX adds to topics used by clients connected through the listener. The prefix is added to topics in `PUBLISH` packets, `SUBSCRIBE` and `UNSUBSCRIBE` requests, and Will messages, and removed from the topics of messages delivered to the client. The mountpoint is transparent to the client and is commonly used to isolate topic spaces between groups of clients, for example in multi-tenant deployments.

```bash
listeners.tcp.demo {
    bind = "0.0.0.0:1883"
    mountpoint = "department-a/"
}
```

The mountpoint supports the placeholders `${clientid}`, `${username}`, `${zone}`, and `${client_attrs.NAME}`. For example, with `mountpoint = "${username}/"`, when a client with username `u1` subscribes to `sensors/#`, the subscription is internally created as `u1/sensors/#`.

### Incompatibility with Topic-Prefix Extension Features

Several EMQX features are triggered by publishing or subscribing to topics that start with a special `$` prefix. EMQX adds the mountpoint prefix before it matches these prefixes. For example, if a client connects through a listener with mountpoint `mp/` and publishes to `$delayed/10/t`, the broker receives the topic as `mp/$delayed/10/t`, which no longer starts with `$delayed/`. The feature is silently bypassed: EMQX routes the message as an ordinary message to the mounted literal topic, and no error is reported to the client.

::: warning Compatibility Limitation
Do not configure a mountpoint on listeners whose clients use any of the following features:

| Feature | Topic Prefix |
| --- | --- |
| [Delayed Publish](../messaging/mqtt-delayed-publish.md) | `$delayed/` |
| [File Transfer](../file-transfer/introduction.md) | `$file/`, `$file-async/`, `$file-response/` |
| [Message Queue](../message-queue/message-queue-concept.md) | `$queue/` |
| [MQTT Streams](../mqtt-stream/mqtt-stream-concept.md) | `$stream/` |
| [Cluster Linking](../cluster-linking/introduction.md) | `$LINK/` |
| [Dynamic Keep Alive Adjustment](./mqtt.md#dynamic-keep-alive-adjustment) | `$SETOPTS/` |
| [A2A over MQTT](../emqx-ai/a2a-over-mqtt/overview.md) | `$a2a/` |

For Cluster Linking, the mountpoint must not be set on the listener that accepts connections from the linked cluster. For A2A over MQTT, a mountpoint of exactly one topic level (for example `acme/`) still works: EMQX parses it as a namespace prefix on `$a2a` topics.
:::

[Shared subscriptions](../messaging/mqtt-shared-subscription.md) (`$share/{group}/`) and [exclusive subscriptions](../messaging/mqtt-exclusive-subscription.md) (`$exclusive/`) are exceptions: they work with a mountpoint. EMQX parses these subscription prefixes before applying the mountpoint, so the mountpoint is added only to the inner topic filter. For example, subscribing to `$share/g/t` through a listener with mountpoint `mp/` joins the shared subscription group `g` on the topic `mp/t`.
