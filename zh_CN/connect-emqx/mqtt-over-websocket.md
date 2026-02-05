# MQTT over WebSocket

EMQX supports MQTT over WebSocket, enabling MQTT clients to communicate with the broker over WebSocket connections instead of raw TCP or TLS.

MQTT over WebSocket is functionally identical to MQTT over TCP/TLS. The only difference lies in the transport layer: MQTT packets are encapsulated inside WebSocket frames and transmitted over HTTP or HTTPS.

This transport is primarily intended for environments where direct TCP connections are unavailable or restricted, such as web browsers and certain enterprise networks.

## Protocol Stack

When using MQTT over WebSocket, the protocol stack is as follows:

```
MQTT
WebSocket
HTTP / HTTPS
TCP
IP
```

The MQTT protocol itself remains unchanged. All MQTT control packets (CONNECT, PUBLISH, SUBSCRIBE, etc.) are transmitted as-is within WebSocket frames.

## How EMQX Implements MQTT over WebSocket

EMQX provides MQTT over WebSocket support through a WebSocket listener.

The connection workflow is:

1. A client initiates an HTTP or HTTPS request to the EMQX WebSocket endpoint.
2. The request includes an `Upgrade: websocket` header.
3. EMQX accepts the upgrade and establishes a WebSocket connection.
4. MQTT packets are exchanged within WebSocket frames.
5. The packets are processed by the same MQTT core engine used for TCP/TLS connections.

Once the WebSocket connection is established, EMQX handles the client session exactly the same way as any other MQTT connection.

## Feature Parity with MQTT over TCP/TLS

Using WebSocket does **not** affect MQTT functionality.

The following features are fully supported and behave identically to MQTT over TCP or TLS:

- MQTT v3.1, v3.1.1, and v5.0
- QoS 0, 1, and 2
- Retained messages
- Will messages
- Persistent sessions and offline messages
- Shared subscriptions
- Authentication and authorization (Username/Password, JWT, OAuth, etc.)

The transport layer is the only difference.

## WebSocket and Secure WebSocket (WS / WSS)

EMQX supports both:

- **WS** (WebSocket over HTTP)
- **WSS** (WebSocket over HTTPS)

When using **WSS**, TLS is applied in the same way as:

- HTTPS
- MQTT over TLS (port 8883)

This allows you to reuse existing TLS certificates and security configurations, including mutual TLS authentication if required.

## Typical Use Cases

MQTT over WebSocket is particularly suitable for:

- **Browser-based clients**

  Web browsers do not support raw TCP sockets but provide native WebSocket APIs.

- **Restricted network environments**

  WebSocket traffic over ports 80 or 443 can traverse proxies and firewalls that block custom TCP ports.

- **Web dashboards and front-end applications**

  Commonly used with JavaScript MQTT libraries such as `mqtt.js`.

For backend services and device connections where TCP is available, MQTT over TCP/TLS is generally recommended for better performance.

## Performance Considerations

Compared to MQTT over TCP/TLS, MQTT over WebSocket introduces:

- Additional framing overhead
- Slightly higher latency
- Marginally lower throughput

These differences are usually negligible for interactive web applications but may be significant for high-throughput or resource-constrained device scenarios.

## Summary

EMQX supports MQTT over WebSocket by providing a WebSocket listener that upgrades HTTP connections and transports standard MQTT packets inside WebSocket frames.

WebSocket affects only the transport layer. All MQTT features and semantics remain identical to native TCP or TLS connections, making MQTT over WebSocket ideal for browser-based clients and restricted network environments.