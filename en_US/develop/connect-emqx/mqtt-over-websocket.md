# MQTT over WebSocket

EMQX Enterprise natively supports MQTT over WebSocket through the built-in WebSocket listener. This allows MQTT clients to communicate with the broker over WebSocket instead of raw TCP or TLS connections.

MQTT over WebSocket is functionally identical to MQTT over TCP/TLS. The only difference is the transport layer: MQTT packets are encapsulated inside WebSocket frames and transmitted over HTTP or HTTPS.

Clients using MQTT over WebSocket can interoperate seamlessly with other MQTT clients connected to the same broker or cluster over TCP, TLS, or QUIC. Since the MQTT protocol remains unchanged, clients connected over different transports share the same topic namespace and routing behavior.

This capability is particularly useful in environments where direct TCP connections are unavailable or restricted, such as web browsers and certain enterprise networks.

## Protocol Stack

When using MQTT over WebSocket, the protocol stack is:

```
MQTT
WebSocket
HTTP / HTTPS
TCP
IP
```

The MQTT protocol itself remains unchanged. All MQTT control packets (CONNECT, PUBLISH, SUBSCRIBE, etc.) are transmitted as-is within WebSocket frames.

## Features

MQTT over WebSocket in EMQX Enterprise provides:

- Full compatibility with MQTT v3.1, v3.1.1, and v5.0
- Support for QoS 0, 1, and 2
- Retained messages and Will messages
- Persistent sessions and offline messages
- Shared subscriptions
- Authentication and authorization mechanisms (Username/Password, JWT, OAuth, etc.)
- TLS encryption via WSS

All MQTT semantics and broker-side processing remain identical to standard TCP or TLS connections.

## Configure WebSocket Listeners

MQTT over WebSocket requires a WebSocket listener to be enabled.

EMQX Enterprise provides both:

- WebSocket listener (WS)
- Secure WebSocket listener (WSS)

You can configure these listeners via:

- EMQX Dashboard: **Management** -> **Listeners**
- Configuration file
- REST API

For detailed configuration instructions, see:

- [Configure WebSocket Listener](../../guides/configuration/listener.md#configure-websocket-listener)
- [Configure Secure WebSocket Listener](../../guides/configuration/listener.md#configure-secure-websocket-listener)

## Get Started

To use MQTT over WebSocket:

1. Enable a WebSocket (WS) or Secure WebSocket (WSS) listener.
2. Connect the client to the corresponding endpoint using `ws://` or `wss://`.
3. Use an MQTT client library that supports MQTT over WebSocket.
4. Configure the client to use WebSocket transport.

From the client perspective, MQTT behavior remains unchanged. The client sends standard MQTT control packets, which are transparently transported inside WebSocket frames.

For browser-based applications, MQTT over WebSocket is commonly used together with JavaScript MQTT client libraries.

For step-by-step examples, see [Connect via JavaScript SDK](./javascript.md).

### Example: Browser Client

The following example demonstrates how a browser client can publish and subscribe to MQTT topics over a secure WebSocket connection:

```javascript
import mqtt from "mqtt";

const client = mqtt.connect("wss://broker.example.com:8084/mqtt", {
  clientId: "web-client-1",
  username: "username",
  password: "password"
});

client.on("connect", () => {
  console.log("Connected over WebSockets");

  client.subscribe("test/topic", () => {
    client.publish("test/topic", "Hello from browser");
  });
});

client.on("message", (topic, message) => {
  console.log(topic, message.toString());
});
```

Publishing and subscribing over WebSockets works exactly the same as over MQTT/TCP or MQTT/TLS.

## Typical Use Cases

MQTT over WebSockets is recommended for:

- Browser-based applications
- Web dashboards and front-end systems
- Environments where only ports 80 or 443 are allowed
- Enterprise networks with strict firewall or proxy policies

For backend services and device connections where TCP is available, MQTT over TCP/TLS is generally preferred for optimal performance.

## Performance Considerations

Compared to MQTT over TCP/TLS, MQTT over WebSocket introduces:

- Additional HTTP and WebSocket framing overhead
- Slightly increased latency
- Marginally reduced throughput

These differences are typically negligible for browser and web applications but may be relevant in high-throughput or latency-sensitive scenarios.
