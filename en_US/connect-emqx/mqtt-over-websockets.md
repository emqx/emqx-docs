# MQTT over WebSockets

EMQX supports MQTT over WebSockets, enabling MQTT clients to communicate with the broker over WebSockets connections instead of raw TCP or TLS.

MQTT over WebSockets is functionally identical to MQTT over TCP/TLS. The only difference lies in the transport layer: MQTT packets are encapsulated inside WebSockets frames and transmitted over HTTP or HTTPS.

This transport is primarily intended for environments where direct TCP connections are unavailable or restricted, such as web browsers and certain enterprise networks.

## Protocol Stack

When using MQTT over WebSockets, the protocol stack is as follows:

```
MQTT
WebSockets
HTTP / HTTPS
TCP
IP
```

The MQTT protocol itself remains unchanged. All MQTT control packets (CONNECT, PUBLISH, SUBSCRIBE, etc.) are transmitted as-is within WebSockets frames.

## How EMQX Implements MQTT over WebSockets

EMQX provides MQTT over WebSockets support through a WebSockets listener.

The connection workflow is:

1. A client initiates an HTTP or HTTPS request to the EMQX WebSockets endpoint.
2. The request includes an `Upgrade: websocket` header.
3. EMQX accepts the upgrade and establishes a WebSockets connection.
4. MQTT packets are exchanged within WebSockets frames.
5. The packets are processed by the same MQTT core engine used for TCP/TLS connections.

Once the WebSockets connection is established, EMQX handles the client session exactly the same way as any other MQTT connection.

## WebSockets and Secure WebSockets (WS / WSS)

EMQX supports both:

- **WS** (WebSockets over HTTP)
- **WSS** (WebSockets over HTTPS)

When using **WSS**, TLS is applied in the same way as:

- HTTPS
- MQTT over TLS (port 8883)

This allows you to reuse existing TLS certificates and security configurations, including mutual TLS authentication if required.

## Typical Use Cases

MQTT over WebSockets is particularly suitable for:

- **Browser-based clients**

  Web browsers do not support raw TCP sockets but provide native WebSockets APIs.

- **Restricted network environments**

  WebSockets traffic over ports 80 or 443 can traverse proxies and firewalls that block custom TCP ports.

- **Web dashboards and front-end applications**

  Commonly used with JavaScript MQTT libraries such as `mqtt.js`.

For backend services and device connections where TCP is available, MQTT over TCP/TLS is generally recommended for better performance.

## Use MQTT over WebSockets

MQTT over WebSockets is typically used by clients that cannot establish direct TCP connections to an MQTT broker, most commonly web browsers.

To use MQTT over WebSockets with EMQX:

1. Ensure that a WebSockets listener is enabled in EMQX Dashboard **Management** -> **Listeners**.
2. Connect the client to the WebSockets endpoint using `ws://` or `wss://`.
3. Use an MQTT client library that supports MQTT over WebSockets.
4. Configure the client to use the WebSockets transport instead of a raw TCP socket.

From the client perspective, MQTT behavior remains unchanged. The client continues to send standard MQTT control packets, which are transparently transported inside WebSockets frames.

For browser-based applications, MQTT over WebSockets is commonly used together with JavaScript MQTT client libraries that rely on the browser’s native WebSockets API.

For step-by-step examples, see [Connect via JavaScript SDK](./javascript.md).

### Example: Using MQTT over WebSockets in a Browser

The following example demonstrates how a web browser can publish and subscribe to MQTT topics over a WebSockets connection using a JavaScript MQTT client library.

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

This example uses a secure WebSockets (wss://) connection. From the client’s perspective, publishing and subscribing works the same way as with MQTT over TCP or TLS.

For step-by-step examples and additional configuration options, see Connect via JavaScript SDK.
