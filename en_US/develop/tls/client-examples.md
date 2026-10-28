---
description: "Configure MQTT client trust, hostname verification, and client certificates, then verify TLS connections and message delivery."
---

# Configure TLS in an MQTT Client

Use the configuration checklist below to prepare your MQTT client's TLS settings, then follow a language-specific SDK example and verify certificate trust and message delivery. Start with [Enable TLS](enable.md) if the broker endpoint is not ready.

## Connection Settings

Configure the following settings in your MQTT client. Use the One-Way TLS column when the listener does not require a client certificate, or the Mutual TLS column when it does. With one-way TLS, only the client verifies the server's certificate. With mutual TLS, EMQX also verifies the client's certificate, so the client must provide its own certificate and private key. The exact option names depend on your client library.

| Client Setting | One-Way TLS | Mutual TLS |
| --- | --- | --- |
| Transport and port | MQTT over TLS, commonly `8883` | Same |
| Hostname | A DNS name or IP listed in the server certificate's SAN | Same |
| Trusted CA | CA trust bundle that validates the server chain | Same |
| Verify certificate and hostname | Enabled | Enabled |
| Client certificate and private key | Not required by TLS | Required when EMQX enforces client certificates |
| MQTT username/password | Supply when configured in EMQX | Still supply when MQTT authentication requires them |

For browser applications, use [WSS](../connect-emqx/mqtt-over-websocket.md). Browser trust comes from the browser/operating system; JavaScript cannot normally supply arbitrary CA files or client private keys as Node.js can.

## Language Examples

Choose an example for your language and apply the settings from the checklist above using the SDK's TLS options.

| Language | Start Here | TLS Setup |
| --- | --- | --- |
| Python | [Paho Python](../connect-emqx/python.md) | Set `tls_set()` before `connect()` and keep hostname verification enabled. |
| JavaScript / Node.js | [MQTT.js](../connect-emqx/javascript.md) | Use `mqtts://`, CA trust, and optional `cert`/`key`; do not disable certificate rejection. |
| Java | [Paho Java](../connect-emqx/java.md) | Configure the SSL socket factory/trust store for the selected Paho package. |
| Go | [Paho Go](../connect-emqx/go.md) | Configure `tls.Config` with trusted roots and optional client identity. |
| C | [Paho C](../connect-emqx/c.md) | Build TLS support and set the client library's SSL options. |

For additional languages and platforms, including Android, C#, ESP32, ESP8266, Electron, Flutter, PHP, Qt, Spring Boot, Vue.js, Swift, and iOS, see the [SDK and platform directory](../../guides/network/mqtt-client-tls.md). Additional [Go](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Go) and [Java](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Java) projects are available in the same example repository. Adapt credentials and dependency versions before using an external project.

## Verify Both Trust and Delivery

1. Connect with the expected hostname and CA bundle.
2. Subscribe to a test topic and wait for subscription acceptance.
3. Publish a distinct payload and confirm the subscriber receives it.
4. Repeat with an untrusted CA or mismatched hostname; the TLS connection must fail.
5. If mutual TLS is required, repeat without a client certificate; the connection must fail.

Use the [MQTTX TLS commands](enable.md#verify-a-message-exchange) to isolate broker configuration from application code. An application that connects only after certificate verification is disabled has not passed this check.
