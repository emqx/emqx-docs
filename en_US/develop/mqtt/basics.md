---
description: "Understand MQTT message flow, connection settings, topics, QoS, sessions, and Keep Alive when developing EMQX client applications."
---

# MQTT Basics

MQTT lets devices exchange messages through a broker without maintaining direct connections to every recipient. Its small protocol overhead and publish/subscribe model suit telemetry, device state, and commands. Your application still chooses the payload format, topic scheme, security settings, and recovery behavior.

## How MQTT Messages Flow

1. A subscriber connects to EMQX and subscribes to `factory/line1/temperature`.
2. A publisher connects and publishes `23.5` to that topic.
3. EMQX checks permissions and routes the publication to the matching subscriber.
4. The subscriber processes the payload. If the publisher must know that processing succeeded, the application sends a separate acknowledgment.

One client can take both roles. Run this exchange with [MQTTX](../clients/test-clients.md).

## MQTT Connection Settings

Configure the broker address and listener port, transport, Client ID, authentication credentials, and protocol version. TCP, TLS, and WebSocket connections need compatible listeners; a WebSocket client cannot use an ordinary TCP port.

Use a stable Client ID when reconnecting to the same session and distinct IDs for concurrently running clients. Configure certificate validation for TLS and provide credentials accepted by the broker's [authentication chain](../../guides/access-control/authn/authn.md). Successful authentication does not automatically authorize every topic.

Keep Alive lets the broker detect an inactive connection. When Keep Alive is nonzero, the client must send a `PINGREQ` if it sends no other MQTT control packets within that interval. See [Keep Alive details](../../get-started/mqtt-advanced.md#keep-alive).

## Topics and Subscription Filters

Use meaningful levels such as `factory/<line>/<device>/temperature`. Topic names are case-sensitive; slashes create levels, not filesystem paths. Do not put credentials or other secrets in topic names.

| Filter | Matches |
| --- | --- |
| `factory/line1/temperature` | That exact topic. |
| `factory/+/temperature` | One intervening level, such as `line1` or `line2`. |
| `factory/#` | `factory` and topics below it. |

Place wildcards only in subscription filters. See [Choose a Filter](wildcard-subscriptions.md#choose-a-filter) before designing filters with empty levels or `$` prefixes.

## Session Persistence and Recovery

In MQTT 5, **Clean Start** decides whether the connection begins with a new session. **Session Expiry Interval** decides how long that session may remain after disconnection. Omitting expiry defaults to zero; setting Clean Start to false does not by itself retain a new session.

For a reconnecting client, set a nonzero expiry interval and keep its Client ID. On reconnect, use Clean Start false and inspect the CONNACK Session Present flag. If it is false, recreate the required subscriptions. Clean Start true discards an existing session for that ID.

In MQTT 3.1.1, Clean Session false requests reuse and retention together; EMQX applies the configured session expiry policy. For survival across broker restarts, follow [durable sessions](../../guides/durability/durability_introduction.md) and preserve the broker's data and node identity.

## Quality of Service (QoS)

| QoS | Wire Exchange | Choose It When |
| --- | --- | --- |
| 0 | PUBLISH | The application only needs the latest sensor reading, can accept occasional message loss, and needs low overhead. |
| 1 | PUBLISH → PUBACK | Delivery retries matter and the application can handle duplicates. |
| 2 | PUBLISH → PUBREC → PUBREL → PUBCOMP | The MQTT exchange must suppress duplicate delivery, with additional protocol state and overhead. |

These exchanges occur separately between publisher and broker and between broker and subscriber. A QoS 2 publication can be delivered at QoS 1 to a subscriber that requested QoS 1. QoS does not prevent expiry, override resource limits, or confirm a database transaction in the receiving application.

When duplicate business operations would be harmful, use application message IDs and idempotent processing in your application. Processing the same message again should have no additional business effect. Read [retransmission](../design/retransmission.md) and [flow control](../design/inflight-window-and-message-queue.md) before relying on ordering or reconnect behavior.

## Related Resources

The [MQTT 5 reference](../../get-started/mqtt-advanced.md) covers request/response, metadata, expiry, and subscription options. For longer tutorials, see EMQX's articles on [getting started](https://www.emqx.com/en/blog/the-easiest-guide-to-getting-started-with-mqtt), [connection parameters](https://www.emqx.com/en/blog/how-to-set-parameters-when-establishing-an-mqtt-connection), and [QoS](https://www.emqx.com/en/blog/introduction-to-mqtt-qos).
