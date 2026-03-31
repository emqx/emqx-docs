# Rate Limiter Configuration

Limiter is a new feature introduced in EMQX 5.0. It is a mechanism to restrict the number of messages that a client or topic can publish or subscribe to in a specified time. For more information on the Limiter and how it works, see [Rate Limit](../rate-limit/rate-limit.md).

## Listener-Level Limiters

Limiters can operate at the listener level. EMQX uses the following types of limiters to specify the rate limits:

| Type           | Dashboard UI                                      | Description                                                  | Post-Overload Behavior          |
| :------------- | ------------------------------------------------- | :----------------------------------------------------------- | :------------------------------ |
| bytes_rate     | Max Message Publishing Traffic (Per Client)       | The size of messages in bytes published per second by a single client. | Pause receiving client messages |
| bytes_burst    | Max Message Publishing Traffic Burst (Per Client) | Number of bytes that can be sent in a burst by a single client, based on the regular `Data Publishing Rate`. | Pause receiving client messages |
| messages_rate  | Max Message Publishing Rate (Per Client)          | The number of messages published per second by a single client. | Pause receiving client messages |
| messages_burst | Max Message Publishing Burst (Per Client)         | Number of messages that can be sent in a burst by a single client, on top of regular `Messages Publish Rate`. | Pause receiving client messages |
| max_conn_rate  | Max Connection Rate (Listener)                    | The number of connections per second for the current listener. | Pause receiving new connections |
| max_conn_burst | Max Connection Burst (Listener)                   | The maximum number of connections that the listener can accept in bursts. | Pause receiving new connections |

For delivery rate limiters, which also operate at the listener level but have different overload behavior, see [Delivery Rate Limiters](#delivery-rate-limiters).

For example, to set a limiter for the default TCP listener, you can use the configuration below:

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_conn_rate = "1000/s"
  max_conn_burst = "10000/60m"
  messages_rate = "1000/s"
  messages_burst = "10000/60m"
  bytes_rate = "1MB/s"
  bytes_burst = "100MB/60m"
}
```

This configuration implies:

- The maximum rate of connection establishment on the listener is 1000 per second.
- The listener can accept a maximum of 10,000 connections within 60 minutes.
- The maximum publishing rate for messages is 1000 per second per client.
- The listener allows a burst of up to 10,000 messages within a short period every 60 minutes.
- The maximum publishing rate for data is 1MB per second per client.
- The listener allows a burst of up to 100MB within a short period every 60 minutes.

## Node-Level Limiters

Limiters can also operate at the node level, limiting the speed of individual client connections to each EMQX node and the rate at which messages or data are published to the node. EMQX nodes use the following types of limiters to specify rate limits:

| Type           | Dashboard UI             | Description                                                  | Post-Overload Behavior                                       |
| -------------- | ------------------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| bytes_rate     | Data Publish Rate        | The amount of data (in bytes) published by a single client to each EMQX node. | When the limit is reached, EMQX will drop QoS 0 messages and reject QoS 1 and QoS 2 messages with a "Quota Exceeded" error (0x97). |
| bytes_burst    | Data Publish Burst       | The burst amount of data allowed per client, based on the regular `data publish rate`. | When the limit is reached, EMQX will drop QoS 0 messages and reject QoS 1 and QoS 2 messages with a "Quota Exceeded" error (0x97). |
| messages_rate  | Message Publish Rate     | The rate at which a single client publishes messages to each EMQX node. | When the limit is reached, EMQX will drop QoS 0 messages and reject QoS 1 and QoS 2 messages with a "Quota Exceeded" error (0x97). |
| messages_burst | Message Publish Burst    | The number of messages allowed to be published per node in bursts, based on the regular `message publishing rate`. | When the limit is reached, EMQX will drop QoS 0 messages and reject QoS 1 and QoS 2 messages with a "Quota Exceeded" error (0x97). |
| max_conn_rate  | Maximum Connection Rate  | The rate at which new connections are accepted per node.     | When the limit is reached, EMQX will pause processing connections in the Accept queue, delaying or rejecting new connections. |
| max_conn_burst | Maximum Connection Burst | The maximum number of connections that a node can accept in bursts. | Pause receiving new connections                              |

 For example, to set a limiter to an EMQX node, you can configure the following in `emqx.conf`:

```bash
mqtt.limiter {
  max_conn_rate = "1000/s"
  max_conn_burst = "10000/60m"
  messages_rate = "500/10s"
  messages_burst = "10000/60m"
  bytes_rate = "500KB/s"
  bytes_burst = "100MB/60m"
}
```

Zone-level limiters can be embedded in the `zone` section as follows:

```bash
zones.my_zone.mqtt {
  limiter {...}
}
```

- The node can receive a maximum of 500 messages every 10 seconds, and any excess will be dropped/rejected.
- The node allows a burst of up to 10,000 messages within a short period every 60 minutes.
- The node can receive a maximum of 500MB of data every 10 seconds, and any excess will be dropped/rejected.
- The node allows a burst of up to 100MB within a short period every 60 minutes.

## Delivery Rate Limiters

In addition to the publish-side limiters above, EMQX supports subscriber-side delivery rate limiting. These limiters control how fast EMQX delivers messages to a subscribing client, regardless of which clients published those messages.

| Type                    | Dashboard UI                                    | Description                                                  | Post-Overload Behavior                                       |
| ----------------------- | ----------------------------------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| delivery_messages_rate  | Max Message Delivery Rate (Per Client)          | The maximum rate at which messages are delivered to a single subscriber per node. | QoS 0 messages are dropped. QoS 1 and QoS 2 messages are queued internally and retried after a delay derived from the limiter configuration. |
| delivery_messages_burst | Max Message Delivery Burst (Per Client)         | Burst allowance on top of `delivery_messages_rate`.          | Same as above.                                               |
| delivery_bytes_rate     | Max Message Delivery Traffic (Per Client)       | The maximum rate (in bytes) at which data is delivered to a single subscriber per node. | QoS 0 messages are dropped. QoS 1 and QoS 2 messages are queued internally and retried after a delay derived from the limiter configuration. |
| delivery_bytes_burst    | Max Message Delivery Traffic Burst (Per Client) | Burst allowance on top of `delivery_bytes_rate`.             | Same as above.                                               |

Unlike publish-side limiters, delivery limiters are channel-only: they apply per client connection and are not shared across zone or listener groups.

:::tip
Delivery rate limiters are only supported for memory sessions (`durable_sessions.enable = false`). They have no effect when durable sessions are enabled.
:::

You can set delivery rate limits for each listener on the **Management** -> **Listeners** page in the Dashboard.

Alternatively, you can configure them through the configuration file. For example, to set delivery rate limits for the default TCP listener, configure it in the `emqx.conf` file as follows:

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  delivery_messages_rate = "100/s"
  delivery_messages_burst = "500/10s"
  delivery_bytes_rate = "1MB/s"
  delivery_bytes_burst = "10MB/10s"
}
```

If unspecified, the default values are `infinity`, maintaining backward compatibility.

::: tip

EMQX offers more configuration items to better serve customized needs. For details, see the [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

:::
