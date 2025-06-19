# Rate Limit

EMQX allows for specifying limits on connection speed and messaging speed, using a backpressure scheme that avoids system overload at the entry point and guarantees system stability with predictable throughput.

## Listener-Level Limiters

Limiters can operate at the listener level. EMQX uses the following types of limiters to specify the rate limits:

| Type           | Dashboard UI                                      | Description                                                  | Post-Overload Behavior          |
| :------------- | ------------------------------------------------- | :----------------------------------------------------------- | :------------------------------ |
| bytes_rate     | Max Message Publishing Traffic (Per Client)       | The size of messages in bytes published per second by a single client | Pause receiving client messages |
| bytes_burst    | Max Message Publishing Traffic Burst (Per Client) | Number of bytes that can be sent in a burst by a single client, based on the regular `Data Publishing Rate`. | Pause receiving client messages |
| messages_rate  | Max Message Publishing Rate (Per Client)          | The number of messages published per second by a single client | Pause receiving client messages |
| messages_burst | Max Message Publishing Burst (Per Client)         | Number of messages that can be sent in a burst by a single client, on top of regular `Messages Publish Rate` | Pause receiving client messages |
| max_conn_rate  | Max Connection Rate (Listener)                    | The number of connections per second for the current listener | Pause receiving new connections |
| max_conn_rate  | Max Connection Burst (Listener)                   | The maximum number of connections that the listener can accept in bursts | Pause receiving new connections |

### Configure Listener-Level Limiters

You can set rate limits for each listener on the **Management** -> **Listeners** page in the Dashboard.

Alternatively, you can configure them through the configuration file. For example, to set limiters for the default TCP listener, configure it in the `emqx.conf` file as follows:

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
| bytes_rate     | Data Publish Rate        | The amount of data (in bytes) sent by a single client to each EMQX node | When the limit is reached, EMQX will drop QoS 0 messages and reject QoS 1 and QoS 2 messages with a "Quota Exceeded" error (0x97). |
| bytes_burst    | Data Publish Burst       | The burst amount of data allowed per client, based on the regular `data publish rate` | When the limit is reached, EMQX will drop QoS 0 messages and reject QoS 1 and QoS 2 messages with a "Quota Exceeded" error (0x97). |
| messages_rate  | Message Publish Rate     | The rate at which a single client sends messages to each EMQX node | When the limit is reached, EMQX will drop QoS 0 messages and reject QoS 1 and QoS 2 messages with a "Quota Exceeded" error (0x97). |
| messages_burst | Message Publish Burst    | The number of messages allowed to be sent per node in bursts, based on the regular `message publishing rate` | When the limit is reached, EMQX will drop QoS 0 messages and reject QoS 1 and QoS 2 messages with a "Quota Exceeded" error (0x97). |
| max_conn_rate  | Maximum Connection Rate  | The rate at which new connections are accepted per node      | When the limit is reached, EMQX will pause processing connections in the Accept queue, delaying or rejecting new connections. |
| max_conn_burst | Maximum Connection Burst | The maximum number of connections that a node can accept in bursts | Pause receiving new connections                              |

### Configuring Node-Level Limiters

You can configure rate limits for each node on the **Management** -> **MQTT Configuration** page in the Dashboard.

Alternatively, you can configure them through the configuration file. For example, you can configure the following in `emqx.conf`:

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

## Rate Unit

### Time Unit

The supported time unit in the rate value could be:

- **s** : Second
- **m** : Minute
- **h** : Hour
- **d** : Day

The time unit also can be an interval value, like `1000/10s` means setting the limit to 1000 per every 10 seconds.

### Size Unit

The supported size unit in the rate value could be:

- **KB** : Kilobyte
- **MB** : Megabyte
- **GB** : Gigabyte

