# MQTT Configuration

[MQTT](https://mqtt.org/) is a standard messaging protocol for the Internet of Things (IoT). It is designed as an extremely lightweight publish/subscribe messaging transport that is ideal for connecting remote devices with a small code footprint and minimal network bandwidth. 

EMQX is 100% MQTT 5.0 and 3.x compliant. This section introduces the basic configuration items for MQTT-related features, covering topics like basic MQTT settings, subscription settings, session settings, force shutdown settings, and forced garbage collection settings.

## Basic MQTT Configurations

This section will introduce the configuration settings that determine how the MQTT protocol will behave in terms of packet size, client ID length, topic levels, quality of service (QoS), topic alias, and retention. 

:::tip

You can also find the corresponding configuration items in the EMQX Dashboard (**Management** -> **MQTT Settings** -> **General**). Once you have configured these items with the Dashboard, your settings will override the same configuration items in config files.
If you want to configure MQTT from config files, it is recommended to use `base.hocon` instead of `emqx.conf`.
This is because if the configuration is set in emqx.conf, any changes made through the Dashboard will only be temporary and will be lost when EMQX restarts.

:::

**Example Code:**

```bash
mqtt {
  max_packet_size = 1MB
  max_clientid_len = 65535
  max_topic_levels = 128
  max_qos_allowed = 2
  max_topic_alias = 65535
  retain_available = true
}  
```

Where, 

| **Configuration Items** | Dashboard UI         | **Description**                                              | **Default value** | **Optional Values** |
| ----------------------- | -------------------- | ------------------------------------------------------------ | ----------------- | ------------------- |
| `max_packet_size`       | Max Packet Size      | MQTT packets are used to send messages between MQTT clients and EMQX.<br /><br /> This sets the maximum MQTT packet size allowed. | `1MB`             |                     |
| `max_clientid_len`      | Max Client ID Length | This sets the maximum length of an MQTT client ID.<br /><br />It can help to prevent clients from using excessively long client IDs that could cause issues. | `65535`           | `23` - `65535`      |
| `max_topic_levels`      | Max Topic Levels     | MQTT topics are used to organize and categorize messages. <br /><br />This sets the maximum number of levels allowed in an MQTT topic. | `128`             | `1` - `35`          |
| `max_qos_allowed`       | Max QoS              | QoS levels determine the level of reliability and delivery assurance for messages.<br /><br /> This sets maximum quality of service (QoS) level that is allowed for MQTT messages. |                   |                     |
| `max_topic_alias`       | Max Topic Alias      | Topic aliases are a way to reduce the size of MQTT packets by using a shorter alias instead of the full topic name.<br /><br /> This sets the maximum number of topic aliases that can be used in an MQTT session. | `65535`           | `1` - `65535`       |
| `retain_available`      | Retain Available     | Retained messages are used to store the last message published to a topic, so that new subscribers to the topic can receive the most recent message.<br /><br /> This sets whether to enable retained messages feature in MQTT. | `true`            | `true`, `false`     |

## Subscription Settings

In EMQX, subscription refers to the process of a client subscribing to a topic on EMQX. When a client subscribes to a topic, it indicates that it wants to receive messages published to that topic.

This section introduces how to configure shared, wildcard, and exclusive subscriptions. 

:::tip

You can also find the corresponding configuration items in the EMQX Dashboard (**Management** -> **MQTT Settings** -> **General**). Once you have configured these items with the Dashboard, your settings will override the same configuration items in config files.
If you want to configure MQTT from config files, it is recommended to use `base.hocon` instead of `emqx.conf`.
This is because if the configuration is set in emqx.conf, any changes made through the Dashboard will only be temporary and will be lost when EMQX restarts.

:::

**Example code:** <!--code to be reviewed-->

```bash
mqtt {
	wildcard_subscription = true
  exclusive_subscription = false
  shared_subscription = true
  shared_subscription_strategy  =  round_robin
}
```

Where, 

| **Configuration Items**        | Dashboard UI                    | **Description**                                              | **Default value** | Optional Values                                              |
| ------------------------------ | ------------------------------- | ------------------------------------------------------------ | ----------------- | ------------------------------------------------------------ |
| `wildcard_subscription`        | Wildcard Subscription Available | Wildcard subscriptions allow MQTT clients to subscribe to multiple topics using a single subscription, using wildcards such as `+` and `#`. <br /><br />This sets whether to enable wildcard subscription. | `true`            | `true`, `false`                                              |
| `exclusive_subscription`       | Exclusive Subscription          | Exclusive subscriptions allow only one MQTT client to subscribe to a topic at a time.<br /><br />This sets whether to enable exclusive subscriptions. | `true`            | `true`, `false`                                              |
| `shared_subscription`          | Shared Subscription Available   | Shared subscriptions allow multiple MQTT clients to share a subscription to a topic. <br /><br />This sets whether to enable shared subscriptions in MQTT. | `true`            | `true`, `false`                                              |
| `shared_subscription_strategy` |                                 | This setting defines the strategy for distributing messages among MQTT clients that share a subscription.<br /><br />Needed only `shared_subscription` is set to `true`. | `round_robin`     | - `random` (Dispatch the message to a random selected subscriber) <br /><br />- `round_robin` (Select the subscribers in a round-robin manner) <br /><br />-  `sticky` (Always use the last selected subscriber to dispatch, until the subscriber disconnects.)<br /><br />- `hash` (Select the subscribers by the hash of `clientIds`)<br /> |

## Delayed Publish Settings

The Delayed Publish feature allows clients to delay the publishing of a message to a topic for a specified amount of time. This feature is useful for scenarios where messages need to be published at specific times or when a certain condition is met.

This section introduces how to enable delayed publishing and how to set the maximum number of delayed messages allowed:

**Example code:**

```bash
delay {
  delayed_publish_enabled = true
  max_delayed_messages = 0
}
```

Where, 

- `delayed_publish_enabled` sets whether to enable the Delayed Publish feature in EMQX; default value: `true`, optional values: `true`, `false`.
- `max_delayed_messages` sets the maximum number of delayed messages allowed; default value: `0`.

## Keep Alive Settings

The Keep Alive is a two-byte integer, a time interval measured in seconds. It is a mechanism to ensure that an MQTT client and EMQX connection remain active even if no data is transmitted. When an MQTT client establishes a connection with EMQX, setting a non-zero value in the Keep Alive variable header field of the CONNECT packet can enable the Keep Alive mechanism between both parties. For details about how Keep Alive works, see [What is the MQTT Keep Alive parameter for?](https://www.emqx.com/en/blog/mqtt-keep-alive).

According to the MQTT 5.0 protocol, for clients with Keep Alive enabled, if the server does not receive an MQTT Control Packet from the client within 1.5 times the Keep Alive duration, it must close the network connection with the client. Therefore, EMQX introduces a configuration option `keepalive_multiplier` to periodically check clients' Keep Alive timeout status. The default value for `keepalive_multiplier` is `1.5`:

```bash
keepalive_multiplier = 1.5
```

The timeout calculation formula is as follows:
$$
\text{Keep Alive} \times \text{keepalive\_multiplier}
$$

### Dynamic Keepalive Adjustment

In scenarios such as vehicle networking (T-Box) and mobile IoT, MQTT clients need to switch between an **active state** (frequent communication) and a **sleep state** (low-power idle). A single fixed keepalive value cannot satisfy both needs simultaneously:

- A short keepalive keeps disconnection detection fast during active use but causes excessive heartbeat traffic and battery drain when the device is parked or idle.
- A long keepalive reduces traffic during sleep but delays disconnection detection during active use.

EMQX supports dynamic, per-client keepalive adjustment through a set of `$SETOPTS/` system topics. A client can publish to these topics to update its own broker-side keepalive tolerance, or a privileged backend service can update multiple clients at once, all without disconnecting or renegotiating the MQTT connection. The adjustment is applied in-memory to the active session only and is not persisted. The client reverts to its originally negotiated keepalive value after a broker restart.

#### Single-Client Update: `$SETOPTS/mqtt/keepalive`

A client publishes to this topic to update its own broker-side keepalive timeout. EMQX derives the client ID from the publishing session automatically.

**Payload:** A string representation of a non-negative integer, in seconds.

```text
300
```

**Valid range:** `0`–`65535` seconds. `0` disables the keepalive check for that session. Values above `65535` are clamped to `65535`. If `mqtt.server_keepalive` is configured for the client's zone, the effective value is also clamped to that maximum.

**Example use case:** When a vehicle enters a parked state, the T-Box client publishes `300` to `$SETOPTS/mqtt/keepalive`. EMQX extends the keepalive tolerance to 300 s (450 s effective idle timeout with the default `1.5×` multiplier), reducing heartbeat frequency while keeping the MQTT connection alive for remote command delivery.

#### Batch Update: `$SETOPTS/mqtt/keepalive-bulk`

A backend service publishes to this topic to update keepalive for multiple clients in a single message.

**Payload:** A JSON array. Each element must contain:

| Field | Type | Required | Description |
|---|---|---|---|
| `clientid` | String | Yes | Target MQTT client identifier |
| `keepalive` | Integer | Yes | New keepalive interval in seconds (0–65535) |

```json
[
  { "clientid": "tbox-001", "keepalive": 300 },
  { "clientid": "tbox-002", "keepalive": 60 }
]
```

Batch updates are processed asynchronously and are cluster-aware: EMQX locates the node hosting each target client and applies the update via inter-node RPC. If more than 10 batch requests are queued internally, additional requests are dropped and a warning is logged.

#### Access Control

The two topics are intentionally separate to support fine-grained ACL:

- Allow all clients to publish to `$SETOPTS/mqtt/keepalive` so each device can self-adjust its keepalive.
- Restrict `$SETOPTS/mqtt/keepalive-bulk` to trusted backend services only.

Messages published to either topic are intercepted and consumed by EMQX before routing. They are never delivered to subscribers.

## Session Settings

In MQTT, a session refers to the connection between a client and a broker. As in EMQX, when a client connects to EMQX, it establishes a session that allows it to subscribe to topics and receive messages, as well as publish messages to EMQX.

This section introduces how to configure sessions.

**Example code:**

```bash
mqtt {
    max_subscriptions = infinity
    upgrade_qos = false
    max_inflight = 32
    retry_interval = 30s
    max_awaiting_rel = 100
    await_rel_timeout = 300s
    session_expiry_interval = 2h
    max_mqueue_len = 1000
    mqueue_priorities = disabled
    mqueue_default_priority = lowest
    mqueue_store_qos0 = true
    force_shutdown {
      max_mailbox_size = 1000
      max_heap_size = 32MB
    }
    force_gc {
      count  =  16000
      bytes  =  16MB
    }
  }
```

Where, 

| **Configuration Item**            | Dashboard UI                | **Description**                                              | **Default Value**                                            | **Optional Values**                 |
| --------------------------------- | --------------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ----------------------------------- |
| `max_subscriptions`               | Max Subscriptions           | This sets the maximum number of subscriptions that the client is allowed to have | `infinity`                                                   | `1` - `infinity`                    |
| `upgrade_qos`                     | Upgrade QoS                 | This sets whether the client is allowed to upgrade the QoS (Quality of Service) level of a message after it has been published. | `false` (disabled)                                           | `true`, `false`                     |
| `max_inflight`                    | Max Inflight                | This sets the maximum number of QoS 1 and QoS 2 messages that can be in flight (i.e., sent but not yet acknowledged) at any given time | `32`                                                         | `1` - `65535`                       |
| `retry_interval`                  | Retry Interval              | This sets the interval at which the client should retry sending a QoS 1 or QoS 2 message. | `30s`<br />unit: s                                           | --                                  |
| `max_awaiting_rel`                | Max Awaiting PUBREL         | This sets the pending QoS 2 messages in each session until either `PUBREL` is received or timed out. After reaching this limit, new QoS 2 `PUBLISH` requests will be rejected with error code `147(0x93)`.<br />In MQTT, `PUBREL` is a control packet used in the message flow for QoS  2, which provides guaranteed message delivery. | `100`                                                        | `1` - `infinity`                    |
| `await_rel_timeout`               | Max Awaiting PUBREL TIMEOUT | This sets the amount of time to wait for a release of a QoS 2 message before receiving `PUBREL`.  After reaching this limit, EMQX will release the packet ID and also generate a warning level log. <br />Note:  ﻿EMQX will forwarding of the received QoS 2 message whether it has received the `PUBREL`﻿ or not. | `300s`<br />unit: s                                          | --                                  |
| `session_expiry_interval`         | Session Expiry Interval     | This sets the amount of time that a session can be idle before it is automatically closed. Note: For non-MQTT 5.0 clients only. | `2h`                                                         |                                     |
| `max_mqueue_len`                  | Max Message Queue Length    | This sets the maximum allowed queue length when persistent clients are disconnected or inflight window is full. | `1000`                                                       | `0` - `infinity`                    |
| `mqueue_priorities`               | Topic Priorities            | This sets the topic priorities, the configuration here will override that defined in `mqueue_default_priority`. | `disabled` <br />The session uses the priority set by `mqueue_default_priority`. | `disabled`<br />or<br />`1` - `255` |
| `mqueue_default_priority`         | Default Topic Priorities    | This sets the default topic priority.                        | `lowest`                                                     | `highest`， `lowest`                |
| `mqueue_store_qos0`               | Store QoS 0 Message         | This sets whether to store QoS 0 message in the message queue when the connection is down but the session remains. | `true`                                                       | `true`, `false`                     |
| `force_shutdown`                  | Enable Force Shutdown       | This sets whether to enable the force shutdown feature. The client connection process will be forcibly shut down if the mailbox queue length (`max_mailbox_size`) or heap size (`max_heap_size`) reaches the specified value. | `true`                                                       | `true`, `false`                     |
| `force_shutdown.max_mailbox_size` | Max Mailbox Size            | This sets the maximum mailbox queue length to trigger a forced shutdown. | `1000`                                                       | `1` - `infinity`                    |
| `force_shutdown.max_heap_size`    | Max Heap Size               | This sets the maximum heap size to trigger a forced shutdown. | `32MB`                                                       | --                                  |
| `force_gc`                        | --                          | This sets whether to enable forced garbage collection if the specified message number (`count`) or byte received (`bytes`)  is reached: | `true`                                                       | `true`, `false`                     |
| `force_gc.count`                  | --                          | This sets the received message number that will trigger the forced garbage collection. | `16000`                                                      | `0` - `infinity`                    |
| `force_gc.bytes`                  | --                          | This sets the received byte number that will trigger the forced garbage collection. | `16MB`<br />Unit: `MB`                                       | --                                  |

:::tip

To configure MQTT settings via Dashboard,  click **Management** -> **MQTT Settings** on the left navigation menu of the Dashboard. Once you have configured these items with the Dashboard, your settings will override the same configuration items in config files.
If you want to configure MQTT from config files, it is recommended to use `base.hocon` instead of `emqx.conf`.
This is because if the configuration is set in emqx.conf, any changes made through the Dashboard will only be temporary and will be lost when EMQX restarts.

:::

::: tip

EMQX offers more configuration items to better serve customized needs. For details, see the [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

:::
