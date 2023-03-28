# MQTT

[MQTT](https://mqtt.org/) is a standard messaging protocol for the Internet of Things (IoT). It is designed as an extremely lightweight publish/subscribe messaging transport that is ideal for connecting remote devices with a small code footprint and minimal network bandwidth. 

EMQX is 100% MQTT 5.0 and 3.x compliant, this section will introduce the basic configuration items for MQTT-related features, covering topics like basic MQTT settings, subscription settings, session settings, force shutdown settings, and forced garbage collection settings.

## Basic MQTT Configurations

This section will introduce the configuration settings that determine how the MQTT protocol will behave in terms of packet size, client ID length, topic levels, quality of service (QoS), topic alias, and retention.

**Example Code:**

```bash
mqtt {
  max_packet_size = 1MB
  max_clientid_len = 65535
  max_topic_levels = 128
  max_qos_allowed = 2
  max_topic_alias = 65535
  retain_available = true
  wildcard_subscription = true
  shared_subscription = true
  shared_subscription_strategy  =  round_robin
  exclusive_subscription = false
}  
```

Where, 

| **Configuration Items**                            | **Description**                                              | **Default value** | **Optinal Values** |
| -------------------------------------------------- | ------------------------------------------------------------ | ----------------- | ------------------ |
| **Max Packet Size <br>(**`max_packet_size`)        | MQTT packets are used to send messages between MQTT clients and EMQX.<br> This sets the maximum MQTT packet size allowed. | `1MB`             |                    |
| **Max Client ID Length** <br/>(`max_clientid_len`) | This sets the maximum length of an MQTT client ID.<br/>It can help to prevent clients from using excessively long client IDs that could cause issues. | `65535`           | `23` ～ `65535`    |
| **Max Topic Levels** <br/>(`max_topic_levels`)     | MQTT topics are used to organize and categorize messages. <br/>This sets the maximum number of levels allowed in an MQTT topic. | `128`             | `1` ～ `65535`     |
| **Max QoS** <br/>(`max_qos_allowed`)               | QoS levels determine the level of reliability and delivery assurance for messages.<br/> This sets maximum quality of service (QoS) level that is allowed for MQTT messages. |                   |                    |
| **Max Topic Alias** <br/>(`max_topic_alias`)       | Topic aliases are a way to reduce the size of MQTT packets by using a shorter alias instead of the full topic name.<br/> This sets the maximum number of topic aliases that can be used in an MQTT session. | `65535`           | `1` ～ `65535`     |
| **Retain Available** <br/>(`retain_available`)     | Retained messages are used to store the last message published to a topic, so that new subscribers to the topic can receive the most recent message.<br/> This sets whether to enable retained messages feature in MQTT. | `true`            | `true`, `false`    |



## Subscription Settings

In EMQX, subscription refers to the process of a client subscribing to a topic on EMQX. When a client subscribes to a topic, it is indicating that it wants to receive messages published to that topic.

This section introduces how to configure shared subscription, wildcard subscription, and exclusive subscription. 

**Example code:** <!--code to be reviewed-->

```bash
mqtt {
	wildcard_subscription = true
  shared_subscription = true
  shared_subscription_strategy  =  round_robin
  exclusive_subscription = false
}
```

Where, 

| **Configuration Items**                                      | **Description**                                              | **Default value** | Optinal Values                                               |
| ------------------------------------------------------------ | ------------------------------------------------------------ | ----------------- | ------------------------------------------------------------ |
| **Wildcard Subscription Available**<br/>(`wildcard_subscription`) | Wildcard subscriptions allow MQTT clients to subscribe to multiple topics using a single subscription, using wildcards such as `+` and `#`. <br/>This sets whether to enable wildcard subscription. | `true`            | `true`, `false`                                              |
| **Exclusive Subscription** <br>(`exclusive_subscription`)    | Exclusive subscriptions allow only one MQTT client to subscribe to a topic at a time.<br/>This sets whether to enable exclusive subscriptions. | `true`            | `true`, `false`                                              |
| **Shared Subscription Available** <br/>(`shared_subscription`) | Shared subscriptions allow multiple MQTT clients to share a subscription to a topic. <br/>This sets whether to enable shared subscriptions in MQTT. | `true`            | `true`, `false`                                              |
| `shared_subscription_strategy`                               | This setting defines the strategy for distributing messages among MQTT clients that share a subscription.<br>Needed only `shared_subscription` is set to `true`. | `round_robin`     | - `random` (Dispatch the message to a random selected subscriber) <br><br/>- `round_robin` (Select the subscribers in a round-robin manner) <br><br/>-  `sticky` (Always use the last selected subscriber to dispatch, until the subscriber disconnects.)<br><br/>- `hash` (Select the subscribers by the hash of `clientIds`)<br/> |

## Delayed Publish Settings

The Delayed Publish feature allows clients to delay the publishing of a message to a topic for a specified amount of time. This feature is useful for scenarios where messages need to be published at specific times or when a certain condition is met.

This section introduces how to enable delayed publishing and how to set the maximum of delayed messages allowed:

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

Keep Alive is the mechanism that ensures that a connection between an MQTT client and EMQX remains active even if no data is transmitted. This is how it works: when an MQTT client creates a connection to EMQX, it can set the Keep Alive variable header field in the connection request protocol packet to a non-zero value. For details about how Keep Alive works, see [What is the MQTT Keep Alive parameter for?](https://www.emqx.com/en/blog/mqtt-keep-alive)

For clients with Keep Alive enabled, you can continue to customize the coefficient EMQX uses to confirm whether the keep alive duration of the client expires.

```bash
keepalive_backoff = 0.75
```

Where, **Keep Alive Backoff** (`keepalive_backoff`) is the coefficient EMQX uses to confirm whether the keep alive duration of the client expires. Default: `0.75`. The calculation formular is as follows:
$$
Keep Alive * Backoff * 2
$$

## Session Settings

In MQTT, a session refers to the connection between a client and a broker. As in EMQX, When a client connects to EMQX, it establishes a session that allows it to subscribe to topics and receive messages, as well as publish messages to EMQX.

This section introduces how to configure sessions.

**Example code:**

```bash
session {
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
      max_message_queue_len = 1000
      max_heap_size = 32MB
    }

    force_gc {
      count  =  16000
      bytes  =  16MB
    }
  }
```

Where, 

| **Configuration Item**                                       | **Description**                                              | **Default Value**                                            | **Optional Values**              |
| ------------------------------------------------------------ | ------------------------------------------------------------ | ------------------------------------------------------------ | -------------------------------- |
| **Max Subscriptions** <br>(`max_subscriptions`)              | This sets the maximum number of subscriptions that the client is allowed to have | `infinity`                                                   | `1` ~ `infinity`                 |
| **Upgrade QoS** <br/>(`upgrade_qos`)                         | This sets whether the client is allowed to upgrade the QoS (Quality of Service) level of a message after it has been published. | `false` (disabled)                                           | `true`, `false`                  |
| **Max Inflight** <br/>(`max_inflight`)                       | This sets the maximum number of QoS 1 and QoS 2 messages that can be in flight (i.e., sent but not yet acknowledged) at any given time | `32`                                                         | `1` ~ `65535`                    |
| **Retry Interval** <br/>(`retry_interval`)                   | This sets the interval at which the client should retry sending a QoS 1 or QoS 2 message. | `30s`<br>unit: s                                             | --                               |
| **Max Awaiting PUBREL** <br/>(`max_awaiting_rel`)            | This sets the pending QoS 2 messages in each session until either `PUBREL` is received or timed out. After reaching this limit, new QoS 2 `PUBLISH` requests will be rejected with error code `147(0x93)`.<br>In MQTT, `PUBREL` is a control packet used in the message flow for QoS  2, which provides guaranteed message delivery. | `100`                                                        | `1` ~ `infinity`                 |
| **Max Awaiting PUBREL TIMEOUT** <br/>(`await_rel_timeout`)   | This sets the amount of time to wait for a release of a QoS 2 message before receiving `PUBREL`.  After reaching this limit, EMQX will release the packet ID and also generate a warning level log. <br>Note:  ﻿EMQX will forwarding of the received QoS 2 message whether it has received the `PUBREL`﻿ or not. | `300s`<br>unit: s                                            | --                               |
| **Session Expiry Interval** <br/>(`session_expiry_interval`) | This sets the amount of time that a session can be idle before it is automatically closed. Note: For non-MQTT 5.0 clients only. | `2h`                                                         |                                  |
| **Max Message Queue Length** <br/>(`max_mqueue_len`)         | This sets the maximum allowed queue length when persistent clients are disconnected or inflight window is full. | `1000`                                                       | `0` ~ `infinity`                 |
| **Topic Priorities**  <br/>(`mqueue_priorities`)             | This sets the topic priorities, the configuration here will override that defined in **Default Topic Priorities** (`mqueue_default_priority`). | `disabled` <br>Then the session uses the priority set by **Default Topic Priorities** (`mqueue_default_priority`). | `disabled`<br>or<br>`1` ～ `255` |
| **Default Topic Priorities** <br/>(`mqueue_default_priority`) | This sets the default topic priority.                        | `lowest`                                                     | `highest`， `lowest`             |
| **Store QoS 0 Message** <br/>(`mqueue_store_qos0`)           | This sets whether to store QoS 0 message in the message queue when the connection is down but the session remains. | `true`                                                       | `true`, `false`                  |
| `force_shutdown` <!--with dashboard configuration item to be added--> | This sets whether to enable the force shutdown feature if the queue length (`max_message_queue_le`) or heap size (`max_heap_size`) reaches the specified value. | `true`                                                       | `true`, `false`                  |
|                                                              | `max_message_queue_le`: This sets the maximum queue length to trigger a forced shutdown. | `1000`                                                       | `1` ~ `infinity`                 |
|                                                              | `max_heap_size`: This sets the maximum heap size to trigger a forced shutdown. | `32MB`                                                       | --                               |
| `force_gc`<!--with dashboard configuration item to be added--> | This sets whether to enable forced garbage collection if the specified message number (`count`) or byte (`bytes`) received is reached: | `true`                                                       | `true`, `false`                  |
|                                                              | `count`: ﻿This sets the received messages number that will trigger the forced garbage collection. | `16000`                                                      | `0` ~ `infinity`                 |
|                                                              | `bytes`: This sets the received byte number that will trigger the forced garbage collection. | `16MB`<br>Unit: `MB`                                         | --                               |