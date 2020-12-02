---
# 标题
title: 指标监控
# 编写日期
date: 2020-02-25 17:15:26
# 作者 Github 名称
author: tigercl
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# Metrics

EMQ X Broker provides users with metrics monitoring functions, allowing users and operation and maintenance personnel to know the current service status based on these metrics. The metrics monitoring function is forcibly enabled. This function has high performance, and users do not need to worry about affecting system performance in high-throughput scenarios.

EMQ X Broker provides users with multiple ways to view metrics and status. Most directly, users can see this data on the Overview page of EMQ X Dashboard.

If it is not convenient to access the Dashboard, they can also obtain these data through HTTP API and system topic messages. For specific operations, see [HTTP API](./http-api.md#endpoint-metrics) and [$SYS system topic](./system-topic.md#).

::: tip
EMQ X Broker provides the [emqx_statsd](https://github.com/emqx/emqx-statsd) plug-in, which is used to output the system's monitoring data to a third-party monitoring system. You can refer to [Prometheus Monitoring Alarm](../tutorial/prometheus.md) for an example.
:::

## Metrics & Stats

EMQ X Broker divides metrics into Metrics and Stats. Metrics usually refer to data that will only increase monotonically, such as the number of sent bytes and the number of sent messages. Metrics currently provided by EMQ X Broker covers the four dimensions of bytes, packets, messages, and events. Stats usually refers to data that appears in pairs, including current values and historical maximums, such as the current number of subscriptions and the historical maximum number of subscription.

### Metrics

### Byte

| Name           | Data Type | Description              |
| -------------- | --------- | ------------------------ |
| bytes.received | Integer   | Number of received bytes |
| bytes.sent     | Integer   | Number of send bytes     |

### Packet

| Name                         | Data Type | Description                                                  |
| ---------------------------- | --------- | ------------------------------------------------------------ |
| packets.received             | Integer   | Number of received packets                                   |
| packets.sent                 | Integer   | Number of sent packets                                       |
| packets.connect.received     | Integer   | Number of received CONNECT packets                           |
| packets.connack.auth_error   | Integer   | Number of sent CONNACK messages with reason codes 0x86 and 0x87 |
| packets.connack.error        | Integer   | Number of sent CONNACK packets where reason code is not 0x00. The value of this indicator is greater than or equal to the value of `packets.connack.auth_error` |
| packets.connack.sent         | Integer   | Number of sent CONNACK packets                               |
| packets.publish.received     | Integer   | Number of received PUBLISH packets                           |
| packets.publish.sent         | Integer   | Number of sent PUBLISH packets                               |
| packets.publish.inuse        | Integer   | Number of received PUBLISH packets with occupied packet identifiers |
| packets.publish.auth_error   | Integer   | Number of received PUBLISH packets that failed the ACL check |
| packets.publish.error        | Integer   | Number of received PUBLISH packets that cannot be published  |
| packets.publish.dropped      | Integer   | Number of PUBLISH packets that were discarded due to the receiving limit |
| packets.puback.received      | Integer   | Number of received PUBACK packets                            |
| packets.puback.sent          | Integer   | Number of sent PUBACK packets                                |
| packets.puback.inuse         | Integer   | Number of received PUBACK messages with occupied identifiers |
| packets.puback.missed        | Integer   | Number of received PUBACK packets with unknown identifiers   |
| packets.pubrec.received      | Integer   | Number of received PUBREC packets                            |
| packets.pubrec.sent          | Integer   | Number of sent PUBREC packets                                |
| packets.pubrec.inuse         | Integer   | Number of received PUBREC messages with occupied identifiers |
| packets.pubrec.missed        | Integer   | Number of received PUBREC packets with unknown identifiers   |
| packets.pubrel.received      | Integer   | Number of received PUBREL packets                            |
| packets.pubrel.sent          | Integer   | Number of sent PUBREL packets                                |
| packets.pubrel.missed        | Integer   | Number of received PUBREL packets with unknown identifiers   |
| packets.pubcomp.received     | Integer   | Number of received PUBCOMP packets                           |
| packets.pubcomp.sent         | Integer   | Number of sent PUBCOMP packets                               |
| packets.pubcomp.inuse        | Integer   | Number of received PUBCOMP messages with occupied identifiers |
| packets.pubcomp.missed       | Integer   | Number of missed PUBCOMP packets                             |
| packets.subscribe.received   | Integer   | Number of received SUBSCRIBE packets                         |
| packets.subscribe.error      | Integer   | Number of received SUBSCRIBE packets with failed subscriptions |
| packets.subscribe.auth_error | Integer   | Number of received SUBACK packets that failed the ACL check  |
| packets.suback.sent          | Integer   | Number of sent SUBACK packets                                |
| packets.unsubscribe.received | Integer   | Number of received UNSUBSCRIBE packets                       |
| packets.unsubscribe.error    | Integer   | Number of received UNSUBSCRIBE packets with failed unsubscriptions |
| packets.unsuback.sent        | Integer   | Number of sent UNSUBACK packets                              |
| packets.pingreq.received     | Integer   | Number of received PINGREQ packets                           |
| packets.pingresp.sent        | Integer   | Number of sent PUBRESP packets                               |
| packets.disconnect.received  | Integer   | Number of received DISCONNECT packets                        |
| packets.disconnect.sent      | Integer   | Number of sent DISCONNECT packets                            |
| packets.auth.received        | Integer   | Number of received AUTH packets                              |
| packets.auth.sent            | Integer   | Number of sent AUTH packets                                  |

### Message (PUBLISH packet)

| Name                            | Data Type | Description                                                  |
| ------------------------------- | --------- | ------------------------------------------------------------ |
| delivery.dropped.too_large      | Integer   | The number of messages that were dropped because the length exceeded the limit when sending |
| delivery.dropped.queue_full     | Integer   | Number of messages with a non-zero QoS that were dropped because the message queue was full when sending |
| delivery.dropped.qos0_msg       | Integer   | Number of messages with QoS of 0 that were dropped because the message queue was full when sending |
| delivery.dropped.expired        | Integer   | Number of messages that were dropped due to message expiration when sending |
| delivery.dropped.no_local       | Integer   | Number of messages that were dropped due to the `No Local` subscription option when sending |
| delivery.dropped                | Integer   | Total number of messages that were dropped when sent         |
| messages.delayed                | Integer   | Number of delay-published messages stored by EMQ X Broker    |
| messages.delivered              | Integer   | Number of messages forwarded to the subscription process internally by EMQ X Broker |
| messages.dropped                | Integer   | Total number of messages dropped by EMQ X Broker before forwarding to the subscription process |
| messages.dropped.expired        | Integer   | Number of messages that were dropped due to message expiration when receiving |
| messages.dropped.no_subscribers | Integer   | Number of messages dropped due to no subscribers             |
| messages.forward                | Integer   | Number of messages forwarded to other nodes                  |
| messages.publish                | Integer   | Number of messages published in addition to system messages  |
| messages.qos0.received          | Integer   | Number of QoS 0 messages received from clients               |
| messages.qos2.received          | Integer   | Number of QoS 1 messages received from clients               |
| messages.qos1.received          | Integer   | Number of QoS 2 messages received from clients               |
| messages.qos0.sent              | Integer   | Number of QoS 0 messages sent to clients                     |
| messages.qos1.sent              | Integer   | Number of QoS 1 messages sent to clients                     |
| messages.qos2.sent              | Integer   | Number of QoS 2 messages sent to clients                     |
| messages.received               | Integer   | Number of messages received from the client, which is equal to the sum of `messages.qos0.received`,` messages.qos1.received`, and `messages.qos2.received` |
| messages.sent                   | Integer   | The number of messages sent to the client, which is equal to the sum of `messages.qos0.sent`,` messages.qos1.sent`, and `messages.qos2.sent` |
| messages.retained               | Integer   | Number of retained messages stored by EMQ X Broker           |
| messages.acked                  | Integer   | Number of acked messages                                     |

### Event

| Name                  | Data Type | Description                                               |
| --------------------- | --------- | --------------------------------------------------------- |
| actions.failure       | Integer   | Number of successful executions of the rule engine action |
| actions.success       | Integer   | Number of failed executions of the rule engine action     |
| rules.matched         | Integer   | Number of matched rules                                   |
| client.auth.anonymous | Integer   | The number of client's final anonymous login              |
| client.connect        | Integer   | `client.connect` hook trigger times                       |
| client.authenticate   | Integer   | `client.authenticate` hook trigger times                  |
| client.connack        | Integer   | `client.connack` hook trigger times                       |
| client.connected      | Integer   | `client.connected` hook trigger times                     |
| client.disconnected   | Integer   | `client.disconnected` hook trigger times                  |
| client.check_acl      | Integer   | `client.check_acl` hook trigger times                     |
| client.subscribe      | Integer   | `client.subscribe` hook trigger times                     |
| client.unsubscribe    | Integer   | `client.unsubscribe` hook trigger times                   |
| session.created       | Integer   | `session.created` hook trigger times                      |
| session.discarded     | Integer   | `session.discarded` hook trigger times                    |
| session.resumed       | Integer   | `session.resumed` hook trigger times                      |
| session.takeovered    | Integer   | `session.takeovered` hook trigger times                   |
| session.terminated    | Integer   | `session.terminated` hook trigger times                   |

## Stats

| Name                       | Data Type | Description                                                  |
| -------------------------- | --------- | ------------------------------------------------------------ |
| connections.count          | Integer   | Current connections                                          |
| connections.max            | Integer   | Historical maximum number of connections                     |
| channels.count             | Integer   | `sessions.count`                                             |
| channels.max               | Integer   | `session.max`                                                |
| sessions.count             | Integer   | Number of current sessions                                   |
| sessions.max               | Integer   | Historical maximum number of sessions                        |
| topics.count               | Integer   | Number of current topics                                     |
| topics.max                 | Integer   | Historical maximum number of topics                          |
| suboptions.count           | Integer   | `subscriptions.count`                                        |
| suboptions.max             | Integer   | `subscriptions.max`                                          |
| subscribers.count          | Integer   | Number of current subscribers                                |
| subscribers.max            | Integer   | Historical maximum number of subscribers                     |
| subscriptions.count        | Integer   | Number of current subscriptions, including shared subscriptions |
| subscriptions.max          | Integer   | Historical maximum number of subscriptions                   |
| subscriptions.shared.count | Integer   | Number of current shared subscriptions                       |
| subscriptions.shared.max   | Integer   | Historical maximum number of shared subscriptions            |
| routes.count               | Integer   | Number of current routes                                     |
| routes.max                 | Integer   | Historical maximum number of routes                          |
| retained.count             | Integer   | Number of currently retained messages                        |
| retained.max               | Integer   | Historical maximum number of retained messages               |


