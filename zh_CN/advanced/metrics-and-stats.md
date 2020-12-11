---
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
ref:
---

# 指标监控

EMQ X 为用户提供了指标监控功能，允许用户以及运维人员根据这些指标来了解当前服务状态。指标监控功能强制启用，但此功能拥有很高的性能，用户不必担心影响高吞吐场景下的系统性能。

EMQ X 为用户提供了多种查看指标与状态的手段。最直接的，用户可以在 EMQ X Dashboard 的 Overview 页面看到这些数据。

如果不方便访问 Dashboard，你还可以通过 HTTP API 和系统主题消息来获取这些数据，具体操作方法分别参见 [HTTP API](./http-api.md#endpoint-metrics) 与 [$SYS 系统主题](./system-topic.md#)。

::: tip
EMQ X 提供 [emqx_statsd](https://github.com/emqx/emqx-statsd) 插件，用于将系统的监控数据输出到第三方的监控系统中，使用示例参考 [Prometheus 监控告警](../tutorial/prometheus.md)。
:::

## Metrics & Stats

EMQ X 将指标分为了 Metrics 与 Stats 两种。Metrics 通常指那些只会单调递增的数据，例如发送字节数量、发送报文数量。EMQ X 目前提供的 Metrics 覆盖了字节、报文、消息和事件四个维度。Stats 则通常指那些成对出现的数据，包括当前值和历史最大值，例如当前订阅数量和订阅历史最大数量。

从 v4.1.0 版本开始，EMQ X 增加了针对指定主题的 Metrics 统计，包括消息收发数量和**收发速率**。我们提供了新建主题统计、取消主题统计和返回指定主题统计信息的 HTTP API，参见 [HTTP API](./http-api.md#endpoint-topic-metrics)，你也可以直接在 Dashboard -> Analysis -> Topic Metrics 页面进行相关操作。

### Metrics

### 字节

| Name           | Data Type | Description  |
| -------------- | --------- | ------------ |
| bytes.received | Integer   | 接收字节数量 |
| bytes.sent     | Integer   | 发送字节数量 |

### 报文

| Name                         | Data Type | Description                                                  |
| ---------------------------- | --------- | ------------------------------------------------------------ |
| packets.received             | Integer   | 接收的报文数量                                               |
| packets.sent                 | Integer   | 发送的报文数量                                               |
| packets.connect.received     | Integer   | 接收的 CONNECT 报文数量                                      |
| packets.connack.auth_error   | Integer   | 发送的原因码为 0x86 和 0x87 的 CONNACK 报文数量              |
| packets.connack.error        | Integer   | 发送的原因码不为 0x00 的 CONNACK 报文数量，此指标的值大于等于 `packets.connack.auth_error` 的值 |
| packets.connack.sent         | Integer   | 发送的 CONNACK 报文数量                                      |
| packets.publish.received     | Integer   | 接收的 PUBLISH 报文数量                                      |
| packets.publish.sent         | Integer   | 发送的 PUBLISH 报文数量                                      |
| packets.publish.inuse        | Integer   | 接收的报文标识符已被占用的 PUBLISH 报文数量                  |
| packets.publish.auth_error   | Integer   | 接收的未通过 ACL 检查的 PUBLISH 报文数量                     |
| packets.publish.error        | Integer   | 接收的无法被发布的 PUBLISH 报文数量                          |
| packets.publish.dropped      | Integer   | 超出接收限制而被丢弃的 PUBLISH 报文数量                      |
| packets.puback.received      | Integer   | 接收的 PUBACK 报文数量                                       |
| packets.puback.sent          | Integer   | 发送的 PUBACK 报文数量                                       |
| packets.puback.inuse         | Integer   | 接收的报文标识符已被占用的 PUBACK 报文数量                   |
| packets.puback.missed        | Integer   | 接收的未知报文标识符 PUBACK 报文数量                         |
| packets.pubrec.received      | Integer   | 接收的 PUBREC 报文数量                                       |
| packets.pubrec.sent          | Integer   | 发送的 PUBREC 报文数量                                       |
| packets.pubrec.inuse         | Integer   | 接收的报文标识符已被占用的 PUBREC 报文数量                   |
| packets.pubrec.missed        | Integer   | 接收的未知报文标识符 PUBREC 报文数量                         |
| packets.pubrel.received      | Integer   | 接收的 PUBREL 报文数量                                       |
| packets.pubrel.sent          | Integer   | 发送的 PUBREL 报文数量                                       |
| packets.pubrel.missed        | Integer   | 接收的未知报文标识符 PUBREL 报文数量                         |
| packets.pubcomp.received     | Integer   | 接收的 PUBCOMP 报文数量                                      |
| packets.pubcomp.sent         | Integer   | 发送的 PUBCOMP 报文数量                                      |
| packets.pubcomp.inuse        | Integer   | 接收的报文标识符已被占用的 PUBCOMP 报文数量                  |
| packets.pubcomp.missed       | Integer   | 发送的 PUBCOMP 报文数量                                      |
| packets.subscribe.received   | Integer   | 接收的 SUBSCRIBE 报文数量                                    |
| packets.subscribe.error      | Integer   | 接收的订阅失败的 SUBSCRIBE 报文数量                          |
| packets.subscribe.auth_error | Integer   | 接收的未通过 ACL 检查的 SUBACK 报文数量                      |
| packets.suback.sent          | Integer   | 发送的 SUBACK 报文数量                                       |
| packets.unsubscribe.received | Integer   | 接收的 UNSUBSCRIBE 报文数量                                  |
| packets.unsubscribe.error    | Integer   | 接收的取消订阅失败的 UNSUBSCRIBE 报文数量                    |
| packets.unsuback.sent        | Integer   | 发送的 UNSUBACK 报文数量                                     |
| packets.pingreq.received     | Integer   | 接收的 PINGREQ 报文数量                                      |
| packets.pingresp.sent        | Integer   | 发送的 PUBRESP 报文数量                                      |
| packets.disconnect.received  | Integer   | 接收的 DISCONNECT 报文数量                                   |
| packets.disconnect.sent      | Integer   | 发送的 DISCONNECT 报文数量                                   |
| packets.auth.received        | Integer   | 接收的 AUTH 报文数量                                         |
| packets.auth.sent            | Integer   | 发送的 AUTH 报文数量                                         |

### 消息 (PUBLISH 报文)

| Name                            | Data Type | Description                                                  |
| ------------------------------- | --------- | ------------------------------------------------------------ |
| delivery.dropped.too_large      | Integer   | 发送时由于长度超过限制而被丢弃的消息数量                     |
| delivery.dropped.queue_full     | Integer   | 发送时由于消息队列满而被丢弃的 QoS 不为 0 的消息数量         |
| delivery.dropped.qos0_msg       | Integer   | 发送时由于消息队列满而被丢弃的 QoS 为 0 的消息数量           |
| delivery.dropped.expired        | Integer   | 发送时由于消息过期而被丢弃的消息数量                         |
| delivery.dropped.no_local       | Integer   | 发送时由于 `No Local` 订阅选项而被丢弃的消息数量             |
| delivery.dropped                | Integer   | 发送时丢弃的消息总数                                         |
| messages.delayed                | Integer   | EMQ X 存储的延迟发布的消息数量                        |
| messages.delivered              | Integer   | EMQ X 内部转发到订阅进程的消息数量                    |
| messages.dropped                | Integer   | EMQ X 内部转发到订阅进程前丢弃的消息总数              |
| messages.dropped.expired        | Integer   | 接收时由于消息过期而被丢弃的消息数量                         |
| messages.dropped.no_subscribers | Integer   | 由于没有订阅者而被丢弃的消息数量                             |
| messages.forward                | Integer   | 向其他节点转发的消息数量                                     |
| messages.publish                | Integer   | 除系统消息外发布的消息数量                                   |
| messages.qos0.received          | Integer   | 接收来自客户端的 QoS 0 消息数量                              |
| messages.qos2.received          | Integer   | 接收来自客户端的 QoS 1 消息数量                              |
| messages.qos1.received          | Integer   | 接收来自客户端的 QoS 2 消息数量                              |
| messages.qos0.sent              | Integer   | 发送给客户端的 QoS 0 消息数量                                |
| messages.qos1.sent              | Integer   | 发送给客户端的 QoS 1 消息数量                                |
| messages.qos2.sent              | Integer   | 发送给客户端的 QoS 2 消息数量                                |
| messages.received               | Integer   | 接收来自客户端的消息数量，等于 `messages.qos0.received`，`messages.qos1.received` 与 `messages.qos2.received` 之和 |
| messages.sent                   | Integer   | 发送给客户端的消息数量，等于 `messages.qos0.sent`，`messages.qos1.sent` 与 `messages.qos2.sent` 之和 |
| messages.retained               | Integer   | EMQ X 存储的保留消息数量                              |
| messages.acked                  | Integer   | 已经应答的消息数量                                           |

### 事件

| Name                  | Data Type | Description                        |
| --------------------- | --------- | ---------------------------------- |
| actions.failure       | Integer   | 规则引擎 action 执行成功次数       |
| actions.success       | Integer   | 规则引擎 action 执行失败次数       |
| rules.matched         | Integer   | 规则的匹配次数                     |
| client.auth.anonymous | Integer   | 客户端最终匿名形式登录的次数       |
| client.connect        | Integer   | `client.connect` 钩子触发次数      |
| client.authenticate   | Integer   | `client.authenticate` 钩子触发次数 |
| client.connack        | Integer   | `client.connack` 钩子触发次数      |
| client.connected      | Integer   | `client.connected` 钩子触发次数    |
| client.disconnected   | Integer   | `client.disconnected` 钩子触发次数 |
| client.check_acl      | Integer   | `client.check_acl` 钩子触发次数    |
| client.subscribe      | Integer   | `client.subscribe` 钩子触发次数    |
| client.unsubscribe    | Integer   | `client.unsubscribe` 钩子触发次数  |
| session.created       | Integer   | `session.created` 钩子触发次数     |
| session.discarded     | Integer   | `session.discarded` 钩子触发次数   |
| session.resumed       | Integer   | `session.resumed` 钩子触发次数     |
| session.takeovered    | Integer   | `session.takeovered` 钩子触发次数  |
| session.terminated    | Integer   | `session.terminated` 钩子触发次数  |

## Stats

| Name                       | Data Type | Description                |
| -------------------------- | --------- | -------------------------- |
| connections.count          | Integer   | 当前连接数量               |
| connections.max            | Integer   | 连接数量的历史最大值       |
| channels.count             | Integer   | 即 `sessions.count`        |
| channels.max               | Integer   | 即 `session.max`           |
| sessions.count             | Integer   | 当前会话数量               |
| sessions.max               | Integer   | 会话数量的历史最大值       |
| topics.count               | Integer   | 当前主题数量               |
| topics.max                 | Integer   | 主题数量的历史最大值       |
| suboptions.count           | Integer   | 即 `subscriptions.count`   |
| suboptions.max             | Integer   | 即 `subscriptions.max`     |
| subscribers.count          | Integer   | 当前订阅者数量             |
| subscribers.max            | Integer   | 订阅者数量的历史最大值     |
| subscriptions.count        | Integer   | 当前订阅数量，包含共享订阅 |
| subscriptions.max          | Integer   | 订阅数量的历史最大值       |
| subscriptions.shared.count | Integer   | 当前共享订阅数量           |
| subscriptions.shared.max   | Integer   | 共享订阅数量的历史最大值   |
| routes.count               | Integer   | 当前路由数量               |
| routes.max                 | Integer   | 路由数量的历史最大值       |
| retained.count             | Integer   | 当前保留消息数量           |
| retained.max               | Integer   | 保留消息的历史最大值       |


