# 保留消息

EMQX 中处理 MQTT 的保留消息 (Retained messages) 的组件叫做 Retainer，这个名字在 EMQX 的文档和技术资料中会经常用到。

## 简介

MQTT 服务端收到 Retain 标志为 1 的 PUBLISH 报文时，会将该报文视为保留消息，除了被正常转发以外，
保留消息会被存储在服务端，每个主题下只能存在一份保留消息，因此如果已经存在相同主题的保留消息，则该保留消息被替换。

当客户端建立订阅时，如果服务端存在主题匹配的保留消息，则这些保留消息将被立即发送给该客户端。
借助保留消息，新的订阅者能够立即获取最近的状态，而不需要等待无法预期的时间，这在很多场景下非常重要的。

EMQX 默认开启保留消息的功能，可以在 `etc/emqx.conf` 中修改 `mqtt.retain_available` 为 `false` 以禁用保留消息功能。

::: warning
如果 EMQX 在保留消息功能被禁用的情况下依然收到了保留消息，那么将返回原因码为 0x9A（不支持保留消息）的 DISCONNECT 报文。
:::

<!---
## 流控

The message read and deliver rate can be controlled.
When a client subscribes to a wildcard topic, it may match a large number of topics having messages retained.
Without flow control, the all matched messages will be copied into the subscriber's process memory space,
this may cause the subscriber Erlang process (the actor) to allocate excessive amount of RAM and risk at
shutdown forced by the `force_shutdown` policy.

To make it less aggressive, `retainer.flow_control` settings can be used.
The processing flow is as follows:

1. Load `batch_read_number` of retained message from the retainer storage
1. Deliver `batch_deliver_number` of messages
1. Repeat, until all retained messages are delivered

You may find more detailed information from configuration documents.
-->
