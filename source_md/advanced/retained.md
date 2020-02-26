---
# 标题
title: 保留消息
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# 保留消息

## 什么是保留消息

服务端收到 Retain 标志为 1 的 PUBLISH 报文时，会将该报文视为保留消息，除了被正常转发以外，保留消息会被存储在服务端，每个主题下只能存在一份保留消息，因此如果已经存在相同主题的保留消息，则该保留消息被替换。当客户端建立订阅时，如果服务端存在主题匹配的保留消息，则这些保留消息将被立即发送给该客户端。

保留消息虽然存储在服务端中，但它并不属于会话的一部分。也就是说，即便发布这个保留消息的会话终结，保留消息也不会被删除。

借助保留消息，新的订阅者能够立即获取最近的状态，而不需要等待无法预期的时间，这在很多场景下很非常重要的。

## EMQ X Broker 的保留消息

#### 接收和拒绝保留消息

EMQ X Broker 默认开启保留消息的功能，可以在 `etc/emqx.conf` 中通过修改 `mqtt.retain_available` 实现拒绝接收来自客户端的保留消息。当 `mqtt.retain_available = false` 的时候，EMQ X Broker 收到收到 Retain 标志为 1 的 PUBLISH 报文会返回原因码为 0x9A（不支持保留消息）的 DISCONNECT 报文。

#### 关于保留消息的配置

EMQ X Broker 的保留消息机制是通过 `emqx_retainer` 插件来实现的，`emqx_retainer` 插件实现了保留消息的储存机制，通过修改 `emqx_retainer` 插件的配置，可以调整 EMQ X Broker 储存保留消息的位置，限制接收保留消息数量和 Payload 最大长度，以及调整保留消息的过期时间。关于 EMQ X Broker 插件的更多信息， 请参见 [插件](advanced/plugins.md)。

`emqx_retainer` 插件默认开启，插件的配置路径为 `etc/plugins/emqx_retainer.conf`。

| 配置项                         | 类型     | 可取值                   | 默认值 | 说明                                                         |
| ------------------------------ | -------- | ------------------------ | ------ | ------------------------------------------------------------ |
| retainer.storage_type          | enum     | ram,<br /> disc,<br />disc_only | ram |ram：仅储存在内存中；<br />disc：储存在内存和硬盘中；<br />disc_only：仅储存在硬盘中。|
| retainer.max_retained_messages | integer  | \>= 0                    | 0      | 保留消息的最大数量，0 表示没有限制。保留消息数量超出最大值限制后 EMQ X Broker 会把收到的保留消息作为普通消息处理。 |
| retainer.max_payload_size      | bytesize |                          | 1MB    | 保留消息的最大 Payload 值。Payload 大小超出最大值后 EMQ Ｘ 消息服务器会把收到的保留消息作为普通消息处理。 |
| retainer.expiry_interval       | duration |                          | ０     | 保留消息的过期时间，0 表示永不过期。如果 PUBLISH 报文中设置了消息过期间隔，那么以 PUBLISH 报文中的消息过期间隔为准。 |
