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

当客户端发给服务端的 PUBLISH 报文的 Retain 标志被设置为 1 时，这条消息就是保留消息。当服务端收到 Retain 标志为 1 的 PUBLISH 报文时，它将进行以下操作：

1. 如果存在匹配此主题名的订阅者，则按正常逻辑进行转发，并在转发前清除 Retain 标志。MQTT v3.1.1 协议中 Retain 标志必须被清除，而 MQTT v5.0 协议则在订阅选项中新增了一个 Retain As Publish 字段，由客户端自行指示服务端在转发前是否需要清除 Retain 标志。

2. 如果 Payload 非空，存储此应用消息，如果此主题名下已经存在保留消息则进行替换。如果 Payload 为空，服务端不会存储此应用消息，同时清除此主题名下已经存在的保留消息。

而每当有订阅者建立订阅时，服务端就会查找是否存在匹配该订阅的保留消息，如果保留消息存在，就会立即转发给订阅者。当保留消息在这种情况下被转发给订阅者时，它的 Retain 标志必须保持为 1。相比 MQTT v3.1.1，MQTT v5.0 对于订阅建立时是否发送保留消息做了更细致的划分，并在订阅选项中提供了 Retain Handling 字段。例如某些客户端可能仅希望在首次订阅时接收保留消息，又或者不希望在订阅建立时接收保留消息，都可以通过 Retain Handling 选项调整。

保留消息虽然存储在服务端中，但它并不属于会话的一部分。也就是说，即便发布这个保留消息的会话终结，保留消息也不会被删除。删除保留消息只有两种方式：

1. 前文已经提到过的，客户端往某个主题发送一个 Payload 为空的保留消息，服务端就会删除这个主题下的保留消息。

2. 消息过期间隔属性在保留消息中同样适用，如果客户端设置了这一属性，那么保留消息在服务端存储超过过期时间后就会被删除。

借助保留消息，新的订阅者能够立即获取最近的状态，而不需要等待无法预期的时间，这在很多场景下很非常重要的。

## EMQ X Broker 的保留消息

#### 接收和拒绝保留消息

EMQ X Broker 默认开启保留消息的功能，可以在 `etc/emqx.conf` 中通过修改 `zone.external.retain_available` 实现拒绝接收来自客户端的保留消息。EMQ X Broker 使用 Zone 来管理配置组。一个 Zone 定义了一组配置项 (比如最大连接数等)，Listener 可以指定使用某个 Zone，以使用该 Zone 下的所有配置。关于 Zone 的更多信息，请参见 [配置项](configuration/index.md)。

#### 关于保留消息的配置

EMQ X Broker 的保留消息机制是通过 `emqx_retainer` 插件来实现的，`emqx_retainer` 插件实现了保留消息的储存机制，通过修改 `emqx_retainer` 插件的配置，可以调整 EMQ X Broker 储存保留消息的位置，限制接收保留消息数量和 Payload 的最大值，以及调整保留消息的默认过期时间。关于 EMQ X Broker 插件的更多信息， 请参加 [插件](advanced/plugins.md)。

`emqx_retainer` 插件默认开启，插件的配置路径为 `etc/plugins/emqx_retainer.conf`。

| 配置项                         | 类型     | 可取值                   | 默认值 | 说明                                                         |
| ------------------------------ | -------- | ------------------------ | ------ | ------------------------------------------------------------ |
| retainer.storage_type          | enum     | ram,<br /> disc,<br />disc_only | ram |ram：仅储存在内存中；<br />disc：储存在内存和硬盘中；<br />disc_only：仅储存在硬盘中。|
| retainer.max_retained_messages | integer  | \>= 0                    | 0      | 保留消息的最大数量，0 表示没有限制。保留消息数量超出最大值限制后 EMQ X Broker 会把收到的保留消息作为普通消息处理。 |
| retainer.max_payload_size      | bytesize |                          | 1MB    | 保留消息的最大 payload 值。payload 大小超出最大值后 EMQ Ｘ 消息服务器会把收到的保留消息作为普通消息处理。 |
| retainer.expiry_interval       | duration |                          | ０     | 保留消息的过期时间，0 表示永不过期。如果 Publish 报文中设置了消息过期间隔，那么以 Publish 报文中的消息过期间隔为准。 |
