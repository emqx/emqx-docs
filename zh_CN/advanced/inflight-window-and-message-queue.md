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

# 飞行窗口和消息队列

## 简介

为了提高消息吞吐效率和减少网络波动带来的影响，EMQ X 允许多个未确认的 QoS 1 和 QoS 2 报文同时存在于网路链路上。这些已发送但未确认的报文将被存放在 Inflight Window 中直至完成确认。

当网络链路中同时存在的报文超出限制，即 Inflight Window 到达长度限制（见 `max_inflight`）时，EMQ X 将不再发送后续的报文，而是将这些报文存储在 Message Queue 中。一旦 Inflight Window 中有报文完成确认，Message Queue 中的报文就会以先入先出的顺序被发送，同时存储到 Inflight Window 中。

当客户端离线时，Message Queue 还会被用来存储 QoS 0 消息，这些消息将在客户端下次上线时被发送。这功能默认开启，当然你也可以手动关闭，见 `mqueue_store_qos0`。

需要注意的是，如果 Message Queue 也到达了长度限制，后续的报文将依然缓存到 Message Queue，但相应的 Message Queue 中最先缓存的消息将被丢弃。如果队列中存在 QoS 0 消息，那么将优先丢弃 QoS 0 消息。因此，根据你的实际情况配置一个合适的 Message Queue 长度限制（见 `max_mqueue_len`）是非常重要的。

## 飞行队列与 Receive Maximum

MQTT v5.0 协议为 CONNECT 报文新增了一个 `Receive Maximum` 的属性，官方对它的解释是：

客户端使用此值限制客户端愿意同时处理的 QoS 为 1 和 QoS 为 2 的发布消息最大数量。没有机制可以限制服务端试图发送的 QoS 为 0 的发布消息 。

也就是说，服务端可以在等待确认时使用不同的报文标识符向客户端发送后续的 PUBLISH 报文，直到未被确认的报文数量到达 `Receive Maximum` 限制。

不难看出，`Receive Maximum` 其实与 EMQ X 中的 Inflight Window 机制如出一辙，只是在 MQTT v5.0 协议发布前，EMQ X 就已经对接入的 MQTT 客户端提供了这一功能。现在，使用 MQTT v5.0 协议的客户端将按照 `Receive Maximum` 的规范来设置 Inflight Window 的最大长度，而更低版本 MQTT 协议的客户端则依然按照配置来设置。

## 配置项

| 配置项            | 类型    | 可取值            | 默认值                                     | 说明                                                   |
| ----------------- | ------- | ----------------- | ------------------------------------------ | ------------------------------------------------------ |
| max_inflight      | integer | >= 0              | 32 *(external)*,<br /> 128 *(internal)*    | Inflight Window 长度限制，0 即无限制                    |
| max_mqueue_len    | integer | >= 0              | 1000 *(external)*,<br />10000 *(internal)* | Message Queue 长度限制，0 即无限制                     |
| mqueue_store_qos0 | enum    | `true`, `false`   | true                                       | 客户端离线时 EMQ X 是否存储 QoS 0 消息至 Message Queue |







