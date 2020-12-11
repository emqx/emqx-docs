---
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
ref:
---

# 保留消息

## 简介

服务端收到 Retain 标志为 1 的 PUBLISH 报文时，会将该报文视为保留消息，除了被正常转发以外，保留消息会被存储在服务端，每个主题下只能存在一份保留消息，因此如果已经存在相同主题的保留消息，则该保留消息被替换。

当客户端建立订阅时，如果服务端存在主题匹配的保留消息，则这些保留消息将被立即发送给该客户端。借助保留消息，新的订阅者能够立即获取最近的状态，而不需要等待无法预期的时间，这在很多场景下非常重要的。

EMQ X 默认开启保留消息的功能，可以在 `etc/emqx.conf` 中修改 `mqtt.retain_available` 为 `false` 以禁用保留消息功能。如果 EMQ X 在保留消息功能被禁用的情况下依然收到了保留消息，那么将返回原因码为 0x9A（不支持保留消息）的 DISCONNECT 报文。

## 配置

EMQ X 的保留消息功能是由 `emqx_retainer` 插件实现，该插件默认开启，通过修改 `emqx_retainer` 插件的配置，可以调整 EMQ X 储存保留消息的位置，限制接收保留消息数量和 Payload 最大长度，以及调整保留消息的过期时间。关于 EMQ X 插件的更多信息， 请参见 [插件](advanced/plugins.md)。

`emqx_retainer` 插件默认开启，插件的配置路径为 `etc/plugins/emqx_retainer.conf`。

| 配置项                         | 类型     | 可取值                   | 默认值 | 说明                                                         |
| ------------------------------ | -------- | ------------------------ | ------ | ------------------------------------------------------------ |
| retainer.storage_type          | enum     | `ram`, `disc`, `disc_only` | ram |ram：仅储存在内存中；<br />disc：储存在内存和硬盘中；<br />disc_only：仅储存在硬盘中。|
| retainer.max_retained_messages | integer  | \>= 0                    | 0      | 保留消息的最大数量，0 表示没有限制。保留消息数量超出最大值限制后，可以替换已存在的保留消息，但不能为新的主题储存保留消息。 |
| retainer.max_payload_size      | bytesize |                          | 1MB    | 保留消息的最大 Payload 值。Payload 大小超出最大值后 EMQ Ｘ 消息服务器会把收到的保留消息作为普通消息处理。 |
| retainer.expiry_interval       | duration |                          | ０     | 保留消息的过期时间，0 表示永不过期。如果 PUBLISH 报文中设置了消息过期间隔，那么以 PUBLISH 报文中的消息过期间隔为准。 |

::: tip
EMQ X Enterprise 中可将保留消息存储到多种外部数据库。
:::