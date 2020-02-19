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

当客户端发给服务端的 PUBLISH 报文的保留标志被设置为 1时，这条消息就是保留消息. 当服务端收到保留消息时，服务端必须存储此应用消息，并用其替换此主题下任何已存在的消息，服务端需要将保留消息分发给未来订阅此主题的客户端。

## EMQ X Broker 的保留消息

EMQ X Broker 默认开启保留消息的功能，可以在 `etc/emqx.conf` 中通过修改 `mqtt.retain_available` 或 `zone.external.retain_available` 实现拒绝接收来自客户端的保留消息。

EMQ X Broker 的保留消息机制是通过 `emqx_retainer` 插件来实现的，`emqx_retainer` 插件默认开启，插件的配置路径为 `etc/plugins/emqx_retainer.conf`

| 配置项                         | 类型     | 可取值                   | 默认值 | 说明                                                         |
| ------------------------------ | -------- | ------------------------ | ------ | ------------------------------------------------------------ |
| retainer.storage_type          | enum     | ram,<br /> disc,<br />disc_only | ram |ram：仅储存在内存中；<br />disc：储存在内存和硬盘中；<br />disc_only：仅储存在硬盘中。|
| retainer.max_retained_messages | integer  | \>= 0                    | 0      | 保留消息的最大数量，0 表示没有限制。保留消息数量超出最大值限制后 EMQ X Broker 会把收到的保留消息作为普通消息处理。 |
| retainer.max_payload_size      | bytesize |                          | 1MB    | 保留消息的最大 payload 值。payload 大小超出最大值后 EMQ Ｘ 消息服务器会把收到的保留消息作为普通消息处理。 |
| retainer.expiry_interval       | duration |                          | ０     | 保留消息的过期时间，0 表示永不过期。如果 Publish 报文中设置了消息过期间隔，那么以 Publish 报文中的消息过期间隔为准。 |
