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

当客户端发给服务端的 PUBLISH 报文的保留(Retain)标志被设置为 1时，这条消息就是保留消息. 当服务端收到保留消息时，服务端必须存储此应用消息，并用其替换此主题下任何已存在的消息，服务端需要将保留消息分发给未来订阅此主题的客户端。

## EMQ X 的保留消息

EMQ X 默认开启保留消息的功能，可以在 `etc/emqx.conf` 中通过修改 `mqtt.retain_available` 或 `zone.external.retain_available` 实现拒绝接收来自客户端的保留消息。

EMQ X 的保留消息机制是通过 `emqx_retainer` 插件来实现的，`emqx_retainer` 插件默认开启，插件的配置路径为 `etc/plugins/emqx_retainer.conf`

```
##--------------------------------------------------------------------
## EMQ X Retainer
##--------------------------------------------------------------------

## Where to store the retained messages.
##
## Notice that all nodes in the same cluster have to be configured to
## use the same storage_type.
##
## Value: ram | disc | disc_only
##  - ram: memory only
##  - disc: both memory and disc
##  - disc_only: disc only
##
## Default: ram
retainer.storage_type = ram

## Maximum number of retained messages. 0 means no limit.
##
## Value: Number >= 0
retainer.max_retained_messages = 0

## Maximum retained message size.
##
## Value: Bytes
retainer.max_payload_size = 1MB

## Expiry interval of the retained messages. Never expire if the value is 0.
##
## Value: Duration
##  - h: hour
##  - m: minute
##  - s: second
##
## Examples:
##  - 2h:  2 hours
##  - 30m: 30 minutes
##  - 20s: 20 seconds
##
## Defaut: 0
retainer.expiry_interval = 0
```

根据配置文件中的注释可以清楚的了解到各项配置的作用，`retainer.storage_type`  决定保留消息的储存位置，`retainer.max_retained_messages` 和 `retainer.max_payload_size` 决定 EMQ X 可以接受的最大的保留消息的数量以及每条保留消息的最大荷载，`retainer.expiry_interval` 决定保留消息的过期时间。
