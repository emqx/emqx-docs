---
# 标题
title: EMQ X 消息服务器简介
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

# EMQ X 消息服务器简介

*EMQ X* (Erlang/Enterprise/Elastic MQTT Broker) 是基于 Erlang/OTP
平台开发的开源物联网 MQTT 消息服务器。Erlang/OTP
是出色的软实时(Soft-Realtime)、低延时(Low-Latency)、分布式(Distributed)
的语言平台。MQTT 是轻量的(Lightweight)、发布订阅模式(PubSub) 的物联网消息协议。

*EMQ X* 面向海量的 **移动/物联网/车载** 等终端接入，并实现在海量物理网设备间快速低延时的消息路由:

1.  稳定承载大规模的 MQTT 客户端连接，单服务器节点支持百万连接。
2.  分布式节点集群，快速低延时的消息路由，单集群支持千万规模的路由。
3.  消息服务器内扩展，支持定制多种认证方式、高效存储消息到后端数据库。
4.  完整物联网协议支持，MQTT、MQTT-SN、CoAP、LwM2M、私有 TCP/UDP 协议支持。
