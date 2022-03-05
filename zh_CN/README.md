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

# 产品概览

*EMQX* (Erlang/Enterprise/Elastic MQTT Broker) 是基于 Erlang/OTP 平台开发的开源物联网 MQTT 消息服务器。

Erlang/OTP是出色的软实时 (Soft-Realtime)、低延时 (Low-Latency)、分布式 (Distributed)的语言平台。

MQTT 是轻量的 (Lightweight)、发布订阅模式 (PubSub) 的物联网消息协议。

EMQX 设计目标是实现高可靠，并支持承载海量物联网终端的 MQTT 连接，支持在海量物联网设备间低延时消息路由:

1. 稳定承载大规模的 MQTT 客户端连接，单服务器节点支持 200 万连接。
2. 分布式节点集群，快速低延时的消息路由。
3. 消息服务器内扩展，支持定制多种认证方式、高效存储消息到后端数据库。
4. 完整物联网协议支持，MQTT、MQTT-SN、CoAP、LwM2M、WebSocket 或私有协议支持。

## EMQX 消息服务器功能列表

- 完整的 MQTT V3.1/V3.1.1 及 V5.0 协议规范支持
  - QoS0, QoS1, QoS2 消息支持
  - 持久会话与离线消息支持
  - Retained 消息支持
  - Last Will 消息支持
- MQTT/WebSocket TCP/SSL 支持
- HTTP 消息发布接口支持
- $SYS/# 系统主题支持
- 客户端在线状态查询与订阅支持
- 客户端 ID 或 IP 地址认证支持
- 用户名密码认证支持
- LDAP、Redis、MySQL、PostgreSQL、MongoDB、HTTP 认证集成
- 浏览器 Cookie 认证
- 基于客户端 ID、IP 地址、用户名的访问控制 (ACL)
- 多服务器节点集群 (Cluster)
- 支持 manual、mcast、dns、etcd、k8s 等多种集群发现方式
- 网络分区自动愈合
- 消息速率限制
- 连接速率限制
- 按分区配置节点
- 多服务器节点桥接 (Bridge)
- MQTT Broker 桥接支持
- Stomp 协议支持
- MQTT-SN 协议支持
- CoAP 协议支持
- LwM2M 协议支持
- Stomp/SockJS 支持
- 延时 Publish ($delay/topic)
- Flapping 检测
- 黑名单支持
- 共享订阅 ($share/:group/topic)
- TLS/PSK 支持
- 规则引擎
  - 空动作 (调试)
  - 消息重新发布
  - 桥接数据到 MQTT Broker
  - 检查 (调试)
  - 发送数据到 Web 服务
