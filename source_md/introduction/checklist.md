---
# 标题
title: EMQ X 消息服务器功能列表
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

# EMQ X 消息服务器功能列表

- 完整的 MQTT V3.1/V3.1.1 及V5.0协议规范支持
  - QoS0, QoS1, QoS2 消息支持
  - 持久会话与离线消息支持
  - Retained 消息支持
  - Last Will 消息支持
  - TCP/SSL 连接支持
  - MQTT/WebSocket/SSL 支持
  - HTTP消息发布接口支持
  - $SYS/\# 系统主题支持
  - 客户端在线状态查询与订阅支持
  - 客户端 ID 或 IP 地址认证支持
  - 用户名密码认证支持
  - LDAP 认证
  - Redis、MySQL、PostgreSQL、MongoDB、HTTP 认证集成
  - 浏览器 Cookie 认证
  - 基于客户端 ID、IP 地址、用户名的访问控制(ACL)
  - 多服务器节点集群(Cluster)
  - 支持 manual、mcast、dns、etcd、k8s 等多种集群发现方式
  - 网络分区自动愈合
  - 消息速率限制
  - 连接速率限制
  - 按分区配置节点
  - 多服务器节点桥接(Bridge)
  - MQTT Broker 桥接支持
  - Stomp 协议支持
  - MQTT-SN 协议支持
  - CoAP 协议支持
  - Stomp/SockJS 支持
  - 延时 Publish ($delay/topic)
  - Flapping 检测
  - 黑名单支持
  - 共享订阅($share/\<group\>/topic)
  - TLS/PSK 支持
  - 规则引擎支持