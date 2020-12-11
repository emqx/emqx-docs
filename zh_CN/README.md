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

# EMQ X 消息服务器简介

*EMQ X* (Erlang/Enterprise/Elastic MQTT Broker) 是基于 Erlang/OTP 平台开发的开源物联网 MQTT 消息服务器。

Erlang/OTP是出色的软实时 (Soft-Realtime)、低延时 (Low-Latency)、分布式 (Distributed)的语言平台。

MQTT 是轻量的 (Lightweight)、发布订阅模式 (PubSub) 的物联网消息协议。

EMQ X 设计目标是实现高可靠，并支持承载海量物联网终端的MQTT连接，支持在海量物联网设备间低延时消息路由:

1. 稳定承载大规模的 MQTT 客户端连接，单服务器节点支持50万到100万连接。
2. 分布式节点集群，快速低延时的消息路由，单集群支持1000万规模的路由。
3. 消息服务器内扩展，支持定制多种认证方式、高效存储消息到后端数据库。
4. 完整物联网协议支持，MQTT、MQTT-SN、CoAP、LwM2M、WebSocket 或私有协议支持。


**建议您在使用前仔细阅读一遍下面列出的文档，未列出的其他文档可以按需选择查看：**

## 开始使用
  - [安装](getting-started/install.md)：不同操作系统与安装包类型的下载、安装步骤。
  - [启动 EMQ X](getting-started/start.md)：启动 EMQ X 并查看启动状态。
  - [Dashboard](getting-started/dashboard.md)：通过 Dashboard 管理 EMQ X 及在线设备。

## 认证鉴权
  - [认证简介](advanced/auth.md)：选择内置插件、外部数据库、JWT 或者 HTTP 服务作为认证数据源，验证客户端连接合法性。
  - [发布订阅 ACL](advanced/acl.md)：选择内置插件、外部数据库、或者 HTTP 服务作为 ACL 数据源，验证客户端发布订阅权限。
  - [内置 ACL](modules/internal_acl.md)：内置 ACL 可能会影响到重要功能，使用前请详细了解。

## FAQ 常见问题解答

[FAQ 常见问题解答](faq/faq.md)定期收集整理 EMQ X 用户常见问题和经常遇到的错误，如 Topic 数量限制、开源版/企业版区别，企业服务收费等；开源版如何存储数据等。


## 社区交流
 - [资源](awesome/awesome.md)：社区交流，包含社区热门教程、项目展示等资源。

## HTTP API

HTTP API 是物联网平台开发与 EMQ X 运维中频繁使用的功能，HTTP API 可以实现与外部系统的集成，例如查询并管理客户端信息、代理订阅、发布消息和创建规则等。

  - [HTTP API](advanced/http-api.md)：包含 HTTP API 接入点、接入认证方式。
  - [基本信息](advanced/http-api.md#endpoint-brokers)：获取 EMQ X 版本、运行状态等基本信息。
  - [节点](advanced/http-api.md#endpoint-nodes)：获取 EMQ X 节点信息。
  - [客户端](advanced/http-api.md#endpoint-clients)：查看在线客户端信息，支持踢出客户端。
  - [订阅信息](advanced/http-api.md#endpoint-subscriptions)：查看订阅主题列表与订阅关系。
  - [路由](advanced/http-api.md#endpoint-routes)：查看已订阅的主题。
  - [消息发布](advanced/http-api.md#endpoint-publish)：通过 HTTP 调用 EMQ X 发布 MQTT 消息，应用程序与客户端通信可靠的方式。
  - [主题订阅](advanced/http-api.md#endpoint-subscribe)：动态管理客户端订阅列表，无需客户端主动发起订阅/取消订阅。
  - [插件](advanced/http-api.md#endpoint-plugins)：插件的状态管理，启动、停止操作。

其他更多 API 请通过左侧目录查看。

## 规则引擎

规则引擎实现了消息数据与通过规则引擎能够筛选、处理、转发/存储消息到外部数据源，包括关系数据库、消息队列、Web 服务等等。

  - [规则引擎](rule/rule-engine.md)：规则引擎的概念、基础使用方式。
  - [创建规则](rule/rule-create.md)：如何创建一条规则。
  - [使用示例](rule/rule-example.md#发送数据到-web-服务)：规则引擎使用各类数据源的教程。

## 数据存储

EMQ X 企业版特有功能，数据存储将客户端上下线状态，订阅关系，离线消息、消息内容，消息抵达后发送的消息回执等操作记录到各种数据库中。数据存储包含运行时数据与消息数据，能够在服务崩溃、客户端异常离线后仍然保留数据。

  - [数据存储](backend/backend.md)：基本概念与使用场景。
  - [数据存储配置](backend/backend.md#redis-数据存储)：使用不同的数据源进行数据存储。

## 消息桥接

EMQ X 企业版桥接转发 MQTT 消息到 Kafka、RabbitMQ、Pulsar、RocketMQ、MQTT Broker 或其他 EMQ X 节点。

  - [MQTT 桥接](bridge/bridge.md#mqtt-桥接)：实现跨地域、跨集群部署。
  - [RPC 桥接](bridge/bridge.md#rpc-桥接)
  - [Kafka 桥接](bridge/bridge.md#kafka-桥接)
  - [RabbitMQ 桥接](bridge/bridge.md#rabbitmq-桥接)
  - [Pulsar 桥接](bridge/bridge.md#pulsar-桥接)
  - [RocketMQ 桥接](bridge/bridge.md#rocketmq-桥接)


## 运维部署

包含官方使用指南、最佳实践等信息。

 - [设备管理](tutorial/device-management.md)
 - [系统调优](tutorial/tune.md)
 - [生产部署](tutorial/deploy.md)
 - [Prometheus 监控告警](tutorial/prometheus.md)
 - [性能测试](tutorial/benchmark.md)

## 协议介绍
 - [MQTT 协议](development/protocol.md)
 - [MQTT-SN 协议](development/protocol.md#mqtt-sn-协议)
 - [LwM2M 协议](development/protocol.md#lwm2m-协议)
 - [私有 TCP 协议](development/protocol.md#私有-tcp-协议)

