---
# 标题
title: 版本发布
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

# 版本发布

## 4.0.10 版本

*发布日期: 2020-05-22*

## 增强

+ 桥接消息到0.8/0.9版本的kafka
+ 显示告警触发时间
+ 导入规则时，不需要资源处于已连接状态
+ auth_mnesia支持全局ACL规则
+ 规则引擎-动作支持添加备用动作

## 修复
+ 无法导入导出auth_username/auth_clientid问题
+ 在进程号大于6万后无法启动emqx的问题
+ MQTT报文乱序导致的问题
+ kafka主题修改分区后无法重建规则的问题
+ 相同clientid出现多个客户端同时在线
+ 在打印大量日志时，LOG丢失问题

## 4.0.8 版本

*发布日期: 2020-04-29*

+ 规则引擎支持修改和暂停
+ dashboard支持导入导出数据
+ 添加emqx_auth_mnesia插件
+ 修复规则引擎相关BUG

## 4.0.6 版本

*发布日期: 2020-04-02*

### 新增 

+ emqx数据导入导出功能
+ 规则引擎数据桥接到Kafka支持配置是否磁盘缓存
+ 规则引擎数据桥接到MQTT Broker支持payload 模板
+ 规则引擎数据桥接到Kafka支持payload 模板
+ 规则引擎数据桥接到Pulsar支持payload 模板
+ 规则引擎数据桥接到RocketMQ支持payload 模板
+ 规则引擎数据桥接到RatbitMQ支持payload 模板

### 修复

+ emqx_bridge_kafka 在K8S下挂载相同的数据目录导致的问题
+ dashboard 数据流入流出数值不准确问题
+ 多个MySQL规则时，MySQL 连接无法重连的问题

## 4.0.4 版本

*发布日期: 2020-03-17*

emqx_bridge_kafka
    Fix the issue that deleting kafka topics when another rule using the same topic.

emqx_bridge_pulsar
    Fix the issue that deleting pulsar topics when another rule using the same topic.

emqx_bridge_rocket
    Fix the issue that deleting rocket topics when another rule using the same topic.

emqx_rule_engine
    Fix the json encoding error for utf8 string in rule-engine.

emqx_statsd
    Add a new HTTP API for getting emqx metrics.

## 4.0.2 版本

*发布日期: 2020-02-11*

EMQ X 4.0.2 现已发布。此版本主要进行了错误修复和性能优化。

emqx
----

**功能增强:**

- 提升 Json 编解码性能

  Github PR:
  `emqx/emqx#3213 <https://github.com/emqx/emqx/pull/3213>`_,
  `emqx/emqx#3230 <https://github.com/emqx/emqx/pull/3230>`_,
  `emqx/emqx#3235 <https://github.com/emqx/emqx/pull/3235>`_

- 压缩生成的项目大小

  Github PR:
  `emqx/emqx#3214 <https://github.com/emqx/emqx/pull/3214>`_

**错误修复:**

- 修复某些情况下没有发送 DISCONNECT 报文的问题

  Github PR:
  `emqx/emqx#3208 <https://github.com/emqx/emqx/pull/3208>`_

- 修复收到相同 PacketID 的 PUBLISH 报文时会断开连接的问题

  Github PR:
  `emqx/emqx#3233 <https://github.com/emqx/emqx/pull/3233>`_

emqx-stomp (plugin)
-------------------

**错误修复:**

- 修复最大连接数限制不生效的问题

  Github PR:
  `emqx/emqx-stomp#93 <https://github.com/emqx/emqx-stomp/pull/93>`_

emqx-auth-redis (plugin)
------------------------

**错误修复:**

- 修复内部模块启动失败的问题

  Github PR:
  `emqx/emqx-auth-redis#151 <https://github.com/emqx/emqx-auth-redis/pull/151>`_

cowboy (dependency)
-------------------

**错误修复:**

- 修复 Websocket 连接某些情况下不会发送遗嘱消息的问题

  Github Commit:
  `emqx/cowboy#3b6bda <https://github.com/emqx/cowboy/commit/3b6bdaf4f2e3c5b793a0c3cada2c3b74c3d5e885>`_


## 4.0.0 版本

*发布日期: 2020-01-18*

### Introduction

在这个版本中，我们通过重构会话和通道显著地提高了代理的吞吐量，通过添加更多的hooks和计数器改进了可扩展性和可监控性，重新设计了规则引擎SQL来主要根据主题过滤消息/事件。在REST api、身份验证插件和MQTT客户机方面也有很多改进。

### The Broker

- 提高了代理的吞吐量::

  Session进程被删除。现在，所有关于会话和连接的逻辑都放到了单个进程中。这极大地提高了代理的吞吐量。**在我们的一个基准测试中，我们在一个具有16个CPU核心的节点上实现了QoS0消息的最大吞吐量100万TPS。另一项测试显示，emqx能够在一个节点上处理200万个并发连接，并且只使用大约17G内存**。有关更多信息，请参见emqx v4.0.0的基准测试报告。

- 优化了对MQTT数据包的处理:

  对MQTT包的处理进行了很大的更改，以使代码基更简洁。有关更多信息，请参见模块emqx_channel。

- 改进的指标:

  我们重命名了一些计数器，也增加了更多的计数器:

  - client.connect
  - client.connack
  - client.connected
  - client.authenticate
  - client.check_acl
  - client.subscribe
  - client.unsubscribe
  - client.disconnected
  - session.created
  - session.resumed
  - session.takeovered
  - session.discarded
  - session.terminated

- 改善了 hooks:

  我们修改了一些 hooks 的参数，增加了更多的 hook-points:

  - client.connect: MQTT CONNECT Packet Received
  - client.connack: MQTT CONNACK Packet Sent
  - client.connected: The MQTT Client is connected
  - client.disconnected: The MQTT Client is disconnected
  - client.authenticate: Authenticate the MQTT Client
  - client.check_acl: Check Pub/Sub ACL
  - client.subscribe: MQTT SUBSCRIBE Packet Received
  - client.unsubscribe: MQTT UNSUBSCRIBE Packet Received
  - session.created: A new session is created
  - session.subscribed: After session subscribed a topic
  - session.unsubscribed: After session unsubscribed a topic
  - session.resumed: A session is resumed
  - session.takeovered: A session is takeovered
  - session.discarded: A session is discarded
  - session.terminated: A session is terminated
  - message.publish: A message is published
  - message.delivered: A message is delivered
  - message.acked: A messaeg is acked
  - message.dropped: A message is dropped due to no subscribers

- 修正了SSL握手失败会导致进程崩溃的问题:

  以保护进程崩溃，并在SSL握手失败时给出可读的错误日志消息。

- 修复了`max_subscriptions`不能工作的问题: [emqx/emqx#2908](https://github.com/emqx/emqx/issues/2908) 

- 修正了跨集群转发时出现的消息无序问题:
  
  节点间消息通过多个RPC通道发送，默认选择通道的策略是随机的。这将导致相同主题的消息在发送到其他节点后出现顺序混乱。在这个修复中，我们将默认策略更改为按主题的哈希值。

- Fixed the issue that REST API and CLI cannot get multiple routes for a topic:

- 修正了REST API和CLI不能为一个主题获取多个路由的问题: [emqx/emqx-management#150](https://github.com/emqx/emqx-management/pull/150).

### REST API

- 在REST API中支持IPv6:

  现在所有的REST API都支持IPv6。

- HTTP API服务器的默认监听端口从8080改为8081:

  旧的 HTTP API 默认8080端口很容易与同一节点上运行的其他服务发生冲突。我们把默认值改为8081。

- 重新设计用于会话和连接的api

- 支持在订阅API中返回共享订阅的真实主题:

- 支持配置默认的AppID和AppSecret:
  
  现在默认的AppID和AppSecret可以在`etc/emqx_management.conf` 中配置。

- 发布消息的HTTP API现在支持base64编码的有效载荷:

  有些用户可能希望通过HTTP API发布二进制消息。通过这个特性，他们可以通过发送一个base64编码的有效负载来实现这一点。

- 修正了URI编码处理不正确的问题


### 身份认证插件

- 支持在配置文件中定义HTTP请求头。

- 支持在配置文件中配置默认的clientids和用户名:

   该特性已从 EMQ X V3.0中删除，现在再次添加。

### MQTT 客户端 ([emqtt](https://github.com/emqx/emqtt))

- emqtt为发布和订阅提供了命令行接口。

### 规则引擎 ([emqx-rule-engine](https://github.com/emqx/emqx-rule-engine))

- 重新设计的规则引擎的SQL:

   规则引擎的SQL在它的FROM子句中做了一点更改。旧的语法是这样的:

  `SELECT * FROM "message.publish"`

  现在改为:

  `SELECT * FROM "t/#"`

  因此，它现在主要根据主题过滤消息/事件。如果主题不匹配，此更改将阻止规则引擎处理SELECT子句，从而提高了性能。




## 3.4.5 版本

*发布日期: 2019-12-02*

+ 重新支持了通过配置文件配置默认的 username/clientid
+ 保持数据类型写入数据库
+ 修复dashboard显示问题

## 3.4.4 版本

*发布日期: 2019-11-15*

+ 修复emqx_bridge_rocket异步生产的BUG
+ 修复emqx_bridge_pulsar异步生产的BUG
+ 规则引擎SQL语句 新增 FOREACH/DO/INCASE CASE/WHEN 语法
+ 数据存储到时序数据库增加详细日志

## 3.4.3 版本

*发布日期: 2019-10-28*

+ 规则引擎 RocketMQ 支持主题模板
+ 修复热配置引起的配置文件失效
+ 优化了规则引擎中 JSON Payload 的解析语句

## 3.4.2 版本

*发布日期: 2019-10-12*

+ 新增批量 发布/订阅/取消订阅 API
+ emqx_backend_redis 支持存储最新一条消息
+ schema registry 删除版本管理

## 3.4.1 版本

*发布日期: 2019-09-30*

+ 支持RocketMQ消息桥接
+ 支持以集群为单位开启/关闭插件

## 3.4.0 版本

*发布日期: 2019-09-02*

+ 重新设计了 Dashboard 界面
+ 支持在 Dashboard 热配置
+ 支持在 Dashboard 邀请节点加入集群
+ 支持 JT/T808 协议
+ 支持编解码(Schema Registry) 功能

## 3.2.2 版本

*发布日期: 2019-08-05*

# Version 3.2.2

+ 支持私有TCP协议
+ 支持复杂json数据数据化到timescale

## 3.2.1 版本

*发布日期: 2019-07-20*

# Version 3.2.1

*Release Date: 2019-07-20*

**EMQ X 3.2.1 is mainly for bug fixes and performance enhancements. Changes in this version include:**

- Change the default uptime heartbeat interval to 30s for better performance

- Rule Engine:

  - Improve parameters for republish action

  - Fix the issue that fail to select payload fields using '.'

- Dashboard:

  - Fix the issue rendering resources list incorrectly on Safari

- LwM2M:

  - Compatible with client login using LwM2M v1.1

- Windows:

  - Delay EMQ X windows service auto start
  
  - Supports bridging data to Kafka and Pulsar


## 3.2.0 版本

*发布日期: 2019-07-12*

**EMQ X 3.2.0 is mainly for improvements of rule engine. Changes in this version include:**

- Support more backends and message brokers:

  - Backends:
    DynamoDB, InfluxDB, OpenTSDB, TimescaleDB

  - Message Brokers:
    Pulsar

- Bridging to MQTT Broker is now provided by emqx-bridge-mqtt (plugin) instead.

- Enhance rule engine:

  - Support more resources and actions:

     + Databases: MySQL, PostgreSQL, MongoDB, DynamoDB, Redis, Cassandra, OpenTSDB, TimescaleDB, InfluxDB

     + Message Brokers: Kafka, Pulsar, RabbitMQ, MQTT Broker

     + Other: WebServer, Republish, Inspect/DoNothing (debug)

 - Support more trigger events
 
 - Support emqx cluster
 
 - Support resource reconnection, startup, status monitoring and alarm
 
 - Support metrics of rules and actions
 
 - Improve Dashbaord UI for rule engine

- Optimized write performance for MySQL and PostgreSQL.

- Support rebar3 to  build project.

- The authentication plugin via http supports https now.

- Improved stability of emqx cluster.

- Provided enterprise installation packages for windows.

## 3.0.0 版本

*发布日期: 2019-01-17*

# EMQ X Enterprise

*EMQ X  Enterprise* is a massively scalable, highly available, distributed MQTT message broker for IoT, M2M and Mobile applications that can handle tens of millions of concurrent clients with enhanced security features and extended functionalities such as data bridging and data persistence .

It supports MQTT V3.1 and V3.1.1,  as well as other communication protocols such as WebSocket. It can bridge and forward MQTT messages to enterprise messaging middleware like Kafka and RabbitMQ; it also can persist MQTT messages to Redis, MySQL, PostgreSQL, MongoDB, Cassandra and other databases.

- For more information, please visit [EMQ X homepage](https://www.emqx.io).
- For more *EMQ X Enterprise* usage, please see [EMQ X User Guide](https://docs.emqx.io/broker/v2/en/index.html).

## 2.4.3 版本

*发布日期: 2018-07-23*

*EMQ X  Enterprise* is a massively scalable, highly available, distributed MQTT message broker for IoT, M2M and Mobile applications that can handle tens of millions of concurrent clients with enhanced security features and extended functionalities such as data bridging and data persistence .

It supports MQTT V3.1 and V3.1.1,  as well as other communication protocols such as WebSocket. It can bridge and forward MQTT messages to enterprise messaging middleware like Kafka and RabbitMQ; it also can persist MQTT messages to Redis, MySQL, PostgreSQL, MongoDB, Cassandra and other databases.

- For more information, please visit [EMQ X homepage](http://emqx.io).
- For more *EMQ X Enterprise* usage, please see [EMQ X User Guide](https://docs.emqx.io/broker/v2/en/index.html).
