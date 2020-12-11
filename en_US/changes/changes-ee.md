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
ref: undefined
---

# Release version

## 4.0.10 version

*Release date: 2020-05-22*

## Enhanced
+ Bridge message to 0.8/0.9 version of kafka
+ Show Alarm Trigger Time
+ When you import a rule, you don't need a resource to be in a connected state.
+ auth_mnesia support global ACL rule
+ Rule Engine - Action Support adds alternate action

## Fix
+ Cannot import export auth_username/auth_clientid issues
+ Problems that cannot start emqx after the process number is greater than 60,000
+ Problems caused by the disorderof ness of MQTT messages
+ Issues where the kafka topic cannot rebuild rules after partitioning
+ The same clientid appears multiple clients online at the same time
  LOG loss issue when printing a large number of logs

## 4.0.8 version

*Release date: 2020-04-29*

+ The rules engine supports modifications and pauses
+ Dashboard supports importing and exporting data
+ Add the emqx_auth_mnesia plug-in
+ Fixed bugs related to rule engine

## 4.0.6 version

*Release date: 2020-04-02*

### Enhancements:

+ emqx data import and export function
+ rule engine data bridge to Kafka support configuration for disk caching
+ the rules engine data bridge is connected to the MQTT Broker support payload template
The rules engine data bridge connects to the Kafka payload template
The rules engine data bridge is connected to the Pulsar payload template
+ the rules engine data bridge receives the RocketMQ support payload template
+ the rules engine data bridge receives the RatbitMQ support payload template

### Bug fixes:

+ emqx_bridge_kafka caused problems by mounting the same data directory under K8S
+ dashboard data in and out of the numerical problem is not accurate
MySQL connection cannot be reconnected with multiple MySQL rules

## 4.0.4 version

*Release date: 2020-03-17*

emqx_bridge_kafka
    修复规则引擎删除相同主题的BUG

emqx_bridge_pulsar
    修复规则引擎删除相同主题的BUG

emqx_bridge_rocket
    修复规则引擎删除相同主题的BUG

emqx_rule_engine
    修复规则引擎UT8F编码失败BUG

emqx_statsd
    添加HTTP API获取emqx统计指标

## 4.0.2 version

*Release date: 2020-02-11*

EMQ X 4.0.2 is released now. This version mainly focuses on bug fixes and performance optimizes.

### emqx

#### Enhancements:

- Enhance performance of json encode/decode

  Github PR:
  + [emqx/emqx#3213](https://github.com/emqx/emqx/pull/3213)
  + [emqx/emqx#3230](https://github.com/emqx/emqx/pull/3230)
  + [emqx/emqx#3235](https://github.com/emqx/emqx/pull/3235)

- Compress the generated object code

  Github PR:
  
  + [emqx/emqx#3214](https://github.com/emqx/emqx/pull/3214)

#### Bug fixes:

- Fix the issue that DISCONNECT packet will not be sent in some cases

  Github PR:

  + [emqx/emqx#3208](https://github.com/emqx/emqx/pull/3208)

- Fix the issue that the connection will be closed when broker received the same Packet ID

  Github PR:
  
  + [emqx/emqx#3233](https://github.com/emqx/emqx/pull/3233)

### emqx-stomp (plugin)

#### Bug fixes

- Fix the issue that the maximum number of connections doesn't take effect

  Github PR:
  
  + [emqx/emqx-stomp#93](https://github.com/emqx/emqx-stomp/pull/93)

### emqx-auth-redis (plugin)

#### Bug fixes:

- Fix the issue that internal module start failed

  Github PR:
  
  + [emqx/emqx-auth-redis#151](https://github.com/emqx/emqx-auth-redis/pull/151)



## 4.0.0 version

*Release date: 2020-01-18*

### Introduction

EMQ X 4.0.0 is a major release.

In this version we significantly improved the throughput of the broker by refactoring the session and channel, improved the extensibility and monitorability by adding more hooks and counters, redesigned rule-engine SQL to filtering messages/events mainly by topics. And also lots of improvements in REST APIs, authentication plugins, and the MQTT client.

### The Broker

- Improved the throughput of the broker:

  The session process is removed. Now all the logics about session and connection are put into a single process. This significantly improves the throughput of the broker. **In one of our benchmarks, we've achieved throughput max to 1 million TPS of QoS0 messages on a single node with 16 CPU cores. And another test shows emqx is able to handle 2 million concurrent connections on a single node without any problem using only about 17G memory**. See the benchmark reports of emqx v4.0.0 for more information.

- Optimized handling of MQTT packets:

  The handling of MQTT packets is changed a lot to make a cleaner code base. See the module emqx_channel for more infomation.

- Improved the metrics:
 
  We've renamed some counters and also added more counters:

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

- Improved the hooks:

  We've modified parameters of some hooks and also added more hook-points:

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

- Fixed the issue that failure of SSL handshake would crash the process:
  
  To defend the process crash and give a readable error log message if SSL handshake failed.

- Fixed the issue that ``max_subscriptions`` not working:

  Fixed the issue that the `zone.external.max_subscriptions` in emqx.conf not working. See [emqx/emqx#2908](https://github.com/emqx/emqx/issues/2908) for more information.

- Fixed message out-of-order issues when forwarding across clusters:
  
  The inter-node messages are sent via multiple RPC channels, and the strategy of choosing the channels is `random` by default. This causes the messages of the same topic out-of-order after being sent to the other node. We changed the default strategy to hashed by topic in this fix.

- Fixed the issue that REST API and CLI cannot get multiple routes for a topic:

  See [emqx/emqx-management#150](https://github.com/emqx/emqx-management/pull/150) for more information.

### REST API

- Supported IPv6 in REST APIs:

  IPv6 is now supported in all REST APIs.

- The default listening port for the HTTP API server is changed from 8080 to 8081:

  The old default port of management APIs 8080 is easily conflicted with other services running on the same node. We changed it defaults to 8081.

- Redesgin the APIs for sessions and connections:

  The session in emqx is now a concept part of the clients. ``connections`` APIs are changed to ``clients`` APIs, and the new APIs support all the old features was in session APIs.

- Support returning the real topic of shared subscription in the subscriptions API:

  Now the shared subscriptions are shown in the form of '$shared/<group>/topic'.

- Support configuring the default AppID and AppSecret:
  
  Now default AppID and AppSecret can be configured in etc/emqx_management.conf.

- The HTTP API for publishing message now supports base64 encoded payload:

  Some users may want to publish binary message over HTTP APIs. From this feature they can do that by sending a base64 encoded payload.

- Fix the issue that encoded URI isn't handled correctly:
 
  The API for deleting banned clients was not handling the percent encoded URL before this fix.

### Authentication Plugins

- Support defining HTTP request headers in config files.

- Support configuring the default clientids and usernames in config files:

   This feature was removed from emqx v3.0, and is added again now.

### MQTT Client ([emqtt](https://github.com/emqx/emqtt))

- emqtt provides command line interfaces for publishing and subscribing.

### Rule Engine ([emqx-rule-engine](https://github.com/emqx/emqx-rule-engine))

- Redesigned the SQL for rule engine:

   SQL for rule-engine is changed a little bit in its `FROM` clause. The old syntax looks like:

  `SELECT * FROM "message.publish"`

   But now it is changed to:

  `SELECT * FROM "t/#"`

   So it is now filtering messages/events mainly by topics.

   This change stops rule engine from processing the `SELECT` clause if the topic is not matched, so improved the performance.


## 3.4.5 version

*Release date: 2019-12-02*

+ Re-supported configuring default username/clientid via configuration file
+ Keep data types written to the database
+ Fix dashboard display issues

## 3.4.4 version

*Release date: 2019-11-15*

+ Fix the bug of emqx_bridge_rocket asynchronous production
+ Fix the bug of emqx_bridge_pulsar asynchronous production
+ Rule Engine SQL Statement Added FOREACH/DO/INCASE CASE/WHEN Syntax
+ Data storage to the timing database to add detailed logs

## 3.4.3 version

*Release date: 2019-10-28*

+ Support topic template in RocketMQ rule-engine actions.
+ Fixed the in-consistency of configs caused by hot config changing.
+ Improved the SQL syntax for decoding the payload of JSON format.

## 3.4.2 version

*Release date: 2019-10-12*

+ Add HTTP API for batching publish/subscrie/unsubscrie.
+ Support storing only the most latest message in emqx_backend_redis
+ Remove the version control from schema registry.

## 3.4.1 version

*Release date: 2019-09-30*

+ Support for RocketMQ message bridge
+ Support for opening/closing plugins in clusters

## 3.4.0 version

*Release date: 2019-09-02*

+ Redesigned the dashboard.
+ Support hot configurations on dashboard.
+ Support cluster management on dashboard.
+ Support JT/T808 protocol.
+ Support schema registry.

## 3.2.2 version

*Release date: 2019-08-05*

# Version 3.2.2

+ supports private TCP protocol
+ supports datalization of complex json data to timescale

## 3.2.1 version

*Release date: 2019-07-20*

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


## 3.2.0 version

*Release date: 2019-07-12*

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

## 3.0.0 version

*Release date: 2019-01-17*

# EMQ X Enterprise

*EMQ X  Enterprise* is a massively scalable, highly available, distributed MQTT message broker for IoT, M2M and Mobile applications that can handle tens of millions of concurrent clients with enhanced security features and extended functionalities such as data bridging and data persistence .

It supports MQTT V3.1 and V3.1.1,  as well as other communication protocols such as WebSocket. It can bridge and forward MQTT messages to enterprise messaging middleware like Kafka and RabbitMQ; it also can persist MQTT messages to Redis, MySQL, PostgreSQL, MongoDB, Cassandra and other databases.

- For more information, please visit [EMQ X homepage](https://www.emqx.io).
- For more *EMQ X Enterprise* usage, please see [EMQ X User Guide](https://docs.emqx.io/broker/v2/en/index.html).

## 2.4.3 version

*Release date: 2018-07-23*

# EMQ X Enterprise

*EMQ X  Enterprise* is a massively scalable, highly available, distributed MQTT message broker for IoT, M2M and Mobile applications that can handle tens of millions of concurrent clients with enhanced security features and extended functionalities such as data bridging and data persistence .

It supports MQTT V3.1 and V3.1.1,  as well as other communication protocols such as WebSocket. It can bridge and forward MQTT messages to enterprise messaging middleware like Kafka and RabbitMQ; it also can persist MQTT messages to Redis, MySQL, PostgreSQL, MongoDB, Cassandra and other databases.

- For more information, please visit [EMQ X homepage](http://emqx.io).
- For more *EMQ X Enterprise* usage, please see [EMQ X User Guide](https://docs.emqx.io/broker/v2/en/index.html).

