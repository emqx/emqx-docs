---
# 编写日期
date: 2020-02-20 12:44:32
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

# Business Service

## What's the major difference between EMQ X enterprise and broker?

**Tags:** [*Enterprise*](tags.md#enterprise)


EMQ X Enterprise (enterprise version) is based on Broker (open source version), it includes all of the features of open source version.  Comparing to open source version, it has following difference:

- Concurrent connection level: the stable concurrent connection level for open source version is 100k, while enterprise version is 1M.
- Data persistence: Enterprise version supports to persist data to several kinds of databases, includes the popular relational database, such as MySQL, PostgreSQL; Memory database, such as Redis; Non-SQL DB, such as MongoDB.
- Kafka bridge: Forward MQTT message to Kafka clusters through internal bridge plugins, application can consume Kafka message to implement the streaming data process.
- RabbitMQ bridge: Support to forward MQTT message to RabbitMQ, application can consume RabbitMQ message to integrate with 3rd party system.
- System monitoring (EMQ X Control Center)

  - EMQ X cluster monitor: Include statistics of connections, topics, message & sessions.

  - Erlang VM monitor: Erlang process, threads, memory, distributed database & distributed locks etc.

  - Host monitor: Measurements of CPU, memory, disk, network and operating system.
- security: By configuration of TLS, DTLS connections and certifications to get higher secured connections.




## Does EMQ X provide consulting service?

**Tags:** [*Enterprise*](tags.md#enterprise)


Yes. We have rich experience at consulting of building IoT platforms, include practice of helping Internet companies and carriers to build IoT platform that supports 10M level concurrent connections. We can help by customizing solutions for creating load-balancing, clustering, security policies, data storage and analytics, and make the solution can satisfy future business evolvement.
