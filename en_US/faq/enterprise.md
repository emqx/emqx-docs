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

## What's the major difference between EMQX Enterprise and EMQX?

**Tags:** [*Enterprise*](tags.md#enterprise)


EMQX Enterprise is based on EMQX, it includes all of the features from EMQX. More features are added:


- Data persistence: EMQX Enterprise supports to persist data to several kinds of databases, includes the popular relational database, such as MySQL, PostgreSQL; Memory database, such as Redis; Non-SQL DB, such as MongoDB.

- Kafka bridge: Forward MQTT message to Kafka clusters through internal bridge plugins, application can consume Kafka message to implement the streaming data process.

- RabbitMQ bridge: Forward MQTT message to RabbitMQ, application can consume RabbitMQ message to integrate with 3rd party system.

- More rule engine integrations: Various data integration with databases and streaming platforms, to name a few

  - Streaming platforms: Kafka, Pulsar, RocketMQ (or AMQP in general), SAP Event Mesh,

  - Relational Databases: MySQL, PostgreSQL, Oracle, SQL Server,

  - NoSQL Databases: MongoDB, redis, DynamoDB, Cassandra, ClickHouse

  - Time-eries Databases: InfluxDB, TimescaleDB, OpenTSDB

- Runtime Config Change: Configurations which can be set on-the-fly without restarting the service can be changed from dashboard UI, while EMQX always require restarts.

- System monitoring (EMQX Control Center)

  - EMQX cluster monitor: Including statistics of connections, topics, message & sessions.

  - Erlang VM monitor: Erlang process, threads, memory, distributed database & distributed locks etc.

  - Host monitor: Measurements of CPU, memory, disk, network and operating system.

## Does EMQX provide consulting service?

**Tags:** [*Enterprise*](tags.md#enterprise)


Yes. We have rich experience at consulting of building IoT platforms, include practice of helping Internet companies and carriers to build IoT platform that supports 10M level concurrent connections. We can help by customizing solutions for creating load-balancing, clustering, security policies, data storage and analytics, and make the solution can satisfy future business evolvement.
