---
# 标题
title: 企业功能
# 编写日期
date: 2020-02-21 16:06:34
# 作者 Github 名称
author: terry-xiaoyu
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# 企业功能

## 开源版、企业版和专业版的对比

|                                     | EMQ X Broker | EMQ X Enterprise | EMQ X Platform |
| ----------------------------------- | ------------ | ---------------- | -------------- |
| **设备连接**                        |              |                  |                |
| 最大连接数                          | 十万级       | 百万级           | 千万级         |
| **基础功能**                        |              |                  |                |
| 完整 MQTT v3.1/v3.1.1/v5.0 协议支持 | √            | √                | √              |
| QoS0/1/2 消息发布与订阅             | √            | √                | √              |
| 持久会话与离线消息                  | √            | √                | √              |
| Retained 消息                       | √            | √                | √              |
| 遗愿消息 (Last Will)                | √            | √                | √              |
| 系统主题 (`$SYS/#`)                 | √            | √                | √              |
| 通过 Eclipse Paho 互操作性测试      | √            | √                | √              |
| 本地订阅                            | √            | √                | √              |
| 可通过钩子、模块、插件扩展          | √            | √                | √              |
| 单节点上的共享订阅                  | √            | √                | √              |
| 集群上的共享订阅                    | √            | √                | √              |
| **协议支持**                        |              |                  |                |
| TCP/SSL 连接                        | √            | √                | √              |
| WebSocket/SSL 连接                  | √            | √                | √              |
| HTTP Pub/Sub API                    | √            | √                | √              |
| STOMP 协议                          | √            | √                | √              |
| MQTT-SN 协议                        | √            | √                | √              |
| CoAP 协议                           | √            | √                | √              |
| LwM2M 协议                          | √            | √                | √              |
| **集群支持**                        |              |                  |                |
| 基础集群                            | √            | √                | √              |
| 扩展集群                            |              | √                | √              |
| 跨多个 DataCenter                   |              |                  | √              |
| **认证鉴权**                        |              |                  |                |
| ClientID 认证                       | √            | √                | √              |
| Username 认证                       | √            | √                | √              |
| IP 地址认证                         | √            | √                | √              |
| 黑名单 (Banned)                     | √            | √                | √              |
| Redis 认证                          | √            | √                | √              |
| MongoDB 认证                        | √            | √                | √              |
| MySQL 认证                          | √            | √                | √              |
| PostgreSQL 认证                     | √            | √                | √              |
| HTTP API 认证                       | √            | √                | √              |
| JWT 认证                            | √            | √                | √              |
| LDAP 认证                           | √            | √                | √              |
| **规则引擎**                        |              |                  |                |
| 基础功能                            | √            | √                | √              |
| 调试 Action (inspect)               | √            | √                | √              |
| 重新发布 Action (republish)         | √            | √                | √              |
| 空 Action (do_nothing)              | √            | √                | √              |
| 数据存储到 Web 服务 Action          | √            | √                | √              |
| 数据存储到 Kafka Action             |              | √                | √              |
| 数据存储到 MQTT Broker Action       |              | √                | √              |
| 数据存储到 RabbitMQ Action          |              | √                | √              |
| 数据存储到 RocketMQ Action          |              | √                | √              |
| 数据存储到 Pulsar Action            |              | √                | √              |
| 数据存储到 Redis Action             |              | √                | √              |
| 数据存储到 Cassandra Action         |              | √                | √              |
| 数据存储到 TimescaleDB Action       |              | √                | √              |
| 数据存储到 InfluxDB Action          |              | √                | √              |
| 数据存储到 PostgresQL Action        |              | √                | √              |
| 数据存储到 MySQL Action             |              | √                | √              |
| 数据存储到 OpenTSDB Action          |              | √                | √              |
| 数据存储到 MongoDB Action           |              | √                | √              |
| 数据存储到 DynamoDB Action          |              | √                | √              |
| **编解码**                          |              |                  |                |
| Avro 编解码                         |              | √                | √              |
| ProtoBuf 编解码                     |              | √                | √              |
| 第三方定制编解码                    |              | √                | √              |
| **消息存储**                        |              | √                | √              |
| MySQL 消息存储                      |              | √                | √              |
| PostgreSQL 消息存储                 |              | √                | √              |
| MongoDB 消息存储                    |              | √                | √              |
| DynamoDB 消息存储                   |              | √                | √              |
| InfluxDB 消息存储                   |              | √                | √              |
| OpenTSDB 消息存储                   |              | √                | √              |
| Redis 消息存储                      |              | √                | √              |
| Cassandra 消息存储                  |              | √                | √              |
| **数据桥接**                        |              |                  |                |
| 数据桥接到 MQTT Brokers             | √            | √                | √              |
| 数据桥接到 Kafka                    |              | √                | √              |
| 数据桥接到 Rabbitmq                 |              | √                | √              |
| 数据桥接到 RocketMQ                 |              | √                | √              |
| 数据桥接到 Pulsar                   |              |                  |                |
| **监控中心**                        |              |                  |                |
| Metrics/Counters                    | √            | √                | √              |
| VM Status                           | √            | √                | √              |
| Prometheus                          | √            | √                | √              |
| Dashboard 数据图表                  |              | √                | √              |
| ECC                                 |              | √                | √              |
| **技术支持**                        |              |                  |                |
| 社区支持                            | √            | √                | √              |
| 基础支持                            |              | √                | √              |
| 额外付费支持                        |              | √                | √              |
| 产品培训                            |              | √                | √              |
| **咨询服务**                        |              |                  |                |
| 基础咨询                            |              | √                | √              |
| 架构设计咨询                        |              |                  | √              |
| 项目集成咨询                        |              |                  | √              |
| 客户业务集成定制                    |              |                  | √              |
| 现场服务                            |              |                  | √              |
| **价格**                            |              | 联系商务         | 联系商务       |

