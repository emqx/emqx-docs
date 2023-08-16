# 数据集成

数据桥接是用来对接 EMQX 和外部数据系统的通道，比如 MySQL、MongoDB 等数据库， 或 Kafka，RabbitMQ 等消息中间件，或 HTTP 服务器等。

本章详细介绍了 EMQX 目前支持的外部数据系统，如何通过创建桥接连接这些数据系统，如何制定数据转发规则，以及如何对新建的数据桥接和规则进行测试。

目前，EMQX 支持以下数据集成：

- [桥接数据到 MQTT Broker](./bridge_mqtt.md)
- [集成 Kafka](./bridge_kafka.md)
- [集成 InfluxDB](./backend_influxdb.md)
- [集成 InfluxDB V2](./backend_influxdb_v2.md)
- [集成 MySQL](./backend_mysql.md)
- [集成 PostgreSQL](./backend_pgsql.md)
- [集成 Redis](./backend_redis.md)
- [集成 MongoDB](./backend_mongodb.md)
- [集成 HStreamDB](./backend_hstreamdb.md)
- [集成 Pulsar](./bridge_pulsar.md)
- [集成 RabbitMQ](./bridge_rabbitmq.md)
- [集成 RocketMQ](./bridge_rocketmq.md)
- [集成 Lindorm](./backend_lindorm.md)
- [集成 DynamoDB](./backend_dynamodb.md)
- [集成 TDengine](./backend_tdengine.md)
- [集成 TimescaleDB](./backend_timescaledb.md)
- [集成 OpenTSDB](./backend_opentsdb.md)
- [集成 DolphinDB](./backend_dolphindb.md)
- [集成 ClickHouse](./backend_clickhouse.md)
- [集成 Cassandra](./backend_cassandra.md)
- [集成 Oracle DB](./backend_oracle.md)
- [集成 SQL Server](./backend_sqlserver.md)
- [桥接数据到多个 MQTT Broker](./bridge_emqx.md)