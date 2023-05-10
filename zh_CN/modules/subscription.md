# MQTT 代理订阅

EMQX 的代理订阅功能使得客户端在连接建立时，不需要发送额外的 SUBSCRIBE 报文，便能自动建立用户预设的订阅关系。

目前 EMQX 支持从以下数据库中获取用户预设的订阅关系：

- [从 Redis 中获取订阅关系](../rule/get_subs_from_redis.md)
- [从 MySQL 中获取订阅关系](../rule/get_subs_from_mysql.md)
- [从 PostgreSQL 中获取订阅关系](../rule/get_subs_from_pgsql.md)
- [从 Cassandra 中获取订阅关系](../rule/get_subs_from_cassandra.md)
- [从 MongoDB 中获取订阅关系](../rule/get_subs_from_mongodb.md)
- [从 ClickHouse 中获取订阅关系](../rule/get_subs_from_clickhouse.md)