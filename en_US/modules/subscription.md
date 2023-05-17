# Auto Subscription

The auto subscription function of EMQX enables the client to automatically establish the user's preset subscription relationship without sending additional SUBSCRIBE messages when the connection is established.

Currently, EMQX supports:

- [Redis](../rule/get_subs_from_redis.md)
- [MySQL](../rule/get_subs_from_mysql.md)
- [PostgreSQL](../rule/get_subs_from_pgsql.md)
- [Cassandra](../rule/get_subs_from_cassandra.md)
- [MongoDB](../rule/get_subs_from_mongodb.md)
- [ClickHouse](../rule/get_subs_from_clickhouse.md)