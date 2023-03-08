# 密码认证

EMQX 支持通过密码进行身份验证，这种最简单，也是使用最多的认证方式。此时，客户端需要提供能够表明身份的凭据，例如用户名、客户端 ID 以及对应的密码并将其存储在特定数据源（数据库）中。

除简单便捷的内置数据库外，EMQX 还支持通过与多类后端数据库的集成提供密码认证，包括 MySQL、PostgreSQL、MongoDB 和 Redis。

- [内置数据库](./mnesia.md)
- [集成 MySQL](./mysql.md)
- [集成 PostgreSQL](./postgresql.md)
- [集成 Redis](./redis.md)
- [集成 MongoDB](./mongodb.md)
- [使用 HTTP 服务](./http.md)
