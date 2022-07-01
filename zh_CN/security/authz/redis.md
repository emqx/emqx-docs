# Redis

Redis Authorizer 支持客户端的授权规则存储在 Redis 数据库中。

用户应该提供一个 Redis 命令模板，返回一个键值列表，以主题过滤器为键，以允许执行的操作为值（`publish`，`subscribe` 或 `all`）。

例如，规则可以存储为 Redis 的 [hashes](https://redis.io/docs/manual/data-types/#hashes)：

```
>redis-cli
127.0.0.1:6379> HSET users:someuser foo/# subscribe
(integer) 1
127.0.0.1:6379> HSET users:someuser bar/baz publish
(integer) 1
```

对应的配置项为：

```
cmd = "HGET users:${username}"
```

获取到的规则用作许可规则，即如果主题过滤器和操作匹配，则接受请求。

## 配置

Redis authorizer 由 `type=redis` 标识。

EMQX 支持 3 种 Redis 部署方式。

* Standalone Redis.

  ```
  {
      type = redis
      enable = true

      redis_type = single
      server = "127.0.0.1:6379"

      cmd = "HGETALL mqtt_user:${username}"
      database => 1
      password = public
      server = "127.0.0.1:6379"

  }
  ```

* [Redis Sentinel](https://redis.io/docs/manual/sentinel/).

  ```
  {
      type = redis
      enable = true

      redis_type = sentinel
      servers = "10.123.13.11:6379,10.123.13.12:6379"
      sentinel = "mymaster"

      cmd = "HGETALL mqtt_user:${username}"
      database => 1
      password = public

  }
  ```

* [Redis Cluster](https://redis.io/docs/manual/scaling/).

  ```
  {
      type = redis
      enable = true

      redis_type = cluster
      servers = "10.123.13.11:6379,10.123.13.12:6379"

      cmd = "HGETALL mqtt_user:${username}"
      database => 1
      password = public
  }
  ```

### 通用配置参数

#### `redis_type`

可选值为 `single`、 `cluster` 和 `sentinel`，分别对应 Redis 的 3 种部署类型：

1. Standalone Redis
2. [Redis Cluster](https://redis.io/docs/manual/scaling/)
3. [Redis Sentinel](https://redis.io/docs/manual/sentinel/)

#### `cmd`

必选的字符串类型配置项。用于指定查询权限规则的命令，支持使用占位符。主题过滤器中允许使用 [主题占位符](./authz.md#主题占位符)。

#### `database`

必选的整型配置。指定 Redis 数据库的 Index。

#### `password`

可选的字符串类型配置。指定用于 Redis [认证](https://redis.io/docs/manual/security/#authentication) 的密码。

#### `auto_reconnect`

可选的布尔类型配置。默认值为 `true`。指定 EMQX 是否自动重新连接到 Redis。

#### `pool_size`

可选的整型配置。指定从 EMQX 节点到 Redis 的并发连接数。默认值为 8。

#### `ssl`

用于 [安全连接至 Redis](https://redis.io/docs/manual/security/encryption/) 的标准 [SSL options](../ssl.md)。

### Standalone Redis options (`redis_type = single`).

#### `server`

必选的字符串类型配置，格式为 `host:port`，用于指定 Redis 服务端地址。

### Redis Cluster options (`redis_type = cluster`).

#### `servers`

必选的字符串类型配置，格式为 `host1:port1,host2:port2,...`，用于指定 Redis Cluster 端点地址列表。

### Redis Sentinel options (`redis_type = sentinel`).

#### `servers`

必选的字符串类型配置，格式为 `host1:port1,host2:port2,...`，用于指定 Redis Sentinel 端点地址列表。

#### `sentinel`

必选的字符串类型配置。用于指定 Redis Sentinel 配置需要的 [主服务器名称](https://redis.io/docs/manual/sentinel/#configuring-sentinel)。
