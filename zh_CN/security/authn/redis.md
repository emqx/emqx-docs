# 使用 Redis 的密码认证

::: tip
先决条件：

- 了解 [EMQX 认证基本概念](../authn/authn.md)
:::

该认证器实现了密码认证，并使用 MySQL 数据库作为数据源。

## 数据结构与查询指令

Redis 认证器支持使用 [Redis hashes](https://redis.io/docs/manual/data-types/#hashes) 存储认证数据，用户需要提供一个查询指令模板，且确保查询结果包含以下字段：

- `password_hash`: 必需，数据库中的明文或散列密码字段
- `salt`: 可选，为空或不存在时视为空盐（`salt = ""`）
- `is_superuser`: 可选，标记当前客户端是否为超级用户，默认为 `false`

添加用户名为 `emqx_u`、密码为 `public`、盐值为 `slat_foo123`、散列方式为 `sha256` 且超级用户标志为 `true` 的用户示例：

```bash
>redis-cli
127.0.0.1:6379> HSET mqtt_user:emqx_u is_superuser 1 salt slat_foo123 password_hash 44edc2d57cde8d79c98145003e105b90a14f1460b79186ea9cfe83942fc5abb5
(integer) 0
```

相关的配置参数为：

```hocon
password_hash_algorithm {
    name = sha256
    salt_position = suffix
}

cmd = "HMGET mqtt_user:${username} password_hash salt is_superuser"
```

::: tip
`password_hash` 这一字段名称直观表明用户应当在数据库中存储散列密码。但鉴于 Redis 没有类似 MySQL 的 `as` 语法，我们保留了 4.x 对 `password` 的兼容。

所以，我们也可以将 `cmd` 配置为 `HMGET mqtt_user:${username} password salt is_superuser`。
:::

## 配置项

此认证器支持 3 种部署模式的 Redis，详细配置请参考 [authn-redis:standalone](../../admin/cfg.md#authn-redis:standalone)、[authn-redis:sentinel](../../admin/cfg.md#authn-redis:sentinel) 与 [authn-redis:cluster](../../admin/cfg.md#authn-redis:cluster)。

Standalone Redis:

```hocon
{
  mechanism = password_based
  backend = redis
  enable = true

  redis_type = single
  server = "127.0.0.1:6379"

  password_hash_algorithm {
      name = sha256
      salt_position = suffix
  }

  cmd = "HMGET mqtt_user:${username} password_hash salt is_superuser"
  database = 1
  password = "public"
  auto_reconnect = true
}
```

[Redis Sentinel](https://redis.io/docs/manual/sentinel/):

```hocon
{
  mechanism = password_based
  backend = redis
  enable = true

  redis_type = sentinel
  servers = "10.123.13.11:6379,10.123.13.12:6379"
  sentinel = "mymaster"

  password_hash_algorithm {
      name = sha256
      salt_position = suffix
  }

  cmd = "HMGET mqtt_user:${username} password_hash salt is_superuser"
  database = 1
  password = "public"
  auto_reconnect = true
}
```

[Redis Cluster](https://redis.io/docs/manual/scaling/):

```hocon
{
  mechanism = password_based
  backend = redis
  enable = true

  redis_type = cluster
  servers = "10.123.13.11:6379,10.123.13.12:6379"

  password_hash_algorithm {
      name = sha256
      salt_position = suffix
  }

  cmd = "HMGET mqtt_user:${username} password_hash salt is_superuser"
  database = 1
  password = "public"
  auto_reconnect = true
}
```

### redis_type

可选值为 `single`、 `cluster` 和 `sentinel`，分别对应 Redis 的 3 种部署类型：

1. Standalone Redis
2. [Redis Cluster](https://redis.io/docs/manual/scaling/)
3. [Redis Sentinel](https://redis.io/docs/manual/sentinel/)

### password_hash_algorithm

标准 [密码散列选项](./authn.md#密码散列)。

### cmd

用户凭据的查询命令，支持以下 Redis 命令：

- `HMGET KEY_TEMPLATE ...Fields...`，其中可能的字段是 `password_hash`、`salt`、`is_superuser`。
- `HGET KEY_TEMPLATE password_hash`。

`KEY_TEMPLATE` 支持 [placeholders](./authn.md#认证占位符)。`password_hash` 是必须的。

### database

必选的整型配置。指定 Redis 数据库的 Index。

### password

用于 Redis [认证](https://redis.io/docs/manual/security/#authentication) 的密码。

### auto_reconnect

可选的布尔类型配置。默认值为 `true`。指定 EMQX 是否自动重新连接到 Redis。

### pool_size

可选的整型配置。指定从 EMQX 节点到 Redis 的并发连接数。默认值为 8。

### ssl

用于 [安全连接至 Redis](https://redis.io/docs/manual/security/encryption/) 的标准 [SSL options](../ssl.md)。

### Standalone Redis options (`redis_type = single`).

#### server

必选的字符串类型配置，格式为 `host:port`，用于指定 Redis 服务端地址。

#### Redis Cluster options (`redis_type = cluster`).

#### servers

必选的字符串类型配置，格式为 `host1:port1,host2:port2,...`，用于指定 Redis Cluster 端点地址列表。

#### Redis Sentinel options (`redis_type = sentinel`).

#### servers

必选的字符串类型配置，格式为 `host1:port1,host2:port2,...`，用于指定 Redis Sentinel 端点地址列表。

#### sentinel

必选的字符串类型配置。用于指定 Redis Sentinel 配置需要的 [主服务器名称](https://redis.io/docs/manual/sentinel/#configuring-sentinel)。
