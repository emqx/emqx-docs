# 使用 Redis 的密码认证

该认证器实现了密码验证算法，并使用 Redis 数据库作为凭证存储。

## 存储架构

Redis 认证器适用于存储到 Redis [hashes](https://redis.io/docs/manual/data-types/#hashes) 中的凭据，并且使用预定义的字段名称：`password_hash`、`salt`、`is_superuser`。 `password_hash` 字段是必需的，其他字段是可选的。`salt` 字段的缺失被解释为空盐（`salt = ""`）；`is_superuser` 的缺失将被设置为默认值 `false`。

添加用户名为 `user123`、密码为 `secret`、盐值 `salt` 和超级用户标识为 `true` 的用户示例：

```
>redis-cli
127.0.0.1:6379> HSET mqtt:user123 is_superuser 1 salt salt password_hash ac63a624e7074776d677dd61a003b8c803eb11db004d0ec6ae032a5d7c9c5caf
(integer) 3
```

相关的配置参数为：

```
password_hash_algorithm {
    name = sha256
    salt_position = prefix
}

cmd = "HMGET mqtt:${username} password_hash salt is_superuser"
```

::: tip
`password_hash` 这一名称传递了我们对存储散列密码的青睐。但是鉴于 Redis 没有类似 MySQL 的 as 语法，我们保留了对 `password` 的兼容。

所以，我们也可以将 `cmd` 配置为 `HMGET mqtt:${username} password salt is_superuser`。
:::

## 配置

Redis 认证器由 `mechanism = password_based` 和 `backend = redis` 标识。

EMQX 支持 3 种 Redis 部署方式：

- Standalone Redis。

  ```
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

- [Redis Sentinel](https://redis.io/docs/manual/sentinel/)。

  ```
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

- [Redis Cluster](https://redis.io/docs/manual/scaling/)。

  ```
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

### 通用配置参数

#### `redis_type`

可选值为 `single`、 `cluster` 和 `sentinel`，分别对应 Redis 的 3 种部署类型：

1. Standalone Redis
2. [Redis Cluster](https://redis.io/docs/manual/scaling/)
3. [Redis Sentinel](https://redis.io/docs/manual/sentinel/)

#### `password_hash_algorithm`

标准 [密码散列选项](./authn.md#密码散列)。

#### `cmd`

用户凭据的查询命令，支持以下 Redis 命令：

- `HMGET KEY_TEMPLATE ...Fields...`，其中可能的字段是 `password_hash`、`salt`、`is_superuser`。 
- `HGET KEY_TEMPLATE password_hash`。

`KEY_TEMPLATE` 支持 [placeholders](./authn.md#认证占位符)。`password_hash` 是必须的。

#### `database`

必选的整型配置。指定 Redis 数据库的 Index。

#### `password`

用于 Redis [认证](https://redis.io/docs/manual/security/#authentication) 的密码。

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
