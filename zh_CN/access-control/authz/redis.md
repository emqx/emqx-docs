# 基于 Redis 进行授权

Redis Authorizer 支持客户端的权限列表存储在 Redis 数据库中。

::: tip 前置准备

熟悉 [EMQX 授权基本概念](./authz.md)
:::

## 数据结构与查询指令

Redis 认证器支持使用 [Redis hashes](https://redis.io/docs/manual/data-types/#hashes) 存储权限数据，用户需要提供一个查询指令模板，且确保查询结果包含以下数据：

- `topic`: 用于指定当前规则适用的主题，可以使用主题过滤器和[主题占位符](./authz.md#主题占位符)。
- `action`: 用于指定当前规则适用于哪些操作，可选值有 `publish`、`subscribe` 和 `all`。
- `qos`:（可选）用于指定当前规则适用的消息 QoS，可选值有 `0`、`1`、`2`，也可以使用 Number 数组同时指定多个 QoS。默认为所有 QoS。
- `retain`: （可选）用于指定当前规则是否支持发布保留消息，可选值有 `true`、`false`，默认允许保留消息。

:::
从 EMQX v5.1.1 版本开始引入了 `qos` 和 `retain` 字段。
:::

添加用户名为 `emqx_u`，允许订阅 `t/1` 主题的权限数据：

```bash
HSET mqtt_acl:emqx_u t/1 subscribe
```

由于 Redis 结构限制，使用 `qos` 与 `retain` 字段时，需要将除 `topic` 外的信息放到 JSON 字符串中，例如：

- 添加用户名为 `emqx_u`，允许以 QoS1 和 QoS2 订阅 `t/2` 主题的权限数据：

```bash
HSET mqtt_acl:emqx_u t/2 '{ "action": "subscribe", "qos": [1, 2] }'
```

- 添加用户名为 `emqx_u`，拒绝向 `t/3` 主题发布保留消息的权限数据：

```bash
HSET mqtt_acl:emqx_u t/3 '{ "action": "publish", "retain": false }'
```

对应的配置项为：

```bash
cmd = "HGETALL mqtt_acl:${username}"
```

::: tip
Redis Authorizer 中添加的所有规则都是**允许**规则，即 Redis Authorizer 需要在白名单模式下使用。
:::

## 配置项

Redis Authorizer 支持 3 种部署模式的 Redis，详细配置请参考 [redis_standalone](../../configuration/configuration-manual.html#authz:redis_standalone)、[authz:redis_sentinel](../../configuration/configuration-manual.html#authz:redis_sentinel) 与 [authz:redis_cluster](../../configuration/configuration-manual.html#authz:redis_cluster)。

Redis authorizer 由 `type=redis` 标识。

Standalone Redis:

```hcl
{
    type = redis
    enable = true

    redis_type = single
    server = "127.0.0.1:6379"

    cmd = "HGETALL mqtt_user:${username}"
    database = 1
    password = public
    server = "127.0.0.1:6379"

}
```

[Redis Sentinel](https://redis.io/docs/manual/sentinel/):

```hcl
{
    type = redis
    enable = true

    redis_type = sentinel
    servers = "10.123.13.11:6379,10.123.13.12:6379"
    sentinel = "mymaster"

    cmd = "HGETALL mqtt_user:${username}"
    database = 1
    password = public

}
```

[Redis Cluster](https://redis.io/docs/manual/scaling/):

```hcl
{
    type = redis
    enable = true

    redis_type = cluster
    servers = "10.123.13.11:6379,10.123.13.12:6379"

    cmd = "HGETALL mqtt_user:${username}"
    password = public
}
```

### redis_type

可选值为 `single`、 `cluster` 和 `sentinel`，分别对应 Redis 的 3 种部署类型：

1. Standalone Redis
2. [Redis Cluster](https://redis.io/docs/manual/scaling/)
3. [Redis Sentinel](https://redis.io/docs/manual/sentinel/)

### cmd

必选的字符串类型配置项。用于指定查询权限规则的命令，支持使用占位符。主题过滤器中允许使用 [主题占位符](./authz.md#主题占位符)。

### database

必选的整型配置。指定 Redis 数据库的 Index。

### password

可选的字符串类型配置。指定用于 Redis [认证](https://redis.io/docs/manual/security/#authentication) 的密码。

### auto_reconnect

可选的布尔类型配置。默认值为 `true`。指定 EMQX 是否自动重新连接到 Redis。

### pool_size

可选的整型配置。指定从 EMQX 节点到 Redis 的并发连接数。默认值为 8。

### ssl

用于 [安全连接至 Redis](https://redis.io/docs/manual/security/encryption/) 的标准 [SSL options](../../configuration/configuration.md#tls-ciphers)。

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
