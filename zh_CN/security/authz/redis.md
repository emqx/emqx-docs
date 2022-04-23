# Redis ACL


Redis ACL 使用外部 Redis 数据库存储 ACL 规则，可以存储大量数据、动态管理 ACL，方便与外部设备管理系统集成

插件：

```bash
emqx_auth_redis
```

::: tip
emqx_auth_redis 插件同时包含认证功能，可通过注释禁用。
:::


## Redis 连接信息

Redis 基础连接信息，需要保证集群内所有节点均能访问。

```bash
# etc/plugins/emqx_auth_redis.conf

## 服务器地址
auth.redis.server = 127.0.0.1:6379

## 连接池大小
auth.redis.pool = 8

auth.redis.database = 0

auth.redis.password =
```


## 默认数据结构

Redis 认证插件默认配置下使用哈希表存储认证数据，使用 `mqtt_acl:` 作为 Redis 键前缀。

### ACL 规则数据

```bash
## 格式
HSET mqtt_acl:[username clientid] [topic] [access]

## 结构
redis> hgetall mqtt_acl:emqx
  testtopic/1 1
```

Redis ACL 一条规则中定义了发布、订阅或发布/订阅的信息，在规则中的都是**允许**列表，即白名单规则。
对应主题有权且时将直接

规则字段说明：

  - %u：用户名
  - %c：Client ID

默认配置下示例数据：

```bash
HSET mqtt_acl:emqx # 1
HSET mqtt_acl:testtopic/2 2
```

启用 Redis ACL 后并以用户名 emqx 成功连接后，客户端应当数据具有相应的主题权限。



## 超级用户查询命令（super cmd）

进行 ACL 鉴权时，EMQX 将使用当前客户端信息填充并执行用户配置的超级用户查询命令，查询客户端是否为超级用户。客户端为超级用户时将跳过 ACL 查询命令。

```bash
# etc/plugins/emqx_auth_redis.conf

auth.redis.super_cmd = HGET mqtt_user:%u is_superuser
```

你可以在命令中使用以下占位符，执行时 EMQX 将自动填充为客户端信息：

- %u：用户名
- %c：Client ID
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效


你可以根据业务需要调整超级用户查询命令，如添加多个查询条件、使用数据库预处理函数，以实现更多业务相关的功能。但是任何情况下超级用户查询命令需要满足以下条件：

1. 查询结果中第一个数据必须为 is_superuser 数据


::: tip
如果不需要超级用户功能，注释并禁用该选项能有效提高效率
:::


## ACL 查询命令（acl cmd）

进行 ACL 鉴权时，EMQX 将使用当前客户端信息填充并执行用户配置的超级用户 SQL，如果没有启用超级用户 SQL 或客户端不是超级用户，则使用 ACL 查询命令查询出该客户端在数据库中的 ACL 规则。

```bash
# etc/plugins/emqx_auth_redis.conf

auth.redis.acl_cmd = HGETALL mqtt_acl:%u
```

你可以在 ACL 查询命令中使用以下占位符，执行时 EMQX 将自动填充为客户端信息：

- %u：用户名
- %c：Client ID

你可以根据业务需要调整 ACL 查询命令，但是任何情况下 ACL 查询命令需要满足以下条件：

1. 哈希中使用 topic 作为键，access 作为值


::: tip
Redis ACL 规则需严格使用上述数据结构。

Redis ACL 中添加的所有规则都是 **允许** 规则。即白名单。

Redis 中某个客户端的规则列表为空时将交由下一个 acl 插件继续检查，否则将立即终止认证链并返回认证结果。
规则非空且未匹配到相应的 pub/sub 权限时，将返回认证失败（拒绝相应的 pub/sub 行为）并终止认证链。

同时启用多个 auth/ACL 插件时，建议将 Redis ACL 认证置于其他启用的 auth/ACL 插件后。
:::
