# 简介

EMQX 授权（authorization）是指对 MQTT 客户端的发布和订阅操作进行权限控制。

授权机制在 EMQX 中的基本运作原理为：在客户端发布或订阅时，EMQX 将使用特定的流程或用户指定的查询语句从数据源中查询该客户端的权限列表，与当前操作进行匹配并根据匹配结果允许或拒绝当前操作。

客户端权限列表需要提前存储到特定数据源（数据库、文件）中，更新对应的数据即可实现权限的运行时动态更新。

## 授权数据源

EMQX 授权支持与多种数据源集成，包括文件、MySQL、PostgreSQL、MongoDB 和 Redis。同时 EMQX 也支持权限列表存储到内置数据库（基于 Mnesia）中，这种方式提供了非常简单的配置流程和数据管理接口。用户可以通过 REST API 或 Dashboard 管理权限数据，或从 CSV 或 JSON 文件批量导入数据。

除此之外，EMQX 还可以通过 HTTP 方式对接用户自己开发的服务，借此实现更复杂的授权逻辑。

按照数据源来划分，EMQX 支持以下 7 种不同类型的授权检查器：

| 数据源                      | 描述                                            |
| --------------------------- | ----------------------------------------------- |
| built内置数据库_in_database | [使用内置数据库存授权数据](./mnesia.md)         |
| MySQL                       | [使用 MySQL 存放授权数据](./mysql.md)             |
| PostgreSQL                  | [使用 PostgreSQL 存放授权数据](./postgresql.md)   |
| MongoDB                     | [使用 MongoDB 存放授权数据](./mongodb.md)         |
| Redis                       | [使用 Redis 存放授权数据](./redis.md)             |
| HTTP Server                 | [通过访问外部 HTTP 服务来获取授权信息](./http.md) |
| File                        | [将授权信息存放在文件中](./file.md)             |

每个授权检查器都有自己的配置。

以下是 MySQL 授权检查器的例子：

```hocon
{
    enable = true

    type = mysql
    database = "mqtt"
    username = "root"
    password = "public"

    query = "SELECT permission, action, topic FROM acl WHERE username = ${username}"
    server = "127.0.0.1:3306"
}
```

## 授权链

EMQX 允许创建多个授权检查器构成一条认证链，授权检查器将按照在链中的位置顺序执行，如果在当前授权检查器中未检索到权限数据，将会切换至链上的下一个启用的授权检查器继续权限检查。

通常这会产生以下 3 种情况：

1. 当前授权检查器执行时检索到了客户端的权限信息，匹配当前执行的操作与客户端的权限信息：
   1. 操作与权限匹配，根据权限允许或拒绝客户端的操作；
   2. 操作与权限不匹配，交由下一授权检查器继续检查。
2. 当前授权检查器执行时没有检索到了客户端的权限信息，例如数据源中没有查找到数据：
   1. 授权链中还有授权检查器：忽略检查，交由下一授权检查器继续检查；
   2. 当前授权检查器是链中最后一个授权检查器：根据 `no_match` 配置决定检查结果（允许或拒绝）。

跟[认证链](../authn/authn.md#认证链)不太一样的地方是：授权链是全局唯一的。

## 授权数据的缓存

大量的客户端订阅和发布将会对授权数据后端产生访问压力，因此 EMQX 引入了缓存机制。

::: tip
缓存的配置参数会大大影响系统性能，所以调整缓存参数变得非常重要。请参考文档下文中的配置章节。
:::

## 授权检查优先级

除了缓存与授权检查器之外，授权结果还可能受到认证阶段设置的[超级用户角色与权限](../authn/authn.md#超级用户与权限)影响。

如果客户端在认证阶段设置了超级用户角色，则发布订阅操作不会再触发授权检查；如果设置了权限列表，则优先匹配客户端权限数据。三者匹配优先级如下：

```bash
超级用户 > 权限数据 > 授权检查
```

## 授权占位符

EMQX 允许使用占位符动态构造授权数据查询语句、HTTP 请求，占位符会在授权检查器执行时替换为真实的客户端信息，以构造出与当前客户端匹配的查询语句或 HTTP 请求。

以 MySQL 授权检查器为例，默认的查询 SQL 中使用了 `${username}` 占位符：

```sql
SELECT action, permission, topic FROM mqtt_acl where username = ${username}
```

当用户名为 `emqx_u` 的客户端触发授权检查时，实际执行权限数据查询 SQL 将被替换为：

```sql
SELECT action, permission, topic FROM mqtt_acl where username = 'emqx_u'
```

### 数据查询占位符

在查询权限数据时，EMQX 支持以下占位符：

- `${clientid}`: 将在运行时被替换为客户端 ID。客户端 ID 一般由客户端在 `CONNECT` 报文中显式指定，如果启用了 `use_username_as_clientid` 或 `peer_cert_as_clientid`，则会在连接时被用户名、证书中的字段或证书内容所覆盖。

- `${username}`: 将在运行时被替换为用户名。用户名来自 `CONNECT` 报文中的 `Username` 字段。如果启用了 `peer_cert_as_username`，则会在连接时被证书中的字段或证书内容所覆盖。

- `${password}`: 将在运行时被替换为密码。密码来自 `CONNECT` 报文中的 `Password` 字段。

- `${peerhost}`: 将在运行时被替换为客户端的 IP 地址。EMQX 支持 [Proxy Protocol](http://www.haproxy.org/download/1.8/doc/proxy-protocol.txt)，即使 EMQX 部署在某些 TCP 代理或负载均衡器之后，用户也可以使用此占位符获得真实 IP 地址。

- `${cert_subject}`: 将在运行时被替换为客户端 TLS 证书的主题（Subject），仅适用于 TLS 连接。

- `${cert_common_name}`: 将在运行时被替换为客户端 TLS 证书的通用名称（Common Name），仅适用于 TLS 连接。

### 主题占位符

EMQX 还允许在匹配规则时将占位符用于主题，以实现动态主题。支持的占位符如下：

- `${clientid}`
- `${username}`

占位符只能用于替换主题的整个字段，例如 `a/b/${username}/c/d`，但是不能用于替换字段的一部分，例如 `a/b${username}c/d`。

为了避免占位符跟想要的主题冲突的问题，EMQX 5.0 中引入了一个 `eq` 语法，例如 `eq a/b/${username}/c/d`。
这样规则将会不做替换，而是保持 MQTT 主题 `a/b/${username}/c/d` 不变。

## 配置方式

EMQX 提供了 3 种使用权限的配置方式，分别为：配置文件、HTTP API 和 Dashboard。

### 配置文件

授权配置结构大致如下。

```hocon
authorization {
  sources = [
    { ...   },
    { ...   }
  ]
  no_match = allow
  deny_action = ignore
  cache {
    enable = true
    max_size = 1024
    duration = 1m
  }
}
```

#### sources

带顺序的数组（非必需字段），每个数组元素是一个授权检查器的数据源相关配置。授权检查器详细配置请参考相应的配置文件文档。

#### no_match

可选值为 `allow` 或 `deny`，默认 `allow`。

当当前客户端操作无法匹配到任何规则时候，将会使用这个默认的规则决定允许或拒绝操作。

此配置也是黑名单/白名单模式的开关。

#### deny_action

可选值为 `ignore` 或 `disconnect`，默认 `ignore`。

当当前客户端操作被拒绝时，应该执行的后续操作：

- `ignore`: 当前操作会被丢弃，例如，如果是一个发布动作，那么这消息会被丢弃；如果是一个订阅操作，那么订阅请求会被拒绝。
- `disconnect`: 丢弃当前操作，并将客户端断开连接。

#### cache

授权缓存的配置：

- `cache.enable`: 默认是 `true`，为授权开启缓存。如果仅使用认证中提供权限信息进行权限检查，这建议关闭缓存。
- `cache.max_size`: 默认值 `32`，每个客户端允许缓存的授权结果数量。当超过上限时，老的记录将会被删掉。
- `cache.ttl` — 默认 `1m`（一分钟），授权缓存有效时间。

### HTTP API

EMQX 为授权参数暴露如下 REST API 来支持进行运行时动态修改。

- `/api/v5/authorization/settings`: 查看、修改授权参数，例如 `no_match`， `deny_action` 和 `cache`
- `/api/v5/authorization/sources`: 用于管理授权检查器
- `/api/v5/authorization/cache`: 清除授权数据缓存
- `/api/v5/authorization/sources/built_in_database`: 内饰数据库数据管理

详细的请求方式与参数请参考 [HTTP API](../../admin/api.md)。

### Dashboard

Dashboard 底层调用了 HTTP API，提供了相对更加易用的可视化操作页面。在 Dashboard 中可以方便的设置授权功能参数、查看授权检查器状态、调整认证器在认证链中的位置。
