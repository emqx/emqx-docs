# 简介

MQTT鉴权（authorization）是指对MQTT客户端的发布和订阅操作进行 _权限控制_。
控制的内容主要是哪些客户端可以发布或者订阅哪些MQTT主题。

EMQX支持集中类型的鉴权。
* 权限列表（亦即 _ACL_）。可以从例如MongoDB， MySQL，PostgreSQL，Redis，或者EMQX的内置数据库中读取这个列表。
* 加载一个包含全局的ACL的文件。
* 动态访问一个HTTP后端服务，并通过该HTTP调用的返回值来客户端是否有访问的权限。
* 通过提取认证过程中携带的鉴权数据，例如JWT的某个字段。

## 鉴权数据源

_鉴权数据源_ （或称 _鉴权器_） 是EMQX中实现权限空值的一个模块。
每个控制器都有一个类型，EMQX内部也把它用作唯一的标识符。

EMQX默认提供如下这些鉴权器：

| 类型（ID）| 描述 |
| ---- | --- |
| built_in_database  | [使用内置数据库存鉴权数据](./mnesia.md) |
| mysql              | [使用MySQL存放鉴权数据](./mysql.md) |
| postgresql         | [使用PostgreSQL存放鉴权数据](./postgresql.md) |
| mongodb            | [使用MongoDB存放鉴权数据](./mongodb.md) |
| redis              | [使用Redis存放鉴权数据](./redis.md) |
| http               | [通过访问外部HTTP服务来获取鉴权信息](./http.md) |
| file               | [将鉴权信息存放在文件中](./file.md) |

每个控制器都有自己的配置。

例如，下面是一个MySQL的例子：

```
{
    enable => true

    type = mysql
    database = "mqtt"
    username = "root"
    password = "public"

    query = "SELECT permission, action, topic FROM acl WHERE username = ${username}"
    server = "10.12.43.12:3306"
}
```

## 鉴权链

一组鉴权器，可以组成一个鉴权链。当一个客户端进行发布或者订阅操作的时候，EMQ会按顺序逐个检查，
如果一个鉴权器能够找到该客户端的鉴权规则（ACL），那么就会对这个规则进行匹配。
匹配的结果要么是允许（allow），要么是拒绝（deny）。如果没有找到适用于该客户端的规则，
那么就会继续到鉴权链中的下一个鉴权器进行检查。

如果整个链都没能找到适用于该客户端的规则（ACL），那么就使用默认规则。

跟[认证链](../authn/authn.md#authentication-chains)不太一样的地方是：鉴权链是全局唯一的。

## 认证器提供的鉴权规则

有些认证器的认证结果中，有携带鉴权数据的能力，当这个数据存在的时候，这些规则的检查会发生在全局鉴权链之前。
例如，[JWT](../authn/jwt.md#jwt-authorization)。

## 鉴权数据的缓存

如果一个客户端大量的发送请求，就会对鉴权数据后端产生访问压力。
因此EMQX为鉴权数据引入了缓存。

::: tip Tip
缓存的配置参数会大大影响系统性能，所以调整缓存参数变得非常重要。请参考文档下文中的配置章节。
:::

## 规则占位符

多数鉴权器支持在鉴权规则中使用占位符。
占位符就像是一个模版，在运行时根据客户端信息进行替换，得到真正使用的鉴权规则。

### 鉴权器配置中的占位符

配置中的占位符会在运行时进行替换并使用在后端数据库或服务的链接或者请求中。
下面是EMQX支持的占位符：
* `${clientid}` — 客户端的 clientid。
* `${username}` — 客户端的用户名（username）。
* `${peerhost}` — 客户端的源IP地址。
* `${cert_subject}` — 客户端x.509证书中的主题名（Subject）。
* `${cert_common_name}` 客户端x.509证书中的通用名（Common Name）.

Redis 鉴权器配置中使用占位符的例子：

```
{
  enable = true
  type = redis

  ... other parameters ...

  cmd = "HGETALL mqtt_user:${username}"
}
```

### 鉴权规则中的占位符

在鉴权器后端返回的规则中，MQTT主题是字符串格式。这些字符串会被当作模版进行占位符的替换。
在鉴权规则中可以使用的占位符如下：

* `${clientid}` — MQTT客户端的Client ID。当在规则中使用Client ID，这个ID应由客户端在连接EMQX前就指定，而不是让EMQX随机生成。
* `${username}` — 使用客户端用户名 对规则进行替换。

占位符只能用于替换主题的整个字段，例如 `a/b/${username}/c/d`，但是不能用于替换字段的一部分，例如 `a/b${username}c/d`。

为了避免占位符跟想要的主题冲突的问题，EMQX 5.0中引入了一个 `eq` 语法，例如 `eq a/b/${username}/c/d`。
这样规则将会不做替换，而是保持MQTT主题 `a/b/${username}/c/d` 不变。

## 配置结构

鉴权配置结构大致如下。

```
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

### `sources`

带顺序的数组（非必需字段）。每个数组元素是一个鉴权器的数据源相关配置。
数组中的每个元素都有一个 `enable` 字段，可用于快速启停切换。
如果该配置项缺失，则当作空链处理。

每个鉴权器的详细配置，可以参考相应的配置文件文档。

### `no_match`

可选值，可以设置为 `allow` 或 `deny`。缺省值是 `allow`。
当EMQX无法从认证链中为某个客户端匹配到任何规则时候，将会使用这个默认的规则。

### `deny_action`

可选值，可以设置为 `ignore` 或 `disconnect`。默认值是 `ignore`。
这个配置用于指定当拒绝访问发生时，应该如何对待这个客户端的MQTT链接。
如果配置成 `ignore`，那么这个操作会被丢弃，例如，如果是一个发布动作，那么这消息会被丢弃；如果是一个订阅操作，那么订阅请求会被拒绝。
如果配置成 `disconnect`，那么这个客户端将会被断开连接。

### `cache`

ACL缓存的配置。

* `cache.enable` — 默认是 `true`。为ACL开启缓存。如果仅使用认证 JWT 中提供 鉴权信息，这建议关闭缓存。
* `cache.max_size` — 默认值 `32`。此配置规定每个客户端允许缓存的ACL规则数量。当超过上限时，老的记录将会被删掉。
* `cache.ttl` — 默认 `1m`（一分钟）。 该配置规定ACL规则缓存有效时间。

## 使用API对鉴权参数进行配置

EMQX 为鉴权参数暴露如下REST API来支持进行运行时动态修改。

* `/api/v5/authorization/settings` — 对通用鉴权参数进行修改，例如 `no_match`， `deny_action`，和 `cache`；
* `/api/v5/authorization/sources` — 用于管理鉴权链；
* `/api/v5/authorization/cache` — 用于强制清除鉴权数据缓存；
* `/api/v5/authorization/sources/built_in_database` — 用于动态增加或删除内置数据库的条目。

详细的API文档，请参考 `/api-docs/index.html`，例如本地部署时：http://localhost:18083/api-docs/index.html