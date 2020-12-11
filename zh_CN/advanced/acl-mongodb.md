---
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---

# MongoDB ACL

MongoDB ACL 使用外部 MongoDB 数据库存储 ACL 规则，可以存储大量数据、动态管理 ACL，方便与外部设备管理系统集成

插件：

```bash
emqx_auth_mongo
```

::: tip 
emqx_auth_mongo 插件同时包含认证功能，可通过注释禁用。
:::


## MongoDB 连接信息

MongoDB 基础连接信息，需要保证集群内所有节点均能访问。

```bash
# etc/plugins/emqx_auth_mongo.conf

## MongoDB 架构类型
##
## Value: single | unknown | sharded | rs
auth.mongo.type = single

## rs 模式需要设置 rs name
## auth.mongo.rs_set_name =

## 服务器列表，集群模式下使用逗号分隔每个服务器
## Examples: 127.0.0.1:27017,127.0.0.2:27017...
auth.mongo.server = 127.0.0.1:27017

auth.mongo.pool = 8

auth.mongo.login =

auth.mongo.password =

## auth.mongo.auth_source = admin

auth.mongo.database = mqtt

auth.mongo.query_timeout = 5s

## SSL 选项
# auth.mongo.ssl = false

## auth.mongo.ssl_opts.keyfile =

## auth.mongo.ssl_opts.certfile =

## auth.mongo.ssl_opts.cacertfile =

## MongoDB write mode.
##
## Value: unsafe | safe
## auth.mongo.w_mode =

## Mongo read mode.
##
## Value: master | slave_ok
## auth.mongo.r_mode =

## MongoDB 拓扑配置，一般情况下用不到，详见 MongoDB 官网文档
auth.mongo.topology.pool_size = 1
auth.mongo.topology.max_overflow = 0
## auth.mongo.topology.overflow_ttl = 1000
## auth.mongo.topology.overflow_check_period = 1000
## auth.mongo.topology.local_threshold_ms = 1000
## auth.mongo.topology.connect_timeout_ms = 20000
## auth.mongo.topology.socket_timeout_ms = 100
## auth.mongo.topology.server_selection_timeout_ms = 30000
## auth.mongo.topology.wait_queue_timeout_ms = 1000
## auth.mongo.topology.heartbeat_frequency_ms = 10000
## auth.mongo.topology.min_heartbeat_frequency_ms = 1000
```


## 默认数据结构

MongoDB 认证默认配置下需要确保数据库中有如下集合：

### 认证/超级集合

```sql
{
  username: "user",
  password: "password hash",
  salt: "password salt",
  is_superuser: false,
  created: "2020-02-20 12:12:14"
}
```

示例数据：

```bash
use mqtt

db.mqtt_user.insert({
  "username": "emqx",
  "password": "efa1f375d76194fa51a3556a97e641e61685f914d446979da50a551a4333ffd7",
  "is_superuser": false,
  "salt": ""
})
```

### ACL 规则集合

```json
{
    username: "username",
    clientid: "clientid",
    publish: ["topic1", "topic2", ...],
    subscribe: ["subtop1", "subtop2", ...],
    pubsub: ["topic/#", "topic1", ...]
}
```

MongoDB ACL 一条规则中定义了发布、订阅和发布/订阅的信息，在规则中的都是**允许**列表。

规则字段说明：

- username：连接客户端的用户名
- clientid：连接客户端的 Client ID
- publish：允许发布的主题数值，支持通配符
- subscribe：允许订阅的主题数值，支持通配符
- pubsub：允许发布订阅的主题数值，支持通配符

::: tip 
主题可以使用通配符，并且可以在主题中加入占位符来匹配客户端信息，例如 `t/%c` 则在匹配时主题将会替换为当前客户端的 Client ID
  - %u：用户名
  - %c：Client ID
::: 


默认配置下示例数据：

```bash
use mqtt

## 所有用户不可以订阅、发布系统主题
## 允许客户端订阅包含自身 Client ID 的 /smarthome/${clientid}/temperature 主题
db.mqtt_acl.insert({
  username: "$all",
  clientid: "$all",
  publish: ["#"],
  subscribe: ["/smarthome/%c/temperature"]
})
```

启用 MongoDB ACL 后并以用户名 emqx 成功连接后，客户端应当数据具有相应的主题权限。


## 超级用户查询（super_query）

进行 ACL 鉴权时，EMQ X 将使用当前客户端信息填充并执行用户配置的超级用户查询，查询客户端是否为超级用户。客户端为超级用户时将跳过 ACL 查询。

```bash
# etc/plugins/emqx_auth_mongo.conf

## 启用超级用户
auth.mongo.super_query = on

## 超级查询使用集合
auth.mongo.super_query.collection = mqtt_user

## 超级用户使用字段
auth.mongo.super_query.super_field = is_superuser

## 超级用户查询选择器，多个条件使用逗号分隔
#auth.mongo.super_query.selector = username=%u, clientid=$all
auth.mongo.super_query.selector = username=%u
```

同一个**选择器**的多个条件时实际查询中使用 MongoDB `and` 查询：

```bash
db.mqtt_user.find({ 
  "username": "wivwiv"
  "clientid": "$all"
})
```

你可以在查询条件中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名
- %c：Client ID

你可以根据业务需要调整超级用户查询，如添加多个查询条件、使用数据库预处理函数，以实现更多业务相关的功能。但是任何情况下超级用户查询需要满足以下条件：

1. 查询结果中必须包含 is_superuser 字段，is_superuser 应该显式的为 true


::: tip 
如果不需要超级用户功能，注释并禁用该选项能有效提高效率
:::


## ACL 查询（acl_query）

进行 ACL 鉴权时，EMQ X 将使用当前客户端信息填充并执行用户配置的超级用户查询，如果没有启用超级用户查询或客户端不是超级用户，则使用 ACL 查询 查询出该客户端在数据库中的 ACL 规则。

```bash
# etc/plugins/emqx_auth_mongo.conf

auth.mongo.acl_query = on

auth.mongo.acl_query.collection = mqtt_acl

## 查询选择器，多个条件时使用逗号分隔
## auth.mongo.acl_query.selector = username=%u,clientid=%c
auth.mongo.acl_query.selector = username=%u

## 使用多个查询选择器
## auth.mongo.acl_query.selector.1 = username=$all
## auth.mongo.acl_query.selector.2 = username=%u
```

同一个选择器的多个**条件**时实际查询中使用 MongoDB `and` 查询：

```bash
db.mqtt_acl.find({ 
  "username": "emqx"
  "clientid": "$all"
})
```

多个**选择器**时实际查询中使用 MongoDB `or` 查询：

```bash
db.mqtt_acl.find({
  "$or": [
    {
      "username": "$all"
    },
    {
      "username": "emqx"
    }
  ]
})
```


你可以在 ACL 查询中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名
- %c：Client ID


::: danger 
MongoDB ACL 规则需严格使用上述数据结构。
MongoDB ACL 中添加的所有规则都是 允许 规则，可以搭配 `etc/emqx.conf` 中 `acl_nomatch = deny` 使用。
:::
