# MongoDB 认证

MongoDB 认证使用外部 MongoDB 数据库作为认证数据源，可以存储大量数据，同时方便与外部设备管理系统集成。

插件：

```bash
emqx_auth_mongo
```

::: tip 
emqx_auth_mongo 插件同时包含 ACL 功能，可通过注释禁用。
:::



要启用 MongoDB 认证，需要在 `etc/plugins/emqx_auth_mongo.conf` 中配置以下内容：

## MongoDB 连接信息

MongoDB 基础连接信息，需要保证集群内所有节点均能访问。

```bash
# etc/plugins/emqx_auth_mongo.conf

## MongoDB 部署类型
##
## Value: single | unknown | sharded | rs
auth.mongo.type = single

## 是否启用 SRV 和 TXT 记录解析
auth.mongo.srv_record = false

## 如果您的 MongoDB 以副本集方式部署，则需要指定相应的副本集名称
##
## 如果启用了 SRV 记录，即 auth.mongo.srv_record 设置为 true，
## 且您的 MongoDB 服务器域名添加了包含 replicaSet 选项的 DNS TXT 记录，
## 那么可以忽略此配置项
## auth.mongo.rs_set_name =

## MongoDB 服务器地址列表
##
## 如果你的 URI 具有以下格式：
## mongodb://[username:password@]host1[:port1][,...hostN[:portN]][/[defaultauthdb][?options]]
## 请将 auth.mongo.server 配置为 host1[:port1][,...hostN[:portN]]
##
## 如果你的 URI 具有以下格式：
## mongodb+srv://server.example.com
## 请将 auth.mongo.server 配置为 server.example.com，并将 srv_record
## 设置为 true，EMQX 将自动查询 SRV 和 TXT 记录以获取服务列表和 replicaSet 等选项
##
## 现已支持 IPv6 和域名
auth.mongo.server = 127.0.0.1:27017

auth.mongo.pool = 8

auth.mongo.login =

auth.mongo.password =

## 指定用于授权的数据库，没有指定时默认为 admin
##
## 如果启用了 SRV 记录，即 auth.mongo.srv_record 设置为 true，
## 且您的 MongoDB 服务器域名添加了包含 authSource 选项的 DNS TXT 记录，
## 那么可以忽略此配置项
## auth.mongo.auth_source = admin

auth.mongo.database = mqtt

auth.mongo.query_timeout = 5s

## SSL 选项
# auth.mongo.ssl = false

## auth.mongo.ssl_opts.keyfile =

## auth.mongo.ssl_opts.certfile =

## auth.mongo.ssl_opts.cacertfile =

## MongoDB 写模式
##
## 可设置为 unsafe 或 safe。设置为 safe 时会等待 MongoDB Server 的响应并返回给调用者。未指定时将使用默认值 unsafe。
## auth.mongo.w_mode =

## MongoDB 读模式
##
## 可设置为 master 或 slave_ok，设置为 master 时表示每次查询都将从主节点读取最新数据。未指定时将使用默认值 master。
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

```json
{
  username: "user",
  password: "password hash",
  salt: "password salt",
  is_superuser: false,
  created: "2020-02-20 12:12:14"
}
```

默认配置下示例数据如下：

```bash
use mqtt

db.mqtt_user.insert({
  "username": "emqx",
  "password": "efa1f375d76194fa51a3556a97e641e61685f914d446979da50a551a4333ffd7",
  "is_superuser": false,
  "salt": ""
})
```

启用 MongoDB 认证后，你可以通过用户名： emqx，密码：public 连接。


::: tip 
这是默认配置使用的集合结构，熟悉该插件的使用后你可以使用任何满足条件的集合进行认证。
:::



## 加盐规则与哈希方法

MongoDB 认证支持配置[加盐规则与哈希方法](./auth.md#加盐规则与哈希方法)：

```bash
# etc/plugins/emqx_auth_mongo.conf

auth.mongo.password_hash = sha256
```


## 认证查询（auth_selector）

进行身份认证时，EMQX 将使用当前客户端信息填充并执行用户配置的认证 SQL，查询出该客户端在数据库中的认证数据。

MongoDB 支持配置集合名称、密码字段、selector 命令

```bash
# etc/plugins/emqx_auth_mongo.conf

auth.mongo.auth_query.collection = mqtt_user

## 如果启用了加盐处理，此处需配置为 password,salt
## Value:  password | password,salt
auth.mongo.auth_query.password_field = password

auth.mongo.auth_query.selector = username=%u
```

你可以在认证查询（selector）中使用以下占位符，执行时 EMQX 将自动填充为客户端信息：

- %u：用户名
- %c：Client ID
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效


你可以根据业务需要调整认证查询，如添加多个查询条件、使用数据库预处理函数，以实现更多业务相关的功能。但是任何情况下认证查询需要满足以下条件：

1. 查询结果中必须包含 password 字段，EMQX 使用该字段与客户端密码比对
2. 如果启用了加盐配置，查询结果中必须包含 salt 字段，EMQX 使用该字段作为 salt（盐）值
3. MongoDB 使用 findOne 查询命令，确保你期望的查询结果能够出现在第一条数据中
