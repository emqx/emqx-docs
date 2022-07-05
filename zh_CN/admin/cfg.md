# 配置文件

<!--5.0.2-bb854a96-->
EMQX的配置文件格式是 [HOCON](https://github.com/emqx/hocon) 。
HOCON（Human-Optimized Config Object Notation）是一个JSON的超集，非常适用于易于人类读写的配置数据存储。

## 分层结构

EMQX的配置文件可分为三层，自底向上依次是：

1. 不可变的基础层 `emqx.conf` 加上 `EMQX_` 前缀的环境变量。</br>
   修改这一层的配置之后，需要重启节点来使之生效。
1. 集群范围重载层：`$EMQX_NODE__DATA_DIR/configs/cluster-override.conf`
1. 节点本地重载层：`$EMQX_NODE__DATA_DIR/configs/local-override.conf`

如果环境变量 `$EMQX_NODE__DATA_DIR` 没有设置，那么该目录会从 emqx.conf 的 `node.data_dir`配置中读取。

配置文件 `cluster-override.conf` 的内容会在运行时被EMQX重写。
这些重写发生在 dashboard UI，管理HTTP API，或者CLI对集群配置进行修改时。
当EMQX运行在集群中时，一个EMQX节点重启之后，会从集群中其他节点复制该文件内容到本地。

:::tip Tip
有些配置项是不能被重载的（例如 `node.name`）.
配置项如果有 `mapping: path.to.boot.config.key` 这个属性，
则不能被添加到重载文件中 `*-override.conf` 中。
:::

更多的重载规则，请参考下文 [配置重载规则](#配置重载规则)。

## 配置文件语法

在配置文件中，值可以被记为类似JSON的对象，例如

```
node {
    name = "emqx@127.0.0.1"
    cookie = "mysecret"
}
```

另一种等价的表示方法是扁平的，例如

```
node.name = "127.0.0.1"
node.cookie = "mysecret"
```

这种扁平格式几乎与EMQX的配置文件格式向后兼容
在4.x系列中（所谓的'cuttlefish'格式）。

它并不是完全兼容，因为HOCON经常要求字符串两端加上引号。
而cuttlefish把`=`符右边的所有字符都视为值。

例如，cuttlefish：`node.name = emqx@127.0.0.1`，HOCON：`node.name = "emqx@127.0.0.1"`。

没有特殊字符的字符串在HOCON中也可以不加引号。
例如：`foo`，`foo_bar`和`foo_bar_1`。

关于更多的HOCON语法，请参考[规范](https://github.com/lightbend/config/blob/main/HOCON.md)

## Schema

为了使HOCON对象类型安全，EMQX为它引入了一个schema。
该schema定义了数据类型，以及数据字段的名称和元数据，用于配置值的类型检查等等。

::: tip Tip
当前阅读到配置文件的文档本身就是由模式元数据生成的。
:::

### 复杂数据类型

EMQX的配置文件中，有4种复杂数据结构类型，它们分别是：

1. Struct：结构体都是有类型名称的，结构体中可以有任意多个字段。
   结构体和字段的名称由不带特殊字符的全小些字母组成，名称中可以带数字，但不得以数字开头，多个单词可用下划线分隔。
1. Map: Map与Struct（结构体）类似，但是内部的字段不是预先定义好的。
1. Union: 联合 `MemberType1 | MemberType2 | ...`，可以理解为：“不是这个，就是那个”
1. Array: 数组 `[ElementType]`

::: tip Tip
如果Map的字段名称是纯数字，它会被解释成一个数组。
例如
```
myarray.1 = 74
myarray.2 = 75
```
会被解析成 `myarray = [74, 75]`。这个用法在重载数组元素的值时候非常有用。
:::

### 原始数据类型

复杂类型定义了数据 "盒子"，其中可能包含其他复杂数据或原始值。
有很多不同的原始类型，仅举几个例子。

* 原子 `atom()`
* 布尔 `boolean()`
* 字符串 `string()`
* 整形 `integer()`
* 浮点数 `float()`
* 数值 `number()`
* 二进制编码的字符串 `binary()` # 是 `string()` 的另一种格式
* 时间间隔 `emqx_schema:duration()` # 时间间隔，是 `integer()` 的另一种格式
* ...

::: tip Tip
原始类型的名称大多是自我描述的，所以不需要过多的注释。
但是有一些不是那么直观的数据类型，则需要配合字段的描述文档进行理解。
:::


### 配置路径

如果我们把EMQX的配置值理解成一个类似目录树的结构，那么类似于文件系统中使用斜杠或反斜杠进行层级分割，
EMQX使用的配置路径的层级分割符是 `'.'`。

被`'.'`号分割的每一段，则是Struct（结构体）的字段，或Map的key。

下面有几个例子：

```
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

### 环境变量重载

因为 `'.'` 分隔符不能使用于环境变量，所以我们需要使用另一个分割符。EMQX选用的是双下划线 `__`。
为了与其他的环境变量有所区分，EMQX还增加了一个前缀 `EMQX_` 来用作环境变量命名空间。

例如 `node.name` 的重载变量名是 `EMQX_NODE__NAME`。

环境变量的值，是解析成HOCON值的。所以这也使得环境变量可以用来传递复杂数据类型的值。

例如，下面这个环境变量传入一个数组类型的值。

```
export EMQX_LISTENERS__SSL__L1__AUTHENTICATION__SSL__CIPHERS="[\"TLS_AES_256_GCM_SHA384\"]"
```

::: tip Tip
未定义的根路径会被EMQX忽略，例如 `EMQX_UNKNOWN_ROOT__FOOBAR` 这个环境变量会被EMQX忽略，
因为 `UNKNOWN_ROOT` 不是预先定义好的根路径。
对于已知的根路径，未知的字段名称将被记录为warning日志，比如下面这个例子。

```
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

这是因为正确的字段名称是 `enable`，而不是 `enabled`.
:::

### 配置重载规则

HOCON的值是分层覆盖的，普遍规则如下：

- 在同一个文件中，后（在文件底部）定义的值，覆盖前（在文件顶部）到值。
- 当按层级覆盖时，高层级的值覆盖低层级的值。

结下来的文档将解释更详细的规则。

#### 结构体

合并覆盖规则。在如下配置中，最后一行的 `debug` 值会覆盖覆盖原先 `level` 字段的 `error` 值
但是 `enable` 字段保持不变。
```
log {
    console_handler{
        enable=true,
        level=error
    }
}

## 控制台日志打印先定义为`error`级，后被覆写成`debug`级

log.console_handler.level=debug
```

#### Map

Map与结构体类似，也是合并覆盖规则。
如下例子中，`zone1` 的 `max_packet_size` 可以在文件后面覆写。

```
zone {
    zone1 {
        mqtt.max_packet_size = 1M
    }
}

## 报文大小限制最先被设置成1MB，后被覆写为10MB

zone.zone1.mqtt.max_packet_size = 10M
```

#### 数组元素

如上面介绍过，EMQX配置中的数组有两种表达方式。

* 列表格式，例如： `[1, 2, 3]`
* 带下标的Map格式，例如： `{"1"=1, "2"=2, "3"=3}`

点好（`'.'`）分隔到路径中的纯数字会被解析成数组下标。
例如，`authentication.1={...}`  会被解析成 `authentication={"1": {...}}`，进而进一步解析成 `authentication=[{...}]`
有了这个特性，我们就可以轻松覆写数组某个元素的值，例如：

```
authentication=[{enable=true, backend="built_in_database", mechanism="password_based"}]
# 可以用下面的方式将第一个元素的 `enable` 字段覆写
authentication.1.enable=false
```

::: warning Warning
使用列表格式是的数组将全量覆写原值，如下例：

```
authentication=[{enable=true, backend="built_in_database", mechanism="password_based"}]
## 下面这中方式会导致数组第一个元素的除了 `enable` 以外的其他字段全部丢失
authentication=[{enable=true}]
```
:::

## Root Config Keys




**Fields**

- listeners: <code>[broker:listeners](#broker-listeners)</code>



- zones: <code>{$name -> [broker:zone](#broker-zone)}</code>

  <code>zone</code> 是按 <code>name</code> 分组的一组配置。
  对于灵活的配置映射，可以将 <code>name</code> 设置为侦听器的 <code>zone</code> 配置。
  注：名为 <code>default</code> 的内置区域是自动创建的，无法删除。

- mqtt: <code>[broker:mqtt](#broker-mqtt)</code>

  全局的 MQTT 配置项。
  mqtt 下所有的配置作为全局的默认值存在，它可以被 <code>zone</code> 中的配置覆盖。

- authentication: <code>[[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)]</code>

  全局 MQTT 监听器的默认认证配置。 为每个监听器配置认证参考监听器器配置中的 <code>authentication</code> 配置。

  该配置可以被配置为：
  <ul>
    <li><code>[]</code>: 默认值，允许所以登录请求
    <li>配置为单认证器，例如 <code>{enable:true,backend:"built_in_database",mechanism="password_based"}</code></li>
    <li>配置为认证器数组</li>
  </ul>

  当配置为认证链后，登录凭证会按照配置的顺序进行检查，直到做出 <code>allow</code> 或 <code>deny</code> 的结果。

  如果在所有的认证器都执行完后，还是没有结果，登录将被拒绝。


- authorization: <code>[authorization](#authorization)</code>

   授权（ACL）。EMQX 支持完整的客户端访问控制（ACL）。</br> 

- node: <code>[node](#node)</code>



- cluster: <code>[cluster](#cluster)</code>



- log: <code>[log](#log)</code>



- rpc: <code>[rpc](#rpc)</code>



- broker: <code>[broker](#broker)</code>

  Broker 相关配置项。

- sys_topics: <code>[broker:sys_topics](#broker-sys_topics)</code>

  系统主题配置。

- force_shutdown: <code>[broker:force_shutdown](#broker-force_shutdown)</code>



- overload_protection: <code>[broker:overload_protection](#broker-overload_protection)</code>



- force_gc: <code>[broker:force_gc](#broker-force_gc)</code>



- conn_congestion: <code>[broker:conn_congestion](#broker-conn_congestion)</code>



- stats: <code>[broker:stats](#broker-stats)</code>



- sysmon: <code>[broker:sysmon](#broker-sysmon)</code>



- alarm: <code>[broker:alarm](#broker-alarm)</code>



- flapping_detect: <code>[broker:flapping_detect](#broker-flapping_detect)</code>



- persistent_session_store: <code>[broker:persistent_session_store](#broker-persistent_session_store)</code>



- trace: <code>[broker:trace](#broker-trace)</code>



- bridges: <code>[bridge:bridges](#bridge-bridges)</code>



- retainer: <code>[retainer](#retainer)</code>



- statsd: <code>[statsd](#statsd)</code>



- auto_subscribe: <code>[auto_subscribe](#auto_subscribe)</code>



- delayed: <code>[modules:delayed](#modules-delayed)</code>



- telemetry: <code>[modules:telemetry](#modules-telemetry)</code>



- rewrite: <code>[[modules:rewrite](#modules-rewrite)]</code>

  List of topic rewrite rules.

- topic_metrics: <code>[[modules:topic_metrics](#modules-topic_metrics)]</code>

  List of topics whose metrics are reported.

- plugins: <code>[plugin:plugins](#plugin-plugins)</code>



- dashboard: <code>[dashboard](#dashboard)</code>



- gateway: <code>[gateway](#gateway)</code>



- prometheus: <code>[prometheus](#prometheus)</code>



- rule_engine: <code>[rule_engine](#rule_engine)</code>



- exhook: <code>[exhook](#exhook)</code>



- psk_authentication: <code>[authn-psk:psk_authentication](#authn-psk-psk_authentication)</code>



- limiter: <code>[limiter](#limiter)</code>



- connectors: <code>[connector:connectors](#connector-connectors)</code>



- slow_subs: <code>[slow_subs](#slow_subs)</code>




## authz:file
使用静态文件鉴权


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>file</code>

  数据后端类型

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源

- path: <code>string()</code>


  包含 ACL 规则的文件路径。
  如果在启动 EMQX 节点前预先配置该路径，
  那么可以将该文件置于任何 EMQX 可以访问到的位置。

  如果从 EMQX Dashboard 或 HTTP API 创建或修改了规则集，
  那么EMQX将会生成一个新的文件并将它存放在 `data_dir` 下的 `authz` 子目录中,
  并从此弃用旧的文件。


## authz:http_get
使用外部 HTTP 服务器鉴权(GET 请求)。


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>http</code>

  数据后端类型

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源

- url: <code>binary()</code>

  鉴权 HTTP 服务器地址。

- request_timeout: <code>string()</code>
  * default: 
  `"30s"`

  HTTP 请求超时时长。

- body: <code>map()</code>

  HTTP request body。

- connect_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  连接HTTP服务器的超时时间。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `5`

  请求出错时的最大重试次数。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- request: <code>[connector-http:request](#connector-http-request)</code>


  如果提供了请求，调用者可以通过以下方式发送 HTTP 请求
  <code>emqx_resource:query(ResourceId, {send_message, BridgeId, Message})</code>


- retry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"1s"`

  重试之间的间隔时间。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- method: <code>get</code>
  * default: 
  `get`

  HTTP 请求方法

- headers: <code>[{binary(), binary()}]</code>
  * default: 

  ```
  {
    accept = "application/json"
    "cache-control" = "no-cache"
    connection = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
  }
  ```

  HTTP Headers 列表 (无 <code>content-type</code>) 。


## authz:http_post
使用外部 HTTP 服务器鉴权(POST 请求)。


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>http</code>

  数据后端类型

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源

- url: <code>binary()</code>

  鉴权 HTTP 服务器地址。

- request_timeout: <code>string()</code>
  * default: 
  `"30s"`

  HTTP 请求超时时长。

- body: <code>map()</code>

  HTTP request body。

- connect_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  连接HTTP服务器的超时时间。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `5`

  请求出错时的最大重试次数。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- request: <code>[connector-http:request](#connector-http-request)</code>


  如果提供了请求，调用者可以通过以下方式发送 HTTP 请求
  <code>emqx_resource:query(ResourceId, {send_message, BridgeId, Message})</code>


- retry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"1s"`

  重试之间的间隔时间。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- method: <code>post</code>
  * default: 
  `post`

  HTTP 请求方法

- headers: <code>[{binary(), binary()}]</code>
  * default: 

  ```
  {
    accept = "application/json"
    "cache-control" = "no-cache"
    connection = "keep-alive"
    "content-type" = "application/json"
    "keep-alive" = "timeout=30, max=1000"
  }
  ```

  HTTP Headers 列表


## authz:mnesia
使用内部数据库鉴权 (mnesia)


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>built_in_database</code>

  数据后端类型

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源


## authz:mongo_rs
使用 MongoDB 鉴权(副本集模式)


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>mongodb</code>

  数据后端类型

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源

- collection: <code>atom()</code>

  `MongoDB` 鉴权数据集

- filter: <code>map()</code>
  * default: 
  `{}`


  在查询中定义过滤条件的条件表达式。
  过滤器支持如下占位符：
  - <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符


- mongo_type: <code>rs</code>
  * default: 
  `rs`

  Replica set模式。

- servers: <code>[term()]</code>


  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
  每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
  主机名具有以下形式：`Host[:Port]`。
  如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写模式。

- r_mode: <code>master | slave_ok</code>
  * default: 
  `master`

  读模式。

- replica_set_name: <code>binary()</code>

  副本集的名称。

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>binary()</code>

  内部数据库密码。

- auth_source: <code>binary()</code>

  与用户证书关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[topology](#topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authz:mongo_sharded
使用 MongoDB 鉴权(分片集群模式)


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>mongodb</code>

  数据后端类型

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源

- collection: <code>atom()</code>

  `MongoDB` 鉴权数据集

- filter: <code>map()</code>
  * default: 
  `{}`


  在查询中定义过滤条件的条件表达式。
  过滤器支持如下占位符：
  - <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符


- mongo_type: <code>sharded</code>
  * default: 
  `sharded`

  Sharded cluster模式。

- servers: <code>[term()]</code>


  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
  每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
  主机名具有以下形式：`Host[:Port]`。
  如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写模式。

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>binary()</code>

  内部数据库密码。

- auth_source: <code>binary()</code>

  与用户证书关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[topology](#topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authz:mongo_single
使用 MongoDB 鉴权(单实例)


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>mongodb</code>

  数据后端类型

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源

- collection: <code>atom()</code>

  `MongoDB` 鉴权数据集

- filter: <code>map()</code>
  * default: 
  `{}`


  在查询中定义过滤条件的条件表达式。
  过滤器支持如下占位符：
  - <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符


- mongo_type: <code>single</code>
  * default: 
  `single`

  Standalone模式。

- server: <code>emqx_schema:ip_port()</code>


  将要连接的 IPv4 或 IPv6 地址，或者主机名。</br>
  主机名具有以下形式：`Host[:Port]`。</br>
  如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写模式。

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>binary()</code>

  内部数据库密码。

- auth_source: <code>binary()</code>

  与用户证书关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[topology](#topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authz:mysql
使用 MySOL 数据库鉴权


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>mysql</code>

  数据后端类型

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源

- server: <code>emqx_schema:ip_port()</code>


  将要连接的 IPv4 或 IPv6 地址，或者主机名。</br>
  主机名具有以下形式：`Host[:Port]`。</br>
  如果未指定 `[:Port]`，则使用 MySQL 默认端口 3306。


- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>binary()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  自动重连数据库。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- prepare_statement: <code>map()</code>

  SQL 预处理语句列表。

- query: <code>binary()</code>

  访问控制数据查询语句/查询命令


## authz:postgresql
使用 PostgreSQL 数据库鉴权


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>postgresql</code>

  数据后端类型

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源

- server: <code>emqx_schema:ip_port()</code>


  将要连接的 IPv4 或 IPv6 地址，或者主机名。</br>
  主机名具有以下形式：`Host[:Port]`。</br>
  如果未指定 `[:Port]`，则使用 PostgreSQL 默认端口 5432。


- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>binary()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  自动重连数据库。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- prepare_statement: <code>map()</code>

  SQL 预处理语句列表。

- query: <code>binary()</code>

  访问控制数据查询语句/查询命令


## authz:redis_cluster
使用 Redis 鉴权(集群模式)


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>redis</code>

  数据后端类型

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源

- servers: <code>[term()]</code>



  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
  每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
  主机名具有以下形式：`Host[:Port]`。
  如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


- redis_type: <code>cluster</code>

  集群模式。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- password: <code>binary()</code>

  内部数据库密码。

- database: <code>integer()</code>
  * default: 
  `0`

  Redis 数据库 ID。

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  自动重连数据库。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- cmd: <code>binary()</code>

  访问控制数据查查询命令


## authz:redis_sentinel
使用 Redis 鉴权(哨兵模式)


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>redis</code>

  数据后端类型

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源

- servers: <code>[term()]</code>



  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
  每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
  主机名具有以下形式：`Host[:Port]`。
  如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


- redis_type: <code>sentinel</code>

  哨兵模式。

- sentinel: <code>string()</code>

  Redis 哨兵模式下的集群名称。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- password: <code>binary()</code>

  内部数据库密码。

- database: <code>integer()</code>
  * default: 
  `0`

  Redis 数据库 ID。

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  自动重连数据库。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- cmd: <code>binary()</code>

  访问控制数据查查询命令


## authz:redis_single
使用 Redis 鉴权(单实例)


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>redis</code>

  数据后端类型

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源

- server: <code>emqx_schema:ip_port()</code>


  将要连接的 IPv4 或 IPv6 地址，或者主机名。</br>
  主机名具有以下形式：`Host[:Port]`。</br>
  如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


- redis_type: <code>single</code>

  单机模式。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- password: <code>binary()</code>

  内部数据库密码。

- database: <code>integer()</code>
  * default: 
  `0`

  Redis 数据库 ID。

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  自动重连数据库。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- cmd: <code>binary()</code>

  访问控制数据查查询命令


## broker:alarm
Settings for the alarms.


**Config paths**

 - <code>alarm</code>


**Env overrides**

 - <code>EMQX_ALARM</code>



**Fields**

- actions: <code>[atom()]</code>
  * default: 
  `[log, publish]`

  警报激活时触发的动作。</br>
  目前，支持以下操作：<code>log</code> 和 <code>publish</code>。
  <code>log</code> 将告警写入日志 (控制台或者文件)。
  <code>publish</code> 将告警作为 MQTT 消息发布到系统主题:
  <code>$SYS/brokers/emqx@xx.xx.xx.x/alarms/activate</code> and
  <code>$SYS/brokers/emqx@xx.xx.xx.x/alarms/deactivate</code>

- size_limit: <code>1..3000</code>
  * default: 
  `1000`

  要保留为历史记录的已停用报警的最大总数。当超过此限制时，将删除最旧的停用报警，以限制总数。

- validity_period: <code>emqx_schema:duration()</code>
  * default: 
  `"24h"`

  停用报警的保留时间。报警在停用时不会立即删除，而是在保留时间之后删除。


## broker
Message broker options.


**Config paths**

 - <code>broker</code>


**Env overrides**

 - <code>EMQX_BROKER</code>



**Fields**

- enable_session_registry: <code>boolean()</code>
  * default: 
  `true`

  是否启用 Session Registry

- session_locking_strategy: <code>local | leader | quorum | all</code>
  * default: 
  `quorum`

  Session 在集群中的锁策略。
    - `loca`: 仅锁本节点的 Session
    - `one`: 任选一个其它节点加锁
    - `quorum`: 选择集群中半数以上的节点加锁
    - `all`: 选择所有节点加锁


- shared_subscription_strategy: <code>random | round_robin | sticky | local | hash_topic | hash_clientid</code>
  * default: 
  `round_robin`

  共享订阅消息派发策略。
    - `random`: 随机挑选一个共享订阅者派发
    - `round_robin`: 使用 round-robin 策略派发
    - `sticky`: 总是使用上次选中的订阅者派发，直到它断开连接
    - `hash`: 使用发送者的 Client ID 进行 Hash 来选择订阅者


- shared_dispatch_ack_enabled: <code>boolean()</code>
  * default: 
  `false`

  启用/禁用 QoS 1 和 QoS 2 消息的共享派发确认。
  开启后，允许将消息从未及时回复 ACK 的订阅者 （例如，客户端离线）重新派发给另外一个订阅者。


- route_batch_clean: <code>boolean()</code>
  * default: 
  `true`

  是否开启批量清除路由。

- perf: <code>[broker:broker_perf](#broker-broker_perf)</code>



- shared_subscription_group: <code>{$name -> [broker:shared_subscription_group](#broker-shared_subscription_group)}</code>




## broker:broker_perf
Broker performance tuning parameters.


**Config paths**

 - <code>broker.perf</code>


**Env overrides**

 - <code>EMQX_BROKER__PERF</code>



**Fields**

- route_lock_type: <code>key | tab | global</code>
  * default: 
  `key`

  通配主题订阅/取消订阅性能调优。
  建议仅当通配符主题较多时才更改此参数。

  注：当从/更改为 `global` 锁时，它要求集群中的所有节点在更改之前停止。
    - `key`: 为 Mnesia 事务涉及到的每个 key 上锁，建议单节点时使用。
    - `tab`: 为 Mnesia 事务涉及到的表上锁，建议在集群中使用。
    - `global`: 所以更新操作都被全局的锁保护，仅建议在超大规模集群中使用。


- trie_compaction: <code>boolean()</code>
  * default: 
  `true`

  是否开启主题表压缩存储。
  启用它会显着提高通配符主题订阅率，如果通配符主题具有唯一前缀，例如：'sensor/{{id}}/+/'，其中每个订阅者的 ID 是唯一的。
  如果消息主要发布到具有大量级别的主题，则主题匹配性能（发布时）可能会降低。

  注意：这是一个集群范围的配置。 它要求在更改之前停止所有节点。



## broker:cache
Settings for the authorization cache.


**Config paths**

 - <code>authorization.cache</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__CACHE</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`


  启用或禁用授权缓存。


- max_size: <code>1..1048576</code>
  * default: 
  `32`


  缓存项的最大数量。


- ttl: <code>emqx_schema:duration()</code>
  * default: 
  `"1m"`


  缓存数据的生存时间。



## broker:conn_congestion
Settings for `conn_congestion` alarm.

Sometimes the MQTT connection (usually an MQTT subscriber) may
get "congested", because there are too many packets to be sent.
The socket tries to buffer the packets until the buffer is
full. If more packets arrive after that, the packets will be
"pending" in the queue, and we consider the connection
congested.

Note: `sndbuf` can be set to larger value if the
alarm is triggered too often.
The name of the alarm is of format `conn_congestion/<ClientID>/<Username>`,
where the `<ClientID>` is the client ID of the congested MQTT connection,
and `<Username>` is the username or `unknown_user`.


**Config paths**

 - <code>conn_congestion</code>


**Env overrides**

 - <code>EMQX_CONN_CONGESTION</code>



**Fields**

- enable_alarm: <code>boolean()</code>
  * default: 
  `true`

  启用或者禁用连接阻塞告警功能

- min_alarm_sustain_duration: <code>emqx_schema:duration()</code>
  * default: 
  `"1m"`

  清除警报前的最短时间。

  只有当队列中没有挂起的数据，并且连接至少被堵塞了 "min_alarm_sustain_duration" 毫秒时，
  报警才会被清除。这是为了避免太频繁地清除和再次发出警报.


## broker:deflate_opts
Compression options.


**Config paths**

 - <code>listeners.ws.$name.websocket.deflate_opts</code>
 - <code>listeners.wss.$name.websocket.deflate_opts</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WS__$NAME__WEBSOCKET__DEFLATE_OPTS</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__WEBSOCKET__DEFLATE_OPTS</code>



**Fields**

- level: <code>none | default | best_compression | best_speed</code>

  压缩级别

- mem_level: <code>1..9</code>
  * default: 
  `8`


  指定压缩状态的大小</br>
  较低的值会减少每个连接的内存使用。


- strategy: <code>default | filtered | huffman_only | rle</code>
  * default: 
  `default`


  指定压缩策略。


- server_context_takeover: <code>takeover | no_takeover</code>
  * default: 
  `takeover`

  接管意味着在服务器消息之间保留压缩状态。

- client_context_takeover: <code>takeover | no_takeover</code>
  * default: 
  `takeover`


  接管意味着在客户端消息之间保留压缩状态。


- server_max_window_bits: <code>8..15</code>
  * default: 
  `15`


  指定服务器压缩上下文的大小。


- client_max_window_bits: <code>8..15</code>
  * default: 
  `15`


  指定客户端压缩上下文的大小。



## broker:event_names
Enable or disable client lifecycle event publishing.

The following options affect MQTT clients as well as
gateway clients. The types of the clients
are distinguished by the topic prefix:

- For the MQTT clients, the format is:
`$SYS/broker/<node>/clients/<clientid>/<event>`
- For the Gateway clients, it is
`$SYS/broker/<node>/gateway/<gateway-name>/clients/<clientid>/<event>`



**Config paths**

 - <code>sys_topics.sys_event_messages</code>


**Env overrides**

 - <code>EMQX_SYS_TOPICS__SYS_EVENT_MESSAGES</code>



**Fields**

- client_connected: <code>boolean()</code>
  * default: 
  `true`

  是否开启客户端已连接事件消息。

- client_disconnected: <code>boolean()</code>
  * default: 
  `true`

  是否开启客户端已断开连接事件消息。

- client_subscribed: <code>boolean()</code>
  * default: 
  `false`

  是否开启客户端已成功订阅主题事件消息。

- client_unsubscribed: <code>boolean()</code>
  * default: 
  `false`

  是否开启客户端已成功取消订阅主题事件消息。


## broker:flapping_detect
This config controls the allowed maximum number of `CONNECT` packets received
from the same clientid in a time frame defined by `window_time`.
After the limit is reached, successive `CONNECT` requests are forbidden
(banned) until the end of the time period defined by `ban_time`.


**Config paths**

 - <code>flapping_detect</code>


**Env overrides**

 - <code>EMQX_FLAPPING_DETECT</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  启用抖动检测功能

- max_count: <code>integer()</code>
  * default: 
  `15`

  MQTT 客户端在"窗口"时间内允许的最大断开次数

- window_time: <code>emqx_schema:duration()</code>
  * default: 
  `"1m"`

  抖动检测的时间窗口。

- ban_time: <code>emqx_schema:duration()</code>
  * default: 
  `"5m"`

  抖动的客户端将会被禁止登陆多长时间


## broker:force_gc
Force garbage collection in MQTT connection process after
 they process certain number of messages or bytes of data.


**Config paths**

 - <code>force_gc</code>


**Env overrides**

 - <code>EMQX_FORCE_GC</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用强制垃圾回收

- count: <code>0..inf</code>
  * default: 
  `16000`

  在进程收到多少消息之后，对此进程执行垃圾回收

- bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `"16MB"`

  在进程处理过多少个字节之后，对此进程执行垃圾回收


## broker:force_shutdown
When the process message queue length, or the memory bytes
reaches a certain value, the process is forced to close.

Note: "message queue" here refers to the "message mailbox"
of the Erlang process, not the `mqueue` of QoS 1 and QoS 2.


**Config paths**

 - <code>force_shutdown</code>


**Env overrides**

 - <code>EMQX_FORCE_SHUTDOWN</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 `force_shutdown` 功能

- max_message_queue_len: <code>0..inf</code>
  * default: 
  `1000`

  消息队列的最大长度

- max_heap_size: <code>emqx_schema:wordsize()</code>
  * default: 
  `"32MB"`

  Heap 的总大小


## broker:listener_ssl_opts
Socket options for SSL connections.


**Config paths**

 - <code>gateway.exproto.listeners.ssl.$name.ssl_options</code>
 - <code>gateway.stomp.listeners.ssl.$name.ssl_options</code>
 - <code>listeners.ssl.$name.ssl_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__SSL_OPTIONS</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__SSL_OPTIONS</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>


  受信任的PEM格式CA证书捆绑文件</br>
  此文件中的证书用于验证TLS对等方的证书。
  如果要信任新CA，请将新证书附加到文件中。
  无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）</br>
  注意：从文件中失效（删除）证书不会影响已建立的连接。


- certfile: <code>binary()</code>


  PEM格式证书链文件</br>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，然后是直接颁发者证书，依此类推。
  虽然根CA证书是可选的，但它应该放在
  如果要添加文件，请将其删除。


- keyfile: <code>binary()</code>


  PEM格式的私钥文件。


- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`


  启用或禁用对等验证。


- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`


  启用 TLS 会话重用。


- depth: <code>integer()</code>
  * default: 
  `10`


  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。因此，如果深度为0，则对等方必须由受信任的根CA直接签名；如果1，路径可以是PEER、CA、ROOT-CA；如果是2，则路径可以是PEER、CA、CA、ROOT-CA等等。默认值为10。


- password: <code>string()</code>


  包含用户密码的字符串。
  仅在私钥文件受密码保护时使用。


- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2, tlsv1.1, tlsv1]`


  支持所有TLS/DTLS版本</br>

  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


- ciphers: <code>[string()]</code>
  * default: 
  `["TLS_AES_256_GCM_SHA384", "TLS_AES_128_GCM_SHA256", "TLS_CHACHA20_POLY1305_SHA256", "TLS_AES_128_CCM_SHA256", "TLS_AES_128_CCM_8_SHA256", "ECDHE-ECDSA-AES256-GCM-SHA384", "ECDHE-RSA-AES256-GCM-SHA384", "ECDHE-ECDSA-AES256-SHA384", "ECDHE-RSA-AES256-SHA384", "ECDH-ECDSA-AES256-GCM-SHA384", "ECDH-RSA-AES256-GCM-SHA384", "ECDH-ECDSA-AES256-SHA384", "ECDH-RSA-AES256-SHA384", "DHE-DSS-AES256-GCM-SHA384", "DHE-DSS-AES256-SHA256", "AES256-GCM-SHA384", "AES256-SHA256", "ECDHE-ECDSA-AES128-GCM-SHA256", "ECDHE-RSA-AES128-GCM-SHA256", "ECDHE-ECDSA-AES128-SHA256", "ECDHE-RSA-AES128-SHA256", "ECDH-ECDSA-AES128-GCM-SHA256", "ECDH-RSA-AES128-GCM-SHA256", "ECDH-ECDSA-AES128-SHA256", "ECDH-RSA-AES128-SHA256", "DHE-DSS-AES128-GCM-SHA256", "DHE-DSS-AES128-SHA256", "AES128-GCM-SHA256", "AES128-SHA256", "ECDHE-ECDSA-AES256-SHA", "ECDHE-RSA-AES256-SHA", "DHE-DSS-AES256-SHA", "ECDH-ECDSA-AES256-SHA", "ECDH-RSA-AES256-SHA", "ECDHE-ECDSA-AES128-SHA", "ECDHE-RSA-AES128-SHA", "DHE-DSS-AES128-SHA", "ECDH-ECDSA-AES128-SHA", "ECDH-RSA-AES128-SHA", "RSA-PSK-AES256-GCM-SHA384", "RSA-PSK-AES256-CBC-SHA384", "RSA-PSK-AES128-GCM-SHA256", "RSA-PSK-AES128-CBC-SHA256", "RSA-PSK-AES256-CBC-SHA", "RSA-PSK-AES128-CBC-SHA"]`


  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> 或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  </br>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式</br>
  注意：某些密码套件仅与特定的 TLS <code>版本</code> 兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  </br>
  注：PSK 的 Ciphers 不支持 tlsv1.3</br>
  如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在<code>ssl.versions</code>中禁用。

  </br>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code></br>


- user_lookup_fun: <code>string()</code>
  * default: 
  `"emqx_tls_psk:lookup"`


  用于查找预共享密钥（PSK）标识的 EMQX 内部回调。


- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`


  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


- dhfile: <code>string()</code>


  如果协商使用Diffie-Hellman密钥交换的密码套件，则服务器将使用包含PEM编码的Diffie-Hellman参数的文件的路径。如果未指定，则使用默认参数</br>

  注意：TLS 1.3不支持<code>dhfile</code>选项。


- fail_if_no_peer_cert: <code>boolean()</code>
  * default: 
  `false`


  TLS/DTLS 服务器与 {verify，verify_peer} 一起使用。
  如果设置为true，则如果客户端没有要发送的证书，即发送空证书，服务器将失败。
  如果设置为false，则仅当客户端发送无效证书（空证书被视为有效证书）时才会失败。


- honor_cipher_order: <code>boolean()</code>
  * default: 
  `true`


  一个重要的安全设置，它强制根据服务器指定的顺序而不是客户机指定的顺序设置密码，从而强制服务器管理员执行（通常配置得更正确）安全顺序。


- client_renegotiation: <code>boolean()</code>
  * default: 
  `true`


  在支持客户机发起的重新协商的协议中，这种操作的资源成本对于服务器来说高于客户机。
  这可能会成为拒绝服务攻击的载体。
  SSL 应用程序已经采取措施来反击此类尝试，但通过将此选项设置为 false，可以严格禁用客户端发起的重新协商。
  默认值为 true。请注意，由于基础密码套件可以加密的消息数量有限，禁用重新协商可能会导致长期连接变得不可用。



## broker:listener_wss_opts
Socket options for WebSocket/SSL connections.


**Config paths**

 - <code>listeners.wss.$name.ssl_options</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WSS__$NAME__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>


  受信任的PEM格式CA证书捆绑文件</br>
  此文件中的证书用于验证TLS对等方的证书。
  如果要信任新CA，请将新证书附加到文件中。
  无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）</br>
  注意：从文件中失效（删除）证书不会影响已建立的连接。


- certfile: <code>binary()</code>


  PEM格式证书链文件</br>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，然后是直接颁发者证书，依此类推。
  虽然根CA证书是可选的，但它应该放在
  如果要添加文件，请将其删除。


- keyfile: <code>binary()</code>


  PEM格式的私钥文件。


- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`


  启用或禁用对等验证。


- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`


  启用 TLS 会话重用。


- depth: <code>integer()</code>
  * default: 
  `10`


  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。因此，如果深度为0，则对等方必须由受信任的根CA直接签名；如果1，路径可以是PEER、CA、ROOT-CA；如果是2，则路径可以是PEER、CA、CA、ROOT-CA等等。默认值为10。


- password: <code>string()</code>


  包含用户密码的字符串。
  仅在私钥文件受密码保护时使用。


- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2, tlsv1.1, tlsv1]`


  支持所有TLS/DTLS版本</br>

  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


- ciphers: <code>[string()]</code>
  * default: 
  `["TLS_AES_256_GCM_SHA384", "TLS_AES_128_GCM_SHA256", "TLS_CHACHA20_POLY1305_SHA256", "TLS_AES_128_CCM_SHA256", "TLS_AES_128_CCM_8_SHA256", "ECDHE-ECDSA-AES256-GCM-SHA384", "ECDHE-RSA-AES256-GCM-SHA384", "ECDHE-ECDSA-AES256-SHA384", "ECDHE-RSA-AES256-SHA384", "ECDH-ECDSA-AES256-GCM-SHA384", "ECDH-RSA-AES256-GCM-SHA384", "ECDH-ECDSA-AES256-SHA384", "ECDH-RSA-AES256-SHA384", "DHE-DSS-AES256-GCM-SHA384", "DHE-DSS-AES256-SHA256", "AES256-GCM-SHA384", "AES256-SHA256", "ECDHE-ECDSA-AES128-GCM-SHA256", "ECDHE-RSA-AES128-GCM-SHA256", "ECDHE-ECDSA-AES128-SHA256", "ECDHE-RSA-AES128-SHA256", "ECDH-ECDSA-AES128-GCM-SHA256", "ECDH-RSA-AES128-GCM-SHA256", "ECDH-ECDSA-AES128-SHA256", "ECDH-RSA-AES128-SHA256", "DHE-DSS-AES128-GCM-SHA256", "DHE-DSS-AES128-SHA256", "AES128-GCM-SHA256", "AES128-SHA256", "ECDHE-ECDSA-AES256-SHA", "ECDHE-RSA-AES256-SHA", "DHE-DSS-AES256-SHA", "ECDH-ECDSA-AES256-SHA", "ECDH-RSA-AES256-SHA", "ECDHE-ECDSA-AES128-SHA", "ECDHE-RSA-AES128-SHA", "DHE-DSS-AES128-SHA", "ECDH-ECDSA-AES128-SHA", "ECDH-RSA-AES128-SHA", "RSA-PSK-AES256-GCM-SHA384", "RSA-PSK-AES256-CBC-SHA384", "RSA-PSK-AES128-GCM-SHA256", "RSA-PSK-AES128-CBC-SHA256", "RSA-PSK-AES256-CBC-SHA", "RSA-PSK-AES128-CBC-SHA"]`


  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> 或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  </br>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式</br>
  注意：某些密码套件仅与特定的 TLS <code>版本</code> 兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  </br>
  注：PSK 的 Ciphers 不支持 tlsv1.3</br>
  如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在 <code>ssl.versions</code> 中禁用。

  </br>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code></br>


- user_lookup_fun: <code>string()</code>
  * default: 
  `"emqx_tls_psk:lookup"`


  用于查找预共享密钥（PSK）标识的 EMQX 内部回调。


- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`


  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


- dhfile: <code>string()</code>


  如果协商使用Diffie-Hellman密钥交换的密码套件，则服务器将使用包含PEM编码的Diffie-Hellman参数的文件的路径。如果未指定，则使用默认参数</br>

  注意：TLS 1.3不支持 <code>dhfile</code> 选项。


- fail_if_no_peer_cert: <code>boolean()</code>
  * default: 
  `false`


  TLS/DTLS 服务器与 {verify，verify_peer} 一起使用。
  如果设置为true，则如果客户端没有要发送的证书，即发送空证书，服务器将失败。
  如果设置为false，则仅当客户端发送无效证书（空证书被视为有效证书）时才会失败。


- honor_cipher_order: <code>boolean()</code>
  * default: 
  `true`


  一个重要的安全设置，它强制根据服务器指定的顺序而不是客户机指定的顺序设置密码，从而强制服务器管理员执行（通常配置得更正确）安全顺序。


- client_renegotiation: <code>boolean()</code>
  * default: 
  `true`


  在支持客户机发起的重新协商的协议中，这种操作的资源成本对于服务器来说高于客户机。
  这可能会成为拒绝服务攻击的载体。
  SSL 应用程序已经采取措施来反击此类尝试，但通过将此选项设置为 false，可以严格禁用客户端发起的重新协商。
  默认值为 true。请注意，由于基础密码套件可以加密的消息数量有限，禁用重新协商可能会导致长期连接变得不可用。


- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"15s"`


  握手完成所允许的最长时间



## broker:listeners
MQTT listeners identified by their protocol type and assigned names


**Config paths**

 - <code>listeners</code>


**Env overrides**

 - <code>EMQX_LISTENERS</code>



**Fields**

- tcp: <code>{$name -> [broker:mqtt_tcp_listener](#broker-mqtt_tcp_listener)}</code>

  TCP 监听器

- ssl: <code>{$name -> [broker:mqtt_ssl_listener](#broker-mqtt_ssl_listener)}</code>

  SSL 监听器

- ws: <code>{$name -> [broker:mqtt_ws_listener](#broker-mqtt_ws_listener)}</code>

  HTTP websocket 监听器

- wss: <code>{$name -> [broker:mqtt_wss_listener](#broker-mqtt_wss_listener)}</code>

  HTTPS websocket 监听器

- quic: <code>{$name -> [broker:mqtt_quic_listener](#broker-mqtt_quic_listener)}</code>

  QUIC 监听器


## broker:mqtt
Global MQTT configuration.</br>
The configs here work as default values which can be overridden
in <code>zone</code> configs


**Config paths**

 - <code>mqtt</code>


**Env overrides**

 - <code>EMQX_MQTT</code>



**Fields**

- idle_timeout: <code>infinity | emqx_schema:duration()</code>
  * default: 
  `"15s"`

  TCP 连接建立后，如果在 <code>idle_timeout</code> 指定的时间内未收到客户端的 MQTT CONNECT 报文，则连接将被断开。

- max_packet_size: <code>emqx_schema:bytesize()</code>
  * default: 
  `"1MB"`

  允许的最大 MQTT 报文大小。

- max_clientid_len: <code>23..65535</code>
  * default: 
  `65535`

  允许的最大 MQTT Client ID 长度。

- max_topic_levels: <code>1..65535</code>
  * default: 
  `65535`

  允许的最大主题层级。

- max_qos_allowed: <code>qos()</code>
  * default: 
  `2`

  允许的最大 QoS 等级。

- max_topic_alias: <code>0..65535</code>
  * default: 
  `65535`

  允许的最大主题别名数，0 表示不支持主题别名。

- retain_available: <code>boolean()</code>
  * default: 
  `true`

  是否启用对 MQTT 保留消息的支持。

- wildcard_subscription: <code>boolean()</code>
  * default: 
  `true`

  是否启用对 MQTT 通配符订阅的支持。

- shared_subscription: <code>boolean()</code>
  * default: 
  `true`

  是否启用对 MQTT 共享订阅的支持。

- exclusive_subscription: <code>boolean()</code>
  * default: 
  `false`

  是否启用对 MQTT 排它订阅的支持。

- ignore_loop_deliver: <code>boolean()</code>
  * default: 
  `false`

  是否为 MQTT v3.1.1/v3.1.0 客户端忽略投递自己发布的消息，类似于 MQTT 5.0 中的 <code>No Local</code> 订阅选项

- strict_mode: <code>boolean()</code>
  * default: 
  `false`

  是否以严格模式解析 MQTT 消息。
  当设置为 true 时，例如客户端 ID、主题名称等中的无效 utf8 字符串将导致客户端断开连接。

- response_information: <code>string()</code>
  * default: 
  `""`

  指定返回给客户端的响应信息。如果设置为 ""，则禁用此功能。仅适用于使用 MQTT 5.0 协议的客户端。

- server_keepalive: <code>integer() | disabled</code>
  * default: 
  `disabled`

  EMQX 要求客户端使用的保活时间，配置为 <code>disabled</code> 表示将使用客户端指定的保活时间。需要用到 MQTT 5.0 中的 <code>Server Keep Alive</code>，因此仅适用于使用 MQTT 5.0 协议的客户端。

- keepalive_backoff: <code>number()</code>
  * default: 
  `0.75`

  Broker 判定客户端保活超时使用的退避乘数。如果 EMQX 在 <code>Keep Alive * Backoff * 2</code> 秒内未收到任何报文，EMQX 将关闭当前连接。

- max_subscriptions: <code>1..inf | infinity</code>
  * default: 
  `infinity`

  允许每个客户端建立的最大订阅数量。

- upgrade_qos: <code>boolean()</code>
  * default: 
  `false`

  投递消息时，是否根据订阅主题时的 QoS 等级来强制提升派发的消息的 QoS 等级。

- max_inflight: <code>1..65535</code>
  * default: 
  `32`

  允许在完成应答前同时投递的 QoS 1 和 QoS 2 消息的最大数量。

- retry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"30s"`

  QoS 1/2 消息的重新投递间隔。

- max_awaiting_rel: <code>integer() | infinity</code>
  * default: 
  `100`

  PUBREL (Client -> Broker) 最大等待队列长度。

- await_rel_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"300s"`

  PUBREL (Client -> Broker) 最大等待时间，超时则会被丢弃。

- session_expiry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"2h"`

  指定会话将在连接断开后多久过期，仅适用于非 MQTT 5.0 的连接。

- max_mqueue_len: <code>non_neg_integer() | infinity</code>
  * default: 
  `1000`

  消息队列最大长度。持久客户端断开连接或飞行窗口已满时排队的消息长度。

- mqueue_priorities: <code>map() | disabled</code>
  * default: 
  `disabled`

  主题优先级。取值范围 [1-255]
  默认优先级表为空，即所有的主题优先级相同。

  注：优先主题名称中不支持使用逗号和等号。
  注：不在此列表中的主题，被视为最高/最低优先级，这取决于 <code>mqtt.mqueue_default_priority</code> 的配置

  示例：
  配置 <code>"topic/1" > "topic/2"</code>:
  <code>mqueue_priorities: {"topic/1": 10, "topic/2": 8}</code>


- mqueue_default_priority: <code>highest | lowest</code>
  * default: 
  `lowest`

  默认的主题优先级，不在 <code>主题优先级</code>（<code>mqueue_priorities</code>）中的主题将会使用该优先级。

- mqueue_store_qos0: <code>boolean()</code>
  * default: 
  `true`

  指定在连接断开但会话保持期间，是否需要在消息队列中存储 QoS 0 消息。

- use_username_as_clientid: <code>boolean()</code>
  * default: 
  `false`

  是否使用用户名作为客户端 ID。
  此设置的作用时间晚于 <code>使用对端证书作为用户名</code>（<code>peer_cert_as_username</code>）和 <code>使用对端证书作为客户端 ID</code>（<code>peer_cert_as_clientid</code>）。


- peer_cert_as_username: <code>disabled | cn | dn | crt | pem | md5</code>
  * default: 
  `disabled`

  使用对端证书中的 CN, DN 字段或整个证书内容来作为用户名。仅适用于 TLS 连接。
  目前支持配置为以下内容：
  - <code>cn</code>: 取证书的 CN 字段作为 Username
  - <code>dn</code>: 取证书的 DN 字段作为 Username
  - <code>crt</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书的内容作为 Username
  - <code>pem</code>: 将 <code>DER</code> 证书内容转换为 <code>PEM</code> 格式后作为 Username
  - <code>md5</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书的内容的 MD5 值作为 Username


- peer_cert_as_clientid: <code>disabled | cn | dn | crt | pem | md5</code>
  * default: 
  `disabled`

  使用对端证书中的 CN, DN 字段或整个证书内容来作为客户端 ID。仅适用于 TLS 连接。
  目前支持配置为以下内容：
  - <code>cn</code>: 取证书的 CN 字段作为 Client ID
  - <code>dn</code>: 取证书的 DN 字段作为 Client ID
  - <code>crt</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书的内容作为 Client ID
  - <code>pem</code>: 将 <code>DER</code> 证书内容转换为 <code>PEM</code> 格式后作为 Client ID
  - <code>md5</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书的内容的 MD5 值作为 Client ID



## broker:mqtt_quic_listener
Settings for the MQTT over QUIC listener.


**Config paths**

 - <code>listeners.quic.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__QUIC__$NAME</code>



**Fields**

- certfile: <code>string()</code>

  证书文件

- keyfile: <code>string()</code>

  私钥文件

- ciphers: <code>[string()]</code>
  * default: 
  `["TLS_AES_256_GCM_SHA384", "TLS_AES_128_GCM_SHA256", "TLS_CHACHA20_POLY1305_SHA256"]`


  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> 或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  </br>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式</br>
  注意：某些密码套件仅与特定的 TLS <code>版本</code> 兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  </br>
  注：PSK 的 Ciphers 不支持 tlsv1.3</br>
  如果打算使用PSK密码套件，<code>tlsv1.3</code>。应在 <code>ssl.versions</code> 中禁用。

  </br>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code></br>

  注：QUIC 监听器不支持 tlsv1.3 的 ciphers


- idle_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"15s"`


  关闭在此间隔内未发送 MQTT CONNECT 消息的客户端的传输层连接。


- enabled: <code>boolean()</code>
  * default: 
  `true`

  启停监听器

- bind: <code>emqx_schema:ip_port() | integer()</code>
  * default: 
  `14567`


  监听套接字的 IP 地址和端口。


- acceptors: <code>pos_integer()</code>
  * default: 
  `16`

  监听器接收池的大小。

- max_connections: <code>infinity | pos_integer()</code>
  * default: 
  `infinity`


  监听器允许的最大并发连接数。


- mountpoint: <code>binary()</code>
  * default: 
  `""`


  发布或订阅时，请在所有主题前面加上 mountpoint 字符串。

  将消息传递给订阅者时，将从主题名称中删除带前缀的字符串。挂载点是一种用户可以用来实现不同侦听器之间消息路由隔离的方法。

  例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。</br>
  类似地，如果另一个客户端B（与客户端A连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息</br>

  设置为 <code>""</code> 以禁用该功能</br>

  mountpoint 字符串中的变量：
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username


- zone: <code>atom()</code>
  * default: 
  `default`


  监听器所属的配置组。


- limiter: <code>{$ratelimit_name -> emqx_limiter_schema:bucket_name()}</code>
  * default: 
  `{connection = "default"}`


  速率限制类型


- enable_authn: <code>boolean()</code>
  * default: 
  `true`


  配置 <code>true</code>（默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。



## broker:mqtt_ssl_listener
Settings for the MQTT over SSL listener.


**Config paths**

 - <code>listeners.ssl.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__SSL__$NAME</code>



**Fields**

- enabled: <code>boolean()</code>
  * default: 
  `true`

  启停监听器

- bind: <code>emqx_schema:ip_port() | integer()</code>
  * default: 
  `8883`


  监听套接字的 IP 地址和端口。


- acceptors: <code>pos_integer()</code>
  * default: 
  `16`

  监听器接收池的大小。

- max_connections: <code>infinity | pos_integer()</code>
  * default: 
  `infinity`


  监听器允许的最大并发连接数。


- mountpoint: <code>binary()</code>
  * default: 
  `""`


  发布或订阅时，请在所有主题前面加上 mountpoint 字符串。

  将消息传递给订阅者时，将从主题名称中删除带前缀的字符串。挂载点是一种用户可以用来实现不同侦听器之间消息路由隔离的方法。

  例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。</br>
  类似地，如果另一个客户端B（与客户端A连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息</br>

  设置为 <code>""</code> 以禁用该功能</br>

  mountpoint 字符串中的变量：
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username


- zone: <code>atom()</code>
  * default: 
  `default`


  监听器所属的配置组。


- limiter: <code>{$ratelimit_name -> emqx_limiter_schema:bucket_name()}</code>
  * default: 
  `{connection = "default"}`


  速率限制类型


- enable_authn: <code>boolean()</code>
  * default: 
  `true`


  配置 <code>true</code>（默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。


- access_rules: <code>[string()]</code>
  * default: 
  `["allow all"]`

  此监听器的访问控制规则。

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`


  如果EMQX集群部署在 HAProxy 或 Nginx 之后，请启用代理协议 V1/2 </br>
  详情见: https://www.haproxy.com/blog/haproxy/proxy-protocol/


- proxy_protocol_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"3s"`


  代理协议超时。如果在超时时间内未收到代理协议数据包，EMQX将关闭TCP连接。


- authentication: <code>[[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)]</code>


  监听器认证重载。

  认证配置可以是单个认证器实例，也可以是一个认证器数组组成的认证链。
  执行登录验证时（用户名、客户端 ID 等），将按配置的顺序执行</br>


- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>



- ssl_options: <code>[broker:listener_ssl_opts](#broker-listener_ssl_opts)</code>




## broker:mqtt_tcp_listener
Settings for the MQTT over TCP listener.


**Config paths**

 - <code>listeners.tcp.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__TCP__$NAME</code>



**Fields**

- enabled: <code>boolean()</code>
  * default: 
  `true`

  启停监听器

- bind: <code>emqx_schema:ip_port() | integer()</code>
  * default: 
  `1883`


  监听套接字的 IP 地址和端口。


- acceptors: <code>pos_integer()</code>
  * default: 
  `16`

  监听器接收池的大小。

- max_connections: <code>infinity | pos_integer()</code>
  * default: 
  `infinity`


  监听器允许的最大并发连接数。


- mountpoint: <code>binary()</code>
  * default: 
  `""`


  发布或订阅时，请在所有主题前面加上 mountpoint 字符串。

  将消息传递给订阅者时，将从主题名称中删除带前缀的字符串。挂载点是一种用户可以用来实现不同侦听器之间消息路由隔离的方法。

  例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。</br>
  类似地，如果另一个客户端B（与客户端A连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息</br>

  设置为 <code>""</code> 以禁用该功能</br>

  mountpoint 字符串中的变量：
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username


- zone: <code>atom()</code>
  * default: 
  `default`


  监听器所属的配置组。


- limiter: <code>{$ratelimit_name -> emqx_limiter_schema:bucket_name()}</code>
  * default: 
  `{connection = "default"}`


  速率限制类型


- enable_authn: <code>boolean()</code>
  * default: 
  `true`


  配置 <code>true</code>（默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。


- access_rules: <code>[string()]</code>
  * default: 
  `["allow all"]`

  此监听器的访问控制规则。

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`


  如果EMQX集群部署在 HAProxy 或 Nginx 之后，请启用代理协议 V1/2 </br>
  详情见: https://www.haproxy.com/blog/haproxy/proxy-protocol/


- proxy_protocol_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"3s"`


  代理协议超时。如果在超时时间内未收到代理协议数据包，EMQX将关闭TCP连接。


- authentication: <code>[[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)]</code>


  监听器认证重载。

  认证配置可以是单个认证器实例，也可以是一个认证器数组组成的认证链。
  执行登录验证时（用户名、客户端 ID 等），将按配置的顺序执行</br>


- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>




## broker:mqtt_ws_listener
Settings for the MQTT over WebSocket listener.


**Config paths**

 - <code>listeners.ws.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WS__$NAME</code>



**Fields**

- enabled: <code>boolean()</code>
  * default: 
  `true`

  启停监听器

- bind: <code>emqx_schema:ip_port() | integer()</code>
  * default: 
  `8083`


  监听套接字的 IP 地址和端口。


- acceptors: <code>pos_integer()</code>
  * default: 
  `16`

  监听器接收池的大小。

- max_connections: <code>infinity | pos_integer()</code>
  * default: 
  `infinity`


  监听器允许的最大并发连接数。


- mountpoint: <code>binary()</code>
  * default: 
  `""`


  发布或订阅时，请在所有主题前面加上 mountpoint 字符串。

  将消息传递给订阅者时，将从主题名称中删除带前缀的字符串。挂载点是一种用户可以用来实现不同侦听器之间消息路由隔离的方法。

  例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。</br>
  类似地，如果另一个客户端B（与客户端A连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息</br>

  设置为 <code>""</code> 以禁用该功能</br>

  mountpoint 字符串中的变量：
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username


- zone: <code>atom()</code>
  * default: 
  `default`


  监听器所属的配置组。


- limiter: <code>{$ratelimit_name -> emqx_limiter_schema:bucket_name()}</code>
  * default: 
  `{connection = "default"}`


  速率限制类型


- enable_authn: <code>boolean()</code>
  * default: 
  `true`


  配置 <code>true</code>（默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。


- access_rules: <code>[string()]</code>
  * default: 
  `["allow all"]`

  此监听器的访问控制规则。

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`


  如果EMQX集群部署在 HAProxy 或 Nginx 之后，请启用代理协议 V1/2 </br>
  详情见: https://www.haproxy.com/blog/haproxy/proxy-protocol/


- proxy_protocol_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"3s"`


  代理协议超时。如果在超时时间内未收到代理协议数据包，EMQX将关闭TCP连接。


- authentication: <code>[[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)]</code>


  监听器认证重载。

  认证配置可以是单个认证器实例，也可以是一个认证器数组组成的认证链。
  执行登录验证时（用户名、客户端 ID 等），将按配置的顺序执行</br>


- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>



- websocket: <code>[broker:ws_opts](#broker-ws_opts)</code>




## broker:mqtt_wss_listener
Settings for the MQTT over WebSocket/SSL listener.


**Config paths**

 - <code>listeners.wss.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WSS__$NAME</code>



**Fields**

- enabled: <code>boolean()</code>
  * default: 
  `true`

  启停监听器

- bind: <code>emqx_schema:ip_port() | integer()</code>
  * default: 
  `8084`


  监听套接字的 IP 地址和端口。


- acceptors: <code>pos_integer()</code>
  * default: 
  `16`

  监听器接收池的大小。

- max_connections: <code>infinity | pos_integer()</code>
  * default: 
  `infinity`


  监听器允许的最大并发连接数。


- mountpoint: <code>binary()</code>
  * default: 
  `""`


  发布或订阅时，请在所有主题前面加上 mountpoint 字符串。

  将消息传递给订阅者时，将从主题名称中删除带前缀的字符串。挂载点是一种用户可以用来实现不同侦听器之间消息路由隔离的方法。

  例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。</br>
  类似地，如果另一个客户端B（与客户端A连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息</br>

  设置为 <code>""</code> 以禁用该功能</br>

  mountpoint 字符串中的变量：
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username


- zone: <code>atom()</code>
  * default: 
  `default`


  监听器所属的配置组。


- limiter: <code>{$ratelimit_name -> emqx_limiter_schema:bucket_name()}</code>
  * default: 
  `{connection = "default"}`


  速率限制类型


- enable_authn: <code>boolean()</code>
  * default: 
  `true`


  配置 <code>true</code>（默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。


- access_rules: <code>[string()]</code>
  * default: 
  `["allow all"]`

  此监听器的访问控制规则。

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`


  如果EMQX集群部署在 HAProxy 或 Nginx 之后，请启用代理协议 V1/2 </br>
  详情见: https://www.haproxy.com/blog/haproxy/proxy-protocol/


- proxy_protocol_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"3s"`


  代理协议超时。如果在超时时间内未收到代理协议数据包，EMQX将关闭TCP连接。


- authentication: <code>[[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)]</code>


  监听器认证重载。

  认证配置可以是单个认证器实例，也可以是一个认证器数组组成的认证链。
  执行登录验证时（用户名、客户端 ID 等），将按配置的顺序执行</br>


- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>



- ssl_options: <code>[broker:listener_wss_opts](#broker-listener_wss_opts)</code>



- websocket: <code>[broker:ws_opts](#broker-ws_opts)</code>




## broker:overload_protection
Overload protection mechanism monitors the load of the system and temporarily
disables some features (such as accepting new connections) when the load is high.


**Config paths**

 - <code>overload_protection</code>


**Env overrides**

 - <code>EMQX_OVERLOAD_PROTECTION</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  是否对系统过载做出反应

- backoff_delay: <code>0..inf</code>
  * default: 
  `1`

  一些不重要的任务可能会延迟执行，以毫秒为单位设置延迟

- backoff_gc: <code>boolean()</code>
  * default: 
  `false`

  如有必要，跳过强制GC

- backoff_hibernation: <code>boolean()</code>
  * default: 
  `true`

  如有必要，跳过进程休眠

- backoff_new_conn: <code>boolean()</code>
  * default: 
  `true`

  如有必要，关闭新进来的连接


## broker:persistent_session_builtin
Settings for the built-in storage engine of persistent messages.


**Config paths**

 - <code>persistent_session_store.backend</code>


**Env overrides**

 - <code>EMQX_PERSISTENT_SESSION_STORE__BACKEND</code>



**Fields**

- type: <code>builtin</code>
  * default: 
  `builtin`



- session: <code>[broker:persistent_table_mria_opts](#broker-persistent_table_mria_opts)</code>

  用于内建会话表的性能调优参数

- session_messages: <code>[broker:persistent_table_mria_opts](#broker-persistent_table_mria_opts)</code>

  优化内置的会话消息表的配置。

- messages: <code>[broker:persistent_table_mria_opts](#broker-persistent_table_mria_opts)</code>

  用于内建消息表的性能调优参数


## broker:persistent_session_store
Settings for message persistence.


**Config paths**

 - <code>persistent_session_store</code>


**Env overrides**

 - <code>EMQX_PERSISTENT_SESSION_STORE</code>



**Fields**

- enabled: <code>boolean()</code>
  * default: 
  `false`

  使用数据库存储有关持久会话的信息。
  这使得在节点停止时，可以将客户端连接迁移到另一个群集节点。

- on_disc: <code>boolean()</code>
  * default: 
  `true`

  将持久会话数据保存在磁盘上。如果为 false 则存储在内存中。
  如开启， 持久会话数据可在集群重启后恢复。
  如关闭， 数据仅存储在内存中， 则在整个集群停止后丢失。

- ram_cache: <code>boolean()</code>
  * default: 
  `false`

  在内存中保持一份数据的副本，以便更快地访问。

- backend: <code>[broker:persistent_session_builtin](#broker-persistent_session_builtin)</code>
  * default: 

  ```
  {
    messages {ram_cache = "false"}
    session {ram_cache = "true"}
    session_messages {ram_cache = "true"}
    type = "builtin"
  }
  ```

  用于存储持久性会话和信息的数据库管理后端
  - `builtin`: 使用内置的数据库（mria）

- max_retain_undelivered: <code>emqx_schema:duration()</code>
  * default: 
  `"1h"`

  如果重新启动时处理上一个会话的节点已停止，则未传递到持久会话的消息在垃圾收集之前会被存储。

- message_gc_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"1h"`

  将未送达的消息垃圾收集到持久会话的开始间隔。
  这会影响检查 "max_retain_undelivered"（最大保留未送达）的删除频率。

- session_message_gc_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"1m"`

  持久会话消息的临时数据垃圾收集的开始间隔。
  这不会影响持久会话消息的生命周期长度。



## broker:persistent_table_mria_opts
Tuning options for the mria table.


**Config paths**

 - <code>persistent_session_store.backend.messages</code>
 - <code>persistent_session_store.backend.session</code>
 - <code>persistent_session_store.backend.session_messages</code>


**Env overrides**

 - <code>EMQX_PERSISTENT_SESSION_STORE__BACKEND__MESSAGES</code>
 - <code>EMQX_PERSISTENT_SESSION_STORE__BACKEND__SESSION</code>
 - <code>EMQX_PERSISTENT_SESSION_STORE__BACKEND__SESSION_MESSAGES</code>



**Fields**

- ram_cache: <code>boolean()</code>
  * default: 
  `true`

  在内存中保持一份数据的副本，以便更快地访问。


## broker:shared_subscription_group
Per group dispatch strategy for shared subscription


**Config paths**

 - <code>broker.shared_subscription_group.$name</code>


**Env overrides**

 - <code>EMQX_BROKER__SHARED_SUBSCRIPTION_GROUP__$NAME</code>



**Fields**

- strategy: <code>random | round_robin | sticky | local | hash_topic | hash_clientid</code>
  * default: 
  `random`




## broker:ssl_client_opts
Socket options for SSL clients.


**Config paths**

 - <code>authentication.$INDEX.ssl</code>
 - <code>authorization.sources.$INDEX.ssl</code>
 - <code>bridges.mqtt.$name.connector.ssl</code>
 - <code>bridges.webhook.$name.ssl</code>
 - <code>cluster.etcd.ssl</code>
 - <code>connectors.mqtt.$name.ssl</code>
 - <code>gateway.coap.authentication.ssl</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.exproto.authentication.ssl</code>
 - <code>gateway.exproto.handler.ssl_options</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.ssl</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.ssl</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.lwm2m.authentication.ssl</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.mqttsn.authentication.ssl</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.stomp.authentication.ssl</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.ssl</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.ssl</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.ssl</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.ssl</code>
 - <code>listeners.ws.$name.authentication.$INDEX.ssl</code>
 - <code>listeners.wss.$name.authentication.$INDEX.ssl</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX__SSL</code>
 - <code>EMQX_BRIDGES__MQTT__$NAME__CONNECTOR__SSL</code>
 - <code>EMQX_BRIDGES__WEBHOOK__$NAME__SSL</code>
 - <code>EMQX_CLUSTER__ETCD__SSL</code>
 - <code>EMQX_CONNECTORS__MQTT__$NAME__SSL</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__HANDLER__SSL_OPTIONS</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__SSL</code>



**Fields**

- cacertfile: <code>binary()</code>


  受信任的PEM格式CA证书捆绑文件</br>
  此文件中的证书用于验证TLS对等方的证书。
  如果要信任新CA，请将新证书附加到文件中。
  无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）</br>
  注意：从文件中失效（删除）证书不会影响已建立的连接。


- certfile: <code>binary()</code>


  PEM格式证书链文件</br>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，然后是直接颁发者证书，依此类推。
  虽然根CA证书是可选的，但它应该放在
  如果要添加文件，请将其删除。


- keyfile: <code>binary()</code>


  PEM格式的私钥文件。


- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`


  启用或禁用对等验证。


- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`


  启用 TLS 会话重用。


- depth: <code>integer()</code>
  * default: 
  `10`


  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。因此，如果深度为0，则对等方必须由受信任的根CA直接签名；如果1，路径可以是PEER、CA、ROOT-CA；如果是2，则路径可以是PEER、CA、CA、ROOT-CA等等。默认值为10。


- password: <code>string()</code>


  包含用户密码的字符串。
  仅在私钥文件受密码保护时使用。


- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2, tlsv1.1, tlsv1]`


  支持所有TLS/DTLS版本</br>

  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


- ciphers: <code>[string()]</code>
  * default: 
  `["TLS_AES_256_GCM_SHA384", "TLS_AES_128_GCM_SHA256", "TLS_CHACHA20_POLY1305_SHA256", "TLS_AES_128_CCM_SHA256", "TLS_AES_128_CCM_8_SHA256", "ECDHE-ECDSA-AES256-GCM-SHA384", "ECDHE-RSA-AES256-GCM-SHA384", "ECDHE-ECDSA-AES256-SHA384", "ECDHE-RSA-AES256-SHA384", "ECDH-ECDSA-AES256-GCM-SHA384", "ECDH-RSA-AES256-GCM-SHA384", "ECDH-ECDSA-AES256-SHA384", "ECDH-RSA-AES256-SHA384", "DHE-DSS-AES256-GCM-SHA384", "DHE-DSS-AES256-SHA256", "AES256-GCM-SHA384", "AES256-SHA256", "ECDHE-ECDSA-AES128-GCM-SHA256", "ECDHE-RSA-AES128-GCM-SHA256", "ECDHE-ECDSA-AES128-SHA256", "ECDHE-RSA-AES128-SHA256", "ECDH-ECDSA-AES128-GCM-SHA256", "ECDH-RSA-AES128-GCM-SHA256", "ECDH-ECDSA-AES128-SHA256", "ECDH-RSA-AES128-SHA256", "DHE-DSS-AES128-GCM-SHA256", "DHE-DSS-AES128-SHA256", "AES128-GCM-SHA256", "AES128-SHA256", "ECDHE-ECDSA-AES256-SHA", "ECDHE-RSA-AES256-SHA", "DHE-DSS-AES256-SHA", "ECDH-ECDSA-AES256-SHA", "ECDH-RSA-AES256-SHA", "ECDHE-ECDSA-AES128-SHA", "ECDHE-RSA-AES128-SHA", "DHE-DSS-AES128-SHA", "ECDH-ECDSA-AES128-SHA", "ECDH-RSA-AES128-SHA", "RSA-PSK-AES256-GCM-SHA384", "RSA-PSK-AES256-CBC-SHA384", "RSA-PSK-AES128-GCM-SHA256", "RSA-PSK-AES128-CBC-SHA256", "RSA-PSK-AES256-CBC-SHA", "RSA-PSK-AES128-CBC-SHA"]`


  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  </br>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式</br>
  注意：某些密码套件仅与特定的 TLS <code>版本</code> 兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  </br>
  注：PSK 的 Ciphers 不支持 tlsv1.3</br>
  如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在 <code>ssl.versions</code> 中禁用。

  </br>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code></br>


- user_lookup_fun: <code>string()</code>
  * default: 
  `"emqx_tls_psk:lookup"`


  用于查找预共享密钥（PSK）标识的 EMQX 内部回调。


- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`


  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


- enable: <code>boolean()</code>
  * default: 
  `false`

  启用 TLS

- server_name_indication: <code>disable | string()</code>


  指定要在 TLS 服务器名称指示扩展中使用的主机名</br>
  例如，当连接到 "server.example.net" 时，接受连接并执行 TLS 握手的真正服务器可能与 TLS 客户端最初连接到的主机不同，
  例如，当连接到 IP 地址时，或者当主机具有多个可解析的 DNS 记录时</br>
  如果未指定，它将默认为使用的主机名字符串
  建立连接，除非使用 IP 地址</br>
  然后，主机名也用于对等机的主机名验证
  证书</br>特殊值'disable'阻止发送服务器名称指示扩展，并禁用主机名验证检查。



## broker:stats
Enable/disable statistic data collection.
Statistic data such as message receive/send count/rate etc. It provides insights of system performance and helps to diagnose issues. You can find statistic data from the dashboard, or from the '/stats' API.


**Config paths**

 - <code>stats</code>


**Env overrides**

 - <code>EMQX_STATS</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用统计数据收集功能


## broker:sys_topics
The EMQX Broker periodically publishes its own status, message statistics,
client online and offline events to the system topic starting with `$SYS/`.

The following options control the behavior of `$SYS` topics.


**Config paths**

 - <code>sys_topics</code>


**Env overrides**

 - <code>EMQX_SYS_TOPICS</code>



**Fields**

- sys_msg_interval: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `"1m"`

  发送 `$SYS` 主题的间隔时间。

- sys_heartbeat_interval: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `"30s"`

  发送心跳系统消息的间隔时间，它包括：
    - `$SYS/brokers/<node>/uptime`
    - `$SYS/brokers/<node>/datetime`


- sys_event_messages: <code>[broker:event_names](#broker-event_names)</code>

  客户端事件消息


## broker:sysmon
Features related to system monitoring and introspection.


**Config paths**

 - <code>sysmon</code>


**Env overrides**

 - <code>EMQX_SYSMON</code>



**Fields**

- vm: <code>[broker:sysmon_vm](#broker-sysmon_vm)</code>



- os: <code>[broker:sysmon_os](#broker-sysmon_os)</code>



- top: <code>[broker:sysmon_top](#broker-sysmon_top)</code>




## broker:sysmon_os
This part of the configuration is responsible for monitoring
 the host OS health, such as free memory, disk space, CPU load, etc.


**Config paths**

 - <code>sysmon.os</code>


**Env overrides**

 - <code>EMQX_SYSMON__OS</code>



**Fields**

- cpu_check_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"60s"`

  定期 CPU 检查的时间间隔。

- cpu_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `"80%"`

  在发出相应警报之前可以使用多少系统 CPU 的阈值，以系统CPU负载的百分比表示。

- cpu_low_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `"60%"`

  在解除相应警报之前可以使用多少系统 CPU 的阈值，以系统CPU负载的百分比表示。

- mem_check_interval: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `"60s"`

  定期内存检查的时间间隔。

- sysmem_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `"70%"`

  在发出相应报警之前可以分配多少系统内存的阈值，以系统内存的百分比表示。

- procmem_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `"5%"`

  在发出相应警报之前，一个Erlang进程可以分配多少系统内存的阈值，以系统内存的百分比表示。


## broker:sysmon_top
This part of the configuration is responsible for monitoring
 the Erlang processes in the VM. This information can be sent to an external
 PostgreSQL database. This feature is inactive unless the PostgreSQL sink is configured.


**Config paths**

 - <code>sysmon.top</code>


**Env overrides**

 - <code>EMQX_SYSMON__TOP</code>



**Fields**

- num_items: <code>non_neg_integer()</code>
  * default: 
  `10`
  * mapping: 
  `system_monitor.top_num_items`

  每个监视组的顶级进程数

- sample_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"2s"`
  * mapping: 
  `system_monitor.top_sample_interval`

  指定应收集进程顶部的频率

- max_procs: <code>non_neg_integer()</code>
  * default: 
  `1000000`
  * mapping: 
  `system_monitor.top_max_procs`

  当VM中的进程数超过此值时，停止收集数据

- db_hostname: <code>string()</code>
  * default: 
  `[]`
  * mapping: 
  `system_monitor.db_hostname`

  收集数据点的 PostgreSQL 数据库的主机名

- db_port: <code>integer()</code>
  * default: 
  `5432`
  * mapping: 
  `system_monitor.db_port`

  收集数据点的 PostgreSQL 数据库的端口

- db_username: <code>string()</code>
  * default: 
  `"system_monitor"`
  * mapping: 
  `system_monitor.db_username`

  PostgreSQL 数据库的用户名

- db_password: <code>binary()</code>
  * default: 
  `"system_monitor_password"`
  * mapping: 
  `system_monitor.db_password`

  PostgreSQL 数据库的密码

- db_name: <code>string()</code>
  * default: 
  `"postgres"`
  * mapping: 
  `system_monitor.db_name`

  PostgreSQL 数据库的数据库名


## broker:sysmon_vm
This part of the configuration is responsible for collecting
 BEAM VM events, such as long garbage collection, traffic congestion in the inter-broker
 communication, etc.


**Config paths**

 - <code>sysmon.vm</code>


**Env overrides**

 - <code>EMQX_SYSMON__VM</code>



**Fields**

- process_check_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"30s"`

  定期进程限制检查的时间间隔。

- process_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `"80%"`

  在发出相应警报之前，本地节点上可以同时存在多少进程的阈值（以进程百分比表示）。

- process_low_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `"60%"`

  在清除相应警报之前，本地节点上可以同时存在多少进程的阈值（以进程百分比表示）。

- long_gc: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `disabled`

  启用长垃圾回收监控

- long_schedule: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `"240ms"`

  启用长调度监控

- large_heap: <code>disabled | emqx_schema:bytesize()</code>
  * default: 
  `"32MB"`

  启用大 heap 监控

- busy_dist_port: <code>boolean()</code>
  * default: 
  `true`

  启用分布式端口过忙监控

- busy_port: <code>boolean()</code>
  * default: 
  `true`

  启用端口过忙监控


## broker:tcp_opts
TCP listener options.


**Config paths**

 - <code>gateway.exproto.listeners.ssl.$name.tcp_options</code>
 - <code>gateway.exproto.listeners.tcp.$name.tcp_options</code>
 - <code>gateway.stomp.listeners.ssl.$name.tcp_options</code>
 - <code>gateway.stomp.listeners.tcp.$name.tcp_options</code>
 - <code>listeners.ssl.$name.tcp_options</code>
 - <code>listeners.tcp.$name.tcp_options</code>
 - <code>listeners.ws.$name.tcp_options</code>
 - <code>listeners.wss.$name.tcp_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_LISTENERS__WS__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__TCP_OPTIONS</code>



**Fields**

- active_n: <code>integer()</code>
  * default: 
  `100`


  为此套接字指定{active，N}选项</br>
  See: https://erlang.org/doc/man/inet.html#setopts-2


- backlog: <code>pos_integer()</code>
  * default: 
  `1024`


  TCP backlog 定义了挂起连接队列可以增长到的最大长度。


- send_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"15s"`


  连接的TCP发送超时。


- send_timeout_close: <code>boolean()</code>
  * default: 
  `true`


  如果发送超时，则关闭连接。


- recbuf: <code>emqx_schema:bytesize()</code>


  连接的 TCP 接收缓冲区（OS内核）。


- sndbuf: <code>emqx_schema:bytesize()</code>


  连接的 TCP 发送缓冲区（OS内核）。


- buffer: <code>emqx_schema:bytesize()</code>
  * default: 
  `"4KB"`


  驱动程序使用的用户空间缓冲区的大小。


- high_watermark: <code>emqx_schema:bytesize()</code>
  * default: 
  `"1MB"`


  当 VM 套接字实现内部排队的数据量达到此限制时，套接字将设置为忙碌状态。


- nodelay: <code>boolean()</code>
  * default: 
  `true`


  连接的 TCP_NODELAY 标识


- reuseaddr: <code>boolean()</code>
  * default: 
  `true`


  连接的 SO_REUSEADDR 标识



## broker:trace
Real-time filtering logs for the ClientID or Topic or IP for debugging.


**Config paths**

 - <code>trace</code>


**Env overrides**

 - <code>EMQX_TRACE</code>



**Fields**

- payload_encode: <code>hex | text | hidden</code>
  * default: 
  `text`


  确定跟踪文件中有效负载格式的格式</br>
  `text`：基于文本的协议或纯文本协议。
  建议在有效负载为JSON编码时使用</br>
  `hex`：二进制十六进制编码。当有效负载是自定义二进制协议时，建议使用此选项</br>
  `hidden`：有效负载被模糊化为 `******`



## broker:ws_opts
WebSocket listener options.


**Config paths**

 - <code>listeners.ws.$name.websocket</code>
 - <code>listeners.wss.$name.websocket</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WS__$NAME__WEBSOCKET</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__WEBSOCKET</code>



**Fields**

- mqtt_path: <code>string()</code>
  * default: 
  `"/mqtt"`


  WebSocket 的 MQTT 协议路径。因此，EMQX Broker的WebSocket地址为：
  <code>ws://{ip}:{port}/mqtt</code>


- mqtt_piggyback: <code>single | multiple</code>
  * default: 
  `multiple`


  WebSocket消息是否允许包含多个 MQTT 数据包。


- compress: <code>boolean()</code>
  * default: 
  `false`


  如果 <code>true</code>，则使用<code>zlib</code> 压缩 WebSocket 消息</br>
  <code>deflate_opts</code> 下的配置项属于压缩相关参数配置。


- idle_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"7200s"`


  关闭在此间隔内未发送 MQTT CONNECT 消息的客户端的传输层连接。


- max_frame_size: <code>infinity | integer()</code>
  * default: 
  `infinity`


  单个 MQTT 数据包的最大长度。


- fail_if_no_subprotocol: <code>boolean()</code>
  * default: 
  `true`


  如果<code>true</code>，当客户端未携带<code>Sec WebSocket Protocol</code>字段时，服务器将返回一个错误。
  </br>注意：微信小程序需要禁用此验证。


- supported_subprotocols: <code>emqx_schema:comma_separated_list()</code>
  * default: 
  `"mqtt, mqtt-v3, mqtt-v3.1.1, mqtt-v5"`


  逗号分隔的 subprotocols 支持列表。


- check_origin_enable: <code>boolean()</code>
  * default: 
  `false`


  如果<code>true</code>，<code>origin</code>HTTP 头将根据<code>check_origins</code>参数中配置的允许来源列表进行验证。


- allow_origin_absence: <code>boolean()</code>
  * default: 
  `true`


  If <code>false</code> and <code>check_origin_enable</code> is <code>true</code>, the server will reject requests that don't have <code>origin</code> HTTP header.


- check_origins: <code>emqx_schema:comma_separated_binary()</code>
  * default: 
  `"http://localhost:18083, http://127.0.0.1:18083"`


  允许的 origins 列表


- proxy_address_header: <code>string()</code>
  * default: 
  `"x-forwarded-for"`


  HTTP 头，用于传递有关客户端 IP 地址的信息。
  当 EMQX 集群部署在负载平衡器后面时，这一点非常重要。


- proxy_port_header: <code>string()</code>
  * default: 
  `"x-forwarded-port"`


  HTTP 头，用于传递有关客户端端口的信息。
  当 EMQX 集群部署在负载平衡器后面时，这一点非常重要。


- deflate_opts: <code>[broker:deflate_opts](#broker-deflate_opts)</code>




## broker:zone
A `Zone` defines a set of configuration items (such as the maximum number of connections) that can be shared between multiple listeners.

`Listener` can refer to a `Zone` through the configuration item <code>listener.\<Protocol>.\<Listener Name>.zone</code>.

The configs defined in the zones will override the global configs with the same key.

For example, given the following config:
```
a {
    b: 1, c: 1
}
zone.my_zone {
  a {
    b:2
  }
}
```

The global config `a` is overridden by the configs `a` inside the zone `my_zone`.

If there is a listener using the zone `my_zone`, the value of config `a` will be: `{b:2, c: 1}`.
Note that although the default value of `a.c` is `0`, the global value is used, i.e. configs in the zone have no default values. To override `a.c` one must configure it explicitly in the zone.

All the global configs that can be overridden in zones are:
 - `stats.*`
 - `mqtt.*`
 - `authorization.*`
 - `flapping_detect.*`
 - `force_shutdown.*`
 - `conn_congestion.*`
 - `force_gc.*`




**Config paths**

 - <code>zones.$name</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME</code>



**Fields**

- mqtt: <code>[zone:mqtt](#zone-mqtt)</code>



- stats: <code>[zone:stats](#zone-stats)</code>



- flapping_detect: <code>[zone:flapping_detect](#zone-flapping_detect)</code>



- force_shutdown: <code>[zone:force_shutdown](#zone-force_shutdown)</code>



- conn_congestion: <code>[zone:conn_congestion](#zone-conn_congestion)</code>



- force_gc: <code>[zone:force_gc](#zone-force_gc)</code>



- overload_protection: <code>[zone:overload_protection](#zone-overload_protection)</code>




## connector:connectors

EMQX 连接器的配置。</br>
连接器维护与外部资源相关的数据，比如 MySQL 数据库。



**Config paths**

 - <code>connectors</code>


**Env overrides**

 - <code>EMQX_CONNECTORS</code>



**Fields**

- mqtt: <code>{$name -> [connector-mqtt:connector](#connector-mqtt-connector)}</code>

  MQTT bridges。


## dashboard
EMQX仪表板配置


**Config paths**

 - <code>dashboard</code>


**Env overrides**

 - <code>EMQX_DASHBOARD</code>



**Fields**

- listeners: <code>[dashboard:listeners](#dashboard-listeners)</code>

  仪表盘监听器设置。

- default_username: <code>binary()</code>
  * default: 
  `"admin"`

  默认的仪表板用户名

- default_password: <code>binary()</code>
  * default: 
  `"public"`

  默认的仪表板用户密码
  为了安全，应该尽快修改密码。

- sample_interval: <code>emqx_schema:duration_s()</code>
  * default: 
  `"10s"`

  更新仪表板中显示的指标的时间间隔。必须小于60，且被60的整除。

- token_expired_time: <code>emqx_schema:duration()</code>
  * default: 
  `"60m"`

  JWT token 过期时间

- cors: <code>boolean()</code>
  * default: 
  `false`

  支持跨域资源共享(CORS)
  允许服务器指示任何来源(域名、协议或端口)，除了本服务器之外的任何浏览器应允许加载资源。

- i18n_lang: <code>en | zh</code>
  * default: 
  `en`

  swagger多语言支持


## dashboard:http
仪表板监听器(HTTP)配置


**Config paths**

 - <code>dashboard.listeners.http</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__LISTENERS__HTTP</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  忽略或启用该监听器配置

- bind: <code>non_neg_integer() | emqx_schema:ip_port()</code>
  * default: 
  `18083`

  监听的地址与端口，在dashboard更新此配置时，会重启dashboard服务。

- num_acceptors: <code>integer()</code>
  * default: 
  `4`

  TCP协议的Socket acceptor池大小

- max_connections: <code>integer()</code>
  * default: 
  `512`

  同时处理的最大连接数

- backlog: <code>integer()</code>
  * default: 
  `1024`

  排队等待连接的队列的最大长度

- send_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"5s"`

  Socket发送超时时间

- inet6: <code>boolean()</code>
  * default: 
  `false`

  启用IPv6， 如果机器不支持IPv6，请关闭此选项，否则会导致仪表盘无法使用。

- ipv6_v6only: <code>boolean()</code>
  * default: 
  `false`

  当开启 inet6 功能的同时禁用 IPv4-to-IPv6 映射。该配置仅在 inet6 功能开启时有效。


## dashboard:https
仪表板监听器(HTTPS)配置


**Config paths**

 - <code>dashboard.listeners.https</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__LISTENERS__HTTPS</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  忽略或启用该监听器配置

- bind: <code>non_neg_integer() | emqx_schema:ip_port()</code>
  * default: 
  `18084`

  监听的地址与端口，在dashboard更新此配置时，会重启dashboard服务。

- num_acceptors: <code>integer()</code>
  * default: 
  `4`

  TCP协议的Socket acceptor池大小

- max_connections: <code>integer()</code>
  * default: 
  `512`

  同时处理的最大连接数

- backlog: <code>integer()</code>
  * default: 
  `1024`

  排队等待连接的队列的最大长度

- send_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"5s"`

  Socket发送超时时间

- inet6: <code>boolean()</code>
  * default: 
  `false`

  启用IPv6， 如果机器不支持IPv6，请关闭此选项，否则会导致仪表盘无法使用。

- ipv6_v6only: <code>boolean()</code>
  * default: 
  `false`

  当开启 inet6 功能的同时禁用 IPv4-to-IPv6 映射。该配置仅在 inet6 功能开启时有效。

- cacertfile: <code>binary()</code>


  受信任的PEM格式CA证书捆绑文件</br>
  此文件中的证书用于验证TLS对等方的证书。
  如果要信任新CA，请将新证书附加到文件中。
  无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）</br>
  注意：从文件中失效（删除）证书不会影响已建立的连接。


- certfile: <code>binary()</code>


  PEM格式证书链文件</br>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，然后是直接颁发者证书，依此类推。
  虽然根CA证书是可选的，但它应该放在
  如果要添加文件，请将其删除。


- keyfile: <code>binary()</code>


  PEM格式的私钥文件。


- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`


  启用或禁用对等验证。


- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`


  启用 TLS 会话重用。


- depth: <code>integer()</code>
  * default: 
  `10`


  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。因此，如果深度为0，则对等方必须由受信任的根CA直接签名；如果1，路径可以是PEER、CA、ROOT-CA；如果是2，则路径可以是PEER、CA、CA、ROOT-CA等等。默认值为10。


- password: <code>string()</code>


  包含用户密码的字符串。
  仅在私钥文件受密码保护时使用。


- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2, tlsv1.1, tlsv1]`


  支持所有TLS/DTLS版本</br>

  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


- ciphers: <code>[string()]</code>
  * default: 
  `["TLS_AES_256_GCM_SHA384", "TLS_AES_128_GCM_SHA256", "TLS_CHACHA20_POLY1305_SHA256", "TLS_AES_128_CCM_SHA256", "TLS_AES_128_CCM_8_SHA256", "ECDHE-ECDSA-AES256-GCM-SHA384", "ECDHE-RSA-AES256-GCM-SHA384", "ECDHE-ECDSA-AES256-SHA384", "ECDHE-RSA-AES256-SHA384", "ECDH-ECDSA-AES256-GCM-SHA384", "ECDH-RSA-AES256-GCM-SHA384", "ECDH-ECDSA-AES256-SHA384", "ECDH-RSA-AES256-SHA384", "DHE-DSS-AES256-GCM-SHA384", "DHE-DSS-AES256-SHA256", "AES256-GCM-SHA384", "AES256-SHA256", "ECDHE-ECDSA-AES128-GCM-SHA256", "ECDHE-RSA-AES128-GCM-SHA256", "ECDHE-ECDSA-AES128-SHA256", "ECDHE-RSA-AES128-SHA256", "ECDH-ECDSA-AES128-GCM-SHA256", "ECDH-RSA-AES128-GCM-SHA256", "ECDH-ECDSA-AES128-SHA256", "ECDH-RSA-AES128-SHA256", "DHE-DSS-AES128-GCM-SHA256", "DHE-DSS-AES128-SHA256", "AES128-GCM-SHA256", "AES128-SHA256", "ECDHE-ECDSA-AES256-SHA", "ECDHE-RSA-AES256-SHA", "DHE-DSS-AES256-SHA", "ECDH-ECDSA-AES256-SHA", "ECDH-RSA-AES256-SHA", "ECDHE-ECDSA-AES128-SHA", "ECDHE-RSA-AES128-SHA", "DHE-DSS-AES128-SHA", "ECDH-ECDSA-AES128-SHA", "ECDH-RSA-AES128-SHA", "RSA-PSK-AES256-GCM-SHA384", "RSA-PSK-AES256-CBC-SHA384", "RSA-PSK-AES128-GCM-SHA256", "RSA-PSK-AES128-CBC-SHA256", "RSA-PSK-AES256-CBC-SHA", "RSA-PSK-AES128-CBC-SHA"]`


  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> 或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  </br>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式</br>
  注意：某些密码套件仅与特定的 TLS <code>版本</code> 兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  </br>
  注：PSK 的 Ciphers 不支持 tlsv1.3</br>
  如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在 <code>ssl.versions</code> 中禁用。

  </br>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code></br>


- user_lookup_fun: <code>string()</code>
  * default: 
  `"emqx_tls_psk:lookup"`


  用于查找预共享密钥（PSK）标识的 EMQX 内部回调。


- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`


  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


- dhfile: <code>string()</code>


  如果协商使用Diffie-Hellman密钥交换的密码套件，则服务器将使用包含PEM编码的Diffie-Hellman参数的文件的路径。如果未指定，则使用默认参数</br>

  注意：TLS 1.3不支持<code>dhfile</code>选项。


- honor_cipher_order: <code>boolean()</code>
  * default: 
  `true`


  一个重要的安全设置，它强制根据服务器指定的顺序而不是客户机指定的顺序设置密码，从而强制服务器管理员执行（通常配置得更正确）安全顺序。


- client_renegotiation: <code>boolean()</code>
  * default: 
  `true`


  在支持客户机发起的重新协商的协议中，这种操作的资源成本对于服务器来说高于客户机。
  这可能会成为拒绝服务攻击的载体。
  SSL 应用程序已经采取措施来反击此类尝试，但通过将此选项设置为 false，可以严格禁用客户端发起的重新协商。
  默认值为 true。请注意，由于基础密码套件可以加密的消息数量有限，禁用重新协商可能会导致长期连接变得不可用。


- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"15s"`


  握手完成所允许的最长时间



## dashboard:listeners
仪表板监听器配置


**Config paths**

 - <code>dashboard.listeners</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__LISTENERS</code>



**Fields**

- http: <code>[dashboard:http](#dashboard-http)</code>

  TCP listeners

- https: <code>[dashboard:https](#dashboard-https)</code>

  SSL listeners


## exhook
External hook (exhook) configuration.


**Config paths**

 - <code>exhook</code>


**Env overrides**

 - <code>EMQX_EXHOOK</code>



**Fields**

- servers: <code>[[exhook:server](#exhook-server)]</code>
  * default: 
  `[]`

  ExHook 服务器列表


## exhook:server
gRPC server configuration.


**Config paths**

 - <code>exhook.servers.$INDEX</code>


**Env overrides**

 - <code>EMQX_EXHOOK__SERVERS__$INDEX</code>



**Fields**

- name: <code>binary()</code>

  ExHook 服务器名称

- enable: <code>boolean()</code>
  * default: 
  `true`

  开启这个 Exhook 服务器

- url: <code>binary()</code>

  gRPC 服务器地址

- request_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"5s"`

  gRPC 服务器请求超时时间

- failed_action: <code>deny | ignore</code>
  * default: 
  `deny`

  当 gRPC 请求失败后的操作

- ssl: <code>[exhook:ssl_conf](#exhook-ssl_conf)</code>



- socket_options: <code>[exhook:socket_options](#exhook-socket_options)</code>
  * default: 
  `{keepalive = true, nodelay = true}`



- auto_reconnect: <code>false | emqx_schema:duration()</code>
  * default: 
  `"60s"`

  自动重连到 gRPC 服务器的设置。
  当 gRPC 服务器不可用时，Exhook 将会按照这里设置的间隔时间进行重连，并重新初始化注册的钩子

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  gRPC 客户端进程池大小


## exhook:socket_options
连接套接字设置


**Config paths**

 - <code>exhook.servers.$INDEX.socket_options</code>


**Env overrides**

 - <code>EMQX_EXHOOK__SERVERS__$INDEX__SOCKET_OPTIONS</code>



**Fields**

- keepalive: <code>boolean()</code>
  * default: 
  `true`

  当没有其他数据交换时，是否向连接的对端套接字定期的发送探测包。如果另一端没有响应，则认为连接断开，并向控制进程发送错误消息

- nodelay: <code>boolean()</code>
  * default: 
  `true`

  如果为 true，则为套接字设置 TCP_NODELAY 选项，这意味着会立即发送数据包

- recbuf: <code>emqx_schema:bytesize()</code>

  套接字的最小接收缓冲区大小

- sndbuf: <code>emqx_schema:bytesize()</code>

  套接字的最小发送缓冲区大小


## exhook:ssl_conf
SSL client configuration.


**Config paths**

 - <code>exhook.servers.$INDEX.ssl</code>


**Env overrides**

 - <code>EMQX_EXHOOK__SERVERS__$INDEX__SSL</code>



**Fields**

- cacertfile: <code>binary()</code>


  受信任的PEM格式CA证书捆绑文件</br>
  此文件中的证书用于验证TLS对等方的证书。
  如果要信任新CA，请将新证书附加到文件中。
  无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）</br>
  注意：从文件中失效（删除）证书不会影响已建立的连接。


- certfile: <code>binary()</code>


  PEM格式证书链文件</br>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，然后是直接颁发者证书，依此类推。
  虽然根CA证书是可选的，但它应该放在
  如果要添加文件，请将其删除。


- keyfile: <code>binary()</code>


  PEM格式的私钥文件。


- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`


  启用或禁用对等验证。


- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`


  启用 TLS 会话重用。


- depth: <code>integer()</code>
  * default: 
  `10`


  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。因此，如果深度为0，则对等方必须由受信任的根CA直接签名；如果1，路径可以是PEER、CA、ROOT-CA；如果是2，则路径可以是PEER、CA、CA、ROOT-CA等等。默认值为10。


- password: <code>string()</code>


  包含用户密码的字符串。
  仅在私钥文件受密码保护时使用。


- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2, tlsv1.1, tlsv1]`


  支持所有TLS/DTLS版本</br>

  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


- ciphers: <code>[string()]</code>
  * default: 
  `["TLS_AES_256_GCM_SHA384", "TLS_AES_128_GCM_SHA256", "TLS_CHACHA20_POLY1305_SHA256", "TLS_AES_128_CCM_SHA256", "TLS_AES_128_CCM_8_SHA256", "ECDHE-ECDSA-AES256-GCM-SHA384", "ECDHE-RSA-AES256-GCM-SHA384", "ECDHE-ECDSA-AES256-SHA384", "ECDHE-RSA-AES256-SHA384", "ECDH-ECDSA-AES256-GCM-SHA384", "ECDH-RSA-AES256-GCM-SHA384", "ECDH-ECDSA-AES256-SHA384", "ECDH-RSA-AES256-SHA384", "DHE-DSS-AES256-GCM-SHA384", "DHE-DSS-AES256-SHA256", "AES256-GCM-SHA384", "AES256-SHA256", "ECDHE-ECDSA-AES128-GCM-SHA256", "ECDHE-RSA-AES128-GCM-SHA256", "ECDHE-ECDSA-AES128-SHA256", "ECDHE-RSA-AES128-SHA256", "ECDH-ECDSA-AES128-GCM-SHA256", "ECDH-RSA-AES128-GCM-SHA256", "ECDH-ECDSA-AES128-SHA256", "ECDH-RSA-AES128-SHA256", "DHE-DSS-AES128-GCM-SHA256", "DHE-DSS-AES128-SHA256", "AES128-GCM-SHA256", "AES128-SHA256", "ECDHE-ECDSA-AES256-SHA", "ECDHE-RSA-AES256-SHA", "DHE-DSS-AES256-SHA", "ECDH-ECDSA-AES256-SHA", "ECDH-RSA-AES256-SHA", "ECDHE-ECDSA-AES128-SHA", "ECDHE-RSA-AES128-SHA", "DHE-DSS-AES128-SHA", "ECDH-ECDSA-AES128-SHA", "ECDH-RSA-AES128-SHA", "RSA-PSK-AES256-GCM-SHA384", "RSA-PSK-AES256-CBC-SHA384", "RSA-PSK-AES128-GCM-SHA256", "RSA-PSK-AES128-CBC-SHA256", "RSA-PSK-AES256-CBC-SHA", "RSA-PSK-AES128-CBC-SHA"]`


  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> 或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  </br>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式</br>
  注意：某些密码套件仅与特定的 TLS <code>版本</code> 兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  </br>
  注：PSK 的 Ciphers 不支持 tlsv1.3</br>
  如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在 <code>ssl.versions</code> 中禁用。

  </br>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code></br>


- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`


  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


- enable: <code>boolean()</code>
  * default: 
  `false`

  启用 TLS

- server_name_indication: <code>disable | string()</code>


  指定要在 TLS 服务器名称指示扩展中使用的主机名</br>
  例如，当连接到 "server.example.net" 时，接受连接并执行 TLS 握手的真正服务器可能与 TLS 客户端最初连接到的主机不同，
  例如，当连接到 IP 地址时，或者当主机具有多个可解析的 DNS 记录时</br>
  如果未指定，它将默认为使用的主机名字符串
  建立连接，除非使用 IP 地址</br>
  然后，主机名也用于对等机的主机名验证
  证书</br>特殊值'disable'阻止发送服务器名称指示扩展，并禁用主机名验证检查。



## gateway:clientinfo_override
ClientInfo override.


**Config paths**

 - <code>gateway.coap.clientinfo_override</code>
 - <code>gateway.exproto.clientinfo_override</code>
 - <code>gateway.lwm2m.clientinfo_override</code>
 - <code>gateway.mqttsn.clientinfo_override</code>
 - <code>gateway.stomp.clientinfo_override</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__CLIENTINFO_OVERRIDE</code>
 - <code>EMQX_GATEWAY__EXPROTO__CLIENTINFO_OVERRIDE</code>
 - <code>EMQX_GATEWAY__LWM2M__CLIENTINFO_OVERRIDE</code>
 - <code>EMQX_GATEWAY__MQTTSN__CLIENTINFO_OVERRIDE</code>
 - <code>EMQX_GATEWAY__STOMP__CLIENTINFO_OVERRIDE</code>



**Fields**

- username: <code>binary()</code>

  username 重写模板

- password: <code>binary()</code>

  password 重写模板

- clientid: <code>binary()</code>

  clientid 重写模板


## gateway:coap
The CoAP protocol gateway provides EMQX with the access capability of the CoAP protocol.
It allows publishing, subscribing, and receiving messages to EMQX in accordance
with a certain defined CoAP message format.


**Config paths**

 - <code>gateway.coap</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP</code>



**Fields**

- heartbeat: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"30s"`

  CoAP 网关要求客户端的最小心跳间隔时间。
  当 <code>connection_required</code> 开启后，该参数用于检查客户端连接是否存活

- connection_required: <code>boolean()</code>
  * default: 
  `false`

  是否开启连接模式。
  连接模式是非标准协议的功能。它维护 CoAP 客户端上线、认证、和连接状态的保持

- notify_type: <code>non | con | qos</code>
  * default: 
  `qos`

  投递给 CoAP 客户端的通知消息类型。当客户端 Observe 一个资源（或订阅某个主题）时，网关会向客户端推送新产生的消息。其消息类型可设置为：
    - non: 不需要客户端返回确认消息;
    - con: 需要客户端返回一个确认消息;
    - qos: 取决于消息的 QoS 等级; QoS 0 会以 `non` 类型下发，QoS 1/2 会以 `con` 类型下发


- subscribe_qos: <code>qos0 | qos1 | qos2 | coap</code>
  * default: 
  `coap`

  客户端订阅请求的默认 QoS 等级。
  当 CoAP 客户端发起订阅请求时，如果未携带 `qos` 参数则会使用该默认值。默认值可设置为：
    - qos0, qos1, qos2: 设置为固定的 QoS 等级
    - coap: 依据订阅操作的 CoAP 报文类型来动态决定
      * 当订阅请求为 `non-confirmable` 类型时，取值为 qos0
      * 当订阅请求为 `confirmable` 类型时，取值为 qos1


- publish_qos: <code>qos0 | qos1 | qos2 | coap</code>
  * default: 
  `coap`

  客户端发布请求的默认 QoS 等级。
  当 CoAP 客户端发起发布请求时，如果未携带 `qos` 参数则会使用该默认值。默认值可设置为：
    - qos0, qos1, qos2: 设置为固定的 QoS 等级
    - coap: 依据发布操作的 CoAP 报文类型来动态决定
      * 当发布请求为 `non-confirmable` 类型时，取值为 qos0
      * 当发布请求为 `confirmable` 类型时，取值为 qos1
   

- mountpoint: <code>binary()</code>
  * default: 
  `""`

   

- listeners: <code>[gateway:udp_listeners](#gateway-udp_listeners)</code>

  配置 UDP 类型的监听器

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该网关

- enable_stats: <code>boolean()</code>
  * default: 
  `true`

  是否开启客户端统计

- idle_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"30s"`

  客户端连接过程的空闲时间。该配置用于：
    1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
    2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo 重写。

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段


## gateway:dtls_listener
Settings for the DTLS listener.


**Config paths**

 - <code>gateway.coap.listeners.dtls.$name</code>
 - <code>gateway.exproto.listeners.dtls.$name</code>
 - <code>gateway.lwm2m.listeners.dtls.$name</code>
 - <code>gateway.mqttsn.listeners.dtls.$name</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME</code>



**Fields**

- acceptors: <code>integer()</code>
  * default: 
  `16`

  Acceptor 进程池大小。

- udp_options: <code>[gateway:udp_opts](#gateway-udp_opts)</code>



- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该监听器。

- bind: <code>emqx_gateway_schema:ip_port() | integer()</code>

  监听器绑定的 IP 地址或端口。

- max_connections: <code>integer()</code>
  * default: 
  `1024`

  监听器支持的最大连接数。

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  监听器支持的最大连接速率。

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  配置 <code>true</code>（默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。

- mountpoint: <code>binary()</code>

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，则客户端实际上订阅了 `some_tenant/t` 主题。类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有主题名称 `t`。设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：
     - <code>${clientid}</code>：clientid
     - <code>${username}</code>：用户名


- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  配置监听器的访问控制规则。
  见：https://github.com/emqtt/esockd#allowdeny

- dtls_options: <code>[gateway:dtls_opts](#gateway-dtls_opts)</code>

  DTLS Socket 配置


## gateway:dtls_opts
Settings for the DTLS protocol.


**Config paths**

 - <code>gateway.coap.listeners.dtls.$name.dtls_options</code>
 - <code>gateway.exproto.listeners.dtls.$name.dtls_options</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.dtls_options</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.dtls_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__DTLS_OPTIONS</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__DTLS_OPTIONS</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__DTLS_OPTIONS</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__DTLS_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>


  受信任的 PEM 格式 CA 证书捆绑文件</br>
  此文件中的证书用于验证 TLS 对等方的证书。
  如果要信任新 CA，请将新证书附加到文件中。
  无需重启 EMQX 即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）</br>
  注意：从文件中失效（删除）证书不会影响已建立的连接。


- certfile: <code>binary()</code>


  PEM 格式证书链文件</br>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，然后是直接颁发者证书，依此类推。
  虽然根 CA 证书是可选的，但它应该放在
  如果要添加文件，请将其删除。


- keyfile: <code>binary()</code>


  PEM格式的私钥文件。


- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`


  启用或禁用对等验证。


- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`


  启用 TLS 会话重用。


- depth: <code>integer()</code>
  * default: 
  `10`


  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。因此，如果深度为 0，则对等方必须由受信任的根 CA 直接签名；如果 1，路径可以是 PEER、CA、ROOT-CA；如果是 2，则路径可以是 PEER、CA、CA、ROOT-CA 等等。默认值为 10。


- password: <code>string()</code>


  包含用户密码的字符串。
  仅在私钥文件受密码保护时使用。


- versions: <code>[atom()]</code>
  * default: 
  `[dtlsv1.2, dtlsv1]`


  支持所有 TLS/DTLS 版本</br>

  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


- ciphers: <code>[string()]</code>
  * default: 
  `["ECDHE-ECDSA-AES256-GCM-SHA384", "ECDHE-RSA-AES256-GCM-SHA384", "ECDHE-ECDSA-AES256-SHA384", "ECDHE-RSA-AES256-SHA384", "ECDH-ECDSA-AES256-GCM-SHA384", "ECDH-RSA-AES256-GCM-SHA384", "ECDH-ECDSA-AES256-SHA384", "ECDH-RSA-AES256-SHA384", "DHE-DSS-AES256-GCM-SHA384", "DHE-DSS-AES256-SHA256", "AES256-GCM-SHA384", "AES256-SHA256", "ECDHE-ECDSA-AES128-GCM-SHA256", "ECDHE-RSA-AES128-GCM-SHA256", "ECDHE-ECDSA-AES128-SHA256", "ECDHE-RSA-AES128-SHA256", "ECDH-ECDSA-AES128-GCM-SHA256", "ECDH-RSA-AES128-GCM-SHA256", "ECDH-ECDSA-AES128-SHA256", "ECDH-RSA-AES128-SHA256", "DHE-DSS-AES128-GCM-SHA256", "DHE-DSS-AES128-SHA256", "AES128-GCM-SHA256", "AES128-SHA256", "ECDHE-ECDSA-AES256-SHA", "ECDHE-RSA-AES256-SHA", "DHE-DSS-AES256-SHA", "ECDH-ECDSA-AES256-SHA", "ECDH-RSA-AES256-SHA", "ECDHE-ECDSA-AES128-SHA", "ECDHE-RSA-AES128-SHA", "DHE-DSS-AES128-SHA", "ECDH-ECDSA-AES128-SHA", "ECDH-RSA-AES128-SHA", "RSA-PSK-AES256-GCM-SHA384", "RSA-PSK-AES256-CBC-SHA384", "RSA-PSK-AES128-GCM-SHA256", "RSA-PSK-AES128-CBC-SHA256", "RSA-PSK-AES256-CBC-SHA", "RSA-PSK-AES128-CBC-SHA"]`


  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> 或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  </br>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式</br>
  注意：某些密码套件仅与特定的 TLS <code>版本</code> 兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  </br>
  注：PSK 的 Ciphers 不支持 tlsv1.3</br>
  如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在 <code>ssl.versions</code> 中禁用。

  </br>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code></br>


- user_lookup_fun: <code>string()</code>
  * default: 
  `"emqx_tls_psk:lookup"`


  用于查找预共享密钥（PSK）标识的 EMQX 内部回调。


- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`


  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


- dhfile: <code>string()</code>


  如果协商使用 Diffie-Hellman 密钥交换的密码套件，则服务器将使用包含 PEM 编码的 Diffie-Hellman 参数的文件的路径。如果未指定，则使用默认参数</br>

  注意：TLS 1.3 不支持 <code>dhfile</code> 选项。


- fail_if_no_peer_cert: <code>boolean()</code>
  * default: 
  `false`


  TLS/DTLS 服务器与 {verify，verify_peer} 一起使用。
  如果设置为 true，则如果客户端没有要发送的证书，即发送空证书，服务器将失败。
  如果设置为 false，则仅当客户端发送无效证书（空证书被视为有效证书）时才会失败。


- honor_cipher_order: <code>boolean()</code>
  * default: 
  `true`


  一个重要的安全设置，它强制根据服务器指定的顺序而不是客户机指定的顺序设置密码，从而强制服务器管理员执行（通常配置得更正确）安全顺序。


- client_renegotiation: <code>boolean()</code>
  * default: 
  `true`


  在支持客户机发起的重新协商的协议中，这种操作的资源成本对于服务器来说高于客户机。
  这可能会成为拒绝服务攻击的载体。
  SSL 应用程序已经采取措施来反击此类尝试，但通过将此选项设置为 false，可以严格禁用客户端发起的重新协商。
  默认值为 true。请注意，由于基础密码套件可以加密的消息数量有限，禁用重新协商可能会导致长期连接变得不可用。



## gateway:exproto
Settings for EMQX extension protocol (exproto).


**Config paths**

 - <code>gateway.exproto</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO</code>



**Fields**

- server: <code>[gateway:exproto_grpc_server](#gateway-exproto_grpc_server)</code>

  配置 ExProto 网关需要启动的 <code>ConnectionAdapter</code> 服务。
  该服务用于提供客户端的认证、发布、订阅和数据下行等功能。

- handler: <code>[gateway:exproto_grpc_handler](#gateway-exproto_grpc_handler)</code>

  配置 ExProto 网关需要请求的 <code>ConnectionHandler</code> 服务地址。
  该服务用于给 ExProto 提供客户端的 Socket 事件处理、字节解码、订阅消息接收等功能。

- mountpoint: <code>binary()</code>
  * default: 
  `""`

   

- listeners: <code>[gateway:tcp_udp_listeners](#gateway-tcp_udp_listeners)</code>

  监听器配置

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该网关

- enable_stats: <code>boolean()</code>
  * default: 
  `true`

  是否开启客户端统计

- idle_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"30s"`

  客户端连接过程的空闲时间。该配置用于：
    1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
    2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo 重写。

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段


## gateway:exproto_grpc_handler
Settings for the exproto gRPC connection handler.


**Config paths**

 - <code>gateway.exproto.handler</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__HANDLER</code>



**Fields**

- address: <code>binary()</code>

  对端 gRPC 服务器地址。

- ssl_options: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>

  gRPC 客户端的 SSL 配置。


## gateway:exproto_grpc_server
Settings for the exproto gRPC server.


**Config paths**

 - <code>gateway.exproto.server</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__SERVER</code>



**Fields**

- bind: <code>emqx_gateway_schema:ip_port() | integer()</code>

  服务监听地址和端口。

- ssl_options: <code>[gateway:ssl_server_opts](#gateway-ssl_server_opts)</code>

  服务 SSL 配置。


## gateway
EMQX Gateway configuration root.


**Config paths**

 - <code>gateway</code>


**Env overrides**

 - <code>EMQX_GATEWAY</code>



**Fields**

- stomp: <code>[gateway:stomp](#gateway-stomp)</code>

  Stomp 网关配置。当前实现支持 v1.2/1.1/1.0 协议版本

- mqttsn: <code>[gateway:mqttsn](#gateway-mqttsn)</code>

  MQTT-SN 网关配置。当前实现仅支持 v1.2 版本

- coap: <code>[gateway:coap](#gateway-coap)</code>

  CoAP 网关配置。
  该网关的实现基于 RFC-7252 和 https://core-wg.github.io/coap-pubsub/draft-ietf-core-pubsub.html

- lwm2m: <code>[gateway:lwm2m](#gateway-lwm2m)</code>

  LwM2M 网关配置。仅支持 v1.0.1 协议

- exproto: <code>[gateway:exproto](#gateway-exproto)</code>

  ExProto 网关


## gateway:lwm2m
The LwM2M protocol gateway.


**Config paths**

 - <code>gateway.lwm2m</code>


**Env overrides**

 - <code>EMQX_GATEWAY__LWM2M</code>



**Fields**

- xml_dir: <code>binary()</code>
  * default: 
  `"etc/lwm2m_xml"`

  LwM2M Resource 定义的 XML 文件目录路径

- lifetime_min: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"15s"`

  允许 LwM2M 客户端允许设置的心跳最小值

- lifetime_max: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"86400s"`

  允许 LwM2M 客户端允许设置的心跳最大值

- qmode_time_window: <code>emqx_gateway_schema:duration_s()</code>
  * default: 
  `"22s"`

  在 QMode 模式下，LwM2M 网关认为网络链接有效的时间窗口的值。
  例如，在收到客户端的更新信息后，在这个时间窗口内的任何信息都会直接发送到 LwM2M 客户端，而超过这个时间窗口的所有信息都会暂时储存在内存中

- auto_observe: <code>boolean()</code>
  * default: 
  `false`

  自动 Observe REGISTER 数据包的 Object 列表

- update_msg_publish_condition: <code>always | contains_object_list</code>
  * default: 
  `contains_object_list`

  发布UPDATE事件消息的策略。
    - always: 只要收到 UPDATE 请求，就发送更新事件。
    - contains_object_list: 仅当 UPDATE 请求携带 Object 列表时才发送更新事件


- translators: <code>[gateway:lwm2m_translators](#gateway-lwm2m_translators)</code>

  LwM2M 网关订阅/发布消息的主题映射配置

- mountpoint: <code>binary()</code>
  * default: 
  `"lwm2m/${endpoint_name}/"`

   

- listeners: <code>[gateway:udp_listeners](#gateway-udp_listeners)</code>

  配置 UDP 类型的监听器

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该网关

- enable_stats: <code>boolean()</code>
  * default: 
  `true`

  是否开启客户端统计

- idle_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"30s"`

  客户端连接过程的空闲时间。该配置用于：
    1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
    2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo 重写。

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段


## gateway:lwm2m_translators
MQTT topics that correspond to LwM2M events.


**Config paths**

 - <code>gateway.lwm2m.translators</code>


**Env overrides**

 - <code>EMQX_GATEWAY__LWM2M__TRANSLATORS</code>



**Fields**

- command: <code>[gateway:translator](#gateway-translator)</code>

  下行命令主题。
  对于每个成功上线的新 LwM2M 客户端，网关会创建一个订阅关系来接收下行消息并将其发送给客户端

- response: <code>[gateway:translator](#gateway-translator)</code>

  用于网关发布来自 LwM2M 客户端的确认事件的主题

- notify: <code>[gateway:translator](#gateway-translator)</code>

  用于发布来自 LwM2M 客户端的通知事件的主题。
  在成功 Observe 到 LwM2M 客户端的资源后，如果客户端报告任何资源状态的变化，网关将通过该主题发送通知事件

- register: <code>[gateway:translator](#gateway-translator)</code>

  用于发布来自 LwM2M 客户端的注册事件的主题

- update: <code>[gateway:translator](#gateway-translator)</code>

  用于发布来自LwM2M客户端的更新事件的主题


## gateway:mqttsn
The MQTT-SN (MQTT for Sensor Networks) protocol gateway.


**Config paths**

 - <code>gateway.mqttsn</code>


**Env overrides**

 - <code>EMQX_GATEWAY__MQTTSN</code>



**Fields**

- gateway_id: <code>integer()</code>
  * default: 
  `1`

  MQTT-SN 网关 ID。
  当 <code>broadcast</code> 打开时，MQTT-SN 网关会使用该 ID 来广播 ADVERTISE 消息

- broadcast: <code>boolean()</code>
  * default: 
  `false`

  是否周期性广播 ADVERTISE 消息 

- enable_qos3: <code>boolean()</code>
  * default: 
  `true`

  是否允许无连接的客户端发送 QoS 等于 -1 的消息。
  该功能主要用于支持轻量的 MQTT-SN 客户端实现，它不会向网关建立连接，注册主题，也不会发起订阅；它只使用 QoS 为 -1 来发布消息

- subs_resume: <code>boolean()</code>
  * default: 
  `false`

  在会话被重用后，网关是否主动向客户端注册对已订阅主题名称

- predefined: <code>[[gateway:mqttsn_predefined](#gateway-mqttsn_predefined)]</code>
  * default: 
  `[]`

  预定义主题列表。
  预定义的主题列表，是一组 主题 ID 和 主题名称 的映射关系。使用预先定义的主题列表，可以减少 MQTT-SN 客户端和网关对于固定主题的注册请求

- mountpoint: <code>binary()</code>
  * default: 
  `""`

   

- listeners: <code>[gateway:udp_listeners](#gateway-udp_listeners)</code>

  配置 UDP 类型的监听器

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该网关

- enable_stats: <code>boolean()</code>
  * default: 
  `true`

  是否开启客户端统计

- idle_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"30s"`

  客户端连接过程的空闲时间。该配置用于：
    1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
    2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo 重写。

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段


## gateway:mqttsn_predefined
The pre-defined topic name corresponding to the pre-defined topic
ID of N.

Note: the pre-defined topic ID of 0 is reserved.


**Config paths**

 - <code>gateway.mqttsn.predefined.$INDEX</code>


**Env overrides**

 - <code>EMQX_GATEWAY__MQTTSN__PREDEFINED__$INDEX</code>



**Fields**

- id: <code>integer()</code>

  主题 ID。范围：1-65535 

- topic: <code>binary()</code>

  主题名称。注：不支持通配符


## gateway:ssl_listener
Settings for the SSL listener.


**Config paths**

 - <code>gateway.exproto.listeners.ssl.$name</code>
 - <code>gateway.stomp.listeners.ssl.$name</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME</code>



**Fields**

- acceptors: <code>integer()</code>
  * default: 
  `16`

  Acceptor 进程池大小。

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>

  TCP Socket 配置

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`

  是否开启 Proxy Protocol V1/2。当 EMQX 集群部署在 HAProxy 或 Nginx 后需要获取客户端真实 IP 时常用到该选项。参考：https://www.haproxy.com/blog/haproxy/proxy-protocol/

- proxy_protocol_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"15s"`

  接收 Proxy Protocol 报文头的超时时间。如果在超时内没有收到 Proxy Protocol 包，EMQX 将关闭 TCP 连接。

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该监听器。

- bind: <code>emqx_gateway_schema:ip_port() | integer()</code>

  监听器绑定的 IP 地址或端口。

- max_connections: <code>integer()</code>
  * default: 
  `1024`

  监听器支持的最大连接数。

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  监听器支持的最大连接速率。

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  配置 <code>true</code>（默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。

- mountpoint: <code>binary()</code>

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，则客户端实际上订阅了 `some_tenant/t` 主题。类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有主题名称 `t`。设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：
     - <code>${clientid}</code>：clientid
     - <code>${username}</code>：用户名


- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  配置监听器的访问控制规则。
  见：https://github.com/emqtt/esockd#allowdeny

- ssl_options: <code>[broker:listener_ssl_opts](#broker-listener_ssl_opts)</code>

  SSL Socket 配置


## gateway:ssl_server_opts
SSL configuration for the server.


**Config paths**

 - <code>gateway.exproto.server.ssl_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__SERVER__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>


  受信任的 PEM 格式 CA 证书捆绑文件</br>
  此文件中的证书用于验证 TLS 对等方的证书。
  如果要信任新 CA，请将新证书附加到文件中。
  无需重启 EMQX 即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）</br>
  注意：从文件中失效（删除）证书不会影响已建立的连接。


- certfile: <code>binary()</code>


  PEM 格式证书链文件</br>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，然后是直接颁发者证书，依此类推。
  虽然根 CA 证书是可选的，但它应该放在
  如果要添加文件，请将其删除。


- keyfile: <code>binary()</code>


  PEM格式的私钥文件。


- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`


  启用或禁用对等验证。


- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`


  启用 TLS 会话重用。


- depth: <code>integer()</code>
  * default: 
  `10`


  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。因此，如果深度为 0，则对等方必须由受信任的根 CA 直接签名；如果 1，路径可以是 PEER、CA、ROOT-CA；如果是 2，则路径可以是 PEER、CA、CA、ROOT-CA 等等。默认值为 10。


- password: <code>string()</code>


  包含用户密码的字符串。
  仅在私钥文件受密码保护时使用。


- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2, tlsv1.1, tlsv1]`


  支持所有 TLS/DTLS 版本</br>

  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


- ciphers: <code>[string()]</code>
  * default: 
  `["TLS_AES_256_GCM_SHA384", "TLS_AES_128_GCM_SHA256", "TLS_CHACHA20_POLY1305_SHA256", "TLS_AES_128_CCM_SHA256", "TLS_AES_128_CCM_8_SHA256", "ECDHE-ECDSA-AES256-GCM-SHA384", "ECDHE-RSA-AES256-GCM-SHA384", "ECDHE-ECDSA-AES256-SHA384", "ECDHE-RSA-AES256-SHA384", "ECDH-ECDSA-AES256-GCM-SHA384", "ECDH-RSA-AES256-GCM-SHA384", "ECDH-ECDSA-AES256-SHA384", "ECDH-RSA-AES256-SHA384", "DHE-DSS-AES256-GCM-SHA384", "DHE-DSS-AES256-SHA256", "AES256-GCM-SHA384", "AES256-SHA256", "ECDHE-ECDSA-AES128-GCM-SHA256", "ECDHE-RSA-AES128-GCM-SHA256", "ECDHE-ECDSA-AES128-SHA256", "ECDHE-RSA-AES128-SHA256", "ECDH-ECDSA-AES128-GCM-SHA256", "ECDH-RSA-AES128-GCM-SHA256", "ECDH-ECDSA-AES128-SHA256", "ECDH-RSA-AES128-SHA256", "DHE-DSS-AES128-GCM-SHA256", "DHE-DSS-AES128-SHA256", "AES128-GCM-SHA256", "AES128-SHA256", "ECDHE-ECDSA-AES256-SHA", "ECDHE-RSA-AES256-SHA", "DHE-DSS-AES256-SHA", "ECDH-ECDSA-AES256-SHA", "ECDH-RSA-AES256-SHA", "ECDHE-ECDSA-AES128-SHA", "ECDHE-RSA-AES128-SHA", "DHE-DSS-AES128-SHA", "ECDH-ECDSA-AES128-SHA", "ECDH-RSA-AES128-SHA", "RSA-PSK-AES256-GCM-SHA384", "RSA-PSK-AES256-CBC-SHA384", "RSA-PSK-AES128-GCM-SHA256", "RSA-PSK-AES128-CBC-SHA256", "RSA-PSK-AES256-CBC-SHA", "RSA-PSK-AES128-CBC-SHA"]`


  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  </br>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式</br>
  注意：某些密码套件仅与特定的 TLS <code>版本</code> 兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  </br>
  注：PSK 的 Ciphers 不支持 tlsv1.3</br>
  如果打算使用 PSK 密码套件 <code>tlsv1.3</code>。应在 <code>ssl.versions</code> 中禁用。

  </br>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code></br>


- user_lookup_fun: <code>string()</code>
  * default: 
  `"emqx_tls_psk:lookup"`


  用于查找预共享密钥（PSK）标识的 EMQX 内部回调。


- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`


  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


- dhfile: <code>string()</code>


  如果协商使用 Diffie-Hellman 密钥交换的密码套件，则服务器将使用包含 PEM 编码的 Diffie-Hellman 参数的文件的路径。如果未指定，则使用默认参数</br>

  注意：TLS 1.3 不支持 <code>dhfile</code> 选项。


- fail_if_no_peer_cert: <code>boolean()</code>
  * default: 
  `false`


  TLS/DTLS 服务器与 {verify，verify_peer} 一起使用。
  如果设置为true，则如果客户端没有要发送的证书，即发送空证书，服务器将失败。
  如果设置为false，则仅当客户端发送无效证书（空证书被视为有效证书）时才会失败。


- honor_cipher_order: <code>boolean()</code>
  * default: 
  `true`


  一个重要的安全设置，它强制根据服务器指定的顺序而不是客户机指定的顺序设置密码，从而强制服务器管理员执行（通常配置得更正确）安全顺序。


- client_renegotiation: <code>boolean()</code>
  * default: 
  `true`


  在支持客户机发起的重新协商的协议中，这种操作的资源成本对于服务器来说高于客户机。
  这可能会成为拒绝服务攻击的载体。
  SSL 应用程序已经采取措施来反击此类尝试，但通过将此选项设置为 false，可以严格禁用客户端发起的重新协商。
  默认值为 true。请注意，由于基础密码套件可以加密的消息数量有限，禁用重新协商可能会导致长期连接变得不可用。


- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"15s"`


  握手完成所允许的最长时间



## gateway:stomp
The STOMP protocol gateway provides EMQX with the ability to access STOMP
(Simple (or Streaming) Text Orientated Messaging Protocol) protocol.


**Config paths**

 - <code>gateway.stomp</code>


**Env overrides**

 - <code>EMQX_GATEWAY__STOMP</code>



**Fields**

- frame: <code>[gateway:stomp_frame](#gateway-stomp_frame)</code>



- mountpoint: <code>binary()</code>
  * default: 
  `""`

   

- listeners: <code>[gateway:tcp_listeners](#gateway-tcp_listeners)</code>

  配置 TCP 类型的监听器

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该网关

- enable_stats: <code>boolean()</code>
  * default: 
  `true`

  是否开启客户端统计

- idle_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"30s"`

  客户端连接过程的空闲时间。该配置用于：
    1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
    2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo 重写。

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段


## gateway:stomp_frame
Size limits for the STOMP frames.


**Config paths**

 - <code>gateway.stomp.frame</code>


**Env overrides**

 - <code>EMQX_GATEWAY__STOMP__FRAME</code>



**Fields**

- max_headers: <code>non_neg_integer()</code>
  * default: 
  `10`

  允许的 Header 最大数量

- max_headers_length: <code>non_neg_integer()</code>
  * default: 
  `1024`

  允许的 Header 字符串的最大长度

- max_body_length: <code>integer()</code>
  * default: 
  `65536`

  允许的 Stomp 报文 Body 的最大字节数


## gateway:tcp_listener
Settings for the TCP listener.


**Config paths**

 - <code>gateway.exproto.listeners.tcp.$name</code>
 - <code>gateway.stomp.listeners.tcp.$name</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME</code>



**Fields**

- acceptors: <code>integer()</code>
  * default: 
  `16`

  Acceptor 进程池大小。

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>

  TCP Socket 配置

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`

  是否开启 Proxy Protocol V1/2。当 EMQX 集群部署在 HAProxy 或 Nginx 后需要获取客户端真实 IP 时常用到该选项。参考：https://www.haproxy.com/blog/haproxy/proxy-protocol/

- proxy_protocol_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"15s"`

  接收 Proxy Protocol 报文头的超时时间。如果在超时内没有收到 Proxy Protocol 包，EMQX 将关闭 TCP 连接。

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该监听器。

- bind: <code>emqx_gateway_schema:ip_port() | integer()</code>

  监听器绑定的 IP 地址或端口。

- max_connections: <code>integer()</code>
  * default: 
  `1024`

  监听器支持的最大连接数。

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  监听器支持的最大连接速率。

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。

- mountpoint: <code>binary()</code>

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，则客户端实际上订阅了 `some_tenant/t` 主题。类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有主题名称 `t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：
     - <code>${clientid}</code>：clientid
     - <code>${username}</code>：用户名


- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  配置监听器的访问控制规则。
  见：https://github.com/emqtt/esockd#allowdeny


## gateway:tcp_listeners
Settings for the TCP listeners.


**Config paths**

 - <code>gateway.stomp.listeners</code>


**Env overrides**

 - <code>EMQX_GATEWAY__STOMP__LISTENERS</code>



**Fields**

- tcp: <code>{$name -> [gateway:tcp_listener](#gateway-tcp_listener)}</code>

   

- ssl: <code>{$name -> [gateway:ssl_listener](#gateway-ssl_listener)}</code>

   


## gateway:tcp_udp_listeners
Settings for the listeners.


**Config paths**

 - <code>gateway.exproto.listeners</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS</code>



**Fields**

- tcp: <code>{$name -> [gateway:tcp_listener](#gateway-tcp_listener)}</code>

   

- ssl: <code>{$name -> [gateway:ssl_listener](#gateway-ssl_listener)}</code>

   

- udp: <code>{$name -> [gateway:udp_listener](#gateway-udp_listener)}</code>

   

- dtls: <code>{$name -> [gateway:dtls_listener](#gateway-dtls_listener)}</code>

   


## gateway:translator
MQTT topic that corresponds to a particular type of event.


**Config paths**

 - <code>gateway.lwm2m.translators.command</code>
 - <code>gateway.lwm2m.translators.notify</code>
 - <code>gateway.lwm2m.translators.register</code>
 - <code>gateway.lwm2m.translators.response</code>
 - <code>gateway.lwm2m.translators.update</code>


**Env overrides**

 - <code>EMQX_GATEWAY__LWM2M__TRANSLATORS__COMMAND</code>
 - <code>EMQX_GATEWAY__LWM2M__TRANSLATORS__NOTIFY</code>
 - <code>EMQX_GATEWAY__LWM2M__TRANSLATORS__REGISTER</code>
 - <code>EMQX_GATEWAY__LWM2M__TRANSLATORS__RESPONSE</code>
 - <code>EMQX_GATEWAY__LWM2M__TRANSLATORS__UPDATE</code>



**Fields**

- topic: <code>binary()</code>

  主题名称

- qos: <code>qos()</code>
  * default: 
  `0`

  QoS 等级


## gateway:udp_listener
Settings for the UDP listener.


**Config paths**

 - <code>gateway.coap.listeners.udp.$name</code>
 - <code>gateway.exproto.listeners.udp.$name</code>
 - <code>gateway.lwm2m.listeners.udp.$name</code>
 - <code>gateway.mqttsn.listeners.udp.$name</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME</code>



**Fields**

- udp_options: <code>[gateway:udp_opts](#gateway-udp_opts)</code>



- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该监听器。

- bind: <code>emqx_gateway_schema:ip_port() | integer()</code>

  监听器绑定的 IP 地址或端口。

- max_connections: <code>integer()</code>
  * default: 
  `1024`

  监听器支持的最大连接数。

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  监听器支持的最大连接速率。

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。

- mountpoint: <code>binary()</code>

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，则客户端实际上订阅了 `some_tenant/t` 主题。类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有主题名称 `t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：
     - <code>${clientid}</code>：clientid
     - <code>${username}</code>：用户名


- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  配置监听器的访问控制规则。
  见：https://github.com/emqtt/esockd#allowdeny


## gateway:udp_listeners
Settings for the UDP listeners.


**Config paths**

 - <code>gateway.coap.listeners</code>
 - <code>gateway.lwm2m.listeners</code>
 - <code>gateway.mqttsn.listeners</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__LISTENERS</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS</code>



**Fields**

- udp: <code>{$name -> [gateway:udp_listener](#gateway-udp_listener)}</code>

   

- dtls: <code>{$name -> [gateway:dtls_listener](#gateway-dtls_listener)}</code>

   


## gateway:udp_opts
Settings for the UDP sockets.


**Config paths**

 - <code>gateway.coap.listeners.dtls.$name.udp_options</code>
 - <code>gateway.coap.listeners.udp.$name.udp_options</code>
 - <code>gateway.exproto.listeners.dtls.$name.udp_options</code>
 - <code>gateway.exproto.listeners.udp.$name.udp_options</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.udp_options</code>
 - <code>gateway.lwm2m.listeners.udp.$name.udp_options</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.udp_options</code>
 - <code>gateway.mqttsn.listeners.udp.$name.udp_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__UDP_OPTIONS</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__UDP_OPTIONS</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__UDP_OPTIONS</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__UDP_OPTIONS</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__UDP_OPTIONS</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__UDP_OPTIONS</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__UDP_OPTIONS</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__UDP_OPTIONS</code>



**Fields**

- active_n: <code>integer()</code>
  * default: 
  `100`

  为 Socket 指定 {active, N} 选项。
  参见：https://erlang.org/doc/man/inet.html#setopts-2

- recbuf: <code>emqx_gateway_schema:bytesize()</code>

  Socket 在内核空间接收缓冲区的大小。

- sndbuf: <code>emqx_gateway_schema:bytesize()</code>

  Socket 在内核空间发送缓冲区的大小。

- buffer: <code>emqx_gateway_schema:bytesize()</code>

  Socket 在用户空间的缓冲区大小。

- reuseaddr: <code>boolean()</code>
  * default: 
  `true`

  允许重用本地处于 TIME_WAIT 的端口号


## limiter:bucket_opts
Settings for the bucket.


**Config paths**

 - <code>limiter.batch.bucket.$bucket_name</code>
 - <code>limiter.bytes_in.bucket.$bucket_name</code>
 - <code>limiter.connection.bucket.$bucket_name</code>
 - <code>limiter.message_in.bucket.$bucket_name</code>
 - <code>limiter.message_routing.bucket.$bucket_name</code>


**Env overrides**

 - <code>EMQX_LIMITER__BATCH__BUCKET__$BUCKET_NAME</code>
 - <code>EMQX_LIMITER__BYTES_IN__BUCKET__$BUCKET_NAME</code>
 - <code>EMQX_LIMITER__CONNECTION__BUCKET__$BUCKET_NAME</code>
 - <code>EMQX_LIMITER__MESSAGE_IN__BUCKET__$BUCKET_NAME</code>
 - <code>EMQX_LIMITER__MESSAGE_ROUTING__BUCKET__$BUCKET_NAME</code>



**Fields**

- rate: <code>emqx_limiter_schema:rate()</code>
  * default: 
  `"infinity"`

  桶的令牌生成速率

- capacity: <code>emqx_limiter_schema:capacity()</code>
  * default: 
  `"infinity"`

  该令牌桶的容量

- initial: <code>emqx_limiter_schema:initial()</code>
  * default: 
  `"0"`

  桶中的初始令牌数

- per_client: <code>[limiter:client_bucket](#limiter-client_bucket)</code>
  * default: 
  `{}`

  对桶的每个使用者的速率控制设置，这个不是必须的


## limiter:client_bucket
Settings for the client bucket.


**Config paths**

 - <code>limiter.batch.bucket.$bucket_name.per_client</code>
 - <code>limiter.bytes_in.bucket.$bucket_name.per_client</code>
 - <code>limiter.connection.bucket.$bucket_name.per_client</code>
 - <code>limiter.message_in.bucket.$bucket_name.per_client</code>
 - <code>limiter.message_routing.bucket.$bucket_name.per_client</code>


**Env overrides**

 - <code>EMQX_LIMITER__BATCH__BUCKET__$BUCKET_NAME__PER_CLIENT</code>
 - <code>EMQX_LIMITER__BYTES_IN__BUCKET__$BUCKET_NAME__PER_CLIENT</code>
 - <code>EMQX_LIMITER__CONNECTION__BUCKET__$BUCKET_NAME__PER_CLIENT</code>
 - <code>EMQX_LIMITER__MESSAGE_IN__BUCKET__$BUCKET_NAME__PER_CLIENT</code>
 - <code>EMQX_LIMITER__MESSAGE_ROUTING__BUCKET__$BUCKET_NAME__PER_CLIENT</code>



**Fields**

- rate: <code>emqx_limiter_schema:rate()</code>
  * default: 
  `"infinity"`

  桶的令牌生成速率

- initial: <code>emqx_limiter_schema:initial()</code>
  * default: 
  `"0"`

  桶中的初始令牌数

- low_watermark: <code>emqx_limiter_schema:initial()</code>
  * default: 
  `"0"`

  当桶中剩余的令牌数低于这个值，即使令牌申请成功了，也会被强制暂停一会儿

- capacity: <code>emqx_limiter_schema:capacity()</code>
  * default: 
  `"infinity"`

  每个使用者的令牌容量上限

- divisible: <code>boolean()</code>
  * default: 
  `false`

  申请的令牌数是否可以被分割

- max_retry_time: <code>emqx_schema:duration()</code>
  * default: 
  `"10s"`

  申请失败后，尝试重新申请的时长最大值

- failure_strategy: <code>emqx_limiter_schema:failure_strategy()</code>
  * default: 
  `force`

  当所有的重试都失败后的处理策略


## limiter
Settings for the rate limiter.


**Config paths**

 - <code>limiter</code>


**Env overrides**

 - <code>EMQX_LIMITER</code>



**Fields**

- bytes_in: <code>[limiter:limiter_opts](#limiter-limiter_opts)</code>
  * default: 
  `{}`

  流入字节率控制器.
  这个是用来控制当前节点上的数据流入的字节率，每条消息将会消耗和其二进制大小等量的令牌，当达到最大速率后，会话将会被限速甚至被强制挂起一小段时间

- message_in: <code>[limiter:limiter_opts](#limiter-limiter_opts)</code>
  * default: 
  `{}`

  流入速率控制器。
  这个用来控制当前节点上的消息流入速率，当达到最大速率后，会话将会被限速甚至被强制挂起一小段时间

- connection: <code>[limiter:limiter_opts](#limiter-limiter_opts)</code>
  * default: 

  ```
  {
    bucket {
      default {capacity = 1000, rate = "1000/s"}
    }
    rate = "1000/s"
  }
  ```

  连接速率控制器。
  这个用来控制当前节点上的连接速率，当达到最大速率后，新的连接将会被拒绝

- message_routing: <code>[limiter:limiter_opts](#limiter-limiter_opts)</code>
  * default: 
  `{}`

  消息派发速率控制器。
  这个用来控制当前节点内的消息派发速率，当达到最大速率后，新的推送将会被拒绝

- batch: <code>[limiter:limiter_opts](#limiter-limiter_opts)</code>
  * default: 
  `{}`

  批量操作速率控制器。
  这是给 EMQX 内部的批量操作使用的，比如用来控制保留消息的派发速率


## limiter:limiter_opts
Settings for the limiter.


**Config paths**

 - <code>limiter.batch</code>
 - <code>limiter.bytes_in</code>
 - <code>limiter.connection</code>
 - <code>limiter.message_in</code>
 - <code>limiter.message_routing</code>


**Env overrides**

 - <code>EMQX_LIMITER__BATCH</code>
 - <code>EMQX_LIMITER__BYTES_IN</code>
 - <code>EMQX_LIMITER__CONNECTION</code>
 - <code>EMQX_LIMITER__MESSAGE_IN</code>
 - <code>EMQX_LIMITER__MESSAGE_ROUTING</code>



**Fields**

- rate: <code>emqx_limiter_schema:rate()</code>
  * default: 
  `"infinity"`

  桶的令牌生成速率

- burst: <code>emqx_limiter_schema:burst_rate()</code>
  * default: 
  `0`

  突发速率。
  突发速率允许短时间内速率超过设置的速率值，突发速率 + 速率 = 当前桶能达到的最大速率值

- bucket: <code>{$bucket_name -> [limiter:bucket_opts](#limiter-bucket_opts)}</code>
  * default: 

  ```
  {
    default {}
  }
  ```

  桶的配置


## modules:delayed
Settings for the delayed module.


**Config paths**

 - <code>delayed</code>


**Env overrides**

 - <code>EMQX_DELAYED</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否开启该功能

- max_delayed_messages: <code>integer()</code>
  * default: 
  `0`

  延迟消息的数量上限(0 代表无限)


## modules:rewrite
EMQX 的主题重写功能支持根据用户配置的规则在客户端订阅主题、发布消息、取消订阅的时候将 A 主题重写为 B 主题。
重写规则分为 Pub 规则和 Sub 规则，Pub 规则匹配 PUSHLISH 报文携带的主题，Sub 规则匹配 SUBSCRIBE、UNSUBSCRIBE 报文携带的主题。
每条重写规则都由主题过滤器、正则表达式、目标表达式三部分组成。
在主题重写功能开启的前提下，EMQX 在收到诸如 PUBLISH 报文等带有主题的 MQTT 报文时，将使用报文中的主题去依次匹配配置文件中规则的主题过滤器部分，一旦成功匹配，则使用正则表达式提取主题中的信息，然后替换至目标表达式以构成新的主题。
目标表达式中可以使用 `$N` 这种格式的变量匹配正则表达中提取出来的元素，`$N` 的值为正则表达式中提取出来的第 N 个元素，比如 `$1` 即为正则表达式提取的第一个元素。
需要注意的是，EMQX 使用倒序读取配置文件中的重写规则，当一条主题可以同时匹配多条主题重写规则的主题过滤器时，EMQX 仅会使用它匹配到的第一条规则进行重写，如果该条规则中的正则表达式与 MQTT 报文主题不匹配，则重写失败，不会再尝试使用其他的规则进行重写。
因此用户在使用时需要谨慎的设计 MQTT 报文主题以及主题重写规则。


**Config paths**

 - <code>rewrite.$INDEX</code>


**Env overrides**

 - <code>EMQX_REWRITE__$INDEX</code>



**Fields**

- action: <code>subscribe | publish | all</code>

  主题重写在哪种操作上生效：
    - `subscribe`：订阅时重写主题；
    - `publish`：发布时重写主题；
    -`all`：全部重写主题

- source_topic: <code>binary()</code>

  源主题，客户端业务指定的主题

- dest_topic: <code>binary()</code>

  目标主题。

- re: <code>binary()</code>

  正则表达式


## modules:telemetry
Settings for the telemetry module.


**Config paths**

 - <code>telemetry</code>


**Env overrides**

 - <code>EMQX_TELEMETRY</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable telemetry.


## modules:topic_metrics



**Config paths**

 - <code>topic_metrics.$INDEX</code>


**Env overrides**

 - <code>EMQX_TOPIC_METRICS__$INDEX</code>



**Fields**

- topic: <code>binary()</code>

  Collect metrics for the topic.


## rule_engine:builtin_action_console
配置打印到控制台


**Config paths**

 - <code>rule_engine.rules.$id.actions.$INDEX</code>


**Env overrides**

 - <code>EMQX_RULE_ENGINE__RULES__$ID__ACTIONS__$INDEX</code>



**Fields**

- function: <code>console</code>

  将输出打印到控制台


## rule_engine:builtin_action_republish
配置重新发布。


**Config paths**

 - <code>rule_engine.rules.$id.actions.$INDEX</code>


**Env overrides**

 - <code>EMQX_RULE_ENGINE__RULES__$ID__ACTIONS__$INDEX</code>



**Fields**

- function: <code>republish</code>

  将消息重新发布为新的 MQTT 消息

- args: <code>[rule_engine:republish_args](#rule_engine-republish_args)</code>
  * default: 
  `{}`




## rule_engine:republish_args

内置 'republish' 动作的参数。
可以在参数中使用变量。
变量是规则中选择的字段。 例如规则 SQL 定义如下：
<code>
    SELECT clientid, qos, payload FROM "t/1"
</code>
然后有 3 个变量可用：<code>clientid</code>、<code>qos</code> 和 <code>payload</code>。 如果我们将参数设置为：
<code>
    {
        topic = "t/${clientid}"
        qos = "${qos}"
        payload = "msg: ${payload}"
    }
</code>
当收到一条消息 payload = `hello`, qos = 1, clientid = `Steve` 时，将重新发布一条新的 MQTT 消息到主题 `t/Steve`
消息内容为 payload = `msg: hello`, and `qos = 1


**Config paths**

 - <code>rule_engine.rules.$id.actions.$INDEX.args</code>


**Env overrides**

 - <code>EMQX_RULE_ENGINE__RULES__$ID__ACTIONS__$INDEX__ARGS</code>



**Fields**

- topic: <code>binary()</code>


  重新发布消息的目标主题。
  允许使用带有变量的模板，请参阅“republish_args”的描述。


- qos: <code>qos() | binary()</code>
  * default: 
  `"${qos}"`


  要重新发布的消息的 qos。允许使用带有变量的模板，请参阅“republish_args”的描述。
  默认为 ${qos}。 如果从规则的选择结果中没有找到变量 ${qos}，则使用 0。


- retain: <code>boolean() | binary()</code>
  * default: 
  `"${retain}"`


  要重新发布的消息的“保留”标志。允许使用带有变量的模板，请参阅“republish_args”的描述。
  默认为 ${retain}。 如果从所选结果中未找到变量 ${retain}，则使用 false。


- payload: <code>binary()</code>
  * default: 
  `"${payload}"`


  要重新发布的消息的有效负载。允许使用带有变量的模板，请参阅“republish_args”的描述。。
  默认为 ${payload}。 如果从所选结果中未找到变量 ${payload}，则使用字符串 "undefined"。



## rule_engine
配置 EMQX 规则引擎。


**Config paths**

 - <code>rule_engine</code>


**Env overrides**

 - <code>EMQX_RULE_ENGINE</code>



**Fields**

- ignore_sys_message: <code>boolean()</code>
  * default: 
  `true`

  当设置为“true”（默认）时，规则引擎将忽略发布到 $SYS 主题的消息。

- rules: <code>{$id -> [rule_engine:rules](#rule_engine-rules)}</code>
  * default: 
  `{}`

  规则

- jq_function_default_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"10s"`

  规则引擎内建函数 `jq` 默认时间限制


## rule_engine:rules
配置规则


**Config paths**

 - <code>rule_engine.rules.$id</code>


**Env overrides**

 - <code>EMQX_RULE_ENGINE__RULES__$ID</code>



**Fields**

- name: <code>binary()</code>
  * default: 
  `""`

  规则名字

- sql: <code>binary()</code>


  用于处理消息的 SQL 。
  示例：<code>SELECT * FROM "test/topic" WHERE payload.x = 1</code>


- actions: <code>[binary() | [rule_engine:builtin_action_republish](#rule_engine-builtin_action_republish) | [rule_engine:builtin_action_console](#rule_engine-builtin_action_console) | [rule_engine:user_provided_function](#rule_engine-user_provided_function)]</code>
  * default: 
  `[]`


  规则的动作列表。
  动作可以是指向 EMQX bridge 的引用，也可以是一个指向函数的对象。
  我们支持一些内置函数，如“republish”和“console”，我们还支持用户提供的函数，它的格式为：“{module}:{function}”。
  列表中的动作按顺序执行。这意味着如果其中一个动作执行缓慢，则以下所有动作都不会被执行直到它返回。
  如果其中一个动作崩溃，在它之后的所有动作仍然会被按照原始顺序执行。
  如果运行动作时出现任何错误，则会出现错误消息，并且相应的计数器会增加。


- enable: <code>boolean()</code>
  * default: 
  `true`

  启用或禁用规则引擎

- description: <code>binary()</code>
  * default: 
  `""`

  规则的描述


## rule_engine:user_provided_function
配置用户函数


**Config paths**

 - <code>rule_engine.rules.$id.actions.$INDEX</code>


**Env overrides**

 - <code>EMQX_RULE_ENGINE__RULES__$ID__ACTIONS__$INDEX</code>



**Fields**

- function: <code>binary()</code>


  用户提供的函数。 格式应为：'{module}:{function}'。
  其中 {module} 是 Erlang 回调模块， {function} 是 Erlang 函数。
  要编写自己的函数，请检查源文件：<code>apps/emqx_rule_engine/src/emqx_rule_actions.erl</code> 中的示例函数 <code>console</code> 和 <code>republish</code>。


- args: <code>map()</code>
  * default: 
  `{}`


  用户提供的参数将作为函数 module:function/3 的第三个参数，
  请检查源文件：<code>apps/emqx_rule_engine/src/emqx_rule_actions.erl</code> 中的示例函数 <code>console</code> 和 <code>republish</code>。



## egress
MQTT Bridge 配置


**Config paths**

 - <code>bridges.mqtt.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME</code>



**Fields**

- direction: <code>egress</code>
  * default: 
  `egress`

  The direction of the bridge. Can be one of 'ingress' or 'egress'.</br>
  The egress config defines how this bridge forwards messages from the local broker to the remote
  broker.</br>
  Template with variables is allowed in 'remote_topic', 'qos', 'retain', 'payload'.</br>
  NOTE: if this bridge is used as the action of a rule (emqx rule engine), and also local_topic
  is configured, then both the data got from the rule and the MQTT messages that matches
  local_topic will be forwarded.


- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用 Bridge

- connector: <code>binary() | [connector-mqtt:connector](#connector-mqtt-connector)</code>


  Bridge 使用的 Connector 的 ID 或者配置。Connector ID 的格式必须为：<code>{type}:{name}</code>。</br>
  在配置文件中，您可以通过以下路径找到 Connector 的相应配置条目：'connector.{type}.{name}'。</br>

- local_topic: <code>binary()</code>

  要转发到远程broker的本地主题

- remote_topic: <code>binary()</code>


  转发到远程broker的哪个topic。</br>
  允许使用带有变量的模板。


- remote_qos: <code>qos() | binary()</code>


  待发送 MQTT 消息的 QoS。</br>
  允许使用带有变量的模板。


- retain: <code>boolean() | binary()</code>


  要发送的 MQTT 消息的“保留”标志。</br>
  允许使用带有变量的模板。


- payload: <code>binary()</code>


  要发送的 MQTT 消息的负载。</br>
  允许使用带有变量的模板。



## ingress
MQTT Bridge 配置


**Config paths**

 - <code>bridges.mqtt.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME</code>



**Fields**

- direction: <code>ingress</code>
  * default: 
  `egress`

  The direction of the bridge. Can be one of 'ingress' or 'egress'.</br>
  The ingress config defines how this bridge receive messages from the remote MQTT broker, and then
  send them to the local broker.</br>
  Template with variables is allowed in 'local_topic', 'remote_qos', 'qos', 'retain',
  'payload'.</br>
  NOTE: if this bridge is used as the input of a rule (emqx rule engine), and also local_topic is
  configured, then messages got from the remote broker will be sent to both the 'local_topic' and
  the rule.


- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用 Bridge

- connector: <code>binary() | [connector-mqtt:connector](#connector-mqtt-connector)</code>


  Bridge 使用的 Connector 的 ID 或者配置。Connector ID 的格式必须为：<code>{type}:{name}</code>。</br>
  在配置文件中，您可以通过以下路径找到 Connector 的相应配置条目：'connector.{type}.{name}'。</br>

- remote_topic: <code>binary()</code>

  从远程 broker 的哪个 topic 接收消息

- remote_qos: <code>qos() | binary()</code>
  * default: 
  `1`

  订阅远程borker时要使用的 QoS 级别

- local_topic: <code>binary()</code>


  向本地 broker 的哪个 topic 发送消息。</br>
  允许使用带有变量的模板。


- local_qos: <code>qos() | binary()</code>
  * default: 
  `"${qos}"`


  待发送 MQTT 消息的 QoS。</br>
  允许使用带有变量的模板。


- retain: <code>boolean() | binary()</code>
  * default: 
  `"${retain}"`


  要发送的 MQTT 消息的“保留”标志。</br>
  允许使用带有变量的模板。


- payload: <code>binary()</code>
  * default: 
  `"${payload}"`


  要发送的 MQTT 消息的负载。</br>
  允许使用带有变量的模板。



## cluster_dns
DNS SRV 记录服务发现。


**Config paths**

 - <code>cluster.dns</code>


**Env overrides**

 - <code>EMQX_CLUSTER__DNS</code>



**Fields**

- name: <code>string()</code>
  * default: 
  `"localhost"`

  指定 DNS A 记录的名字。emqx 会通过访问这个 DNS A 记录来获取 IP 地址列表。
  当 <code>cluster.discovery_strategy</code> 为 <code>dns</code> 时有效。


- record_type: <code>a | srv</code>
  * default: 
  `a`

  DNS 记录类型。


## cluster_etcd
使用 'etcd' 服务的服务发现。


**Config paths**

 - <code>cluster.etcd</code>


**Env overrides**

 - <code>EMQX_CLUSTER__ETCD</code>



**Fields**

- server: <code>emqx_schema:comma_separated_list()</code>

  指定 etcd 服务的地址。如有多个服务使用逗号 , 分隔。
  当 cluster.discovery_strategy 为 etcd 时，此配置项才有效。
            

- prefix: <code>string()</code>
  * default: 
  `"emqxcl"`

  指定 etcd 路径的前缀。每个节点在 etcd 中都会创建一个路径:
  v2/keys/<prefix>/<cluster.name>/<node.name> </br>
  当 cluster.discovery_strategy 为 etcd 时，此配置项才有效。
        

- node_ttl: <code>emqx_schema:duration()</code>
  * default: 
  `"1m"`

  指定 etcd 中节点信息的过期时间。
  当 cluster.discovery_strategy 为 etcd 时，此配置项才有效。
            

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>

  当使用 TLS 连接 etcd 时的配置选项。
  当 cluster.discovery_strategy 为 etcd 时，此配置项才有效。
            


## cluster_k8s
Kubernetes 服务发现。


**Config paths**

 - <code>cluster.k8s</code>


**Env overrides**

 - <code>EMQX_CLUSTER__K8S</code>



**Fields**

- apiserver: <code>string()</code>
  * default: 
  `"http://10.110.111.204:8080"`

  指定 Kubernetes API Server。如有多个 Server 使用逗号 , 分隔。
  当 cluster.discovery_strategy 为 k8s 时，此配置项才有效。
            

- service_name: <code>string()</code>
  * default: 
  `"emqx"`

  指定 Kubernetes 中 EMQX 的服务名。
  当 cluster.discovery_strategy 为 k8s 时，此配置项才有效。
            

- address_type: <code>ip | dns | hostname</code>
  * default: 
  `ip`

  当使用 k8s 方式集群时，address_type 用来从 Kubernetes 接口的应答里获取什么形式的 Host 列表。
  指定 <code>cluster.k8s.address_type</code> 为 <code>ip</code>，则将从 Kubernetes 接口中获取集群中其他节点的 IP 地址。


- namespace: <code>string()</code>
  * default: 
  `"default"`

  当使用 k8s 方式并且 cluster.k8s.address_type 指定为 dns 类型时，
  可设置 emqx 节点名的命名空间。与 cluster.k8s.suffix 一起使用用以拼接得到节点名列表。
        

- suffix: <code>string()</code>
  * default: 
  `"pod.local"`

  当使用 k8s 方式并且 cluster.k8s.address_type 指定为 dns 类型时，可设置 emqx 节点名的后缀。
  与 cluster.k8s.namespace 一起使用用以拼接得到节点名列表。
            


## cluster_mcast
UDP 组播服务发现。


**Config paths**

 - <code>cluster.mcast</code>


**Env overrides**

 - <code>EMQX_CLUSTER__MCAST</code>



**Fields**

- addr: <code>string()</code>
  * default: 
  `"239.192.0.1"`

  指定多播 IPv4 地址。
  当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。
            

- ports: <code>[integer()]</code>
  * default: 
  `[4369,4370]`

  指定多播端口。如有多个端口使用逗号 , 分隔。
  当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。
            

- iface: <code>string()</code>
  * default: 
  `"0.0.0.0"`

  指定节点发现服务需要绑定到本地 IP 地址。
  当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。
            

- ttl: <code>0..255</code>
  * default: 
  `255`

  指定多播的 Time-To-Live 值。
  当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。
            

- loop: <code>boolean()</code>
  * default: 
  `true`

  设置多播的报文是否投递到本地回环地址。
  当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。
            

- sndbuf: <code>emqx_schema:bytesize()</code>
  * default: 
  `"16KB"`

  外发数据报的内核级缓冲区的大小。
  当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。
            

- recbuf: <code>emqx_schema:bytesize()</code>
  * default: 
  `"16KB"`

  接收数据报的内核级缓冲区的大小。
  当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。
            

- buffer: <code>emqx_schema:bytesize()</code>
  * default: 
  `"32KB"`

  用户级缓冲区的大小。
  当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。
            


## cluster_static
静态节点服务发现。新节点通过连接一个节点来加入集群。


**Config paths**

 - <code>cluster.static</code>


**Env overrides**

 - <code>EMQX_CLUSTER__STATIC</code>



**Fields**

- seeds: <code>[atom()]</code>
  * default: 
  `[]`

  集群中的 EMQX 节点名称列表，
  指定固定的节点列表，多个节点间使用逗号 , 分隔。
  当 cluster.discovery_strategy 为 static 时，此配置项才有效。
  适合于节点数量较少且固定的集群。
            


## authorization
授权相关


**Config paths**

 - <code>authorization</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION</code>



**Fields**

- no_match: <code>allow | deny</code>
  * default: 
  `allow`


  如果用户或客户端不匹配ACL规则，或者从可配置授权源（比如内置数据库、HTTP API 或 PostgreSQL 等。）内未找
  到此类用户或客户端时，模式的认访问控制操作。
  在“授权”中查找更多详细信息。


- deny_action: <code>ignore | disconnect</code>
  * default: 
  `ignore`


  授权检查拒绝操作时的操作。


- cache: <code>[broker:cache](#broker-cache)</code>



- sources: <code>[[authz:file](#authz-file) | [authz:http_get](#authz-http_get) | [authz:http_post](#authz-http_post) | [authz:mnesia](#authz-mnesia) | [authz:mongo_single](#authz-mongo_single) | [authz:mongo_rs](#authz-mongo_rs) | [authz:mongo_sharded](#authz-mongo_sharded) | [authz:mysql](#authz-mysql) | [authz:postgresql](#authz-postgresql) | [authz:redis_single](#authz-redis_single) | [authz:redis_sentinel](#authz-redis_sentinel) | [authz:redis_cluster](#authz-redis_cluster)]</code>
  * default: 
  `[]`


  鉴权数据源。</br>
  鉴权（ACL）数据源的列表。
  它被设计为一个数组，而不是一个散列映射，
  所以可以作为链式访问控制。</br>

  当授权一个 'publish' 或 'subscribe' 行为时，
  该配置列表中的所有数据源将按顺序进行检查。
  如果在某个客户端未找到时（使用 ClientID 或 Username），
  将会移动到下一个数据源。直至得到 'allow' 或 'deny' 的结果。</br>

  如果在任何数据源中都未找到对应的客户端信息,
  配置的默认行为 ('authorization.no_match') 将生效.</br>

  注意:
  数据源使用 'type' 进行标识.
  使用同一类型的数据源多于一次不被允许.



## cluster
EMQX 节点可以组成一个集群，以提高总容量。</br> 这里指定了节点之间如何连接。


**Config paths**

 - <code>cluster</code>


**Env overrides**

 - <code>EMQX_CLUSTER</code>



**Fields**

- name: <code>atom()</code>
  * default: 
  `emqxcl`
  * mapping: 
  `ekka.cluster_name`

  EMQX集群名称。每个集群都有一个唯一的名称。服务发现时会用于做路径的一部分。

- discovery_strategy: <code>manual | static | mcast | dns | etcd | k8s</code>
  * default: 
  `manual`

  集群节点发现方式。可选值为:
  - manual: 手动加入集群</br>
  - static: 配置静态节点。配置几个固定的节点，新节点通过连接固定节点中的某一个来加入集群。</br>
  - mcast: 使用 UDP 多播的方式发现节点。</br>
  - dns: 使用 DNS A 记录的方式发现节点。</br>
  - etcd: 使用 etcd 发现节点。</br>
  - k8s: 使用 Kubernetes 发现节点。</br>
             

- core_nodes: <code>emqx_schema:comma_separated_atoms()</code>
  * default: 
  `[]`
  * mapping: 
  `mria.core_nodes`

  当前节点连接的核心节点列表。</br>
  注意：该参数仅在设置 <code>backend</code> 时生效到 <code>rlog</code>
  并且设置 <code>role</code> 为 <code>replicant</code> 时生效。</br>
  该值需要在手动或静态集群发现机制下设置。</br>
  如果使用了自动集群发现机制（如 <code>etcd</code>），则不需要设置该值。
            

- autoclean: <code>emqx_schema:duration()</code>
  * default: 
  `"5m"`
  * mapping: 
  `ekka.cluster_autoclean`

  指定多久之后从集群中删除离线节点。

- autoheal: <code>boolean()</code>
  * default: 
  `true`
  * mapping: 
  `ekka.cluster_autoheal`

  集群脑裂自动恢复机制开关。

- proto_dist: <code>inet_tcp | inet6_tcp | inet_tls</code>
  * default: 
  `inet_tcp`
  * mapping: 
  `ekka.proto_dist`

  分布式 Erlang 集群协议类型。可选值为:
  - inet_tcp: 使用 IPv4 </br>
  - inet6_tcp 使用 IPv6 </br>
  - inet_tls: 使用 TLS，需要与 node.ssl_dist_optfile 配置一起使用。</br>
           

- static: <code>[cluster_static](#cluster_static)</code>



- mcast: <code>[cluster_mcast](#cluster_mcast)</code>



- dns: <code>[cluster_dns](#cluster_dns)</code>



- etcd: <code>[cluster_etcd](#cluster_etcd)</code>



- k8s: <code>[cluster_k8s](#cluster_k8s)</code>




## cluster_call
集群调用功能的选项。


**Config paths**

 - <code>node.cluster_call</code>


**Env overrides**

 - <code>EMQX_NODE__CLUSTER_CALL</code>



**Fields**

- retry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"1m"`

  当集群间调用出错时，多长时间重试一次。

- max_history: <code>1..500</code>
  * default: 
  `100`

  集群间调用最多保留的历史记录数。只用于排错时查看。

- cleanup_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"5m"`

  清理过期事务的时间间隔


## console_handler
日志处理进程将日志事件打印到 EMQX 控制台。


**Config paths**

 - <code>log.console_handler</code>


**Env overrides**

 - <code>EMQX_LOG__CONSOLE_HANDLER</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  启用此日志处理进程。

- level: <code>emqx_conf_schema:log_level()</code>
  * default: 
  `warning`


  当前日志处理进程的日志级别。
  默认为 warning 级别。


- time_offset: <code>string()</code>
  * default: 
  `"system"`


  日志中的时间戳使用的时间偏移量。
  可选值为：
    - <code>system</code>: 本地系统使用的时区偏移量
    - <code>utc</code>: 0 时区的偏移量
    - <code>+-[hh]:[mm]</code>: 自定义偏移量，比如 "-02:00" 或者 "+00:00"
  默认值为本地系统的时区偏移量：<code>system</code>。


- chars_limit: <code>unlimited | 100..inf</code>
  * default: 
  `unlimited`

  设置单个日志消息的最大长度。如果超过此长度，则日志消息将被截断。最小可设置的长度为 100。

- formatter: <code>text | json</code>
  * default: 
  `text`

  选择日志格式类型。<code>text</code> 用于纯文本，<code>json</code> 用于结构化日志记录。

- single_line: <code>boolean()</code>
  * default: 
  `true`

  如果设置为 true，则单行打印日志。 否则，日志消息可能跨越多行。

- sync_mode_qlen: <code>non_neg_integer()</code>
  * default: 
  `100`

  只要缓冲的日志事件的数量低于这个值，所有的日志事件都会被异步处理。
  这意味着，日志落地速度不会影响正常的业务进程，因为它们不需要等待日志处理进程的响应。
  如果消息队列的增长超过了这个值，处理程序开始同步处理日志事件。也就是说，发送事件的客户进程必须等待响应。
  当处理程序将消息队列减少到低于 sync_mode_qlen 阈值的水平时，异步操作就会恢复。
  默认为 100 条信息，当等待的日志事件大于 100 条时，就开始同步处理日志。

- drop_mode_qlen: <code>pos_integer()</code>
  * default: 
  `3000`

  当缓冲的日志事件数大于此值时，新的日志事件将被丢弃。起到过载保护的功能。
  为了使过载保护算法正常工作必须要：<code>sync_mode_qlen =< drop_mode_qlen =< flush_qlen</code> 且 drop_mode_qlen > 1
  要禁用某些模式，请执行以下操作。
  - 如果 sync_mode_qlen 被设置为 0，所有的日志事件都被同步处理。也就是说，异步日志被禁用。
  - 如果 sync_mode_qlen 被设置为与 drop_mode_qlen 相同的值，同步模式被禁用。也就是说，处理程序总是以异步模式运行，除非调用 drop 或 flushing。
  - 如果 drop_mode_qlen 被设置为与 flush_qlen 相同的值，则 drop 模式被禁用，永远不会发生。


- flush_qlen: <code>pos_integer()</code>
  * default: 
  `8000`

  如果缓冲日志事件的数量增长大于此阈值，则会发生冲刷（删除）操作。日志处理进程会丢弃缓冲的日志消息。
  来缓解自身不会由于内存瀑涨而影响其它业务进程。日志内容会提醒有多少事件被删除。

- overload_kill: <code>[log_overload_kill](#log_overload_kill)</code>



- burst_limit: <code>[log_burst_limit](#log_burst_limit)</code>



- supervisor_reports: <code>error | progress</code>
  * default: 
  `error`


  Supervisor 报告的类型。默认为 error 类型。
    - <code>error</code>：仅记录 Erlang 进程中的错误。
    - <code>progress</code>：除了 error 信息外，还需要记录进程启动的详细信息。


- max_depth: <code>unlimited | non_neg_integer()</code>
  * default: 
  `100`

  Erlang 内部格式日志格式化和 Erlang 进程消息队列检查的最大深度。


## log
EMQX 日志记录支持日志事件的多个接收器。 每个接收器由一个_log handler_表示，可以独立配置。


**Config paths**

 - <code>log</code>


**Env overrides**

 - <code>EMQX_LOG</code>



**Fields**

- console_handler: <code>[console_handler](#console_handler)</code>



- file_handlers: <code>{$name -> [log_file_handler](#log_file_handler)}</code>

  输出到文件的日志处理进程列表


## log_burst_limit
短时间内产生的大量日志事件可能会导致问题，例如：
  - 日志文件变得非常大
  - 日志文件轮换过快，有用信息被覆盖
  - 对系统的整体性能影响

日志突发限制功能可以暂时禁用日志记录以避免这些问题。


**Config paths**

 - <code>log.console_handler.burst_limit</code>
 - <code>log.file_handlers.$name.burst_limit</code>


**Env overrides**

 - <code>EMQX_LOG__CONSOLE_HANDLER__BURST_LIMIT</code>
 - <code>EMQX_LOG__FILE_HANDLERS__$NAME__BURST_LIMIT</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用日志限流保护机制。

- max_count: <code>pos_integer()</code>
  * default: 
  `10000`

  在 `window_time` 间隔内处理的最大日志事件数。达到限制后，将丢弃连续事件，直到 `window_time` 结束。

- window_time: <code>emqx_schema:duration()</code>
  * default: 
  `"1s"`

  参考 <code>max_count</code>。


## log_file_handler
日志处理进程将日志事件打印到文件。


**Config paths**

 - <code>log.file_handlers.$name</code>


**Env overrides**

 - <code>EMQX_LOG__FILE_HANDLERS__$NAME</code>



**Fields**

- file: <code>emqx_conf_schema:file()</code>

  日志文件路径及名字。

- rotation: <code>[log_rotation](#log_rotation)</code>



- max_size: <code>infinity | emqx_schema:bytesize()</code>
  * default: 
  `"50MB"`

  此参数控制日志文件轮换。`infinity` 意味着日志文件将无限增长，否则日志文件将在达到 `max_size`（以字节为单位）时进行轮换。
  与 rotation count 配合使用。如果 counter 为 10，则是 10 个文件轮换。


- enable: <code>boolean()</code>
  * default: 
  `true`

  启用此日志处理进程。

- level: <code>emqx_conf_schema:log_level()</code>
  * default: 
  `warning`


  当前日志处理进程的日志级别。
  默认为 warning 级别。


- time_offset: <code>string()</code>
  * default: 
  `"system"`


  日志中的时间戳使用的时间偏移量。
  可选值为：
    - <code>system</code>: 本地系统使用的时区偏移量
    - <code>utc</code>: 0 时区的偏移量
    - <code>+-[hh]:[mm]</code>: 自定义偏移量，比如 "-02:00" 或者 "+00:00"
  默认值为本地系统的时区偏移量：<code>system</code>。


- chars_limit: <code>unlimited | 100..inf</code>
  * default: 
  `unlimited`

  设置单个日志消息的最大长度。 如果超过此长度，则日志消息将被截断。最小可设置的长度为 100。

- formatter: <code>text | json</code>
  * default: 
  `text`

  选择日志格式类型。<code>text</code> 用于纯文本，<code>json</code> 用于结构化日志记录。

- single_line: <code>boolean()</code>
  * default: 
  `true`

  如果设置为 true，则单行打印日志。否则，日志消息可能跨越多行。

- sync_mode_qlen: <code>non_neg_integer()</code>
  * default: 
  `100`

  只要缓冲的日志事件的数量低于这个值，所有的日志事件都会被异步处理。
  这意味着，日志落地速度不会影响正常的业务进程，因为它们不需要等待日志处理进程的响应。
  如果消息队列的增长超过了这个值，处理程序开始同步处理日志事件。也就是说，发送事件的客户进程必须等待响应。
  当处理程序将消息队列减少到低于 sync_mode_qlen 阈值的水平时，异步操作就会恢复。
  默认为 100 条信息，当等待的日志事件大于 100 条时，就开始同步处理日志。

- drop_mode_qlen: <code>pos_integer()</code>
  * default: 
  `3000`

  当缓冲的日志事件数大于此值时，新的日志事件将被丢弃。起到过载保护的功能。
  为了使过载保护算法正常工作必须要：<code>sync_mode_qlen =< drop_mode_qlen =< flush_qlen</code> 且 drop_mode_qlen > 1
  要禁用某些模式，请执行以下操作。
  - 如果 sync_mode_qlen 被设置为 0，所有的日志事件都被同步处理。也就是说，异步日志被禁用。
  - 如果 sync_mode_qlen 被设置为与 drop_mode_qlen 相同的值，同步模式被禁用。也就是说，处理程序总是以异步模式运行，除非调用 drop 或 flushing。
  - 如果 drop_mode_qlen 被设置为与 flush_qlen 相同的值，则 drop 模式被禁用，永远不会发生。


- flush_qlen: <code>pos_integer()</code>
  * default: 
  `8000`

  如果缓冲日志事件的数量增长大于此阈值，则会发生冲刷（删除）操作。日志处理进程会丢弃缓冲的日志消息。
  来缓解自身不会由于内存瀑涨而影响其它业务进程。日志内容会提醒有多少事件被删除。

- overload_kill: <code>[log_overload_kill](#log_overload_kill)</code>



- burst_limit: <code>[log_burst_limit](#log_burst_limit)</code>



- supervisor_reports: <code>error | progress</code>
  * default: 
  `error`


  Supervisor 报告的类型。默认为 error 类型。
    - <code>error</code>：仅记录 Erlang 进程中的错误。
    - <code>progress</code>：除了 error 信息外，还需要记录进程启动的详细信息。


- max_depth: <code>unlimited | non_neg_integer()</code>
  * default: 
  `100`

  Erlang 内部格式日志格式化和 Erlang 进程消息队列检查的最大深度。


## log_overload_kill

日志过载终止，具有过载保护功能。当日志处理进程使用过多内存，或者缓存的日志消息过多时该功能被激活。</br>
检测到过载时，日志处理进程将终止，并在冷却期后重新启动。



**Config paths**

 - <code>log.console_handler.overload_kill</code>
 - <code>log.file_handlers.$name.overload_kill</code>


**Env overrides**

 - <code>EMQX_LOG__CONSOLE_HANDLER__OVERLOAD_KILL</code>
 - <code>EMQX_LOG__FILE_HANDLERS__$NAME__OVERLOAD_KILL</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  日志处理进程过载时为保护自己节点其它的业务能正常，强制杀死日志处理进程。

- mem_size: <code>emqx_schema:bytesize()</code>
  * default: 
  `"30MB"`

  日志处理进程允许使用的最大内存。

- qlen: <code>pos_integer()</code>
  * default: 
  `20000`

  允许的最大队列长度。

- restart_after: <code>emqx_schema:duration_ms() | infinity</code>
  * default: 
  `"5s"`

  如果处理进程终止，它会在以指定的时间后后自动重新启动。`infinity` 不自动重启。


## log_rotation

默认情况下，日志存储在 `./log` 目录（用于从 zip 文件安装）或 `/var/log/emqx`（用于二进制安装）。</br>
这部分配置，控制每个日志处理进程保留的文件数量。



**Config paths**

 - <code>log.file_handlers.$name.rotation</code>


**Env overrides**

 - <code>EMQX_LOG__FILE_HANDLERS__$NAME__ROTATION</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用日志轮换功能。启动后生成日志文件后缀会加上对应的索引数字，比如：log/emqx.log.1。
  系统会默认生成 <code>*.siz/*.idx<code> 用于记录日志位置，请不要手动修改这两个文件。


- count: <code>1..2048</code>
  * default: 
  `10`

  轮换的最大日志文件数。


## node
节点名称、Cookie、配置文件、数据目录和 Erlang 虚拟机（BEAM）启动参数。


**Config paths**

 - <code>node</code>


**Env overrides**

 - <code>EMQX_NODE</code>



**Fields**

- name: <code>string()</code>
  * default: 
  `"emqx@127.0.0.1"`

  节点名。格式为 \<name>@\<host>。其中 <host> 可以是 IP 地址，也可以是 FQDN。
  详见 http://erlang.org/doc/reference_manual/distributed.html。
            

- cookie: <code>string()</code>
  * default: 
  `"emqxsecretcookie"`
  * mapping: 
  `vm_args.-setcookie`

  分布式 Erlang 集群使用的 cookie 值。集群间保持一致

- process_limit: <code>1024..134217727</code>
  * default: 
  `2097152`
  * mapping: 
  `vm_args.+P`

  Erlang 系统同时存在的最大进程数。
  实际选择的最大值可能比设置的数字大得多。
  参考: https://www.erlang.org/doc/man/erl.html
            

- max_ports: <code>1024..134217727</code>
  * default: 
  `1048576`
  * mapping: 
  `vm_args.+Q`

  Erlang 系统同时存在的最大端口数。
  实际选择的最大值可能比设置的数字大得多。
  参考: https://www.erlang.org/doc/man/erl.html
            

- dist_buffer_size: <code>1..2097151</code>
  * default: 
  `8192`
  * mapping: 
  `vm_args.+zdbbl`

  Erlang 分布式缓冲区的繁忙阈值，单位是KB。

- max_ets_tables: <code>pos_integer()</code>
  * default: 
  `262144`
  * mapping: 
  `vm_args.+e`

  Erlang ETS 表的最大数量

- data_dir: <code>string()</code>
  * mapping: 
  `emqx.data_dir`


  节点数据存放目录，可能会自动创建的子目录如下：</br>
  - `mnesia/<node_name>`。EMQX 的内置数据库目录。例如，`mnesia/emqx@127.0.0.1`。</br>
  如果节点要被重新命名（例如，`emqx@10.0.1.1`）。旧目录应该首先被删除。</br>
  - `configs`。在启动时生成的配置，以及集群/本地覆盖的配置。</br>
  - `patches`: 热补丁文件将被放在这里。</br>
  - `trace`: 日志跟踪文件。</br>

  **注意**: 一个数据dir不能被两个或更多的EMQX节点同时使用。
           

- config_files: <code>[string()]</code>
  * mapping: 
  `emqx.config_files`

  启动时读取的配置文件列表。后面的配置文件项覆盖前面的文件。

- global_gc_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"15m"`
  * mapping: 
  `emqx_machine.global_gc_interval`

  系统调优参数，设置节点运行多久强制进行一次全局垃圾回收。

- crash_dump_file: <code>emqx_conf_schema:file()</code>
  * default: 
  `"log/erl_crash.dump"`
  * mapping: 
  `vm_args.-env ERL_CRASH_DUMP`

  设置 Erlang crash_dump 文件的存储路径和文件名。

- crash_dump_seconds: <code>emqx_schema:duration_s()</code>
  * default: 
  `"30s"`
  * mapping: 
  `vm_args.-env ERL_CRASH_DUMP_SECONDS`

  保存崩溃文件最大允许时间，如果文件太大，在规则时间内没有保存完成，则会直接结束。

- crash_dump_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `"100MB"`
  * mapping: 
  `vm_args.-env ERL_CRASH_DUMP_BYTES`

  限制崩溃文件的大小，当崩溃时节点内存太大，
  如果为了保存现场，需要全部存到崩溃文件中，此处限制最多能保存多大的文件。
            

- dist_net_ticktime: <code>emqx_schema:duration_s()</code>
  * default: 
  `"2m"`
  * mapping: 
  `vm_args.-kernel net_ticktime`

  系统调优参数，此配置将覆盖 vm.args 文件里的 -kernel net_ticktime 参数。当一个节点持续无响应多久之后，认为其已经宕机并断开连接。
        

- backtrace_depth: <code>integer()</code>
  * default: 
  `23`
  * mapping: 
  `emqx_machine.backtrace_depth`

  错误信息中打印的最大堆栈层数

- applications: <code>emqx_schema:comma_separated_atoms()</code>
  * default: 
  `[]`
  * mapping: 
  `emqx_machine.applications`

  当新 EMQX 加入集群时，应重启的 Erlang 应用程序的列表。

- etc_dir: <code>string()</code>

  <code>etc</code> 存放目录

- cluster_call: <code>[cluster_call](#cluster_call)</code>



- db_backend: <code>mnesia | rlog</code>
  * default: 
  `rlog`
  * mapping: 
  `mria.db_backend`

  rlog 是默认的数据库，它适用于大规模的集群。
  mnesia 是备选数据库，在小集群中提供了很好的性能。
        

- db_role: <code>core | replicant</code>
  * default: 
  `core`
  * mapping: 
  `mria.node_role`


  选择节点的角色。</br>
  <code>core</code> 节点提供数据的持久性，并负责写入。建议将核心节点放置在不同的机架或不同的可用区。</br>
  <code>repliant</code> 节点是临时工作节点。 从集群中删除它们，不影响数据库冗余</br>
  建议复制节点多于核心节点。</br>
  注意：该参数仅在设置 <code>backend</code> 时生效到 <code>rlog</code>。
            

- rpc_module: <code>gen_rpc | rpc</code>
  * default: 
  `gen_rpc`
  * mapping: 
  `mria.rlog_rpc_module`

  集群间推送事务日志到复制节点使用的协议。

- tlog_push_mode: <code>sync | async</code>
  * default: 
  `async`
  * mapping: 
  `mria.tlog_push_mode`

  同步模式下，核心节点等待复制节点的确认信息，然后再发送下一条事务日志。


## rpc
EMQX 使用 <code>gen_rpc</code> 库来实现跨节点通信。</br>
大多数情况下，默认的配置应该可以工作，但如果你需要做一些性能优化或者实验，可以尝试调整这些参数。


**Config paths**

 - <code>rpc</code>


**Env overrides**

 - <code>EMQX_RPC</code>



**Fields**

- mode: <code>sync | async</code>
  * default: 
  `async`

  在 <code>sync</code> 模式下，发送端等待接收端的 ack 信号。

- driver: <code>tcp | ssl</code>
  * default: 
  `tcp`
  * mapping: 
  `gen_rpc.driver`

  集群间通信使用的传输协议。

- async_batch_size: <code>integer()</code>
  * default: 
  `256`
  * mapping: 
  `gen_rpc.max_batch_size`

  异步模式下，发送的批量消息的最大数量。

- port_discovery: <code>manual | stateless</code>
  * default: 
  `stateless`
  * mapping: 
  `gen_rpc.port_discovery`

  <code>manual</code>: 通过 <code>tcp_server_port</code> 来发现端口。
  </br><code>stateless</code>: 使用无状态的方式来发现端口，使用如下算法。
  如果节点名称是 <code>emqxN@127.0.0.1</code>, N 是一个数字，那么监听端口就是 5370 + N。
           

- tcp_server_port: <code>integer()</code>
  * default: 
  `5369`
  * mapping: 
  `gen_rpc.tcp_server_port`

  RPC 本地服务使用的 TCP 端口。</br>
  只有当 rpc.port_discovery 设置为 manual 时，此配置才会生效。
        

- ssl_server_port: <code>integer()</code>
  * default: 
  `5369`
  * mapping: 
  `gen_rpc.ssl_server_port`

  RPC 本地服务使用的监听SSL端口。</br>
  只有当 rpc.port_discovery 设置为 manual 且 <code>dirver</code> 设置为 <code>ssl</code>，此配置才会生效。
        

- tcp_client_num: <code>1..256</code>
  * default: 
  `10`

  设置本节点与远程节点之间的 RPC 通信通道的最大数量。

- connect_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"5s"`
  * mapping: 
  `gen_rpc.connect_timeout`

  建立 RPC 连接的超时时间。

- certfile: <code>emqx_conf_schema:file()</code>
  * mapping: 
  `gen_rpc.certfile`

  TLS 证书文件的路径，用于验证集群节点的身份。
  只有当 <code>rpc.driver</code> 设置为 <code>ssl</code> 时，此配置才会生效。
        

- keyfile: <code>emqx_conf_schema:file()</code>
  * mapping: 
  `gen_rpc.keyfile`

  <code>rpc.certfile</code> 的私钥文件的路径。</br>
  注意：此文件内容是私钥，所以需要设置权限为 600。
        

- cacertfile: <code>emqx_conf_schema:file()</code>
  * mapping: 
  `gen_rpc.cacertfile`

  验证 <code>rpc.certfile</code> 的 CA 证书文件的路径。</br>
  注意：集群中所有节点的证书必须使用同一个 CA 签发。
        

- send_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"5s"`
  * mapping: 
  `gen_rpc.send_timeout`

  发送 RPC 请求的超时时间。

- authentication_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"5s"`
  * mapping: 
  `gen_rpc.authentication_timeout`

  远程节点认证的超时时间。

- call_receive_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"15s"`
  * mapping: 
  `gen_rpc.call_receive_timeout`

  同步 RPC 的回复超时时间。

- socket_keepalive_idle: <code>emqx_schema:duration_s()</code>
  * default: 
  `"15m"`
  * mapping: 
  `gen_rpc.socket_keepalive_idle`

  broker 之间的连接在最后一条消息发送后保持打开的时间。

- socket_keepalive_interval: <code>emqx_schema:duration_s()</code>
  * default: 
  `"75s"`
  * mapping: 
  `gen_rpc.socket_keepalive_interval`

  keepalive 消息的间隔。

- socket_keepalive_count: <code>integer()</code>
  * default: 
  `9`
  * mapping: 
  `gen_rpc.socket_keepalive_count`

  keepalive 探测消息发送失败的次数，直到 RPC 连接被认为已经断开。

- socket_sndbuf: <code>emqx_schema:bytesize()</code>
  * default: 
  `"1MB"`
  * mapping: 
  `gen_rpc.socket_sndbuf`

  TCP 调节参数。TCP 发送缓冲区大小。

- socket_recbuf: <code>emqx_schema:bytesize()</code>
  * default: 
  `"1MB"`
  * mapping: 
  `gen_rpc.socket_recbuf`

  TCP 调节参数。TCP 接收缓冲区大小。

- socket_buffer: <code>emqx_schema:bytesize()</code>
  * default: 
  `"1MB"`
  * mapping: 
  `gen_rpc.socket_buffer`

  TCP 调节参数。用户模式套接字缓冲区大小。


## topology
配置 Topology


**Config paths**

 - <code>authentication.$INDEX.topology</code>
 - <code>authorization.sources.$INDEX.topology</code>
 - <code>gateway.coap.authentication.topology</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.topology</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.topology</code>
 - <code>gateway.exproto.authentication.topology</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.topology</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.topology</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.topology</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.topology</code>
 - <code>gateway.lwm2m.authentication.topology</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.topology</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.topology</code>
 - <code>gateway.mqttsn.authentication.topology</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.topology</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.topology</code>
 - <code>gateway.stomp.authentication.topology</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.topology</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.topology</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.topology</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.topology</code>
 - <code>listeners.ws.$name.authentication.$INDEX.topology</code>
 - <code>listeners.wss.$name.authentication.$INDEX.topology</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__TOPOLOGY</code>
 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__TOPOLOGY</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__TOPOLOGY</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__TOPOLOGY</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__TOPOLOGY</code>



**Fields**

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- max_overflow: <code>non_neg_integer()</code>
  * default: 
  `0`

  最大溢出。

- overflow_ttl: <code>emqx_schema:duration_ms()</code>

  时间间隔，例如超时或 TTL。

- overflow_check_period: <code>emqx_schema:duration_ms()</code>

  时间间隔，例如超时或 TTL。

- local_threshold_ms: <code>emqx_schema:duration_ms()</code>

  时间间隔，例如超时或 TTL。

- connect_timeout_ms: <code>emqx_schema:duration_ms()</code>

  时间间隔，例如超时或 TTL。

- socket_timeout_ms: <code>emqx_schema:duration_ms()</code>

  时间间隔，例如超时或 TTL。

- server_selection_timeout_ms: <code>emqx_schema:duration_ms()</code>

  时间间隔，例如超时或 TTL。

- wait_queue_timeout_ms: <code>emqx_schema:duration_ms()</code>

  时间间隔，例如超时或 TTL。

- heartbeat_frequency_ms: <code>emqx_schema:duration_ms()</code>

  时间间隔，例如超时或 TTL。

- min_heartbeat_frequency_ms: <code>emqx_schema:duration_ms()</code>

  时间间隔，例如超时或 TTL。


## zone:conn_congestion
Settings for `conn_congestion` alarm.

Sometimes the MQTT connection (usually an MQTT subscriber) may
get "congested", because there are too many packets to be sent.
The socket tries to buffer the packets until the buffer is
full. If more packets arrive after that, the packets will be
"pending" in the queue, and we consider the connection
congested.

Note: `sndbuf` can be set to larger value if the
alarm is triggered too often.
The name of the alarm is of format `conn_congestion/<ClientID>/<Username>`,
where the `<ClientID>` is the client ID of the congested MQTT connection,
and `<Username>` is the username or `unknown_user`.


**Config paths**

 - <code>zones.$name.conn_congestion</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME__CONN_CONGESTION</code>



**Fields**

- enable_alarm: <code>boolean()</code>

  启用或者禁用连接阻塞告警功能

- min_alarm_sustain_duration: <code>emqx_schema:duration()</code>

  清除警报前的最短时间。

  只有当队列中没有挂起的数据，并且连接至少被堵塞了 "min_alarm_sustain_duration" 毫秒时，
  报警才会被清除。这是为了避免太频繁地清除和再次发出警报.


## zone:flapping_detect
This config controls the allowed maximum number of `CONNECT` packets received
from the same clientid in a time frame defined by `window_time`.
After the limit is reached, successive `CONNECT` requests are forbidden
(banned) until the end of the time period defined by `ban_time`.


**Config paths**

 - <code>zones.$name.flapping_detect</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME__FLAPPING_DETECT</code>



**Fields**

- enable: <code>boolean()</code>

  启用抖动检测功能

- max_count: <code>integer()</code>

  MQTT 客户端在"窗口"时间内允许的最大断开次数

- window_time: <code>emqx_schema:duration()</code>

  抖动检测的时间窗口。

- ban_time: <code>emqx_schema:duration()</code>

  抖动的客户端将会被禁止登陆多长时间


## zone:force_gc
Force garbage collection in MQTT connection process after
 they process certain number of messages or bytes of data.


**Config paths**

 - <code>zones.$name.force_gc</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME__FORCE_GC</code>



**Fields**

- enable: <code>boolean()</code>

  启用强制垃圾回收

- count: <code>0..inf</code>

  在进程收到多少消息之后，对此进程执行垃圾回收

- bytes: <code>emqx_schema:bytesize()</code>

  在进程处理过多少个字节之后，对此进程执行垃圾回收


## zone:force_shutdown
When the process message queue length, or the memory bytes
reaches a certain value, the process is forced to close.

Note: "message queue" here refers to the "message mailbox"
of the Erlang process, not the `mqueue` of QoS 1 and QoS 2.


**Config paths**

 - <code>zones.$name.force_shutdown</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME__FORCE_SHUTDOWN</code>



**Fields**

- enable: <code>boolean()</code>

  启用 `force_shutdown` 功能

- max_message_queue_len: <code>0..inf</code>

  消息队列的最大长度

- max_heap_size: <code>emqx_schema:wordsize()</code>

  Heap 的总大小


## zone:mqtt
Global MQTT configuration.</br>
The configs here work as default values which can be overridden
in <code>zone</code> configs


**Config paths**

 - <code>zones.$name.mqtt</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME__MQTT</code>



**Fields**

- idle_timeout: <code>infinity | emqx_schema:duration()</code>

  TCP 连接建立后，如果在 <code>idle_timeout</code> 指定的时间内未收到客户端的 MQTT CONNECT 报文，则连接将被断开。

- max_packet_size: <code>emqx_schema:bytesize()</code>

  允许的最大 MQTT 报文大小。

- max_clientid_len: <code>23..65535</code>

  允许的最大 MQTT Client ID 长度。

- max_topic_levels: <code>1..65535</code>

  允许的最大主题层级。

- max_qos_allowed: <code>qos()</code>

  允许的最大 QoS 等级。

- max_topic_alias: <code>0..65535</code>

  允许的最大主题别名数，0 表示不支持主题别名。

- retain_available: <code>boolean()</code>

  是否启用对 MQTT 保留消息的支持。

- wildcard_subscription: <code>boolean()</code>

  是否启用对 MQTT 通配符订阅的支持。

- shared_subscription: <code>boolean()</code>

  是否启用对 MQTT 共享订阅的支持。

- exclusive_subscription: <code>boolean()</code>

  是否启用对 MQTT 排它订阅的支持。

- ignore_loop_deliver: <code>boolean()</code>

  是否为 MQTT v3.1.1/v3.1.0 客户端忽略投递自己发布的消息，类似于 MQTT 5.0 中的 <code>No Local</code> 订阅选项

- strict_mode: <code>boolean()</code>

  是否以严格模式解析 MQTT 消息。
  当设置为 true 时，例如客户端 ID、主题名称等中的无效 utf8 字符串将导致客户端断开连接。

- response_information: <code>string()</code>

  指定返回给客户端的响应信息。如果设置为 ""，则禁用此功能。仅适用于使用 MQTT 5.0 协议的客户端。

- server_keepalive: <code>integer() | disabled</code>

  EMQX 要求客户端使用的保活时间，配置为 <code>disabled</code> 表示将使用客户端指定的保活时间。需要用到 MQTT 5.0 中的 <code>Server Keep Alive</code>，因此仅适用于使用 MQTT 5.0 协议的客户端。

- keepalive_backoff: <code>number()</code>

  Broker 判定客户端保活超时使用的退避乘数。如果 EMQX 在 <code>Keep Alive * Backoff * 2</code> 秒内未收到任何报文，EMQX 将关闭当前连接。

- max_subscriptions: <code>1..inf | infinity</code>

  允许每个客户端建立的最大订阅数量。

- upgrade_qos: <code>boolean()</code>

  投递消息时，是否根据订阅主题时的 QoS 等级来强制提升派发的消息的 QoS 等级。

- max_inflight: <code>1..65535</code>

  允许在完成应答前同时投递的 QoS 1 和 QoS 2 消息的最大数量。

- retry_interval: <code>emqx_schema:duration()</code>

  QoS 1/2 消息的重新投递间隔。

- max_awaiting_rel: <code>integer() | infinity</code>

  PUBREL (Client -> Broker) 最大等待队列长度。

- await_rel_timeout: <code>emqx_schema:duration()</code>

  PUBREL (Client -> Broker) 最大等待时间，超时则会被丢弃。

- session_expiry_interval: <code>emqx_schema:duration()</code>

  指定会话将在连接断开后多久过期，仅适用于非 MQTT 5.0 的连接。

- max_mqueue_len: <code>non_neg_integer() | infinity</code>

  消息队列最大长度。持久客户端断开连接或飞行窗口已满时排队的消息长度。

- mqueue_priorities: <code>map() | disabled</code>

  主题优先级。取值范围 [1-255]
  默认优先级表为空，即所有的主题优先级相同。

  注：优先主题名称中不支持使用逗号和等号。
  注：不在此列表中的主题，被视为最高/最低优先级，这取决于<code>mqtt.mqueue_default_priority</code> 的配置

  示例：
  配置 <code>"topic/1" > "topic/2"</code>:
  <code>mqueue_priorities: {"topic/1": 10, "topic/2": 8}</code>


- mqueue_default_priority: <code>highest | lowest</code>

  默认的主题优先级，不在 <code>主题优先级</code>（<code>mqueue_priorities</code>） 中的主题将会使用该优先级。

- mqueue_store_qos0: <code>boolean()</code>

  指定在连接断开但会话保持期间，是否需要在消息队列中存储 QoS 0 消息。

- use_username_as_clientid: <code>boolean()</code>

  是否使用用户名作为客户端 ID。
  此设置的作用时间晚于 <code>使用对端证书作为用户名</code>（<code>peer_cert_as_username</code>） 和 <code>使用对端证书作为客户端 ID</code>（<code>peer_cert_as_clientid</code>）。


- peer_cert_as_username: <code>disabled | cn | dn | crt | pem | md5</code>

  使用对端证书中的 CN, DN 字段或整个证书内容来作为用户名。仅适用于 TLS 连接。
  目前支持配置为以下内容：
  - <code>cn</code>: 取证书的 CN 字段作为 Username
  - <code>dn</code>: 取证书的 DN 字段作为 Username
  - <code>crt</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书的内容作为 Username
  - <code>pem</code>: 将 <code>DER</code> 证书内容转换为 <code>PEM</code> 格式后作为 Username
  - <code>md5</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书的内容的 MD5 值作为 Username


- peer_cert_as_clientid: <code>disabled | cn | dn | crt | pem | md5</code>

  使用对端证书中的 CN, DN 字段或整个证书内容来作为客户端 ID。仅适用于 TLS 连接。
  目前支持配置为以下内容：
  - <code>cn</code>: 取证书的 CN 字段作为 Client ID
  - <code>dn</code>: 取证书的 DN 字段作为 Client ID
  - <code>crt</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书的内容作为 Client ID
  - <code>pem</code>: 将 <code>DER</code> 证书内容转换为 <code>PEM</code> 格式后作为 Client ID
  - <code>md5</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书的内容的 MD5 值作为 Client ID



## zone:overload_protection
Overload protection mechanism monitors the load of the system and temporarily
disables some features (such as accepting new connections) when the load is high.


**Config paths**

 - <code>zones.$name.overload_protection</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME__OVERLOAD_PROTECTION</code>



**Fields**

- enable: <code>boolean()</code>

  是否对系统过载做出反应

- backoff_delay: <code>0..inf</code>

  一些不重要的任务可能会延迟执行，以毫秒为单位设置延迟

- backoff_gc: <code>boolean()</code>

  如有必要，跳过强制GC

- backoff_hibernation: <code>boolean()</code>

  如有必要，跳过进程休眠

- backoff_new_conn: <code>boolean()</code>

  如有必要，关闭新进来的连接


## zone:stats
Enable/disable statistic data collection.
Statistic data such as message receive/send count/rate etc. It provides insights of system performance and helps to diagnose issues. You can find statistic data from the dashboard, or from the '/stats' API.


**Config paths**

 - <code>zones.$name.stats</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME__STATS</code>



**Fields**

- enable: <code>boolean()</code>

  启用/禁用统计数据收集功能


## authn-builtin_db:authentication
使用内置数据库作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证机制。

- backend: <code>built_in_database</code>

  后端类型。

- user_id_type: <code>clientid | username</code>
  * default: 
  `"username"`

  指定使用客户端ID `clientid` 还是用户名 `username` 进行认证。

- password_hash_algorithm: <code>[authn-hash:bcrypt_rw](#authn-hash-bcrypt_rw) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash creation and verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


## authn-hash:bcrypt
Settings for bcrypt password hashing algorithm.


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>
 - <code>gateway.coap.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.ws.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.wss.$name.authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>bcrypt</code>

  BCRYPT password hashing.


## authn-hash:bcrypt_rw
Settings for bcrypt password hashing algorithm (for DB backends with write capability).


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>
 - <code>gateway.coap.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.ws.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.wss.$name.authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>bcrypt</code>

  BCRYPT password hashing.

- salt_rounds: <code>integer()</code>
  * default: 
  `10`

  Salt rounds for BCRYPT password generation.


## authn-hash:other_algorithms
Settings for other password hashing algorithms.


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>
 - <code>gateway.coap.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.ws.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.wss.$name.authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>plain | md5 | sha | sha256 | sha512</code>

  Simple password hashing algorithm.

- salt_position: <code>disable | prefix | suffix</code>
  * default: 
  `prefix`

  Salt position for PLAIN, MD5, SHA, SHA256 and SHA512 algorithms.


## authn-hash:pbkdf2
Settings for PBKDF2 password hashing algorithm.


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>
 - <code>gateway.coap.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.ws.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.wss.$name.authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>pbkdf2</code>

  PBKDF2 password hashing.

- mac_fun: <code>md4 | md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512</code>

  Specifies mac_fun for PBKDF2 hashing algorithm.

- iterations: <code>integer()</code>

  Iteration count for PBKDF2 hashing algorithm.

- dk_length: <code>integer()</code>

  Derived length for PBKDF2 hashing algorithm. If not specified, calculated automatically based on `mac_fun`.


## authn-http:get
使用 HTTP Server 作为认证服务的认证器的配置项 (使用 GET 请求)。


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- method: <code>get</code>
  * default: 
  `get`

  HTTP 请求方法。

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    "cache-control" = "no-cache"
    connection = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
  }
  ```

  HTTP Headers 列表 (无 <code>content-type</code>) 。

- mechanism: <code>password_based</code>

  认证机制。

- backend: <code>http</code>

  后端类型。

- url: <code>binary()</code>

  认证 HTTP 服务器地址。

- body: <code>#{term() => binary()}</code>

  HTTP request body。

- request_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"5s"`

  HTTP 请求超时时长。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。

- connect_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  连接HTTP服务器的超时时间。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `5`

  请求出错时的最大重试次数。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- request: <code>[connector-http:request](#connector-http-request)</code>


  如果提供了请求，调用者可以通过以下方式发送 HTTP 请求
  <code>emqx_resource:query(ResourceId, {send_message, BridgeId, Message})</code>


- retry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"1s"`

  重试之间的间隔时间。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn-http:post
使用 HTTP Server 作为认证服务的认证器的配置项 (使用 POST 请求)。


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- method: <code>post</code>
  * default: 
  `post`

  HTTP 请求方法。

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    "cache-control" = "no-cache"
    connection = "keep-alive"
    "content-type" = "application/json"
    "keep-alive" = "timeout=30, max=1000"
  }
  ```

  HTTP Headers 列表

- mechanism: <code>password_based</code>

  认证机制。

- backend: <code>http</code>

  后端类型。

- url: <code>binary()</code>

  认证 HTTP 服务器地址。

- body: <code>#{term() => binary()}</code>

  HTTP request body。

- request_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"5s"`

  HTTP 请求超时时长。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。

- connect_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  连接HTTP服务器的超时时间。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `5`

  请求出错时的最大重试次数。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- request: <code>[connector-http:request](#connector-http-request)</code>


  如果提供了请求，调用者可以通过以下方式发送 HTTP 请求
  <code>emqx_resource:query(ResourceId, {send_message, BridgeId, Message})</code>


- retry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"1s"`

  重试之间的间隔时间。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn-jwt:hmac-based
用于认证的 JWT 使用 HMAC 算法签发时的配置。


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- use_jwks: <code>false</code>

  是否使用 JWKS。

- algorithm: <code>hmac-based</code>

  JWT 签名算法，支持 HMAC (配置为 <code>hmac-based</code>）和 RSA、ECDSA (配置为 <code>public-key</code>)。

- secret: <code>binary()</code>

  使用 HMAC 算法时用于验证 JWT 的密钥

- secret_base64_encoded: <code>boolean()</code>
  * default: 
  `false`

  密钥是否为 Base64 编码。

- mechanism: <code>jwt</code>

  认证机制。

- acl_claim_name: <code>binary()</code>
  * default: 
  `"acl"`

  JWT claim name to use for getting ACL rules.

- verify_claims: <code>[term()]</code>
  * default: 
  `{}`


  需要验证的自定义声明列表，它是一个名称/值对列表。
  值可以使用以下占位符：
  - <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符
  认证时将验证 JWT（取自 Password 字段）中 claims 的值是否与 <code>verify_claims</code> 中要求的相匹配。


- from: <code>username | password</code>
  * default: 
  `password`

  要从中获取 JWT 的字段。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


## authn-jwt:jwks
用于认证的 JWTs 需要从 JWKS 端点获取时的配置。


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- use_jwks: <code>true</code>

  是否使用 JWKS。

- endpoint: <code>string()</code>

  JWKS 端点, 它是一个以 JWKS 格式返回服务端的公钥集的只读端点。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- refresh_interval: <code>integer()</code>
  * default: 
  `300`

  JWKS 刷新间隔。

- ssl: <code>[authn-jwt:ssl_enable](#authn-jwt-ssl_enable) | [authn-jwt:ssl_disable](#authn-jwt-ssl_disable)</code>
  * default: 
  `{enable = false}`

  SSL 选项。

- mechanism: <code>jwt</code>

  认证机制。

- acl_claim_name: <code>binary()</code>
  * default: 
  `"acl"`

  JWT claim name to use for getting ACL rules.

- verify_claims: <code>[term()]</code>
  * default: 
  `{}`


  需要验证的自定义声明列表，它是一个名称/值对列表。
  值可以使用以下占位符：
  - <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符
  认证时将验证 JWT（取自 Password 字段）中 claims 的值是否与 <code>verify_claims</code> 中要求的相匹配。


- from: <code>username | password</code>
  * default: 
  `password`

  要从中获取 JWT 的字段。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


## authn-jwt:public-key
用于认证的 JWT 使用 RSA 或 ECDSA 算法签发时的配置。


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- use_jwks: <code>false</code>

  是否使用 JWKS。

- algorithm: <code>public-key</code>

  JWT 签名算法，支持 HMAC (配置为 <code>hmac-based</code>）和 RSA、ECDSA (配置为 <code>public-key</code>)。

- public_key: <code>string()</code>

  用于验证 JWT 的公钥。

- mechanism: <code>jwt</code>

  认证机制。

- acl_claim_name: <code>binary()</code>
  * default: 
  `"acl"`

  JWT claim name to use for getting ACL rules.

- verify_claims: <code>[term()]</code>
  * default: 
  `{}`


  需要验证的自定义声明列表，它是一个名称/值对列表。
  值可以使用以下占位符：
  - <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符
  认证时将验证 JWT（取自 Password 字段）中 claims 的值是否与 <code>verify_claims</code> 中要求的相匹配。


- from: <code>username | password</code>
  * default: 
  `password`

  要从中获取 JWT 的字段。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


## authn-jwt:ssl_disable
SSL 配置。


**Config paths**

 - <code>authentication.$INDEX.ssl</code>
 - <code>gateway.coap.authentication.ssl</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.exproto.authentication.ssl</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.ssl</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.ssl</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.lwm2m.authentication.ssl</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.mqttsn.authentication.ssl</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.stomp.authentication.ssl</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.ssl</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.ssl</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.ssl</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.ssl</code>
 - <code>listeners.ws.$name.authentication.$INDEX.ssl</code>
 - <code>listeners.wss.$name.authentication.$INDEX.ssl</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__SSL</code>



**Fields**

- enable: <code>false</code>

  启用/禁用 SSL。


## authn-jwt:ssl_enable
SSL 配置。


**Config paths**

 - <code>authentication.$INDEX.ssl</code>
 - <code>gateway.coap.authentication.ssl</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.exproto.authentication.ssl</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.ssl</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.ssl</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.lwm2m.authentication.ssl</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.mqttsn.authentication.ssl</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.stomp.authentication.ssl</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.ssl</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.ssl</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.ssl</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.ssl</code>
 - <code>listeners.ws.$name.authentication.$INDEX.ssl</code>
 - <code>listeners.wss.$name.authentication.$INDEX.ssl</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__SSL</code>



**Fields**

- enable: <code>true</code>

  启用/禁用 SSL。

- cacertfile: <code>string()</code>

  包含 PEM 编码的 CA 证书的文件的路径。

- certfile: <code>string()</code>

  包含用户证书的文件的路径。

- keyfile: <code>string()</code>

  包含 PEM 编码的用户私钥的文件的路径。

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  指定握手过程中是否校验对端证书。

- server_name_indication: <code>string()</code>

  服务器名称指示（SNI）。


## authn-mongodb:replica-set
使用 MongoDB (Replica Set) 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证机制。

- backend: <code>mongodb</code>

  后端类型。

- collection: <code>binary()</code>

  存储认证数据的集合。

- filter: <code>map()</code>
  * default: 
  `{}`


  在查询中定义过滤条件的条件表达式。
  过滤器支持如下占位符：
  - <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符


- password_hash_field: <code>binary()</code>
  * default: 
  `"password_hash"`

  文档中用于存放密码散列的字段。

- salt_field: <code>binary()</code>
  * default: 
  `"salt"`

  文档中用于存放盐值的字段。

- is_superuser_field: <code>binary()</code>
  * default: 
  `"is_superuser"`

  文档中用于定义用户是否具有超级用户权限的字段。

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。

- mongo_type: <code>rs</code>
  * default: 
  `rs`

  Replica set模式。

- servers: <code>[term()]</code>


  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
  每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
  主机名具有以下形式：`Host[:Port]`。
  如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写模式。

- r_mode: <code>master | slave_ok</code>
  * default: 
  `master`

  读模式。

- replica_set_name: <code>binary()</code>

  副本集的名称。

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>binary()</code>

  内部数据库密码。

- auth_source: <code>binary()</code>

  与用户证书关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[topology](#topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn-mongodb:sharded-cluster
使用 MongoDB (Sharded Cluster) 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证机制。

- backend: <code>mongodb</code>

  后端类型。

- collection: <code>binary()</code>

  存储认证数据的集合。

- filter: <code>map()</code>
  * default: 
  `{}`


  在查询中定义过滤条件的条件表达式。
  过滤器支持如下占位符：
  - <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符


- password_hash_field: <code>binary()</code>
  * default: 
  `"password_hash"`

  文档中用于存放密码散列的字段。

- salt_field: <code>binary()</code>
  * default: 
  `"salt"`

  文档中用于存放盐值的字段。

- is_superuser_field: <code>binary()</code>
  * default: 
  `"is_superuser"`

  文档中用于定义用户是否具有超级用户权限的字段。

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。

- mongo_type: <code>sharded</code>
  * default: 
  `sharded`

  Sharded cluster模式。

- servers: <code>[term()]</code>


  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
  每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
  主机名具有以下形式：`Host[:Port]`。
  如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写模式。

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>binary()</code>

  内部数据库密码。

- auth_source: <code>binary()</code>

  与用户证书关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[topology](#topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn-mongodb:standalone
使用 MongoDB (Standalone) 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证机制。

- backend: <code>mongodb</code>

  后端类型。

- collection: <code>binary()</code>

  存储认证数据的集合。

- filter: <code>map()</code>
  * default: 
  `{}`


  在查询中定义过滤条件的条件表达式。
  过滤器支持如下占位符：
  - <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符


- password_hash_field: <code>binary()</code>
  * default: 
  `"password_hash"`

  文档中用于存放密码散列的字段。

- salt_field: <code>binary()</code>
  * default: 
  `"salt"`

  文档中用于存放盐值的字段。

- is_superuser_field: <code>binary()</code>
  * default: 
  `"is_superuser"`

  文档中用于定义用户是否具有超级用户权限的字段。

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。

- mongo_type: <code>single</code>
  * default: 
  `single`

  Standalone模式。

- server: <code>emqx_schema:ip_port()</code>


  将要连接的 IPv4 或 IPv6 地址，或者主机名。</br>
  主机名具有以下形式：`Host[:Port]`。</br>
  如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写模式。

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>binary()</code>

  内部数据库密码。

- auth_source: <code>binary()</code>

  与用户证书关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[topology](#topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn-mysql:authentication
使用 MySQL 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证机制。

- backend: <code>mysql</code>

  后端类型。

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- query: <code>string()</code>

  用于查询密码散列等用于认证的数据的 SQL 语句。

- query_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"5s"`

  SQL 查询的超时时间。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。

- server: <code>emqx_schema:ip_port()</code>


  将要连接的 IPv4 或 IPv6 地址，或者主机名。</br>
  主机名具有以下形式：`Host[:Port]`。</br>
  如果未指定 `[:Port]`，则使用 MySQL 默认端口 3306。


- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>binary()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  自动重连数据库。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn-postgresql:authentication
使用 PostgreSQL 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证机制。

- backend: <code>postgresql</code>

  后端类型。

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- query: <code>string()</code>

  用于查询密码散列等用于认证的数据的 SQL 语句。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。

- server: <code>emqx_schema:ip_port()</code>


  将要连接的 IPv4 或 IPv6 地址，或者主机名。</br>
  主机名具有以下形式：`Host[:Port]`。</br>
  如果未指定 `[:Port]`，则使用 PostgreSQL 默认端口 5432。


- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>binary()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  自动重连数据库。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn-psk:psk_authentication
此配置用于启用 TLS-PSK 身份验证。

PSK 是 “Pre-Shared-Keys” 的缩写。

注意: 确保 SSL 监听器仅启用了 'tlsv1.2', 并且配置了PSK 密码套件，例如 'RSA-PSK-AES256-GCM-SHA384'。

可以通过查看监听器中的 SSL 选项，了解更多详细信息。

可以通过配置 'init_file' 来设置初始化的 ID 和 密钥


**Config paths**

 - <code>psk_authentication</code>


**Env overrides**

 - <code>EMQX_PSK_AUTHENTICATION</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  是否开启 TLS PSK 支持

- init_file: <code>binary()</code>

  如果设置了初始化文件，EMQX 将在启动时从初始化文件中导入 PSK 信息到内建数据库中。
  这个文件需要按行进行组织，每一行必须遵守如下格式: <code>PSKIdentity:SharedSecret</code>
  例如: <code>mydevice1:c2VjcmV0</code>

- separator: <code>binary()</code>
  * default: 
  `":"`

  PSK 文件中 <code>PSKIdentity</code> 和 <code>SharedSecret</code> 之间的分隔符

- chunk_size: <code>integer()</code>
  * default: 
  `50`

  将 PSK 文件导入到内建数据时每个块的大小


## authn-redis:cluster
使用 Redis (Cluster) 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证机制。

- backend: <code>redis</code>

  后端类型。

- cmd: <code>string()</code>

  用于查询密码散列等用于认证的数据的 Redis Command，目前仅支持 <code>HGET</code> 与 <code>HMGET</code>。

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。

- servers: <code>[term()]</code>



  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
  每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
  主机名具有以下形式：`Host[:Port]`。
  如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


- redis_type: <code>cluster</code>

  集群模式。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- password: <code>binary()</code>

  内部数据库密码。

- database: <code>integer()</code>
  * default: 
  `0`

  Redis 数据库 ID。

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  自动重连数据库。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn-redis:sentinel
使用 Redis (Sentinel) 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证机制。

- backend: <code>redis</code>

  后端类型。

- cmd: <code>string()</code>

  用于查询密码散列等用于认证的数据的 Redis Command，目前仅支持 <code>HGET</code> 与 <code>HMGET</code>。

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。

- servers: <code>[term()]</code>



  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
  每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
  主机名具有以下形式：`Host[:Port]`。
  如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


- redis_type: <code>sentinel</code>

  哨兵模式。

- sentinel: <code>string()</code>

  Redis 哨兵模式下的集群名称。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- password: <code>binary()</code>

  内部数据库密码。

- database: <code>integer()</code>
  * default: 
  `0`

  Redis 数据库 ID。

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  自动重连数据库。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn-redis:standalone
使用 Redis (Standalone) 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证机制。

- backend: <code>redis</code>

  后端类型。

- cmd: <code>string()</code>

  用于查询密码散列等用于认证的数据的 Redis Command，目前仅支持 <code>HGET</code> 与 <code>HMGET</code>。

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。

- server: <code>emqx_schema:ip_port()</code>


  将要连接的 IPv4 或 IPv6 地址，或者主机名。</br>
  主机名具有以下形式：`Host[:Port]`。</br>
  如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


- redis_type: <code>single</code>

  单机模式。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- password: <code>binary()</code>

  内部数据库密码。

- database: <code>integer()</code>
  * default: 
  `0`

  Redis 数据库 ID。

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  自动重连数据库。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn-scram-builtin_db:authentication
Settings for Salted Challenge Response Authentication Mechanism
(SCRAM) authentication.


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>scram</code>

  认证机制。

- backend: <code>built_in_database</code>

  后端类型。

- algorithm: <code>sha256 | sha512</code>
  * default: 
  `sha256`

  Hashing algorithm.

- iteration_count: <code>non_neg_integer()</code>
  * default: 
  `4096`

  Iteration count.

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


## auto_subscribe
设备登陆成功之后，通过预设的订阅表示符，为设备自动完成订阅。支持使用占位符。


**Config paths**

 - <code>auto_subscribe</code>


**Env overrides**

 - <code>EMQX_AUTO_SUBSCRIBE</code>



**Fields**

- topics: <code>[[auto_subscribe:topic](#auto_subscribe-topic)]</code>
  * default: 
  `[]`

  设备登陆成功之后，通过预设的订阅表示符，为设备自动完成订阅。支持使用占位符。


## auto_subscribe:topic
订阅标识符，支持使用占位符，例如 client/${clientid}/username/${username}/host/${host}/port/${port}
必填，且不可为空字符串


**Config paths**

 - <code>auto_subscribe.topics.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTO_SUBSCRIBE__TOPICS__$INDEX</code>



**Fields**

- topic: <code>binary()</code>

  订阅标识符，支持使用占位符，例如 client/${clientid}/username/${username}/host/${host}/port/${port}
  必填，且不可为空字符串

- qos: <code>qos()</code>
  * default: 
  `0`

  缺省值为 0，服务质量，
  QoS 0：消息最多传递一次，如果当时客户端不可用，则会丢失该消息。
  QoS 1：消息传递至少 1 次。
  QoS 2：消息仅传送一次。

- rh: <code>0..2</code>
  * default: 
  `0`

  指定订阅建立时服务端是否向客户端发送保留消息，
  可选值 0：只要客户端订阅成功，服务端就发送保留消息。
  可选值 1：客户端订阅成功且该订阅此前不存在，服务端才发送保留消息。毕竟有些时候客户端重新发起订阅可能只是为了改变一下 QoS，并不意味着它想再次接收保留消息。
  可选值 2：即便客户订阅成功，服务端也不会发送保留消息。

- rap: <code>0..1</code>
  * default: 
  `0`

  缺省值为 0，这一选项用来指定服务端向客户端转发消息时是否要保留其中的 RETAIN 标识，注意这一选项不会影响保留消息中的 RETAIN 标识。因此当 Retain As Publish 选项被设置为 0 时，客户端直接依靠消息中的 RETAIN 标识来区分这是一个正常的转发消息还是一个保留消息，而不是去判断消息是否是自己订阅后收到的第一个消息（转发消息甚至可能会先于保留消息被发送，视不同 Broker 的具体实现而定）。

- nl: <code>0..1</code>
  * default: 
  `0`

  缺省值为0，
  MQTT v3.1.1：如果设备订阅了自己发布消息的主题，那么将收到自己发布的所有消息。
  MQTT v5：如果设备在订阅时将此选项设置为 1，那么服务端将不会向设备转发自己发布的消息


## bridge:bridges
MQTT Bridge 配置


**Config paths**

 - <code>bridges</code>


**Env overrides**

 - <code>EMQX_BRIDGES</code>



**Fields**

- webhook: <code>{$name -> [bridge:config](#bridge-config)}</code>

  转发消息到 HTTP 服务器的 WebHook

- mqtt: <code>{$name -> [ingress](#ingress) | [egress](#egress)}</code>

  桥接到另一个 MQTT Broker 的 MQTT Bridge


## bridge:config
HTTP Bridge 配置


**Config paths**

 - <code>bridges.webhook.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__WEBHOOK__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用 Bridge

- direction: <code>egress</code>
  * default: 
  `egress`

  Bridge 的方向， 必须是 egress

- connect_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  连接HTTP服务器的超时时间。

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `5`

  请求出错时的最大重试次数。

- retry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"1s"`

  重试之间的间隔时间。

- pool_type: <code>emqx_connector_http:pool_type()</code>
  * default: 
  `random`

  连接池的类型，可用类型有`random`, `hash`。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。

- request: <code>[connector-http:request](#connector-http-request)</code>


  如果提供了请求，调用者可以通过以下方式发送 HTTP 请求
  <code>emqx_resource:query(ResourceId, {send_message, BridgeId, Message})</code>


- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- url: <code>binary()</code>


  HTTP Bridge 的 URL。</br>
  路径中允许使用带变量的模板，但是 host， port 不允许使用变量模板。</br>
  例如，<code> http://localhost:9901/${topic} </code> 是允许的，
  但是<code> http://${host}:9901/message </code>
  或 <code> http://localhost:${port}/message </code>
  不允许。


- local_topic: <code>binary()</code>


  发送到 'local_topic' 的消息都会转发到 HTTP 服务器。 </br>
  注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发到 HTTP 服务器。


- method: <code>post | put | get | delete</code>
  * default: 
  `post`


  HTTP 请求的方法。 所有可用的方法包括：post、put、get、delete。</br>
  允许使用带有变量的模板。</br>

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    "cache-control" = "no-cache"
    connection = "keep-alive"
    "content-type" = "application/json"
    "keep-alive" = "timeout=5"
  }
  ```


  HTTP 请求的标头。</br>
  允许使用带有变量的模板。


- body: <code>binary()</code>
  * default: 
  `"${payload}"`


  HTTP 请求的正文。</br>
  允许使用带有变量的模板。

- request_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  HTTP 请求超时


## connector-http:request



**Config paths**

 - <code>authentication.$INDEX.request</code>
 - <code>authorization.sources.$INDEX.request</code>
 - <code>bridges.webhook.$name.request</code>
 - <code>gateway.coap.authentication.request</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.request</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.request</code>
 - <code>gateway.exproto.authentication.request</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.request</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.request</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.request</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.request</code>
 - <code>gateway.lwm2m.authentication.request</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.request</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.request</code>
 - <code>gateway.mqttsn.authentication.request</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.request</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.request</code>
 - <code>gateway.stomp.authentication.request</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.request</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.request</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.request</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.request</code>
 - <code>listeners.ws.$name.authentication.$INDEX.request</code>
 - <code>listeners.wss.$name.authentication.$INDEX.request</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__REQUEST</code>
 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX__REQUEST</code>
 - <code>EMQX_BRIDGES__WEBHOOK__$NAME__REQUEST</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__REQUEST</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__REQUEST</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__REQUEST</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__REQUEST</code>



**Fields**

- method: <code>post | put | get | delete</code>

  HTTP 请求方法。

- path: <code>binary()</code>

  HTTP请求路径。

- body: <code>binary()</code>

  HTTP请求报文主体。

- headers: <code>map()</code>

  HTTP 头字段列表。

- request_timeout: <code>emqx_schema:duration_ms()</code>

  HTTP 请求超时。


## connector-mqtt:connector
连接器的通用配置。


**Config paths**

 - <code>bridges.mqtt.$name.connector</code>
 - <code>connectors.mqtt.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__CONNECTOR</code>
 - <code>EMQX_CONNECTORS__MQTT__$NAME</code>



**Fields**

- mode: <code>cluster_shareload</code>
  * default: 
  `cluster_shareload`


  MQTT 桥的模式。 可用的类型有“cluster_singleton”，“cluster_shareload”。</br>

  - cluster_singleton：在 emqx 集群内创建唯一的 MQTT 连接。</br>
  在“cluster_singleton”节点中，所有发往远程代理的消息都通过相同的MQTT 连接。</br>

  - cluster_shareload：在 emqx 集群的每个节点上创建一个 MQTT 连接。</br>
  在“cluster_shareload”模式下，来自远程代理的传入负载通过共享订阅的方式接收。</br>
  请注意，“clientid”以节点名称为后缀，这是为了避免不同节点之间的clientid冲突。
  而且对于入口连接的“remote_topic”，我们只能使用共享订阅主题过滤器。


- server: <code>emqx_schema:ip_port()</code>

  远程 MQTT Broker的主机和端口。

- reconnect_interval: <code>string()</code>
  * default: 
  `"15s"`

  Reconnect interval. Delay for the MQTT bridge to retry establishing the connection in case of transportation failure. Time interval is a string that contains a number followed by time unit:</br>
  - `ms` for milliseconds,
  - `s` for seconds,
  - `m` for minutes,
  - `h` for hours;
  </br>or combination of whereof: `1h5m0s`

- proto_ver: <code>v3 | v4 | v5</code>
  * default: 
  `v4`

  MQTT 协议版本

- bridge_mode: <code>boolean()</code>
  * default: 
  `false`


  是否启用 Bridge Mode。
  注意：此设置只针对 MQTT 协议版本 < 5.0 有效，并且需要远程 MQTT Broker 支持 Bridge Mode。
      

- username: <code>binary()</code>
  * default: 
  `"emqx"`

  MQTT 协议的用户名

- password: <code>binary()</code>
  * default: 
  `"emqx"`

  MQTT 协议的密码

- clean_start: <code>boolean()</code>
  * default: 
  `true`

  MQTT 清除会话

- keepalive: <code>string()</code>
  * default: 
  `"300s"`

  MQTT Keepalive. Time interval is a string that contains a number followed by time unit:</br>
  - `ms` for milliseconds,
  - `s` for seconds,
  - `m` for minutes,
  - `h` for hours;
  </br>or combination of whereof: `1h5m0s`

- retry_interval: <code>string()</code>
  * default: 
  `"15s"`

  Message retry interval. Delay for the MQTT bridge to retry sending the QoS1/QoS2 messages in case of ACK not received. Time interval is a string that contains a number followed by time unit:</br>
  - `ms` for milliseconds,
  - `s` for seconds,
  - `m` for minutes,
  - `h` for hours;
  </br>or combination of whereof: `1h5m0s`

- max_inflight: <code>non_neg_integer()</code>
  * default: 
  `32`

  MQTT 协议的最大飞行（已发送但未确认）消息

- replayq: <code>[connector-mqtt:replayq](#connector-mqtt-replayq)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## connector-mqtt:replayq
本地磁盘消息队列


**Config paths**

 - <code>bridges.mqtt.$name.connector.replayq</code>
 - <code>connectors.mqtt.$name.replayq</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__CONNECTOR__REPLAYQ</code>
 - <code>EMQX_CONNECTORS__MQTT__$NAME__REPLAYQ</code>



**Fields**

- dir: <code>boolean() | string()</code>


  replayq 文件保存的目录。</br>
  设置为 'false' 会禁用 replayq 功能。


- seg_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `"100MB"`


  单个段的大小（以字节为单位）。</br>
  一个段映射到 replayq 目录中的一个文件。 如果当前段已满，则新段（文件）将被打开写入。


- offload: <code>boolean()</code>
  * default: 
  `false`


  在Offload模式下，磁盘队列仅用于卸载队列尾段。</br>
  消息首先缓存在内存中，然后写入replayq文件。内存缓大小为“seg_bytes” 指定的值。



## plugin:plugins
管理EMQX插件。</br>
插件可以是EMQX安装包中的一部分，也可以是一个独立的安装包。</br>
独立安装的插件称为“外部插件”。
           


**Config paths**

 - <code>plugins</code>


**Env overrides**

 - <code>EMQX_PLUGINS</code>



**Fields**

- states: <code>[[plugin:state](#plugin-state)]</code>
  * default: 
  `[]`

  一组插件的状态。插件将按照定义的顺序启动

- install_dir: <code>string()</code>
  * default: 
  `"plugins"`

  插件安装包的目录, 不要自己创建, 只能由emqx用户创建与修改

- check_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"5s"`

  检查间隔：检查集群中插件的状态是否一致，</br>
  如果连续3次检查结果不一致，则报警。



## plugin:state
描述插件的状态


**Config paths**

 - <code>plugins.states.$INDEX</code>


**Env overrides**

 - <code>EMQX_PLUGINS__STATES__$INDEX</code>



**Fields**

- name_vsn: <code>string()</code>

  插件的名称{name}-{version}。</br>
  它应该与插件的发布包名称一致，如my_plugin-0.1.0。

- enable: <code>boolean()</code>

  设置为“true”以启用此插件


## prometheus
Prometheus 监控数据推送


**Config paths**

 - <code>prometheus</code>


**Env overrides**

 - <code>EMQX_PROMETHEUS</code>



**Fields**

- push_gateway_server: <code>string()</code>
  * default: 
  `"http://127.0.0.1:9091"`

  Prometheus 服务器地址

- interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  数据推送间隔，单位 毫秒

- enable: <code>boolean()</code>
  * default: 
  `false`

  开启或关闭 Prometheus 数据推送


## retainer:flow_control
Retainer batching and rate limiting.


**Config paths**

 - <code>retainer.flow_control</code>


**Env overrides**

 - <code>EMQX_RETAINER__FLOW_CONTROL</code>



**Fields**

- batch_read_number: <code>non_neg_integer()</code>
  * default: 
  `0`

  从存储后端批量加载时的每批数量上限，0 代表一次性读取

- batch_deliver_number: <code>0..1000</code>
  * default: 
  `0`

  批量派发时每批的数量。0 代表一次性全部派发

- batch_deliver_limiter: <code>emqx_limiter_schema:bucket_name()</code>

  批量发送的限流器的名称。
  限流器可以用来防止短时间内向客户端发送太多的消息，从而避免过多的消息导致客户端队列堵塞甚至崩溃。
  这个名称需要是指向 `limiter.batch` 下的一个真实存在的限流器。
  如果这个字段为空，则不使用限流器。
        


## retainer:mnesia_config
Configuration of the internal database storing retained messages.


**Config paths**

 - <code>retainer.backend</code>


**Env overrides**

 - <code>EMQX_RETAINER__BACKEND</code>



**Fields**

- type: <code>built_in_database</code>
  * default: 
  `built_in_database`

  后端类型

- storage_type: <code>ram | disc</code>
  * default: 
  `ram`

  选择消息是存放在磁盘还是内存中

- max_retained_messages: <code>non_neg_integer()</code>
  * default: 
  `0`

  消息保留的数量上限。0 表示无限

- index_specs: <code>[[integer()]]</code>
  * default: 

  ```
  [
    [1, 2, 3],
    [1, 3],
    [2, 3],
    [3]
  ]
  ```

  Retainer index specifications: list of arrays of positive ascending integers. Each array specifies an index. Numbers in an index specification are 1-based word positions in topics. Words from specified positions will be used for indexing.</br>For example, it is good to have <code>[2, 4]</code> index to optimize <code>+/X/+/Y/...</code> topic wildcard subscriptions.


## retainer
Configuration related to handling `PUBLISH` packets with a `retain` flag set to 1.


**Config paths**

 - <code>retainer</code>


**Env overrides**

 - <code>EMQX_RETAINER</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否开启消息保留功能

- msg_expiry_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"0s"`

  消息保留时间。0 代表永久保留

- msg_clear_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"0s"`

  消息清理间隔。0 代表不进行清理

- flow_control: <code>[retainer:flow_control](#retainer-flow_control)</code>

  流控设置

- max_payload_size: <code>emqx_schema:bytesize()</code>
  * default: 
  `"1MB"`

  消息大小最大值

- stop_publish_clear_msg: <code>boolean()</code>
  * default: 
  `false`

  是否不发送保留消息的清理消息，在 MQTT 5.0 中如果一条保留消息的消息体为空，则会清除掉之前存储
  的对应的保留消息，通过这个值控制是否停止发送清理消息

- backend: <code>[retainer:mnesia_config](#retainer-mnesia_config)</code>

  保留消息的存储后端


## slow_subs
Configuration for `slow_subs` feature.


**Config paths**

 - <code>slow_subs</code>


**Env overrides**

 - <code>EMQX_SLOW_SUBS</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  开启慢订阅

- threshold: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"500ms"`

  慢订阅统计的阈值

- expire_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"300s"`

  慢订阅记录的有效时间

- top_k_num: <code>pos_integer()</code>
  * default: 
  `10`

  慢订阅统计表的记录数量上限

- stats_type: <code>whole | internal | response</code>
  * default: 
  `whole`

  慢订阅的统计类型


## statsd
Statsd 监控数据推送


**Config paths**

 - <code>statsd</code>


**Env overrides**

 - <code>EMQX_STATSD</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  开启或关闭 Statsd 数据推送

- server: <code>emqx_schema:ip_port()</code>
  * default: 
  `"127.0.0.1:8125"`

  Statsd 服务器地址

- sample_time_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"10s"`

  数据收集间隔，单位 毫秒

- flush_time_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"10s"`

  数据推送间隔，单位 毫秒


