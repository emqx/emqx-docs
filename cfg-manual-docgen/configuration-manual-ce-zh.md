# EMQX Enterprise Configuration

<!--5.5.0-alpha.1-g4688b36c-->
EMQX的配置文件格式是 [HOCON](https://github.com/emqx/hocon) 。
HOCON（Human-Optimized Config Object Notation）是一个JSON的超集，非常适用于易于人类读写的配置数据存储。

## 分层结构

EMQX的配置文件可分为二层，自底向上依次是：

1. 集群同步配置：`$EMQX_NODE__DATA_DIR/configs/cluster.hocon`。
2. 本地节点配置：`emqx.conf` 加上 `EMQX_` 前缀的环境变量。

:::tip Tip
在 v5.0.23 或 e5.0.3 之前，集群同步配置保存在文件 `cluster-override.conf` 中，并且它覆盖在配置的最上层。

如果从之前的版本升级上来，只要 `cluster-override.conf` 文件存在，
EMQX 就不会创建 `cluster.hocon`，并且 `cluster-override.conf` 会继续覆盖在配置的最上层。
:::

如果环境变量 `$EMQX_NODE__DATA_DIR` 没有设置，那么该目录会从 `emqx.conf` 的 `node.data_dir` 配置中读取。

配置文件 `cluster.hocon` 的内容会在运行时被EMQX重写。
这些重写发生在 dashboard UI，管理HTTP API，或者CLI对集群配置进行修改时。
当EMQX运行在集群中时，一个EMQX节点重启之后，会从集群中其他节点复制该文件内容到本地。

:::tip Tip
为避免歧义，应尽量避免让 `cluster.hocon` 和 `emqx.conf` 出现配置交集。
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

EMQX的配置文件中，有4中复杂数据结构类型，它们分别是：

1. Struct：结构体都是有类型名称的，结构体中可以有任意多个字段。
   结构体和字段的名称由不带特殊字符的全小些字母组成，名称中可以带数字，但不得以数字开头，多个单词可用下划线分隔。
1. Map: Map 与 Struct（结构体）类似，但是内部的字段不是预先定义好的。
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

* 原子 `atom()`。
* 布尔 `boolean()`。
* 字符串 `string()`。
* 整形 `integer()`。
* 浮点数 `float()`。
* 数值 `number()`。
* 二进制编码的字符串 `binary()` 是 `string()` 的另一种格式。
* 时间间隔 `emqx_schema:duration()` 是 `integer()` 的另一种格式。
* ...

::: tip Tip
原始类型的名称大多是自我描述的，所以不需要过多的注释。
但是有一些不是那么直观的数据类型，则需要配合字段的描述文档进行理解。
:::


### 配置路径

如果我们把EMQX的配置值理解成一个类似目录树的结构，那么类似于文件系统中使用斜杠或反斜杠进行层级分割，
EMQX使用的配置路径的层级分割符是 `'.'`

被 `'.'` 号分割的每一段，则是 Struct（结构体）的字段，或 Map 的 key。

下面有几个例子：

```
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

### 环境变量重载

因为 `'.'`  分隔符不能使用于环境变量，所以我们需要使用另一个分割符。EMQX选用的是双下划线 `__`。
为了与其他的环境变量有所区分，EMQX还增加了一个前缀 `EMQX_` 来用作环境变量命名空间。

例如 `node.name` 的重载变量名是 `EMQX_NODE__NAME`。

环境变量的值，是按 HOCON 值解析的，这也使得环境变量可以用来传递复杂数据类型的值。

例如，下面这个环境变量传入一个数组类型的值。

```
export EMQX_LISTENERS__SSL__L1__AUTHENTICATION__SSL__CIPHERS='["TLS_AES_256_GCM_SHA384"]'
```

这也意味着有些带特殊字符（例如`:` 和 `=`），则需要用双引号对这个值包起来。

例如`localhost:1883` 会被解析成一个结构体 `{"localhost": 1883}`。
想要把它当字符串使用时，就必需使用引号，如下：

```
EMQX_BRIDGES__MQTT__MYBRIDGE__CONNECTOR_SERVER='"localhost:1883"'
```


::: tip Tip
未定义的根路径会被EMQX忽略，例如 `EMQX_UNKNOWN_ROOT__FOOBAR` 这个环境变量会被EMQX忽略，
因为 `UNKNOWN_ROOT` 不是预先定义好的根路径。
对于已知的根路径，未知的字段名称将被记录为warning日志，比如下面这个例子。

```
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

这是因为正确的字段名称是 `enable`，而不是 `enabled`。
:::

### 配置重载规则

HOCON的值是分层覆盖的，普遍规则如下：

- 在同一个文件中，后（在文件底部）定义的值，覆盖前（在文件顶部）到值。
- 当按层级覆盖时，高层级的值覆盖低层级的值。

结下来的文档将解释更详细的规则。

#### 结构体

合并覆盖规则。在如下配置中，最后一行的 `debug` 值会覆盖覆盖原先`level`字段的 `error` 值，但是 `enable` 字段保持不变。
```
log {
    console_handler{
        enable=true,
        level=error
    }
}

## 控制台日志打印先定义为 `error` 级，后被覆写成 `debug` 级

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

#### TLS/SSL ciphers

从 v5.0.6 开始 EMQX 不在配置文件中详细列出所有默认的密码套件名称。
而是在配置文件中使用一个空列表，然后在运行时替换成默认的密码套件。

下面这些密码套件是 EMQX 默认支持的：

tlsv1.3:
```
ciphers =
  [ "TLS_AES_256_GCM_SHA384", "TLS_AES_128_GCM_SHA256",
    "TLS_CHACHA20_POLY1305_SHA256", "TLS_AES_128_CCM_SHA256",
    "TLS_AES_128_CCM_8_SHA256"
  ]
```

tlsv1.2 或更早

```
ciphers =
  [ "ECDHE-ECDSA-AES256-GCM-SHA384",
    "ECDHE-RSA-AES256-GCM-SHA384",
    "ECDHE-ECDSA-AES256-SHA384",
    "ECDHE-RSA-AES256-SHA384",
    "ECDH-ECDSA-AES256-GCM-SHA384",
    "ECDH-RSA-AES256-GCM-SHA384",
    "ECDH-ECDSA-AES256-SHA384",
    "ECDH-RSA-AES256-SHA384",
    "DHE-DSS-AES256-GCM-SHA384",
    "DHE-DSS-AES256-SHA256",
    "AES256-GCM-SHA384",
    "AES256-SHA256",
    "ECDHE-ECDSA-AES128-GCM-SHA256",
    "ECDHE-RSA-AES128-GCM-SHA256",
    "ECDHE-ECDSA-AES128-SHA256",
    "ECDHE-RSA-AES128-SHA256",
    "ECDH-ECDSA-AES128-GCM-SHA256",
    "ECDH-RSA-AES128-GCM-SHA256",
    "ECDH-ECDSA-AES128-SHA256",
    "ECDH-RSA-AES128-SHA256",
    "DHE-DSS-AES128-GCM-SHA256",
    "DHE-DSS-AES128-SHA256",
    "AES128-GCM-SHA256",
    "AES128-SHA256",
    "ECDHE-ECDSA-AES256-SHA",
    "ECDHE-RSA-AES256-SHA",
    "DHE-DSS-AES256-SHA",
    "ECDH-ECDSA-AES256-SHA",
    "ECDH-RSA-AES256-SHA",
    "ECDHE-ECDSA-AES128-SHA",
    "ECDHE-RSA-AES128-SHA",
    "DHE-DSS-AES128-SHA",
    "ECDH-ECDSA-AES128-SHA",
    "ECDH-RSA-AES128-SHA"
  ]
```

配置 PSK 认证的监听器

```
ciphers = [
  [ "RSA-PSK-AES256-GCM-SHA384",
    "RSA-PSK-AES256-CBC-SHA384",
    "RSA-PSK-AES128-GCM-SHA256",
    "RSA-PSK-AES128-CBC-SHA256",
    "RSA-PSK-AES256-CBC-SHA",
    "RSA-PSK-AES128-CBC-SHA",
    "PSK-AES256-GCM-SHA384",
    "PSK-AES128-GCM-SHA256",
    "PSK-AES256-CBC-SHA384",
    "PSK-AES256-CBC-SHA",
    "PSK-AES128-CBC-SHA256",
    "PSK-AES128-CBC-SHA"
  ]
```

## emqx:Root Config Keys




**Fields**

- listeners: <code>[broker:listeners](#broker-listeners)</code>



- mqtt: <code>[broker:mqtt](#broker-mqtt)</code>

  全局的 MQTT 配置项。
  mqtt 下所有的配置作为全局的默认值存在，它可以被 <code>zone</code> 中的配置覆盖

- authentication: <code>[[authn:builtin_db](#authn-builtin_db) | [authn:mysql](#authn-mysql) | [authn:postgresql](#authn-postgresql) | [authn:mongo_single](#authn-mongo_single) | [authn:mongo_rs](#authn-mongo_rs) | [authn:mongo_sharded](#authn-mongo_sharded) | [authn:redis_single](#authn-redis_single) | [authn:redis_cluster](#authn-redis_cluster) | [authn:redis_sentinel](#authn-redis_sentinel) | [authn:http_get](#authn-http_get) | [authn:http_post](#authn-http_post) | [authn:jwt_hmac](#authn-jwt_hmac) | [authn:jwt_public_key](#authn-jwt_public_key) | [authn:jwt_jwks](#authn-jwt_jwks) | [authn:scram](#authn-scram) | [authn:ldap](#authn-ldap) | [authn:ldap_deprecated](#authn-ldap_deprecated) | [authn:gcp_device](#authn-gcp_device)]</code>
  * default: 
  `[]`

  默认的 MQTT 监听器的全局认证配置。

  有关每个监听器的单独配置，请参阅监听器配置中的<code>authentication</code>。

  此选项可配置为：
  <ul>
    <li><code>[]</code>: 默认值，允许 *所有* 登录</li>
    <li>one: 例如 <code>{enable:true,backend:"built_in_database",mechanism="password_based"}</code></li>
    <li>chain: 结构体数组。</li>
  </ul>

  当配置了一个认证链时，登录凭据将按照配置的顺序检查后端，直到可以做出'允许'或'拒绝'的决定。

  如果在完全遍历认证链之后没有决定，登录将被拒绝。

- authorization: <code>[emqx:authorization](#emqx-authorization)</code>

  授权（ACL）。EMQX 支持完整的客户端访问控制（ACL）。

- node: <code>[emqx:node](#emqx-node)</code>



- cluster: <code>[emqx:cluster](#emqx-cluster)</code>



- log: <code>[emqx:log](#emqx-log)</code>



- rpc: <code>[emqx:rpc](#emqx-rpc)</code>



- sys_topics: <code>[broker:sys_topics](#broker-sys_topics)</code>

  系统主题配置。

- force_shutdown: <code>[broker:force_shutdown](#broker-force_shutdown)</code>



- force_gc: <code>[broker:force_gc](#broker-force_gc)</code>



- sysmon: <code>[broker:sysmon](#broker-sysmon)</code>



- alarm: <code>[broker:alarm](#broker-alarm)</code>



- flapping_detect: <code>[broker:flapping_detect](#broker-flapping_detect)</code>



- bridges: <code>[bridge:bridges](#bridge-bridges)</code>



- connectors: <code>[connector:connectors](#connector-connectors)</code>



- actions: <code>[actions_and_sources:actions](#actions_and_sources-actions)</code>



- sources: <code>[actions_and_sources:sources](#actions_and_sources-sources)</code>



- retainer: <code>[retainer](#retainer)</code>



- delayed: <code>[modules:delayed](#modules-delayed)</code>



- plugins: <code>[plugin:plugins](#plugin-plugins)</code>



- dashboard: <code>[dashboard](#dashboard)</code>



- gateway: <code>[gateway](#gateway)</code>



- prometheus: <code>[prometheus:recommend_setting](#prometheus-recommend_setting) | [prometheus:legacy_deprecated_setting](#prometheus-legacy_deprecated_setting)</code>
  * default: 
  `{}`



- exhook: <code>[exhook](#exhook)</code>



- psk_authentication: <code>[psk:psk_authentication](#psk-psk_authentication)</code>



- slow_subs: <code>[slow_subs](#slow_subs)</code>



- opentelemetry: <code>[opentelemetry](#opentelemetry)</code>



- api_key: <code>[api_key](#api_key)</code>



- license: <code>[license:key_license](#license-key_license)</code>

  EMQX 企业版 License 。
  EMQX 自带一个默认的试用 License，允许最多接入 100 个连接，签发时间是 2023 年 1 月 9 日，有效期是 5 年（1825 天）。若需要在生产环境部署，
  请购买 License 或访问 https://www.emqx.com/apply-licenses/emqx 申请。

- schema_registry: <code>[schema_registry](#schema_registry)</code>



- file_transfer: <code>[emqx:file_transfer](#emqx-file_transfer)</code>




## api_key
API 密钥， 可用于请求除管理 API 密钥及 Dashboard 用户管理 API 的其它接口


**Config paths**

 - <code>api_key</code>


**Env overrides**

 - <code>EMQX_API_KEY</code>



**Fields**

- bootstrap_file: <code>binary()</code>
  * default: 
  `""`

  用于在启动 EMQX 时，添加 API 密钥，其格式为 {appid}:{secret}，多个密钥用换行分隔。：
        ```
        7e729ae70d23144b:2QILI9AcQ9BYlVqLDHQNWN2saIjBV4egr1CZneTNKr9CpK
        ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL
        ```


## broker:authz_cache
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
  `1m`

  缓存数据的生存时间。

- excludes: <code>[binary()]</code>
  * default: 
  `[]`

  Exclude caching ACL check results for topics matching the given patterns.


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

  警报激活时触发的动作。<br/>目前，支持以下操作：<code>log</code> 和 <code>publish</code>.
  <code>log</code> 将告警写入日志 (控制台或者文件).
  <code>publish</code> 将告警作为 MQTT 消息发布到系统主题:
  <code>$SYS/brokers/emqx@xx.xx.xx.x/alarms/activate</code> and
  <code>$SYS/brokers/emqx@xx.xx.xx.x/alarms/deactivate</code>

- size_limit: <code>1..3000</code>
  * default: 
  `1000`

  要保留为历史记录的已停用报警的最大总数。当超过此限制时，将删除最旧的停用报警，以限制总数。

- validity_period: <code>emqx_schema:duration()</code>
  * default: 
  `24h`

  停用报警的保留时间。报警在停用时不会立即删除，而是在保留时间之后删除。


## broker:deflate_opts
Compression options.


**Config paths**

 - <code>gateway.ocpp.listeners.ws.$name.websocket.deflate_opts</code>
 - <code>gateway.ocpp.listeners.wss.$name.websocket.deflate_opts</code>
 - <code>listeners.ws.$name.websocket.deflate_opts</code>
 - <code>listeners.wss.$name.websocket.deflate_opts</code>


**Env overrides**

 - <code>EMQX_GATEWAY__OCPP__LISTENERS__WS__$NAME__WEBSOCKET__DEFLATE_OPTS</code>
 - <code>EMQX_GATEWAY__OCPP__LISTENERS__WSS__$NAME__WEBSOCKET__DEFLATE_OPTS</code>
 - <code>EMQX_LISTENERS__WS__$NAME__WEBSOCKET__DEFLATE_OPTS</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__WEBSOCKET__DEFLATE_OPTS</code>



**Fields**

- level: <code>none | default | best_compression | best_speed</code>

  压缩级别

- mem_level: <code>1..9</code>
  * default: 
  `8`

  指定压缩状态的大小<br/>
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

  启用抖动检测功能。

- window_time: <code>emqx_schema:duration()</code>
  * default: 
  `1m`

  抖动检测的时间窗口。

- max_count: <code>non_neg_integer()</code>
  * default: 
  `15`

  MQTT 客户端在“窗口”时间内允许的最大断开次数

- ban_time: <code>emqx_schema:duration()</code>
  * default: 
  `5m`

  抖动的客户端将会被禁止登录多长时间。


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

  启用强制垃圾回收。

- count: <code>0..inf</code>
  * default: 
  `16000`

  在进程收到多少消息之后，对此进程执行垃圾回收。

- bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `16MB`

  在进程处理过多少个字节之后，对此进程执行垃圾回收。


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

  启用 `force_shutdown` 功能。

- max_mailbox_size: <code>0..inf</code>
  * default: 
  `1000`

  每个在线客户端在 EMQX 服务器中都是独立的一个进程。该配置可以设为单个进程的邮箱消息队列设置最大长度，当超过该上限时，客户端会被强制下线。

- max_heap_size: <code>emqx_schema:wordsize()</code>
  * default: 
  `32MB`

  Heap 的总大小。


## broker:listener_quic_ssl_opts
TLS options for QUIC transport.


**Config paths**

 - <code>listeners.quic.$name.ssl_options</code>


**Env overrides**

 - <code>EMQX_LISTENERS__QUIC__$NAME__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cacert.pem"`

  受信任的 PEM 格式 CA  证书捆绑文件<br/>
  此文件中的证书用于验证 TLS 对等方的证书。
  如果要信任新 CA，请将新证书附加到文件中。
  无需重启 EMQX 即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
  注意：从文件中失效（删除）证书不会影响已建立的连接。

- certfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cert.pem"`

  PEM 格式证书链文件<br/>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
  然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
  根 CA 证书是可选的，如果想要添加，应加到文件到最末端。

- keyfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/key.pem"`

  PEM 格式的私钥文件。

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  启用或禁用对等验证。

- password: <code>string()</code>

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。


## broker:listener_ssl_opts
Socket options for SSL connections.


**Config paths**

 - <code>gateway.exproto.listeners.ssl.$name.ssl_options</code>
 - <code>gateway.gbt32960.listeners.ssl.$name.ssl_options</code>
 - <code>gateway.stomp.listeners.ssl.$name.ssl_options</code>
 - <code>listeners.ssl.$name.ssl_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__SSL_OPTIONS</code>
 - <code>EMQX_GATEWAY__GBT32960__LISTENERS__SSL__$NAME__SSL_OPTIONS</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__SSL_OPTIONS</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cacert.pem"`

  受信任的 PEM 格式 CA  证书捆绑文件<br/>
  此文件中的证书用于验证 TLS 对等方的证书。
  如果要信任新 CA，请将新证书附加到文件中。
  无需重启 EMQX 即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
  注意：从文件中失效（删除）证书不会影响已建立的连接。

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cert.pem"`

  PEM 格式证书链文件<br/>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
  然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
  根 CA 证书是可选的，如果想要添加，应加到文件到最末端。

- keyfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/key.pem"`

  PEM 格式的私钥文件。

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  启用或禁用对等验证。

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  启用 TLS 会话重用。

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
  因此，如果深度为 0，则对等方必须由受信任的根 CA 直接签名；<br/>
  如果是 1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
  如果是 2，则路径可以是 PEER、中间 CA1、中间 CA2、ROOT-CA。

- password: <code>string()</code>

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

  支持所有 TLS/DTLS 版本<br/>
  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。

- ciphers: <code>[string()]</code>
  * default: 
  `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  <br/>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
  注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  <br/>
  注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
  如果打算使用 PSK 密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

  <br/>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。

- dhfile: <code>string()</code>

  如果协商使用 Diffie-Hellman 密钥交换的密码套件，则服务器将使用包含 PEM 编码的 Diffie-Hellman 参数的文件的路径。如果未指定，则使用默认参数。<br/>
  注意：TLS 1.3 不支持<code>dhfile</code>选项。

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

- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `15s`

  握手完成所允许的最长时间

- gc_after_handshake: <code>boolean()</code>
  * default: 
  `false`

  内存使用调优。如果启用，将在 TLS/SSL 握手完成后立即执行垃圾回收。TLS/SSL 握手建立后立即进行 GC。

- ocsp: <code>[broker:ocsp](#broker-ocsp)</code>



- enable_crl_check: <code>boolean()</code>
  * default: 
  `false`

  是否为该监听器启用 CRL 检查。


## broker:listener_wss_opts
Socket options for WebSocket/SSL connections.


**Config paths**

 - <code>gateway.ocpp.listeners.wss.$name.ssl_options</code>
 - <code>listeners.wss.$name.ssl_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__OCPP__LISTENERS__WSS__$NAME__SSL_OPTIONS</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cacert.pem"`

  受信任的 PEM 格式 CA  证书捆绑文件<br/>
  此文件中的证书用于验证 TLS 对等方的证书。
  如果要信任新 CA，请将新证书附加到文件中。
  无需重启 EMQX 即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
  注意：从文件中失效（删除）证书不会影响已建立的连接。

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cert.pem"`

  PEM 格式证书链文件<br/>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
  然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
  根 CA 证书是可选的，如果想要添加，应加到文件到最末端。

- keyfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/key.pem"`

  PEM 格式的私钥文件。

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  启用或禁用对等验证。

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  启用 TLS 会话重用。

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
  因此，如果深度为 0，则对等方必须由受信任的根 CA 直接签名；<br/>
  如果是 1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
  如果是 2，则路径可以是 PEER、中间 CA1、中间 CA2、ROOT-CA。

- password: <code>string()</code>

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

  支持所有 TLS/DTLS 版本<br/>
  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。

- ciphers: <code>[string()]</code>
  * default: 
  `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  <br/>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
  注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  <br/>
  注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
  如果打算使用 PSK 密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

  <br/>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。

- dhfile: <code>string()</code>

  如果协商使用 Diffie-Hellman 密钥交换的密码套件，则服务器将使用包含 PEM 编码的 Diffie-Hellman 参数的文件的路径。如果未指定，则使用默认参数。<br/>
  注意：TLS 1.3 不支持<code>dhfile</code>选项。

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

- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `15s`

  握手完成所允许的最长时间


## broker:listeners
MQTT listeners identified by their protocol type and assigned names


**Config paths**

 - <code>listeners</code>


**Env overrides**

 - <code>EMQX_LISTENERS</code>



**Fields**

- tcp: <code>{$name -> [broker:mqtt_tcp_listener](#broker-mqtt_tcp_listener) | marked_for_deletion}</code>

  TCP 监听器。

- ssl: <code>{$name -> [broker:mqtt_ssl_listener](#broker-mqtt_ssl_listener) | marked_for_deletion}</code>

  SSL 监听器。

- ws: <code>{$name -> [broker:mqtt_ws_listener](#broker-mqtt_ws_listener) | marked_for_deletion}</code>

  HTTP websocket 监听器。

- wss: <code>{$name -> [broker:mqtt_wss_listener](#broker-mqtt_wss_listener) | marked_for_deletion}</code>

  HTTPS websocket 监听器。

- quic: <code>{$name -> [broker:mqtt_quic_listener](#broker-mqtt_quic_listener) | marked_for_deletion}</code>

  QUIC 监听器。


## broker:mqtt
Global MQTT configuration.


**Config paths**

 - <code>mqtt</code>


**Env overrides**

 - <code>EMQX_MQTT</code>



**Fields**

- idle_timeout: <code>infinity | emqx_schema:duration()</code>
  * default: 
  `15s`

  设置连接被断开或进入休眠状态前的等待时间，空闲超时后，
    - 如暂未收到客户端的 CONNECT 报文，连接将断开；
    - 如已收到客户端的 CONNECT 报文，连接将进入休眠模式以节省系统资源。

  注意：请合理设置该参数值，如等待时间设置过长，可能造成系统资源的浪费。

- max_packet_size: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

  允许的最大 MQTT 报文大小。

- max_clientid_len: <code>23..65535</code>
  * default: 
  `65535`

  允许的最大 MQTT Client ID 长度。

- max_topic_levels: <code>1..65535</code>
  * default: 
  `128`

  允许的最大主题层级。

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

- shared_subscription_strategy: <code>random | round_robin | round_robin_per_group | sticky | local | hash_topic | hash_clientid</code>
  * default: 
  `round_robin`

  共享订阅消息派发策略。
    - `random`：随机选择一个订阅者派发；
    - `round_robin`：单个发布者的消息轮流派发给订阅者；
    - `round_robin_per_group`：所有消息轮流派发给订阅者；
    - `local`：随机选择当前节点上的订阅者，如果当前节点没有订阅者则在集群范围内随机选择;
    - `sticky`：持续向初次选中的订阅者派发消息，直至其结束会话;
    - `hash_clientid`：对发布者客户端 ID 进行 Hash 处理以选择订阅者;
    - `hash_topic`：对发布主题进行 Hash 处理以选择订阅者。

- exclusive_subscription: <code>boolean()</code>
  * default: 
  `false`

  是否启用对 MQTT 排它订阅的支持。

- ignore_loop_deliver: <code>boolean()</code>
  * default: 
  `false`

  设置由 MQTT v3.1.1/v3.1.0 客户端发布的消息是否将转发给其本身；类似 MQTT 5.0 协议中的 <code>No Local</code> 选项。

- strict_mode: <code>boolean()</code>
  * default: 
  `false`

  是否以严格模式解析 MQTT 消息。
  严格模式下，如客户端 ID、主题名称等中包含无效 utf8 字符串，连接将被断开。

- response_information: <code>string()</code>
  * default: 
  `""`

  UTF-8 字符串，用于指定返回给客户端的响应主题，如 <code>reqrsp/</code>，此时请求和应答客户端都需要使用 <code>reqrsp/</code> 前缀的主题来完成通讯。
  如希望禁用此功能，请在下方的文字框中输入<code>""</code>；仅适用于 MQTT 5.0 客户端。

- server_keepalive: <code>pos_integer() | disabled</code>
  * default: 
  `disabled`

  EMQX 要求的保活时间，如设为 disabled，则将使用客户端指定的保持连接时间；仅适用于 MQTT 5.0 客户端。

- keepalive_multiplier: <code>number()</code>
  * default: 
  `1.5`

  EMQX 判定客户端 Keep Alive 超时使用的 Keep Alive 倍数。计算公式为：Keep Alive 超时 = Keep Alive 间隔 × Keep Alive 倍数。 默认值 1.5 遵循 MQTT 5.0 规范。此倍数可调整，为系统管理员提供根据特定需求进行定制的灵活性。例如，如果客户端的 10 秒保持连接间隔的 PINGREQ 因为额外的 10 秒延迟，将倍数更改为 2 可以让 EMQX 容忍此延迟。

- retry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `30s`

  QoS 1/2 消息的重新投递间隔。

- use_username_as_clientid: <code>boolean()</code>
  * default: 
  `false`

  是否使用用户名作为客户端 ID。
  此设置的作用时间晚于 <code>对端证书作为用户名</code> 和 <code>对端证书作为客户端 ID</code>。

- peer_cert_as_username: <code>disabled | cn | dn | crt | pem | md5</code>
  * default: 
  `disabled`

  使用对端证书中的 CN、DN 字段或整个证书内容来作为用户名；仅适用于 TLS 连接。
  目前支持：
  - <code>cn</code>: 取证书的 CN 字段
  - <code>dn</code>: 取证书的 DN 字段
  - <code>crt</code>: 取 <code>DER</code> 或 <code>PEM</code> 的证书内容
  - <code>pem</code>: 将 <code>DER</code> 证书转换为 <code>PEM</code> 格式作为用户名
  - <code>md5</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书内容的 MD5 值

- peer_cert_as_clientid: <code>disabled | cn | dn | crt | pem | md5</code>
  * default: 
  `disabled`

  使用对端证书中的 CN、DN 字段或整个证书内容来作为客户端 ID。仅适用于 TLS 连接；
  目前支持：
  - <code>cn</code>: 取证书的 CN 字段
  - <code>dn</code>: 取证书的 DN 字段
  - <code>crt</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书的内容
  - <code>pem</code>: 将 <code>DER</code> 证书内容转换为 <code>PEM</code> 格式作为客户端 ID
  - <code>md5</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书内容的 MD5 值

- session_expiry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `2h`

  指定会话将在连接断开后多久过期，仅适用于非 MQTT 5.0 的连接。

- max_awaiting_rel: <code>non_neg_integer() | infinity</code>
  * default: 
  `100`

  每个发布者的会话中，都存在一个队列来处理客户端发送的 QoS 2 消息。该队列会存储 QoS 2 消息的报文 ID 直到收到客户端的 PUBREL 或超时，达到队列长度的限制后，新的 QoS 2 消息发布会被拒绝，并返回 `147(0x93)` 错误。

- max_qos_allowed: <code>qos()</code>
  * default: 
  `2`

  允许的最大 QoS 等级。

- mqueue_priorities: <code>disabled | map()</code>
  * default: 
  `disabled`

  主题优先级。取值范围 [1-255]
  默认优先级表为空，即所有的主题优先级相同。

  注：优先主题名称中不支持使用逗号和等号。
  注：不在此列表中的主题，被视为最高/最低优先级，这取决于<code>mqtt.mqueue_default_priority</code> 的配置。

  示例：
  配置 <code>"topic/1" > "topic/2"</code>:
  <code>mqueue_priorities: {"topic/1": 10, "topic/2": 8}</code>

- mqueue_default_priority: <code>highest | lowest</code>
  * default: 
  `lowest`

  默认的主题优先级，不在 <code>主题优先级</code>（<code>mqueue_priorities</code>） 中的主题将会使用该优先级。

- mqueue_store_qos0: <code>boolean()</code>
  * default: 
  `true`

  指定在连接断开但会话保持期间，是否需要在消息队列中存储 QoS 0 消息。

- max_mqueue_len: <code>non_neg_integer() | infinity</code>
  * default: 
  `1000`

  消息队列最大长度。持久客户端断开连接或飞行窗口已满时排队的消息长度。

- max_inflight: <code>1..65535</code>
  * default: 
  `32`

  允许在完成应答前同时投递的 QoS 1 和 QoS 2 消息的最大数量。

- max_subscriptions: <code>1..inf | infinity</code>
  * default: 
  `infinity`

  允许每个客户端建立的最大订阅数量。

- upgrade_qos: <code>boolean()</code>
  * default: 
  `false`

  投递消息时，是否根据订阅主题时的 QoS 等级来强制提升派发的消息的 QoS 等级。

- await_rel_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `300s`

  客户端发布 QoS 2 消息时，服务器等待 `PUBREL` 的最长时延。超过该时长后服务器会放弃等待，该 PACKET ID 会被释放，从而允许后续新的 PUBLISH 消息使用。如果超时后收到 PUBREL，服务器将会产生一条告警日志。注意，向订阅客户端转发消息的动作发生在进入等待之前。


## broker:mqtt_quic_listener
Settings for the MQTT over QUIC listener.


**Config paths**

 - <code>listeners.quic.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__QUIC__$NAME</code>



**Fields**

- ciphers: <code>[string()]</code>
  * default: 
  `[TLS_AES_256_GCM_SHA384, TLS_AES_128_GCM_SHA256, TLS_CHACHA20_POLY1305_SHA256]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  <br/>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式。<br/>
  注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  <br/>
  注：PSK 的 Ciphers 不支持 tlsv1.3。<br/>
  如果打算使用 PSK 密码套件，<code>tlsv1.3</code> 应在 <code>ssl.versions</code> 中禁用。

  <br/>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code><br/>

  注：QUIC 监听器只支持 tlsv1.3 的 ciphers。

- ssl_options: <code>[broker:listener_quic_ssl_opts](#broker-listener_quic_ssl_opts)</code>

  QUIC 传输层的 TLS 选项

- enable: <code>boolean()</code>
  * default: 
  `true`

  启停监听器。

- bind: <code>emqx_schema:ip_port()</code>
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

  例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。<br/>
  类似地，如果另一个客户端 B（与客户端 A 连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息<br/>

  设置为<code>""</code> 以禁用该功能<br/>

  mountpoint 字符串中的变量：
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username

- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
  配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
  配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
  客户直接拒绝，不做使用任何认证器对客户端进行身份检查。

- max_conn_rate: <code>string()</code>

  最大连接速率。<br/>
  这用于限制该节点的连接速率。
  一旦达到限制，新的连接将被推迟或拒绝。<br/>
  例如：<br/>
  - <code>1000/s</code>：每秒只接受1000个连接<br/>
  - <code>1000/10s</code>：每10秒只接受1000个连接。

- messages_rate: <code>string()</code>

  消息发布速率。<br/>
  这用于限制该节点的入站消息数量。
  一旦达到限制，受限制的客户端将减速甚至暂时挂起。<br/>
  例如：<br/>
  - <code>500/s</code>：每秒只发送前500条消息，其他消息被缓冲。<br/>
  - <code>500/10s</code>：即使是10秒，也只发送前500条消息，其他消息被缓冲。

- bytes_rate: <code>string()</code>

  数据发布速率。<br/>
  这用于限制该节点的入站字节速率。
  一旦达到限制，受限制的客户端将减速甚至暂时挂起。<br/>
  字节的单位可以是：KB MB GB。<br/>
  例如：<br/>
  - <code>500KB/s</code>：每秒只发送前500千字节，其他消息被缓冲。<br/>
  - <code>500MB/10s</code>：即使是10秒，也只发送前500兆字节，其他消息被缓冲。


## broker:mqtt_ssl_listener
Settings for the MQTT over SSL listener.


**Config paths**

 - <code>listeners.ssl.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__SSL__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启停监听器。

- bind: <code>emqx_schema:ip_port()</code>
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

  例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。<br/>
  类似地，如果另一个客户端 B（与客户端 A 连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息<br/>

  设置为<code>""</code> 以禁用该功能<br/>

  mountpoint 字符串中的变量：
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username

- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
  配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
  配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
  客户直接拒绝，不做使用任何认证器对客户端进行身份检查。

- max_conn_rate: <code>string()</code>

  最大连接速率。<br/>
  这用于限制该节点的连接速率。
  一旦达到限制，新的连接将被推迟或拒绝。<br/>
  例如：<br/>
  - <code>1000/s</code>：每秒只接受1000个连接<br/>
  - <code>1000/10s</code>：每10秒只接受1000个连接。

- messages_rate: <code>string()</code>

  消息发布速率。<br/>
  这用于限制该节点的入站消息数量。
  一旦达到限制，受限制的客户端将减速甚至暂时挂起。<br/>
  例如：<br/>
  - <code>500/s</code>：每秒只发送前500条消息，其他消息被缓冲。<br/>
  - <code>500/10s</code>：即使是10秒，也只发送前500条消息，其他消息被缓冲。

- bytes_rate: <code>string()</code>

  数据发布速率。<br/>
  这用于限制该节点的入站字节速率。
  一旦达到限制，受限制的客户端将减速甚至暂时挂起。<br/>
  字节的单位可以是：KB MB GB。<br/>
  例如：<br/>
  - <code>500KB/s</code>：每秒只发送前500千字节，其他消息被缓冲。<br/>
  - <code>500MB/10s</code>：即使是10秒，也只发送前500兆字节，其他消息被缓冲。

- access_rules: <code>[string()]</code>
  * default: 
  `["allow all"]`

  此监听器的访问控制规则。

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`

  如果 EMQX 集群部署在 HAProxy 或 Nginx 之后，请启用代理协议 V1/2 <br/>
  详情见: https://www.haproxy.com/blog/haproxy/proxy-protocol/

- proxy_protocol_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `3s`

  代理协议超时。如果在超时时间内未收到代理协议数据包，EMQX 将关闭 TCP 连接。

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>



- ssl_options: <code>[broker:listener_ssl_opts](#broker-listener_ssl_opts)</code>




## broker:mqtt_tcp_listener
Settings for the MQTT over TCP listener.


**Config paths**

 - <code>listeners.tcp.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__TCP__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启停监听器。

- bind: <code>emqx_schema:ip_port()</code>
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

  例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。<br/>
  类似地，如果另一个客户端 B（与客户端 A 连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息<br/>

  设置为<code>""</code> 以禁用该功能<br/>

  mountpoint 字符串中的变量：
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username

- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
  配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
  配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
  客户直接拒绝，不做使用任何认证器对客户端进行身份检查。

- max_conn_rate: <code>string()</code>

  最大连接速率。<br/>
  这用于限制该节点的连接速率。
  一旦达到限制，新的连接将被推迟或拒绝。<br/>
  例如：<br/>
  - <code>1000/s</code>：每秒只接受1000个连接<br/>
  - <code>1000/10s</code>：每10秒只接受1000个连接。

- messages_rate: <code>string()</code>

  消息发布速率。<br/>
  这用于限制该节点的入站消息数量。
  一旦达到限制，受限制的客户端将减速甚至暂时挂起。<br/>
  例如：<br/>
  - <code>500/s</code>：每秒只发送前500条消息，其他消息被缓冲。<br/>
  - <code>500/10s</code>：即使是10秒，也只发送前500条消息，其他消息被缓冲。

- bytes_rate: <code>string()</code>

  数据发布速率。<br/>
  这用于限制该节点的入站字节速率。
  一旦达到限制，受限制的客户端将减速甚至暂时挂起。<br/>
  字节的单位可以是：KB MB GB。<br/>
  例如：<br/>
  - <code>500KB/s</code>：每秒只发送前500千字节，其他消息被缓冲。<br/>
  - <code>500MB/10s</code>：即使是10秒，也只发送前500兆字节，其他消息被缓冲。

- access_rules: <code>[string()]</code>
  * default: 
  `["allow all"]`

  此监听器的访问控制规则。

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`

  如果 EMQX 集群部署在 HAProxy 或 Nginx 之后，请启用代理协议 V1/2 <br/>
  详情见: https://www.haproxy.com/blog/haproxy/proxy-protocol/

- proxy_protocol_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `3s`

  代理协议超时。如果在超时时间内未收到代理协议数据包，EMQX 将关闭 TCP 连接。

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>




## broker:mqtt_ws_listener
Settings for the MQTT over WebSocket listener.


**Config paths**

 - <code>listeners.ws.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WS__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启停监听器。

- bind: <code>emqx_schema:ip_port()</code>
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

  例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。<br/>
  类似地，如果另一个客户端 B（与客户端 A 连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息<br/>

  设置为<code>""</code> 以禁用该功能<br/>

  mountpoint 字符串中的变量：
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username

- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
  配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
  配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
  客户直接拒绝，不做使用任何认证器对客户端进行身份检查。

- max_conn_rate: <code>string()</code>

  最大连接速率。<br/>
  这用于限制该节点的连接速率。
  一旦达到限制，新的连接将被推迟或拒绝。<br/>
  例如：<br/>
  - <code>1000/s</code>：每秒只接受1000个连接<br/>
  - <code>1000/10s</code>：每10秒只接受1000个连接。

- messages_rate: <code>string()</code>

  消息发布速率。<br/>
  这用于限制该节点的入站消息数量。
  一旦达到限制，受限制的客户端将减速甚至暂时挂起。<br/>
  例如：<br/>
  - <code>500/s</code>：每秒只发送前500条消息，其他消息被缓冲。<br/>
  - <code>500/10s</code>：即使是10秒，也只发送前500条消息，其他消息被缓冲。

- bytes_rate: <code>string()</code>

  数据发布速率。<br/>
  这用于限制该节点的入站字节速率。
  一旦达到限制，受限制的客户端将减速甚至暂时挂起。<br/>
  字节的单位可以是：KB MB GB。<br/>
  例如：<br/>
  - <code>500KB/s</code>：每秒只发送前500千字节，其他消息被缓冲。<br/>
  - <code>500MB/10s</code>：即使是10秒，也只发送前500兆字节，其他消息被缓冲。

- access_rules: <code>[string()]</code>
  * default: 
  `["allow all"]`

  此监听器的访问控制规则。

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`

  如果 EMQX 集群部署在 HAProxy 或 Nginx 之后，请启用代理协议 V1/2 <br/>
  详情见: https://www.haproxy.com/blog/haproxy/proxy-protocol/

- proxy_protocol_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `3s`

  代理协议超时。如果在超时时间内未收到代理协议数据包，EMQX 将关闭 TCP 连接。

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>



- websocket: <code>[broker:ws_opts](#broker-ws_opts)</code>




## broker:mqtt_wss_listener
Settings for the MQTT over WebSocket/SSL listener.


**Config paths**

 - <code>listeners.wss.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WSS__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启停监听器。

- bind: <code>emqx_schema:ip_port()</code>
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

  例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。<br/>
  类似地，如果另一个客户端 B（与客户端 A 连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息<br/>

  设置为<code>""</code> 以禁用该功能<br/>

  mountpoint 字符串中的变量：
  - <code>${clientid}</code>: clientid
  - <code>${username}</code>: username

- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
  配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
  配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
  客户直接拒绝，不做使用任何认证器对客户端进行身份检查。

- max_conn_rate: <code>string()</code>

  最大连接速率。<br/>
  这用于限制该节点的连接速率。
  一旦达到限制，新的连接将被推迟或拒绝。<br/>
  例如：<br/>
  - <code>1000/s</code>：每秒只接受1000个连接<br/>
  - <code>1000/10s</code>：每10秒只接受1000个连接。

- messages_rate: <code>string()</code>

  消息发布速率。<br/>
  这用于限制该节点的入站消息数量。
  一旦达到限制，受限制的客户端将减速甚至暂时挂起。<br/>
  例如：<br/>
  - <code>500/s</code>：每秒只发送前500条消息，其他消息被缓冲。<br/>
  - <code>500/10s</code>：即使是10秒，也只发送前500条消息，其他消息被缓冲。

- bytes_rate: <code>string()</code>

  数据发布速率。<br/>
  这用于限制该节点的入站字节速率。
  一旦达到限制，受限制的客户端将减速甚至暂时挂起。<br/>
  字节的单位可以是：KB MB GB。<br/>
  例如：<br/>
  - <code>500KB/s</code>：每秒只发送前500千字节，其他消息被缓冲。<br/>
  - <code>500MB/10s</code>：即使是10秒，也只发送前500兆字节，其他消息被缓冲。

- access_rules: <code>[string()]</code>
  * default: 
  `["allow all"]`

  此监听器的访问控制规则。

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`

  如果 EMQX 集群部署在 HAProxy 或 Nginx 之后，请启用代理协议 V1/2 <br/>
  详情见: https://www.haproxy.com/blog/haproxy/proxy-protocol/

- proxy_protocol_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `3s`

  代理协议超时。如果在超时时间内未收到代理协议数据包，EMQX 将关闭 TCP 连接。

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>



- ssl_options: <code>[broker:listener_wss_opts](#broker-listener_wss_opts)</code>



- websocket: <code>[broker:ws_opts](#broker-ws_opts)</code>




## broker:ocsp
Per listener OCSP Stapling configuration.


**Config paths**

 - <code>gateway.coap.listeners.dtls.$name.dtls_options.ocsp</code>
 - <code>gateway.exproto.listeners.dtls.$name.dtls_options.ocsp</code>
 - <code>gateway.exproto.listeners.ssl.$name.ssl_options.ocsp</code>
 - <code>gateway.gbt32960.listeners.ssl.$name.ssl_options.ocsp</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.dtls_options.ocsp</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.dtls_options.ocsp</code>
 - <code>gateway.stomp.listeners.ssl.$name.ssl_options.ocsp</code>
 - <code>listeners.ssl.$name.ssl_options.ocsp</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__DTLS_OPTIONS__OCSP</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__DTLS_OPTIONS__OCSP</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__SSL_OPTIONS__OCSP</code>
 - <code>EMQX_GATEWAY__GBT32960__LISTENERS__SSL__$NAME__SSL_OPTIONS__OCSP</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__DTLS_OPTIONS__OCSP</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__DTLS_OPTIONS__OCSP</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__SSL_OPTIONS__OCSP</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__SSL_OPTIONS__OCSP</code>



**Fields**

- enable_ocsp_stapling: <code>boolean()</code>
  * default: 
  `false`

  是否为监听器启用 OCSP Stapling 功能。 如果设置为 true，需要定义 OCSP Responder 的 URL 和证书签发者的 PEM 文件路径。

- responder_url: <code>emqx_schema:url()</code>

  用于检查服务器证书的 OCSP Responder 的 URL。

- issuer_pem: <code>binary()</code>

  服务器证书的 OCSP 签发者的 PEM 编码证书。

- refresh_interval: <code>emqx_schema:duration()</code>
  * default: 
  `5m`

  为服务器刷新 OCSP 响应的周期。

- refresh_http_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `15s`

  检查 OCSP 响应时，HTTP 请求的超时。


## broker:ssl_client_opts
Socket options for SSL clients.


**Config paths**

 - <code>authentication.$INDEX.ssl</code>
 - <code>authorization.sources.$INDEX.ssl</code>
 - <code>bridges.cassandra.$name.ssl</code>
 - <code>bridges.greptimedb.$name.ssl</code>
 - <code>bridges.hstreamdb.$name.ssl</code>
 - <code>bridges.influxdb_api_v1.$name.ssl</code>
 - <code>bridges.influxdb_api_v2.$name.ssl</code>
 - <code>bridges.iotdb.$name.ssl</code>
 - <code>bridges.matrix.$name.ssl</code>
 - <code>bridges.mongodb_rs.$name.ssl</code>
 - <code>bridges.mongodb_sharded.$name.ssl</code>
 - <code>bridges.mongodb_single.$name.ssl</code>
 - <code>bridges.mqtt.$name.ssl</code>
 - <code>bridges.mysql.$name.ssl</code>
 - <code>bridges.pgsql.$name.ssl</code>
 - <code>bridges.pulsar_producer.$name.ssl</code>
 - <code>bridges.rabbitmq.$name.ssl</code>
 - <code>bridges.redis_cluster.$name.ssl</code>
 - <code>bridges.redis_sentinel.$name.ssl</code>
 - <code>bridges.redis_single.$name.ssl</code>
 - <code>bridges.timescale.$name.ssl</code>
 - <code>bridges.webhook.$name.ssl</code>
 - <code>cluster.etcd.ssl_options</code>
 - <code>connectors.elasticsearch.$name.ssl</code>
 - <code>connectors.http.$name.ssl</code>
 - <code>connectors.influxdb.$name.ssl</code>
 - <code>connectors.iotdb.$name.ssl</code>
 - <code>connectors.matrix.$name.ssl</code>
 - <code>connectors.mongodb.$name.ssl</code>
 - <code>connectors.mqtt.$name.ssl</code>
 - <code>connectors.mysql.$name.ssl</code>
 - <code>connectors.pgsql.$name.ssl</code>
 - <code>connectors.redis.$name.ssl</code>
 - <code>connectors.timescale.$name.ssl</code>
 - <code>file_transfer.storage.local.exporter.s3.transport_options.ssl</code>
 - <code>gateway.exproto.handler.ssl_options</code>
 - <code>opentelemetry.exporter.ssl_options</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX__SSL</code>
 - <code>EMQX_BRIDGES__CASSANDRA__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__GREPTIMEDB__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__HSTREAMDB__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__INFLUXDB_API_V1__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__INFLUXDB_API_V2__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__IOTDB__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__MATRIX__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__MONGODB_RS__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__MONGODB_SHARDED__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__MONGODB_SINGLE__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__MQTT__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__MYSQL__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__PGSQL__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__PULSAR_PRODUCER__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__RABBITMQ__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__REDIS_CLUSTER__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__REDIS_SENTINEL__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__REDIS_SINGLE__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__TIMESCALE__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__WEBHOOK__$NAME__SSL</code>
 - <code>EMQX_CLUSTER__ETCD__SSL_OPTIONS</code>
 - <code>EMQX_CONNECTORS__ELASTICSEARCH__$NAME__SSL</code>
 - <code>EMQX_CONNECTORS__HTTP__$NAME__SSL</code>
 - <code>EMQX_CONNECTORS__INFLUXDB__$NAME__SSL</code>
 - <code>EMQX_CONNECTORS__IOTDB__$NAME__SSL</code>
 - <code>EMQX_CONNECTORS__MATRIX__$NAME__SSL</code>
 - <code>EMQX_CONNECTORS__MONGODB__$NAME__SSL</code>
 - <code>EMQX_CONNECTORS__MQTT__$NAME__SSL</code>
 - <code>EMQX_CONNECTORS__MYSQL__$NAME__SSL</code>
 - <code>EMQX_CONNECTORS__PGSQL__$NAME__SSL</code>
 - <code>EMQX_CONNECTORS__REDIS__$NAME__SSL</code>
 - <code>EMQX_CONNECTORS__TIMESCALE__$NAME__SSL</code>
 - <code>EMQX_FILE_TRANSFER__STORAGE__LOCAL__EXPORTER__S3__TRANSPORT_OPTIONS__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__HANDLER__SSL_OPTIONS</code>
 - <code>EMQX_OPENTELEMETRY__EXPORTER__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>

  受信任的 PEM 格式 CA  证书捆绑文件<br/>
  此文件中的证书用于验证 TLS 对等方的证书。
  如果要信任新 CA，请将新证书附加到文件中。
  无需重启 EMQX 即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
  注意：从文件中失效（删除）证书不会影响已建立的连接。

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>

  PEM 格式证书链文件<br/>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
  然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
  根 CA 证书是可选的，如果想要添加，应加到文件到最末端。

- keyfile: <code>binary()</code>

  PEM 格式的私钥文件。

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  启用或禁用对等验证。

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  启用 TLS 会话重用。

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
  因此，如果深度为 0，则对等方必须由受信任的根 CA 直接签名；<br/>
  如果是 1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
  如果是 2，则路径可以是 PEER、中间 CA1、中间 CA2、ROOT-CA。

- password: <code>string()</code>

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

  支持所有 TLS/DTLS 版本<br/>
  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。

- ciphers: <code>[string()]</code>
  * default: 
  `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  <br/>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
  注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  <br/>
  注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
  如果打算使用 PSK 密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

  <br/>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。

- enable: <code>boolean()</code>
  * default: 
  `false`

  启用 TLS。

- server_name_indication: <code>disable | string()</code>

  指定要在 TLS 服务器名称指示扩展中使用的主机名。<br/>
  例如，当连接到 "server.example.net" 时，接受连接并执行 TLS 握手的真正服务器可能与 TLS 客户端最初连接到的主机不同，
  例如，当连接到 IP 地址时，或者当主机具有多个可解析的 DNS 记录时<br/>
  如果未指定，它将默认为使用的主机名字符串
  建立连接，除非使用 IP 地址<br/>
  然后，主机名也用于对等机的主机名验证证书<br/>
  特殊值 <code>disable</code> 阻止发送服务器名称指示扩展，并禁用主机名验证检查。


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
  `1m`

  发送 `$SYS` 主题的间隔时间。

- sys_heartbeat_interval: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `30s`

  发送心跳系统消息的间隔时间，它包括：
    - `$SYS/brokers/<node>/uptime`
    - `$SYS/brokers/<node>/datetime`

- sys_event_messages: <code>[broker:event_names](#broker-event_names)</code>

  客户端事件消息。


## broker:sysmon
Features related to system monitoring and introspection.


**Config paths**

 - <code>sysmon</code>


**Env overrides**

 - <code>EMQX_SYSMON</code>



**Fields**

- vm: <code>[broker:sysmon_vm](#broker-sysmon_vm)</code>



- os: <code>[broker:sysmon_os](#broker-sysmon_os)</code>




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
  `60s`

  定期 CPU 检查的时间间隔。

- cpu_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `80%`

  在发出相应警报之前可以使用多少系统 CPU 的阈值，以系统 CPU 负载的百分比表示。

- cpu_low_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `60%`

  在解除相应警报之前可以使用多少系统 CPU 的阈值，以系统 CPU 负载的百分比表示。

- mem_check_interval: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `60s`

  定期内存检查的时间间隔。

- sysmem_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `70%`

  在发出相应报警之前可以分配多少系统内存的阈值，以系统内存的百分比表示。

- procmem_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `5%`

  在发出相应警报之前，一个 Erlang 进程可以分配多少系统内存的阈值，以系统内存的百分比表示。


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
  `30s`

  定期进程限制检查的时间间隔。

- process_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `80%`

  在发出相应警报之前，本地节点上可以同时存在多少进程的阈值（以进程百分比表示）。

- process_low_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `60%`

  在清除相应警报之前，本地节点上可以同时存在多少进程的阈值（以进程百分比表示）。

- long_gc: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `disabled`

  当系统检测到某个 Erlang 进程垃圾回收占用过长时间，会触发一条带有 <code>long_gc</code> 关键字的日志。
  同时还会发布一条主题为 <code>$SYS/sysmon/long_gc</code> 的 MQTT 系统消息。

- long_schedule: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `240ms`

  启用后，如果 Erlang VM 调度器出现某个任务占用时间过长时，会触发一条带有 'long_schedule' 关键字的日志。
  同时还会发布一条主题为 <code>$SYS/sysmon/long_schedule</code> 的 MQTT 系统消息。

- large_heap: <code>disabled | emqx_schema:bytesize()</code>
  * default: 
  `32MB`

  启用后，当一个 Erlang 进程申请了大量内存，系统会触发一条带有 <code>large_heap</code> 关键字的
  warning 级别日志。同时还会发布一条主题为 <code>$SYS/sysmon/busy_dist_port</code> 的 MQTT 系统消息。

- busy_dist_port: <code>boolean()</code>
  * default: 
  `true`

  启用后，当用于集群接点之间 RPC 的连接过忙时，会触发一条带有 <code>busy_dist_port</code> 关键字的 warning 级别日志。
  同时还会发布一条主题为 <code>$SYS/sysmon/busy_dist_port</code> 的 MQTT 系统消息。

- busy_port: <code>boolean()</code>
  * default: 
  `true`

  当一个系统接口（例如 TCP socket）过忙，会触发一条带有 <code>busy_port</code> 关键字的 warning 级别的日志。
  同时还会发布一条主题为 <code>$SYS/sysmon/busy_port</code> 的 MQTT 系统消息。


## broker:tcp_opts
TCP listener options.


**Config paths**

 - <code>gateway.exproto.listeners.ssl.$name.tcp_options</code>
 - <code>gateway.exproto.listeners.tcp.$name.tcp_options</code>
 - <code>gateway.gbt32960.listeners.ssl.$name.tcp_options</code>
 - <code>gateway.gbt32960.listeners.tcp.$name.tcp_options</code>
 - <code>gateway.ocpp.listeners.ws.$name.tcp_options</code>
 - <code>gateway.ocpp.listeners.wss.$name.tcp_options</code>
 - <code>gateway.stomp.listeners.ssl.$name.tcp_options</code>
 - <code>gateway.stomp.listeners.tcp.$name.tcp_options</code>
 - <code>listeners.ssl.$name.tcp_options</code>
 - <code>listeners.tcp.$name.tcp_options</code>
 - <code>listeners.ws.$name.tcp_options</code>
 - <code>listeners.wss.$name.tcp_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_GATEWAY__GBT32960__LISTENERS__SSL__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_GATEWAY__GBT32960__LISTENERS__TCP__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_GATEWAY__OCPP__LISTENERS__WS__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_GATEWAY__OCPP__LISTENERS__WSS__$NAME__TCP_OPTIONS</code>
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

  为此套接字指定{active，N}选项<br/>
  See: https://erlang.org/doc/man/inet.html#setopts-2

- backlog: <code>pos_integer()</code>
  * default: 
  `1024`

  TCP backlog 定义了挂起连接队列可以增长到的最大长度。

- send_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `15s`

  连接的 TCP 发送超时。

- send_timeout_close: <code>boolean()</code>
  * default: 
  `true`

  如果发送超时，则关闭连接。

- recbuf: <code>emqx_schema:bytesize()</code>

  连接的 TCP 接收缓冲区（OS 内核）。

- sndbuf: <code>emqx_schema:bytesize()</code>

  连接的 TCP 发送缓冲区（OS 内核）。

- buffer: <code>emqx_schema:bytesize()</code>
  * default: 
  `4KB`

  驱动程序使用的用户空间缓冲区的大小。

- high_watermark: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

  当 VM 套接字实现内部排队的数据量达到此限制时，套接字将设置为忙碌状态。

- nodelay: <code>boolean()</code>
  * default: 
  `true`

  连接的 TCP_NODELAY 标识

- reuseaddr: <code>boolean()</code>
  * default: 
  `true`

  连接的 SO_REUSEADDR 标识。

- keepalive: <code>string()</code>
  * default: 
  `none`

  为 MQTT 连接在 TCP 或 SSL 上启用 TCP 保活。
  值是以逗号分隔的三个数字，格式为 'Idle,Interval,Probes'
   - Idle: 在服务器开始发送保活探测之前，连接需要处于空闲状态的秒数（Linux 默认为 7200）。
   - Interval: TCP 保活探测间隔的秒数（Linux 默认值为 75）。
   - Probes: 在放弃并终止连接之前，从另一端未获得响应时要发送的 TCP 保活探测的最大数量（Linux 默认值为 9 次）。
  例如 "240,30,5" 表示：在连接空闲 240 秒后发送 TCP 保活探测，每隔 30 秒发送一次，直到收到响应，如果连续丢失 5 个响应，连接应该被关闭。
  默认值为 'none'


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

  WebSocket 的 MQTT 协议路径。因此，EMQX Broker 的 WebSocket 地址为：
  <code>ws://{ip}:{port}/mqtt</code>

- mqtt_piggyback: <code>single | multiple</code>
  * default: 
  `multiple`

  WebSocket 消息是否允许包含多个 MQTT 数据包。

- compress: <code>boolean()</code>
  * default: 
  `false`

  如果 <code>true</code>，则使用 <code>zlib</code> 压缩 WebSocket 消息<br/>
  <code>deflate_opts</code> 下的配置项属于压缩相关参数配置。

- idle_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `7200s`

  关闭在此间隔内未发送 MQTT CONNECT 消息的客户端的传输层连接。

- max_frame_size: <code>infinity | integer()</code>
  * default: 
  `infinity`

  单个 MQTT 数据包的最大长度。

- fail_if_no_subprotocol: <code>boolean()</code>
  * default: 
  `true`

  如果<code>true</code>，当客户端未携带<code>Sec WebSocket Protocol</code>字段时，服务器将返回一个错误。
  <br/>注意：微信小程序需要禁用此验证。

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

  如果设置为 <code>false</code> 并且 <code>check_origin_enable</code> 为 <code>true</code>，服务器将拒绝没有 <code>origin</code> HTTP 头的请求。

- check_origins: <code>emqx_schema:comma_separated_binary()</code>
  * default: 
  `"http://localhost:18083, http://127.0.0.1:18083"`

  允许的 origins 列表

- proxy_address_header: <code>string()</code>
  * default: 
  `x-forwarded-for`

  HTTP 头，用于传递有关客户端 IP 地址的信息。
  当 EMQX 集群部署在负载平衡器后面时，这一点非常重要。

- proxy_port_header: <code>string()</code>
  * default: 
  `x-forwarded-port`

  HTTP 头，用于传递有关客户端端口的信息。当 EMQX 集群部署在负载平衡器后面时，这一点非常重要。

- deflate_opts: <code>[broker:deflate_opts](#broker-deflate_opts)</code>




## connector_influxdb:connector_influxdb_api_v1
InfluxDB HTTP API 协议。支持 Influxdb v1.8 以及之前的版本。


**Config paths**

 - <code>connectors.influxdb.$name.parameters</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__INFLUXDB__$NAME__PARAMETERS</code>



**Fields**

- influxdb_type: <code>influxdb_api_v1</code>
  * default: 
  `influxdb_api_v1`

  InfluxDB HTTP API 协议。支持 Influxdb v1.8 以及之前的版本。

- database: <code>binary()</code>

  InfluxDB 数据库。

- username: <code>binary()</code>

  InfluxDB 用户名。

- password: <code>emqx_schema_secret:secret()</code>

  InfluxDB 密码。


## connector_influxdb:connector_influxdb_api_v2
InfluxDB HTTP API V2 协议。支持 Influxdb v2.0 以及之后的版本。


**Config paths**

 - <code>connectors.influxdb.$name.parameters</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__INFLUXDB__$NAME__PARAMETERS</code>



**Fields**

- influxdb_type: <code>influxdb_api_v2</code>
  * default: 
  `influxdb_api_v2`

  InfluxDB HTTP API V2 协议。支持 Influxdb v2.0 以及之后的版本。

- bucket: <code>binary()</code>

  InfluxDB bucket 名称。

- org: <code>binary()</code>

  InfluxDB 组织名称。

- token: <code>emqx_schema_secret:secret()</code>

  InfluxDB token。


## dashboard
EMQX Dashboard 配置。


**Config paths**

 - <code>dashboard</code>


**Env overrides**

 - <code>EMQX_DASHBOARD</code>



**Fields**

- listeners: <code>[dashboard:listeners](#dashboard-listeners)</code>

  Dashboard 监听器设置。监听器必须有唯一的端口号和 IP 地址的组合。
  例如，可以通过指定 IP 地址 0.0.0.0 来监听机器上给定端口上的所有配置的 IP 地址。
  或者，可以为每个监听器指定唯一的 IP 地址，但使用相同的端口。

- token_expired_time: <code>emqx_schema:duration()</code>
  * default: 
  `60m`

  登录成功返回的 JWT token 过期时间，默认为 60 分钟。

- cors: <code>boolean()</code>
  * default: 
  `false`

  CORS（Cross-Origin Resource Sharing，跨域资源共享）允许服务器响应来自任何来源（域名、协议或端口）的请求，启用后允许另一个域名下的服务直接通过 JavaScript 调用 EMQX REST API。

- sso: <code>[dashboard:sso](#dashboard-sso)</code>




## dashboard:http
Dashboard 监听器(HTTP)配置。


**Config paths**

 - <code>dashboard.listeners.http</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__LISTENERS__HTTP</code>



**Fields**

- bind: <code>emqx_schema:ip_port()</code>
  * default: 
  `0`

  监听地址和端口，热更新此配置时，会重启 Dashboard 服务。

- num_acceptors: <code>integer()</code>
  * default: 
  `16`

  TCP 协议的 Socket acceptor 池大小, 通常配置为 CPU 核数

- max_connections: <code>integer()</code>
  * default: 
  `512`

  同时处理的最大连接数。

- backlog: <code>integer()</code>
  * default: 
  `1024`

  排队等待连接的队列的最大长度。

- send_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `10s`

  发送响应内容的超时时间。

- inet6: <code>boolean()</code>
  * default: 
  `false`

  启用 IPv6， 如果机器不支持 IPv6，请关闭此选项，否则会导致 Dashboard 无法使用。

- ipv6_v6only: <code>boolean()</code>
  * default: 
  `false`

  当开启 inet6 功能的同时禁用 IPv4-to-IPv6 映射。该配置仅在 inet6 功能开启时有效。

- proxy_header: <code>boolean()</code>
  * default: 
  `false`

  启用 Proxy Protocol 以提取客户端连接的原始信息，要求使用了代理服务器并且代理服务器也启用 Proxy Protocol。注意：一旦开启了这个功能，就无法再处理普通的 HTTP 请求。


## dashboard:https
Dashboard 监听器(HTTPS)配置。


**Config paths**

 - <code>dashboard.listeners.https</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__LISTENERS__HTTPS</code>



**Fields**

- bind: <code>emqx_schema:ip_port()</code>
  * default: 
  `0`

  监听地址和端口，热更新此配置时，会重启 Dashboard 服务。

- ssl_options: <code>[dashboard:ssl_options](#dashboard-ssl_options)</code>

  Dashboard 监听器的 SSL/TLS 选项。

- num_acceptors: <code>integer()</code>
  * default: 
  `16`

  TCP 协议的 Socket acceptor 池大小, 通常配置为 CPU 核数

- max_connections: <code>integer()</code>
  * default: 
  `512`

  同时处理的最大连接数。

- backlog: <code>integer()</code>
  * default: 
  `1024`

  排队等待连接的队列的最大长度。

- send_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `10s`

  发送响应内容的超时时间。

- inet6: <code>boolean()</code>
  * default: 
  `false`

  启用 IPv6， 如果机器不支持 IPv6，请关闭此选项，否则会导致 Dashboard 无法使用。

- ipv6_v6only: <code>boolean()</code>
  * default: 
  `false`

  当开启 inet6 功能的同时禁用 IPv4-to-IPv6 映射。该配置仅在 inet6 功能开启时有效。

- proxy_header: <code>boolean()</code>
  * default: 
  `false`

  启用 Proxy Protocol 以提取客户端连接的原始信息，要求使用了代理服务器并且代理服务器也启用 Proxy Protocol。注意：一旦开启了这个功能，就无法再处理普通的 HTTP 请求。


## dashboard:listeners
Dashboard 监听器配置。


**Config paths**

 - <code>dashboard.listeners</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__LISTENERS</code>



**Fields**

- http: <code>[dashboard:http](#dashboard-http)</code>

  TCP listeners

- https: <code>[dashboard:https](#dashboard-https)</code>

  SSL listeners


## dashboard:ssl_options
Dashboard 监听器的 SSL/TLS 选项。


**Config paths**

 - <code>dashboard.listeners.https.ssl_options</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__LISTENERS__HTTPS__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cacert.pem"`

  受信任的 PEM 格式 CA  证书捆绑文件<br/>
  此文件中的证书用于验证 TLS 对等方的证书。
  如果要信任新 CA，请将新证书附加到文件中。
  无需重启 EMQX 即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
  注意：从文件中失效（删除）证书不会影响已建立的连接。

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cert.pem"`

  PEM 格式证书链文件<br/>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
  然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
  根 CA 证书是可选的，如果想要添加，应加到文件到最末端。

- keyfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/key.pem"`

  PEM 格式的私钥文件。

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  启用或禁用对等验证。

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  启用 TLS 会话重用。

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
  因此，如果深度为 0，则对等方必须由受信任的根 CA 直接签名；<br/>
  如果是 1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
  如果是 2，则路径可以是 PEER、中间 CA1、中间 CA2、ROOT-CA。

- password: <code>string()</code>

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

  支持所有 TLS/DTLS 版本<br/>
  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。

- ciphers: <code>[string()]</code>
  * default: 
  `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  <br/>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
  注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  <br/>
  注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
  如果打算使用 PSK 密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

  <br/>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。

- dhfile: <code>string()</code>

  如果协商使用 Diffie-Hellman 密钥交换的密码套件，则服务器将使用包含 PEM 编码的 Diffie-Hellman 参数的文件的路径。如果未指定，则使用默认参数。<br/>
  注意：TLS 1.3 不支持<code>dhfile</code>选项。

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
  `15s`

  握手完成所允许的最长时间


## dashboard:sso
Dashboard Single Sign-On


**Config paths**

 - <code>dashboard.sso</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__SSO</code>



**Fields**

- ldap: <code>[sso:ldap](#sso-ldap)</code>



- saml: <code>[dashboard:saml](#dashboard-saml)</code>




## emqx:cluster_dns
DNS SRV 记录服务发现。


**Config paths**

 - <code>cluster.dns</code>


**Env overrides**

 - <code>EMQX_CLUSTER__DNS</code>



**Fields**

- name: <code>string()</code>
  * default: 
  `localhost`

  指定 DNS A 记录的名字。emqx 会通过访问这个 DNS A 记录来获取 IP 地址列表。
  当<code>cluster.discovery_strategy</code> 为 <code>dns</code> 时有效。

- record_type: <code>a | srv</code>
  * default: 
  `a`

  DNS 记录类型。


## emqx:cluster_etcd
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
  `emqxcl`

  指定 etcd 路径的前缀。每个节点在 etcd 中都会创建一个路径:
  v2/keys/<prefix>/<cluster.name>/<node.name> <br/>
  当 cluster.discovery_strategy 为 etcd 时，此配置项才有效。

- node_ttl: <code>emqx_schema:duration()</code>
  * default: 
  `1m`

  指定 etcd 中节点信息的过期时间。
  当 cluster.discovery_strategy 为 etcd 时，此配置项才有效。

- ssl_options: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>

  当使用 TLS 连接 etcd 时的配置选项。
  当 cluster.discovery_strategy 为 etcd 时，此配置项才有效。


## emqx:cluster_k8s
Kubernetes 服务发现。


**Config paths**

 - <code>cluster.k8s</code>


**Env overrides**

 - <code>EMQX_CLUSTER__K8S</code>



**Fields**

- apiserver: <code>string()</code>
  * default: 
  `"https://kubernetes.default.svc:443"`

  指定 Kubernetes API Server。如有多个 Server 使用逗号 , 分隔。
  当 cluster.discovery_strategy 为 k8s 时，此配置项才有效。

- service_name: <code>string()</code>
  * default: 
  `emqx`

  指定 Kubernetes 中 EMQX 的服务名。
  当 cluster.discovery_strategy 为 k8s 时，此配置项才有效。

- address_type: <code>ip | dns | hostname</code>
  * default: 
  `ip`

  当使用 k8s 方式集群时，address_type 用来从 Kubernetes 接口的应答里获取什么形式的 Host 列表。
  指定 <code>cluster.k8s.address_type</code> 为 <code>ip</code>，则将从 Kubernetes 接口中获取集群中其他节点
  的 IP 地址。

- namespace: <code>string()</code>
  * default: 
  `default`

  当使用 k8s 方式并且 cluster.k8s.address_type 指定为 dns 类型时，
  可设置 emqx 节点名的命名空间。与 cluster.k8s.suffix 一起使用用以拼接得到节点名列表。

- suffix: <code>string()</code>
  * default: 
  `pod.local`

  当使用 k8s 方式并且 cluster.k8s.address_type 指定为 dns 类型时，可设置 emqx 节点名的后缀。
  与 cluster.k8s.namespace 一起使用用以拼接得到节点名列表。


## emqx:cluster_static
静态节点服务发现。新节点通过连接一个节点来加入集群。


**Config paths**

 - <code>cluster.static</code>


**Env overrides**

 - <code>EMQX_CLUSTER__STATIC</code>



**Fields**

- seeds: <code>emqx_schema:comma_separated_atoms() | [atom()]</code>
  * default: 
  `[]`

  集群中的 EMQX 节点名称列表，
  指定固定的节点列表，多个节点间使用逗号 , 分隔。
  当 cluster.discovery_strategy 为 static 时，此配置项才有效。
  适合于节点数量较少且固定的集群。


## emqx:authorization
授权相关


**Config paths**

 - <code>authorization</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION</code>



**Fields**

- no_match: <code>allow | deny</code>
  * default: 
  `allow`

  如果用户或客户端不匹配 ACL 规则，或者从可配置授权源(比如内置数据库、HTTP API 或 PostgreSQL 等。)内未找
  到此类用户或客户端时，模式的认访问控制操作。
  在“授权”中查找更多详细信息。

- deny_action: <code>ignore | disconnect</code>
  * default: 
  `ignore`

  授权检查拒绝操作时的操作。

- cache: <code>[broker:authz_cache](#broker-authz_cache)</code>



- sources: <code>[[authz:file](#authz-file) | [authz:builtin_db](#authz-builtin_db) | [authz:http_get](#authz-http_get) | [authz:http_post](#authz-http_post) | [authz:redis_single](#authz-redis_single) | [authz:redis_sentinel](#authz-redis_sentinel) | [authz:redis_cluster](#authz-redis_cluster) | [authz:mysql](#authz-mysql) | [authz:postgresql](#authz-postgresql) | [authz:mongo_single](#authz-mongo_single) | [authz:mongo_rs](#authz-mongo_rs) | [authz:mongo_sharded](#authz-mongo_sharded) | [authz:ldap](#authz-ldap)]</code>
  * default: 

  ```
  [
    {
      enable = true
      path = "${EMQX_ETC_DIR}/acl.conf"
      type = file
    }
  ]
  ```

  <br/>
  授权（ACL）数据提供者的数组。
  它被设计为一个数组，而不是哈希映射，因此可以
  将源按顺序排列形成访问控制链。<br/>

  在授权 '发布' 或 '订阅' 操作时，配置的
  源会按顺序检查。在检查 ACL 源时，
  如果未找到客户端（通过用户名或客户端 ID 标识），
  则继续检查下一个源。一旦返回 '允许' 或 '拒绝' 决定，
  立即停止检查。<br/>

  如果在任何源中都未找到客户端，
  则应用 'authorization.no_match' 中配置的默认操作。<br/>

  注意：
  源元素由它们的 '类型' 标识。
  不允许配置两个或更多相同类型的源。


## emqx:cluster
EMQX 节点可以组成一个集群，以提高总容量。<br/> 这里指定了节点之间如何连接。


**Config paths**

 - <code>cluster</code>


**Env overrides**

 - <code>EMQX_CLUSTER</code>



**Fields**

- name: <code>atom()</code>
  * default: 
  `emqxcl`

  EMQX 集群名称。每个集群都有一个唯一的名称。服务发现时会用于做路径的一部分。

- discovery_strategy: <code>manual | static | dns | etcd | k8s</code>
  * default: 
  `manual`

  集群节点发现方式。可选值为:
  - manual: 使用 <code>emqx ctl cluster</code> 命令管理集群。<br/>
  - static: 配置静态节点。配置几个固定的节点，新节点通过连接固定节点中的某一个来加入集群。<br/>
  - dns: 使用 DNS A 记录的方式发现节点。<br/>
  - etcd: 使用 etcd 发现节点。<br/>
  - k8s: 使用 Kubernetes API 发现节点。

- autoclean: <code>emqx_schema:duration()</code>
  * default: 
  `24h`

  指定多久之后从集群中删除离线节点。

- autoheal: <code>boolean()</code>
  * default: 
  `true`

  集群脑裂自动恢复机制开关。

- proto_dist: <code>inet_tcp | inet6_tcp | inet_tls | inet6_tls</code>
  * default: 
  `inet_tcp`

  分布式 Erlang 集群协议类型。可选值为:<br/>
  - inet_tcp: 使用 IPv4 <br/>
  - inet_tls: 使用 TLS，需要配合 <code>etc/ssl_dist.conf</code> 一起使用。<br/>
  - inet6_tcp: IPv6 TCP <br/>
  - inet6_tls: IPv6 TLS， 与 <code>etc/ssl_dist.conf</code> 配合使用。

- static: <code>[emqx:cluster_static](#emqx-cluster_static)</code>



- dns: <code>[emqx:cluster_dns](#emqx-cluster_dns)</code>



- etcd: <code>[emqx:cluster_etcd](#emqx-cluster_etcd)</code>



- k8s: <code>[emqx:cluster_k8s](#emqx-cluster_k8s)</code>




## emqx:console_handler
日志处理进程将日志事件打印到 EMQX 控制台。


**Config paths**

 - <code>log.console</code>


**Env overrides**

 - <code>EMQX_LOG__CONSOLE</code>



**Fields**

- level: <code>debug | info | notice | warning | error | critical | alert | emergency | all</code>
  * default: 
  `warning`

  当前日志处理进程的日志级别。
  默认为 warning 级别。

- enable: <code>boolean()</code>
  * default: 
  `false`

  启用此日志处理进程。

- formatter: <code>text | json</code>
  * default: 
  `text`

  选择日志格式类型。 <code>text</code> 用于纯文本，<code>json</code> 用于结构化日志记录。

- time_offset: <code>string()</code>
  * default: 
  `system`

  日志中的时间戳使用的时间偏移量。
  可选值为：
    - <code>system</code>: 本地系统使用的时区偏移量
    - <code>utc</code>: 0 时区的偏移量
    - <code>+-[hh]:[mm]</code>: 自定义偏移量，比如 "-02:00" 或者 "+00:00"
  默认值为本地系统的时区偏移量：<code>system</code>。


## emqx:log_file_handler
日志处理进程将日志事件打印到文件。


**Config paths**

 - <code>log.file</code>
 - <code>log.file.$handler_name</code>


**Env overrides**

 - <code>EMQX_LOG__FILE</code>
 - <code>EMQX_LOG__FILE__$HANDLER_NAME</code>



**Fields**

- path: <code>string()</code>
  * default: 
  `"${EMQX_LOG_DIR}/emqx.log"`

  日志文件路径及名字。

- rotation_count: <code>1..128</code>
  * default: 
  `10`

  轮换的最大日志文件数。

- rotation_size: <code>infinity | emqx_schema:bytesize()</code>
  * default: 
  `50MB`

  此参数控制日志文件轮换。 `infinity` 意味着日志文件将无限增长，否则日志文件将在达到 `max_size`（以字节为单位）时进行轮换。
  与 rotation count 配合使用。如果 counter 为 10，则是 10 个文件轮换。

- level: <code>debug | info | notice | warning | error | critical | alert | emergency | all</code>
  * default: 
  `warning`

  当前日志处理进程的日志级别。
  默认为 warning 级别。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用此日志处理进程。

- formatter: <code>text | json</code>
  * default: 
  `text`

  选择日志格式类型。 <code>text</code> 用于纯文本，<code>json</code> 用于结构化日志记录。

- time_offset: <code>string()</code>
  * default: 
  `system`

  日志中的时间戳使用的时间偏移量。
  可选值为：
    - <code>system</code>: 本地系统使用的时区偏移量
    - <code>utc</code>: 0 时区的偏移量
    - <code>+-[hh]:[mm]</code>: 自定义偏移量，比如 "-02:00" 或者 "+00:00"
  默认值为本地系统的时区偏移量：<code>system</code>。


## emqx:rpc
EMQX 使用 <code>gen_rpc</code> 库来实现跨节点通信。<br/>
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

- protocol: <code>tcp | ssl</code>
  * default: 
  `tcp`

  集群间通信使用的传输协议。

- async_batch_size: <code>integer()</code>
  * default: 
  `256`

  异步模式下，发送的批量消息的最大数量。

- port_discovery: <code>manual | stateless</code>
  * default: 
  `stateless`

  <code>manual</code>: 通过 <code>tcp_server_port</code> 来发现端口。
  <br/><code>stateless</code>: 使用无状态的方式来发现端口，使用如下算法。如果节点名称是 <code>
  emqxN@127.0.0.1</code>, N 是一个数字，那么监听端口就是 5370 + N。

- tcp_server_port: <code>integer()</code>
  * default: 
  `5369`

  RPC 本地服务使用的 TCP 端口。<br/>
  只有当 rpc.port_discovery 设置为 manual 时，此配置才会生效。

- ssl_server_port: <code>integer()</code>
  * default: 
  `5369`

  RPC 本地服务使用的监听 SSL 端口。<br/>
  只有当 rpc.port_discovery 设置为 manual 且 <code> dirver </code> 设置为 <code>ssl</code>，
  此配置才会生效。

- tcp_client_num: <code>1..256</code>
  * default: 
  `10`

  设置本节点与远程节点之间的 RPC 通信通道的最大数量。

- connect_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  建立 RPC 连接的超时时间。

- certfile: <code>string()</code>

  TLS 证书文件的路径，用于验证集群节点的身份。
  只有当 <code>rpc.driver</code> 设置为 <code>ssl</code> 时，此配置才会生效。

- keyfile: <code>string()</code>

  <code>rpc.certfile</code> 的私钥文件的路径。<br/>
  注意：此文件内容是私钥，所以需要设置权限为 600。

- cacertfile: <code>string()</code>

  验证 <code>rpc.certfile</code> 的 CA 证书文件的路径。<br/>
  注意：集群中所有节点的证书必须使用同一个 CA 签发。

- send_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  发送 RPC 请求的超时时间。

- authentication_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  远程节点认证的超时时间。

- call_receive_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `15s`

  同步 RPC 的回复超时时间。

- socket_keepalive_idle: <code>emqx_schema:timeout_duration_s()</code>
  * default: 
  `15m`

  broker 之间的连接在最后一条消息发送后保持打开的时间。

- socket_keepalive_interval: <code>emqx_schema:timeout_duration_s()</code>
  * default: 
  `75s`

  keepalive 消息的间隔。

- socket_keepalive_count: <code>integer()</code>
  * default: 
  `9`

  keepalive 探测消息发送失败的次数，直到 RPC 连接被认为已经断开。

- socket_sndbuf: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

  TCP 调节参数。TCP 发送缓冲区大小。

- socket_recbuf: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

  TCP 调节参数。TCP 接收缓冲区大小。

- socket_buffer: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

  TCP 调节参数。用户模式套接字缓冲区大小。

- insecure_fallback: <code>boolean()</code>
  * default: 
  `true`

  兼容旧的无鉴权模式

- ciphers: <code>[string()]</code>
  * default: 
  `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  <br/>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
  注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  <br/>
  注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
  如果打算使用 PSK 密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

  <br/>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- tls_versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

  支持所有 TLS/DTLS 版本<br/>
  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。

- listen_address: <code>string()</code>
  * default: 
  `"0.0.0.0"`

  指示 RPC 服务器监听的 IP 地址。例如，使用 <code>"0.0.0.0"</code> 表示 IPv4 或 <code>"::"</code> 表示 IPv6。

- ipv6_only: <code>boolean()</code>
  * default: 
  `false`

  此设置仅在 <code>rpc.listen_address</code> 被分配了一个 IPv6 地址时有效。
  如果设置为 <code>true</code>，RPC 客户端将仅使用 IPv6 进行连接。
  否则，即使服务器位于 IPv6 上，客户端也可能选择 IPv4。


## emqx:file_transfer
File transfer settings


**Config paths**

 - <code>file_transfer</code>


**Env overrides**

 - <code>EMQX_FILE_TRANSFER</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  启用文件传输（File Transfer）服务。<br/>
  文件传输服务允许客户端使用 `$file` 主题将分片上传到 EMQX。
  启用文件传输服务表示 `$file` 主题前缀已被预留，用于提供文件传输服务。<br/>
  这个开关也会影响文件传输 REST API 的可用性，以及依赖存储的后台任务（例如垃圾回收）。

- init_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `10s`

  指定文件传输初始化的时间限制。在<br/>
  EMQX 服务器过载时可能发生初始化超时。超时后给 `init` 的 PUBACK 中包含一个错误码 （0x80）。

- store_segment_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5m`

  文件片段保存超时。<br/>
  EMQX 收到文件分片后，会对它进行保存，如果发生超时（例如系统过载），则会给这个发布消息的 PUBACK 中包含一个错误码（0x80）。

- assemble_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5m`

  文件拼接超时。<br/>
  在收到 'fin' 消息后 EMQX 会对文件进行拼接，如果发生超时（例如系统过载），则会给 'fin' 的 PUBACK 中包含一个错误码（0x80）。

- storage: <code>[file_transfer:storage_backend](#file_transfer-storage_backend)</code>
  * default: 

  ```
  {
    local {}
  }
  ```

  文件后端存储配置


## emqx:log
EMQX 支持同时多个日志输出，一个控制台输出，和多个文件输出。
默认情况下，EMQX 运行在容器中，或者在 'console' 或 'foreground' 模式下运行时，会输出到 控制台，否则输出到文件。


**Config paths**

 - <code>log</code>


**Env overrides**

 - <code>EMQX_LOG</code>



**Fields**

- console: <code>[emqx:console_handler](#emqx-console_handler)</code>



- file: <code>[emqx:log_file_handler](#emqx-log_file_handler) | {$handler_name -> [emqx:log_file_handler](#emqx-log_file_handler)}</code>
  * default: 
  `{level = warning}`

  输出到文件的日志处理进程列表

- audit: <code>[emqx:log_audit_handler](#emqx-log_audit_handler)</code>
  * default: 
  `{enable = false, level = info}`

  审计日志文件处理进程


## emqx:log_audit_handler
将日志时间输出到文件的审计日志处理进程。


**Config paths**

 - <code>log.audit</code>


**Env overrides**

 - <code>EMQX_LOG__AUDIT</code>



**Fields**

- path: <code>string()</code>
  * default: 
  `"${EMQX_LOG_DIR}/audit.log"`

  ----

- rotation_count: <code>1..128</code>
  * default: 
  `10`

  轮换的最大日志文件数。

- rotation_size: <code>infinity | emqx_schema:bytesize()</code>
  * default: 
  `50MB`

  此参数控制日志文件轮换。 `infinity` 意味着日志文件将无限增长，否则日志文件将在达到 `max_size`（以字节为单位）时进行轮换。
  与 rotation count 配合使用。如果 counter 为 10，则是 10 个文件轮换。

- max_filter_size: <code>10..30000</code>
  * default: 
  `5000`

  将最新的 N 条日志条目存储在数据库中，以供 /audit HTTP API 进行日志数据的筛选和检索。
  清除多余的日志记录的间隔保持在 10 到 20 秒之间。

- ignore_high_frequency_request: <code>boolean()</code>
  * default: 
  `true`

  忽略高频请求以避免淹没审计日志，例如发布/订阅踢出 http API 请求将被忽略。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用此日志处理进程。

- time_offset: <code>string()</code>
  * default: 
  `system`

  日志中的时间戳使用的时间偏移量。
  可选值为：
    - <code>system</code>: 本地系统使用的时区偏移量
    - <code>utc</code>: 0 时区的偏移量
    - <code>+-[hh]:[mm]</code>: 自定义偏移量，比如 "-02:00" 或者 "+00:00"
  默认值为本地系统的时区偏移量：<code>system</code>。


## emqx:node
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

  分布式 Erlang 集群使用的 cookie 值。集群间保持一致

- max_ports: <code>1024..134217727</code>
  * default: 
  `1048576`

  Erlang 系统同时存在的最大端口数。
  实际选择的最大值可能比设置的数字大得多。
  参考: https://www.erlang.org/doc/man/erl.html

- dist_buffer_size: <code>1..2097151</code>
  * default: 
  `8192`

  Erlang 分布式缓冲区的繁忙阈值，单位是 KB。

- data_dir: <code>string()</code>

  节点数据存放目录，可能会自动创建的子目录如下：<br/>
  - `mnesia/<node_name>`。EMQX 的内置数据库目录。例如，`mnesia/emqx@127.0.0.1`。<br/>
  如果节点要被重新命名（例如，`emqx@10.0.1.1`）。旧目录应该首先被删除。<br/>
  - `configs`。在启动时生成的配置，以及集群/本地覆盖的配置。<br/>
  - `patches`: 热补丁文件将被放在这里。<br/>
  - `trace`: 日志跟踪文件。<br/>

  **注意**: 一个数据 dir 不能被两个或更多的 EMQX 节点同时使用。

- global_gc_interval: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `15m`

  系统调优参数，设置节点运行多久强制进行一次全局垃圾回收。禁用设置为 <code>disabled</code>。

- role: <code>core | replicant</code>
  * default: 
  `core`

  选择节点的角色。<br/>
  <code>core</code> 节点提供数据的持久性，并负责写入。建议将核心节点放置在不同的机架或不同的可用区。<br/>
  <code>repliant</code> 节点是临时工作节点。 从集群中删除它们，不影响数据库冗余<br/>
  建议复制节点多于核心节点。<br/>
  注意：该参数仅在设置<code>backend</code>时生效到 <code>rlog</code>。


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

- request_timeout: <code>emqx_schema:timeout_duration()</code>
  * default: 
  `5s`

  gRPC 服务器请求超时

- failed_action: <code>deny | ignore</code>
  * default: 
  `deny`

  当 gRPC 请求失败后的操作

- ssl: <code>[exhook:ssl_conf](#exhook-ssl_conf)</code>



- socket_options: <code>[exhook:socket_options](#exhook-socket_options)</code>
  * default: 
  `{keepalive = true, nodelay = true}`



- auto_reconnect: <code>false | emqx_schema:timeout_duration()</code>
  * default: 
  `60s`

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

  当没有其他数据交换时，是否向连接的对端套接字定期的发送探测包。如果另一端没有响应，则认为连接断开，并向控制进程发送错误消息。

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

  受信任的 PEM 格式 CA  证书捆绑文件<br/>
  此文件中的证书用于验证 TLS 对等方的证书。
  如果要信任新 CA，请将新证书附加到文件中。
  无需重启 EMQX 即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
  注意：从文件中失效（删除）证书不会影响已建立的连接。

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>

  PEM 格式证书链文件<br/>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
  然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
  根 CA 证书是可选的，如果想要添加，应加到文件到最末端。

- keyfile: <code>binary()</code>

  PEM 格式的私钥文件。

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  启用或禁用对等验证。

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  启用 TLS 会话重用。

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
  因此，如果深度为 0，则对等方必须由受信任的根 CA 直接签名；<br/>
  如果是 1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
  如果是 2，则路径可以是 PEER、中间 CA1、中间 CA2、ROOT-CA。

- password: <code>string()</code>

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

  支持所有 TLS/DTLS 版本<br/>
  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。

- ciphers: <code>[string()]</code>
  * default: 
  `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  <br/>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
  注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  <br/>
  注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
  如果打算使用 PSK 密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

  <br/>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。

- enable: <code>boolean()</code>
  * default: 
  `false`

  启用 TLS。

- server_name_indication: <code>disable | string()</code>

  指定要在 TLS 服务器名称指示扩展中使用的主机名。<br/>
  例如，当连接到 "server.example.net" 时，接受连接并执行 TLS 握手的真正服务器可能与 TLS 客户端最初连接到的主机不同，
  例如，当连接到 IP 地址时，或者当主机具有多个可解析的 DNS 记录时<br/>
  如果未指定，它将默认为使用的主机名字符串
  建立连接，除非使用 IP 地址<br/>
  然后，主机名也用于对等机的主机名验证证书<br/>
  特殊值 <code>disable</code> 阻止发送服务器名称指示扩展，并禁用主机名验证检查。


## file_transfer:local_storage
File transfer local storage settings


**Config paths**

 - <code>file_transfer.storage.local</code>


**Env overrides**

 - <code>EMQX_FILE_TRANSFER__STORAGE__LOCAL</code>



**Fields**

- segments: <code>[file_transfer:local_storage_segments](#file_transfer-local_storage_segments)</code>
  * default: 

  ```
  {
    gc {}
  }
  ```

  本地文件系统存储配置，包括已上传的文件分片和临时数据。

- exporter: <code>[file_transfer:local_storage_exporter_backend](#file_transfer-local_storage_exporter_backend)</code>
  * default: 

  ```
  {
    local {}
  }
  ```

  将文件导出到本地存储<br/>
  该配置项指定所有分片都传输完成了的文件进行导出到本地存储的相关行为。

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该后端


## file_transfer:local_storage_exporter
Local Exporter settings for the File transfer local storage backend


**Config paths**

 - <code>file_transfer.storage.local.exporter.local</code>


**Env overrides**

 - <code>EMQX_FILE_TRANSFER__STORAGE__LOCAL__EXPORTER__LOCAL</code>



**Fields**

- root: <code>string()</code>

  导出到本地文件时使用的根目录。

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该后端


## file_transfer:local_storage_exporter_backend
Exporter for the local file system storage backend


**Config paths**

 - <code>file_transfer.storage.local.exporter</code>


**Env overrides**

 - <code>EMQX_FILE_TRANSFER__STORAGE__LOCAL__EXPORTER</code>



**Fields**

- local: <code>[file_transfer:local_storage_exporter](#file_transfer-local_storage_exporter)</code>

  将文件导出到本地存储。

- s3: <code>[file_transfer:s3_exporter](#file_transfer-s3_exporter)</code>

  将文件导出到 AWS s3 API 兼容的对象存储服务。


## file_transfer:local_storage_segments
File transfer local segments storage settings


**Config paths**

 - <code>file_transfer.storage.local.segments</code>


**Env overrides**

 - <code>EMQX_FILE_TRANSFER__STORAGE__LOCAL__SEGMENTS</code>



**Fields**

- root: <code>string()</code>

  文件分片存储的根目录。

- gc: <code>[file_transfer:local_storage_segments_gc](#file_transfer-local_storage_segments_gc)</code>

  文件系统中临时文件的垃圾回收配置。


## file_transfer:local_storage_segments_gc
Garbage collection settings for the File transfer local segments storage


**Config paths**

 - <code>file_transfer.storage.local.segments.gc</code>


**Env overrides**

 - <code>EMQX_FILE_TRANSFER__STORAGE__LOCAL__SEGMENTS__GC</code>



**Fields**

- interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `1h`

  运行垃圾回收的时间间隔。

- maximum_segments_ttl: <code>emqx_schema:duration_s()</code>
  * default: 
  `24h`

  分片的临时存储的最大 TTL。<br/>
  该配置为系统全局上限，所有的分片都不会超过这个 TTL，即使某些文件传输指定了一个更大的 TTL。

- minimum_segments_ttl: <code>emqx_schema:duration_s()</code>
  * default: 
  `5m`

  分片的临时存储的最小 TTL。<br/>
  分片在超过这个 TTL 之前不被垃圾回收。
  该配置为系统全局下限，所有的分片都不会低于这个 TTL，即使某些文件传输指定了一个更小的 TTL。


## file_transfer:s3_exporter
S3 Exporter settings for the File transfer local storage backend


**Config paths**

 - <code>file_transfer.storage.local.exporter.s3</code>


**Env overrides**

 - <code>EMQX_FILE_TRANSFER__STORAGE__LOCAL__EXPORTER__S3</code>



**Fields**

- access_key_id: <code>string()</code>

  The access key ID of the S3 bucket.

- secret_access_key: <code>string()</code>

  The secret access key of the S3 bucket.

- bucket: <code>string()</code>

  The name of the S3 bucket.

- host: <code>string()</code>

  The host of the S3 endpoint.

- port: <code>pos_integer()</code>

  The port of the S3 endpoint.

- url_expire_time: <code>emqx_schema:duration_s()</code>
  * default: 
  `1h`

  The time in seconds for which the signed URLs to the S3 objects are valid.

- min_part_size: <code>emqx_schema:bytesize()</code>
  * default: 
  `5mb`

  The minimum part size for multipart uploads.<br/>
  Uploaded data will be accumulated in memory until this size is reached.

- max_part_size: <code>emqx_schema:bytesize()</code>
  * default: 
  `5gb`

  The maximum part size for multipart uploads.<br/>
  S3 uploader won't try to upload parts larger than this size.

- acl: <code>private | public_read | public_read_write | authenticated_read | bucket_owner_read | bucket_owner_full_control</code>

  The ACL to use for the uploaded objects.

- transport_options: <code>[s3:transport_options](#s3-transport_options)</code>

  Options for the HTTP transport layer used by the S3 client.

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该后端


## file_transfer:storage_backend
Storage backend settings for file transfer


**Config paths**

 - <code>file_transfer.storage</code>


**Env overrides**

 - <code>EMQX_FILE_TRANSFER__STORAGE</code>



**Fields**

- local: <code>[file_transfer:local_storage](#file_transfer-local_storage)</code>

  EMQX 节点本地用文件系统于存储文件分片的相关配置。


## gateway:clientinfo_override
ClientInfo override.


**Config paths**

 - <code>gateway.coap.clientinfo_override</code>
 - <code>gateway.exproto.clientinfo_override</code>
 - <code>gateway.gbt32960.clientinfo_override</code>
 - <code>gateway.lwm2m.clientinfo_override</code>
 - <code>gateway.mqttsn.clientinfo_override</code>
 - <code>gateway.ocpp.clientinfo_override</code>
 - <code>gateway.stomp.clientinfo_override</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__CLIENTINFO_OVERRIDE</code>
 - <code>EMQX_GATEWAY__EXPROTO__CLIENTINFO_OVERRIDE</code>
 - <code>EMQX_GATEWAY__GBT32960__CLIENTINFO_OVERRIDE</code>
 - <code>EMQX_GATEWAY__LWM2M__CLIENTINFO_OVERRIDE</code>
 - <code>EMQX_GATEWAY__MQTTSN__CLIENTINFO_OVERRIDE</code>
 - <code>EMQX_GATEWAY__OCPP__CLIENTINFO_OVERRIDE</code>
 - <code>EMQX_GATEWAY__STOMP__CLIENTINFO_OVERRIDE</code>



**Fields**

- username: <code>binary()</code>

  username 重写模板

- password: <code>binary()</code>

  password 重写模板

- clientid: <code>binary()</code>

  clientid 重写模板


## gateway:dtls_listener
Settings for DTLS listener.


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

- bind: <code>emqx_gateway_schema:ip_port()</code>

  监听器绑定的 IP 地址或端口。

- max_connections: <code>pos_integer() | infinity</code>
  * default: 
  `1024`

  监听器支持的最大连接数。

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  监听器支持的最大连接速率。

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。

- mountpoint: <code>binary()</code>

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
  则客户端实际上订阅了 `some_tenant/t` 主题。
  类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
  则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：<br/>
     - <code>${clientid}</code>：clientid<br/>
     - <code>${username}</code>：用户名

- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  配置监听器的访问控制规则。
  见：https://github.com/emqtt/esockd#allowdeny

- dtls_options: <code>[gateway:dtls_opts](#gateway-dtls_opts)</code>

  DTLS Socket 配置


## gateway:dtls_opts
Settings for DTLS protocol.


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
  * default: 
  `"${EMQX_ETC_DIR}/certs/cacert.pem"`

  受信任的 PEM 格式 CA  证书捆绑文件<br/>
  此文件中的证书用于验证 TLS 对等方的证书。
  如果要信任新 CA，请将新证书附加到文件中。
  无需重启 EMQX 即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
  注意：从文件中失效（删除）证书不会影响已建立的连接。

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cert.pem"`

  PEM 格式证书链文件<br/>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
  然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
  根 CA 证书是可选的，如果想要添加，应加到文件到最末端。

- keyfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/key.pem"`

  PEM 格式的私钥文件。

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  启用或禁用对等验证。

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  启用 TLS 会话重用。

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
  因此，如果深度为 0，则对等方必须由受信任的根 CA 直接签名；<br/>
  如果是 1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
  如果是 2，则路径可以是 PEER、中间 CA1、中间 CA2、ROOT-CA。

- password: <code>string()</code>

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。

- versions: <code>[atom()]</code>
  * default: 
  `[dtlsv1.2]`

  支持所有 TLS/DTLS 版本<br/>
  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。

- ciphers: <code>[string()]</code>
  * default: 
  `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  <br/>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
  注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  <br/>
  注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
  如果打算使用 PSK 密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

  <br/>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。

- dhfile: <code>string()</code>

  如果协商使用 Diffie-Hellman 密钥交换的密码套件，则服务器将使用包含 PEM 编码的 Diffie-Hellman 参数的文件的路径。如果未指定，则使用默认参数。<br/>
  注意：TLS 1.3 不支持<code>dhfile</code>选项。

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

- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `15s`

  握手完成所允许的最长时间

- gc_after_handshake: <code>boolean()</code>
  * default: 
  `false`

  内存使用调优。如果启用，将在 TLS/SSL 握手完成后立即执行垃圾回收。TLS/SSL 握手建立后立即进行 GC。

- ocsp: <code>[broker:ocsp](#broker-ocsp)</code>



- enable_crl_check: <code>boolean()</code>
  * default: 
  `false`

  是否为该监听器启用 CRL 检查。


## gateway
EMQX Gateway configuration root.


**Config paths**

 - <code>gateway</code>


**Env overrides**

 - <code>EMQX_GATEWAY</code>



**Fields**

- mqttsn: <code>[gateway:mqttsn](#gateway-mqttsn)</code>



- gbt32960: <code>[gateway_gbt32960:gbt32960](#gateway_gbt32960-gbt32960)</code>



- coap: <code>[gateway:coap](#gateway-coap)</code>



- ocpp: <code>[gateway_ocpp:ocpp](#gateway_ocpp-ocpp)</code>



- stomp: <code>[gateway:stomp](#gateway-stomp)</code>



- lwm2m: <code>[gateway:lwm2m](#gateway-lwm2m)</code>



- exproto: <code>[gateway:exproto](#gateway-exproto)</code>




## gateway:ssl_listener
Settings for SSL listener.


**Config paths**

 - <code>gateway.exproto.listeners.ssl.$name</code>
 - <code>gateway.gbt32960.listeners.ssl.$name</code>
 - <code>gateway.stomp.listeners.ssl.$name</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME</code>
 - <code>EMQX_GATEWAY__GBT32960__LISTENERS__SSL__$NAME</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME</code>



**Fields**

- acceptors: <code>integer()</code>
  * default: 
  `16`

  Acceptor 进程池大小。

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>

  TCP Socket 配置。

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`

  是否开启 Proxy Protocol V1/2。当 EMQX 集群部署在 HAProxy 或 Nginx 后需要获取客户端真实 IP 时常用到该选项。参考：https://www.haproxy.com/blog/haproxy/proxy-protocol/

- proxy_protocol_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `3s`

  接收 Proxy Protocol 报文头的超时时间。如果在超时内没有收到 Proxy Protocol 包，EMQX 将关闭 TCP 连接。

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该监听器。

- bind: <code>emqx_gateway_schema:ip_port()</code>

  监听器绑定的 IP 地址或端口。

- max_connections: <code>pos_integer() | infinity</code>
  * default: 
  `1024`

  监听器支持的最大连接数。

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  监听器支持的最大连接速率。

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。

- mountpoint: <code>binary()</code>

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
  则客户端实际上订阅了 `some_tenant/t` 主题。
  类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
  则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：<br/>
     - <code>${clientid}</code>：clientid<br/>
     - <code>${username}</code>：用户名

- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  配置监听器的访问控制规则。
  见：https://github.com/emqtt/esockd#allowdeny

- ssl_options: <code>[broker:listener_ssl_opts](#broker-listener_ssl_opts)</code>

  SSL Socket 配置。


## gateway:tcp_listener
Settings for TCP listener.


**Config paths**

 - <code>gateway.exproto.listeners.tcp.$name</code>
 - <code>gateway.gbt32960.listeners.tcp.$name</code>
 - <code>gateway.stomp.listeners.tcp.$name</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME</code>
 - <code>EMQX_GATEWAY__GBT32960__LISTENERS__TCP__$NAME</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME</code>



**Fields**

- acceptors: <code>integer()</code>
  * default: 
  `16`

  Acceptor 进程池大小。

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>

  TCP Socket 配置。

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`

  是否开启 Proxy Protocol V1/2。当 EMQX 集群部署在 HAProxy 或 Nginx 后需要获取客户端真实 IP 时常用到该选项。参考：https://www.haproxy.com/blog/haproxy/proxy-protocol/

- proxy_protocol_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `3s`

  接收 Proxy Protocol 报文头的超时时间。如果在超时内没有收到 Proxy Protocol 包，EMQX 将关闭 TCP 连接。

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该监听器。

- bind: <code>emqx_gateway_schema:ip_port()</code>

  监听器绑定的 IP 地址或端口。

- max_connections: <code>pos_integer() | infinity</code>
  * default: 
  `1024`

  监听器支持的最大连接数。

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  监听器支持的最大连接速率。

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。

- mountpoint: <code>binary()</code>

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
  则客户端实际上订阅了 `some_tenant/t` 主题。
  类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
  则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：<br/>
     - <code>${clientid}</code>：clientid<br/>
     - <code>${username}</code>：用户名

- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  配置监听器的访问控制规则。
  见：https://github.com/emqtt/esockd#allowdeny


## gateway:tcp_listeners
Settings for the TCP listeners.


**Config paths**

 - <code>gateway.gbt32960.listeners</code>
 - <code>gateway.stomp.listeners</code>


**Env overrides**

 - <code>EMQX_GATEWAY__GBT32960__LISTENERS</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS</code>



**Fields**

- tcp: <code>{$name -> [gateway:tcp_listener](#gateway-tcp_listener)}</code>

  从监听器名称到配置参数的映射。

- ssl: <code>{$name -> [gateway:ssl_listener](#gateway-ssl_listener)}</code>

  从监听器名称到配置参数的映射。


## gateway:tcp_udp_listeners
Settings for TCP and UDP listeners.


**Config paths**

 - <code>gateway.exproto.listeners</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS</code>



**Fields**

- tcp: <code>{$name -> [gateway:tcp_listener](#gateway-tcp_listener)}</code>

  从监听器名称到配置参数的映射。

- ssl: <code>{$name -> [gateway:ssl_listener](#gateway-ssl_listener)}</code>

  从监听器名称到配置参数的映射。

- udp: <code>{$name -> [gateway:udp_listener](#gateway-udp_listener)}</code>

  从监听器名称到配置参数的映射。

- dtls: <code>{$name -> [gateway:dtls_listener](#gateway-dtls_listener)}</code>

  从监听器名称到配置参数的映射。


## gateway:udp_listener
Settings for UDP listener.


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

- bind: <code>emqx_gateway_schema:ip_port()</code>

  监听器绑定的 IP 地址或端口。

- max_connections: <code>pos_integer() | infinity</code>
  * default: 
  `1024`

  监听器支持的最大连接数。

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  监听器支持的最大连接速率。

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。

- mountpoint: <code>binary()</code>

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
  则客户端实际上订阅了 `some_tenant/t` 主题。
  类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
  则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：<br/>
     - <code>${clientid}</code>：clientid<br/>
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

  从监听器名称到配置参数的映射。

- dtls: <code>{$name -> [gateway:dtls_listener](#gateway-dtls_listener)}</code>

  从监听器名称到配置参数的映射。


## gateway:udp_opts
Settings for UDP sockets.


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

  允许重用本地处于 TIME_WAIT 的端口号。


## gateway:lwm2m
The LwM2M protocol gateway.


**Config paths**

 - <code>gateway.lwm2m</code>


**Env overrides**

 - <code>EMQX_GATEWAY__LWM2M</code>



**Fields**

- xml_dir: <code>binary()</code>

  LwM2M Resource 定义的 XML 文件目录路径。

- lifetime_min: <code>emqx_lwm2m_schema:duration()</code>
  * default: 
  `15s`

  允许 LwM2M 客户端允许设置的心跳最小值。

- lifetime_max: <code>emqx_lwm2m_schema:duration()</code>
  * default: 
  `86400s`

  允许 LwM2M 客户端允许设置的心跳最大值。

- qmode_time_window: <code>emqx_lwm2m_schema:duration_s()</code>
  * default: 
  `22s`

  在 QMode 模式下，LwM2M 网关认为网络链接有效的时间窗口的值。
  例如，在收到客户端的更新信息后，在这个时间窗口内的任何信息都会直接发送到 LwM2M 客户端，而超过这个时间窗口的所有信息都会暂时储存在内存中。

- auto_observe: <code>boolean()</code>
  * default: 
  `false`

  自动 Observe REGISTER 数据包的 Object 列表。

- update_msg_publish_condition: <code>always | contains_object_list</code>
  * default: 
  `contains_object_list`

  发布 UPDATE 事件消息的策略。<br/>
    - always: 只要收到 UPDATE 请求，就发送更新事件。<br/>
    - contains_object_list: 仅当 UPDATE 请求携带 Object 列表时才发送更新事件。

- translators: <code>[gateway:lwm2m_translators](#gateway-lwm2m_translators)</code>

  LwM2M 网关订阅/发布消息的主题映射配置。

- mountpoint: <code>binary()</code>
  * default: 
  `"lwm2m/${endpoint_name}/"`

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
  则客户端实际上订阅了 `some_tenant/t` 主题。
  类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
  则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：<br/>
     - <code>${clientid}</code>：clientid<br/>
     - <code>${username}</code>：用户名

- listeners: <code>[gateway:udp_listeners](#gateway-udp_listeners)</code>



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
  `30s`

  客户端连接过程的空闲时间。该配置用于：
    1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
    2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo 重写。


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

  用于网关发布来自 LwM2M 客户端的确认事件的主题。

- notify: <code>[gateway:translator](#gateway-translator)</code>

  用于发布来自 LwM2M 客户端的通知事件的主题。
  在成功 Observe 到 LwM2M 客户端的资源后，如果客户端报告任何资源状态的变化，网关将通过该主题发送通知事件

- register: <code>[gateway:translator](#gateway-translator)</code>

  用于发布来自 LwM2M 客户端的注册事件的主题。

- update: <code>[gateway:translator](#gateway-translator)</code>

  用于发布来自 LwM2M 客户端的更新事件的主题。


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

  是否启用

- max_delayed_messages: <code>integer()</code>
  * default: 
  `0`

  延迟消息的数量上限(0 代表不限数量)


## opentelemetry
Open Telemetry 工具包配置


**Config paths**

 - <code>opentelemetry</code>


**Env overrides**

 - <code>EMQX_OPENTELEMETRY</code>



**Fields**

- metrics: <code>[opentelemetry:otel_metrics](#opentelemetry-otel_metrics)</code>

  Open Telemetry 指标配置。

- logs: <code>[opentelemetry:otel_logs](#opentelemetry-otel_logs)</code>

  Open Telemetry 日志配置。如果启用，EMQX 将安装一个日志处理器，根据 Open Telemetry 日志数据模型格式化事件，并将它们导出到配置的 Open Telemetry 收集器或后端。

- traces: <code>[opentelemetry:otel_traces](#opentelemetry-otel_traces)</code>

  Open Telemetry 追踪配置。

- exporter: <code>[opentelemetry:otel_exporter](#opentelemetry-otel_exporter)</code>

  Open Telemetry 导出器


## opentelemetry:otel_exporter
Open Telemetry 导出器


**Config paths**

 - <code>opentelemetry.exporter</code>


**Env overrides**

 - <code>EMQX_OPENTELEMETRY__EXPORTER</code>



**Fields**

- endpoint: <code>emqx_schema:url()</code>
  * default: 
  `"http://localhost:4317"`

  导出器将要发送 Open Telemetry 信号数据的目标 URL。

- ssl_options: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  Open Telemetry 导出器的 SSL 配置


## opentelemetry:otel_logs
Open Telemetry 日志配置。如果启用，EMQX 将安装一个日志处理器，根据 Open Telemetry 日志数据模型格式化事件，并将它们导出到配置的 Open Telemetry 收集器或后端。


**Config paths**

 - <code>opentelemetry.logs</code>


**Env overrides**

 - <code>EMQX_OPENTELEMETRY__LOGS</code>



**Fields**

- level: <code>debug | info | notice | warning | error | critical | alert | emergency | all</code>
  * default: 
  `warning`

  Open Telemetry 日志处理器的日志级别。

- enable: <code>boolean()</code>
  * default: 
  `false`

  启用或禁用 open telemetry 指标

- scheduled_delay: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `1s`

  两次连续导出 Open Telemetry 信号之间的延迟间隔。


## opentelemetry:otel_metrics
Open Telemetry 指标配置。


**Config paths**

 - <code>opentelemetry.metrics</code>


**Env overrides**

 - <code>EMQX_OPENTELEMETRY__METRICS</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  启用或禁用 open telemetry 指标

- interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `10s`

  两次连续导出 Open Telemetry 信号之间的延迟间隔。


## opentelemetry:otel_traces
Open Telemetry 追踪配置。


**Config paths**

 - <code>opentelemetry.traces</code>


**Env overrides**

 - <code>EMQX_OPENTELEMETRY__TRACES</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  启用或禁用 open telemetry 指标

- scheduled_delay: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  两次连续导出 Open Telemetry 信号之间的延迟间隔。

- filter: <code>[opentelemetry:trace_filter](#opentelemetry-trace_filter)</code>

  Open Telemetry 追踪过滤器配置


## opentelemetry:trace_filter
Open Telemetry 追踪过滤器配置


**Config paths**

 - <code>opentelemetry.traces.filter</code>


**Env overrides**

 - <code>EMQX_OPENTELEMETRY__TRACES__FILTER</code>



**Fields**

- trace_all: <code>boolean()</code>
  * default: 
  `false`

  如果启用，将追踪所有发布的消息，如果无法从消息中提取追踪 ID，则生成新的追踪 ID。
  否则，只追踪带有追踪上下文的消息。默认禁用。


## prometheus:collectors
虚拟机的内部高级指标最初是禁用的
通常只在性能测试期间启用。
启用它们将增加 CPU 负载。


**Config paths**

 - <code>prometheus.collectors</code>


**Env overrides**

 - <code>EMQX_PROMETHEUS__COLLECTORS</code>



**Fields**

- vm_dist: <code>disabled | enabled</code>
  * default: 
  `disabled`

  开启或关闭 VM 分布采集器，收集 Erlang 分布机制中涉及的套接字和进程的信息。

- mnesia: <code>enabled | disabled</code>
  * default: 
  `disabled`

  开启或关闭 Mnesia 采集器, 使用 mnesia:system_info/1 收集 Mnesia 相关指标

- vm_statistics: <code>enabled | disabled</code>
  * default: 
  `disabled`

  开启或关闭 VM 统计采集器, 使用 erlang:statistics/1 收集 Erlang VM 相关指标

- vm_system_info: <code>enabled | disabled</code>
  * default: 
  `disabled`

  开启或关闭 VM 系统信息采集器, 使用 erlang:system_info/1 收集 Erlang VM 相关指标

- vm_memory: <code>enabled | disabled</code>
  * default: 
  `disabled`

  开启或关闭 VM 内存采集器, 使用 erlang:memory/0 收集 Erlang 虚拟机动态分配的内存信息，同时提供基本的 (D)ETS 统计信息

- vm_msacc: <code>enabled | disabled</code>
  * default: 
  `disabled`

  开启或关闭 VM msacc 采集器, 使用 erlang:statistics(microstate_accounting) 收集微状态计数指标


## prometheus:legacy_deprecated_setting
自5.4.0版本起弃用。


**Config paths**

 - <code>prometheus</code>


**Env overrides**

 - <code>EMQX_PROMETHEUS</code>



**Fields**

- push_gateway_server: <code>string()</code>
  * default: 
  `"http://127.0.0.1:9091"`

  自5.4.0版本起弃用，改用 `prometheus.push_gateway.url`。

- interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  自5.4.0版本起弃用，改用 `prometheus.push_gateway.interval`。

- headers: <code>map(string(), string())</code>
  * default: 
  `{}`

  自5.4.0版本起弃用，改用 `prometheus.push_gateway.headers`。

- job_name: <code>binary()</code>
  * default: 
  `"${name}/instance/${name}~${host}"`

  自5.4.0版本起弃用，改用 `prometheus.push_gateway.job_name`。

- enable: <code>boolean()</code>
  * default: 
  `false`

  自5.4.0版本起弃用，改用 `prometheus.push_gateway.url`。

- vm_dist_collector: <code>disabled | enabled</code>
  * default: 
  `disabled`

  自5.4.0版本起弃用，改用 `prometheus.collectors.vm_dist`。

- mnesia_collector: <code>enabled | disabled</code>
  * default: 
  `disabled`

  自5.4.0版本起弃用，改用 `prometheus.collectors.mnesia`。

- vm_statistics_collector: <code>enabled | disabled</code>
  * default: 
  `disabled`

  自5.4.0版本起弃用，改用 `prometheus.collectors.vm_statistics`。

- vm_system_info_collector: <code>enabled | disabled</code>
  * default: 
  `disabled`

  自5.4.0版本起弃用，改用 `prometheus.collectors.vm_system_info`。

- vm_memory_collector: <code>enabled | disabled</code>
  * default: 
  `disabled`

  自5.4.0版本起弃用，改用 `prometheus.collectors.vm_memory`。

- vm_msacc_collector: <code>enabled | disabled</code>
  * default: 
  `disabled`

  自5.4.0版本起弃用，改用 `prometheus.collectors.vm_msacc`。


## prometheus:push_gateway
Push Gateway 是可选的，如果 Prometheus 要抓取 EMQX，则不应配置。


**Config paths**

 - <code>prometheus.push_gateway</code>


**Env overrides**

 - <code>EMQX_PROMETHEUS__PUSH_GATEWAY</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  启用或禁用 Pushgateway。

- url: <code>string()</code>
  * default: 
  `"http://127.0.0.1:9091"`

  Pushgateway 服务器的 URL。Push Gateway 是可选的，如果 Prometheus 要抓取 EMQX，则不应配置。

- interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  数据推送间隔

- headers: <code>map(string(), string())</code>
  * default: 
  `{}`

  推送到 Push Gateway 的 HTTP Headers 列表。<br/>
  例如，<code> { Authorization = "some-authz-tokens"}</code>

- job_name: <code>binary()</code>
  * default: 
  `"${name}/instance/${name}~${host}"`

  推送到 Push Gateway 的 Job 名称。可用变量为：<br/>
  - ${name}: EMQX 节点的名称。
  - ${host}: EMQX 节点主机名。
  例如，当 EMQX 节点名为 <code>emqx@127.0.0.1</code> 则 name 变量的值为 <code>emqx</code>，host 变量的值为 <code>127.0.0.1</code>。<br/>
  默认值为: <code>${name}/instance/${name}~${host}</code>


## prometheus:recommend_setting
推荐的设置


**Config paths**

 - <code>prometheus</code>


**Env overrides**

 - <code>EMQX_PROMETHEUS</code>



**Fields**

- enable_basic_auth: <code>boolean()</code>
  * default: 
  `false`

  启用或禁用 Prometheus 抓取 API 的基本认证，不适用于 Push Gateway。

- push_gateway: <code>[prometheus:push_gateway](#prometheus-push_gateway)</code>

  Push Gateway 是可选的，如果 Prometheus 要抓取 EMQX，则不应配置。

- collectors: <code>[prometheus:collectors](#prometheus-collectors)</code>

  虚拟机的内部高级指标最初是禁用的
  通常只在性能测试期间启用。
  启用它们将增加 CPU 负载。


## redis:action_resource_opts
资源相关的选项。


**Config paths**

 - <code>actions.redis.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_ACTIONS__REDIS__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- batch_size: <code>pos_integer()</code>
  * default: 
  `1`

  这个参数定义了批处理计数的上限。
  将这个值设置为1将有效地禁用批处理，因为它表示每个批处理将只处理一个项目。
  关于 Redis 集群模式的注意事项：
  在 Redis 集群模式的情况下不支持批处理。
  因此，batch_size 总是设置为1，
  反映了该模式对于批处理操作固有的限制。

- batch_time: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0ms`

  在低消息速率下积累批处理时的最大等待间隔，以实现更高效的资源使用。

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## redis:connector_resource_opts
资源相关的选项。


**Config paths**

 - <code>connectors.redis.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__REDIS__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


## redis:redis_action
与 Redis 连接器交互的动作。


**Config paths**

 - <code>actions.redis.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__REDIS__$NAME</code>



**Fields**

- local_topic: <code>binary()</code>

  MQTT 主题或主题过滤器作为数据源（动作输入）。 如果规则动作用作数据源，则应将此配置留空，否则消息将在远程系统中重复。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用（是）或停用（否）此动作。

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- parameters: <code>[bridge_redis:action_parameters](#bridge_redis-action_parameters)</code>

  动作的参数。

- resource_opts: <code>[redis:action_resource_opts](#redis-action_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## redis:config_connector
Redis 动作的配置。


**Config paths**

 - <code>connectors.redis.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__REDIS__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- parameters: <code>[redis:redis_single_connector](#redis-redis_single_connector) | [redis:redis_sentinel_connector](#redis-redis_sentinel_connector) | [redis:redis_cluster_connector](#redis-redis_cluster_connector)</code>

  特定于某个 Redis 连接器类型的参数集，`redis_type`可以是`single`，`cluster`或`sentinel`之一。

- resource_opts: <code>[redis:connector_resource_opts](#redis-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


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

  Retainer index specifications: list of arrays of positive ascending integers. Each array specifies an index. Numbers in an index specification are 1-based word positions in topics. Words from specified positions will be used for indexing.<br/>For example, it is good to have <code>[2, 4]</code> index to optimize <code>+/X/+/Y/...</code> topic wildcard subscriptions.


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
  `0s`

  消息保留时间。0 代表永久保留

- msg_clear_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0s`

  消息清理间隔。0 代表不进行清理

- max_payload_size: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

  消息大小最大值

- stop_publish_clear_msg: <code>boolean()</code>
  * default: 
  `false`

  当 PUBLISH 消息的保留标志被设置且有效载荷为空时，是否继续发布消息。
  参见：
  http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718038

- delivery_rate: <code>string()</code>
  * default: 
  `"1000/s"`

  发送保留消息的最大速率

- backend: <code>[retainer:mnesia_config](#retainer-mnesia_config)</code>

  保留消息的存储后端


## schema_registry:avro
[Apache Avro](https://avro.apache.org/) 序列化格式。


**Config paths**

 - <code>schema_registry.schemas.$name</code>


**Env overrides**

 - <code>EMQX_SCHEMA_REGISTRY__SCHEMAS__$NAME</code>



**Fields**

- type: <code>avro</code>

  Schema 类型。

- source: <code>emqx_schema:json_binary()</code>

  Schema 的源文本。

- description: <code>binary()</code>
  * default: 
  `""`

  改 schema 的描述。


## schema_registry:protobuf
[Protocol Buffers](https://protobuf.dev/) 序列化格式。


**Config paths**

 - <code>schema_registry.schemas.$name</code>


**Env overrides**

 - <code>EMQX_SCHEMA_REGISTRY__SCHEMAS__$NAME</code>



**Fields**

- type: <code>protobuf</code>

  Schema 类型。

- source: <code>binary()</code>

  Schema 的源文本。

- description: <code>binary()</code>
  * default: 
  `""`

  改 schema 的描述。


## schema_registry
Schema registry 的配置。


**Config paths**

 - <code>schema_registry</code>


**Env overrides**

 - <code>EMQX_SCHEMA_REGISTRY</code>



**Fields**

- schemas: <code>{$name -> [schema_registry:avro](#schema_registry-avro) | [schema_registry:protobuf](#schema_registry-protobuf)}</code>
  * default: 
  `{}`

  已注册的 schema。


## actions_and_sources:action_resource_opts
资源相关的选项。


**Config paths**

 - <code>actions.gcp_pubsub_producer.$name.resource_opts</code>
 - <code>actions.influxdb.$name.resource_opts</code>
 - <code>actions.matrix.$name.resource_opts</code>
 - <code>actions.mysql.$name.resource_opts</code>
 - <code>actions.pgsql.$name.resource_opts</code>
 - <code>actions.timescale.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_ACTIONS__GCP_PUBSUB_PRODUCER__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_ACTIONS__INFLUXDB__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_ACTIONS__MATRIX__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_ACTIONS__MYSQL__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_ACTIONS__PGSQL__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_ACTIONS__TIMESCALE__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- batch_size: <code>pos_integer()</code>
  * default: 
  `1`

  最大批量请求大小。如果设为 1，则无批处理。

- batch_time: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## actions_and_sources:actions
数据桥接的配置信息


**Config paths**

 - <code>actions</code>


**Env overrides**

 - <code>EMQX_ACTIONS</code>



**Fields**

- http: <code>{$name -> [bridge_http:http_action](#bridge_http-http_action)}</code>

  HTTP Action Config

- mysql: <code>{$name -> [bridge_mysql:mysql_action](#bridge_mysql-mysql_action)}</code>

  与 MySQL 连接器交互的动作

- mongodb: <code>{$name -> [bridge_mongodb:mongodb_action](#bridge_mongodb-mongodb_action)}</code>

  MongoDB Action Config

- redis: <code>{$name -> [redis:redis_action](#redis-redis_action)}</code>

  Redis Action Config

- mqtt: <code>{$name -> [bridge_mqtt_publisher:mqtt_publisher_action](#bridge_mqtt_publisher-mqtt_publisher_action)}</code>

  MQTT Publisher Action Config

- azure_event_hub_producer: <code>{$name -> [bridge_azure_event_hub:actions](#bridge_azure_event_hub-actions)}</code>

  Azure Event Hub Actions Config

- confluent_producer: <code>{$name -> [confluent:actions](#confluent-actions)}</code>

  Confluent Actions Config

- elasticsearch: <code>{$action_name -> [bridge_elasticsearch:action_config](#bridge_elasticsearch-action_config)}</code>

  Elasticsearch Bridge

- gcp_pubsub_producer: <code>{$name -> [gcp_pubsub_producer:producer_action](#gcp_pubsub_producer-producer_action)}</code>

  GCP PubSub Producer Action Config

- influxdb: <code>{$name -> [bridge_influxdb:influxdb_action](#bridge_influxdb-influxdb_action)}</code>

  InfluxDB Action Config

- iotdb: <code>{$name -> [bridge_iotdb:action_config](#bridge_iotdb-action_config)}</code>

  IoTDB Action Config

- kafka_producer: <code>{$name -> [bridge_kafka:kafka_producer_action](#bridge_kafka-kafka_producer_action)}</code>

  Kafka Producer Action Config

- matrix: <code>{$name -> [bridge_pgsql:pgsql_action](#bridge_pgsql-pgsql_action)}</code>

  Matrix Action Config

- pgsql: <code>{$name -> [bridge_pgsql:pgsql_action](#bridge_pgsql-pgsql_action)}</code>

  PostgreSQL Action Config

- syskeeper_forwarder: <code>{$name -> [syskeeper:config](#syskeeper-config)}</code>

  Syskeeper Forwarder Action Config

- timescale: <code>{$name -> [bridge_pgsql:pgsql_action](#bridge_pgsql-pgsql_action)}</code>

  Timescale Action Config


## actions_and_sources:sources
Configuration for sources.


**Config paths**

 - <code>sources</code>


**Env overrides**

 - <code>EMQX_SOURCES</code>



**Fields**

- mqtt: <code>{$name -> [bridge_mqtt_publisher:mqtt_subscriber_source](#bridge_mqtt_publisher-mqtt_subscriber_source)}</code>

  MQTT Subscriber Source Config


## authn:http_get
使用 HTTP Server 作为认证服务的认证器的配置项 (使用 GET 请求)。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- method: <code>get</code>

  HTTP 请求方法。

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    cache-control = no-cache
    connection = keep-alive
    keep-alive = "timeout=30, max=1000"
  }
  ```

  HTTP Headers 列表 (无 <code>content-type</code>) 。

- mechanism: <code>password_based</code>

  认证方式。

- backend: <code>http</code>

  后端类型。

- url: <code>binary()</code>

  认证 HTTP 服务器地址。

- body: <code>map()</code>

  HTTP 请求体。

- request_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `5s`

  HTTP 请求超时时长。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。

- request: <code>[connector_http:request](#connector_http-request)</code>

  配置 HTTP 请求参数。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- max_retries: <code>non_neg_integer()</code>

  Deprecated since 5.0.4.

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  连接到 HTTP 服务器的超时时间。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  一个正整数。是否连续发送 HTTP 请求，当设置为1时，意味着在发送每个 HTTP 请求后，需要等待服务器返回，然后继续发送下一个请求。

- retry_interval: <code>emqx_schema:timeout_duration()</code>

  Deprecated since 5.0.4.


## authn:http_post
使用 HTTP Server 作为认证服务的认证器的配置项 (使用 POST 请求)。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- method: <code>post</code>

  HTTP 请求方法。

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    cache-control = no-cache
    connection = keep-alive
    content-type = "application/json"
    keep-alive = "timeout=30, max=1000"
  }
  ```

  HTTP Headers 列表

- mechanism: <code>password_based</code>

  认证方式。

- backend: <code>http</code>

  后端类型。

- url: <code>binary()</code>

  认证 HTTP 服务器地址。

- body: <code>map()</code>

  HTTP 请求体。

- request_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `5s`

  HTTP 请求超时时长。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。

- request: <code>[connector_http:request](#connector_http-request)</code>

  配置 HTTP 请求参数。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- max_retries: <code>non_neg_integer()</code>

  Deprecated since 5.0.4.

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  连接到 HTTP 服务器的超时时间。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  一个正整数。是否连续发送 HTTP 请求，当设置为1时，意味着在发送每个 HTTP 请求后，需要等待服务器返回，然后继续发送下一个请求。

- retry_interval: <code>emqx_schema:timeout_duration()</code>

  Deprecated since 5.0.4.


## authn:jwt_hmac
用于认证的 JWT 使用 HMAC 算法签发时的配置。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- algorithm: <code>hmac-based</code>

  JWT 签名算法，支持 HMAC (配置为 <code>hmac-based</code>）和 RSA、ECDSA (配置为 <code>public-key</code>)。

- secret: <code>binary()</code>

  使用 HMAC 算法时用于验证 JWT 的密钥

- secret_base64_encoded: <code>boolean()</code>
  * default: 
  `false`

  密钥是否为 base64 编码。

- mechanism: <code>jwt</code>

  认证方式。

- acl_claim_name: <code>binary()</code>
  * default: 
  `acl`

  用于获取 ACL 规则的 JWT 声明名称。

- verify_claims: <code>map()</code>
  * default: 
  `[]`

  需要验证的自定义声明列表，是一个由名称/值对组成的列表。指定一个键(Key)来查找 JWT 中对应的声明(Claim)，并提供一个预期值(Expected Value)来与声明的实际值进行比较，以确保只有满足特定条件的 JWT 才能被接受和使用。
  例如要求 JWT 中的特定声明(如 clientid)的值必须与当前连接的客户端 ID 相匹配。
  预期值可以使用以下占位符：
  - <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端 ID
  身份认证将确认 JWT 中的声明值（从密码字段中获取）与 <code>verify_claims</code> 中要求的内容是否匹配。

- from: <code>username | password</code>
  * default: 
  `password`

  指定客户端连接请求中 JWT 的位置。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。


## authn:jwt_jwks
用于认证的 JWTs 需要从 JWKS 端点获取时的配置。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- use_jwks: <code>true</code>

  是否使用 JWKS。

- endpoint: <code>string()</code>

  JWKS 端点， 它是一个以 JWKS 格式返回服务端的公钥集的只读端点。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- refresh_interval: <code>integer()</code>
  * default: 
  `300`

  JWKS 刷新间隔。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL 选项。

- mechanism: <code>jwt</code>

  认证方式。

- acl_claim_name: <code>binary()</code>
  * default: 
  `acl`

  用于获取 ACL 规则的 JWT 声明名称。

- verify_claims: <code>map()</code>
  * default: 
  `[]`

  需要验证的自定义声明列表，是一个由名称/值对组成的列表。指定一个键(Key)来查找 JWT 中对应的声明(Claim)，并提供一个预期值(Expected Value)来与声明的实际值进行比较，以确保只有满足特定条件的 JWT 才能被接受和使用。
  例如要求 JWT 中的特定声明(如 clientid)的值必须与当前连接的客户端 ID 相匹配。
  预期值可以使用以下占位符：
  - <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端 ID
  身份认证将确认 JWT 中的声明值（从密码字段中获取）与 <code>verify_claims</code> 中要求的内容是否匹配。

- from: <code>username | password</code>
  * default: 
  `password`

  指定客户端连接请求中 JWT 的位置。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。


## authn:jwt_public_key
用于认证的 JWT 使用 RSA 或 ECDSA 算法签发时的配置。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- algorithm: <code>public-key</code>

  JWT 签名算法，支持 HMAC (配置为 <code>hmac-based</code>）和 RSA、ECDSA (配置为 <code>public-key</code>)。

- public_key: <code>string()</code>

  用于验证 JWT 的公钥。

- mechanism: <code>jwt</code>

  认证方式。

- acl_claim_name: <code>binary()</code>
  * default: 
  `acl`

  用于获取 ACL 规则的 JWT 声明名称。

- verify_claims: <code>map()</code>
  * default: 
  `[]`

  需要验证的自定义声明列表，是一个由名称/值对组成的列表。指定一个键(Key)来查找 JWT 中对应的声明(Claim)，并提供一个预期值(Expected Value)来与声明的实际值进行比较，以确保只有满足特定条件的 JWT 才能被接受和使用。
  例如要求 JWT 中的特定声明(如 clientid)的值必须与当前连接的客户端 ID 相匹配。
  预期值可以使用以下占位符：
  - <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端 ID
  身份认证将确认 JWT 中的声明值（从密码字段中获取）与 <code>verify_claims</code> 中要求的内容是否匹配。

- from: <code>username | password</code>
  * default: 
  `password`

  指定客户端连接请求中 JWT 的位置。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。


## authn:bind_method
通过 LDAP 绑定操作进行认证。


**Config paths**

 - <code>authentication.$INDEX.method</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__METHOD</code>



**Fields**

- type: <code>bind</code>
  * default: 
  `bind`

  认证方式类型。

- bind_password: <code>binary()</code>
  * default: 
  `"${password}"`

  绑定密码的模版


## authn:hash_method
通过将本地密码与经过由`密码属性`指定的算法加密的密码进行比对来进行认证。


**Config paths**

 - <code>authentication.$INDEX.method</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__METHOD</code>



**Fields**

- type: <code>hash</code>
  * default: 
  `hash`

  认证方式类型。

- password_attribute: <code>string()</code>
  * default: 
  `userPassword`

  指示哪个属性用于表示用户密码。

- is_superuser_attribute: <code>string()</code>
  * default: 
  `isSuperuser`

  指示哪个属性用于表示用户是否为超级用户。


## authn:ldap
使用 LDAP 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证方式。

- backend: <code>ldap</code>

  后端类型。

- query_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  LDAP 查询的超时时间。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。

- server: <code>string()</code>

  要连接的 IPv4 或 IPv6 地址或主机名。<br/>
  主机名条目的格式为：`主机[:端口]`。<br/>
  如果 `[:端口]` 未指定， 将使用 LDAP 默认端口 389。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- base_dn: <code>binary()</code>

  与基本对象条目（或根）相关的名称。
  搜索用户的起点。

- filter: <code>binary()</code>
  * default: 
  `"(objectClass=mqttUser)"`

  定义哪些条件必须被依次满足的过滤器
  用于搜索匹配一条给定的条目.<br>
  筛选器的语法遵循 RFC 4515，并且还支持占位符。

- request_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `10s`

  设置每个单独请求所使用的最大时间（以毫秒为单位）。

- ssl: <code>[ldap:ssl](#ldap-ssl)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- method: <code>[authn:hash_method](#authn-hash_method) | [authn:bind_method](#authn-bind_method)</code>

  认证方式。


## authn:ldap_deprecated
这是一种已弃用的形式，应避免使用。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证方式。

- backend: <code>ldap</code>

  后端类型。

- query_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  LDAP 查询的超时时间。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。

- server: <code>string()</code>

  要连接的 IPv4 或 IPv6 地址或主机名。<br/>
  主机名条目的格式为：`主机[:端口]`。<br/>
  如果 `[:端口]` 未指定， 将使用 LDAP 默认端口 389。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- base_dn: <code>binary()</code>

  与基本对象条目（或根）相关的名称。
  搜索用户的起点。

- filter: <code>binary()</code>
  * default: 
  `"(objectClass=mqttUser)"`

  定义哪些条件必须被依次满足的过滤器
  用于搜索匹配一条给定的条目.<br>
  筛选器的语法遵循 RFC 4515，并且还支持占位符。

- request_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `10s`

  设置每个单独请求所使用的最大时间（以毫秒为单位）。

- ssl: <code>[ldap:ssl](#ldap-ssl)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- password_attribute: <code>string()</code>
  * default: 
  `userPassword`

  指示哪个属性用于表示用户密码。

- is_superuser_attribute: <code>string()</code>
  * default: 
  `isSuperuser`

  指示哪个属性用于表示用户是否为超级用户。


## authn:builtin_db
使用内置数据库作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- password_hash_algorithm: <code>[authn_hash:bcrypt_rw](#authn_hash-bcrypt_rw) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash creation and verification.

- mechanism: <code>password_based</code>

  认证方式。

- backend: <code>built_in_database</code>

  后端类型。

- user_id_type: <code>clientid | username</code>
  * default: 
  `username`

  指定用于客户端身份 ID 认证的字段。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。


## authn:mongo_rs
使用 MongoDB (Replica Set) 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证方式。

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
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端 ID

- password_hash_field: <code>binary()</code>
  * default: 
  `password_hash`

  存储密码散列值字段。

- salt_field: <code>binary()</code>
  * default: 
  `salt`

  用于存储盐值的字段。

- is_superuser_field: <code>binary()</code>
  * default: 
  `is_superuser`

  定义用户是否具有超级用户权限的字段。

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。

- mongo_type: <code>rs</code>
  * default: 
  `rs`

  副本集。当MongoDB服务器以`副本集`模式运行时，必须设置为`rs`。

- servers: <code>string()</code>

  集群连接的节点列表。节点应使用逗号分隔，例如：节点[,节点]。
  对于每个节点，应该是要连接的 IPv4 或 IPv6 地址或主机名。
  主机条目具有以下形式：主机[:端口]。
  如果未指定[:端口]，则使用 MongoDB 的默认端口27017。

- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写入模式

- r_mode: <code>master | slave_ok</code>
  * default: 
  `master`

  读取模式。

- replica_set_name: <code>binary()</code>

  副本集的名称。

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  是否使用 MongoDB 的传统协议与数据库通信。默认情况下，将尝试自动确定是否支持较新的协议。

- auth_source: <code>binary()</code>

  与用户认证信息关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[mongo:topology](#mongo-topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn:mongo_sharded
使用 MongoDB (Sharded Cluster) 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证方式。

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
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端 ID

- password_hash_field: <code>binary()</code>
  * default: 
  `password_hash`

  存储密码散列值字段。

- salt_field: <code>binary()</code>
  * default: 
  `salt`

  用于存储盐值的字段。

- is_superuser_field: <code>binary()</code>
  * default: 
  `is_superuser`

  定义用户是否具有超级用户权限的字段。

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。

- mongo_type: <code>sharded</code>
  * default: 
  `sharded`

  分片集群。当 MongoDB 服务器以`分片`模式运行时，必须设置为`sharded`。

- servers: <code>string()</code>

  集群连接的节点列表。节点应使用逗号分隔，例如：节点[,节点]。
  对于每个节点，应该是要连接的 IPv4 或 IPv6 地址或主机名。
  主机条目具有以下形式：主机[:端口]。
  如果未指定[:端口]，则使用 MongoDB 的默认端口27017。

- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写入模式

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  是否使用 MongoDB 的传统协议与数据库通信。默认情况下，将尝试自动确定是否支持较新的协议。

- auth_source: <code>binary()</code>

  与用户认证信息关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[mongo:topology](#mongo-topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn:mongo_single
使用 MongoDB (Standalone) 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证方式。

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
  - <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端 ID

- password_hash_field: <code>binary()</code>
  * default: 
  `password_hash`

  存储密码散列值字段。

- salt_field: <code>binary()</code>
  * default: 
  `salt`

  用于存储盐值的字段。

- is_superuser_field: <code>binary()</code>
  * default: 
  `is_superuser`

  定义用户是否具有超级用户权限的字段。

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。

- mongo_type: <code>single</code>
  * default: 
  `single`

  独立实例。当MongoDB服务器以独立模式运行时，必须设置为`single`。

- server: <code>string()</code>

  要连接的 IPv4 或 IPv6 地址或主机名。<br/>主机条目具有以下形式：主机[:端口]。<br/>如果未指定[:端口]，则使用MongoDB的默认端口27017。

- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写入模式

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  是否使用 MongoDB 的传统协议与数据库通信。默认情况下，将尝试自动确定是否支持较新的协议。

- auth_source: <code>binary()</code>

  与用户认证信息关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[mongo:topology](#mongo-topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn:mysql
使用 MySQL 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证方式。

- backend: <code>mysql</code>

  后端类型。

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- query: <code>string()</code>

  用于查询密码散列等用于认证的数据的 SQL 语句。

- query_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `5s`

  SQL 查询的超时时间。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。

- server: <code>string()</code>

  用于连接的 IPv4 或 IPv6 地址或主机名。<br/>
  主机条目的格式如下：主机[:端口]。<br/>
  如果未指定[:端口]，将使用 MySQL 的默认端口3306。

- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>
  * default: 
  `root`

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn:postgresql
使用 PostgreSQL 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证方式。

- backend: <code>postgresql</code>

  后端类型。

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- query: <code>string()</code>

  用于查询密码散列等用于认证的数据的 SQL 语句。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。

- server: <code>string()</code>

  要连接的 IPv4 或 IPv6 地址或主机名。<br/>
  一个主机条目的格式为：Host[:Port]。<br/>
  如果没有指定 [:Port]，将使用 PostgreSQL 默认端口 5432。

- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn:redis_cluster
使用 Redis (Cluster) 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证方式。

- backend: <code>redis</code>

  后端类型。

- cmd: <code>binary()</code>

  用于查询密码散列等用于认证的数据的 Redis 命令，目前仅支持 <code>HGET</code> 与 <code>HMGET</code>。

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。

- servers: <code>string()</code>

  集群将要连接的节点列表。 节点之间用逗号分隔，如：Node[,Node]。每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。主机名具有以下形式：Host[:Port]。如果未指定 [:Port]，则使用 Redis 默认端口 6379。

- redis_type: <code>cluster</code>
  * default: 
  `cluster`

  Cluster 模式。当 Redis 服务器在集群模式下运行时必须设置为'cluster'。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn:redis_sentinel
使用 Redis (Sentinel) 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证方式。

- backend: <code>redis</code>

  后端类型。

- cmd: <code>binary()</code>

  用于查询密码散列等用于认证的数据的 Redis 命令，目前仅支持 <code>HGET</code> 与 <code>HMGET</code>。

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。

- servers: <code>string()</code>

  集群将要连接的节点列表。 节点之间用逗号分隔，如：Node[,Node]。每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。主机名具有以下形式：Host[:Port]。如果未指定 [:Port]，则使用 Redis 默认端口 6379。

- redis_type: <code>sentinel</code>
  * default: 
  `sentinel`

  Sentinel 模式。 当 Redis 服务器在 Senitel 模式下运行时必须设置为 'sentinel' 。

- sentinel: <code>string()</code>

  Redis sentinel 模式下的集群名称。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- database: <code>non_neg_integer()</code>
  * default: 
  `0`

  Redis 数据库 ID。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn:redis_single
使用 Redis (Standalone) 作为认证数据源的认证器的配置项。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  认证方式。

- backend: <code>redis</code>

  后端类型。

- cmd: <code>binary()</code>

  用于查询密码散列等用于认证的数据的 Redis 命令，目前仅支持 <code>HGET</code> 与 <code>HMGET</code>。

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。

- server: <code>string()</code>

  将要连接的 IPv4 或 IPv6 地址，或者主机名。主机名具有以下形式：Host[:Port]。如果未指定 [:Port]，则使用 Redis 默认端口 6379。

- redis_type: <code>single</code>
  * default: 
  `single`

  Single 模式。 当 Redis 服务器在 Single 模式下运行时必须设置为 'single' 。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- database: <code>non_neg_integer()</code>
  * default: 
  `0`

  Redis 数据库 ID。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authn:scram
Settings for Salted Challenge Response Authentication Mechanism
(SCRAM) authentication.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>scram</code>

  认证方式。

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

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。


## authn:gcp_device
使用 GCP 设备作为认证数据源的认证器配置。


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>gcp_device</code>

  认证方式。

- enable: <code>boolean()</code>
  * default: 
  `true`

  设为 <code>true</code> 或 <code>false</code> 以禁用此认证数据源。


## authn_hash:bcrypt
Settings for bcrypt password hashing algorithm.


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>bcrypt</code>

  BCRYPT password hashing.


## authn_hash:bcrypt_rw
Settings for bcrypt password hashing algorithm (for DB backends with write capability).


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>bcrypt</code>

  BCRYPT password hashing.

- salt_rounds: <code>5..10</code>
  * default: 
  `10`

  Work factor for BCRYPT password generation.


## authn_hash:pbkdf2
Settings for PBKDF2 password hashing algorithm.


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>pbkdf2</code>

  PBKDF2 password hashing.

- mac_fun: <code>md4 | md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512</code>

  Specifies mac_fun for PBKDF2 hashing algorithm.

- iterations: <code>pos_integer()</code>

  Iteration count for PBKDF2 hashing algorithm.

- dk_length: <code>integer()</code>

  Derived length for PBKDF2 hashing algorithm. If not specified, calculated automatically based on `mac_fun`.


## authn_hash:simple
Settings for simple algorithms.


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>plain | md5 | sha | sha256 | sha512</code>

  Simple password hashing algorithm.

- salt_position: <code>disable | prefix | suffix</code>
  * default: 
  `prefix`

  Salt position for PLAIN, MD5, SHA, SHA256 and SHA512 algorithms.


## authz:file
使用 ACL 文件授权。


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

  设置为 <code>true</code> 或 <code>false</code> 来禁用此 ACL 提供者

- path: <code>string()</code>

  包含 ACL 规则的文件路径。
  如果该文件在启动 EMQX 节点之前已经配置好，
  只要 EMQX 有读取权限，它可以放置在任何位置。
  即，EMQX 将把它视为只读。

  如果规则集是从 EMQX Dashboard 或 HTTP API 创建或更新的，
  将创建一个新文件并放置在 EMQX 的 data_dir 中的 authz 子目录下，
  旧文件将不再使用。


## authz:http_get
使用外部 HTTP 服务器授权(GET 请求)。


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

  设置为 <code>true</code> 或 <code>false</code> 来禁用此 ACL 提供者

- url: <code>binary()</code>

  认证服务器地址

- request_timeout: <code>string()</code>
  * default: 
  `30s`

  HTTP 请求超时。

- body: <code>{$name -> binary()}</code>

  HTTP 请求体

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  连接到 HTTP 服务器的超时时间。

- max_retries: <code>non_neg_integer()</code>

  Deprecated since 5.0.4.

- retry_interval: <code>emqx_schema:timeout_duration()</code>

  Deprecated since 5.0.4.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  一个正整数。是否连续发送 HTTP 请求，当设置为1时，意味着在发送每个 HTTP 请求后，需要等待服务器返回，然后继续发送下一个请求。

- request: <code>[connector_http:request](#connector_http-request)</code>

  配置 HTTP 请求参数。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- method: <code>get</code>

  HTTP 请求方法

- headers: <code>map(binary(), binary())</code>
  * default: 

  ```
  {
    accept = "application/json"
    cache-control = no-cache
    connection = keep-alive
    keep-alive = "timeout=30, max=1000"
  }
  ```

  HTTP Headers 列表 (无 <code>content-type</code>) 。


## authz:http_post
使用外部 HTTP 服务器授权(POST 请求)。


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

  设置为 <code>true</code> 或 <code>false</code> 来禁用此 ACL 提供者

- url: <code>binary()</code>

  认证服务器地址

- request_timeout: <code>string()</code>
  * default: 
  `30s`

  HTTP 请求超时。

- body: <code>{$name -> binary()}</code>

  HTTP 请求体

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  连接到 HTTP 服务器的超时时间。

- max_retries: <code>non_neg_integer()</code>

  Deprecated since 5.0.4.

- retry_interval: <code>emqx_schema:timeout_duration()</code>

  Deprecated since 5.0.4.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  一个正整数。是否连续发送 HTTP 请求，当设置为1时，意味着在发送每个 HTTP 请求后，需要等待服务器返回，然后继续发送下一个请求。

- request: <code>[connector_http:request](#connector_http-request)</code>

  配置 HTTP 请求参数。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- method: <code>post</code>

  HTTP 请求方法

- headers: <code>map(binary(), binary())</code>
  * default: 

  ```
  {
    accept = "application/json"
    cache-control = no-cache
    connection = keep-alive
    content-type = "application/json"
    keep-alive = "timeout=30, max=1000"
  }
  ```

  HTTP Headers 列表


## authz:ldap
AuthZ with LDAP


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>ldap</code>

  数据后端类型

- enable: <code>boolean()</code>
  * default: 
  `true`

  设置为 <code>true</code> 或 <code>false</code> 来禁用此 ACL 提供者

- publish_attribute: <code>string()</code>
  * default: 
  `mqttPublishTopic`

  表示使用哪个属性来表示允许`发布`的主题列表。

- subscribe_attribute: <code>string()</code>
  * default: 
  `mqttSubscriptionTopic`

  表示使用哪个属性来表示允许`订阅`的主题列表。

- all_attribute: <code>string()</code>
  * default: 
  `mqttPubSubTopic`

  表示使用哪个属性来表示允许`发布`和`订阅`的主题列表。

- query_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  LDAP 查询超时。

- server: <code>string()</code>

  要连接的 IPv4 或 IPv6 地址或主机名。<br/>
  主机名条目的格式为：`主机[:端口]`。<br/>
  如果 `[:端口]` 未指定， 将使用 LDAP 默认端口 389。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- base_dn: <code>binary()</code>

  与基本对象条目（或根）相关的名称。
  搜索用户的起点。

- filter: <code>binary()</code>
  * default: 
  `"(objectClass=mqttUser)"`

  定义哪些条件必须被依次满足的过滤器
  用于搜索匹配一条给定的条目.<br>
  筛选器的语法遵循 RFC 4515，并且还支持占位符。

- request_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `10s`

  设置每个单独请求所使用的最大时间（以毫秒为单位）。

- ssl: <code>[ldap:ssl](#ldap-ssl)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authz:builtin_db
使用内置数据库 (mnesia) 进行授权。


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

  设置为 <code>true</code> 或 <code>false</code> 来禁用此 ACL 提供者


## authz:mongo_rs
使用 MongoDB 授权（副本集模式）


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

  设置为 <code>true</code> 或 <code>false</code> 来禁用此 ACL 提供者

- collection: <code>binary()</code>

  包含授权数据的 `MongoDB` 集合。

- filter: <code>map()</code>
  * default: 
  `{}`

  定义查询中过滤条件的条件表达式。
  过滤器支持以下占位符<br/>
   - <code>${username}</code>：在连接时将用客户端使用的 <code>用户名</code> 替换<br/>
   - <code>${clientid}</code>：在连接时将用客户端使用的 <code>客户端 ID</code> 替换

- mongo_type: <code>rs</code>
  * default: 
  `rs`

  副本集。当MongoDB服务器以`副本集`模式运行时，必须设置为`rs`。

- servers: <code>string()</code>

  集群连接的节点列表。节点应使用逗号分隔，例如：节点[,节点]。
  对于每个节点，应该是要连接的 IPv4 或 IPv6 地址或主机名。
  主机条目具有以下形式：主机[:端口]。
  如果未指定[:端口]，则使用 MongoDB 的默认端口27017。

- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写入模式

- r_mode: <code>master | slave_ok</code>
  * default: 
  `master`

  读取模式。

- replica_set_name: <code>binary()</code>

  副本集的名称。

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  是否使用 MongoDB 的传统协议与数据库通信。默认情况下，将尝试自动确定是否支持较新的协议。

- auth_source: <code>binary()</code>

  与用户认证信息关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[mongo:topology](#mongo-topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authz:mongo_sharded
使用 MongoDB 授权（分片集群模式）。


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

  设置为 <code>true</code> 或 <code>false</code> 来禁用此 ACL 提供者

- collection: <code>binary()</code>

  包含授权数据的 `MongoDB` 集合。

- filter: <code>map()</code>
  * default: 
  `{}`

  定义查询中过滤条件的条件表达式。
  过滤器支持以下占位符<br/>
   - <code>${username}</code>：在连接时将用客户端使用的 <code>用户名</code> 替换<br/>
   - <code>${clientid}</code>：在连接时将用客户端使用的 <code>客户端 ID</code> 替换

- mongo_type: <code>sharded</code>
  * default: 
  `sharded`

  分片集群。当 MongoDB 服务器以`分片`模式运行时，必须设置为`sharded`。

- servers: <code>string()</code>

  集群连接的节点列表。节点应使用逗号分隔，例如：节点[,节点]。
  对于每个节点，应该是要连接的 IPv4 或 IPv6 地址或主机名。
  主机条目具有以下形式：主机[:端口]。
  如果未指定[:端口]，则使用 MongoDB 的默认端口27017。

- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写入模式

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  是否使用 MongoDB 的传统协议与数据库通信。默认情况下，将尝试自动确定是否支持较新的协议。

- auth_source: <code>binary()</code>

  与用户认证信息关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[mongo:topology](#mongo-topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authz:mongo_single
使用 MongoDB 授权（单实例）。


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

  设置为 <code>true</code> 或 <code>false</code> 来禁用此 ACL 提供者

- collection: <code>binary()</code>

  包含授权数据的 `MongoDB` 集合。

- filter: <code>map()</code>
  * default: 
  `{}`

  定义查询中过滤条件的条件表达式。
  过滤器支持以下占位符<br/>
   - <code>${username}</code>：在连接时将用客户端使用的 <code>用户名</code> 替换<br/>
   - <code>${clientid}</code>：在连接时将用客户端使用的 <code>客户端 ID</code> 替换

- mongo_type: <code>single</code>
  * default: 
  `single`

  独立实例。当MongoDB服务器以独立模式运行时，必须设置为`single`。

- server: <code>string()</code>

  要连接的 IPv4 或 IPv6 地址或主机名。<br/>主机条目具有以下形式：主机[:端口]。<br/>如果未指定[:端口]，则使用MongoDB的默认端口27017。

- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写入模式

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  是否使用 MongoDB 的传统协议与数据库通信。默认情况下，将尝试自动确定是否支持较新的协议。

- auth_source: <code>binary()</code>

  与用户认证信息关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[mongo:topology](#mongo-topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## authz:mysql
使用 MySQL 数据库进行授权。


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

  设置为 <code>true</code> 或 <code>false</code> 来禁用此 ACL 提供者

- server: <code>string()</code>

  用于连接的 IPv4 或 IPv6 地址或主机名。<br/>
  主机条目的格式如下：主机[:端口]。<br/>
  如果未指定[:端口]，将使用 MySQL 的默认端口3306。

- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>
  * default: 
  `root`

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- prepare_statement: <code>map()</code>

  SQL 预处理语句列表。

- query: <code>binary()</code>

  用于检索授权数据的数据库查询。


## authz:postgresql
使用 PostgreSQL 数据库进行授权。


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

  设置为 <code>true</code> 或 <code>false</code> 来禁用此 ACL 提供者

- server: <code>string()</code>

  要连接的 IPv4 或 IPv6 地址或主机名。<br/>
  一个主机条目的格式为：Host[:Port]。<br/>
  如果没有指定 [:Port]，将使用 PostgreSQL 默认端口 5432。

- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- prepare_statement: <code>map()</code>

  SQL 预处理语句列表。

- query: <code>binary()</code>

  用于检索授权数据的数据库查询。


## authz:redis_cluster
使用 Redis 集群进行授权。


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

  设置为 <code>true</code> 或 <code>false</code> 来禁用此 ACL 提供者

- servers: <code>string()</code>

  集群将要连接的节点列表。 节点之间用逗号分隔，如：Node[,Node]。每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。主机名具有以下形式：Host[:Port]。如果未指定 [:Port]，则使用 Redis 默认端口 6379。

- redis_type: <code>cluster</code>
  * default: 
  `cluster`

  Cluster 模式。当 Redis 服务器在集群模式下运行时必须设置为'cluster'。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- cmd: <code>binary()</code>

  用于检索授权数据的数据库查询。


## authz:redis_sentinel
使用 Redis Sentinel 进行授权。


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

  设置为 <code>true</code> 或 <code>false</code> 来禁用此 ACL 提供者

- servers: <code>string()</code>

  集群将要连接的节点列表。 节点之间用逗号分隔，如：Node[,Node]。每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。主机名具有以下形式：Host[:Port]。如果未指定 [:Port]，则使用 Redis 默认端口 6379。

- redis_type: <code>sentinel</code>
  * default: 
  `sentinel`

  Sentinel 模式。 当 Redis 服务器在 Senitel 模式下运行时必须设置为 'sentinel' 。

- sentinel: <code>string()</code>

  Redis sentinel 模式下的集群名称。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- database: <code>non_neg_integer()</code>
  * default: 
  `0`

  Redis 数据库 ID。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- cmd: <code>binary()</code>

  用于检索授权数据的数据库查询。


## authz:redis_single
使用单个 Redis 实例进行授权。


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

  设置为 <code>true</code> 或 <code>false</code> 来禁用此 ACL 提供者

- server: <code>string()</code>

  将要连接的 IPv4 或 IPv6 地址，或者主机名。主机名具有以下形式：Host[:Port]。如果未指定 [:Port]，则使用 Redis 默认端口 6379。

- redis_type: <code>single</code>
  * default: 
  `single`

  Single 模式。 当 Redis 服务器在 Single 模式下运行时必须设置为 'single' 。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- database: <code>non_neg_integer()</code>
  * default: 
  `0`

  Redis 数据库 ID。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- cmd: <code>binary()</code>

  用于检索授权数据的数据库查询。


## bridge:bridges
MQTT 数据桥接配置


**Config paths**

 - <code>bridges</code>


**Env overrides**

 - <code>EMQX_BRIDGES</code>



**Fields**

- webhook: <code>{$name -> [bridge_http:config](#bridge_http-config)}</code>

  转发消息到 HTTP 服务器的 WebHook

- mqtt: <code>{$name -> [bridge_mqtt:config](#bridge_mqtt-config)}</code>

  桥接到另一个 MQTT Broker 的 MQTT 数据桥接

- hstreamdb: <code>{$name -> [bridge_hstreamdb:config](#bridge_hstreamdb-config)}</code>

  HStreamDB Bridge Config

- mysql: <code>{$name -> [bridge_mysql:config](#bridge_mysql-config)}</code>

  MySQL Bridge Config

- tdengine: <code>{$name -> [bridge_tdengine:config](#bridge_tdengine-config)}</code>

  TDengine Bridge Config

- dynamo: <code>{$name -> [bridge_dynamo:config](#bridge_dynamo-config)}</code>

  Dynamo Bridge Config

- rocketmq: <code>{$name -> [bridge_rocketmq:config](#bridge_rocketmq-config)}</code>

  RocketMQ Bridge Config

- cassandra: <code>{$name -> [bridge_cassa:config](#bridge_cassa-config)}</code>

  Cassandra Bridge Config

- opents: <code>{$name -> [bridge_opents:config](#bridge_opents-config)}</code>

  OpenTSDB Bridge Config

- oracle: <code>{$name -> [bridge_oracle:config](#bridge_oracle-config)}</code>

  Oracle Bridge Config

- iotdb: <code>{$name -> [bridge_iotdb:config](#bridge_iotdb-config)}</code>

  Apache IoTDB Bridge Config

- kafka: <code>{$name -> [bridge_kafka:kafka_producer](#bridge_kafka-kafka_producer)}</code>

  Kafka Producer Bridge Config

- kafka_consumer: <code>{$name -> [bridge_kafka:kafka_consumer](#bridge_kafka-kafka_consumer)}</code>

  Kafka Consumer Bridge Config

- pulsar_producer: <code>{$name -> [bridge_pulsar:pulsar_producer](#bridge_pulsar-pulsar_producer)}</code>

  Pulsar Producer Bridge Config

- gcp_pubsub: <code>{$name -> [bridge_gcp_pubsub:config_producer](#bridge_gcp_pubsub-config_producer)}</code>

  EMQX Enterprise Config

- gcp_pubsub_consumer: <code>{$name -> [bridge_gcp_pubsub:config_consumer](#bridge_gcp_pubsub-config_consumer)}</code>

  EMQX Enterprise Config

- mongodb_rs: <code>{$name -> [bridge_mongodb:mongodb_rs](#bridge_mongodb-mongodb_rs)}</code>

  MongoDB Bridge Config

- mongodb_sharded: <code>{$name -> [bridge_mongodb:mongodb_sharded](#bridge_mongodb-mongodb_sharded)}</code>

  MongoDB Bridge Config

- mongodb_single: <code>{$name -> [bridge_mongodb:mongodb_single](#bridge_mongodb-mongodb_single)}</code>

  MongoDB Bridge Config

- influxdb_api_v1: <code>{$name -> [bridge_influxdb:influxdb_api_v1](#bridge_influxdb-influxdb_api_v1)}</code>

  InfluxDB Bridge Config

- influxdb_api_v2: <code>{$name -> [bridge_influxdb:influxdb_api_v2](#bridge_influxdb-influxdb_api_v2)}</code>

  InfluxDB Bridge Config

- redis_single: <code>{$name -> [bridge_redis:redis_single](#bridge_redis-redis_single)}</code>

  Redis Bridge Config

- redis_sentinel: <code>{$name -> [bridge_redis:redis_sentinel](#bridge_redis-redis_sentinel)}</code>

  Redis Bridge Config

- redis_cluster: <code>{$name -> [bridge_redis:redis_cluster](#bridge_redis-redis_cluster)}</code>

  Redis Bridge Config

- pgsql: <code>{$name -> [bridge_pgsql:config](#bridge_pgsql-config)}</code>

  PostgreSQL Bridge Config

- timescale: <code>{$name -> [bridge_pgsql:config](#bridge_pgsql-config)}</code>

  Timescale Bridge Config

- matrix: <code>{$name -> [bridge_pgsql:config](#bridge_pgsql-config)}</code>

  Matrix Bridge Config

- clickhouse: <code>{$name -> [bridge_clickhouse:config](#bridge_clickhouse-config)}</code>

  Clickhouse Bridge Config

- sqlserver: <code>{$name -> [bridge_sqlserver:config](#bridge_sqlserver-config)}</code>

  Microsoft SQL Server Bridge Config

- rabbitmq: <code>{$name -> [bridge_rabbitmq:config](#bridge_rabbitmq-config)}</code>

  RabbitMQ Bridge Config

- kinesis_producer: <code>{$name -> [bridge_kinesis:config_producer](#bridge_kinesis-config_producer)}</code>

  Amazon Kinesis Producer Bridge Config

- greptimedb: <code>{$name -> [bridge_greptimedb:greptimedb](#bridge_greptimedb-greptimedb)}</code>

  GreptimeDB Bridge Config

- azure_event_hub_producer: <code>{$name -> [bridge_azure_event_hub:config_producer](#bridge_azure_event_hub-config_producer)}</code>

  EMQX Enterprise Config


## bridge_azure_event_hub:actions
一个动作的配置项


**Config paths**

 - <code>actions.azure_event_hub_producer.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__AZURE_EVENT_HUB_PRODUCER__$NAME</code>



**Fields**

- local_topic: <code>binary()</code>

  将 MQTT 主题或主题过滤器作为数据源（动作输入）。如果使用规则动作作为数据源，则应将此配置保留为空，否则消息将在 Azure Event Hubs 中重复。

- parameters: <code>[bridge_azure_event_hub:producer_kafka_opts](#bridge_azure_event_hub-producer_kafka_opts)</code>

  Azure Event Hubs 生产者配置

- resource_opts: <code>[bridge_kafka:resource_opts](#bridge_kafka-resource_opts)</code>
  * default: 
  `{}`



- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该动作。

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。


## bridge_azure_event_hub:auth_username_password
基于用户名/密码进行认证


**Config paths**

 - <code>bridges.azure_event_hub_producer.$name.authentication</code>
 - <code>connectors.azure_event_hub_producer.$name.authentication</code>


**Env overrides**

 - <code>EMQX_BRIDGES__AZURE_EVENT_HUB_PRODUCER__$NAME__AUTHENTICATION</code>
 - <code>EMQX_CONNECTORS__AZURE_EVENT_HUB_PRODUCER__$NAME__AUTHENTICATION</code>



**Fields**

- password: <code>emqx_schema_secret:secret()</code>

  用于连接 Azure Event Hubs 的连接字符串。应为命名空间共享访问策略的 "连接字符串-主键"。


## bridge_azure_event_hub:kafka_message
呈现 Azure Event Hubs 消息的模版。


**Config paths**

 - <code>actions.azure_event_hub_producer.$name.parameters.message</code>
 - <code>bridges.azure_event_hub_producer.$name.kafka.message</code>


**Env overrides**

 - <code>EMQX_ACTIONS__AZURE_EVENT_HUB_PRODUCER__$NAME__PARAMETERS__MESSAGE</code>
 - <code>EMQX_BRIDGES__AZURE_EVENT_HUB_PRODUCER__$NAME__KAFKA__MESSAGE</code>



**Fields**

- key: <code>string()</code>
  * default: 
  `"${.clientid}"`

  用于呈现 Azure Event Hubs 消息键的模版。如果模板呈现为空值（即在规则引擎上下文中没有此类数据字段），则使用 Azure Event Hubs 的 NULL（而不是空字符串）。

- value: <code>string()</code>
  * default: 
  `"${.}"`

  用于呈现 Azure Event Hubs 消息值的模版。如果模板呈现为空值（即在规则引擎上下文中没有此类数据字段），则使用 Azure Event Hubs 的 NULL（而不是空字符串）。


## bridge_azure_event_hub:producer_kafka_opts
Azure Event Hubs 生产者配置


**Config paths**

 - <code>actions.azure_event_hub_producer.$name.parameters</code>
 - <code>bridges.azure_event_hub_producer.$name.kafka</code>


**Env overrides**

 - <code>EMQX_ACTIONS__AZURE_EVENT_HUB_PRODUCER__$NAME__PARAMETERS</code>
 - <code>EMQX_BRIDGES__AZURE_EVENT_HUB_PRODUCER__$NAME__KAFKA</code>



**Fields**

- topic: <code>string()</code>

  ----

- message: <code>[bridge_azure_event_hub:kafka_message](#bridge_azure_event_hub-kafka_message)</code>

  呈现 Azure Event Hubs 消息的模版。

- max_batch_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `896KB`

  在 Azure Event Hubs 消息批次中收集的最大字节数。

- partition_strategy: <code>random | key_dispatch</code>
  * default: 
  `random`

  分区策略是用来指导生产者如何将消息分配到Azure Event Hubs的各个分区。

  <code>random</code>: 为每条消息随机选择一个分区。
  <code>key_dispatch</code>: 根据 Azure Event Hubs 消息键的哈希值将消息分配到分区，确保拥有相同键的消息能够一致地被分配到特定分区。

- required_acks: <code>all_isr | leader_only</code>
  * default: 
  `all_isr`

  Azure Event Hubs 分区领导者的确认标准。它确定了在向 EMQX Azure Event Hubs 生产者回送确认前，需要从追随者分区获得的确认等级。

  <code>all_isr</code>：要求所有同步副本进行确认。
  <code>leader_only</code>：仅要求分区领导者的确认。

- kafka_headers: <code>binary()</code>

  为Azure Event Hubs头部提供一个占位符。例如：<code>${pub_props}</code>。<br/>
  注意，占位符的值必须是一个对象：
  <code>{"foo": "bar"}</code>
  或者是一组键值对：
  <code>[{"key": "foo", "value": "bar"}]</code>

- kafka_ext_headers: <code>[[bridge_kafka:producer_kafka_ext_headers](#bridge_kafka-producer_kafka_ext_headers)]</code>

  请提供更多的 Azure Event Hubs 标头键值对<br/>这里的键值对将与<code>kafka_headers</code>字段的值合并，然后发送到 Azure Event Hubs。

- kafka_header_value_encode_mode: <code>none | json</code>
  * default: 
  `none`

  Azure Event Hubs 头部值的编码模式。<br/>
   - None: 仅将二进制值添加到 Azure Event Hubs 头部;<br/>
   - JSON: 仅将 JSON 值添加到 Azure Event Hubs 头部，并在发送前将其编码为 JSON 字符串。

- partition_count_refresh_interval: <code>emqx_schema:timeout_duration_s()</code>
  * default: 
  `60s`

  Azure Event Hubs 生产者发现分区数量增加的时间间隔。
  在 Azure Event Hubs 中增加分区数量后，EMQX 将开始根据<code>partition_strategy</code>
  在分发消息时考虑新发现的分区。

- max_inflight: <code>pos_integer()</code>
  * default: 
  `10`

  Azure Event Hubs 生产者在接收到 Azure Event Hubs 的确认之前，每个分区允许发送的批次的最大数量。较高的值通常意味着更好的吞吐量。然而，当这个值大于1时，可能会有消息重新排序的风险。

- buffer: <code>[bridge_kafka:producer_buffer](#bridge_kafka-producer_buffer)</code>

  配置生产者消息缓冲区。

  告诉 Azure Event Hubs 生产者，在 EMQX 需要发送的消息多于 Azure Event Hubs 能够处理的消息时，或者 Azure Event Hubs 宕机时，如何缓冲消息。

- query_mode: <code>async | sync</code>
  * default: 
  `async`

  查询模式。可选'sync/async'，默认为'async'。

- sync_query_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  此参数定义同步查询的超时限制。仅当桥接查询模式配置为 'sync' 时适用。


## bridge_azure_event_hub:ssl_client_opts
用于 Azure Event Hubs 客户端的 TLS/SSL 选项。


**Config paths**

 - <code>bridges.azure_event_hub_producer.$name.ssl</code>
 - <code>connectors.azure_event_hub_producer.$name.ssl</code>


**Env overrides**

 - <code>EMQX_BRIDGES__AZURE_EVENT_HUB_PRODUCER__$NAME__SSL</code>
 - <code>EMQX_CONNECTORS__AZURE_EVENT_HUB_PRODUCER__$NAME__SSL</code>



**Fields**

- cacertfile: <code>binary()</code>

  受信任的 PEM 格式 CA  证书捆绑文件<br/>
  此文件中的证书用于验证 TLS 对等方的证书。
  如果要信任新 CA，请将新证书附加到文件中。
  无需重启 EMQX 即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
  注意：从文件中失效（删除）证书不会影响已建立的连接。

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>

  PEM 格式证书链文件<br/>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
  然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
  根 CA 证书是可选的，如果想要添加，应加到文件到最末端。

- keyfile: <code>binary()</code>

  PEM 格式的私钥文件。

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  启用或禁用对等验证。

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  启用 TLS 会话重用。

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
  因此，如果深度为 0，则对等方必须由受信任的根 CA 直接签名；<br/>
  如果是 1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
  如果是 2，则路径可以是 PEER、中间 CA1、中间 CA2、ROOT-CA。

- password: <code>string()</code>

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

  支持所有 TLS/DTLS 版本<br/>
  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。

- ciphers: <code>[string()]</code>
  * default: 
  `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  <br/>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
  注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  <br/>
  注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
  如果打算使用 PSK 密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

  <br/>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。

- enable: <code>true</code>
  * default: 
  `true`

  启用 TLS。

- server_name_indication: <code>auto | disable | string()</code>
  * default: 
  `auto`

  TLS 握手的服务器名称指示（SNI）设置。<br/>
  - <code>auto</code>：客户端将使用<code>"servicebus.windows.net"</code>作为SNI。<br/>
  - <code>disable</code>：如果您希望阻止客户端发送SNI。<br/>
  - 其他字符串值将按原样发送。


## bridge_azure_event_hub:config_connector
Azure Event Hubs 数据桥接配置项。


**Config paths**

 - <code>connectors.azure_event_hub_producer.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__AZURE_EVENT_HUB_PRODUCER__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- bootstrap_hosts: <code>binary()</code>

  逗号分隔的 Azure Event Hubs Kafka 命名空间主机名 <code>host[:port]</code> ，用于引导客户端。  默认端口号为 9093。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  TCP 连接建立的最大等待时间（包括如果已启用身份认证则包括身份认证时间）

- min_metadata_refresh_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `3s`

  客户端在刷新 Azure Event Hubs Kafka 代理和主题元数据之前必须等待的最小时间间隔。设置太小的值可能会给 Azure Event Hubs 增加额外的负载。

- metadata_request_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  从 Azure Event Hubs 获取元数据时的最大等待时间。

- authentication: <code>[bridge_azure_event_hub:auth_username_password](#bridge_azure_event_hub-auth_username_password)</code>
  * default: 
  `{}`

  认证配置

- socket_opts: <code>[bridge_kafka:socket_opts](#bridge_kafka-socket_opts)</code>

  额外的套接字选项。

- ssl: <code>[bridge_azure_event_hub:ssl_client_opts](#bridge_azure_event_hub-ssl_client_opts)</code>
  * default: 
  `{enable = true}`



- resource_opts: <code>[bridge_kafka:connector_resource_opts](#bridge_kafka-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_azure_event_hub:config_producer
Azure Event Hubs 数据桥接配置项。


**Config paths**

 - <code>bridges.azure_event_hub_producer.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__AZURE_EVENT_HUB_PRODUCER__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- bootstrap_hosts: <code>binary()</code>

  逗号分隔的 Azure Event Hubs Kafka 命名空间主机名 <code>host[:port]</code> ，用于引导客户端。  默认端口号为 9093。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  TCP 连接建立的最大等待时间（包括如果已启用身份认证则包括身份认证时间）

- min_metadata_refresh_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `3s`

  客户端在刷新 Azure Event Hubs Kafka 代理和主题元数据之前必须等待的最小时间间隔。设置太小的值可能会给 Azure Event Hubs 增加额外的负载。

- metadata_request_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  从 Azure Event Hubs 获取元数据时的最大等待时间。

- authentication: <code>[bridge_azure_event_hub:auth_username_password](#bridge_azure_event_hub-auth_username_password)</code>
  * default: 
  `{}`

  认证配置

- socket_opts: <code>[bridge_kafka:socket_opts](#bridge_kafka-socket_opts)</code>

  额外的套接字选项。

- ssl: <code>[bridge_azure_event_hub:ssl_client_opts](#bridge_azure_event_hub-ssl_client_opts)</code>
  * default: 
  `{enable = true}`



- resource_opts: <code>[bridge_kafka:connector_resource_opts](#bridge_kafka-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- local_topic: <code>binary()</code>

  将 MQTT 主题或主题过滤器作为数据源（动作输入）。如果使用规则动作作为数据源，则应将此配置保留为空，否则消息将在 Azure Event Hubs 中重复。

- kafka: <code>[bridge_azure_event_hub:producer_kafka_opts](#bridge_azure_event_hub-producer_kafka_opts)</code>

  Azure Event Hubs 生产者配置


## bridge_cassa:config
Cassandra 桥接配置


**Config paths**

 - <code>bridges.cassandra.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__CASSANDRA__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用桥接

- cql: <code>binary()</code>
  * default: 
  `"insert into mqtt_msg(topic, msgid, sender, qos, payload, arrived, retain) values (${topic}, ${id}, ${clientid}, ${qos}, ${payload}, ${timestamp}, ${flags.retain})"`

  CQL 模板

- local_topic: <code>binary()</code>

  发送到 'local_topic' 的消息都会转发到 Cassandra。 <br/>
  注意：如果这个数据桥接被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。

- resource_opts: <code>[resource_schema:creation_opts](#resource_schema-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- servers: <code>string()</code>

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
  主机名具有以下形式：`Host[:Port][,Host2:Port]`。<br/>
  如果未指定 `[:Port]`，则使用 Cassandra 默认端口 9042。

- keyspace: <code>binary()</code>

  要连接到的 Keyspace 名称。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## bridge_clickhouse:config
Clickhouse 桥接配置


**Config paths**

 - <code>bridges.clickhouse.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__CLICKHOUSE__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用桥接

- sql: <code>binary()</code>
  * default: 
  `"INSERT INTO mqtt_test(payload, arrived) VALUES ('${payload}', ${timestamp})"`

  可以使用 ${field} 占位符来引用消息与客户端上下文中的变量，请确保对应字段存在且数据格式符合预期。此处不支持 [SQL 预处理](https://docs.emqx.com/zh/enterprise/v5.0/data-integration/data-bridges.html#sql-预处理)。

- batch_value_separator: <code>binary()</code>
  * default: 
  `", "`

  默认为逗号 ','，适用于 VALUE 格式。您也可以使用其他分隔符， 请参考 [INSERT INTO 语句](https://clickhouse.com/docs/en/sql-reference/statements/insert-into)。

- local_topic: <code>binary()</code>

  发送到 'local_topic' 的消息都会转发到 Clickhouse。 <br/>
  注意：如果这个数据桥接被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。

- resource_opts: <code>[bridge_clickhouse:creation_opts](#bridge_clickhouse-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- url: <code>emqx_bridge_clickhouse_connector:url()</code>

  你想连接到的 Clickhouse 服务器的 HTTP URL（例如 http://myhostname:8123）。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  连接 HTTP 服务器的超时时间。

- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.


## bridge_clickhouse:creation_opts
资源启动相关的选项。


**Config paths**

 - <code>bridges.clickhouse.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__CLICKHOUSE__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- batch_size: <code>pos_integer()</code>
  * default: 
  `1`

  最大批量请求大小。如果设为 1，则无批处理。

- batch_time: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_dynamo:config
DynamoDB 桥接配置


**Config paths**

 - <code>bridges.dynamo.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__DYNAMO__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用桥接

- template: <code>binary()</code>
  * default: 
  `""`

  模板, 默认为空，为空时将会将整个消息存入数据库

- local_topic: <code>binary()</code>

  发送到 'local_topic' 的消息都会转发到 DynamoDB。 <br/>
  注意：如果这个数据桥接被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。

- resource_opts: <code>[bridge_dynamo:creation_opts](#bridge_dynamo-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- url: <code>binary()</code>

  DynamoDB 的地址。

- table: <code>binary()</code>

  DynamoDB 的表。

- aws_access_key_id: <code>binary()</code>

  DynamoDB 的访问 ID。

- aws_secret_access_key: <code>emqx_schema_secret:secret()</code>

  DynamoDB 的访问密钥。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.


## bridge_dynamo:creation_opts
资源启动相关的选项。


**Config paths**

 - <code>bridges.dynamo.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__DYNAMO__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- batch_size: <code>pos_integer()</code>
  * default: 
  `1`

  最大批量请求大小。如果设为 1，则无批处理。

- batch_time: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_elasticsearch:action_config
ElasticSearch Action Configuration


**Config paths**

 - <code>actions.elasticsearch.$action_name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__ELASTICSEARCH__$ACTION_NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用（是）或停用（否）此动作。

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- parameters: <code>[bridge_elasticsearch:action_create](#bridge_elasticsearch-action_create) | [bridge_elasticsearch:action_delete](#bridge_elasticsearch-action_delete) | [bridge_elasticsearch:action_update](#bridge_elasticsearch-action_update)</code>

  ElasticSearch action parameters

- resource_opts: <code>[bridge_elasticsearch:action_resource_opts](#bridge_elasticsearch-action_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_elasticsearch:action_create
Adds a JSON document to the specified index and makes it searchable.
If the target is an index and the document already exists,
the request updates the document and increments its version.


**Config paths**

 - <code>actions.elasticsearch.$action_name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__ELASTICSEARCH__$ACTION_NAME__PARAMETERS</code>



**Fields**

- action: <code>create</code>

  create

- index: <code>binary()</code>

  Name of index, or index alias to perform the action on.
  This parameter is required.

- id: <code>binary()</code>

  The document ID. If no ID is specified, a document ID is automatically generated.

- doc: <code>binary()</code>

  JSON document. If undefined, rule engine will use JSON format to serialize all visible inputs, such as clientid, topic, payload etc.

- routing: <code>binary()</code>

  Custom value used to route operations to a specific shard.

- require_alias: <code>boolean()</code>

  If true, the request’s actions must target an index alias. Defaults to false

- overwrite: <code>boolean()</code>
  * default: 
  `true`

  Set to false If a document with the specified _id already exists(conflict), the operation will fail.

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `2`

  如果发送请求时出错，最大的重试次数。


## bridge_elasticsearch:action_delete
Removes a JSON document from the specified index.


**Config paths**

 - <code>actions.elasticsearch.$action_name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__ELASTICSEARCH__$ACTION_NAME__PARAMETERS</code>



**Fields**

- action: <code>delete</code>

  delete

- index: <code>binary()</code>

  Name of index, or index alias to perform the action on.
  This parameter is required.

- id: <code>binary()</code>

  The document ID. If no ID is specified, a document ID is automatically generated.

- routing: <code>binary()</code>

  Custom value used to route operations to a specific shard.

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `2`

  如果发送请求时出错，最大的重试次数。


## bridge_elasticsearch:action_resource_opts
Resource options.


**Config paths**

 - <code>actions.elasticsearch.$action_name.resource_opts</code>


**Env overrides**

 - <code>EMQX_ACTIONS__ELASTICSEARCH__$ACTION_NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_elasticsearch:action_update
Updates a document using the specified doc.


**Config paths**

 - <code>actions.elasticsearch.$action_name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__ELASTICSEARCH__$ACTION_NAME__PARAMETERS</code>



**Fields**

- action: <code>update</code>

  update

- index: <code>binary()</code>

  Name of index, or index alias to perform the action on.
  This parameter is required.

- id: <code>binary()</code>

  The document ID. If no ID is specified, a document ID is automatically generated.

- doc: <code>binary()</code>

  JSON document. If undefined, rule engine will use JSON format to serialize all visible inputs, such as clientid, topic, payload etc.

- doc_as_upsert: <code>boolean()</code>
  * default: 
  `false`

  Instead of sending a partial doc plus an upsert doc,
  you can set doc_as_upsert to true to use the contents of doc as the upsert value.

- routing: <code>binary()</code>

  Custom value used to route operations to a specific shard.

- require_alias: <code>boolean()</code>

  If true, the request’s actions must target an index alias. Defaults to false

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `2`

  如果发送请求时出错，最大的重试次数。


## bridge_gcp_pubsub:consumer
GCP PubSub 消费者配置。


**Config paths**

 - <code>bridges.gcp_pubsub_consumer.$name.consumer</code>


**Env overrides**

 - <code>EMQX_BRIDGES__GCP_PUBSUB_CONSUMER__$NAME__CONSUMER</code>



**Fields**

- pull_max_messages: <code>pos_integer()</code>
  * default: 
  `100`

  从 GCP PubSub 中在一个拉取请求里检索的最大消息数。 实际数量可能小于指定的值。

- topic_mapping: <code>[[bridge_gcp_pubsub:consumer_topic_mapping](#bridge_gcp_pubsub-consumer_topic_mapping)]</code>

  定义 GCP PubSub 主题和 MQTT 主题之间的映射。必须包含至少一项。


## bridge_gcp_pubsub:consumer_topic_mapping
定义 GCP PubSub 主题和 MQTT 主题之间的映射。必须包含至少一项。


**Config paths**

 - <code>bridges.gcp_pubsub_consumer.$name.consumer.topic_mapping.$INDEX</code>


**Env overrides**

 - <code>EMQX_BRIDGES__GCP_PUBSUB_CONSUMER__$NAME__CONSUMER__TOPIC_MAPPING__$INDEX</code>



**Fields**

- pubsub_topic: <code>binary()</code>

  要从中消费的 GCP PubSub 主题。

- mqtt_topic: <code>binary()</code>

  指定从 GCP PubSub 消费的消息被发布到的本地 MQTT 主题。

- qos: <code>qos()</code>
  * default: 
  `0`

  从 GCP PubSub 消费的消息发布时应用的 MQTT QoS。

- payload_template: <code>string()</code>
  * default: 
  `"${.}"`

  转换传入 GCP PubSub 消息格式的模板。默认情况下，它将使用 JSON 格式序列化来自 GCP PubSub 消息的输入。可用字段包括：
  <code>message_id</code>：由 GCP PubSub 分配的消息 ID。
  <code>publish_time</code>：GCP PubSub 分配的消息时间戳。
  <code>topic</code>：GCP PubSub 主题。
  <code>value</code>：GCP PubSub 消息的有效载荷。如果没有有效载荷，则省略。
  <code>attributes</code>：包含字符串键值对的对象。如果没有属性，则省略。
  <code>ordering_key</code>：GCP PubSub 消息排序键。如果没有，则省略。


## bridge_gcp_pubsub:key_value_pair
键值对


**Config paths**

 - <code>actions.gcp_pubsub_producer.$name.parameters.attributes_template.$INDEX</code>
 - <code>bridges.gcp_pubsub.$name.attributes_template.$INDEX</code>


**Env overrides**

 - <code>EMQX_ACTIONS__GCP_PUBSUB_PRODUCER__$NAME__PARAMETERS__ATTRIBUTES_TEMPLATE__$INDEX</code>
 - <code>EMQX_BRIDGES__GCP_PUBSUB__$NAME__ATTRIBUTES_TEMPLATE__$INDEX</code>



**Fields**

- key: <code>binary()</code>

  键

- value: <code>binary()</code>

  值


## bridge_gcp_pubsub:config_consumer
GCP PubSub 桥接配置


**Config paths**

 - <code>bridges.gcp_pubsub_consumer.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__GCP_PUBSUB_CONSUMER__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用数据桥接

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- resource_opts: <code>[bridge_gcp_pubsub:consumer_resource_opts](#bridge_gcp_pubsub-consumer_resource_opts)</code>

  资源启动相关的选项。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  连接 HTTP 服务器的超时时间。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `2`

  请求出错时的最大重试次数。

- request_timeout: <code>emqx_schema:timeout_duration_ms()</code>

  Deprecated since e5.0.1.

- service_account_json: <code>emqx_bridge_gcp_pubsub:service_account_json()</code>

  包含将与 PubSub 一起使用的 GCP 服务账户凭证的 JSON。
  当创建 GCP 服务账户时（如 https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount），可以选择下载 JSON 形式的凭证，然后在该配置项中使用。

- consumer: <code>[bridge_gcp_pubsub:consumer](#bridge_gcp_pubsub-consumer)</code>

  本地 MQTT 发布和 GCP PubSub 消费者配置。


## bridge_gcp_pubsub:config_producer
GCP PubSub 桥接配置


**Config paths**

 - <code>bridges.gcp_pubsub.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__GCP_PUBSUB__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用数据桥接

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- resource_opts: <code>[resource_schema:creation_opts](#resource_schema-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  连接 HTTP 服务器的超时时间。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `2`

  请求出错时的最大重试次数。

- request_timeout: <code>emqx_schema:timeout_duration_ms()</code>

  Deprecated since e5.0.1.

- service_account_json: <code>emqx_bridge_gcp_pubsub:service_account_json()</code>

  包含将与 PubSub 一起使用的 GCP 服务账户凭证的 JSON。
  当创建 GCP 服务账户时（如 https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount），可以选择下载 JSON 形式的凭证，然后在该配置项中使用。

- attributes_template: <code>[[bridge_gcp_pubsub:key_value_pair](#bridge_gcp_pubsub-key_value_pair)]</code>
  * default: 
  `[]`

  格式化出站消息属性的模板。未定义的值将被呈现为空字符串值。属性映射中的空键将被移除。

- ordering_key_template: <code>binary()</code>
  * default: 
  `""`

  格式化出站消息排序键的模板。未定义的值将被呈现为空字符串值。如果此值为空，则不会将其添加到消息中。

- payload_template: <code>binary()</code>
  * default: 
  `""`

  用于格式化外发信息的模板。 如果未定义，将以 JSON 格式发送所有可用的上下文。

- local_topic: <code>binary()</code>

  发送到 'local_topic' 的消息都会转发到 GCP PubSub。 <br/>
  注意：如果这个数据桥接被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发到 GCP PubSub。

- pubsub_topic: <code>binary()</code>

  要发布消息的 GCP PubSub 主题。


## bridge_gcp_pubsub:consumer_resource_opts
资源启动相关的选项。


**Config paths**

 - <code>bridges.gcp_pubsub_consumer.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__GCP_PUBSUB_CONSUMER__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `30s`

  健康检查间隔。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。


## bridge_greptimedb:greptimedb
GreptimeDB 的行协议。支持 GreptimeDB v1.8 以及之前的版本。


**Config paths**

 - <code>bridges.greptimedb.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__GREPTIMEDB__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用数据桥接

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- local_topic: <code>binary()</code>

  要转发到 GreptimeDB 的 MQTT 主题过滤器。所有 MQTT 'PUBLISH' 消息中与 local_topic 匹配的主题都将被转发。<br/>
  注意：如果此动作用作规则引擎 (EMQX 规则引擎) 的动作，并且配置了 local_topic，那么将会转发规则获取的数据以及与 local_topic 匹配的 MQTT 消息。

- write_syntax: <code>string()</code>

  GreptimeDB gRPC 协议写入数据点的配置。写入语法是一种文本格式，提供了数据点的测量、标签集、字段集和时间戳，并支持占位符，与 InfluxDB 行协议相同。
  参见 [InfluxDB 2.3 行协议](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) 和
  [GreptimeDB 1.8 行协议](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/) <br/>
  简而言之:<br/>
  ```
  <measurement>[,<tag_key>=<tag_value>[,<tag_key>=<tag_value>]] <field_key>=<field_value>[,<field_key>=<field_value>] [<timestamp>]
  ```
  请注意，整数值的占位符必须用后缀 `i` 注释。例如 `${payload.int_value}i`。

- resource_opts: <code>[resource_schema:creation_opts](#resource_schema-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- server: <code>string()</code>
  * default: 
  `"127.0.0.1:4001"`

  要连接的 IPv4 或 IPv6 地址或主机名。<br/>
  主机条目的格式如下：主机名[:端口]。<br/>
  如果未指定 [:端口]，则使用 GreptimeDB 的默认端口 8086。

- precision: <code>ns | us | ms | s</code>
  * default: 
  `ms`

  GreptimeDB 的时间精度。

- dbname: <code>binary()</code>

  GreptimeDB 数据库

- username: <code>binary()</code>

  GreptimeDB 用户名。

- password: <code>emqx_schema_secret:secret()</code>

  GreptimeDB 密码。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## bridge_hstreamdb:config
HStreamDB 动作的配置。


**Config paths**

 - <code>bridges.hstreamdb.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__HSTREAMDB__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用数据桥接

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- direction: <code>egress</code>
  * default: 
  `egress`

  此桥接的方向，必须为'egress'

- local_topic: <code>binary()</code>

  要转发到 HStreamDB 的 MQTT 主题过滤器。所有与 local_topic 匹配的 MQTT 'PUBLISH' 消息都将被转发。<br/>
  注意：如果此动作用作规则的操作（EMQX 规则引擎），并且还配置了 local_topic，则将同时转发从规则获取的数据和与 local_topic 匹配的 MQTT 消息。

- record_template: <code>binary()</code>
  * default: 
  `"${payload}"`

  要转发到 HStreamDB 的 HStream 记录模板。支持占位符。<br>
  注意：当您使用'原始记录'模板（这意味着数据不是有效的 JSON）时，您应该在 HStream 中使用 'read' 或 'subscription' 来获取数据。

- resource_opts: <code>[resource_schema:creation_opts](#resource_schema-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- url: <code>binary()</code>
  * default: 
  `"http://127.0.0.1:6570"`

  HStreamDB 服务器 URL。使用 gRPC http 服务器地址。

- stream: <code>binary()</code>

  HStreamDB 流名称

- partition_key: <code>binary()</code>

  HStreamDB 分区键。支持占位符。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- grpc_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `30s`

  HStreamDB gRPC 连接超时时间。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## bridge_http:action_resource_opts
资源相关的选项。


**Config paths**

 - <code>actions.http.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_ACTIONS__HTTP__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_http:connector_resource_opts
资源相关的选项。


**Config paths**

 - <code>connectors.elasticsearch.$name.resource_opts</code>
 - <code>connectors.http.$name.resource_opts</code>
 - <code>connectors.iotdb.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__ELASTICSEARCH__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_CONNECTORS__HTTP__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_CONNECTORS__IOTDB__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


## bridge_http:config
HTTP 动作的配置


**Config paths**

 - <code>bridges.webhook.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__WEBHOOK__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用或停用动作

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  连接到 HTTP 服务器的超时时间。

- retry_interval: <code>emqx_schema:timeout_duration()</code>

  Deprecated since 5.0.4.

- pool_type: <code>random | hash</code>
  * default: 
  `random`

  连接池类型。可以是random、hash之一。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  一个正整数。是否连续发送 HTTP 请求，当设置为1时，意味着在发送每个 HTTP 请求后，需要等待服务器返回，然后继续发送下一个请求。

- request: <code>map()</code>

  Deprecated since 5.3.2.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- url: <code>binary()</code>

  HTTP 动作的 URL。<br/>
  此路径允许使用带有变量的模板，但变量不能用于方案、主机或端口部分。<br/>
  例如，<code> http://localhost:9901/${topic} </code> 是允许的，但
  <code> http://${host}:9901/message </code> 或 <code> http://localhost:${port}/message </code>
  是不允许的。

- direction: <code>egress</code>

  Deprecated since 5.0.12.

- local_topic: <code>binary()</code>

  将要转发到 HTTP 服务器的 MQTT 主题过滤器。所有与 local_topic 匹配的 MQTT 'PUBLISH' 消息都将被转发。<br/>
  注意：如果将此动作用作规则的操作（EMQX 规则引擎），并且同时配置了 local_topic，那么将同时转发从规则获取的数据和与 local_topic 匹配的 MQTT 消息。

- method: <code>post | put | get | delete</code>
  * default: 
  `post`

  HTTP 请求的方法。所有可用的方法包括：post、put、get、delete。<br/>
  允许使用带有变量的模板。

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    cache-control = no-cache
    connection = keep-alive
    content-type = "application/json"
    keep-alive = "timeout=5"
  }
  ```

  HTTP 请求头。<br/>
  允许使用带有变量的模板。

- body: <code>binary()</code>

  HTTP 请求的主体。<br/>
  如果未提供，主体将是所有可用字段的 JSON 对象。<br/>
  这里的“所有可用字段”是指在触发此 Webhook 时的 MQTT 消息的上下文（当 local_topic 已设置并接收到 MQTT 消息时触发），<br/>或者当此 Webhook 用作规则的动作时，在触发此 Webhook 时的事件上下文。<br/>允许使用带有变量的模板。

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `2`

  如果发送请求时出错，最大的重试次数。

- request_timeout: <code>emqx_schema:duration_ms()</code>

  Deprecated since v5.0.26.

- resource_opts: <code>[bridge_http:v1_resource_opts](#bridge_http-v1_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_http:config_connector
HTTP 动作的配置


**Config paths**

 - <code>connectors.http.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__HTTP__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- url: <code>binary()</code>

  HTTP 动作的 URL。<br/>
  此路径允许使用带有变量的模板，但变量不能用于方案、主机或端口部分。<br/>
  例如，<code> http://localhost:9901/${topic} </code> 是允许的，但
  <code> http://${host}:9901/message </code> 或 <code> http://localhost:${port}/message </code>
  是不允许的。

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    cache-control = no-cache
    connection = keep-alive
    content-type = "application/json"
    keep-alive = "timeout=5"
  }
  ```

  HTTP 请求头。<br/>
  允许使用带有变量的模板。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  连接到 HTTP 服务器的超时时间。

- retry_interval: <code>emqx_schema:timeout_duration()</code>

  Deprecated since 5.0.4.

- pool_type: <code>random | hash</code>
  * default: 
  `random`

  连接池类型。可以是random、hash之一。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  一个正整数。是否连续发送 HTTP 请求，当设置为1时，意味着在发送每个 HTTP 请求后，需要等待服务器返回，然后继续发送下一个请求。

- request: <code>map()</code>

  Deprecated since 5.3.2.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- resource_opts: <code>[bridge_http:connector_resource_opts](#bridge_http-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_http:http_action
HTTP 动作的配置


**Config paths**

 - <code>actions.http.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__HTTP__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用或停用动作

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- parameters: <code>[bridge_http:parameters_opts](#bridge_http-parameters_opts)</code>

  HTTP 动作的参数

- resource_opts: <code>[bridge_http:action_resource_opts](#bridge_http-action_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_http:parameters_opts
HTTP 动作的参数


**Config paths**

 - <code>actions.http.$name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__HTTP__$NAME__PARAMETERS</code>



**Fields**

- path: <code>binary()</code>

  此动作的 URL 路径。<br/>
  此路径将附加到连接器的 <code>url</code> 配置，以形成完整的 URL 地址。
  此选项允许使用带有变量的模板。例如，<code>/room/{$room_no}</code>。

- method: <code>post | put | get | delete</code>
  * default: 
  `post`

  HTTP 请求的方法。所有可用的方法包括：post、put、get、delete。<br/>
  允许使用带有变量的模板。

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    cache-control = no-cache
    connection = keep-alive
    content-type = "application/json"
    keep-alive = "timeout=5"
  }
  ```

  HTTP 请求头。<br/>
  允许使用带有变量的模板。

- body: <code>binary()</code>

  HTTP 请求的主体。<br/>
  如果未提供，主体将是所有可用字段的 JSON 对象。<br/>
  这里的“所有可用字段”是指在触发此 Webhook 时的 MQTT 消息的上下文（当 local_topic 已设置并接收到 MQTT 消息时触发），<br/>或者当此 Webhook 用作规则的动作时，在触发此 Webhook 时的事件上下文。<br/>允许使用带有变量的模板。

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `2`

  如果发送请求时出错，最大的重试次数。

- request_timeout: <code>emqx_schema:duration_ms()</code>

  Deprecated since v5.0.26.


## bridge_http:v1_resource_opts
资源启动相关的选项。


**Config paths**

 - <code>bridges.webhook.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__WEBHOOK__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_influxdb:action_parameters
Additional parameters specific to this action type


**Config paths**

 - <code>actions.influxdb.$name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__INFLUXDB__$NAME__PARAMETERS</code>



**Fields**

- write_syntax: <code>string()</code>

  使用 InfluxDB API Line Protocol 写入 InfluxDB 的数据，支持占位符<br/>
  参考 [InfluxDB 2.3 Line Protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) 及
  [InfluxDB 1.8 Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/) <br/>
  TLDR: <br/>

  ```
  <measurement>[,<tag_key>=<tag_value>[,<tag_key>=<tag_value>]] <field_key>=<field_value>[,<field_key>=<field_value>] [<timestamp>]
  ```
  注意，整形数值占位符后需要添加一个字符 `i` 类型标识。例如 `${payload.int_value}i`

- precision: <code>ns | us | ms | s</code>
  * default: 
  `ms`

  InfluxDB 时间精度。


## bridge_influxdb:connector_resource_opts
资源相关的选项。


**Config paths**

 - <code>connectors.influxdb.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__INFLUXDB__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


## bridge_influxdb:influxdb_action
Action to interact with a InfluxDB connector


**Config paths**

 - <code>actions.influxdb.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__INFLUXDB__$NAME</code>



**Fields**

- local_topic: <code>binary()</code>

  MQTT 主题或主题过滤器作为数据源（动作输入）。 如果规则动作用作数据源，则应将此配置留空，否则消息将在远程系统中重复。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用（是）或停用（否）此动作。

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- parameters: <code>[bridge_influxdb:action_parameters](#bridge_influxdb-action_parameters)</code>

  Additional parameters specific to this action type

- resource_opts: <code>[actions_and_sources:action_resource_opts](#actions_and_sources-action_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_influxdb:influxdb_api_v1
InfluxDB HTTP API 协议。支持 Influxdb v1.8 以及之前的版本。


**Config paths**

 - <code>bridges.influxdb_api_v1.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__INFLUXDB_API_V1__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用数据桥接

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- local_topic: <code>binary()</code>

  发送到 'local_topic' 的消息都会转发到 InfluxDB。 <br/>
  注意：如果这个数据桥接被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发到 InfluxDB。

- write_syntax: <code>string()</code>

  使用 InfluxDB API Line Protocol 写入 InfluxDB 的数据，支持占位符<br/>
  参考 [InfluxDB 2.3 Line Protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) 及
  [InfluxDB 1.8 Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/) <br/>
  TLDR: <br/>

  ```
  <measurement>[,<tag_key>=<tag_value>[,<tag_key>=<tag_value>]] <field_key>=<field_value>[,<field_key>=<field_value>] [<timestamp>]
  ```
  注意，整形数值占位符后需要添加一个字符 `i` 类型标识。例如 `${payload.int_value}i`

- resource_opts: <code>[resource_schema:creation_opts](#resource_schema-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- server: <code>string()</code>
  * default: 
  `"127.0.0.1:8086"`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
  主机名具有以下形式：`Host[:Port]`。<br/>
  如果未指定 `[:Port]`，则使用 InfluxDB 默认端口 8086。

- precision: <code>ns | us | ms | s</code>
  * default: 
  `ms`

  InfluxDB 时间精度。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- database: <code>binary()</code>

  InfluxDB 数据库。

- username: <code>binary()</code>

  InfluxDB 用户名。

- password: <code>emqx_schema_secret:secret()</code>

  InfluxDB 密码。


## bridge_influxdb:influxdb_api_v2
InfluxDB HTTP API V2 协议。支持 Influxdb v2.0 以及之后的版本。


**Config paths**

 - <code>bridges.influxdb_api_v2.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__INFLUXDB_API_V2__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用数据桥接

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- local_topic: <code>binary()</code>

  发送到 'local_topic' 的消息都会转发到 InfluxDB。 <br/>
  注意：如果这个数据桥接被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发到 InfluxDB。

- write_syntax: <code>string()</code>

  使用 InfluxDB API Line Protocol 写入 InfluxDB 的数据，支持占位符<br/>
  参考 [InfluxDB 2.3 Line Protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) 及
  [InfluxDB 1.8 Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/) <br/>
  TLDR: <br/>

  ```
  <measurement>[,<tag_key>=<tag_value>[,<tag_key>=<tag_value>]] <field_key>=<field_value>[,<field_key>=<field_value>] [<timestamp>]
  ```
  注意，整形数值占位符后需要添加一个字符 `i` 类型标识。例如 `${payload.int_value}i`

- resource_opts: <code>[resource_schema:creation_opts](#resource_schema-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- server: <code>string()</code>
  * default: 
  `"127.0.0.1:8086"`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
  主机名具有以下形式：`Host[:Port]`。<br/>
  如果未指定 `[:Port]`，则使用 InfluxDB 默认端口 8086。

- precision: <code>ns | us | ms | s</code>
  * default: 
  `ms`

  InfluxDB 时间精度。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- bucket: <code>binary()</code>

  InfluxDB bucket 名称。

- org: <code>binary()</code>

  InfluxDB 组织名称。

- token: <code>emqx_schema_secret:secret()</code>

  InfluxDB token。


## bridge_influxdb:config_connector
InfluxDB 桥接配置。


**Config paths**

 - <code>connectors.influxdb.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__INFLUXDB__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- server: <code>string()</code>
  * default: 
  `"127.0.0.1:8086"`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
  主机名具有以下形式：`Host[:Port]`。<br/>
  如果未指定 `[:Port]`，则使用 InfluxDB 默认端口 8086。

- parameters: <code>[connector_influxdb:connector_influxdb_api_v1](#connector_influxdb-connector_influxdb_api_v1) | [connector_influxdb:connector_influxdb_api_v2](#connector_influxdb-connector_influxdb_api_v2)</code>

  Set of parameters specific for the given type of this InfluxDB connector, `influxdb_type` can be one of `influxdb_api_v1`, `influxdb_api_v1`.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- resource_opts: <code>[bridge_influxdb:connector_resource_opts](#bridge_influxdb-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_iotdb:action_config
IoTDB 数据桥接配置


**Config paths**

 - <code>actions.iotdb.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__IOTDB__$NAME</code>



**Fields**

- local_topic: <code>binary()</code>

  MQTT 主题或主题过滤器作为数据源（动作输入）。 如果规则动作用作数据源，则应将此配置留空，否则消息将在远程系统中重复。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用（是）或停用（否）此动作。

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- parameters: <code>[bridge_iotdb:action_parameters](#bridge_iotdb-action_parameters)</code>

  IoTDB action parameters

- resource_opts: <code>[bridge_iotdb:action_resource_opts](#bridge_iotdb-action_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_iotdb:action_parameters
IoTDB action parameters


**Config paths**

 - <code>actions.iotdb.$name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__IOTDB__$NAME__PARAMETERS</code>



**Fields**

- is_aligned: <code>boolean()</code>
  * default: 
  `false`

  是否对齐时间序列。

- device_id: <code>binary()</code>

  IoTDB 的设备 ID（DeviceID）。可以使用一个占位符。如果留空则 MQTT 消息体中必须有一个 `device_id` 字段，
  或者 EMQX 规则引擎的 SQL 必须输出一个 `device_id` 字段。

- data: <code>[[bridge_iotdb:action_parameters_data](#bridge_iotdb-action_parameters_data)]</code>
  * default: 
  `[]`

  IoTDB action parameter data

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `2`

  如果发送请求时出错，最大的重试次数。


## bridge_iotdb:action_parameters_data
IoTDB action parameter data


**Config paths**

 - <code>actions.iotdb.$name.parameters.data.$INDEX</code>


**Env overrides**

 - <code>EMQX_ACTIONS__IOTDB__$NAME__PARAMETERS__DATA__$INDEX</code>



**Fields**

- timestamp: <code>now | now_ms | now_ns | now_us | binary()</code>
  * default: 
  `now`

  Timestamp. Placeholders in format of ${var} is supported, the final value can be:</br>
  - now: use the `now_ms` which is contained in the payload as timestamp
  - now_ms: same as above
  - now_us: use the `now_us` which is contained in the payload as timestamp
  - now_ns: use the `now_ns` which is contained in the payload as timestamp
  - any other: use the value directly as the timestamp

- measurement: <code>binary()</code>

  Measurement. Placeholders in format of ${var} is supported

- data_type: <code>text | boolean | int32 | int64 | float | double | binary()</code>

  Data Type, an enumerated or a string. </br>
  For string placeholders in format of ${var} is supported, the final value can be:</br>
  - TEXT
  - BOOLEAN
  - INT32
  - INT64
  - FLOAT
  - DOUBLE

- value: <code>binary()</code>

  Value. Placeholders in format of ${var} is supported


## bridge_iotdb:action_resource_opts
Action Resource Options


**Config paths**

 - <code>actions.iotdb.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_ACTIONS__IOTDB__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_iotdb:auth_basic
Basic Authentication


**Config paths**

 - <code>bridges.iotdb.$name.authentication</code>


**Env overrides**

 - <code>EMQX_BRIDGES__IOTDB__$NAME__AUTHENTICATION</code>



**Fields**

- username: <code>binary()</code>

  Basic auth 用户名。类似 IoTDB REST 接口中的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  Basic auth 密码。类似 IoTDB REST 接口中的密码。


## bridge_iotdb:config
IoTDB 数据桥接配置


**Config paths**

 - <code>bridges.iotdb.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__IOTDB__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用或禁用该桥接

- authentication: <code>[bridge_iotdb:auth_basic](#bridge_iotdb-auth_basic)</code>
  * default: 
  `auth_basic`

  认证信息

- is_aligned: <code>boolean()</code>
  * default: 
  `false`

  是否对齐时间序列。

- device_id: <code>binary()</code>

  IoTDB 的设备 ID（DeviceID）。可以使用一个占位符。如果留空则 MQTT 消息体中必须有一个 `device_id` 字段，
  或者 EMQX 规则引擎的 SQL 必须输出一个 `device_id` 字段。

- iotdb_version: <code>v1.1.x | v1.0.x | v0.13.x</code>
  * default: 
  `v1.1.x`

  IoTDB 版本。

- resource_opts: <code>[bridge_iotdb:creation_opts](#bridge_iotdb-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  连接到 HTTP 服务器的超时时间。

- retry_interval: <code>emqx_schema:timeout_duration()</code>

  Deprecated since 5.0.4.

- pool_type: <code>random | hash</code>
  * default: 
  `random`

  连接池类型。可以是random、hash之一。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  一个正整数。是否连续发送 HTTP 请求，当设置为1时，意味着在发送每个 HTTP 请求后，需要等待服务器返回，然后继续发送下一个请求。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- base_url: <code>emqx_schema:url()</code>

  IoTDB REST 服务的 URL。

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `2`

  HTTP 请求的最大重试次数。


## bridge_iotdb:creation_opts
Creation Options


**Config paths**

 - <code>bridges.iotdb.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__IOTDB__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_kafka:auth_gssapi_kerberos
使用 GSSAPI/Kerberos 认证。


**Config paths**

 - <code>bridges.kafka.$name.authentication</code>
 - <code>bridges.kafka_consumer.$name.authentication</code>
 - <code>connectors.kafka_producer.$name.authentication</code>


**Env overrides**

 - <code>EMQX_BRIDGES__KAFKA__$NAME__AUTHENTICATION</code>
 - <code>EMQX_BRIDGES__KAFKA_CONSUMER__$NAME__AUTHENTICATION</code>
 - <code>EMQX_CONNECTORS__KAFKA_PRODUCER__$NAME__AUTHENTICATION</code>



**Fields**

- kerberos_principal: <code>binary()</code>

  SASL GSSAPI 认证方法的 Kerberos principal，例如 <code>client_name@MY.KERBEROS.REALM.MYDOMAIN.COM</code>注意：这里使用的 realm 需要配置在 EMQX 服务器的 /etc/krb5.conf 中。

- kerberos_keytab_file: <code>binary()</code>

  SASL GSSAPI 认证方法的 Kerberos keytab 文件。注意：该文件需要上传到 EMQX 服务器中，且运行 EMQX 服务的系统账户需要有读取权限。


## bridge_kafka:auth_username_password
基于用户名密码的认证。


**Config paths**

 - <code>bridges.kafka.$name.authentication</code>
 - <code>bridges.kafka_consumer.$name.authentication</code>
 - <code>connectors.kafka_producer.$name.authentication</code>


**Env overrides**

 - <code>EMQX_BRIDGES__KAFKA__$NAME__AUTHENTICATION</code>
 - <code>EMQX_BRIDGES__KAFKA_CONSUMER__$NAME__AUTHENTICATION</code>
 - <code>EMQX_CONNECTORS__KAFKA_PRODUCER__$NAME__AUTHENTICATION</code>



**Fields**

- mechanism: <code>plain | scram_sha_256 | scram_sha_512</code>

  SASL 认证方法名称。

- username: <code>binary()</code>

  SASL 认证的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  SASL 认证的密码。


## bridge_kafka:connector_resource_opts
资源相关的选项。


**Config paths**

 - <code>bridges.azure_event_hub_producer.$name.resource_opts</code>
 - <code>bridges.kafka.$name.resource_opts</code>
 - <code>bridges.kafka_consumer.$name.resource_opts</code>
 - <code>connectors.azure_event_hub_producer.$name.resource_opts</code>
 - <code>connectors.confluent_producer.$name.resource_opts</code>
 - <code>connectors.kafka_producer.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__AZURE_EVENT_HUB_PRODUCER__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__KAFKA__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__KAFKA_CONSUMER__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_CONNECTORS__AZURE_EVENT_HUB_PRODUCER__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_CONNECTORS__CONFLUENT_PRODUCER__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_CONNECTORS__KAFKA_PRODUCER__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


## bridge_kafka:consumer_kafka_opts
Kafka 消费者配置。


**Config paths**

 - <code>bridges.kafka_consumer.$name.kafka</code>


**Env overrides**

 - <code>EMQX_BRIDGES__KAFKA_CONSUMER__$NAME__KAFKA</code>



**Fields**

- max_batch_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `896KB`

  设置每次从 Kafka 拉取数据的字节数。如该配置小于 Kafka 消息的大小，可能会影响消费性能。

- offset_reset_policy: <code>latest | earliest</code>
  * default: 
  `latest`

  如不存在偏移量历史记录或历史记录失效，消费者应使用哪个偏移量开始消费。

- offset_commit_interval_seconds: <code>emqx_schema:timeout_duration_s()</code>
  * default: 
  `5s`

  指定 Kafka 消费组偏移量提交的时间间隔。


## bridge_kafka:consumer_topic_mapping
指定 Kafka 主题和 MQTT 主题之间的映射关系。 应至少包含一项。


**Config paths**

 - <code>bridges.kafka_consumer.$name.topic_mapping.$INDEX</code>


**Env overrides**

 - <code>EMQX_BRIDGES__KAFKA_CONSUMER__$NAME__TOPIC_MAPPING__$INDEX</code>



**Fields**

- kafka_topic: <code>binary()</code>

  指定从哪个 Kafka 主题消费消息。

- mqtt_topic: <code>binary()</code>

  设置 Kafka 消息向哪个本地 MQTT 主题转发消息。

- qos: <code>qos()</code>
  * default: 
  `0`

  转发 MQTT 消息时使用的 QoS。

- payload_template: <code>string()</code>
  * default: 
  `"${.}"`

  用于转换收到的 Kafka 消息的模板。 默认情况下，它将使用 JSON 格式来序列化来自 Kafka 的所有字段。 这些字段包括：<code>headers</code>：一个包含字符串键值对的 JSON 对象。
  <code>key</code>：Kafka 消息的键（使用选择的编码方式编码）。
  <code>offset</code>：消息的偏移量。
  <code>topic</code>：Kafka 主题。
  <code>ts</code>: 消息的时间戳。
  <code>ts_type</code>：消息的时间戳类型，值可能是： <code>create</code>， <code>append</code> 或 <code>undefined</code>。
  <code>value</code>: Kafka 消息值（使用选择的编码方式编码）。


## bridge_kafka:kafka_consumer
Kafka 消费者配置。


**Config paths**

 - <code>bridges.kafka_consumer.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__KAFKA_CONSUMER__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- bootstrap_hosts: <code>binary()</code>

  用逗号分隔的 <code>host[:port]</code> 主机列表。默认端口号为 9092。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  建立 TCP 连接时的最大等待时长（若启用认证，这个等待时长也包含完成认证所需时间）。

- min_metadata_refresh_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `3s`

  刷新 Kafka broker 和 Kafka 主题元数据段最短时间间隔。设置太小可能会增加 Kafka 压力。

- metadata_request_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  刷新元数据时最大等待时长。

- authentication: <code>none | [bridge_kafka:auth_username_password](#bridge_kafka-auth_username_password) | [bridge_kafka:auth_gssapi_kerberos](#bridge_kafka-auth_gssapi_kerberos)</code>
  * default: 
  `none`

  认证参数。

- socket_opts: <code>[bridge_kafka:socket_opts](#bridge_kafka-socket_opts)</code>

  更多 Socket 参数设置。

- ssl: <code>[bridge_kafka:ssl_client_opts](#bridge_kafka-ssl_client_opts)</code>



- resource_opts: <code>[bridge_kafka:connector_resource_opts](#bridge_kafka-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- kafka: <code>[bridge_kafka:consumer_kafka_opts](#bridge_kafka-consumer_kafka_opts)</code>

  Kafka 消费者配置。

- topic_mapping: <code>[[bridge_kafka:consumer_topic_mapping](#bridge_kafka-consumer_topic_mapping)]</code>

  指定 Kafka 主题和 MQTT 主题之间的映射关系。 应至少包含一项。

- key_encoding_mode: <code>none | base64</code>
  * default: 
  `none`

  通过 MQTT 转发之前，如何处理 Kafka 消息的 Key。<code>none</code> 使用 Kafka 消息中的 Key 原始值，不进行编码。  注意：在这种情况下，Key 必须是一个有效的 UTF-8 字符串。
  <code>base64</code> 对收到的密钥或值使用 base-64 编码。

- value_encoding_mode: <code>none | base64</code>
  * default: 
  `none`

  通过 MQTT 转发之前，如何处理 Kafka 消息的 Value。<code>none</code> 使用 Kafka 消息中的 Value 原始值，不进行编码。  注意：在这种情况下，Value 必须是一个有效的 UTF-8 字符串。
  <code>base64</code> 对收到的 Value 使用 base-64 编码。


## bridge_kafka:kafka_message
用于生成 Kafka 消息的模版。


**Config paths**

 - <code>actions.kafka_producer.$name.parameters.message</code>
 - <code>bridges.kafka.$name.kafka.message</code>


**Env overrides**

 - <code>EMQX_ACTIONS__KAFKA_PRODUCER__$NAME__PARAMETERS__MESSAGE</code>
 - <code>EMQX_BRIDGES__KAFKA__$NAME__KAFKA__MESSAGE</code>



**Fields**

- key: <code>string()</code>
  * default: 
  `"${.clientid}"`

  生成 Kafka 消息 Key 的模版。如果模版生成后为空值，则会使用 Kafka 的 <code>NULL</code> ，而非空字符串。

- value: <code>string()</code>
  * default: 
  `"${.}"`

  生成 Kafka 消息 Value 的模版。如果模版生成后为空值，则会使用 Kafka 的 <code>NULL</code>，而非空字符串。

- timestamp: <code>string()</code>
  * default: 
  `"${.timestamp}"`

  生成 Kafka 消息时间戳的模版。该时间必需是一个整型数值（可以是字符串格式）例如 <code>1661326462115</code> 或 <code>'1661326462115'</code>。当所需的输入字段不存在，或不是一个整型时，则会使用当前系统时间。


## bridge_kafka:kafka_producer
Kafka 生产者配置。


**Config paths**

 - <code>bridges.kafka.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__KAFKA__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- bootstrap_hosts: <code>binary()</code>

  用逗号分隔的 <code>host[:port]</code> 主机列表。默认端口号为 9092。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  建立 TCP 连接时的最大等待时长（若启用认证，这个等待时长也包含完成认证所需时间）。

- min_metadata_refresh_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `3s`

  刷新 Kafka broker 和 Kafka 主题元数据段最短时间间隔。设置太小可能会增加 Kafka 压力。

- metadata_request_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  刷新元数据时最大等待时长。

- authentication: <code>none | [bridge_kafka:auth_username_password](#bridge_kafka-auth_username_password) | [bridge_kafka:auth_gssapi_kerberos](#bridge_kafka-auth_gssapi_kerberos)</code>
  * default: 
  `none`

  认证参数。

- socket_opts: <code>[bridge_kafka:socket_opts](#bridge_kafka-socket_opts)</code>

  更多 Socket 参数设置。

- ssl: <code>[bridge_kafka:ssl_client_opts](#bridge_kafka-ssl_client_opts)</code>



- resource_opts: <code>[bridge_kafka:connector_resource_opts](#bridge_kafka-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- local_topic: <code>binary()</code>

  MQTT 主题数据源由桥接指定，或留空由规则动作指定。

- kafka: <code>[bridge_kafka:producer_kafka_opts](#bridge_kafka-producer_kafka_opts)</code>

  Kafka 生产者参数。


## bridge_kafka:kafka_producer_action
Kafka 生产者动作


**Config paths**

 - <code>actions.kafka_producer.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__KAFKA_PRODUCER__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用（true）或停用该（false）Kafka 数据桥接。

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- local_topic: <code>binary()</code>

  MQTT 主题数据源由桥接指定，或留空由规则动作指定。

- parameters: <code>[bridge_kafka:producer_kafka_opts](#bridge_kafka-producer_kafka_opts)</code>

  Kafka 生产者参数。

- resource_opts: <code>[bridge_kafka:resource_opts](#bridge_kafka-resource_opts)</code>
  * default: 
  `{}`




## bridge_kafka:producer_buffer
配置消息缓存的相关参数。

当 EMQX 需要发送的消息超过 Kafka 处理能力，或者当 Kafka 临时下线时，EMQX 内部会将消息缓存起来。


**Config paths**

 - <code>actions.azure_event_hub_producer.$name.parameters.buffer</code>
 - <code>actions.confluent_producer.$name.parameters.buffer</code>
 - <code>actions.kafka_producer.$name.parameters.buffer</code>
 - <code>bridges.azure_event_hub_producer.$name.kafka.buffer</code>
 - <code>bridges.kafka.$name.kafka.buffer</code>


**Env overrides**

 - <code>EMQX_ACTIONS__AZURE_EVENT_HUB_PRODUCER__$NAME__PARAMETERS__BUFFER</code>
 - <code>EMQX_ACTIONS__CONFLUENT_PRODUCER__$NAME__PARAMETERS__BUFFER</code>
 - <code>EMQX_ACTIONS__KAFKA_PRODUCER__$NAME__PARAMETERS__BUFFER</code>
 - <code>EMQX_BRIDGES__AZURE_EVENT_HUB_PRODUCER__$NAME__KAFKA__BUFFER</code>
 - <code>EMQX_BRIDGES__KAFKA__$NAME__KAFKA__BUFFER</code>



**Fields**

- mode: <code>memory | disk | hybrid</code>
  * default: 
  `memory`

  消息缓存模式。
  <code>memory</code>: 所有的消息都缓存在内存里。如果 EMQX 服务重启，缓存的消息会丢失。
  <code>disk</code>: 缓存到磁盘上。EMQX 重启后会继续发送重启前未发送完成的消息。
  <code>hybrid</code>: 先将消息缓存在内存中，当内存中的消息堆积超过一定限制（配置项 <code>segment_bytes</code> 描述了该限制）后，后续的消息会缓存到磁盘上。与 <code>memory</code> 模式一样，如果 EMQX 服务重启，缓存的消息会丢失。

- per_partition_limit: <code>emqx_schema:bytesize()</code>
  * default: 
  `2GB`

  为每个 Kafka 分区设置的最大缓存字节数。当超过这个上限之后，老的消息会被丢弃，为新的消息腾出空间。

- segment_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `100MB`

  当缓存模式是 <code>disk</code> 或 <code>hybrid</code> 时适用。该配置用于指定缓存到磁盘上的文件的大小。

- memory_overload_protection: <code>boolean()</code>
  * default: 
  `false`

  缓存模式是 <code>memory</code> 或 <code>hybrid</code> 时适用。当系统处于高内存压力时，从队列中丢弃旧的消息以减缓内存增长。内存压力值由配置项 <code>sysmon.os.sysmem_high_watermark</code> 决定。注意，该配置仅在 Linux 系统中有效。


## bridge_kafka:producer_kafka_ext_headers
请提供更多的 Kafka 头部键值对<br/>
这里的键值对将与 <code>kafka_headers</code> 字段的值合并，然后发送到 Kafka。


**Config paths**

 - <code>actions.azure_event_hub_producer.$name.parameters.kafka_ext_headers.$INDEX</code>
 - <code>actions.confluent_producer.$name.parameters.kafka_ext_headers.$INDEX</code>
 - <code>actions.kafka_producer.$name.parameters.kafka_ext_headers.$INDEX</code>
 - <code>bridges.azure_event_hub_producer.$name.kafka.kafka_ext_headers.$INDEX</code>
 - <code>bridges.kafka.$name.kafka.kafka_ext_headers.$INDEX</code>


**Env overrides**

 - <code>EMQX_ACTIONS__AZURE_EVENT_HUB_PRODUCER__$NAME__PARAMETERS__KAFKA_EXT_HEADERS__$INDEX</code>
 - <code>EMQX_ACTIONS__CONFLUENT_PRODUCER__$NAME__PARAMETERS__KAFKA_EXT_HEADERS__$INDEX</code>
 - <code>EMQX_ACTIONS__KAFKA_PRODUCER__$NAME__PARAMETERS__KAFKA_EXT_HEADERS__$INDEX</code>
 - <code>EMQX_BRIDGES__AZURE_EVENT_HUB_PRODUCER__$NAME__KAFKA__KAFKA_EXT_HEADERS__$INDEX</code>
 - <code>EMQX_BRIDGES__KAFKA__$NAME__KAFKA__KAFKA_EXT_HEADERS__$INDEX</code>



**Fields**

- kafka_ext_header_key: <code>binary()</code>

  Kafka 头部的键。支持格式为 ${var} 的占位符。

- kafka_ext_header_value: <code>binary()</code>

  Kafka 头部的值。支持格式为 ${var} 的占位符。


## bridge_kafka:producer_kafka_opts
Kafka 生产者参数。


**Config paths**

 - <code>actions.kafka_producer.$name.parameters</code>
 - <code>bridges.kafka.$name.kafka</code>


**Env overrides**

 - <code>EMQX_ACTIONS__KAFKA_PRODUCER__$NAME__PARAMETERS</code>
 - <code>EMQX_BRIDGES__KAFKA__$NAME__KAFKA</code>



**Fields**

- topic: <code>string()</code>

  Kafka 主题名称

- message: <code>[bridge_kafka:kafka_message](#bridge_kafka-kafka_message)</code>

  用于生成 Kafka 消息的模版。

- max_batch_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `896KB`

  最大消息批量字节数。大多数 Kafka 环境的默认最低值是 1 MB，EMQX 的默认值比 1 MB 更小是因为需要补偿 Kafka 消息编码所需要的额外字节（尤其是当每条消息都很小的情况下）。当单个消息的大小超过该限制时，它仍然会被发送，（相当于该批量中只有单个消息）。

- compression: <code>no_compression | snappy | gzip</code>
  * default: 
  `no_compression`

  压缩方法。

- partition_strategy: <code>random | key_dispatch</code>
  * default: 
  `random`

  设置消息发布时应该如何选择 Kafka 分区。

  <code>random</code>: 为每个消息随机选择一个分区。
  <code>key_dispatch</code>: Hash Kafka message key to a partition number

- required_acks: <code>all_isr | leader_only | none</code>
  * default: 
  `all_isr`

  设置 Kafka leader 在返回给 EMQX 确认之前需要等待多少个 follower 的确认。

  <code>all_isr</code>: 需要所有的在线复制者都确认。
  <code>leader_only</code>: 仅需要分区 leader 确认。
  <code>none</code>: 无需 Kafka 回复任何确认。

- kafka_headers: <code>binary()</code>

  请提供要用作 Kafka 头部的占位符<br/>
  例如：<code>${pub_props}</code><br/>
  请注意，占位符的值必须是对象形式的：<code>{"foo": "bar"}</code>
  或者是键值对的数组形式：<code>[{"key": "foo", "value": "bar"}]</code>。

- kafka_ext_headers: <code>[[bridge_kafka:producer_kafka_ext_headers](#bridge_kafka-producer_kafka_ext_headers)]</code>

  请提供更多的 Kafka 头部键值对<br/>
  这里的键值对将与 <code>kafka_headers</code> 字段的值合并，然后发送到 Kafka。

- kafka_header_value_encode_mode: <code>none | json</code>
  * default: 
  `none`

  Kafka 头部值编码模式<br/>
   - None: 仅将二进制值添加到 Kafka 头部；<br/>
   - JSON: 仅将 JSON 值添加到 Kafka 头部，并在发送前对其进行 JSON 字符串编码。

- partition_count_refresh_interval: <code>emqx_schema:timeout_duration_s()</code>
  * default: 
  `60s`

  配置 Kafka 刷新分区数量的时间间隔。
  EMQX 发现 Kafka 分区数量增加后，会开始按 <code>partition_strategy<code> 配置，把消息发送到新的分区中。

- max_inflight: <code>pos_integer()</code>
  * default: 
  `10`

  设置 Kafka 生产者（每个分区一个）在收到 Kafka 的确认前最多发送多少个请求（批量）。调大这个值通常可以增加吞吐量，但是，当该值设置大于 1 时存在消息乱序的风险。

- buffer: <code>[bridge_kafka:producer_buffer](#bridge_kafka-producer_buffer)</code>

  配置消息缓存的相关参数。

  当 EMQX 需要发送的消息超过 Kafka 处理能力，或者当 Kafka 临时下线时，EMQX 内部会将消息缓存起来。

- query_mode: <code>async | sync</code>
  * default: 
  `async`

  查询模式。可选 'sync/async'，默认 'async'。

- sync_query_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  该参数定义同步查询的超时限制。仅当桥接查询模式配置为'sync'时才适用。


## bridge_kafka:resource_opts
资源相关的选项。


**Config paths**

 - <code>actions.azure_event_hub_producer.$name.resource_opts</code>
 - <code>actions.confluent_producer.$name.resource_opts</code>
 - <code>actions.kafka_producer.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_ACTIONS__AZURE_EVENT_HUB_PRODUCER__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_ACTIONS__CONFLUENT_PRODUCER__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_ACTIONS__KAFKA_PRODUCER__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。


## bridge_kafka:socket_opts
更多 Socket 参数设置。


**Config paths**

 - <code>bridges.azure_event_hub_producer.$name.socket_opts</code>
 - <code>bridges.kafka.$name.socket_opts</code>
 - <code>bridges.kafka_consumer.$name.socket_opts</code>
 - <code>connectors.azure_event_hub_producer.$name.socket_opts</code>
 - <code>connectors.confluent_producer.$name.socket_opts</code>
 - <code>connectors.kafka_producer.$name.socket_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__AZURE_EVENT_HUB_PRODUCER__$NAME__SOCKET_OPTS</code>
 - <code>EMQX_BRIDGES__KAFKA__$NAME__SOCKET_OPTS</code>
 - <code>EMQX_BRIDGES__KAFKA_CONSUMER__$NAME__SOCKET_OPTS</code>
 - <code>EMQX_CONNECTORS__AZURE_EVENT_HUB_PRODUCER__$NAME__SOCKET_OPTS</code>
 - <code>EMQX_CONNECTORS__CONFLUENT_PRODUCER__$NAME__SOCKET_OPTS</code>
 - <code>EMQX_CONNECTORS__KAFKA_PRODUCER__$NAME__SOCKET_OPTS</code>



**Fields**

- sndbuf: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

  TCP socket 的发送缓存调优。默认值是针对高吞吐量的一个推荐值。

- recbuf: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

  TCP socket 的收包缓存调优。默认值是针对高吞吐量的一个推荐值。

- nodelay: <code>boolean()</code>
  * default: 
  `true`

  设置‘true’让系统内核立即发送。否则当需要发送的内容很少时，可能会有一定延迟（默认 40 毫秒）。

- tcp_keepalive: <code>string()</code>
  * default: 
  `none`

  为 Kafka 桥接连接启用 TCP keepalive。
  该值是3个由逗号分隔的数字，格式为 'Idle,Interval,Probes'。
   - Idle: 连接在服务器开始发送 keep-alive 探测（Linux 默认 7200）之前需要空闲的秒数。
   - Interval: TCP keep-alive 探测发送间隔的秒数（Linux 默认 75）。
   - Probes: 如果没有从另一端获得响应，在放弃并终止连接之前发送的 TCP keep-alive 探测的最大数量 （Linux 默认 9）。
  例如 "240,30,5" 表示： 在连接空闲 240 秒后发送 TCP keepalive 探测 ，并且每 30 秒发送一次探测，直到收到响应，如果连续错过 5 个响应，则应关闭连接。
  默认值： 'none'


## bridge_kafka:ssl_client_opts
Kafka 客户端的 TLS/SSL 选项


**Config paths**

 - <code>bridges.kafka.$name.ssl</code>
 - <code>bridges.kafka_consumer.$name.ssl</code>
 - <code>connectors.kafka_producer.$name.ssl</code>


**Env overrides**

 - <code>EMQX_BRIDGES__KAFKA__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__KAFKA_CONSUMER__$NAME__SSL</code>
 - <code>EMQX_CONNECTORS__KAFKA_PRODUCER__$NAME__SSL</code>



**Fields**

- cacertfile: <code>binary()</code>

  受信任的 PEM 格式 CA  证书捆绑文件<br/>
  此文件中的证书用于验证 TLS 对等方的证书。
  如果要信任新 CA，请将新证书附加到文件中。
  无需重启 EMQX 即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
  注意：从文件中失效（删除）证书不会影响已建立的连接。

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>

  PEM 格式证书链文件<br/>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
  然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
  根 CA 证书是可选的，如果想要添加，应加到文件到最末端。

- keyfile: <code>binary()</code>

  PEM 格式的私钥文件。

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  启用或禁用对等验证。

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  启用 TLS 会话重用。

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
  因此，如果深度为 0，则对等方必须由受信任的根 CA 直接签名；<br/>
  如果是 1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
  如果是 2，则路径可以是 PEER、中间 CA1、中间 CA2、ROOT-CA。

- password: <code>string()</code>

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

  支持所有 TLS/DTLS 版本<br/>
  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。

- ciphers: <code>[string()]</code>
  * default: 
  `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  <br/>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
  注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  <br/>
  注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
  如果打算使用 PSK 密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

  <br/>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。

- enable: <code>boolean()</code>
  * default: 
  `false`

  启用 TLS。

- server_name_indication: <code>auto | disable | string()</code>
  * default: 
  `auto`

  TLS 握手的 Server Name Indication (SNI) 设置。<br/>
  - <code>auto</code>：客户端将使用 <code>"servicebus.windows.net"</code> 作为 SNI。<br/>
  - <code>disable</code>：如果您希望阻止客户端发送 SNI。<br/>
  - 其他字符串值将按原样发送。


## bridge_kafka:config_connector
一个 Kafka 生产者客户端的配置项


**Config paths**

 - <code>connectors.kafka_producer.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__KAFKA_PRODUCER__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- bootstrap_hosts: <code>binary()</code>

  用逗号分隔的 <code>host[:port]</code> 主机列表。默认端口号为 9092。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  建立 TCP 连接时的最大等待时长（若启用认证，这个等待时长也包含完成认证所需时间）。

- min_metadata_refresh_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `3s`

  刷新 Kafka broker 和 Kafka 主题元数据段最短时间间隔。设置太小可能会增加 Kafka 压力。

- metadata_request_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  刷新元数据时最大等待时长。

- authentication: <code>none | [bridge_kafka:auth_username_password](#bridge_kafka-auth_username_password) | [bridge_kafka:auth_gssapi_kerberos](#bridge_kafka-auth_gssapi_kerberos)</code>
  * default: 
  `none`

  认证参数。

- socket_opts: <code>[bridge_kafka:socket_opts](#bridge_kafka-socket_opts)</code>

  更多 Socket 参数设置。

- ssl: <code>[bridge_kafka:ssl_client_opts](#bridge_kafka-ssl_client_opts)</code>



- resource_opts: <code>[bridge_kafka:connector_resource_opts](#bridge_kafka-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_kinesis:config_producer
Amazon Kinesis 动作的配置。


**Config paths**

 - <code>bridges.kinesis_producer.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__KINESIS_PRODUCER__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用数据桥接

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- resource_opts: <code>[bridge_kinesis:creation_opts](#bridge_kinesis-creation_opts)</code>
  * default: 
  `{}`

  资源启动相关的选项。

- aws_access_key_id: <code>binary()</code>

  连接到 Amazon Kinesis 的访问密钥 ID。

- aws_secret_access_key: <code>emqx_schema_secret:secret()</code>

  连接到 Amazon Kinesis 的 AWS 秘密访问密钥。

- endpoint: <code>binary()</code>

  Amazon Kinesis 终端节点的 URL。

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `2`

  发送请求时发生错误的最大重试次数。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小

- payload_template: <code>binary()</code>
  * default: 
  `"${.}"`

  用于格式化传出消息的模板。如果未定义，将以 JSON 格式发送所有可用的上下文。

- local_topic: <code>binary()</code>

  要转发到 Amazon Kinesis 的 MQTT 主题过滤器。所有与主题匹配的 MQTT 'PUBLISH' 消息
  都将被转发。<br/>
  注意：如果此动作用作规则的操作（EMQX 规则引擎），并且还配置了 'local_topic'，则将转发从规则获取的数据以及与 'local_topic' 匹配的 MQTT 消息。


- stream_name: <code>binary()</code>

  消息将要被发布到的 Amazon Kinesis 流。

- partition_key: <code>binary()</code>

  与发布消息关联的 Amazon Kinesis 分区键。支持格式为 ${var} 的占位符。


## bridge_kinesis:creation_opts
资源启动相关的选项。


**Config paths**

 - <code>bridges.kinesis_producer.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__KINESIS_PRODUCER__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- batch_size: <code>pos_integer()</code>
  * default: 
  `1`

  最大批量请求大小。如果设为 1，则无批处理。

- batch_time: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_matrix:config_connector
PostgreSQL 连接器的配置。


**Config paths**

 - <code>connectors.matrix.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__MATRIX__$NAME</code>



**Fields**

- server: <code>string()</code>

  用于连接的 IPv4 或 IPv6 地址或主机名。<br/>
  一个主机条目具有以下格式：Host[:Port]。<br/>
  如果未指定 [:Port]，将使用 PostgreSQL 默认端口5432。

- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- resource_opts: <code>[connector_postgres:resource_opts](#connector_postgres-resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_mongodb:action_parameters
特定于此动作类型的附加参数


**Config paths**

 - <code>actions.mongodb.$name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__MONGODB__$NAME__PARAMETERS</code>



**Fields**

- collection: <code>binary()</code>
  * default: 
  `mqtt`

  数据将存储在其中的集合

- payload_template: <code>binary()</code>

  用于格式化发送消息的模板。如果未定义，规则引擎将使用 JSON 格式序列化所有可见输入，例如 clientid、主题、有效载荷等。


## bridge_mongodb:action_resource_opts
资源相关的选项。


**Config paths**

 - <code>actions.mongodb.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_ACTIONS__MONGODB__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- batch_time: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_mongodb:connector_resource_opts
资源相关的选项。


**Config paths**

 - <code>connectors.mongodb.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__MONGODB__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


## bridge_mongodb:mongodb_action
与 MongoDB 连接器交互的动作


**Config paths**

 - <code>actions.mongodb.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__MONGODB__$NAME</code>



**Fields**

- local_topic: <code>binary()</code>

  MQTT 主题或主题过滤器作为数据源（动作输入）。 如果规则动作用作数据源，则应将此配置留空，否则消息将在远程系统中重复。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用（是）或停用（否）此动作。

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- parameters: <code>[bridge_mongodb:action_parameters](#bridge_mongodb-action_parameters)</code>

  特定于此动作类型的附加参数

- resource_opts: <code>[bridge_mongodb:action_resource_opts](#bridge_mongodb-action_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_mongodb:mongodb_rs
MongoDB (Replica Set) 配置


**Config paths**

 - <code>bridges.mongodb_rs.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MONGODB_RS__$NAME</code>



**Fields**

- mongo_type: <code>rs</code>
  * default: 
  `rs`

  副本集。当MongoDB服务器以`副本集`模式运行时，必须设置为`rs`。

- servers: <code>string()</code>

  集群连接的节点列表。节点应使用逗号分隔，例如：节点[,节点]。
  对于每个节点，应该是要连接的 IPv4 或 IPv6 地址或主机名。
  主机条目具有以下形式：主机[:端口]。
  如果未指定[:端口]，则使用 MongoDB 的默认端口27017。

- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写入模式

- r_mode: <code>master | slave_ok</code>
  * default: 
  `master`

  读取模式。

- replica_set_name: <code>binary()</code>

  副本集的名称。

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  是否使用 MongoDB 的传统协议与数据库通信。默认情况下，将尝试自动确定是否支持较新的协议。

- auth_source: <code>binary()</code>

  与用户认证信息关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[mongo:topology](#mongo-topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用或停用该 MongoDB 动作

- collection: <code>binary()</code>
  * default: 
  `mqtt`

  数据将存储在其中的集合

- payload_template: <code>binary()</code>

  用于格式化发送消息的模板。如果未定义，规则引擎将使用 JSON 格式序列化所有可见输入，例如 clientid、主题、有效载荷等。

- resource_opts: <code>[bridge_mongodb:creation_opts](#bridge_mongodb-creation_opts)</code>

  资源启动相关的选项。


## bridge_mongodb:mongodb_sharded
MongoDB (Sharded) 配置


**Config paths**

 - <code>bridges.mongodb_sharded.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MONGODB_SHARDED__$NAME</code>



**Fields**

- mongo_type: <code>sharded</code>
  * default: 
  `sharded`

  分片集群。当 MongoDB 服务器以`分片`模式运行时，必须设置为`sharded`。

- servers: <code>string()</code>

  集群连接的节点列表。节点应使用逗号分隔，例如：节点[,节点]。
  对于每个节点，应该是要连接的 IPv4 或 IPv6 地址或主机名。
  主机条目具有以下形式：主机[:端口]。
  如果未指定[:端口]，则使用 MongoDB 的默认端口27017。

- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写入模式

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  是否使用 MongoDB 的传统协议与数据库通信。默认情况下，将尝试自动确定是否支持较新的协议。

- auth_source: <code>binary()</code>

  与用户认证信息关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[mongo:topology](#mongo-topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用或停用该 MongoDB 动作

- collection: <code>binary()</code>
  * default: 
  `mqtt`

  数据将存储在其中的集合

- payload_template: <code>binary()</code>

  用于格式化发送消息的模板。如果未定义，规则引擎将使用 JSON 格式序列化所有可见输入，例如 clientid、主题、有效载荷等。

- resource_opts: <code>[bridge_mongodb:creation_opts](#bridge_mongodb-creation_opts)</code>

  资源启动相关的选项。


## bridge_mongodb:mongodb_single
MongoDB (Standalone) 配置


**Config paths**

 - <code>bridges.mongodb_single.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MONGODB_SINGLE__$NAME</code>



**Fields**

- mongo_type: <code>single</code>
  * default: 
  `single`

  独立实例。当MongoDB服务器以独立模式运行时，必须设置为`single`。

- server: <code>string()</code>

  要连接的 IPv4 或 IPv6 地址或主机名。<br/>主机条目具有以下形式：主机[:端口]。<br/>如果未指定[:端口]，则使用MongoDB的默认端口27017。

- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写入模式

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  是否使用 MongoDB 的传统协议与数据库通信。默认情况下，将尝试自动确定是否支持较新的协议。

- auth_source: <code>binary()</code>

  与用户认证信息关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[mongo:topology](#mongo-topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用或停用该 MongoDB 动作

- collection: <code>binary()</code>
  * default: 
  `mqtt`

  数据将存储在其中的集合

- payload_template: <code>binary()</code>

  用于格式化发送消息的模板。如果未定义，规则引擎将使用 JSON 格式序列化所有可见输入，例如 clientid、主题、有效载荷等。

- resource_opts: <code>[bridge_mongodb:creation_opts](#bridge_mongodb-creation_opts)</code>

  资源启动相关的选项。


## bridge_mongodb:config_connector
MongoDB 动作的配置


**Config paths**

 - <code>connectors.mongodb.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__MONGODB__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- parameters: <code>[mongo:connector_single](#mongo-connector_single) | [mongo:connector_sharded](#mongo-connector_sharded) | [mongo:connector_rs](#mongo-connector_rs)</code>

  特定于此 MongoDB 连接器类型的一组参数，mongo_type 可以是 single（独立）、sharded（分片）或 rs（副本集）之一。

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  使用 DNS SRV 记录。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  是否使用 MongoDB 的传统协议与数据库通信。默认情况下，将尝试自动确定是否支持较新的协议。

- auth_source: <code>binary()</code>

  与用户认证信息关联的数据库名称。

- database: <code>binary()</code>

  数据库名字。

- topology: <code>[mongo:topology](#mongo-topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- resource_opts: <code>[bridge_mongodb:connector_resource_opts](#bridge_mongodb-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_mongodb:creation_opts
资源启动相关的选项。


**Config paths**

 - <code>bridges.mongodb_rs.$name.resource_opts</code>
 - <code>bridges.mongodb_sharded.$name.resource_opts</code>
 - <code>bridges.mongodb_single.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MONGODB_RS__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__MONGODB_SHARDED__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__MONGODB_SINGLE__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- batch_time: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_mqtt:config
MQTT 数据桥接的配置。


**Config paths**

 - <code>bridges.mqtt.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用数据桥接

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- resource_opts: <code>[bridge_mqtt:creation_opts](#bridge_mqtt-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- mode: <code>cluster_shareload</code>

  Deprecated since v5.1.0 & e5.1.0.

- server: <code>string()</code>

  远程 MQTT 代理的主机和端口

- clientid_prefix: <code>binary()</code>

  附加到 egress 动作使用的 clientid 前缀（可选）。

- reconnect_interval: <code>string()</code>

  Deprecated since v5.0.16.

- proto_ver: <code>v3 | v4 | v5</code>
  * default: 
  `v4`

  MQTT协议版本

- bridge_mode: <code>boolean()</code>
  * default: 
  `false`

  如果启用桥接模式。
  注意：此设置仅适用于 MQTT 协议版本早于5.0的情况，远程 MQTT
  代理必须支持此功能。
  如果将 bridge_mode 设置为true，则桥接将指示远程代理它是一个桥接而不是普通客户端。
  这意味着循环检测将更加有效，并且保留的消息将被正确传递。

- username: <code>binary()</code>

  MQTT 协议的用户名

- password: <code>emqx_schema_secret:secret()</code>

  MQTT 协议的密码

- clean_start: <code>boolean()</code>
  * default: 
  `true`

  在重新连接到入口动作时是否启动新会话

- keepalive: <code>string()</code>
  * default: 
  `300s`

  MQTT Keepalive. Time interval is a string that contains a number followed by time unit:<br/>- `ms` for milliseconds,
  - `s` for seconds,
  - `m` for minutes,
  - `h` for hours;
  <br/>or combination of whereof: `1h5m0s`

- retry_interval: <code>string()</code>
  * default: 
  `15s`

  Message retry interval. Delay for the MQTT bridge to retry sending the QoS1/QoS2 messages in case of ACK not received. Time interval is a string that contains a number followed by time unit:<br/>- `ms` for milliseconds,
  - `s` for seconds,
  - `m` for minutes,
  - `h` for hours;
  <br/>or combination of whereof: `1h5m0s`

- max_inflight: <code>non_neg_integer()</code>
  * default: 
  `32`

  MQTT 协议的最大 inflight（已发送但未确认）消息数

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- ingress: <code>[connector_mqtt:ingress](#connector_mqtt-ingress)</code>

  ingress 配置定义了此动作如何从远程 MQTT 代理接收消息，然后将它们发送到本地代理。<br/>
  允许在'remote.qos'、'local.topic'、'local.qos'、'local.retain'、'local.payload'中使用带有变量的模板。<br/>
  注意：如果将此动作用作规则的输入，并且还配置了'local.topic'，则从远程代理获取的消息将发送到'local.topic'和规则。

- egress: <code>[connector_mqtt:egress](#connector_mqtt-egress)</code>

  egress 配置定义了此动作如何将消息从本地代理转发到远程代理。<br/>
  允许在'remote.topic'、'local.qos'、'local.retain'、'local.payload'中使用带有变量的模板。<br/>
  注意：如果将此动作用作规则的动作，并且还配置了'local.topic'，则从规则获取的数据和与
  'local.topic'匹配的 MQTT 消息都将被转发。


## bridge_mqtt:creation_opts
资源启动相关的选项。


**Config paths**

 - <code>bridges.mqtt.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_mqtt_publisher:action_parameters
Action specific configs.


**Config paths**

 - <code>actions.mqtt.$name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__MQTT__$NAME__PARAMETERS</code>



**Fields**

- topic: <code>binary()</code>

  要转发到远程代理的主题。<br/>
  允许使用带有变量的模板。

- qos: <code>qos() | binary()</code>
  * default: 
  `1`

  要发送的 MQTT 消息的 QoS 级别。<br/>
  允许使用带有变量的模板。

- retain: <code>boolean() | binary()</code>
  * default: 
  `false`

  要发送的 MQTT 消息的'retain'标志。<br/>
  允许使用带有变量的模板。

- payload: <code>binary()</code>

  要发送的 MQTT 消息的有效载荷。<br/>
  允许使用带有变量的模板。


## bridge_mqtt_publisher:action_resource_opts
资源启动相关的选项。


**Config paths**

 - <code>actions.mqtt.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_ACTIONS__MQTT__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_mqtt_publisher:ingress_parameters
Source specific configs.


**Config paths**

 - <code>sources.mqtt.$name.parameters</code>


**Env overrides**

 - <code>EMQX_SOURCES__MQTT__$NAME__PARAMETERS</code>



**Fields**

- topic: <code>binary()</code>

  从远程代理接收消息的主题

- qos: <code>qos()</code>
  * default: 
  `1`

  订阅远程代理时要使用的 QoS 级别.


## bridge_mqtt_publisher:source_resource_opts
资源启动相关的选项。


**Config paths**

 - <code>sources.mqtt.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_SOURCES__MQTT__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。


## bridge_mqtt_publisher:mqtt_publisher_action
Action configs.


**Config paths**

 - <code>actions.mqtt.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__MQTT__$NAME</code>



**Fields**

- local_topic: <code>binary()</code>

  MQTT 主题或主题过滤器作为数据源（动作输入）。 如果规则动作用作数据源，则应将此配置留空，否则消息将在远程系统中重复。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用（是）或停用（否）此动作。

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- parameters: <code>[bridge_mqtt_publisher:action_parameters](#bridge_mqtt_publisher-action_parameters)</code>

  Action specific configs.

- resource_opts: <code>[bridge_mqtt_publisher:action_resource_opts](#bridge_mqtt_publisher-action_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_mqtt_publisher:mqtt_subscriber_source
Source configs.


**Config paths**

 - <code>sources.mqtt.$name</code>


**Env overrides**

 - <code>EMQX_SOURCES__MQTT__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用（是）或停用（否）此动作。

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- parameters: <code>[bridge_mqtt_publisher:ingress_parameters](#bridge_mqtt_publisher-ingress_parameters)</code>



- resource_opts: <code>[bridge_mqtt_publisher:source_resource_opts](#bridge_mqtt_publisher-source_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_mysql:action_parameters
特定于此动作类型的附加参数


**Config paths**

 - <code>actions.mysql.$name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__MYSQL__$NAME__PARAMETERS</code>



**Fields**

- sql: <code>binary()</code>
  * default: 
  `"insert into t_mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, FROM_UNIXTIME(${timestamp}/1000))"`

  SQL 模版


## bridge_mysql:connector_resource_opts
资源相关的选项。


**Config paths**

 - <code>connectors.mysql.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__MYSQL__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


## bridge_mysql:mysql_action
与 MySQL 连接器交互的动作


**Config paths**

 - <code>actions.mysql.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__MYSQL__$NAME</code>



**Fields**

- local_topic: <code>binary()</code>

  MQTT 主题或主题过滤器作为数据源（动作输入）。 如果规则动作用作数据源，则应将此配置留空，否则消息将在远程系统中重复。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用（是）或停用（否）此动作。

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- parameters: <code>[bridge_mysql:action_parameters](#bridge_mysql-action_parameters)</code>

  特定于此动作类型的附加参数

- resource_opts: <code>[actions_and_sources:action_resource_opts](#actions_and_sources-action_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_mysql:config
MySQL 动作的配置


**Config paths**

 - <code>bridges.mysql.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MYSQL__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用或停用该动作

- sql: <code>binary()</code>
  * default: 
  `"insert into t_mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, FROM_UNIXTIME(${timestamp}/1000))"`

  SQL 模版

- local_topic: <code>binary()</code>

  要转发到 MySQL 的 MQTT 主题过滤器。所有与 local_topic 匹配的 MQTT 'PUBLISH' 消息都将被转发。<br/>
  注意：如果此动作用作规则的操作（EMQX 规则引擎），并且还配置了 local_topic，则规则中获取的数据和与 local_topic 匹配的 MQTT 消息都将被转发。

- resource_opts: <code>[resource_schema:creation_opts](#resource_schema-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- server: <code>string()</code>

  用于连接的 IPv4 或 IPv6 地址或主机名。<br/>
  主机条目的格式如下：主机[:端口]。<br/>
  如果未指定[:端口]，将使用 MySQL 的默认端口3306。

- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>
  * default: 
  `root`

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## bridge_mysql:config_connector
MySQL 动作的配置


**Config paths**

 - <code>connectors.mysql.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__MYSQL__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- server: <code>string()</code>

  用于连接的 IPv4 或 IPv6 地址或主机名。<br/>
  主机条目的格式如下：主机[:端口]。<br/>
  如果未指定[:端口]，将使用 MySQL 的默认端口3306。

- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>
  * default: 
  `root`

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- resource_opts: <code>[bridge_mysql:connector_resource_opts](#bridge_mysql-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_opents:config
OpenTSDB 桥接配置


**Config paths**

 - <code>bridges.opents.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__OPENTS__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用桥接

- resource_opts: <code>[resource_schema:creation_opts](#resource_schema-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- server: <code>binary()</code>

  服务器的地址。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- summary: <code>boolean()</code>
  * default: 
  `true`

  是否返回摘要信息。

- details: <code>boolean()</code>
  * default: 
  `false`

  是否返回详细信息。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.


## bridge_oracle:config
Oracle Database 桥接配置


**Config paths**

 - <code>bridges.oracle.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__ORACLE__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用桥接

- sql: <code>binary()</code>
  * default: 
  `"insert into t_mqtt_msgs(msgid, topic, qos, payload) values (${id}, ${topic}, ${qos}, ${payload})"`

  SQL 模板。模板字符串可以包含消息元数据和有效载荷字段的占位符。占位符的插入不需要任何检查和特殊格式化，因此必须确保插入的数值格式化和转义正确。模板字符串可以包含占位符模板字符串可以包含消息元数据和有效载荷字段的占位符。这些占位符被插入所以必须确保插入的值的格式正确。因此，确保插入的值格式化和转义正确是非常重要的。模板字符串可以包含占位符模板字符串可以包含消息元数据和有效载荷字段的占位符。这些占位符被插入所以必须确保插入的值的格式正确。确保插入的值被正确地格式化和转义。

- local_topic: <code>binary()</code>

  发送到 'local_topic' 的消息都会转发到 Oracle Database。 <br/>注意：如果这个数据桥接被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。

- resource_opts: <code>[resource_schema:creation_opts](#resource_schema-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- server: <code>string()</code>

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>主机名具有以下形式：`Host[:Port]`。<br/>如果未指定 `[:Port]`，则使用 Oracle Database 默认端口 1521。

- sid: <code>binary()</code>

  Oracle Database Sid 名称

- service_name: <code>binary()</code>

  Oracle Database 服务名称。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.


## bridge_pgsql:action_parameters
特定于 PostgreSQL 动作的参数配置


**Config paths**

 - <code>actions.matrix.$name.parameters</code>
 - <code>actions.pgsql.$name.parameters</code>
 - <code>actions.timescale.$name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__MATRIX__$NAME__PARAMETERS</code>
 - <code>EMQX_ACTIONS__PGSQL__$NAME__PARAMETERS</code>
 - <code>EMQX_ACTIONS__TIMESCALE__$NAME__PARAMETERS</code>



**Fields**

- sql: <code>binary()</code>
  * default: 
  `"insert into t_mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, TO_TIMESTAMP((${timestamp} :: bigint)/1000))"`

  SQL 模板


## bridge_pgsql:pgsql_action
PostgreSQL 动作的配置


**Config paths**

 - <code>actions.matrix.$name</code>
 - <code>actions.pgsql.$name</code>
 - <code>actions.timescale.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__MATRIX__$NAME</code>
 - <code>EMQX_ACTIONS__PGSQL__$NAME</code>
 - <code>EMQX_ACTIONS__TIMESCALE__$NAME</code>



**Fields**

- local_topic: <code>binary()</code>

  MQTT 主题或主题过滤器作为数据源（动作输入）。 如果规则动作用作数据源，则应将此配置留空，否则消息将在远程系统中重复。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用（是）或停用（否）此动作。

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- parameters: <code>[bridge_pgsql:action_parameters](#bridge_pgsql-action_parameters)</code>

  特定于 PostgreSQL 动作的参数配置

- resource_opts: <code>[actions_and_sources:action_resource_opts](#actions_and_sources-action_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_pgsql:config
PostgreSQL 桥接配置


**Config paths**

 - <code>bridges.matrix.$name</code>
 - <code>bridges.pgsql.$name</code>
 - <code>bridges.timescale.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MATRIX__$NAME</code>
 - <code>EMQX_BRIDGES__PGSQL__$NAME</code>
 - <code>EMQX_BRIDGES__TIMESCALE__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用桥接

- sql: <code>binary()</code>
  * default: 
  `"insert into t_mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, TO_TIMESTAMP((${timestamp} :: bigint)/1000))"`

  SQL 模板

- local_topic: <code>binary()</code>

  发送到 'local_topic' 的消息都会转发到 PostgreSQL。 <br/>
  注意：如果这个数据桥接被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。

- resource_opts: <code>[resource_schema:creation_opts](#resource_schema-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- server: <code>string()</code>

  要连接的 IPv4 或 IPv6 地址或主机名。<br/>
  一个主机条目的格式为：Host[:Port]。<br/>
  如果没有指定 [:Port]，将使用 PostgreSQL 默认端口 5432。

- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## bridge_pgsql:config_connector
PostgreSQL 连接器的配置。


**Config paths**

 - <code>connectors.pgsql.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__PGSQL__$NAME</code>



**Fields**

- server: <code>string()</code>

  用于连接的 IPv4 或 IPv6 地址或主机名。<br/>
  一个主机条目具有以下格式：Host[:Port]。<br/>
  如果未指定 [:Port]，将使用 PostgreSQL 默认端口5432。

- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- resource_opts: <code>[connector_postgres:resource_opts](#connector_postgres-resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## bridge_pulsar:auth_basic
基本认证的参数。


**Config paths**

 - <code>bridges.pulsar_producer.$name.authentication</code>


**Env overrides**

 - <code>EMQX_BRIDGES__PULSAR_PRODUCER__$NAME__AUTHENTICATION</code>



**Fields**

- username: <code>binary()</code>

  基本认证用户名。

- password: <code>emqx_schema_secret:secret()</code>

  基本认证密码。


## bridge_pulsar:auth_token
令牌认证的参数。


**Config paths**

 - <code>bridges.pulsar_producer.$name.authentication</code>


**Env overrides**

 - <code>EMQX_BRIDGES__PULSAR_PRODUCER__$NAME__AUTHENTICATION</code>



**Fields**

- jwt: <code>emqx_schema_secret:secret()</code>

  JWT 认证令牌。


## bridge_pulsar:producer_buffer
配置消息缓存的相关参数。

当 EMQX 需要发送的消息超过 Pulsar 处理能力，或者当 Pulsar 临时下线时，EMQX 内部会将消息缓存起来。


**Config paths**

 - <code>bridges.pulsar_producer.$name.buffer</code>


**Env overrides**

 - <code>EMQX_BRIDGES__PULSAR_PRODUCER__$NAME__BUFFER</code>



**Fields**

- mode: <code>memory | disk | hybrid</code>
  * default: 
  `memory`

  消息缓存模式。
  <code>memory</code>: 所有的消息都缓存在内存里。如果 EMQX 服务重启，缓存的消息会丢失。
  <code>disk</code>: 缓存到磁盘上。EMQX 重启后会继续发送重启前未发送完成的消息。
  <code>hybrid</code>: 先将消息缓存在内存中，当内存中的消息堆积超过一定限制（配置项 <code>segment_bytes</code> 描述了该限制）后，后续的消息会缓存到磁盘上。与 <code>memory</code> 模式一样，如果 EMQX 服务重启，缓存的消息会丢失。

- per_partition_limit: <code>emqx_schema:bytesize()</code>
  * default: 
  `2GB`

  为每个 Pulsar 分区设置的最大缓存字节数。当超过这个上限之后，老的消息会被丢弃，为新的消息腾出空间。

- segment_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `100MB`

  当缓存模式是 <code>disk</code> 或 <code>hybrid</code> 时适用。该配置用于指定缓存到磁盘上的文件的大小。

- memory_overload_protection: <code>boolean()</code>
  * default: 
  `false`

  缓存模式是 <code>memory</code> 或 <code>hybrid</code> 时适用。当系统处于高内存压力时，从队列中丢弃旧的消息以减缓内存增长。内存压力值由配置项 <code>sysmon.os.sysmem_high_watermark</code> 决定。注意，该配置仅在 Linux 系统中有效。


## bridge_pulsar:producer_pulsar_message
用于生成 Pulsar 消息的模版。


**Config paths**

 - <code>bridges.pulsar_producer.$name.message</code>


**Env overrides**

 - <code>EMQX_BRIDGES__PULSAR_PRODUCER__$NAME__MESSAGE</code>



**Fields**

- key: <code>string()</code>
  * default: 
  `"${.clientid}"`

  生成 Pulsar 消息 Key 的模版。

- value: <code>string()</code>
  * default: 
  `"${.}"`

  生成 Pulsar 消息 Value 的模版。


## bridge_pulsar:producer_resource_opts
资源启动相关的选项。


**Config paths**

 - <code>bridges.pulsar_producer.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__PULSAR_PRODUCER__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `1s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


## bridge_pulsar:pulsar_producer
Pulsar 桥接配置


**Config paths**

 - <code>bridges.pulsar_producer.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__PULSAR_PRODUCER__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用（true）或停用（false）该 Pulsar 数据桥接。

- servers: <code>binary()</code>

  以逗号分隔的 <code>scheme://host[:port]</code> 格式的 Pulsar URL 列表，支持的 scheme 有 <code>pulsar://</code> （默认）和<code>pulsar+ssl://</code>。默认的端口是 6650。

- authentication: <code>none | [bridge_pulsar:auth_basic](#bridge_pulsar-auth_basic) | [bridge_pulsar:auth_token](#bridge_pulsar-auth_token)</code>
  * default: 
  `none`

  认证参数。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  建立 TCP 连接时的最大等待时长（若启用认证，这个等待时长也包含完成认证所需时间）。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- batch_size: <code>pos_integer()</code>
  * default: 
  `100`

  在一个 Pulsar 消息中批处理的单个请求的最大数量。

- compression: <code>no_compression | snappy | zlib</code>
  * default: 
  `no_compression`

  压缩方法。

- send_buffer: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

  TCP socket 的发送缓存调优。默认值是针对高吞吐量的一个推荐值。

- sync_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `3s`

  同步发布时，从 Pulsar 接收发送回执的最长等待时间。

- retention_period: <code>infinity | emqx_schema:duration_ms()</code>
  * default: 
  `infinity`

  当没有连接到 Pulsar 代理时，信息将被缓冲的时间。 较长的时间意味着将使用更多的内存/磁盘

- max_batch_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `900KB`

  最大消息批量字节数。大多数 Pulsar 环境的默认最低值是 5 MB，EMQX 的默认值比 5 MB 更小是因为需要补偿 Pulsar 消息编码所需要的额外字节（尤其是当每条消息都很小的情况下）。当单个消息的大小超过该限制时，它仍然会被发送，（相当于该批量中只有单个消息）。

- local_topic: <code>binary()</code>

  MQTT 主题数据源由桥接指定，或留空由规则动作指定。

- pulsar_topic: <code>binary()</code>

  Pulsar 主题名称

- strategy: <code>random | roundrobin | key_dispatch</code>
  * default: 
  `random`

  设置消息发布时应该如何选择 Pulsar 分区。

  <code>random</code>: 为每个消息随机选择一个分区。
  <code>roundrobin</code>: 依次为每条信息挑选可用的生产商。
  <code>key_dispatch</code>: 将一批信息中的第一条信息的 Pulsar 信息密钥哈希到一个分区编号。

- buffer: <code>[bridge_pulsar:producer_buffer](#bridge_pulsar-producer_buffer)</code>

  配置消息缓存的相关参数。

  当 EMQX 需要发送的消息超过 Pulsar 处理能力，或者当 Pulsar 临时下线时，EMQX 内部会将消息缓存起来。

- message: <code>[bridge_pulsar:producer_pulsar_message](#bridge_pulsar-producer_pulsar_message)</code>

  用于生成 Pulsar 消息的模版。

- resource_opts: <code>[bridge_pulsar:producer_resource_opts](#bridge_pulsar-producer_resource_opts)</code>

  资源启动相关的选项。


## bridge_rabbitmq:config
RabbitMQ 桥接配置


**Config paths**

 - <code>bridges.rabbitmq.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__RABBITMQ__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用或禁用桥接

- local_topic: <code>binary()</code>

  转发到 RabbitMQ 的消息 MQTT 主题或主题过滤器。对所有的 'PUBLISH' 消息，如果匹配到该配置的主题或主题过滤器时，
  注意：如果此桥接被用作规则的动作，同时又配置了 'local_topic'，那么这两份消息都会被转发到 RabbitMQ，这可能会导致消息重复。

- resource_opts: <code>[bridge_rabbitmq:creation_opts](#bridge_rabbitmq-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- server: <code>binary()</code>
  * default: 
  `localhost`

  RabbitMQ 服务器的主机名或 IP 地址（例如 localhost）。

- port: <code>emqx_schema:port_number()</code>
  * default: 
  `5672`

  RabbitMQ 服务器的主机名或 IP 地址（例如 localhost）。

- username: <code>binary()</code>

  用于认证的 RabbitMQ 用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池的大小。

- timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  连接超时时间。

- wait_for_publish_confirmations: <code>boolean()</code>
  * default: 
  `true`

  是否等待 RabbitMQ 确认消息发布。

- publish_confirmation_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `30s`

  连接超时时间。

- virtual_host: <code>binary()</code>
  * default: 
  `"/"`

  RabbitMQ 虚拟主机的名称。

- heartbeat: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `30s`

  发送心跳的间隔时间。

- exchange: <code>binary()</code>

   RabbitMQ Exchange 名称。

- routing_key: <code>binary()</code>

  用于路由消息的 Routing Key。

- delivery_mode: <code>non_persistent | persistent</code>
  * default: 
  `non_persistent`

  消息的投递模式。non_persistent(1) 表示非持久化消息，persistent(2) 表示持久化消息。

- payload_template: <code>binary()</code>
  * default: 
  `"${.}"`

  用于生成 RabbitMQ 消息的模版。模板中的占位符（例如 '${fields.sub_field}'）会被替换成消息的字段值。如果该留空，则表示所有的输入字段都会被转发到 RabbitMQ，与使用 '${.}' 作为占位符的效果等价。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## bridge_rabbitmq:creation_opts
资源启动相关的选项。


**Config paths**

 - <code>bridges.rabbitmq.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__RABBITMQ__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- batch_size: <code>pos_integer()</code>
  * default: 
  `1`

  最大批量请求大小。如果设为 1，则无批处理。

- batch_time: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_redis:action_parameters
动作的参数。


**Config paths**

 - <code>actions.redis.$name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__REDIS__$NAME__PARAMETERS</code>



**Fields**

- command_template: <code>[binary()]</code>

  用于导出消息的 Redis 命令模板。每个列表元素代表一个命令名称或其参数。
  例如，要将有效负载推送到 Redis 列表中的键 msgs，元素应该如下所示：
  rpush，msgs，${payload}。


## bridge_redis:redis_cluster
Cluster 模式。当 Redis 服务器在集群模式下运行时必须设置为'cluster'。


**Config paths**

 - <code>bridges.redis_cluster.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__REDIS_CLUSTER__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用数据桥接

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- local_topic: <code>binary()</code>

  要转发到 Redis 的 MQTT 主题过滤器。所有与 local_topic 匹配的 MQTT 'PUBLISH' 消息都将被转发。<br/>
  注意：如果此动作用作规则的动作（EMQX 规则引擎），并且还配置了 local_topic，则规则中获取的数据和与 local_topic 匹配的 MQTT 消息都将被转发。

- command_template: <code>[binary()]</code>

  用于导出消息的 Redis 命令模板。每个列表元素代表一个命令名称或其参数。
  例如，要将有效负载推送到 Redis 列表中的键 msgs，元素应该如下所示：
  rpush，msgs，${payload}。

- resource_opts: <code>[bridge_redis:creation_opts_redis_cluster](#bridge_redis-creation_opts_redis_cluster)</code>
  * default: 
  `{}`

  资源相关的选项。

- servers: <code>string()</code>

  集群将要连接的节点列表。 节点之间用逗号分隔，如：Node[,Node]。每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。主机名具有以下形式：Host[:Port]。如果未指定 [:Port]，则使用 Redis 默认端口 6379。

- redis_type: <code>cluster</code>
  * default: 
  `cluster`

  Cluster 模式。当 Redis 服务器在集群模式下运行时必须设置为'cluster'。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## bridge_redis:redis_sentinel
Sentinel 模式。 当 Redis 服务器在 Senitel 模式下运行时必须设置为 'sentinel' 。


**Config paths**

 - <code>bridges.redis_sentinel.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__REDIS_SENTINEL__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用数据桥接

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- local_topic: <code>binary()</code>

  要转发到 Redis 的 MQTT 主题过滤器。所有与 local_topic 匹配的 MQTT 'PUBLISH' 消息都将被转发。<br/>
  注意：如果此动作用作规则的动作（EMQX 规则引擎），并且还配置了 local_topic，则规则中获取的数据和与 local_topic 匹配的 MQTT 消息都将被转发。

- command_template: <code>[binary()]</code>

  用于导出消息的 Redis 命令模板。每个列表元素代表一个命令名称或其参数。
  例如，要将有效负载推送到 Redis 列表中的键 msgs，元素应该如下所示：
  rpush，msgs，${payload}。

- resource_opts: <code>[bridge_redis:creation_opts_redis_sentinel](#bridge_redis-creation_opts_redis_sentinel)</code>
  * default: 
  `{}`

  资源相关的选项。

- servers: <code>string()</code>

  集群将要连接的节点列表。 节点之间用逗号分隔，如：Node[,Node]。每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。主机名具有以下形式：Host[:Port]。如果未指定 [:Port]，则使用 Redis 默认端口 6379。

- redis_type: <code>sentinel</code>
  * default: 
  `sentinel`

  Sentinel 模式。 当 Redis 服务器在 Senitel 模式下运行时必须设置为 'sentinel' 。

- sentinel: <code>string()</code>

  Redis sentinel 模式下的集群名称。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- database: <code>non_neg_integer()</code>
  * default: 
  `0`

  Redis 数据库 ID。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## bridge_redis:redis_single
Single 模式。 当 Redis 服务器在 Single 模式下运行时必须设置为 'single' 。


**Config paths**

 - <code>bridges.redis_single.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__REDIS_SINGLE__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用数据桥接

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- local_topic: <code>binary()</code>

  要转发到 Redis 的 MQTT 主题过滤器。所有与 local_topic 匹配的 MQTT 'PUBLISH' 消息都将被转发。<br/>
  注意：如果此动作用作规则的动作（EMQX 规则引擎），并且还配置了 local_topic，则规则中获取的数据和与 local_topic 匹配的 MQTT 消息都将被转发。

- command_template: <code>[binary()]</code>

  用于导出消息的 Redis 命令模板。每个列表元素代表一个命令名称或其参数。
  例如，要将有效负载推送到 Redis 列表中的键 msgs，元素应该如下所示：
  rpush，msgs，${payload}。

- resource_opts: <code>[bridge_redis:creation_opts_redis_single](#bridge_redis-creation_opts_redis_single)</code>
  * default: 
  `{}`

  资源相关的选项。

- server: <code>string()</code>

  将要连接的 IPv4 或 IPv6 地址，或者主机名。主机名具有以下形式：Host[:Port]。如果未指定 [:Port]，则使用 Redis 默认端口 6379。

- redis_type: <code>single</code>
  * default: 
  `single`

  Single 模式。 当 Redis 服务器在 Single 模式下运行时必须设置为 'single' 。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- database: <code>non_neg_integer()</code>
  * default: 
  `0`

  Redis 数据库 ID。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## bridge_redis:creation_opts_redis_cluster
资源启动相关的选项。


**Config paths**

 - <code>bridges.redis_cluster.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__REDIS_CLUSTER__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_redis:creation_opts_redis_sentinel
资源启动相关的选项。


**Config paths**

 - <code>bridges.redis_sentinel.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__REDIS_SENTINEL__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- batch_size: <code>pos_integer()</code>
  * default: 
  `1`

  最大批量请求大小。如果设为 1，则无批处理。

- batch_time: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_redis:creation_opts_redis_single
资源启动相关的选项。


**Config paths**

 - <code>bridges.redis_single.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__REDIS_SINGLE__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- batch_size: <code>pos_integer()</code>
  * default: 
  `1`

  最大批量请求大小。如果设为 1，则无批处理。

- batch_time: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_rocketmq:config
RocketMQ 桥接配置


**Config paths**

 - <code>bridges.rocketmq.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__ROCKETMQ__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用桥接

- template: <code>binary()</code>
  * default: 
  `""`

  模板, 默认为空，为空时将会将整个消息转发给 RocketMQ。 <br/>
              模板可以是任意带有占位符的合法字符串, 例如:<br/>            ${id}, ${username}, ${clientid}, ${timestamp}<br/>
              {"id" : ${id}, "username" : ${username}}

- local_topic: <code>binary()</code>

  发送到 'local_topic' 的消息都会转发到 RocketMQ。 <br/>
  注意：如果这个数据桥接被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。

- resource_opts: <code>[resource_schema:creation_opts](#resource_schema-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- servers: <code>string()</code>

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
  主机名具有以下形式：`Host[:Port]`。<br/>
  如果未指定 `[:Port]`，则使用 RocketMQ 默认端口 9876。

- topic: <code>binary()</code>
  * default: 
  `TopicTest`

  RocketMQ 主题

- access_key: <code>binary()</code>
  * default: 
  `""`

  RocketMQ 服务器的 `accessKey`。

- secret_key: <code>emqx_schema_secret:secret()</code>
  * default: 
  `""`

  RocketMQ 服务器的 `secretKey`。

- security_token: <code>emqx_schema_secret:secret()</code>
  * default: 
  `""`

  RocketMQ 服务器安全令牌

- sync_timeout: <code>emqx_schema:timeout_duration()</code>
  * default: 
  `3s`

  RocketMQ 驱动同步调用的超时时间。

- refresh_interval: <code>emqx_schema:timeout_duration()</code>
  * default: 
  `3s`

  RocketMQ 主题路由更新间隔。

- send_buffer: <code>emqx_schema:bytesize()</code>
  * default: 
  `1024KB`

  RocketMQ 驱动的套字节发送消息的缓冲区大小

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.


## bridge_sqlserver:config
Microsoft SQL Server 桥接配置


**Config paths**

 - <code>bridges.sqlserver.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__SQLSERVER__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用桥接

- sql: <code>binary()</code>
  * default: 
  `"insert into t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )"`

  SQL 模板

- driver: <code>binary()</code>
  * default: 
  `ms-sql`

  SQL Server Driver 名称

- local_topic: <code>binary()</code>

  发送到 'local_topic' 的消息都会转发到 Microsoft SQL Server。 <br/>
  注意：如果这个数据桥接被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。

- resource_opts: <code>[bridge_sqlserver:creation_opts](#bridge_sqlserver-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- server: <code>string()</code>

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
  主机名具有以下形式：`Host[:Port]`。<br/>
  如果未指定 `[:Port]`，则使用 SQL Server 默认端口 1433。

- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>
  * default: 
  `sa`

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.


## bridge_sqlserver:creation_opts
资源启动相关的选项。


**Config paths**

 - <code>bridges.sqlserver.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__SQLSERVER__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- batch_size: <code>pos_integer()</code>
  * default: 
  `1`

  最大批量请求大小。如果设为 1，则无批处理。

- batch_time: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## bridge_tdengine:config
TDengine 桥接配置


**Config paths**

 - <code>bridges.tdengine.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__TDENGINE__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用/禁用桥接

- sql: <code>binary()</code>
  * default: 
  `"insert into t_mqtt_msg(ts, msgid, mqtt_topic, qos, payload, arrived) values (${ts}, '${id}', '${topic}', ${qos}, '${payload}', ${timestamp})"`

  SQL 模板

- local_topic: <code>binary()</code>

  发送到 'local_topic' 的消息都会转发到 TDengine。 <br/>
  注意：如果这个数据桥接被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。

- resource_opts: <code>[resource_schema:creation_opts](#resource_schema-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- server: <code>string()</code>

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
  主机名具有以下形式：`Host[:Port]`。<br/>
  如果未指定 `[:Port]`，则使用 TDengine 默认端口 6041。

- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>
  * default: 
  `root`

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.


## bridge_timescale:config_connector
PostgreSQL 连接器的配置。


**Config paths**

 - <code>connectors.timescale.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__TIMESCALE__$NAME</code>



**Fields**

- server: <code>string()</code>

  用于连接的 IPv4 或 IPv6 地址或主机名。<br/>
  一个主机条目具有以下格式：Host[:Port]。<br/>
  如果未指定 [:Port]，将使用 PostgreSQL 默认端口5432。

- database: <code>binary()</code>

  数据库名字。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- resource_opts: <code>[connector_postgres:resource_opts](#connector_postgres-resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## confluent:actions
动作的配置。


**Config paths**

 - <code>actions.confluent_producer.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__CONFLUENT_PRODUCER__$NAME</code>



**Fields**

- local_topic: <code>binary()</code>

  将 MQTT 主题或主题过滤器作为数据源（动作输入）。如果使用规则动作作为数据源，则应将此配置保留为空，否则消息将在 Confluent 中重复。

- parameters: <code>[confluent:producer_kafka_opts](#confluent-producer_kafka_opts)</code>

  Confluent 生产者配置。

- resource_opts: <code>[bridge_kafka:resource_opts](#bridge_kafka-resource_opts)</code>
  * default: 
  `{}`



- enable: <code>boolean()</code>
  * default: 
  `true`

  启用（是）或停用（否）此动作。

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。


## confluent:auth_username_password
基于用户名/密码的认证。


**Config paths**

 - <code>connectors.confluent_producer.$name.authentication</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__CONFLUENT_PRODUCER__$NAME__AUTHENTICATION</code>



**Fields**

- username: <code>binary()</code>

  Confluent 键。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。


## confluent:kafka_message
呈现 Confluent 消息的模版。


**Config paths**

 - <code>actions.confluent_producer.$name.parameters.message</code>


**Env overrides**

 - <code>EMQX_ACTIONS__CONFLUENT_PRODUCER__$NAME__PARAMETERS__MESSAGE</code>



**Fields**

- key: <code>string()</code>
  * default: 
  `"${.clientid}"`

  用于呈现 Confluent 消息键的模版。如果模板呈现为空值（即在规则引擎上下文中没有此类数据字段），则使用 Confluent 的 NULL（而不是空字符串）。

- value: <code>string()</code>
  * default: 
  `"${.}"`

  用于呈现 Confluent 消息键的模版。如果模板呈现为空值（即在规则引擎上下文中没有此类数据字段），则使用 Confluent 的 NULL（而不是空字符串）。


## confluent:producer_kafka_opts
Confluent 生产者配置。


**Config paths**

 - <code>actions.confluent_producer.$name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__CONFLUENT_PRODUCER__$NAME__PARAMETERS</code>



**Fields**

- topic: <code>string()</code>

  事件中心名称

- message: <code>[confluent:kafka_message](#confluent-kafka_message)</code>

  呈现 Confluent 消息的模版。

- max_batch_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `896KB`

  最大 Confluent 消息批量字节数。大多数 Kafka 环境的默认最低值是 1 MB，EMQX 的默认值比 1 MB 更小是因为需要补偿 Kafka 消息编码所需要的额外字节（尤其是当每条消息都很小的情况下）。当单个消息的大小超过该限制时，它仍然会被发送，（相当于该批量中只有单个消息）。

- compression: <code>no_compression | snappy | gzip</code>
  * default: 
  `no_compression`

  压缩方法。

- partition_strategy: <code>random | key_dispatch</code>
  * default: 
  `random`

  分区策略是用来指导生产者如何将消息分配到 Confluent 的各个分区。

  <code>random</code>: 为每条消息随机选择一个分区。
  <code>key_dispatch</code>: 根据 Confluent 消息键的哈希值将消息分配到分区，确保拥有相同键的消息能够一致地被分配到特定分区。

- required_acks: <code>all_isr | leader_only | none</code>
  * default: 
  `all_isr`

  Confluent 分区领导者的确认标准。它确定了在向 EMQX Confluent 生产者回送确认前，需要从追随者分区获得的确认等级。

  <code>all_isr</code>：要求所有同步副本进行确认。
  <code>leader_only</code>：仅要求分区领导者的确认。

- kafka_headers: <code>binary()</code>

  提供用作 Confluent 头部的占位符<br/>
  例如<code>${pub_props}</code><br/>
  注意，占位符的值必须是对象：
  <code>{"foo": "bar"}</code>
  或键值对数组：
  <code>[{"key": "foo", "value": "bar"}]</code>

- kafka_ext_headers: <code>[[bridge_kafka:producer_kafka_ext_headers](#bridge_kafka-producer_kafka_ext_headers)]</code>

  请提供更多的 Confluent 头部键值对<br/>
  这里的键值对将与<code>kafka_headers</code>字段的值结合后发送给 Confluent。

- kafka_header_value_encode_mode: <code>none | json</code>
  * default: 
  `none`

  Confluent 头部值编码模式<br/>
   - None：仅向 Confluent 头部添加二进制值；<br/>
   - JSON：仅向 Confluent 头部添加 JSON 值，并在发送前将其编码为 JSON 字符串。

- partition_count_refresh_interval: <code>emqx_schema:timeout_duration_s()</code>
  * default: 
  `60s`

  Confluent 生产者发现分区数量增加的时间间隔。
  在 Confluent 中增加分区数量后，EMQX 将开始根据<code>partition_strategy</code>
  在分发消息时考虑新发现的分区。

- max_inflight: <code>pos_integer()</code>
  * default: 
  `10`

  Confluent 生产者在接收到 Confluent 的确认之前，每个分区允许发送的批次的最大数量。较高的值通常意味着更好的吞吐量。然而，当这个值大于1时，可能会有消息重新排序的风险。

- buffer: <code>[bridge_kafka:producer_buffer](#bridge_kafka-producer_buffer)</code>

  配置生产者消息缓冲区。

  在 EMQX 有更多消息需要发送而 Confluent 跟不上时，或者 Confluent 宕机时，告诉 Confluent 生产者如何缓冲消息。

- query_mode: <code>async | sync</code>
  * default: 
  `async`

  查询模式。可选'sync/async'，默认'async'。

- sync_query_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  此参数定义同步查询的超时限制。仅在动作查询模式配置为'sync'时适用。


## confluent:ssl_client_opts
Confluent 客户端的 TLS/SSL 选项。


**Config paths**

 - <code>connectors.confluent_producer.$name.ssl</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__CONFLUENT_PRODUCER__$NAME__SSL</code>



**Fields**

- cacertfile: <code>binary()</code>

  受信任的 PEM 格式 CA  证书捆绑文件<br/>
  此文件中的证书用于验证 TLS 对等方的证书。
  如果要信任新 CA，请将新证书附加到文件中。
  无需重启 EMQX 即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
  注意：从文件中失效（删除）证书不会影响已建立的连接。

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>

  PEM 格式证书链文件<br/>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
  然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
  根 CA 证书是可选的，如果想要添加，应加到文件到最末端。

- keyfile: <code>binary()</code>

  PEM 格式的私钥文件。

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  启用 TLS 会话重用。

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
  因此，如果深度为 0，则对等方必须由受信任的根 CA 直接签名；<br/>
  如果是 1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
  如果是 2，则路径可以是 PEER、中间 CA1、中间 CA2、ROOT-CA。

- password: <code>string()</code>

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

  支持所有 TLS/DTLS 版本<br/>
  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。

- ciphers: <code>[string()]</code>
  * default: 
  `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  <br/>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
  注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  <br/>
  注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
  如果打算使用 PSK 密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

  <br/>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。

- server_name_indication: <code>auto | disable | string()</code>
  * default: 
  `auto`

  TLS 握手的服务器名称指示（SNI）设置。<br/>
  - <code>auto</code>：客户端将使用<code>"servicebus.windows.net"</code>作为 SNI。<br/>
  - <code>disable</code>：如果您不希望客户端发送 SNI。<br/>
  - 其他字符串值将按原样发送。


## confluent:config_connector
Confluent 动作的配置


**Config paths**

 - <code>connectors.confluent_producer.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__CONFLUENT_PRODUCER__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- bootstrap_hosts: <code>binary()</code>

  逗号分隔的 Confluent Kafka 命名空间主机名 <code>host[:port]</code> ，用于引导客户端。  默认端口号为 9092。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  TCP 连接建立的最大等待时间（包括启用认证时的认证时间）。

- min_metadata_refresh_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `3s`

  客户端刷新 Confluent Kafka 代理和主题元数据的最短时间间隔。设置过小的值可能会给 Confluent 增加额外的负载。

- metadata_request_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  从 Confluent 获取元数据的最大等待时间。

- authentication: <code>[confluent:auth_username_password](#confluent-auth_username_password)</code>
  * default: 
  `{}`

  认证配置

- socket_opts: <code>[bridge_kafka:socket_opts](#bridge_kafka-socket_opts)</code>

  额外的套接字选项。

- ssl: <code>[confluent:ssl_client_opts](#confluent-ssl_client_opts)</code>
  * default: 
  `{enable = true}`



- resource_opts: <code>[bridge_kafka:connector_resource_opts](#bridge_kafka-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## connector:connectors
用于连接到外部系统的连接器。


**Config paths**

 - <code>connectors</code>


**Env overrides**

 - <code>EMQX_CONNECTORS</code>



**Fields**

- http: <code>{$name -> [bridge_http:config_connector](#bridge_http-config_connector)}</code>

  HTTP Connector Config

- mqtt: <code>{$name -> [connector_mqtt:config_connector](#connector_mqtt-config_connector)}</code>

  MQTT Publisher Connector Config

- azure_event_hub_producer: <code>{$name -> [bridge_azure_event_hub:config_connector](#bridge_azure_event_hub-config_connector)}</code>

  Azure Event Hub Connector Config

- confluent_producer: <code>{$name -> [confluent:config_connector](#confluent-config_connector)}</code>

  Confluent Connector Config

- gcp_pubsub_producer: <code>{$name -> [gcp_pubsub_producer:config_connector](#gcp_pubsub_producer-config_connector)}</code>

  GCP PubSub Producer Connector Config

- kafka_producer: <code>{$name -> [bridge_kafka:config_connector](#bridge_kafka-config_connector)}</code>

  Kafka Connector Config

- matrix: <code>{$name -> [bridge_matrix:config_connector](#bridge_matrix-config_connector)}</code>

  Matrix Connector Config

- mongodb: <code>{$name -> [bridge_mongodb:config_connector](#bridge_mongodb-config_connector)}</code>

  MongoDB Connector Config

- influxdb: <code>{$name -> [bridge_influxdb:config_connector](#bridge_influxdb-config_connector)}</code>

  InfluxDB Connector Config

- mysql: <code>{$name -> [bridge_mysql:config_connector](#bridge_mysql-config_connector)}</code>

  MySQL Connector Config

- pgsql: <code>{$name -> [bridge_pgsql:config_connector](#bridge_pgsql-config_connector)}</code>

  PostgreSQL Connector Config

- redis: <code>{$name -> [redis:config_connector](#redis-config_connector)}</code>

  Redis Connector Config

- syskeeper_forwarder: <code>{$name -> [syskeeper_forwarder:config](#syskeeper_forwarder-config)}</code>

  Syskeeper Connector Config

- syskeeper_proxy: <code>{$name -> [connector_syskeeper_proxy:config](#connector_syskeeper_proxy-config)}</code>

  Syskeeper Proxy Connector Config

- timescale: <code>{$name -> [bridge_timescale:config_connector](#bridge_timescale-config_connector)}</code>

  Timescale Connector Config

- iotdb: <code>{$name -> [iotdb:config](#iotdb-config)}</code>

  IoTDB Connector Config

- elasticsearch: <code>{$name -> [elasticsearch:config](#elasticsearch-config)}</code>

  ElasticSearch Connector Config


## connector_http:request



**Config paths**

 - <code>authentication.$INDEX.request</code>
 - <code>authorization.sources.$INDEX.request</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__REQUEST</code>
 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX__REQUEST</code>



**Fields**

- method: <code>binary()</code>

  HTTP 请求方法。

- path: <code>binary()</code>

  URL 路径。

- body: <code>binary()</code>

  HTTP 请求的主体。

- headers: <code>map()</code>

  HTTP 请求头列表。

- max_retries: <code>non_neg_integer()</code>

  如果发送请求时出错，最大重试次数。

- request_timeout: <code>emqx_schema:timeout_duration_ms()</code>

  HTTP 请求超时时间。


## connector_mqtt:resource_opts
资源相关的选项。


**Config paths**

 - <code>connectors.mqtt.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__MQTT__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


## connector_mqtt:config_connector
Configurations for an MQTT connector.


**Config paths**

 - <code>connectors.mqtt.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__MQTT__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  将发布消息到远程代理的 MQTT 客户端池的大小。<br/>
  每个 MQTT 客户端都将分配'clientid'，格式为'${clientid_prefix}:${bridge_name}:egress:${node}:${n}'
  其中'n'是池中客户端的编号。

- resource_opts: <code>[connector_mqtt:resource_opts](#connector_mqtt-resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- mode: <code>cluster_shareload</code>

  Deprecated since v5.1.0 & e5.1.0.

- server: <code>string()</code>

  远程 MQTT 代理的主机和端口

- clientid_prefix: <code>binary()</code>

  附加到 egress 动作使用的 clientid 前缀（可选）。

- reconnect_interval: <code>string()</code>

  Deprecated since v5.0.16.

- proto_ver: <code>v3 | v4 | v5</code>
  * default: 
  `v4`

  MQTT协议版本

- bridge_mode: <code>boolean()</code>
  * default: 
  `false`

  如果启用桥接模式。
  注意：此设置仅适用于 MQTT 协议版本早于5.0的情况，远程 MQTT
  代理必须支持此功能。
  如果将 bridge_mode 设置为true，则桥接将指示远程代理它是一个桥接而不是普通客户端。
  这意味着循环检测将更加有效，并且保留的消息将被正确传递。

- username: <code>binary()</code>

  MQTT 协议的用户名

- password: <code>emqx_schema_secret:secret()</code>

  MQTT 协议的密码

- clean_start: <code>boolean()</code>
  * default: 
  `true`

  在重新连接到入口动作时是否启动新会话

- keepalive: <code>string()</code>
  * default: 
  `300s`

  MQTT Keepalive. Time interval is a string that contains a number followed by time unit:<br/>- `ms` for milliseconds,
  - `s` for seconds,
  - `m` for minutes,
  - `h` for hours;
  <br/>or combination of whereof: `1h5m0s`

- retry_interval: <code>string()</code>
  * default: 
  `15s`

  Message retry interval. Delay for the MQTT bridge to retry sending the QoS1/QoS2 messages in case of ACK not received. Time interval is a string that contains a number followed by time unit:<br/>- `ms` for milliseconds,
  - `s` for seconds,
  - `m` for minutes,
  - `h` for hours;
  <br/>or combination of whereof: `1h5m0s`

- max_inflight: <code>non_neg_integer()</code>
  * default: 
  `32`

  MQTT 协议的最大 inflight（已发送但未确认）消息数

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## connector_mqtt:egress
egress 配置定义了此动作如何将消息从本地代理转发到远程代理。<br/>
允许在'remote.topic'、'local.qos'、'local.retain'、'local.payload'中使用带有变量的模板。<br/>
注意：如果将此动作用作规则的动作，并且还配置了'local.topic'，则从规则获取的数据和与
'local.topic'匹配的 MQTT 消息都将被转发。


**Config paths**

 - <code>bridges.mqtt.$name.egress</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__EGRESS</code>



**Fields**

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  将发布消息到远程代理的 MQTT 客户端池的大小。<br/>
  每个 MQTT 客户端都将分配'clientid'，格式为'${clientid_prefix}:${bridge_name}:egress:${node}:${n}'
  其中'n'是池中客户端的编号。

- local: <code>[connector_mqtt:egress_local](#connector_mqtt-egress_local)</code>

  关于从本地代理接收消息的配置。

- remote: <code>[connector_mqtt:egress_remote](#connector_mqtt-egress_remote)</code>

  关于向远程代理发送消息的配置。


## connector_mqtt:egress_local
关于从本地代理接收消息的配置。


**Config paths**

 - <code>bridges.mqtt.$name.egress.local</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__EGRESS__LOCAL</code>



**Fields**

- topic: <code>binary()</code>

  要转发到远程代理的本地主题


## connector_mqtt:egress_remote
关于向远程代理发送消息的配置。


**Config paths**

 - <code>bridges.mqtt.$name.egress.remote</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__EGRESS__REMOTE</code>



**Fields**

- topic: <code>binary()</code>

  要转发到远程代理的主题。<br/>
  允许使用带有变量的模板。

- qos: <code>qos() | binary()</code>
  * default: 
  `1`

  要发送的 MQTT 消息的 QoS 级别。<br/>
  允许使用带有变量的模板。

- retain: <code>boolean() | binary()</code>
  * default: 
  `false`

  要发送的 MQTT 消息的'retain'标志。<br/>
  允许使用带有变量的模板。

- payload: <code>binary()</code>

  要发送的 MQTT 消息的有效载荷。<br/>
  允许使用带有变量的模板。


## connector_mqtt:ingress
ingress 配置定义了此动作如何从远程 MQTT 代理接收消息，然后将它们发送到本地代理。<br/>
允许在'remote.qos'、'local.topic'、'local.qos'、'local.retain'、'local.payload'中使用带有变量的模板。<br/>
注意：如果将此动作用作规则的输入，并且还配置了'local.topic'，则从远程代理获取的消息将发送到'local.topic'和规则。


**Config paths**

 - <code>bridges.mqtt.$name.ingress</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__INGRESS</code>



**Fields**

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  将从远程代理接收消息的 MQTT 客户端池的大小。<br/>
  仅当'remote.topic'是共享订阅主题或主题过滤器时才会尊重此值，
  （例如$share/name1/topic1或$share/name2/topic2/#），否则只会使用一个 MQTT 客户端。
  每个 MQTT 客户端都将分配'clientid'，格式为'${clientid_prefix}:${bridge_name}:ingress:${node}:${n}'
  其中'n'是池中客户端的编号。
  注意：当 EMQX 进行集群化时，非共享订阅将无法正常工作。

- remote: <code>[connector_mqtt:ingress_remote](#connector_mqtt-ingress_remote)</code>

  关于向远程代理订阅的配置。

- local: <code>[connector_mqtt:ingress_local](#connector_mqtt-ingress_local)</code>

  关于向本地代理发送消息的配置。


## connector_mqtt:ingress_local
关于向本地代理发送消息的配置。


**Config paths**

 - <code>bridges.mqtt.$name.ingress.local</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__INGRESS__LOCAL</code>



**Fields**

- topic: <code>binary()</code>

  要发送到本地代理的主题。<br/>
  允许使用带有变量的模板。

- qos: <code>qos() | binary()</code>
  * default: 
  `"${qos}"`

  要发送的 MQTT 消息的 QoS 级别。<br/>
  允许使用带有变量的模板。

- retain: <code>boolean() | binary()</code>
  * default: 
  `"${retain}"`

  要发送的 MQTT 消息的'retain'标志。<br/>
  允许使用带有变量的模板。

- payload: <code>binary()</code>

  要发送的 MQTT 消息的有效载荷。<br/>
  允许使用带有变量的模板。


## connector_mqtt:ingress_remote
关于向远程代理订阅的配置。


**Config paths**

 - <code>bridges.mqtt.$name.ingress.remote</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__INGRESS__REMOTE</code>



**Fields**

- topic: <code>binary()</code>

  从远程代理接收消息的主题

- qos: <code>qos()</code>
  * default: 
  `1`

  订阅远程代理时要使用的 QoS 级别.


## connector_postgres:resource_opts
资源相关的选项。


**Config paths**

 - <code>connectors.matrix.$name.resource_opts</code>
 - <code>connectors.pgsql.$name.resource_opts</code>
 - <code>connectors.timescale.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__MATRIX__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_CONNECTORS__PGSQL__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_CONNECTORS__TIMESCALE__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


## connector_syskeeper_proxy:config
Syskeeper 代理连接器的配置


**Config paths**

 - <code>connectors.syskeeper_proxy.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__SYSKEEPER_PROXY__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- listen: <code>string()</code>

  此 Syskeeper 代理服务器的监听地址

- acceptors: <code>non_neg_integer()</code>
  * default: 
  `16`

  接受者的数量

- handshake_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `10s`

  在创建连接时等待握手的最长时间

- resource_opts: <code>[connector_syskeeper_proxy:connector_resource_opts](#connector_syskeeper_proxy-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## connector_syskeeper_proxy:connector_resource_opts
资源相关的选项。


**Config paths**

 - <code>connectors.syskeeper_proxy.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__SYSKEEPER_PROXY__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


## dashboard:saml
saml


**Config paths**

 - <code>dashboard.sso.saml</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__SSO__SAML</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  是否启用该后端

- backend: <code>saml</code>

  ----

- dashboard_addr: <code>binary()</code>
  * default: 
  `"https://127.0.0.1:18083"`

  EMQX Dashboard 的地址。

- idp_metadata_url: <code>binary()</code>
  * default: 
  `"https://idp.example.com"`

  身份提供商的元数据地址。

- sp_sign_request: <code>boolean()</code>
  * default: 
  `false`

  是否签署 SAML 请求。

- sp_public_key: <code>binary()</code>
  * default: 
  `"Pub Key"`

  ----

- sp_private_key: <code>binary()</code>

  ----


## elasticsearch:auth_basic
Basic Authentication


**Config paths**

 - <code>connectors.elasticsearch.$name.authentication</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__ELASTICSEARCH__$NAME__AUTHENTICATION</code>



**Fields**

- username: <code>binary()</code>

  The username as configured at the ElasticSearch REST interface

- password: <code>emqx_schema_secret:secret()</code>

  The password as configured at the ElasticSearch REST interface


## elasticsearch:config
Configuration for ElasticSearch bridge.


**Config paths**

 - <code>connectors.elasticsearch.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__ELASTICSEARCH__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  连接到 HTTP 服务器的超时时间。

- pool_type: <code>random | hash</code>
  * default: 
  `random`

  连接池类型。可以是random、hash之一。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  一个正整数。是否连续发送 HTTP 请求，当设置为1时，意味着在发送每个 HTTP 请求后，需要等待服务器返回，然后继续发送下一个请求。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- resource_opts: <code>[bridge_http:connector_resource_opts](#bridge_http-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- server: <code>string()</code>
  * default: 
  `"127.0.0.1:9200"`

  The IPv4 or IPv6 address or the hostname to connect to.
  A host entry has the following form: `Host[:Port]`.
  The Elasticsearch default port 9200 is used if `[:Port]` is not specified.

- authentication: <code>[elasticsearch:auth_basic](#elasticsearch-auth_basic)</code>

  Authentication configuration


## gateway:coap
The CoAP protocol gateway provides EMQX with the access capability of the CoAP protocol.
It allows publishing, subscribing, and receiving messages to EMQX in accordance
with a certain defined CoAP message format.


**Config paths**

 - <code>gateway.coap</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP</code>



**Fields**

- heartbeat: <code>emqx_coap_schema:duration()</code>
  * default: 
  `30s`

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

  投递给 CoAP 客户端的通知消息类型。当客户端 Observe 一个资源（或订阅某个主题）时，网关会向客户端推送新产生的消息。其消息类型可设置为：<br/>

    - non: 不需要客户端返回确认消息;<br/>
    - con: 需要客户端返回一个确认消息;<br/>
    - qos: 取决于消息的 QoS 等级; QoS 0 会以 `non` 类型下发，QoS 1/2 会以 `con` 类型下发

- subscribe_qos: <code>qos0 | qos1 | qos2 | coap</code>
  * default: 
  `coap`

  客户端订阅请求的默认 QoS 等级。
  当 CoAP 客户端发起订阅请求时，如果未携带 `qos` 参数则会使用该默认值。默认值可设置为：<br/>
    - qos0、 qos1、qos2: 设置为固定的 QoS 等级<br/>
    - coap: 依据订阅操作的 CoAP 报文类型来动态决定<br/>
      * 当订阅请求为 `non-confirmable` 类型时，取值为 qos0<br/>
      * 当订阅请求为 `confirmable` 类型时，取值为 qos1

- publish_qos: <code>qos0 | qos1 | qos2 | coap</code>
  * default: 
  `coap`

  客户端发布请求的默认 QoS 等级。
  当 CoAP 客户端发起发布请求时，如果未携带 `qos` 参数则会使用该默认值。默认值可设置为：<br/>

    - qos0、qos1、qos2: 设置为固定的 QoS 等级<br/>
    - coap: 依据发布操作的 CoAP 报文类型来动态决定<br/>
      * 当发布请求为 `non-confirmable` 类型时，取值为 qos0<br/>
      * 当发布请求为 `confirmable` 类型时，取值为 qos1

- mountpoint: <code>binary()</code>
  * default: 
  `""`

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
  则客户端实际上订阅了 `some_tenant/t` 主题。
  类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
  则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：<br/>
     - <code>${clientid}</code>：clientid<br/>
     - <code>${username}</code>：用户名

- listeners: <code>[gateway:udp_listeners](#gateway-udp_listeners)</code>



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
  `30s`

  客户端连接过程的空闲时间。该配置用于：
    1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
    2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo 重写。


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

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
  则客户端实际上订阅了 `some_tenant/t` 主题。
  类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
  则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：<br/>
     - <code>${clientid}</code>：clientid<br/>
     - <code>${username}</code>：用户名

- listeners: <code>[gateway:tcp_udp_listeners](#gateway-tcp_udp_listeners)</code>



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
  `30s`

  客户端连接过程的空闲时间。该配置用于：
    1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
    2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo 重写。


## gateway:exproto_grpc_handler
Settings for the exproto gRPC connection handler.


**Config paths**

 - <code>gateway.exproto.handler</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__HANDLER</code>



**Fields**

- address: <code>binary()</code>

  对端 gRPC 服务器地址。

- service_name: <code>ConnectionHandler | ConnectionUnaryHandler</code>
  * default: 
  `ConnectionUnaryHandler`

  用于处理连接事件的服务名称
  在初始版本中，我们期望使用流来提高 ConnectionHandler 中请求的效率。
  但不幸的是，不同流之间的事件顺序混乱。
  这导致 OnSocketCreated 事件可能会
  在 OnReceivedBytes 之后到达。
  因此，自 v5.0.25 起，我们添加了 `ConnectionUnaryHandler` 服务，并强制在其中使用了 Unary，以避免顺序问题。

- ssl_options: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>

  gRPC 客户端的 SSL 配置。


## gateway:exproto_grpc_server
Settings for the exproto gRPC server.


**Config paths**

 - <code>gateway.exproto.server</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__SERVER</code>



**Fields**

- bind: <code>emqx_exproto_schema:ip_port()</code>

  服务监听地址和端口。

- ssl_options: <code>[gateway:ssl_server_opts](#gateway-ssl_server_opts)</code>

  服务 SSL 配置。


## gateway:ssl_server_opts
SSL configuration for the server.


**Config paths**

 - <code>gateway.exproto.server.ssl_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__SERVER__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cacert.pem"`

  受信任的 PEM 格式 CA  证书捆绑文件<br/>
  此文件中的证书用于验证 TLS 对等方的证书。
  如果要信任新 CA，请将新证书附加到文件中。
  无需重启 EMQX 即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
  注意：从文件中失效（删除）证书不会影响已建立的连接。

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cert.pem"`

  PEM 格式证书链文件<br/>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
  然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
  根 CA 证书是可选的，如果想要添加，应加到文件到最末端。

- keyfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/key.pem"`

  PEM 格式的私钥文件。

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  启用或禁用对等验证。

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  启用 TLS 会话重用。

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
  因此，如果深度为 0，则对等方必须由受信任的根 CA 直接签名；<br/>
  如果是 1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
  如果是 2，则路径可以是 PEER、中间 CA1、中间 CA2、ROOT-CA。

- password: <code>string()</code>

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

  支持所有 TLS/DTLS 版本<br/>
  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。

- ciphers: <code>[string()]</code>
  * default: 
  `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  <br/>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
  注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  <br/>
  注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
  如果打算使用 PSK 密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

  <br/>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。

- dhfile: <code>string()</code>

  如果协商使用 Diffie-Hellman 密钥交换的密码套件，则服务器将使用包含 PEM 编码的 Diffie-Hellman 参数的文件的路径。如果未指定，则使用默认参数。<br/>
  注意：TLS 1.3 不支持<code>dhfile</code>选项。

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

- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `15s`

  握手完成所允许的最长时间


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

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
  则客户端实际上订阅了 `some_tenant/t` 主题。
  类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
  则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：<br/>
     - <code>${clientid}</code>：clientid<br/>
     - <code>${username}</code>：用户名

- listeners: <code>[gateway:udp_listeners](#gateway-udp_listeners)</code>



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
  `30s`

  客户端连接过程的空闲时间。该配置用于：
    1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
    2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo 重写。


## gateway:mqttsn_predefined
The pre-defined topic name corresponding to the pre-defined topic
ID of N.

Note: the pre-defined topic ID of 0 is reserved.


**Config paths**

 - <code>gateway.mqttsn.predefined.$INDEX</code>


**Env overrides**

 - <code>EMQX_GATEWAY__MQTTSN__PREDEFINED__$INDEX</code>



**Fields**

- id: <code>1..1024</code>

  主题 ID。范围：1-65535

- topic: <code>binary()</code>

  主题名称。注：不支持通配符


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

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
  则客户端实际上订阅了 `some_tenant/t` 主题。
  类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
  则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：<br/>
     - <code>${clientid}</code>：clientid<br/>
     - <code>${username}</code>：用户名

- listeners: <code>[gateway:tcp_listeners](#gateway-tcp_listeners)</code>



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
  `30s`

  客户端连接过程的空闲时间。该配置用于：
    1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
    2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo 重写。


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

  头部的最大数量

- max_headers_length: <code>non_neg_integer()</code>
  * default: 
  `1024`

  允许的 Header 字符串的最大长度

- max_body_length: <code>integer()</code>
  * default: 
  `65536`

  每个 Stomp 数据包允许的 Body 的最大字节数


## gateway_gbt32960:gbt32960
The GBT-32960 gateway


**Config paths**

 - <code>gateway.gbt32960</code>


**Env overrides**

 - <code>EMQX_GATEWAY__GBT32960</code>



**Fields**

- mountpoint: <code>binary()</code>
  * default: 
  `"gbt32960/${clientid}/"`

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
  则客户端实际上订阅了 `some_tenant/t` 主题。
  类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
  则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：<br/>
     - <code>${clientid}</code>：clientid<br/>
     - <code>${username}</code>：用户名

- retry_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `8s`

  重新发送时间间隔

- max_retry_times: <code>non_neg_integer()</code>
  * default: 
  `3`

  最大重新发送次数

- message_queue_len: <code>non_neg_integer()</code>
  * default: 
  `10`

  最大消息队列长度

- listeners: <code>[gateway:tcp_listeners](#gateway-tcp_listeners)</code>



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
  `30s`

  客户端连接过程的空闲时间。该配置用于：
    1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
    2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo 重写。


## gateway_ocpp:dnstream
Download stream topic to forward the system message to device. Available placeholders:
- <code>cid</code>: Charge Point ID
- <code>clientid</code>: Equal to Charge Point ID
- <code>action</code>: Message Name in OCPP


**Config paths**

 - <code>gateway.ocpp.dnstream</code>


**Env overrides**

 - <code>EMQX_GATEWAY__OCPP__DNSTREAM</code>



**Fields**

- topic: <code>string()</code>
  * default: 
  `"cs/${cid}"`

  用于接收来自第三方系统的请求/控制消息的下载流主题。
  此值是每个连接的充电桩都订阅的通配符主题名称。

- max_mqueue_len: <code>integer()</code>
  * default: 
  `100`

  下载流消息传递的最大消息队列长度。


## gateway_ocpp:ocpp
The OCPP gateway


**Config paths**

 - <code>gateway.ocpp</code>


**Env overrides**

 - <code>EMQX_GATEWAY__OCPP</code>



**Fields**

- mountpoint: <code>binary()</code>
  * default: 
  `"ocpp/"`

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
  则客户端实际上订阅了 `some_tenant/t` 主题。
  类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
  则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：<br/>
     - <code>${clientid}</code>：clientid<br/>
     - <code>${username}</code>：用户名

- default_heartbeat_interval: <code>emqx_schema:duration_s()</code>
  * default: 
  `60s`

  默认的心跳时间间隔

- heartbeat_checking_times_backoff: <code>integer()</code>
  * default: 
  `1`

  用于心跳检查次数的退避时间

- upstream: <code>[gateway_ocpp:upstream](#gateway_ocpp-upstream)</code>



- dnstream: <code>[gateway_ocpp:dnstream](#gateway_ocpp-dnstream)</code>



- message_format_checking: <code>all | upstream_only | dnstream_only | disable</code>
  * default: 
  `disable`

  是否启用消息格式合法性检查。
  EMQX 会根据 json-schema 中定义的格式检查上传流和下载流的消息格式。
  当检查失败时，EMQX 将回复相应的答复消息。

  检查策略可以是以下值之一：
  - <code>all</code>：检查所有消息
  - <code>upstream_only</code>：仅检查上传流消息
  - <code>dnstream_only</code>：仅检查下载流消息
  - <code>disable</code>：不检查任何消息

- json_schema_dir: <code>string()</code>
  * default: 
  `"${application_priv}/schemas"`

  OCPP 消息定义的 JSON 模式目录。
  默认值：${application}/priv/schemas

- json_schema_id_prefix: <code>string()</code>
  * default: 
  `"urn:OCPP:1.6:2019:12:"`

  OCPP 消息模式的 ID 前缀。

- listeners: <code>[gateway_ocpp:ws_listeners](#gateway_ocpp-ws_listeners)</code>



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
  `30s`

  客户端连接过程的空闲时间。该配置用于：
    1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
    2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo 重写。


## gateway_ocpp:upstream
Upload stream topic to notify third-party system what's messages/events reported by Charge Point. Available placeholders:
- <code>cid</code>: Charge Point ID
- <code>clientid</code>: Equal to Charge Point ID
- <code>action</code>: Message Name in OCPP


**Config paths**

 - <code>gateway.ocpp.upstream</code>


**Env overrides**

 - <code>EMQX_GATEWAY__OCPP__UPSTREAM</code>



**Fields**

- topic: <code>string()</code>
  * default: 
  `"cp/${cid}"`

  上传流呼叫请求消息主题。

- topic_override_mapping: <code>{$name -> string()}</code>
  * default: 
  `{}`

  通过消息名称进行的上传流主题覆盖映射。

- reply_topic: <code>string()</code>
  * default: 
  `"cp/${cid}/Reply"`

  上传流回复消息主题。

- error_topic: <code>string()</code>
  * default: 
  `"cp/${cid}/Reply"`

  上传流错误主题。


## gateway_ocpp:websocket
Websocket options


**Config paths**

 - <code>gateway.ocpp.listeners.ws.$name.websocket</code>
 - <code>gateway.ocpp.listeners.wss.$name.websocket</code>


**Env overrides**

 - <code>EMQX_GATEWAY__OCPP__LISTENERS__WS__$NAME__WEBSOCKET</code>
 - <code>EMQX_GATEWAY__OCPP__LISTENERS__WSS__$NAME__WEBSOCKET</code>



**Fields**

- path: <code>string()</code>
  * default: 
  `"/ocpp"`

  WebSocket 的M QTT 协议路径。因此，EMQX Broker 的 WebSocket 地址是：<code>ws://{ip}:{port}/mqtt</code>

- piggyback: <code>single | multiple</code>
  * default: 
  `single`

  WebSocket 消息是否允许包含多个 MQTT 数据包。

- compress: <code>boolean()</code>
  * default: 
  `false`

  如果设置为<code>true</code>，将使用<code>zlib</code>压缩 WebSocket 消息。<br/>
  <code>deflate_opts</code>下的配置项属于与压缩相关的参数配置。

- idle_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `7200s`

  关闭未在此间隔内发送 MQTT CONNECT 消息的客户端的传输层连接。

- max_frame_size: <code>infinity | integer()</code>
  * default: 
  `infinity`

  单个 MQTT 数据包的最大长度。

- fail_if_no_subprotocol: <code>boolean()</code>
  * default: 
  `true`

  如果设置为<code>true</code>，当客户端不携带<code>Sec-WebSocket-Protocol</code>字段时，服务器将返回错误。
  <br/>注意：小程序需要禁用此验证。

- supported_subprotocols: <code>emqx_schema:comma_separated_list()</code>
  * default: 
  `"ocpp1.6, ocpp2.0"`

  逗号分隔的支持的子协议列表。

- check_origin_enable: <code>boolean()</code>
  * default: 
  `false`

  如果设置为<code>true</code>，将验证<code>origin</code> HTTP 请求头是否在<code>check_origins</code>参数中配置的允许来源列表中。

- allow_origin_absence: <code>boolean()</code>
  * default: 
  `true`

  如果设置为<code>false</code>，且<code>check_origin_enable</code>为<code>true</code>，服务器将拒绝没有<code>origin</code> HTTP 请求头的请求。

- check_origins: <code>emqx_schema:comma_separated_binary()</code>
  * default: 
  `"http://localhost:18083, http://127.0.0.1:18083"`

  允许来源的列表。<br/>参见<code>check_origin_enable</code>。

- proxy_address_header: <code>string()</code>
  * default: 
  `x-forwarded-for`

  用于传递客户端 IP 地址信息的 HTTP 请求头。
  当 EMQX 集群部署在负载均衡器后面时相关。

- proxy_port_header: <code>string()</code>
  * default: 
  `x-forwarded-port`

  用于传递客户端端口信息的 HTTP 请求头。
  当 EMQX 集群部署在负载均衡器后面时相关。

- deflate_opts: <code>[broker:deflate_opts](#broker-deflate_opts)</code>




## gateway_ocpp:ws_listener
Websocket listener


**Config paths**

 - <code>gateway.ocpp.listeners.ws.$name</code>


**Env overrides**

 - <code>EMQX_GATEWAY__OCPP__LISTENERS__WS__$NAME</code>



**Fields**

- acceptors: <code>integer()</code>
  * default: 
  `16`

  Acceptor 进程池大小。

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>

  TCP Socket 配置。

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`

  是否开启 Proxy Protocol V1/2。当 EMQX 集群部署在 HAProxy 或 Nginx 后需要获取客户端真实 IP 时常用到该选项。参考：https://www.haproxy.com/blog/haproxy/proxy-protocol/

- proxy_protocol_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `3s`

  接收 Proxy Protocol 报文头的超时时间。如果在超时内没有收到 Proxy Protocol 包，EMQX 将关闭 TCP 连接。

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该监听器。

- bind: <code>emqx_gateway_schema:ip_port()</code>

  监听器绑定的 IP 地址或端口。

- max_connections: <code>pos_integer() | infinity</code>
  * default: 
  `1024`

  监听器支持的最大连接数。

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  监听器支持的最大连接速率。

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。

- mountpoint: <code>binary()</code>

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
  则客户端实际上订阅了 `some_tenant/t` 主题。
  类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
  则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：<br/>
     - <code>${clientid}</code>：clientid<br/>
     - <code>${username}</code>：用户名

- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  配置监听器的访问控制规则。
  见：https://github.com/emqtt/esockd#allowdeny

- websocket: <code>[gateway_ocpp:websocket](#gateway_ocpp-websocket)</code>




## gateway_ocpp:ws_listeners
Websocket listeners


**Config paths**

 - <code>gateway.ocpp.listeners</code>


**Env overrides**

 - <code>EMQX_GATEWAY__OCPP__LISTENERS</code>



**Fields**

- ws: <code>{$name -> [gateway_ocpp:ws_listener](#gateway_ocpp-ws_listener)}</code>

  WebSocket 监听器。

- wss: <code>{$name -> [gateway_ocpp:wss_listener](#gateway_ocpp-wss_listener)}</code>

  WebSocket over TLS 监听器。


## gateway_ocpp:wss_listener
Websocket over TLS listener


**Config paths**

 - <code>gateway.ocpp.listeners.wss.$name</code>


**Env overrides**

 - <code>EMQX_GATEWAY__OCPP__LISTENERS__WSS__$NAME</code>



**Fields**

- acceptors: <code>integer()</code>
  * default: 
  `16`

  Acceptor 进程池大小。

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>

  TCP Socket 配置。

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`

  是否开启 Proxy Protocol V1/2。当 EMQX 集群部署在 HAProxy 或 Nginx 后需要获取客户端真实 IP 时常用到该选项。参考：https://www.haproxy.com/blog/haproxy/proxy-protocol/

- proxy_protocol_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `3s`

  接收 Proxy Protocol 报文头的超时时间。如果在超时内没有收到 Proxy Protocol 包，EMQX 将关闭 TCP 连接。

- enable: <code>boolean()</code>
  * default: 
  `true`

  是否启用该监听器。

- bind: <code>emqx_gateway_schema:ip_port()</code>

  监听器绑定的 IP 地址或端口。

- max_connections: <code>pos_integer() | infinity</code>
  * default: 
  `1024`

  监听器支持的最大连接数。

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  监听器支持的最大连接速率。

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证。
  配置 <code>false</code> 时，将不对客户端做任何认证。

- mountpoint: <code>binary()</code>

  发布或订阅时，在所有主题前增加前缀字符串。
  当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
  例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
  则客户端实际上订阅了 `some_tenant/t` 主题。
  类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
  则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
  挂载点字符串中可用的变量：<br/>
     - <code>${clientid}</code>：clientid<br/>
     - <code>${username}</code>：用户名

- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  配置监听器的访问控制规则。
  见：https://github.com/emqtt/esockd#allowdeny

- ssl_options: <code>[broker:listener_wss_opts](#broker-listener_wss_opts)</code>

  SSL Socket 配置。

- websocket: <code>[gateway_ocpp:websocket](#gateway_ocpp-websocket)</code>




## gcp_pubsub_producer:action_parameters
动作的具体配置。


**Config paths**

 - <code>actions.gcp_pubsub_producer.$name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__GCP_PUBSUB_PRODUCER__$NAME__PARAMETERS</code>



**Fields**

- attributes_template: <code>[[bridge_gcp_pubsub:key_value_pair](#bridge_gcp_pubsub-key_value_pair)]</code>
  * default: 
  `[]`

  格式化出站消息属性的模板。未定义的值将被呈现为空字符串值。属性映射中的空键将被移除。

- ordering_key_template: <code>binary()</code>
  * default: 
  `""`

  格式化出站消息排序键的模板。未定义的值将被呈现为空字符串值。如果此值为空，则不会将其添加到消息中。

- payload_template: <code>binary()</code>
  * default: 
  `""`

  用于格式化外发信息的模板。 如果未定义，将以 JSON 格式发送所有可用的上下文。

- pubsub_topic: <code>binary()</code>

  要发布消息的 GCP PubSub 主题。


## gcp_pubsub_producer:connector_resource_opts
资源相关的选项。


**Config paths**

 - <code>connectors.gcp_pubsub_producer.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__GCP_PUBSUB_PRODUCER__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


## gcp_pubsub_producer:producer_action
动作的配置


**Config paths**

 - <code>actions.gcp_pubsub_producer.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__GCP_PUBSUB_PRODUCER__$NAME</code>



**Fields**

- local_topic: <code>binary()</code>

  MQTT 主题或主题过滤器作为数据源（动作输入）。 如果规则动作用作数据源，则应将此配置留空，否则消息将在远程系统中重复。

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用（是）或停用（否）此动作。

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- parameters: <code>[gcp_pubsub_producer:action_parameters](#gcp_pubsub_producer-action_parameters)</code>

  动作的配置

- resource_opts: <code>[actions_and_sources:action_resource_opts](#actions_and_sources-action_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## gcp_pubsub_producer:config_connector
GCP PubSub 生产者客户端的具体配置。


**Config paths**

 - <code>connectors.gcp_pubsub_producer.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__GCP_PUBSUB_PRODUCER__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  连接 HTTP 服务器的超时时间。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `2`

  请求出错时的最大重试次数。

- request_timeout: <code>emqx_schema:timeout_duration_ms()</code>

  Deprecated since e5.0.1.

- service_account_json: <code>emqx_bridge_gcp_pubsub:service_account_json()</code>

  包含将与 PubSub 一起使用的 GCP 服务账户凭证的 JSON。
  当创建 GCP 服务账户时（如 https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount），可以选择下载 JSON 形式的凭证，然后在该配置项中使用。

- resource_opts: <code>[gcp_pubsub_producer:connector_resource_opts](#gcp_pubsub_producer-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## iotdb:auth_basic
Basic Authentication


**Config paths**

 - <code>connectors.iotdb.$name.authentication</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__IOTDB__$NAME__AUTHENTICATION</code>



**Fields**

- username: <code>binary()</code>

  The username as configured at the IoTDB REST interface

- password: <code>emqx_schema_secret:secret()</code>

  The password as configured at the IoTDB REST interface


## iotdb:config
Configuration for Apache IoTDB bridge.


**Config paths**

 - <code>connectors.iotdb.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__IOTDB__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  连接到 HTTP 服务器的超时时间。

- pool_type: <code>random | hash</code>
  * default: 
  `random`

  连接池类型。可以是random、hash之一。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  一个正整数。是否连续发送 HTTP 请求，当设置为1时，意味着在发送每个 HTTP 请求后，需要等待服务器返回，然后继续发送下一个请求。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- resource_opts: <code>[bridge_http:connector_resource_opts](#bridge_http-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。

- base_url: <code>emqx_schema:url()</code>

  IoTDB REST 服务的 URL。

- iotdb_version: <code>v1.1.x | v1.0.x | v0.13.x</code>
  * default: 
  `v1.1.x`

  IoTDB 版本。

- authentication: <code>[iotdb:auth_basic](#iotdb-auth_basic)</code>
  * default: 
  `auth_basic`

  Authentication configuration


## ldap:ssl
启用 SSL 连接。


**Config paths**

 - <code>authentication.$INDEX.ssl</code>
 - <code>authorization.sources.$INDEX.ssl</code>
 - <code>dashboard.sso.ldap.ssl</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX__SSL</code>
 - <code>EMQX_DASHBOARD__SSO__LDAP__SSL</code>



**Fields**

- cacertfile: <code>binary()</code>

  受信任的 PEM 格式 CA  证书捆绑文件<br/>
  此文件中的证书用于验证 TLS 对等方的证书。
  如果要信任新 CA，请将新证书附加到文件中。
  无需重启 EMQX 即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
  注意：从文件中失效（删除）证书不会影响已建立的连接。

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>

  PEM 格式证书链文件<br/>
  此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
  然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
  根 CA 证书是可选的，如果想要添加，应加到文件到最末端。

- keyfile: <code>binary()</code>

  PEM 格式的私钥文件。

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  启用或禁用对等验证。

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  启用 TLS 会话重用。

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
  因此，如果深度为 0，则对等方必须由受信任的根 CA 直接签名；<br/>
  如果是 1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
  如果是 2，则路径可以是 PEER、中间 CA1、中间 CA2、ROOT-CA。

- password: <code>string()</code>

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

  支持所有 TLS/DTLS 版本<br/>
  注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。

- ciphers: <code>[string()]</code>
  * default: 
  `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
  <br/>
  密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
  选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

  名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
  EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
  注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
  不兼容的密码套件将被自动删除。

  例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

  <br/>
  注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
  如果打算使用 PSK 密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

  <br/>
  PSK 密码套件：
  <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
  RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。

- enable: <code>boolean()</code>
  * default: 
  `false`

  启用 TLS。

- server_name_indication: <code>disable | string()</code>

  指定要在 TLS 服务器名称指示扩展中使用的主机名。<br/>
  例如，当连接到 "server.example.net" 时，接受连接并执行 TLS 握手的真正服务器可能与 TLS 客户端最初连接到的主机不同，
  例如，当连接到 IP 地址时，或者当主机具有多个可解析的 DNS 记录时<br/>
  如果未指定，它将默认为使用的主机名字符串
  建立连接，除非使用 IP 地址<br/>
  然后，主机名也用于对等机的主机名验证证书<br/>
  特殊值 <code>disable</code> 阻止发送服务器名称指示扩展，并禁用主机名验证检查。


## license:key_license
License provisioned as a string.


**Config paths**

 - <code>license</code>


**Env overrides**

 - <code>EMQX_LICENSE</code>



**Fields**

- key: <code>default | binary()</code>
  * default: 
  `default`

  申请或购买 License 时获得的密钥字符串

- connection_low_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `75%`

  连接数低于此值是，系统会清除连接配额使用告警

- connection_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `80%`

  连接数超过该值时，系统会触发 License 连接配额使用告警


## mongo:topology
MongoDB 的拓扑结构。


**Config paths**

 - <code>authentication.$INDEX.topology</code>
 - <code>authorization.sources.$INDEX.topology</code>
 - <code>bridges.mongodb_rs.$name.topology</code>
 - <code>bridges.mongodb_sharded.$name.topology</code>
 - <code>bridges.mongodb_single.$name.topology</code>
 - <code>connectors.mongodb.$name.topology</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__TOPOLOGY</code>
 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX__TOPOLOGY</code>
 - <code>EMQX_BRIDGES__MONGODB_RS__$NAME__TOPOLOGY</code>
 - <code>EMQX_BRIDGES__MONGODB_SHARDED__$NAME__TOPOLOGY</code>
 - <code>EMQX_BRIDGES__MONGODB_SINGLE__$NAME__TOPOLOGY</code>
 - <code>EMQX_CONNECTORS__MONGODB__$NAME__TOPOLOGY</code>



**Fields**

- max_overflow: <code>non_neg_integer()</code>
  * default: 
  `0`

  在池中的所有工作线程都繁忙时，可以创建的附加工作线程的最大数量。这有助于通过允许更多并发连接到 MongoDB 服务器来管理工作负载的暂时性波动。

- overflow_ttl: <code>emqx_schema:timeout_duration_ms()</code>

  超出配置的池大小（"溢出"）的工作线程在终止之前的时间段。

- overflow_check_period: <code>emqx_schema:timeout_duration_ms()</code>

  检查是否存在多余工作线程（"溢出"）的周期。

- local_threshold_ms: <code>emqx_schema:timeout_duration_ms()</code>

  用于在多个合适的 MongoDB 实例中进行选择的延迟窗口的大小。

- connect_timeout_ms: <code>emqx_schema:timeout_duration_ms()</code>

  在超时之前尝试连接的持续时间。

- socket_timeout_ms: <code>emqx_schema:timeout_duration_ms()</code>

  在套接字上尝试发送或接收超时之前的持续时间。

- server_selection_timeout_ms: <code>emqx_schema:timeout_duration_ms()</code>

  指定在抛出异常之前进行服务器选择的阻塞时间。

- wait_queue_timeout_ms: <code>emqx_schema:timeout_duration_ms()</code>

  工作线程等待连接可用的最长时间。

- heartbeat_frequency_ms: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `200s`

  控制驱动程序何时检查 MongoDB 部署的状态。指定检查之间的时间间隔，从上次检查结束到下次检查开始计算。如果连接数增加（例如，如果增加了池大小），则可能需要增加此时间间隔，以避免在 MongoDB 日志文件中创建过多的日志条目。

- min_heartbeat_frequency_ms: <code>emqx_schema:timeout_duration_ms()</code>

  控制心跳之间等待的最短时间。


## mongo:connector_rs
副本集的设置。


**Config paths**

 - <code>connectors.mongodb.$name.parameters</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__MONGODB__$NAME__PARAMETERS</code>



**Fields**

- mongo_type: <code>rs</code>
  * default: 
  `rs`

  副本集。当MongoDB服务器以`副本集`模式运行时，必须设置为`rs`。

- servers: <code>string()</code>

  集群连接的节点列表。节点应使用逗号分隔，例如：节点[,节点]。
  对于每个节点，应该是要连接的 IPv4 或 IPv6 地址或主机名。
  主机条目具有以下形式：主机[:端口]。
  如果未指定[:端口]，则使用 MongoDB 的默认端口27017。

- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写入模式

- r_mode: <code>master | slave_ok</code>
  * default: 
  `master`

  读取模式。

- replica_set_name: <code>binary()</code>

  副本集的名称。


## mongo:connector_sharded
分片集群的设置。


**Config paths**

 - <code>connectors.mongodb.$name.parameters</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__MONGODB__$NAME__PARAMETERS</code>



**Fields**

- mongo_type: <code>sharded</code>
  * default: 
  `sharded`

  分片集群。当 MongoDB 服务器以`分片`模式运行时，必须设置为`sharded`。

- servers: <code>string()</code>

  集群连接的节点列表。节点应使用逗号分隔，例如：节点[,节点]。
  对于每个节点，应该是要连接的 IPv4 或 IPv6 地址或主机名。
  主机条目具有以下形式：主机[:端口]。
  如果未指定[:端口]，则使用 MongoDB 的默认端口27017。

- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写入模式


## mongo:connector_single
单个 MongoDB 实例的设置


**Config paths**

 - <code>connectors.mongodb.$name.parameters</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__MONGODB__$NAME__PARAMETERS</code>



**Fields**

- mongo_type: <code>single</code>
  * default: 
  `single`

  独立实例。当MongoDB服务器以独立模式运行时，必须设置为`single`。

- server: <code>string()</code>

  要连接的 IPv4 或 IPv6 地址或主机名。<br/>主机条目具有以下形式：主机[:端口]。<br/>如果未指定[:端口]，则使用MongoDB的默认端口27017。

- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  写入模式


## plugin:plugins
管理 EMQX 插件。<br/>
插件可以是 EMQX 安装包中的一部分，也可以是一个独立的安装包。<br/>
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
  `plugins`

  插件安装包的目录，出于安全考虑，该目录应该值允许 <code>emqx</code>，或用于运行 EMQX 服务的用户拥有写入权限。

- check_interval: <code>emqx_schema:duration()</code>

  Deprecated since 5.0.24.


## plugin:state
描述插件的状态


**Config paths**

 - <code>plugins.states.$INDEX</code>


**Env overrides**

 - <code>EMQX_PLUGINS__STATES__$INDEX</code>



**Fields**

- name_vsn: <code>string()</code>

  插件的名称{name}-{version}。<br/>
  它应该与插件的发布包名称一致，如 my_plugin-0.1.0。

- enable: <code>boolean()</code>

  设置为“true”以启用此插件。


## psk:psk_authentication
此配置用于启用 TLS-PSK 身份验证。

PSK 是 “Pre-Shared-Keys” 的缩写。

注意: 确保 SSL 监听器仅启用了 'tlsv1.2'，并且配置了 PSK 密码套件，例如 'RSA-PSK-AES256-GCM-SHA384'。

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


## redis:redis_cluster_connector
集群模式下的 Redis 连接器。


**Config paths**

 - <code>connectors.redis.$name.parameters</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__REDIS__$NAME__PARAMETERS</code>



**Fields**

- servers: <code>string()</code>

  集群将要连接的节点列表。 节点之间用逗号分隔，如：Node[,Node]。每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。主机名具有以下形式：Host[:Port]。如果未指定 [:Port]，则使用 Redis 默认端口 6379。

- redis_type: <code>cluster</code>
  * default: 
  `cluster`

  Cluster 模式。当 Redis 服务器在集群模式下运行时必须设置为'cluster'。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.


## redis:redis_sentinel_connector
Sentinel 模式下的 Redis 连接器。


**Config paths**

 - <code>connectors.redis.$name.parameters</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__REDIS__$NAME__PARAMETERS</code>



**Fields**

- servers: <code>string()</code>

  集群将要连接的节点列表。 节点之间用逗号分隔，如：Node[,Node]。每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。主机名具有以下形式：Host[:Port]。如果未指定 [:Port]，则使用 Redis 默认端口 6379。

- redis_type: <code>sentinel</code>
  * default: 
  `sentinel`

  Sentinel 模式。 当 Redis 服务器在 Senitel 模式下运行时必须设置为 'sentinel' 。

- sentinel: <code>string()</code>

  Redis sentinel 模式下的集群名称。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- database: <code>non_neg_integer()</code>
  * default: 
  `0`

  Redis 数据库 ID。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.


## redis:redis_single_connector
Single 模式下的 Redis 连接器。


**Config paths**

 - <code>connectors.redis.$name.parameters</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__REDIS__$NAME__PARAMETERS</code>



**Fields**

- server: <code>string()</code>

  将要连接的 IPv4 或 IPv6 地址，或者主机名。主机名具有以下形式：Host[:Port]。如果未指定 [:Port]，则使用 Redis 默认端口 6379。

- redis_type: <code>single</code>
  * default: 
  `single`

  Single 模式。 当 Redis 服务器在 Single 模式下运行时必须设置为 'single' 。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- database: <code>non_neg_integer()</code>
  * default: 
  `0`

  Redis 数据库 ID。

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.


## resource_schema:creation_opts
资源启动相关的选项。


**Config paths**

 - <code>bridges.cassandra.$name.resource_opts</code>
 - <code>bridges.gcp_pubsub.$name.resource_opts</code>
 - <code>bridges.greptimedb.$name.resource_opts</code>
 - <code>bridges.hstreamdb.$name.resource_opts</code>
 - <code>bridges.influxdb_api_v1.$name.resource_opts</code>
 - <code>bridges.influxdb_api_v2.$name.resource_opts</code>
 - <code>bridges.matrix.$name.resource_opts</code>
 - <code>bridges.mysql.$name.resource_opts</code>
 - <code>bridges.opents.$name.resource_opts</code>
 - <code>bridges.oracle.$name.resource_opts</code>
 - <code>bridges.pgsql.$name.resource_opts</code>
 - <code>bridges.rocketmq.$name.resource_opts</code>
 - <code>bridges.tdengine.$name.resource_opts</code>
 - <code>bridges.timescale.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__CASSANDRA__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__GCP_PUBSUB__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__GREPTIMEDB__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__HSTREAMDB__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__INFLUXDB_API_V1__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__INFLUXDB_API_V2__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__MATRIX__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__MYSQL__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__OPENTS__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__ORACLE__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__PGSQL__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__ROCKETMQ__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__TDENGINE__$NAME__RESOURCE_OPTS</code>
 - <code>EMQX_BRIDGES__TIMESCALE__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- batch_size: <code>pos_integer()</code>
  * default: 
  `1`

  最大批量请求大小。如果设为 1，则无批处理。

- batch_time: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## s3:transport_options
Options for the HTTP transport layer used by the S3 client


**Config paths**

 - <code>file_transfer.storage.local.exporter.s3.transport_options</code>


**Env overrides**

 - <code>EMQX_FILE_TRANSFER__STORAGE__LOCAL__EXPORTER__S3__TRANSPORT_OPTIONS</code>



**Fields**

- ipv6_probe: <code>boolean()</code>
  * default: 
  `false`

  Whether to probe for IPv6 support.

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  连接到 HTTP 服务器的超时时间。

- pool_type: <code>random | hash</code>
  * default: 
  `random`

  连接池类型。可以是random、hash之一。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  连接池大小。

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  一个正整数。是否连续发送 HTTP 请求，当设置为1时，意味着在发送每个 HTTP 请求后，需要等待服务器返回，然后继续发送下一个请求。

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。

- headers: <code>map()</code>

  HTTP 请求头列表。

- max_retries: <code>non_neg_integer()</code>

  如果发送请求时出错，最大重试次数。

- request_timeout: <code>emqx_schema:timeout_duration_ms()</code>

  HTTP 请求超时时间。


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
  `500ms`

  慢订阅统计的阈值

- expire_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `300s`

  慢订阅记录的有效时间

- top_k_num: <code>pos_integer()</code>
  * default: 
  `10`

  慢订阅统计表的记录数量上限

- stats_type: <code>whole | internal | response</code>
  * default: 
  `whole`

  慢订阅的统计类型


## sso:ldap
LDAP


**Config paths**

 - <code>dashboard.sso.ldap</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__SSO__LDAP</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  是否启用该后端

- backend: <code>ldap</code>

  ----

- query_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  LDAP 查询超时。

- server: <code>string()</code>

  要连接的 IPv4 或 IPv6 地址或主机名。<br/>
  主机名条目的格式为：`主机[:端口]`。<br/>
  如果 `[:端口]` 未指定， 将使用 LDAP 默认端口 389。

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  桥接远端服务时使用的连接池大小。

- username: <code>binary()</code>

  内部数据库的用户名。

- password: <code>emqx_schema_secret:secret()</code>

  内部数据库密码。

- base_dn: <code>binary()</code>

  与基本对象条目（或根）相关的名称。
  搜索用户的起点。

- filter: <code>binary()</code>
  * default: 
  `"(& (objectClass=person) (uid=${username}))"`

  LDAP 中匹配用户的过滤器默认为 `(&(objectClass=person)(uid=${username}))`。 对于 Active Directory，默认过滤器是 `(&(objectClass=user)(sAMAccountName=${username}))`。更多详细内容，请参考 [LDAP Filters](https://ldap.com/ldap-filters/)。

- request_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `10s`

  设置每个单独请求所使用的最大时间（以毫秒为单位）。

- ssl: <code>[ldap:ssl](#ldap-ssl)</code>
  * default: 
  `{enable = false}`

  启用 SSL 连接。


## syskeeper:config
Syskeeper 动作的配置


**Config paths**

 - <code>actions.syskeeper_forwarder.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__SYSKEEPER_FORWARDER__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用或停用该动作

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- connector: <code>binary()</code>

  由动作指定的连接器名称，用于选择外部资源。

- parameters: <code>[syskeeper:parameters](#syskeeper-parameters)</code>

  Syskeeper 动作的参数

- local_topic: <code>binary()</code>

  MQTT 主题或主题过滤器作为数据源（动作输入）。如果规则动作用作数据源，则此配置应保留为空，否则消息将在 Syskeeper 中重复。

- resource_opts: <code>[syskeeper:creation_opts](#syskeeper-creation_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## syskeeper:creation_opts
资源启动相关的选项。


**Config paths**

 - <code>actions.syskeeper_forwarder.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_ACTIONS__SYSKEEPER_FORWARDER__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `infinity`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。

- batch_size: <code>pos_integer()</code>
  * default: 
  `1`

  最大批量请求大小。如果设为 1，则无批处理。

- batch_time: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  每个缓存 worker 允许使用的最大字节数。


## syskeeper:parameters
Syskeeper 动作的参数


**Config paths**

 - <code>actions.syskeeper_forwarder.$name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__SYSKEEPER_FORWARDER__$NAME__PARAMETERS</code>



**Fields**

- target_topic: <code>binary()</code>
  * default: 
  `"${topic}"`

  被转发消息的主题

- target_qos: <code>0..2</code>

  被转发消息的服务质量 (QoS)，-1 表示与原始主题相同

- template: <code>binary()</code>
  * default: 
  `"${payload}"`

  模版


## syskeeper_forwarder:config
Syskeeper 转发连接器的配置


**Config paths**

 - <code>connectors.syskeeper_forwarder.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__SYSKEEPER_FORWARDER__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  启用 (是) 或 停用 (否) 该连接器。

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  描述性文本。

- server: <code>string()</code>

  Syskeeper 代理服务器的地址

- ack_mode: <code>need_ack | no_ack</code>
  * default: 
  `no_ack`

  指定代理服务器是否应该回复消息转发的确认，可以是：<br>- need_ack <br>- no_ack <br>

- ack_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `10s`

  等待代理服务器确认的最大时间

- pool_size: <code>pos_integer()</code>
  * default: 
  `16`

  桥接远端服务时使用的连接池大小。

- resource_opts: <code>[syskeeper_forwarder:connector_resource_opts](#syskeeper_forwarder-connector_resource_opts)</code>
  * default: 
  `{}`

  资源相关的选项。


## syskeeper_forwarder:connector_resource_opts
资源相关的选项。


**Config paths**

 - <code>connectors.syskeeper_forwarder.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__SYSKEEPER_FORWARDER__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  健康检查间隔。

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  是否在创建资源后立即启动资源。

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


