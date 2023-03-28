# HOCON

[HOCON (Human-Optimized Config Object Notation)](https://github.com/emqx/hocon) is a human-readable configuration file format based on the JSON format. HOCON supports hierarchical configurations and variable substitution, making it easier to manage complex configurations. HOCON also allows for comments and includes, which further improves the maintainability and modularity of the configuration files.

Therefore, starting from version 5.0, EMQX uses HOCON as the configuration file format.

## HOCON Syntax

HOCON values can be represented as JSON-like objects, for example:

```bash
node {
  name = "emqx@127.0.0.1"
  cookie = "mysecret"
  cluster_call {
    retry_interval  =  1m
  }
}
```

or in flattening:

```bash
node.name = "127.0.0.1"
node.cookie = "mysecret"
node.cluster_call.retry_interval = "1m"
```

::: tip

This cuttlefish-like flattening format is backward compatible with the previous EMQX versions, but  it is used differently:

- HOCON recommends adding quotes at both ends of the string, but strings without special characters can also be unquoted, for example, `foo`, `foo_bar`.
- In the flattening mode, all characters to the right of `=` are treated as values.

:::

## Schema

To further lower the configuration threshold, EMQX introduced a schema for HOCON, which defines data types, data fields' names, and metadata for config value validation. <!--the following table should be reviewed before I start working on the English Version-->

| **Data Type** | **Description**                                              | **Example**                                                  |
| ------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| `atom()`      | To specify an atom value in the configuration, for example,  function names, module names, and configuration keys. | 如您需要将配置项 emqx_schema 的值设为：`on`、`off` 或 `auto`，则可以写为：<br> `emqx_schema = atom(on | off | auto)` |
| `boolean()`   | 表示布尔值，可选值：true 、 false                            | 如需配置是否启用 emqx_schema 配置项，则可以写为：<br> `emqx_schema = boolean()` |
| `string()`    | 表示字符串，                                                 |                                                              |
| `binary()`    | 表示二进制数据，一种特殊的字符串类型。                       | 例如，您需要定义一个二进制类型的配置项 emqx_schema，则可以写为：<br> `emqx_schema = binary()` |
| `integer()`   | 表示整数，常用于计数器、超时值、端口号等。                   | 如您要定义一个整数类型的配置项 emqx_schema，则可以写为：<br>`emqx_schema = integer()` |
| `duration()`  | 一种特殊的整数类型，表示时间间隔，单位为毫秒。               | 如希望设置某配置项的超时时间为 5 s，则可以写为：<br>`timeout = duration(5000)` |
| `float()`     | 表示浮点数，常用于指定各种参数，例如，消息的QoS级别、保持活动时间、消息的最大字节数等。 | 如希望设置某 MQTT 客户端的保活时间，可以写为：<br>`mqtt_keepalive = float(60)` |
| `number()`    | 表示整数或浮点数，用于指定各种参数，例如连接的最大客户端数，最大消息队列长度等。 | 如希望指定连接的最大客户端数为 1,000，可以写为：`max_clients = number(1000)` |
| Struct        | 表示一个具有固定属性的对象类型，也称结构体，是一个有序的键值对集合，用于定义复杂的对象或结构，比如认证机制、ACL 规则等。Struct 由一组键值对组成，每个键值对包含一个键和一个值。其中：键必须是字符串；值可以是任何类型的数据，例如字符串、数字、布尔值、对象、数组等。注：Struct 中的值可以是任何数据类型，包括其他的 Struct 和数组；且 Struct 必须嵌套在上一级的对象中，用 . 分隔符分隔不同的键，如 acl.rule。 | 如希望允许客户端 test_client 订阅和发布任何主题。<br><br> <!--表格不支持代码块吗？--> |
| Map           | 表示一个具有动态属性的无序的键值对的集合，其他内容关于 Struct 数据类型类似。 | 比如以下 ACL 规则中定义了两条访问控制规则，客户端 test_client 可以访问和发布任何主题。<br><br><!--表格不支持代码块吗？--> |
| Union         | 表示一组可能的值，通常用于描述一个参数可以接收多种数据类型，例如字符串或整数，不同数据类型通过 \| 分隔。还可嵌套在其他数据结构中，作为一个对象的值。 | 例如，希望指定配置项 emqx_schema 可以接收字符串和整数两种数据类型：`type emqx_schema = string | int` |
| Array         | 表示一组有序的值，每个值都有唯一的索引。可以定义 HOCON 配置文件中的数组或列表。 | `my_array = [1, 2, 3, 4, 5]`                                 |

## Dot Syntax

EMQX uses the dot syntax, also known as dot notion, to reference specific elements within a structured data type, including `Struct`, `Map`, or `Array`. For example, for `node.name`, setting the value of a property called `name` on an object or variable called `node`.

::: tip

When accessing elements within an `Array` using dotted notation, a 1-based index should be used. This means that the first element of an Array is referred to using "1", rather than the traditional 0-based indexing. 

:::

Below are some examples, 

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

## 覆盖规则

如果 EMQX 配置文件中的多个配置项具有相同的名称，那么后面的配置项会覆盖前面的配置项，基本规则如下：

- 简单值覆盖：如果一个配置项出现多次，后面的值（文件底部）会覆盖前面的值（文件顶部）。
- 当按层级覆盖时，高层级的值覆盖低层级的值。

例如：

在如下配置中，最后一行的 debug 值会覆盖原先 level 字段的 error 值，但是 enable 字段保持不变：

```
log {
  console_handler{
    enable = true
    level = error
  }
}

## 将 console 日志打印级别设置为 debug，其他配置保持不变
log.console_handler.level = debug
```

但在以下示例中，authentication 的设定将被全量覆写，即指保留 enable = true 这一设定。

```
authentication = [
  {
    enable = true
    backend = "built_in_database"
    mechanism="password_based"
  }
]

## 下面这种方式会导致数组第一个元素的除了 `enable` 以外的其他字段全部丢失。
authentication = [{ enable = true }]
```

更多有关 HOCON 的语法请参考 [HOCON 文档](https://github.com/lightbend/config/blob/main/HOCON.md)。

