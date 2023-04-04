# 集成 Timescale

::: tip

EMQX 3.1 版本后推出强大的规则引擎用于替换插件，建议您前往使用[保存数据到 Timescale](../rule/backend_timescaledb.md)规则引擎中创建 保存数据到 Timescale

:::

## 配置 Timescale 消息存储

etc/plugins/emqx_backend_timescale.conf:

```bash
## Timescale Server
backend.timescale.pool1.server = 127.0.0.1:5432
## Timescale Pool Size
backend.timescale.pool1.pool_size = 8
## Timescale Username
backend.timescale.pool1.username = postgres
## Timescale Password
backend.timescale.pool1.password = password
## Timescale Database
backend.timescale.pool1.database = tutorial
## Timescale SSL
backend.timescale.pool1.ssl = false

## SSL keyfile.
##
## Value: File
## backend.timescale.pool1.keyfile =

## SSL certfile.
##
## Value: File
## backend.timescale.pool1.certfile =

## SSL cacertfile.
##
## Value: File
## backend.timescale.pool1.cacertfile =

## Store Publish Message
backend.timescale.hook.message.publish.1 = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

*Timescale Backend* 消息存储规则参数:

| Option | Description                                      |
| ------ | ------------------------------------------------ |
| topic  | 配置哪些主题下的消息需要执行 hook                              |
| action | 配置 hook 具体动作, function 为 Backend 提供的内置函数, 实现通用功能 |
| pool   | Pool Name, 实现连接多个 Timescale Server 功能            |

Example:

```bash
## Store Publish message with "sensor/#" topic
backend.timescale.hook.message.publish.1 = {"topic": "sensor/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}

## Store Publish message with "stat/#" topic
backend.timescale.hook.message.publish.2 = {"topic": "stat/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

*Timescale Backend* 支持 Hook 与 相应内置函数列表:

| Hook            | Function list        |
| --------------- | -------------------- |
| message.publish | on_message_publish |

Timescale Backend 提供 emqx_backend_timescale.tmpl 模板文件，用于从不同主题的 MQTT
Message 中提取数据以写入 Timescale。

模板文件采用 Json 格式, 组成部分:

  - `key` - MQTT Topic, 字符串, 支持通配符主题
  - `value` - Template, Json 对象, 用于将 MQTT Message 转换成
    `measurement,tag_key=tag_value,... field_key=field_value,...
    timestamp` 的形式以写入 InfluxDB。

你可以为不同 Topic 定义不同的 Template, 也可以为同一个 Topic 定义多个 Template, 类似:

```bash
{
  <Topic 1>: <Template 1>,
  <Topic 2>: <Template 2>
}
```

Template 格式如下:

```bash
{
  "name": <Name of template>,
  "sql": <SQL INSERT INTO>,
  "param_keys": <Param Keys>
}
```

`name`, `sql` 和 `param_keys` 都是必选项。

`name` 可以是任意的字符串，确保没有重复即可。

`sql` 为 Timescale 可用的 SQL INSERT INTO 语句，例如：`insert into
sensor_data(time, location, temperature, humidity) values (NOW(), $1,
$2, $3)`。

`param_keys` 是一个数组，它的第一个元素对应 `sql` 中出现的 `$1`，并以此类推。

数组中任意元素都可以是一个固定值, 它支持的数据类型依赖于你定义的数据表。当然更符合实际情况的是，你可以通过我们提供的占位符来获取 MQTT
消息中的数据。

目前我们支持的占位符如下:

| Placeholder | Description                            |
| ----------- | -------------------------------------- |
| $id         | MQTT 消息 UUID, 由 EMQX 分配               |
| $clientid   | 客户端使用的 Client ID                       |
| $username   | 客户端使用的 Username                        |
| $peerhost   | 客户端 IP                                 |
| $qos        | MQTT 消息的 QoS                           |
| $topic      | MQTT 消息主题                              |
| $payload    | MQTT 消息载荷, 必须为合法的 Json                 |
| $<Number\> | 必须配合 $paylaod 使用, 用于从 Json Array 中获取数据 |
| $timestamp  | EMQX 准备转发消息时设置的时间戳, 精度: 毫秒            |

**$payload 与 $<Number\>:**

你可以直接使用 `$payload` 取得完整的消息载荷, 也可以通过 `["$payload", <Key>, ...]`
取得消息载荷内部的数据。

例如 `payload` 为 `{"data": {"temperature": 23.9}}`, 你可以通过占位符 `["$payload",
"data", "temperature"]` 来获取其中的 `23.9`。

考虑到 Json 还有数组这一数据类型的情况, 我们引入了 `$0` 与 `$<pos_integer>`, `$0` 表示获取数组内所有元素,
`$<pos_integer>` 表示获取数组内第 `<pos_integer>` 个元素。

一个简单例子, `["$payload", "$0", "temp"]` 将从 `[{"temp": 20}, {"temp": 21}]`
中取得 `[20, 21]`, 而 `["$payload", "$1", "temp"]` 将只取得 `20`。

值得注意的是, 当你使用 `$0` 时，我们希望你取得的数据个数都是相等的。因为我们需要将这些数组转换为多条记录写入 Timescale,
而当你一个字段取得了 3 份数据, 另一个字段却取得了 2 份数据, 我们将无从判断应当怎样为你组合这些数据。

**Example**

data/templates 目录下提供了一个示例模板 (emqx_backend_timescale_example.tmpl,
正式使用时请去掉文件名中的 "_example" 后缀) 供用户参考:

```json
{
  "sensor_data": {
      "name": "insert_sensor_data",
      "sql": "insert into sensor_data(time, location, temperature, humidity) values (NOW(), $1, $2, $3)",
      "param_keys": [
          ["$payload", "data", "$0", "location"],
          ["$payload", "data", "$0", "temperature"],
          ["$payload", "data", "$0", "humidity"]
      ]
  },
  "sensor_data2/#": {
      "name": "insert_sensor_data2",
      "sql": "insert into sensor_data(time, location, temperature, humidity) values (NOW(), $1, $2, $3)",
      "param_keys": [
          ["$payload", "location"],
          ["$payload", "temperature"],
          ["$payload", "humidity"]
      ]
  },
  "easy_data": {
      "name": "insert_easy_data",
      "sql": "insert into easy_data(time, data) values (NOW(), $1)",
      "param_keys": [
          "$payload"
      ]
  }
}
```

当 Topic 为 "sensor_data" 的 MQTT Message 拥有以下 Payload 时:

```json
{
  "data":[
      {
          "location":"bedroom",
          "temperature":21.3,
          "humidity":40.3
      },
      {
          "location":"bathroom",
          "temperature":22.3,
          "humidity":61.8
      },
      {
          "location":"kitchen",
          "temperature":29.5,
          "humidity":58.7
      }
  ]
}
```

\["$payload", "data", "$0", "location"\] 会先获取 MQTT Message 的 Payload，如果
Payload 为 json 格式，则继续尝试读取 data。data 的值是数组，这里我们用到了 "$0" 表示获取数组中所有的元素，因此
\["$payload", "data", "$0", "location"\] 将帮我们获得 \["bedroom", "bathroom",
"kitchen"\]。相应的，如果将 "$0" 替换为 "$1"，将只获得 \["bedroom"\]。相应的，如果将

那么在这个场景中，我们将得到以下 SQL
语句:

```sql
insert into sensor_data(time, location, temperature, humidity) values (NOW(), 'bedroom', 21.3, 40.3)
insert into sensor_data(time, location, temperature, humidity) values (NOW(), 'bathroom', 22.3, 61.8)
insert into sensor_data(time, location, temperature, humidity) values (NOW(), 'kitchen', 29.5, 58.7)
```

最终 Timescale Backend 执行这些 SQL 语句将数据写入 Timescale。
