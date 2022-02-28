# OpenTSDB 消息存储

::: tip

EMQX 3.1 版本后推出强大的规则引擎用于替换插件，建议您前往使用[保存数据到 OpenTSDB](../rule/backend_opentsdb.md)规则引擎中创建 保存数据到 OpenTSDB

:::

## 配置 OpenTSDB 消息存储

配置文件：etc/plugins/emqx_backend_opentsdb.conf:

```bash
## OpenTSDB 服务地址
backend.opentsdb.pool1.server = 127.0.0.1:4242

## OpenTSDB 连接池大小
backend.opentsdb.pool1.pool_size = 8

## 是否返回 summary info
##
## Value: true | false
backend.opentsdb.pool1.summary = true

## 是否返回 detailed info
##
## Value: true | false
backend.opentsdb.pool1.details = false

## 是否同步写入
##
## Value: true | false
backend.opentsdb.pool1.sync = false

## 同步写入超时时间，单位毫秒
##
## Value: Duration
##
## Default: 0
backend.opentsdb.pool1.sync_timeout = 0

## 最大批量写条数
##
## Value: Number >= 0
## Default: 20
backend.opentsdb.pool1.max_batch_size = 20

## 存储 PUBLISH 消息
backend.opentsdb.hook.message.publish.1 = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

*OpenTSDB Backend* 消息存储规则参数:

| Option | Description                                      |
| ------ | ------------------------------------------------ |
| topic  | 配置哪些主题下的消息需要执行 hook                              |
| action | 配置 hook 具体动作, function 为 Backend 提供的内置函数, 实现通用功能 |
| pool   | Pool Name, 实现连接多个 OpenTSDB Server 功能             |

示例:

```bash
## 存储主题为 "sensor/#" 的 PUBLISH 消息
backend.influxdb.hook.message.publish.1 = {"topic": "sensor/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}

## 存储主题为 "stat/#" 的 PUBLISH 消息
backend.influxdb.hook.message.publish.2 = {"topic": "stat/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

*OpenTSDB Backend* 支持 Hook 与 相应内置函数列表:

| Hook            | Function list        |
| --------------- | -------------------- |
| message.publish | on_message_publish |

由于 MQTT Message 无法直接写入 OpenTSDB, OpenTSDB Backend 提供了
emqx_backend_opentsdb.tmpl 模板文件将 MQTT Message 转换为可写入 OpenTSDB 的
DataPoint。

模板文件采用 Json 格式, 组成部分:

  - `key` - MQTT Topic, 字符串, 支持通配符主题
  - `value` - Template, Json 对象, 用于将 MQTT Message 转换成 OpenTSDB 的
    DataPoint。

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
  "measurement": <Measurement>,
  "tags": {
      <Tag Key>: <Tag Value>
  },
  "value": <Value>,
  "timestamp": <Timestamp>
}
```

`measurement` 与 `value` 为必选项, `tags` 与 `timestamp` 为可选项。

所有的值 (例如 `<Measurement>`) 你都可以直接在 Template 中配置为一个固定值,
它支持的数据类型依赖于你定义的数据表。当然更符合实际情况的是，你可以通过我们提供的占位符来获取
MQTT 消息中的数据。

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

值得注意的是, 当你使用 `$0` 时，我们希望你取得的数据个数都是相等的。因为我们需要将这些数组转换为多条记录写入 OpenTSDB,
而当你一个字段取得了 3 份数据, 另一个字段却取得了 2 份数据, 我们将无从判断应当怎样为你组合这些数据。

**Example**

data/templates 目录下提供了一个示例模板 (emqx_backend_opentsdb_example.tmpl,
正式使用时请去掉文件名中的 "_example" 后缀) 供用户参考:

```json
{
  "sample": {
      "measurement": "$topic",
      "tags": {
          "host": ["$payload", "data", "$0", "host"],
          "region": ["$payload", "data", "$0", "region"],
          "qos": "$qos",
          "clientid": "$clientid"
      },
      "value": ["$payload", "data", "$0", "temp"],
      "timestamp": "$timestamp"
  }
}
```

当 Topic 为 "sample" 的 MQTT Message 拥有以下 Payload 时:

```json
{
  "data": [
      {
          "temp": 1,
          "host": "serverA",
          "region": "hangzhou"
      },
      {
          "temp": 2,
          "host": "serverB",
          "region": "ningbo"
      }
  ]
}
```

Backend 将 MQTT Message 转换为以下数据写入 OpenTSDB:

```json
[
  {
      "measurement": "sample",
      "tags": {
          "clientid": "mqttjs_ebcc36079a",
          "host": "serverA",
          "qos": "0",
          "region": "hangzhou",
      },
      "value": "1",
      "timestamp": "1560743513626681000"
  },
  {
      "measurement": "sample",
      "tags": {
          "clientid": "mqttjs_ebcc36079a",
          "host": "serverB",
          "qos": "0",
          "region": "ningbo",
      },
      "value": "2",
      "timestamp": "1560743513626681000"
  }
]
```

启用 OpenTSDB 消息存储:

```bash
./bin/emqx_ctl plugins load emqx_backend_opentsdb
```
