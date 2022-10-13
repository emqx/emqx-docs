# InfluxDB 消息存储

::: tip

EMQX 3.1 版本后推出强大的规则引擎用于替换插件，建议您前往使用[保存数据到 InfluxDB](../rule/backend_influxdb.md)规则引擎中创建 保存数据到 InfluxDB

:::

## InfluxDB 配置

EMQX 支持通过 UDP 协议连接 InfluxDB，需要修改 InfluxDB 配置文件：

```bash
[[udp]]
  enabled = true
  bind-address = ":8089"
  # 消息保存的数据库
  database = "emqx"

  # InfluxDB precision for timestamps on received points ("" or "n", "u", "ms", "s", "m", "h")
  # EMQX 默认时间戳是毫秒
  precision = "ms"

  # 其他配置根据需要自行修改
  #   batch-size = 1000
  #   batch-pending = 5
  #   batch-timeout = "5s"
  #   read-buffer = 1024
```

## 配置 InfluxDB 消息存储

配置文件 etc/plugins/emqx_backend_influxdb.conf:

```bash
## 写数据到 InfluxDB 时使用的协议
backend.influxdb.pool1.common.write_protocol = udp

## 批量写入大小
backend.influxdb.pool1.common.batch_size = 1000

## InfluxDB 写进程池大小
backend.influxdb.pool1.pool_size = 8

## InfluxDB UDP 主机地址
backend.influxdb.pool1.udp.host = 127.0.0.1

## InfluxDB UDP 主机端口
backend.influxdb.pool1.udp.port = 8089

## InfluxDB HTTP/HTTPS 主机地址
backend.influxdb.pool1.http.host = 127.0.0.1

## InfluxDB HTTP/HTTPS 主机端口
backend.influxdb.pool1.http.port = 8086

## InflxuDB 数据库名
backend.influxdb.pool1.http.database = mydb

## 连接到 InfluxDB 的用户名
## backend.influxdb.pool1.http.username = admin

## 连接到 InfluxDB 的密码
## backend.influxdb.pool1.http.password = public

## 时间戳精度
backend.influxdb.pool1.http.precision = ms

## 是否启用 HTTPS
backend.influxdb.pool1.http.https_enabled = false

## 连接 InfluxDB 时使用的 TLS 协议版本
## backend.influxdb.pool1.http.ssl.version = tlsv1.2

## 密钥文件
## backend.influxdb.pool1.http.ssl.keyfile =

## 证书文件
## backend.influxdb.pool1.http.ssl.certfile =

## CA 证书文件
## backend.influxdb.pool1.http.ssl.cacertfile =

## 存储 PUBLISH 消息
backend.influxdb.hook.message.publish.1 = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

*InfluxDB Backend* 消息存储规则参数:

| Option | Description                                      |
| ------ | ------------------------------------------------ |
| topic  | 配置哪些主题下的消息需要执行 hook                              |
| action | 配置 hook 具体动作, function 为 Backend 提供的内置函数, 实现通用功能 |
| pool   | Pool Name, 实现连接多个 InfluxDB Server 功能             |

Example:

```bash
## 存储主题为 "sensor/#" 的 PUBLISH 消息
backend.influxdb.hook.message.publish.1 = {"topic": "sensor/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}

## 存储主题为 "stat/#" 的 PUBLISH 消息
backend.influxdb.hook.message.publish.2 = {"topic": "stat/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

*InfluxDB Backend* 支持 Hook 与 相应内置函数列表:

| Hook            | Function list        |
| --------------- | -------------------- |
| message.publish | on_message_publish |

由于 MQTT Message 无法直接写入 InfluxDB, InfluxDB Backend 提供了
emqx_backend_influxdb.tmpl 模板文件将 MQTT Message 转换为可写入 InfluxDB 的
DataPoint。

模板文件采用 JSON 格式, 组成部分:

  - `key` - MQTT Topic, 字符串, 支持通配符
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
  "measurement": <Measurement>,
  "tags": {
      <Tag Key>: <Tag Value>
  },
  "fields": {
      <Field Key>: <Field Value>
  },
  "timestamp": <Timestamp>
}
```

`measurement` 与 `fields` 为必选项, `tags` 与 `timestamp` 为可选项。

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
| $timestamp  | EMQX 准备转发消息时设置的时间戳, 精度: 纳秒            |

**$payload 与 $<Number\>:**

你可以直接使用 `$payload` 取得完整的消息载荷, 也可以通过 `["$payload", <Key>, ...]`
取得消息载荷内部的数据。

例如 `payload` 为 `{"data": {"temperature": 23.9}}`, 你可以通过占位符 `["$payload",
"data", "temperature"]` 来获取其中的 `23.9`。

![image](./assets/backends_3.png)

考虑到 Json 还有数组这一数据类型的情况, 我们引入了 `$0` 与 `$<pos_integer>`, `$0` 表示获取数组内所有元素,
`$<pos_integer>` 表示获取数组内第 `<pos_integer>` 个元素。

一个简单例子, `["$payload", "$0", "temp"]` 将从 `[{"temp": 20}, {"temp": 21}]`
中取得 `[20, 21]`, 而 `["$payload", "$1", "temp"]` 将只取得 `20`。

![image](./assets/backends_4.png)

![image](./assets/backends_5.png)

值得注意的是, 当你使用 `$0` 时，我们希望你取得的数据个数都是相等的。因为我们需要将这些数组转换为多条记录写入 InfluxDB,
而当你一个字段取得了 3 份数据, 另一个字段却取得了 2 份数据, 我们将无从判断应当怎样为你组合这些数据。

![image](./assets/backends_6.png)

**Example**

data/templates 目录下提供了一个示例模板 (emqx_backend_influxdb_example.tmpl,
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
      "fields": {
          "temperature": ["$payload", "data", "$0", "temp"]
      },
      "timestamp": "$timestamp"
  }
}
```

::: tip
当 Template 中设置 timestamp 或插件配置 `backend.influxdb.pool1.set_timestamp = true` 时，请将 InfluxDB UDP 配置中的 precision 设为 "ms"。
:::

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

Backend 会将 MQTT Message 转换为:

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
      "fields": {
          "temperature": "1"
      },
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
      "fields": {
          "temperature": "2"
      },
      "timestamp": "1560743513626681000"
  }
]
```

最终编码为以下数据写入
InfluxDB:

```bash
"sample,clientid=mqttjs_6990f0e886,host=serverA,qos=0,region=hangzhou temperature=\"1\" 1560745505429670000\nsample,clientid=mqttjs_6990f0e886,host=serverB,qos=0,region=ningbo temperature=\"2\" 1560745505429670000\n"
```

启用 InfluxDB 消息存储:

```bash
./bin/emqx_ctl plugins load emqx_backend_influxdb
```
