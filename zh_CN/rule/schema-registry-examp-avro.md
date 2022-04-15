# 编解码举例 - Avro

## 规则需求

设备发布一个使用 Avro 编码的二进制消息，需要通过规则引擎匹配过后，将消息重新发布到与 "name" 字段相关的主题上。主题的格式为 "avro_user/${name}"。

比如，将 "name" 字段为 "Shawn" 的消息重新发布到主题 "avro_user/Shawn"。

## 创建 Schema

在 EMQX 的 [Dashboard](http://127.0.0.1:18083/#/schemas/0?oper=create) 界面，使用下面的参数创建一个 Avro Schema:

1. 名称：avro_user

2. 编解码类型：avro

3. Schema:

```protobuf
{
"type":"record",
"fields":[
    {"name":"name", "type":"string"},
    {"name":"favorite_number", "type":["int", "null"]},
    {"name":"favorite_color", "type":["string", "null"]}
]
}
```

## 创建规则

**使用刚才创建好的 Schema 来编写规则 SQL 语句：**

```sql
SELECT
  schema_decode('avro_user', payload) as avro_user, payload
FROM
  "t/#"
WHERE
  avro_user.name = 'Shawn'
```

这里的关键点在于 `schema_decode('avro_user', payload)`:

- `schema_decode` 函数将 payload 字段的内容按照 'avro_user' 这个 Schema 来做解码;
- `as avro_user` 将解码后的值保存到变量 "avro_user" 里。

**然后使用以下参数添加动作：**

- 动作类型：消息重新发布
- 目的主题：avro_user/${avro_user.name}
- 消息内容模板：${avro_user}

这个动作将解码之后的 "user" 以 JSON 的格式发送到 `avro_user/${avro_user.name}` 这个主题。其中`${avro_user.name}` 是个变量占位符，将在运行时被替换为消息内容中 "name" 字段的值。

## 设备端代码

规则创建好之后，就可以模拟数据进行测试了。

下面的代码使用 Python 语言填充了一个 User 消息并编码为二进制数据，然后将其发送到 "t/1" 主题。详见 [完整代码](https://github.com/terry-xiaoyu/schema-registry-examples/blob/master/avro/avro_mqtt.py)。

```python
def publish_msg(client):
    datum_w = avro.io.DatumWriter(SCHEMA)
    buf = io.BytesIO()
    encoder = avro.io.BinaryEncoder(buf)
    datum_w.write({"name": "Shawn", "favorite_number": 666, "favorite_color": "red"}, encoder)
    message = buf.getvalue()
    topic = "t/1"
    print("publish to topic: t/1, payload:", message)
    client.publish(topic, payload=message, qos=0, retain=False)
```

## 检查规则执行结果

1) 在 Dashboard 的 [Websocket](http://127.0.0.1:18083/#/websocket) 工具里，登录一个 MQTT Client 并订阅 "avro_user/#"。

2) 安装 python 依赖，并执行设备端代码:

```shell
$ pip3 install protobuf
$ pip3 install paho-mqtt

$ python3 avro_mqtt.py
Connected with result code 0
publish to topic: t/1, payload: b'\nShawn\x00\xb4\n\x00\x06red'
```

3) 检查 Websocket 端收到主题为 `avro_user/Shawn` 的消息:

```
{"favorite_color":"red","favorite_number":666,"name":"Shawn"}
```
