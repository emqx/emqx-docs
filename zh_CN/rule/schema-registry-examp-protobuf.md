# 编解码举例 - Protobuf

## 规则需求

设备发布一个使用 Protobuf 编码的二进制消息，需要通过规则引擎匹配过后，将消息重新发布到与 "name" 字段相关的主题上。主题的格式为 "person/${name}"。

比如，将 "name" 字段为 "Shawn" 的消息重新发布到主题 "person/Shawn"。

## 创建 Schema

在 EMQX 的 [Dashboard](http://127.0.0.1:18083/#/schemas/0?oper=create) 界面，使用下面的参数创建一个 Protobuf Schema:

1. 名称：protobuf_person

2. 编解码类型：protobuf

3. Schema：下面的 protobuf schema 定义了一个 Person 消息。

```protobuf
message Person {
  required string name = 1;
  required int32 id = 2;
  optional string email = 3;
}
```

## 创建规则

**使用刚才创建好的 Schema 来编写规则 SQL 语句：**

```sql
SELECT
  schema_decode('protobuf_person', payload, 'Person') as person, payload
FROM
  "t/#"
WHERE
  person.name = 'Shawn'
```

这里的关键点在于 `schema_decode('protobuf_person', payload, 'Person')`:

- `schema_decode` 函数将 payload 字段的内容按照 'protobuf_person' 这个 Schema 来做解码;
- `as person` 将解码后的值保存到变量 "person" 里;
- 最后一个参数 `Person` 指明了 payload 中的消息的类型是 protobuf schema 里定义的 'Person' 类型。

**然后使用以下参数添加动作：**

- 动作类型：消息重新发布
- 目的主题：person/${person.name}
- 消息内容模板：${person}

这个动作将解码之后的 "person" 以 JSON 的格式发送到 `person/${person.name}` 这个主题。其中`${person.name}` 是个变量占位符，将在运行时被替换为消息内容中 "name" 字段的值。

## 设备端代码

规则创建好之后，就可以模拟数据进行测试了。

下面的代码使用 Python 语言填充了一个 Person 消息并编码为二进制数据，然后将其发送到 "t/1" 主题。详见 [完整代码](https://github.com/terry-xiaoyu/schema-registry-examples/blob/master/protobuf/pb2_mqtt.py)。

```python
def publish_msg(client):
    p = person_pb2.Person()
    p.id = 1
    p.name = "Shawn"
    p.email = "liuxy@emqx.io"
    message = p.SerializeToString()
    topic = "t/1"
    print("publish to topic: t/1, payload:", message)
    client.publish(topic, payload=message, qos=0, retain=False)
```

## 检查规则执行结果

1) 在 Dashboard 的 [Websocket](http://127.0.0.1:18083/#/websocket) 工具里，登录一个 MQTT Client 并订阅 "person/#"。

2) 安装 python 依赖，并执行设备端代码:

```shell
$ pip3 install protobuf
$ pip3 install paho-mqtt

$ python3 ./pb2_mqtt.py
Connected with result code 0
publish to topic: t/1, payload: b'\n\x05Shawn\x10\x01\x1a\rliuxy@emqx.io'
t/1 b'\n\x05Shawn\x10\x01\x1a\rliuxy@emqx.io'
```

3) 检查 Websocket 端收到主题为 `person/Shawn` 的消息:

```bash
{"email":"liuxy@emqx.io","id":1,"name":"Shawn"}
```
