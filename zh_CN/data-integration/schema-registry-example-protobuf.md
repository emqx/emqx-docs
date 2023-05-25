# 编解码举例 - Protobuf

本页通过一个例子来让您了解在 EMQX 中使用 Protobuf 编码的消息数据是如何通过编解码进行格式转换并在规则引擎中进行匹配的。

## 解码场景

设备发布一个使用 Protobuf 编码的二进制消息，需要通过规则引擎匹配过后，将消息重新发布到与 `name` 字段相关的主题上。主题的格式为 `person/${name}`。

比如，将 `name` 字段为 `Shawn` 的消息重新发布到主题 `person/Shawn`。

### 创建 Schema

1. 在 Dashboard 左侧导航栏中选择**数据集成** -> **编解码**。

2. 使用下面的参数创建一个 Protobuf Schema:

   - **名称**：protobuf_person

   - **类型**：protobuf

   - **Schema**：下面的 protobuf schema 定义了一个 Person 消息。

     ```protobuf
     message Person {
       required string name = 1;
       required int32 id = 2;
       optional string email = 3;
     }
     ```

3. 点击**创建**。

<img src="/Users/emqx/Documents/GitHub/emqx-docs/zh_CN/data-integration/assets/protobuf_create1.png" alt="protobuf_create1" style="zoom:67%;" />

### 创建规则

1. 在 Dashboard 左侧导航栏中选择**数据集成** -> **规则**。

2. 在**规则**页面，点击右上角的**创建**。

3. 在 **SQL 编辑器**中，使用刚才创建好的 Schema 编写规则 SQL 语句：

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
   - `as person` 将解码后的值保存到变量 `person` 里;
   - 最后一个参数 `Person` 指明了 payload 中的消息的类型是 protobuf schema 里定义的 'Person' 类型。

4. 点击**添加动作**，在**动作**下拉框中选择`消息重发布`。
5. 在**主题** 文本框中输入 `person/${person.name}` 作为目标主题。
6. 在 **Payload** 中使用消息内容模板 `${person}`。

这个动作将解码之后的 “Person” 消息 以 JSON 的格式发送到 `person/${person.name}` 这个主题。其中`${person.name}` 是个变量占位符，将在运行时被替换为消息内容中 `name` 字段的值。

### 准备设备端代码

规则创建好之后，您可以模拟数据进行测试。

下面的代码使用 Python 语言填充了一个 Person 消息并编码为二进制数据，然后将其发送到 `t/1` 主题。详见[完整代码](https://gist.github.com/thalesmg/3c5fdbae2843d63c2380886e69d6123c)。

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

### 检查规则执行结果

1. 在 Dashboard 页面，点击左侧导航目录中的 **问题分析** -> **WebSocket 客户端**。

2. 填写当前 EMQX 的连接信息。

   - 如果 EMQX 在本地运行，可直接使用默认配置。
   - 如果您修改过 EMQX 的默认配置，如修改过访问规则的配置，则需要输入用户名和密码。

3. 点击**连接**，作为 MQTT 客户端连接到 EMQX。

4. 在**订阅**区域，在**主题** 中输入 `person/#`，点击**订阅**。

5. 安装 python 依赖，并执行设备端代码:

   ```shell
   $ pip3 install protobuf
   $ pip3 install paho-mqtt
   
   $ python3 ./pb2_mqtt.py
   Connected with result code 0
   publish to topic: t/1, payload: b'\n\x05Shawn\x10\x01\x1a\rliuxy@emqx.io'
   t/1 b'\n\x05Shawn\x10\x01\x1a\rliuxy@emqx.io'
   ```

6. 检查 Websocket 端收到主题为 `avro_user/Shawn` 的消息:

   ```json
   {"email":"liuxy@emqx.io","id":1,"name":"Shawn"}
   ```

## 编码场景

设备订阅了主题为 `protobuf_out` 的消息，希望收到使用 Protobuf 编码的二进制消息。规则引擎将对消息进行编码并发送到相关主题。

### 创建 Schema

使用在解码场景创建的 [Schema](#创建-schema)。

### 创建规则

1. 在 Dashboard 左侧导航栏中选择**数据集成** -> **规则**。

2. 在**规则**页面，点击右上角的**创建**。

3. 在 **SQL 编辑器**中，使用刚才创建好的 Schema 编写规则 SQL 语句：

   ```sql
   SELECT
     schema_encode('protobuf_person', json_decode(payload), 'Person') as protobuf_person
   FROM
     "protobuf_in"
   ```

   这里的关键点在于 `schema_encode('protobuf_person', json_decode(payload), 'Person')`:

   - `schema_encode` 函数将 payload 字段的内容按照 `protobuf_person` 这个 Schema 来编码;
   - `as person` 将编码后的值保存到变量 `person` 里;
   - `json_decode(payload)` 用来对 `payload` 进行解码，因为 `schema_encode` 的输入必须是 Map 数据格式，而 `payload` 通常是一个 JSON 编码的二进制消息。
   - 最后一个参数 `Person` 指明了 payload 中的消息的类型是 protobuf schema 里定义的 `Person` 类型。

4. 点击**添加动作**，在**动作**下拉框中选择`消息重发布`。

5. 在**主题**文本框中输入 `protobuf_out` 作为目标主题。

6. 在 **Payload** 中使用消息内容模板 `${protobuf_person}`。

这个动作将 Protobuf 编码的消息发送到 `protobuf_out` 这个主题。其中`${protobuf_person}` 是个变量占位符，将在运行时被替换为经 `schema_encode` 编码后的值 (一个二进制的值)。

### 准备设备端代码

规则创建好之后，您可以模拟数据进行测试。

下面的代码使用 Python 语言填充了一个 Person 消息并编码为二进制数据，然后将其发送到 `protobuf_in` 主题。详 [完整代码](https://gist.github.com/thalesmg/c5f03f99f982401d16ef6583e30144fa)。

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

### 检查规则执行结果

1. 在 Dashboard 页面，点击左侧导航目录中的**问题分析** -> **WebSocket 客户端**。

2. 填写当前 EMQX 的连接信息。

   - 如果 EMQX 在本地运行，可直接使用默认配置。
   - 如果您修改过 EMQX 的默认配置，如修改过访问规则的配置，则需要输入用户名和密码。

3. 点击**连接**，作为 MQTT 客户端连接到 EMQX。

4. 在**发布**区域，在**主题**中输入 `protobuf_out`，在 **Paylod** 中输入以下消息：

   ```json
   {"name":"Shawn","id":1,"email":"shawn@example.com"}
   ```

5. 点击**发布**。

6. 安装 python 依赖，并执行设备端代码:

   ```shell
   $ pip3 install protobuf paho-mqtt
   
   $ python3 protobuf_mqtt_sub.py
   Connected with result code 0
   msg payload b'\n\x05Shawn\x10\x01\x1a\x11shawn@example.com'
   protobuf_out name: "Shawn"
   id: 1
   email: "shawn@example.com"
   ```
