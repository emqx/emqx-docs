# Kafka 消费组

Kafka 消费组使用外部 Kafka 作为消息队列，可以从 Kafka 中消费消息转换成为 MQTT 消息发布在 emqx 中。

## 启动 Kafka 并创建主题

::: tip

Kafka 消费组不支持 Kafka0.9 以下版本。

创建资源之前，需要提前创建 Kafka 主题，不然会提示错误。

:::

1. 搭建 Kafka 环境，以 MacOS X 为例:

```bash
wget https://archive.apache.org/dist/kafka/2.8.0/kafka_2.13-2.8.0.tgz

tar -xzf  kafka_2.13-2.8.0.tgz

cd kafka_2.13-2.8.0

# 启动 Zookeeper
./bin/zookeeper-server-start.sh config/zookeeper.properties
# 启动 Kafka
./bin/kafka-server-start.sh config/server.properties
```

2. 创建 Kafka 的主题:

```bash
$ ./bin/kafka-topics.sh --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic testTopic --create
```

## 创建模块

1. 打开 [EMQX Dashboard](http://127.0.0.1:18083/#/modules)，点击左侧菜单中的**模块**，选择**添加模块**：

<img src="./assets/modules.png" alt="modules" style="zoom:67%;" />

2. 在**消息下发**选项卡下选择 **Kafka 消费组**模块:

<img src="./assets/kafka_consumer1.png" alt="kafka_consumer1" style="zoom:67%;" />

3. 填写相关参数:

<img src="./assets/kafka_consumer3.png" alt="kafka_consumer3" style="zoom:67%;" />

- **Kafka 服务器**：填写 Kafka 服务器地址；默认为 `127.0.0.1:9092`。
- **Pool Size**：Kafka consumer 连接池大小。
- **Kafka 用户名和密码**：连接 Kafka 服务器的用户名和密码。
- **Kafka 主题转 MQTT 主题映射关系**：
  - **Kafka 主题**：从 Kafka 下发消息的主题；本示例中使用之前创建的 Kafka 主题 `TestTopic`。
  - **MQTT 主题**：接收到的 MQTT 消息的主题；此处支持使用 `${message.key}` 对主题进行动态设置，使 MQTT 主题与 Kafka 主题始终保持一致。
  - **MQTT QoS**：MQTT 消息质量等级。
  - **MQTT PayLoad**：可选使用 Kafka message.value 或者 message 全部信息。

- **Key 编码模式**：二进制 key 编码模式，UTF-8 或 base64，消息中 key 的编码方式，如果 key 值为非字符串或可能产生字符集编码异常的值，推荐使用 base64 模式。
- **Value 编码模式**：二进制 value 编码模式，UTF-8 或 base64，消息中 value 的编码方式，如果 value 值为非字符串或可能产生字符集编码异常的值，推荐使用 base64 模式。
- **提取消息最大字节数**：Kafka Max Bytes (每次从 Kafka 里消费消息的最大字节数)。
- **Offset 重置策略**：Kafka Offset Reset Policy (重置Offset策略,reset_to_latest | reset_by_subdcriber)
- **是否重连**：Kafka consumer 是否重连。
- **开启 SSL**：SSL 连接参数。

点击添加后，模块添加完成:

## 测试消息下发

资源已经创建完成，现在用 Dashboard 的 Websocket 工具订阅 MQTT 主题 "TestTopic":

![img](./assets/kafka_consumer5.png)

使用kafka 命令行生产一条消息:

```bash
./bin/kafka-console-producer.sh --broker-list localhost:9092 --topic TestTopic
```

![img](./assets/kafka_consumer6.png)

Dashboard的websocket工具接收到了Kafka 生产的消息"hello-kafka":

![img](./assets/kafka_consumer7.png)
