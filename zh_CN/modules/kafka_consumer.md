# Kafka 消费组

Kafka 消费组使用外部 Kafka 作为消息队列，可以从 Kafka 中消费消息转换成为 MQTT 消息发布在 emqx 中。

搭建 Kafka 环境，以 MacOS X 为例:

```bash
wget https://archive.apache.org/dist/kafka/2.8.0/kafka_2.13-2.8.0.tgz

tar -xzf  kafka_2.13-2.8.0.tgz

cd kafka_2.13-2.8.0

# 启动 Zookeeper
./bin/zookeeper-server-start.sh config/zookeeper.properties
# 启动 Kafka
./bin/kafka-server-start.sh config/server.properties
```

::: tip

Kafka 消费组不支持 Kafka0.9 以下版本

创建资源之前，需要提前创建 Kafka 主题，不然会提示错误

:::

创建 Kafka 的主题:

```bash
$ ./bin/kafka-topics.sh --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic testTopic --create
```

## 创建模块

打开 [EMQX Dashboard](http://127.0.0.1:18083/#/modules)，点击左侧的 “模块” 选项卡，选择添加：

![image-20200927213049265](./assets/modules.png)

选择 Kafka 消费组模块:

![img](./assets/kafka_consumer1.png)

填写相关参数:

![img](./assets/kafka_consumer3.png)

- Kafka 服务器地址
- Kafka consumer 连接池大小
- Kafka 的订阅主题
- MQTT 的消息主题
- MQTT 的主题服务质量
- MQTT PayLoad，可选使用 Kafka message.value 或者 message 全部信息
- 二进制 key 编码模式，UTF-8 或 base64，消息中 key 的编码方式，如果 key 值为非字符串或可能产生字符集编码异常的值，推荐使用 base64 模式
- 二进制 value 编码模式，UTF-8 或 base64，消息中 value 的编码方式，如果 value 值为非字符串或可能产生字符集编码异常的值，推荐使用 base64 模式
- Kafka Max Bytes (每次从 Kafka 里消费消息的最大字节数)
- Kafka Offset Reset Policy (重置Offset策略,reset_to_latest | reset_by_subdcriber)
- Kafka consumer 是否重连
- SSL 连接参数

点击添加后，模块添加完成:

![img](./assets/kafka_consumer4.png)

资源已经创建完成，现在用Dashboard的websocket工具订阅MQTT的主题 "TestTopic":

![img](./assets/kafka_consumer5.png)

使用kafka 命令行 生产一条消息:

```bash
./bin/kafka-console-producer.sh --broker-list localhost:9092 --topic TestTopic
```

![img](./assets/kafka_consumer6.png)

Dashboard的websocket工具接收到了Kafka 生产的消息"hello-kafka":

![img](./assets/kafka_consumer7.png)
