# Apache Kafka

<!-- 提供一段简介，描述支数据桥接的基本工作方式、关键特性和价值，如果有局限性也应当在此处说明（如必须说明的版本限制、当前未解决的问题）。 -->

Apache Kafka is a widely-used open-source distributed event streaming platform. EMQX's integration with Apache Kafka/Confluent presents our users with reliable bi-directional data transport and processing capability under high-throughput scenarios.

Being a top IOT data infrastructure provider,  EMQX currently supports authenticating with Apache Kafka/Confluent via SASL/SCRAM, SASL/GSSAPI, or TLS.

## Prerequisites

<!-- 根据情况编写，包含必须的前置知识点、软件版本要求、需要预先创建/初始化的操作。 -->
- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)
- 无论生产者还是消费者模式，均需要预先在 Kafka 创建好对应 Topic。

<!-- 列举功能或性能方面的亮点，如支持批处理、支持异步模式、双向数据桥接，链接到对应的功能介绍章节。 -->

## Features supported

- [Connection pool](./data-bridges.md#连接池) <!-- TODO 确认改版后知否支持-->
- [Async mode](./data-bridges.md#异步请求模式)
- [Batch mode](./data-bridges.md#批量模式)
- [Buffer queue](./data-bridges.md#缓存队列)

## Configuration parameters
<!-- TODO 链接到配置手册对应配置章节。 -->

## Quick starts
<!-- 从安装测试所需步骤，如果有不同的用法增加章节介绍。 -->

### Install Kafka

This section takes macOS as an example to illustrate the process. You can install and run Kafka with the commands below:

```bash
wget https://archive.apache.org/dist/kafka/3.3.1/kafka_2.13-3.3.1.tgz

tar -xzf  kafka_2.13-3.3.1.tgz

cd kafka_2.13-3.3.1

# Use KRaft to run Kafka (optional)
KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"

bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties

bin/kafka-server-start.sh config/kraft/server.properties
```

For detailed operation steps, you may refer to the [*Quick Start section in Kafka Documentation*](https://kafka.apache.org/documentation/#quickstart). 

### Create Kafka topics

Create two topics in Kafka:  `testtopic-in` and `testtopic-out`

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

:::tip
These topics must be created before we create the data bridge to Kafka.
:::

### Create data bridge to Kafka

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.
2. Click **Create** on the top right corner of the page.
3. In the **Create Data Bridge** page, click to select Kafka, and then click **Next**.
4. Input a name for the data bridge. Note: It should be a combination of upper/lower case letters and numbers.
5. Input the connection information.，主机列表填写 **127.0.0.1:9092**，其他参数根据实际情况填写。
6. 配置生产者桥接信息：
   1. MQTT 主题：要桥接的 MQTT 主题，此处填写 `t/#` 表示将匹配此主题的 MQTT 消息转发至 Kafka。您也可以将此项留空，新建规则在规则动作中设置将规则处理结果转发到该数据桥接。
   2. Kafka 主题名称：填写 Kafka 中预先创建好的主题 `testtopic-in`，此处暂不支持使用变量。
   3. Kafka 消息模板：使用变量构造消息模板，将规则或指定 MQTT 主题的消息转发到 6.2 中的 Kafka 主题中，此处使用 Dashboard 默认即可。
7. 调优配置：根据情况配置最大批量字节数、压缩、分区选择策略等参数，详细请参考[配置配置](#配置参数)。

::: tip
目前 Kafka Bridge 仅支持生产者模式。
:::

### Test

Use MQTTX to send messages to topic  `t/1`:

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

Check the running statistic of the data bridge, the 查看数据桥接运行统计，命中、发送成功次数应当 +1。

Check whether messages are written into the topic`testtopic-in`  with the following Kafka command:

```bash
bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in --from-beginning
```