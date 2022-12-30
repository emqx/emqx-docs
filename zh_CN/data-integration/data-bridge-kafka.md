# Apache Kafka

<!-- 提供一段简介，描述支数据桥接的基本工作方式、关键特性和价值，如果有局限性也应当在此处说明（如必须说明的版本限制、当前未解决的问题）。 -->
Apache Kafka 数据桥接实现了 EMQX 客户端消息和事件与 Apache Kafka(包括 Confluent) 的桥接，能够提供 EMQX 与企业应用之间高性能、高可靠的数据集成，有效降低应用复杂度并提升扩展性。

同时，EMQX Apache Kafka 集成提供了极高的数据吞吐能力，支持 Apache Kafka 的 SASL/SCRAM、SASL/GSSAPI 等多种安全认证方式以及 TLS 连接，是物联网数据集成首选方案之一。

## 先决条件

<!-- 根据情况编写，包含必须的前置知识点、软件版本要求、需要预先创建/初始化的操作。 -->
- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。
- 需要预先在 Kafka 创建好对应的 Topic。

<!-- 列举功能或性能方面的亮点，如支持批处理、支持异步模式、双向数据桥接，链接到对应的功能介绍章节。 -->
## 支持特性

- [连接池](./data-bridges.md#连接池)
- [异步请求模式](./data-bridges.md#异步请求模式)
- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)

## 配置参数
<!-- TODO 链接到配置手册对应配置章节。 -->

## 快速开始
<!-- 从安装测试所需步骤，如果有不同的用法增加章节介绍。 -->

### 安装 Kafka

以 macOS 为例，安装并启动 Apache Kafka：

```bash
wget https://archive.apache.org/dist/kafka/3.3.1/kafka_2.13-3.3.1.tgz

tar -xzf  kafka_2.13-3.3.1.tgz

cd kafka_2.13-3.3.1

# 以 KRaft 启动 Kafka（可选）
KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"

bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties

bin/kafka-server-start.sh config/kraft/server.properties
```

更多详细内容请参考 [Kafka Quick Start](https://kafka.apache.org/documentation/#quickstart)

### 创建 Kafka 主题

在 Kafka 中创建名为 `testtopic-in` 与 `testtopic-out` 的两个主题：

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

:::tip
创建数据桥接前必须先在 Kafka 中创建好所需主题。
:::

### 连接到 Apache Kafka

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。
2. 点击页面右上角的创建。
3. 在数据桥接类型中选择 Kafka，点击下一步。
4. 输入数据桥接名称，要求是大小写英文字母或数字组合。
5. 输入 Kafka 连接信息，主机列表填写 **127.0.0.1:9092**，其他参数根据实际情况填写。
6. 配置生产者桥接信息：
   1. MQTT 主题：要桥接的 MQTT 主题，此处填写 `t/#` 表示将匹配此主题的 MQTT 消息转发至 Kafka。您可以选择将此项留空，通过新建规则指定发往 Kafka 的数据。
   2. Kafka 主题名称：填写 Kafka 中预先创建好的主题 `testtopic-in`，此处暂不支持使用变量。
模版用于将规则或指定 MQTT 主题的消息转发到我们之前创建的 Kafka 主题。此处您可以使用默认配置，或通过变量构造消息模板。
7. 高级配置（可选）：根据情况配置最大批量字节数、压缩、分区选择策略等参数，详细请参考[配置参数](#配置参数)。

::: tip
目前 Apache Kafka 集成仅支持单向桥接，即只可以将数据数据发送到 Kafka，无法从 Kafka 消费下发。
:::

### 测试

使用 MQTTX 向 `t/1` 主题发布消息：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

查看数据桥接运行统计，命中、发送成功次数应当 +1。

通过 Kafka 命令 `testtopic-in` 主题是否写入消息：

```bash
bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in --from-beginning
```
