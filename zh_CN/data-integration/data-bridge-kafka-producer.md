# Apache Kafka 生产者

<!-- 提供一段简介，描述支数据桥接的基本工作方式、关键特性和价值，如果有局限性也应当在此处说明（如必须说明的版本限制、当前未解决的问题）。 -->
Apache Kafka 数据桥接实现了 EMQX 客户端消息和事件与 Apache Kafka (包括 Confluent) 的桥接，能够提供 EMQX 与企业应用之间高性能、高可靠的数据集成，有效降低应用复杂度并提升扩展性。

同时，EMQX Apache Kafka 集成提供了极高的数据吞吐能力，支持 Apache Kafka 的 SASL/SCRAM、SASL/GSSAPI 等多种安全认证方式以及 TLS 连接，是物联网数据集成首选方案之一。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

:::tip 前置准备

<!-- 根据情况编写，包含必须的前置知识点、软件版本要求、需要预先创建/初始化的操作。 -->
- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。
- 需要预先在 Kafka 创建好对应的 Topic。

<!-- 列举功能或性能方面的亮点，如支持批处理、支持异步模式、双向数据桥接，链接到对应的功能介绍章节。 -->

:::

## 功能清单

- [连接池](./data-bridges.md#连接池)
- [异步请求模式](./data-bridges.md#异步请求模式)
- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)

<!-- TODO 配置参数 需补充链接到配置手册对应配置章节。 -->

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

更多详细内容请参考 [Kafka Quick Start](https://kafka.apache.org/documentation/#quickstart)。

### 创建 Kafka 主题

在 Kafka 中创建名为 `testtopic-in` 与 `testtopic-out` 的两个主题：

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

:::tip
创建数据桥接前必须先在 Kafka 中创建好所需主题。
:::

### 通过 Dashboard 配置 Kafka 生产者桥接

EMQX 支持双向桥接，即可以将数据发送到 Kafka ，也可以从 Kafka 消费下发。以下步骤将指导你如何以生产者角色创建 Kafka 桥接。如需以消费者角色创建 Kafka 桥接，参见 [Apache Kafka 消费者](./data-bridge-kafka-consumer.md)。

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。
2. 点击页面右上角的**创建**。
3. 在**数据桥接类型**中选择 **Kafka**，点击**下一步**。
6. 在**桥接角色** 中选择**生产者**。配置**生产者**桥接信息：
   - 填写必填信息（标星号选项）。
   - 输入数据桥接名称，要求是大小写英文字母或数字组合。
   - 输入 Kafka 连接信息，**主机列表**填写 **127.0.0.1:9092**，其他参数根据实际情况填写。
   - **源 MQTT 主题**：要桥接的 MQTT 主题，此处填写 `t/#` 表示将匹配此主题的 MQTT 消息转发至 Kafka。您可以选择将此项留空，通过新建规则指定发往 Kafka 的数据。
   - **Kafka 主题名称**：填写 Kafka 中预先创建好的主题 `testtopic-in`，此处暂不支持使用变量。
     模版用于将规则或指定 MQTT 主题的消息转发到我们之前创建的 Kafka 主题。此处您可以使用默认配置，或通过变量构造消息模板。
   - **Kafka 消息模版**：模版规定了桥接规则或者将被发送到 kafka 主题的 MQTT 主题和消息。你可以保留默认设置或者使用变量创建模版。
7. 高级配置（可选）：根据情况配置**最大批量字节数**、**压缩**、**分区选择策略**等参数，详细请参考[配置参数](#配置参数)。

::: tip
目前 Apache Kafka 集成仅支持单向桥接，即只可以将数据数据发送到 Kafka，无法从 Kafka 消费下发。
:::

### 测试

使用 MQTTX 向 `t/1` 主题发布消息：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

查看数据桥接运行统计，命中、发送成功次数应当 +1。

通过 Kafka 命令查看 `testtopic-in` 主题是否写入消息：

```bash
bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in --from-beginning
```
