# Apache Kafka

<!-- 提供一段简介，描述支数据桥接的基本工作方式、关键特性和价值，如果有局限性也应当在此处说明（如必须说明的版本限制、当前未解决的问题）。 -->

Apache Kafka is a widely-used open-source distributed event streaming platform. EMQX's integration with Apache Kafka/Confluent presents our users with reliable bi-directional data transport and processing capability under high-throughput scenarios.

Being a top IOT data infrastructure provider,  EMQX currently supports authenticating with Apache Kafka/Confluent via SASL/SCRAM, SASL/GSSAPI, or TLS.

## Prerequisites

<!-- 根据情况编写，包含必须的前置知识点、软件版本要求、需要预先创建/初始化的操作。 -->
- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)
- Relevant Kafka topics should be created before creating the data bridge in Producer or Consumer mode. 

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

### Create a data bridge to Kafka

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select Kafka, and then click **Next**.

4. Input a name for the data bridge. Note: It should be a combination of upper/lower case letters and numbers.

5. Input the connection information. Input **127.0.0.1:9092** for the **Bootstrap Hosts**. For the other fields set as the actual condition. 

6. Configure the data bridge in **Producer** mode. 
   1. **Topic**: The MQTT topics to create the data bridge for. Here we will input `t/#`, indicating all MQTT messages matching this topic will be sent to Kafka. You can also leave it blank, and create a rule to specify data to be sent to Kafka. 
   
   1. **Kafka Topic Name**: Input the Kafka topics we created before, that is, the  `testtopic-in`. Note: Variables are not supported here.
   2. **Kafka Message Template**: Template will specify the rule or MQTT topics with messages to be sent to the Kafka topic. You can keep the default setting or use  variables to create the template. 
   
7. Advanced settings (optional): Set the **Max Batch Bytes**, **Compression**, and **Partition Strategy** as your business needs. For details, see [Configuration parameters](#Configuration).

::: tip
Currently EMQX only supports creating data bridges to Kafka in **Producer** mode. 
:::

### Test

Use MQTTX to send messages to topic  `t/1`:

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

Check the running status of the two data bridges, there should be one new incoming and one new outgoing message. 

Check whether messages are written into the topic `testtopic-in`  with the following Kafka command:

```bash
bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in --from-beginning
```

