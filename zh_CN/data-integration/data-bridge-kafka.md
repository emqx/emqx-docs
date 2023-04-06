# Apache Kafka

<!-- 提供一段简介，描述支数据桥接的基本工作方式、关键特性和价值，如果有局限性也应当在此处说明（如必须说明的版本限制、当前未解决的问题）。 -->
Apache Kafka 数据桥接实现了 EMQX 客户端消息和事件与 Apache Kafka (包括 Confluent) 的桥接，能够提供 EMQX 与企业应用之间高性能、高可靠的数据集成，有效降低应用复杂度并提升扩展性。

同时，EMQX 与 Apache Kafka 的集成提供了极高的数据吞吐能力，支持 Apache Kafka 的 SASL/SCRAM、SASL/GSSAPI 等多种安全认证方式以及 TLS 连接，是物联网数据集成首选方案之一。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

:::tip 前置准备

<!-- 根据情况编写，包含必须的前置知识点、软件版本要求、需要预先创建/初始化的操作。 -->
- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

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
在 EMQX 创建数据桥接前，必须首先在 Kafka 中创建好所需主题。
:::

### 通过 Dashboard 配置 Kafka 桥接

Kafka 桥接有两种桥接角色: 生产者（将数据发送到 Kafka）和消费者（将数据从 Kafka 消费下发）。以下步骤将指导你如何分别以这两种角色创建 Kafka 桥接。

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。

2. 点击页面右上角的**创建**。

3. 在**数据桥接类型**中选择 **Kafka**，点击**下一步**。

   <img src="./assets/bridge-create-next.png" alt="bridge-create-next" style="zoom:67%;" />

6. 在**桥接角色** 中选择**生产者**或**消费者**。
   
   :::: tabs type:card
   
   ::: tab 配置生产者桥接
   
   - 填写必填信息（标星号字段）。
   - 输入数据桥接名称，要求是大小写英文字母和数字的组合。
   - 输入 Kafka 连接信息，**主机列表**填写 **127.0.0.1:9092**，其他参数根据实际情况填写。
   - **源 MQTT 主题**：选择要为其建立桥接的 MQTT 主题，此处填写 `t/#` 表示将匹配此主题的 MQTT 消息转发至 Kafka。您也可以选择将此项留空，通过[新建规则]指定发往 Kafka 的数据。
   - **Kafka 主题名称**：填写 Kafka 中预先创建好的主题 `testtopic-in`，此处暂不支持使用变量。
     模版用于将规则或指定 MQTT 主题的消息转发到我们之前创建的 Kafka 主题。此处您可以使用默认配置，或通过变量构造消息模板。
   - **消息键**：Kafka 消息键，此处填写字符串或者包含占位符（ ${var}）的字符串。
   - **消息值**：Kafka 消息值，此处填写字符串或者包含占位符（ ${var}）的字符串。
   - 高级配置（可选）：根据情况配置**最大批量字节数**、**压缩**、**分区选择策略**等参数。
   
   :::
   
   ::: tab 配置消费者桥接
   
   - 填写必填信息（标星号字段）。
   - 输入数据桥接名称，要求是大小写英文字母或数字组合。
   - 输入 Kafka 连接信息，**主机列表**填写 **127.0.0.1:9092**，其他参数根据实际情况填写。
   - **主题映射关系**：必须包含至少一个 Kafka 主题和 MQTT 主题之间的映射。**MQTT Payload Template** 子字段指定应使用的 MQTT 载荷，并提供以下 Kafka 消息字段以进行模板化：
   
     | 字段名称  | 描述                                               |
     | --------- | -------------------------------------------------- |
     | `headers` | 包含字符串键-值对的对象                            |
     | `key`     | Kafka 消息键（由选择的键编码）                     |
     | `offset`  | Kafka 主题分区中消息的偏移量                       |
     | `topic`   | Kafka 源主题                                       |
     | `ts`      | 消息时间戳                                         |
     | `ts_type` | 消息时间戳类型,  `create`, `append` or `undefined` |
     | `value`   | Kafka 消息值 (由选择的值编码)                      |
   
   **MQTT Payload Template** 的默认值为`${.}`，其中包括编码为 JSON 对象的所有可用数据。例如，选择`${.}`作为模板将会产生以下 Kafka 消息内容：
   
     ```json
     {
      "value": "value",
      "ts_type": "create",
      "ts": 1679665968238,
      "topic": "my-kafka-topic",
      "offset": 2,
      "key": "key",
      "headers": {"header_key": "header_value"}
     }
     ```
   
   可以使用点符号选择 Kafka 消息的子字段。例如：`${.value}` 将解析为 Kafka 消息的值，`${.headers.h1}` 将解析为 Kafka `h1` 标题的值（如果存在）。缺失的值将被替换为空字符串。
   
   **注意**：每一对 Kafka 和 MQTT 的主题映射关系只能有一个唯一的 Kafka 主题名称，即 Kafka 主题名称不能出现在一对以上的映射关系中。
   
     :::
   
     ::::
   
5. 点击**创建**，将提示是否使用该数据桥接创建规则。这可以通过规则进一步处理 Kafka 消息，然后再发送到 MQTT 客户端。有关创建规则的更多信息，请参阅[规则引擎](./rules.md)。

   :::tip 提示

   创建关联的规则不是必需的。没有规则的情况下消息也可以发布到 **主题映射** 中设置的 MQTT 主题。

   :::

### 通过配置文件配置 Kafka 桥接

想要通过配置文件配置 Kafka 生产者桥接，在 `emqx.conf` 文件的最后加入下列配置

```js
bridges.kafka.kproducer {
  authentication {
    mechanism = "plain"
    password = "******"
    username = "emqxuser"
  }
  bootstrap_hosts = "kafka-1.emqx.net:9093"
  connect_timeout = "5s"
  enable = false
  kafka {
    buffer {
      memory_overload_protection = true
      mode = "hybrid"
      per_partition_limit = "2GB"
      segment_bytes = "100MB"
    }
    compression = "no_compression"
    max_batch_bytes = "896KB"
    max_inflight = 10
    message {
      key = "${.clientid}"
      timestamp = "${.timestamp}"
      value = "${.}"
    }
    partition_count_refresh_interval = "60s"
    partition_strategy = "random"
    required_acks = "all_isr"
    topic = "test-topic-two-partitions"
  }
  metadata_request_timeout = "5s"
  min_metadata_refresh_interval = "3s"
  socket_opts {
    nodelay = true
    recbuf = "1024KB"
    sndbuf = "1024KB"
  }
  ssl {
    ciphers = []
    depth = 10
    enable = false
    hibernate_after = "5s"
    reuse_sessions = true
    secure_renegotiate = true
    user_lookup_fun = "emqx_tls_psk:lookup"
    verify = "verify_peer"
    versions = ["tlsv1.3", "tlsv1.2", "tlsv1.1", "tlsv1"]
  }
}
```

想要通过配置文件配置 Kafka 消费者桥接，在 `emqx.conf` 文件的最后加入下列配置。

```js
bridges.kafka_consumer.my_consumer {
  enable = true
  bootstrap_hosts = "kafka-1.emqx.net:9092"
  connect_timeout = 5s
  min_metadata_refresh_interval = 3s
  metadata_request_timeout = 5s
  authentication = {
    mechanism = plain
    username = emqxuser
    password = password
  }
  kafka {
    max_batch_bytes = 896KB
    max_rejoin_attempts = 5
    offset_commit_interval_seconds = 3
    offset_reset_policy = reset_to_latest
  }
  topic_mapping = [
    {
      kafka_topic = "kafka-topic-1"
      mqtt_topic = "mqtt/topic/1"
      qos = 1
      payload_template = "${.}"
    },
    {
      kafka_topic = "kafka-topic-2"
      mqtt_topic = "mqtt/topic/2"
      qos = 2
      payload_template = "v = ${.value}"
    }
  ]
  key_encoding_mode = none
  value_encoding_mode = none
  ssl {
    enable = false
    verify = verify_none
    server_name_indication = "auto"
  }
}
```

### 测试桥接

使用 MQTTX 向 `t/1` 主题发布消息：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

查看数据桥接运行统计，命中、发送成功次数应当 +1。

通过 Kafka 命令查看 `testtopic-in` 主题是否写入消息：

```bash
bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in --from-beginning
```
