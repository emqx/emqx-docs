

# Stream Data into Apache Kafka

<!-- 提供一段简介，描述支数据桥接的基本工作方式、关键特性和价值，如果有局限性也应当在此处说明（如必须说明的版本限制、当前未解决的问题）。 -->

[Apache Kafka](https://kafka.apache.org/) is a widely-used open-source distributed event streaming platform. EMQX's integration with Apache Kafka/Confluent presents our users with reliable bi-directional data transport and processing capability under high-throughput scenarios.

Being a top IoT data infrastructure provider,  EMQX currently supports authenticating with Apache Kafka/Confluent via SASL/SCRAM or SASL/GSSAPI.

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

:::tip Prerequisites

<!-- 根据情况编写，包含必须的前置知识点、软件版本要求、需要预先创建/初始化的操作。 -->
- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

<!-- 列举功能或性能方面的亮点，如支持批处理、支持异步模式、双向数据桥接，链接到对应的功能介绍章节。 -->

:::

## Feature List

- [Connection pool](./data-bridges.md#connection-pool) <!-- TODO 确认改版后知否支持-->
- [Async mode](./data-bridges.md#async-mode)
- [Batch mode](./data-bridges.md#batch-mode)
- [Buffer queue](./data-bridges.md#buffer-queue)

<!--  Configuration parameters TODO 链接到配置手册对应配置章节。 -->

## Quick Start
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

For detailed operation steps, you may refer to the [Quick Start section in Kafka Documentation](https://kafka.apache.org/documentation/#quickstart).

### Create Kafka Topics

Relevant Kafka topics should be created before creating the data bridge in EMQX. Use the command below to create two topics in Kafka:  `testtopic-in` and `testtopic-out`:

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

### Configure Kafka Bridge via Dashboard

The data bridge to Kafka takes two roles: Producer (sends messages to Kafka) and Consumer (receives messages from Kafka). The following steps guide you to create a data bridge in either of the modes.

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **Kafka**, and then click **Next**.

   <img src="./assets/kafka_consumer/setup2.png" alt="setup2" style="zoom:67%;" />

6. In **Bridge Role** field, select **Producer** or **Consumer**. Click the corresponding tabs for configuration of each role. 
   
   :::: tabs type:card
   
   ::: tab Configure as Producer Role
   
   - Fill in the required fields (marked with an asterisk). 
   - Input a name for the data bridge. Note: It should be a combination of upper/lower case letters and numbers.
   - Input the connection information. Input **127.0.0.1:9092** for the **Bootstrap Hosts**. For the other fields set as the actual condition.
   - **Source MQTT Topic**: Set the MQTT topics to create the data bridge. In this example, it is set to `t/#`, indicating all MQTT messages matching this topic will be sent to Kafka. You can also leave it blank, and create a [rule] to specify data to be sent to Kafka.
   - **Kafka Topic Name**: Input the Kafka topics we created before, that is, the  `testtopic-in`. Note: Variables are not supported here.
   - **Message Key**: Kafka message key. Insert a string here, either a plain string or a string containing placeholders (${var}).
   - **Message Value**: Kafka message value. Insert a string here, either a plain string or a string containing placeholders (${var}).
   - Advanced settings (optional): Set the **Max Batch Bytes**, **Compression**, and **Partition Strategy** as your business needs.
   
   :::
   
   ::: tab Configure in Consumer role
   
   - Fill the required fields (marked with an asterisk). 
   - Input a name for the data bridge. Note: It should be a combination of upper/lower case letters or numbers.
   - Input the connection information. Input **127.0.0.1:9092** for the **Bootstrap Hosts**. For the other fields set as the actual condition.
   - The **Topic Mapping** field must contain at least one Kafka-to-MQTT topic mapping. The **MQTT Payload Template** subfield specifies the MQTT payload that should be used, and has the following Kafka message fields available for templating:
   
     | Field Name | Description                                                  |
     | ---------- | ------------------------------------------------------------ |
     | `headers`  | An object containing string key-value pairs                  |
     | `key`      | Kafka message key (encoded by the chosen key encoding)       |
     | `offset`   | Offset for the message in Kafka's topic partition            |
     | `topic`    | Originating Kafka topic                                      |
     | `ts`       | Message timestamp                                            |
     | `ts_type`  | Message timestamp type, which is one of `create`, `append` or `undefined` |
     | `value`    | Kafka message value (encoded by the chosen value encoding)   |
   
     The default value for **MQTT Payload Template** is `${.}`, which includes all available data encoded as a JSON object.  For example, choosing `${.}` as a template will produce the following for a Kafka message:
   
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
   
     Subfields from the Kafka message may be accessed with dot notation. For example: `${.value}` will resolve to the Kafka message value, and `${.headers.h1}` will resolve to the value of the `h1` Kafka header, if present.  Absent values will be replaced by empty strings.
   
     **Note**: Each Kafka-to-MQTT topic mapping must contain a unique Kafka topic name.  That is, the Kafka topic must not be present in more than one mapping.
   
     :::
   
     ::::
   
   5. Click **Create**, you'll be offered the option of creating an associated rule. This will allow Kafka messages matching the rule to be further transformed and filtered if needed, and then forwarded to other rule actions, like different bridges.  Refer to the [Rules](./rules.md) for more info on creating rules.
   
      :::tip Tip
   
      It's not strictly necessary to create an associated rule. The MQTT topics defined in **Topic Mapping** will start having messages published to them without further configuration.
   
      :::
   
### Configure Kafka Bridge via Configration File

Add the following configuration to the end of the `emqx.conf` file if you want to configure Kafka producer bridge using the configuration file.

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

Add the following configuration to the end of the `emqx.conf` file if you wish to configure Kafka consumer bridge using the configuration file.

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

### Test the Bridge

 Use MQTTX to send messages to topic  `t/1`:

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
   ```

Check the running status of the two data bridges, there should be one new incoming and one new outgoing message.

Check whether messages are written into the topic `testtopic-in`  with the following Kafka command:

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in --from-beginning
   ```

   
