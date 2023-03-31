# Apache Kafka Consumer

Apache Kafka is a widely-used open-source distributed event streaming platform. EMQX's integration with Apache Kafka/Confluent presents ourusers with reliable bi-directional data transport and processing capability under high-throughput scenarios.

Being a top IoT data infrastructure provider, EMQX currently supports authenticating with Apache Kafka/Confluent via SASL/SCRAM or SASL/GSSAPI.

{% emqxce %}
:::tip

EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.

:::
{% endemqxce %}

## Configure Kafka Consumer Bridge via Dashboard

The following steps guide you to create data bridge to Kafka in consumer mode.

:::tip Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)
- Relevant Kafka topics should be created before creating the data bridge.

:::

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

   ![Creating a Kafka Consumer Bridge](./assets/kafka_consumer/setup1.png)

3. In the **Create Data Bridge** page, click to select **Kafka**, and then click **Next**.

   ![Creating a Kafka Consumer Bridge](./assets/kafka_consumer/setup2.png)

4. In the **Bridge Role** field, select **Consumer**.  Configure the data bridge in **Consumer** mode.

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

     The default value for **MQTT Payload Template** is `${.}`, which includes all available data encoded as a JSON object.  For example, choosing `${.}` as a template would produce the following for a Kafka message:
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
     
     :::tip
     
     Each Kafka-to-MQTT topic mapping must contain a unique Kafka topic name.  That is, the Kafka topic must not be present in more than one mapping.
     
     :::
     
     ![Creating a Kafka Consumer Bridge](./assets/kafka_consumer/setup3.png)
     

5. Click **Create**, you'll be offered the option of creating an associated rule. This will allow Kafka messages matching the rule to be further transformed and filtered if needed, and then forwarded to other rule actions, like different bridges.  Refer to the [Rules](./rules.md) for more info on creating rules.

   :::tip Tip

   It's not strictly necessary to create an associated rule. The MQTT topics defined in **Topic Mapping** will start having messages published to them without further configuration.

   :::

## Configure Kafka Consumer Bridge via Configuration File

Add the following configuration to the end of the `emqx.conf` file if you wish to configure this bridge using the configuration file.

```json
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
