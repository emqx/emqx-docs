# Stream MQTT Data into Apache Kafka

{% emqxce %}
:::tip
The Kafka data integration is an EMQX Enterprise Edition feature. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

[Apache Kafka](https://kafka.apache.org/) is a widely used open-source distributed event streaming platform that can handle the real-time transfer of data streams between applications and systems. In the IoT realm, data generated from devices and applications are transmitted using the lightweight MQTT protocol. EMQX’s integration with Kafka/[Confluent](https://www.confluent.io/) enables users to stream MQTT data seamlessly into or from Kafka. MQTT data streams are ingested into Kafka topics, ensuring real-time processing, storage, and analytics. Conversely, Kafka topics data can be consumed by MQTT devices, enabling timely actions. 

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

This page provides a comprehensive introduction to the data integration between EMQX and Kafka with practical using instructions. Here is a list of topics covered on this page:

- [How It Works](#how-it-works)
- [Features and Benefits](#features-and-benefits)
- [Before You Start](#before-you-start)
  - [Prerequisites](#prerequisites)
  - [Set Up a Kafka Server](#set-up-a-kafka-server)
  - [Create Kafka Topics](#create-kafka-topics)
- [Kafka Producer Data Bridge](#kafka-producer-data-bridge)
  - [Create Rule and Data Bridge for Kafka Producer](#create-rule-and-data-bridge-for-kafka-producer)
  - [Test Kafka Producer Data Bridge and Rule](#test-kafka-producer-data-bridge-and-rule)
- [Kafka Consumer Data Bridge](#kafka-consumer-data-bridge)
  - [Create Data Bridge for Kafka Consumer](#create-data-bridge-for-kafka-consumer)
  - [Create Rule for Consumer Data Bridge (Optional)](#create-rule-for-consumer-data-bridge-optional)
  - [Test Kafka Consumer Data Bridge and Rule](#test-kafka-consumer-data-bridge-and-rule)
- [Advanced Configurations](#advanced-configurations)
- [More Information](#more-information)

## How It Works

Apache Kafka data integration is an out-of-the box feature in EMQX designed to bridge the gap between MQTT-based IoT data and Kafka's powerful data processing capabilities. With a built-in [rule engine](https://docs.emqx.com/en/enterprise/v5.1/data-integration/rules.html) component, the integration simplifies the process of streaming and processing data between the two platforms, eliminating the need for complex coding. 

The diagram below illustrates a typical architecture of data integration between EMQX and Kafka used in automotive IoT.

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

Streaming data into or from Apache Kafka involves creating data bridges to Kafka in two roles: producer (sends messages to Kafka) and consumer (receives messages from Kafka). EMQX enables you to create data bridges in either of the roles. If you configure the data bridge in a producer role, it works as follows:

1. **Message publication from IoT devices:** The connected vehicles periodically publish messages containing status data using the MQTT protocol. These messages include information such as speed and direction.
2. **Message reception by EMQX:** As an MQTT Broker, EMQX receives these MQTT messages from the connected vehicles. It serves as the centralized hub for handling MQTT-based communication.
3. **Message data processing:** With an embedded rule engine working together with the broker as a single component, these MQTT messages can be processed based on topic matching rules. When a message arrives, it passes through the rule engine, which evaluates the defined rules for that message. If any rules specify payload transformations, those transformations are applied, such as converting data formats, filtering out specific information, or enriching the payload with additional context.
4. **Bridging to Kafka:** The rule defined in the rule engine triggers an action of forwarding the messages to Kafka. Using the Kafka bridging functionality, MQTT topics are mapped to pre-defined Kafka topics, and all processed messages and data are written into Kafka topics. 

After the vehicle data are ingested into Kafka, you can flexibly access and utilize the data:

- Your services can directly integrate with Kafka clients to consume real-time data streams from specific topics, enabling customized business processing.
- Utilize Kafka Streams for stream processing, and perform real-time monitoring by aggregating and correlating vehicle statuses in memory.
- By using Kafka Connect components, you can select various connectors to output data to external systems such as MySQL, ElasticSearch, for storage.

## Features and Benefits

The data integration with Apache Kafka brings the following features and benefits to your business:

- **Dependable and bi-directional IoT data messaging:**  The data communication between Kafka and resource-limited IoT devices running on unpredictable mobile networks can be processed under the MQTT protocol that excels in messaging in uncertain networks. EMQX not only batch forwards MQTT messages to Kafka but also subscribes to Kafka messages from backend systems and delivers them to connected IoT clients.
- **Payload transformation**: Message payload can be processed by the defined SQL rules during the transmission. For example, payloads containing some real-time metrics such as total message count, successful/failed delivery count, and message rate can go through data extraction, filtering, enrichment, and transformation before the messages are ingested into Kafka.
- **Effective topic mapping:** Numerous IoT business topics can be mapped into Kakfa topics by the configured kafka data bridge. EMQX supports the MQTT user property mapping to Kafka headers and adopts various flexible topic mapping methods, including one-to-one, one-to-many, many-to-many, and also includes support for MQTT topic filters (wildcards).
- **Flexible partition selection strategy**: Supports forwarding messages to the same Kafka partition based on MQTT topics or clients.
- **Processing capabilities in high-throughput situations:** EMQX Kafka producer supports synchronous/asynchronous writing modes, enabling flexible balancing between latency and throughput according to different scenarios.

These features enhance the integration capabilities and flexibility that help you establish an effective and robust IoT platform architecture. Your increasing volumes of IoT data can be transmitted under stable network connections and can be further stored and managed effectively. 

## Before You Start

This section describes the preparations you need to complete before you start to create the Kafka data bridges in EMQX Dashboard.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

### Set Up a Kafka Server

This section takes macOS as an example to illustrate the process. You can install and run Kafka with the commands below:

```bash
wget https://archive.apache.org/dist/kafka/3.3.1/kafka_2.13-3.3.1.tgz

tar -xzf  kafka_2.13-3.3.1.tgz

cd kafka_2.13-3.3.1

# Use KRaft start Kafka
KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"

bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties

bin/kafka-server-start.sh config/kraft/server.properties
```

For detailed operation steps, you may refer to the [Quick Start section in Kafka Documentation](https://kafka.apache.org/documentation/#quickstart).

### Create Kafka Topics

Relevant Kafka topics should be created before creating the data bridge in EMQX. Use the commands below to create two topics in Kafka:  `testtopic-in` (for the producer role) and `testtopic-out` (for the consumer role). 

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafka Producer Data Bridge

This section provides instructions on how to create a rule and data bridge to forward MQTT data from EMQX to Kafka and how to test the rule and producer data bridge.

### Create Rule and Data Bridge for Kafka Producer

This section demonstrates how to create a rule in EMQX to process messages from the source MQTT topic `t/#`  and send the processed results through the configured Kafka data bridge to produce data into the Kafka `testtopic-in` topic. 

1. Go to EMQX Dashboard, and click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Enter a rule ID, for example, `my_rule`.

4. Enter the following statement in the **SQL Editor** if you want to forward the MQTT messages from the topic `t/#` to Kafka.

   Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the data bridge in the `SELECT` part.

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   Note: If you are a beginner user, you can click **SQL Examples** and **Enable Test** to learn and test the SQL rule. 

5. Click the + **Add Action** button to define an action that will be triggered by the rule. Select **Forwarding with Data Bridge** from the dropdown list. With this action, EMQX sends the data processed by the rule to Kafa.

6. Click the **+** icon next to the **Data bridge** drop-down box to create a data bridge.

7. Select **Kafka** from the **Type of Data Bridge** drop-down list. Fill in the required fields (marked with an asterisk).

8. In the **Bridge Role** field, select **Producer**. 

9. Enter a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

10. Enter the connection information for the data bridge. 
    - Enter `127.0.0.1:9092` for the **Bootstrap Hosts**. Note: The demonstration assumes that you run both EMQX and Kafka on the local machine. If you have Kafka and EMQX running remotely, please adjust the settings accordingly.
    - Leave other options as default or configure them according to your business needs.
    - If you want to establish an encrypted connection, click the **Enable TLS** toggle switch. For more information about TLS connection, see [TLS for External Resource Access](../network/overview.md/#tls-for-external-resource-access).

11. Configure the data bridge options.
    - **Source MQTT Topic:** You can leave this field blank, implying that Kafka only processes data from the rules. In this example, indicating all MQTT messages matching the topic `t/#` will be sent to Kafka. If you wish to establish a Kafka Data Bridge without utilizing rules, you can directly input an MQTT topic as the data source. This field supports wildcards.
    - **Kafka Topic Name**: Enter `testtopic-in`. Note: Variables are not supported here.
    - **Kafka Headers**: Enter the metadata or contextual information related to a Kafka message (optional). The value of the placeholder must be an object. You can select the value encode mode for the header from the **Kafka Header Value Encode Mode** drop-down list. You can also add more than one header by clicking **Add** to add more key-value pairs.
    - **Message Key**: Kafka message key. Ensert a string here, either a plain string or a string containing placeholders (${var}).
    - **Message Value**: Kafka message value. Enter a string here, either a plain string or a string containing placeholders (${var}).
    - **Message Timestamp**: Specify the string format for this data field in a Kafka message.
    - **Compression**: Specify whether or not to use compression algorithms to compress/decompress the records in a Kafka message. 
    - **Partition Strategy**: Select the way for the producer to dispatch messages to Kafka partitions.

12. Advanced settings (optional): See [Advanced Configurations](#advanced-configurations).
13. Click the **Add** button to complete the data bridge configuration. You will be redirected back to the **Add Action** page. Select the Kafka Data Bridge you just created from the **Data bridge** drop-down list. Click the **Add** button at the bottom to include this action in the rule.
14. Back on the **Create Rule** page, verify the configured information. Click the **Create** button to generate the rule.

![Kafka_producer_bridge](./assets/Kafka_producer_bridge.png)

Now you have successfully created the rule and data bridge. You can click **Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#` are sent and saved to Kafka after parsing by rule `my_rule`.

### Test Kafka Producer Data Bridge and Rule

To test if the Kafka producer data bridge and rule work as you expected, you can use the [MQTTX](https://mqttx.app/) to simulate a client to publish MQTT messages to EMQX.

1. Use MQTTX to send messages to topic  `t/1`:

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. Click the name of the data bridge on the **Data Bridge** page to view the statistics. Check the running status of the data bridge, there should be one new incoming and one new outgoing message.
3. Check whether messages are written into the topic `testtopic-in` with the following Kafka command:

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

## Kafka Consumer Data Bridge

This section provides instructions on how to create a data bridge to receive data from Kafka and optionally create a rule to further process the data and re how to test the rule and producer data bridge.

### Create Data Bridge for Kafka Consumer

This section demonstrates how to create a data bridge to receive data from Kafka. Through the data bridge, messages of the Kafka topic `testtopic-out` will be forwarded to a MQTT topic `t/1` in EMQX.

1. Go to EMQX Dashboard, and click **Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **Kafka**, and then click **Next**.

4. In **Bridge Role** field, select **Consumer**.

5. Enter a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

6. Enter the connection information for the data bridge. 
   - Enter `127.0.0.1:9092` for the **Bootstrap Hosts**. Note: The demonstration assumes that you run both EMQX and Kafka on the local machine. If you have Kafka and EMQX running remotely, please adjust the settings accordingly.
   - Leave other options as default or configure according to your business needs.
   - If you want to establish an encrypted connection, click the **Enable TLS** toggle switch. For more information about TLS connection, see **TLS for External Reesource Access**.

7. **Key Encoding Mode** and **Value Encoding Mode**: Select the encoding mode for Kafka message key and message value.

8. Click **Add** to add at least one Kafka-to-MQTT topic mapping in the **Topic Mapping** field. For example, enter `testtopic-out` in **Kafka Topic** and `t/1` in **MQTT Topic** for this demonstration. The **MQTT Payload Template** subfield specifies the MQTT payload that should be used, and has the following Kafka message fields available for templating:

   | **Field Name** | **Description**                                              |
   | :------------- | :----------------------------------------------------------- |
   | `headers`      | An object containing string key-value pairs                  |
   | `key`          | Kafka message key (uses the same encoding method as the selected key) |
   | `offset`       | Offset for the message in Kafka's topic partition            |
   | `topic`        | Original Kafka topic                                         |
   | `ts`           | Message timestamp                                            |
   | `ts_type`      | Message timestamp type, which is one of `create`, `append` or `undefined` |
   | `value`        | Kafka message value (uses the same encoding method as the selected key) |

   The default value for **MQTT Payload Template** is `${.}`, which includes all available data encoded as a JSON object. For example, choosing `${.}` as a template will produce the following for a Kafka message:

   ```json
   {
       "value": "value",
       "ts_type": "create",
       "ts": 1679665968238,
       "topic": "testtopic-out",
       "offset": 2,
       "key": "key",
       "headers": {"header_key": "header_value"}
   }
   ```

   Subfields from the Kafka message may be accessed with dot notation. For example, `${.value}` will resolve to the Kafka message value, and `${.headers.h1}` will resolve to the value of the `h1` Kafka header if such a subfield exists. Absent values will be replaced by empty strings.

   **Note**: Each Kafka-to-MQTT topic mapping must contain a unique Kafka topic name. That is, the Kafka topic must not be present in more than one mapping.

9. **Offset Reset Policy**: Select the policy for resetting the offset where Kafaka consumers start to read from a Kafka topic partition when there is no consumer’s offset or the offset becomes invalid.
   
   - Select **lastest** if you want the consumer to start reading messages from the latest offset, skipping messages that were produced before the consumer started.
   - Select **earliest** if you want the consumer to start reading messages from the beginning of the partition, including messages that were produced before the consumer started, that is, to read all the historical data in a topic.
   
10. Advanced settings (optional): See **Advanced Configurations.**

11. Before clicking **Create**, you can click **Test Connection** to test that the bridge can connect to the Kafka server.

12. Click **Create**. You will be offered the option of creating an associated rule. For the Kafka consumer data bridge, it is not strictly necessary to create a rule for further data processing. If you need to create a rule for the data bridge, see [Create Rule and Data Bridge for Kafka Consumer (Optional)](#create-rule-for-consumer-data-bridge-optional).

<img src="./assets/Kafka_consumer_bridge.png" alt="Kafka_consumer_bridge" style="zoom:67%;" />

### Create Rule for Consumer Data Bridge (Optional)

This section demonstrates how to create a rule in EMQX to further process the message forwarded by configured Kafka data bridge and republish the message to MQTT topic.

1. Go to EMQX Dashboard, and click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Enter a rule ID, for example, `my_rule`.

4. Enter the following statement in the **SQL Editor** if you want to forward the messages transformed from the Kafka bridge `$bridges/kafka_consumer:<bridgeName>` to EMQX.

   Note: If you want to specify your own SQL syntax, make sure that the `SELECT` part includes all fields required by the republishing action set in later steps.

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<bridgeName>"
   ```

   Note: If you are a beginner user, you can click **SQL Examples** and **Enable Test** to learn and test the SQL rule. 

5. Click the + **Add Action** button to define an action that will be triggered by the rule. Select **Republish** from the drop-down list.
6. In **Topic** and **Payload** fields, you can enter the topic and payload for the messages you want to republish. For example, enter `t/1` and `${.}` for this demonstration.
7. Click **Add** to include the action to the rule.
8. Back on the **Create Rule** page, click **Create**.

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

### Test Kafka Consumer Data Bridge and Rule

To test if the Kafka consumer data bridge and rule work as expected, you can use [MQTTX](https://mqttx.app/) to simulate a client that subscribes to a topic in EMQX and use the Kafaka producer to produce data to a Kafka topic. Then, check if the data from Kafka is republished by EMQX to the topic subscribed by the client.

1. Use MQTTX to subscribe to topic `t/1`: 

   ```bash
   mqttx sub -t t/1 -v
   ```

2. Open a new command line window and start the Kafka producer using the command below: 

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   You will be prompted to input a message.

3. Enter `{"msg": "Hello EMQX"}` to produce a message to the `testtopic-out` topic using the producer and press enter. 

4. Check the subscription in MQTTX. The following message from Kafka should be received under the topic `t/1`: 

   ```json
   {
       "value": "{\"msg\": \"Hello EMQX\"}",
       "ts_type": "create",
       "ts": 1679665968238,
       "topic": "testtopic-out",
       "offset": 2,
       "key": "key",
       "headers": {
           "header_key": "header_value"
       }
   }
   ```

## Advanced Configurations

This section describes some advanced configuration options that can optimize the performance of your data bridge and customize the operation based on your specific scenarios. When creating the data bridge, you can unfold the **Advanced Settings** and configure the following settings according to your business needs.

| Fields                                      | Descriptions                                                 | Recommended Values |
| ------------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Min Metadata Refresh Interval               | The minimum time interval the client must wait before refreshing Kafka broker and topic metadata. Setting this value too small may increase the load on the Kafka server unnecessarily. | `3`                |
| Metadata Request Timeout                    | The maximum duration to wait when the bridge requests metadata from Kafka. | `5`                |
| Connect Timeout                             | The maximum time to wait for TCP connection establishment, which includes the authentication time if enabled. | `5`                |
| Fetch Bytes (Consumer)                      | The byte size to pull from Kafka with each fetch request. Note that if the configured value is smaller than the message size in Kafka, it may negatively impact fetch performance. | `896`              |
| Max Batch Bytes (Producer)                  | The maximum size, in bytes, for collecting messages within a Kafka batch. Typically, Kafka brokers have a default batch size limit of 1 MB. However, EMQX's default value is intentionally set slightly lower than 1 MB to account for Kafka message encoding overheads, particularly when individual messages are very small. If a single message exceeds this limit, it will still be sent as a separate batch. | `896`              |
| Offset Commit Interval (Consumer)           | The time interval between two offset commit requests sent for each consumer group. | `5`                |
| Required Acks (Producer)                    | Required acknowledgments for the Kafka partition leader to await from its followers before sending an acknowledgment back to the EMQX Kafka producer: <br />`all_isr`: Requires acknowledgment from all in-sync replicas.<br />`leader_only`: Requires acknowledgment only from the partition leader.<br />`none`: No acknowledgment from Kafka is needed. | `all_isr`          |
| Partition Count Refresh Interval (Producer) | The time interval at which the Kafka producer detects an increased number of partitions. Once Kafka's partition count is augmented, EMQX will incorporate these newly discovered partitions into its message dispatching process, based on the specified `partition_strategy`. | `60`               |
| Max Inflight (Producer)                     | The maximum number of batches allowed for Kafka producer (per-partition) to send before receiving acknowledgment from Kafka. Greater value typically means better throughput. However, there can be a risk of message reordering when this value is greater than 1.<br />This option controls the number of unacknowledged messages in transit, effectively balancing the load to prevent overburdening the system. | `10`               |
| Query Mode (Producer)                       | Allows you to choose asynchronous or synchronous query modes to optimize message transmission based on different requirements. In asynchronous mode, writing to Kafka does not block the MQTT message publish process. However, this might result in clients receiving messages ahead of their arrival in Kafka. | `Async`            |
| Synchronous Query Timeout (Producer)        | In synchronous query mode, establishes a maximum wait time for confirmation. This ensures timely message transmission completion to avoid prolonged waits.<br />It applies only when the bridge query mode is configured to `Sync`. | `5`                |
| Buffer Mode (Producer)                      | Defines whether messages are stored in a buffer before being sent. Memory buffering can increase transmission speeds.<br />`memory`: Messages are buffered in memory. They will be lost in the event of an EMQX node restart.<br />`disk`: Messages are buffered on disk, ensuring they can survive an EMQX node restart.<br />`hybrid`: Messages are initially buffered in memory. When they reach a certain limit (refer to the `segment_bytes` configuration for more details), they are gradually offloaded to disk. Similar to the memory mode, messages will be lost if the EMQX node restarts. | `memory`           |
| Per-partition Buffer Limit (Producer)       | Maximum allowed buffer size, in bytes, for each Kafka partition. When this limit is reached, older messages will be discarded to make room for new ones by reclaiming buffer space. <br />This option helps to balance memory usage and performance. | `2`                |
| Segment File Bytes (Producer)               | This setting is applicable when the buffer mode is configured as `disk` or `hybrid`. It controls the size of segmented files used to store messages, influencing the optimization level of disk storage. | `100`              |
| Memory Overload Protection (Producer)       | This setting applies when the buffer mode is configured as `memory`. EMQX will automatically discard older buffered messages when it encounters high memory pressure. It helps prevent system instability due to excessive memory usage, ensuring system reliability. <br />**Note**: The threshold for high memory usage is defined in the configuration parameter `sysmon.os.sysmem_high_watermark`. This configuration is effective only on Linux systems. | Disabled           |
| Socket Send / Receive Buffer Size           | Manages the size of socket buffers to optimize network transmission performance. | `1024`             |
| TCP Keepalive                               | This configuration enables TCP keepalive mechanism for Kafka bridge connections to maintain ongoing connection validity, preventing connection disruptions caused by extended periods of inactivity. The value should be provided as a comma-separated list of three numbers in the format `Idle, Interval, Probes`:<br />Idle: This represents the number of seconds a connection must remain idle before the server initiates keep-alive probes. The default value on Linux is 7200 seconds.<br />Interval: The interval specifies the number of seconds between each TCP keep-alive probe. On Linux, the default is 75 seconds.<br />Probes: This parameter defines the maximum number of TCP keep-alive probes to send before considering the connection as closed if there's no response from the other end. The default on Linux is 9 probes.<br />For example, if you set the value to '240,30,5,' it means that TCP keepalive probes will be sent after 240 seconds of idle time, with subsequent probes sent every 30 seconds. If there are no responses for 5 consecutive probe attempts, the connection will be marked as closed. | `none`             |
| Health Check Interval                       | The time interval for checking the running status of the data bridge. | `15`               |

## More Information

EMQX provides bunches of learning resources on the data integration with Apache Kafka. Check out the following links to learn more:

**Blogs:**

- [MQTT with Kafka: Supercharging IoT Data Integration](https://www.emqx.com/en/blog/mqtt-and-kafka) 
- [Bridging MQTT Data to Kafka | EMQX Rule Engine Series](https://www.emqx.com/en/blog/emqx-rule-engine-series-bridge-data-to-message-queue-kafka) 
- [MQTT Performance Benchmark Testing: EMQX-Kafka Integration](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-kafka-integration) 
- [EMQX Enterprise + Apache Kafka Build a high-performance IoT message processing backend](https://www.emqx.com/en/blog/emqx-enterprise-mqtt-broker-apache-kafka-build-high-performance-iot-message-processing-backend) 

**Videos:**

- [Bridge device data to Kafka using the EMQX Cloud Rule Engine](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine) (This video is about Cloud rule engine; will be replaced with more suitable videos in the future)
