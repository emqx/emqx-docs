# Stream MQTT Data into Amazon Kinesis

{% emqxce %}
::: tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

[AWS Kinesis](https://aws.amazon.com/cn/kinesis/) 是 AWS 上完全托管的实时流数据处理服务，可以轻松地进行流数据的收集、处理和分析。它可以经济高效地处理任意规模的实时流数据，并具有高度的灵活性，能够低时延的处理来自数十万个来源的任意数量的流数据。

EMQX supports seamless integration with [Amazon Kinesis Data Streams](https://aws.amazon.com/kinesis/data-streams/) which 能够实现海量 IoT 设备连接，进行实时消息进行采集、传输，并通过 EMQX 数据集成连接到 Amazon Kinesis Data Streams，进行实时分析与复杂的流处理。

This page provides a comprehensive introduction to the data integration between EMQX and Amazon Kinesis with practical instructions on creating and validating the data integration.

## How It Works

Amazon Kinesis data bridge is an out-of-the-box feature of EMQX designed to help users seamlessly integrate MQTT data streams with Amazon Kinesis and leverage its rich services and capabilities for IoT application development.

![kinesis_architecture](./assets/kinesis_architecture.svg)

EMQX forwards MQTT data to Amazon Kinesis through the rule engine and data bridge. The complete process is as follows:

1. **IoT Devices Publish Messages**: Devices publish telemetry and status data through specific topics, triggering the rule engine.
2. **Rule Engine Processes Messages**: Using the built-in rule engine, MQTT messages from specific sources are processed based on topic matching. The rule engine matches corresponding rules and processes messages, such as converting data formats, filtering specific information, or enriching messages with contextual information.
3. **Bridging to Amazon Kinesis**: The rule triggers the action of forwarding messages to Amazon Kinesis, 可以自定义配置分区键，要写入的数据流以及消息格式，实现灵活的数据集成。

After MQTT message data is written to Amazon Kinesis, you can perform flexible application development, such as:

- Real-time Data Processing and Analysis: Utilize powerful Amazon Kinesis data processing and analysis tools and its own streaming capabilities to perform real-time processing and analysis of message data, obtaining valuable insights and decision support.
- Event-Driven Functionality: Trigger Amazon event handling to achieve dynamic and flexible function triggering and processing.
- Data Storage and Sharing: Transmit message data to Amazon Kinesis storage services for secure storage and management of large volumes of data. This allows you to share and analyze this data with other Amazon services to meet various business needs.

## Features and Benefits

EMQX 与 AWS Kinesis Data Streams 的数据集成可以为您的业务带来以下功能和优势：

- **可靠的数据传输和顺序保证**：EMQX 和 AWS Kinesis Data Streams 都提供了可靠的数据传输机制，EMQX 通过 MQTT 协议确保消息的可靠传输，而 AWS Kinesis Data Streams 使用分区和顺序号来保证消息的顺序性。两者结合可以确保设备发送的消息准确无误地到达目的地，并按照正确的顺序进行处理。
- **实时数据处理**：设备的高频数据能够经过 EMQX 规则 SQL 进行初步的实时处理，可以毫不费力地过滤、提取、丰富和转换 MQTT 消息。将数据发送到 AWS Kinesis Data Streams 后，可以进一步结合 AWS Lambda、AWS 托管的 Apache Flink 实现运行实时分析。
- **弹性伸缩支持**：EMQX 能够轻松连接数百万台物联网设备，并提供了弹性伸缩能力，AWS Kinesis Data Streams 则采用按需模式的自动资源调配和扩展，两者构建的应用能够随连接和数据规模进行扩展，持续满足适配业务的增长需求。
- **持久化数据存储**：AWS Kinesis Data Streams 提供持久化的数据存储能力，能够可靠地保存每秒数百万流入的设备数据流，并在需要时随时回溯历史数据，并进行离线分析和处理。

利用 AWS Kinesis Data Streams 构建的流数据管道，可以大幅降低 EMQX 与 AWS 平台之间的接入难度，为用户提供更丰富、灵活的数据处理方案。够助力 EMQX 用户在 AWS 上构建功能完备、性能卓越的数据驱动型应用。

## Before You Start

This section describes the preparations you need to complete before you start to create an Amazon Kinesis data bridge, including how to set up the Kinesis service and emulate data streams.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridges](./data-bridges.md)

### Create Stream in Amazon Kinesis Data Streams

Follow the steps below to create a Stream via the AWS Management Console (see [this tutorial](https://docs.aws.amazon.com/streams/latest/dev/how-do-i-create-a-stream.html) for more details).

1. Sign in to the AWS Management Console and open the [Kinesis console](https://console.aws.amazon.com/kinesis).

2. In the navigation bar, expand the Region selector and choose a Region.

3. Choose **Create data stream**.

4. On the **Create Kinesis stream** page, enter a name for your data stream and then choose the **On-demand** capacity mode.

### Emulate Amazon Kinesis Data Streams locally

To facilitate the development and test, you can emulate the Amazon Kinesis Data Streams service locally via [LocalStack](https://localstack.cloud/). With LocalStack, you can run your AWS applications entirely on your local machine without connecting to a remote cloud provider.

1. Install and run it using a Docker Image:

   ```bash
   # To start the LocalStack docker image locally
   docker run --name localstack -p '4566:4566' -e 'KINESIS_LATENCY=0' -d localstack/localstack:2.1
   
   # Access the container
   docker exec -it localstack bash
   ```

2. Create a stream named **my_stream** with only one shard:

   ```bash
   awslocal kinesis create-stream --stream-name "my_stream" --shard-count 1
   ```

## Create a Kinesis Data Bridge

1. Go to EMQX Dashboard, click **Integration** -> **Data Bridge**.
2. Click **Create** on the top right corner of the page.
3. In the **Create Data Bridge** page, click to select **Amazon Kinesis**, and then click **Next**.
4. Enter a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.
5. Enter the connection information:

   - **AWS Access Key ID**: Enter the [Access key ID](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html). If using [LocalStack](#emulate-amazon-kinesis-data-streams-locally), enter any value.
   - **AWS Secret Access Key**: Enter the [secret access key](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html). If using [LocalStack](#emulate-amazon-kinesis-data-streams-locally), enter any value.
   - **Amazon Kinesis Endpoint**: Enter the [Endpoint](https://docs.aws.amazon.com/general/latest/gr/ak.html) for the Kinesis service. If using [LocalStack](#emulate-amazon-kinesis-data-streams-locally), input `http://localhost:4566`.
   - **Amazon Kinesis Stream**: Enter the stream name you created in [Create Stream in Amazon Kinesis Data Streams](#create-stream-in-amazon-kinesis-data-streams).
   - **Partition Key**: Enter the Partition Key that shall be associated with records that are sent to this stream. Placeholders of the form `${variable_name}` are allowed (see next step for example on placeholders).
6. In the **Payload Template** field, leave it blank or define a template.

      -  If left blank, it will encode all visible inputs from the MQTT message using JSON format, such as clientid, topic, payload, etc.

      - If using the defined template, placeholders of the form `${variable_name}` will be filled with the corresponding value from the MQTT context. For example, `${topic}` will be replaced with `my/topic` if such is the MQTT message topic.


11. Advanced settings (optional): Choose whether to use buffer queue and batch mode as needed. For details, see [Data Integration](./data-bridges.md).

11. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the Amazon Kinesis server.

12. Click **Create** to finish the creation of the data bridge.

    A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into Amazon Kinesis. You can also create rules by following the steps in [Create Rules for Amazon Kinesis Data Bridge](#create-a-rule-for-amazon-kinesis-data-bridge).

## Create a Rule for Amazon Kinesis Data Bridge

You can continue to create rules to specify the data to be saved into Amazon Kinesis.

1. Go to EMQX Dashboard, click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input `my_rule` as the rule ID.

3. Set the rules in the **SQL Editor**. If you want to save the MQTT messages under topic `t/#` to Amazon Kinesis Data Streams, you can use the SQL syntax below.

   Note: If you want to specify your own SQL syntax, make sure that the `SELECT` part includes all fields required by the payload template in the data bridge.

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list, and then select the data bridge you just created under **Data Bridge**. Then click the **Add** button.

4. Click **Create** at the page bottom to finish the creation.

Now a rule to forward data to Amazon Kinesis Data Streams via the Amazon Kinesis bridge is created. You can click **Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#` are sent and saved to Amazon Kinesis Data Streams after parsing by rule `my_rule`.

## Test Data Bridge and Rule

1. Use MQTTX to send messages on the topic `t/my_topic`.

   ```bash
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

2. Check the running status of the data bridge, there should be one new incoming and one new outgoing message.

3. Go to [Amazon Kinesis Data Viewer](https://docs.aws.amazon.com/streams/latest/dev/data-viewer.html). You should see the message when getting records.

### Use LocalStack to Check

If you use LocalStack, follow the steps below to check the received data.

1. Use the following command to get the *ShardIterator* before sending the message to the bridge.
   
   ```bash
   awslocal kinesis get-shard-iterator --stream-name my_stream --shard-id shardId-000000000000 --shard-iterator-type LATEST
   {
   "ShardIterator": "AAAAAAAAAAG3YjBK9sp0uSIFGTPIYBI17bJ1RsqX4uJmRllBAZmFRnjq1kPLrgcyn7RVigmH+WsGciWpImxjXYLJhmqI2QO/DrlLfp6d1IyJFixg1s+MhtKoM6IOH0Tb2CPW9NwPYoT809x03n1zL8HbkXg7hpZjWXPmsEvkXjn4UCBf5dBerq7NLKS3RtAmOiXVN6skPpk="
   }
   ```
   
2. Use MQTTX to send messages on the topic `t/my_topic`.

   ```bash
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

3. Read the records and decode the received data.
   ```bash
   awslocal kinesis get-records --shard-iterator="AAAAAAAAAAG3YjBK9sp0uSIFGTPIYBI17bJ1RsqX4uJmRllBAZmFRnjq1kPLrgcyn7RVigmH+WsGciWpImxjXYLJhmqI2QO/DrlLfp6d1IyJFixg1s+MhtKoM6IOH0Tb2CPW9NwPYoT809x03n1zL8HbkXg7hpZjWXPmsEvkXjn4UCBf5dBerq7NLKS3RtAmOiXVN6skPpk="
   {
       "Records": [
           {
               "SequenceNumber": "49642650476690467334495639799144299020426020544120356866",
               "ApproximateArrivalTimestamp": 1689389148.261,
               "Data": "eyAibXNnIjogImhlbGxvIEFtYXpvbiBLaW5lc2lzIiB9",
               "PartitionKey": "key",
               "EncryptionType": "NONE"
           }
       ],
       "NextShardIterator": "AAAAAAAAAAFj5M3+6XUECflJAlkoSNHV/LBciTYY9If2z1iP+egC/PtdVI2t1HCf3L0S6efAxb01UtvI+3ZSh6BO02+L0BxP5ssB6ONBPfFgqvUIjbfu0GOmzUaPiHTqS8nNjoBtqk0fkYFDOiATdCCnMSqZDVqvARng5oiObgigmxq8InciH+xry2vce1dF9+RRFkKLBc0=",
       "MillisBehindLatest": 0
   }
   
   echo 'eyAibXNnIjogImhlbGxvIEFtYXpvbiBLaW5lc2lzIiB9' | base64 -d
   { "msg": "hello Amazon Kinesis" }
   ```
