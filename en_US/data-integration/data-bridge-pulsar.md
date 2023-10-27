# Stream MQTT Data into Apache Pulsar

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

[Apache Pulsar](https://pulsar.apache.org/) is a popular open-source distributed event streaming platform. EMQX's integration with Apache Pulsar offers users dependable data transport and processing capabilities in high-throughput situations.

Streaming data into Apache Pulsar involves creating data bridges to Pulsar in the producer role (sends messages to Pulsar).

## How It Works



## Features and Benefits



## Before You Start

This section describes the preparations you need to complete before you start to create the Pulsar data bridges in EMQX Dashboard.

This tutorial assumes you run EMQX and Pulsar on the local machine. If you have Pulsar and EMQX running remotely, adjust the settings accordingly.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

### Install Pulsar

Run Pulsar in Docker.

```bash
docker run --rm -it -p 6650:6650 --name pulsar apachepulsar/pulsar:2.11.0 bin/pulsar standalone -nfw -nss
```

Refer to the [Quick Start section in Pulsar Documentation](https://pulsar.apache.org/docs/2.11.x/getting-started-home/) for detailed operation steps.

### Create Pulsar Topic

Relevant Pulsar topics should be created before creating the data bridge in EMQX. Use the commands below to create a topic called `my-topic` in Pulsar under the `public` tenant, `default` namespace, with 1 partition.

```bash
docker exec -it pulsar bin/pulsar-admin topics create-partitioned-topic persistent://public/default/my-topic -p 1
```

## Create Rule and Pulsar Data Bridge

This section demonstrates how to create a Pulsar producer data bridge via the Dashboard.

1. Go to EMQX Dashboard, and click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Enter a rule ID, for example, `my_rule`.

4. Enter the following statement in the **SQL Editor** if you want to save the MQTT messages under topic `t/#` to Pulsar.

   Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the data bridge in the `SELECT` part.

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   Note: If you are a beginner user, click **SQL Examples** and **Enable Test** to learn and test the SQL rule. 

5. Click the **+ Add Action** button to define an action that will be triggered by the rule. Select **Forwarding with Data Bridge** from the dropdown list. With this action, EMQX sends the data processed by the rule to Pulsar.

6. Click the **+** icon next to the **Data bridge** drop-down box to create a data bridge.

7. Select `Pulsar` from the **Type of Data Bridge** drop-down list. 

8. Enter a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

9. Configure the following options for the data bridge in the producer role: 

   - **Name**: Enter a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.
   - **Bridge Role**: `Producer` is selected by default.
   - Configure the information for connecting to the Pulsar server and message writing:
     - **Servers**: Enter `pulsar://localhost:6650` for the **Servers**. If you have Pulsar and EMQX running remotely, adjust the settings accordingly.
     - **Authentication**: Select the authentication method based on your actual situation: `none`, `Basic auth`, or `token`.
     - **Pulsar Topic Name**: Enter `persistent://public/default/my-topic`, the pulsar topic you created before. Note: Variables are not supported here.
     - **Partition Strategy**: Select the way for the producer to dispatch messages to Kafka partitions: `random`, `roundrobin`, or `key_dispatch`.
     - **Compression**: Specify whether or not to use compression algorithms and which algorithms are used to compress/decompress the records in a Pulsar message. The optional values are: `no_compression`, `snappy`, or `zlib`.
   - **Message Key**: Pulsar message key. Insert a string here, either a plain string or a string containing placeholders (${var}).
   - **Message Value**: Pulsar message value. Insert a string here, either a plain string or a string containing placeholders (${var}).

10. Advanced settings (optional): See [Advanced Configurations](#advanced-configurations).
11. Click the **Add** button to complete the data bridge configuration. You will be redirected back to the **Add Action** page. Select the Pulsar Data Bridge you created from the **Data bridge** drop-down list. Click the **Add** button at the bottom to include this action in the rule.

- Back on the **Create Rule** page, verify the configured information. Click the **Create** button to generate the rule. The rule you created should be shown in the rule list and the **status** should be connected.

Now a rule of forwarding data to Pulsar via a Pulsar data bridge is created.  You can click **Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#` are sent and saved to Pulsar after parsing by rule `my_rule`.

## Test Rule and Data Bridge

 Use MQTTX to send messages to topic  `t/1`:

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Pulsar" }'
```

Check the data bridge running status, there should be one new incoming and one new outgoing message.

Check whether messages are written into the topic `persistent://public/default/my-topic` with the following Pulsar command:

   ```bash
   docker exec -it pulsar bin/pulsar-client consume -n 0 -s mysubscriptionid -p Earliest persistent://public/default/my-topic
   ```

## Advanced Configurations

This section describes some advanced configuration options that can optimize the performance of your data bridge and customize the operation based on your specific scenarios. When creating the data bridge, you can unfold the **Advanced Settings** and configure the following settings according to your business needs.

| Fields                                      | Descriptions                                                 | Recommended Values |
| ------------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Sync Publish Timeout                        |                                                              | `3`                |
| Retention Period                            | The maximum duration to wait when the bridge requests metadata from Kafka. | `5`                |
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
