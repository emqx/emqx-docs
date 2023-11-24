# Ingest MQTT Data into InfluxDB

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

InfluxDB is a database for storing and analyzing time series data. Its powerful data throughput capability and stable performance make it very suitable to be applied in the field of Internet of Things (IoT). EMQX now supports connection to mainstream versions of InfluxDB Cloud, InfluxDB OSS, or InfluxDB Enterprise. 

This page provides a comprehensive introduction to the data integration between EMQX and InfluxDB with practical instructions on creating a rule and data bridge.

## How It Works

InfluxDB data integration is an out-of-the-box feature in EMQX that combines EMQX's real-time data capturing and transmission capabilities with InfluxDB's data storage and analysis functionality. With a built-in [rule engine](./rules.md) component, the integration simplifies the process of ingesting data from EMQX to InfluxDB for storage and analysis, eliminating the need for complex coding. EMQX forwards device data to InfluxDB through the rule engine and data bridge. InfluxDB analyzes the data using SQL statements, generates reports, charts, and other data analysis results, and presents them to users through InfluxDB's visualization tools. 

The diagram below illustrates the typical architecture of data integration between EMQX and InfluxDB in an energy storage scenario.

![MQTT to InfluxDB](./assets/mqtt-to-influxdb.jpg)

EMQX and InfluxDB provide an extensible IoT platform for efficiently collecting and analyzing energy consumption data in real-time. In this architecture, EMQX serves as the IoT platform, handling device access, message transmission, and data routing, while InfluxDB serves as the data storage and analysis platform, responsible for data storage and analysis functions. The workflow is as follows:

1. **Message publication and reception**: Energy storage devices and Industrial IoT devices establish successful connections to EMQX through the MQTT protocol and regularly publish energy consumption data using the MQTT protocol, including information such as power consumption, input/output power, etc. When EMQX receives these messages, it initiates the matching process within its rules engine.  
3. **Message data processing**: Using the built-in rule engine, messages from specific sources can be processed based on topic matching. When a message arrives, it passes through the rule engine, which matches it with the corresponding rule and processes the message data, such as transforming data formats, filtering specific information, or enriching messages with contextual information.
4. **Data ingestion into InfluxDB**: Rules defined in the rule engine trigger the operation of writing messages to InfluxDB. The InfluxDB data bridge provides Line Protocol templates that allow flexible definitions of the data format to be written, mapping specific fields from the message to the corresponding measurement and field in InfluxDB.

After energy consumption data is written to InfluxDB, you can use SQL statements flexibly to analyze the data, for example:

- Connect to visualization tools like Grafana to generate charts based on the data, displaying energy storage data.
- Connect to business systems for monitoring and alerting on the status of energy storage devices.

## Features and Benefits

The InfluxDB data integration offers the following features and advantages:

- **Efficient Data Processing**: EMQX can handle a massive number of IoT device connections and message throughput, while InfluxDB excels in data writing, storage, and querying, providing outstanding performance to meet the data processing needs of IoT scenarios without overburdening the system.
- **Message Transformation**: Messages can undergo extensive processing and transformation through EMQX rules before being written into InfluxDB.
- **Scalability**: Both EMQX and InfluxDB are capable of cluster scaling, allowing flexible horizontal expansion of clusters as business needs grow.
- **Rich Query Capabilities**: InfluxDB offers optimized functions, operators, and indexing techniques, enabling efficient querying and analysis of timestamped data, and accurately extracting valuable insights from IoT time-series data.
- **Efficient Storage**: InfluxDB uses encoding methods with high compression ratios, significantly reducing storage costs. It also allows customization of storage durations for different data types to avoid unnecessary data occupying storage space.

## Before You Start

This section describes the preparations you need to complete before you start to create the InfluxDB data bridges, including installing and setting up InfluxDB.

### Prerequisites

- Knowledge about [InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/), as EMQX will follow this protocol when writing data into InfluxDB

- Knowledge about EMQX data integration [rules](./rules.md)

- Knowledge about [data bridge](./data-bridges.md)


### Install and Set Up InfluxD

1. [Install InfluxDB](https://docs.influxdata.com/influxdb/v2.5/install/) via Docker, and then run the docker image.

```bash
# TO start the InfluxDB docker image
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. With InfluxDB running, visit [http://localhost:8086](http://localhost:8086). Set the **Username**, **Password**, **Organization Name**, and **Bucket Name**.
3. In the InfluxDB UI, click **Load Data** -> **API Token** and then follow the instructions to [create all-access tokens](https://docs.influxdata.com/influxdb/v2.5/install/#create-all-access-tokens).

## Create Rule and InfluxDB Data Bridge

This section demonstrates how to create a rule in EMQX to process messages from the source MQTT topic `t/#`  and send the processed results through a configured data bridge to InfluxDB. 

This tutorial assumes that you run both EMQX and InfluxDB on the local machine. If you have InfluxDB and EMQX running remotely, adjust the settings accordingly.

1. Go to EMQX Dashboard, and click **Integration** -> **Rules** from the left navigation menu.

2. Click **Create** on the top right corner of the page.

3. On the Create Rule page, enter `my_rule` as the rule ID.

4. Set the rules in the **SQL Editor**, for example, if you want to save the MQTT messages of the topic `t/#`  to InfluxDB, you can use the SQL syntax below. 

   ::: tip

   If you want to specify your own SQL syntax, make sure that the fields selected (in the `SELECT` part) include all variables in the data format specified in the later configured data bridge.

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   Note: If you are a beginner user, click **SQL Examples** and **Enable Test** to learn and test the SQL rule. 

5. Click the **+ Add Action** button to define an action that will be triggered by the rule. Select **Forwarding with Data Bridge** from the dropdown list. With this action, EMQX sends the data processed by the rule to InfluxDB. 

6. Click the **+** icon next to the **Data bridge** drop-down box to create a data bridge. Select `InfluxDB` from the **Type of Data Bridge** drop-down list. 

7. Configure the following information on the **Create Data Bridge** page.

   - Enter a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.
   - Select the InfluxDB version as needed, by default `v2` is selected.
   - Enter the InfluxDB connection information:
     - In **Server Host**, enter `127.0.0.1:8086`. If you are creating a connection to InfluxDB Cloud, use 443 as the port number, that is, enter `{url}:443` and click **Enable TLS**.
     - Enter the **Organization**, **Bucket**, and **Token** you set in the [Install InfluxDB Server](#install-influxdb-server). Note: If you select `v1` as **Version of InfluxDB**, please set the **Database**, **Username** and **Password** as required.
     - Set the **Time Precision**, it is set to millisecond by default. 

   - Enable TLS connection as necessary by clicking the **Enable TLS** toggle switch.

   - Select **Data Format** as `JSON` or `Line Protocol` for how data should be parsed and written into InfluxDB.

     - For JSON format, define data parsing method, including **Measurement**, **Timestamp**, **Fields,** and **Tags**. Note: All key values can be variables or placeholders, and you can also follow the [InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/) to set them.
     - For Line Protocol format, specify a text-based format that provides the measurement, tag set, field set, timestamp of a data point, and placeholder supported according to the [InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) syntax.

     ::: tip

     - To write a signed integer type value to InfluxDB 1.x or 2.x, add `i` as the type identifier after the placeholder, for example, `${payload.int}i`. See also [InfluxDB 1.8 write integer value](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb).
     - To write an unsigned integer type value to InfluxDB 1.x or 2.x, add `u` as the type identifier after the placeholder, for example, `${payload.int}u`. See also [InfluxDB 1.8 write integer value](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb).

     :::

8. Advanced settings (optional):  See [Advanced Configurations](#advanced-configurations).

9. Click the **Add** button to complete the data bridge configuration. You will be redirected back to the **Add Action** page. Select the InfluxDB Data Bridge you just created from the **Data bridge** drop-down list. Click the **Add** button at the bottom to include this action in the rule.

10. Back on the **Create Rule** page, verify the configured information. Click the **Create** button to generate the rule. The rule you created is shown in the rule list and the **status** should be connected.

Now a rule to forward data to InfluxDB via an InfluxDB bridge is created. You can click **Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to InfluxDB after parsing by the rule  `my_rule`.

### Test Rule and Data Bridge

Use MQTTX  to send a message to topic  `t/1`  to trigger an online/offline event.

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

Check the running status of the data bridge, there should be one new incoming and one new outgoing message.

In the InfluxDB UI, you can confirm whether the message is written into the InfluxDB via the **Data Explorer** window.

## Advanced Configurations

This section delves deeper into the advanced configuration options available for the InfluxDB data bridge. When configuring the data bridge in the Dashboard, navigate to **Advanced Settings** to tailor the following parameters to meet your specific needs.

| **Fields**                | **Descriptions**                                             | **Recommended Value** |
| ------------------------- | ------------------------------------------------------------ | --------------------- |
| **Start Timeout**         | Determines the maximum time interval, in seconds, that the EMQX data bridge will wait for an auto-started resource to reach a healthy state before responding to resource creation requests. This setting helps ensure that the data bridge does not proceed with operations until it verifies that the connected resource—such as a database instance in InfluxDB—is fully operational and ready to handle data transactions. | `5`                   |
| **Buffer Pool Size**      | Specifies the number of buffer worker processes that will be allocated for managing data flow in egress-type bridges between EMQX and InfluxDB. These worker processes are responsible for temporarily storing and handling data before it is sent to the target service. This setting is particularly relevant for optimizing performance and ensuring smooth data transmission in egress (outbound) scenarios. For bridges that only deal with ingress (inbound) data flow, this option can be set to "0" as it is not applicable. | `16`                  |
| **Request TTL**           | The "Request TTL" (Time To Live) configuration setting specifies the maximum duration, in seconds, that a request is considered valid once it enters the buffer. This timer starts ticking from the moment the request is buffered. If the request stays in the buffer for a period exceeding this TTL setting or if it is sent but does not receive a timely response or acknowledgment from InfluxDB, the request is deemed to have expired. | `45`                  |
| **Health Check Interval** | Specifies the time interval, in seconds, at which the data bridge will perform automated health checks on the connection to InfluxDB. | `15`                  |
| **Max Buffer Queue Size** | Specifies the maximum number of bytes that can be buffered by each buffer worker in the InfluxDB data bridge. Buffer workers temporarily store data before it is sent to InfluxDB, serving as an intermediary to handle data flow more efficiently. Adjust the value according to your system's performance and data transfer requirements. | `256`                 |
| **Max Batch Size**        | Specifies the maximum size of data batches that can be transmitted from EMQX to InfluxDB in a single transfer operation. By adjusting the size, you can fine-tune the efficiency and performance of data transfer between EMQX and InfluxDB.<br />If the "Max Batch Size" is set to "1," data records are sent individually, without being grouped into batches. | `1`                   |
| **Query Mode**            | Allows you to choose `asynchronous` or `synchronous` query modes to optimize message transmission based on different requirements. In asynchronous mode, writing to InfluxDB does not block the MQTT message publish process. However, this might result in clients receiving messages ahead of their arrival in InfluxDB. | `Async`               |
| **Inflight Window**       | An "in-flight query" refers to a query that has been initiated but has not yet received a response or acknowledgment. This setting controls the maximum number of in-flight queries that can exist simultaneously when the data bridge is communicating with InfluxDB.<br/>When the **Query Mode** is set to `async` (asynchronous), the "Inflight Window" parameter gains special importance. If it is crucial for messages from the same MQTT client to be processed in strict order, you should set this value to 1. | `100`                 |

## More Information

Check out the following links to learn more:

**Blogs**:

[Build EMQX + InfluxDB + Grafana IoT data visualization solution in one hour](https://www.emqx.com/en/blog/build-emqx-influxdb-grafana-iot-data-visualization-solution-in-one-hour)

[EMQX Persistence Plug-in Series (II) - InfluxDB Data Storage](https://www.emqx.com/en/blog/emqx-persistence-plug-in-series-influxdb-data-storage)

[MQTT Performance Benchmark Testing: EMQX-InfluxDB Integration](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-influxdb-integration)
