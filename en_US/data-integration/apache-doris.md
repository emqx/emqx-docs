# Ingest MQTT Data into Apache Doris

[Apache Doris](https://doris.apache.org/) is a modern Massively Parallel Processing (MPP) analytical database system known for high concurrency, high performance, and ease of use. It is particularly well-suited for scenarios involving real-time analytics and data warehousing. With EMQX 5.10.0, you can integrate MQTT data with Apache Doris, enabling efficient storage, real-time analysis, and powerful data visualization.

This guide provides practical instructions on how to configure and validate the data integration between EMQX and Apache Doris.

## How It Works

Apache Doris data integration is an out-of-the-box feature in EMQX, which enables complex business development through simple configuration. In a typical IoT application, EMQX, as the IoT platform, is responsible for device connection and transmitting messages. Apache Doris, as the data storage platform, is responsible for storing device status and metadata, as well as message data storage and data analysis.

<img src="./assets/doris-integration.png" alt="doris-integration" style="zoom:67%;" />

EMQX forwards device events and data to Apache Doris through the rule engine and Sink. Applications can read the data in Apache Doris to sense the device status, obtain device online and offline records, and analyze device data. The specific workflow is as follows:

- **IoT devices connect to EMQX**: After IoT devices are successfully connected through the MQTT protocol, online events will be triggered. The events include information such as device ID, source IP address, and other attributes.
- **Message publication and reception**: The devices publish telemetry and status data to specific topics. When EMQX receives these messages, it initiates the matching process within its rules engine.
- **Rule Engine Processing Messages**: With the built-in rules engine, messages and events from specific sources can be processed based on topic matching. The rules engine matches the corresponding rules and processes messages and events, such as converting data formats, filtering out specific information, or enriching messages with contextual information.
- **Write to Apache Doris**: The rule triggers the writing of messages to Apache Doris. With the help of SQL templates, users can extract data from the rule processing results to construct SQL and send it to Apache Doris for execution, so that specific fields of the message can be written or updated into the corresponding tables and columns of the database.

After the event and message data are written to Apache Doris, you can connect to Apache Doris to read the data for flexible application development, such as:

- Connect to visualization tools, such as Grafana, to generate charts based on data and show data changes.
- Connect to the device management system, view the device list and status, detect abnormal device behavior, and eliminate potential problems in a timely manner.

## Features and Benefits

The data integration with Apache Doris can bring the following features and advantages to your business:

- **Flexible Event Handling**: Through the EMQX rules engine, Apache Doris can handle device lifecycle events, greatly facilitating the development of various management and monitoring tasks required for implementing IoT applications. By analyzing event data, you can promptly detect device failures, abnormal behavior, or trend changes to take appropriate measures.
- **Message Transformation**: Messages can undergo extensive processing and transformation through EMQX rules before being written to Apache Doris, making storage and usage more convenient.
- **Real-Time Data Ingestion**: Apache Doris supports real-time data ingestion via HTTP and JDBC interfaces. When integrated with EMQX, MQTT data can be written directly into Doris tables with low latency, making it ideal for scenarios that require immediate query and analytics capabilities.
- **Streaming Synchronization**: Apache Doris also supports ingesting real-time data streams from sources like Flink, Kafka, and transactional databases. This makes it ideal for building unified pipelines that combine MQTT data from EMQX with other streaming data sources for comprehensive real-time analysis.
- **Standard SQL and Ecosystem Compatibility**: Doris is fully compatible with MySQL syntax and supports standard SQL, allowing users to perform powerful analytical queries without learning new languages. It integrates easily with business intelligence (BI) tools and client applications for dashboards, reports, and automation workflows.
- **Runtime Metrics**: Support for viewing runtime metrics of each Sink, such as total message count, success/failure counts, current rates, and more.

Through flexible event handling, extensive message transformation, flexible data operations, and real-time monitoring and analysis capabilities, you can build efficient, reliable, and scalable IoT applications, benefiting your business decisions and optimizations.

## Before You Start

This section describes the preparations you need to complete before you start to create the Apache Doris data integration in EMQX Dashboard, including installing the Apache Doris server and creating data tables.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)

- Knowledge about [Data Integration](./data-bridges.md)

### Install Apache Doris Server

Follow the [official guide](https://doris.apache.org/docs/dev/gettingStarted/quick-start#use-docker-for-quick-deployment) to run Doris locally using Docker Compose.

### Create Data Tables

You can use a MySQL client to connect to a Doris Frontend and issue commands.  See the [official documentation](https://doris.apache.org/docs/dev/gettingStarted/quick-start#run-queries).

For example:

```sh
mysql -uroot -P9030 -h127.0.0.1
```

You need to create a database and two tables in Apache Doris:

- The data table `emqx_messages` is for storing the client ID, topic, payload, and creation time of every message.
- The data table `emqx_client_events` is for storing the client ID, event type, and creation time of every event.

```sql
create database mqtt;
use mqtt;

create table if not exists
  emqx_messages(
    clientid varchar,
    topic string,
    payload string,
    created_at datetime
  )
  properties (replication_num = 1);

create table if not exists
  emqx_client_events(
    clientid varchar,
    event varchar,
    created_at datetime)
  properties (replication_num = 1);
```

## Create a Connector

This section demonstrates how to create a Connector to connect the Sink to the Apache Doris server.

The following steps assume that you run both EMQX and Apache Doris on the local machine. If you have Apache Doris and EMQX running remotely, adjust the settings accordingly.

1. Enter the EMQX Dashboard and click **Integration** -> **Connectors**.
2. Click **Create** in the top right corner of the page.
3. On the **Create Connector** page, select **Doris** and then click **Next**.
4. In the **Configuration** step, configure the following information:
   - **Connector name**: Enter a name for the connector, which should be a combination of upper and lower case letters and numbers, for example: `my_doris`.
   - **Server Host**: Enter `127.0.0.1:9030`, or the actual hostname if the Apache Doris server is running remotely.
   - **Database Name**: Enter `mqtt`.
   - **Username**: Enter `root`.
   - **Password**: Enter `public`.
5. Advanced settings (optional):  See [Advanced Configurations](#advanced-configurations).
6. Before clicking **Create**, you can click **Test Connectivity** to test if the connector can connect to the Apache Doris server.
7. Click the **Create** button at the bottom to complete the creation of the connector. In the pop-up dialog, you can click **Back to Connector List** or click **Create Rule** to continue creating rules with Sinks to specify the data to be forwarded to Apache Doris and record client events. For detailed steps, see [Create a Rule with Apache Doris Sink for Message Storage](#create-a-rule-with-apache-doris-sink-for-message-storage) and [Create a Rule with Apache Doris Sink for Events Recording](#create-a-rule-with-apache-doris-sink-for-events-recording).

## Create a Rule with Apache Doris Sink for Message Storage

This section demonstrates how to create a rule in the Dashboard for processing messages from the source MQTT topic `t/#`, and saving the processed data to the Apache Doris data table `emqx_messages` via the configured Sink.

This demonstration assumes that you run both EMQX and Apache Doris on the local machine. If you have Apache Doris and EMQX running remotely, adjust the settings accordingly.

1. Go to EMQX Dashboard, click **Integration** -> **Rules**.

2. Click **Create** in the top right corner of the page.

3. Enter `my_rule` as the rule ID, and set the rules in the **SQL Editor** with the following statement, which means the MQTT messages under topic `t/#`  will be saved to Apache Doris.

   Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the Sink in the `SELECT` part.

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   If you are a beginner user, click **SQL Examples** and **Enable Test** to learn and test the SQL rule.

   :::

4. Click the + **Add Action** button to define an action to be triggered by the rule. With this action, EMQX sends the data processed by the rule to Apache Doris.

5. Select `Apache Doris` from the **Type of Action** dropdown list. Keep the **Action** dropdown with the default `Create Action` value. You can also select a Sink if you have created one. This demonstration will create a new Sink.

6. Enter a name for the Sink. The name should be a combination of upper/lower case letters and numbers.

7. Select the `my_mysql` just created from the **Connector** dropdown box. You can also create a new Connector by clicking the button next to the dropdown box. For the configuration parameters, see [Create a Connector](#create-a-connector).

8. Configure the **SQL Template** based on the feature to use:

   Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements.

   ```sql
   INSERT INTO emqx_messages(clientid, topic, payload, created_at) VALUES(
     ${clientid},
     ${topic},
     ${payload},
     FROM_UNIXTIME(${timestamp}/1000)
   )
   ```

   If a placeholder variable is undefined in the SQL template, you can toggle the **Undefined Vars as Null** switch above the **SQL template** to define the rule engine behavior:

   - **Disabled** (default): The rule engine can insert the string `undefined` into the database.

   - **Enabled**: Allow the rule engine to insert `NULL` into the database when a variable is undefined.

     ::: tip

     If possible, this option should always be enabled; disabling the option is only used to ensure backward compatibility.

     :::

9. **Fallback Actions (Optional)**: If you want to improve reliability in case of message delivery failure, you can define one or more fallback actions. These actions will be triggered if the primary Sink fails to process a message. See [Fallback Actions](./data-bridges.md#fallback-actions) for more details.

10. **Advanced settings (optional)**:  See [Advanced Configurations](#advanced-configurations).

11. Click the **Create** button to complete the Sink configuration. A new Sink will be added to the **Action Outputs.**

12. Back on the **Create Rule** page, verify the configured information. Click the **Create** button to generate the rule.

You have now successfully created the rule. You can see the newly created rule on the **Integration** -> **Rules** page. Click the **Actions(Sink)** tab and you can see the new Apache Doris Sink.

You can also click **Integration** -> **Flow Designer** to view the topology and you can see that the messages under topic `t/#`  are sent and saved to Apache Doris.

## Create a Rule with Apache Doris Sink for Events Recording

This section demonstrates how to create a rule for recording the clients' online/offline status and saving the events data to the Apache Doris table `emqx_client_events` via a configured Sink.

The rule creation steps are similar to those in [Creating a rule with Apache Doris Sink for Message Storage](#create-a-rules-with-apache-doris-sink-for-message-storage) except for the SQL rule syntax and SQL template.

To create a rule for online/offline status recording, you can enter the following statement in the **SQL Editor**:

```sql
SELECT
  *
FROM
  "$events/client_connected", "$events/client_disconnected"
```

To insert the client events data to the data table, you can use the following SQL template:

```sql
INSERT INTO emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  FROM_UNIXTIME(${timestamp}/1000)
)
```

## Test the Rules

Use MQTTX to send a message to topic  `t/1`  to trigger an online/offline event.

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello Apache Doris" }'
```

Check the running status of the two Sinks, there should be one new incoming and one new outgoing message, and 2 event records.

Check whether the data is written into the `emqx_messages` data table.

```bash
mysql> select * from emqx_messages;
+----------+-------+--------------------------+---------------------+
| clientid | topic | payload                  | created_at          |
+----------+-------+--------------------------+---------------------+
| emqx_c   | t/1   | { "msg": "hello Apache Doris" } | 2022-12-09 08:44:07 |
+----------+-------+--------------------------+---------------------+
1 row in set (0.01 sec)
```

Check whether the data is written into the `emqx_client_events` table.

```bash
mysql> select * from emqx_client_events;
+----------+---------------------+---------------------+
| clientid | event               | created_at          |
+----------+---------------------+---------------------+
| emqx_c   | client.connected    | 2022-12-09 08:44:07 |
| emqx_c   | client.disconnected | 2022-12-09 08:44:07 |
+----------+---------------------+---------------------+
2 rows in set (0.00 sec)
```

## Advanced Configurations

This section delves deeper into the advanced configuration options available for the Apache Doris Connector and Sink. When configuring the Connector and Sink in the Dashboard, navigate to **Advanced Settings** to tailor the following parameters to meet your specific needs.

| **Fields**                | **Descriptions**                                             | **Recommended Value** |
| ------------------------- | ------------------------------------------------------------ | --------------------- |
| **Connection Pool Size**  | Specifies the number of concurrent connections that can be maintained in the connection pool when interfacing with the Apache Doris service. This option helps in managing the application's scalability and performance by limiting or increasing the number of active connections between EMQX and Apache Doris.<br/>**Note**: Setting an appropriate connection pool size depends on various factors such as system resources, network latency, and the specific workload of your application. Too large a pool size may lead to resource exhaustion, while too small a size may limit throughput. | `8`                   |
| **Start Timeout**         | Determines the maximum time interval, in seconds, that the Connector will wait for an auto-started resource to reach a healthy state before responding to resource creation requests. This setting helps ensure that the Connector does not proceed with operations until it verifies that the connected resource—such as a database instance in Apache Doris—is fully operational and ready to handle data transactions. | `5` second            |
| **Buffer Pool Size**      | Specifies the number of buffer worker processes that will be allocated for managing data flow in egress-type Sinks between EMQX and Apache Doris. These worker processes are responsible for temporarily storing and handling data before it is sent to the target service. This setting is particularly relevant for optimizing performance and ensuring smooth data transmission in egress (outbound) scenarios. For Sinks that only deal with ingress (inbound) data flow, this option can be set to "0" as it is not applicable. | `16`                  |
| **Request TTL**           | The "Request TTL" (Time To Live) configuration setting specifies the maximum duration, in seconds, that a request is considered valid once it enters the buffer. This timer starts ticking from the moment the request is buffered. If the request stays in the buffer for a period exceeding this TTL setting or if it is sent but does not receive a timely response or acknowledgment from Apache Doris, the request is deemed to have expired. | `45` second           |
| **Health Check Interval** | Specifies the time interval, in seconds, at which the Connector will perform automated health checks on the connection to Apache Doris. | `15` second           |
| **Max Buffer Queue Size** | Specifies the maximum number of bytes that can be buffered by each buffer worker in the Connector. Buffer workers temporarily store data before it is sent to Apache Doris, serving as an intermediary to handle data flow more efficiently. Adjust the value according to your system's performance and data transfer requirements. | `256` MB              |
| **Max Batch Size**        | Specifies the maximum size of data batches transmitted from EMQX to Apache Doris in a single transfer operation. By adjusting the size, you can fine-tune the efficiency and performance of data transfer between EMQX and Apache Doris.<br />If the "Max Batch Size" is set to "1," data records are sent individually, without being grouped into batches. | `1`                   |
| **Query Mode**            | Allows you to choose `asynchronous` or `synchronous` query modes to optimize message transmission based on different requirements. In asynchronous mode, writing to Apache Doris does not block the MQTT message publish process. However, this might result in clients receiving messages ahead of their arrival in Apache Doris. | `Async`               |
| **Inflight Window**       | An "in-flight query" refers to a query that has been initiated but has not yet received a response or acknowledgment. This setting controls the maximum number of in-flight queries that can exist simultaneously when the Connector is communicating with Apache Doris.<br/>When the **Query Mode** is set to `async` (asynchronous), the "Inflight Window" parameter gains special importance. If it is crucial for messages from the same MQTT client to be processed in strict order, you should set this value to 1. | `100`                 |
