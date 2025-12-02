# Ingest MQTT Data into AWS Timestream for InfluxDB

[AWS Timestream for InfluxDB](https://docs.aws.amazon.com/timestream/latest/developerguide/timestream-for-influxdb.html) is a fully managed time series database service that enables you to run InfluxDB 2.x workloads on AWS with simplified data ingestion and real-time analytics. It provides millisecond-level queries with automated backups, updates, and high availability, making it well-suited for IoT and real-time analytics. Existing InfluxDB 2.x APIs and tools work seamlessly with Amazon Timestream for InfluxDB.

Starting from EMQX 6.1, EMQX adds native support for integrating with Amazon Timestream for InfluxDB in addition to existing support for InfluxDB Cloud, InfluxDB OSS, and InfluxDB Enterprise.

This page provides a comprehensive introduction to data integration between EMQX and Amazon Timestream for InfluxDB, along with practical instructions for configuring and validating the data flow.

## How It Works

Amazon Timestream for InfluxDB integration builds on EMQX’s real-time data processing and routing capabilities and combines them with Timestream’s fully managed, high-performance InfluxDB engine.

Using the built-in [rule engine](./rules.md), EMQX can transform MQTT messages and write them directly into a Timestream for InfluxDB DB instance without requiring custom application code.

Through the rule engine and the Timestream for InfluxDB Sink, EMQX forwards device data to an InfluxDB-compatible organization and bucket for storage and analysis. After ingestion, you can use the InfluxUI, Flux/InfluxQL queries, or visualization tools to analyze data in real time.

The diagram below illustrates the typical data integration architecture between EMQX and Amazon Timestream for InfluxDB in an energy storage scenario.

![MQTT to InfluxDB](/Users/emqx/Documents/GitHub/emqx-docs/en_US/data-integration/assets/mqtt-to-influxdb.jpg)

EMQX and Amazon Timestream for InfluxDB together provide a scalable IoT data pipeline for real-time energy monitoring and analytics. EMQX serves as the IoT messaging layer, handling device connectivity and data routing, while Timestream for InfluxDB provides managed time series storage and query capabilities. The workflow is as follows:

1. **Message publication and reception**: Devices connect to EMQX over MQTT and publish telemetry (e.g., power usage, charge/discharge metrics). When EMQX receives these messages, it initiates the matching process within its rules engine.  
2. **Message processing**: The rule engine matches topics and applies transformations such as filtering, field extraction, or data enrichment, preparing the payload for ingestion into the target Timestream for InfluxDB bucket.
3. **Data ingestion into InfluxDB**: When a rule triggers the Timestream for InfluxDB Sink, EMQX writes the data using InfluxDB Line Protocol. Templates define how MQTT fields map to measurements, tags, and fields.

Once stored in Timestream for InfluxDB, you can use Flux/InfluxQL queries, the InfluxUI, or tools like Grafana to visualize power metrics or integrate with business systems for monitoring and alerting.

## Features and Benefits

The Amazon Timestream for InfluxDB integration offers the following features and advantages:

- **Efficient Data Processing**: EMQX handles large-scale IoT connections and high-throughput MQTT data, while Timestream for InfluxDB provides fast ingestion and millisecond-level query performance for real-time analytics.
- **Message Transformation**: EMQX rules allow flexible filtering, extraction, and transformation of message data before writing it to Timestream for InfluxDB using InfluxDB Line Protocol.
- **Managed Scalability**: EMQX supports horizontal clustering for massive IoT deployments, and Timestream for InfluxDB provides managed instance scaling, automated backups, and seamless version updates.
- **Rich Query Capabilities**: Timestream for InfluxDB supports the full InfluxDB 2.x query ecosystem, including Flux and InfluxQL, enabling powerful time-series analysis and integration with downstream tools.
- **Optimized Storage**: Timestream for InfluxDB uses AWS-managed storage with preconfigured IOPS and throughput tiers, delivering efficient, cost-optimized performance for time-series data workloads.

## Before You Start

This section outlines the preparations required before creating the data integration, including setting up your Amazon Timestream for InfluxDB environment and obtaining the necessary connection parameters.

### Prerequisites

Before configuring the integration, ensure you have:

- Familiarity with [InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/), which EMQX uses when writing data to Timestream for InfluxDB.
- Understanding of EMQX data integration [rules](./rules.md) and how the rule engine transforms and routes MQTT messages.
- Basic knowledge of EMQX [data integration](./data-bridges.md), including how Sinks are configured and triggered.

### Prepare Amazon Timestream for InfluxDB

To enable EMQX to send data to your Timestream for InfluxDB instance, complete the following preparation steps in AWS.

::: tip Prerequisite

Ensure you have an AWS account with permissions to create and manage Timestream for InfluxDB resources.

:::

#### Create a Timestream for InfluxDB DB Instance

Refer to the AWS official document for detailed instructions: [Create an InfluxDB DB Instance](https://docs.aws.amazon.com/timestream/latest/developerguide/timestream-for-influx-getting-started-creating-db-instance.html#timestream-for-influx-getting-started-creating-db-instance-step2).

After creation, AWS will assign a unique DB instance endpoint, such as:

```
c5vasdqn0b-3ksj4dla5nfjhi.timestream-influxdb.us-east-1.on.aws
```

This endpoint will be required later when configuring the EMQX Connector.

#### Configure Network and Security Groups

To enable EMQX to connect to your Timestream for InfluxDB instance, configure the instance’s VPC security group to permit incoming TCP connections on port 8086 from the network where EMQX is deployed. Use the following settings:

- **Protocol**: TCP
- **Port**: 8086 (the InfluxDB API port used by Timestream for InfluxDB)
- **Source**: The IP address range or security group corresponding to your EMQX deployment environment.

If EMQX is deployed in the same VPC as the Timestream for InfluxDB instance, the connection can occur through private network routes defined within the VPC. If EMQX runs outside AWS, ensure that the security group permits connections from EMQX’s external network. Additionally, verify that no outbound firewall rules in your environment block HTTPS/TCP 8086 traffic from EMQX to the Timestream endpoint.

For more details about connection requirements and security considerations, see the AWS documentation: [Connecting to an Amazon Timestream for InfluxDB DB instance](https://docs.aws.amazon.com/timestream/latest/developerguide/timestream-for-influx-db-connecting.html).

### Obtain InfluxDB Token, Organization, and Bucket

1. Open the **InfluxUI** using the DB instance endpoint:

   ```
   https://<endpoint>:8086
   ```

   > If your DB instance is not publicly accessible, you must access the InfluxUI from a host within the same VPC (for example, via a bastion host or SSM port forwarding). See the [AWS documentation](https://docs.aws.amazon.com/timestream/latest/developerguide/timestream-for-influx-getting-started-creating-db-instance.html) for details.

2. Log in using the master user credentials created with the DB instance.

3. Generate or retrieve a personal access token with write permissions to the target bucket.

   This is the token EMQX uses to authenticate with Timestream for InfluxDB.

   ::: tip Note

   Newly created tokens are shown only once. Be sure to copy and save them.

   :::

4. Confirm the **Organization** and **Bucket** values configured for your instance. These values must match exactly when configuring EMQX.

#### Required Connection Parameters

When configuring the Amazon Timestream for InfluxDB Connector in EMQX, you will need the following parameters:

| Parameter        | Description                                                  |
| ---------------- | ------------------------------------------------------------ |
| **Endpoint**     | The host of your DB instance (AWS-generated endpoint), e.g. `xxxxxxx-yyyyyyyy.timestream-influxdb.<region>.on.aws`. |
| **Port**         | Always **8086**, the InfluxDB API port.                      |
| **Organization** | The InfluxDB organization name created in your DB instance.  |
| **Bucket**       | The bucket where EMQX writes telemetry data.                 |
| **Token**        | InfluxDB API token (operator token or personal access token). |

## Create a Connector

This section demonstrates how to create a Connector to connect the Sink to the AWS Timestream for InfluxDB DB instance.

1. Enter the EMQX Dashboard and click **Integration** -> **Connectors**.

2. Click **Create** in the top right corner of the page.

3. On the **Create Connector** page, select **Amazon Timestream** from the **Data Persistence** type, and click **Next**.

4. In the **Configuration** step, configure the following fields:
   - **Connector Name**: A name starting with a letter or number; letters, numbers, hyphens, and underscores are allowed.
      Example: `my_timestream`.
   
   - **Server Host**: Enter the endpoint and port of your Timestream for InfluxDB instance, for example:
     
     ```
     <instance-endpoint>:8086
     ```
     
   - **Type <!-- field name not decided -->**: `Timestream for InfluxDB`.
     
   - **Token**, **Organization**, and **Bucket**: Provide the personal access token, organization name, and bucket name collected earlier in [Obtain InfluxDB Token, Organization, and Bucket](#obtain-influxdb-token-organization-and-bucket). These values must match your InfluxDB configuration exactly.
     
   - **TLS** (optional): Enable TLS if your Timestream for InfluxDB endpoint requires HTTPS (recommended). For detailed information on TLS connection options, see [TLS for External Resource Access](../network/overview.md#enabling-tls-for-external-resource-access).
   
5. Before clicking **Create**, you can click **Test Connectivity** to test if the connector can connect to the Timestream InfluxDB DB instance.

6. Click the **Create** button at the bottom to complete the creation of the connector. In the pop-up dialog, you can click **Back to Connector List** or click **Create Rule** to continue creating rules and Sink to specify the data to be forwarded to InfluxDB. For detailed steps, see [Create a Rule with Amazon Timestream Sink](#create-a-rule-with-amazon-timestream-sink).

## Create a Rule with Amazon Timestream Sink

This section demonstrates how to create a rule in EMQX to process messages from the source MQTT topic `t/#`  and send the processed results through a configured Sink to AWS Timestream for InfluxDB. 

1. Go to EMQX Dashboard, and click **Integration** -> **Rules** from the left navigation menu.

2. Click **Create** on the top right corner of the page.

3. On the Create Rule page, enter `my_rule` as the rule ID.

4. Set the rules in the **SQL Editor**, for example, if you want to save the MQTT messages of the topic `t/#`  to Timestream for InfluxDB, you can use the SQL syntax below. 

   ::: tip

   If you want to specify your own SQL syntax, make sure that the fields selected (in the `SELECT` part) include all variables in the data format specified in the later configured Sink.

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   Note: If you are a beginner user, click **SQL Examples** and **Enable Test** to learn and test the SQL rule. 

5. Click the + **Add Action** button to define an action that the rule will trigger. With this action, EMQX sends the data processed by the rule to Timestream for InfluxDB. 

6. Select `Amazon Timestream` from the **Type of Action** dropdown list. Keep the **Action** dropdown with the default `Create Action` value. You can also select a Sink if you have created one. This demonstration will create a new Sink.

7. Enter the name and description of the Sink in the form below.

8. From the **Connector** dropdown box, select the `my_timestream` created before. You can also create a new Connector by clicking the button next to the dropdown box. For the configuration parameters, see [Create a Connector](#create-a-connector).

9. Specify the **Time Precision**: Select `millisecond` by default. 

10. Select **Data Format** as `JSON` or `Line Protocol` for how data should be parsed and written into InfluxDB.

    - For JSON format, define data parsing method, including **Measurement**, **Timestamp**, **Fields,** and **Tags**. Note: All key values can be variables or placeholders, and you can also follow the [InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/) to set them. The **Fields** field supports batch setting via a CSV file; for details, refer to [Batch Setting](#batch-setting).
    - For Line Protocol format, specify a text-based format that provides the measurement, tag set, field set, timestamp of a data point, and placeholder supported according to the [InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) syntax.

    ::: tip

    - To write a signed integer type value to InfluxDB 1.x or 2.x, add `i` as the type identifier after the placeholder, for example, `${payload.int}i`. See also [InfluxDB 1.8 write integer value](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb).
    - To write an unsigned integer type value to InfluxDB 1.x or 2.x, add `u` as the type identifier after the placeholder, for example, `${payload.int}u`. See also [InfluxDB 1.8 write integer value](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb).

    :::

11. **Fallback Actions (Optional)**: If you want to improve reliability in case of message delivery failure, you can define one or more fallback actions. These actions will be triggered if the primary Sink fails to process a message. See [Fallback Actions](./data-bridges.md#fallback-actions) for more details.

12. **Advanced settings (optional)**:  See [Advanced Configurations](#advanced-configurations).

13. Before clicking **Create**, you can click **Test Connectivity** to test if the Sink can be connected to the InfluxDB server.

14. Click **Create** to complete the Sink creation. Back on the **Create Rule** page, you will see the new Sink appear under the **Action Outputs** tab.

15. On the **Create Rule** page, verify the configured information. Click the **Create** button to generate the rule.

Now you have successfully created the rule and you can see the new rule appear on the **Rule** page. Click the **Actions(Sink)** tab, you can see the new Amazon Timestream Sink.

You can also click **Integration** -> **Flow Designer** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to Amazon Timestream after parsing by the rule  `my_rule`.

### Batch Setting

In InfluxDB, a data entry typically includes hundreds of fields, making the setup of data formats a challenging task. To address this, EMQX offers a feature for batch setting of fields.

When setting data formats via JSON, you can use the batch setting feature to import key-value pairs of fields from a CSV file.

1. Click the **Batch Setting** button in the **Fields** table to open the **Import Batch Setting** popup.

2. Follow the instructions to first download the batch setting template file, then fill in the key-value pairs of Fields in the template file. The default template file content is as follows:

   | Field  | Value              | Remarks (Optional)                                           |
   | ------ | ------------------ | ------------------------------------------------------------ |
   | temp   | ${payload.temp}    |                                                              |
   | hum    | ${payload.hum}     |                                                              |
   | precip | ${payload.precip}i | Append an i to the field value to tell InfluxDB to store the number as an integer. |

   - **Field**: Field key, supports constants or ${var} format placeholders.
   - **Value**: Field value, supports constants or placeholders, can append type identifiers according to the line protocol.
   - **Remarks**: Used only for notes within the CSV file, cannot be imported into EMQX.

   Note that the data in the CSV file for batch setting should not exceed 2048 rows.

3. Save the filled template file and upload it to the **Import Batch Setting** popup, then click **Import** to complete the batch setting.

4. After importing, you can further adjust the key-value pairs of fields in the **Fields** setting table.

## Test the Rule

Use MQTTX  to send a message to topic  `t/1`  to trigger an online/offline event.

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello Timestream for InfluxDB" }'
```

Check the running status of the Sink, there should be one new incoming and one new outgoing message.

<!-- Check --> In the InfluxDB UI, you can confirm whether the message is written into the Timestrem for InfluxDB via the **Data Explorer** window.

## Advanced Configurations

This section delves deeper into the advanced configuration options available for the Amazon Timestream Connector and Sink. When configuring the Connector and Sink in the Dashboard, navigate to **Advanced Settings** to tailor the following parameters to meet your specific needs.

| **Fields**            | **Descriptions**                                             | **Recommended Value** |
| --------------------- | ------------------------------------------------------------ | --------------------- |
| Start Timeout         | Determines the maximum time interval, in seconds, that the Connector will wait for an auto-started resource to reach a healthy state before responding to resource creation requests. This setting helps ensure that the Connector does not proceed with operations until it verifies that the connected resource, such as a database instance in Timestream for InfluxDB, is fully operational and ready to handle data transactions. | `5`                   |
| Buffer Pool Size      | Specifies the number of buffer worker processes that will be allocated for managing data flow in egress-type bridges between EMQX and Timestream for InfluxDB. These worker processes are responsible for temporarily storing and handling data before it is sent to the target service. This setting is particularly relevant for optimizing performance and ensuring smooth data transmission in egress (outbound) scenarios. For Sinks that only deal with ingress (inbound) data flow, this option can be set to "0" as it is not applicable. | `4`                   |
| Request TTL           | The "Request TTL" (Time To Live) configuration setting specifies the maximum duration, in seconds, that a request is considered valid once it enters the buffer. This timer starts ticking from the moment the request is buffered. If the request stays in the buffer for a period exceeding this TTL setting or if it is sent but does not receive a timely response or acknowledgment from Timestream for InfluxDB, the request is deemed to have expired. | `45`                  |
| Health Check Interval | Specifies the time interval, in seconds, at which the Sink will perform automated health checks on the connection to Timestream for InfluxDB. | `15`                  |
| Max Buffer Queue Size | Specifies the maximum number of bytes that can be buffered by each buffer worker in the Amazon Timestream Sink. Buffer workers temporarily store data before it is sent to Timestream for InfluxDB, serving as an intermediary to handle data flow more efficiently. Adjust the value according to your system's performance and data transfer requirements. | `1`                   |
| Max Batch Size        | Specifies the maximum size of data batches that can be transmitted from EMQX to Timestream for InfluxDB in a single transfer operation. By adjusting the size, you can fine-tune the efficiency and performance of data transfer between EMQX and Timestream for InfluxDB.<br />If the "Max Batch Size" is set to `1`, data records are sent individually, without being grouped into batches. | `100`                 |
| Query Mode            | Allows you to choose `asynchronous` or `synchronous` query modes to optimize message transmission based on different requirements. In asynchronous mode, writing to Timestream for InfluxDB does not block the MQTT message publish process. However, this might result in clients receiving messages ahead of their arrival in Timestream for InfluxDB. | `Async`               |
| Inflight Window       | An "in-flight query" refers to a query that has been initiated but has not yet received a response or acknowledgment. This setting controls the maximum number of in-flight queries that can exist simultaneously when the Sink is communicating with Timestream for InfluxDB.<br/>When the **Query Mode** is set to `async` (asynchronous), the "Inflight Window" parameter gains special importance. If it is crucial for messages from the same MQTT client to be processed in strict order, you should set this value to 1. | `100`                 |
