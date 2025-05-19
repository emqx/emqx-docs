# Ingest MQTT Data into Amazon S3 Tables

[Amazon S3 Tables](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables.html) is a purpose-built storage solution optimized for analytics workloads. It provides high-performance, scalable, and secure storage for tabular data, such as IoT sensor readings, in the Apache Iceberg format.

EMQX now supports seamless integration with Amazon S3 Tables, enabling efficient storage of MQTT messages into S3 table buckets. This integration allows for flexible and scalable IoT data storage, facilitating advanced analytics and processing using AWS services like Amazon Athena, Amazon Redshift, and Amazon EMR.

This guide provides detailed instructions on configuring EMQX to ingest MQTT data into Amazon S3 Tables, including steps for creating rules and sinks to route data appropriately.

## How It Works

Amazon S3 Tables data integration in EMQX is a ready-to-use feature that can be easily configured for complex business development. In a typical IoT application, EMQX acts as the IoT platform responsible for device connectivity and message transmission, while Amazon S3 Tables serves as the data storage platform, handling message data storage.

![emqx-integration-s3](/Users/emqx/Documents/GitHub/emqx-docs/en_US/data-integration/assets/emqx-integration-s3.jpg)

EMQX utilizes rules engines and Sinks to forward device events and data to Amazon S3 Tables. Applications can read data from Amazon S3 Tables for further data applications. The specific workflow is as follows:

1. **Device Connection to EMQX**: IoT devices trigger an online event upon successfully connecting via the MQTT protocol. The event includes device ID, source IP address, and other property information.
2. **Device Message Publishing and Receiving**: Devices publish telemetry and status data through specific topics. EMQX receives the messages and compares them within the rules engine.
3. **Rules Engine Processing Messages**: The built-in rules engine processes messages and events from specific sources based on topic matching. It matches corresponding rules and processes messages and events, such as data format transformation, filtering specific information, or enriching messages with context information.
4. **Writing to Amazon S3 Tables**: The rule triggers an action to write the message to S3 Tables. Using the Amazon S3 Tables Sink, users can extract data from processing results and send it to S3 Tables. Messages can be stored in text or binary format, or multiple lines of structured data can be aggregated into a single CSV or JSON Lines file, depending on the message content and the Sink configuration.

After events and message data are written to Amazon S3 Tables, you can connect to Amazon S3 Tables to read the data for flexible application development, such as:

- Data archiving: Store device messages as objects in Amazon S3 Tables for long-term preservation to meet compliance requirements or business needs.
- Data analysis: Import data from S3 Tables into analytics services like Snowflake for predictive maintenance, device efficiency evaluation, and other data analysis services.

## Features and Advantages

Using Amazon S3 Tables data integration in EMQX can bring the following features and advantages to your business:

- **Message Transformation**: Messages can undergo extensive processing and transformation in EMQX rules before being written to Amazon S3 Tables, facilitating subsequent storage and use.
- **Flexible Data Operations**: With the S3 Tables Sink, specific fields of data can be conveniently written into Amazon S3 Tables buckets, supporting the dynamic setting of bucket and object keys for flexible data storage.
- **Integrated Business Processes**: The S3 Tables Sink allows device data to be combined with the rich ecosystem applications of Amazon S3 Tables, enabling more business scenarios like data analysis and archiving.
- **Low-Cost Long-Term Storage**: Compared to databases, Amazon S3 Tables offers a highly available, reliable, and cost-effective object storage service, suitable for long-term storage needs.

These features enable you to build efficient, reliable, and scalable IoT applications and benefit from business decisions and optimizations.

## Before You Start

This section introduces the preparations required before creating an Amazon S3 Tables Sink in EMQX.

### Prerequisites

- Understanding of [rules](./rules.md).
- Understanding of [data integration](./data-bridges.md).

### Prepare an S3 Tables Bucket



## Create a Connector

Before adding the S3 Tables Sink, you need to create the corresponding connector.

1. Go to the Dashboard **Integration** -> **Connector** page.
2. Click the **Create** button in the top right corner.
3. Select **S3 Tables** as the connector type and click next.
4. Enter the connector name, a combination of upper and lowercase letters and numbers. Here, enter `my-s3-tables`.
5. Enter the connection information.
   - **Table ARN**:
   - **Access Key ID**:
   - **Secret Access Key**:
   - **Access Method**:
   - **Request Timeout**:
6. Use the default values for the remaining settings.
7. Before clicking **Create**, you can click **Test Connectivity** to test if the connector can connect to the S3 Tables service.
8. Click the **Create** button at the bottom to complete the connector creation.

You have now completed the connector creation and will proceed to create a rule and Sink for specifying the data to be written into the S3 Tables service.

## Create a Rule with Amazon S3 Tables Sink

This section demonstrates how to create a rule in EMQX to process messages from the source MQTT topic `t/#` and write the processed results to the `iot-data` bucket in S3 Tables through the configured Sink.

1. Go to the Dashboard **Integration** -> **Rules** page.

2. Click the **Create** button in the top right corner.

3. Enter the rule ID `my_rule`, and input the following rule SQL in the SQL editor:

   ```sql
   SELECT
     *
   FROM
       "t/#"
   ```

   ::: tip

   If you are new to SQL, you can click **SQL Examples** and **Enable Debug** to learn and test the rule SQL results.

   :::

4. Add an action, select `S3 Tables` from the **Action Type** dropdown list, keep the action dropdown as the default `create action` option, or choose a previously created S3 Tables action from the action dropdown. Here, create a new Sink and add it to the rule.

5. Enter the Sink's name and description.

6. Select the `my-s3-tables` connector created earlier from the connector dropdown. You can also click the create button next to the dropdown to quickly create a new connector in the pop-up box. The required configuration parameters can be found in [Create a Connector](#create-a-connector).

7. Configure the Sink settings:

   - **Namespace**:
   - **Table**:

   - **Max Records**: When the maximum number of records is reached, the aggregation of a single file will be completed and uploaded, resetting the time interval.

   - **Time Interval**: When the time interval is reached, even if the maximum number of records has not been reached, the aggregation of a single file will be completed and uploaded, resetting the maximum number of records.

   - **Min Part Size**:

   - **Max Part Size**:

8. Set the **Object Content**. By default, it is a JSON text format containing all fields. It supports `${var}` format placeholders. Here, enter `${payload}` to indicate using the message body as the object content. In this case, the object's storage format depends on the message body's format, supporting compressed packages, images, or other binary formats.

9. **Fallback Actions (Optional)**: If you want to improve reliability in case of message delivery failure, you can define one or more fallback actions. These actions will be triggered if the primary Sink fails to process a message. See [Fallback Actions](./data-bridges.md#fallback-actions) for more details.

10. Expand **Advanced Settings** and configure the advanced setting options as needed (optional). For more details, refer to [Advanced Settings](#advanced-settings).

11. Use the default values for the remaining settings. Click the **Create** button to complete the Sink creation. After successful creation, the page will return to the rule creation, and the new Sink will be added to the rule actions.

12. Back on the rule creation page, click the **Create** button to complete the entire rule creation process.

You have now successfully created the rule. You can see the newly created rule on the **Rules** page and the new S3 Tables Sink on the **Actions (Sink)** tab.

You can also click **Integration** -> **Flow Designer** to view the topology. The topology visually shows how messages under the topic `t/#` are written into S3 Tables after being parsed by the rule `my_rule`.

## Test the Rule

This section shows how to test the rule configured with the direct upload method.

Use MQTTX to publish a message to the topic `t/1`:

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello S3 Tables" }'
```



## Advanced Settings

This section delves into the advanced configuration options available for the S3 Tables Sink. In the Dashboard, when configuring the Sink, you can expand **Advanced Settings** to adjust the following parameters based on your specific needs.

| Field Name                | Description                                                  | Default Value  |
| ------------------------- | ------------------------------------------------------------ | -------------- |
| **Buffer Pool Size**      | Specifies the number of buffer worker processes, which are allocated to manage the data flow between EMQX and S3 Tables. These workers temporarily store and process data before sending it to the target service, crucial for optimizing performance and ensuring smooth data transmission. | `16`           |
| **Request TTL**           | The "Request TTL" (Time To Live) configuration setting specifies the maximum duration, in seconds, that a request is considered valid once it enters the buffer. This timer starts ticking from the moment the request is buffered. If the request stays in the buffer for a period exceeding this TTL setting or if it is sent but does not receive a timely response or acknowledgment from S3 Tables, the request is deemed to have expired. |                |
| **Health Check Interval** | Specifies the time interval (in seconds) for the Sink to perform automatic health checks on its connection with S3 Tables. | `15`           |
| **Max Buffer Queue Size** | Specifies the maximum number of bytes that can be buffered by each buffer worker process in the S3 Tables Sink. The buffer workers temporarily store data before sending it to S3 Tables, acting as intermediaries to handle the data stream more efficiently. Adjust this value based on system performance and data transmission requirements. | `256`          |
| **Query Mode**            | Allows you to choose between `synchronous` or `asynchronous` request modes to optimize message transmission according to different requirements. In asynchronous mode, writing to S3 Tables does not block the MQTT message publishing process. However, this may lead to clients receiving messages before they arrive at S3 Tables. | `Asynchronous` |
| **In-flight  Window**     | "In-flight queue requests" refer to requests that have been initiated but have not yet received a response or acknowledgment. This setting controls the maximum number of in-flight queue requests that can exist simultaneously during Sink communication with S3 Tables. <br/>When **Request Mode** is set to `asynchronous`, the "Request In-flight Queue Window" parameter becomes particularly important. If strict sequential processing of messages from the same MQTT client is crucial, then this value should be set to `1`. | `100`          |
| **Min Part Size**         | The minimum chunk size for part uploads after aggregation is complete. The data to be uploaded will accumulate in memory until it reaches this size. | `5MB`          |
| **Max Part Size**         | The maximum chunk size for part uploads. The S3 Tables Sink will not attempt to upload parts exceeding this size. | `5GB`          |