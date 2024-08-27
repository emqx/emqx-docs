# Ingest MQTT Data to Datalayers

::: tip

Datalayers data integration is an EMQX Enterprise feature.

:::

Datalayers is a multi-modal, hyper-converged database designed for industries such as industrial IoT, IoV, energy, and more. With its powerful data throughput and stable performance, Datalayers is ideal for IoT applications. EMQX currently supports storing messages and data in Datalayers via Sink, facilitating data analysis and visualization.

This page provides a detailed overview of EMQX’s data integration with Datalayers and offers practical guidance on creating rules and Sinks.

## How It Works

The Datalayers data integration is an out-of-the-box feature in EMQX that combines EMQX's device connectivity and message transmission capabilities with Datalayers' data storage and analysis functionalities. Through simple configuration, seamless MQTT data integration can be achieved. EMQX uses the rules engine and Sinks to forward device data to Datalayers for storage and analysis. After analyzing the data, Datalayers generates reports, charts, and other data analysis results that are displayed to users through Datalayers' visualization tools.

The diagram below shows a typical architecture for data integration between EMQX and Datalayers in an energy storage scenario.

![MQTT to Datalayers](./assets/mqtt-to-datalayers.jpg)

EMQX and Datalayers provide a scalable IoT platform for efficiently collecting and analyzing energy consumption data in real-time. In this architecture, EMQX serves as the IoT platform responsible for device connectivity, message transmission, and data routing, while Datalayers acts as the data storage and analysis platform, responsible for data storage and analysis. The specific workflow is as follows:

1. **Message Publishing and Receiving**: After successfully connecting via the MQTT protocol, energy storage devices periodically publish energy consumption data, including information on power, input, and output. Once EMQX receives these messages, they are matched in the rules engine.
2. **Rules Engine Processing Messages**: The built-in rules engine can process messages from specific sources based on topic matching. When a message arrives, it goes through the rules engine, which matches the corresponding rules and processes the message data, such as transforming data formats, filtering out specific information, or enriching the message with contextual information.
3. **Writing to Datalayers**: The rules defined in the rules engine trigger actions to write messages to Datalayers. The Datalayers Sink provides SQL templates that flexibly define the data format to be written, allowing specific fields from the messages to be stored in the corresponding tables and columns in Datalayers.

Once energy storage data is written to Datalayers, you can flexibly use the [line protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html) to analyze the data, for example:

- Connect to visualization tools like Grafana to generate charts and display energy storage data.
- Connect to business systems to monitor and alert on the status of energy storage devices.

## Features and Advantages

Datalayers data integration offers the following features and advantages:

- **Efficient Data Processing**: EMQX can handle a large number of IoT device connections and message throughput, while Datalayers excels in data writing, storage, and querying, meeting the data processing needs of IoT scenarios without overwhelming the system.
- **Message Transformation**: Messages can undergo extensive processing and transformation in EMQX rules before being written to Datalayers.
- **Scalability**: Both EMQX and Datalayers have clustering capabilities, allowing for flexible horizontal scaling as business needs grow.
- **Rich Query Capabilities**: Datalayers provides optimized functions, operators, and indexing techniques for efficient querying and analysis of timestamp data, extracting valuable insights from IoT time-series data.
- **Efficient Storage**: Datalayers uses high-compression encoding methods to significantly reduce storage costs. It also allows for customizable data retention periods to avoid unnecessary data occupying storage space.

## Before You Start

This section covers the preparations required before creating a Datalayers Sink in EMQX, including installing and setting up Datalayers.

### Prerequisites

- Familiarity with [Rules](./rules.md).
- Familiarity with [Data Integration](./data-bridges.md).
- Understand the [Datalayers Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html), which is used for data writing in the Datalayers Sink.

### Install and Set Up Datalayers

1. Install and start Datalayers using Docker. For detailed steps, refer to [Install Datalayers](https://docs.datalayers.cn/datalayers/latest/getting-started/docker.html).

   ```bash
   # Start a Datalayers container
   docker run --name datalayers -p 8361:8361 datalayers/datalayers:nightly
   ```

2. After the Datalayers service starts, use the default username and password `admin`/`public` to enter the Datalayers CLI. You can create a database in the Datalayers CLI by following these steps:

   - Access the Datalayers container:

     ```bash
     docker exec -it datalayers bash
     ```
     
   - Enter the Datalayers CLI:

     ```bash
     dlsql -u admin -p public
     ```
     
   - Create a database:
   
     ```sql
     create database mqtt
     ```

## Create a Connector

This section demonstrates how to create a connector to connect the Sink to the Datalayers server.

The following steps assume that EMQX and Datalayers are both running locally. If you are running EMQX and Datalayers remotely, adjust the settings accordingly.

1. Go to the EMQX Dashboard, click **Integration** -> **Connectors**.
2. Click **Create** in the top right corner of the page.
3. On the **Create Connector** page, select **Datalayers**, and then click **Next**.
4. In the **Configuration** step, configure the following information:
   - Enter the connector name, using a combination of uppercase/lowercase letters and numbers, for example: `my_datalayers`.
   - Enter the connection information for the Datalayers server:
     - Server address: `127.0.0.1:8361`.
     - Complete the **Username**, **Password**, and **Database** settings as configured in [Install and Set Up Datalayers](#install-and-set-up-datalayers).
   - Set whether to enable TLS. For detailed information about TLS connection options, refer to [Enable TLS Encryption to Access External Resources](../network/overview.md#tls-for-external-resource-access).
5. Before clicking **Create**, you can click **Test Connectivity** to test whether the connector can connect to the Datalayers server.
6. Click the **Create** button at the bottom to complete the connector creation. In the pop-up dialog, you can click **Back to Connector List** or click **Create Rule** to continue creating rules and Sinks to specify the data to be forwarded to Datalayers. For detailed steps, see [Create Datalayers Sink Rules](#create-datalayers-sink-rules).

## Create a Rule with Datalayers Sink

This section demonstrates how to create a rule in EMQX to process messages from the source MQTT topic `t/#` and send the processed results to Datalayers through the configured Sink.

1. Click **Data Integration** -> **Rules** in the left navigation menu of the Dashboard.

2. Click the **Create** button in the top right corner of the Rules page.

3. Enter the rule ID `my_rule`.

4. In the SQL editor, input the rule to store MQTT messages from the `t/#` topic to Datalayers, such as the following SQL statement:

   ::: tip Note

   If you want to specify your own SQL rules, make sure that the fields selected by the rule (SELECT part) contain all the variables included in the data write format specified in the Sink for Datalayers.

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```
   
   ::: tip

   If you are new to SQL, you can click **SQL Examples** and **Enable Debug** to learn and test the results of the rule SQL.

   :::

5. Click the **Add Action** button on the right to specify an action for the rule when it is triggered. Through this action, EMQX will forward the data processed by the rule to Datalayers.

6. In the **Action** dropdown list, select `Datalayers` and keep the **Action** dropdown set to the default `Create Action`. You can also select an existing Datalayers Sink. In this demonstration, a new Sink will be created.

7. Enter a name for the Sink. The name should be a combination of uppercase/lowercase letters and numbers.

8. Select the previously created `my_datalayers` from the **Connector** dropdown list. You can also create a new connector by clicking the button next to the dropdown. For configuration parameters, see [Create a Connector](#create-a-connector).

9. Set the **Time Precision** to milliseconds by default.

10. Define the data parsing, and specify the **Data Format** and content to be parsed and written to Datalayers. The `JSON` and `InfluxDB Line Protocol` formats are supported.

    - For JSON format, define data parsing method, including **Measurement**, **Timestamp**, **Fields,** and **Tags**. Note: All key values can be variables or placeholders, and you can also follow the [InfluxDB line protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html) to set them. The **Fields** field supports batch setting via a CSV file; for details, refer to [Batch Setting](#batch-setting).

    - For the Line Protocol format, specify the Table, Fields, Timestamp, and Tags of the data points using a statement. Keys and values support constants or placeholder variables and can be set according to the [InfluxDB line protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html).

      ::: tip

      Since the data written to Datalayers is fully compatible with the InfluxDB v1 line protocol, you can refer to the [InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/) for setting up the data format.

      For example, to input a signed integer value, add an `i` as a type indicator after the placeholder, such as `${payload.int}i`. See [InfluxDB 1.8 Write Integer Values](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb).

      :::

      Here, we can use the Line Protocol format and set it up as:

      ```sql
      devices,clientid=${clientid} temp=${payload.temp},hum=${payload.hum},precip=${payload.precip}i ${timestamp}
      ```

11. Expand **Advanced Settings** and configure advanced options as needed (optional). For more details, refer to [Advanced Settings](#advanced-settings).

12. Before clicking **Create**, you can click **Test Connectivity** to test if the Sink can connect to the Datalayers server.

13. Click **Create** to complete the Sink creation. Back on the **Create Rule** page, you will see the new Sink under the **Action Outputs** tab.

14. On the **Create Rule** page, verify the configured information. Click the **Create** button to generate the rule.

Now you have successfully created the rule. You can see the new rule on the **Rules** page. Click the **Actions (Sink)** tab to see the new Datalayers Sink.

You can also click **Integration** -> **Flow Designer** to view the topology. You can see that messages under the `t/#` topic are processed by the rule named `my_rule`, and the results are stored in Datalayers.

### Batch Settings

In Datalayers, a data entry typically contains hundreds of fields, making data format configuration challenging. To solve this issue, EMQX provides a batch field setting feature.

When setting the data format using JSON, you can use the batch settings feature to import key-value pairs of fields from a CSV file.

1. Click the **Batch Settings** button in the **Fields** table to open the **Import Batch Settings** popup.

2. Follow the instructions to download the batch settings template file, then fill in the Fields key-value pairs in the template file. The default template file content is as follows:

   | Field  | Value              | Remarks (Optional)                                           |
   | ------ | ------------------ | ------------------------------------------------------------ |
   | temp   | ${payload.temp}    |                                                              |
   | hum    | ${payload.hum}     |                                                              |
   | precip | ${payload.precip}i | Append `i` after the field value, and Datalayers will store the value as an integer type. |

   - **Field**: Field key, supporting constants or placeholders in the `${var}` format.
   - **Value**: Field value, supporting constants or placeholders, and type identifiers can be appended according to the line protocol.
   - **Remarks**: Only for comments on fields within the CSV file, cannot be imported into EMQX.

   Note that the batch settings CSV file cannot exceed 2048 rows.

3. Save the filled template file and upload it to the **Import Batch Settings** popup, then click **Import** to complete the batch settings.

4. After importing, you can further adjust the field key-value pairs in the **Fields** setting table.

## Test Rules and Sinks

Use MQTTX to publish a message to the `t/1` topic. This operation will also trigger online and offline events:

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello Datalayers" }'
```

Check the running statistics of both Sinks. The hit and successful send counts should each increase by 1.

Go to the Datalayers CLI to check if the data has been successfully written to the database by executing the following commands:

1. Enter the Datalayers console:

   ```bash
   docker exec -it datalayers bash
   dlsql -u admin -p public
   ```

2. Execute an SQL query to view the data:

   ```sql
   use mqtt;
   select * from devices;
   ```

## Advanced Settings

This section delves into advanced configuration options available for Datalayers connectors and Sinks. When configuring connectors and Sinks in the Dashboard, you can expand **Advanced Settings** to adjust the following parameters based on your specific needs.

| Field Name             | Description                                                  | Default        |
| ---------------------- | ------------------------------------------------------------ | -------------- |
| Startup Timeout        | Specifies the maximum time interval (in seconds) for the connector to wait for an auto-started resource to reach a healthy state before responding to resource creation requests. This setting helps ensure that the connector does not proceed with operations until verifying that the connected resource (such as a database instance in Datalayers) is fully operational and ready to handle data transactions. | `5`            |
| Buffer Pool Size       | Specifies the number of buffer worker processes. These processes are allocated to manage the data flow between EMQX and the egress-type Sink in Datalayers. They are responsible for temporarily storing and processing data before sending it to the target service. This setting is especially important for optimizing performance and ensuring smooth data transmission in egress scenarios. For bridges that handle only ingress data flow, this option can be set to `0`, as it is not applicable. | `4`            |
| Request Timeout        | The "Request TTL" (Time to Live) configuration setting specifies the maximum duration (in seconds) that a request is considered valid once it enters the buffer. This timer starts when the request enters the buffer. If the request stays in the buffer for a period exceeding this TTL setting or if it is sent but does not receive a timely response or acknowledgment in Datalayers, the request is considered expired. | `45`           |
| Health Check Interval  | Specifies the time interval (in seconds) for the Sink to perform automatic health checks on its connection with Datalayers. | `15`           |
| Max Buffer Queue Size  | Specifies the maximum number of bytes that can be buffered by each buffer worker process in the Datalayers Sink. The buffer worker processes temporarily store data before sending it to Datalayers, acting as intermediaries to handle the data stream more efficiently. Adjust this value based on system performance and data transmission requirements. | `1`            |
| Max Batch Request Size | Specifies the maximum size of data batches transmitted from EMQX to Datalayers in a single transfer operation. By adjusting this size, you can fine-tune the efficiency and performance of data transfer between EMQX and Datalayers.<br />If the "Max Batch Request Size" is set to `1`, data records are sent individually, without being grouped into batches. | `100`          |
| Request Mode           | Allows you to choose between `synchronous` or `asynchronous` request modes to optimize message transmission according to different requirements. In asynchronous mode, writing to Datalayers does not block the MQTT message publishing process. However, this may lead to clients receiving messages before they arrive in Datalayers. | `Asynchronous` |
| Inflight Queue Window  | "Inflight queue requests" refer to requests that have been initiated but have not yet received a response or acknowledgment. This setting controls the maximum number of inflight queue requests that can exist simultaneously during Sink communication with Datalayers. <br/>When **Request Mode** is set to `asynchronous`, the "Inflight Queue Window" parameter becomes particularly important. If strict sequential processing of messages from the same MQTT client is crucial, then this value should be set to `1`. | `100`          |