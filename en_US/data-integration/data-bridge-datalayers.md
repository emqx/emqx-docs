# Ingest MQTT Data to Datalayers

Datalayers is a multi-modal, hyper-converged database designed for industries such as industrial IoT, IoV, energy, and more. With its powerful data throughput and stable performance, Datalayers is ideal for IoT applications. EMQX currently supports storing messages and data in Datalayers via Sink, facilitating data analysis and visualization.

This page provides a detailed overview of EMQX’s data integration with Datalayers and offers practical guidance on creating a rule and a Sink.

## How It Works

The Datalayers data integration is an out-of-the-box feature in EMQX that allows MQTT messages from devices to be seamlessly forwarded to Datalayers for storage and analysis. By configuring rules and Sinks, users can flexibly route processed MQTT data into Datalayers for further use.

The diagram below illustrates a typical architecture for integrating EMQX and Datalayers in an energy storage scenario:

![MQTT to Datalayers](./assets/mqtt-to-datalayers.jpg)

In this architecture, EMQX handles device connectivity, message transmission, and rule-based processing, while Datalayers takes care of data storage, analysis, and visualization. Together, they form a scalable IoT platform for efficiently collecting and analyzing real-time data on energy consumption.

Starting from EMQX 6.0.0, Datalayers supports Arrow Flight SQL, a high-performance binary communication protocol based on Apache Arrow. Compared to the traditional InfluxDB Line Protocol, Arrow Flight SQL offers more efficient data transfer and stronger support for writing structured data.

::: warning Note

The Arrow Flight driver is implemented in Rust and integrated into the Erlang VM via Native Implemented Function (NIF). This feature is currently experimental and recommended for testing environments only.

:::

The specific workflow is as follows:

1. **Message Publishing and Receiving**: Devices connect to EMQX via MQTT and periodically publish energy-related metrics such as power, current, and voltage. EMQX receives these messages and passes them to its rule engine.

2. **Rules Engine Processing Messages**: EMQX’s built-in rule engine matches incoming messages based on topic patterns and processes the data. This may include transforming payloads, filtering fields, or enriching with contextual information.

3. **Writing to Datalayers**: When a rule is triggered, it executes a Sink action that writes the processed data to Datalayers. Sinks support customizable SQL templates that define how fields are mapped into Datalayers tables and columns. 

   EMQX supports two write methods:

   - InfluxDB Line Protocol
   - Arrow Flight SQL Driver

   The Sink configuration differs depending on the chosen method.

Once energy storage data is written to Datalayers, you can flexibly use supported tools to analyze the data, for example:

- Connect to visualization tools like Grafana to generate charts and display energy storage data.
- Connect to business systems to monitor and alert on the status of energy storage devices.

## Features and Benefits

Datalayers data integration offers the following features and advantages:

- **Efficient Data Processing**: EMQX can handle a large number of IoT device connections and message throughput, while Datalayers excels in data writing, storage, and querying, meeting the data processing needs of IoT scenarios without overwhelming the system.
- **Message Transformation**: Messages can undergo extensive processing and transformation in EMQX rules before being written to Datalayers.
- **Scalability**: Both EMQX and Datalayers have clustering capabilities, allowing for flexible horizontal scaling as business needs grow.
- **Rich Query Capabilities**: Datalayers provides optimized functions, operators, and indexing techniques for efficient querying and analysis of timestamp data, extracting valuable insights from IoT time-series data.
- **Efficient Storage**: Datalayers uses high-compression encoding methods to significantly reduce storage costs. It also allows for customizable data retention periods to avoid unnecessary data occupying storage space.

## Before You Start

This section outlines the necessary preparations before creating a Datalayers Sink in EMQX, including installing Datalayers, creating a database, and defining table structures.

### Prerequisites

- Familiarity with [Rules](./rules.md).
- Familiarity with [Data Integration](./data-bridges.md).
- Familiarize yourself with [InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/) or [Arrow Flight SQL](https://arrow.apache.org/docs/format/FlightSql.html#arrow-flight-sql), depending on the driver type you intend to use for data writing.

### Install and Set Up Datalayers

1. Install and start Datalayers using Docker. For detailed steps, refer to [Install Datalayers](https://docs.datalayers.cn/datalayers/latest/getting-started/docker.html).

   ```bash
   # Start a Datalayers container
   docker run -d --name datalayers -p 8360:8360 -p 8361:8361 datalayers/datalayers:latest
   ```

   - Port `8360` is the default gRPC port for Arrow Flight SQL.
   - Port `8361` is the HTTP port, commonly used for Line Protocol writes and management APIs.

2. After the Datalayers service starts, use the default username and password `admin`/`public` to enter the Datalayers CLI. You can create a database in the Datalayers CLI by following these steps:

   - Access the Datalayers container:

     ```bash
     docker exec -it datalayers bash
     ```
     
   - Enter the Datalayers CLI:

     ```bash
     dlsql -u admin -p public
     ```
     
   - Create a database (e.g., `mqtt`):

     ```sql
     create database mqtt
     ```

4. If you plan to use the Arrow Flight SQL driver, you must create the target table in advance.

   ::: tip Note

   If you are using the InfluxDB Line Protocol, pre-creating tables is not required. Datalayers will automatically create tables based on the `measurement` and field definitions in the incoming line protocol data.

   :::

   For example, use the following SQL to create a table named `t_mqtt_msg`:

   ```sql
   CREATE TABLE IF NOT EXISTS `t_mqtt_msg` (
       time TIMESTAMP(3) NOT NULL,
       msgid STRING NOT NULL,
       sender STRING NOT NULL,
       topic STRING NOT NULL,
       qos INT8 NOT NULL,
       payload STRING,
       arrived TIMESTAMP(3) NOT NULL,
       timestamp key(time)
   ) PARTITION BY HASH (msgid, sender) PARTITIONS 1
   ENGINE = TimeSeries WITH (ttl = '14d');
   ```

## Create a Datalayers Connector

This section demonstrates how to create a connector in EMQX to connect the Sink to the Datalayers server.

The steps below assume that both EMQX and Datalayers are running locally. If you have deployed them in separate or remote environments, make sure to update the connection settings accordingly.

1. Go to the EMQX Dashboard, click **Integration** -> **Connectors**.

2. Click **Create** in the top right corner of the page.

3. On the **Create Connector** page, select **Datalayers**, and then click **Next**.

5. On the **Configuration** page, fill in the connector details:

   - **Connector Name**: Must begin with a letter or number. Only letters, numbers, hyphens, and underscores are allowed. For example: `my_datalayers`.
   - **Description** (Optional): Add a description to help identify the connector later.

   Configure the Datalayers server connection:

   - **Driver Type**:

     - `InfluxDB Line Protocol`: Uses the InfluxDB-compatible line protocol for data ingestion. Table creation is automatic.

     - `Arrow Flight`: Enables high-performance structured data writes using SQL templates. This method is ideal when you require strict schema control and higher write throughput.

       ::: warning Note

       The Arrow Flight driver is implemented in Rust and integrated into the Erlang VM via NIF. This feature is currently experimental and should be evaluated in test environments first.

       :::

   - **Server Host**:

     - Default: `127.0.0.1:8361`
     - If using the `Arrow Flight` driver, communication occurs via gRPC on port `8360`.

   - **Database Name**: The name of the target database in Datalayers, e.g., `mqtt`.

   - **Username / Password**: Provide the credentials for Datalayers access. In this example: `admin` / `public`.

   - **Enable TLS** (Optional): Toggle to enable encrypted connections. When enabled, you can configure certificate paths and validation options. For more details, see [Enable TLS for External Resources](../network/overview.md#tls-for-external-resource-access).

     ::: tip Note

     When using the Arrow Flight SQL protocol, certificate validation cannot be skipped (i.e., `verify_none` is not supported due to library constraints). Ensure the gRPC server’s certificate has a valid Common Name (CN) that matches the server host.

     :::
   
5. If `Arrow Flight` is selected as the driver, an additional option **Enable Prepared Statements** will appear. This determines whether the Sink can use SQL templates for data insertion. It is enabled by default.

6. Before clicking **Create**, you can click **Test Connectivity** to test whether the connector can connect to the Datalayers server.

7. Click the **Create** button at the bottom to complete the connector creation. In the pop-up dialog, you can click **Back to Connector List** or proceed to **Create Rule** to define a rule and Sink for writing data to Datalayers. For detailed steps, see [Create a Datalayers Rule](#create-a-datalayers-rule).

## Create a Datalayers Rule

This section demonstrates how to create a rule in EMQX to process MQTT messages from the source topic `t/#` and send the processed results to Datalayers using a configured Sink.

### Create a Rule with Defined SQL

1. In the EMQX Dashboard, navigate to **Data Integration** -> **Rules** from the left-hand menu.

2. On the **Rules** page, click the **Create** button in the upper right corner.

3. In the rule creation form, enter a Rule ID, for example: `my_rule`.

4. In the **SQL Editor**, define your rule logic. To store MQTT messages published to topic `t/#` in Datalayers, you can use the following SQL:

   ::: tip Note

   If you write a custom SQL rule, make sure that all variables referenced in the Sink template (e.g., `${clientid}`, `${payload.temp}`) are included in the `SELECT` clause of the rule.

   :::

   ```
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   If you are new to SQL in EMQX, you can click **SQL Examples** and **Enable Debug** to explore sample queries and test their output.

   :::

5. Add a Datalayers Sink to the rule to write the processed results into Datalayers.

   - If you are using **InfluxDB Line Protocol**, refer to: [Add an InfluxDB Line Protocol Sink](#add-an-influxdb-line-protocol-sink).
   - If you are using the **Arrow Flight SQL driver**, refer to: [Add an Arrow Flight SQL Sink](#add-an-arrow-flight-sql-sink).

6. On the **Create Rule** page, review your configuration and click **Save** to create the rule.

Once the rule is created, it will appear in the **Rules** list. Click the **Actions (Sink)** tab to view the Datalayers Sink associated with this rule.

You can also go to **Integrations** -> **Flow Designer** to view the topology graph. It will show messages from topic `t/#` being processed by the `my_rule` rule and written to Datalayers.

### Add an InfluxDB Line Protocol Sink

This section demonstrates how to add a Sink to a rule that uses the InfluxDB Line Protocol to write processed data to Datalayers.

1. Click the **Add Action** button on the right side of the rule editor to define an action that is triggered when the rule conditions are met. This action forwards the processed message to Datalayers.

2. In the **Type of Action** dropdown, select `Datalayers`. Keep the **Action** dropdown set to the default `Create Action`. You can also select an existing Datalayers Sink. This example assumes you are creating a new Sink.

3. Enter a name for the Sink, such as `dl_sink_influx`. The name should be a combination of uppercase/lowercase letters and numbers.

4. In the **Connector** dropdown, select a previously created connector configured with the `InfluxDB Line Protocol` driver. If no connector is available, you can create one by clicking the adjacent button. See [Create a Datalayers Connector](#create-a-datalayers-connector).

5. Set the **Time Precision** to milliseconds by default.

6. Define the **Data Format** and content for parsing and writing data to Datalayers. Choose between `JSON` and `Line Protocol`:

   -  **JSON**:

     You must specify the **Measurement**, **Fields**, **Timestamp**, and **Tags**. Keys and values support either constants or variable placeholders (e.g., `${payload.temp}`). Refer to the [InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html) for formatting rules.

     The **Fields** section also supports batch configuration using a CSV file. See [Use CSV to Batch Configure Fields](#use-csv-to-batch-configure-fields).

   - **Line Protocol**: 

     You can define a single line protocol string that includes the Table, Fields, Timestamp, and Tags of the data points using a statement. Keys and values support constants or placeholder variables. Refer to the [InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html) for syntax guidance.

     ::: tip

     Since the data written to Datalayers is fully compatible with the InfluxDB v1 line protocol, you can refer to the [InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/) for setting up the data format.

     For example, to input a signed integer value, add an `i` as a type indicator after the placeholder, such as `${payload.int}i`. See [InfluxDB 1.8 Write Integer Values](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb).

     :::

     Example Line Protocol format:
     
     ```sql
     devices,clientid=${clientid} temp=${payload.temp},hum=${payload.hum},precip=${payload.precip}i ${timestamp}
     ```

7. **Fallback Actions** (Optional): If you want to improve reliability in case of message delivery failure, you can define one or more fallback actions. These actions will be triggered if the primary Sink fails to process a message. See [Fallback Actions](./data-bridges.md#fallback-actions) for more details.

8. Expand **Advanced Settings** and configure advanced options as needed (optional). For more details, refer to [Advanced Settings](#advanced-settings).

9. Before clicking **Create**, you can click **Test Connectivity** to test if the Sink can connect to the Datalayers server.

10. Click **Create** to complete the Sink creation. Back on the **Create Rule** page, you will see the new Sink under the **Action Outputs** tab.

#### Use CSV to Batch Configure Fields

::: tip

This feature is only available for Sinks using the **InfluxDB Line Protocol** with the data format set to `JSON`. It allows you to import field configurations in bulk.

:::

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

### Add an Arrow Flight SQL Sink

This section demonstrates how to add a Sink to a rule using the **Arrow Flight SQL** driver, allowing data to be written into Datalayers using SQL insert statements.

::: warning Note

The Arrow Flight SQL driver is currently experimental. Use with caution in production environments.

:::

1. Click the **Add Action** button on the right to define an action that will be triggered when the rule is matched. This action forwards the processed data to Datalayers.

2. In the **Type of Action** dropdown, select `Datalayers`. Keep the **Action** dropdown set to the default `Create Action`. You can also select an existing Datalayers Sink. This example assumes you are creating a new Sink.

3. Set a **Name** for the Sink, such as `dl_sink_arrow`. It is recommended to use a combination of uppercase/lowercase letters and numbers.

4. In the **Connector** dropdown, select an existing connector configured with the `Arrow Flight` driver.
   If no connector exists, you can create one by clicking the button beside the dropdown. See [Create a Datalayers Connector](#create-a-datalayers-connector) for detailed steps.

5. Configure the **SQL** template that defines how the data should be inserted into the target table.

   ::: tip

   This is a [Preprocessing SQL](./data-bridges.md#prepared-statement) template. Do not wrap field names in quotes, and **do not** include a semicolon `;` at the end of the SQL statement.
   All `${}` placeholders must match the fields selected in your rule SQL.

   :::

   ::: tip

   If you need to insert data into a database other than the one configured in the connector, make sure to specify the target database name explicitly in the SQL template.
   Note that the connector will still check whether the target database exists.
   
   :::
   
   For example:
   
   ```sql
   insert into t_mqtt_msg(time, msgid, sender, topic, qos, payload, arrived) values (${timestamp}, ${id}, ${clientid}, ${topic}, ${qos}, ${payload}, ${timestamp})
   ```
   
6. **Fallback Actions** (Optional): To enhance reliability, you can configure one or more fallback actions. These will be triggered if the Sink fails to process a message. For more information, refer to [Fallback Actions](./data-bridges.md#fallback-actions).

7. Expand **Advanced Settings** to configure additional options as needed. See [Advanced Settings](#advanced-settings) for more details.

8. Before clicking **Create**, use the **Test Connection** button to verify that the Sink can connect to the Datalayers server.

9. Click **Create** to complete the Sink creation. Back on the **Create Rule** page, you will see the new Sink under the **Action Outputs** tab.

## Test the Rule and Sink

After configuring the rule and Sink, you can verify whether data is successfully written to Datalayers by publishing a test MQTT message.

1. Use [MQTTX](https://mqttx.app/) to send a message to the topic `t/1`. This may also trigger session events (e.g., client online/offline):

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "temp": "23.5", "hum": "62", "precip": 2 }'
   ```

   This message will trigger the rule engine and be forwarded to the configured Datalayers Sink. If your rule includes session events, such as client connections or disconnections, those may also be triggered by this operation.

2. Check Sink execution statistics. In the EMQX Dashboard, navigate to the **Rules** page. Locate your rule and switch to the **Actions (Sink)** tab. Confirm that the **Matched** and **Success** count for the target Sink have increased by 1.
   
3. Verify data in Datalayers via CLI.

   Access the Datalayers container and start the CLI tool:

   ```bash
   docker exec -it datalayers bash
   dlsql -u admin -p public
   ```

   Then, run SQL queries based on the data write method you used:

   - If using InfluxDB Line Protocol, the table name defaults to the `measurement` specified in the Sink configuration (e.g., `devices`):
     
     ```sql
     use mqtt
     select * from devices
     ```
     
   - If using Arrow Flight SQL, query the target table that was created beforehand (e.g., `t_mqtt_msg`):
     
     ```sql
     use mqtt
     select * from t_mqtt_msg
     ```

## Advanced Settings

This section delves into advanced configuration options available for Datalayers connectors and Sinks. When configuring connectors and Sinks in the Dashboard, you can expand **Advanced Settings** to adjust the following parameters based on your specific needs.

| Field Name            | Description                                                  | Default  |
| --------------------- | ------------------------------------------------------------ | -------- |
| Buffer Pool Size      | Specifies the number of buffer worker processes. These processes are allocated to manage the data flow between EMQX and the egress-type Sink in Datalayers. They are responsible for temporarily storing and processing data before sending it to the target service. This setting is especially important for optimizing performance and ensuring smooth data transmission in egress scenarios. For bridges that handle only ingress data flow, this option can be set to `0`, as it is not applicable. | `4`      |
| Request TTL           | The "Request TTL" (Time to Live) configuration setting specifies the maximum duration (in seconds) that a request is considered valid once it enters the buffer. This timer starts when the request enters the buffer. If the request stays in the buffer for a period exceeding this TTL setting or if it is sent but does not receive a timely response or acknowledgment in Datalayers, the request is considered expired. | `45`     |
| Health Check Interval | Specifies the time interval (in seconds) for the Sink to perform automatic health checks on its connection with Datalayers. | `15`     |
| Max Buffer Queue Size | Specifies the maximum number of bytes that can be buffered by each buffer worker process in the Datalayers Sink. The buffer worker processes temporarily store data before sending it to Datalayers, acting as intermediaries to handle the data stream more efficiently. Adjust this value based on system performance and data transmission requirements. | `1`      |
| Batch Size            | Specifies the maximum size of data batches transmitted from EMQX to Datalayers in a single transfer operation. By adjusting this size, you can fine-tune the efficiency and performance of data transfer between EMQX and Datalayers.<br />If the "Batch Size" is set to `1`, data records are sent individually, without being grouped into batches. | `100`    |
| Query Mode            | Allows you to choose between `synchronous` or `asynchronous` request modes to optimize message transmission according to different requirements. In asynchronous mode, writing to Datalayers does not block the MQTT message publishing process. However, this may lead to clients receiving messages before they arrive in Datalayers. | `Asynch` |
| Inflight Window       | "Inflight queue requests" refer to requests that have been initiated but have not yet received a response or acknowledgment. This setting controls the maximum number of inflight queue requests that can exist simultaneously during Sink communication with Datalayers. <br/>When **Request Mode** is set to `asynchronous`, the "Inflight Queue Window" parameter becomes particularly important. If strict sequential processing of messages from the same MQTT client is crucial, then this value should be set to `1`. | `100`    |
