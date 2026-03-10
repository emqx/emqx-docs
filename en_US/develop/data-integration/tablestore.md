# Ingest MQTT Data into Tablestore

::: tip

The Tablestore data integration is an EMQX Enterprise edition feature. 
:::

[Tablestore](https://www.alibabacloud.com/en/product/table-store/pricing?spm=a3c0i.29367734.6737026690.8.78847d3fcEhuVv&_p_lc=1) is a scalable, serverless database optimized for IoT scenarios. It offers a one-stop solution called IoTstore for managing time-series, structured, and semi-structured data. It is ideal for scenarios such as IoT, vehicle networking, risk control, messaging, and recommendation systems. Tablestore provides cost-effective, high-performance data storage, with millisecond-level queries, retrieval, and flexible data analysis capabilities. EMQX seamlessly integrates with Tablestore Cloud, Tablestore OSS, and Tablestore Enterprise, enabling efficient data management for IoT use cases.

## How It Works

Tablestore data integration in EMQX seamlessly combines EMQX's real-time data capturing and transmission capabilities with Tablestore's high-performance data storage and analysis functionality. By leveraging the built-in [rule engine](./rules.md), this integration simplifies the process of ingesting and storing data from EMQX into Tablestore, eliminating the need for complex coding. EMQX forwards IoT device data to Tablestore through its rule engine and Sink, enabling efficient storage and analysis.

Once the data is stored, Tablestore provides powerful tools for analysis, including the ability to generate reports, charts, and other visualizations, which are then presented to users via Tablestore’s visualization features.

The diagram below illustrates the typical data integration architecture between EMQX and Tablestore in an energy storage scenario.

![MQTT to Tablestore](./assets/mqtt-to-tablestore.png)

EMQX and Tablestore provide an extensible IoT platform for efficiently collecting and analyzing energy consumption data in real-time. In this architecture, EMQX serves as the IoT platform, handling device access, message transmission, and data routing, while Tablestore serves as the data storage and analysis platform, responsible for data storage and analysis functions. The workflow is as follows:

1. **Message publication and reception**: Energy storage devices and Industrial IoT devices establish successful connections to EMQX through the MQTT protocol and regularly publish energy consumption data using the MQTT protocol, including information such as power consumption, input/output power, etc. When EMQX receives these messages, it initiates the matching process within its rules engine.  
2. **Message data processing**: Using the built-in rule engine, messages from specific sources can be processed based on topic matching. When a message arrives, it passes through the rule engine, which matches it with the corresponding rule and processes the message data, such as transforming data formats, filtering specific information, or enriching messages with contextual information.
3. **Data ingestion into Tablestore**: Rules defined in the rule engine trigger the operation of writing messages to Tablestore. The Tablestore Sink provides configurable fields that allow flexible definitions of the data format to be written, mapping specific fields from the message to the corresponding measurement and field in Tablestore.

After energy consumption data is written to Tablestore, you can analyze the data, for example:

- Connect to visualization tools like Grafana to generate charts based on the data, displaying energy storage data.
- Connect to business systems for monitoring and alerting on the status of energy storage devices.

## Features and Benefits

The Tablestore data integration offers the following features and advantages:

- **Efficient Data Processing**: EMQX can handle a massive number of IoT device connections and message throughput, while Tablestore excels in data writing, storage, and querying. It provides outstanding performance to meet the data processing needs of IoT scenarios without overburdening the system.
- **Message Transformation**: Messages can undergo extensive processing and transformation through EMQX rules before being written into Tablestore.
- **Scalability**: Both EMQX and Tablestore are capable of cluster scaling, allowing flexible horizontal expansion of clusters as business needs grow.
- **Rich Query Capabilities**: Tablestore offers optimized functions, operators, and indexing techniques, enabling efficient querying and analysis of timestamped data, and accurately extracting valuable insights from IoT time-series data.
- **Efficient Storage**: Tablestore uses encoding methods with high compression ratios, significantly reducing storage costs. It also allows customization of storage durations for different data types to avoid unnecessary data occupying storage space.

## Before You Start

This section describes the preparations you need to complete before you start creating the Tablestore data integration, including creating a database instance, creating and managing a time series table.

::: tip

Currently, the data integration with Tablestore only supports the TimeSeries model. Therefore, the following steps focus on the TimeSeries model for data integration.

:::

### Prerequisites

Before you proceed, make sure you have the following:

- Understanding of EMQX data integration [rules](./rules.md).
- Knowledge of how [data integration](./data-bridges.md) works in EMQX.

### Create a Time Series Table

1. Log in to the [Tablestore console](https://account.alibabacloud.com/login/login.htm?spm=5176.12901015-2.0.0.1a364b84fgwsH6).
2. Create a time series model instance. Provide a name for the instance, such as `emqx-demo`. For detailed instructions on creating an instance, refer to the [Tablestore official documentation](https://www.alibabacloud.com/help/en/tablestore/getting-started/use-timeseries-model-in-tablestore-console?spm=a2c63.p38356.help-menu-27278.d_1_2_0.6d7d5e92tyvDzj#section-247-wkm-e7a).
3. Navigate to the **Instance Management** page.
4. In the **Instance Details** tab, select **Time Series Tables** and click the **Create Time Series Table** button.
5. Configure the time series table information, providing a name for the table, such as `timeseries_demo_with_data`. Click **Confirm**.

![img](./assets/tablestore_instance_manage.png)

### Manage a Time Series Table

To manage the Time Series Table created earlier, click on the table name to enter the **Manage Time Series Table** page. From there, you can follow these steps based on your business requirements:

1. Click the **Query Data** tab.

2. Click **Add Time Series**.

   ::: tip

   This step is optional. If the Time Series table does not already exist, Tablestore will automatically create one when data is written. Therefore, this example does not demonstrate any manual operation on the Time Series.

   :::

![img](./assets/tablestore_timeline_mamge.png)

## Create a Connector

This section demonstrates how to create a Connector to connect the Sink to the Tablestore server.

The following steps assume that you run both EMQX and Tablestore on the local machine. If you have Tablestore and EMQX running remotely, adjust the settings accordingly.

1. Enter the EMQX Dashboard and click **Integration** -> **Connectors**.
2. Click **Create** in the top right corner of the page.
3. On the **Create Connector** page, select **Tablestore** and then click **Next**.
4. In the **Configuration** step, configure the following information:
   - Enter the connector name, which should be a combination of upper and lower case letters and numbers. Example: `my_tablestore`.
   - Enter the Tablestore server connection information:
     - **Endpoint**: Enter the access URL for your Tablestore instance. This should be the address where your Tablestore service is hosted, and you can find it on the Instance details page in your Tablestore console. Enter the URL according to your deployment method, for example `https://emqx-demo.cn-hangzhou.ots.aliyuncs.com` for public network.
     - **Instance Name**: The name of the Tablestore instance to connect to. In this example, use the name you created before: `emqx-demo`.
     - **Access Key ID**: The Access Key ID used to authenticate with Tablestore. This key is issued by Alibaba Cloud for accessing Tablestore resources securely.
     - **Access Key Secret**: The Access Key Secret used for authentication, associated with the Access Key ID.
     - **Storage Model Type**: Currently only `TimeSeries` is supported.
   - Configure TLS Parameters. Tablestore uses HTTPS endpoints, so TLS is enabled by default and no additional TLS parameter configuration is required. For detailed information on TLS connection options, see [TLS for External Resource Access](../network/overview.md#enabling-tls-for-external-resource-access).
5. Before clicking **Create**, you can click **Test Connectivity** to test if the connector can connect to the Tablestore server.
6. Click the **Create** button at the bottom to complete the creation of the connector. In the pop-up dialog, you can click **Back to Connector List** or click **Create Rule** to continue creating rules and Sink to specify the data to be forwarded to Tablestore. For detailed steps, see [Create a Rule with Tablestore Sink](#create-a-rule-with-tablestore-sink).

## Create a Rule with Tablestore Sink

This section demonstrates how to create a rule in EMQX to process messages from the source MQTT topic `t/#`  and send the processed results through a configured Sink to Tablestore. 

1. Go to EMQX Dashboard, and click **Integration** -> **Rules** from the left navigation menu.

2. Click **Create** on the top right corner of the page.

3. On the Create Rule page, enter `my_rule` as the rule ID.

4. Set the rules in the **SQL Editor**, for example, if you want to save the MQTT messages of the topic `t/#`  to Tablestore, you can use the SQL syntax below. 

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

5. Click the + **Add Action** button to define an action that the rule will trigger. With this action, EMQX sends the data processed by the rule to Tablestore. 

6. Select `Alibaba Tablestore` from the **Type of Action** dropdown list. Keep the **Action** dropdown with the default `Create Action` value. You can also select a Sink if you have created one. This demonstration will create a new Sink.

7. Enter a name for the Sink. The name should combine upper/lower case letters and numbers.

8. From the **Connector** dropdown box, select the `my_tablestore` created before. You can also create a new Connector by clicking the button next to the dropdown box. For the configuration parameters, see [Create a Connector](#create-a-connector).

9. Configure the following fields:

   - **Data Source**: The data source from which EMQX retrieves the message. It represents the origin of the data being processed. This could be a specific topic or data stream.

   - **Table Name**: The name of the Tablestore table where the data will be stored. Enter the table name you created before. You can also dynamically assign a table name using variables such as `${table}`.

   - **Measurement**: The measurement name used in Tablestore, which typically corresponds to a logical grouping or category of data. For example, it could be something like `temperature_readings` or `sensor_data`. You can also use variables (e.g., `${measurement}`) to dynamically assign the metric name.

   - **Storage Model Type**: The type of data storage model used in Tablestore. Currently, on `timeseries`  is supported, optimized for time-based data.

   - **Tags**: Tags are key-value pairs associated with each data entry in Tablestore. These can be used to add metadata or labels to the data for easier querying and filtering. You can click **Add** to define multiple tags, for example:

     | Key        | Value     |
     | ---------- | --------- |
     | `location` | `office1` |
     | `device`   | `sensor1` |

   - **Fields**:  A list of fields specifying which data is sent to Tablestore. Each field is mapped to a column in the Tablestore table. You can click **Add** to add the following:
     - **Column**: The name of the column in Tablestore. The column name can be defined using variables, such as `${column_name}`, which should match the field in the payload of the message example sent later.
     - **Message value**: The value to be assigned to the column. The value can be a dynamic reference (like `${value}`), a boolean (`true`), a number (`1.3`), or binary data.
     - **Is Int**: If the column is of numeric type, EMQX will, by default, insert it into Tablestore as a floating-point type. To insert integer values, this flag needs to be set to `true`. When configuring through the configuration file, variables (e.g., `${isint}`) can be used to dynamically assign this flag.
     - **Is Binary**: If the column is binary, EMQX will, by default, insert it into Tablestore as a string type. To insert binary data, this flag needs to be set to `true`. When configuring through the configuration file, variables (e.g., `${isbinary}`) can be used to dynamically assign this flag.
     
   - **Timestamp**: The timestamp recorded in Tablestore, represented as an integer value in microseconds. This specifies the timestamp to be inserted into Tablestore. You can provide a fixed value, use the string "NOW" to indicate that EMQX should dynamically fill in the current time when processing the message, or use a variable placeholder (e.g., `${microsecond_timestamp}`) for dynamic assignment.

   - **Meta Update Model**: Defines the update strategy for metadata in Tablestore:
     - `MUM_IGNORE`: Ignores metadata updates, ensuring that metadata remains unchanged even if there are conflicting updates.
     - `MUM_NORMAL`: Performs a normal metadata update. If the metadata does not exist, it will be dynamically created before writing the data. If there is a conflict with existing metadata, it may be overwritten.

10. Advanced settings (optional):  See [Advanced Configurations](#advanced-configurations).

11. Before clicking **Create**, you can click **Test Connectivity** to test if the Sink can be connected to the Tablestore server.

12. Click **Create** to complete the Sink creation. Back on the **Create Rule** page, you will see the new Sink appear under the **Action Outputs** tab.

13. On the **Create Rule** page, verify the configured information. Click the **Create** button to generate the rule.

Now you have successfully created the rule and you can see the new rule appear on the **Rule** page. Click the **Actions(Sink)** tab, you can see the new Tablestore Sink.

You can also click **Integration** -> **Flow Designer** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to Tablestore after parsing by the rule  `my_rule`.

## Test the Rule

1. Use MQTTX  to send a message to topic  `t/1`  to trigger an online/offline event.

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "table": "timeseries_demo_with_data", "measurement": "foo", "microsecond_timestamp": 1734924039271024, "column_name": "cc", "value": 1}'
   ```

2. Check the running status of the Sink, there should be one new incoming and one new outgoing message.

3. Go to the [Tablestore Console](https://account.alibabacloud.com/login/login.htm?spm=5176.12901015-2.0.0.1a364b84fgwsH6) to check if the data has been written into Tablestore. 

   - In **Metric Name**, enter the measurement name (in this demo, it is `foo`). 
   - In **Tag**, use `location=office1` and `device=sensor1` as the query condition, then click **Search**.

   ![tablestore_query_data](./assets/tablestore_query_data.png)

## Advanced Configurations

This section delves deeper into the advanced configuration options available for the Tablestore Connector and Sink. When configuring the Connector and Sink in the Dashboard, navigate to **Advanced Settings** to tailor the following parameters to meet your specific needs.

| **Fields**            | **Descriptions**                                             | **Recommended Value** |
| --------------------- | ------------------------------------------------------------ | --------------------- |
| Buffer Pool Size      | Specifies the number of buffer worker processes that will be allocated for managing data flow in egress-type bridges between EMQX and Tablestore. These worker processes are responsible for temporarily storing and handling data before it is sent to the target service. This setting is particularly relevant for optimizing performance and ensuring smooth data transmission in egress (outbound) scenarios. For Sinks that only deal with ingress (inbound) data flow, this option can be set to "0" as it is not applicable. | `16`                  |
| Request TTL           | The "Request TTL" (Time To Live) configuration setting specifies the maximum duration, in seconds, that a request is considered valid once it enters the buffer. This timer starts ticking from the moment the request is buffered. If the request stays in the buffer for a period exceeding this TTL setting or if it is sent but does not receive a timely response or acknowledgment from Tablestore, the request is deemed to have expired. | `45`                  |
| Health Check Interval | Specifies the time interval, in seconds, at which the Sink will perform automated health checks on the connection to Tablestore. | `15`                  |
| Max Buffer Queue Size | Specifies the maximum number of bytes that can be buffered by each buffer worker in the Tablestore Sink. Buffer workers temporarily store data before it is sent to Tablestore, serving as an intermediary to handle data flow more efficiently. Adjust the value according to your system's performance and data transfer requirements. | `256`                 |
| Batch Size            | Specifies the size of data batches that can be transmitted from EMQX to Tablestore in a single transfer operation. By adjusting the size, you can fine-tune the efficiency and performance of data transfer between EMQX and Tablestore. | `1`                   |
| Query Mode            | Allows you to choose `asynchronous` or `synchronous` query modes to optimize message transmission based on different requirements. In asynchronous mode, writing to Tablestore does not block the MQTT message publish process. However, this might result in clients receiving messages ahead of their arrival in Tablestore. | `Async`               |
| Inflight Window       | An "in-flight query" refers to a query that has been initiated but has not yet received a response or acknowledgment. This setting controls the maximum number of in-flight queries that can exist simultaneously when the Sink is communicating with Tablestore.<br/>When the **Query Mode** is set to `async` (asynchronous), the "Inflight Window" parameter gains special importance. If it is crucial for messages from the same MQTT client to be processed in strict order, you should set this value to 1. | `100`                 |
