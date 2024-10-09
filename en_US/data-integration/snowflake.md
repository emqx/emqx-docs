# Ingest MQTT Data into Snowflake

::: tip

The Snowflake data integration is an EMQX Enterprise edition feature.

:::

[Snowflake](https://www.snowflake.com/en/) is a cloud-based data platform that provides a highly scalable and flexible solution for data warehousing, analytics, and secure data sharing. Known for its ability to handle structured and semi-structured data, Snowflake is designed to store vast amounts of data while providing fast query performance and seamless integration with various tools and services.

This page provides a detailed introduction to the data integration between EMQX and Snowflake, and offers practical guidance on the rule and Sink creation.

## How It Works

Snowflake data integration in EMQX is a ready-to-use feature that can be easily configured for complex business development. In a typical IoT application, EMQX acts as the IoT platform responsible for device connectivity and message transmission, while Snowflake serves as the data storage and processing platform, handling the ingestion, storage, and analysis of this message data.

![azure-blob-storage-architecture](/Users/emqx/Documents/GitHub/emqx-docs/en_US/data-integration/assets/azure-blob-storage-architecture.png)

EMQX utilizes rules engines and Sinks to forward device events and data to Snowflake. End users and applications can then access data in Snowflake tables. The specific workflow is as follows:

1. **Device Connection to EMQX**: IoT devices trigger an online event upon successfully connecting via the MQTT protocol. The event includes device ID, source IP address, and other property information.
2. **Device Message Publishing and Receiving**: Devices publish telemetry and status data through specific topics. EMQX receives the messages and compares them within the rules engine.
3. **Rules Engine Processing Messages**: The built-in rules engine processes messages and events from specific sources based on topic matching. It matches corresponding rules and processes messages and events, such as data format transformation, filtering specific information, or enriching messages with context information.
4. **Writing to Snowflake**: The rule triggers an action to write messages to Snowflake Stage, and load it into a Snowflake table.

After events and message data are written to the Snowflake, they can be accessed for a variety of business and technical purposes, including:

- **Data Archiving**: Safely store IoT data in Snowflake for long-term archival, ensuring compliance and historical data availability.

  **Data Analytics**: Leverage Snowflake’s data warehousing and analytics capabilities to perform real-time or batch analysis, enabling predictive maintenance, operational insights, and device performance assessments.

## Features and Advantages

Using Snowflake data integration in EMQX can bring the following features and advantages to your business:

- **Message Transformation**: Messages can undergo extensive processing and transformation in EMQX rules before being written to Snowflake, facilitating subsequent storage and use.
- **Flexible Data Operations**: The Snowflake Sink offers flexibility in data handling by allowing users to select specific fields to write into Snowflake, enabling efficient and dynamic storage configurations tailored to business needs.
- **Integrated Business Processes**: The Snowflake Sink allows device data to be combined with the rich ecosystem applications of Snowflake, enabling more business scenarios like data analysis and archiving.
- **Low-Cost Long-Term Storage**: Snowflake’s scalable storage infrastructure is optimized for long-term data retention at a lower cost compared to traditional databases, making it an ideal solution for storing large volumes of IoT data.

These features enable you to build efficient, reliable, and scalable IoT applications and benefit from business decisions and optimizations.

## Before You Start

This section introduces the preparations required before creating a Snowflake Sink in EMQX.

### Prerequisites

- Understanding of [rules](./rules.md).
- Understanding of [data integration](./data-bridges.md).

### Initialize Snowflake ODBC driver

To enable EMQX to communicate with Snowflake and efficiently transfer data, it is necessary to install and configure the Snowflake Open Database Connectivity (ODBC) driver. It acts as the communication bridge, ensuring that data is properly formatted, authenticated, and transferred.

#### Linux

Run the following script to install the Snowflake ODBC driver and configure the `odbc.ini` file:

```
scripts/install-snowflake-driver.sh
```

#### macOS

To install and configure the Snowflake ODBC driver on macOS, follow these steps:

1. Install unixODBC, for example:

   ```
   brew install unixodbc
   ```

2. [Download and install iODBC](https://github.com/openlink/iODBC/releases/download/v3.52.16/iODBC-SDK-3.52.16-macOS11.dmg).

3. [Download and install the Snowflake ODBC driver](https://sfc-repo.snowflakecomputing.com/odbc/macuniversal/3.3.2/snowflake_odbc_mac_64universal-3.3.2.dmg).

4. Refer to [Installing and configuring the ODBC Driver for macOS](https://docs.snowflake.com/en/developer-guide/odbc/odbc-mac) for detailed installation and configuration instructions.

5. After installation, update the following configuration files:

   - Update permissions and configuration for the Snowflake ODBC driver:

     ```bash
     chown $(id -u):$(id -g) /opt/snowflake/snowflakeodbc/lib/universal/simba.snowflake.ini
     echo 'ODBCInstLib=libiodbcinst.dylib' >> /opt/snowflake/snowflakeodbc/lib/universal/simba.snowflake.ini
     ```

   - Create or update the `~/.odbc.ini` file to configure the ODBC connection:

     ```
     cat << EOF > ~/.odbc.ini
     [ODBC]
     Trace=no
     TraceFile=
     
     [ODBC Drivers]
     Snowflake = Installed
     
     [ODBC Data Sources]
     snowflake = Snowflake
     
     [Snowflake]
     Driver = /opt/snowflake/snowflakeodbc/lib/universal/libSnowflake.dylib
     EOF
     ```

### Create a User Account and Database

Once the Snowflake ODBC driver is installed, you need to set up a user account, database, and related resources for data ingestion. The following credentials will be required later for configuring the connector and Sink in EMQX:

| Field                  | Value                                            |
| ---------------------- | ------------------------------------------------ |
| Data Source Name (DSN) | `snowflake`                                      |
| Username               | `snowpipeuser`                                   |
| Password               | `Snowpipeuser99`                                 |
| Database Name          | `testdatabase`                                   |
| Schema                 | `public`                                         |
| Stage                  | `emqx`                                           |
| Pipe                   | `emqx`                                           |
| Pipe User              | `snowpipeuser`                                   |
| Private Key            | `file://<path to snowflake_rsa_key.private.pem>` |

#### Generate RSA Key Pair

To securely connect to Snowflake, generate an RSA key pair for authentication using the following commands:

```bash
openssl genrsa 2048 | openssl pkcs8 -topk8 -inform PEM -out snowflake_rsa_key.private.pem -nocrypt
openssl rsa -in snowflake_rsa_key.private.pem -pubout -out snowflake_rsa_key.public.pem
```

#### Set Up Snowflake Resources Using SQL

Once the ODBC driver is set up and the RSA key pair is generated, you can set up the Snowflake resources. This involves creating the necessary database, table, stage, and pipe in Snowflake using SQL commands.

1. In the Snowflake console, open the SQL Worksheet and execute the following SQL commands to create the database, table, stage, and pipe:

   ```sql
   USE ROLE accountadmin;
   
   CREATE DATABASE IF NOT EXISTS testdatabase;
   
   CREATE OR REPLACE TABLE testdatabase.public.emqx (
       clientid STRING,
       topic STRING,
       payload STRING,
       publish_received_at TIMESTAMP_LTZ
   );
   
   CREATE STAGE IF NOT EXISTS testdatabase.public.emqx
   FILE_FORMAT = (TYPE = CSV PARSE_HEADER = TRUE FIELD_OPTIONALLY_ENCLOSED_BY = '"')
   COPY_OPTIONS = (ON_ERROR = CONTINUE PURGE = TRUE);
   
   CREATE PIPE IF NOT EXISTS testdatabase.public.emqx AS
   COPY INTO testdatabase.public.emqx
   FROM @testdatabase.public.emqx
   MATCH_BY_COLUMN_NAME = CASE_INSENSITIVE;
   ```

2. Create a new user and set the RSA public key for that user:

   ```sql
   CREATE USER IF NOT EXISTS snowpipeuser
       PASSWORD = 'Snowpipeuser99'
       MUST_CHANGE_PASSWORD = FALSE;
   
   ALTER USER snowpipeuser SET RSA_PUBLIC_KEY = '
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_1>
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_2>
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_3>
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_4>
   ';
   ```

3. Create and assign the required role to the user for managing the Snowflake resources:

   ```sql
   CREATE OR REPLACE ROLE snowpipe;
   
   GRANT USAGE ON DATABASE testdatabase TO ROLE snowpipe;
   GRANT USAGE ON SCHEMA testdatabase.public TO ROLE snowpipe;
   GRANT INSERT, SELECT ON testdatabase.public.emqx TO ROLE snowpipe;
   GRANT READ, WRITE ON STAGE testdatabase.public.emqx TO ROLE snowpipe;
   GRANT OPERATE, MONITOR ON PIPE testdatabase.public.emqx TO ROLE snowpipe;
   GRANT ROLE snowpipe TO USER snowpipeuser;
   ALTER USER snowpipeuser SET DEFAULT_ROLE = snowpipe;
   ```

## Create a Connector

Before adding the Snowflake Sink, you need to create the corresponding connector in EMQX to establish the connection with Snowflake.

1. Go to the Dashboard **Integration** -> **Connector** page.
2. Click the **Create** button in the top right corner.
3. Select **Snowflake** as the connector type and click next.
4. Enter the connector name, a combination of upper and lowercase letters and numbers. Here, enter `my-snowflake`.
5. Enter the connection information.
   - **Server Host**: The server host is the Snowflake endpoint URL, typically in the format `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com`. You need to replace `<Your Snowflake Organization ID>-<Your Snowflake Account Name>` with the subdomain specific to your Snowflake instance.
   - **Account**: Enter your Snowflake Organization ID and Snowflake account name separated by a dash (`-`), which is part of the URL you use to access the Snowflake platform and can be found in your Snowflake console.
   - **Data Source Name(DSN)**: Enter `snowflake`, which corresponds to the DSN configured in the `.odbc.ini` file during ODBC driver setup.
   - **Username**: Enter `snowpipeuser`, as defined during the previous setup process.
   - **Password**: Enter `Snowpipeuser99`, as defined during the previous setup process.
6. Before clicking **Create**, you can click **Test Connectivity** to test if the connector can connect to the Snowflake.
7. Click the **Create** button at the bottom to complete the connector creation.

You have now completed the connector creation and can proceed to create a rule and Sink to specify how the data will be written into Snowflake.

## Create a Rule with Snowflake Sink

This section demonstrates how to create a rule in EMQX to process messages from the source MQTT topic `t/#` and write the processed results to the Snowflake through the configured Sink.

1. Go to the Dashboard **Integration** -> **Rules** page.

2. Click the **Create** button in the top right corner.

3. Enter the rule ID `my_rule`, and input the following rule SQL in the SQL editor:

   ```sql
   SELECT
     clientid,
     unix_ts_to_rfc3339(publish_received_at, 'millisecond') as publish_received_at,
     topic,
     payload
   FROM
       "t/#"
   ```

   ::: tip

   If you are new to SQL, you can click **SQL Examples** and **Enable Debug** to learn and test the rule SQL results.

   :::
   ::: tip
   
   For Snowflake integration, it's important that the selected fields exactly match the number of columns and their names of the table defined in Snowflake, so avoid adding extra fields or selecting from `*`. 
   
   :::


4. Add an action, select `Snowflake` from the **Action Type** dropdown list, keep the action dropdown as the default `create action` option, or choose a previously created Snowflake action from the action dropdown. Here, create a new Sink and add it to the rule.

5. Enter the Sink's name (for example, `snowflake_sink`) and a brief description.

6. Select the `my-snowflake` connector created earlier from the connector dropdown. You can also click the create button next to the dropdown to quickly create a new connector in the pop-up box. The required configuration parameters can be found in [Create a Connector](#create-a-connector).

7. Configure the following settings:

   - **Database Name**: Enter `testdatabase`. This is the Snowflake database that was created for storing EMQX data.
   - **Schema**: Enter `public`, the schema within the `testdatabase` where the data table is located.
   - **Stage**: Enter `emqx`, the stage created in Snowflake for holding the data before loading it into the table.
   - **Pipe**: Enter `emqx`, the pipe automating the loading process from the stage to the table.
   - **Pipe User**: Enter `snowpipeuser`, the Snowflake user with the appropriate permissions to manage the pipe.
   - **Private Key**: Enter the path to the private RSA key, for example, `file://<path to snowflake_rsa_key.private.pem>`, or the content of RSA private key file. This is the key used for secure authentication, necessary for accessing the Snowflake pipe securely.  Note that, if using a file path, the file path must be the same on all cluster nodes and must be readable by the EMQX application user.

8. Select the **Upload Mode**: Currently, only `Aggregated Upload` is supported. This method groups the results of multiple rule triggers into a single file (e.g., a CSV file) and uploads it to Snowflake, reducing the number of files and improving write efficiency.

9. Select **Aggregation Type**: Currently, only `csv` is supported. Data will be staged to Snowflake in comma-separated CSV format.

   - **Column Order**: Select the order of the columns from the dropdown list based on your desired arrangement. The generated CSV file will be sorted first by the selected columns, with unselected columns sorted alphabetically afterward.

   - **Max Records**: Set the maximum number of records before aggregation is triggered. For example, you can set it to `1000` to upload after collecting 1000 records. When the maximum number of records is reached, the aggregation of a single file will be completed and uploaded, resetting the time interval.

   - **Time Interval**: Set the time interval (in seconds) at which aggregation occurs. For example, if set to `60`, data will be uploaded every 60 seconds even if the maximum number of records hasn’t been reached, resetting the maximum number of records.

10. Expand **Advanced Settings** and configure the advanced setting options as needed (optional). For more details, refer to [Advanced Settings](#advanced-settings).

11. Use the default values for the remaining settings. Click the **Create** button to complete the Sink creation. After successful creation, the page will return to the rule creation, and the new Sink will be added to the rule actions.

12. Back on the rule creation page, click the **Create** button to complete the entire rule creation process.

You have now successfully created the rule. You can see the newly created rule on the **Rules** page and the new Snowflake Sink on the **Actions (Sink)** tab.

You can also click **Integration** -> **Flow Designer** to view the topology. The topology visually shows how messages under the topic `t/#` are written into the Snowflake after being parsed by the rule `my_rule`.

## Test the Rule

This section shows how to test the rule configured with the direct upload method.

### Publish a Test Message

Use MQTTX to publish a message to the topic `t/1`:

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Snowflake" }'
```

Repeat this step a few times to generate multiple test messages.

### Verify the Data in Snowflake

After sending the test messages, you can verify that the data was successfully written to Snowflake by accessing your Snowflake instance and querying the target table.

1. Open the Snowflake web interface and log in to the Snowflake Console with your credentials.

2. In the Snowflake Console, execute the following SQL query to view the data written by the rule into the `emqx` table:

   ```
   SELECT * FROM testdatabase.public.emqx;
   ```

   This will display all the records uploaded to the `emqx` table, including the `clientid`, `topic`, `payload`, and `publish_received_at` fields.

3. You should see the test messages you sent, such as the message content `{ "msg": "Hello Snowflake" }`, along with other metadata like the topic and timestamp.

## Advanced Settings

This section delves into the advanced configuration options available for the Snowflake Sink. In the Dashboard, when configuring the Sink, you can expand **Advanced Settings** to adjust the following parameters based on your specific needs.

| Field Name                | Description                                                  | Default Value  |
| ------------------------- | ------------------------------------------------------------ | -------------- |
| **Max Retries**           | Set the maximum number of retries in case of a failed upload. For example, enter `3` to allow three retry attempts. | `3`            |
| **Buffer Pool Size**      | Specifies the number of buffer worker processes, which are allocated to manage the data flow between EMQX and Snowflake. These workers temporarily store and process data before sending it to the target service, crucial for optimizing performance and ensuring smooth data transmission. | `16`           |
| **Request TTL**           | The "Request TTL" (Time To Live) configuration setting specifies the maximum duration, in seconds, that a request is considered valid once it enters the buffer. This timer starts ticking from the moment the request is buffered. If the request stays in the buffer for a period exceeding this TTL setting or if it is sent but does not receive a timely response or acknowledgment from Snowflake, the request is deemed to have expired. |                |
| **Health Check Interval** | Specifies the time interval (in seconds) for the Sink to perform automatic health checks on its connection with Snowflake. | `15`           |
| **Max Buffer Queue Size** | Specifies the maximum number of bytes that can be buffered by each buffer worker process in the Snowflake Sink. The buffer workers temporarily store data before sending it to Snowflake, acting as intermediaries to handle the data stream more efficiently. Adjust this value based on system performance and data transmission requirements. | `256`          |
| **Query Mode**            | Allows you to choose between `synchronous` or `asynchronous` request modes to optimize message transmission according to different requirements. In asynchronous mode, writing to Snowflake does not block the MQTT message publishing process. However, this may lead to clients receiving messages before they arrive at Snowflake. | `Asynchronous` |
| **Batch Size**            | Specifies the maximum size of data batches transmitted from EMQX to Snowflake in a single transfer operation. By adjusting the size, you can fine-tune the efficiency and performance of data transfer between EMQX and Snowflake.<br />If the "Batch Size" is set to "1," data records are sent individually, without being grouped into batches. | `1`            |
| **Inflight  Window**      | "In-flight queue requests" refer to requests that have been initiated but have not yet received a response or acknowledgment. This setting controls the maximum number of in-flight queue requests that can exist simultaneously during Sink communication with Snowflake. <br/>When **Request Mode** is set to `asynchronous`, the "Request In-flight Queue Window" parameter becomes particularly important. If strict sequential processing of messages from the same MQTT client is crucial, then this value should be set to `1`. | `100`          |
| **Connect Timeout**       | This specifies the time (in seconds) the system will wait for a connection to Snowflake before timing out. For example, you can set this to `30` seconds. If the connection cannot be established within this time, EMQX will attempt to retry (based on your **Max Retries** setting) or raise an error. This setting is useful for managing network latency or connection reliability. | `15`           |
| **HTTP Pipelining **      | Specifies the maximum number of HTTP requests that can be sent out before waiting for responses. | `100`          |
| **Connection Pool Size**  | Defines how many connections EMQX can maintain simultaneously to Snowflake. A larger pool size allows for more concurrent requests, which is important in high-load scenarios, but it also consumes more system resources. | `8`            |