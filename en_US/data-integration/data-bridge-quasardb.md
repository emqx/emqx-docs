# Ingest MQTT Data into QuasarDB

[QuasarDB](https://www.quasardb.net/) is a high-performance, column-oriented time-series database designed for storing and querying large volumes of time-stamped data. EMQX supports integration with QuasarDB, enabling you to save MQTT messages and client events to QuasarDB. This facilitates the construction of data pipelines and analytical processes for IoT telemetry management and analysis.

This page provides a detailed overview of the data integration between EMQX and QuasarDB with practical instructions on creating and validating the data integration.


## How It Works

QuasarDB data integration is an out-of-the-box feature in EMQX that combines EMQX's device connectivity and message transmission capabilities with the high-performance time-series storage of QuasarDB. Through the built-in [rule engine](./rules.md) component and Sink, you can store MQTT messages and client events in QuasarDB. This integration simplifies the process of ingesting data from EMQX to QuasarDB for storage and management, eliminating the need for complex coding.

The diagram below illustrates a typical architecture of data integration between EMQX and QuasarDB:

<!-- TODO: add architecture diagram -->

Ingesting MQTT data into QuasarDB works as follows:

1. **Message publication and reception**: IoT devices establish successful connections to EMQX through the MQTT protocol and publish real-time MQTT data to EMQX. When EMQX receives these messages, it initiates the matching process within its rules engine.
2. **Message data processing**: When a message arrives, it passes through the rule engine and is then processed by the rule defined in EMQX. The rules, based on predefined criteria, determine which messages need to be routed to QuasarDB. If any rules specify payload transformations, those transformations are applied, such as converting data formats, filtering out specific information, or enriching the payload with additional context.
3. **Data ingestion into QuasarDB**: The rule triggers the writing of messages to QuasarDB. With the help of SQL templates, users can extract data from the rule processing results to construct SQL and send it to QuasarDB for execution, so that specific fields of the message can be written into the corresponding tables.
4. **Data storage and utilization**: With data now stored in QuasarDB, businesses can harness its time-series querying capabilities for analytics, monitoring, and operational use cases.

## Features and Benefits

The data integration with QuasarDB offers a range of features and benefits:

- **Real-time data streaming**: EMQX is built for handling real-time data streams, ensuring efficient and reliable data transmission from source systems to QuasarDB. It enables organizations to capture and analyze data in real-time, making it ideal for use cases requiring immediate insights and actions.
- **High-performance time-series storage**: QuasarDB's columnar engine is optimized for time-series workloads, providing fast ingestion throughput and efficient range queries over large volumes of timestamped data.
- **Flexibility in data transformation**: EMQX provides a powerful SQL-based Rule Engine, allowing organizations to pre-process data before storing it in QuasarDB. It supports various data transformation mechanisms such as filtering, routing, aggregation, and enrichment.
- **Batching support**: The QuasarDB Sink supports batch writes, reducing the number of round trips and improving overall ingestion throughput.

## Before You Start

This section describes the preparations you need to complete before creating the QuasarDB data integration, including how to configure the ODBC driver and install QuasarDB.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data integration](./data-bridges.md)

### Install and Configure the ODBC Driver

The QuasarDB connector uses ODBC to connect to the database. You need to install and configure the QuasarDB ODBC driver on the host where EMQX is running before creating a connector.

Refer to the [QuasarDB ODBC documentation](https://doc.quasar.ai/master/user-guide/integration/odbc.html) for full installation instructions. The steps below show a typical setup on Debian-based systems using driver version 3.14.1.

1. Download and install the QuasarDB C API package and ODBC driver:

   ```bash
   curl -fsSL -O https://download.quasar.ai/quasardb/3.14/3.14.1/api/c/qdb-api_3.14.1.deb
   curl -fsSL -O https://download.quasar.ai/quasardb/3.14/3.14.1/api/odbc/qdb-3.14.1-linux-64bit-odbc-driver.tar.gz
   apt-get install -yqq ./qdb-api_3.14.1.deb
   tar -C /tmp/qdb_odbc_driver -xf qdb-3.14.1-linux-64bit-odbc-driver.tar.gz
   ```

2. Register the driver in `/etc/odbcinst.ini`:

   ```ini
   [qdb_odbc_driver]
   Description=Quasardb ODBC Driver
   Driver=/tmp/qdb_odbc_driver/lib/libqdb_odbc_driver.so
   Setup=/tmp/qdb_odbc_driver/lib/libqdb_odbc_driver.so
   ```

3. Create a Data Source Name (DSN) entry in `/etc/odbc.ini`:

   ```ini
   [qdb]
   Driver = qdb_odbc_driver
   Description = QuasarDB ODBC Data Source
   #URI = qdb://172.100.239.30:2836
   #UID = user_name
   #PWD = user_key
   #KEY = cluster_public_key
   ```

The DSN name you set here (e.g., `qdb`) is what you enter in the **ODBC Data Source Name** field when creating the connector.

### Install and Connect to QuasarDB

This section describes how to start a QuasarDB instance using Docker.

1. Pull and start the QuasarDB Docker image:

   ```bash
   docker run -d --name qdb \
     -p 2836:2836 \
     bureau14/qdb:3.14.1
   ```

   ::: tip

   QuasarDB requires connecting via an **IP address**, not a hostname. Use `127.0.0.1` (or the actual host IP) in the URI. Hostname-based connections are not supported.

   :::

2. Verify the instance is running by connecting with the QuasarDB shell:

   ```bash
   docker run -it --rm bureau14/qdbsh --cluster qdb://127.0.0.1:2836
   ```

To enable user authentication or cluster key authentication, refer to the [QuasarDB security documentation](https://doc.quasar.ai/).

### Create a Table

Create a table in QuasarDB to receive ingested data. The example below creates a table for storing temperature and humidity readings:

```sql
CREATE TABLE temp_hum (temp DOUBLE, hum DOUBLE);
```

::: tip

QuasarDB tables always include an implicit `$timestamp` index column. You do not need to declare it when creating a table, but you can reference it in INSERT statements.

:::

## Create a Connector

This section demonstrates how to create a Connector to connect the Sink to QuasarDB.

1. Enter the EMQX Dashboard and click **Integration** -> **Connectors**.

2. Click **Create** in the top right corner of the page.

3. On the **Create Connector** page, select **QuasarDB** and then click **Next**.

4. In the **Configuration** step, configure the following information:

   - **Connector Name**: Enter a name for the connector, which should be a combination of upper and lower-case letters and numbers, for example: `my_quasardb`.
   - **Server URI**: Enter the URI of your QuasarDB cluster using an IP address, for example `qdb://127.0.0.1:2836`.
   - **ODBC Data Source Name**: Enter the DSN name defined in `/etc/odbc.ini`, for example `qdb`.
   - **Username**: Enter the username, if any.
   - **Password**: Enter the user secret key, if any.
   - **Cluster Public Key**: Enter the cluster public key, if any.
   - **Connect Timeout**: Timeout to be used when connecting to QuasarDB.

5. Advanced settings (optional): For details, see [Features of Sink](./data-bridges.md#features-of-sink).

6. Before clicking **Create**, you can click **Test Connectivity** to test if the connector can connect to QuasarDB.

7. Click the **Create** button at the bottom to complete the creation of the connector. In the pop-up dialog, you can click **Back to Connector List** or click **Create Rule** to continue creating rules with Sinks to specify the data to be forwarded to QuasarDB. For detailed steps, see [Create a Rule with QuasarDB Sink](#create-a-rule-with-quasardb-sink).

## Create a Rule with QuasarDB Sink

This section demonstrates how to create a rule in the Dashboard for processing messages from the source MQTT topic `t/#` and saving the processed data to the QuasarDB table `temp_hum` via the configured Sink.

1. Go to EMQX Dashboard, click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Enter `my_rule` as the rule ID. To create a rule for message storage, enter the following statement in the **SQL Editor**:

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

4. Click the **+ Add Action** button to define an action that will be triggered by the rule.

5. Select `QuasarDB` from the **Type of Action** dropdown list. Keep the **Action** dropdown with the default `Create Action` value. You can also select a QuasarDB Sink if you have created one.

6. Enter a name for the Sink. The name should be a combination of upper/lower case letters and numbers.

7. From the **Connector** dropdown box, select the `my_quasardb` connector created before. You can also create a new Connector by clicking the button next to the dropdown box. For the configuration parameters, see [Create a Connector](#create-a-connector).

8. Configure the **SQL Template** to define how data is written to QuasarDB.

   ::: tip

   The SQL Template only accepts **INSERT** statements. Other statement types such as UPDATE and DELETE are not supported.

   :::

   The SQL template supports placeholder variables such as `${clientid}`. QuasarDB uses `$timestamp` as the implicit timestamp index column; you can use `now()` to insert the current server time.

   ::: warning

   The QuasarDB ODBC driver does not support prepared statements. Any value that resolves to a `STRING` or `BLOB` type must be manually quoted with single quotes (`'`) in your SQL template.

   :::

   ```sql
   insert into temp_hum($timestamp, temp, hum)
   values (now(), ${.temp}, ${.hum})
   ```

   You can optionally configure a **Table Name For Health Check**. If set, EMQX runs `SHOW TABLE <table>` against this table during action health checks to probe whether the table exists. If left empty, action-level health checking is skipped.

9. **Fallback Actions (Optional)**: If you want to improve reliability in case of message delivery failure, you can define one or more fallback actions. These actions will be triggered if the primary Sink fails to process a message. See [Fallback Actions](./data-bridges.md#fallback-actions) for more details.

10. **Advanced settings (optional)**: For details, see [Features of Sink](./data-bridges.md#features-of-sink).

11. Before clicking **Create**, you can click **Test Connectivity** to test that the Sink can connect to QuasarDB.

12. Click the **Create** button to complete the Sink configuration. A new Sink will be added to the **Action Outputs**.

13. Back on the **Create Rule** page, verify the configured information. Click the **Create** button to generate the rule.

You have now successfully created the rule for the QuasarDB Sink. You can see the newly created rule on the **Integration** -> **Rules** page. Click the **Actions(Sink)** tab to see the new QuasarDB Sink.

You can also click **Integration** -> **Flow Designer** to view the topology and verify that messages under topic `t/#` are sent and saved to QuasarDB after parsing by rule `my_rule`.

## Test the Rule

Use MQTTX to send a message to topic `t/1` to trigger the rule.

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "temp": "27.5", "hum": "41.8" }'
```

Check the running statistics of the QuasarDB Sink. There should be 1 new matching and 1 new outgoing message. Verify that the data is written into the `temp_hum` table in QuasarDB.
