# Ingest MQTT Data into AlloyDB

[AlloyDB for PostgreSQL](https://cloud.google.com/products/alloydb?hl=en) is Google Cloud’s fully managed, PostgreSQL‑compatible database service engineered for demanding enterprise workloads. EMQX supports seamless integration with AlloyDB, enabling real-time ingestion and storage of MQTT data from IoT devices. Leveraging EMQX’s efficient message routing alongside AlloyDB’s high-throughput transactional capabilities and real-time analytics via its Hybrid Transactional/Analytical Processing (HTAP) engine, you get a powerful pipeline for capturing device status, logging events, and performing insightful analytics.

This page provides a comprehensive guide to configuring EMQX rules and setting up AlloyDB Sinks for streamlined real-time integration with AlloyDB.

## How It Works

AlloyDB data integration in EMQX is a built-in feature that ingests MQTT-based IoT data streams directly into AlloyDB's high-performance, PostgreSQL-compatible database. With a built-in [rule engine](./rules.md) component, the integration simplifies the process of ingesting data from EMQX to AlloyDB for storage and analysis, eliminating the need for complex coding.

The diagram below illustrates a typical architecture of data integration between EMQX and AlloyDB:

<!--To be updated-->

![EMQX Integration AlloyDB](./assets/emqx-integration-postgesql.png)

Ingesting MQTT data into AlloyDB works as follows:

1. **IoT devices connect to EMQX**: After IoT devices are successfully connected through the MQTT protocol, online events will be triggered. The events include information such as device ID, source IP address, and other attributes.
2. **Message publication and reception**: The devices publish telemetry and status data to specific topics. When EMQX receives these messages, it initiates the matching process within its rules engine.
3. **Rule Engine Processing Messages**: EMQX’s rules engine processes events and messages by matching them to defined rules based on topics or message content. Processing can include data transformation (e.g., JSON to SQL-ready format), filtering, and data enrichment with contextual information before database insertion.
4. **Write to AlloyDB**: The matched rule triggers SQL execution against AlloyDB. Using SQL templates, users can map processed data fields to AlloyDB tables and columns. Because AlloyDB supports parallel query execution and optimized storage with a built-in columnar engine, data can be inserted quickly while remaining instantly queryable for analytics.

After the event and message data are written to AlloyDB, you can connect to AlloyDB to read the data for flexible application development, such as:

- Connect to visualization tools, such as Grafana, to generate charts based on data and show data changes.
- Integrate AlloyDB with device management systems or analytical models to track device health, detect anomalies, and trigger alerts.
- Use AlloyDB’s HTAP capabilities to run complex analytics (aggregation, joins, time-series queries) on live IoT data while continuing to process new device telemetry in real time.

## Features and Benefits

The data integration with AlloyDB can bring the following features and advantages to your business:

- **Flexible Event Handling**: Using the EMQX rules engine, AlloyDB can store and process device lifecycle events (connect, disconnect, status changes) with low latency. When paired with AlloyDB’s parallel query execution and independent scaling, you can analyze event data in real time to detect device failures, anomalies, or usage trends.
- **Message Transformation**: Messages can undergo extensive processing and transformation through EMQX rules before being written to AlloyDB, making storage and usage more convenient.
- **Flexible Data Operations with SQL Templates**: Through EMQX’s SQL template mapping, structured IoT data can be inserted or updated in AlloyDB tables and columns. AlloyDB’s PostgreSQL compatibility supports standard SQL, JSONB storage, and indexing, while AI-powered indexing automatically optimizes query performance as workloads evolve.
- **Integration of Business Processes**: AlloyDB’s PostgreSQL ecosystem compatibility allows direct integration with ERP, CRM, GIS, and custom business systems, whether hosted in Google Cloud or on-premises. Paired with EMQX, you can implement event-driven automation and business process orchestration without complex data pipelines.
- **Advanced Geospatial Capabilities**: Via PostgreSQL extensions like PostGIS, AlloyDB supports geospatial data storage, indexing, and querying, enabling geofencing, route tracking, and location analytics. Combined with EMQX’s reliable MQTT ingestion, it’s possible to build fleet tracking, asset monitoring, and other real-time IoT-GIS solutions.
- **Built-in Metrics and Monitoring**: EMQX provides runtime metrics for each AlloyDB sink, while AlloyDB integrates with Cloud Monitoring for query performance, storage utilization, and replica health, ensuring end-to-end observability.

## Before You Start

This section describes the preparations you need to complete before you start to create the AlloyDB Database sinks, including how to set up the AlloyDB server and create data tables.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [Data Integration](./data-bridges.md)

### Set up AlloyDB

<!--To add inputs-->

## Create a Connector

Before add AlloyDB Sink, you need to create the AlloyDB connector. It assumes that you run both EMQX and AlloyDB on the local machine. If you have AlloyDB and EMQX running remotely, adjust the settings accordingly.

1. Go to EMQX Dashboard, and click **Integration** -> **Connector**.
2. Click **Create** on the top right corner of the page.
3. In the **Create Connector** page, click to select **AlloyDB**, and then click **Next**.
4. Enter a name for the sink. The name should be a combination of upper/lower case letters and numbers, for example, `my_alloydb`.
5. Enter the connection information:

   - **Server Host**: Enter ``, or the actual hostname if the AlloyDB server is running remotely.
   - **Database Name**: Enter `emqx_data`.
   - **Username**: Enter ``.
   - **Password**: Enter ``.
   - **Enable TLS**: If you want to establish an encrypted connection, click the toggle switch. For more information about TLS connection, see [TLS for External Resource Access](../network/overview.md/#tls-for-external-resource-access).
6. Advanced settings (optional):  For details, see [Features of Sink](./data-bridges.md#features-of-sink).
7. Before clicking **Create**, you can click **Test Connectivity** to test if the connector can connect to the AlloyDB server.
8. Click the **Create** button at the bottom to complete the creation of the connector. In the pop-up dialog, you can click **Back to Connector List** or click **Create Rule** to continue creating rules with Sinks to specify the data to be forwarded to AlloyDB and record client events. For detailed steps, see [Create a Rule with AlloyDB Sink for Message Storage](#create-a-rule-with-postgresql-sink-for-message-storage) and [Create a Rule with AlloyDB Sink for Events Recording](#create-a-rule-with-postgresql-for-events-recording).

## Create a Rule with AlloyDB Sink for Message Storage

This section demonstrates how to create a rule in the Dashboard for processing messages from the source MQTT topic `t/#`, and saving the processed data to the AlloyDB table `t_mqtt_msg` via the configured Sink.

1. Go to the Dashboard **Integration** -> **Rules** page.

2. Click **Create** in the upper right corner of the page.

3. Enter the rule ID `my_rule` and enter the rule in the SQL editor. Here we choose to store MQTT messages with `t/#` topic to AlloyDB, make sure that the fields selected by the rule (in the SELECT section) contain all the variables used in the SQL template, here the rule SQL is as follows:

   ```sql
   SELECT
   *
   FROM
   "t/#"
   ```

   ::: tip

   If you are a beginner user, click **SQL Examples** and **Enable Test** to learn and test the SQL rule. 

   :::

4. Click the + **Add Action** button to define an action to be triggered by the rule. With this action, EMQX sends the data processed by the rule to AlloyDB.

5. Select AlloyDB from the **Type of Action** drop-down, leave the **Action** drop-down at the default `Create Action` option, or you can select a previously created AlloyDB action from the Action drop-down box. This example will create a brand new Sink and add it to the rule.

6. Enter the name and description of the Sink in the form below.

7. From the **Connector** dropdown box, select the `my_alloydb` created before. You can also create a new Connector by clicking the button next to the dropdown box. For the configuration parameters, see [Create a Connector](#create-a-connector).

8. Configure the **SQL Template**. Use the SQL statements below to insert data.

   Note: This is a [preprocessed SQL](./data-bridges.md#prepared-statement), so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements.

   ```sql
   INSERT INTO t_mqtt_msg(msgid, sender, topic, qos, payload, arrived) VALUES(
     ${id},
     ${clientid},
     ${topic},
     ${qos},
     ${payload},
     TO_TIMESTAMP((${timestamp} :: bigint)/1000)
   )
   ```

9. **Fallback Actions (Optional)**: If you want to improve reliability in case of message delivery failure, you can define one or more fallback actions. These actions will be triggered if the primary Sink fails to process a message. See [Fallback Actions](./data-bridges.md#fallback-actions) for more details.

10. **Advanced settings (optional)**: For details, see [Features of Sink](./data-bridges.md#features-of-sink).

11. Before clicking **Create**, you can click **Test Connectivity** to test that the Sink can be connected to the AlloyDB server.

12. Click the **Create** button to complete the Sink configuration. A new Sink will be added to the **Action Outputs.**

13. Back on the **Create Rule** page, verify the configured information. Click the **Save** button to generate the rule. 

Now that you have successfully created the rule, you can click **Integration** -> **Rules** page to see the newly created rule and also see the newly created AlloyDB Sink in the **Action (Sink)** tab.

You can also click **Integration** -> **Flow Designer** to see the topology, through which you can visualize that the messages under topic `t/#` are being written to AlloyDB after being parsed by the rule `my_rule`.

## Create a Rule with AlloyDB for Events Recording

This section demonstrates how to create a rule for recording the clients' online/offline status and storing the events data to the AlloyDB table `emqx_client_events` via a configured Sink.

The steps are similar to those in [Create a Rule with AlloyDB Sink for Message Storage](#create-a-rule-with-alloydb-sink-for-message-storage) except for the SQL template and SQL rules.

The SQL rule statement for online/offline status recording is as follows.

```sql
SELECT
  *
FROM
  "$events/client_connected", "$events/client_disconnected"
```

The SQL template for events recording is as follows.

Note: This is a [preprocessed SQL](./data-bridges.md#prepared-statement), so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements.

```sql
INSERT INTO emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  TO_TIMESTAMP((${timestamp} :: bigint)/1000)
)
```

## Test the Rules

Use MQTTX to send a message to topic `t/1` to trigger an online/offline event.

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello AlloyDB" }'
```

Check the running status of the two sinks. For the message storage Sink, there should be 1 new incoming and 1 new outgoing message. For the events recording Sink, there 2 events records.

Check whether the data is written into the `t_mqtt_msg` data table.

```bash
emqx_data=# select * from t_mqtt_msg;
 id |              msgid               | sender | topic | qos | retain |            payload
        |       arrived
----+----------------------------------+--------+-------+-----+--------+-------------------------------+---------------------
  1 | 0005F298A0F0AEE2F443000012DC0002 | emqx_c | t/1   |   0 |        | { "msg": "hello AlloyDB" } | 2023-01-19 07:10:32
(1 row)

```

Check whether the data is written into the `emqx_client_events` table.

```bash
emqx_data=# select * from emqx_client_events;
 id | clientid |        event        |     created_at
----+----------+---------------------+---------------------
  3 | emqx_c   | client.connected    | 2023-01-19 07:10:32
  4 | emqx_c   | client.disconnected | 2023-01-19 07:10:32
(2 rows)

```