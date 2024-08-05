# Ingest MQTT Data into PostgreSQL

::: tip

The PostgreSQL data integration is an EMQX Enterprise edition feature.

:::

[PostgreSQL](https://www.postgresql.org/) is the world's most advanced open-source relational database, possessing robust data processing capabilities suitable for everything from simple applications to complex data tasks. EMQX supports integration with PostgreSQL, enabling efficient handling of real-time data streams from IoT devices. This integration supports large-scale data storage, precise querying, and complex data association analysis while ensuring data integrity. Leveraging EMQX's efficient message routing and PostgreSQL's flexible data model, it's easy to monitor device statuses, track events, and audit operations, providing businesses with deep data insights and robust business intelligence support.

This page provides a comprehensive introduction to the data integration between EMQX and PostgreSQL with practical instructions on creating a rule and sink.

::: tip
This page is also applicable to MatrixDB.
:::

## How It Works

PostgreSQL data integration is an out-of-the-box feature in EMQX designed to bridge the gap between MQTT-based IoT data and PostgreSQL's powerful data storage capabilities. With a built-in [rule engine](./rules.md) component, the integration simplifies the process of ingesting data from EMQX to PostgreSQL for storage and management, eliminating the need for complex coding.

The diagram below illustrates a typical architecture of data integration between EMQX and PostgreSQL:

![EMQX Integration PostgreSQL](./assets/emqx-integration-postgesql.png)

Ingesting MQTT data into PostgreSQL works as follows:

- **IoT devices connect to EMQX**: After IoT devices are successfully connected through the MQTT protocol, online events will be triggered. The events include information such as device ID, source IP address, and other attributes.
- **Message publication and reception**: The devices publish telemetry and status data to specific topics. When EMQX receives these messages, it initiates the matching process within its rules engine.
- **Rule Engine Processing Messages**: With the built-in rules engine, messages and events from specific sources can be processed based on topic matching. The rules engine matches the corresponding rules and processes messages and events, such as converting data formats, filtering out specific information, or enriching messages with contextual information.
- **Write to PostgreSQL**: The rule triggers the writing of messages to PostgreSQL. With the help of SQL templates, users can extract data from the rule processing results to construct SQL and send it to PostgreSQL for execution, so that specific fields of the message can be written or updated into the corresponding tables and columns of the database.

After the event and message data are written to PostgreSQL, you can connect to PostgreSQL to read the data for flexible application development, such as:

- Connect to visualization tools, such as Grafana, to generate charts based on data and show data changes.
- Connect to the device management system, view the device list and status, detect abnormal device behavior, and eliminate potential problems in a timely manner.

## Features and Benefits

PostgreSQL is a popular open-source relational database with a rich set of features. The data integration with PostgreSQL can bring the following features and advantages to your business:

- **Flexible Event Handling**: Through the EMQX rules engine, PostgreSQL can handle device lifecycle events, greatly facilitating the development of various management and monitoring tasks required for implementing IoT applications. By analyzing event data, you can promptly detect device failures, abnormal behavior, or trend changes to take appropriate measures.
- **Message Transformation**: Messages can undergo extensive processing and transformation through EMQX rules before being written to PostgreSQL, making storage and usage more convenient.
- **Flexible Data Operations**: With SQL templates provided by PostgreSQL data bridging, it's easy to write or update data from specific fields to the corresponding tables and columns in the PostgreSQL database, enabling flexible data storage and management.
- **Integration of Business Processes**: PostgreSQL data bridging allows you to integrate device data with PostgreSQL's rich ecosystem applications, facilitating integration with systems like ERP, CRM, or other custom business systems to achieve advanced business processes and automation.
- **Combining IoT with GIS Technology**: PostgreSQL offers GIS data storage and querying capabilities, supporting geospatial indexing, geofencing and alerts, real-time location tracking, and geographical data processing, among others. Combined with EMQX's reliable message transmission capability, it can efficiently process and analyze geographical location information from mobile devices such as vehicles, enabling real-time monitoring, intelligent decision-making, and business optimization.
- **Runtime Metrics**: Support for viewing runtime metrics of each sink, such as total message count, success/failure counts, current rates, and more.

Through flexible event handling, extensive message transformation, flexible data operations, and real-time monitoring and analysis capabilities, you can build efficient, reliable, and scalable IoT applications, benefiting your business decisions and optimizations.

## Before You Start

This section describes the preparations you need to complete before you start to create the PostgreSQL Database sinks, including how to set up the PostgreSQL server and create data tables.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [Data Integration](./data-bridges.md)

### Install PostgreSQL

Install PostgreSQL via Docker, and then run the docker image.

```bash
# To start the PostgreSQL docker image and set the password as public
docker run --name PostgreSQL -p 5432:5432 -e POSTGRES_PASSWORD=public -d postgres

# Access the container
docker exec -it PostgreSQL bash

# Locate the PostgreSQL server in the container and input the preset password
psql -U postgres -W

# Create and then select the database

CREATE DATABASE emqx_data;

\c emqx_data;
```

### Create Data Tables

Use the following SQL statements to create data table `t_mqtt_msg` in PostgreSQL database for storing the client ID, topic, payload, and creating time of every message.

```sql
CREATE TABLE t_mqtt_msg (
  id SERIAL primary key,
  msgid character varying(64),
  sender character varying(64),
  topic character varying(255),
  qos integer,
  retain integer,
  payload text,
  arrived timestamp without time zone
);
```

Use the following SQL statements to create data table `emqx_client_events` in PostgreSQL database for storing the client ID, event type, and creating time of every event.

```sql
CREATE TABLE emqx_client_events (
  id SERIAL primary key,
  clientid VARCHAR(255),
  event VARCHAR(255),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

## Create a Connector

Before add PostgreSQL Sink, you need to create the PostgreSQL connector. It assumes that you run both EMQX and PostgreSQL on the local machine. If you have PostgreSQL and EMQX running remotely, adjust the settings accordingly.

1. Go to EMQX Dashboard, and click **Integration** -> **Connector**.
2. Click **Create** on the top right corner of the page.
3. In the **Create Connector** page, click to select **PostgreSQL**, and then click **Next**.
4. Enter a name for the sink. The name should be a combination of upper/lower case letters and numbers, for example, `my_psql`.
5. Enter the connection information:

   - **Server Host**: Enter `127.0.0.1:5432`, or the actual hostname if the PostgreSQL server is running remotely.
   - **Database Name**: Enter `emqx_data`.
   - **Username**: Enter `postgres`.
   - **Password**: Enter `public`.
   - **Enable TLS**: If you want to establish an encrypted connection, click the toggle switch. For more information about TLS connection, see [TLS for External Resource Access](../network/overview.md/#tls-for-external-resource-access).
6. Advanced settings (optional):  For details, see [Features of Sink](./data-bridges.md#features-of-sink).
7. Before clicking **Create**, you can click **Test Connectivity** to test if the connector can connect to the PostgreSQL server.
8. Click the **Create** button at the bottom to complete the creation of the connector. In the pop-up dialog, you can click **Back to Connector List** or click **Create Rule** to continue creating rules with Sinks to specify the data to be forwarded to PostgreSQL and record client events. For detailed steps, see [Create a Rule with PostgreSQL Sink for Message Storage](#create-a-rule-with-postgresql-sink-for-message-storage) and [Create a Rule with PostgreSQL Sink for Events Recording](#create-a-rule-with-postgresql-for-events-recording).

:::tip Note

EMQX v5.7.1 introduced a **Disable Prepared Statements** option. If you are using a PostgreSQL service that does not support prepared statements, such as PGBouncer in transaction mode or Supabase, enable this option in the Advanced Settings.

:::

## Create a Rule with PostgreSQL Sink for Message Storage

This section demonstrates how to create a rule in the Dashboard for processing messages from the source MQTT topic `t/#`, and saving the processed data to the PostgreSQL table `t_mqtt_msg` via the configured Sink.

1. Go to the Dashboard **Integration** -> **Rules** page.

2. Click **Create** in the upper right corner of the page.

3. Enter the rule ID `my_rule` and enter the rule in the SQL editor. Here we choose to store MQTT messages with `t/#` topic to PostgreSQL, make sure that the fields selected by the rule (in the SELECT section) contain all the variables used in the SQL template, here the rule SQL is as follows:

   ```sql
   SELECT
   *
   FROM
   "t/#"
   ```

   ::: tip

   If you are a beginner user, click **SQL Examples** and **Enable Test** to learn and test the SQL rule. 

   :::

4. Click the + **Add Action** button to define an action to be triggered by the rule. With this action, EMQX sends the data processed by the rule to PostgreSQL.

5. Select PostgreSQL from the **Type of Action** drop-down, leave the **Action** drop-down at the default `Create Action` option, or you can select a previously created PostgreSQL action from the Action drop-down box. This example will create a brand new Sink and add it to the rule.

6. Enter the name and description of the Sink in the form below.

7. From the **Connector** dropdown box, select the `my_psql` created before. You can also create a new Connector by clicking the button next to the dropdown box. For the configuration parameters, see [Create a Connector](#create-a-connector).

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

9. Advanced settings (optional): For details, see [Features of Sink](./data-bridges.md#features-of-sink).

10. Before clicking **Create**, you can click **Test Connectivity** to test that the Sink can be connected to the PostgreSQL server.

11. Click the **Create** button to complete the Sink configuration. A new Sink will be added to the **Action Outputs.**

12. Back on the **Create Rule** page, verify the configured information. Click the **Create** button to generate the rule. 

Now that you have successfully created the rule, you can click **Integration** -> **Rules** page to see the newly created rule and also see the newly created PostgreSQL Sink in the **Action (Sink)** tab.

You can also click **Integration** -> **Flow Designer** to see the topology, through which you can visualize that the messages under topic `t/#` are being written to PostgreSQL after being parsed by the rule `my_rule`.

## Create a Rule with PostgreSQL for Events Recording

This section demonstrates how to create a rule for recording the clients' online/offline status and storing the events data to the PostgreSQL table `emqx_client_events` via a configured Sink.

The steps are similar to those in [Create a Rule with PostgreSQL Sink for Message Storage](#create-a-rule-with-postresql-sink-for-message-storage) expect for the SQL template and SQL rules.

The rule SQL statement for online/offline status recording is as follows.

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
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello PostgreSQL" }'
```

Check the running status of the two sinks. For the message storage Sink, there should be 1 new incoming and 1 new outgoing message. For the events recording Sink, there 2 events records.

Check whether the data is written into the `t_mqtt_msg` data table.

```bash
emqx_data=# select * from t_mqtt_msg;
 id |              msgid               | sender | topic | qos | retain |            payload
        |       arrived
----+----------------------------------+--------+-------+-----+--------+-------------------------------+---------------------
  1 | 0005F298A0F0AEE2F443000012DC0002 | emqx_c | t/1   |   0 |        | { "msg": "hello PostgreSQL" } | 2023-01-19 07:10:32
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
