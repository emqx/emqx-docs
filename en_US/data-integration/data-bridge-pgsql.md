# Ingest MQTT Data into PostgreSQL

{% emqxce %}
::: tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

[PostgreSQL](https://www.postgresql.org/) is the world's most advanced open-source relational database. EMQX supports integration with PostgreSQL so you can save MQTT messages and client events to PostgreSQL, or use events to trigger the update or removal of data to record the online status or online/offline of clients.

This page provides a comprehensive introduction to the data integration between EMQX and PostgreSQL with practical instructions on creating a rule and data bridge.

::: tip
This page is also applicable to MatrixDB.
:::

## How It Works

PostgreSQL data integration is an out-of-the-box feature in EMQX designed to bridge the gap between MQTT-based IoT data and PostgreSQL's powerful data storage capabilities. With a built-in [rule engine](./rules.md) component, the integration simplifies the process of ingesting data from EMQX to PostgreSQL for storage and management, eliminating the need for complex coding.

<!-- The diagram below illustrates a typical architecture of data integration between EMQX and PostgreSQL. -->

Ingesting MQTT data into PostgreSQL works as follows:

1. **Message publication and reception**: Industrial IoT devices establish successful connections to EMQX through the MQTT protocol and publish real-time MQTT data from machines, sensors, and product lines based on their operational states, readings, or triggered events to EMQX. When EMQX receives these messages, it initiates the matching process within its rules engine.  
2. **Message data processing:** When a message arrives, it passes through the rule engine and is then processed by the rule defined in EMQX. The rules, based on predefined criteria, determine which messages need to be routed to PostgreSQL. If any rules specify payload transformations, those transformations are applied, such as converting data formats, filtering out specific information, or enriching the payload with additional context.
3. **Data ingestion into PostgreSQL**: Once the rule engine identifies a message for PostgreSQL storage, it triggers an action of forwarding the messages to PostgreSQL. Processed data will be seamlessly written into the collection of the PostgreSQL database.
4. **Data Storage and Utilization**: With the data now stored in PostgreSQL, businesses can harness its querying power for various use cases. For instance, in logistics and supply chain management fields, data from IoT devices such as GPS trackers, temperature sensors, and inventory management systems can be monitored and analyzed for real-time tracking, route optimization, demand forecasting, and efficient inventory management.

## Features and Benefits

The data integration with PostgreSQL offers a range of features and benefits tailored to ensure efficient data transmission, storage, and utilization:

- **Real-time Data Streaming**: EMQX is built for handling real-time data streams, ensuring efficient and reliable data transmission from source systems to PostgreSQL. It enables organizations to capture and analyze data in real-time, making it ideal for use cases requiring immediate insights and actions.
- **High Performance and Scalability**: EMQX's distributed architecture and PostgreSQL's columnar storage format enable seamless scalability as data volumes increase. This ensures consistent performance and responsiveness, even with large datasets.
- **Flexibility in Data Transformation:** EMQX provides a powerful SQL-based Rule Engine, allowing organizations to pre-process data before storing it in PostgreSQL. It supports various data transformation mechanisms, such as filtering, routing, aggregation, and enrichment, enabling organizations to shape the data according to their needs.
- **Easy Deployment and Management:** EMQX provides a user-friendly interface for configuring data sources, pre-processing data rules, and PostgreSQL storage settings. This simplifies the setup and ongoing management of the data integration process.
- **Advanced Analytics:** PostgreSQL's powerful SQL-based query language and support for complex analytical functions empower users to gain valuable insights from IoT data, enabling predictive analytics, anomaly detection, and more.

## Before You Start

This section describes the preparations you need to complete before you start to create the PostgreSQL Database data bridges, including how to set up the PostgreSQL server and create data tables.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

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

## Create PostgreSQL Data Bridges

This section demonstrates how to create PostgreSQL data bridges in EMQX Dashboard. It assumes that you run both EMQX and PostgreSQL on the local machine. If you have PostgreSQL and EMQX running remotely, adjust the settings accordingly.

You need to create 2 data bridges to PostgreSQL for messages storage and event records respectively. 

### Message storage

1. Go to EMQX Dashboard, and click **Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **PostgreSQL**, and then click **Next**.

4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

5. Input the connection information:

   - **Server Host**: Input `127.0.0.1:5432`, or the actual hostname if the PostgreSQL server is running remotely.
   - **Database Name**: Input `emqx_data`.
   - **Username**: Input `postgres`.
   - **Password**: Input `public`.

6. Configure the **SQL Template**. Use the SQL statements below to insert data. 

   Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements. 

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

7. Advanced settings (optional):  Choose whether to use **sync** or **async** query mode as needed. For details, see [Configuration](./data-bridges.md#configuration).
8. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the MySQL server.
9. Then click **Create** to finish the creation of the data bridge.

### Online/Offline Status Recording

The operating steps are similar to those at the [Message Storage](#message-storage) part expect for the SQL template and SQL rules. 

The SQL template for online/offline status recording is as follows. 

Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements.

```sql
INSERT INTO emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  TO_TIMESTAMP((${timestamp} :: bigint)/1000)
)
```

Now the PostgreSQL data bridge should appear in the data bridge list (**Integration** -> **Data Bridge**) with **Resource Status** as **Connected**. 

## Create Rules for PostgreSQL Data Bridge 

After you have successfully created the data bridge to PostgreSQL, you can continue to create rules to specify the data to be saved into PostgreSQL and rules for the online/offline status recording. 

### Message Storage

1. Go to EMQX Dashboard, click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor**. Here we want to save the MQTT messages under topic `t/#`  to PostgreSQL, we can use the SQL syntax below. 

   Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the data bridge in the `SELECT` part. 

  ```sql
SELECT 
  *
FROM
  "t/#"
  ```

4. Then click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge we just created under **Data bridge**.  
5. Then, click the **Add** button. 
6. Then click the **Create** button to finish the setup. 

### Online/Offline Status Recording

The creating steps are similar to those at the [Message Storage](#message-storage) part except for the SQL rules.

The SQL rule is as follows: 

```sql
SELECT
  *
FROM 
  "$events/client_connected", "$events/client_disconnected"
```

Now you have successfully created the data bridge to PostgreSQL. You can click **Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to PostgreSQL after parsing by rule  `my_rule`. 

## Test Data Bridges and Rules

Use MQTTX  to send a message to topic  `t/1`  to trigger an online/offline event. 

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello PostgreSQL" }'
```

Check the running status of the two data bridges, there should be one new incoming and one new outgoing message. 

Check whether the data is written into the `t_mqtt_msg` data table.

```bash
emqx_data=# select * from t_mqtt_msg;
 id |              msgid               | sender | topic | qos | retain |            payload
        |       arrived
----+----------------------------------+--------+-------+-----+--------+-------------------------------+---------------------
  1 | 0005F298A0F0AEE2F443000012DC0002 | emqx_c | t/1   |   0 |        | { "msg": "hello PostgreSQL" } | 2023-01-19 07:10:32
(1 row)

```

Check whether the data is written into the`emqx_client_events` table.

```bash
emqx_data=# select * from emqx_client_events;
 id | clientid |        event        |     created_at
----+----------+---------------------+---------------------
  3 | emqx_c   | client.connected    | 2023-01-19 07:10:32
  4 | emqx_c   | client.disconnected | 2023-01-19 07:10:32
(2 rows)

```
