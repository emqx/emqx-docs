# Ingest Data into PostgreSQL

EMQX supports integration with PostgreSQL so you can save client messages and events to PostgreSQL, or use events to trigger the update or removal of data to record the online status or online/offline of clients.

{% emqxce %}
::: tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

::: tip
This section is also applicable to TimescaleDB and MatrixDB.
:::

## Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

## Features List

- [Connection pool](./data-bridges.md)
- [Async mode](./data-bridges.md)
- [Batch mode](./data-bridges.md)
- [Buffer queue](./data-bridges.md)
- [SQL preprocessing](./data-bridges.md)

## Quick Start Tutorial

This section introduces how to configure the PostgreSQL data bridge, covering topics like how to set up the PostgreSQL server, create data bridges and rules for forwarding data to PostgreSQL and test the data bridges and rules.

This tutorial assumes that you run both EMQX and PostgreSQL on the local machine. If you have PostgreSQL and EMQX running remotely, adjust the settings accordingly.

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

Use the following SQL statements to create data table `emqx_messages` in PostgreSQL database for storing the client ID, topic, payload, and creating time of every message. 

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

### Create PostgreSQL Data Bridges

You need to create 2 data bridges to PostgreSQL for messages storage and event records respectively. 

#### Message storage

1. Go to EMQX Dashboard, and click **Data Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **PostgreSQL**, and then click **Next**.

4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

5. Input the connection information:

   - **Server Host**: Input `http://127.0.0.1:5432`, or the actual URL if the PostgreSQL server is running remotely.
   - **Database Name**: Input `emqx_data`.
   - **Username**: Input `root`.
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

#### Online/Offline Status Recording

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

Now the PostgreSQL data bridge should appear in the data bridge list (**Data Integration** -> **Data Bridge**) with **Resource Status** as **Connected**. 

### Create Rules for PostgreSQL Data Bridge 

After you have successfully created the data bridge to PostgreSQL, you can continue to create rules to specify the data to be saved into PostgreSQL and rules for the online/offline status recording. 

#### Message Storage

1. Go to EMQX Dashboard, click **Data Integration** -> **Rules**.

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

#### Online/Offline Status Recording

The creating steps are similar to those at the [Message Storage](#message-storage) part except for the SQL rules.

The SQL rule is as follows: 

```sql
SELECT
  *
FROM 
  "$events/client_connected", "$events/client_disconnected"
```

Now you have successfully created the data bridge to PostgreSQL. You can click **Data Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to PostgreSQL after parsing by rule  `my_rule`. 

### Test the Data Bridges and Rules

Use MQTTX  to send a message to topic  `t/1`  to trigger an online/offline event. 

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello PostgreSQL" }'
```

Check the running status of the two data bridges, there should be one new incoming and one new outgoing message. 

Check whether the data is written into the `t_mqtt_messages` data table.

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
