# PostgreSQL

EMQX supports integration with PostgreSQL so you can save client messages and events to PostgreSQL, or use events to trigger the update or removal of data to record the online status or online/offline of clients.

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. [Experience](https://www.emqx.com/en/try?product=enterprise) the benefits of this enterprise-ready MQTT messaging platform today.
:::
{% endemqxce %}

:::tip
This section is also applicable to TimescaleDB and MatrixDB.
:::

## Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

## Features list

- [Connection pool](./data-bridges.md#Connection pool)
- [Async mode](./data-bridges.md#Async mode)
- [Batch mode](./data-bridges.md#Batch mode)
- [Buffer queue](./data-bridges.md#Buffer queue)
- [SQL preprocessing](./data-bridges.md#Prepared statement)

## [Configuration parameters](#Configuration)
<!-- TODO LIKN TO THE CONFIG doc。 -->

## Quick starts

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

### Connect to PostgreSQL

We will create 2 data bridges to PostgreSQL for messages storage and event records respectively. 

#### [Messages storage](#Storage)

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.
2. Click **Create** on the top right corner of the page.
3. In the **Create Data Bridge** page, click to select **PostgreSQL**, and then click **Next**.
4. Input a name for the data bridge. Note: It should be a combination of upper/lower case letters and numbers.
5. Input the connection information. Input **127.0.0.1:5432** as the **Server Host**,  **emqx_data** as the **Database Name**, **root** as the **Username**, and **public** as the **Password**.
6. Configure the **SQL Template**. Use the SQL statements below to insert data. Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements. 

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

Before creating the above data bridge, please use the following SQL statements to create data table `emqx_messages` in PostgreSQL database for storing the client ID, topic, payload, and creating time of every message. 

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

7. Advanced settings (optional):  Choose whether to use sync or async query mode as needed. For details, see [Configuration parameters](#Configuration).
8. Then click **Create** to finish the creation of the data bridge.

We have successfully created the data bridge to PostgreSQL, now we can continue to create rules to specify the data to be saved into PostgreSQL. 

1. Go to EMQX Dashboard, click **Data Integration** -> **Rules**.
2. Click **Create** on the top right corner of the page.
3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor**. Here we want to save the MQTT messages under topic `t/#`  to PostgreSQL, we can use the SQL syntax below. Note: If you are testing with your SQL, please ensure you have included all required fields in the `SELECT` part. 

  ```sql
  SELECT 
    *
  FROM
    "t/#"
  ```

4. Then click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge we just created under **Data bridge**.  
5. Then, click the **Add** button. 
6. Then click the **Create** button to finish the setup. 

Now we have successfully created the data bridge to PostgreSQL. You can click **Data Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to InfluxDB after parsing by rule  `my_rule`. 

#### Online/Offline status recording

The operating steps are similar to those at the [Message storage](#Storage) part expect for the SQL template and SQL rules. 

The SQL template for online/offline status recording is as follows. Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements.

```sql
INSERT INTO emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  TO_TIMESTAMP((${timestamp} :: bigint)/1000)
)
```

Before creating this data bridge, please use the following SQL statements to create data table `emqx_client_events` in PostgreSQL database for storing the client ID, event type, and creating time of every event. 

```sql
CREATE TABLE emqx_client_events (
  id SERIAL primary key,
  clientid VARCHAR(255),
  event VARCHAR(255),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

The SQL rule is as follows: 

```sql
SELECT
  *
FROM 
  "$events/client_connected", "$events/client_disconnected"
```

### Test

Use MQTTX  to send a message to topic  `t/1`  to trigger an online/offline event. 

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello PostgreSQL" }'
```

Check the running status of the two data bridges, there should be one new incoming and one new outgoing message. 

Check whether the data is written into the `t_mqtt_messages`  data table. 

```bash
emqx_data=# select * from t_mqtt_msg;
 id |              msgid               | sender | topic | qos | retain |            payload
        |       arrived
----+----------------------------------+--------+-------+-----+--------+-------------------------------+---------------------
  1 | 0005F298A0F0AEE2F443000012DC0002 | emqx_c | t/1   |   0 |        | { "msg": "hello PostgreSQL" } | 2023-01-19 07:10:32
(1 row)

```

`emqx_client_events`  table:

```bash
emqx_data=# select * from emqx_client_events;
 id | clientid |        event        |     created_at
----+----------+---------------------+---------------------
  3 | emqx_c   | client.connected    | 2023-01-19 07:10:32
  4 | emqx_c   | client.disconnected | 2023-01-19 07:10:32
(2 rows)

```
