# TDengine

EMQX supports integration with TDengine so you can save client messages and events to TDengine, or use events to trigger the update or removal of data to record the online status or online/offline of clients.

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

### Install TDengine

Install TDengine via Docker, and then run the docker image. 

```bash
# To start the TDengine docker image 
docker run --name TDengine -p 6041:6041 tdengine/tdengine

# Access the container
docker exec -it TDengine bash

# Locate the TDengine server in the container
taos

# Create and then select the database
CREATE DATABASE mqtt;

use mqtt;
```

### Connect to TDengine

We will create 2 data bridges to TDengine for messages storage and event records respectively. 

#### [Messages storage](#Storage)

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.
2. Click **Create** on the top right corner of the page.
3. In the **Create Data Bridge** page, click to select **TDengine**, and then click **Next**.
4. Input a name for the data bridge. Note: It should be a combination of upper/lower case letters and numbers.
5. Input the connection information. Input **127.0.0.1:6041** as the **Server Host**,  **mqtt** as the **Database Name**, **root** as the **Username**, and **taosdata** as the **Password**.
6. Configure the **SQL Template**. Use the SQL statements below to insert data. Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements. 

  ```sql
    INSERT INTO mqtt.t_mqtt_msg(ts, msgid, mqtt_topic, qos, payload, arrived) 
      VALUES (${ts}, ${id}, ${topic}, ${qos}, ${payload}, ${timestamp})
  ```

Before creating the above data bridge, please use the following SQL statements to create data table `t_mqtt_msg` in TDengine database for storing the client ID, topic, payload, and creating time of every message. 

  ```sql
    CREATE TABLE t_mqtt_msg (
      ts timestamp,
      msgid NCHAR(64),
      mqtt_topic NCHAR(255),
      qos TINYINT,
      payload BINARY(1024),
      arrived timestamp
    );
  ```

7. Advanced settings (optional):  Choose whether to use sync or async query mode as needed. For details, see [Configuration parameters](#Configuration).
8. Then click **Create** to finish the creation of the data bridge.

We have successfully created the data bridge to TDengine, now we can continue to create rules to specify the data to be saved into TDengine. 

1. Go to EMQX Dashboard, click **Data Integration** -> **Rules**.
2. Click **Create** on the top right corner of the page.
3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor**. Here we want to save the MQTT messages under topic `t/#`  to TDengine, we can use the SQL syntax below. Note: If you are testing with your SQL, please ensure you have included all required fields in the `SELECT` part. 

  ```sql
    SELECT
      *,
      now_timestamp('millisecond')  as ts
    FROM
      "t/#"
  ```

4. Then click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge we just created under **Data bridge**.  
5. Then, click the **Add** button. 
6. Then click the **Create** button to finish the setup. 

Now we have successfully created the data bridge to TDengine. You can click **Data Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to InfluxDB after parsing by rule  `my_rule`. 

#### Online/Offline status recording

The operating steps are similar to those at the [Message storage](#Storage) part expect for the SQL template and SQL rules. 

The SQL template for online/offline status recording is as follows. Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements.

```sql
    INSERT INTO emqx_client_events(ts, clientid, event) VALUES (
      ${ts},
      ${clientid},
      ${event}
    )
```

Before creating this data bridge, please use the following SQL statements to create data table `emqx_client_events` in TDengine database for storing the client ID, event type, and creating time of every event. 

```sql
    CREATE TABLE emqx_client_events (
      ts timestamp,
      clientid VARCHAR(255),
      event VARCHAR(255)
    );
```

The SQL rule is as follows: 

```sql
    SELECT
      *,
      now_timestamp('millisecond')  as ts
    FROM 
      "$events/client_connected", "$events/client_disconnected"
```

### Test

Use MQTTX  to send a message to topic  `t/1`  to trigger an online/offline event. 

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello TDengine" }'
```

Check the running status of the two data bridges, there should be one new incoming and one new outgoing message. 

Check whether the data is written into the `t_mqtt_msg`  data table. 

```bash
taos> select * from t_mqtt_msg;
           ts            |             msgid              |           mqtt_topic           | qos  |            payload             |         arrived         |
==============================================================================================================================================================
 2023-02-13 06:10:53.787 | 0005F48EB5A83865F440000014F... | t/1                            |    0 | { "msg": "hello TDengine" }    | 2023-02-13 06:10:53.787 |
Query OK, 1 row(s) in set (0.002968s)

```

`emqx_client_events`  table:

```bash
taos> select * from emqx_client_events;
           ts            |            clientid            |             event              |
============================================================================================
 2023-02-13 06:10:53.777 | emqx_c                         | client.connected               |
 2023-02-13 06:10:53.791 | emqx_c                         | client.disconnected            |
Query OK, 2 row(s) in set (0.002327s)

```
