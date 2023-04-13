# Ingest Data into TDengine

EMQX supports integration with TDengine so you can save client messages and events to TDengine, or use events to trigger the update or removal of data from TDengine to record the online status or online/offline of clients.

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

This section introduces how to configure the TDengine data bridge, covering topics like how to set up the EDengine server, create data bridges and rules for forwarding data to TDengine and test the data bridges and rules.

This tutorial assumes that you run both EMQX and TDengine on the local machine. If you have TDengine and EMQX running remotely, adjust the settings accordingly.

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

### Create Data Tables in TDengine

Before you create data bridges for TDengine, you need to create two data tables in TDengine database for message storage and status recording. 

1. Use the following SQL statements to create data table `t_mqtt_msg` in TDengine database. The data table is used to store the client ID, topic, payload, and creation time of every message. 

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

2. Use the following SQL statements to create data table `emqx_client_events` in TDengine database. This data table is used to store the client ID, event type, and creation time of every event. 

   ```sql
     CREATE TABLE emqx_client_events (
         ts timestamp,
         clientid VARCHAR(255),
         event VARCHAR(255)
       );
   ```

### Create TDengine Data Bridges

Data bridges for message storage and event recording require different SQL templates. Therefore, you need to create 2 different data bridges to TDengine for messages storage and event recording.

1. Go to EMQX Dashboard, and click **Data Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **TDengine**, and then click **Next**.

4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

5. Input the connection information. 

   - **Server Host**: Input `http://127.0.0.1:6041`, or the actual URL if the TDengine server is running remotely.
   - **Database Name**: Input `mqtt`.
   - **Username**: Input `root`.
   - **Password**: Input `taosdata`.

6. Configure the **SQL Template** based on the feature to use: 

   Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements. 

   - To create a data bridge for message storage, use the statement below:

     ```sql
     INSERT INTO mqtt.t_mqtt_msg(ts, msgid, mqtt_topic, qos, payload, arrived) 
         VALUES (${ts}, ${id}, ${topic}, ${qos}, ${payload}, ${timestamp})
     ```

   - To create a data bridge for online/offline status recording, use the statement below:

     ```sql
     INSERT INTO emqx_client_events(ts, clientid, event) VALUES (
           ${ts},
           ${clientid},
           ${event}
         )
     ```

7. Advanced settings (optional):  Choose whether to use **sync** or **async** query mode as needed.

8. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the TDengine.

9. Click **Create** to finish the creation of the data bridge.

   A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into TDengine. You can also create rules by following the steps in [Create Rules for TDengine Data Bridge](#create-rules-for-tdengine-data-bridge).

Now the TDengine data bridge should appear in the data bridge list (**Data Integration** -> **Data Bridge**) with **Resource Status** as **Connected**. 

### Create Rules for TDengine Data Bridge

Now that you have successfully created the data bridge to TDengine, you can continue to create rules to specify the data to be saved into TDengine. You need to create two different rules for messages forward and event records.

1. Go to EMQX Dashboard, and click **Data Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor** based on the feature to use:

   - To create a rule for message storage, input the following statement, which means the MQTT messages under topic `t/#`  will be saved to TDengine.

     Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the data bridge in the `SELECT` part.

     ```sql
       SELECT
         *,
         now_timestamp('millisecond')  as ts
       FROM
         "t/#"
     ```

   - To create a rule for online/offline status recording, input the following statement:

     ```sql
     SELECT
           *,
           now_timestamp('millisecond')  as ts
         FROM 
           "$events/client_connected", "$events/client_disconnected"
     ```

5. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list, and then select the data bridge you just created under **Data Bridge**. Click the **Add** button. 

5. Click the **Create** button to finish the setup. 

Now you have successfully created the data bridge to TDengine. You can click **Data Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to TDengine after parsing by rule `my_rule`. 

### Test the Data Bridge and Rule

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
