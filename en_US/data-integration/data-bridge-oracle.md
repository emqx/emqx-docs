# Ingest Data into Oracle Database

EMQX supports integration with Oracle Database so you can save client messages and events to Oracle Database, or use events to trigger the update or removal of data to record the online status or online/offline of clients.

{% emqxce %}
::: tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

:::tip Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

:::


## Feature List

- [Connection pool](./data-bridges.md#connection-pool)
- [Batch mode](./data-bridges.md#batch-mode)
- [Buffer queue](./data-bridges.md#buffer-queue)
- [Prepared Statement](./data-bridges.md#prepared-statement)

## Quick Start Tutorial

This section introduces how to use the Oracle Database data bridge with a practical tutorial, covering topics like how to install the Oracle Database server and create data tables, create data bridges and rules for forwarding data to Oracle Database, and test the data bridges and rules.

This tutorial assumes that you run both EMQX and Oracle Database on the local machine. If you have Oracle Database and EMQX running remotely, adjust the settings accordingly.

### Install Oracle Database Server

Install Oracle Database server via Docker, and then run the docker image.

```bash
# To start the Oracle Database docker image locally
docker run --name oracledb -p 1521:1521 -d oracleinanutshell/oracle-xe-11g:1.0.0

# To start the Oracle Database docker image remotely
docker run --name oracledb -p 1521:1521 -e ORACLE_ALLOW_REMOTE=true -d oracleinanutshell/oracle-xe-11g:1.0.0

# For performance concern, you may want to disable the disk asynch IO:
docker run --name oracledb -p 1521:1521 -e ORACLE_DISABLE_ASYNCH_IO=true -d oracleinanutshell/oracle-xe-11g:1.0.0

# Access the container
docker exec -it oracledb bash

# Connect to the default database "XE"
# username: "system"
# password: "oracle"
sqlplus
```

### Create Data Tables

Use the following SQL statements to create data table `t_mqtt_msgs` in Oracle Database for storing the message ID, client ID, topic, QoS, retain flag, message payload, and timestamp of every message.

  ```sql
  CREATE TABLE t_mqtt_msgs (
    msgid VARCHAR2(64),
    sender VARCHAR2(64),
    topic VARCHAR2(255),
    qos NUMBER(1),
    retain NUMBER(1),
    payload NCLOB,
    arrived TIMESTAMP
  );
  ```

Use the following SQL statements to create data table `t_emqx_client_events` in Oracle Database for storing the client ID, event type, and creation time of every event.

  ```sql
  CREATE TABLE t_emqx_client_events (
    clientid VARCHAR2(255),
    event VARCHAR2(255),
    created_at TIMESTAMP
  );
  ```

### Create Oracle Database Data Bridges

Data bridges for message storage and event recording require different SQL templates. Therefore, you need to create 2 different data bridges to Oracle Database for message storage and event recording.

1. Go to EMQX Dashboard, and click **Data Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **Oracle Database**, and then click **Next**.

4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

5. Input the connection information:

   - **Server Host**: Input `http://127.0.0.1:1521`, or the actual URL if the Oracle Database server is running remotely.
   - **Database Name**: Input `XE`.
   - **Oracle Database SID**: Input `XE`.
   - **Username**: Input `system`.
   - **Password**: Input `oracle`.

6. Configure the **SQL Template** based on the feature to use.

   Note: This is a preprocessed SQL, so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements.
   
   - To create a data bridge for message storage, use the SQL statement below:
   
     ```sql
     INSERT INTO t_mqtt_msgs(msgid, sender, topic, qos, retain, payload, arrived) VALUES(
       ${id},
       ${clientid},
       ${topic},
       ${qos},
       ${flags.retain},
       ${payload},
       TO_TIMESTAMP('1970-01-01 00:00:00', 'YYYY-MM-DD HH24:MI:SS') + NUMTODSINTERVAL(${timestamp}/1000, 'SECOND')
     )
     ```
   
   - To create a data bridge for online/offline status recording, use the SQL statement below:
   
     ```sql
     INSERT INTO t_emqx_client_events(clientid, event, created_at) VALUES (
       ${clientid},
       ${event},
       TO_TIMESTAMP('1970-01-01 00:00:00', 'YYYY-MM-DD HH24:MI:SS') + NUMTODSINTERVAL(${timestamp}/1000, 'SECOND')
     )
     ```

7. Leave other options as default.

8. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the Oracle Database server.

9. Click **Create** to finish the creation of the data bridge.

   A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into Oracle Database. For detailed steps, refer to [Create Rules for Oracle Database Data Bridge](#create-rules-for-oracle-database-data-bridge).

Now the Oracle Database data bridge should appear in the data bridge list (**Data Integration** -> **Data Bridge**) with **Resource Status** as **Connected**.

### Create Rules for Oracle Database Data Bridge

After you have successfully created the data bridges to Oracle Database, you can continue to create rules to specify the data to be saved into Oracle Database and rules for the online/offline status recording.

1. Go to EMQX Dashboard, click **Data Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor** based on the feature to use:

   - To create a rule for message storage, input the following SQL syntax, which means the MQTT messages under topic `t/#`  will be saved to Oracle Database.

     Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the data bridge in the `SELECT` part.

     ```sql
     SELECT 
       *
     FROM
       "t/#"
     ```

   - To create a rule for online/offline status recording, input the following SQL syntax:

     ```sql
     SELECT
       *
     FROM
       "$events/client_connected", "$events/client_disconnected"
     ```

4. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list, and then select the Oracle Database data bridge just created. Click the **Add** button.
6. Click the **Create** button to finish the setup.

Now you have successfully created the data bridges to Oracle Database. You can click **Data Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to Oracle Database after parsing by rule  `my_rule`.

### Test the Data Bridges and Rules

Use MQTTX  to send a message to topic  `t/1`  to trigger an online/offline event.

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello Oracle Database" }'
```

Check the running status of the two data bridges, there should be one new incoming and one new outgoing message.

Check whether the data is written into the `t_mqtt_msgs` data table.

```sql
SELECT * FROM t_mqtt_msgs;

MSGID                            SENDER TOPIC QOS RETAIN PAYLOAD                            ARRIVED
-------------------------------- ------ ----- --- ------ ---------------------------------- ----------------------------
0005FA6CE9EF9F24F442000048100002 emqx_c t/1   0   0      { "msg": "hello Oracle Database" } 28-APR-23 08.22.51.760000 AM

```

Check whether the data is written into the `t_emqx_client_events` table.

```sql
SELECT * FROM t_emqx_client_events;

CLIENTID EVENT               CREATED_AT
-------- ------------------- ----------------------------
emqx_c   client.connected    28-APR-23 08.22.51.757000 AM
emqx_c   client.disconnected 28-APR-23 08.22.51.760000 AM
```
