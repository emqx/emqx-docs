# Ingest MQTT Data into Oracle Database

{% emqxce %}
::: tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

[Oracle Database](https://www.oracle.com/database/) is one of the leading relational commercial database solutions, widely used in enterprises and organizations of various sizes and types. EMQX supports integration with Oracle Database, enabling you to save MQTT messages and client events to Oracle Database. This allows for the construction of complex data pipelines and analytical processes for data management and analysis, or for managing device connections and integrating with other enterprise systems such as ERP and CRM.

This page provides a comprehensive introduction to the data integration between EMQX and Oracle Database with practical instructions on creating and validating the data integration.

## How It Works

Oracle Database data integration is an out-of-the-box feature in EMQX designed to bridge the gap between MQTT-based IoT data and Oracle Database's powerful data storage capabilities. With a built-in [rule engine](./rules.md) component, the integration simplifies the process of ingesting data from EMQX to Oracle Database for storage and management, eliminating the need for complex coding.

The diagram below illustrates a typical architecture of data integration between EMQX and Oracle Database:

![EMQX Integration Oracel](./assets/emqx-integration-oracle.png)

Ingesting MQTT data into Oracle Database works as follows:

1. **Message publication and reception**: Industrial IoT devices establish successful connections to EMQX through the MQTT protocol and publish real-time MQTT data from machines, sensors, and product lines based on their operational states, readings, or triggered events to EMQX. When EMQX receives these messages, it initiates the matching process within its rules engine.  
2. **Message data processing:** When a message arrives, it passes through the rule engine and is then processed by the rule defined in EMQX. The rules, based on predefined criteria, determine which messages need to be routed to Oracle Database. If any rules specify payload transformations, those transformations are applied, such as converting data formats, filtering out specific information, or enriching the payload with additional context.
3. **Data ingestion into Oracle Database**: The rule triggers the writing of messages to Oracle Database. With the help of SQL templates, users can extract data from the rule processing results to construct SQL and send it to Oracle Database for execution, so that specific fields of the message can be written or updated into the corresponding tables and columns of the database.
4. **Data Storage and Utilization**: With the data now stored in Oracle Database, businesses can harness its querying power for various use cases. For instance, by utilizing Oracle's advanced analytics and predictive capabilities, users can extract valuable information and insights from IoT data.

## Features and Benefits

The data integration with Oracle Database offers a range of features and benefits tailored to ensure efficient data transmission, storage, and utilization:

- **Real-time Data Streaming**: EMQX is built for handling real-time data streams, ensuring efficient and reliable data transmission from source systems to Oracle Database. It enables organizations to capture and analyze data in real-time, making it ideal for use cases requiring immediate insights and actions.
- **High Performance and Scalability**: EMQX's cluster and distributed architecture is capable of handling the ever-increasing volume of device connections and message transmissions. Oracle offers a variety of expansion and scaling solutions, including data partitioning, data replication and redundancy, clustering, and high availability, providing users with flexible, reliable, and high-performance database solutions.
- **Flexibility in Data Transformation:** EMQX provides a powerful SQL-based Rule Engine, allowing organizations to pre-process data before storing it in Oracle Database. It supports various data transformation mechanisms, such as filtering, routing, aggregation, and enrichment, enabling organizations to shape the data according to their needs.
- **Easy Deployment and Management:** EMQX provides a user-friendly interface for configuring data sources, pre-processing data rules, and Oracle Database storage settings. This simplifies the setup and ongoing management of the data integration process.
- **Advanced Analytics:** Oracle Database's powerful SQL-based query language and support for complex analytical functions empower users to gain valuable insights from IoT data, enabling predictive analytics, anomaly detection, and more.

## Before You Start

This section describes the preparations you need to complete before you start to create the Oracle Database data integration, including how to set up the Oracle Database server and create data tables.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data integration](./data-bridges.md)

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

## Create a Connector

This section demonstrates how to create a Connector to connect the Sink to the Oracle Database server.

The following steps assume that you run both EMQX and Oracle Database on the local machine. If you have Oracle Database and EMQX running remotely, adjust the settings accordingly.

1. Enter the EMQX Dashboard and click **Integration** -> **Connectors**.
2. Click **Create** in the top right corner of the page.
3. On the **Create Connector** page, select **Oracle Database** and then click **Next**.
4. In the **Configuration** step, configure the following information:
   - **Connector name**: Enter a name for the connector, which should be a combination of upper and lower-case letters and numbers, for example: `my_oracle`.
   - **Server Host**: Enter `127.0.0.1:1521`, or the actual hostname if the Oracle Database server is running remotely.
   - **Database Name**: Enter `XE`.
   - **Oracle Database SID**: Enter `XE`.
   - **Username**: Enter `system`.
   - **Password**: Enter `oracle`.
5. Advanced settings (optional):  For details, see [Features of Sink](./data-bridges.md#features-of-sink).
6. Before clicking **Create**, you can click **Test Connectivity** to test if the connector can connect to the Oracle Database server.
7. Click the **Create** button at the bottom to complete the creation of the connector. In the pop-up dialog, you can click **Back to Connector List** or click **Create Rule** to continue creating rules with Sinks to specify the data to be forwarded to the Oracle Database and to record client events. For detailed steps, see [Create a Rule with Oracle Database Sink for Message Storage](#create-a-rule-with-oracle-database-sink-for-message-storage) and [Create a Rule with Oracle Database Sink for Events Recording](#create-a-rule-with-oracle-database-sink-for-events-recording).

## Create a Rule with Oracle Database Sink for Message Storage

This section demonstrates how to create a rule to specify the data to be saved into Oracle Database.

1. Go to EMQX Dashboard, and click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Enter `my_rule` as the rule ID, and enter the following SQL syntax in the **SQL Editor**, which means the MQTT messages under topic `t/#`  will be saved to Oracle Database

   Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the Sink in the `SELECT` part.

   ```sql
SELECT 
     *
   FROM
     "t/#"
   ```
   
   Note: If you are a beginner user, click **SQL Examples** and **Enable Test** to learn and test the SQL rule. 

4. Click the + **Add Action** button to define an action that will be triggered by the rule. With this action, EMQX sends the data processed by the rule to Oracle Database. 

5. Select `Oracle Database` from the **Type of Action** dropdown list. Keep the **Action** dropdown with the default `Create Action` value. You can also select an Oracle Database Sink if you have created one. This demonstration will create a new Sink.

6. Enter a name for the Sink. The name should combine upper/lower case letters and numbers.

7. Select the `my_oracle` just created from the **Connector** dropdown box. You can also create a new Connector by clicking the button next to the dropdown box. For the configuration parameters, see [Create a Connector](#create-a-connector).

8. Configure the **SQL Template** based on the feature to use.

   Note: This is a [preprocessed SQL](./data-bridges.md#prepared-statement), so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements.

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
   
9. Leave other options as default.

10. Advanced settings (optional):  Choose whether to use **sync** or **async** query mode as needed. For details, see the relevant configuration information in [Features of Sink](./data-bridges.md#features-of-sink).

11. Before clicking **Create**, you can click **Test Connectivity** to test that the Sink can be connected to the Oracle Database server.

12. Click the **Create** button to complete the Sink configuration. A new Sink will be added to the **Action Outputs.**

13. Back on the **Create Rule** page, verify the configured information. Click the **Create** button to generate the rule. 

You have now successfully created the rule for forwarding data through the Oracle Database Sink. You can see the newly created rule on the **Integration** -> **Rules** page. Click the **Actions(Sink)** tab and you can see the new Oracle Database Sink.

You can also click **Integration** -> **Flow Designer** to view the topology and you can see that the messages under topic `t/#` are sent and saved to Oracle Database after parsing by rule `my_rule`.

## Create a Rule with Oracle Database Sink for Events Recording

This section demonstrates how to create a rule for the online/offline status recording.

The rule creation steps are similar to those in [Create a Rule with Oracle Database Sink for Message Storage](#create-a-rule-with-oracle-database-sink-for-message-storage) except for the SQL rule syntax and SQL template.

The SQL rule syntax for online/offline status recording is as follows:

```sql
SELECT
  *
FROM
  "$events/client_connected", "$events/client_disconnected"
```

The SQL template for the Sink is as follows:

Note: This is a [preprocessed SQL](./data-bridges.md#prepared-statement), so the fields should not be enclosed in quotation marks, and do not write a semicolon at the end of the statements.

```sql
INSERT INTO t_emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  TO_TIMESTAMP('1970-01-01 00:00:00', 'YYYY-MM-DD HH24:MI:SS') + NUMTODSINTERVAL(${timestamp}/1000, 'SECOND')
)
```

## Test the Rules

Use MQTTX  to send a message to topic  `t/1`  to trigger an online/offline event.

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello Oracle Database" }'
```

Check the running status of the two Sinks, there should be one new incoming and one new outgoing message.

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
