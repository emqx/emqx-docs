# Ingest MQTT Data into TDengine

{% emqxce %}
::: tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

[TDengine](https://tdengine.com/) is a big data platform, designed and optimized specifically for the Internet of Things (IoT) and Industrial Internet of Things (IIoT) scenarios. At its heart lies a high-performance time-series database, characterized by its cluster-oriented architecture, cloud-native design, and minimalistic approach. EMQX supports integration with TDengine, enabling massive data transmission, storage, analysis, and distribution from a large number of devices and data collectors. It provides real-time monitoring and early warning of business operation states, offering real-time business insights.

This page provides a comprehensive introduction to the data integration between EMQX and TDengine with practical instructions on creating a rule and data bridge.

## How It Works

TDengine data integration is a built-in feature in EMQX. With a built-in [rule engine](./rules.md) component, the integration simplifies the process of ingesting data from EMQX to TDengine, eliminating the need for complex coding. EMQX forwards device data to TDengine through the rule engine and data bridge. Through the TDengine data bridge, MQTT messages and client events can be stored in TDengine. Additionally, data updates or deletions in TDengine can be triggered by events, thereby enabling the recording of information such as device online status and historical online/offline events.

The diagram below illustrates the typical architecture of EMQX and TDengine data integration in the industrial IoT:

![EMQX Integration TDengine](./assets/emqx-integration-tdengine.png)

Taking the industrial energy consumption management scenario as an example, the workflow is as follows:

1. **Message publication and reception**: Industrial devices establish successful connections to EMQX through the MQTT protocol and regularly publish energy consumption data using the MQTT protocol. This data includes production line identifiers and energy consumption values. When EMQX receives these messages, it initiates the matching process within its rules engine.  
2. **Rule Engine Processes Messages**: The built-in rule engine processes messages from specific sources based on topic matching. When a message arrives, it passes through the rule engine, which matches it with corresponding rules and processes the message data. This can include transforming data formats, filtering specific information, or enriching messages with context information.
3. **Data ingestion into TDengine**: Rules defined in the rule engine trigger operations to write messages to TDengine. The TDengine data bridge provides SQL templates that allow flexible definitions of the data format to write specific message fields to the corresponding tables and columns in TDengine.

After energy consumption data is written to TDengine, you can analyze your data in real-time using standard SQL and powerful time-series extensions, seamlessly integrating with numerous third-party batch analysis, real-time analysis, reporting tools, AI/ML tools, and visualization tools.For example:

- Connect to visualization tools such as Grafana to generate charts and display energy consumption data.
- Connect to application systems such as ERP or Power BI for production analysis and production plan adjustments.
- Connect to business systems to perform real-time energy usage analysis, facilitating data-driven energy management.

## Features and Benefits

Using TDengine data bridging in EMQX brings the following features and advantages to your business:

- **Efficient Data Handling**: EMQX can handle a large number of IoT device connections and message throughput efficiently. TDengine excels in data writing, storage, and querying, meeting the data processing needs of IoT scenarios without overwhelming the system.
- **Message Transformation**: Messages can undergo rich processing and transformation within EMQX rules before being written to TDengine.
- **Cluster and Scalability**: EMQX and TDengine support clustering capabilities and are built on cloud-native architecture, enabling full utilization of the cloud platform's elastic storage, computing, and network resources, allowing for flexible horizontal scaling as your business grows to meet expanding demands. 
- **Advanced Querying Capabilities**: TDengine provides optimized functions, operators, and indexing techniques for efficient querying and analysis of timestamp data, enabling precise insights to be extracted from IoT time-series data.

## Before You Start

This section describes the preparations you need to complete before you start to create the TDengine data bridges, including how to set up the TDengine server and create data tables.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [Data Integration](./data-bridges.md)

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

## Create Connector


This section demonstrate how to create TDengine data bridges in EMQX Dashboard. It assumes that you run both EMQX and TDengine on the local machine. If you have TDengine and EMQX running remotely, adjust the settings accordingly.

Data bridges for message storage and event recording require different SQL templates. Therefore, you need to create 2 different data bridges to TDengine for messages storage and event recording.

1. Go to EMQX Dashboard, and click **Integration** -> **Connector**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Connector** page, click to select **TDengine**, and then click **Next**.

4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

5. Input the connection information. 

   - **Server Host**: Input `http://127.0.0.1:6041`, or the actual URL if the TDengine server is running remotely.
   - **Database Name**: Input `mqtt`.
   - **Username**: Input `root`.
   - **Password**: Input `taosdata`.

6. Configure the **SQL Template** based on the feature to use.

   ::: tip

   There is a breaking change in EMQX 5.1.1. Prior to this version, string-type values were automatically quoted. However, starting from EMQX 5.1.1, users are required to manually quote these values.

   :::
   
   - To create a data bridge for message storage, use the statement below:
   
     ```sql
     INSERT INTO t_mqtt_msg(ts, msgid, mqtt_topic, qos, payload, arrived) 
         VALUES (${ts}, '${id}', '${topic}', ${qos}, '${payload}', ${timestamp})
     ```
   
   - To create a data bridge for online/offline status recording, use the statement below:
   
     ```sql
     INSERT INTO emqx_client_events(ts, clientid, event) VALUES (
           ${ts},
           '${clientid}',
           '${event}'
         )
     ```

7. Advanced settings (optional):  Choose whether to use **sync** or **async** query mode as needed.

8. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the TDengine.

9. Click **Create** to finish the creation of the data bridge.

   A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into TDengine. You can also create rules by following the steps in [Create Rules for TDengine Data Bridge](#create-rules-for-tdengine-data-bridge).

Now the TDengine data bridge should appear in the data bridge list (**Integration** -> **Connector**) with **Resource Status** as **Connected**. 

## Create Connector

Now that you have successfully created the data bridge to TDengine, you can continue to create rules to specify the data to be saved into TDengine. You need to create two different rules for messages forward and event records.

1. Go to EMQX Dashboard, and click **Integration** -> **Rules**.

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

6. Click the **Create** button to finish the setup. 

Now you have successfully created the data bridge to TDengine. You can click **Integration** -> **Flow Designer** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to TDengine after parsing by rule `my_rule`. 

## Test the Data Bridge and Rule

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
