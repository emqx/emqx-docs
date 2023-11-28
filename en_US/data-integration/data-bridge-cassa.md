# Ingest MQTT Data into Cassandra

<!-- 提供一段简介，描述支数据桥接的基本工作方式、关键特性和价值，如果有局限性也应当在此处说明（如必须说明的版本限制、当前未解决的问题）。 -->

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

[Apache Cassandra](https://cassandra.apache.org/_/index.html) is a popular open-source, distributed NoSQL database management system designed to handle large datasets and build high-throughput applications. EMQX's integration with Apache Cassandra provides the ability to store messages and events in the Cassandra database, enabling functionalities such as time-series data storage, device registration and management, as well as real-time data analysis.

This page provides a comprehensive introduction to the data integration between EMQX and Cassandra with practical instructions on creating and validating the data integration.

:::tip
The current implementation only supports Cassandra v3.x, not yet compatible with v4.x.
:::

## How It Works

Cassandra data integration is an out-of-the-box feature in EMQX that combines EMQX's device connectivity and message transmission capabilities with Cassendra's powerful data storage capabilities. With a built-in [rule engine](./rules.md) component, the integration simplifies the process of ingesting data from EMQX to Cassandra for storage and management, eliminating the need for complex coding.

<!-- The diagram below illustrates a typical architecture of data integration between EMQX and Cassandra. -->

Ingesting MQTT data into Cassandra works as follows:

1. **Message publication and reception**: IoT devices, whether they are part of connected vehicles, IIoT systems, or energy management platforms, establish successful connections to EMQX through the MQTT protocol and publish MQTT messages to specific topics. When EMQX receives these messages, it initiates the matching process within its rules engine.
2. **Message data processing:** When a message arrives, it passes through the rule engine and is then processed by the rule defined in EMQX. The rules, based on predefined criteria, determine which messages need to be routed to Cassandra. If any rules specify payload transformations, those transformations are applied, such as converting data formats, filtering out specific information, or enriching the payload with additional context.
3. **Data ingestion into Cassandra**: Once the rule engine identifies a message for Cassandra storage, it triggers an action of forwarding the messages to Cassandra. Processed data will be seamlessly written into the collection of the Cassandra database.
4. **Data storage and utilization**: With the data now stored in Cassandra, businesses can harness its querying power for various use cases. For instance, in the realm of connected vehicles, this stored data can inform fleet management systems about vehicle health, optimize route planning based on real-time metrics, or track assets. Similarly, in IIoT settings, the data might be used to monitor machinery health, forecast maintenance, or optimize production schedules.

## Features and Benefits

The data integration with Cassandra offers a range of features and benefits tailored to ensure efficient data transmission, storage, and utilization:

- **Large-Scale Time-Series Data Storage**: EMQX can handle massive device connections and message transmissions. Leveraging Cassandra's high scalability and distributed storage features, it can achieve storage and management of large-scale datasets, including time-series data, and supports time-range based queries and aggregation operations.
- **Real-time Data Streaming**: EMQX is built for handling real-time data streams, ensuring efficient and reliable data transmission from source systems to Cassandra. It enables organizations to capture and analyze data in real-time, making it ideal for use cases requiring immediate insights and actions.
- **High Availability Assurance**: Both EMQX and Cassandra provide clustering capabilities. When used in combination, device connections and data can be distributed across multiple servers. In case of a node failure, the system can automatically switch to other available nodes, thus ensuring high scalability and fault tolerance.
- **Flexibility in Data Transformation:** EMQX provides a powerful SQL-based Rule Engine, allowing organizations to pre-process data before storing it in Cassandra. It supports various data transformation mechanisms, such as filtering, routing, aggregation, and enrichment, enabling organizations to shape the data according to their needs.
- **Flexible Data Model**: Cassandra uses a column-based data model, supporting flexible data schemas and dynamic addition of columns. This is suitable for storing and managing structured device events and message data, and can easily store various MQTT message data.

## Before You Start

This section describes the preparations you need to complete before you start to create a TimescaleDB data bridge, including how to install a Cassandra server and create keyspace and table.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

### Install Cassandra Server

Start the simple Cassandra service via docker:

```bash
docker run --name cassa --rm -p 9042:9042 cassandra:3.11.14
```

### Create Keyspace and Table

You need to create keyspace and tables before you create the data bridge for Cassandra.

1. Create a Keyspace named `mqtt`:

```bash
docker exec -it cassa cqlsh "-e CREATE KEYSPACE mqtt WITH REPLICATION = {'class': 'SimpleStrategy', 'replication_factor': 1}"
```

2. Create a table in Cassandra: `mqtt_msg`:

```bash
docker exec -it cassa cqlsh "-e \
    CREATE TABLE mqtt.mqtt_msg( \
        msgid text, \
        topic text, \
        qos int,    \
        payload text, \
        arrived timestamp, \
        PRIMARY KEY(msgid, topic));"
```

## Create Cassandra Data Bridge

This section demonstrates how to create a Cassandra data bridge in EMQX Dashboard. It assumes that you run both EMQX and Cassandra on the local machine. If you have Cassandra and EMQX running remotely, adjust the settings accordingly.

1. Go to EMQX Dashboard, and click **Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **Cassandra**, and then click **Next**.

4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

5. Input the connection information. Input `127.0.0.1:9042` for the **Servers**, `mqtt` as the **Keyspace**, and leave others as default.

6. Configure the **CQL template** to save `topic`, `id`, `clientid`, `qos`, `palyload` and `timestamp` to Cassandra. This template will be executed via Cassandra Query Language, and the sample code is as follows:

   ```sql
   insert into mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, ${timestamp})
   ```

7. Advanced settings (optional):  Choose whether to use **sync** or **async** query mode as needed. For details, see [Integration](./data-bridges.md).

8. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the Cassandra server.

9. Click **Create** to finish the creation of the data bridge. 

   A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into Cassandra. You can also create rules by following the steps in [Create Rules for Cassandra Data Bridge](#create-rules-for-cassandra-data-bridge).

Now the Cassandra data bridge should appear in the data bridge list (**Integration** -> **Data Bridge**) with **Resource Status** as **Connected**. 

## Create a Rule for Cassandra Data Bridge

Now that you have successfully created the data bridge, you can continue to create rules to specify the data to be stored in Cassandra. 

1. Go to EMQX Dashboard, and click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor**. Suppose you want to forward the MQTT messages under topic `t/#` to Cassandra, you can use the SQL syntax below. 

   Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the data bridge in the `SELECT` part.
   
   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

4. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list, and then select the data bridge you just created under **Data Bridge**. Click the **Add** button. 
6. Click the **Create** button to finish the setup. 

After creating the data bridge to Cassandra. You can click **Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to Cassandra after parsing by rule `my_rule`.

## Test Data Bridge and Rule

Use MQTTX to send messages to topic  `t/1`:

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Cassandra" }'
```

Check the running status of the rule and bridge, the statistical count here should increase somewhat.

Check whether messages are stored into Cassandra with the following command:

```bash
docker exec -it cassa cqlsh "-e SELECT * FROM mqtt.mqtt_msg;"
```
