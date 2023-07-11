# Ingest Data into Cassandra

<!-- 提供一段简介，描述支数据桥接的基本工作方式、关键特性和价值，如果有局限性也应当在此处说明（如必须说明的版本限制、当前未解决的问题）。 -->

Cassandra is a popular open-source, distributed NoSQL database management system.
EMQX's integration with Apache Cassandra provides the ability to store messages and events in Cassandra database.

The current implementation only supports Cassandra v3.x, not yet compatible with v4.x.

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

::: tip

Prerequisites

<!-- 根据情况编写，包含必须的前置知识点、软件版本要求、需要预先创建/初始化的操作。 -->
- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

:::

<!-- 列举功能或性能方面的亮点，如支持批处理、支持异步模式、双向数据桥接，链接到对应的功能介绍章节。 -->

## Feature List

- [Connection pool](./data-bridges.md#connection-pool)
- [Async mode](./data-bridges.md#async-mode)
- [SQL prepared statement](./data-bridges.md#prepared-statement)

<!--  Configuration parameters TODO 链接到配置手册对应配置章节。 -->

## Quick Start Tutorial
<!-- 从安装测试所需步骤，如果有不同的用法增加章节介绍。 -->

This section introduces how to configure a Cassandra data bridge, including how to set up a Cassandra server, configure a Cassandra data bridge to connect to the Cassandra server, and test the data bridge and rule.

This tutorial assumes that you run both EMQX and Cassandra on the local machine. If you have Cassandra and EMQX running remotely, adjust the settings accordingly.

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

### Create Cassandra Data Bridge

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

### Create a Rule for Cassandra Data Bridge

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

### Test Data Bridge and Rule

Use MQTTX to send messages to topic  `t/1`:

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Cassandra" }'
```

Check the running status of the rule and bridge, the statistical count here should increase somewhat.

Check whether messages are stored into Cassandra with the following command:

```bash
docker exec -it cassa cqlsh "-e SELECT * FROM mqtt.mqtt_msg;"
```
