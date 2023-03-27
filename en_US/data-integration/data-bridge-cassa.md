# Cassandra

<!-- 提供一段简介，描述支数据桥接的基本工作方式、关键特性和价值，如果有局限性也应当在此处说明（如必须说明的版本限制、当前未解决的问题）。 -->

Cassandra is a popular open-source, distributed NoSQL database management system.
EMQX's integration with Apache Cassandra provides the ability to store messages and events in Cassandra database.

In the currently implementation:
- Only supports Cassandra v3.x, not yet compatible with v4.x.
- Only supports storing data in synchronous manner.

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

## Prerequisites

<!-- 根据情况编写，包含必须的前置知识点、软件版本要求、需要预先创建/初始化的操作。 -->
- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

<!-- 列举功能或性能方面的亮点，如支持批处理、支持异步模式、双向数据桥接，链接到对应的功能介绍章节。 -->

:::

## Feature List

- [Connection pool](./data-bridges.md#Connection pool)
- [SQL preprocessing](./data-bridges.md#Prepared statement)

<!--  Configuration parameters TODO 链接到配置手册对应配置章节。 -->

## Quick Starts
<!-- 从安装测试所需步骤，如果有不同的用法增加章节介绍。 -->

### Install Cassandra

Start the simple Cassandra service via docker:

```bash
docker run --name cassa --rm -p 9042:9042 cassandra:3.11.14
```

### Create Keyspace and Table

Create a Keyspace named `mqtt`:
```bash
docker exec -it cassa cqlsh "-e CREATE KEYSPACE mqtt WITH REPLICATION = {'class': 'SimpleStrategy', 'replication_factor': 1}"
```

Create a table in Cassandra: `mqtt_msg`
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

:::tip
These keyspace and tables must be created before you create the data bridge to Cassandra.
:::

### Create a Data Bridge to Cassandra

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.
2. Click **Create** on the top right corner of the page.
3. In the **Create Data Bridge** page, click to select **Cassandra**, and then click **Next**.
4. Input a name for the data bridge. Note: It should be a combination of upper/lower case letters or numbers.
5. Input the connection information. Input `127.0.0.1:9042` for the **Servers**, `mqtt` as the **Keyspace**, and leave others as default.
6. Click the **Create** button to complete the data bridge creation.

Now you have successfully created the data bridge to Cassandra, you can continue to create rules to specify the data to be stored in Cassandra. 

1. Go to EMQX Dashboard, click **Data Integration** -> **Rules**.
2. Click **Create** on the top right corner of the page.
3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor**. Suppose you want to forward the MQTT messages under topic `t/#` to Cassandra, you can use the SQL syntax below. Note: If you are testing with your SQL, please ensure you have included all required fields in the `SELECT` part. 

  ```sql
  SELECT 
    *
  FROM
    "t/#"
  ```

4. Then click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge we just created under **Data bridge**.  
5. Then, click the **Add** button. 
6. Then click the **Create** button to finish the setup. 

After creating the data bridge to Cassandra. You can click **Data Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to Cassandra after parsing by rule `my_rule`.

### Test

Use MQTTX to send messages to topic  `t/1`:

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Cassandra" }'
```

Check the running status of the rule and bridge, the statistical count here should increase somewhat.

Check whether messages are stored into the Cassandra with the following command:

```bash
docker exec -it cassa cqlsh "-e SELECT * FROM mqtt.mqtt_msg;"
```
