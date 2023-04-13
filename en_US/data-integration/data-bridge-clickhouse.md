# Ingest Data into ClickHouse 

ClickHouse is a high-performance distributed database management system designed for processing large-scale data. It features excellent query performance, a flexible data model, and scalable distributed architecture, making it suitable for various data analytics scenarios.

EMQX supports integration with [ClickHouse](https://clickhouse.com/) so you can save messages and events data to ClickHouse. 

{% emqxce %}
:::tip
The ClickHouse bridge is an EMQX Enterprise Edition feature. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

::: tip Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)

- Knowledge about [data bridges](./data-bridges.md)

- Basic knowledge of UNIX terminal and commands 

:::

## Feature List

- [Connection pool](./data-bridges.md)
- [Async mode](./data-bridges.md)
- [Batch mode](./data-bridges.md)
- [Buffer mode](./data-bridges.md)

## Quick Start Tutorial

This section introduces how to use the ClickHouse bridge with a practical tutorial, covering topics like how to create a ClickHouse server, how to set up a bridge, and how to set up a rule for forwarding data to the bridge and testing that it all works. 

This tutorial assumes that you run both EMQX and ClickHouse on the local machine. If you have ClickHouse and EMQX running remotely, please adjust the settings accordingly.

### Start a ClickHouse Server

This section introduces how to start a ClickHouse server using [Docker](https://www.docker.com/). 

1. Create a file called `init.sql` with the following initialization SQL statements, which will help to initialize the database when the container starts up.

   ```bash
   cat >init.sql <<SQL_INIT
   CREATE DATABASE IF NOT EXISTS mqtt_data;
   CREATE TABLE IF NOT EXISTS mqtt_data.messages (
       data String,
       arrived UnixTimestamp
   ) ENGINE = MergeTree();
   SQL_INIT
   ```

2. Then, start a ClickHouse server using the following command, which defines the database name, port number, user name and password. And it will also mount the `init.sql` file in the current directory to the docker directory.

   ```bash
   docker run \
   --rm \
   -e CLICKHOUSE_DB=mqtt_data \
   -e CLICKHOUSE_USER=emqx \
   -e CLICKHOUSE_DEFAULT_ACCESS_MANAGEMENT=1 \
   -e CLICKHOUSE_PASSWORD=public \
   -p 18123:8123 \
   -p 19000:9000 \
   --ulimit nofile=262144:262144 \
   -v ./init.sql:/docker-entrypoint-initdb.d/init.sql \
   clickhouse/clickhouse-server
   ```

You can find more information about running ClickHouse in docker [on dockerhub](https://hub.docker.com/r/clickhouse/clickhouse-server).

### Create a ClickHouse Data Bridge

Then you can start to create an EMQX data bridge to ClickHouse.

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **ClickHouse**, and then click **Next**.

4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

5. Input the connection information:
   *  **Server URL**: Input **http://127.0.0.1:18123**, or the actual URL if the ClickHouse server is running remotely.
   * **Database Name**: Input **mqtt_data**.
   * **Username**: Input **emqx**.
   * **Password**: Input **public**
   
7. **Batch Separator** (optional): In this example, you can keep the default value ",". This setting only needs to be changed if you enable [batching](./data-bridges.md) for the bridge and if you specify an alternative format with [ClickHouse's FORMAT syntax](https://clickhouse.com/docs/en/sql-reference/statements/insert-into).
   
7. Insert the following text in the **SQL Template** text area (You can use [Rule Engine](./rules.md) to ensure that strings in the specified SQL statement are escaped so the SQL statement is not vulnerable to SQL injection attacks.):
   
   ```sql
   INSERT INTO messages(data, arrived) VALUES ('${data}', ${timestamp})
   ```
   The `${data}` and `${timestamp}` are placeholders for the data and timestamp of the message coming from the [rule](#create-a-rule-for-our-clickhouse-bridge) you will configure later. The placeholders will be replaced by the actual data before the message is sent to the ClickHouse server.
   
7. Advanced settings (optional):  Choose whether to use sync or async query mode etc, or batch mode. For details about these advanced features, see the [general data bridge documentation](./data-bridges.md).

8. Before clicking **Create**, you can click **Test Connection** to test that the bridge can connect to the ClickHouse server.

10. Then click **Create** to finish the creation of the data bridge.

Now the ClickHouse data bridge should appear in the data bridge list (**Data Integration** -> **Data Bridge**) with **Resource Status** as **Connected**. You can continue to create a rule to forward data to the new ClickHouse bridge. 

### Create a Rule for the ClickHouse Bridge

1. Go to EMQX Dashboard, click **Data Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input, for example, `my_rule` as the rule ID.

5. Input the following statement in the SQL editor, which will forward the MQTT messages matching the topic pattern `t/#`. 
   
   ```sql
   SELECT 
     payload as data,
     now_timestamp() as timestamp
   FROM
     "t/#"
   ```
   
5. Then click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge you just created under **Data bridge**.  

7. Click the **Add** button to finish the setup. 
8. Click the **Create** button at the page bottom to finish the setup. 

Now a rule to forward data to ClickHouse via a ClickHouse bridge is created. You can click **Data Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to ClickHouse. 

### Test the Rule and Bridge

You can use the built-in WebSocket client in the EMQX dashboard to test our rule and bridge.

Click **Diagnose** -> **WebSocket Client** in the left navigation menu of the Dashboard to access the WebSocket Client. Follow the steps below to set up a WebSocket client and send a message to the topic `t/test`:

1. Fill in the connection information for the current EMQX instance. If you are running EMQX locally, you can use the default values unless you have changed EMQX's default configuration (for example, you might have configured authentication which may require you to type in a username and password). 
2. Click **Connect** to connect the client to the EMQX instance.
3. Scroll down to the publish area and type in the following:
   * **Topic**: `t/test`
   * **Payload**: `Hello World Clickhouse from EMQX`
   * **QoS**: 2
4. Click **Publish** to send the message.

If everything has gone according to the plan, an entry should have been inserted in the table `messages` in the database `mqtt_data` in the ClickHouse server. You can check this by running the following command from a terminal:

```bash
curl -u emqx:public -X POST -d "SELECT * FROM mqtt_data.messages" http://localhost:18123
```

If everything is working correctly the command above should print something like this (obviously, the timestamp will be different):

```
Hello World Clickhouse from EMQX        1679932005
```
