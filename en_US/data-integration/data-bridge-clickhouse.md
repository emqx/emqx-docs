# Clickhouse 

EMQX supports integration with [Clickhouse](https://clickhouse.com/) so you can forward messages and events to Clickhouse. 

{% emqxce %}
:::tip
The Clickhouse bridge is an EMQX Enterprise Edition feature. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

::: tip Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)

- Knowledge about [data bridges](./data-bridges.md)

- Basic knowledge about running commands in a UNIX terminal

:::

## Feature List

- [Connection pool](./data-bridges.md)
- [Async mode](./data-bridges.md)
- [Batch mode](./data-bridges.md)
- [Buffer mode](./data-bridges.md)

## Quick Start Tutorial

Here we will describe how to use the Clickhouse bridge with a practical
tutorial in which we create a Clickouse server, set up a bridge, a rule for
forwarding data to the bridge and finally test that it all works. This tutorial
assumes that you run both EMQX and Clickhouse on the local machine. If you have
Clickhouse and EMQX running somewhere else, you have to adjust the settings
accordingly.

### Start a Clickhouse Server

Here we will describe how to start a Clickhouse server using [Docker](https://www.docker.com/). We assume that you have docker installed on your machine.

First, create a file called `init.sql` with initialization SQL statements:

```bash 
cat >init.sql <<SQL_INIT
CREATE DATABASE IF NOT EXISTS mqtt_data;
CREATE TABLE IF NOT EXISTS mqtt_data.messages (
    data String,
    arrived BIGINT
) ENGINE = Memory;
SQL_INIT
```

Then, start a Clickhouse server using the following command (we map the `init.sql` file to a file in the container so it is executed by Clickhouse after starting):

```bash
docker run \
--rm \
-e CLICKHOUSE_DB=mqtt_data \
-e CLICKHOUSE_USER=emqx \
-e CLICKHOUSE_DEFAULT_ACCESS_MANAGEMENT=1 \
-e CLICKHOUSE_PASSWORD=public \
-p 18123:8123 \
-p19000:9000 \
--ulimit nofile=262144:262144 \
-v ./init.sql:/docker-entrypoint-initdb.d/init.sql \
clickhouse/clickhouse-server
```

You can find more information about running Clickhouse in docker [on dockerhub](https://hub.docker.com/r/clickhouse/clickhouse-server).

### Create a Clickhouse Data Bridge

Let us now create an EMQX data bridge to Clickhouse. We assume that you have already started a Clickhouse server as described in the previous section, that you have an EMQX instance running, and that you can log in the EMQX instance's dashboard.

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.
2. Click **Create** on the top right corner of the page.
3. In the **Create Data Bridge** page, click to select **Clickhouse**, and then click **Next**.
4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.
5. Input the connection information:
   * Input **http://127.0.0.1:18123** as **URL to clickhouse server**
   * Input **mqtt_data** as the **Database Name**
   * Input **emqx** as the **Username**
   * Input **public** as the **Password**
7. Insert the following text in the **SQL Template** text area:
   ```sql
   INSERT INTO messages(data, arrived) VALUES ('${data}', ${timestamp})
   ```
   It is your responsibility to ensure that the SQL statement is correct and not prune to SQL injection attacks etc (this can be accomplished, e.g., by using the rule to escape strings and similar data properly). The `${payload.data}` and `${payload.timestamp}` are placeholders for the data and timestamp of the message coming from the rule we will configure later. The placeholders will be replaced by the actual data before the message is sent to the Clickhouse server.
8. Advanced settings (optional):  Choose whether to use sync or async query mode etc (see the "Advanced Settings" section in the end of this document and [general data bridge documentation](./data-bridges.md) for more details about these settings).
9. Before clicking **Create**, you can click **Test Connection** to test that the bridge can connect to the Clickhouse server.
10. Then click **Create** to finish the creation of the data bridge.

If everything has worked as it should, we have successfully created the data bridge to Clickhouse. You can check that the connection status for the new bridge in the dashboard is "Connected". Now, we can continue to create a rule to forward data to the new Clickhouse bridge. 

### Create a Rule for our Clickhouse Bridge

1. Go to EMQX Dashboard, click **Data Integration** -> **Rules**.
2. Click **Create** on the top right corner of the page.
3. Input, for example, `my_rule` as the rule ID.
4. As a test we want to forward the MQTT messages matching the topic pattern `t/#` to our newly created Clickhouse bridge. 
5. To accomplish this we can paste the following to the SQL editor for the rule:
   ```sql
   SELECT 
     payload as data,
     now_timestamp() as timestamp
   FROM
     "t/#"
   ```
6. Then click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge we just created under **Data bridge**.  
7. Click the **Add** button to finish the setup. 

If everything went well, we have created a rule to forward data to Clickhouse via a clichouse bridge. You can click **Data Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to Clickhouse. 

### Testing our Rule and Bridge

We can use the built-in WebSocket client in the EMQX dashboard to test our rule and bridge.
To open the user interface for configuring a WebSocket client, click **Diagnose** -> **WebSocket Client** (in the main menu to the left).
Now, follow the following steps to set up a WebSocket client and send a message to the topic `t/test`:

1. Fill in the connection information for the current EMQX instance. If you are running EMQX locally, you can use the default values unless you have changed EMQX's default configuration (for example, you might have configured authentication which may require you to type in a username and password). 
2. Click **Connect** to connect the client to the EMQX instance.
3. Scroll down to the publish area and type in the following:
   * Topic: `t/test`
   * Payload: `Hello World Clickhouse from EMQX`
   * QoS: 2
4. Click **Publish** to send the message.

If everything has gone according to the plan, an entry should have been inserted in the table `messages` in the database `mqtt_data` in our Clickhouse server. You can check this by running the following command from a terminal:

```bash
curl -u emqx:public -X POST -d "SELECT * FROM mqtt_data.messages" http://localhost:18123
```

If everything is working correctly the command above should print something like this (obviously, the timestamp will be different):

```
Hello World Clickhouse from EMQX        1679932005
```

## Advanced Settings

The Clickhouse bridge can batch data before sending it to the Clickhouse server to improve efficiency.
This can be configured by setting the **Batch Size**, **Max Batch Wait Time** and **Max buffer queue size**.
You can read more about these settings in the [general data bridge documentation](./data-bridges.md).
The bridge can automatically form an SQL statement to insert more then one data record if the SQL statement for the bridge is a regular `INSERT` statement.
Clickhouse has a special feature that lets the user specify an alternative format for the data being inserted (what comes after the `VALUES` keyword).
See the description of the [Clickhouse FORMAT clause](https://clickhouse.com/docs/en/sql-reference/statements/insert-into) for more information about this feature.
If you are using this feature and have batching enabled, you may need to change the "Batch Value Separator" setting to whatever is appropriate for the format you have chosen.
For example, if you are using the [`JSONCompactEachRow` format](https://clickhouse.com/docs/en/interfaces/formats#jsoncompacteachrow), you should set the "Batch Value Separator" to an empty string.
Please, read the [general data bridge documentation](./data-bridges.md) for information about the remaining settings that we have not discussed in this document.
