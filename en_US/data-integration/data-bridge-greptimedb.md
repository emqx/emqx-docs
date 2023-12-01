# Ingest MQTT Data into GreptimeDB

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

[GreptimeDB](https://github.com/GreptimeTeam/greptimedb) is an open-source time-series database with a special focus on scalability, analytical capabilities and efficiency. It's designed to work on the infrastructure of the cloud era, and users benefit from its elasticity and commodity storage. EMQX now supports connection to mainstream versions of GreptimeDB, GreptimeCloud or GreptimeDB Enterprise.

This page provides a comprehensive introduction to the data integration between EMQX and GreptimeDB with practical instructions on creating and validating the data integration.

## How It Works

GreptimeDB data integration is a built-in feature in EMQX that combines the real-time data capturing and transmission capabilities of EMQX with the data storage and analysis capabilities of GreptimeDB. With a built-in [rule engine](./rules.md) component, the integration simplifies the process of ingesting data from EMQX to GreptimeDB for storage and analysis, eliminating the need for complex coding. The workflow is as follows:

The diagram below illustrates a typical architecture of data integration between EMQX and GreptimeDB:

![EMQX Integration GreptimeDB](./assets/emqx-integration-greptimedb.png)

1. **Message publication and reception**: Industrial devices establish successful connections to EMQX through the MQTT protocol and regularly publish energy consumption data using the MQTT protocol. This data includes production line identifiers and energy consumption values. When EMQX receives these messages, it initiates the matching process within its rules engine.  
2. **Rule Engine Processes Messages**: The built-in rule engine processes messages from specific sources based on topic matching. When a message arrives, it passes through the rule engine, which matches it with corresponding rules and processes the message data. This can include transforming data formats, filtering specific information, or enriching messages with context information.
3. **Data ingestion into GreptimeDB**: Rules defined in the rule engine trigger operations to write messages to GreptimeDB. The GreptimeDB data bridge provides Line Protocol templates that allow flexible definitions of the data format to write specific message fields to the corresponding tables and columns in GreptimeDB.

After energy consumption data is written to GreptimeDB, you can flexibly use SQL statements or Prometheus query language to analyze the data. For example:

- Connect to visualization tools such as Grafana to generate charts and display energy consumption data.
- Connect to application systems such as ERP for production analysis and production plan adjustments.
- Connect to business systems to perform real-time energy usage analysis, facilitating data-driven energy management.

## Features and Benefits

The data integration with GreptimeDB brings the following features and advantages to your business:

- **Ease of Use**: EMQX and GreptimeDB both offer a user-friendly experience in development and deployment. EMQX provides the standard MQTT protocol along with ready-to-use various authentication, authorization, and clustering features. GreptimeDB offers user-friendly designs like Time-Series Tables and schemaless architecture. The integration of both can accelerate the process of business integration and development.
- **Efficient Data Handling**: EMQX can handle a large number of IoT device connections and message throughput efficiently. GreptimeDB excels in data writing, storage, and querying, meeting the data processing needs of IoT scenarios without overwhelming the system.
- **Message Transformation**: Messages can undergo rich processing and transformation within EMQX rules before being written to GreptimeDB.
- **Efficient Storage and Scalability**: EMQX and GreptimeDB both have cluster scaling capabilities, allowing flexible horizontal scaling as your business grows to meet expanding demands.
- **Advanced Querying Capabilities**: GreptimeDB provides optimized functions, operators, and indexing techniques for efficient querying and analysis of timestamp data, enabling precise insights to be extracted from IoT time-series data.

## Before You Start

This section describes the preparations you need to complete before you start to create a GreptimeDB data bridge, including how to install a GreptimeDB server.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

### Install GreptimeDB Server

1. [Install GreptimeDB](https://greptime.com/download) via Docker, and then run the docker image.

```bash
# TO start the GreptimeDB docker image
docker run -p 4000-4004:4000-4004 \
-p 4242:4242 -v "$(pwd)/greptimedb:/tmp/greptimedb" \
--name greptime --rm \
greptime/greptimedb standalone start \
--http-addr 0.0.0.0:4000 \
--rpc-addr 0.0.0.0:4001 \
--mysql-addr 0.0.0.0:4002 \
--user-provider=static_user_provider:cmd:greptime_user=greptime_pwd

```

2. The `user-provider` parameter configures the GreptimeDB authentication. You can configure it by file. For more information, refer to the [documentation](https://docs.greptime.com/user-guide/clients/authentication#authentication).
3. With GreptimeDB running, visit [http://localhost:4000/dashboard](http://localhost:4000/dashboard) to use the GreptimeDB dashbaord. The username and password are `greptime_user` and `greptime_pwd`.

## Create GreptimeDB Data Bridge

This section demonstrates how to create a GreptimeDB data bridge in EMQX Dashboard. It assumes that you run both EMQX and GreptimeDB on the local machine. If you have GreptimeDB and EMQX running remotely, adjust the settings accordingly.

1. Go to EMQX Dashboard, click **Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **GreptimeDB**, and then click **Next**.

4. Enter a name for the data bridge. The name should be a combination of upper/lower case letters and numbers. 

5. Configure the connection information.
   - **Server Host**: Enter `127.0.0.1:4001`. If you are creating a connection to GreptimeCloud, use 443 as the port by entering `{url}:443`.
   - **Database**: Enter `public`. If you are connecting to GreptimeCloud, enter the service name instead.
   - **Username** and **Password**: Enter `greptime_user` and `greptime_pwd`, which are set in the [Install GreptimeDB Server](#install-greptimedb-server). If you are connecting to GreptimeCloud, enter the service username and password.
   - **Time Precision**: Select `millisecond` by default. 
   - **Enable TLS**: Click the toggle switch to enable the TLS connection if you want to establish a secured connection. For more information on TLS connection, refer to [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access).

6. Configure **Write Syntax**. Specify a text-based format that provides the measurement, tags, fields, and timestamp of a data point, and placeholder supported according to the [InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) syntax. GreptimeDB supports data formats compatible with InfluxDB. <!--Select the data format as **JSON** or **Line Protocol**,-->

   <!--For **JSON** format, define data parsing method, including **Measurement**, **Timestamp**, **Fields,** and **Tags**. Note: All key values can be variables and you can also follow the [InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/) to set them.-->

   <!--For **Line Protocol** format, specify a text-based format that provides the measurement, tags, fields, and timestamp of a data point, and placeholder supported according to the [InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) syntax.-->

   ::: tip

   - To write a signed integer type value to GreptimeDB, add `i` as the type identifier after the placeholder, for example, `${payload.int}i`.
   - To write an unsigned integer type value to GreptimeDB, add `u` as the type identifier after the placeholder, for example, `${payload.int}u`. 

   :::

7. Advanced settings (optional): Choose whether to use **sync** or **async** query mode, and whether to enable queue or batch. For details, see [Configuration](./data-bridges.md).

8. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the GreptimeDB server.

9. Click the **Create** button to finish the setup.

Now the GreptimeDB data bridge should appear in the data bridge list (**Integration** -> **Data Bridge**) with **Resource Status** as `Connected`.

## Create a Rule for GreptimeDB Bridge

You can continue to create rules to specify the data to be saved into GreptimeDB.

1. Go to EMQX Dashboard, and click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor**. Here we want to save the MQTT messages under topic `t/#`  to GreptimeDB, we can use the SQL syntax below. 

   Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the data bridge in the `SELECT` part.

  ```sql
  SELECT
    *
  FROM
    "t/#"
  ```

4. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list, and then select the data bridge we just created under **Data Bridge**. Then click the **Add** button.
5. Click **Create** at the page bottom to finish the creation.

Now a rule to forward data to GreptimeDB via a GreptimeDB bridge is created. You can click **Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to GreptimeDB after parsing by rule  `my_rule`.

## Test Data Bridge and Rule

Use MQTTX  to send a message to topic  `t/1`  to trigger an online/offline event.

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello GreptimeDB" }'
```

Check the running status of the data bridge, there should be one new incoming and one new outgoing message.

In the GreptimeDB dashboard, you can confirm whether the message is written into the GreptimeDB via `SQL`.
