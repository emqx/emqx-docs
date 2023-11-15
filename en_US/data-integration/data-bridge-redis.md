# Ingest MQTT Data into Redis

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

[Redis](https://redis.io/) is an open-source, in-memory data store used by millions of developers as a database, cache, streaming engine, and message broker. EMQX supports integration with Redis so you can save MQTT messages and client events to Redis. With Redis data bridge, you can use Redis for message caching and statistics of published/subscribed/discarded messages.

This page provides a comprehensive introduction to the data integration between EMQX and Redis with practical instructions on creating a rule and data bridge.

## How It Works

Redis data integration is an out-of-the-box feature in EMQX designed to bridge the gap between MQTT-based IoT data and Redis's powerful data storage capabilities. With a built-in [rule engine](./rules.md) component, the integration simplifies the process of ingesting data from EMQX to Redis for storage and management, eliminating the need for complex coding.

<!-- The diagram below illustrates a typical architecture of data integration between EMQX and Redis. -->

Ingesting MQTT data into Redis works as follows:

1. **Message publication and reception**: Industrial IoT devices establish successful connections to EMQX through the MQTT protocol and publish real-time MQTT data from machines, sensors, and product lines based on their operational states, readings, or triggered events to EMQX. When EMQX receives these messages, it initiates the matching process within its rules engine.  
2. **Message data processing:** When a message arrives, it passes through the rule engine and is then processed by the rule defined in EMQX. The rules, based on predefined criteria, determine which messages need to be routed to Redis. If any rules specify payload transformations, those transformations are applied, such as converting data formats, filtering out specific information, or enriching the payload with additional context.
3. **Data ingestion into Redis**: Once the rule engine identifies a message for Redis storage, it triggers an action of forwarding the messages to Redis. Processed data will be seamlessly written into the collection of the Redis database.
4. **Data Storage and Utilization**: With the data now stored in Redis, businesses can harness its querying power for various use cases. For instance, in logistics and supply chain management fields, data from IoT devices such as GPS trackers, temperature sensors, and inventory management systems can be monitored and analyzed for real-time tracking, route optimization, demand forecasting, and efficient inventory management.

## Features and Benefits

The data integration with Redis offers a range of features and benefits tailored to ensure efficient data transmission, storage, and utilization:

- **Real-time Data Streaming**: EMQX is built for handling real-time data streams, ensuring efficient and reliable data transmission from source systems to Redis. It enables organizations to capture and analyze data in real-time, making it ideal for use cases requiring immediate insights and actions.
- **High Performance and Scalability**: EMQX's distributed architecture and Redis's columnar storage format enable seamless scalability as data volumes increase. This ensures consistent performance and responsiveness, even with large datasets.
- **Flexibility in Data Transformation:** EMQX provides a powerful SQL-based Rule Engine, allowing organizations to pre-process data before storing it in Redis. It supports various data transformation mechanisms, such as filtering, routing, aggregation, and enrichment, enabling organizations to shape the data according to their needs.
- **Easy Deployment and Management:** EMQX provides a user-friendly interface for configuring data sources, pre-processing data rules, and Redis storage settings. This simplifies the setup and ongoing management of the data integration process.
- **Advanced Analytics:** Redis's powerful SQL-based query language and support for complex analytical functions empower users to gain valuable insights from IoT data, enabling predictive analytics, anomaly detection, and more.

## Before You Start

This section describes the preparations you need to complete before you start to create the Redis data bridges, including how to set up the Redis server.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

### Install Redis Server

Install and run Redis via Docker:

```bash
# Start a Redis container and set the password to public
docker run --name redis -p 6379:6379 -d redis --requirepass "public"

# Access the container
docker exec -it redis bash

# Access the Redis server, use the AUTH command for authentication
redis-cli
127.0.0.1:6379> AUTH public
OK

# Verify the installation
127.0.0.1:6379> set emqx "Hello World"
OK
127.0.0.1:6379> get emqx
"Hello World"
```

Now you have successfully installed Redis and verified the installation with the `SET` and `GET` commands. For more Redis commands, see [Redis Commands](https://redis.io/commands/).

### Create Redis Data Bridge

This section introduces how to configure the Redis data bridges to:

- Cache the last message of every client.
- Collect the message discard statistics.

It assumes that you run both EMQX and Redis on the local machine. If you have Redis and EMQX running remotely, adjust the settings accordingly.

You need to create 2 separate Redis data bridges for the messaging caching and statistics features. Follow the same connection configurations for both data bridges types, but you need to configure different **Redis Command Template** in the specific configuration step.

1. Go to EMQX Dashboard, and click **Integration** -> **Data Bridge**.
2. Click **Create** on the top right corner of the page. In the **Create Data Bridge** page, click to select **Redis**, and then click **Next**.
4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.
5. Set **Redis Mode** as the business needs, for example, **single**.
6. Input the connection information. Input `127.0.0.1:6379` as the **Server Host**, `public` as the **Password**, and `0` for **Database ID**.

6. Configure **Redis Command Template** based on the feature to use:

   - To create data bridge for message caching, use the Redis [HSET](https://redis.io/commands/hset/) command and hash data structure to store messages, the data format uses `clientid` as the key, and stores fields such as `username`, `payload`, and `timestamp`. To distinguish it from other keys in Redis, add an `emqx_messages` prefix to the message and separate it with `:`

     ```bash
     # HSET key filed value [field value...]
     HSET emqx_messages:${clientid} username ${username} payload ${payload} timestamp ${timestamp}
     ```

     <!-- TODO 同时执行多个 Redis 命令? -->

   - To create data bridge for message discard statistics, use [HINCRBY](https://redis.io/commands/hincrby/) command below to collect the discarded messages under every topic.

     ```bash
     # HINCRBY key field increment
     HINCRBY emqx_message_dropped_count ${topic} 1
     ```

     Each time the command is executed, the corresponding counter is incremented by 1.

7. Advanced settings (optional):  Choose whether to use **sync** or **async** query mode as needed. For details, see [Configuration](./data-bridges.md#configuration).

8. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the Redis server.

9. Click **Create** to finish the creation of the data bridge.

   A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into Redis. You can also create rules by following the steps in [Create Rules for Redis Data Bridge](#create-rules-for-redis-data-bridge).

Now the Redis data bridge should appear in the data bridge list (**Integration** -> **Data Bridge**) with **Resource Status** as **Connected**.

### Create Rules for Redis Data Bridge

After you successfully created the data bridge to Redis, you can continue to create rules for message caching and message discard statistics.

1. Go to EMQX Dashboard, click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

4. Input `cache_to_redis` as the rule ID, and set the rules in the **SQL Editor** based on the feature to use:

   - To create a rule for message caching, input the following statement, which means the MQTT messages under topic `t/#`  will be saved to Redis.

     Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the data bridge in the `SELECT` part.

     ```bash
     SELECT
       *
     FROM
       "t/#"
     ```

   - To create a rule for message discard statistics, input the following statement.

     ```bash
     SELECT
       *
     FROM
       "$events/message_dropped", "$events/delivery_dropped"
     ```

     EMQX rules define 2 message discarding events, through which the rules can be triggered and recorded in Redis:

     | Event                                    | Topic                    | Parameter                                                    |
     | ---------------------------------------- | ------------------------ | ------------------------------------------------------------ |
     | Messages are discarded during forwarding | $events/message_dropped  | [$events/message_dropped](./rule-sql-events-and-fields.md#events-message-dropped) |
     | Messages are discarded during delivery   | $events/delivery_dropped | [$events/delivery_dropped](./rule-sql-events-and-fields.md#events-delivery-dropped) |

5. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge we just created under **Data Bridge**. Click the **Add** button.

6. Click the **Create** button to finish the setup.

Now you have successfully finished creating the rules for the Redis data bridge. You can click **Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#` are sent and saved to Redis.

### Test the Data Bridge and Rule

Use MQTTX  to send a message to topic  `t/1`  to trigger a message caching event. If topic  `t/1`  does not have any subscribers, the message will be discarded and trigger the message discard rule.

```bash
mqttx pub -i emqx_c -u emqx_u -t t/1 -m '{ "msg": "hello Redis" }'
```

Check the running status of the two data bridges, there should be one new Matched and one Sent Successfully message.

Check whether the message is cached.

```bash
127.0.0.1:6379> HGETALL emqx_messages:emqx_c
1) "username"
2) "emqx_u"
3) "payload"
4) "{ \"msg\": \"hello Redis\" }"
5) "timestamp"
6) "1675263885119"
```

Rerun the test, the `timestamp` field should be updated.

Check whether the discarded messages are collected:

```bash
127.0.0.1:6379> HGETALL emqx_message_dropped_count
1) "t/1"
2) "1"
```

Repeat the test, the number on the counter corresponding to `t/1` should also increase.
