# Redis

EMQX supports integration with Redis so you can save client messages and events to Redis. By leveraging Redis' high performance and flexible data structure, EMQX provides features like message caching and statistics of published/subscribed/discarded messages. 

<!-- TODO 确认是否支持数据发布订阅操作、消息队列等场景。 -->

:::tip Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

:::

## Feature list

- [Connection pool](./data-bridges.md#Connection pool)
- [Async mode](./data-bridges.md#Async mode)
- [Batch mode](./data-bridges.md#Batch mode)
- [Buffer queue](./data-bridges.md#缓存队列)

<!-- TODO 配置参数 需要补充链接到配置手册对应配置章节。 -->

## Quick start

In this section, we will illustrate how to leverage Redis to:

1. Cache the last message of every client;
2. Collect the message discard statistics. 

### Install Redis

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

### Connect to Redis

We will create two Redis data bridges to illustrate the messaging caching and statistics features. The connection configurations for both data bridges are the same. 

1. Go to EMQX Dashboard, click **Data Integration** -> **Data Bridge**.
2. Click **Create** on the top right corner of the page.
3. In the **Create Data Bridge** page, click to select **Redis**, and then click **Next**.
4. Input a name for the data bridge. Note: It should be a combination of upper/lower case letters and numbers.
5. Set **Redis Mode** as the business needs, for example, **single**. 
6. Input the connection information. Input **127.0.0.1:6379** as the **Server Host**, **public** as the **Password**, and **0** for **Database ID**. 

Now we have configured the connection information, for the Redis Command template, the setting differs a little depending on the feature to use. 

### Message caching

This section will illustrate how to use Redis to cache the last message from every client. 

1. Finish the Redis connection configuration. 
2. Configure **Redis Command Template**: Use the Redis [HSET](https://redis.io/commands/hset/) command and hash data structure to store messages, the data format uses `clientid` as the key, and stores fields such as `username`, `payload`, and `timestamp`. To distinguish it from other keys in Redis, we add an `emqx_messages` prefix to the message and separate it with `:`

```bash
# HSET key filed value [field value...]
HSET emqx_messages:${clientid} username ${username} payload ${payload} timestamp ${timestamp}
```

  <!-- TODO 同时执行多个 Redis 命令? -->

3. Advanced settings (optional):  Choose whether to use sync or async query mode as needed. For details, see [Configuration parameters](#Configuration).
4. Then click **Create** to finish the creation of the data bridge. A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into Redis. You can also access the Rule page by clicking **Data Integration** -> **Rules **on EMQX dashboard. 

#### Create rules

1. Click **Create** on the top right corner of the page.
2. Input `cache_to_redis` as the rule ID, and set the rules in the **SQL Editor**. Here we want to save the MQTT messages under topic `t/#`  to PostgreSQL, we can use the SQL syntax below. Note: If you are testing with your SQL, please ensure you have included all required fields in the `SELECT` part. 

```sql
SELECT
  *
FROM
  "t/#"
```

4. Then click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge we just created under **Data bridge**. Then, click the **Add** button. 
4. Then click the **Create** button to finish the setup. 

Now we have successfully finished the configuration for message caching.

### Message discard statistics

This section will illustrate how to use Redis for message discard statistics. 

Note: Except for the Redis Command template and rules, the other settings are the same as the [Message caching](#消息暂存) section. 

**Redis Command template**

Use [HINCRBY](https://redis.io/commands/hincrby/) command below to collect the discarded messages under this topic.

```bash
# HINCRBY key field increment
HINCRBY emqx_message_dropped_count ${topic} 1
```

One will be added to the corresponding counter every time the command is executed.

**Rule SQL**

EMQX rules define 2 message discarding events, through which the rules can be triggered and recorded in Redis:

| Event                                    | Topic                    | Parameter                                                    |
| ---------------------------------------- | ------------------------ | ------------------------------------------------------------ |
| Messages are discarded during forwarding | $events/message_dropped  | [$events/message_dropped](./rule-sql-events-and-fields.md#消息在转发的过程中被丢弃事件-events-message-dropped) |
| Messages are discarded during delivery   | $events/delivery_dropped | [$events/delivery_dropped](./rule-sql-events-and-fields.md#消息在投递的过程中被丢弃事件-events-delivery-dropped) |

The corresponding SQL is as follows: 

```sql
SELECT
  *
FROM
  "$events/message_dropped", "$events/delivery_dropped"
```

### Test

Use MQTT X  to send a message to topic  `t/1`  to trigger a message caching event. If topic  `t/1`  does not have any subscribers, the message will be discarded and trigger the message discard rule. 

```bash
mqttx pub -i emqx_c -u emqx_u -t t/1 -m '{ "msg": "hello Redis" }'
```

Check the running status of the two data bridges, there should be one new Matched and one Sent Succesfully message. 

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

