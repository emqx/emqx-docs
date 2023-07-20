# 集成 Redis

::: tip

EMQX 3.1 版本后推出强大的规则引擎用于替换插件，建议您前往使用[保存数据到 Redis](../rule/backend_redis.md)规则引擎中创建 保存数据到 Redis

:::

配置文件: emqx_backend_redis.conf

## 配置 Redis 服务器

支持配置多台 Redis 服务器连接池:

```bash
## Redis 服务集群类型: single | sentinel | cluster
backend.redis.pool1.type = single

## Redis 服务器地址列表
backend.redis.pool1.server = 127.0.0.1:6379

## Redis sentinel 模式下的 sentinel 名称
## backend.redis.pool1.sentinel = mymaster

## Redis 连接池大小
backend.redis.pool1.pool_size = 8

## Redis 数据库名称
backend.redis.pool1.database = 0

## Redis 密码
## backend.redis.pool1.password =

## 订阅的 Redis channel 名称
backend.redis.pool1.channel = mqtt_channel
```

## 配置 Redis 存储规则

```bash
backend.redis.hook.client.connected.1    = {"action": {"function": "on_client_connected"}, "pool": "pool1"}
backend.redis.hook.session.created.1     = {"action": {"function": "on_subscribe_lookup"}, "pool": "pool1"}
backend.redis.hook.client.disconnected.1 = {"action": {"function": "on_client_disconnected"}, "pool": "pool1"}
backend.redis.hook.session.subscribed.1  = {"topic": "queue/#", "action": {"function": "on_message_fetch_for_queue"}, "pool": "pool1"}
backend.redis.hook.session.subscribed.2  = {"topic": "pubsub/#", "action": {"function": "on_message_fetch_for_pubsub"}, "pool": "pool1"}
backend.redis.hook.session.subscribed.3  = {"action": {"function": "on_retain_lookup"}, "pool": "pool1"}
backend.redis.hook.session.unsubscribed.1= {"topic": "#", "action": {"commands": ["DEL mqtt:acked:${clientid}:${topic}"]}, "pool": "pool1"}
backend.redis.hook.message.publish.1     = {"topic": "#", "action": {"function": "on_message_publish"}, "expired_time" : 3600, "pool": "pool1"}
backend.redis.hook.message.publish.2     = {"topic": "#", "action": {"function": "on_message_retain"}, "expired_time" : 3600, "pool": "pool1"}
backend.redis.hook.message.publish.3     = {"topic": "#", "action": {"function": "on_retain_delete"}, "pool": "pool1"}
backend.redis.hook.message.acked.1       = {"topic": "queue/#", "action": {"function": "on_message_acked_for_queue"}, "pool": "pool1"}
backend.redis.hook.message.acked.2       = {"topic": "pubsub/#", "action": {"function": "on_message_acked_for_pubsub"}, "pool": "pool1"}

## backend.redis.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch_for_keep_latest"}, "pool": "pool1"}
## backend.redis.hook.message.publish.1     = {"topic": "#", "action": {"function": "on_message_store_keep_latest"}, "expired_time" : 3600, "pool": "pool1"}
## backend.redis.hook.message.acked.1       = {"topic": "#", "action": {"function": "on_message_acked_for_keep_latest"}, "pool": "pool1"}
```

## Redis 存储规则说明

| hook                 | topic     | action/function                 | 说明           |
| -------------------- | --------- | ------------------------------- | ------------ |
| client.connected     |           | on_client_connected           | 存储客户端在线状态    |
| session.created      |           | on_subscribe_lookup           | 订阅主题         |
| client.disconnected  |           | on_client_disconnected        | 存储客户端离线状态    |
| session.subscribed   | queue/#  | on_message_fetch_for_queue  | 获取一对一离线消息    |
| session.subscribed   | pubsub/# | on_message_fetch_for_pubsub | 获取一对多离线消息    |
| session.subscribed   | #        | on_retain_lookup              | 获取 retain 消息 |
| session.unsubscribed | #        |                                 | 删除 acked 消息  |
| message.publish      | #        | on_message_publish            | 存储发布消息       |
| message.publish      | #        | on_message_retain             | 存储 retain 消息 |
| message.publish      | #        | on_retain_delete              | 删除 retain 消息 |
| message.acked        | queue/#  | on_message_acked_for_queue  | 一对一消息 ACK 处理 |
| message.acked        | pubsub/# | on_message_acked_for_pubsub | 一对多消息 ACK 处理 |

## Redis 命令行参数说明

| hook                 | 可用参数                                          | 示例(每个字段分隔，必须是一个空格)                         |
| -------------------- | --------------------------------------------- | ------------------------------------------ |
| client.connected     | clientid                                      | SET conn:${clientid} ${clientid}           |
| client.disconnected  | clientid                                      | SET disconn:${clientid} ${clientid}        |
| session.subscribed   | clientid, topic, qos                          | HSET sub:${clientid} ${topic} ${qos}       |
| session.unsubscribed | clientid, topic                               | SET unsub:${clientid} ${topic}             |
| message.publish      | message, msgid, topic, payload, qos, clientid | RPUSH pub:${topic} ${msgid}                |
| message.acked        | msgid, topic, clientid                        | HSET ack:${clientid} ${topic} ${msgid}     |
| message.deliver      | msgid, topic, clientid                        | HSET deliver:${clientid} ${topic} ${msgid} |

## Redis 命令行配置 Action

Redis 存储支持用户采用 Redis Commands 语句配置 Action，例如:

```bash
## 在客户端连接到 EMQX 服务器后，执行一条 redis
backend.redis.hook.client.connected.3 = {"action": {"commands": ["SET conn:${clientid} ${clientid}"]}, "pool": "pool1"}
```

## Redis 设备在线状态 Hash

*mqtt:client* Hash 存储设备在线状态:

```bash
hmset
key = mqtt:client:${clientid}
value = {state:int, online_at:timestamp, offline_at:timestamp}

hset
key = mqtt:node:${node}
field = ${clientid}
value = ${ts}
```

查询设备在线状态:
```bash
HGETALL "mqtt:client:${clientId}"
```
例如 ClientId 为 test 客户端上线:

```bash
HGETALL mqtt:client:test
1) "state"
2) "1"
3) "online_at"
4) "1481685802"
5) "offline_at"
6) "undefined"
```

例如 ClientId 为 test 客户端下线:

```bash
HGETALL mqtt:client:test
1) "state"
2) "0"
3) "online_at"
4) "1481685802"
5) "offline_at"
6) "1481685924"
```

## Redis 保留消息 Hash

*mqtt:retain* Hash 存储 Retain 消息:

```bash
hmset
key = mqtt:retain:${topic}
value = {id: string, from: string, qos: int, topic: string, retain: int, payload: string, ts: timestamp}
```

查询 retain 消息:

```bash
HGETALL "mqtt:retain:${topic}"
```

例如查看 topic 为 topic 的 retain 消息:

```bash
    HGETALL mqtt:retain:topic
     1) "id"

>   -     2) "6P9NLcJ65VXBbC22sYb4"
>     3)  "from"
>   -     4) "test"
>     5)  "qos"
>     6)  "1"
>     7)  "topic"
>     8)  "topic"
>     9)  "retain"
>   - 10\) "true"
>     11) "payload"
>     12) "Hello world\!"
>     13) "ts"
>     14) "1481690659"
```

## Redis 消息存储 Hash

*mqtt:msg* Hash 存储 MQTT 消息:

```bash
hmset
key = mqtt:msg:${msgid}
value = {id: string, from: string, qos: int, topic: string, retain: int, payload: string, ts: timestamp}

zadd
key = mqtt:msg:${topic}
field = 1
value = ${msgid}
```


## Redis 消息确认 SET

*mqtt:acked* SET 存储客户端消息确认:

```bash
set
key = mqtt:acked:${clientid}:${topic}
value = ${msgid}
```

## Redis 订阅存储 Hash

*mqtt:sub* Hash 存储订阅关系:

```bash
hset
key = mqtt:sub:${clientid}
field = ${topic}
value = ${qos}
```

某个客户端订阅主题:

```bash
HSET mqtt:sub:${clientid} ${topic} ${qos}
```

例如为 ClientId 为 test 的客户端订阅主题 topic1, topic2 :


```bash
HSET "mqtt:sub:test" "topic1" 1
HSET "mqtt:sub:test" "topic2" 2
```

查询 ClientId 为 test 的客户端已订阅主题:

```bash
HGETALL mqtt:sub:test
1) "topic1"
2) "1"
3) "topic2"
4) "2"
```

## Redis SUB/UNSUB 事件发布

设备需要订阅/取消订阅主题时，业务服务器向 Redis 发布事件消息:

```bash
PUBLISH
channel = "mqtt_channel"
message = {type: string , topic: string, clientid: string, qos: int}
\*type: [subscribe/unsubscribe]
```

例如 ClientId 为 test 客户端订阅主题 topic0:

```bash
PUBLISH "mqtt_channel" "{\"type\": \"subscribe\", \"topic\": \"topic0\", \"clientid\": \"test\", \"qos\": \"0\"}"
```

例如 ClientId 为 test 客户端取消订阅主题:

```bash
PUBLISH "mqtt_channel" "{\"type\": \"unsubscribe\", \"topic\": \"test_topic0\", \"clientid\": \"test\"}"
```

::: tip
Redis Cluster 无法使用 Redis PUB/SUB 功能。
:::

## 启用 Redis 数据存储插件

```bash
./bin/emqx_ctl plugins load emqx_backend_redis
```



