# Redis Backend

::: tip

After EMQ X version 3.1, a powerful rule engine is introduced to replace plug-ins. It is recommended that you use it. See [Save data to Redis](../rule/backend_redis.md) to setup Save data to Redis in rule engine.

:::

Config file: emqx\_backend\_redis.conf

## Configure the Redis Server

Config Connection Pool of Multiple Redis Servers:

```bash
## Redis Server
backend.redis.pool1.server = 127.0.0.1:6379

## Redis Sentinel
## backend.redis.server = 127.0.0.1:26378

##Redis Sentinel Cluster name
## backend.redis.sentinel = mymaster

## Redis Pool Size
backend.redis.pool1.pool_size = 8

## Redis database
backend.redis.pool1.database = 1

## Redis subscribe channel
backend.redis.pool1.channel = mqtt_channel
```

## Configure Persistence Hooks

```bash
## Expired after seconds, if =< 0 take the default value
backend.redis.msg.expired_after = 3600

## Client Connected Record
backend.redis.hook.client.connected.1    = {"action": {"function": "on_client_connected"}, "pool": "pool1"}

## Subscribe Lookup Record
backend.redis.hook.client.connected.2    = {"action": {"function": "on_subscribe_lookup"}, "pool": "pool1"}

## Client DisConnected Record
backend.redis.hook.client.disconnected.1 = {"action": {"function": "on_client_disconnected"}, "pool": "pool1"}

## Lookup Unread Message for one QOS > 0
backend.redis.hook.session.subscribed.1  = {"topic": "queue/#", "action": {"function": "on_message_fetch_for_queue"}, "pool": "pool1"}

## Lookup Unread Message for many QOS > 0
backend.redis.hook.session.subscribed.2  = {"topic": "pubsub/#", "action": {"function": "on_message_fetch_for_pubsub"}, "pool": "pool1"}

## Lookup Retain Message
backend.redis.hook.session.subscribed.3  = {"action": {"function": "on_retain_lookup"}, "pool": "pool1"}

## Store Publish Message  QOS > 0
backend.redis.hook.message.publish.1     = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}

## Store Retain Message
backend.redis.hook.message.publish.2     = {"topic": "#", "action": {"function": "on_message_retain"}, "pool": "pool1"}

## Delete Retain Message
backend.redis.hook.message.publish.3     = {"topic": "#", "action": {"function": "on_retain_delete"}, "pool": "pool1"}

## Store Ack for one
backend.redis.hook.message.acked.1       = {"topic": "queue/#", "action": {"function": "on_message_acked_for_queue"}, "pool": "pool1"}

## Store Ack for many
backend.redis.hook.message.acked.2       = {"topic": "pubsub/#", "action": {"function": "on_message_acked_for_pubsub"}, "pool": "pool1"}
```

## Description of Persistence Hooks

| hook                | topic     | action/function                 | Description                         |
| ------------------- | --------- | ------------------------------- | ----------------------------------- |
| client.connected    |           | on\_client\_connected           | Store client connected state        |
| client.connected    |           | on\_subscribe\_lookup           | Subscribe to topics                 |
| client.disconnected |           | on\_client\_disconnected        | Store the client disconnected state |
| session.subscribed  | queue/\#  | on\_message\_fetch\_for\_queue  | Fetch one to one offline message    |
| session.subscribed  | pubsub/\# | on\_message\_fetch\_for\_pubsub | Fetch one to many offline message   |
| session.subscribed  | \#        | on\_retain\_lookup              | Lookup retained message             |
| message.publish     | \#        | on\_message\_publish            | Store the published messages        |
| message.publish     | \#        | on\_message\_retain             | Store retained messages             |
| message.publish     | \#        | on\_retain\_delete              | Delete retained messages            |
| message.acked       | queue/\#  | on\_message\_acked\_for\_queue  | Process ACK of one to one messages  |
| message.acked       | pubsub/\# | on\_message\_acked\_for\_pubsub | Process ACK of one to many messages |

## Redis Command Line Parameters

| hook                 | Parameter                                     | Example (Fields separated exactly by one space) |
| -------------------- | --------------------------------------------- | ----------------------------------------------- |
| client.connected     | clientid                                      | SET conn:${clientid} clientid                   |
| client.disconnected  | clientid                                      | SET disconn:${clientid} clientid                |
| session.subscribed   | clientid, topic, qos                          | HSET sub:${clientid} topic qos                  |
| session.unsubscribed | clientid, topic                               | SET unsub:${clientid} topic                     |
| message.publish      | message, msgid, topic, payload, qos, clientid | RPUSH pub:${topic} msgid                        |
| message.acked        | msgid, topic, clientid                        | HSET ack:${clientid} topic msgid                |
| message.delivered    | msgid, topic, clientid                        | HSET delivered:${clientid} topic msgid          |

## Configure 'action' with Redis Commands

Redis backend supports raw 'commands' in 'action',
e.g.:

```bash
## After a client connected to the EMQ X server, it executes a redis command (multiple redis commands also supported)
backend.redis.hook.client.connected.3 = {"action": {"commands": ["SET conn:${clientid} clientid"]}, "pool": "pool1"}
```

## Using Redis Hash for Devices' Connection State

*mqtt:client* Hash for devices' connection state:

```bash
hmset
key = mqtt:client:${clientid}
value = {state:int, online_at:timestamp, offline_at:timestamp}

hset
key = mqtt:node:${node}
field = ${clientid}
value = ${ts}
```
Lookup devices' connection state:

```bash
HGETALL "mqtt:client:${clientId}"
```

E.g.: Client with ClientId 'test' goes online:

```bash
HGETALL mqtt:client:test
1) "state"
2) "1"
3) "online_at"
4) "1481685802"
5) "offline_at"
6) "undefined"
```

Client with ClientId 'test' goes offline:

```bash
HGETALL mqtt:client:test
1) "state"
2) "0"
3) "online_at"
4) "1481685802"
5) "offline_at"
6) "1481685924"
```

## Using Redis Hash for Retained Messages

*mqtt:retain* Hash for retained messages:

```bash
hmset
key = mqtt:retain:${topic}
value = {id: string, from: string, qos: int, topic: string, retain: int, payload: string, ts: timestamp}
```
Lookup retained message:

```bash
HGETALL "mqtt:retain:${topic}"
```
Lookup retained messages with a topic of 'retain':

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

## Using Redis Hash for messages

*mqtt:msg* Hash for MQTT messages:

```bash
hmset
key = mqtt:msg:${msgid}
value = {id: string, from: string, qos: int, topic: string, retain: int, payload: string, ts: timestamp}

zadd
key = mqtt:msg:${topic}
field = 1
value = ${msgid}
```
## Using Redis Set for Message Acknowledgements

*mqtt:acked* SET stores acknowledgements from the clients:

```bash
set
key = mqtt:acked:${clientid}:${topic}
value = ${msgid}
```
## Using Redis Hash for Subscription

*mqtt:sub* Hash for Subscriptions:

```bash
hset
key = mqtt:sub:${clientid}
field = ${topic}
value = ${qos}
```
A client subscribes to a topic:

```bash
HSET mqtt:sub:${clientid} ${topic} ${qos}
```
A client with ClientId of 'test' subscribes to topic1 and topic2:

```bash
HSET "mqtt:sub:test" "topic1" 1
HSET "mqtt:sub:test" "topic2" 2
```
Lookup the subscribed topics of client with ClientId of 'test':

```bash
HGETALL mqtt:sub:test
1) "topic1"
2) "1"
3) "topic2"
4) "2"
```
## Redis SUB/UNSUB Publish

When a device subscribes / unsubscribes to topics, EMQ X broker publish
an event to the Redis:

```bash
PUBLISH
channel = "mqtt_channel"
message = {type: string , topic: string, clientid: string, qos: int}
\*type: [subscribe/unsubscribe]
```
client with ClientID 'test' subscribe to
    'topic0':

```bash
PUBLISH "mqtt_channel" "{\"type\": \"subscribe\", \"topic\": \"topic0\", \"clientid\": \"test\", \"qos\": \"0\"}"
```
Client with ClientId 'test' unsubscribes to
    'test\_topic0':

```bash
PUBLISH "mqtt_channel" "{\"type\": \"unsubscribe\", \"topic\": \"test_topic0\", \"clientid\": \"test\"}"
```
## Enable Redis Backend

```bash
./bin/emqx_ctl plugins load emqx_backend_redis
```
