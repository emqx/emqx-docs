# MongoDB Backend

::: tip

After EMQX version 3.1, a powerful rule engine is introduced to replace plug-ins. It is recommended that you use it. See [Save data to MongoDB](../rule/backend_mongodb.md) to setup Save data to MongoDB in rule engine.

:::


Config file: emqx\_backend\_mongo.conf

## Configure MongoDB Server

Connection pool of multiple MongoDB servers is supported:

```bash
## MongoDB Server Pools
## Mongo Topology Type single|unknown|sharded|rs
backend.mongo.pool1.type = single

## If type rs, need config setname
## backend.mongo.pool1.rs_set_name = testrs

## Mongo Server 127.0.0.1:27017,127.0.0.2:27017...
backend.mongo.pool1.server = 127.0.0.1:27017

## MongoDB Pool Size
backend.mongo.pool1.c_pool_size = 8

## MongoDB Database
backend.mongo.pool1.database = mqtt

## Mongo User
## backend.mongo.pool1.login =  emqtt
## Mongo Password
## backend.mongo.pool1.password = emqtt

## MongoDB AuthSource
## Value: String
## Default: mqtt
## backend.mongo.pool1.auth_source = admin

## Whether to enable SSL connection.
##
## Value: true | false
## backend.mongo.pool1.ssl = false

## SSL keyfile.
##
## Value: File
## backend.mongo.pool1.keyfile =

## SSL certfile.
##
## Value: File
## backend.mongo.pool1.certfile =

## SSL cacertfile.
##
## Value: File
## backend.mongo.pool1.cacertfile =

# Value: unsafe | safe
## backend.mongo.pool1.w_mode = safe
## Value: master | slave_ok
## backend.mongo.pool1.r_mode = slave_ok

## Mongo Topology Options
## backend.mongo.topology.pool_size = 1
## backend.mongo.topology.max_overflow = 0
## backend.mongo.topology.overflow_ttl = 1000
## backend.mongo.topology.overflow_check_period = 1000
## backend.mongo.topology.local_threshold_ms = 1000
## backend.mongo.topology.connect_timeout_ms = 20000
## backend.mongo.topology.socket_timeout_ms = 100
## backend.mongo.topology.server_selection_timeout_ms = 30000
## backend.mongo.topology.wait_queue_timeout_ms = 1000
## backend.mongo.topology.heartbeat_frequency_ms = 10000
## backend.mongo.topology.min_heartbeat_frequency_ms = 1000
## Max number of fetch offline messages. Without count limit if infinity
## backend.mongo.max_returned_count = 500

## Time Range. Without time limit if infinity
## d - day
## h - hour
## m - minute
## s - second
## backend.mongo.time_range = 2h
```

## Configure MongoDB Persistence Hooks

```bash
## Client Connected Record
backend.mongo.hook.client.connected.1    = {"action": {"function": "on_client_connected"}, "pool": "pool1"}

## Subscribe Lookup Record
backend.mongo.hook.client.connected.2    = {"action": {"function": "on_subscribe_lookup"}, "pool": "pool1"}

## Client DisConnected Record
backend.mongo.hook.client.disconnected.1 = {"action": {"function": "on_client_disconnected"}, "pool": "pool1"}

## Lookup Unread Message QOS > 0
backend.mongo.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch"}, "pool": "pool1"}

## Lookup Retain Message
backend.mongo.hook.session.subscribed.2  = {"topic": "#", "action": {"function": "on_retain_lookup"}, "pool": "pool1"}

## Store Publish Message  QOS > 0, payload_format options mongo_json | plain_text
backend.mongo.hook.message.publish.1     = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1", "payload_format": "mongo_json"}

## Store Retain Message, payload_format options mongo_json | plain_text
backend.mongo.hook.message.publish.2     = {"topic": "#", "action": {"function": "on_message_retain"}, "pool": "pool1", "payload_format": "mongo_json"}

## Delete Retain Message
backend.mongo.hook.message.publish.3     = {"topic": "#", "action": {"function": "on_retain_delete"}, "pool": "pool1"}

## Store Ack
backend.mongo.hook.message.acked.1       = {"topic": "#", "action": {"function": "on_message_acked"}, "pool": "pool1"}

## Get offline messages
### "offline_opts": Get configuration for offline messages
### max_returned_count: Maximum number of offline messages get at a time
### time_range: Get only messages in the current time range
## backend.mongo.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch"}, "offline_opts": {"max_returned_count": 500, "time_range": "2h"}, "pool": "pool1"}

## If you need to store Qos0 messages, you can enable the following configuration
## Tip: When the following configuration is enabled, 'on_message_fetch' needs to be disabled, otherwise qos1, qos2 messages will be stored twice
## backend.mongo.hook.message.publish.4     = {"topic": "#", "action": {"function": "on_message_store"}, "pool": "pool1"}
```

## Description of MongoDB Persistence Hooks

| hook                | topic | action                   | Description                     |
| ------------------- | ----- | ------------------------ | ------------------------------- |
| client.connected    |       | on\_client\_connected    | Store client connected state    |
| client.connected    |       | on\_subscribe\_lookup    | Subscribed topics               |
| client.disconnected |       | on\_client\_disconnected | Store client disconnected state |
| session.subscribed  | \#    | on\_message\_fetch       | Fetch offline messages          |
| session.subscribed  | \#    | on\_retain\_lookup       | Lookup retained messages        |
| message.publish     | \#    | on\_message\_publish     | Store published messages        |
| message.publish     | \#    | on\_message\_retain      | Store retained messages         |
| message.publish     | \#    | on\_retain\_delete       | Delete retained messages        |
| message.acked       | \#    | on\_message\_acked       | Process ACK                     |

## Create MongoDB DB & Collections

```js
use mqtt
db.createCollection("mqtt_client")
db.createCollection("mqtt_sub")
db.createCollection("mqtt_msg")
db.createCollection("mqtt_retain")
db.createCollection("mqtt_acked")

db.mqtt_client.ensureIndex({clientid:1, node:2})
db.mqtt_sub.ensureIndex({clientid:1})
db.mqtt_msg.ensureIndex({sender:1, topic:2})
db.mqtt_retain.ensureIndex({topic:1})
```

::: tip
DB name is free of choice
:::

## MongoDB MQTT Client Collection

*mqtt\_client* stores MQTT clients' connection states:

```js
{
    clientid: string,
    state: 0,1, //0 disconnected 1 connected
    node: string,
    online_at: timestamp,
    offline_at: timestamp
}
```

Query client's connection state:

```js
db.mqtt_client.findOne({clientid: ${clientid}})
```

E.g., if client 'test' is online:

```js
db.mqtt_client.findOne({clientid: "test"})

{
    "_id" : ObjectId("58646c9bdde89a9fb9f7fb73"),
    "clientid" : "test",
    "state" : 1,
    "node" : "emqx@127.0.0.1",
    "online_at" : 1482976411,
    "offline_at" : null
}
```

Client 'test' is offline:

```js
db.mqtt_client.findOne({clientid: "test"})

{
    "_id" : ObjectId("58646c9bdde89a9fb9f7fb73"),
    "clientid" : "test",
    "state" : 0,
    "node" : "emq@127.0.0.1",
    "online_at" : 1482976411,
    "offline_at" : 1482976501
}
```

## MongoDB Subscription Collection

*mqtt\_sub* stores subscriptions of clients:

```js
{
    clientid: string,
    topic: string,
    qos: 0,1,2
}
```

E.g., client 'test' subscribes to topic 'test\_topic1' and
'test\_topic2':

```js
db.mqtt_sub.insert({clientid: "test", topic: "test_topic1", qos: 1})
db.mqtt_sub.insert({clientid: "test", topic: "test_topic2", qos: 2})
```

Query subscription of client 'test':

```js
db.mqtt_sub.find({clientid: "test"})

{ "_id" : ObjectId("58646d90c65dff6ac9668ca1"), "clientid" : "test", "topic" : "test_topic1", "qos" : 1 }
{ "_id" : ObjectId("58646d96c65dff6ac9668ca2"), "clientid" : "test", "topic" : "test_topic2", "qos" : 2 }
```

## MongoDB Message Collection

*mqtt\_msg* stores MQTT messages:

```js
{
    _id: int,
    topic: string,
    msgid: string,
    sender: string,
    qos: 0,1,2,
    retain: boolean (true, false),
    payload: string,
    arrived: timestamp
}
```

Query messages published by a client:

```js
   db.mqtt_msg.find({sender: ${clientid}})
```

Query messages published by client 'test':

```js
db.mqtt_msg.find({sender: "test"})
{
    "_id" : 1,
    "topic" : "/World",
    "msgid" : "AAVEwm0la4RufgAABeIAAQ==",
    "sender" : "test",
    "qos" : 1,
    "retain" : 1,
    "payload" : "Hello world!",
    "arrived" : 1482976729
}
```

## MongoDB Retained Message Collection

*mqtt\_retain* stores retained messages:

```js
{
    topic: string,
    msgid: string,
    sender: string,
    qos: 0,1,2,
    payload: string,
    arrived: timestamp
}
```

Query retained messages:

```js
db.mqtt_retain.findOne({topic: ${topic}})
```

Query retained messages with topic 'retain':

```js
db.mqtt_retain.findOne({topic: "/World"})
{
    "_id" : ObjectId("58646dd9dde89a9fb9f7fb75"),
    "topic" : "/World",
    "msgid" : "AAVEwm0la4RufgAABeIAAQ==",
    "sender" : "c1",
    "qos" : 1,
    "payload" : "Hello world!",
    "arrived" : 1482976729
}
```

## MongoDB Acknowledgement Collection

*mqtt\_acked* stores acknowledgements from the clients:

```js
{
    clientid: string,
    topic: string,
    mongo_id: int
}
```

## Enable MongoDB Backend

```bash
./bin/emqx_ctl plugins load emqx_backend_mongo
```
