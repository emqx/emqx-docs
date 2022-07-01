# Cassandra Backend

::: tip

After EMQX version 3.1, a powerful rule engine is introduced to replace plug-ins. It is recommended that you use it. See [Save data to Cassandra](../rule/backend_cassandra.md) to setup Save data to Cassandra in rule engine.

:::

Config file: etc/plugins/emqx\_backend\_cassa.conf

## Configure Cassandra Cluster

Multi node Cassandra cluster is supported:

```bash
## Cassandra Node
backend.ecql.pool1.nodes = 127.0.0.1:9042

## Cassandra Pool Size
backend.ecql.pool1.size = 8

## Cassandra auto reconnect flag
backend.ecql.pool1.auto_reconnect = 1

## Cassandra Username
backend.ecql.pool1.username = cassandra

## Cassandra Password
backend.ecql.pool1.password = cassandra

## Cassandra Keyspace
backend.ecql.pool1.keyspace = mqtt

## Cassandra Logger type
backend.ecql.pool1.logger = info

## Max number of fetch offline messages. Without count limit if infinity
## backend.cassa.max_returned_count = 500

## Time Range. Without time limit if infinity
## d - day
## h - hour
## m - minute
## s - second
## backend.cassa.time_range = 2h
```

## Configure Cassandra Persistence Hooks

```bash
## Client Connected Record
backend.cassa.hook.client.connected.1    = {"action": {"function": "on_client_connected"}, "pool": "pool1"}

## Subscribe Lookup Record
backend.cassa.hook.client.connected.2    = {"action": {"function": "on_subscription_lookup"}, "pool": "pool1"}

## Client DisConnected Record
backend.cassa.hook.client.disconnected.1 = {"action": {"function": "on_client_disconnected"}, "pool": "pool1"}

## Lookup Unread Message QOS > 0
backend.cassa.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch"}, "pool": "pool1"}

## Lookup Retain Message
backend.cassa.hook.session.subscribed.2  = {"action": {"function": "on_retain_lookup"}, "pool": "pool1"}

## Store Publish Message  QOS > 0
backend.cassa.hook.message.publish.1     = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}

## Delete Acked Record
backend.cassa.hook.session.unsubscribed.1= {"topic": "#", "action": {"cql": ["delete from acked where clientid = ${clientid} and topic = ${topic}"]}, "pool": "pool1"}

## Store Retain Message
backend.cassa.hook.message.publish.2     = {"topic": "#", "action": {"function": "on_message_retain"}, "pool": "pool1"}

## Delete Retain Message
backend.cassa.hook.message.publish.3     = {"topic": "#", "action": {"function": "on_retain_delete"}, "pool": "pool1"}

## Store Ack
backend.cassa.hook.message.acked.1       = {"topic": "#", "action": {"function": "on_message_acked"}, "pool": "pool1"}

## Get offline messages
### "offline_opts": Get configuration for offline messages
### max_returned_count: Maximum number of offline messages get at a time
### time_range: Get only messages in the current time range
## backend.cassa.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch"}, "offline_opts": {"max_returned_count": 500, "time_range": "2h"}, "pool": "pool1"}

## If you need to store Qos0 messages, you can enable the following configuration
## Warning: When the following configuration is enabled, 'on_message_fetch' needs to be disabled, otherwise qos1, qos2 messages will be stored twice
## backend.cassa.hook.message.publish.4     = {"topic": "#", "action": {"function": "on_message_store"}, "pool": "pool1"}
```

## Description of Cassandra Persistence Hooks

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

## CQL Parameters Description

Customized CQL command parameters
includes:

| hook                 | Parameter                            | Example (${name} in CQL represents available parameter         |
| -------------------- | ------------------------------------ | -------------------------------------------------------------- |
| client.connected     | clientid                             | insert into conn(clientid) values(${clientid})                 |
| client.disconnected  | clientid                             | insert into disconn(clientid) values(${clientid})              |
| session.subscribed   | clientid, topic, qos                 | insert into sub(topic, qos) values(${topic}, ${qos})           |
| session.unsubscribed | clientid, topic                      | delete from sub where topic = ${topic}                         |
| message.publish      | msgid, topic, payload, qos, clientid | insert into msg(msgid, topic) values(${msgid}, ${topic})       |
| message.acked        | msgid, topic, clientid               | insert into ack(msgid, topic) values(${msgid}, ${topic})       |
| message.delivered    | msgid, topic, clientid               | insert into delivered(msgid, topic) values(${msgid}, ${topic}) |

## Configure 'action' with CQL

Cassandra backend supports CLQ in
'action':

```bash
## After a client is connected to the EMQX server, it executes a CQL command(multiple command also supported):
backend.cassa.hook.client.connected.3 = {"action": {"cql": ["insert into conn(clientid) values(${clientid})"]}, "pool": "pool1"}
```

## Initializing Cassandra

Create
KeySpace:

```sql
CREATE KEYSPACE mqtt WITH REPLICATION = { 'class' : 'SimpleStrategy', 'replication_factor' : 1 };
USE mqtt;
```

Import Cassandra tables:

```sql
cqlsh -e "SOURCE 'emqx_backend_cassa.cql'"
```

::: tip
KeySpace is free of choice
:::

## Cassandra Client Connection Table

*mqtt.client* stores client connection states:

```sql
CREATE TABLE mqtt.client (
    clientid text,
    node text,
    state int,
    connected timestamp,
    disconnected timestamp,
    PRIMARY KEY(clientid)
);
```

Query a client's connection state:

```sql
select * from mqtt.client where clientid = ${clientid};
```

If client 'test' is online:

```bash
select * from mqtt.client where clientid = 'test';

    clientid | connected                       | disconnected  | node          | state
-----------+---------------------------------+---------------+---------------+-------
        test | 2017-02-14 08:27:29.872000+0000 |          null | emqx@127.0.0.1|     1
```

Client 'test' is offline:

```bash
select * from mqtt.client where clientid = 'test';

    clientid | connected                       | disconnected                    | node          | state
-----------+---------------------------------+---------------------------------+---------------+-------
        test | 2017-02-14 08:27:29.872000+0000 | 2017-02-14 08:27:35.872000+0000 | emqx@127.0.0.1|     0
```

## Cassandra Subscription Table

*mqtt.sub* stores subscriptions of clients:

```sql
CREATE TABLE mqtt.sub (
    clientid text,
    topic text,
    qos int,
    PRIMARY KEY(clientid, topic)
);
```

Client 'test' subscribes to topic 'test\_topic1' and
    'test\_topic2':
```sql
insert into mqtt.sub(clientid, topic, qos) values('test', 'test_topic1', 1);
insert into mqtt.sub(clientid, topic, qos) values('test', 'test_topic2', 2);
```

Query subscriptions of a client:

```sql
select * from mqtt_sub where clientid = ${clientid};
```
Query subscriptions of client 'test':

```bash
select * from mqtt_sub where clientid = 'test';

    clientid | topic       | qos
-----------+-------------+-----
        test | test_topic1 |   1
        test | test_topic2 |   2
```
## Cassandra Message Table

*mqtt.msg* stores MQTT messages:

```sql
CREATE TABLE mqtt.msg (
    topic text,
    msgid text,
    sender text,
    qos int,
    retain int,
    payload blob,
    arrived timestamp,
    PRIMARY KEY(topic, msgid)
    ) WITH CLUSTERING ORDER BY (msgid DESC);
```

Query messages published by a client:

```sql
select * from mqtt_msg where sender = ${clientid};
```
Query messages published by client 'test':

```bash
select * from mqtt_msg where sender = 'test';

    topic | msgid                | arrived                         | payload      | qos | retain | sender
-------+----------------------+---------------------------------+--------------+-----+--------+--------
    hello | 2PguFrHsrzEvIIBdctmb | 2017-02-14 09:07:13.785000+0000 | Hello world! |   1 |      0 |   test
    world | 2PguFrHsrzEvIIBdctmb | 2017-02-14 09:07:13.785000+0000 | Hello world! |   1 |      0 |   test
```

## Cassandra Retained Message Table

*mqtt.retain* stores retained messages:

```sql
CREATE TABLE mqtt.retain (
    topic text,
    msgid text,
    PRIMARY KEY(topic)
);
```

Query retained messages:

```sql
select * from mqtt_retain where topic = ${topic};
```
Query retained messages with topic 'retain':

```sql
select * from mqtt_retain where topic = 'retain';

    topic  | msgid
--------+----------------------
    retain | 2PguFrHsrzEvIIBdctmb
```
## Cassandra Acknowledgement Table

*mqtt.acked* stores acknowledgements from the clients:

```sql
CREATE TABLE mqtt.acked (
    clientid text,
    topic text,
    msgid text,
    PRIMARY KEY(clientid, topic)
);
```

## Enable Cassandra Backend

```bash
./bin/emqx_ctl plugins load emqx_backend_cassa
```
