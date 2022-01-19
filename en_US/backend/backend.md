---
# 标题
title: 数据存储
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---

# Backends

::: danger
Data storage is an exclusive feature of EMQ X Enterprise.
:::

## MQTT Message Persistence

### One-to-one message Persistence

![image](./assets/backends_1.png)

1. PUB publishes a message;
2. Backend records this message in DB;
3. SUB subscribes to a topic;
4. Backend retrieves the messages of this topic from DB;
5. Messages are sent to SUB;
6. Once the SUB acknowledged / received the message, backend removes
    the message from DB.

### Many-to-many message Persistence

![image](./assets/backends_2.png)

1. PUB publishes a message;
2. Backend records the message in DB;
3. SUB1 and SUB2 subscribe to a topic;
4. Backend retrieves the messages of this topic;
5. Messages are sent to SUB1 and SUB2;
6. Backend records the read position of SUB1 and SUB2, the next
    message’s retrieval starts from this position.

### Client Connection State

EMQ X supports retaining the client's connection state in Redis or DB.

### Client Subscription by Broker

EMQ X Persistence supports subscription by broker. When a client goes
online, the persistence module loads the subscriptions of the client
from Redis or Databases.

### List of Persistence Plugins

EMQ X allowes storing messages in Redis, MySQL, PostgreSQL, MongoDB,
Cassandra, DynamoDB, InfluxDB, OpenTSDB and
Timescale:

| Persistence Plugins      | Config File                   | Description                    |
| ------------------------ | ----------------------------- | ------------------------------ |
| emqx\_backend\_redis     | emqx\_backend\_redis.conf     | Redis Message Persistence      |
| emqx\_backend\_mysql     | emqx\_backend\_mysql.conf     | MySQL Message Persistence      |
| emqx\_backend\_pgsql     | emqx\_backend\_pgsql.conf     | PostgreSQL Message Persistence |
| emqx\_backend\_mongo     | emqx\_backend\_mongo.conf     | MongoDB Message Persistence    |
| emqx\_backend\_cassa     | emqx\_backend\_cassa.conf     | Cassandra Message Persistence  |
| emqx\_backend\_dynamo    | emqx\_backend\_dynamo.conf    | DynamoDB Message Persistence   |
| emqx\_backend\_influxdb  | emqx\_backend\_influxdb.conf  | InfluxDB Message Persistence   |
| emqx\_backend\_opentsdb  | emqx\_backend\_opentsdb.conf  | OpenTSDB Message Persistence   |
| emqx\_backend\_timescale | emqx\_backend\_timescale.conf | Timescale Message Persistence  |

## Redis Backend

Config file: emqx\_backend\_redis.conf

### Configure the Redis Server

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

### Configure Persistence Hooks

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

### Description of Persistence Hooks

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

### Redis Command Line Parameters

| hook                 | Parameter                                     | Example (Fields separated exactly by one space) |
| -------------------- | --------------------------------------------- | ----------------------------------------------- |
| client.connected     | clientid                                      | SET conn:${clientid} clientid                   |
| client.disconnected  | clientid                                      | SET disconn:${clientid} clientid                |
| session.subscribed   | clientid, topic, qos                          | HSET sub:${clientid} topic qos                  |
| session.unsubscribed | clientid, topic                               | SET unsub:${clientid} topic                     |
| message.publish      | message, msgid, topic, payload, qos, clientid | RPUSH pub:${topic} msgid                        |
| message.acked        | msgid, topic, clientid                        | HSET ack:${clientid} topic msgid                |
| message.delivered    | msgid, topic, clientid                        | HSET delivered:${clientid} topic msgid          |

### Configure 'action' with Redis Commands

Redis backend supports raw 'commands' in 'action',
e.g.:

```bash
## After a client connected to the EMQ X server, it executes a redis command (multiple redis commands also supported)
backend.redis.hook.client.connected.3 = {"action": {"commands": ["SET conn:${clientid} clientid"]}, "pool": "pool1"}
```

### Using Redis Hash for Devices' Connection State

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

### Using Redis Hash for Retained Messages

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
>    
>     3)  "from"
> 
>   -     4) "test"
>    
>     5)  "qos"
>     6)  "1"
>     7)  "topic"
>     8)  "topic"
>     9)  "retain"
> 
>   - 10\) "true"
>    
>     11) "payload"
>     12) "Hello world\!"
>     13) "ts"
>     14) "1481690659"

```

### Using Redis Hash for messages

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
### Using Redis Set for Message Acknowledgements

*mqtt:acked* SET stores acknowledgements from the clients:

```bash
set
key = mqtt:acked:${clientid}:${topic}
value = ${msgid}
```
### Using Redis Hash for Subscription

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
### Redis SUB/UNSUB Publish

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
### Enable Redis Backend

```bash
./bin/emqx_ctl plugins load emqx_backend_redis
```

## MySQL Backend

Config file: emqx\_backend\_mysql.conf

### Configure MySQL Server

Connection pool of multiple MySQL servers is supported:

```bash
## Mysql Server
backend.mysql.pool1.server = 127.0.0.1:3306

## Mysql Pool Size
backend.mysql.pool1.pool_size = 8

## Mysql Username
backend.mysql.pool1.user = root

## Mysql Password
backend.mysql.pool1.password = public

## Mysql Database
backend.mysql.pool1.database = mqtt

## Max number of fetch offline messages. Without count limit if infinity
## backend.mysql.max_returned_count = 500

## Time Range. Without time limit if infinity
## d - day
## h - hour
## m - minute
## s - second
## backend.mysql.time_range = 2h
```

### Configure MySQL Persistence Hooks

```bash
## Client Connected Record
backend.mysql.hook.client.connected.1    = {"action": {"function": "on_client_connected"}, "pool": "pool1"}

## Subscribe Lookup Record
backend.mysql.hook.client.connected.2    = {"action": {"function": "on_subscribe_lookup"}, "pool": "pool1"}

## Client DisConnected Record
backend.mysql.hook.client.disconnected.1 = {"action": {"function": "on_client_disconnected"}, "pool": "pool1"}

## Lookup Unread Message QOS > 0
backend.mysql.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch"}, "pool": "pool1"}

## Lookup Retain Message
backend.mysql.hook.session.subscribed.2  = {"topic": "#", "action": {"function": "on_retain_lookup"}, "pool": "pool1"}

## Store Publish Message  QOS > 0
backend.mysql.hook.message.publish.1     = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}

## Store Retain Message
backend.mysql.hook.message.publish.2     = {"topic": "#", "action": {"function": "on_message_retain"}, "pool": "pool1"}

## Delete Retain Message
backend.mysql.hook.message.publish.3     = {"topic": "#", "action": {"function": "on_retain_delete"}, "pool": "pool1"}

## Store Ack
backend.mysql.hook.message.acked.1       = {"topic": "#", "action": {"function": "on_message_acked"}, "pool": "pool1"}

## Get offline messages
### "offline_opts": Get configuration for offline messages
### max_returned_count: Maximum number of offline messages get at a time
### time_range: Get only messages in the current time range
## backend.mysql.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch"}, "offline_opts": {"max_returned_count": 500, "time_range": "2h"}, "pool": "pool1"}

## If you need to store Qos0 messages, you can enable the following configuration
## Warning: When the following configuration is enabled, 'on_message_fetch' needs to be disabled, otherwise qos1, qos2 messages will be stored twice
## backend.mysql.hook.message.publish.4     = {"topic": "#", "action": {"function": "on_message_store"}, "pool": "pool1"}
```

### Description of MySQL Persistence Hooks

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

### SQL Parameters Description

| hook                 | Parameters                           | Example (${name} represents available parameter)               |
| -------------------- | ------------------------------------ | -------------------------------------------------------------- |
| client.connected     | clientid                             | insert into conn(clientid) values(${clientid})                 |
| client.disconnected  | clientid                             | insert into disconn(clientid) values(${clientid})              |
| session.subscribed   | clientid, topic, qos                 | insert into sub(topic, qos) values(${topic}, ${qos})           |
| session.unsubscribed | clientid, topic                      | delete from sub where topic = ${topic}                         |
| message.publish      | msgid, topic, payload, qos, clientid | insert into msg(msgid, topic) values(${msgid}, ${topic})       |
| message.acked        | msgid, topic, clientid               | insert into ack(msgid, topic) values(${msgid}, ${topic})       |
| message.delivered    | msgid, topic, clientid               | insert into delivered(msgid, topic) values(${msgid}, ${topic}) |

### Configure 'action' with SQL

MySQL backend supports SQL in
'action':

```bash
## After a client is connected to the EMQ X server, it executes a SQL command (multiple SQL commands also supported)
backend.mysql.hook.client.connected.3 = {"action": {"sql": ["insert into conn(clientid) values(${clientid})"]}, "pool": "pool1"}
```

### Create MySQL DB

```sql
create database mqtt;
```

### Import MySQL DB & Table Schema

```bash
mysql -u root -p mqtt < etc/sql/emqx_backend_mysql.sql
```

::: tip
DB name is free of choice
:::

### MySQL Client Connection Table

*mqtt\_client* stores client connection states:

```sql
DROP TABLE IF EXISTS `mqtt_client`;
CREATE TABLE `mqtt_client` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `clientid` varchar(64) DEFAULT NULL,
  `state` varchar(3) DEFAULT NULL,
  `node` varchar(100) DEFAULT NULL,
  `online_at` datetime DEFAULT NULL,
  `offline_at` datetime DEFAULT NULL,
  `created` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  KEY `mqtt_client_idx` (`clientid`),
  UNIQUE KEY `mqtt_client_key` (`clientid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```

Query the client connection state:

```sql
select * from mqtt_client where clientid = ${clientid};
```

If client 'test' is online:

```sql
select * from mqtt_client where clientid = "test";

+----+----------+-------+----------------+---------------------+---------------------+---------------------+
| id | clientid | state | node           | online_at           | offline_at          | created             |
+----+----------+-------+----------------+---------------------+---------------------+---------------------+
   |  1 | test     | 1     | emqx@127.0.0.1 | 2016-11-15 09:40:40 | NULL                | 2016-12-24 09:40:22 |
+----+----------+-------+----------------+---------------------+---------------------+---------------------+
1 rows in set (0.00 sec)
```

If client 'test' is offline:

```sql
select * from mqtt_client where clientid = "test";

+----+----------+-------+----------------+---------------------+---------------------+---------------------+
| id | clientid | state | node           | online_at           | offline_at          | created             |
+----+----------+-------+----------------+---------------------+---------------------+---------------------+
|  1 | test     | 0     | emqx@127.0.0.1 | 2016-11-15 09:40:40 | 2016-11-15 09:46:10 | 2016-12-24 09:40:22 |
+----+----------+-------+----------------+---------------------+---------------------+---------------------+
1 rows in set (0.00 sec)
```

### MySQL Subscription Table

*mqtt\_sub* table stores MQTT subscriptions of clients:

```sql
DROP TABLE IF EXISTS `mqtt_sub`;
CREATE TABLE `mqtt_sub` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `clientid` varchar(64) DEFAULT NULL,
  `topic` varchar(255) DEFAULT NULL,
  `qos` int(3) DEFAULT NULL,
  `created` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  KEY `mqtt_sub_idx` (`clientid`,`topic`(255),`qos`),
  UNIQUE KEY `mqtt_sub_key` (`clientid`,`topic`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```

E.g., client 'test' subscribes to 'test\_topic1' and
'test\_topic2':

```sql
insert into mqtt_sub(clientid, topic, qos) values("test", "test_topic1", 1);
insert into mqtt_sub(clientid, topic, qos) values("test", "test_topic2", 2);
```

Query subscription of a client:

```sql
select * from mqtt_sub where clientid = ${clientid};
```

E.g., query the Subscription of client 'test':

```sql
select * from mqtt_sub where clientid = "test";

+----+--------------+-------------+------+---------------------+
| id | clientId     | topic       | qos  | created             |
+----+--------------+-------------+------+---------------------+
|  1 | test         | test_topic1 |    1 | 2016-12-24 17:09:05 |
|  2 | test         | test_topic2 |    2 | 2016-12-24 17:12:51 |
+----+--------------+-------------+------+---------------------+
2 rows in set (0.00 sec)
```

### MySQL Message Table

*mqtt\_msg* stores MQTT messages:

```sql
DROP TABLE IF EXISTS `mqtt_msg`;
CREATE TABLE `mqtt_msg` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `msgid` varchar(100) DEFAULT NULL,
  `topic` varchar(1024) NOT NULL,
  `sender` varchar(1024) DEFAULT NULL,
  `node` varchar(60) DEFAULT NULL,
  `qos` int(11) NOT NULL DEFAULT '0',
  `retain` tinyint(2) DEFAULT NULL,
  `payload` blob,
  `arrived` datetime NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```

Query messages published by a client:

```sql
select * from mqtt_msg where sender = ${clientid};
```

Query messages published by client 'test':

```sql
select * from mqtt_msg where sender = "test";

+----+-------------------------------+----------+--------+------+-----+--------+---------+---------------------+
| id | msgid                         | topic    | sender | node | qos | retain | payload | arrived             |
+----+-------------------------------+----------+--------+------+-----+--------+---------+---------------------+
| 1  | 53F98F80F66017005000004A60003 | hello    | test   | NULL |   1 |      0 | hello   | 2016-12-24 17:25:12 |
| 2  | 53F98F9FE42AD7005000004A60004 | world    | test   | NULL |   1 |      0 | world   | 2016-12-24 17:25:45 |
+----+-------------------------------+----------+--------+------+-----+--------+---------+---------------------+
2 rows in set (0.00 sec)
```

### MySQL Retained Message Table

mqtt\_retain stores retained messages:

```sql
DROP TABLE IF EXISTS `mqtt_retain`;
CREATE TABLE `mqtt_retain` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `topic` varchar(200) DEFAULT NULL,
  `msgid` varchar(60) DEFAULT NULL,
  `sender` varchar(100) DEFAULT NULL,
  `node` varchar(100) DEFAULT NULL,
  `qos` int(2) DEFAULT NULL,
  `payload` blob,
  `arrived` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `mqtt_retain_key` (`topic`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```

Query retained messages:

```sql
select * from mqtt_retain where topic = ${topic};
```

Query retained messages with topic 'retain':

```sql
select * from mqtt_retain where topic = "retain";
```

```bash
+----+----------+-------------------------------+---------+------+------+---------+---------------------+
| id | topic    | msgid                         | sender  | node | qos  | payload | arrived             |
+----+----------+-------------------------------+---------+------+------+---------+---------------------+
|  1 | retain   | 53F33F7E4741E7007000004B70001 | test    | NULL |    1 | www     | 2016-12-24 16:55:18 |
+----+----------+-------------------------------+---------+------+------+---------+---------------------+

>    1 rows in set (0.00 sec)
```

### MySQL Acknowledgement Table

*mqtt\_acked* stores acknowledgements from the clients:

```sql
DROP TABLE IF EXISTS `mqtt_acked`;
CREATE TABLE `mqtt_acked` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `clientid` varchar(200) DEFAULT NULL,
  `topic` varchar(200) DEFAULT NULL,
  `mid` int(200) DEFAULT NULL,
  `created` timestamp NULL DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `mqtt_acked_key` (`clientid`,`topic`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```

### Enable MySQL Backend

```bash
./bin/emqx_ctl plugins load emqx_backend_mysql
```

## PostgreSQL Backend

Config file: emqx\_backend\_pgsql.conf

### Configure PostgreSQL Server

Connection pool of multiple PostgreSQL servers is supported:

```bash
## Pgsql Server
backend.pgsql.pool1.server = 127.0.0.1:5432

## Pgsql Pool Size
backend.pgsql.pool1.pool_size = 8

## Pgsql Username
backend.pgsql.pool1.username = root

## Pgsql Password
backend.pgsql.pool1.password = public

## Pgsql Database
backend.pgsql.pool1.database = mqtt

## Pgsql Ssl
backend.pgsql.pool1.ssl = false

## Max number of fetch offline messages. Without count limit if infinity
## backend.pgsql.max_returned_count = 500

## Time Range. Without time limit if infinity
## d - day
## h - hour
## m - minute
## s - second
## backend.pgsql.time_range = 2h
```

### Configure PostgreSQL Persistence Hooks

```bash
## Client Connected Record
backend.pgsql.hook.client.connected.1    = {"action": {"function": "on_client_connected"}, "pool": "pool1"}

## Subscribe Lookup Record
backend.pgsql.hook.client.connected.2    = {"action": {"function": "on_subscribe_lookup"}, "pool": "pool1"}

## Client DisConnected Record
backend.pgsql.hook.client.disconnected.1 = {"action": {"function": "on_client_disconnected"}, "pool": "pool1"}

## Lookup Unread Message QOS > 0
backend.pgsql.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch"}, "pool": "pool1"}

## Lookup Retain Message
backend.pgsql.hook.session.subscribed.2  = {"topic": "#", "action": {"function": "on_retain_lookup"}, "pool": "pool1"}

## Store Publish Message  QOS > 0
backend.pgsql.hook.message.publish.1     = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}

## Store Retain Message
backend.pgsql.hook.message.publish.2     = {"topic": "#", "action": {"function": "on_message_retain"}, "pool": "pool1"}

## Delete Retain Message
backend.pgsql.hook.message.publish.3     = {"topic": "#", "action": {"function": "on_retain_delete"}, "pool": "pool1"}

## Store Ack
backend.pgsql.hook.message.acked.1       = {"topic": "#", "action": {"function": "on_message_acked"}, "pool": "pool1"}

## Get offline messages
### "offline_opts": Get configuration for offline messages
### max_returned_count: Maximum number of offline messages get at a time
### time_range: Get only messages in the current time range
## backend.pgsql.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch"}, "offline_opts": {"max_returned_count": 500, "time_range": "2h"}, "pool": "pool1"}

## If you need to store Qos0 messages, you can enable the following configuration
## Warning: When the following configuration is enabled, 'on_message_fetch' needs to be disabled, otherwise qos1, qos2 messages will be stored twice
## backend.pgsql.hook.message.publish.4     = {"topic": "#", "action": {"function": "on_message_store"}, "pool": "pool1"}
```

### Description of PostgreSQL Persistence Hooks

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

### SQL Parameters Description

| hook                 | Parameters                           | Example (${name} represents available parameter)               |
| -------------------- | ------------------------------------ | -------------------------------------------------------------- |
| client.connected     | clientid                             | insert into conn(clientid) values(${clientid})                 |
| client.disconnected  | clientid                             | insert into disconn(clientid) values(${clientid})              |
| session.subscribed   | clientid, topic, qos                 | insert into sub(topic, qos) values(${topic}, ${qos})           |
| session.unsubscribed | clientid, topic                      | delete from sub where topic = ${topic}                         |
| message.publish      | msgid, topic, payload, qos, clientid | insert into msg(msgid, topic) values(${msgid}, ${topic})       |
| message.acked        | msgid, topic, clientid               | insert into ack(msgid, topic) values(${msgid}, ${topic})       |
| message.delivered    | msgid, topic, clientid               | insert into delivered(msgid, topic) values(${msgid}, ${topic}) |

### Configure 'action' with SQL

PostgreSQL backend supports SQL in
'action':

```bash
## After a client is connected to the EMQ X server, it executes a SQL command (multiple command also supported)
backend.pgsql.hook.client.connected.3 = {"action": {"sql": ["insert into conn(clientid) values(${clientid})"]}, "pool": "pool1"}
```

### Create PostgreSQL DB

```bash
createdb mqtt -E UTF8 -e
```

### Import PostgreSQL DB & Table Schema

```bash
\i etc/sql/emqx_backend_pgsql.sql
```

::: tip
DB name is free of choice
:::
### PostgreSQL Client Connection Table

*mqtt\_client* stores client connection states:

```sql
CREATE TABLE mqtt_client(
    id SERIAL primary key,
    clientid character varying(100),
    state integer,
    node character varying(100),
    online_at timestamp,
    offline_at timestamp,
    created timestamp without time zone,
    UNIQUE (clientid)
);
```

Query a client's connection state:

```sql
select * from mqtt_client where clientid = ${clientid};
```

E.g., if client 'test' is online:

```bash
select * from mqtt_client where clientid = 'test';

id | clientid | state | node             | online_at           | offline_at        | created
----+----------+-------+----------------+---------------------+---------------------+---------------------
 1 | test     | 1     | emqx@127.0.0.1 | 2016-11-15 09:40:40 | NULL                | 2016-12-24 09:40:22
(1 rows)
```
Client 'test' is offline:

```bash
select * from mqtt_client where clientid = 'test';

    id | clientid | state | nod            | online_at           | offline_at          | created
----+----------+-------+----------------+---------------------+---------------------+---------------------
    1 | test     | 0     | emqx@127.0.0.1 | 2016-11-15 09:40:40 | 2016-11-15 09:46:10 | 2016-12-24 09:40:22
(1 rows)
```

### PostgreSQL Subscription Table

*mqtt\_sub* stores subscriptions of clients:

```sql
CREATE TABLE mqtt_sub(
    id SERIAL primary key,
    clientid character varying(100),
    topic character varying(200),
    qos integer,
    created timestamp without time zone,
    UNIQUE (clientid, topic)
);
```
E.g., client 'test' subscribes to topic 'test\_topic1' and
'test\_topic2':

```sql
insert into mqtt_sub(clientid, topic, qos) values('test', 'test_topic1', 1);
insert into mqtt_sub(clientid, topic, qos) values('test', 'test_topic2', 2);
```

Query subscription of a client:

```sql
select * from mqtt_sub where clientid = ${clientid};
```
Query subscription of client 'test':

```bash
select * from mqtt_sub where clientid = 'test';

    id | clientId     | topic       | qos  | created
----+--------------+-------------+------+---------------------
    1 | test         | test_topic1 |    1 | 2016-12-24 17:09:05
    2 | test         | test_topic2 |    2 | 2016-12-24 17:12:51
(2 rows)
```

### PostgreSQL Message Table

*mqtt\_msg* stores MQTT messages:

```sql
CREATE TABLE mqtt_msg (
  id SERIAL primary key,
  msgid character varying(60),
  sender character varying(100),
  topic character varying(200),
  qos integer,
  retain integer,
  payload text,
  arrived timestamp without time zone
);
```

Query messages published by a client:

```sql
select * from mqtt_msg where sender = ${clientid};
```
Query messages published by client 'test':

```bash
select * from mqtt_msg where sender = 'test';

    id | msgid                         | topic    | sender | node | qos | retain | payload | arrived
----+-------------------------------+----------+--------+------+-----+--------+---------+---------------------
    1  | 53F98F80F66017005000004A60003 | hello    | test   | NULL |   1 |      0 | hello   | 2016-12-24 17:25:12
    2  | 53F98F9FE42AD7005000004A60004 | world    | test   | NULL |   1 |      0 | world   | 2016-12-24 17:25:45
(2 rows)
```

### PostgreSQL Retained Message Table

*mqtt\_retain* stores retained messages:

```sql
CREATE TABLE mqtt_retain(
  id SERIAL primary key,
  topic character varying(200),
  msgid character varying(60),
  sender character varying(100),
  qos integer,
  payload text,
  arrived timestamp without time zone,
  UNIQUE (topic)
);
```

Query retained messages:

```sql
select * from mqtt_retain where topic = ${topic};
```
Query retained messages with topic 'retain':

```sql
select * from mqtt_retain where topic = 'retain';

    id | topic    | msgid                         | sender  | node | qos  | payload | arrived
----+----------+-------------------------------+---------+------+------+---------+---------------------
    1 | retain   | 53F33F7E4741E7007000004B70001 | test    | NULL |    1 | www     | 2016-12-24 16:55:18
(1 rows)
```

### PostgreSQL Acknowledgement Table

*mqtt\_acked* stores acknowledgements from the clients:

```sql
CREATE TABLE mqtt_acked (
  id SERIAL primary key,
  clientid character varying(100),
  topic character varying(100),
  mid integer,
  created timestamp without time zone,
  UNIQUE (clientid, topic)
);
```

### Enable PostgreSQL Backend

```bash
./bin/emqx_ctl plugins load emqx_backend_pgsql
```

## MongoDB Backend

Config file: emqx\_backend\_mongo.conf

### Configure MongoDB Server

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

### Configure MongoDB Persistence Hooks

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
## Warning: When the following configuration is enabled, 'on_message_fetch' needs to be disabled, otherwise qos1, qos2 messages will be stored twice
## backend.mongo.hook.message.publish.4     = {"topic": "#", "action": {"function": "on_message_store"}, "pool": "pool1"}
```

### Description of MongoDB Persistence Hooks

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

### Create MongoDB DB & Collections

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

### MongoDB MQTT Client Collection

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

### MongoDB Subscription Collection

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

### MongoDB Message Collection

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

### MongoDB Retained Message Collection

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

### MongoDB Acknowledgement Collection

*mqtt\_acked* stores acknowledgements from the clients:

```js
{
    clientid: string,
    topic: string,
    mongo_id: int
}
```

### Enable MongoDB Backend

```bash
./bin/emqx_ctl plugins load emqx_backend_mongo
```

## Cassandra Backend

Config file: etc/plugins/emqx\_backend\_cassa.conf

### Configure Cassandra Cluster

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

### Configure Cassandra Persistence Hooks

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
backend.cassa.hook.session.unsubscribed.1= {"topic": "#", action": {"cql": ["delete from acked where client_id = ${clientid} and topic = ${topic}"]}, "pool": "pool1"}

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

### Description of Cassandra Persistence Hooks

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

### CQL Parameters Description

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

### Configure 'action' with CQL

Cassandra backend supports CLQ in
'action':

```bash
## After a client is connected to the EMQ X server, it executes a CQL command(multiple command also supported):
backend.cassa.hook.client.connected.3 = {"action": {"cql": ["insert into conn(clientid) values(${clientid})"]}, "pool": "pool1"}
```

### Initializing Cassandra

Create
KeySpace:

```sql
CREATE KEYSPACE mqtt WITH REPLICATION = { 'class' : 'SimpleStrategy', 'replication_factor' : 1 };
USR mqtt;
```

Import Cassandra tables:

```sql
cqlsh -e "SOURCE 'emqx_backend_cassa.cql'"
```

::: tip
KeySpace is free of choice
:::

### Cassandra Client Connection Table

*mqtt.client* stores client connection states:

```sql
CREATE TABLE mqtt.client (
    client_id text,
    node text,
    state int,
    connected timestamp,
    disconnected timestamp,
    PRIMARY KEY(client_id)
);
```
Query a client's connection state:

```sql
select * from mqtt.client where clientid = ${clientid};
```
If client 'test' is online:

```bash
select * from mqtt.client where clientid = 'test';

    client_id | connected                       | disconnected  | node          | state
-----------+---------------------------------+---------------+---------------+-------
        test | 2017-02-14 08:27:29.872000+0000 |          null | emqx@127.0.0.1|     1
```
Client 'test' is offline:

```bash
select * from mqtt.client where clientid = 'test';

    client_id | connected                       | disconnected                    | node          | state
-----------+---------------------------------+---------------------------------+---------------+-------
        test | 2017-02-14 08:27:29.872000+0000 | 2017-02-14 08:27:35.872000+0000 | emqx@127.0.0.1|     0
```

### Cassandra Subscription Table

*mqtt.sub* stores subscriptions of clients:

```sql
CREATE TABLE mqtt.sub (
    client_id text,
    topic text,
    qos int,
    PRIMARY KEY(client_id, topic)
);
```

Client 'test' subscribes to topic 'test\_topic1' and
    'test\_topic2':
```sql
insert into mqtt.sub(client_id, topic, qos) values('test', 'test_topic1', 1);
insert into mqtt.sub(client_id, topic, qos) values('test', 'test_topic2', 2);
```

Query subscriptions of a client:

```sql
select * from mqtt_sub where clientid = ${clientid};
```
Query subscriptions of client 'test':

```bash
select * from mqtt_sub where clientid = 'test';

    client_id | topic       | qos
-----------+-------------+-----
        test | test_topic1 |   1
        test | test_topic2 |   2
```
### Cassandra Message Table

*mqtt.msg* stores MQTT messages:

```sql
CREATE TABLE mqtt.msg (
    topic text,
    msgid text,
    sender text,
    qos int,
    retain int,
    payload text,
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

### Cassandra Retained Message Table

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
### Cassandra Acknowledgement Table

*mqtt.acked* stores acknowledgements from the clients:

```sql
CREATE TABLE mqtt.acked (
    client_id text,
    topic text,
    msgid text,
    PRIMARY KEY(client_id, topic)
);
```

### Enable Cassandra Backend

```bash
./bin/emqx_ctl plugins load emqx_backend_cassa
```

## DynamoDB Backend

### Configure DynamoDB Cluster

Config file: etc/plugins/emqx\_backend\_dynamo.conf

```bash
## DynamoDB Region
backend.dynamo.region = us-west-2

## DynamoDB Server
backend.dynamo.pool1.server = http://localhost:8000

## DynamoDB Pool Size
backend.dynamo.pool1.pool_size = 8

## AWS ACCESS KEY ID
backend.dynamo.pool1.aws_access_key_id = AKIAU5IM2XOC7AQWG7HK

## AWS SECRET ACCESS KEY
backend.dynamo.pool1.aws_secret_access_key = TZt7XoRi+vtCJYQ9YsAinh19jR1rngm/hxZMWR2P

## DynamoDB Backend Hooks
backend.dynamo.hook.client.connected.1    = {"action": {"function": "on_client_connected"}, "pool": "pool1"}
backend.dynamo.hook.session.created.1     = {"action": {"function": "on_subscribe_lookup"}, "pool": "pool1"}
backend.dynamo.hook.client.disconnected.1 = {"action": {"function": "on_client_disconnected"}, "pool": "pool1"}
backend.dynamo.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch_for_queue"}, "pool": "pool1"}
backend.dynamo.hook.session.subscribed.2  = {"topic": "#", "action": {"function": "on_retain_lookup"}, "pool": "pool1"}
backend.dynamo.hook.session.unsubscribed.1= {"topic": "#", "action": {"function": "on_acked_delete"}, "pool": "pool1"}
backend.dynamo.hook.message.publish.1     = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
backend.dynamo.hook.message.publish.2     = {"topic": "#", "action": {"function": "on_message_retain"}, "pool": "pool1"}
backend.dynamo.hook.message.publish.3     = {"topic": "#", "action": {"function": "on_retain_delete"}, "pool": "pool1"}
backend.dynamo.hook.message.acked.1       = {"topic": "#", "action": {"function": "on_message_acked_for_queue"}, "pool": "pool1"}

# backend.dynamo.hook.message.publish.4   = {"topic": "#", "action": {"function": "on_message_store"}, "pool": "pool1"}
```

### Description of DynamoDB Persistence Hooks

| hook                | topic | action                         | Description                     |
| ------------------- | ----- | ------------------------------ | ------------------------------- |
| client.connected    |       | on\_client\_connected          | Store client connected state    |
| client.connected    |       | on\_subscribe\_lookup          | Subscribed topics               |
| client.disconnected |       | on\_client\_disconnected       | Store client disconnected state |
| session.subscribed  | \#    | on\_message\_fetch\_for\_queue | Fetch offline messages          |
| session.subscribed  | \#    | on\_retain\_lookup             | Lookup retained messages        |
| message.publish     | \#    | on\_message\_publish           | Store published messages        |
| message.publish     | \#    | on\_message\_retain            | Store retained messages         |
| message.publish     | \#    | on\_retain\_delete             | Delete retained messages        |
| message.acked       | \#    | on\_message\_acked\_for\_queue | Process ACK                     |

### Create DynamoDB DB

```bash
./test/dynamo_test.sh
```

::: tip
DB name is free of choice
:::

### DynamoDB Client Connection Table

*mqtt\_client* stores client connection states:

```bash
{
    "TableName": "mqtt_client",
    "KeySchema": [
        { "AttributeName": "clientid", "KeyType": "HASH" }
    ],
    "AttributeDefinitions": [
        { "AttributeName": "clientid", "AttributeType": "S" }
    ],
    "ProvisionedThroughput": {
        "ReadCapacityUnits": 5,
        "WriteCapacityUnits": 5
    }
}
```

Query the client connection
state:

```bash
aws dynamodb scan --table-name mqtt_client --region us-west-2  --endpoint-url http://localhost:8000

{
    "Items": [
        {
            "offline_at": { "N": "0" },
            "node": { "S": "emqx@127.0.0.1" },
            "clientid": { "S": "mqttjs_384b9c73a9" },
            "connect_state": { "N": "1" },
            "online_at": { "N": "1562224940" }
        }
    ],
    "Count": 1,
    "ScannedCount": 1,
    "ConsumedCapacity": null
}
```

### DynamoDB Subscription Table

*mqtt\_sub* table stores MQTT subscriptions of clients:

```bash
{
    "TableName": "mqtt_sub",
    "KeySchema": [
        { "AttributeName": "clientid", "KeyType": "HASH" },
        { "AttributeName": "topic", "KeyType": "RANGE" }
    ],
    "AttributeDefinitions": [
        { "AttributeName": "clientid", "AttributeType": "S" },
        { "AttributeName": "topic", "AttributeType": "S" }
    ],
    "ProvisionedThroughput": {
        "ReadCapacityUnits": 5,
        "WriteCapacityUnits": 5
    }
}
```

Query topics subscribed by the client named
"test-dynamo":

```bash
aws dynamodb scan --table-name mqtt_sub --region us-west-2  --endpoint-url http://localhost:8000

{
    "Items": [{"qos": { "N": "2" }, "topic": { "S": "test-dynamo-sub" }, "clientid": { "S": "test-dynamo" }},
               {"qos": { "N": "2" }, "topic": { "S": "test-dynamo-sub-1"}, "clientid": { "S": "test-dynamo" }},
               {"qos": { "N": "2" }, "topic": { "S": "test-dynamo-sub-2"}, "clientid": { "S": "test-dynamo" }}],
    "Count": 3,
    "ScannedCount": 3,
    "ConsumedCapacity": null
}
```

### DynamoDB Message Table

*mqtt\_msg* stores MQTT messages:

```bash
{
    "TableName": "mqtt_msg",
    "KeySchema": [
        { "AttributeName": "msgid", "KeyType": "HASH" }
    ],
    "AttributeDefinitions": [
        { "AttributeName": "msgid", "AttributeType": "S" }
    ],
    "ProvisionedThroughput": {
        "ReadCapacityUnits": 5,
        "WriteCapacityUnits": 5
    }
}
```

*mqtt\_topic\_msg\_map* stores the mapping between topics and messages:

```bash
{
    "TableName": "mqtt_topic_msg_map",
    "KeySchema": [
        { "AttributeName": "topic", "KeyType": "HASH" }
    ],
    "AttributeDefinitions": [
        { "AttributeName": "topic", "AttributeType": "S" }
    ],
    "ProvisionedThroughput": {
        "ReadCapacityUnits": 5,
        "WriteCapacityUnits": 5
    }
}
```

Query *mqtt\_msg* and *mqtt\_topic\_msg\_map* after a client publishes a
message to the "test" topic:

Query
*mqtt\_msg*:

```bash
aws dynamodb scan --table-name mqtt_msg --region us-west-2  --endpoint-url http://localhost:8000
```

```bash
>   - {
>    
>       - "Items": \[
>        
>           - {  
>             "arrived": { "N": "1562308553" }, "qos": { "N": "1" },
>             "sender": { "S": "mqttjs\_231b962d5c" }, "payload": { "S":
>             "{ "msg": "Hello, World\!" }"}, "retain": { "N": "0" },
>             "msgid": { "S":
>             "Mjg4MTk1MDYwNTk0NjYwNzYzMTg4MDk3OTQ2MDU2Nzg1OTD" },
>             "topic": { "S": "test" }
>        
>         }
>    
>     \], "Count": 1, "ScannedCount": 1, "ConsumedCapacity": null
> 
> }
```

Query
*mqtt\_topic\_msg\_map*:

```bash
aws dynamodb scan --table-name mqtt_topic_msg_map --region us-west-2  --endpoint-url http://localhost:8000
```

```bash
>   - {
>    
>       - "Items": \[
>        
>           - {  
>             "topic": { "S": "test" }, "MsgId": { "SS": \[
>             "Mjg4MTk1MDYwNTk0NjYwNzYzMTg4MDk3OTQ2MDU2Nzg1OTD" \]}
>        
>         }
>    
>     \], "Count": 1, "ScannedCount": 1, "ConsumedCapacity": null
> 
> }
```

### DynamoDB Retained Message Table

*mqtt\_retain* stores retained messages:

```bash
{
    "TableName": "mqtt_retain",
    "KeySchema": [
        { "AttributeName": "topic", "KeyType": "HASH" }
    ],
    "AttributeDefinitions": [
        { "AttributeName": "topic", "AttributeType": "S" }
    ],
    "ProvisionedThroughput": {
        "ReadCapacityUnits": 5,
        "WriteCapacityUnits": 5
    }
}
```

Query *mqtt\_retain* after a client publishes a message to the "test"
topic:

```bash
{
    "Items": [
        {
            "arrived": { "N": "1562312113" },
            "qos": { "N": "1" },
            "sender": { "S": "mqttjs_d0513acfce" },
            "payload": { "S": "test" },
            "retain": { "N": "1" },
            "msgid": { "S": "Mjg4MTk1NzE3MTY4MjYxMjA5MDExMDg0NTk5ODgzMjAyNTH" },
            "topic": { "S": "testtopic" }
        }
    ],
    "Count": 1,
    "ScannedCount": 1,
    "ConsumedCapacity": null
}
```

### DynamoDB Acknowledgement Table

*mqtt\_acked* stores acknowledgements from the clients:

```bash
{
    "TableName": "mqtt_acked",
    "KeySchema": [
        { "AttributeName": "topic", "KeyType": "HASH" },
        { "AttributeName": "clientid", "KeyType": "RANGE" }
    ],
    "AttributeDefinitions": [
        { "AttributeName": "topic", "AttributeType": "S" },
        { "AttributeName": "clientid", "AttributeType": "S" }
    ],
    "ProvisionedThroughput": {
        "ReadCapacityUnits": 5,
        "WriteCapacityUnits": 5
    }
}
```

Query *mqtt\_acked* after a client publishes a message to the "test"
topic:

```bash
{
    "Items": [
        {
            "topic": { "S": "test" },
            "msgid": { "S": "Mjg4MTk1MDYwNTk0NjYwNzYzMTg4MDk3OTQ2MDU2Nzg1OTD" },
            "clientid": { "S": "mqttjs_861e582a70" }
        }
    ],
    "Count": 1,
    "ScannedCount": 1,
    "ConsumedCapacity": null
}
```

### Enable DynamoDB Backend

```bash
./bin/emqx_ctl plugins load emqx_backend_dynamo
```

## InfluxDB Backend

### Configure InfluxDB Server

Config file: etc/plugins/emqx\_backend\_influxdb.conf:

```bash
## InfluxDB UDP Server
backend.influxdb.pool1.server = 127.0.0.1:8089

## InfluxDB Pool Size
backend.influxdb.pool1.pool_size = 5

## Wether to add timestamp automatically
backend.influxdb.pool1.set_timestamp = true

backend.influxdb.hook.message.publish.1 = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

Parameters in hook
rule:

| Option | Description                                                                                                     |
| ------ | --------------------------------------------------------------------------------------------------------------- |
| topic  | Configure which topics need to execute hooks                                                                    |
| action | Configure specific action for hook, `function` is a built-in function provided as Backend for general functions |
| pool   | Pool Name, used to connect multiple InfluxDB servers                                                            |

Example:

```bash
## Store PUBLISH message whose topic is "sensor/#"
backend.influxdb.hook.message.publish.1 = {"topic": "sensor/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}

## Store PUBLISH message whose topic is "stat/#"
backend.influxdb.hook.message.publish.2 = {"topic": "stat/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

### Description of InfluxDB Persistence Hooks

| hook            | topic | action               | Description              |
| --------------- | ----- | -------------------- | ------------------------ |
| message.publish | \#    | on\_message\_publish | Store published messages |

Since MQTT Message cannot be written directly to InfluxDB, InfluxDB
Backend provides an emqx\_backend\_influxdb.tmpl template file to
convert MQTT Message to DataPoint that can be written to InfluxDB.

Template file use Json format:

  - `key` - MQTT Topic, Json String, support wildcard characters
  - `value` - Template, Json Object, used to convert MQTT Message into
    `measurement,tag_key=tag_value,... field_key=field_value,...
    timestamp` and write to InfluxDB。

You can define different templates for different topics or multiple
templates for the same topic, likes:

```bash
{
    <Topic 1>: <Template 1>,
    <Topic 2>: <Template 2>
}
```

Template format:

```bash
{
    "measurement": <Measurement>,
    "tags": {
        <Tag Key>: <Tag Value>
    },
    "fields": {
        <Field Key>: <Field Value>
    },
    "timestamp": <Timestamp>
}
```

`measurement` and `fields` are required options, `tags` and `timestamp`
are optional.

All values (such as `<Measurement>`) can be configured directly in the
template as a fixed value that data types supported depending on the
table you define. More realistically, of course, you can access the data
in the MQTT message through the placeholder we provide.

Currently, we support placeholders as
follows:

| Placeholder | Description                                                                         |
| ----------- | ----------------------------------------------------------------------------------- |
| $id         | MQTT Message UUID, assigned by EMQ X                                                |
| $clientid   | Client ID used by the Client                                                        |
| $username   | Username used by the Client                                                         |
| $peerhost   | IP of Client                                                                        |
| $qos        | QoS of MQTT Message                                                                 |
| $topic      | Topic of MQTT Message                                                               |
| $payload    | Payload of MQTT Message, must be valid Json data                                    |
| $<Number\> | It must be used with $paylaod to retrieve data from Json Array                      |
| $timestamp  | The timestamp EMQ X sets when preparing to forward messages, precision: Nanoseconds |

**$payload and $<Number\>:**

You can directly use `$content` to obtain the complete message payload,
you can use `["$payload", <Key>, ...]` to get the data inside the
message payload.

For example `payload` is `{"data": {"temperature": 23.9}}`, you can via
`["$payload", "data", "temperature"]` to get `23.9`.

In the case of array data type in Json, we introduced `$0` and
`$<pos_integer>`, `$0` means to get all elements in the array, and
`$<pos_integer>` means to get the <pos\_integer\>th element in the
array.

A simple example, `["$payload", "$0", "temp"]` will get `[20, 21]` from
`[{"temp": 20}, {"temp": 21}]`, and `["$payload", "$1", "temp"]` will
only get `20`.

It is worth noting that when you use `$0`, we expect the number of data
you get is same. Because we need to convert these arrays into multiple
records and write it into InfluxDB, and when you have three pieces of
data in one field and two in another, we won't know how to combine the
data for you.

**Example**

data/templates directory provides a sample template
(emqx\_backend\_influxdb\_example.tmpl, please remove the "\_example"
suffix from the filename when using it formally) for the user's
reference:

```bash
{
    "sample": {
        "measurement": "$topic",
        "tags": {
            "host": ["$payload", "data", "$0", "host"],
            "region": ["$payload", "data", "$0", "region"],
            "qos": "$qos",
            "clientid": "$clientid"
        },
        "fields": {
            "temperature": ["$payload", "data", "$0", "temp"]
        },
        "timestamp": "$timestamp"
    }
}
```

When an MQTT Message whose Topic is "sample" has the following Payload:

```bash
{
    "data": [
        {
            "temp": 1,
            "host": "serverA",
            "region": "hangzhou"
        },
        {
            "temp": 2,
            "host": "serverB",
            "region": "ningbo"
        }
    ]
}
```

Backend converts MQTT messages to:

```bash
[
    {
        "measurement": "sample",
        "tags": {
            "clientid": "mqttjs_ebcc36079a",
            "host": "serverA",
            "qos": "0",
            "region": "hangzhou",
        },
        "fields": {
            "temperature": "1"
        },
        "timestamp": "1560743513626681000"
    },
    {
        "measurement": "sample",
        "tags": {
            "clientid": "mqttjs_ebcc36079a",
            "host": "serverB",
            "qos": "0",
            "region": "ningbo",
        },
        "fields": {
            "temperature": "2"
        },
        "timestamp": "1560743513626681000"
    }
]
```

The data was finally encoded and written to InfluxDB as
follows:

```bash
"sample,clientid=mqttjs_6990f0e886,host=serverA,qos=0,region=hangzhou temperature=\"1\" 1560745505429670000\nsample,clientid=mqttjs_6990f0e886,host=serverB,qos=0,region=ningbo temperature=\"2\" 1560745505429670000\n"
```

### Enable InfluxDB Backend

```bash
./bin/emqx_ctl plugins load emqx_backend_influxdb
```

## OpenTSDB Backend

### Configure OpenTSDB Server

Config file: etc/plugins/emqx\_backend\_opentsdb.conf:

```bash
## OpenTSDB Server
backend.opentsdb.pool1.server = 127.0.0.1:4242

## OpenTSDB Pool Size
backend.opentsdb.pool1.pool_size = 8

## Whether to return summary info
backend.opentsdb.pool1.summary = true

## Whether to return detailed info
##
## Value: true | false
backend.opentsdb.pool1.details = false

## Synchronous write or not
##
## Value: true | false
backend.opentsdb.pool1.sync = false

## Synchronous write timeout in milliseconds
##
## Value: Duration
##
## Default: 0
backend.opentsdb.pool1.sync_timeout = 0

## Max batch size
##
## Value: Number >= 0
## Default: 20
backend.opentsdb.pool1.max_batch_size = 20

## Store PUBLISH Messages
backend.opentsdb.hook.message.publish.1 = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

Parameters in hook
rule:

| Option | Description                                                                                                     |
| ------ | --------------------------------------------------------------------------------------------------------------- |
| topic  | Configure which topics need to execute hooks                                                                    |
| action | Configure specific action for hook, `function` is a built-in function provided as Backend for general functions |
| pool   | Pool Name, used to connect multiple OpenTSDB servers                                                            |

Example:

```bash
## Store PUBLISH message whose topic is "sensor/#"
backend.influxdb.hook.message.publish.1 = {"topic": "sensor/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}

## Store PUBLISH message whose topic is "stat/#"
backend.influxdb.hook.message.publish.2 = {"topic": "stat/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

### Description of OpenTSDB Persistence Hooks

| hook            | topic | action               | Description              |
| --------------- | ----- | -------------------- | ------------------------ |
| message.publish | \#    | on\_message\_publish | Store published messages |

Since MQTT Message cannot be written directly to OpenTSDB, OpenTSDB
Backend provides an emqx\_backend\_opentsdb.tmpl template file to
convert MQTT Message to DataPoint that can be written to OpenTSDB.

Template file use Json format:

  - `key` - MQTT Topic, Json String, support wildcard characters
  - `value` - Template, Json Object, used to convert MQTT Message into
    `measurement,tag_key=tag_value,... field_key=field_value,...
    timestamp` and write to InfluxDB。

You can define different templates for different topics or multiple
templates for the same topic, likes:

```bash
{
    <Topic 1>: <Template 1>,
    <Topic 2>: <Template 2>
}
```

The template format is as follows:

```bash
{
    "measurement": <Measurement>,
    "tags": {
        <Tag Key>: <Tag Value>
    },
    "value": <Value>,
    "timestamp": <Timestamp>
}
```

`measurement` and `value` are required options, `tags` and `timestamp`
are optional.

All values (such as `<Measurement>`) can be configured directly in the
template as a fixed value that data types supported depending on the
table you define. More realistically, of course, you can access the data
in the MQTT message through the placeholder we provide.

Currently, we support placeholders as
follows:

| Placeholder | Description                                                                         |
| ----------- | ----------------------------------------------------------------------------------- |
| $id         | MQTT Message UUID, assigned by EMQ X                                                |
| $clientid   | Client ID used by the Client                                                        |
| $username   | Username used by the Client                                                         |
| $peerhost   | IP of Client                                                                        |
| $qos        | QoS of MQTT Message                                                                 |
| $topic      | Topic of MQTT Message                                                               |
| $payload    | Payload of MQTT Message, must be valid Json data                                    |
| $<Number\> | It must be used with $paylaod to retrieve data from Json Array                      |
| $timestamp  | The timestamp EMQ X sets when preparing to forward messages, precision: Nanoseconds |

**$payload and $<Number\>:**

You can directly use `$content` to obtain the complete message payload,
you can use `["$payload", <Key>, ...]` to get the data inside the
message payload.

For example `payload` is `{"data": {"temperature": 23.9}}`, you can via
`["$payload", "data", "temperature"]` to get `23.9`.

In the case of array data type in Json, we introduced `$0` and
`$<pos_integer>`, `$0` means to get all elements in the array, and
`$<pos_integer>` means to get the <pos\_integer\>th element in the
array.

A simple example, `["$payload", "$0", "temp"]` will get `[20, 21]` from
`[{"temp": 20}, {"temp": 21}]`, and `["$payload", "$1", "temp"]` will
only get `20`.

It is worth noting that when you use `$0`, we expect the number of data
you get is same. Because we need to convert these arrays into multiple
records and write it into OpenTSDB, and when you have three pieces of
data in one field and two in another, we won't know how to combine the
data for you.

**Example**

data/templates directory provides a sample template
(emqx\_backend\_opentsdb\_example.tmpl, please remove the "\_example"
suffix from the filename when using it formally) for the user's
reference:

```bash
{
    "sample": {
        "measurement": "$topic",
        "tags": {
            "host": ["$payload", "data", "$0", "host"],
            "region": ["$payload", "data", "$0", "region"],
            "qos": "$qos",
            "clientid": "$clientid"
        },
        "value": ["$payload", "data", "$0", "temp"],
        "timestamp": "$timestamp"
    }
}
```

When an MQTT Message whose Topic is "sample" has the following Payload:

```bash
{
    "data": [
        {
            "temp": 1,
            "host": "serverA",
            "region": "hangzhou"
        },
        {
            "temp": 2,
            "host": "serverB",
            "region": "ningbo"
        }
    ]
}
```

Backend converts MQTT messages into the following data and writes it to
OpenTSDB:

```bash
[
    {
        "measurement": "sample",
        "tags": {
            "clientid": "mqttjs_ebcc36079a",
            "host": "serverA",
            "qos": "0",
            "region": "hangzhou",
        },
        "value": "1",
        "timestamp": "1560743513626681000"
    },
    {
        "measurement": "sample",
        "tags": {
            "clientid": "mqttjs_ebcc36079a",
            "host": "serverB",
            "qos": "0",
            "region": "ningbo",
        },
        "value": "2",
        "timestamp": "1560743513626681000"
    }
]
```

### Enable OpenTSDB Backend

```bash
./bin/emqx_ctl plugins load emqx_backend_opentsdb
```

## Timescale Backend

### Configure Timescale Server

Config file: etc/plugins/emqx\_backend\_timescale.conf:

```bash
## Timescale Server
backend.timescale.pool1.server = 127.0.0.1:5432
## Timescale Pool Size
backend.timescale.pool1.pool_size = 8
## Timescale Username
backend.timescale.pool1.username = postgres
## Timescale Password
backend.timescale.pool1.password = password
## Timescale Database
backend.timescale.pool1.database = tutorial
## Timescale SSL
backend.timescale.pool1.ssl = false

## SSL keyfile.
##
## Value: File
## backend.timescale.pool1.keyfile =

## SSL certfile.
##
## Value: File
## backend.timescale.pool1.certfile =

## SSL cacertfile.
##
## Value: File
## backend.timescale.pool1.cacertfile =

## Store Publish Message
backend.timescale.hook.message.publish.1 = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

Parameters in hook
rule:

| Option | Description                                                                                                     |
| ------ | --------------------------------------------------------------------------------------------------------------- |
| topic  | Configure which topics need to execute hooks                                                                    |
| action | Configure specific action for hook, `function` is a built-in function provided as Backend for general functions |
| pool   | Pool Name, used to connect multiple Timescale servers                                                           |

Example:

```bash
## Store PUBLISH message whose topic is "sensor/#"
backend.influxdb.hook.message.publish.1 = {"topic": "sensor/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}

## Store PUBLISH message whose topic is "stat/#"
backend.influxdb.hook.message.publish.2 = {"topic": "stat/#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
```

### Description of Timescale Persistence Hooks

| hook            | topic | action               | Description              |
| --------------- | ----- | -------------------- | ------------------------ |
| message.publish | \#    | on\_message\_publish | Store published messages |

Timescale Backend provides the template file named
`emqx_backend_timescale.tmpl`, which is used to extract data from MQTT
messages with different topics for writing to Timescale.

Template file use Json format:

  - `key` - MQTT Topic, Json String, support wildcard characters
  - `value` - Template, Json Object, used to convert MQTT Message into
    `measurement,tag_key=tag_value,... field_key=field_value,...
    timestamp` and write to InfluxDB。

You can define different templates for different topics or multiple
templates for the same topic, likes:

```json
{
    <Topic 1>: <Template 1>,
    <Topic 2>: <Template 2> 
}
```

The template format is as follows:

```json
{
    "name": <Name of template>,
    "sql": <SQL INSERT INTO>,
    "param_keys": <Param Keys>
}
```

`name`, `sql` and `param_keys` are required options.

`name` can be any string, just make sure there are no duplicates.

`sql` is SQL INSERT INTO statement for Timescale, like `insert into
sensor_data(time, location, temperature, humidity) values (NOW(), $1,
$2, $3)`.

`param_keys` is a array, its first element corresponds to `$1` appearing
in `sql` and so on.

Any element in an array can be a fixed value, and the data type it
supports depends on the table you define. More realistically, of course,
you can access the data in the MQTT message through the placeholder we
provide.

Currently, we support placeholders as
follows:

| Placeholder | Description                                                                         |
| ----------- | ----------------------------------------------------------------------------------- |
| $id         | MQTT Message UUID, assigned by EMQ X                                                |
| $clientid   | Client ID used by the Client                                                        |
| $username   | Username used by the Client                                                         |
| $peerhost   | IP of Client                                                                        |
| $qos        | QoS of MQTT Message                                                                 |
| $topic      | Topic of MQTT Message                                                               |
| $payload    | Payload of MQTT Message, must be valid Json data                                    |
| $<Number\> | It must be used with $paylaod to retrieve data from Json Array                      |
| $timestamp  | The timestamp EMQ X sets when preparing to forward messages, precision: Nanoseconds |

**$payload and $<Number\>:**

You can directly use `$content` to obtain the complete message payload,
you can use `["$payload", <Key>, ...]` to get the data inside the
message payload.

For example `payload` is `{"data": {"temperature": 23.9}}`, you can via
`["$payload", "data", "temperature"]` to get `23.9`.

In the case of array data type in Json, we introduced `$0` and
`$<pos_integer>`, `$0` means to get all elements in the array, and
`$<pos_integer>` means to get the <pos\_integer\>th element in the
array.

A simple example, `["$payload", "$0", "temp"]` will get `[20, 21]` from
`[{"temp": 20}, {"temp": 21}]`, and `["$payload", "$1", "temp"]` will
only get `20`.

It is worth noting that when you use `$0`, we expect the number of data
you get is same. Because we need to convert these arrays into multiple
records and write it into Timescale, and when you have three pieces of
data in one field and two in another, we won't know how to combine the
data for you.

**Example**

data/templates directory provides a sample template
(emqx\_backend\_timescale\_example.tmpl, please remove the "\_example"
suffix from the filename when using it formally) for the user's
reference:

```json
{
    "sensor_data": {
        "name": "insert_sensor_data",
        "sql": "insert into sensor_data(time, location, temperature, humidity) values (NOW(), $1, $2, $3)",
        "param_keys": [
            ["$payload", "data", "$0", "location"],
            ["$payload", "data", "$0", "temperature"],
            ["$payload", "data", "$0", "humidity"]
        ]
    },
    "sensor_data2/#": {
        "name": "insert_sensor_data2",
        "sql": "insert into sensor_data(time, location, temperature, humidity) values (NOW(), $1, $2, $3)",
        "param_keys": [
            ["$payload", "location"],
            ["$payload", "temperature"],
            ["$payload", "humidity"]
        ]
    },
    "easy_data": {
        "name": "insert_easy_data",
        "sql": "insert into easy_data(time, data) values (NOW(), $1)",
        "param_keys": [
            "$payload"
        ]
    }
}
```

When an MQTT Message whose Topic is "sensor\_data" has the following
Payload:

```json
{
    "data":[
        {
            "location":"bedroom",
            "temperature":21.3,
            "humidity":40.3
        },
        {
            "location":"bathroom",
            "temperature":22.3,
            "humidity":61.8
        },
        {
            "location":"kitchen",
            "temperature":29.5,
            "humidity":58.7
        }
    ]
}
```

`["$payload", "data", "$0", "location"]` will extract Payload from MQTT
Message first.

If the format of Payload is json, backend continue to extract `data`
from Payload.

And the value of `data` is an array, we use `$0` to gets all elements in
the array.

`["$payload", "data", "$0", "location"]` will help us get `["bedroom",
"bathroom", "kitchen"]` finally.

Accordingly if you replace `$0` with `$1`, you get only `["bedroom"]`.

So in this scene, we will get the following SQL
statement:

```json
insert into sensor_data(time, location, temperature, humidity) values (NOW(), 'bedroom', 21.3, 40.3)
insert into sensor_data(time, location, temperature, humidity) values (NOW(), 'bathroom', 22.3, 61.8)
insert into sensor_data(time, location, temperature, humidity) values (NOW(), 'kitchen', 29.5, 58.7)
```

Eventually Timescale Backend executes these SQL statements to write data
to Timescale.
