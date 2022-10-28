# MySQL Backend

::: tip

After EMQX version 3.1, a powerful rule engine is introduced to replace plug-ins. It is recommended that you use it. See [Save data to MySQL](../rule/backend_mysql.md) to setup Save data to MySQL in rule engine.

:::

Config file: emqx\_backend\_mysql.conf

## Configure MySQL Server

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

## Configure MySQL Persistence Hooks

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
## Notice: When the following configuration is enabled, 'on_message_fetch' needs to be disabled, otherwise qos1, qos2 messages will be stored twice
## backend.mysql.hook.message.publish.4     = {"topic": "#", "action": {"function": "on_message_store"}, "pool": "pool1"}
```

## Description of MySQL Persistence Hooks

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

## SQL Parameters Description

| hook                 | Parameters                           | Example (${name} represents available parameter)               |
| -------------------- | ------------------------------------ | -------------------------------------------------------------- |
| client.connected     | clientid                             | insert into conn(clientid) values(${clientid})                 |
| client.disconnected  | clientid                             | insert into disconn(clientid) values(${clientid})              |
| session.subscribed   | clientid, topic, qos                 | insert into sub(topic, qos) values(${topic}, ${qos})           |
| session.unsubscribed | clientid, topic                      | delete from sub where topic = ${topic}                         |
| message.publish      | msgid, topic, payload, qos, clientid | insert into msg(msgid, topic) values(${msgid}, ${topic})       |
| message.acked        | msgid, topic, clientid               | insert into ack(msgid, topic) values(${msgid}, ${topic})       |
| message.delivered    | msgid, topic, clientid               | insert into delivered(msgid, topic) values(${msgid}, ${topic}) |

## Configure 'action' with SQL

MySQL backend supports SQL in
'action':

```bash
## After a client is connected to the EMQX server, it executes a SQL command (multiple SQL commands also supported)
backend.mysql.hook.client.connected.3 = {"action": {"sql": ["insert into conn(clientid) values(${clientid})"]}, "pool": "pool1"}
```

## Create MySQL DB

```sql
create database mqtt;
```

## Import MySQL DB & Table Schema

```bash
mysql -u root -p mqtt < etc/sql/emqx_backend_mysql.sql
```

::: tip
DB name is free of choice
:::

## MySQL Client Connection Table

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

## MySQL Subscription Table

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

## MySQL Message Table

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

## MySQL Retained Message Table

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

## MySQL Acknowledgement Table

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

## Enable MySQL Backend

```bash
./bin/emqx_ctl plugins load emqx_backend_mysql
```
