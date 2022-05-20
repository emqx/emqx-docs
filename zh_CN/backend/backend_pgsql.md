# PostgreSQL 数据存储

::: tip

EMQX 3.1 版本后推出强大的规则引擎用于替换插件，建议您前往使用[保存数据到 PostgreSQL](../rule/backend_pgsql.md)规则引擎中创建 保存数据到 PostgreSQL

:::

配置文件: emqx_backend_pgsql.conf

## 配置 PostgreSQL 服务器

::: tip

支持 PostgreSQL 13 及以下版本

:::

支持配置多台PostgreSQL服务器连接池:

```bash
## Pgsql 服务器地址
backend.pgsql.pool1.server = 127.0.0.1:5432

## Pgsql 连接池大小
backend.pgsql.pool1.pool_size = 8

## Pgsql 用户名
backend.pgsql.pool1.username = root

## Pgsql 密码
backend.pgsql.pool1.password = public

## Pgsql 数据库名称
backend.pgsql.pool1.database = mqtt

## Pgsql Ssl
backend.pgsql.pool1.ssl = false
```

## 配置 PostgreSQL 存储规则

```bash
backend.pgsql.hook.client.connected.1    = {"action": {"function": "on_client_connected"}, "pool": "pool1"}
backend.pgsql.hook.session.created.1     = {"action": {"function": "on_subscribe_lookup"}, "pool": "pool1"}
backend.pgsql.hook.client.disconnected.1 = {"action": {"function": "on_client_disconnected"}, "pool": "pool1"}
backend.pgsql.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch"}, "pool": "pool1"}
backend.pgsql.hook.session.subscribed.2  = {"topic": "#", "action": {"function": "on_retain_lookup"}, "pool": "pool1"}
backend.pgsql.hook.session.unsubscribed.1= {"topic": "#", "action": {"sql": ["delete from mqtt_acked where clientid = ${clientid} and topic = ${topic}"]}, "pool": "pool1"}
backend.pgsql.hook.message.publish.1     = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
backend.pgsql.hook.message.publish.2     = {"topic": "#", "action": {"function": "on_message_retain"}, "pool": "pool1"}
backend.pgsql.hook.message.publish.3     = {"topic": "#", "action": {"function": "on_retain_delete"}, "pool": "pool1"}
backend.pgsql.hook.message.acked.1       = {"topic": "#", "action": {"function": "on_message_acked"}, "pool": "pool1"}

## 获取离线消息
### "offline_opts": 获取离线消息的配置
####   - max_returned_count: 单次拉去的最大离线消息数目
####   - time_range: 仅拉去在当前时间范围的消息
## backend.pgsql.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch"}, "offline_opts": {"max_returned_count": 500, "time_range": "2h"}, "pool": "pool1"}

## 如果需要存储 Qos0 消息, 可开启以下配置
## 警告: 当开启以下配置时, 需关闭 'on_message_fetch', 否则 qos1, qos2 消息会被存储俩次
## backend.pgsql.hook.message.publish.4     = {"topic": "#", "action": {"function": "on_message_store"}, "pool": "pool1"}
```

## PostgreSQL 存储规则说明

| hook                | topic | action                   | 说明           |
| ------------------- | ----- | ------------------------ | ------------ |
| client.connected    |       | on_client_connected    | 存储客户端在线状态    |
| session.created     |       | on_subscribe_lookup    | 订阅主题         |
| client.disconnected |       | on_client_disconnected | 存储客户端离线状态    |
| session.subscribed  | \#    | on_message_fetch       | 获取离线消息       |
| session.subscribed  | \#    | on_retain_lookup       | 获取 retain 消息 |
| message.publish     | \#    | on_message_publish     | 存储发布消息       |
| message.publish     | \#    | on_message_retain      | 存储 retain 消息 |
| message.publish     | \#    | on_retain_delete       | 删除 retain 消息 |
| message.acked       | \#    | on_message_acked       | 消息 ACK 处理    |

## SQL 语句参数说明

| hook                 | 可用参数                                 | 示例(sql语句中${name} 表示可获取的参数)                                   |
| -------------------- | ------------------------------------ | ------------------------------------------------------------ |
| client.connected     | clientid                             | insert into conn(clientid) values(${clientid})               |
| client.disconnected  | clientid                             | insert into disconn(clientid) values(${clientid})            |
| session.subscribed   | clientid, topic, qos                 | insert into sub(topic, qos) values(${topic}, ${qos})         |
| session.unsubscribed | clientid, topic                      | delete from sub where topic = ${topic}                       |
| message.publish      | msgid, topic, payload, qos, clientid | insert into msg(msgid, topic) values(${msgid}, ${topic})     |
| message.acked        | msgid, topic, clientid               | insert into ack(msgid, topic) values(${msgid}, ${topic})     |
| message.deliver      | msgid, topic, clientid               | insert into deliver(msgid, topic) values(${msgid}, ${topic}) |

## SQL 语句配置 Action

PostgreSQL 存储支持用户采用SQL语句配置 Action，例如:

```bash
## 在客户端连接到 EMQX 服务器后，执行一条 sql 语句(支持多条sql语句)
backend.pgsql.hook.client.connected.3 = {"action": {"sql": ["insert into conn(clientid) values(${clientid})"]}, "pool": "pool1"}
```

## 创建 PostgreSQL 数据库

```bash
createdb mqtt -E UTF8 -e
```

## 导入 PostgreSQL 库表结构

```bash
\i etc/sql/emqx_backend_pgsql.sql
```

## PostgreSQL 设备在线状态表

*mqtt_client* 存储设备在线状态:

```bash
CREATE TABLE mqtt_client(
  id SERIAL8 primary key,
  clientid character varying(64),
  state integer,
  node character varying(64),
  online_at timestamp ,
  offline_at timestamp,
  created timestamp without time zone,
  UNIQUE (clientid)
);
```

查询设备在线状态:

```sql
select * from mqtt_client where clientid = ${clientid};
```

例如 ClientId 为 test 客户端上线:

```bash
select * from mqtt_client where clientid = 'test';

  id | clientid | state | node             | online_at           | offline_at        | created
----+----------+-------+----------------+---------------------+---------------------+---------------------
   1 | test     | 1     | emqx@127.0.0.1 | 2016-11-15 09:40:40 | NULL                | 2016-12-24 09:40:22
(1 rows)
```

例如 ClientId 为 test 客户端下线:

```bash
select * from mqtt_client where clientid = 'test';

  id | clientid | state | nod            | online_at           | offline_at          | created
----+----------+-------+----------------+---------------------+---------------------+---------------------
  1 | test     | 0     | emqx@127.0.0.1 | 2016-11-15 09:40:40 | 2016-11-15 09:46:10 | 2016-12-24 09:40:22
(1 rows)
```

## PostgreSQL 代理订阅表

*mqtt_sub* 存储订阅关系:

```sql
CREATE TABLE mqtt_sub(
  id SERIAL8 primary key,
  clientid character varying(64),
  topic character varying(255),
  qos integer,
  created timestamp without time zone,
  UNIQUE (clientid, topic)
);
```

例如 ClientId 为 test 客户端订阅主题 test_topic1 test_topic2:

```sql
insert into mqtt_sub(clientid, topic, qos) values('test', 'test_topic1', 1);
insert into mqtt_sub(clientid, topic, qos) values('test', 'test_topic2', 2);
```

某个客户端订阅主题:

```bash
select * from mqtt_sub where clientid = ${clientid};
```

查询 ClientId 为 test 的客户端已订阅主题:

```bash
select * from mqtt_sub where clientid = 'test';

 id | clientId     | topic       | qos  | created
----+--------------+-------------+------+---------------------
  1 | test         | test_topic1 |    1 | 2016-12-24 17:09:05
  2 | test         | test_topic2 |    2 | 2016-12-24 17:12:51
(2 rows)
```

## PostgreSQL 消息存储表

*mqtt_msg* 存储MQTT消息:

```sql
CREATE TABLE mqtt_msg (
  id SERIAL8 primary key,
  msgid character varying(64),
  sender character varying(64),
  topic character varying(255),
  qos integer,
  retain integer,
  payload text,
  arrived timestamp without time zone
);
```

查询某个客户端发布的消息:

```sql
select * from mqtt_msg where sender = ${clientid};
```

查询 ClientId 为 test 的客户端发布的消息:

```bash
select * from mqtt_msg where sender = 'test';

  id | msgid                         | topic    | sender | node | qos | retain | payload | arrived
----+-------------------------------+----------+--------+------+-----+--------+---------+---------------------
  1  | 53F98F80F66017005000004A60003 | hello    | test   | NULL |   1 |      0 | hello   | 2016-12-24 17:25:12
  2  | 53F98F9FE42AD7005000004A60004 | world    | test   | NULL |   1 |      0 | world   | 2016-12-24 17:25:45
(2 rows)
```

## PostgreSQL 保留消息表

*mqtt_retain* 存储 Retain 消息:

```sql
CREATE TABLE mqtt_retain(
  id SERIAL8 primary key,
  topic character varying(255),
  msgid character varying(64),
  sender character varying(64),
  qos integer,
  payload text,
  arrived timestamp without time zone,
  UNIQUE (topic)
);
```

查询 retain 消息:

```sql
select * from mqtt_retain where topic = ${topic};
```

查询 topic 为 retain 的 retain 消息:

```bash
select * from mqtt_retain where topic = 'retain';

  id | topic    | msgid                         | sender  | node | qos  | payload | arrived
----+----------+-------------------------------+---------+------+------+---------+---------------------
  1 | retain   | 53F33F7E4741E7007000004B70001 | test    | NULL |    1 | www     | 2016-12-24 16:55:18
(1 rows)
```

## PostgreSQL 消息确认表

*mqtt_acked* 存储客户端消息确认:

```sql
CREATE TABLE mqtt_acked (
  id SERIAL8 primary key,
  clientid character varying(64),
  topic character varying(64),
  mid integer,
  created timestamp without time zone,
  UNIQUE (clientid, topic)
);
```

## 启用 PostgreSQL 数据存储插件

```bash
./bin/emqx_ctl plugins load emqx_backend_pgsql
```


