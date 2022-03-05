# Cassandra 消息存储

::: tip

EMQX 3.1 版本后推出强大的规则引擎用于替换插件，建议您前往使用[保存数据到 Cassandra](../rule/backend_cassandra.md)规则引擎中创建 保存数据到 Cassandra

:::

## 配置 Cassandra 服务器

配置文件: emqx_backend_cassa.conf

支持配置多台Cassandra服务器连接池:

```bash
## Cassandra 节点地址
backend.ecql.pool1.nodes = 127.0.0.1:9042

## Cassandra 连接池大小
backend.ecql.pool1.size = 8

## Cassandra 自动重连间隔(s)
backend.ecql.pool1.auto_reconnect = 1

## Cassandra 认证用户名/密码
backend.ecql.pool1.username = cassandra
backend.ecql.pool1.password = cassandra

## Cassandra Keyspace
backend.ecql.pool1.keyspace = mqtt

## Cassandra Logger type
backend.ecql.pool1.logger = info

##--------------------------------------------------------------------
## Cassandra Backend Hooks
##--------------------------------------------------------------------

## Client Connected Record
backend.cassa.hook.client.connected.1    = {"action": {"function": "on_client_connected"}, "pool": "pool1"}

## Subscribe Lookup Record
backend.cassa.hook.session.created.1     = {"action": {"function": "on_subscription_lookup"}, "pool": "pool1"}

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

## 获取离线消息
### "offline_opts": 获取离线消息的配置
####   - max_returned_count: 单次拉去的最大离线消息数目
####   - time_range: 仅拉去在当前时间范围的消息
## backend.cassa.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch"}, "offline_opts": {"max_returned_count": 500, "time_range": "2h"}, "pool": "pool1"}

## 如果需要存储 Qos0 消息, 可开启以下配置
## 警告: 当开启以下配置时, 需关闭 'on_message_fetch', 否则 qos1, qos2 消息会被存储俩次
## backend.cassa.hook.message.publish.4     = {"topic": "#", "action": {"function": "on_message_store"}, "pool": "pool1"}
```

*backend*
消息存储规则包括:

| hook                 | topic | action                   | 说明          |
| -------------------- | ----- | ------------------------ | ----------- |
| client.connected     |       | on_client_connected    | 存储客户端在线状态   |
| session.created      |       | on_subscribe_lookup    | 订阅主题        |
| client.disconnected  |       | on_client_disconnected | 存储客户端离线状态   |
| session.subscribed   | \#    | on_message_fetch       | 获取离线消息      |
| session.subscribed   | \#    | on_retain_lookup       | 获取retain消息  |
| session.unsubscribed | \#    |                          | 删除 akced 消息 |
| message.publish      | \#    | on_message_publish     | 存储发布消息      |
| message.publish      | \#    | on_message_retain      | 存储retain消息  |
| message.publish      | \#    | on_retain_delete       | 删除retain消息  |
| message.acked        | \#    | on_message_acked       | 消息ACK处理     |

*自定义 CQL 语句*
可用参数包括:

| hook                 | 可用参数                                 | 示例(cql语句中${name} 表示可获取的参数)                                   |
| -------------------- | ------------------------------------ | ------------------------------------------------------------ |
| client.connected     | clientid                             | insert into conn(clientid) values(${clientid})               |
| client.disconnected  | clientid                             | insert into disconn(clientid) values(${clientid})            |
| session.subscribed   | clientid, topic, qos                 | insert into sub(topic, qos) values(${topic}, ${qos})         |
| session.unsubscribed | clientid, topic                      | delete from sub where topic = ${topic}                       |
| message.publish      | msgid, topic, payload, qos, clientid | insert into msg(msgid, topic) values(${msgid}, ${topic})     |
| message.acked        | msgid, topic, clientid               | insert into ack(msgid, topic) values(${msgid}, ${topic})     |
| message.deliver      | msgid, topic, clientid               | insert into deliver(msgid, topic) values(${msgid}, ${topic}) |

支持 CQL 语句配置:

考虑到用户的需求不同, backend cassandra 自带的函数无法满足用户需求, 用户可根据自己的需求配置 cql 语句

在 etc/plugins/emqx_backend_cassa.conf 中添加如下配置:

```bash
## 在客户端连接到 EMQX 服务器后，执行一条 cql 语句(支持多条 cql 语句)
backend.cassa.hook.client.connected.3 = {"action": {"cql": ["insert into conn(clientid) values(${clientid})"]}, "pool": "pool1"}
```

## Cassandra 创建一个 Keyspace

```bash
CREATE KEYSPACE mqtt WITH replication = {'class': 'SimpleStrategy', 'replication_factor': '1'}  AND durable_writes = true;
USR mqtt;
```

## 导入 Cassandra 表结构

```bash
cqlsh -e "SOURCE 'emqx_backend_cassa.cql'"
```

## Cassandra 用户状态表(Client Table)

*mqtt.client* 存储设备在线状态:

```sql
CREATE TABLE mqtt.client (
  client_id text PRIMARY KEY,
  connected timestamp,
  disconnected timestamp,
  node text,
  state int
);
```

查询设备在线状态:

```bash
select * from mqtt.client where client_id = ${clientid};
```

例如 ClientId 为 test 的客户端上线:

```bash
select * from mqtt.client where client_id = 'test';

 client_id | connected                       | disconnected  | node            | state
-----------+---------------------------------+---------------+-----------------+-------
      test | 2017-02-14 08:27:29.872000+0000 |          null | emqx@127.0.0.1|   1
```

例如ClientId为test客户端下线:

```bash
select * from mqtt.client where client_id = 'test';

 client_id | connected                       | disconnected                    | node            | state
-----------+---------------------------------+---------------------------------+-----------------+-------
      test | 2017-02-14 08:27:29.872000+0000 | 2017-02-14 08:27:35.872000+0000 | emqx@127.0.0.1|     0
```

## Cassandra 用户订阅主题表(Sub Table)

*mqtt.sub* 存储订阅关系:

```sql
CREATE TABLE mqtt.sub (
  client_id text,
  topic text,
  qos int,
  PRIMARY KEY (client_id, topic)
);
```

用户test分别订阅主题test_topic1
test_topic2:

```sql
insert into mqtt.sub(client_id, topic, qos) values('test', 'test_topic1', 1);
insert into mqtt.sub(client_id, topic, qos) values('test', 'test_topic2', 2);
```

某个客户端订阅主题:

```sql
select * from mqtt_sub where clientid = ${clientid};
```

查询ClientId为'test'的客户端已订阅主题:

```bash
select * from mqtt_sub where clientid = 'test';

 client_id | topic       | qos
-----------+-------------+------
      test | test_topic1 |   1
      test | test_topic2 |   2
```

## Cassandra 发布消息表(Msg Table)

*mqtt.msg* 存储MQTT消息:

```sql
CREATE TABLE mqtt.msg (
  topic text,
  msgid text,
  arrived timestamp,
  payload text,
  qos int,
  retain int,
  sender text,
  PRIMARY KEY (topic, msgid)
) WITH CLUSTERING ORDER BY (msgid DESC);
```

查询某个客户端发布的消息:

```sql
select * from mqtt_msg where sender = ${clientid};
```

查询ClientId为'test'的客户端发布的消息:

```bash
select * from mqtt_msg where sender = 'test';

 topic | msgid                | arrived                         | payload      | qos | retain | sender
-------+----------------------+---------------------------------+--------------+-----+--------+--------
 hello | 2PguFrHsrzEvIIBdctmb | 2017-02-14 09:07:13.785000+0000 | Hello world! |   1 |      0 |   test
 world | 2PguFrHsrzEvIIBdctmb | 2017-02-14 09:07:13.785000+0000 | Hello world! |   1 |      0 |   test
```

## Cassandra 保留消息表(Retain Message Table)

*mqtt.retain* 存储 Retain 消息:

```sql
CREATE TABLE mqtt.retain (
  topic text PRIMARY KEY,
  msgid text
);
```

查询 retain 消息:

```sql
select * from mqtt_retain where topic = ${topic};
```

查询 topic 为 't/retain' 的 retain 消息:

```bash
select * from mqtt_retain where topic = 't/retain';

 topic  | msgid
--------+----------------------
 retain | 2PguFrHsrzEvIIBdctmb
```

## Cassandra 接收消息 ack 表(Message Acked Table)

*mqtt.acked* 存储客户端消息确认:

```sql
CREATE TABLE mqtt.acked (
  client_id text,
  topic text,
  msgid text,
  PRIMARY KEY (client_id, topic)
);
```

## 启用 Cassandra 存储插件

```bash
./bin/emqx_ctl plugins load emqx_backend_cassa
```
