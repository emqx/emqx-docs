# MongoDB 消息存储

::: tip

EMQ X 3.1 版本后推出强大的规则引擎用于替换插件，建议您前往使用[保存数据到 MongoDB](../rule/backend_mongodb.md)规则引擎中创建 保存数据到 MongoDB

:::

## 配置 MongoDB 消息存储

配置文件: emqx_backend_mongo.conf

## 配置 MongoDB 服务器

支持配置多台 MongoDB 服务器连接池:

```bash
## MongoDB 集群类型: single | unknown | sharded | rs
backend.mongo.pool1.type = single

## 如果 type = rs; 需要配置 setname
## backend.mongo.pool1.rs_set_name = testrs

## MongoDB 服务器地址列表
backend.mongo.pool1.server = 127.0.0.1:27017

## MongoDB 连接池大小
backend.mongo.pool1.c_pool_size = 8

## 连接的数据库名称
backend.mongo.pool1.database = mqtt

## MongoDB 认证用户名密码
## backend.mongo.pool1.login =  emqtt
## backend.mongo.pool1.password = emqtt

## MongoDB 认证源
## backend.mongo.pool1.auth_source = admin

## 是否开启 SSL
## backend.mongo.pool1.ssl = false

## SSL 密钥文件路径
## backend.mongo.pool1.keyfile =

## SSL 证书文件路径
## backend.mongo.pool1.certfile =

## SSL CA 证书文件路径
## backend.mongo.pool1.cacertfile =

## MongoDB 数据写入模式: unsafe | safe
## backend.mongo.pool1.w_mode = safe

## MongoDB 数据读取模式: master | slaver_ok
## backend.mongo.pool1.r_mode = slave_ok

## MongoDB 底层 driver 配置, 保持默认即可
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

## MongoDB Backend Hooks
backend.mongo.hook.client.connected.1    = {"action": {"function": "on_client_connected"}, "pool": "pool1"}
backend.mongo.hook.session.created.1     = {"action": {"function": "on_subscribe_lookup"}, "pool": "pool1"}
backend.mongo.hook.client.disconnected.1 = {"action": {"function": "on_client_disconnected"}, "pool": "pool1"}
backend.mongo.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch"}, "pool": "pool1", "offline_opts": {"time_range": "2h", "max_returned_count": 500}}
backend.mongo.hook.session.subscribed.2  = {"topic": "#", "action": {"function": "on_retain_lookup"}, "pool": "pool1"}
backend.mongo.hook.session.unsubscribed.1= {"topic": "#", "action": {"function": "on_acked_delete"}, "pool": "pool1"}
backend.mongo.hook.message.publish.1     = {"topic": "#", "action": {"function": "on_message_publish"}, "pool": "pool1"}
backend.mongo.hook.message.publish.2     = {"topic": "#", "action": {"function": "on_message_retain"}, "pool": "pool1"}
backend.mongo.hook.message.publish.3     = {"topic": "#", "action": {"function": "on_retain_delete"}, "pool": "pool1"}
backend.mongo.hook.message.acked.1       = {"topic": "#", "action": {"function": "on_message_acked"}, "pool": "pool1"}

## 获取离线消息
### "offline_opts": 获取离线消息的配置
####   - max_returned_count: 单次拉去的最大离线消息数目
####   - time_range: 仅拉去在当前时间范围的消息
## backend.mongo.hook.session.subscribed.1  = {"topic": "#", "action": {"function": "on_message_fetch"}, "pool": "pool1", "offline_opts": {"time_range": "2h", "max_returned_count": 500}}

## 如果需要存储 Qos0 消息, 可开启以下配置
## 警告: 当开启以下配置时, 需关闭 'on_message_fetch', 否则 qos1, qos2 消息会被存储俩次
## backend.mongo.hook.message.publish.4     = {"topic": "#", "action": {"function": "on_message_store"}, "pool": "pool1", "payload_format": "mongo_json"}
```

*backend* 消息存储规则包括:

| hook                 | topic | action                   | 说明          |
| -------------------- | ----- | ------------------------ | ----------- |
| client.connected     |       | on_client_connected    | 存储客户端在线状态   |
| session.created      |       | on_subscribe_lookup    | 订阅主题        |
| client.disconnected  |       | on_client_disconnected | 存储客户端离线状态   |
| session.subscribed   | \#    | on_message_fetch       | 获取离线消息      |
| session.subscribed   | \#    | on_retain_lookup       | 获取retain消息  |
| session.unsubscribed | \#    | on_acked_delete        | 删除 acked 消息 |
| message.publish      | \#    | on_message_publish     | 存储发布消息      |
| message.publish      | \#    | on_message_retain      | 存储retain消息  |
| message.publish      | \#    | on_retain_delete       | 删除retain消息  |
| message.acked        | \#    | on_message_acked       | 消息ACK处理     |

## MongoDB 数据库初始化

```bash
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

## MongoDB 用户状态集合(Client Collection)

*mqtt_client* 存储设备在线状态:

```js
{
    clientid: string,
    state: 0,1, //0离线 1在线
    node: string,
    online_at: timestamp,
    offline_at: timestamp
}
```

查询设备在线状态:

```js
db.mqtt_client.findOne({clientid: ${clientid}})
```

例如 ClientId 为 test 客户端上线:

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

例如 ClientId 为 test 客户端下线:

```js
db.mqtt_client.findOne({clientid: "test"})

{
    "_id" : ObjectId("58646c9bdde89a9fb9f7fb73"),
    "clientid" : "test",
    "state" : 0,
    "node" : "emqx@127.0.0.1",
    "online_at" : 1482976411,
    "offline_at" : 1482976501
}
```

## MongoDB 用户订阅主题集合(Subscription Collection)

*mqtt_sub* 存储订阅关系:

```js
{
  clientid: string,
  topic: string,
  qos: 0,1,2
}
```

用户 test 分别订阅主题 test_topic0 test_topic1 test_topic2:

```js
db.mqtt_sub.insert({clientid: "test", topic: "test_topic1", qos: 1})
db.mqtt_sub.insert({clientid: "test", topic: "test_topic2", qos: 2})
```

某个客户端订阅主题:

```js
db.mqtt_sub.find({clientid: ${clientid}})
```

查询 ClientId 为 "test" 的客户端已订阅主题:

```js
db.mqtt_sub.find({clientid: "test"})

{ "_id" : ObjectId("58646d90c65dff6ac9668ca1"), "clientid" : "test", "topic" : "test_topic1", "qos" : 1 }
{ "_id" : ObjectId("58646d96c65dff6ac9668ca2"), "clientid" : "test", "topic" : "test_topic2", "qos" : 2 }
```

## MongoDB 发布消息集合(Message Collection)

*mqtt_msg* 存储 MQTT 消息:

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

查询某个客户端发布的消息:

```js
 db.mqtt_msg.find({sender: ${clientid}})
```

查询 ClientId 为 "test" 的客户端发布的消息:

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

## MongoDB 保留消息集合(Retain Message Collection)

mqtt_retain 存储 Retain 消息:

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

查询 retain 消息:

```js
db.mqtt_retain.findOne({topic: ${topic}})
```

查询topic为 "t/retain" 的 retain 消息:

```js
db.mqtt_retain.findOne({topic: "t/retain"})
{
  "_id" : ObjectId("58646dd9dde89a9fb9f7fb75"),
  "topic" : "t/retain",
  "msgid" : "AAVEwm0la4RufgAABeIAAQ==",
  "sender" : "c1",
  "qos" : 1,
  "payload" : "Hello world!",
  "arrived" : 1482976729
}
```

## MongoDB 接收消息 ack 集合(Message Acked Collection)

*mqtt_acked* 存储客户端消息确认:

```js
{
  clientid: string,
  topic: string,
  mongo_id: int
}
```

## 启用 MongoDB 数据存储插件

```bash
./bin/emqx_ctl plugins load emqx_backend_mongo
```


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
## 在客户端连接到 EMQ X 服务器后，执行一条 cql 语句(支持多条 cql 语句)
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


