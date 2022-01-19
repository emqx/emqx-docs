---
# 标题
title: 规则引擎
# 编写日期
date: 2020-02-20 17:46:13
# 作者 Github 名称
author: wivwiv, terry-xiaoyu
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref: undefined
---

::: danger
EMQ X Broker 中仅适用以下操作：
- 检查 (调试)
- 发送数据到 Web 服务
- 桥接数据到 MQTT Broker
- 保存数据到 TDengine(使用 发送数据到 Web 服务 实现)
其余均是 EMQ X Enterprise 专属功能。
:::

## 检查 (调试)

创建一个测试规则，当有消息发送到 't/a' 主题时，打印消息内容以及动作参数细节。

 - 规则的筛选 SQL 语句为: SELECT \* FROM "t/a";
 - 动作是: "打印动作参数细节"，需要使用内置动作 'inspect'。

<!-- end list -->

```bash
$ ./bin/emqx_ctl rules create \
 "SELECT * FROM \"t/a\" WHERE " \
 '[{"name":"inspect", "params": {"a": 1}}]' \
 -d 'Rule for debug'

Rule rule:803de6db created
```

上面的 CLI 命令创建了一个 ID 为 'Rule rule:803de6db' 的规则。

参数中前两个为必参数:

 - SQL 语句: SELECT \* FROM "t/a"
 - 动作列表: \[{"name":"inspect", "params": {"a": 1}}\]。动作列表是用 JSON Array
   格式表示的。name 字段是动作的名字，params 字段是动作的参数。注意 `inspect` 动作是不需要绑定资源的。

最后一个可选参数，是规则的描述: 'Rule for debug'。

接下来当发送 "hello" 消息到主题 't/a' 时，上面创建的 "Rule rule:803de6db" 规则匹配成功，然后
"inspect" 动作被触发，将消息和参数内容打印到 emqx 控制台:

```bash
$ tail -f log/erlang.log.1

(emqx@127.0.0.1)1> [inspect]
    Selected Data: #{client_id => <<"shawn">>,event => 'message.publish',
                    flags => #{dup => false},
                    id => <<"5898704A55D6AF4430000083D0002">>,
                    payload => <<"hello">>,
                    peername => <<"127.0.0.1:61770">>,qos => 1,
                    timestamp => 1558587875090,topic => <<"t/a">>,
                    username => undefined}
    Envs: #{event => 'message.publish',
            flags => #{dup => false},
            from => <<"shawn">>,
            headers =>
                #{allow_publish => true,
                    peername => {{127,0,0,1},61770},
                    username => undefined},
            id => <<0,5,137,135,4,165,93,106,244,67,0,0,8,61,0,2>>,
            payload => <<"hello">>,qos => 1,
            timestamp => {1558,587875,89754},
            topic => <<"t/a">>}
    Action Init Params: #{<<"a">> => 1}
```

 - `Selected Data` 列出的是消息经过 SQL 筛选、提取后的字段，由于我们用的是 `select
   *`，所以这里会列出所有可用的字段。
 - `Envs` 是动作内部可以使用的环境变量。
 - `Action Init Params` 是初始化动作的时候，我们传递给动作的参数。


## 发送数据到 Web 服务

创建一个规则，将所有发送自 client\_id='Steven' 的消息，转发到地址为 '<http://127.0.0.1:9910>' 的
Web 服务器:

 - 规则的筛选条件为: SELECT username as u, payload FROM "\#" where u='Steven';
 - 动作是: "转发到地址为 '<http://127.0.0.1:9910>' 的 Web 服务";
 - 资源类型是: web\_hook;
 - 资源是: "到 url='<http://127.0.0.1:9910>' 的 WebHook 资源"。

<!-- end list -->

首先我们创建一个简易 Web 服务，这可以使用 `nc`
​       命令实现:
```bash
$ while true; do echo -e "HTTP/1.1 200 OK\n\n $(date)" | nc -l 127.0.0.1 9910; done;
```

使用 WebHook 类型创建一个资源，并配置资源参数 url:

1). 列出当前所有可用的资源类型，确保 'web\_hook' 资源类型已存在:
```bash
$ ./bin/emqx_ctl resource-types list
resource_type(name='web_hook', provider='emqx_web_hook', params=#{...}}, on_create={emqx_web_hook_actions,on_resource_create}, description='WebHook Resource')
...
```

2). 使用类型 'web\_hook' 创建一个新的资源，并配置 "url"="<http://127.0.0.1:9910>":

```bash
$ ./bin/emqx_ctl resources create \
    'web_hook' \
    -c '{"url": "http://127.0.0.1:9910", "headers": {"token":"axfw34y235wrq234t4ersgw4t"}, "method": "POST"}'

Resource resource:691c29ba create
```
上面的 CLI 命令创建了一个 ID 为 '<resource:691c29ba>' 的资源，第一个参数是必选参数 -
资源类型(web\_hook)。参数表明此资源指向 URL = "<http://127.0.0.1:9910>" 的
Web 服务，方法为 POST，并且设置了一个 HTTP Header: "token"。

然后创建规则，并选择规则的动作为 'data\_to\_webserver':

1). 列出当前所有可用的动作，确保 'data\_to\_webserver' 动作已存在:

```bash
$ ./bin/emqx_ctl rule-actions list

action(name='data_to_webserver', app='emqx_web_hook', for='$any', types=[web_hook], params=#{'$resource' => ...}, title ='Data to Web Server', description='Forward Messages to Web Server')
...
```
2). 创建规则，选择 data\_to\_webserver 动作，并通过 "$resource" 参数将
<resource:691c29ba> 资源绑定到动作上:

```bash
$ ./bin/emqx_ctl rules create \
"SELECT username as u, payload FROM \"#\" where u='Steven'" \
'[{"name":"data_to_webserver", "params": {"$resource":  "resource:691c29ba"}}]' \
-d "Forward publish msgs from steven to webserver"

rule:26d84768
```

上面的 CLI 命令与第一个例子里创建 Inspect 规则时类似，区别在于这里需要把刚才创建的资源
'<resource:691c29ba>' 绑定到 'data\_to\_webserver'
动作上。这个绑定通过给动作设置一个特殊的参数
'$resource' 完成。'data\_to\_webserver' 动作的作用是将数据发送到指定的 Web 服务器。

现在我们使用 username "Steven" 发送 "hello" 到任意主题，上面创建的规则就会被触发，Web Server收到消息并回复 200 OK:

```bash
$ while true; do echo -e "HTTP/1.1 200 OK\n\n $(date)" | nc -l 127.0.0.1 9910; done;

POST / HTTP/1.1
content-type: application/json
content-length: 32
te:
host: 127.0.0.1:9910
connection: keep-alive
token: axfw34y235wrq234t4ersgw4t

{"payload":"hello","u":"Steven"}
```



## 桥接数据到 MQTT Broker

搭建 MQTT Broker 环境，以 MaxOS X 为例:
```bash
$ brew install mosquitto

启动 mosquitto
$ mosquitto
```
创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```sql
SELECT * FROM "t/#"
```

![image](./assets/rule-engine/rule_sql.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “桥接数据到 MQTT Broker”。

![image](./assets/rule-engine/mqtt-action-0.png)

填写动作参数:

"桥接数据到 MQTT Broker" 动作只需要一个参数：

关联资源。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 MQTT Bridge 资源:

![image](./assets/rule-engine/mqtt-action-1.png)

选择 MQTT Bridge 资源,填写资源配置:

   填写真实的 mosquitto 服务器地址，其他配置保持默认值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “新建” 按钮。

![image](./assets/rule-engine/mqtt-resource-1.png)

返回响应动作界面，点击 “确认”。

![image](./assets/rule-engine/mqtt-action-2.png)

返回规则创建界面，点击 “新建”。

![image](./assets/rule-engine/mqtt-rulesql-1.png)

规则已经创建完成，现在发一条数据:

```bash
Topic: "t/1"

QoS: 0

Payload: "Hello, World\!"
```

然后通过 mqtt 客户端查看消息是否发布成功

![image](./assets/rule-engine/mqtt-result-0.png)

在规则列表里，可以看到刚才创建的规则的命中次数已经增加了 1:

![image](./assets/rule-engine/mqtt-rulelist-0.png)




## 保存数据到 MySQL

搭建 MySQL 数据库，并设置用户名密码为 root/public，以 MacOS X 为例:

```bash
$ brew install mysql

$ brew services start mysql

$ mysql -u root -h localhost -p

ALTER USER 'root'@'localhost' IDENTIFIED BY 'public';
```

初始化 MySQL 表:

```bash
$ mysql -u root -h localhost -ppublic
```

创建 “test” 数据库:
```bash
CREATE DATABASE test;
```
创建 t_mqtt_msg 表:

```sql
USE test;
CREATE TABLE `t_mqtt_msg` (
`id` int(11) unsigned NOT NULL AUTO_INCREMENT,
`msgid` varchar(64) DEFAULT NULL,
`topic` varchar(255) NOT NULL,
`qos` tinyint(1) NOT NULL DEFAULT '0',
`payload` blob,
`arrived` datetime NOT NULL,
PRIMARY KEY (`id`),
INDEX topic_index(`id`, `topic`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8MB4;
```

![image](./assets/rule-engine/mysql_init_1@2x.png)

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```sql
SELECT * FROM "t/#"
```

![image](./assets/rule-engine/rule_sql.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “保存数据到 MySQL”。

![image](./assets/rule-engine/rule_action_1@2x.png)

填写动作参数:

“保存数据到 MySQL” 动作需要两个参数：

1). SQL 模板。这个例子里我们向 MySQL 插入一条数据，SQL
​    模板为:

```sql
insert into t_mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, FROM_UNIXTIME(${timestamp}/1000))
```

![image](./assets/rule-engine/rule_action_2@2x.png)

2). 关联资源的 ID。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 MySQL 资源:

填写资源配置:

数据库名填写 “mqtt”，用户名填写 “root”，密码填写 “123456”

![image](./assets/rule-engine/rule_action_3@2x.png)

点击 “新建” 按钮。

返回响应动作界面，点击 “确认”。

![image](./assets/rule-engine/rule_action_4@2x.png)

返回规则创建界面，点击 “创建”。

![image](./assets/rule-engine/rule_overview_1@2x.png)

在规则列表里，点击 “查看” 按钮或规则 ID 连接，可以预览刚才创建的规则:

![image](./assets/rule-engine/rule_overview_2@2x.png)

规则已经创建完成，现在发一条数据:

```bash
Topic: "t/a"
QoS: 1
Payload: "hello"
```

然后检查 MySQL 表，新的 record 是否添加成功:

![image](./assets/rule-engine/mysql_result_1@2x.png)

## 保存数据到 PostgreSQL

搭建 PostgreSQL 数据库，以 MacOS X 为例:

```bash
$ brew install postgresql
$ brew services start postgresql
```

```bash
## 使用用户名 root 创建名为 'mqtt' 的数据库
$ createdb -U root mqtt

$ psql -U root mqtt

mqtt=> \dn;
List of schemas
Name  | Owner
--------+-------
public | shawn
(1 row)
```

初始化 PgSQL 表:

```bash
$ psql -U root mqtt
```

创建 `t_mqtt_msg` 表:

```sql
CREATE TABLE t_mqtt_msg (
    id SERIAL primary key,
    msgid character varying(64),
    sender character varying(64),
    topic character varying(255),
    qos integer,
    payload text,
    arrived timestamp without time zone
);
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```bash
SELECT * FROM "t/#"
```

![image](./assets/rule-engine/rule_sql.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “保存数据到 PostgreSQL”。

![image](./assets/rule-engine/pgsql-action-0@2x.png)

填写动作参数:

“保存数据到 PostgreSQL” 动作需要两个参数：

1). 关联资源的 ID。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 PostgreSQL 资源:

![image](./assets/rule-engine/pgsql-resource-0@2x.png)

选择 “PostgreSQL 资源”。

填写资源配置:

数据库名填写 “mqtt”，用户名填写 “root”，其他配置保持默认值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “新建” 按钮。

![image](./assets/rule-engine/pgsql-resource-1@2x.png)

返回响应动作界面，点击 “确认”。

2).SQL 模板。这个例子里我们向 PostgreSQL 插入一条数据，SQL
​    模板为:

```bash
insert into t_mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, to_timestamp(${timestamp}::double precision /1000)) returning id
```

插入数据之前，SQL 模板里的 ${key} 占位符会被替换为相应的值。

![image](./assets/rule-engine/pgsql-action-2@2x.png)

返回规则创建界面，点击 “创建”。

![image](./assets/rule-engine/pgsql-rulesql-2@2x.png)

规则已经创建完成，现在发一条数据:

```bash
Topic: "t/1"
QoS: 0
Payload: "hello1"
```

然后检查 PostgreSQL 表，新的 record 是否添加成功:

![image](./assets/rule-engine/pgsql-result-1@2x.png)

在规则列表里，可以看到刚才创建的规则的命中次数已经增加了 1:

![image](./assets/rule-engine/pgsql-rulelist-1@2x.png)

## 保存数据到 Cassandra

搭建 Cassandra 数据库，并设置用户名密码为 root/public，以 MacOS X 为例:

```bash
$ brew install cassandra
## 修改配置，关闭匿名认证
$  vim /usr/local/etc/cassandra/cassandra.yaml

    authenticator: PasswordAuthenticator
    authorizer: CassandraAuthorizer

$ brew services start cassandra

## 创建 root 用户
$ cqlsh -ucassandra -pcassandra

create user root with password 'public' superuser;
```

初始化 Cassandra 表:

```bash
$ cqlsh -uroot -ppublic
```

创建 "test" 表空间:

```bash
CREATE KEYSPACE test WITH replication = {'class': 'SimpleStrategy', 'replication_factor': '1'}  AND durable_writes = true;
```

创建 “t_mqtt_msg” 表:

```sql
USE test;
CREATE TABLE t_mqtt_msg (
    msgid text,
    topic text,
    qos int,
    payload text,
    arrived timestamp,
    PRIMARY KEY (msgid, topic)
);
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:
```bash
SELECT * FROM "t/#"
```

![image](./assets/rule-engine/rule_sql.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “保存数据到 Cassandra”。

![image](./assets/rule-engine/cass-action-0@2x.png)

填写动作参数:

“保存数据到 Cassandra” 动作需要两个参数：

1). 关联资源的 ID。初始状况下，资源下拉框为空，现点击右上角的 “新建资源” 来创建一个 Cassandra 资源。

![image](./assets/rule-engine/cass-resoure-0.png)

填写资源配置:

Keysapce 填写 “test”，用户名填写 “root”，密码填写 “public” 其他配置保持默认值，然后点击 “测试连接”
按钮，确保连接测试成功。

![image](./assets/rule-engine/cass-resoure-1.png)

点击 “新建” 按钮，完成资源的创建。

2). SQL 模板。这个例子里我们向 Cassandra 插入一条数据，SQL
​    模板为:

```sql
insert into t_mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, ${timestamp})
```

插入数据之前，SQL 模板里的 ${key} 占位符会被替换为相应的值。

![image](./assets/rule-engine/cass-resoure-2.png)

在点击 “新建” 完成规则创建

![image](./assets/rule-engine/cass-rule-overview.png)

现在发送一条数据，测试该规则:

```bash
Topic: "t/cass"
QoS: 1
Payload: "hello"
```

然后检查 Cassandra 表，可以看到该消息已成功保存:

![image](./assets/rule-engine/cass-rule-result@2x.png)

在规则列表里，可以看到刚才创建的规则的命中次数已经增加了 1:

![image](./assets/rule-engine/cass-rule-result@3x.png)





## 保存数据到 MongoDB

搭建 MongoDB 数据库，并设置用户名密码为 root/public，以 MacOS X 为例:

```bash
$ brew install mongodb
$ brew services start mongodb

## 新增 root/public 用户
$ use mqtt;
$ db.createUser({user: "root", pwd: "public", roles: [{role: "readWrite", db: "mqtt"}]});

## 修改配置，关闭匿名认证
$ vim /usr/local/etc/mongod.conf

    security:
    authorization: enabled

$ brew services restart mongodb
```

初始化 MongoDB 表:

```bash
$ mongo 127.0.0.1/mqtt -uroot -ppublic
db.createCollection("t_mqtt_msg");
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```bash
SELECT id as msgid, topic, qos, payload, publish_received_at as arrived FROM "t/#"
```

![image](./assets/rule-engine/mongodb_data_to_store1.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “保存数据到 MongoDB”。

![image](./assets/rule-engine/mongo-action-0@2x.png)

填写动作参数:

“保存数据到 MongoDB” 动作需要三个参数：

1). 关联资源的 ID。初始状况下，资源下拉框为空，现点击右上角的 “新建资源” 来创建一个 MongoDB 单节点 资源。

![image](./assets/rule-engine/mongodb_data_to_store2.png)

填写资源配置:

数据库名称 填写 “mqtt”，用户名填写 “root”，密码填写 “public”，连接认证源填写 “mqtt”
其他配置保持默认值，然后点击 “测试连接” 按钮，确保连接测试成功。

![image](./assets/rule-engine/mongo-resoure-1.png)

点击 “新建” 按钮，完成资源的创建。

2). Collection 名称。这个例子我们向刚刚新建的 collection 插入数据，填 “t_mqtt\_msg”

3). Payload Tmpl 模板。这个例子里我们向 MongoDB 插入一条数据，模板为空, 插入的数据是上面SQL语句select出来的结果用json格式写入到MongoDB中


![](./assets/rule-engine/mongodb_data_to_store3.png)

在点击 “新建” 完成规则创建

![image](./assets/rule-engine/mongodb_data_to_store4.png)

现在发送一条数据，测试该规则:

```bash
Topic: "t/mongo"
QoS: 1
Payload: "hello"
```

然后检查 MongoDB 表，可以看到该消息已成功保存:

![image](./assets/rule-engine/mongo-rule-result@2x.png)

在规则列表里，可以看到刚才创建的规则的命中次数已经增加了 1:

![image](./assets/rule-engine/mongo-rule-result@3x.png)





## 保存数据到 DynamoDB

搭建 DynamoDB 数据库，以 MacOS X 为例:

```bash
$ brew install dynamodb-local
$ dynamodb-local
```

创建 DynamoDB 表定义文件 mqtt\_msg.json :

<!-- end list -->

```json
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

初始化 DynamoDB 表:

```bash
$ aws dynamodb create-table --cli-input-json file://mqtt_msg.json --endpoint-url http://localhost:8000
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```sql
SELECT id, topic, payload FROM "#"
```

![image](./assets/rule-engine/dynamo-rulesql-0.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “保存数据到 DynamoDB”。

![image](./assets/rule-engine/dynamo-action-0.png)

填写动作参数:

“保存数据到 DynamoDB” 动作需要两个参数：

1). DynamoDB 表名。这个例子里我们设置的表名为 "mqtt\_msg"

2). DynamoDB Hash Key。这个例子里我们设置的 Hash Key 要与表定义的一致

3). DynamoDB Range Key。由于我们表定义里没有设置 Range Key。这个例子里我们把 Range Key 设置为空。

![image](./assets/rule-engine/dynamo-action-1.png)

4). 关联资源的 ID。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 DynamoDB 资源:

填写资源配置:

![image](./assets/rule-engine/dynamo-resource-1.png)

点击 “新建” 按钮。

返回响应动作界面，点击 “确认”。

![image](./assets/rule-engine/dynamo-action-2.png)

返回规则创建界面，点击 “新建”。

![image](./assets/rule-engine/dynamo-rulesql-1.png)

规则已经创建完成，现在发一条数据:

```bash
Topic: "t/a"
QoS: 1
Payload: "hello"
```

然后检查 DynamoDB 的 mqtt\_msg 表，新的 record 是否添加成功:

![image](./assets/rule-engine/dynamo-result-0.png)

在规则列表里，可以看到刚才创建的规则的命中次数已经增加了 1:

![image](./assets/rule-engine/dynamo-result-1.png)

## 保存数据到 Redis

搭建 Redis 环境，以 MaxOS X 为例:

```bash
 $ wget http://download.redis.io/releases/redis-4.0.14.tar.gz
$ tar xzf redis-4.0.14.tar.gz
$ cd redis-4.0.14
$ make && make install

# 启动 redis
$ redis-server
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```bash
SELECT * FROM "t/#"
```

![image](./assets/rule-engine/rule_sql.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “保存数据到 Redis”。

![image](./assets/rule-engine/redis-action-0@2x.png)

填写动作参数:

“保存数据到 Redis 动作需要两个参数：

1). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 Redis 资源:

![image](./assets/rule-engine/redis-resource-0@2x.png)

选择 Redis 单节点模式资源”。

填写资源配置:

   填写真实的 Redis 服务器地址，其他配置保持默认值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “新建” 按钮。

![image](./assets/rule-engine/redis-resource-2@2x.png)

返回响应动作界面，点击 “确认”。

2). Redis 的命令:

```bash
HMSET mqtt:msg:${id} id ${id} from ${client_id} qos ${qos} topic ${topic} payload ${payload} ts ${timestamp}
```

![image](./assets/rule-engine/redis-action-1@2x.png)

返回规则创建界面，点击 “新建”。

![image](./assets/rule-engine/redis-rulesql-1@2x.png)

规则已经创建完成，现在发一条数据:

```bash
Topic: "t/1"

QoS: 0

Payload: "hello"
```

然后通过 Redis 命令去查看消息是否生产成功:

```bash
$ redis-cli

KEYS mqtt:msg\*

hgetall Key
```

![image](./assets/rule-engine/redis-cli.png)

在规则列表里，可以看到刚才创建的规则的命中次数已经增加了 1:

![image](./assets/rule-engine/redis-rulelist-0@2x.png)

## 保存数据到 ClickHouse

搭建 ClickHouse 数据库，并设置用户名密码为 default/public，以 CentOS 为例:

```bash
## 安装依赖
sudo yum install -y epel-release

## 下载并运行packagecloud.io提供的安装shell脚本
curl -s https://packagecloud.io/install/repositories/altinity/clickhouse/script.rpm.sh | sudo bash

## 安装ClickHouse服务器和客户端
sudo yum install -y clickhouse-server clickhouse-client

## 启动ClickHouse服务器
clickhouse-server

## 启动ClickHouse客户端程序
clickhouse-client
```

创建 “test” 数据库:
```bash
create database test;
```
创建 t_mqtt_msg 表:

```sql
use test;
create table t_mqtt_msg (msgid Nullable(String), topic Nullable(String), clientid Nullable(String), payload Nullable(String)) engine = Log;
```

![](./assets/rule-engine/clickhouse_0.png)

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```sql
SELECT * FROM "#"
```

![image](./assets/rule-engine/clickhouse_1.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “保存数据到 ClickHouse”。

![image](./assets/rule-engine/clickhouse_2.png)

填写动作参数:

“保存数据到 ClickHouse” 动作需要两个参数：

1). 关联资源的 ID。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 ClickHouse 资源:

![image](./assets/rule-engine/clickhouse_3.png)

选择 “ClickHouse 资源”。

填写资源配置:

![image](./assets/rule-engine/clickhouse_4.png)

点击 “新建” 按钮。

2). SQL 模板。这个例子里我们向 ClickHouse 插入一条数据，SQL
​    模板为:

```sql
insert into test.t_mqtt_msg(msgid, clientid, topic, payload) values ('${id}', '${clientid}', '${topic}', '${payload}')
```

![image](./assets/rule-engine/clickhouse_5.png)

返回响应动作界面，点击 “确认”。

![image](./assets/rule-engine/clickhouse_6.png)

在规则列表里，点击 “查看” 按钮或规则 ID 连接，可以预览刚才创建的规则:

![image](./assets/rule-engine/clickhouse_7.png)

规则已经创建完成，现在发一条数据:

```bash
Topic: "t/a"
QoS: 1
Payload: "hello"
```

然后检查 ClickHouse 表，新的 record 是否添加成功:

![image](./assets/rule-engine/clickhouse_8.png)




## 保存数据到 OpenTSDB

搭建 OpenTSDB 数据库环境，以 MaxOS X 为例:

```bash
$ docker pull petergrace/opentsdb-docker

$ docker run -d --name opentsdb -p 4242:4242 petergrace/opentsdb-docker
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```sql
SELECT
    payload as p,
    p.metric as metric, p.tags as tags, p.value as value
FROM
    "#"
```

![image](./assets/rule-engine/opentsdb-rulesql-0@2x.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “保存数据到 OpenTSDB”。

![image](./assets/rule-engine/opentsdb-action-0@2x.png)

填写动作参数:

“保存数据到 OpenTSDB” 动作需要六个参数:

1). 详细信息。是否需要 OpenTSDB Server 返回存储失败的 data point 及其原因的列表，默认为 false。

2). 摘要信息。是否需要 OpenTSDB Server 返回 data point 存储成功与失败的数量，默认为 true。

3). 最大批处理数量。消息请求频繁时允许 OpenTSDB 驱动将多少个 Data Points 合并为一次请求，默认为 20。

4). 是否同步调用。指定 OpenTSDB Server 是否等待所有数据都被写入后才返回结果，默认为 false。

5). 同步调用超时时间。同步调用最大等待时间，默认为 0。

6). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 OpenTSDB 资源:

![image](./assets/rule-engine/opentsdb-action-1@2x.png)

选择 “OpenTSDB 资源”:

填写资源配置:

本示例中所有配置保持默认值即可，点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “新建” 按钮。

![image](./assets/rule-engine/opentsdb-resource-1@2x.png)

返回响应动作界面，点击 “确认”。

![image](./assets/rule-engine/opentsdb-action-2@2x.png)

返回规则创建界面，点击 “新建”。

![image](./assets/rule-engine/opentsdb-rulesql-1@2x.png)

规则已经创建完成，现在发一条消息:

```bash
Topic: "t/1"

QoS: 0

Payload: "{"metric":"cpu","tags":{"host":"serverA"},"value":12}"
```

我们通过 Postman 或者 curl 命令，向 OpenTSDB Server 发送以下请求:

```bash
POST /api/query HTTP/1.1
Host: 127.0.0.1:4242
Content-Type: application/json
cache-control: no-cache
Postman-Token: 69af0565-27f8-41e5-b0cd-d7c7f5b7a037
{
    "start": 1560409825000,
    "queries": [
        {
            "aggregator": "last",
            "metric": "cpu",
            "tags": {
                "host": "*"
            }
        }
    ],
    "showTSUIDs": "true",
    "showQuery": "true",
    "delete": "false"
}
------WebKitFormBoundary7MA4YWxkTrZu0gW--
```

如果 data point 存储成功，将会得到以下应答:

```json
[
  {
      "metric": "cpu",
      "tags": {
          "host": "serverA"
      },
      "aggregateTags": [],
      "query": {
          "aggregator": "last",
          "metric": "cpu",
          "tsuids": null,
          "downsample": null,
          "rate": false,
          "filters": [
              {
                  "tagk": "host",
                  "filter": "*",
                  "group_by": true,
                  "type": "wildcard"
              }
          ],
          "index": 0,
          "tags": {
              "host": "wildcard(*)"
          },
          "rateOptions": null,
          "filterTagKs": [
              "AAAC"
          ],
          "explicitTags": false
      },
      "tsuids": [
          "000002000002000007"
      ],
      "dps": {
          "1561532453": 12
      }
  }
]
```

在规则列表里，可以看到刚才创建的规则的命中次数已经增加了 1:

![image](./assets/rule-engine/opentsdb-rulelist-1@2x.png)



## 保存数据到 TDengine

[TDengine](https://github.com/taosdata/TDengine) 是[涛思数据](https://www.taosdata.com/cn/)推出的一款开源的专为物联网、车联网、工业互联网、IT 运维等设计和优化的大数据平台。除核心的快 10 倍以上的时序数据库功能外，还提供缓存、数据订阅、流式计算等功能，最大程度减少研发和运维的复杂度。

EMQ X 支持通过 **发送到 Web 服务** 的方式保存数据到 TDengine，也在企业版上提供原生的 TDengine 驱动实现直接保存。

使用 Docker 安装 TDengine 或在 [Cloud](https://marketplace.huaweicloud.com/product/OFFI454488918838128640) 上部署：

```bash
docker run --name TDengine -d -p 6020-6050:6020-6050/udp TDengine/TDengine 
```

进入 Docker 容器：

```bash
docker exec -it TDengine bash
taos
```

创建 “test” 数据库:
```bash
create database test;
```
创建 t_mqtt_msg 表，关于 TDengine 数据结构以及 SQL 命令参见 [TAOS SQL](https://www.taosdata.com/cn/documentation/taos-sql/#表管理)：

```sql
use test;
CREATE TABLE t_mqtt_msg (
  ts timestamp,
 	msgid NCHAR(64),
  topic NCHAR(255), 
  qos TINYINT,
  payload BINARY(1024),
  arrived timestamp
);
```

![image-20200729163951206](./assets/rule-engine/image-20200729163951206.png)


创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```sql
SELECT * FROM "t/#"
```

![image](./assets/rule-engine/rule_sql.png)

后续动作创建操作可以根据你的 EMQ X 版本灵活选择。

### 原生方式（企业版）

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “保存数据到 TDengine”。

> 仅限企业版 4.1.1 及以后版本。

填写动作参数:

“保存数据到 TDengine” 动作需要两个参数：

1). SQL 模板。这个例子里我们向 TDengine 插入一条数据，注意我们应当在 SQL 中指定数据库名，字符类型也要用单引号括起来，SQL 模板为：

```sql
insert into test.t_mqtt_msg(ts, msgid, topic, qos, payload) values (now, '${id}', '${topic}', ${qos}, '${payload}')
```

![image-20200729164158454](./assets/rule-engine/image-20200729164158454.png)

2). 关联资源的 ID。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 TDengine资源:

填写资源配置:

用户名填写 “root”，密码填写缺省密码 “taosdata”，**TDengine 不在资源中配置数据库名，请在 SQL 中自行配置。**

![image-20200729165651951](./assets/rule-engine/image-20200729165651951.png)

点击 “新建” 按钮。

返回响应动作界面，点击 “确认”。

![image-20200729174211581](./assets/rule-engine/image-20200729174211581.png)

返回规则创建界面，点击 “创建”。


### 通过发送数据到 Web 服务写入

为支持各种不同类型平台的开发，TDengine 提供符合 REST 设计标准的 API。通过 [RESTful Connector](https://www.taosdata.com/cn/documentation/connector/#RESTful-Connector) 提供了最简单的连接方式，即使用 HTTP 请求携带认证信息与要执行的 SQL 操作 TDengine。

EMQ X 规则引擎中有功能强大的**发送数据到 Web 服务功能**，可以实现无缝实现上述操作。

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “保存数据到 Web 服务“。

EMQ X 规则引擎中有功能强大的***\*发送数据到 Web 服务功能\****，可以实现无缝实现上述操作。

填写动作参数:

“保存数据到 Web 服务” 动作需要两个参数：

1). 消息内容模板，即 HTTP 请求体。这个例子里我们向 TDengine 插入一条数据，应当在请求体内拼接携带 INSERT SQL。注意我们应当在 SQL 中指定数据库名，字符类型也要用单引号括起来， 消息内容模板为：

```sql
insert into test.t_mqtt_msg(ts, msgid, topic, qos, payload) values (now, '${id}', '${topic}', ${qos}, '${payload}')
```

2). 关联资源的 ID。现在资源下拉框为空，可以点击旁边的 “新建” 来创建一个 Web 服务资源:

填写资源配置:

请求 URL 填写 http://127.0.0.1:6020/rest/sql ，请求方法选择 POST;
**还需添加 Authorization 请求头作为认证信息**，请求头的值为 Basic + TDengine {username}:{password} 经过Base64 编码之后的字符串, 例如默认的 root:taosdata 编码后为 `cm9vdDp0YW9zZGF0YQ==`，
填入的值为 `Basic cm9vdDp0YW9zZGF0YQ==`。

![image-20200730093728092](./assets/rule-engine/tdengine-webhook.png)

点击 “新建” 按钮。

返回响应动作界面，点击 “确认”。

![image-20200730093457366](./assets/rule-engine/image-20200730093457366.png)

返回规则创建界面，点击 “创建”。


### 测试

在规则列表里，点击 “查看” 按钮或规则 ID 连接，可以预览刚才创建的规则:

![image-20200729165826748](./assets/rule-engine/image-20200729165826748.png)

规则已经创建完成，现在发一条数据:

```bash
Topic: "t/a"
QoS: 1
Payload: "hello"
```

然后检查 TDengine 表，新的 record 是否添加成功:

```sql
select * from t_mqtt_msg;
```


![image-20200729174914518](./assets/rule-engine/image-20200729174914518.png)


## 保存数据到 TimescaleDB

搭建 TimescaleDB 数据库环境，以 MaxOS X 为例:

```bash
$ docker pull timescale/timescaledb

$ docker run -d --name timescaledb -p 5432:5432 -e POSTGRES_PASSWORD=password timescale/timescaledb:latest-pg11

$ docker exec -it timescaledb psql -U postgres

## 创建并连接 tutorial 数据库
CREATE database tutorial;

\c tutorial

CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE;
```

初始化 TimescaleDB 表:

```bash
$ docker exec -it timescaledb psql -U postgres -d tutorial
```

创建 `conditions` 表:

```sql
CREATE TABLE conditions (
    time        TIMESTAMPTZ       NOT NULL,
    location    TEXT              NOT NULL,
    temperature DOUBLE PRECISION  NULL,
    humidity    DOUBLE PRECISION  NULL
);

SELECT create_hypertable('conditions', 'time');
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```sql
SELECT
    payload as p,
    p.temp as temp,
    p.humidity as humidity,
    p.location as location
FROM
    "#"
```

![image](./assets/rule-engine/timescaledb-rulesql-0@2x.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “保存数据到 TimescaleDB”。

![image](./assets/rule-engine/timescaledb-action-0@2x.png)

填写动作参数:

“保存数据到 TimescaleDB” 动作需要两个参数：

1). SQL 模板。这个例子里我们向 TimescaleDB 插入一条数据，SQL
​    模板为:

```sql
insert into conditions(time, location, temperature, humidity) values (NOW(), ${location}, ${temp}, ${humidity})
```

插入数据之前，SQL 模板里的 ${key} 占位符会被替换为相应的值。

2). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 TimescaleDB 资源:

![image](./assets/rule-engine/timescaledb-resource-0@2x.png)

选择 “TimescaleDB 资源”。

填写资源配置:

数据库名填写 “tutorial”，用户名填写 “postgres”，密码填写 “password”，其他配置保持默认值，然后点击
“测试连接” 按钮，确保连接测试成功。

最后点击 “新建” 按钮。

![image](./assets/rule-engine/timescaledb-resource-2@2x.png)

返回响应动作界面，点击 “确认”。

![image](./assets/rule-engine/timescaledb-action-1@2x.png)

返回规则创建界面，点击 “新建”。

![image](./assets/rule-engine/timescaledb-rulesql-1@2x.png)

规则已经创建完成，现在发一条数据:

```bash
Topic: "t/1"

QoS: 0

Payload: "{"temp":24,"humidity":30,"location":"hangzhou"}"
```

然后检查 TimescaleDB 表，新的 record 是否添加成功:

```bash
tutorial=# SELECT * FROM conditions LIMIT 100;
            time              | location | temperature | humidity
-------------------------------+----------+-------------+----------
2019-06-27 01:41:08.752103+00 | hangzhou |          24 |       30
```

在规则列表里，可以看到刚才创建的规则的命中次数已经增加了 1:

![image](./assets/rule-engine/timescaledb-rulelist-0@2x.png)

## 保存数据到 InfluxDB

搭建 InfluxDB 数据库环境，以 macOS X 为例:

```bash
$ docker pull influxdb

$ docker run --name=influxdb --rm -d -p 8086:8086 -p 8089:8089/udp -v ${PWD}/files/influxdb.conf:/etc/influxdb/influxdb.conf influxdb:latest

```

EMQ X 仅支持通过 UDP 协议连接 InfluxDB，需要修改 InfluxDB 配置文件：

```bash
[[udp]]
  enabled = true
  bind-address = ":8089"
  # 消息保存的数据库
  database = "emqx"

  # InfluxDB precision for timestamps on received points ("" or "n", "u", "ms", "s", "m", "h")
  # EMQ X 默认时间戳是毫秒
  precision = "ms"

  # 其他配置根据需要自行修改
  #   batch-size = 1000
  #   batch-pending = 5
  #   batch-timeout = "5s"
  #   read-buffer = 1024
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:


```bash
SELECT
    payload as p,
    p.host as host,
    p.location as location,
    p.internal as internal,
    p.external as external
FROM
    "#"
```

![image](./assets/rule-engine/influxdb-rulesql-0@2x.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “保存数据到 InfluxDB”。

![image](./assets/rule-engine/influxdb-action-0@2x.png)

填写动作参数:

“保存数据到 InfluxDB” 动作需要六个参数：

1). Measurement。指定写入到 InfluxDB 的 data point 的 measurement。

2). Field Keys。指定写入到 InfluxDB 的 data point 的 fields 的值从哪里获取。

3). Tags Keys。指定写入到 InfluxDB 的 data point 的 tags 的值从哪里获取。

4). Timestamp Key。指定写入到 InfluxDB 的 data point 的 timestamp 的值从哪里获取。

5). 设置时间戳。未指定 Timestamp Key 时是否自动生成。

6). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 InfluxDB 资源:

![image](./assets/rule-engine/influxdb-action-1@2x.png)

选择 “InfluxDB 资源”:

![image](./assets/rule-engine/influxdb-resource-0@2x.png)

填写资源配置:

本示例中所有配置保持默认值即可，点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “新建” 按钮。

![image](./assets/rule-engine/influxdb-resource-1@2x.png)

返回响应动作界面，点击 “确认”。
返回规则创建界面，点击 “新建”。

![image](./assets/rule-engine/influxdb-rulesql-1@2x.png)

规则已经创建完成，现在发一条消息:

```bash
Topic: "t/1"

QoS: 0

Payload:
"{"host":"serverA","location":"roomA","internal":25,"external":37}"
```

然后检查 InfluxDB，新的 data point 是否添加成功:

```bash
$ docker exec -it influxdb influx

use db
Using database db
select * from "temperature"
name: temperature
time                external host    internal location
----                -------- ----    -------- --------
1561535778444457348 35       serverA 25       roomA
```

在规则列表里，可以看到刚才创建的规则的命中次数已经增加了 1:

![image](./assets/rule-engine/influxdb-rulelist-0@2x.png)

## 桥接数据到 Kafka

搭建 Kafka 环境，以 MaxOS X 为例:
```bash
$ wget http://apache.claz.org/kafka/2.3.0/kafka_2.12-2.3.0.tgz

$ tar -xzf  kafka_2.12-2.3.0.tgz

$ cd kafka_2.12-2.3.0

# 启动 Zookeeper
$ ./bin/zookeeper-server-start.sh config/zookeeper.properties
# 启动 Kafka
$ ./bin/kafka-server-start.sh config/server.properties
```

创建 Kafka
​       的主题:

```bash
$ ./bin/kafka-topics.sh --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic testTopic --create
```

::: danger
创建 Kafka Rule 之前必须先在 Kafka 中创建好主题，否则创建 Kafka Rule 失败。
:::

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```sql
SELECT * FROM "t/#"
```

![image](./assets/rule-engine/rule_sql.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “桥接数据到 Kafka”。

![image](./assets/rule-engine/kafka-action-0@2x.png)

填写动作参数:

“保存数据到 Kafka 动作需要两个参数：

1). Kafka 的消息主题

2). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 Kafka 资源:

![image](./assets/rule-engine/kafka-resource-0@2x.png)

选择 Kafka 资源”。

填写资源配置:

填写真实的 Kafka 服务器地址，多个地址用,分隔，其他配置保持默认值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “新建” 按钮。

![image](./assets/rule-engine/kafka-resource-2@2x.png)

返回响应动作界面，点击 “确认”。

![image](./assets/rule-engine/kafka-action-1@2x.png)

返回规则创建界面，点击 “新建”。

![image](./assets/rule-engine/kafka-rulesql-1@2x.png)

规则已经创建完成，现在发一条数据:

```bash
Topic: "t/1"

QoS: 0

Payload: "hello"
```

然后通过 Kafka 命令去查看消息是否生产成功:

```bash
$ ./bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testTopic --from-beginning
```

![image](./assets/rule-engine/kafka-consumer.png)

在规则列表里，可以看到刚才创建的规则的命中次数已经增加了 1:

![image](./assets/rule-engine/kafka-rulelist-0@2x.png)

## 桥接数据到 Pulsar

搭建 Pulsar 环境，以 MaxOS X 为例:

```bash
$ wget http://apache.mirrors.hoobly.com/pulsar/pulsar-2.3.2/apache-pulsar-2.3.2-bin.tar.gz

$ tar xvfz apache-pulsar-2.3.2-bin.tar.gz

$ cd apache-pulsar-2.3.2

# 启动 Pulsar
$ ./bin/pulsar standalone
```

创建 Pulsar 的主题:

```bash
$ ./bin/pulsar-admin topics create-partitioned-topic -p 5 testTopic
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```bash
SELECT * FROM "t/#"
```

![image](./assets/rule-engine/rule_sql.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “桥接数据到 Pulsar”。

![image](./assets/rule-engine/pulsar-action-0@2x.png)

填写动作参数:

“保存数据到 Pulsar 动作需要两个参数：

1). Pulsar 的消息主题

2). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 Pulsar 资源:

![image](./assets/rule-engine/pulsar-resource-0@2x.png)

选择 Pulsar 资源”。

填写资源配置:

   填写真实的 Pulsar 服务器地址，多个地址用,分隔，其他配置保持默认值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “新建” 按钮。

![image](./assets/rule-engine/pulsar-resource-2@2x.png)

返回响应动作界面，点击 “确认”。

![image](./assets/rule-engine/pulsar-action-1@2x.png)

返回规则创建界面，点击 “新建”。

![image](./assets/rule-engine/pulsar-rulesql-1@2x.png)

规则已经创建完成，现在发一条数据:

```bash
Topic: "t/1"

QoS: 0

Payload: "hello"
```

然后通过 Pulsar 命令去查看消息是否生产成功:

```bash
$ ./bin/pulsar-client consume testTopic  -s "sub-name" -n 1000
```

![image](./assets/rule-engine/pulsar-consumer.png)

在规则列表里，可以看到刚才创建的规则的命中次数已经增加了 1:

![image](./assets/rule-engine/pulsar-rulelist-0@2x.png)

## 桥接数据到 RocketMQ

搭建 RocketMQ 环境，以 MaxOS X
​       为例:

```bash
$ wget http://mirror.metrocast.net/apache/rocketmq/4.5.2/rocketmq-all-4.5.2-bin-release.zip

$ unzip rocketmq-all-4.5.2-bin-release.zip

$ cd rocketmq-all-4.5.2-bin-release

# 在conf/broker.conf添加了2个配置
brokerIP1 = 127.0.0.1
autoCreateTopicEnable = true

# 启动 RocketMQ NameServer
$ ./bin/mqnamesrv

# 启动 RocketMQ Broker
$ ./bin/mqbroker -n localhost:9876 -c conf/broker.conf
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```sql
SELECT * FROM "t/#"
```

![image](./assets/rule-engine/rule_sql.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “桥接数据到 RocketMQ”。

![image](./assets/rule-engine/rocket-action-0@2x.png)

填写动作参数:

“保存数据到 RocketMQ 动作需要两个参数：

1). RocketMQ 的消息主题

2). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 RocketMQ 资源:

![image](./assets/rule-engine/rocket-resource-0@2x.png)

填写资源配置:

   填写真实的 RocketMQ 服务器地址，多个地址用,分隔，其他配置保持默认值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “新建” 按钮。

![image](./assets/rule-engine/rocket-resource-2@2x.png)

返回响应动作界面，点击 “确认”。

![image](./assets/rule-engine/rocket-action-1@2x.png)

返回规则创建界面，点击 “新建”。

![image](./assets/rule-engine/rocket-rulesql-1@2x.png)

规则已经创建完成，现在发一条数据:

```bash
Topic: "t/1"

QoS: 0

Payload: "hello"
```

然后通过 RocketMQ 命令去查看消息是否生产成功:
```bash
$ ./bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer TopicTest
```

![image](./assets/rule-engine/rocket-consumer.png)

在规则列表里，可以看到刚才创建的规则的命中次数已经增加了 1:

![image](./assets/rule-engine/rocket-rulelist-0@2x.png)

## 桥接数据到 RabbitMQ

搭建 RabbitMQ 环境，以 MaxOS X 为例:

```bash
$ brew install rabbitmq

# 启动 rabbitmq
$ rabbitmq-server
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```sql
SELECT * FROM "t/#"
```

![image](./assets/rule-engine/rule_sql.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “桥接数据到 RabbitMQ”。

![image](./assets/rule-engine/rabbit-action-0.png)

填写动作参数:

“桥接数据到 RabbitMQ 动作需要四个参数：

1). RabbitMQ Exchange。这个例子里我们设置 Exchange 为 "messages"，

2). RabbitMQ Exchange Type。这个例子我们设置 Exchange Type 为 "topic"

3). RabbitMQ Routing Key。这个例子我们设置 Routing Key 为 "test"

4). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 RabbitMQ 资源:

![image](./assets/rule-engine/rabbit-action-1.png)

选择 RabbitMQ 资源。

填写资源配置:

   填写真实的 RabbitMQ 服务器地址，其他配置保持默认值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “新建” 按钮。

![image](./assets/rule-engine/rabbit-resource-1.png)

返回响应动作界面，点击 “确认”。

![image](./assets/rule-engine/rabbit-action-2.png)

返回规则创建界面，点击 “新建”。

![image](./assets/rule-engine/rabbit-rulesql-1.png)

规则已经创建完成，现在发一条数据:

```bash
Topic: "t/1"

QoS: 0

Payload: "Hello, World\!"
```

编写 amqp 协议的客户端，以下是用 python 写的 amqp 客户端的示例代码:

```python
#!/usr/bin/env python
import pika

connection = pika.BlockingConnection(
    pika.ConnectionParameters(host='localhost'))
channel = connection.channel()

channel.exchange_declare(exchange='messages', exchange_type='topic')

result = channel.queue_declare(queue='', exclusive=True)
queue_name = result.method.queue

channel.queue_bind(exchange='messages', queue=queue_name, routing_key='test')

print('[*] Waiting for messages. To exit press CTRL+C')

def callback(ch, method, properties, body):
    print(" [x] %r" % body)

channel.basic_consume(
    queue=queue_name, on_message_callback=callback, auto_ack=True)

channel.start_consuming()
```

然后通过 amqp 协议的客户端查看消息是否发布成功, 以下是

![image](./assets/rule-engine/rabbit-subscriber-0.png)

在规则列表里，可以看到刚才创建的规则的命中次数已经增加了 1:

![image](./assets/rule-engine/rabbit-rulelist-0.png)


## 桥接数据到 RPC 服务

搭建 EMQ X Broker 环境，以 MaxOS X 为例:

```bash
$ brew tap emqx/emqx/emqx

$ brew install emqx

# 启动 emqx
$ emqx console
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:
```sql
SELECT * FROM "t/#"
```
![image](./assets/rule-engine/rule_sql.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “桥接数据到 MQTT Broker”。

![image](./assets/rule-engine/rpc-action-0.png)

填写动作参数:

桥接数据到 MQTT Broker 动作只需要一个参数：

关联资源。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 RPC Bridge 资源:

![image](./assets/rule-engine/rpc-action-1.png)

选择 RPC Bridge 资源。

填写资源配置:

   填写真实的 emqx 节点名，其他配置保持默认值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “新建” 按钮。

![image](./assets/rule-engine/rpc-resource-1.png)

返回响应动作界面，点击 “确认”。

![image](./assets/rule-engine/rpc-action-2.png)

返回规则创建界面，点击 “新建”。

![image](./assets/rule-engine/rpc-rulesql-1.png)

规则已经创建完成，现在发一条数据:

```bash
Topic: "t/1"

QoS: 0

Payload: "Hello, World\!"
```

然后通过 mqtt 客户端查看消息是否发布成功

![image](./assets/rule-engine/rpc-result-0.png)

在规则列表里，可以看到刚才创建的规则的命中次数已经增加了 1:

![image](./assets/rule-engine/rpc-rulelist-0.png)

## 离线消息保存到 Redis

搭建 Redis 环境，以 MaxOS X 为例:

```bash
 $ wget http://download.redis.io/releases/redis-4.0.14.tar.gz
$ tar xzf redis-4.0.14.tar.gz
$ cd redis-4.0.14
$ make && make install

# 启动 redis
$ redis-server
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

然后填写规则 SQL:

FROM说明

 **t/#**: 发布者发布消息触发保存离线消息到Redis

 **$events/session_subscribed**: 订阅者订阅主题触发获取离线消息

 **$events/message_acked**: 订阅者回复消息ACK后触发删除已经被接收的离线消息

```bash
SELECT * FROM "t/#", "$events/session_subscribed", "$events/message_acked" WHERE topic =~ 't/#'
```

![](./assets/rule-engine/offline_msg_1.png)

关联动作:

在 “响应动作” 界面选择 “添加动作”，然后在 “动作” 下拉框里选择 “离线消息保存到 Redis”。

![](./assets/rule-engine/offline_msg_2.png)

填写动作参数:

“离线消息保存到 Redis 动作需要两个参数：

1). Redis Key 超期的 TTL

2). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 Redis 资源:

![](./assets/rule-engine/offline_msg_3.png)

选择 Redis 单节点模式资源”。

![](./assets/rule-engine/offline_msg_4.png)

填写资源配置:

   填写真实的 Redis 服务器地址，其他配置保持默认值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “新建” 按钮。

![](./assets/rule-engine/offline_msg_5.png)

返回响应动作界面，点击 “确认”。

![](./assets/rule-engine/offline_msg_7.png)

返回规则创建界面，点击 “新建”。

![](./assets/rule-engine/offline_msg_6.png)

规则已经创建完成，通过 Dashboard 的 WebSocket 客户端发一条数据**(发布消息的QoS必须大于0)**:

![](./assets/rule-engine/offline_msg_8.png)


消息发送后，通过 Redis CLI 查看到消息被保存到 Redis 里面:

```bash
$ redis-cli

KEYS mqtt:msg\*

hgetall Key
```

![](./assets/rule-engine/offline_msg_10.png)

使用另外一个客户端，订阅主题 "t/1" **(订阅主题的QoS必须大于0，否则消息会被重复接收, 不支持主题通配符方式订阅获取离线消息)**:

![](./assets/rule-engine/offline_msg_11.png)

订阅后马上接收到了保存到 Redis 里面的离线消息:

![](./assets/rule-engine/offline_msg_12.png)

离线消息被接收后会在 Redis 中删除:

![](./assets/rule-engine/offline_msg_13.png)


## 离线消息保存到 MySQL

搭建 MySQL 数据库，并设置用户名密码为 root/public，以 MacOS X 为例:
```bash
$ brew install mysql

$ brew services start mysql

$ mysql -u root -h localhost -p

ALTER USER 'root'@'localhost' IDENTIFIED BY 'public';
```

初始化 MySQL 数据库:
```bash
$ mysql -u root -h localhost -ppublic

create database mqtt;
```

创建 mqtt_msg 表:
```sql
DROP TABLE IF EXISTS `mqtt_msg`;
CREATE TABLE `mqtt_msg` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `msgid` varchar(64) DEFAULT NULL,
  `topic` varchar(180) NOT NULL,
  `sender` varchar(64) DEFAULT NULL,
  `qos` tinyint(1) NOT NULL DEFAULT '0',
  `retain` tinyint(1) DEFAULT NULL,
  `payload` blob,
  `arrived` datetime NOT NULL,
  PRIMARY KEY (`id`),
  INDEX topic_index(`id`, `topic`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8MB4;
```

::: danger

消息表结构不能修改，请使用上面SQL语句创建

:::

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的“规则”选项卡。

然后填写规则 SQL:

FROM说明

​	**t/#**: 发布者发布消息触发保存离线消息到MySQL

​	**$events/session_subscribed**: 订阅者订阅主题触发获取离线消息

​	**$events/message_acked**: 订阅者回复消息ACK后触发删除已经被接收的离线消息

```bash
SELECT * FROM "t/#", "$events/session_subscribed", "$events/message_acked" WHERE topic =~ 't/#'
```

![](./assets/rule-engine/mysql_offline_msg_01.png)

关联动作:

在“响应动作”界面选择“添加动作”，然后在“动作”下拉框里选择“离线消息保存到 MySQL”。

![](./assets/rule-engine/mysql_offline_msg_02.png)


现在资源下拉框为空，可以点击右上角的 “新建” 来创建一个 MySQL 资源:

![](./assets/rule-engine/mysql_offline_msg_03.png)

弹出一个“创建资源”对话框

![](./assets/rule-engine/mysql_offline_msg_04.png)

填写资源配置:

填写真实的 MySQL 服务器地址，其他配置填写相应的值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “确定” 按钮。

![](./assets/rule-engine/mysql_offline_msg_05.png)

返回响应动作界面，点击 “确认”。

![](./assets/rule-engine/mysql_offline_msg_06.png)

返回规则创建界面，点击 “创建”。

![](./assets/rule-engine/mysql_offline_msg_07.png)

规则已经创建完成，通过 Dashboard 的 WebSocket 客户端发一条数据**(发布消息的QoS必须大于0)**:

![](./assets/rule-engine/mysql_offline_msg_08.png)

消息发送后，通过 mysql 查看到消息被保存到 MySQL 里面:

![](./assets/rule-engine/mysql_offline_msg_09.png)

使用另外一个客户端，订阅主题 "t/1" (订阅主题的QoS必须大于0，否则消息会被重复接收):

![](./assets/rule-engine/mysql_offline_msg_10.png)

订阅后马上接收到了保存到 MySQL 里面的离线消息:

![](./assets/rule-engine/mysql_offline_msg_11.png)

离线消息被接收后会在 MySQL 中删除:

![](./assets/rule-engine/mysql_offline_msg_12.png)


## 离线消息保存到 PostgreSQL

搭建 PostgreSQL 数据库，以 MacOS X 为例:
```bash
$ brew install postgresql
$ brew services start postgresql
```

创建 mqtt 数据库:

```
# 使用用户名 postgres 创建名为 'mqtt' 的数据库
$ createdb -U postgres mqtt

$ psql -U postgres mqtt

mqtt=> \dn;
List of schemas
Name  | Owner
--------+-------
public | postgres
(1 row)
```

创建 mqtt_msg 表:

```sql
$ psql -U postgres mqtt

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

::: danger

消息表结构不能修改，请使用上面SQL语句创建

:::

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的“规则”选项卡。

然后填写规则 SQL:

FROM说明

​	**t/#**: 发布者发布消息触发保存离线消息到PostgreSQL

​	**$events/session_subscribed**: 订阅者订阅主题触发获取离线消息

​	**$events/message_acked**: 订阅者回复消息ACK后触发删除已经被接收的离线消息

```bash
SELECT * FROM "t/#", "$events/session_subscribed", "$events/message_acked" WHERE topic =~ 't/#'
```

![](./assets/rule-engine/pg_offline_msg_01.png)

关联动作:

在“响应动作”界面选择“添加动作”，然后在“动作”下拉框里选择“离线消息保存到 PostgreSQL”。

![](./assets/rule-engine/pg_offline_msg_02.png)


现在资源下拉框为空，可以点击右上角的 “新建” 来创建一个 PostgreSQL 资源:

![](./assets/rule-engine/pg_offline_msg_03.png)

弹出一个“创建资源”对话框

![](./assets/rule-engine/pg_offline_msg_04.png)

填写资源配置:

填写真实的 PostgreSQL 服务器地址，其他配置填写相应的值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “确定” 按钮。

![](./assets/rule-engine/pg_offline_msg_05.png)

返回响应动作界面，点击 “确认”。

![](./assets/rule-engine/pg_offline_msg_06.png)

返回规则创建界面，点击 “创建”。

![](./assets/rule-engine/pg_offline_msg_07.png)

规则已经创建完成，通过 Dashboard 的 WebSocket 客户端发一条数据**(发布消息的QoS必须大于0)**:

![](./assets/rule-engine/pg_offline_msg_08.png)

消息发送后，通过 psql 查看到消息被保存到 PostgreSQL 里面:

![](./assets/rule-engine/pg_offline_msg_09.png)

使用另外一个客户端，订阅主题 "t/1" (订阅主题的QoS必须大于0，否则消息会被重复接收):

![](./assets/rule-engine/pg_offline_msg_10.png)

订阅后马上接收到了保存到 PostgreSQL 里面的离线消息:

![](./assets/rule-engine/pg_offline_msg_11.png)

离线消息被接收后会在 PostgreSQL 中删除:

![](./assets/rule-engine/pg_offline_msg_12.png)


## 离线消息保存到 Cassandra

搭建 Cassandra 数据库，并设置用户名密码为 root/public，以 MacOS X 为例:
```bash
$ brew install cassandra
## 修改配置，关闭匿名认证
$  vim /usr/local/etc/cassandra/cassandra.yaml

    authenticator: PasswordAuthenticator
    authorizer: CassandraAuthorizer

$ brew services start cassandra

## 创建 root 用户
$ cqlsh -ucassandra -pcassandra

create user root with password 'public' superuser;
```

初始化 Cassandra 表空间:
```bash
$ cqlsh -uroot -ppublic

CREATE KEYSPACE mqtt WITH replication = {'class': 'SimpleStrategy', 'replication_factor': '1'}  AND durable_writes = true;

```

创建 mqtt_msg 表:
```sql
CREATE TABLE mqtt.mqtt_msg (
    topic text,
    msgid text,
    arrived timestamp,
    payload text,
    qos int,
    retain int,
    sender text,
    PRIMARY KEY (topic, msgid)
) WITH CLUSTERING ORDER BY (msgid DESC)
    AND bloom_filter_fp_chance = 0.01
    AND caching = {'keys': 'ALL', 'rows_per_partition': 'NONE'}
    AND comment = ''
    AND compaction = {'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4'}
    AND compression = {'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor'}
    AND crc_check_chance = 1.0
    AND dclocal_read_repair_chance = 0.1
    AND default_time_to_live = 0
    AND gc_grace_seconds = 864000
    AND max_index_interval = 2048
    AND memtable_flush_period_in_ms = 0
    AND min_index_interval = 128
    AND read_repair_chance = 0.0
    AND speculative_retry = '99PERCENTILE';

```

::: danger

消息表结构不能修改，请使用上面SQL语句创建

:::

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的“规则”选项卡。

然后填写规则 SQL:

FROM说明

​	**t/#**: 发布者发布消息触发保存离线消息到Cassandra

​	**$events/session_subscribed**: 订阅者订阅主题触发获取离线消息

​	**$events/message_acked**: 订阅者回复消息ACK后触发删除已经被接收的离线消息

```bash
SELECT * FROM "t/#", "$events/session_subscribed", "$events/message_acked" WHERE topic =~ 't/#'
```

![](./assets/rule-engine/cass_offline_msg_01.png)

关联动作:

在“响应动作”界面选择“添加动作”，然后在“动作”下拉框里选择“离线消息保存到 Cassandra ”。

![](./assets/rule-engine/cass_offline_msg_02.png)


现在资源下拉框为空，可以点击右上角的 “新建” 来创建一个 Cassandra 资源:

![](./assets/rule-engine/cass_offline_msg_03.png)

弹出一个“创建资源”对话框

![](./assets/rule-engine/cass_offline_msg_04.png)

填写资源配置:

填写真实的 Cassandra 服务器地址，其他配置填写相应的值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “确定” 按钮。

![](./assets/rule-engine/cass_offline_msg_05.png)

返回响应动作界面，点击 “确认”。

![](./assets/rule-engine/cass_offline_msg_06.png)

返回规则创建界面，点击 “创建”。

![](./assets/rule-engine/cass_offline_msg_07.png)

规则已经创建完成，通过 Dashboard 的 WebSocket 客户端发一条数据**(发布消息的QoS必须大于0)**:

![](./assets/rule-engine/cass_offline_msg_08.png)

消息发送后，通过 cqlsh 查看到消息被保存到 Cassandra 里面:

![](./assets/rule-engine/cass_offline_msg_09.png)

使用另外一个客户端，订阅主题 "t/1" (订阅主题的QoS必须大于0，否则消息会被重复接收):

![](./assets/rule-engine/cass_offline_msg_10.png)

订阅后马上接收到了保存到 Cassandra 里面的离线消息:

![](./assets/rule-engine/cass_offline_msg_11.png)

离线消息被接收后会在 Cassandra 中删除:

![](./assets/rule-engine/cass_offline_msg_12.png)


## 离线消息保存到 MongoDB

搭建 MongoDB 数据库，并设置用户名密码为 root/public，以 MacOS X 为例:
```bash
$ brew install mongodb
$ brew services start mongodb

## 新增 root/public 用户
$ use mqtt;
$ db.createUser({user: "root", pwd: "public", roles: [{role: "readWrite", db: "mqtt"}]});

## 修改配置，关闭匿名认证
$ vi /usr/local/etc/mongod.conf

    security:
    authorization: enabled

$ brew services restart mongodb
```

创建 mqtt_msg 表:
```bash
$ mongo 127.0.0.1/mqtt -uroot -ppublic
db.createCollection("mqtt_msg");
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的“规则”选项卡。

然后填写规则 SQL:

FROM说明

​	**t/#**: 发布者发布消息触发保存离线消息到MongoDB

​	**$events/session_subscribed**: 订阅者订阅主题触发获取离线消息

​	**$events/message_acked**: 订阅者回复消息ACK后触发删除已经被接收的离线消息

```bash
SELECT * FROM "t/#", "$events/session_subscribed", "$events/message_acked" WHERE topic =~ 't/#'
```

![](./assets/rule-engine/mongo_offline_msg_01.png)

关联动作:

在“响应动作”界面选择“添加动作”，然后在“动作”下拉框里选择“离线消息保存到 MongoDB”。

![](./assets/rule-engine/mongo_offline_msg_02.png)


现在资源下拉框为空，可以点击右上角的 “新建” 来创建一个 MongoDB 资源:

![](./assets/rule-engine/mongo_offline_msg_03.png)

弹出一个“创建资源”对话框

![](./assets/rule-engine/mongo_offline_msg_04.png)

填写资源配置:

填写真实的 MongoDB 服务器地址，其他配置填写相应的值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “确定” 按钮。

![](./assets/rule-engine/mongo_offline_msg_05.png)

返回响应动作界面，点击 “确认”。

![](./assets/rule-engine/mongo_offline_msg_06.png)

返回规则创建界面，点击 “创建”。

![](./assets/rule-engine/mongo_offline_msg_07.png)

规则已经创建完成，通过 Dashboard 的 WebSocket 客户端发一条数据**(发布消息的QoS必须大于0)**:

![](./assets/rule-engine/mongo_offline_msg_08.png)

消息发送后，通过 mongo 查看到消息被保存到 MongoDB 里面:

```
db.mqtt_msg.find()
```

![](./assets/rule-engine/mongo_offline_msg_09.png)

使用另外一个客户端，订阅主题 "t/1" (订阅主题的QoS必须大于0，否则消息会被重复接收):

![](./assets/rule-engine/mongo_offline_msg_10.png)

订阅后马上接收到了保存到 MongoDB 里面的离线消息:

![](./assets/rule-engine/mongo_offline_msg_11.png)

离线消息被接收后会在 MongoDB 中删除:

![](./assets/rule-engine/mongo_offline_msg_12.png)


## 从 Redis 中获取订阅关系

搭建 Redis 环境，以 MaxOS X 为例:

```bash
 $ wget http://download.redis.io/releases/redis-4.0.14.tar.gz
$ tar xzf redis-4.0.14.tar.gz
$ cd redis-4.0.14
$ make && make install

# 启动 redis
$ redis-server
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

然后填写规则 SQL:

```bash
SELECT * FROM "$events/client_connected"
```

![](./assets/rule-engine/redis_sub_1.png)

关联动作:

在 “响应动作” 界面选择 “添加动作”，然后在 “动作” 下拉框里选择 “从 Redis 中获取订阅关系”。

![](./assets/rule-engine/redis_sub_2.png)

填写动作参数:

“离线消息保存到 Redis 动作需要一个参数：

1). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 Redis 资源:

![](./assets/rule-engine/redis_sub_3.png)

选择 Redis 单节点模式资源”。

![](./assets/rule-engine/offline_msg_4.png)

填写资源配置:

   填写真实的 Redis 服务器地址，其他配置保持默认值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “新建” 按钮。

![](./assets/rule-engine/redis_sub_5.png)

返回响应动作界面，点击 “确认”。

![](./assets/rule-engine/redis_sub_6.png)

返回规则创建界面，点击 “新建”。

![](./assets/rule-engine/redis_sub_7.png)

规则已经创建完成，通过 Redis CLI 往Redis插入一条订阅关系:

```bash
HSET mqtt:sub:test t1 1
```

![](./assets/rule-engine/redis_sub_8.png)

通过 Dashboard  登录 clientid 为 test 的设备:

![](./assets/rule-engine/redis_sub_9.png)

查看订阅列表，可以看到 **test** 设备已经订阅了 **t1** 主题:

![](./assets/rule-engine/redis_sub_10.png)


# 从 MySQL 中获取订阅关系

搭建 MySQL 数据库，并设置用户名密码为 root/public，以 MacOS X 为例:
```bash
$ brew install mysql

$ brew services start mysql

$ mysql -u root -h localhost -p

ALTER USER 'root'@'localhost' IDENTIFIED BY 'public';
```

初始化 MySQL 数据库:
```bash
$ mysql -u root -h localhost -ppublic

create database mqtt;
```

创建 mqtt_sub 表:
```sql
DROP TABLE IF EXISTS `mqtt_sub`;

CREATE TABLE `mqtt_sub` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `clientid` varchar(64) DEFAULT NULL,
  `topic` varchar(180) DEFAULT NULL,
  `qos` tinyint(1) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `mqtt_sub_idx` (`clientid`,`topic`,`qos`),
  UNIQUE KEY `mqtt_sub_key` (`clientid`,`topic`),
  INDEX topic_index(`id`, `topic`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8MB4;
```

::: danger

订阅关系表结构不能修改，请使用上面SQL语句创建

:::

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的“规则”选项卡。

然后填写规则 SQL:

```bash
SELECT * FROM "$events/client_connected"
```

![](./assets/rule-engine/mysql_sub_01.png)

关联动作:

在“响应动作”界面选择“添加动作”，然后在“新增动作”下拉框里选择“从MySQL中获取订阅列表”

![](./assets/rule-engine/mysql_sub_02.png)

填写动作参数:

“从 MySQL 中获取订阅列表”动作需要一个参数：

1). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建” 来创建一个 MySQL 资源：

![](./assets/rule-engine/mysql_sub_03.png)

弹出“创建资源”对话框

![](./assets/rule-engine/mysql_sub_04.png)

填写资源配置:

   填写真实的 MySQL 服务器地址，其他配置相应的值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “确定” 按钮。

![](./assets/rule-engine/mysql_sub_05.png)

返回响应动作界面，点击 “确认”。

![](./assets/rule-engine/mysql_sub_06.png)

返回规则创建界面，点击 “创建”。

![](./assets/rule-engine/mysql_sub_07.png)

规则已经创建完成，通过 “mysql” 往MySQL插入一条订阅关系:

```
insert into mqtt_sub(clientid, topic, qos) values("test", "t1", 1);
```

![](./assets/rule-engine/mysql_sub_08.png)

通过 Dashboard 登录 clientid 为 test 的设备:

![](./assets/rule-engine/mysql_sub_09.png)

查看“订阅”列表，可以看到 Broker 从 MySQL 里面获取到订阅关系，并代理设备订阅:

![](./assets/rule-engine/mysql_sub_10.png)


# 从 PostgreSQL 中获取订阅关系

搭建 PostgreSQL 数据库，以 MacOS X 为例:
```bash
$ brew install postgresql
$ brew services start postgresql
```

创建 mqtt 数据库:

```
# 使用用户名 postgres 创建名为 'mqtt' 的数据库
$ createdb -U postgres mqtt

$ psql -U postgres mqtt

mqtt=> \dn;
List of schemas
Name  | Owner
--------+-------
public | postgres
(1 row)
```

创建 mqtt_sub 表:

```sql
$ psql -U postgres mqtt
CREATE TABLE mqtt_sub(
  id SERIAL8 primary key,
  clientid character varying(64),
  topic character varying(255),
  qos integer,
  UNIQUE (clientid, topic)
);
```

::: danger

订阅关系表结构不能修改，请使用上面SQL语句创建

:::

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的“规则”选项卡。

然后填写规则 SQL:

```bash
SELECT * FROM "$events/client_connected"
```

![](./assets/rule-engine/pg_sub_01.png)

关联动作:

在“响应动作”界面选择“添加动作”，然后在“新增动作”下拉框里选择“从PostgreSQL中获取订阅列表”

![](./assets/rule-engine/pg_sub_02.png)

填写动作参数:

“从PostgreSQL中获取订阅列表“ 动作需要一个参数：

1). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建” 来创建一个 PostgreSQL 资源：

![](./assets/rule-engine/pg_sub_03.png)

弹出“创建资源”对话框

![](./assets/rule-engine/pg_sub_04.png)

填写资源配置:

   填写真实的 PostgreSQL 服务器地址，其他配置相应的值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “确定” 按钮。

![](./assets/rule-engine/pg_sub_05.png)

返回响应动作界面，点击 “确认”。

![](./assets/rule-engine/pg_sub_06.png)

返回规则创建界面，点击 “创建”。

![](./assets/rule-engine/pg_sub_07.png)

规则已经创建完成，通过 “psql” 往PostgreSQL插入一条订阅关系

```
insert into mqtt_sub(clientid, topic, qos) values('test', 't1', 1)'
```

![](./assets/rule-engine/pg_sub_08.png)

通过 Dashboard 登录 clientid 为 test 的设备:

![](./assets/rule-engine/pg_sub_09.png)

查看“订阅”列表，可以看到 Broker 从 PostgreSQL 里面获取到订阅关系，并代理设备订阅:

![](./assets/rule-engine/pg_sub_10.png)

# 从 Cassandra 中获取订阅关系

搭建 Cassandra 数据库，并设置用户名密码为 root/public，以 MacOS X 为例:
```bash
$ brew install cassandra
## 修改配置，关闭匿名认证
$  vim /usr/local/etc/cassandra/cassandra.yaml

    authenticator: PasswordAuthenticator
    authorizer: CassandraAuthorizer

$ brew services start cassandra

## 创建 root 用户
$ cqlsh -ucassandra -pcassandra

create user root with password 'public' superuser;
```

创建 "mqtt" 表空间:
```bash
$ cqlsh -uroot -ppublic

CREATE KEYSPACE mqtt WITH replication = {'class': 'SimpleStrategy', 'replication_factor': '1'}  AND durable_writes = true;
```

创建 mqtt_sub 表:

```sql

CREATE TABLE mqtt_sub (
    clientid text,
    topic text,
    qos int,
    PRIMARY KEY (clientid, topic)
) WITH CLUSTERING ORDER BY (topic ASC)
    AND bloom_filter_fp_chance = 0.01
    AND caching = {'keys': 'ALL', 'rows_per_partition': 'NONE'}
    AND comment = ''
    AND compaction = {'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4'}
    AND compression = {'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor'}
    AND crc_check_chance = 1.0
    AND dclocal_read_repair_chance = 0.1
    AND default_time_to_live = 0
    AND gc_grace_seconds = 864000
    AND max_index_interval = 2048
    AND memtable_flush_period_in_ms = 0
    AND min_index_interval = 128
    AND read_repair_chance = 0.0
    AND speculative_retry = '99PERCENTILE';

```

::: danger

订阅关系表结构不能修改，请使用上面SQL语句创建

:::

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的“规则”选项卡。

然后填写规则 SQL:

```bash
SELECT * FROM "$events/client_connected"
```

![](./assets/rule-engine/cass_sub_01.png)

关联动作:

在“响应动作”界面选择“添加动作”，然后在“新增动作”下拉框里选择“从 Cassandra 中获取订阅列表”

![](./assets/rule-engine/cass_sub_02.png)

填写动作参数:

“从 Cassandra 中获取订阅列表”动作需要一个参数：

1). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建” 来创建一个 Cassandra 资源：

![](./assets/rule-engine/cass_sub_03.png)

弹出“创建资源”对话框

![](./assets/rule-engine/cass_sub_04.png)

填写资源配置:

   填写真实的 Cassandra 服务器地址，其他配置相应的值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “确定” 按钮。

![](./assets/rule-engine/cass_sub_05.png)

返回响应动作界面，点击 “确认”。

![](./assets/rule-engine/cass_sub_06.png)

返回规则创建界面，点击 “创建”。

![](./assets/rule-engine/cass_sub_07.png)

规则已经创建完成，通过 “cqlsh” 往 Cassandra 插入一条订阅关系:

```
insert into mqtt_sub(clientid, topic, qos) values('test', 't1', 1);
```

![](./assets/rule-engine/cass_sub_08.png)

通过 Dashboard 登录 clientid 为 test 的设备:

![](./assets/rule-engine/cass_sub_09.png)

查看“订阅”列表，可以看到 Broker 从 Cassandra 里面获取到订阅关系，并代理设备订阅:

![](./assets/rule-engine/cass_sub_10.png)

# 从 MongoDB 中获取订阅关系

搭建 MongoDB 数据库，并设置用户名密码为 root/public，以 MacOS X 为例:
```bash
$ brew install mongodb
$ brew services start mongodb

## 新增 root/public 用户
$ use mqtt;
$ db.createUser({user: "root", pwd: "public", roles: [{role: "readWrite", db: "mqtt"}]});

## 修改配置，关闭匿名认证
$ vi /usr/local/etc/mongod.conf

    security:
    authorization: enabled

$ brew services restart mongodb
```

创建 mqtt_sub 表:
```sql
$ mongo 127.0.0.1/mqtt -uroot -ppublic
db.createCollection("mqtt_sub");
```

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的“规则”选项卡。

然后填写规则 SQL:

```bash
SELECT * FROM "$events/client_connected"
```

![](./assets/rule-engine/mongo_sub_01.png)

关联动作:

在“响应动作”界面选择“添加动作”，然后在“新增动作”下拉框里选择“从MongoDB中获取订阅列表”

![](./assets/rule-engine/mongo_sub_02.png)

填写动作参数:

“从 MongoDB 中获取订阅列表”动作需要一个参数：

1). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建” 来创建一个 MongoDB 资源：

![](./assets/rule-engine/mongo_sub_03.png)

弹出“创建资源”对话框

![](./assets/rule-engine/mongo_sub_04.png)

填写资源配置:

   填写真实的 MongoDB 服务器地址，其他配置相应的值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “确定” 按钮。

![](./assets/rule-engine/mongo_sub_05.png)

返回响应动作界面，点击 “确认”。

![](./assets/rule-engine/mongo_sub_06.png)

返回规则创建界面，点击 “创建”。

![](./assets/rule-engine/mongo_sub_07.png)

规则已经创建完成，通过 “mongo” 往MongoDB插入一条订阅关系

```
db.mqtt_sub.insert({clientid: "test", topic: "t1", qos: 1})
```

![](./assets/rule-engine/mongo_sub_08.png)

通过 Dashboard 登录 clientid 为 test 的设备:

![](./assets/rule-engine/mongo_sub_09.png)

查看“订阅”列表，可以看到 Broker 从 MongoDB 里面获取到订阅关系，并代理设备订阅:

![](./assets/rule-engine/mongo_sub_10.png)


## 从 Kafka 消费消息到 EMQ X

搭建 Kafka 环境，以 MaxOS X 为例:

```bash
$ wget http://apache.claz.org/kafka/2.3.1/kafka_2.12-2.3.1.tgz

$ tar -xzf  kafka_2.12-2.3.1.tgz

$ cd kafka_2.12-2.3.1

# 启动 Zookeeper
$ ./bin/zookeeper-server-start.sh config/zookeeper.properties
# 启动 Kafka
$ ./bin/kafka-server-start.sh config/server.properties
```

::: danger

Kafka消费组不支持Kafka0.9以下版本

创建资源之前，需要提前创建Kafka主题，不然会提示错误

:::

创建 Kafka 的主题:

```bash
$ ./bin/kafka-topics.sh --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic testTopic --create
```
创建资源:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “资源” 选项卡。

点击 “新建” 按钮:

![](./assets/rule-engine/kafka_consumer1.png)

选择资源类型 “Kafka 消费组”:

![](./assets/rule-engine/kafka_consumer2.png)

填写资源参数:

![](./assets/rule-engine/kafka_consumer3.png)

1). Kafka 服务器地址

2). Kafka consumer 连接池大小

3). Kafka 的订阅主题

4). Kafka 的消息主题

5). Kafka Max Bytes (每次从 Kafka 里消费消息的最大字节数)

6). Kafka Offset Reset Policy (重置Offset策略,reset_to_latest | reset_by_subdcriber)

7). Kafka consumer 是否重连



最后点击 “确认”，资源创建完成:

![](./assets/rule-engine/kafka_consumer4.png)

资源已经创建完成，现在用Dashboard的websocket工具订阅MQTT的主题 "TestTopic":

![](./assets/rule-engine/kafka_consumer5.png)

使用kafka 命令行 生产一条消息:

```bash
./bin/kafka-console-producer.sh --broker-list localhost:9092 --topic TestTopic
```

![](./assets/rule-engine/kafka_consumer6.png)

Dashboard的websocket工具接收到了Kafka 生产的消息"hello-kafka":

![](./assets/rule-engine/kafka_consumer7.png)

## 从 Pulsar 消费消息到 EMQ X

搭建 Pulsar 环境，以 MaxOS X 为例:

```bash
$ wget http://apache.mirrors.hoobly.com/pulsar/pulsar-2.3.2/apache-pulsar-2.3.2-bin.tar.gz

$ tar xvfz apache-pulsar-2.3.2-bin.tar.gz

$ cd apache-pulsar-2.3.2

# 启动 Pulsar
$ ./bin/pulsar standalone
```

创建 Pulsar 的主题:
```bash
$ ./bin/pulsar-admin topics create-partitioned-topic -p 5 testTopic
```
创建资源:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “资源” 选项卡。

点击 “新建” 按钮:

![](./assets/rule-engine/pulsar_consumer1.png)

选择资源类型 “pulsar 消费组”:

![](./assets/rule-engine/pulsar_consumer2.png)

填写资源参数:

![](./assets/rule-engine/pulsar_consumer3.png)

1). Pulsar 服务器地址

2). Pulsar consumer 进程数量

3). Pulsar 的订阅主题

4). EMQ X 的消息主题

5). Pulsar 流控阈值 (Pulsar 流控阈值，配置 Pulsar 向消费者发送多少条消息后阻塞 Pulsar Consumer)

6). EMQ X 重置流控阈值百分比 (Pulsar 流控阈值重置百分比。此配置让消费者处理完成一定数量的消息之后，提前重置 `Pulsar 流控阈值`。 比如，`Pulsar 流控阈值` 为 1000，`阈值重置百分比` 为 80%，则重置)



最后点击 “确认”，资源创建完成:

![](./assets/rule-engine/pulsar_consumer4.png)

资源已经创建完成，现在用Dashboard的websocket工具订阅MQTT的主题 "TestTopic":

![](./assets/rule-engine/pulsar_consumer5.png)

使用pulsar-cli 生产一条消息:

```bash
./bin/pulsar-client produce TestTopic --messages "hello-pulsar"
```

![](./assets/rule-engine/pulsar_consumer6.png)

Dashboard的websocket工具接收到了pulsar 生产的消息"hello-pulsar":

![](./assets/rule-engine/pulsar_consumer7.png)
