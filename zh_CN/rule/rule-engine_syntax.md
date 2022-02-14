---
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
ref:
---

# 规则引擎

使用 EMQ X 的规则引擎可以灵活地处理消息和事件。使用规则引擎可以方便地实现诸如将消息转换成指定格式，然后存入数据库表，或者发送到消息队列等。

与 EMQ X 规则引擎相关的概念包括: 规则(rule)、动作(action)、资源(resource) 和 资源类型(resource-type)。

规则、动作、资源的关系:
```
规则: {
    SQL 语句,
    动作列表: [
        {
            动作1,
            动作参数,
            绑定资源: {
                资源配置
            }
        },
        {
            动作2,
            动作参数,
            绑定资源: {
                资源配置
            }
         }
    ]
}
```
- 规则(Rule): 规则由 SQL 语句和动作列表组成。动作列表包含一个或多个动作及其参数。
- SQL 语句用于筛选或转换消息中的数据。
- 动作(Action) 是 SQL 语句匹配通过之后，所执行的任务。动作定义了一个针对数据的操作。
  动作可以绑定资源，也可以不绑定。例如，“inspect” 动作不需要绑定资源，它只是简单打印数据内容和动作参数。而 “data_to_webserver” 动作需要绑定一个 web_hook 类型的资源，此资源中配置了 URL。
- 资源(Resource): 资源是通过资源类型为模板实例化出来的对象，保存了与资源相关的配置(比如数据库连接地址和端口、用户名和密码等) 和系统资源(如文件句柄，连接套接字等)。
- 资源类型 (Resource Type): 资源类型是资源的静态定义，描述了此类型资源需要的配置项。

::: tip
动作和资源类型是由 emqx 或插件的代码提供的，不能通过 API 和 CLI 动态创建。
:::

## SQL 语句
### SQL 语法
**FROM、SELECT 和 WHERE 子句:**

规则引擎的 SQL 语句基本格式为:

```sql
SELECT <字段名> FROM <主题> [WHERE <条件>]
```

- `FROM` 子句将规则挂载到某个主题上
- `SELECT` 子句用于对数据进行变换，并选择出感兴趣的字段
- `WHERE` 子句用于对 SELECT 选择出来的某个字段施加条件过滤

**FOREACH、DO 和 INCASE 子句:**

如果对于一个数组数据，想针对数组中的每个元素分别执行一些操作并执行 Actions，需要使用 `FOREACH-DO-INCASE` 语法。其基本格式为:

```sql
FOREACH <字段名> [DO <条件>] [INCASE <条件>] FROM <主题> [WHERE <条件>]
```

- `FOREACH` 子句用于选择需要做 foreach 操作的字段，注意选择出的字段必须为数组类型
- `DO` 子句用于对 FOREACH 选择出来的数组中的每个元素进行变换，并选择出感兴趣的字段
- `INCASE` 子句用于对 DO 选择出来的某个字段施加条件过滤

其中 DO 和 INCASE 子句都是可选的。DO 相当于针对当前循环中对象的 SELECT 子句，而 INCASE 相当于针对当前循环中对象的 WHERE 语句。

### SQL 关键字和符号
#### SELECT - FROM - WHERE 语句
SELECT 语句用于决定最终的输出结果里的字段。比如:

下面 SQL 的输出结果中将只有两个字段 "a" 和 "b":

```
SELECT a, b FROM "t/#"
```

WHERE 语句用于对本事件中可用字段，或 SELECT 语句中定义的字段进行条件过滤。比如:

```
# 选取 username 为 'abc' 的终端发来的消息，输出结果为所有可用字段:

SELECT * FROM "#" WHERE username = 'abc'

## 选取 clientid 为 'abc' 的终端发来的消息，输出结果将只有 cid 一个字段。
## 注意 cid 变量是在 SELECT 语句中定义的，故可在 WHERE 语句中使用:

SELECT clientid as cid FROM "#" WHERE cid = 'abc'

## 选取 username 为 'abc' 的终端发来的消息，输出结果将只有 cid 一个字段。
## 注意虽然 SELECT 语句中只选取了 cid 一个字段，所有消息发布事件中的可用字段 (比如 clientid, username 等) 仍然可以在 WHERE 语句中使用:

SELECT clientid as cid FROM "#" WHERE username = 'abc'

## 但下面这个 SQL 语句就不能工作了，因为变量 xyz 既不是消息发布事件中的可用字段，又没有在 SELECT 语句中定义:

SELECT clientid as cid FROM "#" WHERE xyz = 'abc'
```

FROM 语句用于选择事件来源。如果是消息发布则填写消息的主题，如果是事件则填写对应的事件主题。


#### FOREACH - DO - INCASE 语句
.......

#### 运算符号
| 函数名 |   函数作用            |   返回值   |     |
| ------ | ------------------- | ---------- | --- |
| `+`    | 加法，或字符串拼接     | 加和，或拼接之后的字符串 |     |
| `-`    | 减法                | 差值       |     |
| `*`    | 乘法                | 乘积       |     |
| `/`    | 除法                | 商值       |     |
| `div`  | 整数除法             | 整数商值   |     |
| `mod`  | 取模                 | 模         |     |
| `=`    | 比较两者是否完全相等。可用于比较变量和主题 | true/false |     |
| `=~`   | 比较主题(topic)是否能够匹配到主题过滤器(topic filter)。只能用于主题匹配 | true/false |     |


## 事件和事件主题
规则引擎的 SQL 语句既可以处理消息(消息发布)，也可以处理事件(客户端上下线、客户端订阅等)。对于消息，FROM 子句后面直接跟主题名；对于事件，FROM 子句后面跟事件主题。

事件消息的主题以 `"$events/"` 开头，比如 `"$events/client_connected",` `"$events/session_subscribed"。`
如果想让 emqx 将事件消息发布出来，可以在 `emqx_rule_engine.conf` 文件中配置。

所有支持的事件及其可用字段详见: [规则事件](#rule-sql-events)。


#### FROM 子句可用的事件主题
| 事件主题名                    | 释义     |
| ----------------------------- | :------- |
| $events/message_delivered    | 消息投递 |
| $events/message_acked        | 消息确认 |
| $events/message_dropped      | 消息丢弃 |
| $events/client_connected     | 连接完成 |
| $events/client_disconnected  | 连接断开 |
| $events/session_subscribed   | 订阅     |
| $events/session_unsubscribed | 取消订阅 |

## SQL 语句示例:
### 基本语法举例

- 从 topic 为 "t/a" 的消息中提取所有字段:

```sql
SELECT * FROM "t/a"
```

- 从 topic 为 "t/a" 或 "t/b" 的消息中提取所有字段:

```sql
SELECT * FROM "t/a","t/b"
```

- 从 topic 能够匹配到 't/#' 的消息中提取所有字段。

```sql
SELECT * FROM "t/#"
```

- 从 topic 能够匹配到 't/#' 的消息中提取 qos, username 和 clientid 字段:

```sql
SELECT qos, username, clientid FROM "t/#"
```

- 从任意 topic 的消息中提取 username 字段，并且筛选条件为 username = 'Steven':

```sql
SELECT username FROM "#" WHERE username='Steven'
```

- 从任意 topic 的 JSON 消息体(payload) 中提取 x 字段，并创建别名 x 以便在 WHERE 子句中使用。WHERE 子句限定条件为 x = 1。下面这个 SQL 语句可以匹配到消息体 {"x": 1}, 但不能匹配到消息体 {"x": 2}:

```sql
SELECT payload FROM "#" WHERE payload.x = 1
```

- 类似于上面的 SQL 语句，但嵌套地提取消息体中的数据，下面的 SQL 语句可以匹配到 JSON 消息体 {"x": {"y": 1}}:

```sql
SELECT payload FROM "#" WHERE payload.x.y = 1
```

- 在 clientid = 'c1' 尝试连接时，提取其来源 IP 地址和端口号:

```sql
SELECT peername as ip_port FROM "$events/client_connected" WHERE clientid = 'c1'
```

- 筛选所有订阅 't/#' 主题且订阅级别为 QoS1 的 clientid:

```sql
SELECT clientid FROM "$events/session_subscribed" WHERE topic = 't/#' and qos = 1
```

- 筛选所有订阅主题能匹配到 't/#' 且订阅级别为 QoS1 的 clientid。注意与上例不同的是，这里用的是主题匹配操作符 **'=~'**，所以会匹配订阅 't' 或 't/+/a' 的订阅事件:

```sql
SELECT clientid FROM "$events/session_subscribed" WHERE topic =~ 't/#' and qos = 1
```

- 对于一个 MQTT 5.0 PUBLISH 消息，筛选出 Key 为 "foo" 的 User Property:

```sql
SELECT pub_props.'User-Property'.foo as foo FROM "t/#"
```

::: tip

- FROM 子句后面的主题需要用双引号 `""`，或者单引号 `''` 引起来。
- WHERE 子句后面接筛选条件，如果使用到字符串需要用单引号 `''` 引起来。
- FROM 子句里如有多个主题，需要用逗号 `","` 分隔。例如 SELECT * FROM "t/1", "t/2" 。
- 可以使用使用 `"."` 符号对 payload 进行嵌套选择。
- 尽量不要给 payload 创建别名，否则会影响运行性能。即尽量不要这么写：`SELECT payload as p`
:::

#### 遍历语法(FOREACH-DO-INCASE) 举例

假设有 ClientID 为 `c_steve`、主题为 `t/1` 的消息，消息体为 JSON 格式，其中 sensors 字段为包含多个 Object 的数组:

```json
{
    "date": "2020-04-24",
    "sensors": [
        {"name": "a", "idx":0},
        {"name": "b", "idx":1},
        {"name": "c", "idx":2}
    ]
}
```

**示例1: 要求将 sensors 里的各个对象，分别作为数据输入重新发布消息到 `sensors/${idx}` 主题，内容为 `${name}`。即最终规则引擎将会发出 3 条消息:**

1) 主题：sensors/0
   内容：a
2) 主题：sensors/1
   内容：b
3) 主题：sensors/2
   内容：c

要完成这个规则，我们需要配置如下动作：

- 动作类型：消息重新发布 (republish)
- 目的主题：sensors/${idx}
- 目的 QoS：0
- 消息内容模板：${name}

以及如下 SQL 语句：

```sql
FOREACH
    payload.sensors
FROM "t/#"
```

**示例解析:**

这个 SQL 中，FOREACH 子句指定需要进行遍历的数组 sensors，则选取结果为:

```json
[
  {
    "name": "a",
    "idx": 0
  },
  {
    "name": "b",
    "idx": 1
  },
  {
    "name": "c",
    "idx": 2
  }
]
```

FOREACH 语句将会对于结果数组里的每个对象分别执行 "消息重新发布" 动作，所以将会执行重新发布动作 3 次。

**示例2: 要求将 sensors 里的 `idx` 值大于或等于 1 的对象，分别作为数据输入重新发布消息到 `sensors/${idx}` 主题，内容为 `clientid=${clientid},name=${name},date=${date}`。即最终规则引擎将会发出 2 条消息:**

1) 主题：sensors/1
   内容：clientid=c_steve,name=b,date=2020-04-24
2) 主题：sensors/2
   内容：clientid=c_steve,name=c,date=2020-04-24

要完成这个规则，我们需要配置如下动作：

- 动作类型：消息重新发布 (republish)
- 目的主题：sensors/${idx}
- 目的 QoS：0
- 消息内容模板：clientid=${clientid},name=${name},date=${date}

以及如下 SQL 语句：

```sql
FOREACH
    payload.sensors
DO
    clientid,
    item.name as name,
    item.idx as idx
INCASE
    item.idx >= 1
FROM "t/#"
```

**示例解析:**

这个 SQL 中，FOREACH 子句指定需要进行遍历的数组 `sensors`; DO 子句选取每次操作需要的字段，这里我们选了外层的 `clientid` 字段，以及当前 sensor 对象的 `name` 和 `idx` 两个字段，注意 `item` 代表 sensors 数组中本次循环的对象。INCASE 子句是针对 DO 语句中字段的筛选条件，仅仅当 idx >= 1 满足条件。所以 SQL 的选取结果为:

```json
[
  {
    "name": "b",
    "idx": 1,
    "clientid": "c_emqx"
  },
  {
    "name": "c",
    "idx": 2,
    "clientid": "c_emqx"
  }
]
```

FOREACH 语句将会对于结果数组里的每个对象分别执行 "消息重新发布" 动作，所以将会执行重新发布动作 2 次。

在 DO 和 INCASE 语句里，可以使用 `item` 访问当前循环的对象，也可以通过在 FOREACH 使用 `as` 语法自定义一个变量名。所以本例中的 SQL 语句又可以写为：

```sql
FOREACH
    payload.sensors as s
DO
    clientid,
    s.name as name,
    s.idx as idx
INCASE
    s.idx >= 1
FROM "t/#"
```

**示例3: 在示例2 的基础上，去掉 clientid 字段 `c_steve` 中的 `c_` 前缀**

在 FOREACH 和 DO 语句中可以调用各类 SQL 函数，若要将 `c_steve` 变为 `steve`，则可以把例2 中的 SQL 改为：

```sql
FOREACH
    payload.sensors as s
DO
    nth(2, tokens(clientid,'_')) as clientid,
    s.name as name,
    s.idx as idx
INCASE
    s.idx >= 1
FROM "t/#"
```

另外，FOREACH 子句中也可以放多个表达式，只要最后一个表达式是指定要遍历的数组即可。比如我们将消息体改一下，sensors 外面多套一层 Object：

```json
{
    "date": "2020-04-24",
    "data": {
        "sensors": [
            {"name": "a", "idx":0},
            {"name": "b", "idx":1},
            {"name": "c", "idx":2}
        ]
    }
}
```

则 FOREACH 中可以在决定要遍历的数组之前把 data 选取出来：

```sql
FOREACH
    payload.data as data
    data.sensors as s
...
```

#### CASE-WHEN 语法示例

**示例1: 将消息中 x 字段的值范围限定在 0~7 之间。**

```sql
SELECT
  CASE WHEN payload.x < 0 THEN 0
       WHEN payload.x > 7 THEN 7
       ELSE payload.x
  END as x
FROM "t/#"
```

假设消息为:

```json
{"x": 8}
```

则上面的 SQL 输出为:

```json
{"x": 7}
```

#### 数组操作语法举例

**示例1: 创建一个数组，赋值给变量 a:**

```sql
SELECT
  [1,2,3] as a
FROM
  "t/#"
```

下标从 1 开始，上面的 SQL 输出为:

```json
{
  "a": [1, 2, 3]
}
```

**示例2: 从数组中取出第 N 个元素。下标为负数时，表示从数组的右边取:**

```sql
SELECT
  [1,2,3] as a,
  a[2] as b,
  a[-2] as c
FROM
  "t/#"
```

上面的 SQL 输出为:

```json
{
  "b": 2,
  "c": 2,
  "a": [1, 2, 3]
}
```

**示例3: 从 JSON 格式的 payload 中嵌套的获取值:**

```sql
SELECT
  payload.data[1].id as id
FROM
  "t/#"
```

假设消息为:

```json
{"data": [
    {"id": 1, "name": "steve"},
    {"id": 2, "name": "bill"}
]}
```

则上面的 SQL 输出为:

```json
{"id": 1}
```

**示例4: 数组范围(range)操作:**

```sql
SELECT
  [1..5] as a,
  a[2..4] as b
FROM
  "t/#"
```

上面的 SQL 输出为:

```json
{
  "b": [2, 3, 4],
  "a": [1, 2, 3, 4, 5]
}
```

**示例5: 使用下标语法修改数组中的某个元素:**

```sql
SELECT
  payload,
  'STEVE' as payload.data[1].name
FROM
  "t/#"
```

假设消息为:

```json
{"data": [
    {"id": 1, "name": "steve"},
    {"id": 2, "name": "bill"}
]}
```

则上面的 SQL 输出为:

```json
{
  "payload": {
    "data": [
      {"name": "STEVE", "id": 1},
      {"name": "bill", "id": 2}
    ]
  }
}
```

### SELECT 和 WHERE 子句可用的字段
SELECT 和 WHERE 子句可用的字段与事件的类型相关。其中 `clientid`, `username` 和 `event` 是通用字段，每种事件类型都有。

#### 普通主题 (消息发布)

|        event        |  事件类型，固定为 "message.publish"   |
| :------------------ | :------------------------------------ |
| id                  | MQTT 消息 ID                          |
| clientid            | Client ID                             |
| username            | 用户名                                |
| payload             | MQTT 消息体                           |
| peerhost            | 客户端的 IPAddress                    |
| topic               | MQTT 主题                             |
| qos                 | MQTT 消息的 QoS                       |
| flags               | MQTT 消息的 Flags                     |
| headers             | MQTT 消息内部与流程处理相关的额外数据     |
| pub_props           | PUBLISH Properties (仅适用于 MQTT 5.0) |
| timestamp           | 事件触发时间 (ms)                     |
| publish_received_at | PUBLISH 消息到达 Broker 的时间 (ms)   |
| node                | 事件触发所在节点                      |

#### $events/message_delivered (消息投递)

|        event        | 事件类型，固定为 "message.delivered" |
| ------------------- | ------------------------------------ |
| id                  | MQTT 消息 ID                         |
| from_clientid       | 消息来源 Client ID                   |
| from_username       | 消息来源用户名                       |
| clientid            | 消息目的 Client ID                   |
| username            | 消息目的用户名                       |
| payload             | MQTT 消息体                          |
| peerhost            | 客户端的 IPAddress                   |
| topic               | MQTT 主题                            |
| qos                 | MQTT 消息的 QoS                      |
| flags               | MQTT 消息的 Flags                    |
| pub_props           | PUBLISH Properties (仅适用于 MQTT 5.0) |
| timestamp           | 事件触发时间 (ms)                    |
| publish_received_at | PUBLISH 消息到达 Broker 的时间 (ms)  |
| node                | 事件触发所在节点                     |

#### $events/message_acked (消息确认)

|        event        |  事件类型，固定为 "message.acked"   |
| :------------------ | :---------------------------------- |
| id                  | MQTT 消息 ID                        |
| from_clientid       | 消息来源 Client ID                  |
| from_username       | 消息来源用户名                      |
| clientid            | 消息目的 Client ID                  |
| username            | 消息目的用户名                      |
| payload             | MQTT 消息体                         |
| peerhost            | 客户端的 IPAddress                  |
| topic               | MQTT 主题                           |
| qos                 | MQTT 消息的 QoS                     |
| flags               | MQTT 消息的 Flags                   |
| pub_props           | PUBLISH Properties (仅适用于 MQTT 5.0) |
| puback_props        | PUBACK Properties (仅适用于 MQTT 5.0) |
| timestamp           | 事件触发时间 (ms)                   |
| publish_received_at | PUBLISH 消息到达 Broker 的时间 (ms) |
| node                | 事件触发所在节点                    |

#### $events/message_dropped (消息丢弃)

|        event        | 事件类型，固定为 "message.dropped"  |
| :------------------ | :---------------------------------- |
| id                  | MQTT 消息 ID                        |
| reason              | 消息丢弃原因                        |
| clientid            | 消息目的 Client ID                  |
| username            | 消息目的用户名                      |
| payload             | MQTT 消息体                         |
| peerhost            | 客户端的 IPAddress                  |
| topic               | MQTT 主题                           |
| qos                 | MQTT 消息的 QoS                     |
| flags               | MQTT 消息的 Flags                   |
| pub_props           | PUBLISH Properties (仅适用于 MQTT 5.0) |
| timestamp           | 事件触发时间 (ms)                   |
| publish_received_at | PUBLISH 消息到达 Broker 的时间 (ms) |
| node                | 事件触发所在节点                    |

#### $events/client_connected (终端连接成功)

|      event      | 事件类型，固定为 "client.connected" |
| --------------- | :---------------------------------- |
| clientid        | 消息目的 Client ID                  |
| username        | 消息目的用户名                      |
| mountpoint      | 主题挂载点(主题前缀)                |
| peername        | 终端的 IPAddress 和 Port            |
| sockname        | emqx 监听的 IPAddress 和 Port       |
| proto_name      | 协议名字                            |
| proto_ver       | 协议版本                            |
| keepalive       | MQTT 保活间隔                       |
| clean_start     | MQTT clean_start                    |
| expiry_interval | MQTT Session 过期时间               |
| is_bridge       | 是否为 MQTT bridge 连接             |
| connected_at    | 终端连接完成时间 (s)                |
| conn_props      | CONNECT Properties (仅适用于 MQTT 5.0) |
| timestamp       | 事件触发时间 (ms)                   |
| node            | 事件触发所在节点                    |

#### $events/client_disconnected (终端连接断开)

| event           | 事件类型，固定为 "client.disconnected"                       |
| --------------- | :----------------------------------------------------------- |
| reason          | 终端连接断开原因：<br/>normal：客户端主动断开<br/>kicked：服务端踢出，通过 REST API<br/>keepalive_timeout: keepalive 超时<br/>not_authorized:  认证失败，或者 acl_nomatch = disconnect 时没有权限的 Pub/Sub 会主动断开客户端<br/>tcp_closed: 对端关闭了网络连接<br/>internal_error: 畸形报文或其他未知错误<br/> |
| clientid        | 消息目的 Client ID                                           |
| username        | 消息目的用户名                                               |
| peername        | 终端的 IPAddress 和 Port                                     |
| sockname        | emqx 监听的 IPAddress 和 Port                                |
| disconnected_at | 终端连接断开时间 (s)                                         |
| disconn_props   | DISCONNECT Properties (仅适用于 MQTT 5.0)                    |
| timestamp       | 事件触发时间 (ms)                                            |
| node            | 事件触发所在节点                                             |

#### $events/session_subscribed (终端订阅成功)

|   event   | 事件类型，固定为 "session.subscribed" |
| --------- | ------------------------------------- |
| clientid  | 消息目的 Client ID                    |
| username  | 消息目的用户名                        |
| peerhost  | 客户端的 IPAddress                    |
| topic     | MQTT 主题                             |
| qos       | MQTT 消息的 QoS                       |
| sub_props | SUBSCRIBE Properties (仅适用于 5.0)  |
| timestamp | 事件触发时间 (ms)                     |
| node      | 事件触发所在节点                      |

#### $events/session_unsubscribed (取消终端订阅成功)

|   event   | 事件类型，固定为 "session.unsubscribed" |
| :-------- | :-------------------------------------- |
| clientid  | 消息目的 Client ID                      |
| username  | 消息目的用户名                          |
| peerhost  | 客户端的 IPAddress                      |
| topic     | MQTT 主题                               |
| qos       | MQTT 消息的 QoS                         |
| unsub_props | UNSUBSCRIBE Properties (仅适用于 5.0)  |
| timestamp | 事件触发时间 (ms)                       |
| node      | 事件触发所在节点                        |

[下一部分，规则引擎内置函数](rule-engine_function.md)