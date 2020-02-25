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

# 规则引擎


> 适用版本: **EMQ X Broker v3.1.0+**

> 兼容提示: EMQ X Broker v4.0 对规则引擎 SQL 语法做出较大调整，v3.x 升级用户请参照 [迁移指南](./rule_engine.md# 迁移指南) 进行适配。

EMQ X Broker Rule Engine (以下简称规则引擎) 用于配置 EMQ X Broker 消息流与设备事件的处理、响应规则。规则引擎不仅提供了清晰、灵活的 "配置式" 的业务集成方案，简化了业务开发流程，提升用户易用性，降低业务系统与 EMQ X Broker 的耦合度；也为 EMQ X Broker 的私有功能定制提供了一个更优秀的基础架构。

![image-20190506171815028](../assets/image-20190506171815028.png)

EMQ X Broker 在 **消息发布或事件触发** 时将触发规则引擎，满足触发条件的规则将执行各自的 SQL 语句筛选并处理消息和事件的上下文信息。

### 消息发布

规则引擎借助响应动作可将特定主题的消息处理结果存储到数据库，发送到 HTTP Server，转发到消息队列 Kafka 或 RabbitMQ，重新发布到新的主题甚至是另一个 Broker 集群中，每个规则可以配置多个响应动作。

选择发布到 t/# 主题的消息，并筛选出全部字段：

```sql
SELECT * FROM "t/#"
```

选择发布到 t/a 主题的消息，并从 JSON 格式的消息内容中筛选出 "x" 字段：

```sql
SELECT payload.x as x FROM "t/a"
```

### 事件触发

规则引擎使用 **$events/** 开头的虚拟主题（**事件主题**）处理 EMQ X Broker 内置事件，内置事件提供更精细的消息控制和客户端动作处理能力，可用在 QoS 1 QoS 2 的消息抵达记录、设备上下线记录等业务中。

选择客户端连接事件，筛选 Username 为 'emqx' 的设备并获取连接信息：

```sql
SELECT clientid, connected_at FROM "$events/client_connected" WHERE username = 'emqx'
```

规则引擎数据和 SQL 语句格式，[事件主题](#event-topics) 列表详细教程参见 [SQL 手册](#rule-sql)。

## 最小规则

规则描述了 **数据从哪里来**、**如何筛选并处理数据**、**处理结果到哪里去** 三个配置，即一条可用的规则包含三个要素：

- 触发事件：规则通过事件触发，触发时事件给规则注入事件的上下文信息（数据源），通过 SQL 的 FROM 子句指定事件类型；
- 处理规则（SQL）：使用 SELECT 子句 和 WHERE 子句以及内置处理函数， 从上下文信息中过滤和处理数据；
- 响应动作：如果有处理结果输出，规则将执行相应的动作，如持久化到数据库、重新发布处理后的消息、转发消息到消息队列等。一条规则可以配置多个响应动作。

如图所示是一条简单的规则，该条规则用于处理 **消息发布** 时的数据，将全部主题消息的 `msg` 字段，消息 `topic` 、`qos` 筛选出来，发送到 Web Server 与 /uplink 主题：

![image-20190604103907875](../assets/image-20190604103907875.png)

## 规则引擎典型应用场景举例

- 动作监听：智慧家庭智能门锁开发中，门锁会因为网络、电源故障、人为破坏等原因离线导致功能异常，使用规则引擎配置监听离线事件向应用服务推送该故障信息，可以在接入层实现第一时间的故障检测的能力；
- 数据筛选：车辆网的卡车车队管理，车辆传感器采集并上报了大量运行数据，应用平台仅关注车速大于 40 km/h 时的数据，此场景下可以使用规则引擎对消息进行条件过滤，向业务消息队列写入满足条件的数据；
- 消息路由：智能计费应用中，终端设备通过不同主题区分业务类型，可通过配置规则引擎将计费业务的消息接入计费消息队列并在消息抵达设备端后发送确认通知到业务系统，非计费信息接入其他消息队列，实现业务消息路由配置；
- 消息编解码：其他公共协议 / 私有 TCP 协议接入、工控行业等应用场景下，可以通过规则引擎的本地处理函数（可在 EMQ X Broker 上定制开发）做二进制 / 特殊格式消息体的编解码工作；亦可通过规则引擎的消息路由将相关消息流向外部计算资源如函数计算进行处理（可由用户自行开发处理逻辑），将消息转为业务易于处理的 JSON 格式，简化项目集成难度、提升应用快速开发交付能力。

## 迁移指南

4.0 版本中规则引擎 SQL 语法更加易用，3.x 版本中所有事件 **FROM** 子句后面均需要指定事件名称，4.0 以后我们引入 **事件主题** 概念，默认情况下 **消息发布** 事件不再需要指定事件名称：

```sql
## 3.x 版本
## 需要指定事件名称进行处理
SELECT * FROM "message.publish" WHERE topic ~= 't/#'


## 4.0 及以后版本
## 默认处理 message.publish 事件，FROM 后面直接填写 MQTT 主题
## 上述 SQL 语句等价于:
SELECT * FROM 't/#'

## 其他事件，FROM 后面填写事件主题
SELECT * FROM "$events/message_acked" where topic ~= 't/#'
SELECT * FROM "$events/client_connected"
```

> Dashboard 中提供了旧版 SQL 语法转换功能可以完成 SQL 升级迁移。

## 规则引擎组成

使用 EMQ X Broker 的规则引擎可以灵活地处理消息和事件。使用规则引擎可以方便地实现诸如将消息转换成指定格式，然后存入数据库表，或者发送到消息队列等。

与 EMQ X Broker 规则引擎相关的概念包括: 规则(rule)、动作(action)、资源(resource) 和 资源类型(resource-type)。

规则、动作、资源的关系:

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

- 规则 (Rule): 规则由 SQL 语句和动作列表组成。
  SQL 语句用于筛选或转换消息中的数据。
  动作是 SQL 语句匹配通过之后，所执行的任务。动作列表包含一个或多个动作及其参数。
- 动作 (Action): 动作定义了一个针对数据的操作。
  动作可以绑定资源，也可以不绑定。例如，“inspect” 动作不需要绑定资源，它只是简单打印数据内容和动作参数。而 “data_to_webserver” 动作需要绑定一个 web_hook 类型的资源，此资源中配置了 URL。
- 资源 (Resource): 资源是通过资源类型为模板实例化出来的对象，保存了与资源相关的配置(比如数据库连接地址和端口、用户名和密码等)。
- 资源类型 (Resource Type): 资源类型是资源的静态定义，描述了此类型资源需要的配置项。

**NOTE:** 动作和资源类型是由 emqx 或插件的代码提供的，不能通过 API 和 CLI 动态创建。

## SQL 语句 {#rule-sql}

### SQL 语法 {#rule-sql-syntax}

SQL 语句用于从原始数据中，根据条件筛选出字段，并进行预处理和转换，基本格式为::

    SELECT <字段名> FROM <主题> [WHERE <条件>]

FROM、SELECT 和 WHERE 子句:

- ``FROM`` 子句将规则挂载到某个主题上
- ``SELECT`` 子句用于选择输出结果中的字段
- ``WHERE`` 子句用于根据条件筛选消息

### 事件和事件主题 {#event-topics}

规则引擎的 SQL 语句既可以处理消息(消息发布)，也可以处理事件(客户端上下线、客户端订阅等)。对于消息，FROM 子句后面直接跟主题名；对于事件，FROM 子句后面跟事件主题。

事件消息的主题以 `"$events/"` 开头，比如 `"$events/client_connected",` `"$events/session_subscribed"。`
如果想让 emqx 将事件消息发布出来，可以在 ``emqx_rule_engine.conf`` 文件中配置。

所有支持的事件及其可用字段详见: [规则事件](#rule-sql-events)。

### SQL 语句示例: {#rule-sql-examples}

- 从 topic 为 "t/a" 的消息中提取所有字段::

    SELECT * FROM "t/a"

- 从 topic 为 "t/a" 或 "t/b" 的消息中提取所有字段::

    SELECT * FROM "t/a","t/b"

- 从 topic 能够匹配到 't/#' 的消息中提取所有字段。

    SELECT * FROM "t/#"

- 从 topic 能够匹配到 't/#' 的消息中提取 qos, username 和 clientid 字段::

    SELECT qos, username, clientid FROM "t/#"

- 从任意 topic 的消息中提取 username 字段，并且筛选条件为 username = 'Steven'::

    SELECT username FROM "#" WHERE username='Steven'

- 从任意 topic 的 JSON 消息体(payload) 中提取 x 字段，并创建别名 x 以便在 WHERE 子句中使用。WHERE 子句限定条件为 x = 1。下面这个 SQL 语句可以匹配到消息体 {"x": 1}, 但不能匹配到消息体 {"x": 2}:

  SELECT payload as p FROM "#" WHERE p.x = 1

- 类似于上面的 SQL 语句，但嵌套地提取消息体中的数据，下面的 SQL 语句可以匹配到 JSON 消息体 {"x": {"y": 1}}:

  SELECT payload as a FROM "#" WHERE a.x.y = 1

- 在 clientid = 'c1' 尝试连接时，提取其来源 IP 地址和端口号::

    SELECT peername as ip_port FROM "$events/client_connected" WHERE clientid = 'c1'

- 筛选所有订阅 't/#' 主题且订阅级别为 QoS1 的 clientid::

    SELECT clientid FROM "$events/session_subscribed" WHERE topic = 't/#' and qos = 1

- 筛选所有订阅主题能匹配到 't/#' 且订阅级别为 QoS1 的 clientid。注意与上例不同的是，这里用的是主题匹配操作符 **'=~'**，所以会匹配订阅 't' 或 't/+/a' 的订阅事件::

    SELECT clientid FROM "$events/session_subscribed" WHERE topic =~ 't/#' and qos = 1

**NOTE: **

- FROM 子句后面的主题需要用双引号 ``""`` 引起来。
- WHERE 子句后面接筛选条件，如果使用到字符串需要用单引号 ``''`` 引起来。
- FROM 子句里如有多个主题，需要用逗号 ``","`` 分隔。例如 SELECT * FROM "t/1", "t/2" 。
- 可以使用使用 ``"."`` 符号对 payload 进行嵌套选择。

### FROM 子句可用的事件主题 {#rule-sql-syntax}

| 事件主题名                    | 释义     |
| ----------------------------- | :------- |
| $events/message\_delivered    | 消息投递 |
| $events/message\_acked        | 消息确认 |
| $events/message\_dropped      | 消息丢弃 |
| $events/client\_connected     | 连接完成 |
| $events/client\_disconnected  | 连接断开 |
| $events/session\_subscribed   | 订阅     |
| $events/session\_unsubscribed | 取消订阅 |

### SELECT 和 WHERE 子句可用的字段 {#rule-sql-columns}

SELECT 和 WHERE 子句可用的字段与事件的类型相关。其中 ``clientid``, ``username`` 和 ``event`` 是通用字段，每种事件类型都有。

#### 普通主题 (消息发布)

| event     | 事件类型，固定为 "message.publish"    |
| :-------- | :------------------------------------ |
| id        | MQTT 消息 ID                          |
| clientid  | Client ID                             |
| username  | 用户名                                |
| payload   | MQTT 消息体                           |
| peerhost  | 客户端的 IPAddress                    |
| topic     | MQTT 主题                             |
| qos       | MQTT 消息的 QoS                       |
| flags     | MQTT 消息的 Flags                     |
| headers   | MQTT 消息内部与流程处理相关的额外数据 |
| timestamp | 时间戳 (ms)                           |
| node      | 事件触发所在节点                      |

#### $events/message\_delivered (消息投递)

| event          | 事件类型，固定为 "message.delivered" |
| -------------- | ------------------------------------ |
| id             | MQTT 消息 ID                         |
| from\_clientid | 消息来源 Client ID                   |
| from\_username | 消息来源用户名                       |
| clientid       | 消息目的 Client ID                   |
| username       | 消息目的用户名                       |
| payload        | MQTT 消息体                          |
| peerhost       | 客户端的 IPAddress                   |
| topic          | MQTT 主题                            |
| qos            | MQTT 消息的 QoS                      |
| flags          | MQTT 消息的 Flags                    |
| timestamp      | 时间戳 (ms)                          |
| node           | 事件触发所在节点                     |

#### $events/message_acked (消息确认)
| event          | 事件类型，固定为 "message.acked" |
| :------------- | :------------------------------- |
| id             | MQTT 消息 ID                     |
| from\_clientid | 消息来源 Client ID               |
| from\_username | 消息来源用户名                   |
| clientid       | 消息目的 Client ID               |
| username       | 消息目的用户名                   |
| payload        | MQTT 消息体                      |
| peerhost       | 客户端的 IPAddress               |
| topic          | MQTT 主题                        |
| qos            | MQTT 消息的 QoS                  |
| flags          | MQTT 消息的 Flags                |
| timestamp      | 时间戳 (ms)                      |
| node           | 事件触发所在节点                 |

#### $events/message_dropped (消息丢弃)

| event     | 事件类型，固定为 "message.dropped" |
| :-------- | :--------------------------------- |
| id        | MQTT 消息 ID                       |
| reason    | 消息丢弃原因                       |
| clientid  | 消息目的 Client ID                 |
| username  | 消息目的用户名                     |
| payload   | MQTT 消息体                        |
| peerhost  | 客户端的 IPAddress                 |
| topic     | MQTT 主题                          |
| qos       | MQTT 消息的 QoS                    |
| flags     | MQTT 消息的 Flags                  |
| timestamp | 时间戳 (ms)                        |
| node      | 事件触发所在节点                   |

#### $events/client_connected (终端连接成功)
| event            | 事件类型，固定为 "client.connected" |
| ---------------- | :---------------------------------- |
| clientid         | 消息目的 Client ID                  |
| username         | 消息目的用户名                      |
| mountpoint       | 主题挂载点(主题前缀)                |
| peername         | 终端的 IPAddress 和 Port            |
| sockname         | emqx 监听的 IPAddress 和 Port       |
| proto\_name      | 协议名字                            |
| proto\_ver       | 协议版本                            |
| keepalive        | MQTT 保活间隔                       |
| clean\_start     | MQTT clean\_start                   |
| expiry\_interval | MQTT Session 过期时间               |
| is\_bridge       | 是否为 MQTT bridge 连接             |
| connected\_at    | 终端连接完成时间 (s)                |
| timestamp        | 时间戳 (ms)                         |
| node             | 事件触发所在节点                    |

#### $events/client_disconnected (终端连接断开)

| event            | 事件类型，固定为 "client.disconnected" |
| ---------------- | :------------------------------------- |
| reason           | 终端连接断开原因                       |
| clientid         | 消息目的 Client ID                     |
| username         | 消息目的用户名                         |
| peername         | 终端的 IPAddress 和 Port               |
| sockname         | emqx 监听的 IPAddress 和 Port          |
| disconnected\_at | 终端连接断开时间 (s)                   |
| timestamp        | 时间戳 (ms)                            |
| node             | 事件触发所在节点                       |

#### $events/session_subscribed (终端订阅成功)
| event     | 事件类型，固定为 "session.subscribed" |
| --------- | ------------------------------------- |
| clientid  | 消息目的 Client ID                    |
| username  | 消息目的用户名                        |
| peerhost  | 客户端的 IPAddress                    |
| topic     | MQTT 主题                             |
| qos       | MQTT 消息的 QoS                       |
| timestamp | 时间戳 (ms)                           |
| node      | 事件触发所在节点                      |

#### $events/session_unsubscribed (取消终端订阅成功)

| event     | 事件类型，固定为 "session.unsubscribed" |
| :-------- | :-------------------------------------- |
| clientid  | 消息目的 Client ID                      |
| username  | 消息目的用户名                          |
| peerhost  | 客户端的 IPAddress                      |
| topic     | MQTT 主题                               |
| qos       | MQTT 消息的 QoS                         |
| timestamp | 时间戳 (ms)                             |
| node      | 事件触发所在节点                        |

### SQL 关键字和符号 {#rule-sql-marks}

#### SELECT - FROM - WHERE 语句 {#rule-sql-reserved-keywords}

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

#### 运算符号 {#rule-sql-marks}

| 函数名 | 函数作用                                                     | 返回值     |      |
| ------ | ------------------------------------------------------------ | ---------- | ---- |
| `+`    | 加法                                                         | 加和       |      |
| `-`    | 减法                                                         | 差值       |      |
| `*`    | 乘法                                                         | 乘积       |      |
| `/`    | 除法                                                         | 商值       |      |
| `div`  | 整数除法                                                     | 整数商值   |      |
| `mod`  | 取模                                                         | 模         |      |
| `=`    | 比较两者是否完全相等。可用于比较变量和主题                   | true/false |      |
| `=~`   | 比较主题(topic)是否能够匹配到主题过滤器(topic filter)。只能用于主题匹配 | true/false |      |

### SQL 语句中可用的函数 {#rule-sql-funcs}

#### 数学函数

<table style="width:99%;">
<colgroup>
<col style="width: 12%" />
<col style="width: 23%" />
<col style="width: 41%" />
<col style="width: 20%" />
</colgroup>
<tbody>
<tr class="odd">
<td>函数名</td>
<td>函数作用</td>
<td>参数</td>
<td>返回值</td>
</tr>
<tr class="even">
<td>abs</td>
<td>绝对值</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>绝对值</td>
</tr>
<tr class="odd">
<td>cos</td>
<td>余弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>余弦值</td>
</tr>
<tr class="even">
<td>cosh</td>
<td>双曲余弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>双曲余弦值</td>
</tr>
<tr class="odd">
<td>acos</td>
<td>反余弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>反余弦值</td>
</tr>
<tr class="even">
<td>acosh</td>
<td>反双曲余弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>反双曲余弦值</td>
</tr>
<tr class="odd">
<td>sin</td>
<td>正弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>正弦值</td>
</tr>
<tr class="even">
<td>sinh</td>
<td>双曲正弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>双曲正弦值</td>
</tr>
<tr class="odd">
<td>asin</td>
<td>反正弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>值</td>
</tr>
<tr class="even">
<td>asinh</td>
<td>反双曲正弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>反双曲正弦值</td>
</tr>
<tr class="odd">
<td>tan</td>
<td>正切</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>正切值</td>
</tr>
<tr class="even">
<td>tanh</td>
<td>双曲正切</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>双曲正切值</td>
</tr>
<tr class="odd">
<td>atan</td>
<td>反正切</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>反正切值</td>
</tr>
<tr class="even">
<td>atanh</td>
<td>反双曲正切</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>反双曲正切值</td>
</tr>
<tr class="odd">
<td>ceil</td>
<td>上取整</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>整数值</td>
</tr>
<tr class="even">
<td>floor</td>
<td>下取整</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>整数值</td>
</tr>
<tr class="odd">
<td>round</td>
<td>四舍五入</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>整数值</td>
</tr>
<tr class="even">
<td>exp</td>
<td>幂运算</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>e 的 x 次幂</td>
</tr>
<tr class="odd">
<td>power</td>
<td>指数运算</td>
<td><ol type="1">
<li>左操作数 x 2. 右操作数 y</li>
</ol></td>
<td>x 的 y 次方</td>
</tr>
<tr class="even">
<td>sqrt</td>
<td>平方根运算</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>平方根</td>
</tr>
<tr class="odd">
<td>fmod</td>
<td>负点数取模函数</td>
<td><ol type="1">
<li>左操作数 2. 右操作数</li>
</ol></td>
<td>模</td>
</tr>
<tr class="even">
<td>log</td>
<td>以 e 为底对数</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>值</td>
</tr>
<tr class="odd">
<td>log10</td>
<td>以 10 为底对数</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>值</td>
</tr>
<tr class="even">
<td>log2</td>
<td>以 2 为底对数</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>值</td>
</tr>
</tbody>
</table>

#### 数据类型判断函数

<table>
<colgroup>
<col style="width: 9%" />
<col style="width: 35%" />
<col style="width: 6%" />
<col style="width: 48%" />
</colgroup>
<tbody>
<tr class="odd">
<td>函数名</td>
<td>函数作用</td>
<td>参数</td>
<td>返回值</td>
</tr>
<tr class="even">
<td>is_null</td>
<td>判断变量是否为空值</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。如果为空值(undefined) 则返回 true，否则返回 false</td>
</tr>
<tr class="odd">
<td>is_not_null</td>
<td>判断变量是否不为空值</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。如果为空值(undefined) 则返回 true，否则返回 false</td>
</tr>
<tr class="even">
<td>is_str</td>
<td>判断变量是否为 String 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。</td>
</tr>
<tr class="odd">
<td>is_bool</td>
<td>判断变量是否为 Boolean 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。</td>
</tr>
<tr class="even">
<td>is_int</td>
<td>判断变量是否为 Integer 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。</td>
</tr>
<tr class="odd">
<td>is_float</td>
<td>判断变量是否为 Float 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。</td>
</tr>
<tr class="even">
<td>is_num</td>
<td>判断变量是否为数字类型，包括 Integer 和 Float 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。</td>
</tr>
<tr class="odd">
<td>is_map</td>
<td>判断变量是否为 Map 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。</td>
</tr>
<tr class="even">
<td>is_array</td>
<td>判断变量是否为 Array 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。</td>
</tr>
</tbody>
</table>

#### 数据类型转换函数

<table>
<colgroup>
<col style="width: 9%" />
<col style="width: 29%" />
<col style="width: 9%" />
<col style="width: 51%" />
</colgroup>
<tbody>
<tr class="odd">
<td>函数名</td>
<td>函数作用</td>
<td>参数</td>
<td>返回值</td>
</tr>
<tr class="even">
<td>str</td>
<td>将数据转换为 String 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>String 类型的数据。无法转换将会导致 SQL 匹配失败</td>
</tr>
<tr class="odd">
<td>str_utf8</td>
<td>将数据转换为 UTF-8 String 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>UTF-8 String 类型的数据。无法转换将会导致 SQL 匹配失败</td>
</tr>
<tr class="even">
<td>bool</td>
<td>将数据转换为 Boolean 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。无法转换将会导致 SQL 匹配失败</td>
</tr>
<tr class="odd">
<td>int</td>
<td>将数据转换为整数类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>整数类型的数据。无法转换将会导致 SQL 匹配失败</td>
</tr>
<tr class="even">
<td>float</td>
<td>将数据转换为浮点型类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>浮点型类型的数据。无法转换将会导致 SQL 匹配失败</td>
</tr>
<tr class="odd">
<td>map</td>
<td>将数据转换为 Map 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Map 类型的数据。无法转换将会导致 SQL 匹配失败</td>
</tr>
</tbody>
</table>

#### 字符串函数

<table>
<colgroup>
<col style="width: 6%" />
<col style="width: 10%" />
<col style="width: 69%" />
<col style="width: 14%" />
</colgroup>
<tbody>
<tr class="odd">
<td>函数名</td>
<td>函数作用</td>
<td>参数</td>
<td>返回值</td>
</tr>
<tr class="even">
<td>lower</td>
<td>转为小写</td>
<td><ol type="1">
<li>输入字符串</li>
</ol></td>
<td>小写字符串</td>
</tr>
<tr class="odd">
<td>upper</td>
<td>转为大写</td>
<td><ol type="1">
<li>输入字符串</li>
</ol></td>
<td>大写字符串</td>
</tr>
<tr class="even">
<td>trim</td>
<td>去掉左右空格</td>
<td><ol type="1">
<li>输入字符串</li>
</ol></td>
<td>输出字符串</td>
</tr>
<tr class="odd">
<td>ltrim</td>
<td>去掉左空格</td>
<td><ol type="1">
<li>输入字符串</li>
</ol></td>
<td>输出字符串</td>
</tr>
<tr class="even">
<td>rtrim</td>
<td>去掉右空格</td>
<td><ol type="1">
<li>输入字符串</li>
</ol></td>
<td>输出字符串</td>
</tr>
<tr class="odd">
<td>reverse</td>
<td>字符串反转</td>
<td><ol type="1">
<li>输入字符串</li>
</ol></td>
<td>输出字符串</td>
</tr>
<tr class="even">
<td>strlen</td>
<td>字符串长度</td>
<td><ol type="1">
<li>输入字符串</li>
</ol></td>
<td>整数值</td>
</tr>
<tr class="odd">
<td>substr</td>
<td>取字符的子串</td>
<td><ol type="1">
<li>输入字符串 2. 起始位置. 注意: 下标从 1 开始</li>
</ol></td>
<td>子串</td>
</tr>
<tr class="even">
<td>substr</td>
<td>取字符的子串</td>
<td><ol type="1">
<li>输入字符串 2. 起始位置 3. 终止位置. 注意: 下标从 1 开始</li>
</ol></td>
<td>子串</td>
</tr>
<tr class="odd">
<td>split</td>
<td>字符串分割</td>
<td><ol type="1">
<li>输入字符串 2. 分割符子串</li>
</ol></td>
<td>分割后的字符串数组</td>
</tr>
<tr class="even">
<td>split</td>
<td>字符串分割</td>
<td><ol type="1">
<li>输入字符串 2. 分割符子串 3. 只查找左边或者右边第一个分隔符, 可选的取值为 'leading' 或者 'trailing'</li>
</ol></td>
<td>分割后的字符串数组</td>
</tr>
<tr class="odd">
<td>split</td>
<td>字符串分割</td>
<td><ol type="1">
<li>输入字符串 2. 分割符子串 3. 只查找左边或者右边第一个分隔符, 可选的取值为 'leading' 或者 'trailing'</li>
</ol></td>
<td>分割后的字符串数组</td>
</tr>
</tbody>
</table>

#### Map 函数

<table>
<colgroup>
<col style="width: 6%" />
<col style="width: 34%" />
<col style="width: 22%" />
<col style="width: 35%" />
</colgroup>
<tbody>
<tr class="odd">
<td>函数名</td>
<td>函数作用</td>
<td>参数</td>
<td>返回值</td>
</tr>
<tr class="even">
<td>map_get</td>
<td>取 Map 中某个 Key 的值，如果没有则返回空值</td>
<td><ol type="1">
<li>Key 2. Map</li>
</ol></td>
<td>Map 中某个 Key 的值。支持嵌套的 Key，比如 "a.b.c"</td>
</tr>
<tr class="odd">
<td>map_get</td>
<td>取 Map 中某个 Key 的值，如果没有则返回指定默认值</td>
<td><ol type="1">
<li>Key 2. Map 3. Default Value</li>
</ol></td>
<td>Map 中某个 Key 的值。支持嵌套的 Key，比如 "a.b.c"</td>
</tr>
<tr class="even">
<td>map_put</td>
<td>向 Map 中插入值</td>
<td><ol type="1">
<li>Key 2. Value 3. Map</li>
</ol></td>
<td>插入后的 Map。支持嵌套的 Key，比如 "a.b.c"</td>
</tr>
</tbody>
</table>

#### 数组函数

<table>
<colgroup>
<col style="width: 9%" />
<col style="width: 47%" />
<col style="width: 31%" />
<col style="width: 12%" />
</colgroup>
<tbody>
<tr class="odd">
<td>函数名</td>
<td>函数作用</td>
<td>参数</td>
<td>返回值</td>
</tr>
<tr class="even">
<td>nth</td>
<td>取第 n 个元素，下标从 1 开始</td>
<td><ol type="1">
<li>原数组</li>
</ol></td>
<td>第 n 个元素</td>
</tr>
<tr class="odd">
<td>length</td>
<td>获取数组的长度</td>
<td><ol type="1">
<li>原数组</li>
</ol></td>
<td>数组长度</td>
</tr>
<tr class="even">
<td>sublist</td>
<td>取从第一个元素开始、长度为 len 的子数组。下标从 1 开始</td>
<td><ol type="1">
<li>长度 len 2. 原数组</li>
</ol></td>
<td>子数组</td>
</tr>
<tr class="odd">
<td>sublist</td>
<td>取从第 n 个元素开始、长度为 len 的子数组。下标从 1 开始</td>
<td><ol type="1">
<li>起始位置 n 2. 长度 len 3. 原数组</li>
</ol></td>
<td>子数组</td>
</tr>
<tr class="even">
<td>first</td>
<td>取第 1 个元素。下标从 1 开始</td>
<td><ol type="1">
<li>原数组</li>
</ol></td>
<td>第 1 个元素</td>
</tr>
<tr class="odd">
<td>last</td>
<td>取最后一个元素。</td>
<td><ol type="1">
<li>原数组</li>
</ol></td>
<td>最后一个元素</td>
</tr>
<tr class="even">
<td>contains</td>
<td>判断数据是否在数组里面</td>
<td><ol type="1">
<li>数据 2. 原数组</li>
</ol></td>
<td>Boolean 值</td>
</tr>
</tbody>
</table>

#### 哈希函数

<table style="width:64%;">
<colgroup>
<col style="width: 12%" />
<col style="width: 20%" />
<col style="width: 13%" />
<col style="width: 16%" />
</colgroup>
<tbody>
<tr class="odd">
<td>函数名</td>
<td>函数作用</td>
<td>参数</td>
<td>返回值</td>
</tr>
<tr class="even">
<td>md5</td>
<td>求 MD5 值</td>
<td><ol type="1">
<li>数据</li>
</ol></td>
<td>MD5 值</td>
</tr>
<tr class="odd">
<td>sha</td>
<td>求 SHA 值</td>
<td><ol type="1">
<li>数据</li>
</ol></td>
<td>SHA 值</td>
</tr>
<tr class="even">
<td>sha256</td>
<td>求 SHA256 值</td>
<td><ol type="1">
<li>数据</li>
</ol></td>
<td>SHA256 值</td>
</tr>
</tbody>
</table>

#### 编解码函数

<table>
<colgroup>
<col style="width: 16%" />
<col style="width: 14%" />
<col style="width: 52%" />
<col style="width: 16%" />
</colgroup>
<tbody>
<tr class="odd">
<td>函数名</td>
<td>函数作用</td>
<td>参数</td>
<td>返回值</td>
</tr>
<tr class="even">
<td>base64_encode</td>
<td>BASE64 编码</td>
<td><ol type="1">
<li>数据</li>
</ol></td>
<td>BASE64 字符串</td>
</tr>
<tr class="odd">
<td>base64_decode</td>
<td>BASE64 解码</td>
<td><ol type="1">
<li>BASE64 字符串</li>
</ol></td>
<td>数据</td>
</tr>
<tr class="even">
<td>json_encode</td>
<td>JSON 编码</td>
<td><ol type="1">
<li>JSON 字符串</li>
</ol></td>
<td>内部 Map</td>
</tr>
<tr class="odd">
<td>json_decode</td>
<td>JSON 解码</td>
<td><ol type="1">
<li>内部 Map</li>
</ol></td>
<td>JSON 字符串</td>
</tr>
<tr class="even">
<td>schema_encode</td>
<td>Schema 编码</td>
<td><ol type="1">
<li>Schema ID 2. 内部 Map</li>
</ol></td>
<td>数据</td>
</tr>
<tr class="odd">
<td>schema_encode</td>
<td>Schema 编码</td>
<td><ol type="1">
<li>Schema ID 2. 内部 Map 3. Protobuf Message 名</li>
</ol></td>
<td>数据</td>
</tr>
<tr class="even">
<td>schema_decode</td>
<td>Schema 解码</td>
<td><ol type="1">
<li>Schema ID 2. 数据</li>
</ol></td>
<td>内部 Map</td>
</tr>
<tr class="odd">
<td>schema_decode</td>
<td>Schema 解码</td>
<td><ol type="1">
<li>Schema ID 2. 数据 3. Protobuf Message 名</li>
</ol></td>
<td>内部 Map</td>
</tr>
</tbody>
</table>

### 在 Dashboard 中测试 SQL 语句 {#test-rule-sql-funcs}

Dashboard 界面提供了 SQL 语句测试功能，通过给定的 SQL 语句和事件参数，展示 SQL 测试结果。

1.  在创建规则界面，输入 **规则SQL**，并启用 **SQL 测试** 开关:

    ![image](../assets/sql-test-1@2x.png)

2.  修改模拟事件的字段，或者使用默认的配置，点击 **测试** 按钮:

    ![image](../assets/sql-test-2@2x.png)

3.  SQL 处理后的结果将在 **测试输出** 文本框里展示:

    ![image](../assets/sql-test-3@2x.png)

## 规则引擎管理命令

### 规则引擎(rule engine) 命令

#### rules 命令

| rules list                                             | List all rules |
| ------------------------------------------------------ | -------------- |
| rules show `<RuleId>`                                 | Show a rule    |
| emqx_ctl rules create `<sql> <actions> [-d [<descr>]]` | Create a rule  |
| rules delete `<RuleId>`                                | Delete a rule  |

##### rules create

创建一个新的规则。参数:

  - \<sql\>: 规则 SQL
  - \<actions\>: JSON 格式的动作列表
  - \-d \<descr\>: 可选，规则描述信息

使用举例:

    ## 创建一个测试规则，简单打印所有发送到 't/a' 主题的消息内容
    $ ./bin/emqx_ctl rules create \
      'select * from "t/a"' \
      '[{"name":"inspect", "params": {"a": 1}}]' \
      -d 'Rule for debug'
    
    Rule rule:9a6a725d created

上例创建了一个 ID 为 `rule:9a6a725d` 的规则，动作列表里只有一个动作：动作名为 inspect，动作的参数是
`{"a": 1}`。

##### rules list

列出当前所有的规则:

    $ ./bin/emqx_ctl rules list
    
    rule(id='rule:9a6a725d', for='['t/a']', rawsql='select * from "t/a"', actions=[{"metrics":...,"name":"inspect","params":...}], metrics=..., enabled='true', description='Rule for debug')

##### rules show

查询规则:

    ## 查询 RuleID 为 'rule:9a6a725d' 的规则
    $ ./bin/emqx_ctl rules show 'rule:9a6a725d'
    
    rule(id='rule:9a6a725d', for='['t/a']', rawsql='select * from "t/a"', actions=[{"metrics":...,"name":"inspect","params":...}], metrics=..., enabled='true', description='Rule for debug')

##### rules delete

删除规则:

    ## 删除 RuleID 为 'rule:9a6a725d' 的规则
    $ ./bin/emqx_ctl rules delete 'rule:9a6a725d'
    
    ok

#### rule-actions 命令

|                                |                    |
| ------------------------------ | ------------------ |
| rule-actions show `<ActionId>` | Show a rule action |

**Note: **

动作可以由 emqx 内置(称为系统内置动作)，或者由 emqx 插件编写，但不能通过 CLI/API 添加或删除。

##### rule-actions show

查询动作:

    ## 查询名为 'inspect' 的动作
    $ ./bin/emqx_ctl rule-actions show 'inspect'
    
    action(name='inspect', app='emqx_rule_engine', types=[], title ='Inspect (debug)', description='Inspect the details of action params for debug purpose')

##### rule-actions list

列出符合条件的动作:

    ## 列出当前所有的动作
    $ ./bin/emqx_ctl rule-actions list
    
    action(name='data_to_rabbit', app='emqx_bridge_rabbit', types=[bridge_rabbit], title ='Data bridge to RabbitMQ', description='Store Data to Kafka')
    action(name='data_to_timescaledb', app='emqx_backend_pgsql', types=[timescaledb], title ='Data to TimescaleDB', description='Store data to TimescaleDB')
    ...

#### resources 命令

|                                                              |                   |
| ------------------------------------------------------------ | ----------------- |
| resources create `<type> [-c [<config>]] [-d [<descr>]]`     | Create a resource |
| resources list `[-t <ResourceType>]`                       | List resources    |
| resources show `<ResourceId>`                                | Show a resource   |
| resources delete `<ResourceId>`                              | Delete a resource |

##### resources create

创建一个新的资源，参数:

  - type: 资源类型
  - \-c config: JSON 格式的配置
  - \-d descr: 可选，资源的描述

<!-- end list -->

    $ ./bin/emqx_ctl resources create 'web_hook' -c '{"url": "http://host-name/chats"}' -d 'forward msgs to host-name/chats'
    
    Resource resource:a7a38187 created

##### resources list

列出当前所有的资源:

    $ ./bin/emqx_ctl resources list
    
    resource(id='resource:a7a38187', type='web_hook', config=#{<<"url">> => <<"http://host-name/chats">>}, status=#{is_alive => false}, description='forward msgs to host-name/chats')

##### resources list by type

列出当前所有的资源:

    $ ./bin/emqx_ctl resources list --type='web_hook'
    
    resource(id='resource:a7a38187', type='web_hook', config=#{<<"url">> => <<"http://host-name/chats">>}, status=#{is_alive => false}, description='forward msgs to host-name/chats')

##### resources show

查询资源:

    $ ./bin/emqx_ctl resources show 'resource:a7a38187'
    
    resource(id='resource:a7a38187', type='web_hook', config=#{<<"url">> => <<"http://host-name/chats">>}, status=#{is_alive => false}, description='forward msgs to host-name/chats')

##### resources delete

删除资源:

    $ ./bin/emqx_ctl resources delete 'resource:a7a38187'
    
    ok

#### resource-types 命令

|                              |                         |
| ---------------------------- | ----------------------- |
| resource-types list          | List all resource-types |
| resource-types show `<Type>` | Show a resource-type    |

**Note:**

资源类型可以由 emqx 内置(称为系统内置资源类型)，或者由 emqx 插件编写，但不能通过 CLI/API 添加或删除。

##### resource-types list

列出当前所有的资源类型:

    ./bin/emqx_ctl resource-types list
    
    resource_type(name='backend_mongo_rs', provider='emqx_backend_mongo', title ='MongoDB Replica Set Mode', description='MongoDB Replica Set Mode')
    resource_type(name='backend_cassa', provider='emqx_backend_cassa', title ='Cassandra', description='Cassandra Database')
    ...

##### resource-types show

查询资源类型:

    $ ./bin/emqx_ctl resource-types show backend_mysql

    resource_type(name='backend_mysql', provider='emqx_backend_mysql', title ='MySQL', description='MySQL Database')

## 与规则引擎相关的状态、统计指标和告警

### 规则状态和统计指标

![image](../assets/rule_metrics.png)

  - 已命中: 规则命中(规则 SQL 匹配成功)的次数，
  - 命中速度: 规则命中的速度(次/秒)
  - 最大命中速度: 规则命中速度的峰值(次/秒)
  - 5分钟平均速度: 5分钟内规则的平均命中速度(次/秒)

### 动作状态和统计指标 {#action-metrics}

![image](../assets/action_metrics.png)

  - 成功: 动作执行成功次数
  - 失败: 动作执行失败次数

### 资源状态和告警 {#resource-metrics}

![image](../assets/resource_status.png)

  - 可用: 资源可用
  - 不可用: 资源不可用(比如数据库连接断开)
