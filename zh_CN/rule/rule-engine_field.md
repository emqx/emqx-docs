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

# 规则引擎 SQL 语句中可用的字段
SELECT 和 WHERE 子句可用的字段与事件的类型相关。其中 `clientid`, `username` 和 `event` 是通用字段，每种事件类型都有。

## 使用规则引擎 SQL 语句处理消息发布
规则引擎的 SQL 语句可以处理消息发布。 在一个规则语句中，用户可以用 FROM 子句指定一个或者多个主题，当任何消息发布到指定的主题时都会触发该规则。

| 字段                  | 解释                                   |
|:----------------------|:---------------------------------------|
| id                    | MQTT 消息 ID                           |
| clientid              | 消息来源 Client ID                     |
| username              | 消息来源用户名                         |
| payload               | MQTT 消息体                            |
| peerhost              | 客户端的 IPAddress                     |
| topic                 | MQTT 主题                              |
| qos                   | MQTT 消息的 QoS                        |
| flags                 | MQTT 消息的 Flags                      |
| headers               | MQTT 消息内部与流程处理相关的额外数据  |
| pub\_props            | PUBLISH Properties (仅适用于 MQTT 5.0) |
| timestamp             | 事件触发时间 (ms)                      |
| publish\_received\_at | PUBLISH 消息到达 Broker 的时间 (ms)    |
| node                  | 事件触发所在节点                       |

示例
```sql
SELECT
  payload.msg as msg,
  clientid,
  username,
  payload,
  topic,
  qos
FROM
  "t/#"
```

输出
```json
{
  "username": "u_emqx",
  "topic": "t/a",
  "qos": 1,
  "payload": "{\"msg\":\"hello\"}",
  "msg": "hello",
  "clientid": "c_emqx"
}
```

## 使用规则引擎 SQL 语句处理事件
规则引擎的 SQL 语句既可以处理消息(消息发布)，也可以处理事件(客户端上下线、客户端订阅等)。对于消息，FROM 子句后面直接跟主题名；对于事件，FROM 子句后面跟事件主题。

事件消息的主题以 `"$events/"` 开头，比如 `"$events/client_connected",` `"$events/session_subscribed"。`
如果想让 emqx 将事件消息发布出来，可以在 `emqx_rule_engine.conf` 文件中配置。



### FROM 子句可用的事件主题
| 事件主题名                    | 释义                     |
|-------------------------------|:-------------------------|
| $events/message\_delivered    | 消息投递                 |
| $events/message\_acked        | 消息确认                 |
| $events/message\_dropped      | 消息在转发的过程中被丢弃 |
| $events/delivery\_dropped     | 消息在投递的过程中被丢弃 |
| $events/client\_connected     | 连接完成                 |
| $events/client\_disconnected  | 连接断开                 |
| $events/session\_subscribed   | 订阅                     |
| $events/session\_unsubscribed | 取消订阅                 |

### $events/message_delivered (消息投递)

当消息被放入底层socket时触发规则

|        字段         |  解释                                 |
| :------------------ | :------------------------------------ |
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

示例
```sql
SELECT
  from_clientid,
  from_username,
  topic,
  qos,
  node,
  timestamp
FROM
  "$events/message_delivered"
```
输出
```json
{
  "topic": "t/a",
  "timestamp": 1645002753259,
  "qos": 1,
  "node": "emqx@127.0.0.1",
  "from_username": "u_emqx_1",
  "from_clientid": "c_emqx_1"
}
```
### $events/message_acked (消息确认)

当消息发送到客户端，并收到客户端回复的ack时触发规则，仅QOS1，QOS2会触发

|        字段         |  解释                                 |
| :------------------ | :------------------------------------ |
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

示例
```sql
SELECT
  from_clientid,
  from_username,
  topic,
  qos,
  node,
  timestamp
FROM
  "$events/message_acked"
```
输出
```json
{
  "topic": "t/a",
  "timestamp": 1645002965664,
  "qos": 1,
  "node": "emqx@127.0.0.1",
  "from_username": "u_emqx_1",
  "from_clientid": "c_emqx_1"
}
```

### $events/message_dropped (消息在转发的过程中被丢弃)

当一条消息被丢弃时触发规则

| 字段                  | 解释                                                                                                                                                              |
|:----------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| id                    | MQTT 消息 ID                                                                                                                                                      |
| reason                | 消息丢弃原因，可能的原因：<br/>no\_subscribers: 没有订阅者<br/>receive\_maximum\_exceeded: awaiting\_rel 队列已满</br>packet\_identifier\_inuse: 消息 ID 已被使用 |
| clientid              | 消息来源 Client ID                                                                                                                                                |
| username              | 消息来源用户名                                                                                                                                                    |
| payload               | MQTT 消息体                                                                                                                                                       |
| peerhost              | 客户端的 IPAddress                                                                                                                                                |
| topic                 | MQTT 主题                                                                                                                                                         |
| qos                   | MQTT 消息的 QoS                                                                                                                                                   |
| flags                 | MQTT 消息的 Flags                                                                                                                                                 |
| pub\_props            | PUBLISH Properties (仅适用于 MQTT 5.0)                                                                                                                            |
| timestamp             | 事件触发时间 (ms)                                                                                                                                                 |
| publish\_received\_at | PUBLISH 消息到达 Broker 的时间 (ms)                                                                                                                               |
| node                  | 事件触发所在节点                                                                                                                                                  |

示例
```sql
SELECT
  reason,
  topic,
  qos,
  node,
  timestamp
FROM
  "$events/message_dropped"
```
输出
```json
{
  "topic": "t/a",
  "timestamp": 1645003103004,
  "reason": "no_subscribers",
  "qos": 1,
  "node": "emqx@127.0.0.1"
}
```

### $events/delivery_dropped (消息在投递的过程中被丢弃)

当订阅者的消息队列已满时触发规则

| 字段                  | 解释                                                                                                                                                                                         |
|:----------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| id                    | MQTT 消息 ID                                                                                                                                                                                 |
| reason                | 消息丢弃原因，可能的原因：<br/>queue\_full: 消息队列已满(QoS>0)<br/>no\_local: 不允许客户端接收自己发布的消息<br/>expired: 消息或者会话过期<br/>qos0\_msg: QoS0 的消息因为消息队列已满被丢弃 |
| from\_clientid        | 消息来源 Client ID                                                                                                                                                                           |
| from\_username        | 消息来源用户名                                                                                                                                                                               |
| clientid              | 消息目的 Client ID                                                                                                                                                                           |
| username              | 消息目的用户名                                                                                                                                                                               |
| payload               | MQTT 消息体                                                                                                                                                                                  |
| peerhost              | 客户端的 IPAddress                                                                                                                                                                           |
| topic                 | MQTT 主题                                                                                                                                                                                    |
| qos                   | MQTT 消息的 QoS                                                                                                                                                                              |
| flags                 | MQTT 消息的 Flags                                                                                                                                                                            |
| pub\_props            | PUBLISH Properties (仅适用于 MQTT 5.0)                                                                                                                                                       |
| timestamp             | 事件触发时间 (ms)                                                                                                                                                                            |
| publish\_received\_at | PUBLISH 消息到达 Broker 的时间 (ms)                                                                                                                                                          |
| node                  | 事件触发所在节点                                                                                                                                                                             |

示例
```sql
SELECT
  from_clientid,
  from_username,
  reason,
  topic,
  qos
FROM "$events/delivery_dropped"
```
输出
```json
{
  "topic": "t/a",
  "reason": "queue_full",
  "qos": 1,
  "from_username": "u_emqx_1",
  "from_clientid": "c_emqx_1"
}
```
### $events/client_connected (终端连接成功)

当终端连接成功时触发规则

|        字段         |  解释                                 |
| :------------------ | :------------------------------------ |
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
| connected_at    | 终端连接完成时间 (ms)                |
| conn_props      | CONNECT Properties (仅适用于 MQTT 5.0) |
| timestamp       | 事件触发时间 (ms)                   |
| node            | 事件触发所在节点                    |

示例
```sql
SELECT
  clientid,
  username,
  keepalive,
  is_bridge
FROM
  "$events/client_connected"
```
输出
```json
{
  "username": "u_emqx",
  "keepalive": 60,
  "is_bridge": false,
  "clientid": "c_emqx"
}
```

### $events/client_disconnected (终端连接断开)

当终端连接断开时触发规则

|        字段         |  解释                                 |
| :------------------ | :------------------------------------ |
| reason          | 终端连接断开原因：<br/>normal：客户端主动断开<br/>kicked：服务端踢出，通过 REST API<br/>keepalive_timeout: keepalive 超时<br/>not_authorized:  认证失败，或者 acl_nomatch = disconnect 时没有权限的 Pub/Sub 会主动断开客户端<br/>tcp_closed: 对端关闭了网络连接<br/>discarded: 因为相同 ClientID 的客户端上线且设置 `clean_start = true`<br/>takeovered: 因为相同 ClientID 的客户端上线且设置 `clean_start = false`<br/>internal_error: 畸形报文或其他未知错误<br/> |
| clientid        | 消息目的 Client ID                                           |
| username        | 消息目的用户名                                               |
| peername        | 终端的 IPAddress 和 Port                                     |
| sockname        | emqx 监听的 IPAddress 和 Port                                |
| disconnected_at | 终端连接断开时间 (ms)                                         |
| disconn_props   | DISCONNECT Properties (仅适用于 MQTT 5.0)                    |
| timestamp       | 事件触发时间 (ms)                                            |
| node            | 事件触发所在节点                                             |

示例
```sql
SELECT
  clientid,
  username,
  reason,
  disconnected_at,
  node
FROM
  "$events/client_disconnected"
```
输出
```json
{
  "username": "u_emqx",
  "reason": "normal",
  "node": "emqx@127.0.0.1",
  "disconnected_at": 1645003578536,
  "clientid": "c_emqx"
}
```

### $events/session_subscribed (终端订阅成功)

当终端订阅成功时触发规则

|        字段         |  解释                       |
| :------------------ | :--------------------------|
| clientid  | 消息目的 Client ID                    |
| username  | 消息目的用户名                        |
| peerhost  | 客户端的 IPAddress                    |
| topic     | MQTT 主题                             |
| qos       | MQTT 消息的 QoS                       |
| sub_props | SUBSCRIBE Properties (仅适用于 5.0)  |
| timestamp | 事件触发时间 (ms)                     |
| node      | 事件触发所在节点                      |

示例
```sql
SELECT
  clientid,
  username,
  topic,
  qos
FROM
  "$events/session_subscribed"
```
输出
```json
{
  "username": "u_emqx",
  "topic": "t/a",
  "qos": 1,
  "clientid": "c_emqx"
}
```

### $events/session_unsubscribed (取消终端订阅成功)

当取消终端订阅成功时触发规则

|        字段         |  解释                         |
| :------------------ | :---------------------------- |
| clientid  | 消息目的 Client ID                      |
| username  | 消息目的用户名                          |
| peerhost  | 客户端的 IPAddress                      |
| topic     | MQTT 主题                               |
| qos       | MQTT 消息的 QoS                         |
| unsub_props | UNSUBSCRIBE Properties (仅适用于 5.0)  |
| timestamp | 事件触发时间 (ms)                       |
| node      | 事件触发所在节点         

示例
```sql
SELECT
  clientid,
  username,
  topic,
  qos
FROM
  "$events/session_unsubscribed"
```
输出
```json
{
  "username": "u_emqx",
  "topic": "t/a",
  "qos": 1,
  "clientid": "c_emqx"
}
```

[下一部分，规则引擎内置函数](./rule-engine_buildin_function.md)
