# SQL 数据源和字段

规则的 SQL 语句可以处理的数据源有：MQTT 消息、客户端事件，或是连接外部数据系统的 Source。

SQL 语句使用 `FROM` 来指定数据源，在 `SELECT` 和 `WHERE` 子句中可以引用相应的字段。
数据源类型不同，可以使用的字段也不同。

## MQTT 消息

规则的 SQL 语句可以处理消息发布。 在一个规则语句中，用户可以用 FROM 子句指定一个或者多个主题，
当任何消息发布到指定的主题时都会触发该规则。

| 字段                | 解释                                        |
| :------------------ | :------------------------------------------ |
| id                  | MQTT 消息 ID                                |
| clientid            | 消息来源 Client ID                          |
| username            | 消息来源用户名                              |
| payload             | MQTT 消息体                                 |
| peerhost            | 客户端的 IPAddress                          |
| topic               | MQTT 主题                                   |
| qos                 | MQTT 消息的 QoS                             |
| flags               | MQTT 消息的 Flags                           |
| pub\_props          | PUBLISH Properties (仅适用于 MQTT 5.0)      |
| timestamp           | 事件触发时间 (单位：毫秒)                   |
| publish_received_at | PUBLISH 消息到达 Broker 的时间 (单位：毫秒) |
| node                | 事件触发所在节点                            |
| client_attrs        | [客户端属性](../client-attributes/client-attributes.md) |

SQL 示例

```sql
SELECT
  *
FROM
  "t/#"
```

输出

```json
{
  "clientid": "c_emqx",
  "event": "message.publish",
  "event_type": "message_publish",
  "flags": {},
  "id": "0005E27C1D24E44FF440000017520000",
  "metadata": {
    "rule_id": "sql_tester:099ddfa9c466d1ca"
  },
  "node": "emqx@127.0.0.1",
  "payload": "abc",
  "peerhost": "192.168.0.10",
  "pub_props": {
    "Message-Expiry-Interval": 30,
    "Payload-Format-Indicator": 0,
    "User-Property": {
      "foo": "bar"
    },
    "User-Property-Pairs": [
      {
        "key": "foo"
      },
      {
        "value": "bar"
      }
    ]
  },
  "publish_received_at": 1656397576334,
  "qos": 1,
  "timestamp": 1656397576334,
  "topic": "t/a",
  "username": "u_emqx"
}
```

从 EMQX 6.0.3 开始，启用命名空间且 `rule_engine.limit_selects_in_namespace` 设置为 `true` 时，属于某个命名空间的规则只会被同一命名空间内客户端发布的消息触发。该配置默认启用。

## 客户端事件

规则的 SQL 语句既可以处理消息(消息发布)，也可以处理事件(客户端上下线、客户端订阅等)。对于消息，FROM 子句后面直接跟主题名；对于事件，FROM 子句后面跟事件主题。

事件主题以 `$events/` 开头，比如 `$events/client/connected`，`$events/session/subscribed`。

从 EMQX 6.0.3 开始，启用命名空间且 `rule_engine.limit_selects_in_namespace` 设置为 `true` 时，属于某个命名空间的规则只会被同一命名空间内客户端相关事件触发。系统告警事件不关联任何客户端命名空间；该配置启用时，`$events/sys/alarm_activated` 和 `$events/sys/alarm_deactivated` 不会触发规则。

::: tip

默认情况下，客户端无法直接订阅客户端事件消息。 本节介绍了如何使用规则来订阅这些消息，您也可以通过订阅[系统主题](../observability/mqtt-system-topics.md)直接获取客户端事件消息。

:::

| 事件名称                                                     | 事件主题名                          | 释义                     |
| ------------------------------------------------------------ | ----------------------------------- | ------------------------ |
| [消息投递事件](#消息投递事件-events-message-delivered)       | $events/message/delivered           | 消息投递                 |
| [消息确认事件](#消息确认事件-events-message-acked)           | $events/message/acked               | 消息确认                 |
| [消息在转发的过程中被丢弃事件](#消息在转发的过程中被丢弃事件-events-message-dropped) | $events/message/dropped             | 消息在转发的过程中被丢弃 |
| [消息在投递的过程中被丢弃事件](#消息在投递的过程中被丢弃事件-events-delivery-dropped) | $events/message/delivery_dropped            | 消息在投递的过程中被丢弃 |
| [客户端连接成功事件](#客户端连接成功事件-events-client-connected) | $events/client/connected            | 连接完成                 |
| [客户端连接断开事件](#客户端连接断开事件-events-client-disconnected) | $events/client/disconnected         | 连接断开                 |
| [连接确认事件](#连接确认事件-events-client-connack)          | $events/client/connack              | 连接确认                 |
| [鉴权完成事件](#鉴权完成事件-events-client-check-authz-complete) | $events/auth/check_authz_complete | 鉴权完成                 |
| [认证完成事件](#认证完成事件-events-client-check-authn-complete) | $events/auth/check_authn_complete | 认证完成                 |
| [客户端订阅成功事件](#客户端订阅成功事件-events-session-subscribed) | $events/session/subscribed          | 订阅                     |
| [客户端取消订阅成功事件](#客户端取消订阅成功事件-events-session-unsubscribed) | $events/session/unsubscribed        | 取消订阅                 |
| [系统告警激活事件](#告警激活事件-events-sys-alarm-activated) | $events/sys/alarm_activated         | 系统告警激活             |
| [系统告警解除事件](#告警解除事件-events-sys-alarm-deactivated) | $events/sys/alarm_deactivated       | 系统告警解除             |
| [客户端 Keepalive（PING）事件](#客户端-keepaliveping-事件-events-client-ping) | $events/client/ping | 收到 `PINGREQ` 报文 |

::: tip

从 EMQX 5.10.0 开始，客户端事件主题采用了命名空间结构，将事件主题重新组织为逻辑清晰的分层结构。这一调整使事件主题的分类更加直观，便于后续的筛选、管理与扩展。

为保证向后兼容，旧版事件主题仍然可用。但推荐在新配置中优先使用新的命名空间事件主题。下表展示了旧事件主题与新（命名空间）事件主题之间的对应关系：

| 旧事件主题                              | 新事件主题                              |
| :-------------------------------------- | :-------------------------------------- |
| `$events/client_connected`              | `$events/client/connected`              |
| `$events/client_disconnected`           | `$events/client/disconnected`           |
| `$events/client_connack`                | `$events/client/connack`                |
| `$events/client_check_authz_complete`   | `$events/auth/check_authz_complete`     |
| `$events/client_check_authn_complete`   | `$events/auth/check_authn_complete`     |
| `$events/session_subscribed`            | `$events/session/subscribed`            |
| `$events/session_unsubscribed`          | `$events/session/unsubscribed`          |
| `$events/message_delivered`             | `$events/message/delivered`             |
| `$events/message_acked`                 | `$events/message/acked`                 |
| `$events/message_dropped`               | `$events/message/dropped`               |
| `$events/delivery_dropped`              | `$events/message/delivery_dropped`      |
| `$events/message_transformation_failed` | `$events/message_transformation/failed` |
| `$events/schema_validation_failed`      | `$events/schema_validation/failed`      |

:::

### 消息投递事件 ("$events/message/delivered")

当消息被放入底层socket时触发规则。

| 字段                  | 解释                                        |
| :-------------------- | :------------------------------------------ |
| id                    | MQTT 消息 ID                                |
| from\_clientid        | 消息来源 Client ID                          |
| from\_username        | 消息来源用户名                              |
| clientid              | 消息目的 Client ID                          |
| username              | 消息目的用户名                              |
| payload               | MQTT 消息体                                 |
| peerhost              | 客户端的 IPAddress                          |
| topic                 | MQTT 主题                                   |
| qos                   | MQTT 消息的 QoS                             |
| flags                 | MQTT 消息的 Flags                           |
| pub\_props            | PUBLISH Properties (仅适用于 MQTT 5.0)      |
| timestamp             | 事件触发时间 (单位：毫秒)                   |
| publish\_received\_at | PUBLISH 消息到达 Broker 的时间 (单位：毫秒) |
| node                  | 事件触发所在节点                            |

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
  "$events/message/delivered"
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

### 消息确认事件 ("$events/message/acked")

当消息发送到客户端，并收到客户端回复的ack时触发规则，仅QOS1，QOS2会触发。

| 字段                  | 解释                                        |
| :-------------------- | :------------------------------------------ |
| id                    | MQTT 消息 ID                                |
| from\_clientid        | 消息来源 Client ID                          |
| from\_username        | 消息来源用户名                              |
| clientid              | 消息目的 Client ID                          |
| username              | 消息目的用户名                              |
| payload               | MQTT 消息体                                 |
| peerhost              | 客户端的 IPAddress                          |
| topic                 | MQTT 主题                                   |
| qos                   | MQTT 消息的 QoS                             |
| flags                 | MQTT 消息的 Flags                           |
| pub\_props            | PUBLISH Properties (仅适用于 MQTT 5.0)      |
| puback\_props         | PUBACK Properties (仅适用于 MQTT 5.0)       |
| timestamp             | 事件触发时间 (单位：毫秒)                   |
| publish\_received\_at | PUBLISH 消息到达 Broker 的时间 (单位：毫秒) |
| node                  | 事件触发所在节点                            |

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
  "$events/message/acked"
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

### 消息在转发的过程中被丢弃事件 ("$events/message/dropped")

当一条消息无任何订阅者时触发规则。

| 字段                  | 解释                                                                                                                                                              |
| :-------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| id                    | MQTT 消息 ID                                                                                                                                                      |
| reason                | 消息丢弃原因，可能的原因：<br/>no\_subscribers：没有订阅者<br/>receive\_maximum\_exceeded: awaiting\_rel 队列已满<br/>packet\_identifier\_inuse: 消息 ID 已被使用 |
| clientid              | 消息来源 Client ID                                                                                                                                                |
| username              | 消息来源用户名                                                                                                                                                    |
| payload               | MQTT 消息体                                                                                                                                                       |
| peerhost              | 客户端的 IPAddress                                                                                                                                                |
| topic                 | MQTT 主题                                                                                                                                                         |
| qos                   | MQTT 消息的 QoS                                                                                                                                                   |
| flags                 | MQTT 消息的 Flags                                                                                                                                                 |
| pub\_props            | PUBLISH Properties (仅适用于 MQTT 5.0)                                                                                                                            |
| timestamp             | 事件触发时间 (单位：毫秒)                                                                                                                                         |
| publish\_received\_at | PUBLISH 消息到达 Broker 的时间 (单位：毫秒)                                                                                                                       |
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
  "$events/message/dropped"
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

### 消息在投递的过程中被丢弃事件 ("$events/message/delivery_dropped")

当订阅者的消息队列已满时触发规则。

| 字段                  | 解释                                                                                                                                                                                          |
| :-------------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| id                    | MQTT 消息 ID                                                                                                                                                                                  |
| reason                | 消息丢弃原因，可能的原因：<br/>queue\_full：消息队列已满(QoS>0)<br/>no\_local：不允许客户端接收自己发布的消息<br/>expired：消息或者会话过期<br/>qos0\_msg：QoS 0 的消息因为消息队列已满被丢弃 |
| from\_clientid        | 消息来源 Client ID                                                                                                                                                                            |
| from\_username        | 消息来源用户名                                                                                                                                                                                |
| clientid              | 消息目的 Client ID                                                                                                                                                                            |
| username              | 消息目的用户名                                                                                                                                                                                |
| payload               | MQTT 消息体                                                                                                                                                                                   |
| peerhost              | 客户端的 IPAddress                                                                                                                                                                            |
| topic                 | MQTT 主题                                                                                                                                                                                     |
| qos                   | MQTT 消息的 QoS                                                                                                                                                                               |
| flags                 | MQTT 消息的 Flags                                                                                                                                                                             |
| pub\_props            | PUBLISH Properties (仅适用于 MQTT 5.0)                                                                                                                                                        |
| timestamp             | 事件触发时间 (单位：毫秒)                                                                                                                                                                     |
| publish\_received\_at | PUBLISH 消息到达 Broker 的时间 (单位：毫秒)                                                                                                                                                   |
| node                  | 事件触发所在节点                                                                                                                                                                              |

示例

```sql
SELECT
  from_clientid,
  from_username,
  reason,
  topic,
  qos
FROM "$events/message/delivery_dropped"
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

### 客户端连接成功事件 ("$events/client/connected")

当客户端连接成功时触发规则。

| 字段             | 解释                                   |
| :--------------- | :------------------------------------- |
| clientid         | 消息目的 Client ID                     |
| username         | 消息目的用户名                         |
| mountpoint       | 主题挂载点(主题前缀)                   |
| peername         | 客户端的 IPAddress 和 Port               |
| sockname         | emqx 监听的 IPAddress 和 Port          |
| proto\_name      | 协议名字                               |
| proto\_ver       | 协议版本                               |
| keepalive        | MQTT 保活间隔                          |
| clean\_start     | MQTT clean\_start                      |
| expiry\_interval | MQTT Session 过期时间                  |
| is\_bridge       | 是否为 MQTT bridge 连接                |
| connected\_at    | 客户端连接完成时间 (单位：毫秒)          |
| conn\_props      | CONNECT Properties (仅适用于 MQTT 5.0) |
| timestamp        | 事件触发时间 (单位：毫秒)              |
| node             | 事件触发所在节点                       |
| client_attrs        | [客户端属性](../client-attributes/client-attributes.md) |

示例

```sql
SELECT
  clientid,
  username,
  keepalive,
  is_bridge
FROM
  "$events/client/connected"
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

### 客户端连接断开事件 ("$events/client/disconnected")

当客户端连接断开时触发规则。

| 字段             | 解释                                                         |
| :--------------- | :----------------------------------------------------------- |
| reason           | 客户端连接断开原因：<br/>normal：客户端主动断开<br/>kicked：服务端踢出，通过 REST API<br/>keepalive\_timeout：keepalive 超时<br/>not\_authorized：认证失败，或者 acl\_nomatch = disconnect 时没有权限的 Pub/Sub 会主动断开客户端<br/>tcp\_closed：对端关闭了网络连接<br/>discarded: 另一个客户端使用相同的 ClientID 连接并设置 `clean_start = true`<br/>takenover: 另一个客户端使用相同的 ClientID 连接并设置 `clean_start = false`<br/>internal\_error：畸形报文或其他未知错误<br/> |
| clientid         | 消息目的 Client ID                                           |
| username         | 消息目的用户名                                               |
| peername         | 客户端的 IPAddress 和 Port                                   |
| sockname         | emqx 监听的 IPAddress 和 Port                                |
| connected_at | 客户端连接开始时间（单位：毫秒）。该时间戳表示当前会话建立的时间，有助于识别断开事件所属的连接会话。<br />此字段可确保延迟的断开事件不会覆盖较新的连接状态。 |
| disconnected\_at | 客户端连接断开时间 (单位：毫秒)                              |
| disconn\_props   | DISCONNECT Properties (仅适用于 MQTT 5.0)                    |
| timestamp        | 事件触发时间 (单位：毫秒)                                    |
| node             | 事件触发所在节点                                             |
| client_attrs        | [客户端属性](../client-attributes/client-attributes.md) |

示例

```sql
SELECT
  clientid,
  username,
  reason,
  connected_at,
  disconnected_at,
  node
FROM
  "$events/client/disconnected"
```

输出

```json
{
  "username": "u_emqx",
  "reason": "normal",
  "node": "emqx@127.0.0.1",
  "connected_at": 1645003578036,
  "disconnected_at": 1645003578536,
  "clientid": "c_emqx"
}
```

### 连接确认事件 ("$events/client/connack")

当服务端向客户端发送CONNACK报文时触发规则，reason_code 包含各种错误原因代码。

| 字段             | 解释                                   |
| ---------------- | :------------------------------------- |
| reason\_code     | 各种原因代码                           |
| clientid         | 消息目的 Client ID                     |
| username         | 消息目的用户名                         |
| peername         | 客户端的 IPAddress 和 Port               |
| sockname         | emqx 监听的 IPAddress 和 Port          |
| proto\_name      | 协议名字                               |
| proto\_ver       | 协议版本                               |
| keepalive        | MQTT 保活间隔                          |
| clean\_start     | MQTT clean\_start                      |
| expiry\_interval | MQTT Session 过期时间                  |
| conn\_props      | CONNECT Properties (仅适用于 MQTT 5.0) |
| timestamp        | 事件触发时间 (ms)                      |
| node             | 事件触发所在节点                       |

MQTT v5.0 协议将返回码重命名为原因码，增加了一个原因码来指示更多类型的错误([Reason code and ACK - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-new-features-reason-code-and-ack))。
因此reason_code 在MQTT v3.1.1与MQTT v5.0中有很大的不同。

MQTT v3.1.1
| reason\_code                      | 描述                                            |
| --------------------------------- | ----------------------------------------------- |
| connection\_accepted              | 已接受连接                                      |
| unacceptable\_protocol\_version   | 服务器不支持客户端请求的 MQTT 协议              |
| client\_identifier\_not\_valid    | 客户端 ID 是正确的 UTF-8 字符串，但服务器不允许 |
| server\_unavaliable               | 网络连接已建立，但 MQTT 服务不可用              |
| malformed\_username\_or\_password | 用户名或密码中的数据格式错误                    |
| unauthorized\_client              | 客户端连接未授权                                |

MQTT v5.0
| reason\_code                    | 描述               |
| ------------------------------- | ------------------ |
| success                         | 连接成功           |
| unspecified\_error              | 未指定的错误       |
| malformed\_packet               | 畸形数据包         |
| protocol\_error                 | 协议错误           |
| implementation\_specific\_error | 实现特定错误       |
| unsupported\_protocol\_version  | 不支持的协议版本   |
| client\_identifier\_not\_valid  | 客户端标识符无效   |
| bad\_username\_or\_password     | 错误的用户名或密码 |
| not\_authorized                 | 未经授权           |
| server\_unavailable             | 服务器无法使用     |
| server\_busy                    | 服务器繁忙         |
| banned                          | 禁止访问           |
| bad\_authentication\_method     | 错误的身份验证方法 |
| topic\_name\_invalid            | 主题名称无效       |
| packet\_too\_large              | 数据包太大         |
| quota\_exceeded                 | 超出配额           |
| retain\_not\_supported          | 不支持的retain     |
| qos\_not\_supported             | 不支持的qos        |
| use\_another\_server            | 使用另一台服务器   |
| server\_moved                   | 服务器迁移了       |
| connection\_rate\_exceeded      | 超出连接速率       |

示例

```sql
SELECT
  clientid,
  username,
  reason_code,
  node
FROM
  "$events/client/connack"
```

输出

```json
{
  "username": "u_emqx",
  "reason_code": "success",
  "node": "emqx@127.0.0.1",
  "connected_at": 1645003578536,
  "clientid": "c_emqx"
}
```

### 鉴权完成事件 ("$events/auth/check_authz_complete")

当客户端鉴权结束时触发规则。

| 字段          | 解释                                   |
| ------------- | :------------------------------------- |
| clientid      | 消息目的 Client ID                     |
| username      | 消息目的用户名                         |
| peerhost      | 客户端的 IPAddress                     |
| topic         | MQTT 主题                              |
| action        | publish or subscribe，发布或者订阅事件 |
| result        | allow or deny，鉴权完成                |
| authz\_source | 认证源                                 |
| timestamp     | 事件触发时间 (ms)                      |
| node          | 事件触发所在节点                       |
| client_attrs        | [客户端属性](../client-attributes/client-attributes.md) |

示例

```sql
SELECT
  clientid,
  username,
  topic,
  action,
  result,
  authz_source,
  node
FROM
  "$events/auth/check_authz_complete"
```

输出

```json
{
  "username": "u_emqx",
  "topic": "t/a",
  "action": "publish",
  "result": "allow",
  "authz_source": "cache",
  "node": "emqx@127.0.0.1",
  "clientid": "c_emqx"
}
```

### 认证完成事件 ("$events/auth/check_authn_complete")

当客户端认证结束时触发规则。

| 字段          | 解释                                   |
| ------------- | :------------------------------------- |
| clientid      | 消息目的 Client ID                     |
| username      | 消息目的用户名                         |
| peername      | 客户端的 IPAddress                     |
| `reason_code`     | 认证结果                           |
| `is_superuser`    | 是否是超级用户                      |
| `is_anonymous`    | 是否是匿名用户                      |
| `client_attrs`        | [客户端属性](../client-attributes/client-attributes.md) |

示例

```sql
SELECT
  clientid,
  username,
  reason_code,
  is_superuser,
  is_anonymous
FROM
  "$events/auth/check_authn_complete"
```

输出

```json
{
  "clientid": "c_emqx",
  "username": "u_emqx",
  "reason_code": "success",
  "is_superuser": true,
  "is_anonymous": false
}
```

### 客户端订阅成功事件 ("$events/session/subscribed")

当客户端订阅成功时触发规则。

| 字段      | 解释                                |
| :-------- | :---------------------------------- |
| clientid  | 消息目的 Client ID                  |
| username  | 消息目的用户名                      |
| peerhost  | 客户端的 IPAddress                  |
| topic     | MQTT 主题                           |
| qos       | MQTT 消息的 QoS                     |
| sub_props | SUBSCRIBE Properties (仅适用于 5.0) |
| timestamp | 事件触发时间 (单位：毫秒)           |
| node      | 事件触发所在节点                    |
| client_attrs        | [客户端属性](../client-attributes/client-attributes.md) |

示例

```sql
SELECT
  clientid,
  username,
  topic,
  qos
FROM
  "$events/session/subscribed"
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

### 客户端取消订阅成功事件 ("$events/session/unsubscribed")

当客户端取消订阅成功时触发规则。

| 字段        | 解释                                  |
| :---------- | :------------------------------------ |
| clientid    | 消息目的 Client ID                    |
| username    | 消息目的用户名                        |
| peerhost    | 客户端的 IPAddress                    |
| topic       | MQTT 主题                             |
| qos         | MQTT 消息的 QoS                       |
| unsub_props | UNSUBSCRIBE Properties (仅适用于 5.0) |
| timestamp   | 事件触发时间 (单位：毫秒)             |
| node        | 事件触发所在节点                      |
| client_attrs        | [客户端属性](../client-attributes/client-attributes.md) |

示例

```sql
SELECT
  clientid,
  username,
  topic,
  qos
FROM
  "$events/session/unsubscribed"
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

### 系统告警激活事件 ("$events/sys/alarm_activated")

此事件主题可用于在 EMQX 系统告警被激活时触发规则。

例如，要从 `"$events/sys/alarm_activated"` 事件主题中提取告警名称、详细信息、描述信息以及激活时间，可以使用以下 SQL 语句：

示例

```sql
SELECT
  name,
  details,
  message,
  activated_at,
  node
FROM
  "$events/sys/alarm_activated"
```

输出

```json
{
  "name": "too_many_processes",
  "details": {
    "usage": "99%",
    "high_watermark": "80%"
  },
  "message": "99% process usage",
  "activated_at": 1645003578536000,
  "node": "emqx@127.0.0.1"
}
```

请参考下表了解可提取的字段：

| 字段           | 说明                                                         |
| -------------- | ------------------------------------------------------------ |
| `name`         | 告警的短标识符（例如：`"too_many_processes"`）               |
| `details`      | 包含告警详细信息的 JSON 对象，无固定格式（例如：`{"usage": "99%", "high_watermark": "80%"}`） |
| `message`      | 告警的描述性信息（例如：`"99% process usage"`）              |
| `activated_at` | Unix 时间戳（微秒），表示告警被激活的时间                    |
| `node`         | 触发事件的 EMQX 节点                                         |

### 系统告警解除事件 ("$events/sys/alarm_deactivated")

此事件主题可用于在 EMQX 系统告警被解除时触发规则。

例如，要从 `"$events/sys/alarm_deactivated"` 事件主题中提取告警名称、详细信息、描述信息、激活时间以及解除时间，可以使用以下 SQL 语句：

示例

```sql
SELECT
  name,
  details,
  message,
  activated_at,
  deactivated_at,
  node
FROM
  "$events/sys/alarm_deactivated"
```

输出

```json
{
  "name": "too_many_processes",
  "details": {
    "usage": "99%",
    "high_watermark": "80%"
  },
  "message": "99% process usage",
  "activated_at": 1645003578536000,
  "deactivated_at": 1645004000000000,
  "node": "emqx@127.0.0.1"
}
```

请参考下表了解可提取的字段：

| 字段             | 说明                                                         |
| ---------------- | ------------------------------------------------------------ |
| `name`           | 告警的短标识符（例如：`"too_many_processes"`）               |
| `details`        | 包含告警详细信息的 JSON 对象，无固定格式（例如：`{"usage": "99%", "high_watermark": "80%"}`） |
| `message`        | 告警的描述性信息（例如：`"99% process usage"`）              |
| `activated_at`   | Unix 时间戳（微秒），表示告警被激活的时间                    |
| `deactivated_at` | Unix 时间戳（微秒），表示告警被解除的时间                    |
| `node`           | 触发事件的 EMQX 节点                                         |

### 客户端 Keepalive（PING）事件（"$events/client/ping"）

当 EMQX 接收到已连接 MQTT 客户端发送的 `PINGREQ` 报文时，将触发该事件主题，表示客户端心跳已被成功接收。

该事件主要用于诊断和故障排查。`$events/client/disconnected` 用于说明客户端为何断开连接（例如 `keepalive_timeout`），而 `"$events/client/ping"` 事件则提供了 EMQX 实际接收到客户端心跳报文的直接证据，有助于区分客户端或网络侧问题与 Broker 侧问题。

例如，如需从 `"$events/client/ping"` 事件主题中提取客户端 ID、用户名、协商的 keepalive 间隔、事件触发时间以及触发事件的 EMQX 节点信息，可使用如下 SQL 语句：

**示例：**

```sql
SELECT
  clientid,
  username,
  keepalive,
  timestamp,
  node
FROM
  "$events/client/ping"
```

**输出：**

```json
{
  "clientid": "c_emqx",
  "username": "u_emqx",
  "keepalive": 60,
  "timestamp": 1645003800123,
  "node": "emqx@127.0.0.1"
}
```

请参考下表了解 Client PING 事件中可提取的字段。

| 字段              | 说明                               |
| ----------------- | ---------------------------------- |
| `clientid`        | 客户端 ID                          |
| `clean_start`     | MQTT clean_start 标志              |
| `username`        | 客户端用户名                       |
| `peername`        | 客户端 IP 地址和端口               |
| `sockname`        | EMQX 监听的 IP 地址和端口          |
| `proto_name`      | 协议名称                           |
| `proto_ver`       | 协议版本                           |
| `keepalive`       | 协商后的 MQTT keepalive 间隔       |
| `timestamp`       | 事件触发时间（单位：毫秒）         |
| `node`            | 触发该事件的 EMQX 节点             |
| `conn_props`      | CONNECT 属性（仅 MQTT 5.0 客户端） |
| `expiry_interval` | MQTT 会话过期时间                  |

## Source

规则使用 `$bridges/` 开头的主题来表示 Source 的消息或事件。格式为：`$bridges/<type>:<name>`。

其中 `<type>:<name>` 部分是 Source 的 ID，`<type>` 是 Source 的类型，`<name>` 是 Source 的名字。
比如 `$bridges/mqtt:my_mqtt_bridge`。

### MQTT 订阅者事件 ("$bridges/mqtt:*")

当该 MQTT 订阅者从外部 MQTT Broker 接收到消息时触发规则

| 字段                  | 解释                                                |
| :-------------------- | :-------------------------------------------------- |
| id                    | MQTT 消息 ID                                        |
| server                | 远程 MQTT Broker 的地址，例如 "broker.emqx.io:1883" |
| payload               | MQTT 消息体                                         |
| topic                 | MQTT 主题                                           |
| qos                   | MQTT 消息的 QoS                                     |
| dup                   | MQTT 消息的 DUP Flag                                |
| retain                | MQTT 消息的 Retain Flag                             |
| pub\_props            | PUBLISH Properties (仅适用于 MQTT 5.0)              |
| message\_received\_at | PUBLISH 消息到达 Broker 的时间 (单位：毫秒)         |

示例

```sql
SELECT
  *
FROM
  "$bridges/mqtt:my_mqtt_bridge"
```

输出:

```json
{
  "id": "0005E27C1D24E44FF440000017520000",
  "server": "broker.emqx.io:1883",
  "payload": "hello",
  "topic": "t/a",
  "qos": 1,
  "dup": false,
  "retain": false,
  "pub_props": {
    "Message-Expiry-Interval": 30,
    "Payload-Format-Indicator": 0,
    "User-Property": {
      "foo": "bar"
    },
    "User-Property-Pairs": [
      {
        "key": "foo"
      },
      {
        "value": "bar"
      }
    ]
  },
  "message_received_at": 1645002753259,
}
```
