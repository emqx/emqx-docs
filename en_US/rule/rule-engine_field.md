# Available fields in rule engine SQL statements
The fields available in the SELECT and WHERE clauses are related to the type of event. Among them, `clientid`, `username` and ` event` are common fields that is contained by each type of event.

## Events and event topics 
The SQL statements of the rule engine can handle both messages (message publishing) and events (client online and offline, client subscription, etc.). For messages, the FROM clause is directly followed by the topic name; for events, the FROM clause is followed by the event topic.

The topic of the event message starts with `"$events/"`, such as `"$events/client_connected",` `"$events/session_subscribed"`.
If you want emqx to publish the event message, you can configure it in the `emqx_rule_engine.conf` file.

### Event topic available for FROM clause

| Event topic name              | Explanation          |
| ----------------------------- | :------------------- |
| $events/message\_delivered    | message delivery     |
| $events/message\_acked        | message acknowledged |
| $events/message\_dropped      | Message dropped      |
| $events/client\_connected     | Connection complete  |
| $events/client\_disconnected  | Disconnect           |
| $events/session\_subscribed   | Subscribe            |
| $events/session\_unsubscribed | Unsubcribe           |


## Message Publish

Trigger the rule when a message is published on the topic specified by FROM

| event               | Event type, fixed at "message.publish"                    |
| :------------------ | :-------------------------------------------------------- |
| id                  | MQTT message ID                                           |
| clientid            | Client ID of the sender                                   |
| username            | Username of the sender                                    |
| payload             | MQTT payload                                              |
| peerhost            | Client IPAddress                                          |
| topic               | MQTT topic                                                |
| qos                 | Enumeration of message QoS 0,1,2                          |
| flags               | Flags                                                     |
| headers             | Internal data related to the message processing           |
| pub_props           | The PUBLISH Properties (MQTT 5.0 only)                    |
| timestamp           | Timestamp (ms)                                            |
| publish_received_at | Time when PUBLISH message reaches Broker (ms)             |
| node                | Node name of the trigger event                            |

example
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

output
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

## $events/message_delivered

Trigger the rule when a message is put into the underlying socket

| event               | Event type, fixed at "message.delivered"      |
| ------------------- | --------------------------------------------- |
| id                  | MQTT message ID                               |
| from\_clientid      | Client ID of the sender                       |
| from\_username      | Username of the sender                        |
| clientid            | Client ID of the receiver                     |
| username            | Username of the receiver                      |
| payload             | MQTT payload                                  |
| peerhost            | client IPAddress                              |
| topic               | MQTT topic                                    |
| qos                 | Enumeration of message QoS 0,1,2              |
| flags               | flags                                         |
| pub_props           | The PUBLISH Properties (MQTT 5.0 only)        |
| timestamp           | Event trigger time(millisecond)               |
| publish_received_at | Time when PUBLISH message reaches Broker (ms) |
| node                | Node name of the trigger event                |

example
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
output
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
## $events/message_acked

The rule is triggered when the message is sent to the client and an ack is received from the client. Only QOS1 and QOS2 messages will be triggered

| event               | Event type, fixed at "message.acked"          |
| :------------------ | :-------------------------------------------- |
| id                  | MQTT message id                               |
| from\_clientid      | Client ID of the sender                       |
| from\_username      | Username of the sender                        |
| clientid            | Client ID of the receiver                     |
| username            | Username of the receiver                      |
| payload             | MQTT payload                                  |
| peerhost            | client IPAddress                              |
| topic               | MQTT topic                                    |
| qos                 | Enumeration of message QoS 0,1,2              |
| flags               | flags                                         |
| pub_props           | The PUBLISH Properties (MQTT 5.0 only)        |
| puback_props        | The PUBACK Properties (MQTT 5.0 only)         |
| timestamp           | Event trigger time(millisecond)               |
| publish_received_at | Time when PUBLISH message reaches Broker (ms) |
| node                | Node name of the trigger event                |

example
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

output
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

## $events/message_dropped

Trigger rule when a message has no subscribers

| event               | Event type, fixed at "message.dropped"        |
| :------------------ | :-------------------------------------------- |
| id                  | MQTT message id                               |
| reason              | reason for dropping, possible reasons: <br/>no_subscribers: no clients subscribes the topic|
| clientid            | Client ID of the sender                       |
| username            | Username of the sender                        |
| payload             | MQTT payload                                  |
| peerhost            | Client IPAddress                              |
| topic               | MQTT topic                                    |
| qos                 | Enumeration of message QoS 0,1,2              |
| flags               | flags                                         |
| pub_props           | The PUBLISH Properties (MQTT 5.0 only)        |
| timestamp           | Event trigger time(millisecond)               |
| publish_received_at | Time when PUBLISH message reaches Broker (ms) |
| node                | Node name of the trigger event                |

example
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
output
```json
{
  "topic": "t/a",
  "timestamp": 1645003103004,
  "reason": "no_subscribers",
  "qos": 1,
  "node": "emqx@127.0.0.1"
}
```

## $events/delivery_dropped

Trigger rule when subscriber's message queue is full


|        event        | Event type, fixed at "delivery.dropped" |
| ------------------- | ------------------------------------ |
| id                  | MQTT message id                               |
| reason              | reason for dropping, possible reasons: <br/>queue_full: the message queue is full(QoS>0)<br/>no_local: it's not allowed for the client to received messages published by themselves<br/>expired: the message or the session is expired<br/>qos0_msg: the message queue is full(QoS0)|
| from\_clientid      | Client ID of the sender                       |
| from\_username      | Username of the sender                        |
| clientid            | Client ID of the receiver                     |
| username            | Username of the receiver                      |
| payload             | MQTT payload                                  |
| peerhost            | client IPAddress                              |
| topic               | MQTT topic                                    |
| qos                 | Enumeration of message QoS 0,1,2              |
| flags               | flags                                         |
| pub_props           | The PUBLISH Properties (MQTT 5.0 only)        |
| timestamp           | Event trigger time(millisecond)               |
| publish_received_at | Time when PUBLISH message reaches Broker (ms) |
| node                | Node name of the trigger event                |

example
```sql
SELECT
  from_clientid,
  from_username,
  reason,
  topic,
  qos
FROM "$events/delivery_dropped"
```
output
```json
{
  "topic": "t/a",
  "reason": "queue_full",
  "qos": 1,
  "from_username": "u_emqx_1",
  "from_clientid": "c_emqx_1"
}
```
## $events/client_connected

Trigger the rule when the terminal is connected successfully

| event            | Event type, fixed at "client.connected" |
| ---------------- | :-------------------------------------- |
| clientid         | clientid                                |
| username         | Current MQTT username                   |
| mountpoint       | Mountpoint for bridging messages        |
| peername         | IPAddress and Port of terminal          |
| sockname         | IPAddress and Port listened by emqx     |
| proto\_name      | protocol name                           |
| proto\_ver       | protocol version                        |
| keepalive        | MQTT keepalive interval                 |
| clean\_start     | MQTT clean\_start                       |
| expiry\_interval | MQTT Session Expiration time            |
| is\_bridge       | whether it is MQTT bridge connection    |
| connected\_at    | Terminal connection completion time (s) |
| conn_props       | The CONNECT Properties (MQTT 5.0 only)  |
| timestamp        | Event trigger time(millisecond)         |
| node             | Node name of the trigger event          |

example
```sql
SELECT
  clientid,
  username,
  keepalive,
  is_bridge
FROM
  "$events/client_connected"
```
output
```json
{
  "username": "u_emqx",
  "keepalive": 60,
  "is_bridge": false,
  "clientid": "c_emqx"
}
```

## $events/client_disconnected

Trigger rule when terminal connection is lost

| event            | Event type, fixed at "client.disconnected"                   |
| ---------------- | :----------------------------------------------------------- |
| reason           | Reason for disconnection of terminal<br/>normal：the client is actively disconnected <br/>kicked：the server kicks out, and it is kicked out through REST API<br/>keepalive_timeout: keepalive timeout<br/>not_authorized: auth failed，or `acl_nomatch = disconnect`, Pub/Sub without permission will disconnect the client<br/>tcp_closed: the peer has closed the network connection<br/>internal_error: malformed message or other unknown errors<br/> |
| clientid         | client ID                                                    |
| username         | Current MQTT username                                        |
| peername         | IPAddress and Port of terminal                               |
| sockname         | IPAddress and Port listened by emqx                          |
| disconnected\_at | Terminal disconnection completion time (s)                   |
| disconn_props    | The DISCONNECT Properties (MQTT 5.0 only)                    |
| timestamp        | Event trigger time(millisecond)                              |
| node             | Node name of the trigger event                               |

example
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
output
```json
{
  "username": "u_emqx",
  "reason": "normal",
  "node": "emqx@127.0.0.1",
  "disconnected_at": 1645003578536,
  "clientid": "c_emqx"
}
```

## $events/session_subscribed

Trigger the rule when the terminal subscribes successfully

| event     | Event type, fixed at "session.subscribed" |
| --------- | ----------------------------------------- |
| clientid  | Client ID                                 |
| username  | Current MQTT username                     |
| peerhost  | client IPAddress                          |
| topic     | MQTT topic                                |
| qos       | Enumeration of message QoS 0,1,2          |
| sub_props | The SUBSCRIBE Properties (MQTT 5.0 only)  |
| timestamp | Event trigger time(millisecond)           |
| node      | Node name of the trigger event            |

example
```sql
SELECT
  clientid,
  username,
  topic,
  qos
FROM
  "$events/session_subscribed"
```
output
```json
{
  "username": "u_emqx",
  "topic": "t/a",
  "qos": 1,
  "clientid": "c_emqx"
}
```

## $events/session_unsubscribed

Triggered when the terminal subscription is cancelled successfully

| event     | Event type, fixed at "session.unsubscribed" |
| :-------- | :------------------------------------------ |
| clientid  | Client ID                                   |
| username  | Current MQTT username                       |
| peerhost  | client IPAddress                            |
| topic     | MQTT topic                                  |
| qos       | Enumeration of message QoS 0,1,2            |
| unsub_props | The UNSUBSCRIBE Properties (MQTT 5.0 only)  |
| timestamp | Event trigger time(millisecond)             |
| node      | Node name of the trigger event              |


example
```sql
SELECT
  clientid,
  username,
  topic,
  qos
FROM
  "$events/session_unsubscribed"
```
output
```json
{
  "username": "u_emqx",
  "topic": "t/a",
  "qos": 1,
  "clientid": "c_emqx"
}
```

[Rule engine buildin functions](rule-engine_buildin_function.md)