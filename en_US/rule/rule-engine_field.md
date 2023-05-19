# Available fields in rule engine SQL statements
The fields available in the SELECT and WHERE clauses are related to the type of event. Among them, `clientid`, `username` and ` event` are common fields that is contained by each type of event.

## Use Rule Engine SQL to Handle Message Publishing
The SQL statement of the rules engine can handle the message publishing. In a rule statement, the user can specify one or more topics with the FROM clause, and the rule will be triggered when any message is published to the specified topic.

| Field               | Explanation                                     |
| :------------------ | :---------------------------------------------- |
| id                  | MQTT message ID                                 |
| clientid            | Client ID of the sender                         |
| username            | Username of the sender                          |
| payload             | MQTT payload                                    |
| peerhost            | Client IPAddress                                |
| topic               | MQTT topic                                      |
| qos                 | Enumeration of message QoS 0,1,2                |
| flags               | Flags                                           |
| headers             | Internal data related to the message processing |
| pub_props           | The PUBLISH Properties (MQTT 5.0 only)          |
| timestamp           | Timestamp (ms)                                  |
| publish_received_at | Time when PUBLISH message reaches Broker (ms)   |
| node                | Node name of the trigger event                  |

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

## Use Rule Engine SQL to Handle Events
The SQL statements of the rule engine can handle both messages (message publishing) and events (client online and offline, client subscription, etc.). For messages, the FROM clause is directly followed by the topic name; for events, the FROM clause is followed by the event topic.

The topic of the event message starts with `"$events/"`, such as `"$events/client_connected",` `"$events/session_subscribed"`.
If you want emqx to publish the event message, you can configure it in the `emqx_rule_engine.conf` file.

### Event topic available for FROM clause

| Event topic name                   | Explanation                     |
| ---------------------------------- | :------------------------------ |
| $events/message\_delivered         | Message delivery                |
| $events/message\_acked             | Message acknowledged            |
| $events/message\_dropped           | Message dropped when routing    |
| $events/delivery\_dropped          | Message dropped when delivering |
| $events/client\_connected          | Connection complete             |
| $events/client\_disconnected       | Disconnect                      |
| $events/client\_connack            | Connection ack                  |
| $events/client\_check_acl_complete | ACL check complete              |
| $events/session\_subscribed        | Subscribe                       |
| $events/session\_unsubscribed      | Unsubcribe                      |

### $events/message_delivered

Trigger the rule when a message is put into the underlying socket


| Field               | Explanation                                   |
| :------------------ | :-------------------------------------------- |
| id                  | MQTT message ID                               |
| from\_clientid      | Client ID of the sender                       |
| from\_username      | Username of the sender                        |
| clientid            | Client ID of the receiver                     |
| username            | Username of the receiver                      |
| payload             | MQTT payload                                  |
| peerhost            | Client IPAddress                              |
| topic               | MQTT topic                                    |
| qos                 | Enumeration of message QoS 0,1,2              |
| flags               | Flags                                         |
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
### $events/message_acked

The rule is triggered when the message is sent to the client and an ack is received from the client. Only QOS1 and QOS2 messages will be triggered


| Field               | Explanation                                   |
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
| flags               | Flags                                         |
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

### $events/message_dropped

Trigger rule when a message has no subscribers

| Field               | Explanation                                                                                                                                                                                                                           |
| :------------------ | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| id                  | MQTT message id                                                                                                                                                                                                                       |
| reason              | Reasons of dropping, possible reasons: <br/>no\_subscribers: no clients subscribes the topic<br/>receive\_maximum\_exceeded: awaiting\_rel queue is full<br/>packet\_identifier\_inuse: send a qos2 message with unreleased packet ID |
| clientid            | Client ID of the sender                                                                                                                                                                                                               |
| username            | Username of the sender                                                                                                                                                                                                                |
| payload             | MQTT payload                                                                                                                                                                                                                          |
| peerhost            | Client IPAddress                                                                                                                                                                                                                      |
| topic               | MQTT topic                                                                                                                                                                                                                            |
| qos                 | Enumeration of message QoS 0,1,2                                                                                                                                                                                                      |
| flags               | Flags                                                                                                                                                                                                                                 |
| pub_props           | The PUBLISH Properties (MQTT 5.0 only)                                                                                                                                                                                                |
| timestamp           | Event trigger time(millisecond)                                                                                                                                                                                                       |
| publish_received_at | Time when PUBLISH message reaches Broker (ms)                                                                                                                                                                                         |
| node                | Node name of the trigger event                                                                                                                                                                                                        |

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

### $events/delivery_dropped

Trigger rule when subscriber's message queue is full

| Field               | Explanation                                                                                                                                                                                                                                                                          |
| :------------------ | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| id                  | MQTT message ID                                                                                                                                                                                                                                                                      |
| reason              | Reasons of dropping, possible reasons: <br/>queue_full: the message queue is full(QoS>0)<br/>no_local: it's not allowed for the client to received messages published by themselves<br/>expired: the message or the session is expired<br/>qos0_msg: the message queue is full(QoS0) |
| from\_clientid      | Client ID of the sender                                                                                                                                                                                                                                                              |
| from\_username      | Username of the sender                                                                                                                                                                                                                                                               |
| clientid            | Client ID of the receiver                                                                                                                                                                                                                                                            |
| username            | Username of the receiver                                                                                                                                                                                                                                                             |
| payload             | MQTT payload                                                                                                                                                                                                                                                                         |
| peerhost            | Client IPAddress                                                                                                                                                                                                                                                                     |
| topic               | MQTT topic                                                                                                                                                                                                                                                                           |
| qos                 | Enumeration of message QoS 0,1,2                                                                                                                                                                                                                                                     |
| flags               | Flags                                                                                                                                                                                                                                                                                |
| pub_props           | The PUBLISH Properties (MQTT 5.0 only)                                                                                                                                                                                                                                               |
| timestamp           | Event trigger time(millisecond)                                                                                                                                                                                                                                                      |
| publish_received_at | Time when PUBLISH message reaches Broker (ms)                                                                                                                                                                                                                                        |
| node                | Node name of the trigger event                                                                                                                                                                                                                                                       |

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
### $events/client_connected

Trigger the rule when the terminal is connected successfully

| Field            | Explanation                             |
| :--------------- | :-------------------------------------- |
| clientid         | Client ID                               |
| username         | Current MQTT username                   |
| mountpoint       | Mountpoint for bridging messages        |
| peername         | IPAddress and Port of terminal          |
| sockname         | IPAddress and Port listened by emqx     |
| proto\_name      | Protocol name                           |
| proto\_ver       | Protocol version                        |
| keepalive        | MQTT keepalive interval                 |
| clean\_start     | MQTT clean\_start                       |
| expiry\_interval | MQTT Session Expiration time            |
| is\_bridge       | Whether it is MQTT bridge connection    |
| connected\_at    | Client connected timestamp (millisecond) |
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

| Field            | Explanation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| :--------------- | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| reason           | Reasons of disconnection of terminal<br/>normal：the client is actively disconnected <br/>kicked：the server kicks out, and it is kicked out through REST API<br/>keepalive_timeout: keepalive timeout<br/>not_authorized: auth failed，or `acl_nomatch = disconnect`, Pub/Sub without permission will disconnect the client<br/>tcp_closed: the peer has closed the network connection<br/>discarded: another client connected with the same ClientID and set `clean_start = true`<br/>takeovered: another client connected with the same ClientID and set `clean_start = false`<br/>internal_error: malformed message or other unknown errors<br/> |
| clientid         | Client ID                                                    |
| username         | Current MQTT username                                        |
| peername         | IPAddress and Port of terminal                               |
| sockname         | IPAddress and Port listened by emqx                          |
| disconnected\_at | Client disconnected timestamp (millisecond)                  |
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

## $events/client_connack

The rule event is triggered when the server sends a CONNACK packet to the client. reason_code contains the error reason code.

| Field           | Explanation                            |
| --------------- | :------------------------------------- |
| reason_code     | Reason code                            |
| clientid        | Client ID of the sender                |
| username        | Username of the sender                 |
| peername        | IPAddress and Port of terminal         |
| sockname        | IPAddress and Port listened by emqx    |
| proto_name      | protocol name                          |
| proto_ver       | protocol version                       |
| keepalive       | MQTT keepalive interval                |
| clean_start     | MQTT clean_start                       |
| expiry_interval | MQTT Session Expiration time           |
| conn_props      | The CONNECT Properties (MQTT 5.0 only) |
| timestamp       | Event trigger time(millisecond)        |
| node            | Node name of the trigger event         |


The MQTT v5.0 protocol renames the return code to a reason code, adding a reason code to indicate more types of errors([Reason code and ACK - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-new-features-reason-code-and-ack)).

MQTT v3.1.1
| reason_code                    | description                                                                 |
| ------------------------------ | --------------------------------------------------------------------------- |
| connection_accepted            | Connection accepted                                                         |
| unacceptable_protocol_version  | The server does not support the MQTT protocol requested by the client       |
| client_identifier_not_valid    | The client ID is the correct UTF-8 string, but is not allowed by the server |
| server_unavaliable             | Network connection has been established, but MQTT service is unavailable    |
| malformed_username_or_password | The data in the username or password is in the wrong format                 |
| unauthorized_client            | Client connection is not authorized                                         |

MQTT v5.0
| reason_code                   | description                   |
| ----------------------------- | ----------------------------- |
| success                       | connect success               |
| unspecified_error             | Unspecified error             |
| malformed_packet              | Malformed Packet              |
| protocol_error                | Protocol Error                |
| implementation_specific_error | Implementation specific error |
| unsupported_protocol_version  | Unsupported Protocol Version  |
| client_identifier_not_valid   | Client Identifier not valid   |
| bad_username_or_password      | Bad User Name or Password     |
| not_authorized                | Not authorized                |
| server_unavailable            | Server unavailable            |
| server_busy                   | Server busy                   |
| banned                        | Banned                        |
| bad_authentication_method     | Bad authentication method     |
| topic_name_invalid            | Topic Name invalid            |
| packet_too_large              | Packet too large              |
| quota_exceeded                | Quota exceeded                |
| retain_not_supported          | Retain not supported          |
| qos_not_supported             | QoS not supported             |
| use_another_server            | Use another server            |
| server_moved                  | Server moved                  |
| connection_rate_exceeded      | Connection rate exceeded      |

example
```sql
SELECT
  clientid,
  username,
  reason_code,
  node
FROM
  "$events/client_connack"
```
output
```json
{
  "username": "u_emqx",
  "reason_code": "success",
  "node": "emqx@127.0.0.1",
  "connected_at": 1645003578536,
  "clientid": "c_emqx"
}
```
## $events/client_check_acl_complete

The rule event is triggered when the client check acl complete.

| Field     | Explanation                                                                                                                                |
| --------- | :----------------------------------------------------------------------------------------------------------------------------------------- |
| clientid  | Client ID of the sender                                                                                                                    |
| username  | Username of the sender                                                                                                                     |
| peerhost  | Client IPAddress                                                                                                                           |
| topic     | MQTT topic                                                                                                                                 |
| action    | publish or subscribe                                                                                                                       |
| result    | allow or deny, acl check result                                                                                                            |
| is_cache  | true or false <br/>When is_cache is true, the acl data comes from the cache <br/>When is_cache is false, the acl data comes from the plugs |
| timestamp | Timestamp (ms)                                                                                                                             |
| node      | Node name of the trigger event                                                                                                             |

example
```sql
SELECT
  clientid,
  username,
  topic,
  action,
  result,
  is_cache,
  node
FROM
  "$events/client_check_acl_complete"
```
output
```json
{
  "username": "u_emqx",
  "topic": "t/a",
  "action": "publish",
  "result": "allow",
  "is_cache": "false",
  "node": "emqx@127.0.0.1",
  "clientid": "c_emqx"
}
```

## $events/session_subscribed

Trigger the rule when the terminal subscribes successfully

| Field     | Explanation                              |
| :-------- | :--------------------------------------- |
| clientid  | Client ID                                |
| username  | Current MQTT username                    |
| peerhost  | Client IPAddress                         |
| topic     | MQTT topic                               |
| qos       | Enumeration of message QoS 0,1,2         |
| sub_props | The SUBSCRIBE Properties (MQTT 5.0 only) |
| timestamp | Event trigger time(millisecond)          |
| node      | Node name of the trigger event           |

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

| Field        | Explanation                                |
| :----------- | :----------------------------------------- |
| clientid     | Client ID                                  |
| username     | Current MQTT username                      |
| peerhost     | Client IPAddress                           |
| topic        | MQTT topic                                 |
| qos          | Enumeration of message QoS 0,1,2           |
| unsub\_props | The UNSUBSCRIBE Properties (MQTT 5.0 only) |
| timestamp    | Event trigger time(millisecond)            |
| node         | Node name of the trigger event             |


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

[Rule engine buildin functions](./rule-engine_buildin_function.md)
