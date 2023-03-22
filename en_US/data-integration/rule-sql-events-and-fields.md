# SQL data sources and fields

The data sources that SQL statements can process are **MQTT Messages**, **MQTT Events**, or **Data Bridges**.

The SQL statement uses `FROM` clause to specify the data source, and the corresponding fields can be referenced in the `SELECT` and `where` clauses.

Different data source types have different fields.

## Data Bridges

Rules use topics prefixed by `$bridges/` to present messages or events triggered by a data bridge.
The format is: `$bridges/<type>:<name>`.

Where `<type>:<name>` is the bridge Id, `<type>` is the bridge type, `<name>` is the bridge name.
Such as `$bridges/mqtt:my_mqtt_bridge`.

### MQTT Bridge Events ("$bridges/mqtt:*")

Triggered by an MQTT Bridge when a message is received from the remote MQTT broker.

| Field                 | Explanation                                                          |
|:----------------------|:---------------------------------------------------------------------|
| id                    | MQTT message ID                                                      |
| server                | Server name of the remove MQTT broker, such as "broker.emqx.io:1883" |
| payload               | MQTT payload                                                         |
| topic                 | MQTT topic                                                           |
| qos                   | MQTT QoS                                                             |
| dup                   | MQTT DUP flag                                                        |
| retain                | MQTT Retain Flag                                                     |
| pub\_props            | PUBLISH Properties (only for MQTT 5.0)                               |
| message\_received\_at | The timestamp when the message is received (ms)                      |

Example:
```sql
SELECT
  *
FROM
  "$bridges/mqtt:my_mqtt_bridge"
```

Output:
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

## MQTT Message

The SQL statement of the rules engine can handle the message publishing. In a rule statement, the user can specify one or more topics with the FROM clause, and the rule will be triggered when any message is published to the specified topic.

| Field                 | Explanation                                     |
|:----------------------|:------------------------------------------------|
| id                    | MQTT message ID                                 |
| clientid              | Client ID of the sender                         |
| username              | Username of the sender                          |
| payload               | MQTT payload                                    |
| peerhost              | Client IPAddress                                |
| topic                 | MQTT topic                                      |
| qos                   | Enumeration of message QoS 0,1,2                |
| flags                 | Flags                                           |
| headers               | Internal data related to the message processing |
| pub\_props            | The PUBLISH Properties (MQTT 5.0 only)          |
| timestamp             | Timestamp (ms)                                  |
| publish\_received\_at | Time when PUBLISH message reaches Broker (ms)   |
| node                  | Node name of the trigger event                  |

Example:
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

Output:
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

## MQTT Events
The SQL statements of the rule engine can handle both messages (message publishing) and events (client online and offline, client subscription, etc.). For messages, the FROM clause is directly followed by the topic name; for events, the FROM clause is followed by the event topic.

The topic of the event message starts with `"$events/"`, such as `"$events/client_connected",` `"$events/session_subscribed"`.
If you want emqx to publish the event message, you can configure it in the `emqx_rule_engine.conf` file.

### Event topic available for FROM clause

| Event topic name                     | Explanation                     |
|--------------------------------------|:--------------------------------|
| $events/message\_delivered           | Message delivery                |
| $events/message\_acked               | Message acknowledged            |
| $events/message\_dropped             | Message dropped when routing    |
| $events/delivery\_dropped            | Message dropped when delivering |
| $events/client\_connected            | Connection complete             |
| $events/client\_disconnected         | Disconnect                      |
| $events/client\_connack              | Connection ack                  |
| $events/client\_check_authz_complete | Authorization check complete    |
| $events/session\_subscribed          | Subscribe                       |
| $events/session\_unsubscribed        | Unsubcribe                      |


### "$events/message_delivered"

Trigger the rule when a message is put into the underlying socket


| Field               | Explanation                                   |
|:--------------------|:----------------------------------------------|
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
| pub\_props          | The PUBLISH Properties (MQTT 5.0 only)        |
| timestamp           | Event trigger time(ms)                        |
| publish_received_at | Time when PUBLISH message reaches Broker (ms) |
| node                | Node name of the trigger event                |

Example:
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
Output:
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
### "$events/message_acked"

The rule is triggered when the message is sent to the client and an ack is received from the client. Only QOS1 and QOS2 messages will be triggered.


| Field                 | Explanation                                   |
|:----------------------|:----------------------------------------------|
| id                    | MQTT message id                               |
| from\_clientid        | Client ID of the sender                       |
| from\_username        | Username of the sender                        |
| clientid              | Client ID of the receiver                     |
| username              | Username of the receiver                      |
| payload               | MQTT payload                                  |
| peerhost              | Client IPAddress                              |
| topic                 | MQTT topic                                    |
| qos                   | Enumeration of message QoS 0,1,2              |
| flags                 | Flags                                         |
| pub\_props            | The PUBLISH Properties (MQTT 5.0 only)        |
| puback\_props         | The PUBACK Properties (MQTT 5.0 only)         |
| timestamp             | Event trigger time(ms)                        |
| publish\_received\_at | Time when PUBLISH message reaches Broker (ms) |
| node                  | Node name of the trigger event                |

Example:
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

Output:
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

### "$events/message_dropped"

The rule is triggered when a message has no subscribers.

| Field                 | Explanation                                                                                                                                                                                                                           |
|:----------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| id                    | MQTT message id                                                                                                                                                                                                                       |
| reason                | Reasons of dropping, possible reasons: <br/>no\_subscribers: no clients subscribes the topic<br/>receive\_maximum\_exceeded: awaiting\_rel queue is full<br/>packet\_identifier\_inuse: send a qos2 message with unreleased packet ID |
| clientid              | Client ID of the sender                                                                                                                                                                                                               |
| username              | Username of the sender                                                                                                                                                                                                                |
| payload               | MQTT payload                                                                                                                                                                                                                          |
| peerhost              | Client IPAddress                                                                                                                                                                                                                      |
| topic                 | MQTT topic                                                                                                                                                                                                                            |
| qos                   | Enumeration of message QoS 0,1,2                                                                                                                                                                                                      |
| flags                 | Flags                                                                                                                                                                                                                                 |
| pub\_props            | The PUBLISH Properties (MQTT 5.0 only)                                                                                                                                                                                                |
| timestamp             | Event trigger time(ms)                                                                                                                                                                                                                |
| publish\_received\_at | Time when PUBLISH message reaches Broker (ms)                                                                                                                                                                                         |
| node                  | Node name of the trigger event                                                                                                                                                                                                        |

Example:
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
Output:
```json
{
  "topic": "t/a",
  "timestamp": 1645003103004,
  "reason": "no_subscribers",
  "qos": 1,
  "node": "emqx@127.0.0.1"
}
```

### "$events/delivery_dropped"

The rule is triggered when subscriber's message queue is full.


| Field                 | Explanation                                                                                                                                                                                                                                                                             |
|:----------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| id                    | MQTT message id                                                                                                                                                                                                                                                                         |
| reason                | Reason of dropping, possible reasons: <br/>queue\_full: the message queue is full(QoS>0)<br/>no\_local: it's not allowed for the client to received messages published by themselves<br/>expired: the message or the session is expired<br/>qos0\_msg: the message queue is full(QoS 0) |
| from\_clientid        | Client ID of the sender                                                                                                                                                                                                                                                                 |
| from\_username        | Username of the sender                                                                                                                                                                                                                                                                  |
| clientid              | Client ID of the receiver                                                                                                                                                                                                                                                               |
| username              | Username of the receiver                                                                                                                                                                                                                                                                |
| payload               | MQTT payload                                                                                                                                                                                                                                                                            |
| peerhost              | Client IPAddress                                                                                                                                                                                                                                                                        |
| topic                 | MQTT topic                                                                                                                                                                                                                                                                              |
| qos                   | Enumeration of message QoS 0,1,2                                                                                                                                                                                                                                                        |
| flags                 | Flags                                                                                                                                                                                                                                                                                   |
| pub\_props            | The PUBLISH Properties (MQTT 5.0 only)                                                                                                                                                                                                                                                  |
| timestamp             | Event trigger time(ms)                                                                                                                                                                                                                                                                  |
| publish\_received\_at | Time when PUBLISH message reaches Broker (ms)                                                                                                                                                                                                                                           |
| node                  | Node name of the trigger event                                                                                                                                                                                                                                                          |

Example:
```sql
SELECT
  from_clientid,
  from_username,
  reason,
  topic,
  qos
FROM "$events/delivery_dropped"
```
Output:
```json
{
  "topic": "t/a",
  "reason": "queue_full",
  "qos": 1,
  "from_username": "u_emqx_1",
  "from_clientid": "c_emqx_1"
}
```
### "$events/client_connected"

The rule is triggered when the terminal is connected successfully.

| Field            | Explanation                              |
|:-----------------|:-----------------------------------------|
| clientid         | Client ID                                |
| username         | Current MQTT username                    |
| mountpoint       | Mountpoint for bridging messages         |
| peername         | IPAddress and Port of terminal           |
| sockname         | IPAddress and Port listened by emqx      |
| proto\_name      | protocol name                            |
| proto\_ver       | protocol version                         |
| keepalive        | MQTT keepalive interval                  |
| clean\_start     | MQTT clean\_start                        |
| expiry\_interval | MQTT Session Expiration time             |
| is\_bridge       | whether it is MQTT bridge connection     |
| connected\_at    | Terminal connection completion time (ms) |
| conn_props       | The CONNECT Properties (MQTT 5.0 only)   |
| timestamp        | Event trigger time(ms)                   |
| node             | Node name of the trigger event           |

Example:
```sql
SELECT
  clientid,
  username,
  keepalive,
  is_bridge
FROM
  "$events/client_connected"
```
Output:
```json
{
  "username": "u_emqx",
  "keepalive": 60,
  "is_bridge": false,
  "clientid": "c_emqx"
}
```

### "$events/client_disconnected"

The rule is triggered when terminal connection is lost.

| Field            | Explanation                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|:-----------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| reason           | Reason for disconnection of terminal<br/>normal：the client is actively disconnected <br/>kicked：the server kicks out, and it is kicked out through REST API<br/>keepalive_timeout: keepalive timeout<br/>not_authorized: auth failed, or `acl_nomatch = disconnect`, Pub/Sub without permission will disconnect the client<br/>tcp_closed: the peer has closed the network connection<br/>discarded: another client connected with the same ClientID and set `clean_start = true`<br/>takenover: another client connected with the same ClientID and set `clean_start = false`<br/>internal_error: malformed message or other unknown errors<br/> |
| clientid         | Client ID                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| username         | Current MQTT username                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| peername         | IPAddress and Port of terminal                                                                                                                                                                                                                                                                                                                                                                                                                             |
| sockname         | IPAddress and Port listened by emqx                                                                                                                                                                                                                                                                                                                                                                                                                        |
| disconnected\_at | Terminal disconnection completion time (ms)                                                                                                                                                                                                                                                                                                                                                                                                                |
| disconn_props    | The DISCONNECT Properties (MQTT 5.0 only)                                                                                                                                                                                                                                                                                                                                                                                                                  |
| timestamp        | Event trigger time(ms)                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| node             | Node name of the trigger event                                                                                                                                                                                                                                                                                                                                                                                                                             |

Example:
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
Output:
```json
{
  "username": "u_emqx",
  "reason": "normal",
  "node": "emqx@127.0.0.1",
  "disconnected_at": 1645003578536,
  "clientid": "c_emqx"
}
```

### "$events/client_connack"

The rule is triggered when the server sends a CONNACK packet to the client. reason_code contains the error reason code.

| Field            | Explanation                            |
|------------------|:---------------------------------------|
| reason_code      | Reason code                            |
| clientid         | Client ID of the sender                |
| username         | Username of the sender                 |
| peername         | IPAddress and Port of terminal         |
| sockname         | IPAddress and Port listened by emqx    |
| proto\_name      | Protocol name                          |
| proto\_ver       | Protocol version                       |
| keepalive        | MQTT keepalive interval                |
| clean\_start     | MQTT clean\_start                      |
| expiry\_interval | MQTT Session Expiration time           |
| conn\_props      | The CONNECT Properties (MQTT 5.0 only) |
| timestamp        | Event trigger time(ms)                 |
| node             | Node name of the trigger event         |


The MQTT v5.0 protocol renames the return code to a reason code, adding a reason code to indicate more types of errors([Reason code and ACK - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-new-features-reason-code-and-ack)).

MQTT v3.1.1
| reason\_code                      | description                                                                 |
|-----------------------------------|-----------------------------------------------------------------------------|
| connection\_accepted              | Connection accepted                                                         |
| unacceptable\_protocol\_version   | The server does not support the MQTT protocol requested by the client       |
| client\_identifier\_not\_valid    | The client ID is the correct UTF-8 string, but is not allowed by the server |
| server\_unavaliable               | Network connection has been established, but MQTT service is unavailable    |
| malformed\_username\_or\_password | The data in the username or password is in the wrong format                 |
| unauthorized\_client              | Client connection is not authorized                                         |

MQTT v5.0
| reason\_code                    | description                   |
|---------------------------------|-------------------------------|
| success                         | Connect success               |
| unspecified\_error              | Unspecified error             |
| malformed\_packet               | Malformed Packet              |
| protocol\_error                 | Protocol Error                |
| implementation\_specific\_error | Implementation specific error |
| unsupported\_protocol\_version  | Unsupported Protocol Version  |
| client\_identifier\_not\_valid  | Client Identifier not valid   |
| bad\_username\_or\_password     | Bad User Name or Password     |
| not\_authorized                 | Not authorized                |
| server\_unavailable             | Server unavailable            |
| server\_busy                    | Server busy                   |
| banned                          | Banned                        |
| bad\_authentication\_method     | Bad authentication method     |
| topic\_name\_invalid            | Topic Name invalid            |
| packet\_too\_large              | Packet too large              |
| quota\_exceeded                 | Quota exceeded                |
| retain\_not\_supported          | Retain not supported          |
| qos\_not\_supported             | QoS not supported             |
| use\_another\_server            | Use another server            |
| server\_moved                   | Server moved                  |
| connection\_rate\_exceeded      | Connection rate exceeded      |

Example:
```sql
SELECT
  clientid,
  username,
  reason,
  node
FROM
  "$events/client_connack"
```
Output:
```json
{
  "username": "u_emqx",
  "reason": "success",
  "node": "emqx@127.0.0.1",
  "connected_at": 1645003578536,
  "clientid": "c_emqx"
}
```
### "$events/client_check_authz_complete"

The rule is triggered when the client check acl complete.

| Field     | Explanation                                                                                                                                  |
|-----------|:---------------------------------------------------------------------------------------------------------------------------------------------|
| clientid  | Client ID of the sender                                                                                                                      |
| username  | Username of the sender                                                                                                                       |
| peerhost  | Client IPAddress                                                                                                                             |
| topic     | MQTT topic                                                                                                                                   |
| action    | publish or subscribe                                                                                                                         |
| result    | allow or deny, acl check result                                                                                                              |
| is\_cache | true or false <br/>When is\_cache is true, the acl data comes from the cache <br/>When is\_cache is false, the acl data comes from the plugs |
| timestamp | Timestamp (ms)                                                                                                                               |
| node      | Node name of the trigger event                                                                                                               |

Example:
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
  "$events/client_check_authz_complete"
```
Output:
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

### "$events/session_subscribed"

The rule is triggered when the terminal subscribes successfully.

| Field     | Explanation                              |
|:----------|:-----------------------------------------|
| clientid  | Client ID                                |
| username  | Current MQTT username                    |
| peerhost  | Client IPAddress                         |
| topic     | MQTT topic                               |
| qos       | Enumeration of message QoS 0,1,2         |
| sub_props | The SUBSCRIBE Properties (MQTT 5.0 only) |
| timestamp | Event trigger time(ms)                   |
| node      | Node name of the trigger event           |

Example:
```sql
SELECT
  clientid,
  username,
  topic,
  qos
FROM
  "$events/session_subscribed"
```
Output:
```json
{
  "username": "u_emqx",
  "topic": "t/a",
  "qos": 1,
  "clientid": "c_emqx"
}
```

### "$events/session_unsubscribed"

The rule is triggered when the terminal subscription is cancelled successfully.

| Field       | Explanation                                |
|:------------|:-------------------------------------------|
| clientid    | Client ID                                  |
| username    | Current MQTT username                      |
| peerhost    | Client IPAddress                           |
| topic       | MQTT topic                                 |
| qos         | Enumeration of message QoS 0,1,2           |
| unsub_props | The UNSUBSCRIBE Properties (MQTT 5.0 only) |
| timestamp   | Event trigger time(ms)                     |
| node        | Node name of the trigger event             |


Example:
```sql
SELECT
  clientid,
  username,
  topic,
  qos
FROM
  "$events/session_unsubscribed"
```
Output:
```json
{
  "username": "u_emqx",
  "topic": "t/a",
  "qos": 1,
  "clientid": "c_emqx"
}
```
