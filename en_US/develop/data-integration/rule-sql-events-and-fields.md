# Data Sources and Fields

Rules in EMQX can process data from various data sources, including **MQTT Messages**, **MQTT Events**, or **Data Bridges**.

As discussed in the [Rule Engine Syntax](./rule-sql-syntax.md) section, you can use the `FROM` clause to specify the data source and the corresponding fields can be referenced in the `SELECT` and `where` clauses. This section will introduce the fields for [MQTT Messages](#mqtt-message), [MQTT Events](#mqtt-events), and [Data Bridges](#data-bridges).

## MQTT Message

You can use EMQX rules to handle message publishing, in this case, you need to specify the message topics with the `FROM` clause.

For example, in the following statement, you will select fields `payload.msg` (then rename as msg with the `AS` clause), `clientid`, `username`, `payload`, `topic`, and `qos` for any message published to topics following the pattern `t/#`.

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

Starting from EMQX 6.0.3, when namespaces are enabled and `rule_engine.limit_selects_in_namespace` is set to `true`, a rule that belongs to a namespace is triggered only by messages published by clients in that same namespace. This setting is enabled by default.

Refer to the table below for fields that can be selected from the received MQTT messages: <!--need tech review @WIVWIV-->


| Field                 | Explanation                                         |
| :-------------------- | :-------------------------------------------------- |
| `id`                  | MQTT message ID                                     |
| `clientid`            | Client ID of the publisher                          |
| `username`            | Username of the publisher                           |
| `payload`             | MQTT payload                                        |
| `peerhost`            | Client IP Address                                   |
| `topic`               | MQTT topic                                          |
| `qos`                 | QoS level                                           |
| `flags`               | Flags <!--do we need more explanation?-->           |
| `headers`             | Internal data related to the message processing     |
| `pub_props`           | PUBLISH Properties (MQTT 5.0 clients only)          |
| `timestamp`           | Timestamp (unit: ms)                                |
| `publish_received_at` | Time when PUBLISH message reaches EMQX (unit: ms)   |
| `node`                | Node where the event is triggered<!--tech review--> |
| `client_attrs`        | [Client attributes](../client-attributes/client-attributes.md) |

## MQTT Events

You can use EMQX rules to extract data from event topics to get event notifications, for example, client online and offline, client subscriptions, etc. The event topic starts with `"$events/"`, such as `"$events/client/connected"`, which can be specified in the `FROM` clause of the rule.

Starting from EMQX 6.0.3, when namespaces are enabled and `rule_engine.limit_selects_in_namespace` is set to `true`, a rule that belongs to a namespace is triggered only by client-related events from clients in that same namespace. System alarm events are not associated with any client namespace. When this setting is enabled, `"$events/sys/alarm_activated"` and `"$events/sys/alarm_deactivated"` do not trigger rules.

::: tip

By default, clients are unable to subscribe directly to MQTT event messages. This section describes using rules to subscribe to these messages. You can also obtain the data from MQTT event messages by subscribing to [system topics](../../guides/observability/mqtt-system-topics.md).

:::

See the table below for the supported event topic list.

### Event Topic List

| Event topic name                                             | Explanation                     |
| ------------------------------------------------------------ | :------------------------------ |
| [$events/message/delivered](#message-delivery-event-events-message-delivered) | Message delivery                |
| [$events/message/acked](#message-acknowledged-event-events-message-acked) | Message acknowledged            |
| [$events/message/dropped](#message-dropped-when-routing-event-events-message-dropped) | Message dropped when routing    |
| [$events/message/delivery_dropped](#message-dropped-when-delivering-event-events-delivery-dropped) | Message dropped when delivering |
| [$events/client/connected](#connection-complete-event-events-client-connected) | Connection complete             |
| [$events/client/disconnected](#disconnect-event-events-client-disconnected) | Disconnect                      |
| [$events/client/connack](#connection-acknowledge-event-events-client-connack) | Connection acknowledged         |
| [$events/auth/check_authz_complete](#authorization-check-complete-event-events-client-check-authz-complete) | Authorization check complete    |
| [$events/auth/check_authn_complete](#authentication-check-complete-event-events-client-check-authn-complete) | Authentication check complete    |
| [$events/session/subscribed](#subscriber-event-events-session-subscribed) | Subscribe                       |
| [$events/session/unsubscribed](#unsubscribe-event-events-session-unsubscribed) | Unsubscribe                     |
| [$events/sys/alarm_activated](#system-alarm-activated-event-events-sys-alarm-activated) | Alarm activated |
| [$events/sys/alarm_deactivated](#system-alarm-deactivated-event-events-sys-alarm-deactivated) | Alarm deactivated |

::: tip

Starting from EMQX 5.10.0, namespaces have been introduced for event topics, reorganizing them into a logical, hierarchical structure. This enhancement improves the clarity, filtering, and management of event topics.

For backward compatibility, the old event topics are still supported. However, it is recommended to use the new namespaced topics for all new configurations. See the table below for the mapping between old and new (namespaced) topics.

| Previous event topic                    | New event topic                         |
|:----------------------------------------|:----------------------------------------|
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


### Message Delivery Event ("$events/message/delivered")

This event topic can be used to trigger a rule when a message is delivered to a client.

For example, to extract data from the `"$events/message/delivered"` event topic that includes the following data fields, ID and username of the publisher, message topic, message QoS, EMQX node with the event triggered, and the time when the event was triggered, you can use the statement below:

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
  "$events/message/delivered"
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
Below are detailed explanations of each field.

| Code                  | Explanation                                             |
| :-------------------- | :------------------------------------------------------ |
| `id`                  | MQTT message ID                                         |
| `from_clientid`       | Client ID of the publisher                              |
| `from_username`       | Username of the publisher                               |
| `clientid`            | Client ID of the subscriber                             |
| `username`            | Username of the subscriber                              |
| `payload`             | MQTT payload                                            |
| `peerhost`            | Client IP address                                       |
| `topic`               | MQTT topic                                              |
| `qos`                 | QoS level                                               |
| `flags`               | Flags                                                   |
| `pub_props`           | PUBLISH Properties (MQTT 5.0 clients only)              |
| `timestamp`           | Event trigger time (unit: ms)                           |
| `publish_received_at` | Time when PUBLISH message reaches EMQX <br />(unit: ms) |
| `node`                | EMQX node where the event triggered                     |

### Message Acknowledged Event ("$events/message/acked")

This event topic can be used to trigger a rule when the message delivery is acknowledged.

::: tip

Only available for QOS 1 and QOS 2 messages.

:::

For example, to extract data from the `"$events/message/acked"` event topic that includes the following data fields, ID and username of the publisher, message topic, message QoS, EMQX node where the event triggered, and the time when the event was triggered, you can use the statement below: <!--need to confirm the node part-->

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
  "$events/message/acked"
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

Below are detailed explanations of each field.

| Code                  | Explanation                                       |
| :-------------------- | :------------------------------------------------ |
| `id`                  | MQTT message ID                                   |
| `from_clientid`       | Client ID of the publisher                        |
| `from_username`       | Username of the publisher                         |
| `clientid`            | Client ID of the subscriber                       |
| `username`            | Username of the subscriber                        |
| `payload`             | MQTT payload                                      |
| `peerhost`            | Client IPAddress                                  |
| `topic`               | MQTT topic                                        |
| `qos`                 | QoS levels                                        |
| `flags`               | Flags                                             |
| `pub_props`           | PUBLISH Properties (MQTT 5.0 only)                |
| `puback_props`        | PUBACK Properties (MQTT 5.0 only)                 |
| `timestamp`           | Event trigger time (in milliseconds)              |
| `publish_received_at` | Time when PUBLISH message reaches EMQX (unit: ms) |
| `node`                | EMQX node where the event triggered               |

### Message Dropped When Routing Event ("$events/message/dropped")

This event topic can be used to trigger a rule when a message is dropped during routing.

For example, to extract data from the `"$events/message/dropped"` event topic that includes the following data fields: drop reason, message topic, message QoS, EMQX node where the event triggered, and the time when the event was triggered, you can use the statement below:

Example:
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

| Field                 | Explanation                                                  |
| :-------------------- | :----------------------------------------------------------- |
| `id`                  | MQTT message ID                                              |
| `reason`              | Dropping reasons: <br/><br/>`no_subscribers`: No clients subscribe to the topic<br/><br/>`receive_maximum_exceeded`: `awaiting_rel` queue is full<br/><br/>`packet_identifier_inuse`: Receive a QoS 2 message with unreleased packet ID <!--originally was send--> |
| `clientid`            | Client ID of the publisher                                   |
| `username`            | Username of the publisher                                    |
| `payload`             | MQTT payload                                                 |
| `peerhost`            | Client IP Address                                            |
| `topic`               | MQTT topic                                                   |
| `qos`                 | QoS levels                                                   |
| `flags`               | Flags                                                        |
| `pub_props`           | PUBLISH Properties (MQTT 5.0 only)                           |
| `timestamp`           | Event trigger time (unit: ms)                                |
| `publish_received_at` | Time when PUBLISH message reaches EMQX (unit: ms)            |
| `node`                | Node where the event is triggered                            |

### Message Dropped When Delivering Event ("$events/message/delivery_dropped")

This event topic can trigger a rule when a message is dropped during delivery.

For example, to extract data from the `"$events/message/delivery_dropped"` event topic that includes the following data fields: publisher ID and username, drop reason, message topic and QoS, you can use the statement below:

Example:
```sql
SELECT
  from_clientid,
  from_username,
  reason,
  topic,
  qos
FROM "$events/message/delivery_dropped"
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
Below are detailed explanations of each field.

| Explanation           |                                                              |
| :-------------------- | ------------------------------------------------------------ |
| `id`                  | MQTT message ID                                              |
| `reason`              | Dropping reasons: <br/><br/>`queue_full`: Message (QoS>0) queue is full.<br/><br/>`no_local`: Clients are not allowed to receive messages published by themselves.<br/><br/>`expired`: Message or the Session expired.<br/><br/>`qos0_msg`: Message (QoS 0) queue is full. |
| `from_clientid`       | Client ID of the publisher                                   |
| `from_username`       | Username of the publisher                                    |
| `clientid`            | Client ID of the subscriber                                  |
| `username`            | Username of the subscriber                                   |
| `payload`             | MQTT payload                                                 |
| `peerhost`            | Client IP Address                                            |
| `topic`               | MQTT topic                                                   |
| `qos`                 | Message QoS                                                  |
| `flags`               | Flags                                                        |
| `pub_props`           | PUBLISH Properties (MQTT 5.0 clients only)                   |
| `timestamp`           | Event trigger time (unit: ms)                                |
| `publish_received_at` | Time when PUBLISH message reaches EMQX (unit: ms)            |
| `node`                | EMQX node where the event is triggered                       |

### Connection Complete Event ("$events/client/connected")

This event topic can be used to trigger a rule when a client is connected successfully.

For example, to extract data from the `"$events/client/connected"` event topic that includes the following data fields: client ID and username, keepalive interval, and whether the connected MQTT client is acting as a bridge, you can use the statement below:

Example:
```sql
SELECT
  clientid,
  username,
  keepalive,
  is_bridge
FROM
  "$events/client/connected"
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

Refer to the table below for fields that can be selected from the received MQTT messages:

| Field             | Explanation                                                  |
| :---------------- | :----------------------------------------------------------- |
| `clientid`        | Client ID                                                    |
| `username`        | Client username                                              |
| `mountpoint`      | Mountpoint for bridging messages                             |
| `peername`        | IP address and port of terminal <!--whether all should include port--> |
| `sockname`        | IP Address and Port listened by EMQX                         |
| `proto_name`      | Protocol name                                                |
| `proto_ver`       | Protocol version                                             |
| `keepalive`       | MQTT keepalive interval                                      |
| `clean_start`     | MQTT clean_start                                             |
| `expiry_interval` | MQTT session expiration time                                 |
| `is_bridge`       | Whether the client is acting as a bridge                     |
| `connected_at`    | Client connection completion time (unit: ms)                 |
| `conn_props`      | CONNECT Properties (MQTT 5.0 clients only)                   |
| `timestamp`       | Event trigger time (unit: ms)                                |
| `node`            | EMQX node where the event is triggered                       |
| `client_attrs`        | [Client attributes](../client-attributes/client-attributes.md) |

### Disconnect Event ("$events/client/disconnected")

This event topic can be used to trigger a rule when a client is disconnected.


For example, you can use the statement below to extract data from the `"$events/client/disconnected"` event topic that includes the following data fields: client ID, username, disconnect reason, disconnect time, and EMQX node where the event is triggered.

Example:
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
Output:
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

| Field             | Explanation                                                  |
| :---------------- | :----------------------------------------------------------- |
| `reason`          | Disconnect reasons<br/><br/>`normal`: The client is intentionally disconnected <br/><br/>`kicked`: EMQX has forcibly removed the client through REST API<br/><br/>`keepalive_timeout`: The specified keepalive time period expired.<br/><br/>`not_authorized`: Authorization failed.<br/><br/>`tcp_closed`: The peer has closed network connection.<br/><br/>`discarded`: Another client ( with `clean_start` set to `true`) connected with the same ClientID, causing the previous connection to be dropped.<br/><br/>`takenover`: Another client ( with `clean_start` set to `false`) connected with the same ClientID, taking over the previous connection..<br/><br/>`internal_error`: An error has occurred due to an improperly formatted message or other unknown issues. |
| `clientid`        | Client ID                                                    |
| `username`        | Client username                                              |
| `peername`        | IP Address and Port number                                   |
| `sockname`        | IP Address and Port number listened by EMQX                  |
| `connected_at` | Client connection start time (unit: ms). This timestamp represents when the current session was established and helps identify which connection session the disconnect event belongs to.<br />It ensures that delayed disconnect events do not overwrite newer connection states. |
| `disconnected_at` | Client disconnection completion time (unit: ms)              |
| `disconn_props`   | DISCONNECT Properties (MQTT 5.0 clients only)                |
| `timestamp`       | Event trigger time (unit: ms)                                |
| `node`            | EMQX node where the event is triggered                       |
| `client_attrs`        | [Client attributes](../client-attributes/client-attributes.md) |

### Connection Acknowledge Event ("$events/client/connack")

This event topic can be used to trigger a rule when the EMQX sends a `CONNACK` packet to the client.

Example:

```sql
SELECT
  clientid,
  username,
  reason_code,
  node
FROM
  "$events/client/connack"
```

Output:

```json
{
  "username": "u_emqx",
  "reason_code": "success",
  "node": "emqx@127.0.0.1",
  "connected_at": 1645003578536,
  "clientid": "c_emqx"
}
```

Refer to the table below for fields that can be extracted:

| Field             | Explanation                                |
| ----------------- | :----------------------------------------- |
| `reason_code`     | Reason code*                               |
| `clientid`        | Client ID of the publisher                 |
| `username`        | Username of the publisher                  |
| `peername`        | IP and port                                |
| `sockname`        | IP and port listened by EMQX               |
| `proto_name`      | Protocol name                              |
| `proto_ver`       | Protocol version                           |
| `keepalive`       | MQTT keepalive interval                    |
| `clean_start`     | MQTT clean_start                           |
| `expiry_interval` | MQTT session expiration time               |
| `conn_props`      | CONNECT Properties (MQTT 5.0 clients only) |
| `timestamp`       | Event trigger time (unit: ms)              |
| `node`            | EMQX node where the alarm is triggered.    |

[^*]: The MQTT v5.0 protocol renames the return code to a reason code, adding a reason code to indicate more types of errors ([Reason code and ACK - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-new-features-reason-code-and-ack)).

Here is the reason code for MQTT v3.1.1 and MQTT v5.0.

:::: tabs type:card

::: tab MQTT v3.1.1

| Reason Code                      | Description                                                  |
| -------------------------------- | ------------------------------------------------------------ |
| `connection_accepted`            | Connection accepted                                          |
| `unacceptable_protocol_version`  | EMQX does not support the MQTT protocol requested by the client. <!--MQTT protocol?--> |
| `client_identifier_not_valid`    | The client ID is not allowed by EMQX. <!--tech review-->     |
| `server_unavaliable`             | Network connection has been established, but MQTT service is unavailable. |
| `malformed_username_or_password` | Username or password is in the wrong data format.            |
| `unauthorized_client`            | Client connection is not authorized.                         |

:::

::: tab MQTT v5.0

| Reason Code                     | Description                                                  |
| ------------------------------- | ------------------------------------------------------------ |
| `success`                       | Connection successful                                        |
| `unspecified_error`             | Unknown error                                                |
| `malformed_packet`              | Packet malformed                                             |
| `protocol_error`                | Protocol issue                                               |
| `implementation_specific_error` | Specific implementation error                                |
| `unsupported_protocol_version`  | Unsupported protocol version                                 |
| `client_identifier_not_valid`   | Invalid client ID                                            |
| `bad_username_or_password`      | Invalid username/password                                    |
| `not_authorized`                | Unauthorized                                                 |
| `server_unavailable`            | Server unavailable                                           |
| `server_busy`                   | Server busy                                                  |
| `banned`                        | Banned                                                       |
| `bad_authentication_method`     | Invalid authentication method                                |
| `topic_name_invalid`            | Invalid topic name                                           |
| `packet_too_large`              | Packet too big                                               |
| `quota_exceeded`                | Quota exceeded                                               |
| `retain_not_supported`          | Retain message feature unsupported                           |
| `qos_not_supported`             | Unsupported QoS level                                        |
| `use_another_server`            | Use different broker  <!--need to confirm server or broker--> |
| `server_moved`                  | Broker relocated <!--need to confirm server or broker-->     |
| `connection_rate_exceeded`      | Connection rate limit reached                                |

:::

::::

### Authorization Check Complete Event ("$events/auth/check_authz_complete")

This event topic can be used to trigger a rule when the authorization check for the client is complete.

Example:

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

Output:

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

Refer to the table below for fields that can be extracted.

| Field       | Explanation                                                  |
| ----------- | :----------------------------------------------------------- |
| `clientid`  | Client ID                                                    |
| `username`  | Username                                                     |
| `peerhost`  | Client IP Address                                            |
| `topic`     | MQTT topic                                                   |
| `action`    | Publish or subscribe action                                  |
| `result`    | Access control check result                                  |
| `authz_source `  | The authorization source |
| `timestamp` | Timestamp (unit: ms)                                         |
| `node`      | EMQX node where the event is triggered.                      |
| `client_attrs`        | [Client attributes](../client-attributes/client-attributes.md) |

### Authentication Check Complete Event ("$events/auth/check_authn_complete")

This event topic can be used to trigger a rule when the authentication check for the client is complete.

Example:

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

Output:

```json
{
  "clientid": "c_emqx",
  "username": "u_emqx",
  "reason_code": "success",
  "is_superuser": true,
  "is_anonymous": false
}
```

Refer to the table below for fields that can be extracted.

| Field       | Explanation                                                  |
| ----------- | :----------------------------------------------------------- |
| `clientid`  | Client ID                                                    |
| `username`  | Username                                                     |
| `peername`  | Client IP Address                                            |
| `reason_code`     | Authentication result                                  |
| `is_superuser`    | Whether this client is a super user                    |
| `is_anonymous`    | Whether this client is a anonymous user                |
| `client_attrs`        | [Client attributes](../client-attributes/client-attributes.md) |

### Subscriber Event ("$events/session/subscribed")

This event topic can be used to trigger a rule when the client subscribes successfully.

Example:

```sql
SELECT
  clientid,
  username,
  topic,
  qos
FROM
  "$events/session/subscribed"
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

Refer to the table below for fields that can be extracted.

| Field       | Explanation                                 |
| :---------- | :------------------------------------------ |
| `clientid`  | Client ID                                   |
| `username`  | Client username                             |
| `peerhost`  | Client IP Address                           |
| `topic`     | MQTT topic                                  |
| `qos`       | QoS levels                                  |
| `sub_props` | SUBSCRIBE Properties (MQTT 5.0 client only) |
| `timestamp` | Event trigger time (unit: ms)               |
| `node`      | EMQX node where the event is triggered      |
| `client_attrs`        | [Client attributes](../client-attributes/client-attributes.md) |

### Unsubscribe Event ("$events/session/unsubscribed")

The rule is triggered when the terminal subscription is cancelled successfully.


Example:
```sql
SELECT
  clientid,
  username,
  topic,
  qos
FROM
  "$events/session/unsubscribed"
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
Refer to the table below for fields that can be extracted.

| Field         | Explanation                                    |
| ------------- | :--------------------------------------------- |
| `clientid`    | Client ID                                      |
| `username`    | Client username                                |
| `peerhost`    | Client IP Address                              |
| `topic`       | MQTT topic                                     |
| `qos`         | QoS levels                                     |
| `unsub_props` | UNSUBSCRIBE Properties (MQTT 5.0 clients only) |
| `timestamp`   | Event trigger time (unit: ms)                  |
| `node`        | EMQX node where the event is triggered         |
| `client_attrs`        | [Client attributes](../client-attributes/client-attributes.md) |

### System Alarm Activated Event ("$events/sys/alarm_activated")

This event topic can be used to trigger a rule when an EMQX system alarm is activated.

For example, to extract data from the `"$events/sys/alarm_activated"` event topic, including the alarm name, details, descriptive message, and activation time, you can use the statement below:


Example:

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

Output:

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

Refer to the table below for fields that can be extracted.

| Field          | Explanation                                                  |
| -------------- | :----------------------------------------------------------- |
| `name`         | A short identifier for the alarm (e.g., `"too_many_processes"`) |
| `details`      | A JSON object containing additional details about the alarm; schema is not fixed (e.g., `{"usage": "99%", "high_watermark": "80%"}`) |
| `message`      | A descriptive message for the alarm (e.g., `"99% process usage"`) |
| `activated_at` | Unix timestamp (µs) when the alarm was activated             |
| `node`         | The EMQX node where the event was triggered                  |

### System Alarm Deactivated Event ("$events/sys/alarm_deactivated")

The rule is triggered when the EMQX system alarms are deactivated.

For example, to extract data from the `"$events/sys/alarm_deactivated"` event topic that includes the alarm name, details, description message, activation timestamp, and deactivation timestamp, you can use the following SQL statement:


Example:

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

Output:

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

Refer to the table below for fields that can be extracted.

| Field            | Explanation                                                  |
| ---------------- | :----------------------------------------------------------- |
| `name`           | A short identifier for the alarm (e.g., `"too_many_processes"`) |
| `details`        | A JSON object containing additional details about the alarm; schema is not fixed (e.g., `{"usage": "99%", "high_watermark": "80%"}`) |
| `message`        | A descriptive message for the alarm (e.g., `"99% process usage"`) |
| `activated_at`   | Unix timestamp (µs) when the alarm was activated             |
| `deactivated_at` | Unix timestamp (µs) when the alarm was deactivated           |
| `node`           | The EMQX node where the event was triggered                  |

## Data Bridges

Rules use topics prefixed by `$bridges/` to present messages or events triggered by a data bridge. The format is:

 `$bridges/<type>:<name>`

Where

-  `<type>:<name>` is the bridge Id,
-  `<type>` is the bridge type,
-  `<name>` is the bridge name.

For example, the MQTT Bridge events can be referred to in the format of  `"$bridges/mqtt:*`. To set a rule for all messages sent by the MQTT data bridge named `my_mqtt_bridge`, you can use the statement below:

**Example:**

```sql
SELECT
  *
FROM
  "$bridges/mqtt:my_mqtt_bridge"
```

**Output:**

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

For each field in the returned output:

| Field                 | Explanation                                                  |
| :-------------------- | :----------------------------------------------------------- |
| `id`                  | MQTT message ID                                              |
| `server`              | Server name of the remote MQTT broker, such as "broker.emqx.io:1883" |
| `payload`             | MQTT payload                                                 |
| `topic`               | MQTT topic                                                   |
| `qos`                 | MQTT QoS                                                     |
| `dup`                 | MQTT DUP flag                                                |
| `retain`              | MQTT Retain flag                                             |
| `pub_props`           | PUBLISH Properties (MQTT 5.0 clients only)                   |
| `message_received_at` | Timestamp when the message is received (unit: ms)            |
