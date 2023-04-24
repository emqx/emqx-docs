# Metrics and Statistics

EMQX provides metrics monitoring functions, based on which the operation and maintenance personnel can monitor the current service status and troubleshoot possible system malfunctions. 

EMQX divides metrics into Metrics and Statistics. 

- Metrics are counters used to measure simple incrementing and decrementing numbers, such as the number of sent bytes and messages. EMQX Metrics currently covers four dimensions: bytes, packets, messages, and events. 
- Statistics are gauges used to return a single value at the point of time the metric was requested. Statistics appear in pairs, including current values and historical maximums, such as the current number of subscriptions and the historical maximum number of subscriptions.

Both metrics and statistics are integer values.

EMQX provides users with multiple ways to view metrics and status. Most directly, you can view these data on the EMQX Dashboard. When it is not convenient to access the Dashboard, you can also obtain these data through [HTTP API](#request-metrics-and-statitics-via-rest-api) and [system topic](#obtain-metrics-and-statistics-via-system-topics) messages. 

## View Metrics and Statistics on Dashboard

On EMQX Dashboard, click **Monitoring** -> **Cluster Overview** from the left navigation menu. On the **Cluster Overview** page, you can see metrics by clicking the **Metrics** tab and statistics by clicking the **Nodes** tab.

### Connection and Session Metrics

You can see the metrics and event-related statistics for the cluster or node, such as client connection, connection sessions, and client access.

<img src="./assets/dashboard-event-metrics-ee.png" alt="dashboard-event-metrics-ee" style="zoom:50%;" />

#### Connections

| Metric Key          | Name                     | Description                              |
| ------------------- | ------------------------ | ---------------------------------------- |
| client.connect      | Connections/Connet       | `client.connect` hook trigger times      |
| client.authenticate | Access/Authenticate      | `client.authenticate` hook trigger times |
| client.connack      | Connections/Connack      | `client.connack` hook trigger times      |
| client.connected    | Connections/Connected    | `client.connected` hook trigger times    |
| client.disconnected | Connections/Disconnected | `client.disconnected` hook trigger times |
| client.authorize    | Access/Authorize         | `client.authorize` hook trigger times    |
| client.subscribe    | Connections/Subscribe    | `client.subscribe` hook trigger times    |
| client.unsubscribe  | Connections/Unsubscribe  | `client.unsubscribe` hook trigger times  |
| session.created     | Sessions/Created         | `session.created` hook trigger times     |
| session.discarded   | Sessions/Discarded       | `session.discarded` hook trigger times   |
| session.resumed     | Sessions/Resumed         | `session.resumed` hook trigger times     |
| session.takenover   | Sessions/Takenover       | `session.takenover` hook trigger times   |
| session.terminated  | Sessions/Terminated      | `session.terminated` hook trigger times  |

#### Sessions

| Metric Key         | Name               | Description                           |
| ------------------ | ------------------ | ------------------------------------- |
| session.created    | Session/Created    | session.created hook trigger times    |
| session.discarded  | Session/Discarded  | session.discarded hook trigger times  |
| session.resumed    | Session/Resumed    | session.resumed hook trigger times    |
| session.takenover  | Session/Takenover  | session.takenover hook trigger times  |
| session.terminated | Session/Terminated | session.terminated hook trigger times |

#### Access

| Metric Key                  | Name (in Dashboard)            | Description                                                  |
| --------------------------- | ------------------------------ | ------------------------------------------------------------ |
| authorization.allow         | Access/Allow                   | Number of client authorization passes                        |
| authorization.deny          | Access/Deny                    | Number of client authorization failures                      |
| authorization.matched.allow | Access/Matched Allow           | Number of client authorization passes due to authorized by some rules |
| authorization.matched.deny  | Access/Matched Deny            | Number of client authorization failures due to being rejected by some rules |
| authorization.nomatch       | Access/No Match                | Number of client authorization request not be matched any rules |
| authorization.cache_hit     | Access/Cache Hit               | Number of client getting authorization result (allow or deny) by cache |
| authorization.superuser     | Authorization/Superuser        | Superuser                                                    |
| client.auth.anonymous       | Client/Authorization Anonymous | Number of clients who log in anonymously                     |
| client.authenticate         | Client/Authenticate            | client.authenticate hook trigger times                       |
| client.authorize            | Client/Authorize               | client.authorize hook trigger times                          |

### Messaging

Scroll down the **Metrics** page, and you can see message related metrics, including bytes, packets, messages, and delivery. 

<img src="./assets/dashboard-messaging-metrics-ee.png" alt="dashboard-messaging-metrics-ee" style="zoom:50%;" />

#### Bytes

| Metric Key     | Name | Description              |
| -------------- | ------------------- | ------------------------ |
| bytes.received | Bytes/Received	   | Number of received bytes |
| bytes.sent     | Bytes/Sent          | Number of send bytes     |

#### Packets

| Metric Key                   | Name                         | Description                                                  |
| ---------------------------- | ---------------------------- | ------------------------------------------------------------ |
| packets.received             | Packets/Received             | Number of received packets                                   |
| packets.sent                 | Packets/Sent                 | Number of sent packets                                       |
| packets.connect.received     | Packets/Connect Received     | Number of received CONNECT packets                           |
| packets.connack.auth_error   | Packets/Connack Auth Error   | Number of sent CONNACK messages with reason codes 0x86 and 0x87 |
| packets.connack.error        | Packets/Connack Error        | Number of sent CONNACK packets where reason code is not 0x00. The value of this indicator is greater than or equal to the value of `packets.connack.auth_error` |
| packets.connack.sent         | Packets/Connack Sent         | Number of sent CONNACK packets                               |
| packets.publish.received     | Packets/Publish Received     | Number of received PUBLISH packets                           |
| packets.publish.sent         | Packets/Publish Sent         | Number of sent PUBLISH packets                               |
| packets.publish.inuse        | Packets/Publish Inuse        | Number of received PUBLISH packets with occupied packet identifiers |
| packets.publish.auth_error   | Packets/Publish Auth Error   | Number of received PUBLISH packets that failed the ACL check |
| packets.publish.error        | Packets/Publish Error        | Number of received PUBLISH packets that cannot be published  |
| packets.publish.dropped      | Packets/Publish Dropped      | Number of PUBLISH packets that were discarded due to the receiving limit |
| packets.puback.received      | Packets/Puback Received      | Number of received PUBACK packets                            |
| packets.puback.sent          | Packets/Puback Sent          | Number of sent PUBACK packets                                |
| packets.puback.inuse         | Packets/Puback Inuse         | Number of received PUBACK messages with occupied identifiers |
| packets.puback.missed        | Packets/Puback Missed        | Number of received PUBACK packets with unknown identifiers   |
| packets.pubrec.received      | Packets/Pubrec Received      | Number of received PUBREC packets                            |
| packets.pubrec.sent          | Packets/Pubrec Sent          | Number of sent PUBREC packets                                |
| packets.pubrec.inuse         | Packets/Pubrec Inuse         | Number of received PUBREC messages with occupied identifiers |
| packets.pubrec.missed        | Packets/Pubrec Missed        | Number of received PUBREC packets with unknown identifiers   |
| packets.pubrel.received      | Packets/Pubrel Received      | Number of received PUBREL packets                            |
| packets.pubrel.sent          | Packets/Pubrel Sent          | Number of sent PUBREL packets                                |
| packets.pubrel.missed        | Packets/Pubrel Missed        | Number of received PUBREL packets with unknown identifiers   |
| packets.pubcomp.received     | Packets/Pubcomp Received     | Number of received PUBCOMP packets                           |
| packets.pubcomp.sent         | Packets/Pubcomp Sent         | Number of sent PUBCOMP packets                               |
| packets.pubcomp.inuse        | Packets/Pubcomp Inuse        | Number of received PUBCOMP messages with occupied identifiers |
| packets.pubcomp.missed       | Packets/Pubcomp Missed       | Number of missed PUBCOMP packets                             |
| packets.subscribe.received   | Packets/Subscribe Received   | Number of received SUBSCRIBE packets                         |
| packets.subscribe.error      | Packets/Subscribe Error      | Number of received SUBSCRIBE packets with failed subscriptions |
| packets.subscribe.auth_error | Packets/Subscribe Auth Error | Number of received SUBACK packets that failed the ACL check  |
| packets.suback.sent          | Packets/Suback Sent          | Number of sent SUBACK packets                                |
| packets.unsubscribe.received | Packets/Unsubscribe Received | Number of received UNSUBSCRIBE packets                       |
| packets.unsubscribe.error    | Packets/Unsubscribe Error    | Number of received UNSUBSCRIBE packets with failed unsubscriptions |
| packets.unsuback.sent        | Packets/Unsuback Sent        | Number of sent UNSUBACK packets                              |
| packets.pingreq.received     | Packets/Pingreq Received     | Number of received PINGREQ packets                           |
| packets.pingresp.sent        | Packets/Pingresp Sent        | Number of sent PUBRESP packets                               |
| packets.disconnect.received  | Packets/Disconnect Received  | Number of received DISCONNECT packets                        |
| packets.disconnect.sent      | Packets/Disconnect Sent      | Number of sent DISCONNECT packets                            |
| packets.auth.received        | Packets/Auth Received        | Number of received AUTH packets                              |
| packets.auth.sent            | Packets/Auth Sent            | Number of sent AUTH packets                                  |

#### Message (PUBLISH packet)

| Metric Key                    | Name                    | Description                                                  |
| ------------------------------- | -------------------------------------- | ------------------------------------------------------------ |
| delivery.dropped.too_large      | Delivery/Dropped Too Large Messages    | Number of messages dropped because the length exceeded the limit when sending |
| delivery.dropped.queue_full     | Delivery/Dropped Messages (Queue Full) | Number of messages with a non-zero QoS that were dropped because the message queue was full when sending |
| delivery.dropped.qos0_msg       | Delivery/Dropped QoS 0 Messages        | Number of messages with QoS of 0 that were dropped because the message queue was full when sending |
| delivery.dropped.expired        | Delivery/Dropped Expired Messages      | Number of messages that were dropped due to message expiration when sending |
| delivery.dropped.no_local       | Delivery/Dropped No Local Messages     | Number of messages dropped due to the `No Local` subscription option when sending |
| delivery.dropped                | Delivery/Dropped                       | Total number of messages that were dropped when sent         |
| messages.delayed                | Messages/Delayed                       | Number of delay-published messages stored by EMQX    |
| messages.delivered              | Messages/Delivered                     | Number of messages forwarded to the subscription process internally by EMQX |
| messages.dropped                | Messages/Dropped                       | Total number of messages dropped by EMQX before forwarding to the subscription process |
| messages.dropped.no_subscribers | Messages/Dropped No Subscribers        | Number of messages dropped due to no subscribers             |
| messages.dropped.await_pubrel_timeout | Messages/Dropped Await Pubrel Timeout | Number of messages dropped due to await PUBREL timeout |
| messages.forward                | Messages/Forward                       | Number of messages forwarded to other nodes                  |
| messages.publish                | Messages/Publish                       | Number of messages published in addition to system messages  |
| messages.qos0.received          | Messages/QoS 0 Received                | Number of QoS 0 messages received from clients               |
| messages.qos1.received          | Messages/QoS 1 Received                | Number of QoS 2 messages received from clients               |
| messages.qos2.received          | Messages/QoS 2 Received                | Number of QoS 1 messages received from clients               |
| messages.qos0.sent              | Messages/QoS 0 Sent                    | Number of QoS 0 messages sent to clients                     |
| messages.qos1.sent              | Messages/QoS 1 Sent                    | Number of QoS 1 messages sent to clients                     |
| messages.qos2.sent              | Messages/QoS 2 Sent                    | Number of QoS 2 messages sent to clients                     |
| messages.received               | Messages/Received                      | Number of messages received from the client, which is equal to the sum of `messages.qos0.received`,` messages.qos1.received`, and `messages.qos2.received` |
| messages.sent                   | Messages/Sent                          | The number of messages sent to the client, which is equal to the sum of `messages.qos0.sent`,` messages.qos1.sent`, and `messages.qos2.sent` |
| messages.acked                  | Messages/Acked                         | Number of acked messages                                     |

#### Delivery

| Key                         | Name                        | Description                                                  |
| --------------------------- | --------------------------- | ------------------------------------------------------------ |
| delivery.dropped            | Delivery/Dropped            | Total number of messages that were dropped when sent         |
| delivery.dropped.expired    | Delivery/Dropped   Expired  | Number of messages that were dropped due to message expiration when sending |
| delivery.dropped.no_local   | Delivery/Dropped No Local   | Number of messages that were dropped due to the `No Local` subscription option when sending |
| delivery.dropped.qos0_msg   | Delivery/Dropped QoS0 Msg   | Number of messages with QoS of 0 that were dropped because the message queue was full when sending |
| delivery.dropped.queue_full | Delivery/Dropped Queue Full | Number of messages with a non-zero QoS that were dropped because the message queue was full when sending |
| delivery.dropped.too_large  | Delivery/Dropped Too Large  | The number of messages that were dropped because the length exceeded the limit when sending |

### Statstics

Click the **Nodes** tab on the **Cluster Overview** page. Click the name of the node to see the details about the node statistics of the specific node on the right.

<img src="./assets/node-statistics-ee.png" alt="node-statistics-ee" style="zoom:45%;" />

Here is the EMQX statistics list:


| Statistic Key              | Description                                                  |
| -------------------------- | ------------------------------------------------------------ |
| connections.count          | Current connections                                          |
| connections.max            | Historical maximum number of connections                     |
| live_connections.count     | Number of currently live connections                         |
| live_connections.max       | Historical maximum number of live connections                |
| channels.count             | same as `sessions.count`                                     |
| channels.max               | same as `sessions.max`                                       |
| sessions.count             | Number of current sessions                                   |
| sessions.max               | Historical maximum number of sessions                        |
| topics.count               | Number of current topics                                     |
| topics.max                 | Historical maximum number of topics                          |
| suboptions.count           | same as `subscriptions.count`                                |
| suboptions.max             | same as `subscriptions.max`                                  |
| subscribers.count          | Number of current subscribers                                |
| subscribers.max            | Historical maximum number of subscribers                     |
| subscriptions.count        | Number of current subscriptions, including shared subscriptions |
| subscriptions.max          | Historical maximum number of subscriptions                   |
| subscriptions.shared.count | Number of current shared subscriptions                       |
| subscriptions.shared.max   | Historical maximum number of shared subscriptions            |
| retained.count             | Number of currently retained messages                        |
| retained.max               | Historical maximum number of retained messages               |
| delayed.count              | Number of currently delayed messages                         |
| delayed.max                | Historical maximum number of delayed  messages               |

## Request Metrics and Statitics via REST API

You can also get the metrics and statistics through the [API Docs](https://docs.emqx.com/en/enterprise/v5.0/admin/api-docs.html). Click **Metrics** on the left navigation menu on the UI to execute this API request. For how to work with EMQX API, see [REST API](../admin/api.md).

<img src="./assets/metrics-api-doc.png" alt="metrics-api-doc" style="zoom:35%;" />

## Obtain Metrics and Statistics via System Topics

EMQX periodically publish messages about the running status, message statistics, client online and offline events through system topics. Clients can subscribe to system topics by add the prefix `$SYS/` before the topic name. For more information on different types of system topics, see [System Topic](../mqtt/mqtt-system-topics.md).

You can configure system topic settings on Dashboard. Click **Management** -> **MQTT Settings** from the left navigation menu. Select **System Topic** tab.

<img src="./assets/system-topic-setting.png" alt="system-topic-setting" style="zoom:40%;" />

- **Messages publish interval**: Set the time interval for sending `$sys` topic. 
- **Heartbeat interval**: Set the time interval for sending heartbeat messages.
- **Client connected notification**: When enabled, event messages about client being connected will be published.
- **Client disconnected notification**: When enabled, event messages about client being disconnected will be published.
- **Client subscribed notification**: When enabled, event messages about a client subscribing to a topic will be published.
- **Client unsubscribed notification**: When enabled, event messages about a client unsubscribing to a topic will be published.

