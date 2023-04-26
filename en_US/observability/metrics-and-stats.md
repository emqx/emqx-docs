# Statistics and Metrics

EMQX provides metrics monitoring functions, based on which the operation and maintenance personnel can monitor the current service status and troubleshoot possible system malfunctions. 

EMQX divides monitoring status into Statistics and Metrics. 

- Statistics are integer-type gauges used to return a single value at the point of time the metric was requested. 
- Metrics are integer-type counters used to measure simple incrementing and decrementing numbers, such as the number of sent bytes and messages.  

EMQX provides users with multiple ways to view statistics and metrics. Most directly, you can view these data on the EMQX Dashboard. When it is not convenient to access the Dashboard, you can also obtain these data through [HTTP API](#request-metrics-and-statitics-via-rest-api) and [system topic](#obtain-metrics-and-statistics-via-system-topics) messages. 

## View Statistics on Dashboard

On EMQX Dashboard, click **Monitoring** -> **Cluster Overview** from the left navigation menu. On the **Cluster Overview** page, click the **Nodes** tab. Click the name of the node to see the details about the node statistics of the specific node on the right.

<img src="./assets/node-statistics-ee.png" alt="node-statistics-ee" style="zoom:45%;" />

Statistics include two values: current values and historical maximums, for example, the current number of subscriptions and the historical maximum number of subscriptions. Here is the EMQX statistics list:


| Statistics                 | Description                                                  |
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

## View Metrics on Dashboard

On EMQX Dashboard, click **Monitoring** -> **Cluster Overview** from the left navigation menu. On the **Cluster Overview** page, you can see metrics by clicking the **Metrics** tab. EMQX Metrics currently covers four dimensions: bytes, packets, messages, and events.

### Connection and Session Metrics

You can see the event-related metrics for the cluster or node, such as client connection, connection sessions, and client access.

<img src="./assets/dashboard-event-metrics-ee.png" alt="dashboard-event-metrics-ee" style="zoom:50%;" />

#### Connections

| Metrics             | Description                              |
| ------------------- | ---------------------------------------- |
| client.connect      | `client.connect` hook trigger times      |
| client.authenticate | `client.authenticate` hook trigger times |
| client.connack      | `client.connack` hook trigger times      |
| client.connected    | `client.connected` hook trigger times    |
| client.disconnected | `client.disconnected` hook trigger times |
| client.authorize    | `client.authorize` hook trigger times    |
| client.subscribe    | `client.subscribe` hook trigger times    |
| client.unsubscribe  | `client.unsubscribe` hook trigger times  |
| session.created     | `session.created` hook trigger times     |
| session.discarded   | `session.discarded` hook trigger times   |
| session.resumed     | `session.resumed` hook trigger times     |
| session.takenover   | `session.takenover` hook trigger times   |
| session.terminated  | `session.terminated` hook trigger times  |

#### Sessions

| Metrics            | Description                           |
| ------------------ | ------------------------------------- |
| session.created    | session.created hook trigger times    |
| session.discarded  | session.discarded hook trigger times  |
| session.resumed    | session.resumed hook trigger times    |
| session.takenover  | session.takenover hook trigger times  |
| session.terminated | session.terminated hook trigger times |

#### Access

| Metrics                     | Description                                                  |
| --------------------------- | ------------------------------------------------------------ |
| authorization.allow         | Number of client authorization passes                        |
| authorization.deny          | Number of client authorization failures                      |
| authorization.matched.allow | Number of client authorization passes due to authorized by some rules |
| authorization.matched.deny  | Number of client authorization failures due to being rejected by some rules |
| authorization.nomatch       | Number of client authorization request not be matched any rules |
| authorization.cache_hit     | Number of client getting authorization result (allow or deny) by cache |
| authorization.superuser     | Superuser                                                    |
| client.auth.anonymous       | Number of clients who log in anonymously                     |
| client.authenticate         | client.authenticate hook trigger times                       |
| client.authorize            | client.authorize hook trigger times                          |

### Messaging

Scroll down the **Metrics** page, and you can see message related metrics, including bytes, packets, messages, and delivery. 

<img src="./assets/dashboard-messaging-metrics-ee.png" alt="dashboard-messaging-metrics-ee" style="zoom:50%;" />

#### Bytes

| Metrics    | Description              |
| -------------- | ------------------------ |
| bytes.received | Number of received bytes |
| bytes.sent     | Number of send bytes     |

#### Packets

| Metrics                      | Description                                                  |
| ---------------------------- | ------------------------------------------------------------ |
| packets.received             | Number of received packets                                   |
| packets.sent                 | Number of sent packets                                       |
| packets.connect.received     | Number of received CONNECT packets                           |
| packets.connack.auth_error   | Number of sent CONNACK messages with reason codes 0x86 and 0x87 |
| packets.connack.error        | Number of sent CONNACK packets where reason code is not 0x00. The value of this indicator is greater than or equal to the value of `packets.connack.auth_error` |
| packets.connack.sent         | Number of sent CONNACK packets                               |
| packets.publish.received     | Number of received PUBLISH packets                           |
| packets.publish.sent         | Number of sent PUBLISH packets                               |
| packets.publish.inuse        | Number of received PUBLISH packets with occupied packet identifiers |
| packets.publish.auth_error   | Number of received PUBLISH packets that failed the ACL check |
| packets.publish.error        | Number of received PUBLISH packets that cannot be published  |
| packets.publish.dropped      | Number of PUBLISH packets that were discarded due to the receiving limit |
| packets.puback.received      | Number of received PUBACK packets                            |
| packets.puback.sent          | Number of sent PUBACK packets                                |
| packets.puback.inuse         | Number of received PUBACK messages with occupied identifiers |
| packets.puback.missed        | Number of received PUBACK packets with unknown identifiers   |
| packets.pubrec.received      | Number of received PUBREC packets                            |
| packets.pubrec.sent          | Number of sent PUBREC packets                                |
| packets.pubrec.inuse         | Number of received PUBREC messages with occupied identifiers |
| packets.pubrec.missed        | Number of received PUBREC packets with unknown identifiers   |
| packets.pubrel.received      | Number of received PUBREL packets                            |
| packets.pubrel.sent          | Number of sent PUBREL packets                                |
| packets.pubrel.missed        | Number of received PUBREL packets with unknown identifiers   |
| packets.pubcomp.received     | Number of received PUBCOMP packets                           |
| packets.pubcomp.sent         | Number of sent PUBCOMP packets                               |
| packets.pubcomp.inuse        | Number of received PUBCOMP messages with occupied identifiers |
| packets.pubcomp.missed       | Number of missed PUBCOMP packets                             |
| packets.subscribe.received   | Number of received SUBSCRIBE packets                         |
| packets.subscribe.error      | Number of received SUBSCRIBE packets with failed subscriptions |
| packets.subscribe.auth_error | Number of received SUBACK packets that failed the ACL check  |
| packets.suback.sent          | Number of sent SUBACK packets                                |
| packets.unsubscribe.received | Number of received UNSUBSCRIBE packets                       |
| packets.unsubscribe.error    | Number of received UNSUBSCRIBE packets with failed unsubscriptions |
| packets.unsuback.sent        | Number of sent UNSUBACK packets                              |
| packets.pingreq.received     | Number of received PINGREQ packets                           |
| packets.pingresp.sent        | Number of sent PUBRESP packets                               |
| packets.disconnect.received  | Number of received DISCONNECT packets                        |
| packets.disconnect.sent      | Number of sent DISCONNECT packets                            |
| packets.auth.received        | Number of received AUTH packets                              |
| packets.auth.sent            | Number of sent AUTH packets                                  |

#### Message (PUBLISH packet)

| Metrics                   | Description                                                  |
| ------------------------------- | ------------------------------------------------------------ |
| delivery.dropped.too_large      | Number of messages dropped because the length exceeded the limit when sending |
| delivery.dropped.queue_full     | Number of messages with a non-zero QoS that were dropped because the message queue was full when sending |
| delivery.dropped.qos0_msg       | Number of messages with QoS of 0 that were dropped because the message queue was full when sending |
| delivery.dropped.expired        | Number of messages that were dropped due to message expiration when sending |
| delivery.dropped.no_local       | Number of messages dropped due to the `No Local` subscription option when sending |
| delivery.dropped                | Total number of messages that were dropped when sent         |
| messages.delayed                | Number of delay-published messages stored by EMQX    |
| messages.delivered              | Number of messages forwarded to the subscription process internally by EMQX |
| messages.dropped                | Total number of messages dropped by EMQX before forwarding to the subscription process |
| messages.dropped.no_subscribers | Number of messages dropped due to no subscribers             |
| messages.dropped.await_pubrel_timeout | Number of messages dropped due to await PUBREL timeout |
| messages.forward                | Number of messages forwarded to other nodes                  |
| messages.publish                | Number of messages published in addition to system messages  |
| messages.qos0.received          | Number of QoS 0 messages received from clients               |
| messages.qos1.received          | Number of QoS 2 messages received from clients               |
| messages.qos2.received          | Number of QoS 1 messages received from clients               |
| messages.qos0.sent              | Number of QoS 0 messages sent to clients                     |
| messages.qos1.sent              | Number of QoS 1 messages sent to clients                     |
| messages.qos2.sent              | Number of QoS 2 messages sent to clients                     |
| messages.received               | Number of messages received from the client, which is equal to the sum of `messages.qos0.received`,` messages.qos1.received`, and `messages.qos2.received` |
| messages.sent                   | The number of messages sent to the client, which is equal to the sum of `messages.qos0.sent`,` messages.qos1.sent`, and `messages.qos2.sent` |
| messages.acked                  | Number of acked messages                                     |

#### Delivery

| Metrics                     | Description                                                  |
| --------------------------- | ------------------------------------------------------------ |
| delivery.dropped            | Total number of messages that were dropped when sent         |
| delivery.dropped.expired    | Number of messages that were dropped due to message expiration when sending |
| delivery.dropped.no_local   | Number of messages that were dropped due to the `No Local` subscription option when sending |
| delivery.dropped.qos0_msg   | Number of messages with QoS of 0 that were dropped because the message queue was full when sending |
| delivery.dropped.queue_full | Number of messages with a non-zero QoS that were dropped because the message queue was full when sending |
| delivery.dropped.too_large  | The number of messages that were dropped because the length exceeded the limit when sending |

## Request Monitoring Status via REST API

You can also get the metrics and statistics through the [API Docs](https://docs.emqx.com/en/enterprise/v5.0/admin/api-docs.html). Click **Metrics** on the left navigation menu on the UI to execute this API request. For how to work with EMQX API, see [REST API](../admin/api.md).

<img src="./assets/metrics-api-doc.png" alt="metrics-api-doc" style="zoom:35%;" />

## Obtain Monitoring Status via System Topics

EMQX periodically publishes messages about the running status, message statistics, and client online and offline events through system topics. Clients can subscribe to system topics by adding the prefix `$SYS/` before the topic name. For more information on different types of system topics, see [System Topic](./mqtt-system-topics.md).

You can configure system topic settings on Dashboard. Click **Management** -> **MQTT Settings** from the left navigation menu. Select **System Topic** tab.

<img src="./assets/system-topic-setting.png" alt="system-topic-setting" style="zoom:40%;" />

- **Messages publish interval**: Set the time interval for sending `$sys` topic. 
- **Heartbeat interval**: Set the time interval for sending heartbeat messages.
- **Client connected notification**: Enabled by default and event messages about client being connected will be published.
- **Client disconnected notification**: Enabled by default and event messages about client being disconnected will be published.
- **Client subscribed notification**: Disabled by default; When enabled, event messages about a client subscribing to a topic will be published.
- **Client unsubscribed notification**: Disabled by default; When enabled, event messages about a client unsubscribing to a topic will be published.

