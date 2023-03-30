# Metrics

EMQX provides metrics monitoring functions, based on which the operation and maintenance personnel can monitor the current service status and troubleshoot possible system malfunctions. 

EMQX provides users with multiple ways to view metrics and status. Most directly, users can see this data on the **Metrics** tab of the EMQX Dashboard.

![Metrics in Dashboard](./assets/dashboard-metrics.jpg)

If it is not convenient to access the Dashboard, they can also obtain these data through HTTP API and system topic messages. You can refer to [HTTP API](../admin/api.md) and [$SYS system topic](../mqtt/mqtt-system-topics.md).

## Integrate with Monitor Systems

EMQX supports integration with third-party monitoring systems, such as [Prometheus](../observability/prometheus.md). Using a third-party monitoring system can bring the following advantages:

- A complete monitoring system, where the monitoring data of EMQX will be integrated with that of the other systems. For example, we can get the monitoring information of the server host;
- More intuitive monitoring report with figures and charts, such as using Grafana's dashboard;
- Various alarm notification means, such as using Prometheus's Alertmanager.

## Metrics & Stats

EMQX divides metrics into Metrics and Stats. 

- Metrics are data that increase monotonically, such as the number of sent bytes and messages. EMQX Metrics currently covers four dimensions: bytes, packets, messages, and events. 
- Stats appear in pairs, including current values and historical maximums, such as the current number of subscriptions and the historical maximum number of subscriptions.

### Metrics

In this section, we will elaborate on the four types of metrics, bytes, packets, messages, and events. 

#### Bytes

| Key            | Name (in Dashboard) | Data type | Description              |
| -------------- | ------------------- | --------- | ------------------------ |
| bytes.received | Bytes/Received	   | Integer   | Number of received bytes |
| bytes.sent     | Bytes/Sent          | Integer   | Number of send bytes     |

#### Packets

| Key                          | Name (in Dashboard)          | Data type | Description                                                  |
| ---------------------------- | ---------------------------- | --------- | ------------------------------------------------------------ |
| packets.received             | Packets/Received             | Integer   | Number of received packets                                   |
| packets.sent                 | Packets/Sent                 | Integer   | Number of sent packets                                       |
| packets.connect.received     | Packets/Connect Received     | Integer   | Number of received CONNECT packets                           |
| packets.connack.auth_error   | Packets/Connack Auth Error   | Integer   | Number of sent CONNACK messages with reason codes 0x86 and 0x87 |
| packets.connack.error        | Packets/Connack Error        | Integer   | Number of sent CONNACK packets where reason code is not 0x00. The value of this indicator is greater than or equal to the value of `packets.connack.auth_error` |
| packets.connack.sent         | Packets/Connack Sent         | Integer   | Number of sent CONNACK packets                               |
| packets.publish.received     | Packets/Publish Received     | Integer   | Number of received PUBLISH packets                           |
| packets.publish.sent         | Packets/Publish Sent         | Integer   | Number of sent PUBLISH packets                               |
| packets.publish.inuse        | Packets/Publish Inuse        | Integer   | Number of received PUBLISH packets with occupied packet identifiers |
| packets.publish.auth_error   | Packets/Publish Auth Error   | Integer   | Number of received PUBLISH packets that failed the ACL check |
| packets.publish.error        | Packets/Publish Error        | Integer   | Number of received PUBLISH packets that cannot be published  |
| packets.publish.dropped      | Packets/Publish Dropped      | Integer   | Number of PUBLISH packets that were discarded due to the receiving limit |
| packets.puback.received      | Packets/Puback Received      | Integer   | Number of received PUBACK packets                            |
| packets.puback.sent          | Packets/Puback Sent          | Integer   | Number of sent PUBACK packets                                |
| packets.puback.inuse         | Packets/Puback Inuse         | integer   | Number of received PUBACK messages with occupied identifiers |
| packets.puback.missed        | Packets/Puback Missed        | Integer   | Number of received PUBACK packets with unknown identifiers   |
| packets.pubrec.received      | Packets/Pubrec Received      | Integer   | Number of received PUBREC packets                            |
| packets.pubrec.sent          | Packets/Pubrec Sent          | Integer   | Number of sent PUBREC packets                                |
| packets.pubrec.inuse         | Packets/Pubrec Inuse         | Integer   | Number of received PUBREC messages with occupied identifiers |
| packets.pubrec.missed        | Packets/Pubrec Missed        | Integer   | Number of received PUBREC packets with unknown identifiers   |
| packets.pubrel.received      | Packets/Pubrel Received      | Integer   | Number of received PUBREL packets                            |
| packets.pubrel.sent          | Packets/Pubrel Sent          | Integer   | Number of sent PUBREL packets                                |
| packets.pubrel.missed        | Packets/Pubrel Missed        | Integer   | Number of received PUBREL packets with unknown identifiers   |
| packets.pubcomp.received     | Packets/Pubcomp Received     | Integer   | Number of received PUBCOMP packets                           |
| packets.pubcomp.sent         | Packets/Pubcomp Sent         | Integer   | Number of sent PUBCOMP packets                               |
| packets.pubcomp.inuse        | Packets/Pubcomp Inuse        | Integer   | Number of received PUBCOMP messages with occupied identifiers |
| packets.pubcomp.missed       | Packets/Pubcomp Missed       | Integer   | Number of missed PUBCOMP packets                             |
| packets.subscribe.received   | Packets/Subscribe Received   | Integer   | Number of received SUBSCRIBE packets                         |
| packets.subscribe.error      | Packets/Subscribe Error      | Integer   | Number of received SUBSCRIBE packets with failed subscriptions |
| packets.subscribe.auth_error | Packets/Subscribe Auth Error | Integer   | Number of received SUBACK packets that failed the ACL check  |
| packets.suback.sent          | Packets/Suback Sent          | Integer   | Number of sent SUBACK packets                                |
| packets.unsubscribe.received | Packets/Unsubscribe Received | Integer   | Number of received UNSUBSCRIBE packets                       |
| packets.unsubscribe.error    | Packets/Unsubscribe Error    | Integer   | Number of received UNSUBSCRIBE packets with failed unsubscriptions |
| packets.unsuback.sent        | Packets/Unsuback Sent        | Integer   | Number of sent UNSUBACK packets                              |
| packets.pingreq.received     | Packets/Pingreq Received     | Integer   | Number of received PINGREQ packets                           |
| packets.pingresp.sent        | Packets/Pingresp Sent        | Integer   | Number of sent PUBRESP packets                               |
| packets.disconnect.received  | Packets/Disconnect Received  | Integer   | Number of received DISCONNECT packets                        |
| packets.disconnect.sent      | Packets/Disconnect Sent      | Integer   | Number of sent DISCONNECT packets                            |
| packets.auth.received        | Packets/Auth Received        | Integer   | Number of received AUTH packets                              |
| packets.auth.sent            | Packets/Auth Sent            | Integer   | Number of sent AUTH packets                                  |

#### Message (PUBLISH packet)

| Key                             | Name (in Dashboard)                    | Data type | Description                                                  |
| ------------------------------- | -------------------------------------- | --------- | ------------------------------------------------------------ |
| delivery.dropped.too_large      | Delivery/Dropped Too Large Messages    | Integer   | Number of messages dropped because the length exceeded the limit when sending |
| delivery.dropped.queue_full     | Delivery/Dropped Messages (Queue Full) | Integer   | Number of messages with a non-zero QoS that were dropped because the message queue was full when sending |
| delivery.dropped.qos0_msg       | Delivery/Dropped QoS 0 Messages        | Integer   | Number of messages with QoS of 0 that were dropped because the message queue was full when sending |
| delivery.dropped.expired        | Delivery/Dropped Expired Messages      | Integer   | Number of messages that were dropped due to message expiration when sending |
| delivery.dropped.no_local       | Delivery/Dropped No Local Messages     | Integer   | Number of messages dropped due to the `No Local` subscription option when sending |
| delivery.dropped                | Delivery/Dropped                       | Integer   | Total number of messages that were dropped when sent         |
| messages.delayed                | Messages/Delayed                       | Integer   | Number of delay-published messages stored by EMQX    |
| messages.delivered              | Messages/Delivered                     | Integer   | Number of messages forwarded to the subscription process internally by EMQX |
| messages.dropped                | Messages/Dropped                       | Integer   | Total number of messages dropped by EMQX before forwarding to the subscription process |
| messages.dropped.no_subscribers | Messages/Dropped No Subscribers        | Integer   | Number of messages dropped due to no subscribers             |
| messages.dropped.await_pubrel_timeout | Messages/Dropped Await Pubrel Timeout | Integer   | Number of messages dropped due to await PUBREL timeout |
| messages.forward                | Messages/Forward                       | Integer   | Number of messages forwarded to other nodes                  |
| messages.publish                | Messages/Publish                       | Integer   | Number of messages published in addition to system messages  |
| messages.qos0.received          | Messages/QoS 0 Received                | Integer   | Number of QoS 0 messages received from clients               |
| messages.qos1.received          | Messages/QoS 1 Received                | Integer   | Number of QoS 2 messages received from clients               |
| messages.qos2.received          | Messages/QoS 2 Received                | Integer   | Number of QoS 1 messages received from clients               |
| messages.qos0.sent              | Messages/QoS 0 Sent                    | Integer   | Number of QoS 0 messages sent to clients                     |
| messages.qos1.sent              | Messages/QoS 1 Sent                    | Integer   | Number of QoS 1 messages sent to clients                     |
| messages.qos2.sent              | Messages/QoS 2 Sent                    | Integer   | Number of QoS 2 messages sent to clients                     |
| messages.received               | Messages/Received                      | Integer   | Number of messages received from the client, which is equal to the sum of `messages.qos0.received`,` messages.qos1.received`, and `messages.qos2.received` |
| messages.sent                   | Messages/Sent                          | Integer   | The number of messages sent to the client, which is equal to the sum of `messages.qos0.sent`,` messages.qos1.sent`, and `messages.qos2.sent` |
| messages.acked                  | Messages/Acked                         | Integer   | Number of acked messages                                     |

#### Event

| Key                 | Name (in Dashboard)      | Data type | Description                              |
| ------------------- | ------------------------ | --------- | ---------------------------------------- |
| client.connect      | Connections/Connet       | Integer   | `client.connect` hook trigger times      |
| client.authenticate | Access/Authenticate      | Integer   | `client.authenticate` hook trigger times |
| client.connack      | Connections/Connack      | Integer   | `client.connack` hook trigger times      |
| client.connected    | Connections/Connected    | Integer   | `client.connected` hook trigger times    |
| client.disconnected | Connections/Disconnected | Integer   | `client.disconnected` hook trigger times |
| client.authorize    | Access/Authorize         | Integer   | `client.authorize` hook trigger times    |
| client.subscribe    | Connections/Subscribe    | Integer   | `client.subscribe` hook trigger times    |
| client.unsubscribe  | Connections/Unsubscribe  | Integer   | `client.unsubscribe` hook trigger times  |
| session.created     | Sessions/Created         | Integer   | `session.created` hook trigger times     |
| session.discarded   | Sessions/Discarded       | Integer   | `session.discarded` hook trigger times   |
| session.resumed     | Sessions/Resumed         | Integer   | `session.resumed` hook trigger times     |
| session.takenover   | Sessions/Takenover       | Integer   | `session.takenover` hook trigger times   |
| session.terminated  | Sessions/Terminated      | Integer   | `session.terminated` hook trigger times  |

#### Authentication & Authorization

| Key                         | Name (in Dashboard)  | Data type | Description                                                  |
| --------------------------- | -------------------- | --------- | ------------------------------------------------------------ |
| authorization.allow         | Access/Allow         | Integer   | Number of client authorization passes                        |
| authorization.deny          | Access/Deny          | Integer   | Number of client authorization failures                      |
| authorization.matched.allow | Access/Matched Allow | Integer   | Number of client authorization passes due to authorized by some rules |
| authorization.matched.deny  | Access/Matched Deny  | Integer   | Number of client authorization failures due to being rejected by some rules |
| authorization.nomatch       | Access/No Match      | Integer   | Number of client authorization request not be matched any rules |
| authorization.cache_hit     | Access/Cache Hit     | Integer   | Number of client getting authorization result (allow or deny) by cache |

### Stats

Here is the EMQX Stats list:


| Key                        | Data type | Description                                                  |
| -------------------------- | --------- | ------------------------------------------------------------ |
| connections.count          | Integer   | Current connections                                          |
| connections.max            | Integer   | Historical maximum number of connections                     |
| live_connections.count     | Integer   | Number of currently live connections                         |
| live_connections.max       | Integer   | Historical maximum number of live connections                |
| channels.count             | Integer   | same as `sessions.count`                                     |
| channels.max               | Integer   | same as `sessions.max`                                       |
| sessions.count             | Integer   | Number of current sessions                                   |
| sessions.max               | Integer   | Historical maximum number of sessions                        |
| topics.count               | Integer   | Number of current topics                                     |
| topics.max                 | Integer   | Historical maximum number of topics                          |
| suboptions.count           | Integer   | same as `subscriptions.count`                                |
| suboptions.max             | Integer   | same as `subscriptions.max`                                  |
| subscribers.count          | Integer   | Number of current subscribers                                |
| subscribers.max            | Integer   | Historical maximum number of subscribers                     |
| subscriptions.count        | Integer   | Number of current subscriptions, including shared subscriptions |
| subscriptions.max          | Integer   | Historical maximum number of subscriptions                   |
| subscriptions.shared.count | Integer   | Number of current shared subscriptions                       |
| subscriptions.shared.max   | Integer   | Historical maximum number of shared subscriptions            |
| retained.count             | Integer   | Number of currently retained messages                        |
| retained.max               | Integer   | Historical maximum number of retained messages               |
| delayed.count              | Integer   | Number of currently delayed messages                         |
| delayed.max                | Integer   | Historical maximum number of delayed  messages               |
