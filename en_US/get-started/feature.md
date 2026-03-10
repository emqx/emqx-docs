# Feature FAQs

## Is there a limit on the number of ACLs for a single client?

Theoretically, there is no limit. However, to improve the performance of message subscription and publishing, it is advisable to avoid having too many ACL rules. It is recommended that a single client have no more than 10 ACLs. Using wildcard rules can help reduce the number of ACL entries.

## Can EMQX store messages to database?

The EMQX Enterprise supports data persistence. Please refer to [Data Integration](../data-integration/data-bridges.md).

## Does EMQX support saving MQTT messages to a database?

Yes. You can use the [Data Integration](../data-integration/data-bridges.md) of **EMQX Enterprise** to achieve message persistence. EMQX supports a variety of SQL, NoSQL, and time-series databases, and you can choose as needed.

## Does EMQX support forwarding MQTT messages to message queues such as Kafka?

Yes. You can forward messages to message queues such as Kafka and RabbitMQ through the [Data Integration](../data-integration/data-bridges.md) of **EMQX Enterprise**.

## Does EMQX support forwarding MQTT messages to other MQTT services?

Yes. You can use EMQX's [MQTT bridge](../data-integration/data-bridge-mqtt.md) to forward messages to other MQTT services, including IoT Hub deployed on public clouds such as AWS and Azure, as well as standard MQTT brokers such as EMQX and Mosquitto.

## What is shared subscription?

Shared subscription is a new feature introduced in MQTT 5.0 that allows clients to consume messages in a load balanced manner. We can divide clients into multiple subscription groups. Messages will still be forwarded to all subscription groups, but clients within a subscription group will receive messages alternately with a random, round robin, etc. strategy.

Clients of MQTT 3.1.1 can use shared subscriptions in the same way, since all the processing logic for shared subscriptions is done on the server, and the client only needs to subscribe to `$share/group/{topic filter}`.

Shared subscriptions are useful in data collection scenarios where there are many more message producers than consumers. For more information, see [Shared Subscription](../messaging/mqtt-shared-subscription.md).

## What is the system topic?

EMQX periodically publishes its operational status, MQTT message counts, and client online/offline events to system topics starting with `$SYS/`. Clients can subscribe to the system topic to obtain relevant information.

For a complete introduction to the system topic, please refer to [here](../observability/mqtt-system-topics.md).

## Does EMQX support clients subscribing to system topics via a shared subscription?

Yes. Some system messages may be published frequently, such as client online and offline events, so it is very useful for clients to use shared subscriptions. Subscription example: `$share/group1/$SYS/brokers/+/clients/+/connected`.

## Does EMQX support access to customized protocols?

EMQX provides an ExProto gateway that supports users to develop gRPC services using their familiar programming languages ​​(such as Java, Python, Go, etc.) to parse customized protocols used by devices and facilitate functions such as device connection, authentication, and message transmission.

For details, please refer to [ExProto Protocol Gateway](../gateway/exproto.md).

## Does EMQX support limiting the topics that clients can publish or subscribe to?

Yes. The authorization management mechanism in EMQX can achieve fine-grained management of client permissions. 

EMQX queries ACL rules from the `acl.conf` file by default. Users can also configure a database as the data source for ACL rules, such as EMQX's built-in database, MySQL, Redis, etc.

We usually recommend adding rules that are effective for multiple clients in `acl.conf`, such as only allowing clients in the same network segment to subscribe to system topics; and adding rules that are effective for a single client in the database, such as allowing client `client1` to subscribe to the topic `example`.

For a complete introduction to authorization, please refer to [here](../access-control/authz/authz.md).

## Does EMQX support rate limit?

Yes. EMQX supports connection rate and message inflow rate control to avoid system overload at the inlet. For a complete introduction, please refer to [here](../rate-limit/rate-limit.md).

## Is there a limit to the message receive rate for EMQX clients?

The EMQX or MQTT protocols do not directly limit the rate at which each client can receive messages. However, when too many messages are received and cannot be processed by the client in time, the messages may get heaped up and eventually discarded. To ensure system stability and message reliability, it is recommended that each client subscribe to receive messages at a rate of no more than 1500 messages/second (1KB per message).

If the message receive rate exceeds this recommendation, you can use [Shared Subscription](../messaging/mqtt-shared-subscription.md) to add multiple subscribers to spread the load and reduce the rate of messages received by a single subscriber.

## Does EMQX support cluster autodiscovery? What are the implementation methods?

In addition to creating clusters manually, EMQX also supports DNS, etcd and other node discovery strategies to achieve automatic clustering, see [Create and Manage Cluster](../deploy/cluster/create-cluster.md).

## Does EMQX support users to actively disconnect MQTT connections on the server side?

Yes. EMQX provides the [Command Line Interface](../admin/cli.md#clients) `emqx ctl clients kick <Client ID>` and the [REST API](https://docs.emqx.com/en/emqx/v@CE_MINOR_VERSION@/admin/api-docs.html) `DELETE /clients/{clientid}`, allowing users to manually kick MQTT connections. Users can also complete this operation on the clients page of the Dashboard.

For detailed instructions on the REST API, see [EMQX Open Source API](https://docs.emqx.com/en/emqx/v@CE_MINOR_VERSION@/admin/api-docs.html) and [EMQX Enterprise API](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html).

## I want to monitor the online and offline events of the device, how can I do it?

EMQX provides three ways to monitor the online and offline events of the device:

- Use the [WebHook](../data-integration/data-bridge-webhook.md) to forward the online and offline event messages to the external HTTP service.
- Use the MQTT client to subscribe to the [System Topic](../observability/mqtt-system-topics.md) to obtain the online and offline event notifications.
- Use the [Rule Engine](../data-integration/rules.md) to monitor the `client.connected` and `client.disconnected` events, and cooperate with the [Data Integration](../data-integration/data-bridges.md) to write the event messages to the specified database. (EMQX Enterprise only)

## How can data throughput and reliability be improved when integrating server with EMQX using MQTT?

When application services integrate with EMQX using the MQTT protocol, each client typically handles a high load. To fully leverage client performance and ensure system availability, here are some best practice recommendations:

1. **Separate Message Subscription and Publishing**: Avoid having a single client act as both publisher and subscriber.
2. **Use Shared Subscriptions**: Prioritize using shared subscriptions to receive messages, and set the number of subscriber clients based on the business scenario and message volume.
3. **Use Multiple Clients for Publishing Messages**: Configure the number of clients for publishing messages according to business needs and message volume, and implement a load-balancing strategy.

The core principle is to reduce the message load on a single client. By using multiple channels for MQTT interaction, overall message throughput performance can be enhanced, and system high availability can be increased.