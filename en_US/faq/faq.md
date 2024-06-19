---
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefinedBasic concept
---

# FAQs

## What's EMQX?

EMQX is an open-source, distributed [MQTT](https://mqtt.org/) messaging broker that can support millions of concurrent MQTT connections. EMQX can be used for connecting to devices that support MQTT protocol, and EMQX can also be used for delivering messages from a server to a client (e.g., a device).


## Which Products Do We Offer?


EMQX has [3 products](https://www.emqx.com/en/products/emqx) in total. The different products support different number of connections, features, services, etc.

- EMQX Broker: EMQX open source version. It supports the popular IoT protocols MQTT, CoAP and LwM2M.
- EMQX Enterprise: EMQX enterprise version. It is based on the open source version, and adds data persistence (support Redis, MySQL, MongoDB or PostgreSQL), data bridge to Kafka, LoRaWAN support, EMQX monitoring, Kubernetes deployment etc. It supports 1 million concurrent MQTT connections.
- EMQX Cloud: [EMQX Cloud](https://www.emqx.com/cloud) is an MQTT middleware for the IoT from EMQ. As the world's first fully managed MQTT 5.0 public cloud service, EMQX Cloud provides a one-stop O&M colocation and a unique isolated environment for MQTT services. In the era of Internet of Everything, EMQX Cloud can help you quickly build industry applications and easily realize the collection, transmission, computation and persistence of IoT data.

## Is There a Limit to the Message Receive Rate for EMQX Clients?

The EMQX or MQTT protocols do not directly limit the rate at which each client can receive messages. However, when too many messages are received and cannot be processed by the client in time, the messages may get heaped up and eventually discarded. To ensure system stability and message reliability, it is recommended that each client subscribe to receive messages at a rate of no more than 1500 messages/second (1KB per message).

If the message receive rate exceeds this recommendation, you can use [Shared Subscription](../advanced/shared-subscriptions.md) to add multiple subscribers to spread the load and reduce the rate of messages received by a single subscriber.


## How to Improve Data Throughput and Reliability When Integrating Server with EMQX Using MQTT?

When application services integrate with EMQX using the MQTT protocol, each client typically handles a high load. To fully leverage client performance and ensure system availability, here are some best practice recommendations:

1. **Separate Message Subscription and Publishing**: Avoid having a single client act as both publisher and subscriber.
2. **Use Shared Subscriptions**: Prioritize using shared subscriptions to receive messages, and set the number of subscriber clients based on the business scenario and message volume.
3. **Use Multiple Clients for Publishing Messages**: Configure the number of clients for publishing messages according to business needs and message volume, and implement a load-balancing strategy.

The core principle is to reduce the message load on a single client. By using multiple channels for MQTT interaction, overall message throughput performance can be enhanced, and system high availability can be increased.

## What are EMQX's Authentication Options?

**Tags:** [*Auth*](tags.md#auth)


When a client connects to an EMQX server, the EMQX server can authenticate it in different ways. EMQX supports the following 3 approaches:

- Username and password: A client connection can be established only when passing the correct user name and password (which can be configured at server).

- ClientID: Every MQTT client will have a unique ClientID. A list of acceptable ClientIDs can be configured for server. Only ClientIDs in this list can be authenticated successfully.

- Anonymous: Allows anonymous access.

Besides using the configuration file (to configure authentication), EMQX can also use database and integration with external applications, such as MySQL, PostgreSQL, Redis, MongoDB, HTTP and LDAP.

## Is There a Limit on the Number of ACLs for a Single Client?

Theoretically, there is no limit. However, to improve the performance of message subscription and publishing, it is advisable to avoid having too many ACL rules. It is recommended that a single client have no more than 10 ACLs. Using wildcard rules can help reduce the number of ACL entries.

## What's a Hook and When is a Hook Useful?

**Tags:** [*WebHook*](tags.md#webhook)


A Hook is an interface provided by EMQX, which will be triggered when a connection, session or message is established/delivered. EMQX provides the hook types listed below. Hooks allows a user to, for example, save events to a database. This could be useful if one wants to gather different kinds of information, such as when a client connects and disconnects.  

- client.connected: client online
- client.disconnected: client offline
- client.subscribe: client subscribes topics
- client.unsubscribe: client unsubscribes topics
- session.created: session was created
- session.resumed: session is resumed
- session.subscribed: after session subscribe topic
- session.unsubscribed: after session unsubscribe topic
- session.terminated: session is terminated
- message.publish: MQTT message is published
- message.delivered: MQTT message is delivered
- message.acked: MQTT message is acknowledged
- message.dropped: MQTT message is dropped




## What's a mqueue? How to Configure mqueues in EMQX?

**Tags:** [*Message Queue*](tags.md#message-queue)


A mqueue is a message queue that store messages for a session. If the clean session flag is set to false in the MQTT connect packet, then EMQX would maintain the session for the client even when the client has been disconnected from EMQX. Then the session would receive messages from the subscribed topic and store these messages into the sessions mqueue. And when the client is online again, these messages would be delivered to the client instantly. Because of low priority of QOS 0 message in mqtt protocol, EMQX do not save QOS 0 message in the mqueue. However, this behavior can be overridden by setting `zone.$name.mqueue_store_qos0 = true` in `emqx.conf`. With the `zone.$name.mqueue_store_qos0 = true`, even a QOS 0 message would been saved in the mqueue. The maximum size of the mqueue can be configured with the setting `zone.external.max_mqueue_len`. Notice that the mqueue is stored in memory, so please do not set the mqueue length to 0 (0 means there is no limit for mqueue), otherwise the system would risk running out of memory.




## What's a WebSocket? When to Use a Websocket to Connect to EMQX?

**Tags:** [*WebSocket*](tags.md#websocket)


WebSocket is a full-duplex communication protocol with an API supported by modern web browsers. A user can use the WebSocket API to create a dual-direction communication channel between a web browser and a server. Through a WebSocket, the server can push messages to the web browser. EMQX provides support for WebSocket. This means that users can publish to MQTT topics and subscribe to MQTT topics from browsers.




## What's a Shared Subscription, and When Is It Useful?

**Tags:** [*Shared Subscription*](tags.md#shared-subscription)  [*Pub/Sub*](tags.md#pub-sub)


Shared subscription is an MQTT feature that was introduced in MQTT 5.0 specification. Before the feature was introduced in MQTT 5.0 specification, EMQ 2.x already supported the feature as a non-standard MQTT protocol. In general, all of subscribers will receive ALL messages for the subscribed topics. However, clients that share a subscription to a topic will receive the messages in a round-robin way, so only one of the clients that share a subscription will receive each message. This feature can thus be used for load-balancing.

Shared subscription is very useful in data collection and centralized data analysis applications. In such cases, the number of data producers is much larger than consumers, and one message ONLY need to be consumed once.




## What is an Off-Line Message?

**Tags:** [*Retain*](tags.md#retain)


Usually an MQTT client receives messages only when it is connected to an EMQX broker, and it will not receive messages if it is off-line. But if a client has a fixed ClientID, and it connects to the broker with clean_session = false, the broker will store particular messages for it when it is off-line. If the Pub/Sub is done at certain QoS level (broker configuration), these messages will be delivered when this client is reconnected.  

Off-line messages are useful when the connection is not stable, or the application has special requirements on QoS.




## What is Subscription by Broker? And Its Use Scenario?

**Tags:** [*Subscription by Broke*](tags.md#subscription-by-broke)


Usually an MQTT client has to subscribe to the topics explicitly by itself, if it wants to receive the messages under these topics. Subscription by Broker means that the broker can subscribe to particular topics for a client without client's interaction. The relation of such clients and the topics they should be subscribed to is stored at broker side.

Usage of Subscription by Broker can ease the management of massive clients, and save computational resources and bandwidth for devices.

::: tip Tip
Currently this feature is available in the EMQX Enterprise. 
:::




## What Is the Usage of System Topics? What System Topics Are Available?

**Tags:** [*System Topic*](tags.md#system-topic)


The system topics have a prefix of `$SYS/`. Periodically, EMQX publishes system messages to system topics, these messages include system status, statistics, client's online/offline status and so on.

Here are some examples of system topics (for a complete list of system topic please refer to EMQX documentation):

- $SYS/brokers:  List of nodes in cluster
- $SYS/brokers/${node}/clients/${clientid}/connected: this message is published when a client connects
- $SYS/broker/${node}/stats/connections/count: Number of connections on a node
- $SYS/broker/${node}/stats/sessions/count: Number of sessions on a node


## Why Can a Client without Username and Password Still Connect After Authentication is Enabled?

**Tags:** [*Auth*](tags.md#auth)

EMQX supports anonymous authentication and it is enabled by default. After authentication is enabled, clients without username and password will successfully login using anonymous authentication. To change this behavior, you need to set the `allow_anonymous` configuration item in `emqx.conf` to false, and restart EMQX.

Note: If your client is connected to port 11883, you need to modify `zone.internal.allow_anonymous`. For the relevant knowledge of Zone and Listener, please refer to [Configuration](../getting-started/config.md).
