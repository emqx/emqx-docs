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

# Introduction

## What's EMQX?

EMQX is an open-source, distributed MQTT messaging broker, it can support up to million level of concurrent MQTT connections.  It can be used for connecting to any devices that support MQTT protocol, and it can also be used for delivering message from server side to client.




## How many products in EMQX?


EMQX totally has [3 products.](https://www.emqx.com/en/products/emqx) Different products support different level of connections, features and services etc.

- EMQX Broker: EMQX open source version, supports the popular IoT protocols, such as MQTT, CoAP and LwM2M.
- EMQX Enterprise: EMQX enterprise version.  It is based on the open source version, and adds data persistence (support Redis, MySQL, MongoDB or PostgreSQL), data bridge to Kafka, LoRaWAN support, EMQX monitoring, Kubernetes deployment etc. It supports 1M level concurrent MQTT connections.
- EMQX Cloud: [EMQX Cloud](https://www.emqx.com/cloud) is an MQTT middleware for the IoT from EMQ. As the world's first fully managed MQTT 5.0 public cloud service, EMQX Cloud provides a one-stop O&M colocation and a unique isolated environment for MQTT services. In the era of Internet of Everything, EMQX Cloud can help you quickly build industry applications and easily realize the collection, transmission, computation and persistence of IoT data.




## What's EMQX authentication and it's use scenario?

**Tags:** [*Auth*](tags.md#auth)


When a client connects to EMQX server,  EMQX can authenticate it in different ways. It includes following 3 approaches,

- Username and password: Per every MQTT client connection, which can be configured at the server, only by passing with correct username and password, the client connection can be established.

- ClientID: Every MQTT client will have a unique ClientID,  and a list of ClientIds can be configured in server, and only ClientIds in the list can be authenticated successfully.

- Anonymous: Allows anonymous access.

Besides using the configuration file (to configure authentication), EMQX can also use database and integration with external applications, such as MySQL, PostgreSQL, Redis, MongoDB, HTTP and LDAP.




## What's Hook? What's the use scenario?

**Tags:** [*WebHook*](tags.md#webhook)


Hook is an interface provided by EMQX, which will be triggered when a connection, session or message is established/delivered. EMQX provides hooks listed in below, which allows user to save these triggered events to database, and user can conveniently query all kinds of information, such as client connect,  disconnect.  

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




## What's mqueue? How to use mqueue in EMQX?

**Tags:** [*Message Queue*](tags.md#message-queue)


mqueue is message queue stored in session during the message publish process. If the clean session is set false in mqtt connect packet, then EMQX would maintain session for the client which conneced to EMQX even the client has been disconnected from EMQX. Then the session would receive messages from the subscribed topic and store these messages into mqueue. And when the client online again, these messages would be delivered to client instantly. Because of low priority of qos 0 message in mqtt protocol, EMQX do not cache qos 0 message into mqueue. However, the mqueue could be configured in `emqx.conf`, if this entry has been configured as `zone.$name.mqueue_store_qos0 = true`, the qos0 mesage would been stored into mqueue, too. mqueue has limit size, it would be configured with `zone.external.max_mqueue_len` to determine the number of messages cached into mqueue. Notice that these messages are stored in memory, so please do not set the mqueue length to 0 (0 means there is no limit for mqueue), otherwise it would risk running out of memory.




## What's WebSocket? When to use Websocket to connect EMQX?

**Tags:** [*WebSocket*](tags.md#websocket)


WebSocket is a full-duplex communication protocol based on HTTP protocol, user can realize dual direction communications between browser and server. Through Websocket, server can push message to web browser. EMQX provides support of WebSocket, user can realize pub to topics and sub to topic from browsers.




## What's shared subscription, and it's use scenario?

**Tags:** [*Shared Subscription*](tags.md#shared-subscription)  [*Pub/Sub*](tags.md#pub-sub)


Shared subscription is a new feature of MQTT 5.0 specification. Before the feature was introduced in MQTT 5.0 specification, EMQ 2.x already supported the feature as non-standard MQTT protocol. In general, all of subscribers will receive ALL message for the subscribed topics, while clients that subscribe the same topic will receive the message with round-robin way, so one message will not be delivered to different clients. By this way, the subscribers can be load-balanced.

Shared subscription is very useful in data collection and centralized data analysis applications. In such cases,  number of data producers is much larger than consumers, and one message is ONLY need to be consumed by once.




## What is off-line message?

**Tags:** [*Retain*](tags.md#retain)


Usually an MQTT client receives messages only when it is connected to an EMQX, if this client is off-line, it will not receive messages. But if a client has a fixed ClientID, and it connects to the broker with clean_seesion = false, the broker will store particular message for it when it is off-line, if the Pub/Sub is done at certain QoS level (broker configuration). These messages will be delivered when this client is reconnected.  

Off-line message is useful when the connection is not stable, or the application has special requirements on QoS.




## What is Subscription by Broker? And its use scenario?

**Tags:** [*Subscription by Broke*](tags.md#subscription-by-broke)


Usually an MQTT client has to subscribe to the topics expressly by itself, if it wants to receive the messages under these topics. Subscription by Broker means that the broker can subscribe to particular topics for a client without client's interaction. The relation of such clients and their topics to be subscribed to is stored at broker side.

Using of Subscription by Broker can ease the management of massive client, and save the computational resource and bandwidth on device.

::: tip Tip
Currently this feature is available in the EMQX Enterprise edition. 
:::




## What is the usage of system topics? What system topics are available?

**Tags:** [*System Topic*](tags.md#system-topic)


The system topics have a prefix of `$SYS/`. Periodically, EMQX publishes system messages to system topics, these messages include system status, statistics of MQTT, client's online/offline status and so on.

Here are some examples of system topics, for a complete list of system topic please refer to EMQX document:

- $SYS/brokers:  List of nodes in cluster
- $SYS/brokers/${node}/clients/${clientid}/connected: this message is published when a client connects
- $SYS/broker/${node}/stats/connections/count: Number of connections on a node
- $SYS/broker/${node}/stats/sessions/count: Number of sessions on a node


## Why can the client without username and password still connect after authentication is enabled?

**Tags:** [*Auth*](tags.md#auth)

EMQX supports anonymous authentication and is enabled by default. After authentication is enabled, clients without username and password will successfully login using anonymous authentication. To change this behavior, you need to modify the `allow_anonymous` configuration item in `emqx.conf`, set it to false, and then restart EMQX.

Note: If your client is connected to port 11883, you need to modify `zone.internal.allow_anonymous`. For the relevant knowledge of Zone and Listener, please refer to [Configuration](../getting-started/config.md).
