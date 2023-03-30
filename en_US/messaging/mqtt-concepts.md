# MQTT Core Concepts

Message Queue Telemetry Transport (MQTT) is the most commonly used lightweight messaging protocol for the Internet of Things (IoT). The protocol is based on a publish/subsribe (Pub/Sub) pattern for message communication. It allows devices and applications to exchange data in real-time using a simple and efficient message format, which minimizes network overhead and reduces power consumption. 

Served as an MQTT messaging platform, EMQX Enterprise provides full support to a complete set of MQTT messaging features. This section provides brief introductions to the core conecpts of MQTT. You can learn further about each concept and more about MQTT by following the links to the [MQTT blog series](https://www.emqx.com/en/blog/category/mqtt). 

## Publish/Subscribe Pattern

The protocol is event driven and connects devices using the Pub/Sub pattern. The sender (Publisher) and the receiver (Subscriber) communicate via Topics and are decoupled from each other. The connection between them is handled by the MQTT broker who routes and filters all incoming messages and distributes them correctly to the Subscribers.

The publisher and subscriber do not need to know each other's existence. Their sole connection is based on a predetermined agreement regarding the message. The Pub/sub pattern enables flexible message communication, as subscribers and publishers can be dynamically added or removed as needed. It also makes the implementation of message broadcasting, multicasting, and unicasting more easier. 

For more information on the Pub/Sub pattern, see [Introduction to MQTT Publish-subscribe Pattern](https://www.emqx.com/en/blog/mqtt-5-introduction-to-publish-subscribe-model).

## MQTT Server

The MQTT server acts as a broker between the publishing clients and subscribing clients, forwarding all received messages to the matching subscribing clients. Therefore, sometimes the server is directly referred to as the MQTT Broker.

## MQTT Client

The clients refer to devices or applications who can connect to an MQTT server using the MQTT protocol. They can act as both publishers and subscribers, or in either of those roles separately.

## Topic and Wildcards

Topics are used to identify and differentiate between different messages, forming the basis of MQTT message routing. Publishers can specify the topic of a message when publishing, while subscribers can choose to subscribe to topics of interest to receive relevant messages.

Subscribers can use wildcards in the subscribed topics to achieve the goal of subscribing to multiple topics at once. MQTT provides two types of topic wildcards, single-level wildcard and multi-level wildcard, to meet different subscription needs.

For more information on topics and wildcards, see [Understanding MQTT Topics & Wildcards by Case](https://www.emqx.com/en/blog/advanced-features-of-mqtt-topics).

## Quality of Service (QoS)

MQTT defines three levels of QoS to provide different levels of message reliability. Each message can independently set its own QoS when published. 

- QoS 0: delivers a message at most once and may be lost; 
- QoS 1: delivers a message at least once and guarantees arrival, but may be duplicated; 
- QoS 2: delivers a message exactly once and guarantees arrival without duplication. 

As the QoS level increases, the complexity of message transmission also increases. You need to choose the appropriate QoS level based on the actual scenario.

For more information on QoS, see [Introduction to MQTT QoS 0, 1, 2](https://www.emqx.com/en/blog/introduction-to-mqtt-qos).

## Session

QoS is a theoretical mechanism designed to ensure reliable message delivery, while a session ensures the proper implementation of QoS 1 and 2 protocol procedures.

A session refers to the stateful interaction between a client and a server, which can persist for the same duration as the network connection or span across multiple network connections, commonly known as a persistent session. The connection can either resume from an existing session or start from a new session.

For more information on sessions, see [MQTT Persistent Session and Clean Session Explained](https://www.emqx.com/en/blog/mqtt-session).

## Retained Message

Unlike regular messages, retained messages can be stored on an MQTT server. When any new subscriber subscribes to a topic that matches the topic of a retained message, they immediately receive that message, even if it was published before they subscribed to the topic.

The retained message feature allows subscribers to receive data updates immediately upon connecting, without having to wait for the publisher to re-publish the message. Retained messages can be thought of as a message "cloud drive" in some ways: upload messages to the "cloud drive" at any time, and retrieve messages from the "cloud drive" at any time. However, this "cloud drive" is limited to storing only one latest retained message per topic.

You can try to publish a retained message using the MQTT X Client by following the instructions in [Retained Message](./mqtt-retained-message.md).

To learn more about retained message technologies, see [The Beginner's Guide to MQTT Retained Messages](https://www.emqx.com/en/blog/mqtt5-features-retain-message).

## Will Message

The feature of Pub/Sub pattern determines that no client, other than the server, is aware of a client leaving the communication network. However, a will message provides the ability for a disconnected client to notify other clients.

Clients can set their own will message with the server when they establish a connection, and the server publishes this message immediately or after a specified delay if the client disconnects unexpectedly. Clients subscribed to the corresponding will message topic will receive this message and take appropriate action, such as updating the online status of that client, etc.

You can try to publish a will message using the MQTT X Client by following the instructions in [Will Message](./mqtt-will-message.md).

To learn more about will message technologies, see [Use of MQTT Will Message](https://www.emqx.com/en/blog/use-of-mqtt-will-message).

## Shared Subscription

In common cases, messages are forwarded to all matching subscribers. However, in some cases, you may want to coordinate multiple clients to process received messages in a horizontally scalable way to increase load capacity. Alternatively, users may want to add a backup client for clients to seamlessly switch to when the primary client goes offline, ensuring high availability. 

The shared subscription feature provides such capability. Clients can be divided into multiple subscription groups, and messages are still forwarded to all subscription groups, but only one client within each subscription group receives the message at a time.

You can try to create a shared subscription using the MQTT X Client by following the instructions in [Shared Subscription](./mqtt-shared-subscription.md).

To learn more about shared subscription technologies, see [Shared subscription - MQTT 5.0 new features](https://www.emqx.com/en/blog/introduction-to-mqtt5-protocol-shared-subscription).

## System Topic

Topics prefixed with `$SYS/` are reserved for the server to publish specific messages, such as server uptime, client online/offline event notifications, and the current number of connected clients. These topics are commonly referred to as system topics, and clients can subscribe to these system topics to obtain information about the server.

For more information on system topic, see [Understanding MQTT Topics & Wildcards by Case](https://www.emqx.com/en/blog/advanced-features-of-mqtt-topics).