# Publish/Subscribe

As a world-class MQTT broker, EMQX supports the [publish/subscribe messaging pattern](./mqtt-concepts.md#publish-subscribe-pattern), which is a key feature of the MQTT protocol. EMQX's publish/subscribe functionality offers a variety of features that make it well-suited for complex and high-performance messaging applications. These features include support for wildcard topics, topic-based message filtering, message persistence, and Quality of Service (QoS) settings. 

The **publish** function allows devices that are connected to the EMQX broker to send messages to a particular topic. The message can contain any type of data, such as sensor readings, status updates, or commands. When a device publishes a message to a topic, EMQX receives the message and forwards it to all devices that have subscribed to that topic.

The **subscribe** function in EMQX allows devices to receive messages from a particular topic. Devices can subscribe to one or more topics, and they will receive all messages that are published on those topics. This allows devices to monitor specific events or data streams in real-time, without having to constantly poll for updates.

<img src="./assets/pub-sub-pattern.png" alt="pub-sub-pattern" style="zoom:35%;" />

## Extended Publish/Subscribe Capabilities

EMQX not only supports the native MQTT publish/subscribe model but also extends it with a number of powerful features to support more advanced and asynchronous messaging scenarios.

In this chapter, you will learn the [MQTT Core Concepts](./mqtt-concepts.md). You will also learn how to try the publish/subscribe function in EMQX and test the following MQTT-specific features using MQTT client tools:

- [Shared Subscription](./mqtt-shared-subscription.md)
- [Retained Message](./mqtt-retained-message.md)
- [Will Message](./mqtt-will-message.md)
- [Topic Wildcards](./mqtt-wildcard-subscription.md)

### MQTT Extensions in EMQX

In addition to the native MQTT features, EMQX provides several extended capabilities that build on or enhance the basic pub/sub model. These features are designed to handle advanced messaging patterns, offline consumption, topic management, and more:

- [Exclusive Subscription](./mqtt-exclusive-subscription.md)
- [Delayed Publish](./mqtt-delayed-publish.md)
- [Auto Subscribe](./mqtt-auto-subscription.md)
- [Topic Rewrite](./mqtt-topic-rewrite.md)
- [Message Queue](../message-queue/message-queue-concept.md)

### Message Queue (Advanced Feature)

[Message Queue](../message-queue/message-queue-concept.md) is an advanced feature introduced in EMQX 6.0 that extends the MQTT pub/sub model with durable and asynchronous message queuing capabilities. It allows messages to be stored even when subscribers are offline and supports advanced dispatch strategies and message compaction through Last-Value Semantics.

Unlike shared subscriptions or retained messages, Message Queues provide:

- Durable message persistence independent of client connection state
- Configurable queue lifecycle and message Time-to-Live (TTL)
- Queue-based delivery semantics with fine-grained control
