# Publish/Subscribe

As a world-class MQTT broker, EMQX supports the [publish/subscribe messaging pattern](./mqtt-concepts.md/#publish-subscribe-pattern), which is a key feature of the MQTT protocol. EMQX's publish/subscribe functionality offers a variety of features that make it well-suited for complex and high-performance messaging applications. These features include support for wildcard topics, topic-based message filtering, message persistence, and Quality of Service (QoS) settings. 

The publish function allows devices that are connected to the EMQX broker to send messages to a particular topic. The message can contain any type of data, such as sensor readings, status updates, or commands. When a device publishes a message to a topic, EMQX receives the message and forwards it to all devices that have subscribed to that topic.

The subscribe function in EMQX allows devices to receive messages from a particular topic. Devices can subscribe to one or more topics, and they will receive all messages that are published on those topics. This allows devices to monitor specific events or data streams in real-time, without having to constantly poll for updates.

<img src="./assets/pub-sub-pattern.png" alt="pub-sub-pattern" style="zoom:35%;" />

In this chapter, you will learn the [MQTT Core Concepts](./mqtt-concepts.md). You will also learn how to try the publish/subscribe function in EMQX and also try the following MQTT-specific features using the MQTT client tools.

- [Shared Subscription](./mqtt-shared-subscription.md)
- [Retained Message](./mqtt-retained-message.md)
- [Will Message](./mqtt-will-message.md)

Except for the MQTT-specific features, some extended features are also implemented in EMQX. This chapter also introduces the following extended features and how to configure them in EMQX Dashboard and test them using the client tools:

- [Exclusive Subscription](./mqtt-exclusive-subscription.md)
- [Delayed Publish](./mqtt-delayed-publish.md)
- [Auto Subscribe](./mqtt-auto-subscription.md)





