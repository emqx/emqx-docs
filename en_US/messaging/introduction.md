# Publish/Subscribe

The MQTT protocol is based on the publish(pub)/subscribe(sub) pattern, which is different from the traditional client/server pattern. Pub/sub is a messaging pattern in which senders, or publishers, do not send messages directly to specific receivers, or subscribers. Instead, publishers categorize messages into topics, and subscribers subscribe to specific topics that they are intereste in. When a publisher sends a message to a topic, the broker then delivers the message to all the subscribers that have expressed interest in that topic.

<img src="./assets/pub-sub-pattern.png" alt="pub-sub-pattern" style="zoom:35%;" />

EMQX Enterprise's publish/subscribe functionality offers a variety of features that make it well-suited for complex and high-performance messaging applications. These features include support for wildcard topics, topic-based message filtering, message persistence, and Quality of Service (QoS) settings. 

This chapter introduces you with the [MQTT Core Concepts](./mqtt-concepts.md)  and provides instructions on how to try the publish/subscribe function using the client tools. It also introduces how to use the client tools to try the  following MQTT specific features.

- [Shared Subscription](./mqtt-shared-subscription.md)
- [Retained Message](./mqtt-retained-message.md)
- [Will Message](./mqtt-will-message.md)

Except for the MQTT specific features, EMQX also implements some extended features. In this chapter, it also introduces the following extended features and how to configure them in Dashboard and test them using the client tools:

- [Exclusive Subscription](./mqtt-exclusive-subscription.md)
- [Delayed Publish](./mqtt-delayed-publish.md)
- [Auto Subscribe](./mqtt-auto-subscription.md)





