# MQTT Basics

## What is MQTT and Why Does It Matter

MQTT (Message Queuing Telemetry Transport) is a lightweight, publish-subscribe messaging protocol optimized for high-latency or unreliable networks—perfect for IoT scenarios. It enables devices to communicate in real-time using a broker model, making it a standard for smart homes, industrial automation, and connected vehicles.

**Read more:** [Mastering MQTT: The Ultimate Beginner's Guide for 2025](https://www.emqx.com/en/blog/the-easiest-guide-to-getting-started-with-mqtt)

## MQTT Publish-Subscribe Pattern

The publish-subscribe pattern in MQTT decouples message producers (publishers) from consumers (subscribers). Publishers send messages to topics without knowledge of subscribers, and subscribers receive messages from topics without knowledge of publishers. This model enhances scalability and flexibility in message distribution.

**Read more:** [Introduction to MQTT Publish-Subscribe Pattern](https://www.emqx.com/en/blog/mqtt-5-introduction-to-publish-subscribe-model)

## How to Establish an MQTT Connection?

Establishing an MQTT connection involves a client initiating a connection to a broker, optionally providing authentication credentials, and specifying connection parameters like the keep-alive interval and clean session flag. Proper configuration ensures reliable and secure communication.

**Read more:** [How to Set Parameters When Establishing an MQTT Connection?](https://www.emqx.com/en/blog/how-to-set-parameters-when-establishing-an-mqtt-connection)

## MQTT Topics and Wildcards

MQTT topics define the routing structure for publish/subscribe messaging. By using hierarchical topic levels and wildcards (`+` for single-level, `#` for multi-level), developers can flexibly subscribe to entire device groups or sensor types. EMQX fully supports MQTT standard topic matching and enhances it with powerful features such as wildcard subscription management, shared subscriptions, and fine-grained access control. Understanding how topics and wildcards work is key to building scalable, secure, and efficient IoT applications with EMQX.

**Read more:** [MQTT Topics and Wildcards: A Beginner's Guide](https://www.emqx.com/en/blog/advanced-features-of-mqtt-topics)

## MQTT Session

MQTT sessions help maintain client context across network interruptions, addressing issues like missed messages or re-subscription overheads. Sessions store critical data such as client subscriptions and QoS messages, allowing seamless communication even after disconnections. MQTT 5.0 introduces key parameters like **Clean Start** and **Session Expiry Interval** to control session lifecycles, offering flexibility in session retention. Clean Start determines whether to reuse an existing session, while Session Expiry Interval defines how long sessions are kept after disconnections. EMQX enhances session management by providing fine-tuned control over session expiration, improving resource efficiency and system reliability, especially for IoT scenarios with intermittent connectivity.

**Read more:** [Introduction to MQTT Clean Start and Session Expiry Interval | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-new-feature-clean-start-and-session-expiry-interval)

## MQTT QoS and Protocol Flows

MQTT defines three Quality of Service (QoS) levels to balance message delivery reliability and network efficiency:

- QoS 0: At most once delivery
- QoS 1: At least once delivery
- QoS 2: Exactly once delivery

Each level dictates the protocol flow between client and broker to ensure the desired delivery guarantee.

**Read more:** [MQTT QoS 0, 1, 2 Explained: A Quickstart Guide](https://www.emqx.com/en/blog/introduction-to-mqtt-qos)