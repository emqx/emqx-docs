# MQTT Advanced

## Retained Messages

MQTT Retained Messages enable brokers to store the most recent message for each topic, allowing newly connected subscribers to instantly receive the latest available data, without waiting for the next publish event. This mechanism is particularly valuable in scenarios like smart homes and industrial IoT, where real-time state awareness is essential even if data updates are infrequent.

EMQX offers full support for retained messages in compliance with MQTT 5.0. It allows users to view, manage, and delete retained messages through an intuitive Dashboard or via management APIs. Users can configure storage modes (memory or disk), message expiration intervals, and maximum retained entries to align with system reliability and persistence requirements.

To try it out, simply run `docker run -d --name emqx ...` and access the built-in Dashboard to manage retained messages effortlessly. For more advanced use cases, EMQX also supports MQTT features such as session persistence, wildcard subscriptions, and message expiry, making it an ideal platform for building robust MQTT-based applications.

**Read more:** [MQTT Retained Messages: Beginner's Guide with Example](https://www.emqx.com/en/blog/mqtt5-features-retain-message#mqtt-retained-messages-in-emqx)

## Will Messages

The MQTT Will Message is a crucial feature for handling unexpected client disconnections, allowing for graceful actions like notifying other clients or switching backup devices. When a client connects, it can specify a Will Message, which is sent by the server if the client disconnects unexpectedly, without sending a proper DISCONNECT packet. This mechanism helps monitor client status and ensure reliability in IoT applications.

In MQTT 5.0, the Will Delay Interval was introduced to delay the Will Message's publication, reducing unnecessary notifications during temporary network issues. The message is stored in the server session, and its delivery depends on whether the session expires or the delay interval is met first. EMQX supports Will Message features, including retaining the message for future subscribers and integrating with session expiry notifications for enhanced client status monitoring. This functionality is integral for building resilient, real-time IoT applications, ensuring that system operators are informed of any device or client status changes promptly.

**Read more:** [MQTT Will Message (Last Will & Testament) Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/use-of-mqtt-will-message)

## Request / Response

MQTT 5.0 improves the Request/Response pattern with three key features: **Response Topic**, which allows requesters to specify a unique topic for responses, reducing conflicts; **Correlation Data**, which helps match responses to requests, even in asynchronous or multiple-responder scenarios; and **Response Information**, which facilitates topic permission management by allowing the requester to receive server-specific information for building response topics. These features ensure more reliable and organized communication, especially in complex IoT environments.

**Read more:** [MQTT Request / Response Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-request-response#suggestions-for-using-mqtt-request-response)

## User Properties

User Properties in MQTT 5.0 enable clients to attach custom metadata to messages using key-value pairs, similar to HTTP headers. This greatly enhances protocol flexibility by supporting use cases like file transfer, resource format identification, and intelligent message routing. User Properties can be added when connecting, publishing, subscribing, or disconnecting, allowing seamless metadata exchange between clients and brokers. For instance, they can carry file info, data format, or region tags, helping servers process messages efficiently and enabling traceable, application-level routing. EMQX fully supports User Properties, offering rich compatibility in clients like MQTT.js and the upcoming MQTTX.

Read more: [User Properties - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-user-properties)

## Topic Alias

Topic Alias allows users to reduce the possibly long and repeatedly used topic name to a 2-byte integer, so as to reduce the bandwidth consumption when publishing messages.

EMQX supports Topic Alias, optimizing message sizes and improving efficiency, especially in scenarios with limited bandwidth.

Read more: [Topic Alias - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-topic-alias)

## Payload Format Indicator & Content Type

Payload Format Indicator and Content Type are two key MQTT 5.0 properties that make message parsing more transparent. The former identifies whether the payload is binary or UTF-8 text, while the latter describes the payload format using MIME types (e.g., `application/json`).

Together, they help subscribers efficiently interpret messages and allow flexible content handling without relying on topic naming conventions. EMQX supports both properties, enabling smarter payload processing in diverse IoT and messaging applications.

Read more: [Introduction to MQTT Payload Format Indicator and Content Type | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-new-features-payload-format-indicator-and-content-type)

## Shared Subscriptions

Shared Subscriptions in MQTT 5.0 enable multiple clients to share the consumption of messages from a single topic, ensuring balanced load distribution and improved system scalability. EMQX fully supports this feature and even extends compatibility to MQTT 3.1.1 clients, allowing existing devices to benefit from shared consumption without code changes—just by using the `$share/{group}/{topic}` format.

This mechanism enhances throughput, prevents single-client bottlenecks, and ensures high availability. With built-in support for multiple load balancing strategies like round robin, hash, and local-first, EMQX helps developers flexibly manage traffic distribution across clustered environments.

Read more: [MQTT Shared Subscriptions: Practical Guidelines and Use Cases | MQTT 5 Features](https://www.emqx.com/en/blog/introduction-to-mqtt5-protocol-shared-subscription)

## Subscription Options

MQTT Subscription Options empower clients to customize how they receive messages. MQTT 5.0 introduces four key options—QoS, No Local, Retain As Published, and Retain Handling—to enhance flexibility and control. These options allow users to manage message quality, avoid message loops in bridging, preserve the Retain flag, and decide when to receive retained messages.

EMQX fully supports all MQTT 5.0 subscription options, enabling fine-grained control over message delivery behavior. For example, it helps users prevent message storms in bridge setups with No Local, and ensures consistent retained message handling across brokers with Retain As Published.

Read more: [MQTT Subscription Options Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/an-introduction-to-subscription-options-in-mqtt)

## Subscription Identifier

The Subscription Identifier in MQTT 5.0 assigns a unique identifier to each subscription, allowing clients to manage and track multiple subscriptions efficiently. This feature is particularly useful in complex applications with numerous subscriptions.

EMQX fully supports this feature, allowing clients to accurately identify which subscription a message originates from, even in cases of wildcard or overlapping subscriptions. By including Subscription Identifiers in PUBLISH packets, EMQX eliminates the need for topic filter matching at the client side, significantly improving message processing efficiency and enabling precise callback execution.

Read more: [MQTT Subscription Identifier Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/subscription-identifier-and-subscription-options)

## Keep Alive

The MQTT Keep Alive mechanism prevents half-open TCP connections by requiring the client to send regular packets within a set interval. If no data is sent, a `PINGREQ` is used to confirm connectivity. EMQX fully supports both client-specified and server-enforced Keep Alive values, including MQTT 5.0’s Server Keep Alive feature. Through configuration fields like `server_keepalive` and `keepalive_backoff`, EMQX allows fine-grained control over connection timeouts, enhancing reliability in IoT deployments and ensuring timely Will Message delivery when clients disconnect unexpectedly.

Read more: [What is the MQTT Keep Alive parameter for?](https://www.emqx.com/en/blog/mqtt-keep-alive)

## Message Expiry Interval

Message Expiry Interval in MQTT 5.0 allows clients to set a time limit for how long a message should be stored by the broker if it cannot be delivered immediately. After the interval expires, the message is discarded, preventing outdated information from being sent.

EMQX fully supports this feature, including decrementing the expiry interval during message forwarding or bridging, ensuring message timeliness across distributed deployments.

Read more: [Introduction to MQTT Message Expiry Interval | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt-message-expiry-interval)

## Maximum Packet Size

The Maximum Packet Size property in MQTT 5.0 allows clients and servers to negotiate a safe upper limit for packet size, preventing resource exhaustion on constrained devices. Clients declare their receiving limit in the `CONNECT` packet, while servers respond with their limit via the `CONNACK` packet. EMQX enforces this bi-directional constraint and ensures protocol compliance, dropping oversized messages or stripping low-priority metadata (like User Properties) from response packets to maintain connection stability. In shared subscription scenarios, EMQX can redirect oversized messages to eligible group members.

Read more: [MQTT Maximum Packet Size Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/best-practices-of-maximum-packet-size-in-mqtt)

## Reason Codes

MQTT 5.0 introduces a comprehensive Reason Code system that significantly enhances protocol feedback compared to MQTT 3.1.1’s limited status responses. These Reason Codes provide detailed results for connection, publish, subscribe, and other operations, enabling developers to quickly diagnose issues and optimize device management and message interactions.

EMQX fully supports MQTT 5.0 Reason Codes, empowering users with precise error handling and intelligent operation management to improve IoT system stability and responsiveness.

Read more: [MQTT Reason Code Introduction and Quick Reference](https://www.emqx.com/en/blog/mqtt5-new-features-reason-code-and-ack)

## Enhanced Authentication

MQTT 5.0 introduces Enhanced Authentication, a more secure framework beyond simple password authentication, addressing its vulnerabilities, such as plaintext password transmission and lack of mutual identity verification. Enhanced Authentication leverages the AUTH packet to support multiple message exchanges and SASL mechanisms like DIGEST-MD5, SCRAM, and Kerberos.

EMQX supports SCRAM, enabling users to choose stronger, flexible authentication methods to safeguard their IoT infrastructures.

Read more: [Leveraging Enhanced Authentication for MQTT Security](https://www.emqx.com/en/blog/leveraging-enhanced-authentication-for-mqtt-security)

## Control Packets

MQTT control packets define how clients and brokers communicate, including 15 types covering operations like connecting, publishing, and subscribing. Each packet includes a Fixed Header, optional Variable Header, and optional Payload, structured for lightweight and efficient data exchange. MQTT 5.0 further enhances flexibility by introducing properties and improved authentication.

Read more: [MQTT Control Packets: A Beginner's Guide](https://www.emqx.com/en/blog/introduction-to-mqtt-control-packets)

For more information about MQTT, please visit: [MQTT Guide 2025: Beginner to Advanced](https://www.emqx.com/en/mqtt-guide)