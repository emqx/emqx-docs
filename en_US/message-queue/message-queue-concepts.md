# Message Queue

The Message Queue feature introduced in EMQX 6.0 extends the MQTT subscribe/publish pattern with durable queue semantics, enabling reliable, asynchronous message delivery. It enhances native MQTT capabilities with features commonly found in enterprise-grade message queues, such as RabbitMQ, without requiring additional infrastructure.

This page provides a complete overview of the Message Queue feature in EMQX, covering its design motivation, key concepts, internal architecture, message flow, and real-world application scenarios.

## What is a Message Queue?

A Message Queue in EMQX is a durable, server-side buffer that holds MQTT messages independently of subscriber availability. Each queue is associated with a specific topic filter, and automatically stores all messages that match the filter during its lifetime. 

Unlike traditional MQTT behavior, Message Queues persist messages even when no clients are online. Clients can consume these messages by subscribing to the special `$q/{topic}` format.

## Why Use the Message Queue?

MQTT is a lightweight and widely adopted publish/subscribe protocol. However, its default behavior tightly couples message delivery to subscriber availability, which can be limiting for asynchronous or delayed-consumption scenarios.

### Limitations of MQTT

While MQTT supports some queue-like features through [shared subscriptions](../messaging/mqtt-shared-subscription.md) (`$share/{group}/topic`), it has limitations:

- **Messages are not retained** if no subscribers are online.
- **No built-in support for TTL**, queue size limits, or overflow control.
- **No message deduplication**, such as keeping only the latest value per key.
- **No explicit lifecycle management** for queues.

These limitations make it difficult to implement patterns like:

- Sending commands to devices before they come online.
- Submitting tasks to workers who are not always connected.
- Retaining only the latest state or configuration update.

### Extend MQTT with Message Queue

Message Queue extends the MQTT protocol in EMQX. It allows messages to be persisted regardless of the subscribers' online status for further processing. It offers:

- **Persistent message storage (even when clients are offline)**: While queues are not strictly ordered, they are designed for reliable and asynchronous delivery, bridging the gap between lightweight MQTT communication and more advanced enterprise messaging needs.
- **Explicit queue declaration and property configuration**: Each queue has a configurable lifecycle, with support for TTL (time-to-live), size limits, and dispatch strategies, allowing fine-grained control over how messages are retained and delivered.
- **Optional Last-Value Semantics**: Messages with the same `mq-key` overwrite previous ones, ideal for retaining only the latest state or configuration update.

## Message Queue Key Concepts

- **Queue Name**
   An MQTT topic or topic filter that identifies the queue. Messages published to matching topics are automatically enqueued.
- **Queue Declaration**
   The process of creating a durable queue and defining its behavior through configurable properties.
- **Queue Deletion**
   The removal of a queue along with all its stored messages.
- **Last-Value Semantics**
   An optional feature enabled by setting a *Queue Key Expression* during queue declaration. When enabled, the broker expects each message to include an `mq-key` user property. Messages with the same key will replace the previous unconsumed message in the queue, ensuring that only the most recent value per key is retained. This behavior is ideal for stateful messaging or configuration updates, where only the latest value matters and older messages can be safely discarded.
- **Topic Prefix**
   Queue subscriptions use the special `$q/{topic}` prefix to distinguish them from regular MQTT subscriptions.
- **Queue Properties**
   Customizable settings that control queue behavior, such as message retention time, maximum length, and dispatch strategy.
- **Quality of Service (QoS)**
   Supports MQTT QoS 0 (at-most-once) and QoS 1 (at-least-once). Messages published with QoS 2 are automatically downgraded to QoS 1. Likewise, subscribers requesting QoS 2 will be granted QoS 1.
- **Message Persistence**
   Messages are retained even when no subscribers are connected. By default, queues apply last-value semantics. For regular queues (without a key expression), messages are stored in the order received.

## How Message Queue Works

The EMQX Message Queue feature is implemented as a loosely coupled extension and intercepts publish and subscribe operations using internal hooks. These hooks interact with a registry and storage layer to persist and deliver messages reliably.

### Main Components

The following main components are involved:

- **Message Queue Registry**
  Manages the lifecycle of all message queues. Responsible for creating, deleting, and looking up queues.
- **Message Queue Message DB**
  Stores the actual messages published to queues. Built on EMQX’s [Durable Storage](../durability/durability_introduction.md#durable-storage-architecture).
- **Message Queue State Storage**
  Persists consumption progress and queue metadata (e.g., TTL, properties).
- **Message Queue Consumer**
  Retrieves messages from the queue and dispatches them to connected subscribers based on the dispatch strategy.
- **Message Queue Subscription Registry**
  Tracks which channels (clients) are subscribed to which queues. Stores subscription state in each channel’s context.
- **Message Queue Hooks**
  Hook into publish and subscribe events to intercept messages and route them to queues or consumers.

### Schematic View

The diagram below shows the data flow between major Message Queue components:

```
+-----------------------+                                     +-----------------------------+
| Message Queue DS DB   |                                     | Message Queue State Storage |
+-----------------------+                                     +-----------------------------+
      ^      ^                                                    ^                      ^
      |      |                                                    | persist              |
      |      |                                                    | progress             |
      |      |              subscription on topic data            |                      |
      |      |                  via emqx_ds_client          +-------------+              |
      |      +--------------------------------------------->| MQ Consumer |              |
      |                                                     |             |              |
      | write tx                                            +-------------+              | message metadata
      |                                                           ^                      | persist/lookup
      |                                                           | proto                |
      |                                                           V                      |
+---------------------------+                         +----------------------------+     |
| Channel (writing)         |                         | Channel (subscribing)      |     |
|                           |                         | [MQ subscription registry] |     |
+---------------------------+                         +----------------------------+     |
      |                                                                    |             |
      |                                                              queue |             |
      |                                                             lookup V             |
      |                                                                  +--------------------------+
      |        fast queue lookup in the index                            | MQ Registry              |
      +----------------------------------------------------------------> |                          |
                                                                         +--------------------------+
```

### Publishing Workflow

1. A client publishes a message to a regular topic, such as `some/topic`.
2. An internal MQ hook is triggered to process the message.
3. The hook checks the Message Queue Registry for any queues whose topic filter matches the published topic.
4. If a matching queue is found, the message is written to the queue’s message database.

### Subscribing & Consuming Workflow

1. A client subscribes to a topic.
2. An MQ hook is triggered to handle the subscription.
3. If the topic is a message queue topic (`$q/some/topic`), the hook initializes a subscription in the channel's state and initiates a connection to the Message Queue Consumer.
4. If no consumer exists for the queue, a new Message Queue Consumer is started.
5. The consumer restores message consumption progress and starts to fetch data from the message database.
6. The Consumer dispatches received messages to the subscriber channels based on the configured dispatch strategy.
7. Subscribers' channels deliver the messages to the clients via standard MQTT mechanisms.

## Core Features

The Message Queue feature in EMQX provides a set of core capabilities that enable reliable, decoupled, and configurable message delivery.

- **Enqueueing Messages**
  Messages published to topics matching a declared queue are automatically enqueued. 

  If the queue is configured with a Key Expression (for last-value semantics), the broker evaluates the expression against each message:

  - If a key is derived, it replaces any unconsumed message with the same key.
  - If no key is defined or resolved, messages are enqueued in FIFO (first-in, first-out) order.

- **Dequeueing Messages**
  Subscribed clients receive messages from the queue according to the configured dispatch strategy. Acknowledgments (for QoS 1) trigger message removal from the queue.

- **Quality of Service (QoS)**

  - Supports QoS 0 (at-most-once) and QoS 1 (at-least-once).
  - QoS 2 messages are automatically downgraded to QoS 1 upon publishing or subscription.

- **Dispatch Strategies**
   Defines how messages are distributed across subscribers:

  - `random`: Distribute randomly.
  - `round_robin`: Rotate among available subscribers.
  - `least_inflight`: Prefer subscribers with fewer in-progress messages.

- **Queue Management**
   Full queue lifecycle operations (create, update, delete, query) are available via REST APIs.

## Use Cases

Message Queue enables reliable, asynchronous messaging patterns that are critical in many IoT and event-driven application scenarios, especially where devices or consumers may not always be online.

- **Device Command Queuing**: Cloud applications queue commands for IoT devices, ensuring commands will not be lost when devices are offline.
- **Batch Processing**: Break large datasets or workloads into smaller tasks and distribute them to worker clients for parallel or delayed processing.
- **Sensor Data Processing**: Temporarily queue high-frequency sensor data for batch processing, aggregation, or analysis at a later time.
- **Latest Configuration Dispatch**: Ensure devices always attempt to fetch and process the latest configuration command; older, unhandled commands (for the same config item/key) are superseded or marked obsolete in the queue.

## Reference & Related Features

- [Shared Subscriptions](#) – Compared with Message Queues
- MQTT Durable Sessions

## What's Next
