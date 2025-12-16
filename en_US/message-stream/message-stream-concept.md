# Message Stream

Message Stream is a streaming and replay feature introduced in EMQX 6.1. It extends MQTT’s real-time publish/subscribe model with persistent, replayable message streams, enabling Kafka-like streaming capabilities while preserving MQTT semantics.

This page provides a complete overview of the Message Stream feature in EMQX, covering its design motivation, key concepts, internal architecture, message flow, and real-world application scenarios.

## What Is Message Stream?

A Message Stream is a logical collection of MQTT messages that is automatically populated by messages matching a topic filter. It continuously collects MQTT messages that match a topic filter during its lifetime, stores them durably, and allows clients to consume historical messages by subscribing to a stream-specific topic.

Message Streams enable historical data replay and state-oriented messaging patterns while preserving MQTT semantics and client behavior.

## Why Use Message Stream?

MQTT is optimized for real-time messaging, but it has inherent limitations:

- Messages are typically delivered only to online subscribers.
- Historical data replay is not natively supported.
- Reprocessing past data requires external systems.
- Maintaining ordered, replayable event logs is difficult

Message Streams address these gaps by introducing:

- Durable message storage
- Consumer-controlled replay
- Time- and size-based retention
- Optional compaction (last-value semantics)
- Fine-grained consumption control

## Message Streams Concepts

- **Message Stream**

  A logical resource that stores messages from MQTT topics matching a specific topic filter. It is identified by that topic filter and has an explicit lifecycle. While it exists, it is automatically replenished with matching messages. The stored data is bounded by time or size limits.

  Messages in the stream can be replayed by subscribing consumers. Publishers do not need to be aware of the stream.

- **Topic Filter**

  An MQTT topic filter that defines which messages are captured into a stream, for example, `sensors/+/data`. Only messages matching the filter are ingested. A single message may be captured by multiple streams.

- **Stream Subscription**

  A special MQTT subscription used to consume messages from a stream. Clients subscribe using the `$s/<timestamp>/<topic_filter>` format. The timestamp specifies the starting point for replay. Stream subscriptions are independent of regular MQTT subscriptions and are handled by the External Subscription mechanism.

- **Key Expression**

  A user-defined expression evaluated on each incoming message to extract a key. The expression may reference message content or metadata. The extracted key determines per-key ordering and enables Last-Value semantics, where newer messages overwrite older ones with the same key.

## Message Streams Architecture

Message Streams are implemented as a standalone EMQX application that is loosely coupled with the broker core and reuses existing infrastructure. Integration with EMQX is achieved through internal hooks and the External Subscription framework, allowing stream messages to be delivered to MQTT clients without altering standard publish or subscribe behavior.

### Main Components

- **Streams Registry**: Responsible for managing the lifecycle of Message Streams. It creates, updates, deletes, and looks up streams, and maintains stream metadata and indexes. For efficient stream lookup, the registry relies on a Mnesia table to index streams by topic filter.
- **Streams Message Database**: Provides durable storage for stream messages. It is built on top of EMQX [Durable Storage](../design/durable-storage.md#design-for-durable-storage) and is responsible for persisting messages, enforcing retention limits, and applying Last-Value semantics when enabled.
- **Streams ExtSub Handler**: Integrates Message Streams with MQTT client sessions. It retrieves messages from Durable Storage and delivers them to subscribing clients through the External Subscription framework.
- **Durable Storage (DS)**： Provides persistence and efficient message retrieval. Stream messages remain available in storage until they expire according to retention policies.

Streams also reuse the quota and flow control mechanisms originally developed for Message Queues.

### Message Stream Data Flow Diagram

The following diagram shows the data flow between the Message Stream components:

```ascii
+-----------------------+
| Message Stream DS DB  |
+-----------------------+
      ^      ^
      |      |
      |      |
      |      |          subscription on topic data
      |      |             via emqx_ds_client
      |      +-------------------------------------+  +--------------------------------+
      |                                            |  | Channel (subscribing)          |
      | write tx                                   |  | +----------------------------+ |
      |                                            |  | | ExtSub                     | |
      |                                            |  | | +------------------------+ | |
      |                                            |  | | | Streams ExtSub Handler | | |
+---------------------------+                      +----->|                        | | |
| Channel (writing)         |                         | | +----------------|-------+ | |
|                           |                         | +------------------|---------+ |
+---------------------------+                         +--------------------|-----------+
      |                                                                    |
      |                                                             stream |
      |                                                             lookup V
      |                                                                  +--------------------------+
      |        fast stream lookup in the index                           | Streams Registry         |
      +----------------------------------------------------------------> |                          |
                                                                         +--------------------------+
```

### Publishing Flow

1. A client publishes a message to an MQTT topic
2. A Message Streams hook is triggered
3. The Streams Registry is consulted to find matching streams
4. Matching streams persist the message in Durable Storage

### Subscribing and Consuming Flow

1. A client subscribes to a stream topic (`$s/...`)
2. The ExtSub framework initializes a Streams ExtSub handler
3. The handler subscribes to Durable Storage
4. Messages are fetched according to retention and position rules
5. Messages are delivered to the client via standard MQTT delivery

## Message Streams Core Features

Message Streams provide a set of core capabilities that define how messages are stored, ordered, retained, and delivered for replay-based consumption.

- **Timestamp-Based Replay**

  Message Streams support replay starting from a specified timestamp. Consumers choose the timestamp when subscribing. Messages published before the timestamp are skipped—the stream’s retention policy limits replay.

- **Retention**

  Retention constraints bound Message Streams. Messages are retained for a limited time or size. Expired messages are removed automatically, regardless of whether they have been consumed.

- **Per-Key Ordering**

  Message Streams are not globally ordered. Messages that share the same key are delivered in strict publish order. Messages with different keys may be interleaved.

- **Last-Value Semantics**

  A stream may enable Last-Value semantics. Messages with the same key overwrite earlier messages. Only the most recent message per key is retained. Messages without a resolved key are stored normally.

- **MQTT-Native Delivery**

  Stream messages are delivered using standard MQTT mechanisms. Publishers do not need to change their behavior. Message delivery to subscribers is integrated through External Subscription.

## Typical Use Cases

- **Historical Data Replay**
   Reprocess past MQTT events for debugging or new business logic.
- **Time-Series Analysis**
   Store and replay sensor data for analytics and predictive maintenance.
- **Event Sourcing**
   Persist all state changes as an immutable event log.
- **IoT Digital Twins**
   Maintain the latest state of devices using compaction keys.
- **Configuration Synchronization**
   Ensure devices always receive the most recent configuration.

