# Message Queue

Message Queue is a feature that extends MQTT subscribe/publish pattern.

## Why Message Queue?

MQTT is a publish/subscribe messaging protocol with wide adoption for device management.

In many MQTT applications, we need to decouple the lifetime of messages from the lifetime of publishers and subscribers.

For example, we may need to configure a device before it appears online. Another example is a work queue — we want to submit some tasks to be executed by workers without waiting for the workers to be online.

Message Queue is a feature that extends MQTT subscribe/publish pattern to solve these kinds of problems.

It allows messages to be persisted regardless of the subscribers' online status for further processing, individually or cooperatively.

## Key Concepts

* Queue Name: An MQTT topic or topic-filter that identifies the queue. Messages published to matching topics are automatically queued into the queue.

* Queue Declaration: The process of creating a durable queue and setting its properties.

* Queue Deletion: Removal of a queue and its messages.

* Last Value semantic: An optional feature enabled by defining a Queue Key Expression during queue declaration.

* Topic Prefix: Uses `$q/{name}` prefix to identify queue subscriptions.

* Queue Properties: Configurable attributes including data retention period and dispatch strategy.

* QoS Levels: Primarily supports QoS 0 (at-most-once) and QoS 1 (at-least-once). QoS 2 messages published to a queue are typically downgraded to QoS 1. Subscribers attempting QoS 2 subscriptions are also granted QoS 1.

* Persistence: Messages persist even when no subscribers are online. Last Value semantic is the default behavior for queues. In a Last Value Queuue, the latest message will overwrite the previous messages from the same topic with the same key. For regular queues (without Last Value semantic), all messages are written directly to the queue.

## Core Features

* Enqueue: Messages published to matching topics are automatically queued. If the queue was declared with a Compaction Key Expression, the broker evaluates this expression against the incoming message. If a key is successfully derived, the broker will replace any existing unconsumed message with the same key. If the expression fails to resolve a key, or if no expression was defined for the queue, the message is enqueued directly following FIFO principles.

* Dequeue: Subscribers receive messages based on the dispatch strategy

* QoS Support: Supports QoS 0 and 1. The publisher's original QoS 2 request is fully honored. When delivering a message from the persistent Queue, if a subscriber requested QoS 2, the broker may downgrade it to QoS 1 (depends on the final implementation design).

* Dispatch Strategies: random, round_robin, least_inflight

* Management: REST APIs for queue CRUD operations and configuration.

## Use Cases

Device Command Queuing: Cloud applications queue commands for IoT devices, ensuring commands aren't lost when devices are offline

Batch Processing: Large datasets split into smaller tasks distributed across multiple workers

Sensor Data Processing: Queue sensor readings for batch processing and analysis

Latest Configuration Dispatch: Ensure devices always attempt to fetch and process the latest configuration command; older, unhandled commands (for the same config item/key) are superseded or marked obsolete in the queue.

## How Message Queue Works?

### Publishing

* A client publishes a message to `some/topic`.
* An MQ hook is triggered to handle the message publication.
* The hook looks up in the MQ registry if there are any Message Queues whose topic filter matches the message topic.
* If yes, the hook writes the message to the corresponding Message Queues.

### Subscribing/Consuming

* A client subscribes to some topic.
* An MQ hook is triggered to handle the subscription.
* If the topic is a Message Queue topic (`$q/some/topic`), the hook initializes a subscription in the Channel's state and
initiates a connection to the Message Queue Consumer.
* If a Consumer is not yet found, a new consumer is started.
* The Consumer restores message consumption progress and starts to fetch data from the Message Queue message database.
* The Consumer dispatches received messages to the connected subscribers.
* The subscribers (channels) deliver MQ messages to the clients.

## Enable/Create Message Queue

### Via API



### Explicitly Declare a Queue

To be done.

## Configure Message Queue

### Dashboard

MQTT Settings -> Message Queue



### Configuration File

```hocon
mq {
    ## The interval at which the Message Queues will clean up expired messages.
    gc_interval = 1h
    ## The maximum retention period of messages in regular Message Queues.
    regular_queue_retention_period = 1d
    ## The interval at which subscribers will retry to find a queue if the queue is not found
    ## when subscribing to a queue topic.
    find_queue_retry_interval = 10s
    ## Settings for the database storing the Message Queue state.
    ## See Durable Storage configuration for more details.
    state_db {
        transaction {
            flush_interval = 10
            idle_flush_interval = 5
            conflict_window = 5000
        }
    }
    ## Settings for the database storing the Message Queue messages.
    ## See Durable Storage configuration for more details.
    message_db {
        transaction {
            flush_interval = 100
            idle_flush_interval = 20
            conflict_window = 5000
        }
    }
}
```

## Manage Message Queue

### Dashboard

### REST API

## FAQ & Troubleshooting (Optional but Recommended)

- Why messages aren’t enqueued.

- What happens when queues overflow?

## Reference & Related Features

- [Rule Engine](#) – To route queued messages
- [Shared Subscriptions](#) – Compared with Message Queues
- [EMQX REST API Reference](#)
