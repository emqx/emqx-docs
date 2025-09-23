# Message Queue User Guide

This page walks you through the practical usage of the Message Queue feature in EMQX, from creating queues to configuring their behavior and managing them using the Dashboard, REST API, or configuration files.

## Create Message Queue via Dashboard

Message Queues must be explicitly declared/created before they can store or dispatch messages.

To create a new Message Queue using the EMQX Dashboard:

1. Navigate to **Message Queue** in the left menu.

2. Click the **Create** button in the upper-right corner of the page.

3. In the **Create Message Queue** dialog:

   - **Topic Filter**: Enter the topic or topic filter (e.g., `t/1`).  It defines which published messages are enqueued based on topic matching. A queue will collect all messages that match this topic filter.

     To consume messages from the queue, clients must subscribe to the topic using the `$q/{Topic Filter}` format.

   - **Dispatch Strategy**: Select how messages should be distributed among subscribers. Available strategies include:

     - `Least Inflight Subscriber`: Prefer subscribers with the fewest unacknowledged messages.
     - `Random`: (default) Select a subscriber at random.
     - `Round Robin`: Rotate delivery evenly across all subscribers.

   - **Data Retention Period**: Specify how long messages should be retained in the queue. You can set the time unit (e.g., days).

   - **Last Value Semantics**: Toggle this switch **on** if you want the queue to overwrite older messages with the same key, keeping only the latest value per key.

   - **Queue Key Expression**: When Last-Value Semantics is enabled, this field defines the expression used to extract the key from each message (e.g., `message.from`). The key is used to determine whether a new message should replace an existing one.

4. Click **Create** to save the queue.

The new queue will appear in the Message Queue list, showing its topic filter, dispatch strategy, last-value semantics status, and data retention period. You can edit or delete queues using the buttons in the **Actions** column.

## Configure Message Queue Settings

This section explains how to configure global settings that apply to all Message Queues in EMQX. These settings control message retention, cleanup intervals, and internal queue behavior. You can configure them via the Dashboard, REST API, or configuration file.

### Dashboard

You can update Message Queue settings directly from the EMQX Dashboard without restarting the broker. This is useful for making changes to system-wide behavior at runtime.

To configure global settings for Message Queues via the Dashboard:

1. Navigate to the **Message Queue** page from the left menu.
2. Click the **Settings** button in the top-right corner of the page.
3. You will be redirected to the **MQTT Settings** -> **Message Queue** tab. In this panel, you can configure the following parameters:
   - **GC Interval**: The interval at which expired messages are cleaned up from queues.
   - **Regular Queue Retention Period**: The maximum duration for which messages are retained in regular queues.
4. After making changes, click **Save Changes** to apply the new settings.

These settings apply to all regular (non-last-value) message queues.

### REST API

You can also configure global Message Queue settings via the REST API. These settings apply system-wide and affect how all queues are managed internally.

```bash
curl -v -u key:secret -X PUT -H "Content-Type: application/json" http://localhost:18083/api/v5/message_queues/config -d '{"find_queue_retry_interval": "10s", "gc_interval": "1h", "regular_queue_retention_period": "7d"}'
```

### Configuration File

For persistent and version-controlled configuration, you can define Message Queue settings in the EMQX configuration file (`emqx.conf`). Below is an example with key settings:

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

#### Configuration Descriptions

- **`gc_interval`**:
  Defines how often EMQX scans message queues to remove expired messages.
- **`regular_queue_retention_period`**:
  Sets the maximum time that messages are retained in a regular queue (not using Last-Value semantics). After this period, messages will be purged.
- **`find_queue_retry_interval`**:
  Determines how frequently a subscriber retries to locate a queue when subscribing to a `$q/` topic that does not yet exist.
- **`state_db`**:
  Configuration for the **Message Queue State Database**, which stores queue metadata (like properties and consumption progress).
  - `flush_interval`: Commit interval for active transactions.
  - `idle_flush_interval`: Commit interval for idle transactions.
  - `conflict_window`: Time window (in ms) used for conflict detection during concurrent writes.
- **`message_db`**:
  Configuration for the **Message Queue Message Database**, where the actual messages are stored.
  - `flush_interval`: Commit interval for message write transactions.
  - `idle_flush_interval`: Commit interval for idle message writes.
  - `conflict_window`: Time window (in ms) used for conflict detection on message writes.

## Manage Message Queue via REST API

EMQX provides a set of REST APIs to manage the lifecycle of Message Queues, including creation, retrieval, update, and deletion.

### Create a Message Queue

Create a new message queue by specifying the topic filter and queue properties such as whether to enable Last-Value Semantics:

```bash
curl -s -u key:secret -X POST -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues \
-d '{"topic_filter": "t1/#", "is_lastvalue": false}' | jq
```

### List All Message Queues

Retrieve the list of existing message queues:

```bash
curl -s -u key:secret -X GET -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues | jq
```

### Update a Message Queue

Update the properties of an existing queue, such as its dispatch strategy:

```bash
curl -s -u key:secret -X PUT -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues/t1%2F%23 \
-d '{"dispatch_strategy": "least_inflight"}' | jq
```

### Delete a Message Queue

Remove a message queue and all messages retained in it:

```bash
curl -s -u key:secret -X DELETE \
http://localhost:18083/api/v5/message_queues/t1%2F%23
```

> **Note:**
>
> - Topic filters in the URL must be URL-encoded (e.g., `t1/#` becomes `t1%2F%23`).
> - Authentication is required (`key:secret`).

## FAQ and Troubleshooting

### Why aren't messages being enqueued?

- Make sure the topic filter of the declared Message Queue matches the topic of the published message.
- Verify that the queue exists and is properly configured.
- Check the EMQX logs for relevant errors or warnings. Look specifically for entries with the `mq_` prefix to diagnose queue-related issues.

### What happens when queues exceed capacity?

- Currently, Message Queues are not limited by size (number of messages or total bytes), but they are time-limited via the configured *retention period*.
- Once messages expire (i.e., exceed the retention period), they are no longer eligible for delivery and will be automatically purged by the broker during regular garbage collection cycles.

