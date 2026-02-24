# Message Queue User Guide

This page walks you through the practical usage of the Message Queue feature in EMQX, from creating queues to configuring their behavior and managing them using the Dashboard, REST API, or configuration files.

## Manually Create Queues via Dashboard

Message queues must be explicitly declared/created before they can store or dispatch messages. You can create message queues either manually or automatically. For details about automatic creation, see [Automatically Create Message Queues via Dashboard](#automatically-create-message-queues-via-dashboard).

1. Navigate to **Queues** in the left menu.

2. Click the **Create** button on the page.

3. In the **Create Queue** dialog, configure the following options:

   - **Name**: Specifies the unique name of the queue. Queue names may contain only the following:

     - Alphanumeric characters (`A–Z`, `a–z`, `0–9`)
     - Underscores (`_`)
     - Hyphens (`-`)
     - Dots (`.`)

     The queue is identified and managed by this name.

     Clients can consume messages using the following subscription formats:

     - `$queue/<name>` is used when the queue already exists.
     - `$queue/<name>/<topic_filter>` is optional when subscribing to an existing queue. It can be used when auto-creation is enabled. If the queue does not yet exist, EMQX uses the provided `<topic_filter>` to create it automatically.

   - **Topic Filter**: Enter the topic or topic filter (e.g., `t/1`).  It defines which published messages are enqueued based on topic matching. A queue will collect all messages that match this topic filter.

     > The topic filter is part of the queue’s configuration but does not define the queue’s identity.

   - **Dispatch Strategy**: Select how messages should be distributed among subscribers. Available strategies include:

     - `Least Inflight Subscriber`: Prefer subscribers with the fewest unacknowledged messages.
     - `Random`: (default) Select a subscriber at random.
     - `Round Robin`: Rotate delivery evenly across all subscribers.

   - **Data Retention Period**: Specify how long messages should be retained in the queue. You can set the time unit (e.g., days).

   - **Last Value Semantics**: This option is enabled by default. When enabled, a new message with the same queue key will overwrite any previous, unconsumed message with that key in the same queue. This ensures only the most recent message per key is retained. The default key is the client ID of the message publisher.

     - **[Queue Key Expression](#queue-key-expression)**: When Last-Value Semantics is enabled, this field defines the expression used to extract the key from each message. The default value is `message.from`, which means the client ID of the message publisher. This field supports configuration using [Variform expressions](../configuration/configuration.md#variform-expressions).

   - **Max Shard Message Count**: (Optional) Sets the maximum number of messages allowed in each shard of the queue. You can toggle this setting on and enter a custom value, or leave it disabled to allow unlimited messages (`infinity`). This setting is persisted to durable storage.

   - **Max Shard Message Bytes**: (Optional) Sets the maximum total size (in bytes) of messages in each shard of the queue. You can toggle this setting on and enter a value (e.g., `200MB`), or leave it disabled for unlimited size (`infinity`). This setting is persisted to durable storage.

     ::: tip Performance Note

     Queues with size limits may have slower write performance, especially under high throughput conditions.

     :::

4. Click **Create** to save the queue.

The new queue will appear in the Message Queue list, showing its name, topic filter, dispatch strategy, last-value semantics status, and data retention period. You can edit queue settings or delete queues using the buttons in the **Actions** column.

### Queue Key Expression

The Queue Key Expression specifies how to extract the key used for message deduplication in Last-Value Semantics mode. This expression is evaluated against a message's metadata and follows the syntax of [Variform expressions](../configuration/configuration.md#variform-expressions).

The expression is evaluated against a message context that includes fields such as `from`, `topic`, `payload`, `headers.properties`, and more. For example, to use a user property as the key, you could set the expression to:

```
message.headers.properties.'User-Property'.user-prop
```

If the key cannot be extracted based on the expression (e.g., the field doesn't exist), the message will be discarded and not enqueued.

#### Message Context Example

Queue Key Expressions are evaluated against the following message structure:

<details>
<summary><strong>JSON Example</strong></summary>

```json
{
  "message": {
    "qos": 0,
    "topic": "some/topic",
    "payload": "some-payload",
    "headers": {
      "client_attrs": {},
      "proto_ver": 5,
      "properties": {
        "User-Property": {
          "user-prop": "some-value"
        }
      },
      "peerhost": "127.0.0.1",
      "username": "undefined",
      "protocol": "mqtt",
      "peername": "127.0.0.1:49352"
    },
    "from": "clientid",
    "timestamp": 1759238376252,
    "id": "..non utf8 bytes...",
    "flags": {
      "retain": false,
      "dup": false
    },
    "extra": {}
  }
}
```


</details>

<details> <summary><strong>Erlang Term Example</strong></summary>

```erlang
#{message =>
      #{extra => #{},
        flags => #{dup => false, retain => false},
        id => <<0,6,64,4,154,125,229,77,244,69,0,0,28,21,0,2>>,
        timestamp => 1759238376252, from => <<"clientid">>,
        headers =>
            #{peername => <<"127.0.0.1:49352">>, protocol => mqtt,
              username => undefined, peerhost => <<"127.0.0.1">>,
              properties =>
                  #{'User-Property' => #{<<"user-prop">> => <<"some-value">>}},
              proto_ver => 5, client_attrs => #{}
            },
        payload => <<"some-payload">>, topic => <<"some/topic">>,
        qos => 0
      }
    }
```

</details>

## Automatically Create Queues via Dashboard

Message queues can be automatically created when clients subscribe to a `$queue/`-prefixed topic. This allows queues to be provisioned dynamically without manual setup.

When auto-creation is enabled:

- Subscribing to `$queue/<name>` works only if the queue already exists.
- Subscribing to `$queue/<name>/<topic_filter>` allows EMQX to automatically create the queue using the provided `<topic_filter>` if it does not already exist.

Queues may be auto-created either as regular queues or last-value semantics queues. 

::: tip Note

To ensure proper queue behavior, you can enable either **Auto Create Regular Queue** or **Auto Create Last Value Semantics Queue**, but not both at the same time.

:::

### Auto Create Last Value Semantics Queue

This option is turned on by default in the **Queues** tab under **MQTT Settings**. It allows EMQX to automatically create queues that support Last-Value Semantics, where only the most recent message with a given key is retained.

1. Navigate to **Management** -> **MQTT Settings** -> **Queues** tab.

2. By default, **Enable Auto Create Queue** -> **Last Value Semantics Queue** is enabled.

   Configure the following:

   - **Queue Key Expression**: Required. Defines how to extract a unique key from each message (default: `message.from`).
   - **Dispatch Strategy**: Determines how messages are distributed to subscribers (default: `Random`).
   - **Data Retention Period**: Specifies how long messages should be retained in the queue.

3. Click **Save Changes**.

When a client subscribes to a topic such as `$queue/my_queue/test`, if `my_queue` does not already exist, EMQX automatically creates a Last-Value semantics queue named `my_queue` using `test` as its topic filter. The queue then appears in the **Queues** list.

### Auto Create Regular Queue

This option can be enabled manually if you prefer regular queues where messages are stored independently and not overwritten.

1. Go to **Management** -> **MQTT Settings** -> **Queues** tab.
2. Turn on **Enable Auto Create Queue** -> **Regular Queue**.
3. Configure the following:
   - **Dispatch Strategy**: Determines how messages are distributed to subscribers (default: `Random`).
   - **Data Retention Period**: Specifies how long messages should be retained in the queue.
4. Click **Save Changes**.

## Configure Queue Settings

This section explains how to configure global settings that apply to all message queues in EMQX. These settings control message retention, cleanup intervals, internal queue behavior, and queue auto-creation behavior. You can configure them via the Dashboard, REST API, or configuration file.

### Dashboard

You can update Message Queue settings directly from the EMQX Dashboard without restarting the broker. This is useful for making changes to system-wide behavior at runtime.

1. Go to **Management** -> **MQTT Settings** -> **Queues** tab.

   Alternatively, you can click the **Settings** button in the top-right corner of the **Queues** page.

2. In the **Queues** panel, the following configuration options are available:
   - **Enable Queues**: Enable the Message Queue feature.

     >  You cannot disable Queues via the Dashboard. To disable it, you must modify the configuration file directly.

   - **Max Queue Count**: Sets the maximum number of queues that can be created.

   - **GC Interval**: The interval at which expired messages are cleaned up from queues. Default is `1` hour.

   - **Regular Queue Retention Period**: The maximum duration for which messages are retained in regular queues. Default is `7` days.

   - **Find Queue Retry Interval**: When a client subscribes to `$queue/<name>` and the corresponding queue is not found, this setting controls how frequently the subscriber retries to locate the queue. Default is `10` seconds.

   - **Enable Auto Create Queue **: Enables automatic creation of queues when clients subscribe to queue topics and no matching queue exists.

   - **Auto Create Queue Type**: Specifies the type of queues to create automatically:

     - **Last Value Semantics Queue** (enabled by default): When a client subscribes to a `$queue/<name>/<topic_filter>` topic and no matching queue exists, EMQX automatically creates a queue with Last-Value Semantics enabled. 

       For details of the settings, see [Auto Create Last Value Semantics Queue](#auto-create-last-value-semantics-queue).

     - **Regular Queue**: When enabled, EMQX automatically creates regular (non-overwriting) queues for `$queue/<name>/<topic_filter>` subscriptions.

       For details of the settings, see [Auto Create Regular Message Queue](#auto-create-regular-message-queue).
   
3. After making changes, click **Save Changes** to apply the new settings.

### REST API

You can also configure global Message Queue settings via the REST API. These settings apply system-wide and affect how all queues are managed internally.

```bash
curl -v -u key:secret -X PUT -H "Content-Type: application/json" http://localhost:18083/api/v5/message_queues/config -d '{"find_queue_retry_interval": "10s", "gc_interval": "1h", "regular_queue_retention_period": "7d"}'
```

### Configuration File

For persistent and version-controlled configuration, you can define Message Queue settings in the EMQX configuration file (`emqx.conf`). Below is an example with key settings:

```hocon
mq {
    gc_interval = 1h
    regular_queue_retention_period = 1d
    find_queue_retry_interval = 10s
    max_queue_count = 100
    }
}
```

#### Configuration Descriptions

- **`gc_interval`**: Defines the interval at which the Message Queues will clean up expired messages.
- **`regular_queue_retention_period`**: Sets the maximum time that messages are retained in a regular queue. After this period, messages will be purged.
- **`find_queue_retry_interval`**: Determines how frequently a subscriber retries to locate a queue when subscribing to a `$queue/<name>` topic that is not yet found.
- **`max_queue_count`**: (Optional) Sets the maximum number of queues that can be created.

## Manage Queues via REST API

EMQX provides a set of REST APIs to manage the lifecycle of Message Queues, including creation, retrieval, update, and deletion.

::: tip Note

All REST API operations require appropriate authentication and permissions. For detailed request and response schemas, refer to the "Message Queue" section in [REST API](../admin/api.md).

:::

All examples below assume basic authentication using an API key and secret.

### Create a Queue

Create a new message queue by specifying the queue name, topic filter, and queue properties such as whether to enable Last-Value Semantics:

```bash
curl -s -u key:secret -X POST -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues \
-d '{"name": "my_queue", "topic_filter": "t1/#", "is_lastvalue": false, "limits": {"max_shard_message_count": 10000, "max_shard_message_bytes": "200MB"}}' | jq
```

The response includes the details of the newly created queue, including its `name` and configuration.

### List All Queues

Retrieve the list of existing message queues:

```bash
curl -s -u key:secret -X GET -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues | jq
```

### Update a Queue

Update the properties of an existing queue, such as its dispatch strategy:

```bash
curl -s -u key:secret -X PUT -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues/my_queue \
-d '{"dispatch_strategy": "least_inflight", "limits": {"max_shard_message_count": 5000, "max_shard_message_bytes": "100MB"}}' | jq
```

### Delete a Queue

Remove a message queue and all messages retained in it:

```bash
curl -s -u key:secret -X DELETE \
http://localhost:18083/api/v5/message_queues/my_queue
```

Once deleted, the queue stops accepting new messages and its stored data is removed.

## FAQ and Troubleshooting

### Why aren't messages being enqueued?

- Make sure the topic filter of the declared Message Queue matches the topic of the published message.
- Verify that the queue exists and is properly configured.
- Check the EMQX logs for relevant errors or warnings. Look specifically for entries with the `mq_` prefix to diagnose queue-related issues.

### What happens when queues exceed capacity?

Message Queues in EMQX now support multiple types of capacity limits. If any of these limits are reached, EMQX will remove the oldest messages during garbage collection (GC) until the queue size returns within the configured bounds.

- **Time-based limit**: All queues are still subject to the configured *retention period*. Once messages exceed the retention period, they are no longer eligible for delivery and will be automatically purged during GC.

- **Size-based limits**: You can optionally configure per-shard limits on:

  - **Max number of messages** (`max_shard_message_count`)
  - **Max total size of messages in bytes** (`max_shard_message_bytes`)

  These limits are soft and applied during GC, not in real time. Queues may temporarily exceed the configured thresholds between GC cycles.
  
  Note that these limits apply per shard in durable storage. For information on how to configure the number of shards, see [Number of Shards](../durability/managing-replication.md#number-of-shards). In addition, size limits do not account for the [replication factor](../durability/managing-replication.md#replication-factor); the actual physical storage used by a queue will be multiplied by the replication factor.
  

