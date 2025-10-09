# Message Queue User Guide

This page walks you through the practical usage of the Message Queue feature in EMQX, from creating queues to configuring their behavior and managing them using the Dashboard, REST API, or configuration files.

## Create Message Queue via Dashboard

Message Queues must be explicitly declared/created before they can store or dispatch messages.

To create a new Message Queue using the EMQX Dashboard:

1. Navigate to **Message Queue** in the left menu.

2. Click the **Create** button on the page.

3. In the **Create Message Queue** dialog:

   - **Topic Filter**: Enter the topic or topic filter (e.g., `t/1`).  It defines which published messages are enqueued based on topic matching. A queue will collect all messages that match this topic filter.

     To consume messages from the queue, clients must subscribe to the topic using the `$q/{Topic Filter}` format.

   - **Dispatch Strategy**: Select how messages should be distributed among subscribers. Available strategies include:

     - `Least Inflight Subscriber`: Prefer subscribers with the fewest unacknowledged messages.
     - `Random`: (default) Select a subscriber at random.
     - `Round Robin`: Rotate delivery evenly across all subscribers.

   - **Data Retention Period**: Specify how long messages should be retained in the queue. You can set the time unit (e.g., days).

   - **Last Value Semantics**: Toggle this switch on if you want new messages with the same queue key to overwrite older messages in the same queue. When enabled, a new message with the same queue key will overwrite any previous, unconsumed message with that key in the queue.

     - **[Queue Key Expression](#queue-key-expression)**: When Last-Value Semantics is enabled, this field defines the expression used to extract the key from each message. The default value is `message.from`, which means the client ID of the message publisher. This field supports configuration using [Variform expressions](../configuration/configuration.md#variform-expressions).

4. Click **Create** to save the queue.

The new queue will appear in the Message Queue list, showing its topic filter, dispatch strategy, last-value semantics status, and data retention period. You can edit or delete queues using the buttons in the **Actions** column.

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
        flags => #{dup => false,retain => false},
        id => <<0,6,64,4,154,125,229,77,244,69,0,0,28,21,0,2>>,
        timestamp => 1759238376252,from => <<"clientid">>,
        headers =>
            #{peername => <<"127.0.0.1:49352">>,protocol => mqtt,
              username => undefined,peerhost => <<"127.0.0.1">>,
              properties =>
                  #{'User-Property' => #{<<"user-prop">> => <<"some-value">>}},
              proto_ver => 5,client_attrs => #{}},
        payload => <<"some-payload">>,topic => <<"some/topic">>,
        qos => 0}}
```

</details>

## Configure Message Queue Settings

This section explains how to configure global settings that apply to all Message Queues in EMQX. These settings control message retention, cleanup intervals, and internal queue behavior. You can configure them via the Dashboard, REST API, or configuration file.

### Dashboard

You can update Message Queue settings directly from the EMQX Dashboard without restarting the broker. This is useful for making changes to system-wide behavior at runtime.

To configure global settings for Message Queues via the Dashboard:

1. Navigate to the **Message Queue** page from the left menu.
2. Click the **Settings** button in the top-right corner of the page.
3. You will be redirected to the **MQTT Settings** -> **Message Queue** tab. In this panel, you can configure the following parameters:
   - **GC Interval**: The interval at which expired messages are cleaned up from queues. Default is `1` hour.
   - **Regular Queue Retention Period**: The maximum duration for which messages are retained in regular queues. Default is `7` days.
   - **Find Queue Retry Interval**: When a client subscribes to a `$q/`-prefixed queue topic and the corresponding queue does not yet exist, this setting controls how often the client retries to find the queue. Default is `10` seconds.
4. After making changes, click **Save Changes** to apply the new settings.

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
}
```

#### Configuration Descriptions

- **`gc_interval`**:
  Defines the interval at which the Message Queues will clean up expired messages.
- **`regular_queue_retention_period`**:
  Sets the maximum time that messages are retained in a regular queue. After this period, messages will be purged.
- **`find_queue_retry_interval`**:
  Determines how frequently a subscriber retries to locate a queue when subscribing to a `$q/` topic that does not yet exist.

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
- Once messages expire (i.e., exceed the retention period), they are no longer eligible for delivery and will be automatically purged by EMQX during regular garbage collection cycles.

