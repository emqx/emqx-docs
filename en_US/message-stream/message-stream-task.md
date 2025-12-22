# Message Stream User Guide

This page walks you through the practical usage of the Message Stream feature in EMQX, from creating streams to configuring their behavior and managing them using the Dashboard, REST API, or configuration files.

## Enable Message Stream Feature

Message Stream is **disabled by default**. Before creating or using any message streams, you must enable the feature in the Dashboard.

1. Navigate to **Message Stream** in the left menu.
2. If Message Stream is not enabled, you will see a prompt indicating that the feature is disabled.
3. Click **Settings** to open the **Message Stream** settings page.
4. Toggle **Enable Message Stream** to **On**.
5. Click **Save Changes**.

Once enabled, the Message Stream feature becomes available immediately, and you can start creating and managing streams.

## Manually Create Message Streams via Dashboard

Message streams must be explicitly created before they can store or replay messages. You can create and manage message streams either manually or automatically. For details about automatic creation, see [Automatically Create Message Streams via Dashboard](#automatically-create-message-streams-via-dashboard).

1. Navigate to **Message Stream** in the left menu.

2. Click **Create Stream** to open the **Create Message Stream** dialog.

3. Configure the following options:

   - **Topic Filter**: Enter the topic or topic filter (for example, `t/1` or `sensors/+/data`) that defines which published messages are captured into the stream. All messages published to topics matching this filter will be stored in the stream.

     > Clients consume messages from the stream by subscribing to a stream topic in the `$s/<timestamp>/<topic_filter>` format.

   - **Data Retention Period**: Specify how long messages are retained in the stream. Messages older than the configured retention period are automatically removed, which limits how far back messages can be replayed.
     
   - **Last-Value Semantics**: Enable this option to keep only the most recent message for each key. When enabled, a new message with the same key overwrites older messages with that key in the stream. This is useful for state-oriented data such as device status or configuration.
     
   - **Stream Key Expression**: Required. Defines the expression used to extract a key from each incoming message. The default value is `message.from`, which means the client ID of the message publisher. This field supports configuration using [Variform expressions](../configuration/configuration.md#variform-expressions).
     
      The extracted key serves different purposes depending on the stream type:
        - For **Last-Value** message streams, the key acts as the primary key. Messages with the same key overwrite earlier ones, and only the most recent message per key is retained.
      
        - For **regular** message streams, the key is used as the sharding key to determine which storage shard a message is written to. Messages with the same key are routed to the same shard, preserving per-key ordering while enabling parallel storage across shards.
      
          ::: tip
      
          For regular message streams, avoid using constant or low-cardinality expressions, as this may cause messages to be written to a single shard and impact write performance.
      
          :::
      
      ::: tip
      
      The Stream Key Expression is similar to the Queue Key Expression in Message Queue. See [Queue Key Expression](../message-queue/message-queue-task.md/#queue-key-expression) for examples of key extraction.
      
      :::
      
   - **Limiter**: Configure limits for each shard of the stream to control storage usage:
     
      - **Max Shard Message Count**: Sets the maximum number of messages retained in each shard of the stream. You can enable this option and provide a value, or leave it disabled to allow an unlimited number of messages (`infinity`).
     - **Max Shard Message Bytes**: Sets the maximum total size of messages retained in each shard of the stream. You can enable this option and specify a size (for example, `200MB`), or leave it disabled for unlimited storage (`infinity`).
     
      These limits are persisted to durable storage and work together with the retention period.

4. Click **Create** to save the Message Stream.

Once created, the Message Stream becomes active immediately. Messages published to topics matching the configured topic filter are stored according to the retention and limiter settings and can be replayed by clients subscribing to the stream.

## Automatically Create Message Streams via Dashboard

Message streams can be automatically created when clients subscribe to a `$s/`-prefixed topic. This allows streams to be provisioned dynamically without manual setup.

::: tip Note

Automatic stream creation is available only when the Message Stream feature is enabled globally.

:::

The streams may be auto-created either as regular streams or last-value semantics streams. 

::: tip Note

To ensure proper stream behavior, you can enable auto create either **Regular Message Stream** or **Last Value Message Stream**, but not both at the same time.

:::

### Auto Create Last Value Message Stream

This option is turned on by default in the **Message Stream** tab under **MQTT Settings**. It allows EMQX to automatically create streams that support Last-Value Semantics, where only the most recent message with a given key is retained.

1. Navigate to **Management** -> **MQTT Settings** -> **Message Stream** tab.

2. By default, **Enable Auto Create Message Stream** is enabled and **Last Value Message Stream** type is selected.

   Configure the following:

   - **Stream Key Expression**: Required. Defines how to extract a unique key from each message (default: `message.from`). In Last-Value message streams, this key acts as the primary key. Messages with the same key overwrite earlier messages, and only the most recent value is retained.
   - **Data Retention Period**: Specifies how long messages should be retained in the queue.

3. Click **Save Changes**.

When a client subscribes to a topic such as `$s/<timestamp>/test`, EMQX will automatically create a last-value semantics queue, which will appear in the **Message Stream** list.
### Auto Create Regular Message Stream

This option can be enabled manually if you prefer regular streams where messages are stored independently and not overwritten.

1. Go to **Management** -> **MQTT Settings** -> **Message Stream** tab.

2. By default, **Enable Auto Create Message Stream** is enabled. Select **Regular Message Stream** type.

3. Configure the following:

   - **Stream Key Expression**: Required. Defines how to extract a unique key from each message (default: `message.from`). 

     In Regular message streams, this key is used as the sharding key to determine which storage shard a message is written to. Messages with the same key are routed to the same shard, helping preserve per-key ordering and distribute load across shards.

   - **Data Retention Period**: Specifies how long messages should be retained in the queue.

4. Click **Save Changes**.

## Configure Message Stream Settings

This section explains how to configure global settings that apply to all message streams in EMQX. These settings control message retention, cleanup intervals, internal queue behavior, and queue auto-creation behavior. You can configure them via the Dashboard, REST API, or configuration file.

### Dashboard

You can update Message Stream settings directly from the EMQX Dashboard without restarting the broker. This is useful for adjusting system-wide Message Stream behavior at runtime.

1. Go to **Management** -> **MQTT Settings** -> **Message Stream** tab.

2. Configure the following options:

   - **Enable Message Stream**: Enables or disables the Message Stream feature globally. When disabled, no message streams can be created or used.

   - **Max Stream Count**: Sets the maximum number of message streams that can exist in the cluster. This helps prevent excessive resource usage caused by uncontrolled stream creation.

   - **GC Interval**: Specifies how often expired stream messages are cleaned up. The default value is `1` hour.

   - **Regular Stream Retention Period**: Defines the default retention period for regular (non–Last-Value) message streams. Messages older than this duration are automatically removed. The default is `7` days.

   - **Enable Auto Create Message Stream**: Enables automatic creation of message streams when clients subscribe to stream topics and no matching stream exists.

   - **Auto Create Message Stream Type**: Specifies the type of message streams to create automatically:

     - **Last Value Message Stream** (default): Automatically creates streams with Last-Value semantics enabled.
     - **Regular Message Stream**: Automatically creates streams that retain all messages without overwriting.

   - **Stream Key Expression**: Defines the key expression used for automatically created streams when Last-Value semantics are enabled. The default value is `message.from`. This expression determines how keys are extracted for per-key ordering and overwriting behavior.

   - **Data Retention Period**: Specifies the retention period for automatically created streams. Messages older than this period are removed automatically.

   - **Max Shard Message Bytes**: Limits the amount of data that can be stored in each shard of a message stream. You can enable this option to set a limit, or leave it disabled to allow unlimited storage (`infinity`). 

   - **Max Shard Message Count**: Limits the maximum number of messages in each shard of a message stream. You can enable this option to set a limit, or leave it disabled to allow unlimited messages (`infinity`).

     ::: tip

     The number of [shards](../design/durable-storage.md#shard) is defined globally by the Durable Storage configuration and applies to all message streams. This limit applies per shard and does not account for data replication. When planning storage capacity, note that the total disk usage of a message stream scales with the number of shards and the replication factor. 

     :::

3. After making changes, click **Save Changes** to apply the new settings.

The updated configuration takes effect immediately and applies to all existing and newly created message streams where applicable.

### REST API

You can configure global Message Stream settings programmatically using the EMQX REST API. This is useful for automation, infrastructure-as-code workflows, or managing large deployments.

To update Message Stream global settings, send a `PUT` request to the following endpoint:

```
PUT /api/v5/message_streams/config
```

**Request example**:

```
curl -s -u key:secret \
  -X PUT \
  -H "Content-Type: application/json" \
  http://localhost:18083/api/v5/message_streams/config \
  -d '{
    "gc_interval": "1h",
    "regular_stream_retention_period": "1d",
    "check_stream_status_interval": "10s"
  }'
```

### Configuration File

You can configure global Message Stream settings by editing the EMQX configuration file. This method is useful for defining default behavior at startup or managing settings in environments where configuration files are the primary control mechanism.

**Configuration example**:

Message Stream settings are defined under the `streams` section of the EMQX configuration file (`emqx.conf`).

```hocon
streams {
    gc_interval = 1h
    regular_stream_retention_period = 1d
    check_stream_status_interval = 10s
}
```

#### Configuration Options

- **gc_interval**: Controls how often expired messages are removed from Message Streams. This setting affects the garbage collection cycle for stream storage.
- **regular_stream_retention_period**: Specifies the default maximum retention period for regular message streams. Messages older than this duration are automatically deleted.
- **check_stream_status_interval**: Determines how frequently a subscriber retries to find a stream when subscribing to a `$s/` topic and the corresponding stream does not yet exist.

All duration values use standard time units, such as `s` (seconds), `m` (minutes), `h` (hours), and `d` (days).

#### Durable Storage Configuration

Message Stream messages are stored using EMQX Durable Storage. Storage-related settings for Message Stream are configured under the `durable_storage.streams_messages` section.

```hocon
durable_storage {
    ## Settings for the database storing Message Stream messages.
    ## See Durable Storage configuration for more details.
    streams_messages {
        transaction {
            flush_interval = 100
            idle_flush_interval = 20
            conflict_window = 5000
        }
    }
}
```

These settings control how Message Stream data is written to durable storage, including transaction batching and flush behavior. In most cases, the default values are sufficient and do not need adjustment unless you are tuning storage performance.

## Manage Message Stream via REST API

EMQX provides REST APIs for managing message streams. You can use these APIs to create, update, list, query, and delete message streams, as well as configure global Message Stream settings. This is useful for automation, integration with external systems, and managing streams at scale.

::: tip Note

All REST API operations require appropriate authentication and permissions. For detailed request and response schemas, refer to the Message Stream API reference.

:::

All examples below assume basic authentication using an API key and secret.

### Create a Message Stream

To create a new message stream, send a `POST` request to the streams endpoint and specify the stream configuration in the request body.

```bash
curl -s -u key:secret \
  -X POST \
  -H "Content-Type: application/json" \
  http://localhost:18083/api/v5/message_streams/streams \
  -d '{
    "topic_filter": "t1/#",
    "is_lastvalue": false
  }' | jq
```

The response includes the details of the newly created stream, including its `topic_filter`.

### List Message Streams

To retrieve a list of existing message streams, send a `GET` request to the streams endpoint.

```bash
curl -s -u key:secret \
  -X GET \
  -H "Content-Type: application/json" \
  http://localhost:18083/api/v5/message_streams/streams | jq
```

The response contains a list of streams and pagination metadata.

```bash
{
  "data": [
    {
      "topic_filter": "t1/#"
    }
  ],
  "meta": {
    "hasnext": false
  }
}
```

### Update a Message Stream

To update an existing message stream, send a `PUT` request to the stream resource identified by its topic filter. The topic filter must be URL-encoded.

```bash
curl -s -u key:secret \
  -X PUT \
  -H "Content-Type: application/json" \
  http://localhost:18083/api/v5/message_streams/streams/t1%2F%23 \
  -d '{
    "key_expression": "message.from",
    "is_lastvalue": false
  }' | jq
```

The response returns the updated stream configuration.

### Delete a Message Stream

To delete a Message Stream, send a `DELETE` request to the stream resource identified by its URL-encoded topic filter.

```
curl -s -u key:secret \
  -X DELETE \
  http://localhost:18083/api/v5/message_streams/streams/t1%2F%23
```

Once deleted, the stream stops collecting messages and its stored data is removed according to internal cleanup rules.

### Configure Message Stream Global Settings

See [Configure Message Stream Settings -RESP API](#rest-api).