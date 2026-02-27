# MQTT Streams User Guide

This page walks you through the practical usage of the MQTT Streams feature in EMQX, from creating streams to configuring their behavior and managing them using the Dashboard, REST API, or configuration files.

## Enable MQTT Streams Feature

The MQTT Streams feature is disabled by default. Before creating or using any streams, you must enable the feature in the Dashboard.

1. Navigate to **Streams** in the left menu.
2. If the feature is not enabled, you will see a prompt indicating that the feature is disabled.
3. Click **Settings** to open the **Streams** settings page.
4. Toggle **Enable Streams** to **On**.
5. Click **Save Changes**.

Once enabled, the MQTT Stream feature becomes available immediately, and you can start creating and managing streams.

## Manually Create Streams via Dashboard

MQTT streams must be explicitly created before they can store or replay messages. You can create and manage streams either manually or automatically. For details about automatic creation, see [Automatically Create MQTT Streams via Dashboard](#automatically-create-mqtt-streams-via-dashboard).

1. Navigate to **Streams** in the left menu.

2. Click **Create Stream** to open the **Create Stream** dialog.

3. Configure the following options:

   - **Name**: Required. Specifies the unique name of the stream. Stream names must contain only the following:

     - Alphanumeric characters (`A–Z`, `a–z`, `0–9`)
     - Underscores (`_`)
     - Hyphens (`-`)
     - Dots (`.`)

     The stream is identified and managed by this name. 

   - **Topic Filter**: Enter the topic or topic filter (for example, `t/1` or `sensors/+/data`) that defines which published messages are captured into the stream. All messages published to topics matching this filter will be stored in the stream.
   
     > Clients can consume messages using the following subscription formats:
     >
     > - `$stream/<name>` is used when the stream already exists.
     > - `$stream/<name>/<topic_filter>` is optional when subscribing to an existing stream. It can be used when auto-creation is enabled. If the stream does not yet exist, EMQX uses the provided `<topic_filter>` to create it automatically.
     > 
     > The `<topic_filter>` segment must match the stream’s configured topic filter.
     >
     > To replay historical messages, specify the MQTT 5 subscription property `stream-offset`. The value can be:
     >
     > - A Unix timestamp in microseconds
     > - `earliest`
     > - `latest`
     
   - **Data Retention Period**: Specify how long messages are retained in the stream. Messages older than the configured retention period are automatically removed, which limits how far back messages can be replayed.
     
   - **Last-Value Semantics**: Enable this option to keep only the most recent message for each key. When enabled, a new message with the same key overwrites older messages with that key in the stream. This is useful for state-oriented data such as device status or configuration.
     
   - **Stream Key Expression**: Required. Defines the expression used to extract a key from each incoming message. The default value is `message.from`, which means the client ID of the message publisher. This field supports configuration using [Variform expressions](../configuration/configuration.md#variform-expressions).
     
      ::: tip
      
      The Stream Key Expression is similar to the Queue Key Expression in Message Queue. See [Queue Key Expression](../message-queue/message-queue-task.md#queue-key-expression) for examples of key extraction.
      
      :::
      
      The extracted key serves different purposes depending on the stream type:
        - For **Last-Value** streams, the key acts as the primary key. Messages with the same key overwrite earlier ones, and only the most recent message per key is retained.
      
        - For **regular** streams, the key is used as the sharding key to determine which storage shard a message is written to. Messages with the same key are routed to the same shard, preserving per-key ordering while enabling parallel storage across shards.
      
          ::: tip
      
          For regular streams, avoid using constant or low-cardinality expressions, as this may cause messages to be written to a single shard and impact write performance.
      
          :::
      
   - **Limiter**: Configure limits for each shard of the stream to control storage usage:
     
      - **Max Shard Message Count**: Sets the maximum number of messages retained in each shard of the stream. You can enable this option and provide a value, or leave it disabled to allow an unlimited number of messages (`infinity`).
     - **Max Shard Message Bytes**: Sets the maximum total size of messages retained in each shard of the stream. You can enable this option and specify a size (for example, `200MB`), or leave it disabled for unlimited storage (`infinity`).
     
      These limits are persisted to durable storage and work together with the retention period.
   
4. Click **Create** to save the stream.

Once created, the MQTT stream becomes active immediately. Messages published to topics matching the configured topic filter are stored according to the retention and limiter settings and can be replayed by clients subscribing to the stream.

## Automatically Create Streams via Dashboard

MQTT streams can be automatically created when clients subscribe to a `$stream/<name>`-prefixed topic. The `<name>` in the subscription becomes the stream name.

::: tip Note

Automatic stream creation is available only when the MQTT Stream feature is enabled globally.

:::

The streams may be auto-created either as regular streams or last-value semantics streams. 

::: tip Note

To ensure proper stream behavior, you can enable auto create either regular streams or last-value semantics streams, but not both at the same time.

:::

### Auto Create Last-Value Streams

This option is turned on by default in the **Streams** tab under **MQTT Settings**. It allows EMQX to automatically create streams that support Last-Value Semantics, where only the most recent message with a given key is retained.

1. Navigate to **Management** -> **MQTT Settings** -> **Messages** tab.

2. By default, **Enable Auto Create Stream** is enabled and **Last Value Stream** type is selected.

   Configure the following:

   - **Stream Key Expression**: Required. Defines how to extract a unique key from each message (default: `message.from`). In Last-Value streams, this key acts as the primary key. Messages with the same key overwrite earlier messages, and only the most recent value is retained.
   - **Data Retention Period**: Specifies how long messages should be retained in the stream.

3. Click **Save Changes**.

When a client subscribes to a topic such as `$stream/my_stream/test`, EMQX will automatically create a last-value stream named `my_stream`, which will appear in the **Streams** list.
### Auto Create Regular Streams

This option can be enabled manually if you prefer regular streams where messages are stored independently and not overwritten.

1. Go to **Management** -> **MQTT Settings** -> **Streams** tab.

2. By default, **Enable Auto Create Message Stream** is enabled. Select **Regular Message Stream** type.

3. Configure the following:

   - **Stream Key Expression**: Required. Defines how to extract a unique key from each message (default: `message.from`). 

     In Regular streams, this key is used as the sharding key to determine which storage shard a message is written to. Messages with the same key are routed to the same shard, helping preserve per-key ordering and distribute load across shards.

   - **Data Retention Period**: Specifies how long messages should be retained in the stream.

4. Click **Save Changes**.

## Configure Streams Settings

This section explains how to configure global settings that apply to all MQTT streams in EMQX. These settings control message retention, cleanup intervals, internal stream behavior, and stream auto-creation behavior. You can configure them via the Dashboard, REST API, or configuration file.

### Dashboard

You can update MQTT Streams settings directly from the EMQX Dashboard without restarting the broker. This is useful for adjusting system-wide stream behavior at runtime.

1. Go to **Management** -> **MQTT Settings** -> **Streams** tab.

2. Configure the following options:

   - **Enable Streams**: Enables or disables the MQTT Stream feature globally. When disabled, no streams can be created or used.

   - **Max Stream Count**: Sets the maximum number of streams that can exist in the cluster. This helps prevent excessive resource usage caused by uncontrolled stream creation.

   - **GC Interval**: Specifies how often expired stream messages are cleaned up. The default value is `1` hour.

   - **Regular Stream Retention Period**: Defines the default retention period for regular (non–Last-Value) streams. Messages older than this duration are automatically removed. The default is `7` days.

   - **Enable Auto Create Message Stream**: Enables automatic creation of streams when clients subscribe to stream topics and no matching stream exists.

   - **Auto Create Stream Type**: Specifies the type of streams to create automatically:

     - **Last Value Stream** (default): Automatically creates streams with Last-Value semantics enabled.
     - **Regular Stream**: Automatically creates streams that retain all messages without overwriting.

   - **Stream Key Expression**: Defines the key expression used for automatically created streams when Last-Value semantics are enabled. The default value is `message.from`. This expression determines how keys are extracted for per-key ordering and overwriting behavior.

   - **Data Retention Period**: Specifies the retention period for automatically created streams. Messages older than this period are removed automatically.

   - **Max Shard Message Bytes**: Limits the amount of data that can be stored in each shard of a stream. You can enable this option to set a limit, or leave it disabled to allow unlimited storage (`infinity`). 

   - **Max Shard Message Count**: Limits the maximum number of messages in each shard of a stream. You can enable this option to set a limit, or leave it disabled to allow unlimited messages (`infinity`).

     ::: tip

     The number of [shards](../design/durable-storage.md#shard) is defined globally by the Durable Storage configuration and applies to all streams. This limit applies per shard and does not account for data replication. When planning storage capacity, note that the total disk usage of a stream scales with the number of shards and the replication factor. 

     :::

3. After making changes, click **Save Changes** to apply the new settings.

The updated configuration takes effect immediately and applies to all existing and newly created streams where applicable.

### REST API

You can configure global MQTT Streams settings programmatically using the EMQX REST API.

To update MQTT Streams global settings, send a `PUT` request to the following endpoint:

```
PUT /api/v5/message_streams/config
```

**Request example**:

```bash
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

You can configure global MQTT Streams settings by editing the EMQX configuration file. This method is useful for defining default behavior at startup or managing settings in environments where configuration files are the primary control mechanism.

**Configuration example**:

MQTT Streams settings are defined under the `streams` section of the EMQX configuration file (`emqx.conf`).

```hocon
streams {
    gc_interval = 1h
    regular_stream_retention_period = 1d
    check_stream_status_interval = 10s
}
```

#### Configuration Options

- **gc_interval**: Controls how often expired messages are removed from MQTT streams. This setting affects the garbage collection cycle for stream storage.
- **regular_stream_retention_period**: Specifies the default maximum retention period for regular streams. Messages older than this duration are automatically deleted.
- **check_stream_status_interval**: Determines how frequently a subscriber retries to find a stream when subscribing to a `$stream/<name>` topic and the corresponding stream does not yet exist.

All duration values use standard time units, such as `s` (seconds), `m` (minutes), `h` (hours), and `d` (days).

#### Durable Storage Configuration

Stream messages are stored using EMQX Durable Storage. Storage-related settings for MQTT streams are configured under the `durable_storage.streams_messages` section.

```hocon
durable_storage {
    ## Settings for the database storing stream messages.
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

These settings control how MQTT stream data is written to durable storage, including transaction batching and flush behavior. In most cases, the default values are sufficient and do not need adjustment unless you are tuning storage performance.

## Manage Streams via REST API

EMQX provides REST APIs for managing streams. You can use these APIs to create, update, list, query, and delete streams, as well as configure global MQTT Stream settings. This is useful for automation, integration with external systems, and managing streams at scale.

::: tip Note

All REST API operations require appropriate authentication and permissions. For detailed request and response schemas, refer to the "MQTT Stream" section in [REST API](../admin/api.md).

:::

All examples below assume basic authentication using an API key and secret.

### Create a Stream

To create a new stream, send a `POST` request to the streams endpoint and specify the stream configuration in the request body.

```bash
curl -s -u key:secret \
  -X POST \
  -H "Content-Type: application/json" \
  http://localhost:18083/api/v5/message_streams/streams \
  -d '{
    "name": "my_stream",
    "topic_filter": "t1/#",
    "is_lastvalue": false
  }' | jq
```

The response includes the details of the newly created stream, including its `topic_filter`.

### List Streams

To retrieve a list of existing streams, send a `GET` request to the streams endpoint.

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
      "name": "my_stream",
      "topic_filter": "t1/#"
    }
  ],
  "meta": {
    "hasnext": false
  }
}
```

### Update a Stream

To update an existing stream, send a `PUT` request to the stream resource identified by its name. The topic filter must be URL-encoded.

```bash
curl -s -u key:secret \
  -X PUT \
  -H "Content-Type: application/json" \
  http://localhost:18083/api/v5/message_streams/streams/my_stream \
  -d '{
    "key_expression": "message.from",
    "is_lastvalue": false
  }' | jq
```

The response returns the updated stream configuration.

### Delete a Stream

To delete a stream, send a `DELETE` request to the stream resource identified by its name.

```bash
curl -s -u key:secret \
  -X DELETE \
  http://localhost:18083/api/v5/message_streams/streams/my_stream
```

Once deleted, the stream stops collecting messages and its stored data is removed according to internal cleanup rules.

### Configure Streams Global Settings

See [Configure Streams Settings -RESP API](#rest-api).