# 消息流用户指南

本页面将引导你了解 EMQX 中消息流功能的实际使用方式，包括如何创建消息流、配置其行为，以及通过 Dashboard、REST API 或配置文件对消息流进行管理。

## 启用消息流功能

消息流功能默认是关闭的。在创建或使用任何消息流之前，必须先在 Dashboard 中启用该功能。

1. 在左侧菜单中进入**消息流**。
2. 如果消息流尚未启用，页面会显示提示信息，说明该功能当前处于关闭状态。
3. 点击**设置**，进入**消息流**设置页面。
4. 将**启用消息流**切换为**开启**。
5. 点击**保存修改**。

启用后，消息流功能将立即生效，你可以开始创建和管理消息流。

## 通过 Dashboard 手动创建消息流

消息流在存储或回放消息之前，必须被显式创建。你可以通过手动或自动方式创建和管理消息流。关于自动创建的详细说明，请参见[通过 Dashboard 自动创建消息流](#通过-dashboard-自动创建消息流)。

1. 在左侧菜单中进入**消息流**。

2. 点击**创建消息流**，打开**创建消息流**对话框。

3. 配置以下选项：

   - **主题过滤器**：输入主题或主题过滤器（例如 `t/1` 或 `sensors/+/data`），用于定义哪些已发布的消息会被捕获到该消息流中。所有发布到与该过滤器匹配的 MQTT 主题的消息，都会被存储到消息流中。

     > 客户端通过订阅 `$s/<timestamp>/<topic_filter>` 格式的主题来消费消息流中的消息。

   - **数据保留期**：指定消息在消息流中保留的时间长度。超过该保留期的消息将被自动删除，从而限制消息可被回放的时间范围。

   - **最后值语义**：启用后，消息流将只保留每个键对应的最新一条消息。当具有相同键的新消息写入时，旧消息会被覆盖。这非常适合设备状态、配置等状态型数据场景。

   - **流键表达式**（必填）：用于从每条进入消息流的消息中提取键值的表达式。默认值为 `message.from`，表示使用消息发布者的客户端 ID。该字段支持使用 [Variform 表达式](../configuration/configuration.md#variform-表达式)进行配置。

     提取出的键在不同类型的消息流中承担不同角色：

     - 对于**最后值消息流**，该键作为主键使用。具有相同键的消息会相互覆盖，消息流中始终只保留该键对应的最新一条消息。

     - 对于**常规消息流**，该键作为**分片键**使用，用于决定消息会被写入哪个存储分片。具有相同键的消息会被路由到同一个分片，从而在实现多分片并行存储的同时，保证按键的消息顺序性。

       ::: tip

       对于常规消息流，应避免使用常量或低基数的表达式作为流键表达式，否则可能导致所有消息写入同一个分片，形成写入热点并影响性能。

       :::

     ::: tip

     流键表达式与消息队列中的队列键表达式用法类似。关于如何从消息中提取键值的更多示例，请参见[队列键表达式](../message-queue/message-queue-task.md/#队列键表达式)。

     :::

   - **消息限制**：用于限制消息流中每个分片的存储使用情况：

     - **最大分片消息数量**：限制每个分片中可保留的最大消息条数。你可以启用该选项并设置具体数值，或保持关闭以允许无限数量（`infinity`）。
     - **最大分片消息字节数**：限制每个分片中消息的最大总字节数。你可以启用该选项并设置具体大小（例如 `200MB`），或保持关闭以允许无限存储（`infinity`）。

     这些限制会持久化到持久化存储中，并与数据保留期共同生效。

   4. 点击**创建**保存消息流。

   创建完成后，消息流将立即生效。发布到与配置的主题过滤器匹配的主题上的消息，会按照保留策略和限制规则进行存储，并可被客户端通过订阅进行回放。

## 通过 Dashboard 自动创建消息流

当客户端订阅 `$s/` 前缀的主题时，EMQX 可以自动创建对应的消息流，从而实现无需手动配置的动态消息流创建。

::: tip 注意

只有在全局启用了消息流功能后，自动创建消息流功能才可用。

:::

自动创建的消息流可以是**常规消息流**或**最后值消息流**。

::: tip 注意

为了确保消息流行为清晰可控，自动创建时只能启用**常规消息流**或**最后值消息流**其中之一，不能同时启用。

:::

### 自动创建最后值消息流

该选项在 **MQTT 配置** -> **消息流**页面中默认开启。启用后，当客户端订阅不存在的消息流时，EMQX 会自动创建支持最后值语义的消息流。

1. 进入**管理** -> **MQTT 配置** -> **消息流**。
2. 默认情况下，**启用自动创建消息流**已开启，且已选中**最后值消息流**类型。
3. 配置以下选项：
   - **流键表达式**（必填）：定义如何从每条消息中提取唯一键（默认：`message.from`）。在最后值消息流中，该键作为主键使用，具有相同键的消息会覆盖旧消息。
   - **数据保留期**：指定消息在消息流中保留的时间。
4. 点击**保存修改**。

当客户端订阅 `$s/<timestamp>/test` 这样的主题时，EMQX 会自动创建一个最后值消息流，并在**消息流** 列表中显示。

### 自动创建常规消息流

如果你希望消息流保留所有消息、不进行覆盖，可以选择自动创建常规消息流。

1. 进入**管理** -> **MQTT 配置** -> **消息流**。
2. 保持**启用自动创建消息流**为开启状态，并选择**常规消息流**类型。
3. 配置以下选项：
   - **流键表达式**（必填）：定义如何从消息中提取键值（默认：`message.from`）。在常规消息流中，该键用于决定消息写入的存储分片，有助于在保证按键顺序的同时实现负载分布。
   - **数据保留期**：指定消息在消息流中的保留时间。
4. 点击**保存修改**。

## 配置消息流全局设置

本节介绍如何配置作用于**所有消息流**的全局设置。这些设置用于控制消息保留、清理周期、内部行为以及自动创建策略。你可以通过 Dashboard、REST API 或配置文件进行配置。

### Dashboard

你可以直接在 EMQX Dashboard 中修改消息流的全局设置，无需重启 EMQX。

1. 进入**管理** -> **MQTT 配置** -> **消息流**。

2. 配置以下选项：

   - **启用消息流**：全局启用或禁用消息流功能。禁用后，无法创建或使用任何消息流。

   - **最大消息流数**：限制集群中允许存在的消息流数量，用于防止过度创建导致资源耗尽。

   - **垃圾回收间隔**：指定清理过期消息的周期，默认值为 `1 小时`。

   - **常规消息流保留期**：常规（非最后值）消息流的默认消息保留时间，默认值为 `7 天`。

   - **启用自动创建消息流**：当客户端订阅消息流主题且对应消息流不存在时，是否自动创建。

   - **自动创建消息流类型**：

     - **最后值消息流**（默认）
     - **常规消息流**

   - **流键表达式**：为自动创建的消息流指定流键表达式（默认：`message.from`）。

   - **数据保留期**：自动创建消息流的消息保留时间。

   - **最大分片消息字节数**：限制每个分片可存储的最大消息数据量。

   - **最大分片消息数量**：限制每个分片可存储的最大消息条数。

     ::: tip

     分片数量由持久化存储的全局配置决定，并适用于所有消息流。上述限制是按[分片](../design/durable-storage.md#分片-shard)生效的，不考虑副本因子。在规划磁盘容量时，需要同时考虑分片数量和副本因子。

     :::

3. 点击**保存修改**。

修改后的配置会立即生效，并作用于现有和新创建的消息流（适用的情况下）。

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