# Durable MQTT Sessions

Starting from release v5.7.0, EMQX contains embedded durable storage for MQTT sessions and messages.
This page gives a high-level introduction to the session durability feature in EMQX and how it ensures the resumption of sessions after the restart of EMQX nodes.

::: warning Important Notice
EMQX v5.7.0 does not support shared subscriptions for the durable sessions yet.
This feature will be implemented in a later release.
:::

## Types of Client Sessions in EMQX

According to the MQTT standard, client sessions facilitate the management of client connections and states within the MQTT broker. Informally, EMQX separates client sessions into 2 logical categories:

- **Persistent**: Persistent sessions are kept by the broker after the client's connection terminates, and can be resumed if the client reconnects to the broker within the session expiry interval. Messages sent to the topics while the client was offline are delivered.
- **Ephemeral**: Ephemeral sessions exist only for the duration of the client's connection to EMQX. When a client with an ephemeral session disconnects, all session information, including subscriptions and undelivered messages, is discarded.

The client session is considered persistent in following cases:

- For the clients using the MQTT 5 protocol, [Session Expiry Interval](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901048) property of `CONNECT` or `DISCONNECT` packet is set to a value greater than zero.

- For the clients using MQTT 3.* protocol, [Clean Session](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718030) flag is set to 0, and `mqtt.session_expiry_interval` configuration parameter is set to a value greater than 0.

## Session Implementation

EMQX offers 2 different implementations for client sessions, each optimized for specific use cases:

- **RAM**
- **Durable**

The implementation choice depends on the session type (persistent or ephemeral) and the `durable_sessions.enable` configuration parameter, which can be set globally or per zone. 

::: tip

For more information on zone, see [Zone Override](../configuration/configuration.md#zone-override).

:::

The implementation is selected based on the following criteria:

| `durable_sessions.enable` | Ephemeral | Persistent |
|------------------------------|-----------|------------|
| `false`                      | RAM       | RAM        |
| `true`                       | RAM       | durable   |

### RAM Client Sessions

The RAM session implementation is the default and has been used in all EMQX releases before version 5.7. As the name implies, the state of RAM sessions is maintained entirely in volatile memory.

Advantages of RAM client sessions include:

- Very high throughput and low latency.
- Immediate message dispatch to clients.

However, there are some drawbacks:

- Session data is lost when the EMQX node hosting the session stops or restarts, due to the volatility of RAM.
- Undelivered messages are stored in a memory queue, with a limit to prevent memory exhaustion. New messages are discarded when this limit is reached, leading to potential message loss.

### Durable Client Sessions

Introduced in EMQX v5.7.0, the durable session implementation stores session state and messages routed to the durable sessions on disk. This feature is disabled by default and can be enabled by setting the `durable_sessions.enable` configuration parameter to `true`.

Durable sessions provide robust durability and high availability by consistently replicating session metadata and MQTT messages across multiple nodes within an EMQX cluster. The configurable [replication factor](./managing-replication.md#replication-factor) determines the number of replicas for each message or session, enabling users to customize the balance between durability and performance to meet their specific requirements.

Advantages of durable client sessions include:

- Sessions can be resumed after EMQX nodes are restarted or stopped.
- MQTT messages are stored in a shared, replicated, durable storage instead of a memory queue, reducing RAM usage for both online and offline sessions.

However, there are some disadvantages:

- Storing messages on disk results in lower overall system throughput.
- Durable sessions have higher latency compared to RAM sessions because both writing and reading MQTT messages are performed in batches. While batching improves throughput, it also increases end-to-end latency (the delay before clients see the published messages).

## How Durable Sessions Work

EMQX uses a unique approach to manage message durability, allowing RAM and durable sessions to coexist while minimizing storage costs.

When a durable session subscribes to a topic filter, EMQX marks topics matching the filter as "durable." This ensures that, aside from routing MQTT PUBLISH messages from these topics to RAM sessions, the broker also saves such messages to the durable storage.

Each durable MQTT message is stored exactly once on each replica, regardless of the number of subscribing durable sessions or their connection status. This efficient fan-out minimizes disk writes.

### Durable Storage Architecture

EMQX's durable storage is organized into a hierarchical structure comprising storages, shards, generations, and streams.

![Diagram of EMQX durable storage sharding](./assets/emqx_ds_sharding.png)

#### Storage

Storage encapsulates all data of a certain type, such as MQTT messages or MQTT sessions.

#### Shard

Messages are segregated by client and stored in shards based on the publisher's client ID. The number of shards is determined by [n_shards](./managing-replication.md#number-of-shards) configuration parameter during the initial startup of EMQX. A shard is also a unit of replication. Each shard is consistently replicated the number of times specified by `durable_storage.messages.replication_factor` across different nodes, ensuring identical message sets in each replica.

#### Generation

Messages within a shard are segmented into generations corresponding to specific time frames. New messages are written to the current generation, while previous generations are read-only. Old generations are deleted based on the `durable_sessions.message_retention_period` parameter.

Generations can organize data differently according to the storage layout specification. Currently, only one layout is supported, optimized for high throughput of wildcard and single-topic subscriptions. Future updates will introduce layouts optimized for different workloads.

The storage layout for new generations is configured by the `durable_storage.messages.layout` parameter, with each layout engine defining its own configuration parameters.

#### Stream

Messages in each shard and generation are split into streams. Streams serve as units of message serialization in EMQX. Streams can contain messages from multiple topics. Various storage layouts can employ different strategies for mapping topics into streams.

Durable sessions fetch messages in batches from the streams, with batch size adjustable via the `durable_sessions.batch_size` parameter.

### Durable Session Across Cluster

Each node within an EMQX cluster is assigned a unique *Site ID*, which serves as a stable identifier, independent of the Erlang node name (`emqx@...`). Site IDs are persistent, and they are randomly generated at the first startup of the node. This stability maintains the integrity of the data, especially in scenarios where nodes might undergo name modifications or reconfigurations.

Administrators can manage and monitor durable sessions across the cluster by using the `emqx_ctl ds info` CLI command to view the status of different sites.

## Hardware Requirements for Session Persistence

When session persistence is enabled, EMQX saves the metadata of persistent sessions and MQTT messages sent to the persistent sessions on disk. Therefore, EMQX must be deployed on a server with sufficiently large storage capacity. To achieve the best throughput, it is recommended to use Solid State Drive (SSD) storage.

The storage requirements can be estimated according to the following guidelines:

- **Message Storage**: The space required for storing messages on each replica is proportional to the rate of incoming messages multiplied by the duration specified by the `durable_sessions.message_retention_period` parameter. This parameter dictates how long messages are retained, influencing the total storage needed.
- **Session Metadata Storage**: The amount of storage for session metadata is proportional to the number of sessions multiplied by the number of streams to which they are subscribed.
- **Stream Calculation**: The number of streams is proportional to the number of shards. It also depends (in a non-linear fashion) on the number of topics. EMQX automatically combines topics that have a similar structure into the same stream, ensuring that the number of streams doesn't grow too fast with the number of topics, minimizing the volume of metadata stored per session.

## Next Step

You can learn more about the durable session configuration and function operation and management through the following pages:

- Configure and Manage Durable Storage
- Manage Data Replication
