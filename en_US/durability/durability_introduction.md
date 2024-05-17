# Durable MQTT Sessions and Messages

Starting from release v5.7, EMQX contains an embedded durable storage for MQTT sessions and messages.
This page gives a high-level introduction of the session durability feature in EMQX and how it ensures the resumption of sessions after restart of EMQX nodes.

:::warning
EMQX v5.7.0 don't support shared subscriptions for the durable sessions yet.
This feature will be implemented in a later release.
:::

## Types of Client Sessions in EMQX

According to the MQTT standard, client sessions facilitate the management of client connections and states within the MQTT broker.
Informally, EMQX separates client sessions into two logical categories:

- **Persistent**: persistent sessions are kept by the broker after the client's connection terminates, and can be resumed if the client reconnects to the broker within the session expiry interval. Messages sent to the topics while the client was offline are delivered.
- **Ephemeral**: ephemeral sessions exist only for the duration of the client's connection to EMQX. When a client with an ephemeral session disconnects, all session information, including subscriptions and undelivered messages, is discarded.

The client session is considered persistent in two cases:

1. For the clients using MQTT 5 protocol,
   [Session Expiry Interval](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901048) property of `CONNECT` or `DISCONNECT` packet is set to a value greater than zero.

2. For the clients using MQTT 3.* protocol,
   [Clean Session](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718030) flag is set to 0,
   and `mqtt.session_expiry_interval` configuration parameter is set to a value greater than 0.

## Session Implementation

EMQX contains two alternative implementations for the client sessions, optimized for different use cases:

- **RAM**
- **Durable**

Choice of the implementation depends on the type of the session (persistent or ephemeral) and the value of `durable_sessions.enable` configuration parameter, global or per zone.

Implementation is chosen according to the following rule:

| `durable_sessions.enable` | Ephemeral | Persistent |
|------------------------------|-----------|------------|
| `false`                      | RAM       | RAM        |
| `true`                       | RAM       | durable    |

### RAM Client Sessions

This implementation is used by default. Technically, all EMQX releases before 5.7 used this implementation.
As the name suggests, state of the RAM sessions is kept entirely in the volatile memory.

RAM client sessions have the following advantages:

- Generally, RAM sessions have very high throughput and low latency.
- Messages are dispatched to the clients using RAM session immediately.

They also have some disadvantages:

- Since RAM is a volatile memory, session data is lost when the EMQX node holding the session is stopped or restarted.
- RAM sessions store undelivered messages in a memory queue.
  In order to avoid running out of memory, EMQX puts a limit on the size of the queue, so when the number of undelivered messages reaches the limit, new messages are discarded.
  This can lead to the message loss.

### Durable Client Sessions

Durable sessions is an implementation of the client session introduced in EMQX v5.7.
State of durable sessions, as well as messages routed to the durable sessions are stored on disk.
This session implementation is disabled by default. It can be enabled by setting `durable_sessions.enable` configuration parameter to `true`.

The session persistence feature ensures robust durability and high availability by consistently replicating session metadata and MQTT messages sent to the durable sessions across multiple nodes within an EMQX cluster.
The configurable [replication factor](./managing-replication.md#replication-factor) determines the number of replicas for each message or session, enabling users to customize the balance between durability and performance to meet their specific requirements.

This implementation has the following advantages:
- Durable sessions can be resumed after restarting or stopping of EMQX nodes.
- They store MQTT messages in a shared replicated durable storage instead of the memory queue, so the RAM cost of durable sessions (online or offline) in the broker is lower.

They also have certain disadvantages:
- Since the messages have to be stored on disk, the overall throughput of the system is expected to be lower.
- Currently the durable sessions have higher latency than the RAM sessions, because both writing and reading MQTT messages is done in batches.
  While batching improves the throughput, it leads to increased end-to-end latency (delay before the published messages are seen by the client).

## How Durable Sessions Work

EMQX introduces a unique approach to manage message durability that allows RAM and durable sessions coexist and minimizes the cost of storing messages.
When a durable session subscribes to a topic filter, EMQX marks topics matching the filter as "durable." This ensures that, aside from routing MQTT PUBLISH messages from these topics to RAM sessions, the broker also saves such messages to the durable storage.

Strong durability and high availability guarantees are achieved by replicating the message and session data across multiple nodes in an EMQX cluster in a consistent manner. The *replication factor*, which determines the number of copies each message or session should have, can be adjusted to achieve a desired balance between reliability and performance.

Each durable MQTT message is stored exactly once on each replica, regardless of the number of persistent sessions subscribing to the matching topic filter and whether those sessions are currently connected or not. This enables an efficient fan-out of messages to the persistent sessions, and minimizes the volume of data that has to be written on disk.

### Durable Storage Architecture

The architecture of EMQX’s durable storage is organized into a hierarchical structure comprising storages, shards, generations, and streams.

#### Storage

Storage encapsulates all data of a certain type, such as MQTT messages or MQTT sessions.

#### Shard

At this level, messages are segregated by client, and stored in distinct shards based on the publisher's client ID. The number of shards is determined by [n_shards](./managing-replication.md#number-of-shards) configuration parameter during the initial startup of EMQX.

A shard is also a unit of replication, and EMQX ensures that each shard is consistently replicated `durable_storage.messages.replication_factor` times across different nodes in the cluster so that each shard replica contains the same set of messages in the same order.

#### Generation

Messages within a shard are further segmented into generations corresponding to specific time frames. New messages are written only into the current generation, while the previous generations are only accessible for reading. EMQX cleans up old messages by deleting old generations in their entirety. The retention period of the older generations is defined by the `durable_sessions.message_retention_period` parameter.

Different generations can organize the data differently, according to the *storage layout* specification. Currently, only one layout is supported, optimized for managing the high throughput of wildcard subscriptions spanning a large number of topics and single-topic subscriptions. Future updates will introduce additional layouts to optimize for the different types of workloads, such as prioritizing low latency over high throughput for certain applications.

Storage layout used for the new generations is configured by `durable_storage.messages.layout` parameter.
Each layout engine can define its own set of configuration parameters, depending on its type.

#### Stream

Messages in each shard and generation are split into streams. Streams serve as units of message serialization in EMQX. Streams can contain messages from multiple topics. Various storage layouts can employ different strategies for mapping topics into streams.

Persistent sessions interact with this structure by fetching messages in batches from the streams, with the batch size adjustable via the `durable_sessions.batch_size` parameter.

### Session Persistence Across Cluster

Each node within an EMQX cluster is assigned a unique *Site ID*, which serves as a stable identifier, independent of the Erlang node name (`emqx@...`). Site IDs are persistent, and they are randomly generated at the first startup of the node.
This stability maintains the integrity of the data, especially in scenarios where nodes might undergo name modifications or reconfigurations.

Administrators can manage and monitor persistent sessions across the cluster by using the `emqx_ctl ds info` CLI command to view the status of different sites.

## Hardware Requirements for Session Persistence

When session persistence is enabled, EMQX saves the metadata of persistent sessions and MQTT messages sent to the persistent sessions on disk. Therefore, EMQX must be deployed on a server with sufficiently large storage capacity. To achieve the best throughput, it is recommended to use Solid State Drive (SSD) storage.

The storage requirements can be estimated according to the following guidelines:

- **Message Storage**: The space required for storing messages on each replica is proportional to the rate of incoming messages multiplied by the duration specified by the `durable_sessions.message_retention_period` parameter. This parameter dictates how long messages are retained, influencing the total storage needed.
- **Session Metadata Storage**: The amount of storage for session metadata is proportional to the number of sessions multiplied by the number of streams to which they are subscribed.
- **Stream Calculation**: The number of streams is proportional to the number of shards. It also depends (in a non-linear fashion) on the number of topics. EMQX automatically combines topics that have a similar structure into the same stream, ensuring that the number of streams doesn't grow too fast with the number of topics, minimizing the volume of metadata stored per session.
