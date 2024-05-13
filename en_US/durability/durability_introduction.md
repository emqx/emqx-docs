# Durable MQTT Sessions and Messages

Starting from release v5.7, EMQX contains an embedded durable storage for MQTT sessions and messages. This page gives a high-level introduction of the session durability feature in EMQX and how it ensures the resumption of sessions after an EMQX node restart.

::: warning Important Notice
This feature is in the public beta phase. We advise against its use in production environments containing critical data until further notice.

During the beta phase, there may be changes to the on-disk representation of MQTT messages and sessions. Consequently, data recorded in one version of EMQX may not be readable after upgrading to a subsequent version. This caution will become obsolete with the final release.

:::

## Types of Client Sessions in EMQX

According to the MQTT standard, client sessions facilitate the management of client connections and states within the MQTT broker.
Informally, EMQX separates client sessions into two logical categories:

- **Persistent**: persistent sessions are kept by the broker after the client's connection terminates, and can be resumed if the client reconnects to the broker within the session expiry interval.
- **Volatile**: volatile sessions expire immediately after the client disconnects from the broker.

The client session is considered persistent in two cases:

1. For the clients using MQTT 5 protocol,
   [Session Expiry Interval](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901048) property of `CONNECT` or `DISCONNECT` packet is set to a value greater than zero.

2. For the clients using MQTT 3.* protocol,
   [Clean Session](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718030) flag is set to 0,
   and `mqtt.session_expiry_interval` configuration parameter is set to a value greater than 0.

Client sessions that don't meet the above criteria are considered volatile.

## Session Implementations

EMQX contains two alternative implementations for the client sessions:

- **RAM**
- **Durable**

Choice of the implementation depends on the type of the session (persistent or volatile) and the value of `session_persistence.enable` configuration parameter for the zone.

Implementation is chosen according to the following rule:

| `session_persistence.enable` | Volatile | Persistent |
|------------------------------|----------|------------|
| `false`                      | RAM      | RAM        |
| `true`                       | RAM      | durable    |

### RAM Client Sessions

This implementation is used by default. This implementation has been has been used by EMQX since the beginning.
As the name suggests, state of the RAM sessions is kept entirely in the operating memory.
This offers advantages as well as disadvantages.

- Generally, RAM sessions have very high throughput and low latency.

- However, they are lost when the EMQX node handling the client connection is stopped or restarted.
- Another disadvantage of RAM sessions is that they have to store all undelivered messages in the memory queue. This leads to higher RAM consumption, and potential loss of messages when the maximum mqueue size is reached by the session.

### Durable Client Sessions

Durable sessions is a implementation of the client session introduced in EMQX v5.7.
State of durable sessions, as well as messages routed to the durable sessions are backed up on disk.
This implementation is disabled by default. It can be enabled by setting `session_persistence.enable` configuration parameter to `true`.

This implementation has the following advantages:
- Durable sessions survive stopping or restarting the EMQX nodes.
- They don't have to store messages in the memory queue, so the RAM cost of durable sessions (online or offline) in the broker is usually lower.

They also have certain disadvantages:
- Since the messages have to be stored on disk, the overall throughput is lower
- Currently the durable sessions have higher latency than the RAM sessions, because they process messages in batches.

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

This level segregates messages by client, storing them in distinct shards determined by the publisher's client ID. The number of shards is determined by `durable_storage.messages.n_shards` configuration parameter during the initial startup of EMQX.

A shard is also a unit of replication, and EMQX ensures that each shard is consistently replicated `durable_storage.messages.replication_factor` times across different nodes in the cluster, so that each shard replica contains the same set of messages in the same order.

#### Generation

Within each shard, data is further divided into generations that span specific time frames.
New messages are written only into the current generation, while the previous generations are only accessible for reading. EMQX cleans up old messages by deleting old generations. For how long messages are retained is specified by the `session_persistence.message_retention_period` parameter.

Different generations can organize the data in a different manner, according to the *storage layout* specification. Currently only one layout is supported, designed to optimize the throughput of wildcard subscriptions covering very large number of topics, and the throughput of single-topic subscriptions.

Future updates will introduce additional layouts to optimize for the different types of workloads, such as prioritizing low latency over high throughput for certain applications.

#### Stream

Messages in each shard and generation are split into streams. Streams serve as units of message serialization in EMQX. Streams can contain messages from multiple topics. Various storage layouts can employ different strategies for mapping topics into streams.

Persistent sessions interact with this structure by fetching messages in batches from the streams, with the batch size adjustable via the `session_persistence.max_batch_size` parameter.

### Session Persistence Across Cluster

Each node within an EMQX cluster is assigned a unique *Site ID*, which serves as a stable identifier, independent of the Erlang node name (`emqx@...`). Site IDs are persistent, and they are randomly generated at the first startup of the node.
This stability maintains the integrity of the data, especially in scenarios where nodes might undergo name modifications or reconfigurations.

To manage and monitor persistent sessions across the cluster, administrators can use `emqx_ctl ds info` CLI command to inspect the status of different sites.

## Hardware Requirements for Session Persistence

When session persistence is enabled, EMQX saves the metadata of persistent sessions and MQTT messages sent to the persistent sessions on disk. Therefore, EMQX must be deployed on a server with sufficiently large storage capacity. To achieve the best throughput, it is recommended to use Solid State Drive (SSD) storage.

The storage requirements can be estimated according to the following guidelines:

- **Message Storage**: The space required for storing messages on each replica is proportional to the rate of incoming messages multiplied by the duration specified by the `session_persistence.message_retention_period` parameter. This parameter dictates how long messages are retained, influencing the total storage needed.
- **Session Metadata Storage**: The amount of storage for session metadata is proportional to the number of sessions multiplied by the number of streams to which they are subscribed.
- **Stream Calculation**: The number of streams is proportional to the number of shards.
