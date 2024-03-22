# Durable MQTT Sessions and Messages

Starting from release v5.5.x, EMQX contains an embedded durable storage for MQTT sessions and messages. This page introduces the session persistence feature EMQX and how it's capability ensures the resumption of sessions after an EMQX node restart, marking a significant enhancement for IoT applications requiring high reliability.

::: warning Important Notice
This feature is in the public alpha phase. We advise against its use in production environments containing critical data until further notice.

During the alpha phase, there may be changes to the on-disk representation of MQTT messages and sessions. Consequently, data recorded in one version of EMQX may not be readable after upgrading to a subsequent version. This caution will become obsolete with the final release.

:::

## Understand Client Sessions in EMQX

In EMQX, client sessions facilitate the management of client connections and states within the MQTT broker. Starting from version 5.5, EMQX offers two different implementations for managing the client session: regular client sessions and persistent client sessions.

::: tip

For more information on sessions, see [MQTT Persistent Session and Clean Session Explained](https://www.emqx.com/en/blog/mqtt-session).

:::

### Regular Client Sessions

Regular client sessions are temporary and exist for the duration of the client's connection to EMQX. When a client with a regular session disconnects, all session information, including subscriptions and undelivered messages, is discarded. 

### Persistent Client Sessions

Persistent client sessions are designed to retain session information even after the client disconnects. This includes subscriptions and messages that are marked for delivery but have not yet been sent to or acknowledged by the client. When a client reconnects with the same session ID, it can resume its session state, receiving any messages that were retained during its absence. 

Persistent sessions in EMQX are determined based on the client's protocol version and session settings:

- For the clients using MQTT 5 protocol, [Session Expiry Interval](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901048) property of `CONNECT` or `DISCONNECT` packet is set to a value greater than 0.

- For the clients using MQTT 3.* protocol, [Clean Session](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718030) flag is set to 0, and `mqtt.session_expiry_interval` configuration parameter is set to a value greater than 0.

The session persistence feature only affects persistent client sessions. Client sessions not meeting the above criteria remain unaffected by this feature and are processed as usual.

## Enable Session Persistence

The session persistence feature in EMQX is disabled by default. It can be enabled by setting `session_persistence.enable` configuration parameter to `true`.

## How Session Persistence Works

EMQX introduces a unique approach to manage message durability for persistent sessions. When a client with a persistent session subscribes to a topic filter, EMQX designates topics matching the filter as "durable." This ensures that, aside from routing MQTT PUBLISH messages from these topics to active sessions, the broker also saves these messages on disk.

Each durable MQTT message is stored on disk exactly once, regardless of the number of persistent sessions subscribing to the matching topic filter and whether those sessions are currently connected or not. This enables an efficient fan-out of messages to the persistent sessions.

### Durable Storage Architecture

The architecture of EMQX’s durable storage is organized into a hierarchical structure comprising shards, generations, and streams.

#### Shard

This level segregates messages by client, storing them in distinct shards determined by the publisher's client ID. The number of shards is determined by `session_persistence.storage.builtin.n_shards` configuration parameter during the initial startup of EMQX.

#### Generation

Within each shard, data is further divided into generations that span specific time frames. 
New messages are written only into the current generation, while the previous generations are only accessible for reading. EMQX cleans up old messages by deleting old generations. For how long messages are retained is specified by the `session_persistence.message_retention_period` parameter.

Different generations can organize the data in a different manner, according to the *storage layout* specification. Currently only one layout is supported, designed to optimize the throughput of wildcard subscriptions covering very large number of topics, and the throughput of single-topic subscriptions.

Future updates will introduce additional layouts to optimize for the different types of workloads, such as prioritizing low latency over high throughput for certain applications.

#### Stream

Messages in each shard and generation are split into streams. Streams serve as units of message serialization in EMQX. Streams can contain messages from multiple topics. Various storage layouts can employ different strategies for mapping topics into streams.

Persistent sessions interact with this structure by fetching messages in batches from the streams, with the batch size adjustable via the `session_persistence.max_batch_size` parameter. This comprehensive system ensures that EMQX can efficiently handle durable message storage and retrieval.

### Session Persistence Across Cluster

Each node within an EMQX cluster is assigned a unique site ID, which serves as a stable identifier, independent of the Erlang node name (`emqx@...`). Site IDs are persistent, and they are randomly generated at the first startup of the node. This stability maintains the integrity of persistent sessions and messages, especially in scenarios where nodes might undergo name modifications or reconfigurations. 

By associating persistent sessions and messages with a unique site ID rather than just the node's name, EMQX ensures that these sessions can be reliably managed and recovered, even if the underlying node details change. 

To manage and monitor persistent sessions across the cluster, administrators can use `emqx_ctl ds info` CLI command to inspect the status of different sites.

## Hardware Requirements for Session Persistence

When session persistence is enabled, EMQX saves the metadata of persistent sessions and MQTT messages sent to the persistent sessions on disk. Therefore, EMQX must be deployed on a server with sufficiently large storage capacity. To achieve the best throughput, it is recommended to use Solid State Drive (SSD) storage.

The storage requirements can be estimated according to the following guidelines:

- **Message Storage**: The space required for storing messages is proportional to the rate of incoming messages multiplied by the duration specified by the `session_persistence.message_retention_period` parameter. This parameter dictates how long messages are retained, influencing the total storage needed.
- **Session Metadata Storage**: The amount of storage for session metadata is proportional to the number of sessions multiplied by the number of streams to which they are subscribed.
- **Stream Calculation**: The number of streams is proportional to the number of shards.

