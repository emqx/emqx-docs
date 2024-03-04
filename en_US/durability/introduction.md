# Durable MQTT sessions and messages

::: warning
This feature is currently in a public alpha stage.
Please don't enable it in the production environments that contain valuable data, until this notice is removed.

During the alpha stage, the on-disc representation of MQTT messages and sessions MAY change, and the data recorded on one EMQX release MAY become unreadable after an upgrade to the next EMQX version.
This limitation won't apply to the final product.
:::

Starting from release v5.5.x, EMQX contains an embedded durable storage for MQTT sessions and messages.
This allows the sessions to resume after a restart of the broker node.

Session persistence feature is disabled by default, and it can be activated by setting `session_persistence.enable` configuration parameter to `true`.

## Overview

### Persistent sessions

Starting from version 5.5, EMQX contains two alternative implementations of the client session: regular and persistent.
This feature only affects persistent sessions.

EMQX considers the client's session persistent in two cases:

1. For the clients using MQTT 5 protocol,
   [Session Expiry Interval](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901048) property of `CONNECT` or `DISCONNECT` packet is set to a value greater than zero.

2. For the clients using MQTT 3.* protocol,
   [Clean Session](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718030) flag is set to 0,
   and `mqtt.session_expiry_interval` configuration parameter is set to a value greater than 0.

Client sessions that don't meet the above criteria won't be affected by this feature, and will be processed as usual.

### How does message durability work

EMQX treats the subscriptions of the persistent sessions in a special manner:
when the client subscribes to a topic filter, EMQX marks topics matching the filter as "durable".
In addition to routing the MQTT PUBLISH messages from such topics to the regular sessions, the broker will also save the messages on disc.

Each durable MQTT message is stored on disc exactly once, regardless of the number of persistent sessions subscribing to the matching topic-filter, whether online or offline.
This enables an efficient fan-out of messages to the persistent sessions.

EMQX durable storage is partitioned into the following hierarchy:

- **Shard**: messages published by different clients are stored in different shards.
  Sharing is performed based on the client ID of the publisher.
  Number of shards is determined by `session_persistence.storage.builtin.n_shards` configuration parameter during the first startup of the EMQX broker.

- **Generation**:
   each shard is additionally split into partitions called generations, each one covering a particular period of time.
   New messages are written only into the _current_ generation, while the previous generations are only accessible for reading.

   Different generations can organize the data in a different manner, according to the *storage layout* specification.
   Currently only one layout is supported, that is optimized for the throughput of wildcard subscriptions covering very large number of topics, as well as the throughput of single-topic subscriptions.
   In the future we will add other layouts to optimize for the different types of workloads, for example where low latency is more important than the high throughput.

- **Stream**:
   Finally, messages in each shard and generation are split into streams.
   Streams can contain messages from multiple topics.
   Various storage layouts can employ different strategies for mapping topics into streams.

   Streams serve as units of message serialization in the EMQX broker.


Persistent sessions read messages from the streams in batches.
Size of the batch is configured by `session_persistence.max_batch_size` parameter.

### Hardware requirements

When session persistence is enabled, EMQX will save the metadata of persistent sessions, and any MQTT message sent to the persistent session on disc.
It means that EMQX broker must be deployed on a server with sufficiently large storage capacity.
To achieve the best throughput it's recommended to use SSD storage.

Storage requirements can be estimated according to the following formulae:

- Storage utilized by the messages is proportional to the rate of incoming messages multiplied by the value of configuration parameter `session_persistence.message_retention_period`.

- Storage utilized by the persistent sessions is proportional to the number of sessions multiplied by the number of streams that the sessions subscribe to.

- Number of streams is proportional to the number of shards.

## Message retention

EMQX cleans up old messages by deleting old generations.
Messages are stored for at least `session_persistence.message_retention_period`.

## Sites and site IDs

Each node in the cluster has a unique site ID, that is independent from the Erlang node name (`emqx@...`).
This allows to preserve the persistent messages and sessions in case the node name changes.
Site IDs are persistent, and they are randomly generated at the first startup of the node.

Status of different sites can be inspected using `emqx_ctl ds info` CLI command.
