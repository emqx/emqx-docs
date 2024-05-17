# Configure and Manage Durable Storage 

This document describes configuration, and operation and management interfaces related to the durable sessions.

## Configuration

Configuration related to the durable sessions is split into two sub-trees:

- `durable_sessions` contains the configuration related to the MQTT clients' sessions and how they consume data from the durable storage, as well as data retention parameters.
- `durable_storage` contains the configuration of the durable storage holding the data.

### Session configuration

| Parameter                                   | Description                                                                                                                                                                                                                          |
|---------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `durable_sessions.enable`                   | Enables session durability. Note: this setting cannot be changed in the runtime. EMQX node should be restarted for it to take an effect.                                                                                             |
| `durable_sessions.batch_size`               | Durable sessions consume MQTT messages from the storage in batches. This parameter controls maximum size of the batch.                                                                                                               |
| `durable_sessions.idle_poll_interval`       | This parameter controls how often the durable sessions query the storage for the new messages. When new messages are found, the sessions ask for the next batch immediately (as long as the client has space in the in-flight queue) |
| `durable_sessions.heartbeat_interval`       | Every heartbeat interval durable sessions save their metadata (such as list of subscriptions, processed packet IDs, etc.) to the durable storage.                                                                                    |
| `durable_sessions.renew_streams_interval`   | This parameter defines how often sessions query the durable storage for the new streams.                                                                                                                                             |
| `durable_sessions.session_gc_interval`      | This parameter defines how often EMQX sweeps through the sessions and deletes the expired ones.                                                                                                                                      |
| `durable_sessions.message_retention_period` | This parameter defines the retention period of MQTT messages in the durable storage. Note: this parameter is global.                                                                                                                 |


The following parameters can be overridden per zone:

- `durable_sessions.enable`
- `durable_sessions.batch_size`
- `durable_sessions.idle_poll_interval`
- `durable_sessions.renew_streams_interval`

### Durable storage configuration

`<DS>` stands for "durable storage". Currently, the following durable storages are available:

- `messages`

| Parameter                                 | Description                                                                                                        |
|-------------------------------------------|--------------------------------------------------------------------------------------------------------------------|
| `durable_storage.<DS>.data_dir`           | Directory in the file system where EMQX stores the data                                                            |
| `durable_storage.<DS>.n_shards`           | [Numer of shards](./managing-replication.md#number-of-shards)                                                      |
| `durable_storage.<DS>.n_sites`            | [Number of sites](./managing-replication.md##number-of-sites)                                                      |
| `durable_storage.<DS>.replication_factor` | [Replication factor](./managing-replication.md#replication-factor) determines the number of replicas for each shard |
| `durable_storage.<DS>.local_write_buffer` | This sub-tree contains parameters related to the message buffering. See description below.                         |
| `durable_storage.<DS>.layout`             | This sub-tree contains the parameters that control how EMQX lays out the data on disk. See description below.      |

### Local write buffer configuration

In order to maximize the throughput, EMQX writes MQTT messages from the clients to the durable storage in batches.
Batching is configured using the following parameters under `durable_storage.<DS>.layout` configuration sub-tree:

| Parameter        | Description                                                                                            |
|------------------|--------------------------------------------------------------------------------------------------------|
| `max_items`      | The buffer is flushed when its size reaches this value                                                 |
| `flush_interval` | Additionally, the buffer is flushed every flush interval, as long as it contains at least one message. |

### Storage layout configuration

Storage layout determines how EMQX organizes data on disk.
Layout used by the new [generations](./durability_introduction.html#generation) can be changed by setting `durable_storage.<DS>.layout.type` parameter.
It does not affect the generations that has been already created.
Currently, the following layouts are available:

- `wildcard_optimized`

Layout configuration, contained under the `durable_storage.<DS>.layout` sub-tree, varies by type.

#### Configuration of `wildcard_optimized` layout type

Wildcard-optimized layout is designed to maximize the efficiency of wildcard subscriptions covering large numbers of topics.
It accumulates the knowledge about the topic structure in the background over time, and uses a very lightweight machine learning algorithm to predict what wildcard topic filters the clients are likely to subscribe to.
Then it uses this information to group such topics together into a single stream that can be efficiently consumed in a single batch.


| Parameter              | Description                             |
|------------------------|-----------------------------------------|
| `bits_per_topic_level` | Determines size of the topic level hash |
| `epoch_bits`           | See description below.                  |
| `topic_index_bytes`    | Size of the stream identifier in bytes  |

Wildcard-optimized streams are split into time intervals called epochs.
Offset of the message within the epoch is calculated by taking the least significant bits of the message timestamp (in microseconds).
The number of bits comprising the offset is determined by `epoch_bits` parameter.
Therefore, the time interval covered by the epoch can be calculated by the formula:
`epoch length (μs) = 2 ^ epoch_bits`.

| Epoch bits | Epoch length |
|------------|--------------|
| 1          | 2 μs         |
| 2          | 4 μs         |
| 10         | ~1 ms        |
| 17         | ~100 ms      |
| 20         | ~1 s         |
| 21         | ~2 s         |
| 24         | ~17 s        |

Messages within each epoch can be consumed in a single sweep, leading to higher efficiency and better throughput.
However, larger epochs lead to higher latency, since messages from the current epoch can't be consumed immediately.

The default value of `epoch_bits` is 20 (~1 s), striking good balance between latency and efficiency.

## CLI

The following CLI commands are available for managing the durable storage:

### `emqx_ctl ds info`

Get a quick overview of the durable storage state.

Example:

```bash
$ emqx_ctl ds info

THIS SITE:
D8894F95DC86DFDB

SITES:
5C6028D6CE9459C7    'emqx@n2.local'        up
D8894F95DC86DFDB    'emqx@n1.local'        up
F4E92DEA197C8EBC    'emqx@n3.local'    (x) down

SHARDS:
Shard                             Replicas
messages/0                        5C6028D6CE9459C7
messages/1                        5C6028D6CE9459C7
messages/10                       5C6028D6CE9459C7
messages/11                       5C6028D6CE9459C7
messages/2                        5C6028D6CE9459C7
messages/3                        5C6028D6CE9459C7
messages/4                        5C6028D6CE9459C7
messages/5                        5C6028D6CE9459C7
messages/6                        5C6028D6CE9459C7
messages/7                        5C6028D6CE9459C7
messages/8                        5C6028D6CE9459C7
messages/9                        5C6028D6CE9459C7
```

The output of this command is comprised of several sections:

- `THIS SITE`:  ID of the site claimed by the local EMQX node
- `SITES`: list of all known sites together with names of the EMQX nodes claiming them, and their statuses
- `SHARDS`: list of durable storage shards and sites IDs where their replicas are located

### `emqx_ctl ds set_replicas <DS> <Site1> <Site2> ...`

This command allows to set the list of sites containing replicas of the durable storage in the cluster.
Once executed, it creates a plan of operations that leads to fair allocation of the shards between the sites, and then continues to execute it in the background.

::: warning
Updating the list of durable storage replicas is a costly operation, since it may involve copying large volumes of data between the sites.
:::

Example:

```bash
$ emqx_ctl ds set_replicas messages 5C6028D6CE9459C7 D8894F95DC86DFDB F4E92DEA197C8EBC
ok
```

After execution of this command the output of `ds info` may look like this:

```bash
$ emqx_ctl ds info

THIS SITE:
D8894F95DC86DFDB

SITES:
5C6028D6CE9459C7    'emqx@n2.local'        up
D8894F95DC86DFDB    'emqx@n1.local'        up
F4E92DEA197C8EBC    'emqx@n3.local'        up

SHARDS:
Shard                             Replicas
messages/0                        5C6028D6CE9459C7      D8894F95DC86DFDB
messages/1                        5C6028D6CE9459C7      D8894F95DC86DFDB
messages/10                       5C6028D6CE9459C7
messages/11                       5C6028D6CE9459C7      D8894F95DC86DFDB
messages/2                        5C6028D6CE9459C7      D8894F95DC86DFDB
messages/3                        5C6028D6CE9459C7
messages/4                        5C6028D6CE9459C7      D8894F95DC86DFDB
messages/5                        5C6028D6CE9459C7      D8894F95DC86DFDB
messages/6                        5C6028D6CE9459C7
messages/7                        5C6028D6CE9459C7      D8894F95DC86DFDB
messages/8                        5C6028D6CE9459C7      D8894F95DC86DFDB
messages/9                        5C6028D6CE9459C7

REPLICA TRANSITIONS:
Shard                         Transitions
messages/0                    +F4E92DEA197C8EBC
messages/1                    +F4E92DEA197C8EBC
messages/10                   +F4E92DEA197C8EBC  +D8894F95DC86DFDB
messages/11                   +F4E92DEA197C8EBC
messages/2                    +F4E92DEA197C8EBC
messages/3                    +F4E92DEA197C8EBC  +D8894F95DC86DFDB
messages/4                    +F4E92DEA197C8EBC
messages/5                    +F4E92DEA197C8EBC
messages/6                    +F4E92DEA197C8EBC  +D8894F95DC86DFDB
messages/7                    +F4E92DEA197C8EBC
messages/8                    +F4E92DEA197C8EBC
messages/9                    +F4E92DEA197C8EBC  +D8894F95DC86DFDB
```

Note a new section `REPLICA TRANSITIONS` containing the list of pending operations.
Once all pending operations are complete, this list becomes empty.

### `emqx_ctl ds join <DS> <Site>` / `emqx_ctl ds leave <DS> <Site>`

These commands allow to add or remove a site from the list of replicas of the durable storage.
They are similar to `set_replicas` command, but update one site at a time.

Example:

```bash
$ bin/emqx_ctl ds join messages B2A7DBB2413CD6EE
ok
```

## REST API

The following REST API endpoints are available for managing and monitoring the builtin durable storage:

- `/ds/sites` — list known sites
- `/ds/sites/:site` — get information about the site (its status, current EMQX node name managing the site, etc.)
- `/ds/storages` — list durable storages
- `/ds/storages/:ds` — get information about the durable storage and its shards
- `/ds/storages/:ds/replicas` — list or update sites that contain replicas of a durable storage
- `/ds/storages/:ds/replicas/:site` — allows to add or remove replica of the durable storage on the site

See EMQX OpenAPI schema for more information.

## Metrics

This section lists Prometheus metrics relevant to the durable sessions.

### `emqx_ds_egress_batches`

This counter is increased every time when a batch of messages is successfully written to the durable storage.

### `emqx_ds_egress_messages`

This metric counts messages successfully written to the durable storage.

### `emqx_ds_egress_bytes`

This metric counts total volume of payload data successfully written to the durable storage.
Note: this counter only takes message payloads into consideration, so the actual volume of data written to the durable storage may be larger.

### `emqx_ds_egress_batches_failed`

This counter is incremented every time when writing data to the durable storage fails for any reason.

### `emqx_ds_egress_flush_time`

This is a rolling average of time spent writing batches to the durable storage.
It's a key indicator of the replication speed.

### `emqx_ds_store_batch_time`

This is a rolling average of time spent writing batches to the local RocksDB storage.
Unlike `emqx_ds_egress_flush_time`, it does not include network replication costs, so it's the key indicator of the disk IO efficiency.

### `emqx_ds_builtin_next_time`

This is a rolling average of time spent consuming a batch of messages from the durable storage.

### `emqx_ds_storage_bitfield_lts_counter_seek` and `emqx_ds_storage_bitfield_lts_counter_next`

These counters are specific to the "wildcard optimized" storage layout.
They measure the efficiency of consuming data from the local storage.

Wildcard optimized layout uses two primitives for looking up data in RocksDB: one that searches for a key (seek), and one that simply jumps to the next key (next).
`seek` primitive is generally slower, so ideally the rate of growth of `emqx_ds_storage_bitfield_lts_counter_next` counter must be much greater than the rate of growth of `seek` counter.

Increasing `durable_storage.messages.layout.epoch_bits` parameter can help to increase this ratio.
