# Configure and Manage Durable Storage

This document provides references and instructions for configuring, managing, and optimizing durable storage within EMQX, including sessions and storage configuration.

## Configuration Parameters

Configuration for durable storage is divided into 2 main categories:

- `durable_sessions`: Contains settings related to MQTT clients' sessions, including how they consume data from durable storage and data retention parameters.
- `durable_storage` Manages the settings of the durable storage system holding the MQTT message data.

### Durable Sessions Configuration

| Parameter                                   | Description                                                  |
| ------------------------------------------- | ------------------------------------------------------------ |
| `durable_sessions.enable`                   | Enables session durability. Note: Restart of the EMQX node is required for changes to take effect. |
| `durable_sessions.batch_size`               | Controls the maximum size of message batches consumed from the storage by durable sessions. |
| `durable_sessions.idle_poll_interval`       | Controls the frequency of querying the storage for new messages by durable sessions. If new messages are found, the next batch is retrieved immediately if the client's in-flight queue has space. |
| `durable_sessions.heartbeat_interval`       | Specifies the interval for saving session metadata to the durable storage. |
| `durable_sessions.renew_streams_interval`   | Defines how often sessions query the storage for new streams. |
| `durable_sessions.session_gc_interval`      | Specifies the interval for sweeping through sessions and deleting expired ones. |
| `durable_sessions.message_retention_period` | Defines the retention period of MQTT messages in durable storage. Note: this parameter is global. |


The following parameters can be overridden per [zone](../configuration/configuration.md#zone-override):

- `durable_sessions.enable`
- `durable_sessions.batch_size`
- `durable_sessions.idle_poll_interval`
- `durable_sessions.renew_streams_interval`

### Durable Storage Configuration

The `<DS>` placeholder stands for "durable storage".  Currently, the available parameter for `<DS>` is `message`.

| Parameter                                 | Description                                                  |
| ----------------------------------------- | ------------------------------------------------------------ |
| `durable_storage.<DS>.data_dir`           | Directory in the file system where EMQX stores the data.     |
| `durable_storage.<DS>.n_shards`           | [Numer of shards](./managing-replication.md#number-of-shards). |
| `durable_storage.<DS>.n_sites`            | [Number of sites](./managing-replication.md##number-of-sites). |
| `durable_storage.<DS>.replication_factor` | [Replication factor](./managing-replication.md#replication-factor) determines the number of replicas for each shard. |
| `durable_storage.<DS>.local_write_buffer` | Contains parameters related to message buffering. See [Local Write Buffer Configuration](#local-write-buffer-configuration). |
| `durable_storage.<DS>.layout`             | Contains parameters that control how EMQX lays out data on disk. See [Storage Layout Configuration](#storage-layout-configuration). |

#### Local Write Buffer Configuration

To maximize the throughput, EMQX writes MQTT messages from the clients to the durable storage in batches. Batching is configured using the following parameters under `durable_storage.<DS>.layout` configuration sub-tree:

| Parameter        | Description                                                  |
| ---------------- | ------------------------------------------------------------ |
| `max_items`      | The buffer is flushed when its size reaches this value.      |
| `flush_interval` | The buffer is also flushed at this interval, provided it contains at least one message. |

#### Storage Layout Configuration

Storage layout determines how EMQX organizes data on disk. Setting `durable_storage.<DS>.layout.type` parameter can change the layout used by the new [generations](./durability_introduction.html#generation). This change does not affect existing generations. The configuration of each layout type varies and is contained under the `durable_storage.<DS>.layout` sub-tree. Currently, the `wildcard_optimized` layout type is available.

##### Configuration of `wildcard_optimized` Layout Type

The `wildcard_optimized` layout is aimed to optimize wildcard subscriptions across a vast array of topics. It achieves this by autonomously accumulating knowledge about topic structures over time. Leveraging a lightweight machine learning algorithm, it predicts the wildcard topic filters that clients are likely to subscribe to. Subsequently, it organizes these topics into a unified stream, facilitating efficient consumption in a single batch.


| Parameter              | Description                                                  |
| ---------------------- | ------------------------------------------------------------ |
| `bits_per_topic_level` | Determines the size of the topic level hash.                 |
| `epoch_bits`           | Defines the message offset within an epoch, calculated using the least significant bits of the message timestamp (in microseconds). The number of bits comprising the offset is determined by this parameter. |
| `topic_index_bytes`    | Specifies the size of the stream identifier in bytes.        |

**Epoch Configuration**

Wildcard-optimized streams are segmented into time intervals known as epochs. Messages within each epoch can be processed in a single sweep, thereby enhancing efficiency and throughput. However, larger epochs introduce latency as messages from the current epoch cannot be immediately consumed.

The time interval covered by each epoch can be calculated using the formula: `epoch length (μs) = 2 ^ epoch_bits`.

| Epoch Bits | Epoch Length |
| ---------- | ------------ |
| 1          | 2 μs         |
| 2          | 4 μs         |
| 10         | ~1 ms        |
| 17         | ~100 ms      |
| 20         | ~1 s         |
| 21         | ~2 s         |
| 24         | ~17 s        |

By default, the `epoch_bits` parameter is configured to 20 (~1 s), striking a balance between latency and efficiency. Adjusting this value can fine-tune the trade-off between latency and throughput.

## CLI Commands

The following CLI commands are available for managing the durable storage:

### `emqx_ctl ds info`

Displays an overview of the durable storage state.

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

This command output includes:

- `THIS SITE`: ID of the site claimed by the local EMQX node.
- `SITES`: List of all known sites, including EMQX node names and their statuses.
- `SHARDS`: List of durable storage shards and site IDs where their replicas are located.

### `emqx_ctl ds set_replicas <DS> <Site1> <Site2> ...`

This command allows to set the list of sites containing replicas of the durable storage in the cluster.
Once executed, it creates a plan of operations that leads to fair allocation of the shards between the sites, and then continues to execute it in the background.

::: warning
Updating the list of durable storage replicas can be costly as it may involve copying large volumes of data between sites.
:::

Example:

```bash
$ emqx_ctl ds set_replicas messages 5C6028D6CE9459C7 D8894F95DC86DFDB F4E92DEA197C8EBC
ok
```

After executing this command, the output of `ds info` may look like this:

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

The new section `REPLICA TRANSITIONS` lists pending operations. Once all operations are complete, this list will be empty.

### `emqx_ctl ds join <DS> <Site>` / `emqx_ctl ds leave <DS> <Site>`

These commands add or remove a site from the list of replicas of the durable storage. They are similar to the `set_replicas` command but update one site at a time.

Example:

```bash
$ bin/emqx_ctl ds join messages B2A7DBB2413CD6EE
ok
```

For more detailed information, see [Add Sites](./management.md#add-sites) and [Remove Sites](./management.md#remove-sites).

## REST API

The following REST API endpoints are available for managing and monitoring the built-in durable storage:

- `/ds/sites`: Lists known sites.
- `/ds/sites/:site`: Provides information about a site (status, current EMQX node name managing the site, etc.).
- `/ds/storages`: Lists durable storages.
- `/ds/storages/:ds`: Provides information about the durable storage and its shards.
- `/ds/storages/:ds/replicas`: Lists or updates sites containing replicas of a durable storage.
- `/ds/storages/:ds/replicas/:site`: Adds or removes a replica of the durable storage on a site.

See EMQX OpenAPI schema for more information.

## Metrics

The following Prometheus metrics are relevant to durable sessions:

### `emqx_ds_egress_batches`

Increments each time a batch of messages is successfully written to durable storage.

### `emqx_ds_egress_messages`

Counts messages successfully written to durable storage.

### `emqx_ds_egress_bytes`

Counts the total volume of payload data successfully written to durable storage. Note: This metric only considers message payloads, so the actual volume of data written may be larger.

### `emqx_ds_egress_batches_failed`

Increments each time writing data to durable storage fails for any reason.

### `emqx_ds_egress_flush_time`

A rolling average of time (in μs) spent writing batches to durable storage. It's a key indicator of replication speed.

### `emqx_ds_store_batch_time`

A rolling average of time (in μs) spent writing batches to the local RocksDB storage. Unlike `emqx_ds_egress_flush_time`, it excludes network replication costs, making it a key indicator of disk I/O efficiency.

### `emqx_ds_builtin_next_time`

A rolling average of time (in μs) spent consuming a batch of messages from durable storage.

### `emqx_ds_storage_bitfield_lts_counter_seek` and `emqx_ds_storage_bitfield_lts_counter_next`

These counters are specific to the "wildcard optimized" storage layout. They measure the efficiency of consuming data from local storage. The `seek` primitive is generally slower, so the rate of `emqx_ds_storage_bitfield_lts_counter_next` should ideally grow faster than `seek`.

Increasing the `durable_storage.messages.layout.epoch_bits` parameter can help improve this ratio.

