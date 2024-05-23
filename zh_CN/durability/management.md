# 配置和管理会话持久化

本文档提供了有关在 EMQX 中配置、管理和优化会话持久化功能的参考和说明，包括会话和存储配置。

## 配置参数

会话持久化的配置分为两个主要类别：

- `durable_sessions`：包含与 MQTT 客户端会话相关的设置，包括它们如何从持久存储中消费数据以及数据保留参数。
- `durable_storage`：管理持久存储如何保存 MQTT 消息数据的设置。

### 持久会话配置

| 参数                                        | 描述                                                         |
| ------------------------------------------- | ------------------------------------------------------------ |
| `durable_sessions.enable`                   | 启用会话持久性。注意：需要重新启动 EMQX 节点才能使更改生效。 |
| `durable_sessions.batch_size`               | 控制持久会话从存储中消费的消息批次的最大大小。               |
| `durable_sessions.idle_poll_interval`       | 控制持久会话查询新消息的频率。如果发现新消息，则下一批将立即从存储中检索，如果客户端的传输队列有空间的话。 |
| `durable_sessions.heartbeat_interval`       | 指定将会话元数据保存到会话持久化的间隔。                     |
| `durable_sessions.renew_streams_interval`   | 定义会话多久查询存储以获取新流。                             |
| `durable_sessions.session_gc_interval`      | 指定清除会话并删除过期会话的间隔。                           |
| `durable_sessions.message_retention_period` | 定义会话持久化中 MQTT 消息的保留期。注意：此参数是全局的。   |

以下参数可以在 [zone](../configuration/configuration.md#zone-override) 级别覆盖：
- `durable_sessions.enable`
- `durable_sessions.batch_size`
- `durable_sessions.idle_poll_interval`
- `durable_sessions.renew_streams_interval`

### 会话持久化配置

`<DS>` 占位符代表 "durable storage"。当前，`<DS>` 的可用参数为 `message`。

| 参数                                      | 描述                                                         |
| ----------------------------------------- | ------------------------------------------------------------ |
| `durable_storage.<DS>.data_dir`           | EMQX 存储数据的文件系统中的目录。                            |
| `durable_storage.<DS>.n_shards`           | 设置[分片数量](./managing-replication.md#number-of-shards)。 |
| `durable_storage.<DS>.n_sites`            | 设置[站点数量](./managing-replication.md##number-of-sites)。 |
| `durable_storage.<DS>.replication_factor` | 设置[复制因子](./managing-replication.md#replication-factor)以确定每个分片的副本数量。 |
| `durable_storage.<DS>.local_write_buffer` | 包含与消息缓冲相关的参数。请参阅[本地写缓冲配置](#local-write-buffer-configuration)。 |
| `durable_storage.<DS>.layout`             | 包含控制 EMQX 如何在磁盘上布局数据的参数。请参阅[存储布局配置](#storage-layout-configuration)。 |

#### 本地写缓冲配置

为了最大化吞吐量，EMQX 将来自客户端的 MQTT 消息批量写入会话持久化。批处理是使用 `durable_storage.<DS>.layout` 配置子树下的以下参数进行配置的：

| 参数             | 描述                                                 |
| ---------------- | ---------------------------------------------------- |
| `max_items`      | 当缓冲区大小达到此值时，将刷新缓冲区。               |
| `flush_interval` | 如果缓冲区包含至少一条消息，将在此间隔内刷新缓冲区。 |

#### 存储布局配置

存储布局决定了 EMQX 如何在磁盘上组织数据。设置 `durable_storage.<DS>.layout.type` 参数可以更改新一代中使用的布局。此更改不会影响现有的生成。每种布局类型的配置都不同，包含在 `durable_storage.<DS>.layout` 子树下。当前，可用的布局类型是 `wildcard_optimized`。

##### `wildcard_optimized` 布局类型的配置

`wildcard_optimized` 布局旨在优化广泛的主题通配符订阅。它通过随时间自动积累关于主题结构的知识来实现这一目标。利用轻量级机器学习算法，它预测客户端可能订阅的通配符主题过滤器。随后，它将这些主题组织成统一的流，从而在单个批次中实现高效消费。

| 参数                   | 描述                                                         |
| ---------------------- | ------------------------------------------------------------ |
| `bits_per_topic_level` | 确定主题级别哈希的大小。                                     |
| `epoch_bits`           | 定义了一个 epoch 内的消息偏移量，使用消息时间戳（微秒）的最低有效位来计算。偏移量所占的位数由此参数确定。 |
| `topic_index_bytes`    | 指定流标识符的大小，以字节为单位。                           |

**Epoch 配置**

通配符优化流被分成称为 epoch 的时间间隔。每个 epoch 内的消息可以在单个扫描中处理，从而提高效率和吞吐量。但是，较大的 epoch 会引入延迟，因为当前 epoch 中的消息无法立即消费。

每个 epoch 覆盖的时间间隔可以使用以下公式计算：`epoch length (μs) = 2 ^ epoch_bits`。

| Epoch bits | Epoch length |
| ---------- | ------------ |
| 1          | 2 μs         |
| 2          | 4 μs         |
| 10         | ~1 ms        |
| 17         | ~100 ms      |
| 20         | ~1 s         |
| 21         | ~2 s         |
| 24         | ~17 s        |

默认情况下，`epoch_bits` 参数配置为 20（~1 秒），在延迟和效率之间取得平衡。调整此值可以微调延迟和吞吐量之间的权衡。

## CLI 命令

以下是用于管理会话持久化的 CLI 命令：

### `emqx_ctl ds info`

显示会话持久化状态的概述。

示例：
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

此命令输出包括： 

- `THIS SITE`：本地 EMQX 节点声明的站点 ID。 
- `SITES`：所有已知站点的列表，包括 EMQX 节点名称及其状态。 
- `SHARDS`：会话持久化分片列表以及其副本所在的站点 ID。

### `emqx_ctl ds set_replicas <DS> <Site1> <Site2> ...`

此命令允许设置包含集群中会话持久化副本的站点列表。 一旦执行，它会创建一个操作计划，以在站点之间公平分配分片，并继续在后台执行。 

::: warning

更新会话持久化副本列表可能成本高昂，因为可能涉及在站点之间复制大量数据。 

::: 

示例：
```bash
$ emqx_ctl ds set_replicas messages 5C6028D6CE9459C7 D8894F95DC86DFDB F4E92DEA197C8EBC
ok
```

执行此命令后，`ds info` 的输出类似如下所示：
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

新的 `REPLICA TRANSITIONS` 部分列出了待处理的操作。一旦所有操作完成，此列表将为空。

### `emqx_ctl ds join <DS> <Site>` / `emqx_ctl ds leave <DS> <Site>`

这些命令将一个站点添加到会话持久化副本列表中或从中移除。它们类似于 `set_replicas` 命令，但每次更新一个站点。 

示例：
```bash
$ bin/emqx_ctl ds join messages B2A7DBB2413CD6EE
ok
```

更多详细内容，请见[添加站点](./management.md#添加站点)和[移除站点](./management.md#移除站点)。

## REST API

以下是用于管理和监控内置会话持久化的 REST API 端点：

- `/ds/sites`：列出已知站点。
- `/ds/sites/:site`：提供有关站点的信息（状态、管理站点的当前 EMQX 节点名称等）。
- `/ds/storages`：列出会话持久化。
- `/ds/storages/:ds`：提供有关会话持久化及其分片的信息。
- `/ds/storages/:ds/replicas`：列出或更新包含会话持久化副本的站点。
- `/ds/storages/:ds/replicas/:site`：在站点上添加或删除会话持久化的副本。

有关更多信息，请参阅 EMQX OpenAPI schema。

## 指标

以下 Prometheus 指标与持久会话相关：

### `emqx_ds_egress_batches`

每次成功将一批消息写入会话持久化时递增。

### `emqx_ds_egress_messages`

计算成功写入会话持久化的消息数量。

### `emqx_ds_egress_bytes`

计算成功写入会话持久化的有效载荷数据总量。注意：此指标仅考虑消息有效载荷，因此实际写入的数据量可能更大。

### `emqx_ds_egress_batches_failed`

每次写入会话持久化失败时递增。

### `emqx_ds_egress_flush_time`

记录写入批次到会话持久化所花费时间（以微秒为单位）的滚动平均值。这是复制速度的关键指标。

### `emqx_ds_store_batch_time`

记录写入批次到本地 RocksDB 存储所花费时间（以微秒为单位）的滚动平均值。与 `emqx_ds_egress_flush_time` 不同，它不包括网络复制成本，因此是磁盘 I/O 效率的关键指标。

### `emqx_ds_builtin_next_time`

记录从会话持久化中消费一批消息所花费时间（以微秒为单位）的滚动平均值。

### `emqx_ds_storage_bitfield_lts_counter_seek` 和 `emqx_ds_storage_bitfield_lts_counter_next`

这些计数器特定于 "wildcard optimized" 存储布局。它们衡量从本地存储消费数据的效率。`seek` 操作通常较慢，因此理想情况下 `emqx_ds_storage_bitfield_lts_counter_next` 的增长速度应快于 `seek`。

增加 `durable_storage.messages.layout.epoch_bits` 参数可以帮助改善此比率。
