# EMQX 5.8 中的不兼容变更

## e5.8.9

- [#16062](https://github.com/emqx/emqx/pull/16062) 修复了 RocketMQ 动作忽略所配置的 payload 模板、而直接渲染整个规则输出的问题。
- [#16491](https://github.com/emqx/emqx/pull/16491) 停止为 macOS 13（Ventura）发布安装包。

## e5.8.6

- [#14802](https://github.com/emqx/emqx/pull/14802) 从此版本开始，通过 REST API 或 Dashboard 安装插件需要显式授权。
   用户必须在安装插件前，使用以下 CLI 命令获取权限：

  ```bash
  emqx ctl plugins allow NAME-VSN
  ```

  此更改提升了安全性，可防止未经授权的插件安装。使用 API 或 Dashboard 管理插件的用户需相应调整操作流程。

## e5.8.5

- [#14703](https://github.com/emqx/emqx/pull/14703) 引入了 `force_shutdown.max_heap_size` 最大允许值的变更，现将其设置为 `128GB`。如果之前将 `max_heap_size` 设置为超过 128GB 的值，升级后可能会导致问题，例如在更新或重新加载配置时出现问题。

## e5.8.4

- [#14360](https://github.com/emqx/emqx/pull/14360) 在请求 Prometheus 指标的 JSON 格式时，`client` 顶层键将始终是一个 JSON 对象数组，而不再是单个 JSON 对象。此更改可能会影响您的监控工具处理数据的方式。

- [#14370](https://github.com/emqx/emqx/pull/14370) IoTDB 数据集成配置更改：

  - 移除了自描述模板。EMQX 现在仅处理使用配置的数据模板的消息，不再尝试从消息 payload 中提取模板。

  - 每条 MQTT 消息现在只能携带单一的 `payload`。不再支持携带多个 payload 数组。因此，每条 MQTT 消息将作为单个原子插入操作（单次或批量插入）处理到 IoTDB。通过一条 MQTT 消息生成多个 IoTDB 操作不再可能。

  - `数据类型` 现在被视为普通值，而非模板值。

  - REST API 驱动程序现在仅支持 IoTDB 1.3.x 及更高版本。

  - Thrift 驱动程序现在支持“批量”模式。

    **重要提示**：为了防止批量模式中的时间戳重叠，建议使用 MQTT 消息时间戳（`${timestamp}`）或在 payload 中包含时间字段（例如，`${payload.time}`）。

## e5.8.3

- [#14305](https://github.com/emqx/emqx/pull/14305) 认证中移除了对哈希算法 `MD4`、`MD5` 和 `RIPEMD-160` 的支持，因为它们不符合 [NIST 安全哈希标准](https://www.nist.gov/publications/secure-hash-standard)。

## e5.8.2

- [#14004](https://github.com/emqx/emqx/pull/14004) 修复了集群连接中的一个问题，即在 `topics` 配置中，重叠的主题过滤器导致跨集群消息路由不一致且不完整。现在每个主题过滤器都会被单独处理，因此在集群连接的 `topics` 配置中，重叠的主题过滤器（例如，`t/1` 和 `t/+`）现在被视为无效配置。如果检测到重叠过滤器，连接将无法启动，以防止路由问题。
- [#14015](https://github.com/emqx/emqx/pull/14015) Kafka/Confluent/Azure Event Hub Producer 动作不再支持带有动态主题（即包含占位符的主题）的磁盘缓冲。现在仅支持内存和混合模式。
- [#14106](https://github.com/emqx/emqx/pull/14106) 增加了验证，防止单个 Kafka 消费者连接器包含具有重复 Kafka 主题的 source。如果需要在多个 source 中使用相同的主题，请创建新的连接器及相应的 source。

## e5.8.1

- [#13792](https://github.com/emqx/emqx/pull/13792) 在新增黑名单记录时，对于未指定 `until`  参数的默认过期时间已从 1 年改为 `无限期`。

- [#13742](https://github.com/emqx/emqx/pull/13742) 修复了客户端在订阅主题 `#` 或 `+` 时，会收到以 `$` 开头的主题的保留消息的问题。

  该修复满足了 [MQTT-4.7.2-1](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901246) 的要求。


## e5.8.0

- [#13080](https://github.com/emqx/emqx/pull/13080) 将 `mqtt.retry_interval` 配置的默认值从 30 秒更新为 `infinity`。

  之前，EMQX 默认每 30 秒会自动重试消息发送。新的默认值设置为 `infinity` 后，EMQX 将不再自动重试消息发送。此更改符合 MQTT 规范标准，因为通常不建议在会话期间重试消息传递。

  我们理解一些用户依赖于重试功能，因此仍然保留了配置特定重试间隔的选项，以确保向后兼容性。

- [#13190](https://github.com/emqx/emqx/pull/13190) 停止对 CentOS 7 和 Ubuntu 18 的版本支持。由于这些操作系统已达到生命周期终止状态，EMQX 将不再提供这些操作系统的构建版本。

- [#13248](https://github.com/emqx/emqx/pull/13248) 替换了 `builtin` 持久存储后端，引入了两个新的后端以提供更好的灵活性和可扩展性：

  - **`builtin_local`**：一种不支持复制的持久存储后端，适用于单节点部署。此后端可用于 EMQX 的开源版和企业版，但不兼容多节点集群。
  - **`builtin_raft`**：一种使用 Raft 共识算法进行数据复制的持久存储后端。此后端仅在 EMQX 企业版中提供，提供增强的数据持久性和容错能力。

  此外，为更好地反映其功能，对几个 Prometheus 指标进行了重命名：

  - `emqx_ds_egress_batches` 重命名为 `emqx_ds_buffer_batches`
  - `emqx_ds_egress_batches_retry` 重命名为 `emqx_ds_buffer_batches_retry`
  - `emqx_ds_egress_batches_failed` 重命名为 `emqx_ds_buffer_batches_failed`
  - `emqx_ds_egress_messages` 重命名为 `emqx_ds_buffer_messages`
  - `emqx_ds_egress_bytes` 重命名为 `emqx_ds_buffer_bytes`
  - `emqx_ds_egress_flush_time` 重命名为 `emqx_ds_buffer_flush_time`

- [#13526](https://github.com/emqx/emqx/pull/13526) 移除了开源版中的核心副本功能。从 5.8 版本开始，所有运行开源版的节点将以核心角色运行。此更改不会影响企业版用户，他们将继续拥有核心副本功能。此外，已移除不再需要的过时配置参数 `cluster.core_nodes`。

- [#13372](https://github.com/emqx/emqx/pull/13372) 现在，网关接受的连接数量受 license 的约束，以确保符合允许的连接限制。
