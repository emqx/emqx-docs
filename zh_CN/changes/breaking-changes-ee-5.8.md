# EMQX 5.8 中的不兼容变更

## e5.8.1

- [#13792](https://github.com/emqx/emqx/pull/13792) 黑名单查询 API `GET /banned` 现支持使用以下参数进行条件查找：

  - clientid
  - username
  - peerhost
  - like_clientid
  - like_username
  - like_peerhost
  - like_peerhost_net

  在新增黑名单记录时，对于未指定 `until`  参数的默认过期时间已从 1 年改为 `无限期`。

- [#13742](https://github.com/emqx/emqx/pull/13742) 修复了当以 `+` 作为第一级，或使用 `#` 作为通配符进行订阅时，错误接收到以 `$` 开头主题的保留消息的问题。

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
