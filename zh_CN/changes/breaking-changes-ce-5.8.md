# EMQX 5.8 中的不兼容更改

## v5.8.1

- [#13792](https://github.com/emqx/emqx/pull/13792) 在新增黑名单记录时，对于未指定 `until`  参数的默认过期时间已从 1 年改为 `无限期`。

- [#13742](https://github.com/emqx/emqx/pull/13742) 修复了客户端在订阅主题 `#` 或 `+` 时，会收到以 `$` 开头的主题的保留消息的问题。

  该修复满足了 [MQTT-4.7.2-1](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901246) 的要求。


## v5.8.0

- [#13080](https://github.com/emqx/emqx/pull/13080) 将 `mqtt.retry_interval` 配置的默认值从 30 秒更新为 `infinity`。

  之前，EMQX 默认每 30 秒会自动重试消息发送。现在，默认值设置为 `infinity`，EMQX 将不再自动重试消息发送。此更改符合 MQTT 规范标准，因为通常不建议在会话期间重试消息传递。

  我们理解一些用户依赖于重试功能，因此仍然保留了配置特定重试间隔的选项，以确保向后兼容性。

- [#13190](https://github.com/emqx/emqx/pull/13190) 停止对 CentOS 7 和 Ubuntu 18 的版本支持。由于这些操作系统已达到生命周期结束状态，EMQX 将不再提供这些操作系统的构建版本。

- [#13248](https://github.com/emqx/emqx/pull/13248) 替换了 `builtin` 持久化存储后端，引入了两个新的后端以提供更好的灵活性和可扩展性：

  - **`builtin_local`**：一种不支持复制的持久化存储后端，适用于单节点部署。此后端可用于 EMQX 的开源版和企业版，但不兼容多节点集群。
  - **`builtin_raft`**：一种使用 Raft 共识算法进行多节点数据复制的持久化存储后端。此后端仅在 EMQX 企业版中提供，提供增强的数据持久性和容错能力。

  此外，为更好地反映其功能，多个 Prometheus 指标已重命名：

  - `emqx_ds_egress_batches` 已重命名为 `emqx_ds_buffer_batches`
  - `emqx_ds_egress_batches_retry` 已重命名为 `emqx_ds_buffer_batches_retry`
  - `emqx_ds_egress_batches_failed` 已重命名为 `emqx_ds_buffer_batches_failed`
  - `emqx_ds_egress_messages` 已重命名为 `emqx_ds_buffer_messages`
  - `emqx_ds_egress_bytes` 已重命名为 `emqx_ds_buffer_bytes`
  - `emqx_ds_egress_flush_time` 已重命名为 `emqx_ds_buffer_flush_time`

- [#13526](https://github.com/emqx/emqx/pull/13526) 移除了开源版中的核心副本功能。从 5.8 版本开始，所有运行开源版的节点将以核心角色运行。此更改不影响企业版用户，他们将继续拥有核心副本功能。此外，已移除不再需要的过时配置参数 `cluster.core_nodes`。

- **Dashboard 更新**：以下功能在开源版 Dashboard 中被移除或受限：

  - 监控：
    - 延迟发布
    - 告警
  - 访问控制：
    - 客户端认证（LDAP）
    - 客户端授权（LDAP）
    - 连接抖动
  - 集成：
    - Flow 设计器
  - 管理：
    - 监控
    - MQTT 高级特性
      - 主题重写
      - 自动订阅
      - 延迟发布
  - 问题分析：
    - 主题监控
    - 慢订阅