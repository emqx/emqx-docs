# EMQX 5.x 与 EMQX 6.0 之间的不兼容变更

## 停止支持的安装包

- [#15939](https://github.com/emqx/emqx/pull/15939) 停止为已达生命周期终止（EOL）的操作系统发布安装包：
  - Debian 10 (Buster)
  - 企业版 Linux (CentOS) 7
  - Ubuntu 18.04
  - Ubuntu 20.04
  - macOS 13 (Ventura)

## MQTT 会话持久化

如果之前未启用 MQTT 会话持久化功能，可以忽略本节内容。

在 EMQX 6.0 中，会话持久化及其消息的内部表示方式已发生变更。对于在 5.x 版本中启用了会话持久化的集群，在升级到 6.0 时必须重新创建一个不带历史数据的集群。

有关详细的升级说明，请参见[滚动升级文档](../deploy/rolling-upgrades.md#emqx-enterprise-rolling-upgrade)。

- [#15496](https://github.com/emqx/emqx/pull/15496) 会话持久化的状态已从 Mnesia 迁移到基于 EMQX 持久存储的新数据库。
  - 因此，6.0.0 之前创建的所有会话持久化状态在迁移过程中都会丢失。
  - 此改动解决了由于 Mnesia 事务隔离性不足而可能导致的会话状态损坏问题（见 [#14039](https://github.com/emqx/emqx/issues/14039)）。
  - 同时通过分片和更高效的数据表示方式提升了会话持久化的性能与可扩展性。


## 遗嘱消息行为

决定持久会话是否有资格发布遗嘱消息的授权检查现在在客户端断开连接时运行。
以前，它们在 `Will-Delay-Interval` 过期后运行。

## 配置变更

**会话持久化**

- `durable_sessions.heartbeat_interval` 已更名为 `durable_sessions.checkpoint_interval`。
- `durable_sessions.idle_poll_interval` 和 `durable_sessions.renew_streams_interval` 已被移除，因为会话现已完全事件驱动。
- `durable_sessions.session_gc_interval` 和 `durable_sessions.session_gc_batch_size` 已作为过时参数移除。
- `durable_storage.messages.n_sites` 参数已重命名为 `durable_storage.n_sites`。该参数现已适用于所有持久化存储。
- 新增了 `durable_storage.sessions` 和 `durable_storage.timers` 配置项。
- [#15734](https://github.com/emqx/emqx/pull/15734) 提升了会话持久化的可靠性和吞吐量。

**持久存储**

- `durable_storage.messages.n_sites` 已更名为 `durable_storage.n_sites`，该参数现适用于所有持久存储类型。
- 新增了 `durable_storage.sessions` 和 `durable_storage.timers` 配置项。

**RocketMQ**

- [#15635](https://github.com/emqx/emqx/pull/15635) `parameters.strategy` 字段不再接受键模板（此前会隐式选择 `key_dispatch` 策略）。
  现在必须显式设置 `parameters.strategy = key_dispatch`，并在 `parameters.key` 中指定键模板。

**平台支持**

- [#15613](https://github.com/emqx/emqx/pull/15613) 停止提供 Debian 10 的安装包构建。

## 速率限制

- [#15743](https://github.com/emqx/emqx/pull/15743) 监听器连接速率限制（`max_conn_rate` 和 `max_conn_burst`）现在按监听器而不是按接收器强制执行，恢复了 5.9.0 之前的行为。因此，来自 5.9.0、5.9.1 和 5.10.0 版本的配置不兼容：指定的速率必须按相应监听器配置的接收器数量进行放大。
