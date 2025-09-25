# EMQX 5.x 与 EMQX 6.0 之间的不兼容变更

## 停止支持的安装包

- [#15939](https://github.com/emqx/emqx/pull/15939) 停止为已达生命周期终止（EOL）的操作系统发布安装包：
  - 企业版 Linux (CentOS) 7
  - Ubuntu 20.04
  - macOS 13 (Ventura)

## 持久会话

如果之前未启用持久会话功能，可以忽略以下信息。

6.0 版本更改了持久会话和消息的内部表示。
如果集群先前在 5.x 版本上运行并启用了该功能，则必须从全新状态重新创建。

- [#15496](https://github.com/emqx/emqx/pull/15496) 持久会话的状态已从 Mnesia 移至基于 EMQX 持久存储的新数据库。
  因此，在 6.0.0 版本发布之前创建的持久会话状态将在迁移过程中丢失。

  这解决了由于 Mnesia 事务隔离不足可能导致的会话状态损坏问题（如 [#14039](https://github.com/emqx/emqx/issues/14039) 中报告）。
  此更改还通过分片和更高效的数据表示提高了持久会话的总体性能。


## 遗嘱消息行为

决定持久会话是否有资格发布遗嘱消息的授权检查现在在客户端断开连接时运行。
以前，它们在 `Will-Delay-Interval` 过期后运行。

## 配置变更

- `durable_sessions.heartbeat_interval` 参数已重命名为 `durable_sessions.checkpoint_interval`。

- `durable_sessions.idle_poll_interval` 和 `durable_sessions.renew_streams_interval` 参数已被移除，因为会话已变为完全基于事件。

- `durable_sessions.session_gc_interval` 和 `durable_sessions.session_gc_batch_size` 参数因过时而被移除。

- `durable_storage.messages.n_sites` 参数已重命名为 `durable_storage.n_sites`。此参数已成为所有持久存储的通用参数。

- 为新的持久存储添加了配置：`durable_storage.sessions` 和 `durable_storage.timers`。
- [#15613](https://github.com/emqx/emqx/pull/15613) 停止为 Debian 10 发布软件包。

- [#15635](https://github.com/emqx/emqx/pull/15635) RocketMQ 动作中的 `parameters.strategy` 字段不再接受密钥模板（该模板隐式选择了 `key_dispatch` 策略）。
  用户必须显式设置 `parameters.strategy = key_dispatch` 并在 `parameters.key` 中提供密钥模板。

- [#15734](https://github.com/emqx/emqx/pull/15734) 提高了持久会话的可靠性和吞吐量。

## 速率限制

- [#15743](https://github.com/emqx/emqx/pull/15743) 监听器连接速率限制（`max_conn_rate` 和 `max_conn_burst`）现在按监听器而不是按接收器强制执行，恢复了 5.9.0 之前的行为。因此，来自 5.9.0、5.9.1 和 5.10.0 版本的配置不兼容：指定的速率必须按相应监听器配置的接收器数量进行放大。