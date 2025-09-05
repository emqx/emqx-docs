# EMQX 5.8 已知问题

## e5.8.8

- **删除前置条目后，禁用消息转换或 Schema 验证不生效（自 5.8.0 起，将在 5.8.8 修复）。**

  如果你删除了一条消息转换或 Schema 验证条目，然后再禁用列表中其后的任意条目，该条目仍会保持启用状态。

  > **解决方法：**
  > 在任意一个 EMQX 节点上执行以下命令。
  >
  > ```
  > $ emqx eval "begin ets:delete_all_objects(emqx_message_transformation_index), emqx_message_transformation_config:load() end."
  > ```

- **节点重启后不会加载外部 Schema Registry（自 5.8.1 起，将在 5.8.8 修复）。**

## e5.8.6

| 始于版本 | 问题描述                                                     | 解决方法                                                     | 状态              |
| -------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ----------------- |
| 5.8.5    | <!-- https://emqx.atlassian.net/browse/EMQX-14055 -->**由于尝试连接已离开集群的节点失败，日志中偶尔出现 RPC 错误**<br />这种日志消息通常在常规的滚动升级后出现。以下是此类日志时间的示例：<br />`pid: <0.123456.0>, msg: event=connect_to_remote_server, peer=emqx@10.11.12.13, port=5370, reason=ehostunreach`<br />这些消息的出现并不表示会影响现有连接的消息传递。 | 在集群中的任意一台 EMQX 主机上运行以下命令。将 `emqx@10.11.12.13` 替换为错误日志中提到的实际节点名称。<br />在执行此命令之前，请再次确认该节点不再是集群的一部分。<br />`$ emqx eval "emqx_router:cleanup_routes('emqx@10.11.12.13')"` | 已在 5.9.0 中修复 |
| 5.4.0    | **TLS 监听器默认配置启动后，无法热更新为只使用 tlsv1.3**<br />错误信息如：`incompatible,[client_renegotiation,{versions,['tlsv1.3']}]` | 先禁用监听器，修改配置后再启用。                             | 已在 5.9.0 中修复 |
| 5.0.0    | **Linux 单调时钟回调导致 EMQX 节点重启**<br />在某些虚拟 Linux 环境中，操作系统无法保持时钟的单调性，这可能会导致 Erlang VM 因为错误消息 `OS monotonic time stepped backwards!` 而退出。 | 对于这类环境，可以在 `etc/vm.args` 中将 `+c` 标志设置为 `false`。 |                   |
| 5.0.0    | **IoTDB 在批处理模式下（当 `batch_size > 1` 时）可能无法正常工作**<br />出现该问题的原因是 EMQX 使用了 IoTDB v1 API，而该 API 不支持原生的批处理操作。为模拟批处理操作，系统采用了迭代方式，然而，此方法不是原子的，可能会导致出现错误。 | -                                                            |                   |
| 5.8.1    | IoTDB 的 Thrift 驱动不支持 `async` 模式。                    | -                                                            |                   |
| 5.3.0    | **基于 SAML 的单点登录限制**<br />EMQX Dashboard 支持基于安全断言标记语言（SAML）2.0标准的单点登录（SSO），并与 Okta 和OneLogin 作为身份提供商集成。然而，基于 SAML 的 SSO 目前不支持证书签名验证机制，并且由于其复杂性，无法与 Azure Entra ID 兼容。 | -                                                            |                   |

## e5.8.4

| 始于版本 | 问题描述                                                     | 解决方法                                | 状态              |
| -------- | ------------------------------------------------------------ | --------------------------------------- | ----------------- |
| 5.0.0    | <!-- https://emqx.atlassian.net/browse/EMQX-12290 -->**如果一个新节点在原节点停止时加入集群，原节点无法启动**<br />在包含两个或更多节点的集群中，如果在某些节点停止运行时有新节点加入集群，那么这些停止的节点将无法重新启动，并会产生如下日志<br />`2024-10-03T17:13:45.063985+00:00 [error] Mnesia('emqx@172.17.0.5'): ** ERROR ** (core dumped to file: "/opt/emqx/MnesiaCore.emqx@172.17.0.5_1727_975625_63176"), ** FATAL ** Failed to merge schema: {aborted,function_clause}` | 删除 `data/mnesia` 目录并重新启动节点。 | 已在 5.8.5 中修复 |
| 5.8.0    | <!-- https://emqx.atlassian.net/browse/EMQX-13886 -->**分片副本集变化在丢失节点数量达到一定程度后卡住**<br />该问题仅在启用了持久会话并且后端使用 DS Raft 存储时发生。<br />当作为持久存储数据复制站点的节点在没有先交接数据的情况下永久离开集群时，可能会导致任何请求的副本集转换永远无法完成。<br />以下是一个简化的示例，展示了在 `emqx ctl ds info` 输出中的表现。在此示例中，节点 `emqx@emqxc1-core0.local` 在仍然负责并且是所有分片的唯一复制站点的情况下离开了集群，然后请求 `emqx@emqxc2-core0.local` 接管并执行 `emqx ds join messages ABCDEF2222222222`。<br />`Site`<br />`ABCDEF1111111111 'emqx@emqxc1-core0.local' (!) UNIDENTIFIED`<br />`ABCDEF2222222222 'emqx@emqxc2-core0.local' up`<br />`<...>`<br /><br />`Shard            Replicas`<br />`messages/0       (!) ABCDEF1111111111`<br />`messages/1       (!) ABCDEF1111111111`<br />`<...>`<br />`messages/9       (!) ABCDEF1111111111`<br /><br />`Shard             Transitions`<br />`messages/0        +ABCDEF2222222222 -ABCDEF1111111111`<br />`messages/1        +ABCDEF2222222222 -ABCDEF1111111111`<br />`<...>`<br />`messages/9        +ABCDEF2222222222 -ABCDEF1111111111`<br />在这个例子中，转换 `+ABCDEF2222222222` 永远不会完成。 | -                                       | 已在 5.8.5 中修复 |

## e5.8.1

| 始于版本 | 问题描述                                                     | 解决方法                                                     | 状态              |
| -------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ----------------- |
| 5.8.0    | <!-- https://emqx.atlassian.net/browse/EMQX-13248 -->**Kafka 磁盘缓冲区目录名称变化**<br />引入的用于 Kafka（Azure EventHubs，Confluent Platform）生产者集成的动态主题模板，导致了磁盘缓冲区目录名称的不兼容变化。 <br />如果使用 `disk` 缓存模式，请等待 5.8.2 版本以避免升级到新版本后缓冲的消息丢失。<br />如果使用 `hybrid` 缓存模式，升级后需要手动清理旧目录。 | -                                                            | 已在 5.8.2 中修复 |
| 5.8.0    | <!-- https://emqx.atlassian.net/browse/EMQX-13242 -->**Kafka磁盘缓冲区恢复问题**<br />如果使用 `disk` 缓存模式，在节点重启后，Kafka（Azure EventHubs，Confluent Platform）生产者不会自动从磁盘开始向 Kafka 发送数据。只有在有新消息触发动态添加主题生产者时，才会开始发送数据。 | -                                                            | 已在 5.8.2 中修复 |
| 5.4.0    | **查看审计事件时性能下降**<br />启用审计日志并在 Dashboard 中查看特定事件时，可能会在极少数情况下导致显著的性能下降，甚至在极端情况下（尤其是内存受限的节点）导致 EMQX 节点崩溃。已知会引发此问题的事件包括备份和恢复 API 请求，以及在 EMQX 远程控制台中执行操作大型数据结构的命令。在这些情况下，节点启动和响应时间可能也会变长。 | 通过 Dashboard 调整**最大 Dashboard 记录数**，或将 `log.audit.max_filter_size` 设置为较低的值。随着新事件的记录，问题事件将逐渐从审计日志中清除。 | 已在 5.8.2 中修复 |
| 5.8.1    | **`GET /monitor` HTTP API 和 Dashboard 中的指标值失真**<br />使用 `GET /monitor` HTTP API 时，该 API 同时也为 Dashboard 提供数据。如果将时间窗口从 1 小时调整为更长的时间范围，可能会导致最近 1 小时内的数据点显示失真。例如，3 个连接可能错误地显示为 9 个或更多。对于过去 1 小时内的数据点，这个问题仅为视觉上的失真。然而，对于超过 1 小时的数据，失真是不可逆的。<br />受影响的指标：<br />`disconnected_durable_sessions`<br />`subscriptions_durable`<br />`subscriptions`<br />`topics`<br />`connections`<br />`live_connections` | -                                                            | 已在 5.8.2 中修复 |

## e5.8.0

| 始于版本 | 问题描述                                                     | 解决方法 | 状态              |
| -------- | ------------------------------------------------------------ | -------- | ----------------- |
| 5.0.0    | **节点崩溃竞态条件**<br />如果节点在 RPC 通道建立过程中关闭，可能导致对等节点崩溃。 | -        | 已在 5.8.1 中修复 |
