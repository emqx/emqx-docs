# EMQX 5.8 已知问题

## e5.8.1

- **如果一个新节点在原节点停止时加入集群，原节点无法启动 （始于 5.0）**

  在包含两个或更多节点的集群中，如果在某些节点停止运行时有新节点加入集群，那么这些停止的节点将无法重新启动，并会产生如下日志： `2024-10-03T17:13:45.063985+00:00 [error] Mnesia('emqx@172.17.0.5'): ** ERROR ** (核心转储至文件: "/opt/emqx/MnesiaCore.emqx@172.17.0.5_1727_975625_63176"), ** FATAL ** 合并 schema 失败: {aborted,function_clause}`

  > **解决方法：** 删除 `data/mnesia` 目录并重新启动节点。

  <!-- https://emqx.atlassian.net/browse/EMQX-12290 -->

- **Kafka 磁盘缓冲区目录名称变化（始于 5.8.0）**

  引入的用于 Kafka（Azure EventHubs，Confluent Platform）生产者集成的动态主题模板，导致了磁盘缓冲区目录名称的不兼容变化。 如果使用 `disk` 缓存模式，请等待 5.8.2 版本以避免升级到新版本后缓冲的消息丢失。 如果使用 `hybrid` 缓存模式，升级后需要手动清理旧目录。

  <!-- https://emqx.atlassian.net/browse/EMQX-13248 -->

- **Kafka磁盘缓冲区恢复问题（始于 5.8.0）**

  如果使用 `disk` 缓存模式，在节点重启后，Kafka（Azure EventHubs，Confluent Platform）生产者不会自动从磁盘开始向 Kafka 发送数据。只有在有新消息触发动态添加主题生产者时，才会开始发送数据。 该问题将在 5.8.2 版本中修复。

  <!-- https://emqx.atlassian.net/browse/EMQX-13242 -->

- **基于 SAML 的单点登录限制（始于 5.3）**

  EMQX Dashboard 支持基于安全断言标记语言（SAML）2.0标准的单点登录（SSO），并与 Okta 和OneLogin 作为身份提供商集成。然而，基于 SAML 的 SSO 目前不支持证书签名验证机制，并且由于其复杂性，无法与 Azure Entra ID 兼容。

## e5.8.0

- **节点崩溃竞态条件（始于 5.0，已在 5.8.1 中修复）**

  如果节点在 RPC 通道建立过程中关闭，可能导致对等节点崩溃。

- **删除与 Source 名称相同的 Action 时出现 500 错误（始于 5.5.0）**

  在 Dashboard 数据集成中，如果 Action 列表中的一个 Action 条目的名称与 Source 列表中的一个 Source 条目名称相同，删除该 Action 条目将返回错误代码 500。

  错误关键词示例：`{name_clash_action_source, mqtt, <<"test">>}`，其中 `mqtt` 和 `test` 分别是具有相同名称的 Source 条目的类型和名称。

  > **解决方法：** 您可以执行以下命令来删除该 Action 条目，例如：
  >
  > ```bash
  > ./bin/emqx eval 'emqx_bridge_v2:remove(mqtt, <<"test">>)'
  > ```
  >
  > 然后，您需要检查规则列表并删除与此 Action 关联的任何规则。
