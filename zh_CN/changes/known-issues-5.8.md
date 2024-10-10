# EMQX 5.8 已知问题

## e5.8.1

- **如果一个新节点在原节点停止时加入集群，原节点无法启动 （始于5.0）**

  在包含两个或更多节点的集群中，如果在某些节点停止运行时有新节点加入集群，那么这些停止的节点将无法重新启动，并会产生如下日志： `2024-10-03T17:13:45.063985+00:00 [error] Mnesia('emqx@172.17.0.5'): ** ERROR ** (核心转储至文件: "/opt/emqx/MnesiaCore.emqx@172.17.0.5_1727_975625_63176"), ** FATAL ** 合并 schema 失败: {aborted,function_clause}`

  > **解决方法：** 删除 `data/mnesia` 目录并重新启动节点。

  <!-- https://emqx.atlassian.net/browse/EMQX-12290 -->

- **Kafka 磁盘缓冲区目录名称变化（始于5.8.0）**

  引入的用于 Kafka（Azure EventHubs，Confluent Platform）生产者集成的动态主题模板，导致了磁盘缓冲区目录名称的不兼容变化。 如果使用 `disk` 缓存模式，请等待5.8.2版本以避免升级到新版本后缓冲的消息丢失。 如果使用 `hybrid` 缓存模式，升级后需要手动清理旧目录。

  <!-- https://emqx.atlassian.net/browse/EMQX-13248 -->

- **Kafka磁盘缓冲区恢复问题（始于5.8.0）**

  如果使用 `disk` 缓存模式，在节点重启后，Kafka（Azure EventHubs，Confluent Platform）生产者不会自动从磁盘开始向 Kafka 发送数据。只有在有新消息触发动态添加主题生产者时，才会开始发送数据。 该问题将在5.8.2版本中修复。

  <!-- https://emqx.atlassian.net/browse/EMQX-13242 -->

- **基于 SAML 的单点登录限制（始于5.3）**

  EMQX Dashboard 支持基于安全断言标记语言（SAML）2.0标准的单点登录（SSO），并与 Okta 和OneLogin 作为身份提供商集成。然而，基于 SAML 的 SSO 目前不支持证书签名验证机制，并且由于其复杂性，无法与 Azure Entra ID 兼容。

## e5.8.0

- **节点崩溃竞态条件（始于5.0，已在5.8.1中修复）**

  如果节点在 RPC 通道建立过程中关闭，可能导致对等节点崩溃。
