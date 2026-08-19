# 插件目录

本目录收录 EMQX 自定义插件的独立文档页面。

当标准产品功能无法完全满足需求，或者某个领域问题更适合通过扩展而非内建能力来解决时，EMQX 通常会选择开发插件。

有些插件会一直保持为特定场景的扩展能力；也有一些插件如果被证明适用于更广泛的真实业务场景，后续可能会被纳入标准 EMQX 功能。

本页面列出的插件均维护在 [`emqx.git` monorepo](https://github.com/emqx/emqx/tree/master/plugins) 中。

## 安全

[EMQX ACME 插件](./plugin-catalog/6.1/emqx-acme.md)

该插件通过 Let's Encrypt 等兼容 ACME 的证书颁发机构，为 EMQX SSL 监听器自动签发和续期 TLS 证书。

## 运维

[热升级（Relup）](./plugin-catalog/6.1/emqx-relup.md)

该插件在运行中的 EMQX 节点上应用 `.relup` 代码变更指令，使运维人员可以在不重启 VM 的情况下发布补丁版本。

[备份同步（Backup Sync）](./plugin-catalog/6.1/emqx-backup-sync.md)

该插件通过数据备份 API，定期将选定的备份数据从主 EMQX 集群同步到备用集群，使备用集群保持同步以用于灾难恢复。

## 数据集成

[带磁盘队列的 MQTT 桥接](./plugin-catalog/6.1/emqx-bridge-mqtt-dq.md)

该插件将本地 MQTT 消息转发到另一个 MQTT Broker，并在磁盘上缓冲消息，以在网络中断时提供更好的可靠性。

## 消息持久化

[离线消息](./plugin-catalog/6.1/emqx-offline-messages.md)

该插件将 MQTT 消息持久化到 MySQL 或 Redis，使订阅者重新上线后可以获取在离线期间到达的消息，覆盖标准 MQTT 会话持久化能力之外的场景。

## 连接管理

[按用户名的会话配额](./plugin-catalog/6.1/emqx-username-quota.md)

该插件在集群范围内按用户名强制执行会话配额，当某个用户名达到配置的上限时，以 `quota_exceeded` 拒绝其认证。

## 命名空间治理

[UNS 治理](./plugin-catalog/6.1/emqx-unsgov.md)

该插件强制执行统一命名空间（UNS）的主题结构，并可对受 UNS 治理的发布主题的消息载荷进行校验。
