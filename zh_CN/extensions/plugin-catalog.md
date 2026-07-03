# 插件目录

本目录收录 EMQX 自定义插件的独立文档页面。

当标准产品功能无法完全满足需求，或者某个领域问题更适合通过扩展而非内建能力来解决时，EMQX 通常会选择开发插件。

有些插件会一直保持为特定场景的扩展能力；也有一些插件如果被证明适用于更广泛的真实业务场景，后续可能会被纳入标准 EMQX 功能。

本页面列出的插件均维护在 [`emqx.git` monorepo](https://github.com/emqx/emqx/tree/master/plugins) 中。

## 数据集成

[MQTT 磁盘队列桥接](./plugin-catalog/emqx-bridge-mqtt-dq.md)

该插件将 MQTT 消息从 EMQX 转发到远端 MQTT Broker，并通过磁盘队列在下游故障或间歇性网络中断时提升传输韧性。

## 数据治理

[UNS 治理](./plugin-catalog/emqx-unsgov.md)

该插件用于强制执行 Unified Namespace 主题结构，并可选地校验消息负载模式，帮助您在发布阶段统一主题层级与数据契约。

## 访问控制

[用户名会话配额](./plugin-catalog/emqx-username-quota.md)

该插件按用户名实施集群范围内的并发会话配额控制，并提供配额覆盖、基于快照的查询 API 以及管理级会话控制能力。

## 消息持久化

[离线消息](./plugin-catalog/emqx-offline-messages.md)

该插件将 MQTT 消息持久化到 MySQL 或 Redis，使订阅者重新上线后可以获取在离线期间到达的消息，覆盖标准 MQTT 会话持久化能力之外的场景。

## 运维

[热升级（Relup）](./plugin-catalog/emqx-relup.md)

该插件在运行中的 EMQX 节点上应用 `.relup` 代码变更指令，使运维人员可以在不重启 VM 的情况下发布补丁版本。
