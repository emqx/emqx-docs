# 升级 EMQX Operator

EMQX Operator 3.0 引入了 `apps.emqx.io/v3beta1` API，无法转换使用早期 API 版本创建的 EMQX 自定义资源。因此，从 Operator 2.3 升级到 3.0 时需要执行迁移，而不能进行原地升级。

EMQX Operator 计划在 3.1 版本中支持在线迁移流程。

## 从 Operator 2.3 迁移到 3.0

按照[从 EMQX Operator 2.3 迁移到 3.0](./migration/migrate-from-2.3-to-3.0.md)中的步骤，备份现有集群、转换其清单、使用 Operator 3.0 部署新集群并切换客户端流量。

## 从 Operator 2.2 升级到 2.3

如果必须继续使用 2.3 版本系列，请参阅 EMQX Operator 2.3 文档。Operator 3.0 不支持早于 `v3beta1` 的 EMQX API 版本。
