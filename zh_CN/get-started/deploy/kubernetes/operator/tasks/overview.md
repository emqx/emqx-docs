# 管理 EMQX 集群

本章介绍如何在 Kubernetes 集群中完成常见的 EMQX 配置和运维任务。

## 配置和设置

- License 和安全
  - [管理 License](./configure-emqx-license.md)
  - [为 EMQX 监听器启用 TLS](./configure-emqx-tls.md)
- 集群配置
  - [修改 EMQX 配置](./configure-emqx-config.md)
  - [启用 Core-Replicant 部署](./configure-emqx-core-replicant.md)
  - [启用持久化存储](./configure-emqx-persistence.md)
  - [配置 Pod 干扰预算](./configure-disruption-budgets.md)
  - [通过 LoadBalancer 访问 EMQX 集群](./configure-emqx-service.md)
  - [使用 HPA 自动伸缩 Replicant 节点](./configure-emqx-hpa.md)

## 升级和维护

- 升级
  - [配置滚动更新](./configure-emqx-rolling-update.md)
- 日志管理
  - [采集 EMQX 日志](./configure-emqx-log-collection.md)
  - [修改 EMQX 日志等级](./configure-emqx-log-level.md)

## 监控和性能

- [使用 Prometheus 监控 EMQX 集群](./configure-emqx-prometheus.md)
