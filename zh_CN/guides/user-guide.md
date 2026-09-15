---
prev:
  text: 'LLM 资源'
  link: '../get-started/llms-txt'
---

# 使用指南

本章节介绍如何执行各类 EMQX 运维和管理操作。

这些指南可帮助管理员和运维人员管理、配置、加固、监控和维护 EMQX 部署。本章节主要涵盖以下内容：

- [集群管理](./cluster/create-cluster.md)介绍如何创建和管理 EMQX 集群，包括集群架构、集群安全、负载均衡、节点疏散、集群负载重平衡以及性能调优。
- [配置 EMQX](./configuration/configuration.md)提供配置文件、配置项以及详细配置参考的基本信息。
- [命名空间](./multi-tenancy/namespace-overview.md)介绍如何在共享的 EMQX 集群中对 MQTT 客户端进行逻辑分组，并管理租户级隔离、配额和速率限制。
- [REST API](./api.md)介绍如何使用 EMQX 提供的 HTTP 管理 API 管理客户端、主题、订阅等资源。
- [命令行接口](./cli.md)介绍 EMQX 支持的各类启动与管理命令。
- [安全指南](./security-guide.md)涵盖网络与 TLS、认证、授权、黑名单、连接抖动检测、身份治理、审计日志、速率限制以及备份与恢复等内容。
- [MQTT 会话持久化](./durability/durability_introduction.md)指导您如何配置会话持久化功能，并为高可用数据副本设置参数。
- [EMQX Dashboard](./dashboard/introduction.md)全面介绍 EMQX 内置的管理控制台，帮助您了解如何管理和监控 EMQX 集群，并配置和使用所需功能。
- [日志及可观测性](./observability/overview.md)介绍 EMQX 中的指标观测和监控功能，便于系统监控和调试。
- [扩展](./extensions/introduction.md)介绍如何通过 Hook 和 gRPC Hook 等扩展机制扩展 EMQX。
- [插件](./extensions/plugins.md)帮助您开发、定制、管理和使用 EMQX 插件。
- [遥测](./telemetry/telemetry.md)介绍如何启用遥测并共享使用情况，以帮助改进产品。
