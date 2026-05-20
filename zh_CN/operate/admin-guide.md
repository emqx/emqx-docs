---
prev:
  text: 'MQTT 参考指南'
  link: '../develop/mqtt-reference'
---

# 管理员指南

本章节旨在帮助管理员和运维人员有效地管理和维护 EMQX。在本章节中，我们将探讨各种管理任务，并提供全面的指导和最佳实践，以确保您的 EMQX 集群平稳、高效地运行。本章节主要涵盖以下内容：

- [集群管理](./cluster/create-cluster.md)介绍了如何创建和管理 EMQX 集群，包括集群安全、负载均衡配置、节点疏散与集群负载重平衡以及性能调优。
- [配置 EMQX](./configuration/configuration.md) 为您提供了配置文件基本信息、配置项以及详细的配置参考信息。
- [REST API](../develop/api.md) 指导您快速上手 EMQX 提供的 HTTP 管理 API，用于管理客户端、主题、订阅等资源。
- [命令行接口](./cli.md)介绍了 EMQX 支持的各类启动与管理命令。
- [安全指南](./security-guide.md)涵盖了网络与 TLS 配置、认证、授权、禁用客户端、连接抖动检测、身份管理、API 密钥以及审计日志等内容。
- [MQTT 会话持久化](./durability/management.md)指导您如何配置和管理会话持久化功能以及如何为高可用性数据副本设置参数。
- [EMQX Dashboard](./dashboard/introduction.md) 为您全面介绍 EMQX 内置的管理控制台，您将了解如何管理和监控 EMQX 集群并配置和使用所需的各项功能。
- [日志及可观测性](./observability/overview.md)介绍了 EMQX 中的指标观测和监控功能，便于您进行系统监控和调试。
- [插件与扩展](./extensions/introduction.md)帮助您通过开发插件来扩展 EMQX 的功能。
- [遥测](./telemetry/telemetry.md)介绍了通过启用遥测来共享您的使用情况以帮助产品改进。遥测功能仅针对 EMQX 开源版。

