---
prev:
  text: '遥测'
  link: '/zh/emqx/latest/guides/telemetry/telemetry'
---

# 开发者指南

开发者指南旨在帮助开发者快速入门 EMQX，并构建物联网应用程序。本章节涵盖客户端连接、API 使用、数据处理、外部系统集成以及高级协议特性等内容，主要包括以下主题：

- [客户端 SDK](./connect-emqx/introduction.md) 提供了使用 C、Java、Go、Python 和 JavaScript 等主流 MQTT 客户端库连接 EMQX 的分步说明和代码示例。

- [实用教程](./tutorial/tutorial.md) 提供了涵盖客户端连接、数据采集、MQTT 通信优化、系统集成、安全以及部署等多个主题的实践指南。

- [规则引擎](./data-integration/rules.md) 介绍了 EMQX 内置的数据处理引擎，可实时对物联网数据进行提取、过滤、丰富和转换，并与数据集成功能配合使用。

- [数据集成](./data-integration/data-bridges.md) 介绍了如何通过 Sink 和 Source 组件将 EMQX 与数据库、消息队列、云服务等外部数据系统进行连接。

- [Flow 设计器](./flow-designer/introduction.md)（企业版功能）是一款可视化无代码工具，通过图形化界面将规则、动作和集成连接起来，快速构建数据处理流水线。

- [高级功能](./advanced-feature.md) 介绍了 EMQX 的扩展协议能力，包括 MQTT over WebSocket、MQTT over QUIC、集群连接、基于 MQTT 的文件传输、多协议网关以及客户端属性等功能。

- [架构设计](./architecture-introduction.md) 介绍了 EMQX 核心模块的设计原理，包括集群机制、MQTT 会话持久化、飞行窗口与消息队列，以及消息重传。

- [MQTT 参考指南](./mqtt-reference.md) 提供了 MQTT 协议的完整参考资料，涵盖协议版本、术语、功能特性以及原因码等内容。
