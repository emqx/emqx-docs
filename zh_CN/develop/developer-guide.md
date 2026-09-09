---
prev:
  text: '遥测'
  link: '../guides/telemetry/telemetry'
---

# 开发者指南

开发者指南帮助开发者将客户端连接到 EMQX、处理 MQTT 数据、集成外部系统，并使用 EMQX 高级功能。本章节主要包括：

- [客户端 SDK](./connect-emqx/introduction.md) 提供使用 C、Java、Go、Python 和 JavaScript MQTT 客户端连接 EMQX 的 SDK 示例。

- [使用 curl 连接 EMQX](./connect-emqx/curl.md) 介绍如何通过命令行发布和订阅 MQTT 主题。

- [使用 Node-RED 连接 EMQX](./connect-emqx/node-red.md) 介绍如何将 Node-RED 连接到 EMQX，并构建 MQTT 消息流。

- [实用教程](./tutorial/tutorial.md) 提供 MQTT 编程和常见 EMQX 开发场景的实践指南。

- [规则引擎](./data-integration/rules.md) 介绍了 EMQX 内置的数据处理引擎，可实时对物联网数据进行提取、过滤、丰富和转换，并与数据集成功能配合使用。

- [数据智能中心](./data-integration/smart-data-hub.md) 提供一站式 MQTT 数据处理能力，用于管理 Schema、验证数据并实时转换消息。

- [数据集成](./data-integration/data-bridges.md) 介绍了如何通过 Sink 和 Source 组件将 EMQX 与数据库、消息队列、云服务等外部数据系统进行连接。

- [Flow 设计器](./flow-designer/introduction.md)（企业版功能）是一款可视化无代码工具，通过图形化界面将规则、动作和集成连接起来，快速构建数据处理流水线。

- [高级功能](./advanced-features.md) 介绍消息队列、MQTT over WebSocket、MQTT over QUIC、集群连接、基于 MQTT 的文件传输、多协议网关和客户端属性等高级能力。

- [EMQX AI](./emqx-ai/overview.md) 介绍 EMQX 的 AI 能力，包括 MCP over MQTT、MCP Bridge、SDK，以及实时音视频 AI 服务。

- [架构设计](./architecture-introduction.md) 介绍 EMQX 集群、持久化存储、飞行窗口与消息队列，以及消息重传等架构内容。

- [MQTT 参考指南](./mqtt-reference.md) 提供 MQTT 协议版本、术语、功能特性以及原因码等参考信息。
