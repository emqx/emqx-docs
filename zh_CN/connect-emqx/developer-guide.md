# 开发者指南

开发者指南旨在帮助开发者们快速入门 EMQX，并构建应用程序。开发者指南将探讨 MQTT 的核心概念和功能以及 EMQX 的一些扩展功能，还会介绍如何在 EMQX 仪表板中配置这些功能并使用客户端工具进行测试。具体将涵盖以下内容：

- [MQTT 核心概念](../messaging/mqtt-concepts.md)
- [MQTT 客户端工具演示](../messaging/publish-and-subscribe.md)
- [共享订阅](../messaging/mqtt-shared-subscription.md)
- [保留消息](../messaging/mqtt-retained-message.md)
- [遗嘱消息](../messaging/mqtt-will-message.md)
- [通配符订阅](../messaging/mqtt-wildcard-subscription.md)
- [排他订阅](../messaging/mqtt-exclusive-subscription.md)
- [延迟发布](../messaging/mqtt-delayed-publish.md)
- [自动订阅](../messaging/mqtt-auto-subscription.md)

由于 EMQX 支持 MQTT 协议，所以能够兼容大多数 MQTT 客户端库和 SDK。本指南中还提供了[代码示例](./introduction.md)，帮助开发者快速开始构建其 MQTT 项目。有关 MQTT 客户端 SDK 及其比较的完整列表，参见 [MQTT 客户端 & SDKs](https://www.emqx.com/zh/mqtt-client-sdk)。

::: tip

文档中并不包括所有的 SDK。

:::

EMQX 还为开发者提供了 API 文档来帮助开发。[REST API](../admin/api.md) 指导您快速上手使用管理监控 API。