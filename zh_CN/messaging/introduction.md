# 发布/订阅


作为世界级的 MQTT 消息服务器，EMQX 支持[发布/订阅消息模式](./mqtt-concepts.md#publish-subscribe-pattern)，这是 MQTT 协议的一个关键特性。EMQX 的发布/订阅功能提供了多种特性，使其非常适用于复杂和高性能的消息应用程序。这些特性包括支持通配符主题、基于主题的消息过滤、消息持久化和消息质量等级（QoS）设置。

EMQX 中的**发布**功能允许连接到 EMQX 的设备向特定主题发送消息。消息可以包含任何类型的数据，例如传感器读数、状态更新或命令。当设备发布消息到一个主题时，EMQX 接收该消息并将其转发给所有订阅了该主题的设备。

**订阅**功能允许设备从特定主题接收消息。设备可以订阅一个或多个主题，并接收在这些主题上发布的所有消息。这使得设备能够实时监控特定事件或数据流，而无需不断轮询更新。

<img src="./assets/pub-sub-pattern.png" alt="pub-sub-pattern" style="zoom:35%;" />

## 扩展的发布/订阅能力

EMQX 不仅支持原生的 MQTT 发布/订阅模型，还扩展了一系列强大的功能，以支持更高级的异步消息传递场景。

在本章节中，你将学习 [MQTT 核心概念](./mqtt-concepts.md)，同时你还将学习如何在 EMQX 中尝试发布/订阅功能，并使用 MQTT 客户端工具测试以下 MQTT 原生特性：

- [共享订阅](./mqtt-shared-subscription.md)
- [保留消息](./mqtt-retained-message.md)
- [遗嘱消息](./mqtt-will-message.md)
- [主题通配符](./mqtt-wildcard-subscription.md)

### EMQX 中的 MQTT 扩展功能

除了原生 MQTT 特性之外，EMQX 还提供了一些构建在基本发布/订阅模型之上的扩展能力。这些功能旨在处理更复杂的消息模式、离线消息消费、主题管理等需求：

- [排他订阅](./mqtt-exclusive-subscription.md)
- [延迟发布](./mqtt-delayed-publish.md)
- [自动订阅](./mqtt-auto-subscription.md)
- [主题重写](./mqtt-topic-rewrite.md)
- [消息队列](../message-queue/message-queue-concept.md)

### 消息队列（高级功能）

[消息队列](../message-queue/message-queue-concept.md)是 EMQX 6.0 引入的一项高级功能，它在 MQTT 发布/订阅模型的基础上，提供了持久化和异步的消息排队能力。即使订阅者处于离线状态，消息也可以被存储。同时它还支持高级的消息分发策略以及通过最后值语义进行的消息压缩。

与共享订阅或保留消息不同，消息队列提供以下能力：

- 与客户端连接状态无关的持久化消息存储
- 可配置的队列生命周期和消息的 TTL（生存时间）
- 基于队列的消息投递语义，支持细粒度的控制
