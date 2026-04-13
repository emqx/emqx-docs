# 架构设计

本节将介绍 EMQX 的实现原理和技术架构，目前包括以下主题：

- [EMQX 集群](./cluster/introduction.md)

  EMQX 在单节点上已展现出卓越的性能，经过基准测试验证，可实现数百万的连接数。然而，为了保证可靠性和高可用性，EMQX 必须通过组建集群来实现横向扩展。本章节将探讨 MQTT Broker 集群化的复杂性，以及 EMQX 是如何设计来应对这些挑战的。

- [MQTT 会话持久化](./durability_introduction.md)（企业版功能）介绍了 EMQX 中持久化 MQTT 会话的技术架构，说明会话状态和消息如何被持久化存储，以确保在节点重启或网络中断后仍能正常恢复。

- [飞行窗口和消息队列](./design/inflight-window-and-message-queue.md)

  为了提高消息吞吐效率和减少网络波动带来的影响，EMQX 允许多个未确认的 QoS 1 和 QoS 2 报文同时存在于网路链路上。这些已发送但未确认的报文将被存放在飞行窗口（Inflight Window）中直至完成确认。但当飞行窗口到达长度限制时，EMQX 将不再发送后续的报文，而是将这些报文存储在消息队列（Message Queue）中。本节将介绍相关的技术原理与配置项信息。

- [消息重传](./design/retransmission.md)

  消息重传 (Message Retransmission) 是属于 MQTT 协议标准规范的一部分。本节将介绍消息重传的基础配置，协议规范与设计。
