# 设计与实现

本章将介绍 EMQX 的实现原理和技术架构，目前包括以下主题：

- [飞行窗口和消息队列](./inflight-window-and-message-queue.md)

  为了提高消息吞吐效率和减少网络波动带来的影响，EMQX 允许多个未确认的 QoS 1 和 QoS 2 报文同时存在于网路链路上。这些已发送但未确认的报文将被存放在飞行窗口（Inflight Window）中直至完成确认。但当飞行窗口到达长度限制时，MQX 将不再发送后续的报文，而是将这些报文存储在消息队列（Message Queue）中。本节将介绍相关的技术原理与配置项信息。

- [消息重传](./retransmission.md)

  消息重传 (Message Retransmission) 是属于 MQTT 协议标准规范的一部分。本节将介绍消息重传的基础配置，协议规范与设计。

更多主题正在持续上线中，敬请期待。
