# MQTT 协议介绍

## 概览

[MQTT](https://mqtt.org/) 是一种基于发布 / 订阅模式的轻量级消息传输协议，专门针对低带宽和不稳定网络环境的物联网应用而设计，可以用极少的代码为联网设备提供实时可靠的消息服务。

MQTT 协议广泛应用于物联网、移动互联网、智能硬件、车联网、智慧城市、远程医疗、电力、石油与能源等领域。

- MQTT v3.1.1 协议规范: [http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html)
- MQTT v5.0 协议规范: [http://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html](http://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html)

## 主要优势

1. 开放消息协议，简单易实现
2. 发布订阅模式，一对多消息发布
3. 基于 TCP/IP 网络连接
4. 1 字节固定报头，2 字节心跳报文，报文结构紧凑
5. 消息 QoS 支持，可靠传输保证
6. 灵活的消息传输，不关心 Payload 数据格式
7. 持续的会话感知能力，时刻知道设备是否在线

## 核心特性

以下是 MQTT 协议规范以及 EMQX 扩展的 MQTT 基本概念：

- [发布订阅模式](./mqtt-publish-and-subscribe.md)
- [会话与消息过期](./mqtt-session-and-message-expiry.md)
- [QoS](./mqtt-qos.md)
- [保留消息](./mqtt-retained-messages.md)
- [遗嘱消息](./mqtt-last-will-and-testament.md)
- [共享订阅](./mqtt-shared-subscription.md)
- [排它订阅](./mqtt-exclusive-subscription.md)
- [延迟发布](./mqtt-delayed-publish.md)
- [自动订阅](./mqtt-auto-subscription.md)
- [主题重写](./mqtt-topic-rewrite.md)
- [系统主题](./mqtt-system-topics.md)

## 与其他协议对比

由于物联网场景复杂多样，涉及到许多不同的设备、网络、应用程序和服务，因此诞生了多种物联网协议，MQTT 与其对比请参照 [主流物联网协议选择：MQTT、CoAP 还是 LwM2M](https://www.emqx.com/zh/blog/iot-protocols-mqtt-coap-lwm2m)。
