# 高级功能

随着应用场景的丰富 和 MQTT 协议的不断发展，EMQX 也在不断引入新的功能，以满足物联网应用中愈加复杂的需求。本章节将重点探讨 EMQX 的高级功能，主要涵盖以下内容：

- [MQTT over QUIC](./introduction.md) 介绍了 EMQX 中的这一开创性功能，以及如何在 EMQX 中启用该功能。
- [集群连接](../cluster-linking/introduction.md) （企业版功能）介绍了一项新功能，可以连接多个独立的集群，并促进分散在不同地理位置的集群之间的客户端通信。
- [基于 MQTT 的文件传输](../file-transfer/introduction) （企业版功能）让您了解如何将大文件通过 MQTT 协议传输到 EMQX。
- [多协议网关](../gateway/gateway.md) 介绍了几种常用网关的设计和使用，包括 Stomp、 MQTT-SN、CoAP、LwM2M 和 ExProto。
- [客户端属性](../client-attributes/client-attributes.md) 功能允许开发人员为 MQTT 客户端定义和设置附加属性，从而增强访问控制、数据集成和 MQTT 扩展功能，同时支持灵活的模板化，以实现个性化的客户端配置和简化的认证过程。

这些功能扩展了 EMQX 的能力，使您能够利用更多协议，提升 MQTT 应用的连接性和互操作性。
