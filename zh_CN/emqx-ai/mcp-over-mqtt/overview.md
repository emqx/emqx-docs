# MCP over MQTT

MCP over MQTT 是一种基于 MQTT 协议的 [MCP](https://modelcontextprotocol.io/docs/getting-started/intro) 实现，旨在为 AI 应用提供高效、低延迟的工具调用以及消息通信能力。通过 MCP over MQTT，开发者可以轻松地在物联网系统中集成 AI 模型和服务，实现设备与 AI 之间的无缝交互。

## 为什么使用 MQTT 实现 MCP

MQTT 是一种轻量级且广泛使用的 IoT 和边缘计算协议。它旨在应对不可靠的网络和低带宽的情况，因此非常适合边缘设备与云服务之间的通信。

通过使用 MQTT 作为 MCP 的传输层，我们将 MCP 的应用范围扩展到更广泛的场景中，包括边缘计算、物联网和云服务等任何需要使用 MQTT 的地方。

## 特性

MCP over MQTT 支持 MCP 的所有特性，并增加了以下特性：

- **内置服务注册和发现**: MCP 客户端可以从 MQTT 代理发现可用的 MCP 服务器。

- **内置负载均衡和可扩展性**: MCP 服务器可以通过添加更多的 MCP 服务器实例进行水平扩展，同时保持 MCP 服务器端的状态。

- **支持集中式的认证和授权**: MCP over MQTT 可以利用 MQTT Broker 的认证和授权机制，确保只有经过授权的客户端可以访问 MCP 服务。

- **支持服务名管理和下发**: 在 MCP 的基础上，EMQX 增加了 MCP 服务名的概念，用于 MCP 服务的标识和分类管理。用户可以在 EMQX 上集中式地设计和下发 MCP 服务名，简化多 MCP 服务的管理和维护。
