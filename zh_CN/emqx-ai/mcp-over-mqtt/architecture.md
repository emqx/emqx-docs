# 架构

## MQTT 传输的核心组件

MCP over MQTT 引入了中心化的 MQTT Broker，其他组件（Host、Client、Server）保持不变。

```mermaid
graph LR
    subgraph "Application Host Process"
        H[Host]
        C1[Client 1]
        C2[Client 2]
        C3[Client 3]
        H --> C1
        H --> C2
        H --> C3
    end

    subgraph "MQTT Broker"
        B[Broker]
        C1 --> B
        C2 --> B
        C3 --> B
    end

    subgraph "Servers"
        S1[Server A<br>External APIs]
        R1[("Remote<br>Resource A")]
        B --> S1
        S1 <--> R1
    end

    subgraph "Servers"
        S2[Server B<br>External APIs]
        R2[("Remote<br>Resource B")]
        B --> S2
        S2 <--> R2
    end
```

### Host、Client 和 Server

Host、Client 和 Server 组件保持不变：

- Host 进程作为客户端的容器和协调者。
- 每个 Client 由 Host 创建，并维护与 Server 的独立连接。
- Server 提供专用的上下文和能力。

最大的区别是，Client 和 Server 现在通过 MQTT Broker 进行通信，而不是直接相互通信。另外，由于 MQTT Broker 的引入，Client 和 Server 变为多对多的关系，而不是一对一。

详见 [核心组件](https://modelcontextprotocol.io/docs/learn/architecture#concepts-of-mcp)。

### MQTT Broker

MQTT Broker 作为中心化的消息路由器：
- 促进 Client 与 Server 之间的通信。
- 支持服务发现和服务注册（通过保留消息）。
- 对 Client 和 Server 进行认证和授权。

## 服务端负载均衡与可扩展性

为实现 MCP 服务端负载均衡与可扩展性，MCP Server 可以启动多个实例（进程），每个实例使用唯一的 `server-id` 作为 MQTT Client ID 建立独立的 MQTT 连接。所有 MCP Server 实例共享同一个 `server-name`。

Client 首先需要订阅服务发现主题，以获取指定 `server-name` 下的所有 `server-id` 列表。然后，Client 根据自定义的 Server 选择策略（如随机选择或轮询），向其中一个 `server-id` 发起 `initialize` 请求。初始化完成后，MCP Client 会在特定的 RPC 主题上与选定的 MCP Server 实例进行通信。

```mermaid
graph LR

    C1["MCP Client1"]
    C2["MCP Client2"]
    C3["MCP Client3"]
    C4["MCP Client4"]

    subgraph "MCP Server Instances (server-name-a)"
        S1[Server Instance 1]
        S2[Server Instance 2]
    end

    C1 <-- "RPC topic of client-1 and server instance 1" --> S1
    C2 <-- "RPC topic of client-2 and server instance 1" --> S1
    C3 <-- "RPC topic of client-3 and server instance 2" --> S2
    C4 <-- "RPC topic of client-4 and server instance 2" --> S2

```

这使我们能够实现 MCP 服务端的高可用性和可扩展性：

- 扩容时，已有的 MCP 客户端会继续连接到旧的服务端实例，而新的 MCP 客户端则有机会向新的服务端实例发起初始化请求。

- 缩容时，MCP 客户端可以重新向 MCP 服务端发起初始化请求，从而连接到其他服务端实例。