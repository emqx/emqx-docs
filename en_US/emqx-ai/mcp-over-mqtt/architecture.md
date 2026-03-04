# MCP over MQTT Architecture

MCP over MQTT inherits the core concepts of the standard MCP architecture (Host, Client, Server), while introducing a centralized MQTT Broker as the transport layer. The broker enables message routing, service registration and discovery, authentication, and authorization.

This architecture not only preserves MCP’s original context interaction model but also leverages MQTT’s lightweight and broadly applicable design, providing the foundation for many-to-many communication, load balancing, and scalability in IoT and edge computing scenarios.

## Core Components of the MQTT Transport

In the MCP over MQTT architecture, a centralized MQTT Broker is introduced as the message router, while other components (Host, Client, Server) remain consistent with the standard MCP design.

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

### Host, Client, and Server

The Host, Client, and Server components remain unchanged (see [MCP core concepts](https://modelcontextprotocol.io/docs/learn/architecture#concepts-of-mcp)):

- **Host** acts as a container and coordinator for clients.
- Each **Client** is created by the Host and maintains an independent connection with a Server.
- **Server** provides dedicated context and capabilities.

The key difference is that Clients and Servers now communicate through the MQTT Broker, instead of directly. With the broker in place, the relationship between Clients and Servers becomes many-to-many rather than one-to-one.

### Role of the MQTT Broker

The MQTT Broker serves as the centralized message router:

- Forwards messages between Clients and Servers.
- Supports service registration and discovery (via retained messages).
- Handles authentication and authorization for Clients and Servers.

## Server Scaling and Load Balancing

To achieve scalability and load balancing, an MCP Server can launch multiple instances (processes). Each instance connects to the broker with a unique `server-id` as its MQTT Client ID, while all instances share the same `server-name`.

**Client interaction flow:**

1. The Client subscribes to the service discovery topic to obtain all available `server-id`s under the target `server-name`.
2. The Client selects a Server instance based on a custom policy (e.g., random or round-robin) and sends an `initialize` request.
3. After initialization, the Client communicates with the selected Server instance through a dedicated RPC topic.

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

This approach enables high availability and scalability of MCP servers:

- **During scaling up**, existing MCP clients remain connected to old server instances, while new clients can initialize with newly added instances.
- **During scaling down**, MCP clients can reinitialize and connect to other available server instances.