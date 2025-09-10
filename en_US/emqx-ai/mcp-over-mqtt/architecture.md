# Architecture

## Core Components of the MQTT Transport

MCP over MQTT introduces a centralized MQTT broker, while other components (Hosts, Clients, Servers) remain unchanged.

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

The Host, Client, and Server components remain unchanged:

- The host process acts as the container and coordinator of the clients.
- Each client is created by the host and maintains an isolated server connection.
- Servers provide specialized context and capabilities.

With the exception that the clients and servers communicate with the MQTT broker instead of directly with each other.

See [Core Components](https://modelcontextprotocol.io/docs/learn/architecture#concepts-of-mcp) for more details.

### MQTT Broker

The MQTT broker acts as a centralized message router:
- Facilitates communication between clients and servers.
- Support service discovery and service registration (via retained messages).
- Authenticates and authorizes clients and servers.

## Server Side Load Balancing and Scalability

To achieve MCP server-side load balancing and scalability, an MCP server can start multiple instances (processes), each using a unique `server-id` as the MQTT Client ID to establish an independent MQTT connection. All instances of an MCP server share the same `server-name`.

The client must first subscribe to the service discovery topic to obtain the list of `server-id`s for a specific `server-name`. Then, based on a client-defined server selection strategy (e.g., random selection or round-robin), it initiates an `initialize` request to one of the `server-id`s. After initialization is complete, the MCP client communicates with the selected MCP server instance on a specific RPC topic.

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

This allows us to achieve high availability and scalability on the MCP server side:

- When scaling up, existing MCP clients remain connected to the old server instances, while new MCP clients have the opportunity to initiate initialization requests to the new server instances.

- When scaling down, MCP clients could re-initiate initialization requests to the MCP server, thereby connecting to another server instance.