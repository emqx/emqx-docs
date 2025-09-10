# MCP over MQTT

MCP over MQTT is an implementation of [MCP](https://modelcontextprotocol.io/docs/getting-started/intro) based on the MQTT protocol, designed to provide efficient, low-latency tool invocation and messaging capabilities for AI applications. With MCP over MQTT, developers can easily integrate AI models and services into IoT systems, enabling seamless interaction between devices and AI.

## Why Use MQTT for MCP

MQTT is a lightweight and widely used protocol for IoT and edge computing. It is designed to handle unreliable networks and low bandwidth, making it ideal for communication between edge devices and cloud services.

By using MQTT as the transport layer for MCP, we extend the application scope of MCP to a wider range of scenarios, including edge computing, IoT, and cloud services—anywhere MQTT is needed.

## Features

MCP over MQTT supports all MCP features and adds the following:

- **Built-in service registration and discovery**: MCP clients can discover available MCP servers from the MQTT broker.

- **Built-in load balancing and scalability**: MCP servers can be horizontally scaled by adding more MCP server instances, while maintaining server-side state.

- **Support for centralized authentication and authorization**: MCP over MQTT can leverage the authentication and authorization mechanisms of the MQTT broker, ensuring that only authorized clients can access MCP services.

- **Support for service name management and distribution**: On top of MCP, EMQX introduces the concept of MCP service names for identification and classification. Users can centrally design and distribute MCP service names on EMQX, simplifying the management and maintenance of multiple MCP services.
