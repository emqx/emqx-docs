# MCP over MQTT

MCP over MQTT is an implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/docs/getting-started/intro) that runs on top of the MQTT protocol.

By leveraging MQTT’s lightweight, efficient, and widely adopted characteristics, it extends MCP’s context and tool invocation capabilities to IoT and edge computing scenarios, enabling low-latency, seamless interaction between devices and AI services.

## Why Use MQTT for MCP

MQTT is a lightweight, publish/subscribe messaging protocol that is widely used in IoT and edge computing. It is specifically designed to handle unreliable networks and low-bandwidth environments, making it an ideal choice for communication between edge devices and cloud services.

By using MQTT as the transport layer for MCP, we extend MCP’s applicability to a broader range of scenarios, including edge computing, IoT, and cloud services, wherever MQTT is already in use.

## Key Features

In addition to preserving all the standard MCP capabilities, MCP over MQTT enhances and extends them with the following features:

- **Built-in Service Registration and Discovery**
  MCP clients can automatically discover available MCP servers via the MQTT broker, simplifying service integration.
- **Load Balancing and Horizontal Scalability**
  MCP servers can be scaled horizontally by deploying multiple instances while maintaining state consistency, improving both availability and throughput.
- **Centralized Authentication and Authorization**
  By leveraging the MQTT broker’s authentication and access control mechanisms, MCP over MQTT ensures that only authorized clients can access designated MCP services.
- **Service Name Management and Distribution**
  On top of the standard MCP protocol, EMQX introduces the concept of **MCP service names** for service identification and classification. Administrators can centrally manage and distribute service names in EMQX, simplifying unified management and maintenance in multi-service environments.
