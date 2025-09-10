# Specification

This specification defines the MQTT-specific requirements like MQTT topics and client ID formats. It also outlines the lifecycle of the MQTT transport, including service discovery, initialization, capability list changes, resource updates, and shutdown procedures.

It should be read in conjunction with the [MCP Specification](https://modelcontextprotocol.io/specification/2025-06-18).

## Terminology

- **server-name**: The server name is the identifier of a MCP server, which will be included in the topic.

  Multiple connections with the same `server-name` are considered multiple instances of the same MCP server and provide exactly the same service. When the MCP client sends an initialize message, it should select one of them according to a client-side determined strategy.

  Multiple MCP servers with different `server-name`s may still provide similar functions. In this case, when the client sends an initialize message, it should select one of them to establish a connection as needed. The selection criteria can be based on the client's permissions, recommendations from a LLM, or the user's choice.

  After connected to the MQTT broker, the broker may suggest a `server-name` to the MCP server by including a `MCP-SERVER-NAME` user property in the MQTT CONNECT message. If so, the MCP server **MUST** use this `server-name` as its server name. If the broker does not suggest a `server-name`, the MCP server **SHOULD** use a default `server-name` based on the functionality it provides.

  The `server-name` must be a hierarchical topic style separated by `/` so that the client can subscribe to a certain type of MCP server using MQTT topic wildcards, for example: `server-type/sub-type/name`.

  The `server-name` should not `+` and `#` characters.

  The `server-name` should be unique among all MCP servers.

- **server-name-filter**: The MQTT topic filter to match the `server-name`, it may include `/`, `+` and `#` characters. See descriptions about **server-name** for more details.

  After connected to the MQTT broker, the broker may suggest a `server-name-filter` to the MCP client by including a `MCP-SERVER-NAME-FILTERS` user property in the MQTT CONNACK message. If so, the MCP client **MUST** use this `server-name-filter` to subscribe to the server's presence topic. The value of the `MCP-SERVER-NAME-FILTERS` is a JSON array of strings, each string is a MQTT topic filter.
  If the broker does not suggest a `server-name-filter`, the MCP client **SHOULD** use a default `server-name-filter` based on the functionality it provides.

- **server-id**: The MQTT Client ID of a MCP server instance. Any string except `/`, `+` and `#`. It must be globally unique and will also be included in the topic.

- **mcp-client-id**: The MQTT Client ID of the client. Any string except `/`, `+` and `#`. It must be globally unique and will be included in the topic. Each time an initialization request is made, a different client-id must be used.

## Message Topics

MCP over MQTT transmits messages through MQTT topics. This protocol includes the following message topics:

| Topic Name                       | Topic Name                                                          | Description                                                                        |
|----------------------------------|---------------------------------------------------------------------|------------------------------------------------------------------------------------|
| Server's Control Topic           | `$mcp-server/{server-id}/{server-name}`                             | Used for sending and receiving initialization messages and other control messages. |
| Server's Capability Change Topic | `$mcp-server/capability/{server-id}/{server-name}`     | Used for sending and receiving server capability list changed or the resource updated notification.        |
| Server's Presence Topic          | `$mcp-server/presence/{server-id}/{server-name}`                    | Used for sending and receiving server's online/offline status messages.            |
| Client's Presence Topic          | `$mcp-client/presence/{mcp-client-id}`                              | Used for sending and receiving client's online/offline status messages.            |
| Client's Capability Change Topic | `$mcp-client/capability/{mcp-client-id}`               | Used for sending and receiving client capability list changed notification.        |
| RPC Topic                        | `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`                   | Used for sending and receiving RPC requests/responses, and notification messages.  |

## MQTT Protocol Version

The MCP server and client **MUST** use MQTT Protocol version 5.0.

## User Property

For `CONNECT` messages, the following user properties **MUST** be set:
- `MCP-COMPONENT-TYPE`: `mcp-client` or `mcp-server`.
- `MCP-META`: A JSON object containing metadata about the MCP component, such as its version, implementation details, and location. This metadata can be used by the broker to suggest a server name to the MCP server or a server name filter to the MCP client.

For `CONNACK` messages sent by the broker, the following user properties **MAY** be set:
- `MCP-SERVER-NAME`: The broker suggested server name for the MCP server. Only present if it's a MCP server.
- `MCP-RBAC`: A JSON array of server names and its corresponding role names, which can be used by the MCP client to determine the roles it has for the MCP server. Each element in the array is a JSON object with two fields: `server_name` and `role_name`. Only present if it's a MCP client.
- `MCP-SERVER-NAME-FILTERS`: The broker suggested server name filters. It's a JSON array of strings, each string is a MQTT topic filter that the MCP client can use to subscribe to the server's presence topic. This allows the client to filter the servers it is interested in based on its permissions or other criteria. Only present if it's a MCP client.

For `PUBLISH` messages, the following user properties **MUST** be set:
- `MCP-COMPONENT-TYPE`: `mcp-client` or `mcp-server`.
- `MCP-MQTT-CLIENT-ID`: MQTT client ID of the sender.

## Session Expiry Interval

The session expiry interval **MUST** be set to 0, meaning the session will be cleaned up when the client disconnects.

## MQTT Client ID

### MCP Server

The Client ID of the MCP server can be any string except `/`, `+` and `#`, referred to as `server-id`.

### MCP Client

The Client ID of the MCP client, referred to as `mcp-client-id`, can be any string except `/`, `+` and `#`, each time an initialization request is made, a different client-id must be used.

## MQTT Topics and Topic Filters

### MCP Server Subscriptions

| Topic Filter                                          | Explanation                                                                                              |
|-------------------------------------------------------|----------------------------------------------------------------------------------------------------------|
| `$mcp-server/{server-id}/{server-name}`                         | The control topic of the MCP server to receive control messages.                                         |
| `$mcp-client/capability/{mcp-client-id}` | The MCP client’s capability change topic to receive capability list changed notification of the clients. |
| `$mcp-client/presence/{mcp-client-id}`                | The MCP client’s presence topic to receive the disconnected notification of the clients.                 |
| `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`    | The RPC topic to receive RPC requests, RPC responses, and notifications from a MCP client.               |

::: info
- The server **MUST** set the **No Local** option for the RPC topic (`$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`) subscription to avoid receiving its own messages.
:::

### MCP Server Publications

| Topic Name                                                             | Messages                                                                                               |
|------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------|
| `$mcp-server/capability/{server-id}/{server-name}`     | capability list changed or resource updated notification.                                                                  |
| `$mcp-server/presence/{server-id}/{server-name}`                    | Presence messages for the MCP server. <br> See [ServiceDiscovery](#service-discovery) for more details |
| `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`                     | RPC requests, responses and notifications.                                                             |

::: info
- The server **MUST** set the **RETAIN** flag to `True` for the topic `$mcp-server/presence/{server-id}/{server-name}` when publishing the server presence message.
- When connecting to the MQTT broker, the server **MUST** set `$mcp-server/presence/{server-id}/{server-name}` as the will topic with an empty payload to clear the retain message in case of an unexpected disconnection.
:::


### MCP Client Subscriptions

| Topic Filter                                                       | Explanation                                                                                    |
|--------------------------------------------------------------------|------------------------------------------------------------------------------------------------|
| `$mcp-server/capability/{server-id}/{server-name-filter}`     | The capability change topic to receive capability list changed or resource updated notification of the MCP server. |
| `$mcp-server/presence/+/{server-name-filter}`                    | The presence topic to receive the presence message of the MCP server.                          |
| `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name-filter}`          | The RPC topic to receive PRC requests, responses and notifications sent by the MCP server.     |

::: info
- The client **MUST** set the **No Local** option for the RPC topic (`$mcp-rpc/{mcp-client-id}/{server-id}/{server-name-filter}`) subscription to avoid receiving its own messages.
:::

### MCP Client Publications

| Topic Name                                           | Messages                                                           |
|------------------------------------------------------|--------------------------------------------------------------------|
| `$mcp-server/{server-id}/{server-name}`              | Send control messages like the initialize request.                 |
| `$mcp-client/capability/{mcp-client-id}`             | Send client capability list changed notification                   |
| `$mcp-client/presence/{mcp-client-id}`               | Send disconnected notification for the MCP client.                 |
| `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}` | The RPC topic to send RPC requests/responses to a specific server. |

::: info
- When connecting to the MQTT broker, the client **MUST** set `$mcp-client/presence/{mcp-client-id}` as the will topic with a "disconnected" notification as the payload to notify the server in case of an unexpected disconnection.
:::

## Service Discovery

### Service Registration

After the MCP server starts, it registers its service with the MQTT broker. The presence topic for service discovery and registration is: `$mcp-server/presence/{server-id}/{server-name}`.

The MCP server **MUST** publish a "server/online" notification to the service presence topic when they start, with the **RETAIN** flag set to `True`.

The "server/online" notification **SHOULD** provide only limited information about the server to avoid the message size being too large. The client can request more detailed information after initialization.

- A brief description of the MCP server's functionality to help clients determine which MCP servers they need to initialize.
- Some metadata, such as roles and permissions, to help clients understand the access control policies of the MCP server. The `rbac` field in the metadata can include roles, each with a name, description, allowed methods, allowed tools, and allowed resources, which maybe used by the MQTT broker to implement role-based access control (RBAC) for the MCP server.

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/server/online",
  "params": {
      "server_name": "example/server",
      "description": "This is a brief description about the functionalities provided by this MCP server to allow clients to choose as needed. If tools are provided, it explains what tools are available but does not include tool parameters to reduce message size.",
      "meta": {
        "rbac": {
          "roles": [
            {
              "name": "admin",
              "description": "Administrator role with full access",
              "allowed_methods": [
                "notifications/initialized",
                "ping", "tools/list", "tools/call", "resources/list", "resources/read",
                "resources/subscribe", "resources/unsubscribe"
              ],
              "allowed_tools": "all",
              "allowed_resources": "all"
            },
            {
              "name": "user",
              "description": "User role with limited access",
              "allowed_methods": [
                "notifications/initialized",
                "ping", "tools/list", "tools/call", "resources/list", "resources/read"
              ],
              "allowed_tools": [
                "get_vehicle_status", "get_vehicle_location"
              ],
              "allowed_resources": [
                "file:///vehicle/telemetry.data"
              ]
            }
          ]
        }
      }
  }
}
```

More detailed information, such as parameter details of the tools, **SHOULD** only be fetched by the client when needed, by sending `**/list` requests to the server.

The client can subscribe to the `$mcp-server/presence/+/{server-name-filter}` topic at any time, where `{server-name-filter}` is a filter for the server name.

For example, if the server name is `{server-type}/{sub-type}/{name}`, and the client determines through its permissions that it can only access MCP servers of type `{server-type}/{sub-type}`, it can subscribe to `$mcp-server/presence/+/{server-type}/{sub-type}/#`, thereby subscribing to the service presence topic for all MCP servers of the `{sub-type}` type at once.

Although the client can subscribe to `$mcp-server/presence/+/#` to get all types of MCP servers, the administrator might restrict it through ACL (Access Control List) on the MQTT broker to only send and receive messages on RPC topics like `$mcp-rpc/{mcp-client-id}/{server-id}/{server-type}/{sub-type}/#`. Therefore, subscribing to overly broad topics is not useful. By designing the `{server-name-filter}` appropriately, the client can reduce interference from irrelevant information.

### Service Unregistration

When connecting to the MQTT broker, the server must set `$mcp-server/presence/{server-id}/{server-name}` as the will topic, with an empty payload will message, to clear the registration information in case of an unexpected disconnection.

Before actively disconnecting from the MQTT broker, the server **MUST** send an empty payload message to the `$mcp-server/presence/{server-id}/{server-name}` topic to clear the registration information.

On the `$mcp-server/presence/{server-id}/{server-name}` topic:

- When the client receives a `server/online` notification, it should record the `{server-id}` as one of the instances of that `{server-name}`.
- When the client receives an empty payload message, it should clear the cached `{server-id}`. As long as any instance of that `{server-name}` is online, the client should consider the MCP server to be online.

The message flow for service registration and unregistration is as follows:

```mermaid
sequenceDiagram
    participant MCP_Server as MCP Server
    participant MQTT_Broker as MQTT Broker
    participant MCP_Client as MCP Client

    MCP_Server ->> MQTT_Broker: Register Service<br/>Topic: $mcp-server/presence/{server-id}/{server-name}<br/>Retain: True
    Note right of MQTT_Broker: Store Retained Messages

    MCP_Client ->> MQTT_Broker: Subscribe Services<br/>Topic Filter: $mcp-server/presence/+/ {server-name-filter}

    MQTT_Broker ->> MCP_Client: Description of Service<br/>Topic: $mcp-server/presence/{server-id}/{server-name}<br/>Payload: "notifications/server/online"
    Note left of MCP_Client: Record the server-id for a server-name.

    MCP_Server ->> MQTT_Broker: Unregister Service<br/>Topic: $mcp-server/presence/{server-id}/{server-name}<br/>Retain: True<br/>Payload: Empty
    Note right of MQTT_Broker: Clean Retained Messages

    MQTT_Broker ->> MCP_Client: Description of Service<br/>Topic: $mcp-server/presence/{server-id}/{server-name}<br/>Payload: Empty
    Note left of MCP_Client: Remove the server-id
```

## Initialization

This section only describes the MQTT transport specific parts of the initialization phase, please see [Lifecycle](https://modelcontextprotocol.io/specification/2025-06-18/basic/lifecycle#initialization) for more details.

The initialization phase **MUST** be the first interaction between client and server.

The client **MUST** subscribe to the RPC topic (`$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`) before sending the initialization request, with the **No Local** subscription option.

The server **MUST** subscribe to the RPC topic (`$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`) before responding to the initialization response, with the **No Local** subscription option.

```mermaid
sequenceDiagram
  participant MCP_Client as MCP Client
  participant MCP_Server as MCP Server

  Note right of MCP_Client: Subscribe the<br/>server's RPC topic
  MCP_Client ->> MCP_Server: Initialize Request<br/>Topic: $mcp-server/{server-id}/{server-name}
  Note left of MCP_Server: Subscribe the<br/>client's RPC topic
  MCP_Server ->> MCP_Client: Initialize Response<br/>Topic: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}
  MCP_Client ->> MCP_Server: Initialized<br/>Topic: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}
  MCP_Client ->> MCP_Server: RPC Req/Resp/Notification<br/>Topic: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}
  MCP_Server ->> MCP_Client: RPC Req/Resp/Notification<br/>Topic: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}
```

The client **MUST** initiate this phase by sending an `initialize` request to the topic `$mcp-server/{server-id}/{server-name}` containing:

- Protocol version supported
- Client capabilities
- Client implementation information

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "roots": {
        "listChanged": true
      },
      "sampling": {}
    },
    "clientInfo": {
      "name": "ExampleClient",
      "version": "1.0.0"
    }
  }
}
```

The server **MUST** respond with its own capabilities to the topic `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}` and information:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "logging": {},
      "prompts": {
        "listChanged": true
      },
      "resources": {
        "subscribe": true,
        "listChanged": true
      },
      "tools": {
        "listChanged": true
      }
    },
    "serverInfo": {
      "name": "ExampleServer",
      "version": "1.0.0"
    }
  }
}
```

After successful initialization, the client **MUST** send an initialized notification to indicate it is ready to begin normal operations, to the topic `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`:

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized"
}
```

## Capability List Changed

Before initiating the Initialize request, the MCP client **MUST** subscribe to the MCP server's capability list changed topic: `$mcp-server/capability/{server-id}/{server-name-filter}`, where `{server-name-filter}` is a filter for the server name.

Before the MCP server responds to the initialization request, it **MUST** first subscribe to the MCP client's capability list changed topic: `$mcp-client/capability/{mcp-client-id}`.

If there are subsequent capability list updates:

- The server will send a notification to: `$mcp-server/capability/{server-id}/{server-name}`
- The client will send a notification to: `$mcp-client/capability/{mcp-client-id}`

The payload of the capability list changed notification depends on the specific capability that has changed. For example "notifications/tools/list_changed" for tools. After receiving a capability list change notification, the client or server needs to retrieve the updated capability list. See the specific capability documentation for details.

```mermaid

sequenceDiagram
    participant MCP_Client as MCP Client
    participant MCP_Server as MCP Server

    Note right of MCP_Client: Client subscribes the server's<br/>capability change topic
    MCP_Client ->> MCP_Server: Initialize

    Note left of MCP_Server: Server subscribes the client's<br/>capability change topic
    MCP_Server ->> MCP_Client: Initialize Response
    MCP_Client ->> MCP_Server: Initialized

    MCP_Server -->> MCP_Client: Capability List Changed<br/>Topic: $mcp-server/capability/{server-id}/{server-name}

    MCP_Client ->> MCP_Server: List Capability<br/>Topic: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}

    MCP_Server -->> MCP_Client: List Capability Response<br/>$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}
```

## Resource Update

The MCP protocol specifies that the client can subscribe to changes of a specific resource.

If the server provides the capability to subscribe to resources, the client can subscribe to the resource changes before sending the initialized notification.

The topic for the client to subscribe to resource changes is: `$mcp-server/capability/{server-id}/{server-name}`.

When a resource changes, the server **SHOULD** send a notification to `$mcp-server/capability/{server-id}/{server-name}`.

```mermaid

sequenceDiagram
    participant MCP_Client as MCP Client
    participant MCP_Server as MCP Server

    MCP_Client ->> MCP_Server: Initialize
    MCP_Server ->> MCP_Client: Initialize Response
    Note right of MCP_Client: Client subscribes the server's<br/>resource update topic
    MCP_Client ->> MCP_Server: Initialized

    MCP_Client ->> MCP_Server: List Resources<br/>Topic: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}

    MCP_Server -->> MCP_Client: List Resources Response<br/>Topic: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}<br/>URIs: [{resource-uri}, {resource-uri}, ...]

    MCP_Server -->> MCP_Client: Resource Updated<br/>Topic: $mcp-server/capability/{server-id}/{server-name}<br/>URI: {resource-uri}

    MCP_Client ->> MCP_Server: Read Resource<br/>Topic: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}<br/>URI: {resource-uri}

    MCP_Server -->> MCP_Client: Read Resource Response<br/>Topic: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}<br/>URI: {resource-uri}
```

## Shutdown

### Server Disconnect

The server **MUST** connect with a will message to notify the client when it disconnects unexpectedly, the will topic is `$mcp-server/presence/{server-id}/{server-name}` and the payload is empty.

Before a MCP server disconnects from the MQTT broker, the server **MUST** send an empty message to the presence topic `$mcp-server/presence/{server-id}/{server-name}`.

The MCP server may want to 'de-initialize' with a MCP client but still keep the connection with the MQTT broker, in this case it **MUST** send a "disconnected" notification to the rpc topic `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}` and then unsubscribe from the following topics:
- `$mcp-client/capability/{mcp-client-id}`
- `$mcp-client/presence/{mcp-client-id}`
- `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`

The message format for the MCP server's "disconnected" notification is:

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/disconnected"
}
```

When a MCP client receives an empty payload message on the server's presence topic or a "disconnected" notification on the rpc topic, it **MUST** consider the server to be offline and clear the cached `{server-id}` for that `{server-name}` and unsubscribe from the following topics:
- `$mcp-server/capability/{server-id}/{server-name-filter}`
- `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name-filter}`

### Client Disconnect

The server **MUST** subscribe to the client's presence topic (`$mcp-client/presence/{mcp-client-id}`) before sending the initialization response.

The client **MUST** connect with a will message to notify the server when it disconnects unexpectedly, the will topic is `$mcp-client/presence/{mcp-client-id}` and the payload is a "disconnected" notification.

Before the client disconnects from the MQTT broker, it **MUST** send a "disconnected" notification to the topic `$mcp-client/presence/{mcp-client-id}`.

The client may want to 'de-initialize' with a MCP server but still keep the connection with the MQTT broker, in this case it **MUST** send a "disconnected" notification to the rpc topic `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}` and then unsubscribe from the following topics:
- `$mcp-server/capability/{server-id}/{server-name-filter}`
- `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name-filter}`

After the MCP server receives the "disconnected" notification, it **MUST** unsubscribe from the following topics:
- `$mcp-client/capability/{mcp-client-id}`
- `$mcp-client/presence/{mcp-client-id}`
- `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`

The message format for the MCP client's "disconnected" notification is:

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/disconnected"
}
```

## Health Checks

The client or the server **MAY** send `ping` requests to the server at any time to check the health of their counterpart.

- If the client does not receive a `ping` response from the server within a reasonable time, it **MUST** send a "disconnected" notification to the topic `$mcp-client/presence/{mcp-client-id}` and disconnect itself.
- If the server does not receive a `ping` response from the client within a reasonable time, it **MUST** send any other PRC requests to the client.

For more information, see [Ping](https://modelcontextprotocol.io/specification/2025-06-18/basic/utilities/ping).

## Timeouts

All RPC requests are sent asynchronously via MQTT messages, so timeout issues need to be considered. The timeout duration may vary for different RPC requests, but it should be configurable.

Below are the recommended default timeout values for each type of RPC request in this protocol:

- "initialize": 30 seconds
- "ping": 10 seconds
- "roots/list": 30 seconds
- "resources/list": 30 seconds
- "tools/list": 30 seconds
- "prompts/list": 30 seconds
- "prompts/get": 30 seconds
- "sampling/createMessage": 60 seconds
- "resources/read": 30 seconds
- "resources/templates/list": 30 seconds
- "resources/subscribe": 30 seconds
- "tools/call": 60 seconds
- "completion/complete": 60 seconds
- "logging/setLevel": 30 seconds

<!-- {< callout type="info" >}
Progress requests are sent as notifications and do not require a response, so no timeout is needed.
{< /callout >} -->

## Error Handling

Implementations **SHOULD** be prepared to handle these error cases:

- Protocol version mismatch
- Failure to negotiate required capabilities
- Initialize request timeout
- Shutdown timeout

Implementations **SHOULD** implement appropriate timeouts for all requests, to prevent
hung connections and resource exhaustion.

Example initialization error:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32602,
    "message": "Unsupported protocol version",
    "data": {
      "supported": ["2025-03-26"],
      "requested": "1.0.0"
    }
  }
}
```