# C SDK with Paho MQTT

This guide demonstrates how to use the [MCP over MQTT C SDK with Paho MQTT](https://github.com/mqtt-ai/paho-mcp-over-mqtt) to create a simple **MCP over MQTT server**.
 Currently, only the MCP server is supported. You can use the Python SDK to create an MCP client for interaction.

## Create an MCP Server

Following the instructions in the [C SDK with Paho MQTT README](https://github.com/mqtt-ai/paho-mcp-over-mqtt), after installing the dependencies and SDK, create a file named `demo_mcp_server.c` and add the following code:

```c
#include "mcp_server.h"

// Callback function to get temperature
const char* get_temperature_callback(int n_args, property_t *args) {
    // Read sensor data
    float temp = read_temperature_sensor();

    // Return JSON formatted result
    static char result[64];
    snprintf(result, sizeof(result), "{\"temperature\": %.2f}", temp);
    return result;
}

// Define MCP tools
mcp_tool_t my_tools[] = {
    {
        .name = "get_temperature",
        .description = "Get device temperature",
        .property_count = 0,
        .properties = NULL,
        .call = get_temperature_callback
    }
};

// Initialize MCP server
mcp_server_t *server = mcp_server_init(
    "sensor",                // Server name
    "Sensor MCP Server",     // Description
    "mqtt://broker.example.com", // MQTT Broker URI
    "client_001",            // Client ID
    "username",              // Username
    "password",              // Password
    NULL                     // Certificate (optional)
);

// Register tools
mcp_server_register_tool(server, 1, my_tools);

// Start server
mcp_server_run(server);
```

## Create and Run an MCP Client with Python SDK

Refer to the [Python SDK](./mcp-sdk-python.md) documentation to create an MCP client that connects to the MCP server created above and invokes the `get_temperature` tool.

## Build and Compile the MCP Server with CMake

You can follow the **CMake examples** provided in the [paho-mcp-over-mqtt repository](https://github.com/mqtt-ai/paho-mcp-over-mqtt).

After building, run the generated executable to start the MCP server:

```bash
./demo_mcp_server
```