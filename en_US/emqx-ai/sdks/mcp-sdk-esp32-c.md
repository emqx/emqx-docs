# ESP32 C SDK

This guide demonstrates how to use the [MCP over MQTT C SDK for ESP32](https://github.com/mqtt-ai/esp-mcp-over-mqtt) to create a simple MCP over MQTT server.
 Currently, only the MCP server is supported. You can create an MCP client using the Python SDK for interaction.

The SDK uses the MQTT library included in ESP-IDF, making it suitable for ESP32 devices. Therefore, it must be used within the ESP-IDF environment.

## Create an MCP Server

Following the instructions in the [ESP32 C SDK README](https://github.com/mqtt-ai/esp-mcp-over-mqtt), create a new file named `mcp_server_example.c` in your ESP-IDF project and add the following code:

```c
#include "mcp_server.h"

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
    "esp32_sensor",             // Server name
    "ESP32 Sensor MCP Server",  // Description
    "mqtt://broker.example.com",// MQTT Broker URI
    "esp32_client_001",         // Client ID
    "username",                 // Username
    "password",                 // Password
    NULL                        // Certificate (optional)
);

// Register tools
mcp_server_register_tool(server, 1, my_tools);

// Start server
mcp_server_run(server);
```

## Use MCP Server in an ESP-IDF Project

For detailed usage, see the [ESP32 MCP Demo](https://github.com/mqtt-ai/esp32-mcp-mqtt-tutorial/tree/main/samples/blog_3) project.
 This example demonstrates how to integrate the MCP over MQTT C SDK for ESP32 into an ESP-IDF project, set up an MCP server, and interact with it using an MCP client implemented in the Python SDK.

After building the project with ESP-IDF and flashing it onto the ESP32 device, the MCP server will start automatically.