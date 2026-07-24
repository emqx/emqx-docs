# ESP32 C SDK

本文将使用 [MCP over MQTT C SDK for ESP32](https://github.com/mqtt-ai/esp-mcp-over-mqtt) 创建一个简单的 MCP over MQTT 服务端。目前仅支持 MCP 服务端，可以通过 Python SDK 创建 MCP 客户端进行交互。

此 SDK 使用的 MQTT 库是 ESP-IDF 中自带的 MQTT 库，适用于 ESP32 设备，因此必须在 ESP-IDF 环境中使用。

## 创建 MCP 服务器

参照 [ESP32 C SDK](https://github.com/mqtt-ai/esp-mcp-over-mqtt) README 中的说明，在基于 ESP-IDF 创建的 ESP32 项目中，创建一个名为 `mcp_server_example.c` 的文件，并添加以下代码：

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
    "esp32_sensor",           // Server name
    "ESP32 Sensor MCP Server", // Description
    "mqtt://broker.example.com", // MQTT Broker URI
    "esp32_client_001",       // Client ID
    "username",               // Username
    "password",               // Password
    NULL                      // Certificate (optional)
);

// Register tools
mcp_server_register_tool(server, 1, my_tools);

// Start server
mcp_server_run(server);
```

## 基于 ESP-IDF 构建的项目中使用 MCP 服务器

详细使用方法请参考 [ESP32 MCP Demo](https://github.com/mqtt-ai/esp32-mcp-mqtt-tutorial/tree/main/samples/blog_3) 示例项目，该项目展示了如何在 ESP-IDF 项目中集成 MCP over MQTT C SDK for ESP32，并创建一个 MCP 服务器，以及如何通过 Python SDK 创建 MCP 客户端进行交互。

基于 ESP-IDF 构建好项目后，烧录到 ESP32 设备上运行，即可启动 MCP 服务器。