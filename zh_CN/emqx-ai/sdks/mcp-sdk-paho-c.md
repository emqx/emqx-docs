# C SDK with Paho MQTT

本文将使用 [MCP over MQTT C SDK with Paho MQTT](https://github.com/mqtt-ai/paho-mcp-over-mqtt) 创建一个简单的 MCP over MQTT 服务端。目前仅支持 MCP 服务端，可以通过 Python SDK 创建 MCP 客户端进行交互。

## 创建 MCP 服务器

参照 [C SDK with Paho MQTT](https://github.com/mqtt-ai/paho-mcp-over-mqtt) README 中的说明，安装完相关依赖以及 SDK 后，创建一个名为 `demo_mcp_server.c` 的文件，并添加以下代码：

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
    "sensor",           // Server name
    "Sensor MCP Server", // Description
    "mqtt://broker.example.com", // MQTT Broker URI
    "client_001",       // Client ID
    "username",               // Username
    "password",               // Password
    NULL                      // Certificate (optional)
);

// Register tools
mcp_server_register_tool(server, 1, my_tools);

// Start server
mcp_server_run(server);
```

## 使用 Python SDK 创建 MCP 客户端并运行

参考 [Python SDK](./mcp-sdk-python.md) 中的说明，创建一个 MCP 客户端连接到上述 C SDK 创建的 MCP 服务器，并调用 `get_temperature` 工具。

## 使用 CMake 构建并编译 MCP 服务器

CMake 使用可以参考 [paho-mcp-over-mqtt](https://github.com/mqtt-ai/paho-mcp-over-mqtt) 中的示例。

构建后，运行生成的可执行文件，即可启动 MCP 服务器。

```bash
./demo_mcp_server
```
