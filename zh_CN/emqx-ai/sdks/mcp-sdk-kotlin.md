# Kotlin SDK 

本指南将使用支持 MCP over MQTT 的 [Kotlin SDK](https://github.com/terry-xiaoyu/kotlin-sdk)，创建一个简单的 MCP 服务器。

## 环境准备

### 安装 Kotlin 编译环境

确保你已经安装了 JDK 21+，Kotlin 2.2+，以及 Gradle 9.2+。使用 [SDKMAN](https://sdkman.io/) 是安装和管理这些工具的推荐方式：

```bash
curl -s "https://get.sdkman.io" | bash
source "$HOME/.sdkman/bin/sdkman-init.sh"

sdk install java 21.0.9-tem
sdk install kotlin 2.2.21
sdk install gradle 9.2.1
```

### 安装和运行 EMQX

请参考 [快速开始](../../getting-started/getting-started.md) 文档，安装并运行 EMQX 服务器。

## 下载并运行 MCP Server 示例源码

克隆示例程序的源码：

```bash
git clone https://github.com/terry-xiaoyu/kotlin-mcp-server-demo.git
cd kotlin-mcp-server-demo
```

该示例程序中注册了两个 [MCP 工具](https://github.com/terry-xiaoyu/kotlin-mcp-server-demo/blob/e83d5166c5eefb3a45758623e3ee69f92cecb911/src/main/kotlin/io/modelcontextprotocol/sample/server/server.kt#L93)：一个计算器工具，一个电灯控制工具。

- 计算器工具：提供基本的加减乘除运算。
- 电灯控制工具：可以控制电灯的开关状态、亮度等。

```kotlin
// Add a calculator tool
server.addTool(
    name = "calculator",
    description = "This tool can perform basic math operations: addition, subtraction, multiplication, and division.",
    inputSchema = ToolSchema(
        properties = buildJsonObject {
            putJsonObject("num1") {
                put("type", JsonPrimitive("number"))
                put("description", JsonPrimitive("The first number"))
            }
            putJsonObject("num2") {
                put("type", JsonPrimitive("number"))
                put("description", JsonPrimitive("The second number"))
            }
            putJsonObject("op") {
                put("type", JsonPrimitive("string"))
                put("description", JsonPrimitive("The operation to perform: +, -, *, /"))
            }
        },
        required = listOf("num1", "num2", "op"),
    ),
) { request : CallToolRequest ->
    val num1 = request.params.arguments?.get("num1")?.jsonPrimitive?.content?.toDoubleOrNull()
        ?: throw IllegalArgumentException("Invalid or missing argument: num1")
    val num2 = request.params.arguments?.get("num2")?.jsonPrimitive?.content?.toDoubleOrNull()
        ?: throw IllegalArgumentException("Invalid or missing argument: num2")
    val op = request.params.arguments?.get("op")?.jsonPrimitive?.content?.firstOrNull()
        ?: throw IllegalArgumentException("Invalid or missing argument: op")
    val x = num1.toDouble()
    val y = num2.toDouble()
    val result = when(op) {
        '+' -> x + y
        '-' -> x - y
        '*' -> x * y
        '/' -> if (y == 0.0) throw ArithmeticException("Division by zero") else x / y
        else -> throw IllegalArgumentException("Don't support Operator: $op")
    }
    CallToolResult(
        content = listOf(TextContent("The result is: $result")),
    )
}

// Add a light control tool
server.addTool(
    name = "set_light_brightness",
    description = "Control the light on the panel. You can change the brightness, if you want to off the light, set brightness to 0. Set brightness value to 'last_value' to restore the previous brightness, which is useful when the light is off and you want to turn it back on.",
    inputSchema = ToolSchema(
        properties = buildJsonObject {
            putJsonObject("value") {
                put("type", JsonArray(listOf(JsonPrimitive("number"), JsonPrimitive("string"))))
                put("description", JsonPrimitive("Brightness value between 0 and 100, or 'last_value' to restore previous brightness"))
            }
        },
        required = listOf("value")
    ),
) { request ->
    val value = request.params.arguments?.get("value")?.jsonPrimitive?.content
    // Handle the request and control the light brightness
    CallToolResult(
        content = listOf(TextContent("Light brightness set to: ${value}%")),
    )
}
```

最后使用下面的命令，启动 MCP 服务器示例程序：

```bash
./gradlew run --args="--mqtt kt001 demo/kotlin-mcp-server"
```

其中参数说明如下：
- MQTT Client ID = `kt001`
- MCP Server Name = `demo/kotlin-mcp-server`

## 使用 MCP 客户端测试

目前 Kotlin SDK 还没有提供 MCP 客户端的实现，可以使用 Python SDK 创建一个简单的 MCP 客户端进行测试。

```python
uv init light_controller
cd light_controller
uv add git+https://github.com/emqx/mcp-python-sdk --branch main
uv add "mcp[cli]"
source .venv/bin/activate
```

添加一个 `light_controller.py` 文件，内容如下：

```python
# light_controller.py
import logging
import anyio
import mcp.client.mqtt as mcp_mqtt
from mcp.shared.mqtt import configure_logging

configure_logging(level="INFO")
logger = logging.getLogger(__name__)

async def on_mcp_server_discovered(client, server_name):
    logger.info(f"Discovered {server_name}, connecting ...")
    await client.initialize_mcp_server(server_name)

async def on_mcp_connect(client, server_name, connect_result):
    success, init_result = connect_result
    if success == 'error':
        logger.error(f"Failed to connect to {server_name}: {init_result}")
        return
    logger.info(f"Connected to {server_name}, success={success}, init_result={init_result}")
    capabilities = init_result.capabilities
    if capabilities.tools:
        toolsResult = await client.list_tools(server_name)
        tools = toolsResult.tools
        logger.info(f"Tools of {server_name}: {tools}")
        for tool in tools:
            logger.info(f" - {tool.name}: {tool.description}")
            if tool.name == "set_light_brightness":
                result = await client.call_tool(server_name, name = tool.name, arguments={"value": 50})
                logger.info(f"Calling the tool as set_light_brightness(value=50), result: {result}")

async def main():
    async with mcp_mqtt.MqttTransportClient(
        "test_client",
        server_name_filter = "demo/kotlin-mcp-server",
        auto_connect_to_mcp_server = True,
        on_mcp_server_discovered = on_mcp_server_discovered,
        on_mcp_connect = on_mcp_connect,
        mqtt_options = mcp_mqtt.MqttOptions(
            host="localhost",
        )
    ) as client:
        await client.start()
        while True:
            ## Simulate other works while the MQTT transport client is running in the background...
            await anyio.sleep(20)

if __name__ == "__main__":
    anyio.run(main)
```

这个 Python 客户端会自动发现名为 `demo/kotlin-mcp-server` 的 MCP 服务器，并调用 `set_light_brightness` 工具将电灯亮度设置为 50:

```bash
INFO 2025-12-19 13:07:44,445 - Calling the tool as set_light_brightness(value=50), result: meta=None
                             content=[TextContent(type='text', text='Light brightness set to: 50%',
                             annotations=None)] isError=False
```
