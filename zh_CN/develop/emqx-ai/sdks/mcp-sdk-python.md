# Python SDK

本文将使用 [MCP over MQTT Python SDK](https://github.com/emqx/mcp-python-sdk) 创建一个简单的 MCP over MQTT 服务端和客户端。

## 创建一个演示项目

使用 [uv](https://docs.astral.sh/uv/) 创建一个演示项目：

```bash
uv init mcp_over_mqtt_demo
cd mcp_over_mqtt_demo
```

## 创建一个简单的 MCP 服务器

在 `mcp_over_mqtt_demo` 项目中，让我们创建一个简单的 MCP 服务器，暴露一个工具做整数的加法运算，和一个简单的资源返回字符串。创建一个名为 `demo_mcp_server.py` 的文件，代码如下：

```python
# demo_mcp_server.py
from mcp.server.fastmcp import FastMCP

# Create an MCP server
mcp = FastMCP(
    "demo_mcp_server/calculator",
    log_level="DEBUG",
    mqtt_server_description="A simple FastMCP server that exposes a calculator tool",
    mqtt_options={
        "host": "broker.emqx.io",
    },
)

# Add an addition tool
@mcp.tool()
def add(a: int, b: int) -> int:
    """Add two numbers"""
    return a + b

# Add a dynamic greeting resource
@mcp.resource("greeting://{name}")
def get_greeting(name: str) -> str:
    """Get a personalized greeting"""
    return f"Hello, {name}!"
```

## 创建一个简单的 MCP 客户端

在同一个项目中，让我们创建一个简单的 MCP 客户端，连接到服务器并列出可用的工具和资源。创建一个名为 `demo_mcp_client.py` 的文件，并添加以下代码：

```python
# demo_mcp_client.py
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
    if capabilities.prompts:
        prompts = await client.list_prompts(server_name)
        logger.info(f"Prompts of {server_name}: {prompts}")
    if capabilities.resources:
        resources = await client.list_resources(server_name)
        logger.info(f"Resources of {server_name}: {resources}")
        resource_templates = await client.list_resource_templates(server_name)
        logger.info(f"Resources templates of {server_name}: {resource_templates}")
    if capabilities.tools:
        toolsResult = await client.list_tools(server_name)
        tools = toolsResult.tools
        logger.info(f"Tools of {server_name}: {tools}")
        if tools[0].name == "add":
            result = await client.call_tool(server_name, name = tools[0].name, arguments={"a": 1, "b": 2})
            logger.info(f"Calling the tool as add(a=1, b=2), result: {result}")

async def on_mcp_disconnect(client, server_name):
    logger.info(f"Disconnected from {server_name}")

async def main():
    async with mcp_mqtt.MqttTransportClient(
        "test_client",
        auto_connect_to_mcp_server = True,
        on_mcp_server_discovered = on_mcp_server_discovered,
        on_mcp_connect = on_mcp_connect,
        on_mcp_disconnect = on_mcp_disconnect,
        mqtt_options = mcp_mqtt.MqttOptions(
            host="broker.emqx.io",
        )
    ) as client:
        await client.start()
        while True:
            ## Simulate other works while the MQTT transport client is running in the background...
            await anyio.sleep(20)

if __name__ == "__main__":
    anyio.run(main)
```

## 运行演示

1. 首先，安装所需的依赖项：

```bash
uv add git+https://github.com/emqx/mcp-python-sdk --branch main
uv add "mcp[cli]"
```

2. 现在运行客户端：

```bash
uv run demo_mcp_client.py
```

3. 打开一个新终端并运行服务器:

```bash
uv run mcp run --transport mqtt ./demo_mcp_server.py
```

即便客户端先于服务器启动，它也会发现服务器并连接到它。客户端将列出可用的工具和资源，并使用参数 `a=1` 和 `b=2` 调用 `add` 工具。
