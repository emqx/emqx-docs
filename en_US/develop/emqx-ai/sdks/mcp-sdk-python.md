# Python SDK

This guide demonstrates how to use the [MCP over MQTT Python SDK](https://github.com/emqx/mcp-python-sdk) to create a simple MCP over MQTT server and client.

## Create a Demo Project

Let's use [uv](https://docs.astral.sh/uv/) to create a demo project:

```bash
uv init mcp_over_mqtt_demo
cd mcp_over_mqtt_demo
```

## Create a Simple MCP Server

In the `mcp_over_mqtt_demo` project, create a simple MCP server that exposes a calculator tool and some resources. Create a file named `demo_mcp_server.py` and add the following code:

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

## Create a Simple MCP Client

In the same project, create a simple MCP client that connects to the server and lists available tools and resources. Create a file named `demo_mcp_client.py` and add the following code:

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
        client.start()
        while True:
            ## Simulate other works while the MQTT transport client is running in the background...
            await anyio.sleep(20)

if __name__ == "__main__":
    anyio.run(main)
```

## Run the Demo

1. Install the required dependencies:

```bash
uv add git+https://github.com/emqx/mcp-python-sdk --branch main
uv add "mcp[cli]"
```

2. Now run the client:

```bash
uv run demo_mcp_client.py
```

3. Open a new terminal and run the server:

```bash
uv run mcp run --transport mqtt ./demo_mcp_server.py
```

Even if the client starts before the server, it will discover the server and connect to it. The client will list available tools and resources, and call the `add` tool with parameters `a=1` and `b=2`.
