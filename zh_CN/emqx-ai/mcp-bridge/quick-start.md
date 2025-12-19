# 使用 EMQX MCP 桥接访问物联网设备

本指南介绍如何使用 EMQX MCP 桥接 EMQX 与支持 MCP 的模型或者 AI 智能体进行集成，从而实现对物联网设备的访问和控制。

## 前置条件

- 已经安装并运行 EMQX 服务器，版本要求 5.7.0 及以上。
- 设备端使用 MQTT 协议或者 MCP over MQTT 协议连接到 EMQX 服务器。

## 安装和配置 MCP 桥接插件

1. 从 https://github.com/emqx/emqx_mcp_bridge/releases 下载最新版本的 MCP 桥接插件。

2. 按照 [安装插件](../../extensions/plugin-management.md#安装流程) 的步骤将插件安装到 EMQX 服务器中。

3. 配置插件：

   使用浏览器访问 http://localhost:18083/#/plugins/ 地址，点击 MCP 桥接插件进入配置页面，这里可以修改插件的监听地址，配置证书等参数。点击保存后，配置会自动被应用无需手动重启插件。

   注意，如果将监听地址配置为 `https://your-hostname:9909/mcp`，MCP 插件会在指定的端口上启动两个 HTTP 端点：
    - `/sse`：用于 SSE 协议的 MCP 连接。
    - `/mcp`：用于 Streamable HTTP 协议的 MCP 连接。
   如果希望仅支持 SSE 协议，可以将监听地址配置为 `https://your-hostname:9909/sse`。

   另外，某些模型或者 AI 智能体可能要求 MCP 服务器必须使用 HTTPS 协议进行连接，这时需要为 MCP 桥接插件配置有效且可信任的 SSL 证书，并且保证 URL 可以公网访问。

   将 `获取目标 MQTT 客户端 ID 的方式` 配置为 `工具参数`，这样 MCP 客户端就可以在调用时以参数的方式传递设备的 MQTT Client ID，而不必在建立 HTTP 连接的时候，使用 HTTP 头指定一个固定的 Client ID。

## 使用 MCP over MQTT SDK 模拟设备

首先参照 [安装 MCP SDK](../sdks/mcp-sdk-python.md) 文档下载安装 MCP SDK for Python：

```bash
uv init smart_light
cd smart_light
uv add git+https://github.com/emqx/mcp-python-sdk --branch main
uv add "mcp[cli]"
source .venv/bin/activate
```

以下 Python 代码将会启动一个 MCP over MQTT 协议的 MCP Server，该设备模拟了一个智能电灯，可以通过 MCP 工具进行开关和调节亮度。注意我们指定了 `mqtt_client_id = "abc123"`：

```python
# smart_light.py
from mcp.server.fastmcp import FastMCP

status = "off"

# Create server
mcp = FastMCP(
    "devices/light",
    log_level="DEBUG",
    mqtt_server_description="A simple FastMCP server that controls a light device. You can turn the light on and off, and change its brightness.",
    mqtt_client_id = "abc123",
    mqtt_options= {
        "username": "aaa",
        "host": "localhost",
        "port": 1883,
    },
)

@mcp.tool()
def change_brightness(level: int) -> str:
    """Change the brightness of the light, level should be between 0 and 100"""
    if 0 <= level <= 100:
        return f"Changed brightness to {level}"
    return "Invalid brightness level. Please provide a level between 0 and 100."

@mcp.tool()
def turn_on() -> str:
    """Turn the light on"""
    global status
    if status == "on":
        return "OK, but the light is already on"
    status = "on"
    return "Light turned on"

@mcp.tool()
def turn_off() -> str:
    """Turn the light off"""
    global status
    if status == "off":
        return "OK, but the light is already off"
    status = "off"
    return "Light turned off"
```

然后使用以下命令运行该 MCP 服务器：

```bash
uv run mcp run --transport mqtt ./smart_light.py
```

## 使用 5ire 客户端测试

这里我们选择支持 MCP 的 5ire 客户端作为 MCP Client 来测试 EMQX MCP 桥接插件。

1. 参考 [5ire 文档](https://5ire.app/) 安装 MCP 插件。

2. 在 `Providers` 页面添加 LLM 提供商，填写大模型的端点地址和 API Key 等信息。

3. 在 `Tools` 页面，选择 `Remote`，添加 MCP 服务器，填写 MCP 桥接插件中配置的 URL 地址，例如 `https://your-emqx-host:port/sse`。5ire 支持 HTTP 和 SSE 协议，如果是本地测试，这里我们可以填写 `http://0.0.0.0:9909/sse`。

4. 创建一个新的对话，在对话框的右上角中设置系统提示词，以告知模型我们的设备的 Client ID：

```
你是一个可以访问和控制 MQTT 设备的 AI 助手。
我的客厅里有一个智能灯，它的 Client ID 是 abc123。
```

5. 在对话框中输入指令测试智能灯泡的控制功能，例如：
```
打开客厅的灯。
将亮度设置为 75。
```
