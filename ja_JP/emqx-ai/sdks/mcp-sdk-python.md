# Python SDK

<<<<<<< HEAD
このガイドでは、[MCP over MQTT Python SDK](https://github.com/emqx/mcp-python-sdk) を使用して、シンプルなMCP over MQTTサーバーとクライアントを作成する方法を説明します。
=======
このガイドでは、[MCP over MQTT Python SDK](https://github.com/emqx/mcp-python-sdk) を使用して、シンプルな MCP over MQTT サーバーとクライアントを作成する方法を説明します。
>>>>>>> origin/release-6.1

## デモプロジェクトの作成

[uv](https://docs.astral.sh/uv/) を使ってデモプロジェクトを作成しましょう。

```bash
uv init mcp_over_mqtt_demo
cd mcp_over_mqtt_demo
```

<<<<<<< HEAD
## シンプルなMCPサーバーの作成

`mcp_over_mqtt_demo` プロジェクト内に、計算ツールといくつかのリソースを公開するシンプルなMCPサーバーを作成します。`demo_mcp_server.py` というファイルを作成し、以下のコードを追加してください。
=======
## シンプルな MCP サーバーの作成

`mcp_over_mqtt_demo` プロジェクト内で、計算ツールといくつかのリソースを公開するシンプルな MCP サーバーを作成します。`demo_mcp_server.py` というファイルを作成し、以下のコードを追加してください。
>>>>>>> origin/release-6.1

```python
# demo_mcp_server.py
from mcp.server.fastmcp import FastMCP

# MCPサーバーを作成
mcp = FastMCP(
    "demo_mcp_server/calculator",
    log_level="DEBUG",
    mqtt_server_description="A simple FastMCP server that exposes a calculator tool",
    mqtt_options={
        "host": "broker.emqx.io",
    },
)

<<<<<<< HEAD
# 足し算ツールを追加
@mcp.tool()
def add(a: int, b: int) -> int:
    """2つの数値を足し合わせる"""
=======
# 加算ツールを追加
@mcp.tool()
def add(a: int, b: int) -> int:
    """2つの数値を加算する"""
>>>>>>> origin/release-6.1
    return a + b

# 動的な挨拶リソースを追加
@mcp.resource("greeting://{name}")
def get_greeting(name: str) -> str:
<<<<<<< HEAD
    """パーソナライズされた挨拶を取得"""
    return f"Hello, {name}!"
```

## シンプルなMCPクライアントの作成

同じプロジェクト内に、サーバーに接続して利用可能なツールやリソースを一覧表示するシンプルなMCPクライアントを作成します。`demo_mcp_client.py` というファイルを作成し、以下のコードを追加してください。
=======
    """パーソナライズされた挨拶を取得する"""
    return f"Hello, {name}!"
```

## シンプルな MCP クライアントの作成

同じプロジェクト内で、サーバーに接続し利用可能なツールやリソースを一覧表示するシンプルな MCP クライアントを作成します。`demo_mcp_client.py` というファイルを作成し、以下のコードを追加してください。
>>>>>>> origin/release-6.1

```python
# demo_mcp_client.py
import logging
import anyio
import mcp.client.mqtt as mcp_mqtt
from mcp.shared.mqtt import configure_logging

configure_logging(level="INFO")
logger = logging.getLogger(__name__)

async def on_mcp_server_discovered(client, server_name):
<<<<<<< HEAD
    logger.info(f"{server_name} を検出しました。接続中…")
=======
    logger.info(f"{server_name} を発見しました。接続中・・・")
>>>>>>> origin/release-6.1
    await client.initialize_mcp_server(server_name)

async def on_mcp_connect(client, server_name, connect_result):
    success, init_result = connect_result
    if success == 'error':
        logger.error(f"{server_name} への接続に失敗しました: {init_result}")
        return
    logger.info(f"{server_name} に接続しました。success={success}, init_result={init_result}")
    capabilities = init_result.capabilities
    if capabilities.prompts:
        prompts = await client.list_prompts(server_name)
        logger.info(f"{server_name} のプロンプト: {prompts}")
    if capabilities.resources:
        resources = await client.list_resources(server_name)
        logger.info(f"{server_name} のリソース: {resources}")
        resource_templates = await client.list_resource_templates(server_name)
        logger.info(f"{server_name} のリソーステンプレート: {resource_templates}")
    if capabilities.tools:
        toolsResult = await client.list_tools(server_name)
        tools = toolsResult.tools
        logger.info(f"{server_name} のツール: {tools}")
        if tools[0].name == "add":
            result = await client.call_tool(server_name, name=tools[0].name, arguments={"a": 1, "b": 2})
            logger.info(f"ツール add(a=1, b=2) を呼び出し、結果: {result}")

async def on_mcp_disconnect(client, server_name):
    logger.info(f"{server_name} から切断されました")

async def main():
    async with mcp_mqtt.MqttTransportClient(
        "test_client",
        auto_connect_to_mcp_server=True,
        on_mcp_server_discovered=on_mcp_server_discovered,
        on_mcp_connect=on_mcp_connect,
        on_mcp_disconnect=on_mcp_disconnect,
        mqtt_options=mcp_mqtt.MqttOptions(
            host="broker.emqx.io",
        )
    ) as client:
        client.start()
        while True:
<<<<<<< HEAD
            ## MQTTトランスポートクライアントがバックグラウンドで動作している間、他の処理をシミュレートします…
=======
            ## MQTTトランスポートクライアントがバックグラウンドで動作している間に他の処理をシミュレートします...
>>>>>>> origin/release-6.1
            await anyio.sleep(20)

if __name__ == "__main__":
    anyio.run(main)
```

## デモの実行

1. 必要な依存関係をインストールします。

```bash
uv add git+https://github.com/emqx/mcp-python-sdk --branch main
uv add "mcp[cli]"
```

2. クライアントを実行します。

```bash
uv run demo_mcp_client.py
```

3. 新しいターミナルを開き、サーバーを実行します。

```bash
uv run mcp run --transport mqtt ./demo_mcp_server.py
```

クライアントがサーバーより先に起動しても、サーバーを検出して接続します。クライアントは利用可能なツールやリソースを一覧表示し、`add` ツールをパラメータ `a=1`、`b=2` で呼び出します。
