# Kotlin SDK

<<<<<<< HEAD
このガイドでは、Kotlin SDK を使用して MCP over MQTT 対応のシンプルな MCP サーバーを作成する方法を説明します。  
リポジトリ：https://github.com/terry-xiaoyu/kotlin-sdk

## 環境構築
=======
このガイドでは、Kotlin SDK を使用して MCP over MQTT 対応のシンプルな MCPサーバーを作成する方法を説明します。  
リポジトリ：https://github.com/terry-xiaoyu/kotlin-sdk

## 環境セットアップ
>>>>>>> origin/release-5.10

### Kotlin ツールチェーンのインストール

以下のツールがインストールされていることを確認してください。

- JDK 21以上
- Kotlin 2.2以上
- Gradle 9.2以上

<<<<<<< HEAD
[SDKMAN](https://sdkman.io/) を使用すると、これらのツールを簡単にインストールおよび管理できます。
=======
これらのツールのインストールおよび管理には、[SDKMAN](https://sdkman.io/) の利用を推奨します。
>>>>>>> origin/release-5.10

```bash
curl -s "https://get.sdkman.io" | bash
source "$HOME/.sdkman/bin/sdkman-init.sh"

sdk install java 21.0.9-tem
sdk install kotlin 2.2.21
sdk install gradle 9.2.1
```

### EMQX のインストールと起動

EMQX ブローカーのインストールおよび起動については、[Getting Started](../../getting-started/getting-started.md) ガイドに従ってください。

<<<<<<< HEAD
## MCP サーバーのサンプルをダウンロードして実行する
=======
## MCPサーバーのサンプルをダウンロードして実行
>>>>>>> origin/release-5.10

サンプルプロジェクトをクローンします。

```bash
git clone https://github.com/terry-xiaoyu/kotlin-mcp-server-demo.git
cd kotlin-mcp-server-demo
```

<<<<<<< HEAD
このサンプルでは、以下の2つの MCP ツールを登録しています。

- **Calculator tool**：基本的な算術演算（加算、減算、乗算、除算）を提供します。
- **Light control tool**：ライトのオン/オフ状態と明るさを制御します。

ツール登録のコードはこちらで確認できます。  
=======
このサンプルでは、以下の2つの MCPツールを登録しています。

- **Calculator tool**：基本的な算術演算（加算、減算、乗算、除算）を提供します。
- **Light control tool**：ライトのオン／オフ状態と明るさを制御します。

ツール登録のコードは以下で確認できます。  
>>>>>>> origin/release-5.10
https://github.com/terry-xiaoyu/kotlin-mcp-server-demo/blob/e83d5166c5eefb3a45758623e3ee69f92cecb911/src/main/kotlin/io/modelcontextprotocol/sample/server/server.kt#L93

```bash
// Calculator tool を追加
server.addTool(
    name = "calculator",
    description = "このツールは基本的な数学演算（加算、減算、乗算、除算）を実行できます。",
    inputSchema = ToolSchema(
        properties = buildJsonObject {
            putJsonObject("num1") {
                put("type", JsonPrimitive("number"))
                put("description", JsonPrimitive("最初の数値"))
            }
            putJsonObject("num2") {
                put("type", JsonPrimitive("number"))
                put("description", JsonPrimitive("2番目の数値"))
            }
            putJsonObject("op") {
                put("type", JsonPrimitive("string"))
<<<<<<< HEAD
                put("description", JsonPrimitive("実行する演算子：+, -, *, /"))
=======
                put("description", JsonPrimitive("実行する演算子: +, -, *, /"))
>>>>>>> origin/release-5.10
            }
        },
        required = listOf("num1", "num2", "op"),
    ),
) { request : CallToolRequest ->
    val num1 = request.params.arguments?.get("num1")?.jsonPrimitive?.content?.toDoubleOrNull()
        ?: throw IllegalArgumentException("無効または欠落している引数: num1")
    val num2 = request.params.arguments?.get("num2")?.jsonPrimitive?.content?.toDoubleOrNull()
        ?: throw IllegalArgumentException("無効または欠落している引数: num2")
    val op = request.params.arguments?.get("op")?.jsonPrimitive?.content?.firstOrNull()
        ?: throw IllegalArgumentException("無効または欠落している引数: op")
    val x = num1.toDouble()
    val y = num2.toDouble()
    val result = when(op) {
        '+' -> x + y
        '-' -> x - y
        '*' -> x * y
<<<<<<< HEAD
        '/' -> if (y == 0.0) throw ArithmeticException("ゼロによる除算") else x / y
=======
        '/' -> if (y == 0.0) throw ArithmeticException("ゼロ除算") else x / y
>>>>>>> origin/release-5.10
        else -> throw IllegalArgumentException("サポートされていない演算子: $op")
    }
    CallToolResult(
        content = listOf(TextContent("結果は: $result")),
    )
}

// Light control tool を追加
server.addTool(
    name = "set_light_brightness",
<<<<<<< HEAD
    description = "パネル上のライトを制御します。明るさを変更できます。ライトを消すには明るさを0に設定します。明るさを 'last_value' に設定すると、前回の明るさに戻せます。これはライトがオフのときに再度オンにする場合に便利です。",
=======
    description = "パネル上のライトを制御します。明るさを変更できます。ライトを消すには明るさを0に設定してください。明るさを 'last_value' に設定すると、前回の明るさを復元します。これはライトが消灯中に再点灯したい場合に便利です。",
>>>>>>> origin/release-5.10
    inputSchema = ToolSchema(
        properties = buildJsonObject {
            putJsonObject("value") {
                put("type", JsonArray(listOf(JsonPrimitive("number"), JsonPrimitive("string"))))
<<<<<<< HEAD
                put("description", JsonPrimitive("0から100の明るさの値、または前回の明るさに戻すための 'last_value'"))
=======
                put("description", JsonPrimitive("0から100の明るさの値、または前回の明るさを復元する 'last_value'"))
>>>>>>> origin/release-5.10
            }
        },
        required = listOf("value")
    ),
) { request ->
    val value = request.params.arguments?.get("value")?.jsonPrimitive?.content
    // リクエストを処理し、ライトの明るさを制御
    CallToolResult(
        content = listOf(TextContent("ライトの明るさを ${value}% に設定しました")),
    )
}
```

<<<<<<< HEAD
以下のコマンドで MCP サーバーのサンプルを起動します。
=======
以下のコマンドで MCPサーバーのサンプルを起動します。
>>>>>>> origin/release-5.10

```bash
./gradlew run --args="--mqtt kt001 demo/kotlin-mcp-server"
```

パラメーターの説明：

<<<<<<< HEAD
- **MQTT クライアント ID**：`kt001`
- **MCP サーバー名**：`demo/kotlin-mcp-server`

## MCP クライアントでテストする

現在、Kotlin SDK には MCP クライアントの実装がありません。テスト用に Python SDK を使ってシンプルな MCP クライアントを作成できます。
=======
- **MQTT クライアントID**：`kt001`
- **MCPサーバー名**：`demo/kotlin-mcp-server`

## MCPクライアントでのテスト

現在、Kotlin SDK には MCPクライアントの実装がありません。  
テスト用に Python SDK を使ってシンプルな MCPクライアントを作成できます。
>>>>>>> origin/release-5.10

Python 環境のセットアップ：

```bash
uv init light_controller
cd light_controller
uv add git+https://github.com/emqx/mcp-python-sdk --branch main
uv add "mcp[cli]"
source .venv/bin/activate
```

<<<<<<< HEAD
`light_controller.py` というファイルを作成し、以下の内容を記述します。
=======
`light_controller.py` というファイルを作成し、以下の内容を記述してください。
>>>>>>> origin/release-5.10

```bash
# light_controller.py
import logging
import anyio
import mcp.client.mqtt as mcp_mqtt
from mcp.shared.mqtt import configure_logging

configure_logging(level="INFO")
logger = logging.getLogger(__name__)

async def on_mcp_server_discovered(client, server_name):
<<<<<<< HEAD
    logger.info(f"{server_name} を検出しました。接続しています...")
=======
    logger.info(f"{server_name} を検出しました。接続中・・・")
>>>>>>> origin/release-5.10
    await client.initialize_mcp_server(server_name)

async def on_mcp_connect(client, server_name, connect_result):
    success, init_result = connect_result
    if success == 'error':
        logger.error(f"{server_name} への接続に失敗しました: {init_result}")
        return
    logger.info(f"{server_name} に接続しました。success={success}, init_result={init_result}")
    capabilities = init_result.capabilities
    if capabilities.tools:
        toolsResult = await client.list_tools(server_name)
        tools = toolsResult.tools
        logger.info(f"{server_name} のツール一覧: {tools}")
        for tool in tools:
            logger.info(f" - {tool.name}: {tool.description}")
            if tool.name == "set_light_brightness":
                result = await client.call_tool(
                    server_name,
                    name=tool.name,
                    arguments={"value": 50}
                )
                logger.info(
<<<<<<< HEAD
                    f"set_light_brightness(value=50) ツールを呼び出しました。結果: {result}"
=======
                    f"set_light_brightness(value=50) を呼び出しました。結果: {result}"
>>>>>>> origin/release-5.10
                )

async def main():
    async with mcp_mqtt.MqttTransportClient(
        "test_client",
        server_name_filter="demo/kotlin-mcp-server",
        auto_connect_to_mcp_server=True,
        on_mcp_server_discovered=on_mcp_server_discovered,
        on_mcp_connect=on_mcp_connect,
        mqtt_options=mcp_mqtt.MqttOptions(
            host="localhost",
        )
    ) as client:
        await client.start()
        while True:
<<<<<<< HEAD
            # MQTT クライアントがバックグラウンドで動作している間、他の処理をシミュレート
=======
            # MQTTクライアントがバックグラウンドで動作している間に他の処理をシミュレート
>>>>>>> origin/release-5.10
            await anyio.sleep(20)

if __name__ == "__main__":
    anyio.run(main)
```

<<<<<<< HEAD
この Python クライアントは、`demo/kotlin-mcp-server` という MCP サーバーを自動検出し、`set_light_brightness` ツールを呼び出してライトの明るさを50に設定します。

```bash
INFO 2025-12-19 13:07:44,445 - set_light_brightness(value=50) ツールを呼び出しました。結果: meta=None
=======
この Pythonクライアントは、`demo/kotlin-mcp-server` という名前の MCPサーバーを自動検出し、`set_light_brightness` ツールを呼び出してライトの明るさを50に設定します。

```bash
INFO 2025-12-19 13:07:44,445 - set_light_brightness(value=50) を呼び出しました。結果: meta=None
>>>>>>> origin/release-5.10
                             content=[TextContent(type='text', text='ライトの明るさを 50% に設定しました',
                             annotations=None)] isError=False
```
