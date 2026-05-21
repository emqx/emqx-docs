<<<<<<< HEAD
# Paho MQTTを用いたC SDK

本ガイドでは、[MCP over MQTT C SDK with Paho MQTT](https://github.com/mqtt-ai/paho-mcp-over-mqtt) を使用して、シンプルな**MCP over MQTTサーバー**を作成する方法を示します。現時点ではMCPサーバーのみがサポートされています。MCPクライアントを作成してやり取りを行う場合は、Python SDKをご利用ください。

## MCPサーバーの作成

[C SDK with Paho MQTT README](https://github.com/mqtt-ai/paho-mcp-over-mqtt) の手順に従い、依存関係とSDKをインストールした後、`demo_mcp_server.c` というファイルを作成し、以下のコードを追加してください。
=======
# C SDK と Paho MQTT

このガイドでは、[MCP over MQTT C SDK with Paho MQTT](https://github.com/mqtt-ai/paho-mcp-over-mqtt) を使用して、シンプルな **MCP over MQTT サーバー** を作成する方法を説明します。現在、MCPサーバーのみがサポートされています。Python SDK を使用して MCP クライアントを作成し、相互作用させることが可能です。

## MCP サーバーの作成

[C SDK with Paho MQTT README](https://github.com/mqtt-ai/paho-mcp-over-mqtt) の手順に従い、依存関係と SDK をインストールした後、`demo_mcp_server.c` というファイルを作成し、以下のコードを追加してください。
>>>>>>> origin/release-5.10

```c
#include "mcp_server.h"

// 温度取得のコールバック関数
const char* get_temperature_callback(int n_args, property_t *args) {
    // センサーからデータを読み取る
    float temp = read_temperature_sensor();

    // JSON形式の結果を返す
    static char result[64];
    snprintf(result, sizeof(result), "{\"temperature\": %.2f}", temp);
    return result;
}

// MCPツールの定義
mcp_tool_t my_tools[] = {
    {
        .name = "get_temperature",
        .description = "デバイスの温度を取得",
        .property_count = 0,
        .properties = NULL,
        .call = get_temperature_callback
    }
};

// MCPサーバーの初期化
mcp_server_t *server = mcp_server_init(
    "sensor",                // サーバー名
    "Sensor MCP Server",     // 説明
    "mqtt://broker.example.com", // MQTTブローカーURI
    "client_001",            // クライアントID
    "username",              // ユーザー名
    "password",              // パスワード
    NULL                     // 証明書（任意）
);

// ツールの登録
mcp_server_register_tool(server, 1, my_tools);

// サーバーの起動
mcp_server_run(server);
```

<<<<<<< HEAD
## Python SDKでMCPクライアントを作成・実行する

上記で作成したMCPサーバーに接続し、`get_temperature` ツールを呼び出すMCPクライアントの作成方法は、[Python SDK](./mcp-sdk-python.md) のドキュメントをご参照ください。

## CMakeによるMCPサーバーのビルドとコンパイル

[paho-mcp-over-mqttリポジトリ](https://github.com/mqtt-ai/paho-mcp-over-mqtt) にある**CMakeの例**に従ってビルドを行えます。

ビルド後、生成された実行ファイルを実行してMCPサーバーを起動してください。
=======
## Python SDK で MCP クライアントを作成・実行

上記で作成した MCP サーバーに接続し、`get_temperature` ツールを呼び出す MCP クライアントの作成については、[Python SDK](./mcp-sdk-python.md) のドキュメントを参照してください。

## CMake で MCP サーバーをビルド・コンパイル

[paho-mcp-over-mqtt リポジトリ](https://github.com/mqtt-ai/paho-mcp-over-mqtt) にある **CMake の例** を参考にしてください。

ビルド後、生成された実行ファイルを実行して MCP サーバーを起動します。
>>>>>>> origin/release-5.10

```bash
./demo_mcp_server
```
