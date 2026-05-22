# ESP32 C SDK

<<<<<<< HEAD
このガイドでは、[MCP over MQTT C SDK for ESP32](https://github.com/mqtt-ai/esp-mcp-over-mqtt) を使用してシンプルな MCP over MQTT サーバーを作成する方法を説明します。現在、MCPサーバーのみがサポートされています。MCPクライアントを作成してやり取りする場合は、Python SDK をご利用ください。

このSDKは ESP-IDF に含まれる MQTT ライブラリを使用しているため、ESP32 デバイスに適しており、ESP-IDF 環境内で使用する必要があります。

## MCPサーバーの作成

[ESP32 C SDK README](https://github.com/mqtt-ai/esp-mcp-over-mqtt) の指示に従い、ESP-IDF プロジェクト内に `mcp_server_example.c` という新しいファイルを作成し、以下のコードを追加してください。
=======
このガイドでは、[ESP32向けMCP over MQTT C SDK](https://github.com/mqtt-ai/esp-mcp-over-mqtt)を使用して、シンプルなMCP over MQTTサーバーを作成する方法を説明します。現在、MCPサーバーのみがサポートされています。MCPクライアントはPython SDKを使用して作成し、相互作用させることが可能です。

このSDKはESP-IDFに含まれるMQTTライブラリを使用しているため、ESP32デバイスに適しており、ESP-IDF環境内で使用する必要があります。

## MCPサーバーの作成

[ESP32 C SDK README](https://github.com/mqtt-ai/esp-mcp-over-mqtt)の指示に従い、ESP-IDFプロジェクト内に`mcp_server_example.c`という新しいファイルを作成し、以下のコードを追加してください。
>>>>>>> origin/release-6.1

```c
#include "mcp_server.h"

const char* get_temperature_callback(int n_args, property_t *args) {
    // センサーのデータを読み取る
    float temp = read_temperature_sensor();
    
    // JSON形式の結果を返す
    static char result[64];
    snprintf(result, sizeof(result), "{\"temperature\": %.2f}", temp);
    return result;
}

<<<<<<< HEAD
// MCPツールの定義
=======
// MCPツールを定義
>>>>>>> origin/release-6.1
mcp_tool_t my_tools[] = {
    {
        .name = "get_temperature",
        .description = "デバイスの温度を取得",
        .property_count = 0,
        .properties = NULL,
        .call = get_temperature_callback
    }
};

<<<<<<< HEAD
// MCPサーバーの初期化
mcp_server_t *server = mcp_server_init(
    "esp32_sensor",             // サーバー名
    "ESP32 Sensor MCP Server",  // 説明
    "mqtt://broker.example.com",// MQTTブローカーのURI
    "esp32_client_001",         // クライアントID
    "username",                 // ユーザー名
    "password",                 // パスワード
    NULL                        // 証明書（オプション）
);

// ツールの登録
mcp_server_register_tool(server, 1, my_tools);

// サーバーの起動
=======
// MCPサーバーを初期化
mcp_server_t *server = mcp_server_init(
    "esp32_sensor",             // サーバー名
    "ESP32 Sensor MCP Server",  // 説明
    "mqtt://broker.example.com",// MQTTブローカーURI
    "esp32_client_001",         // クライアントID
    "username",                 // ユーザー名
    "password",                 // パスワード
    NULL                        // 証明書（任意）
);

// ツールを登録
mcp_server_register_tool(server, 1, my_tools);

// サーバーを起動
>>>>>>> origin/release-6.1
mcp_server_run(server);
```

## ESP-IDFプロジェクトでのMCPサーバーの使用

<<<<<<< HEAD
詳細な使用方法については、[ESP32 MCP Demo](https://github.com/mqtt-ai/esp32-mcp-mqtt-tutorial/tree/main/samples/blog_3) プロジェクトを参照してください。この例では、ESP-IDFプロジェクトに MCP over MQTT C SDK for ESP32 を組み込み、MCPサーバーをセットアップし、Python SDKで実装されたMCPクライアントと連携する方法を示しています。
=======
詳細な使用方法については、[ESP32 MCP Demo](https://github.com/mqtt-ai/esp32-mcp-mqtt-tutorial/tree/main/samples/blog_3)プロジェクトをご参照ください。この例では、ESP-IDFプロジェクトにMCP over MQTT C SDK for ESP32を統合し、MCPサーバーをセットアップし、Python SDKで実装されたMCPクライアントと連携する方法を示しています。
>>>>>>> origin/release-6.1

ESP-IDFでプロジェクトをビルドし、ESP32デバイスにフラッシュすると、MCPサーバーが自動的に起動します。
