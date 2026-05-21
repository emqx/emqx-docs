# ESP32 C SDK

<<<<<<< HEAD
このガイドでは、[ESP32向けMCP over MQTT C SDK](https://github.com/mqtt-ai/esp-mcp-over-mqtt)を使用して、シンプルなMCP over MQTTサーバーを作成する方法を説明します。現在、MCPサーバーのみがサポートされています。MCPクライアントはPython SDKを使用して作成し、相互作用させることが可能です。
=======
このガイドでは、[ESP32向けMCP over MQTT C SDK](https://github.com/mqtt-ai/esp-mcp-over-mqtt)を使用して、シンプルなMCP over MQTTサーバーを作成する方法を説明します。現在、MCPサーバーのみがサポートされています。MCPクライアントはPython SDKを使用して作成し、インタラクションを行うことが可能です。
>>>>>>> origin/release-5.10

このSDKはESP-IDFに含まれるMQTTライブラリを使用しているため、ESP32デバイスに適しており、ESP-IDF環境内で使用する必要があります。

## MCPサーバーの作成

[ESP32 C SDK README](https://github.com/mqtt-ai/esp-mcp-over-mqtt)の指示に従い、ESP-IDFプロジェクト内に`mcp_server_example.c`という新しいファイルを作成し、以下のコードを追加してください。

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
// MCPツールを定義
mcp_tool_t my_tools[] = {
    {
        .name = "get_temperature",
        .description = "デバイスの温度を取得",
=======
// MCPツールの定義
mcp_tool_t my_tools[] = {
    {
        .name = "get_temperature",
        .description = "デバイスの温度を取得する",
>>>>>>> origin/release-5.10
        .property_count = 0,
        .properties = NULL,
        .call = get_temperature_callback
    }
};

<<<<<<< HEAD
// MCPサーバーを初期化
=======
// MCPサーバーの初期化
>>>>>>> origin/release-5.10
mcp_server_t *server = mcp_server_init(
    "esp32_sensor",             // サーバー名
    "ESP32 Sensor MCP Server",  // 説明
    "mqtt://broker.example.com",// MQTTブローカーURI
    "esp32_client_001",         // クライアントID
    "username",                 // ユーザー名
    "password",                 // パスワード
    NULL                        // 証明書（任意）
);

<<<<<<< HEAD
// ツールを登録
mcp_server_register_tool(server, 1, my_tools);

// サーバーを起動
=======
// ツールの登録
mcp_server_register_tool(server, 1, my_tools);

// サーバーの起動
>>>>>>> origin/release-5.10
mcp_server_run(server);
```

## ESP-IDFプロジェクトでのMCPサーバーの使用

<<<<<<< HEAD
詳細な使用方法については、[ESP32 MCP Demo](https://github.com/mqtt-ai/esp32-mcp-mqtt-tutorial/tree/main/samples/blog_3)プロジェクトをご参照ください。この例では、ESP-IDFプロジェクトにMCP over MQTT C SDK for ESP32を統合し、MCPサーバーをセットアップし、Python SDKで実装されたMCPクライアントと連携する方法を示しています。
=======
詳細な使用方法については、[ESP32 MCP Demo](https://github.com/mqtt-ai/esp32-mcp-mqtt-tutorial/tree/main/samples/blog_3)プロジェクトを参照してください。この例では、ESP-IDFプロジェクトにMCP over MQTT C SDK for ESP32を統合し、MCPサーバーをセットアップし、Python SDKで実装されたMCPクライアントを使ってインタラクションする方法を示しています。
>>>>>>> origin/release-5.10

ESP-IDFでプロジェクトをビルドし、ESP32デバイスにフラッシュすると、MCPサーバーが自動的に起動します。
