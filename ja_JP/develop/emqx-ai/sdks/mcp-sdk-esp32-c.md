# ESP32 C SDK

このガイドでは、[MCP over MQTT C SDK for ESP32](https://github.com/mqtt-ai/esp-mcp-over-mqtt) を使用してシンプルな MCP over MQTT サーバーを作成する方法を説明します。現時点では MCP サーバーのみがサポートされています。MCP クライアントを作成してやり取りする場合は、Python SDK をご利用ください。

この SDK は ESP-IDF に含まれる MQTT ライブラリを使用しているため、ESP32 デバイスに適しており、ESP-IDF 環境内で使用する必要があります。

## MCP サーバーの作成

[ESP32 C SDK README](https://github.com/mqtt-ai/esp-mcp-over-mqtt) の指示に従い、ESP-IDF プロジェクト内に `mcp_server_example.c` という新しいファイルを作成し、以下のコードを追加してください。

```c
#include "mcp_server.h"

const char* get_temperature_callback(int n_args, property_t *args) {
    // センサーのデータを読み取る
    float temp = read_temperature_sensor();
    
    // JSON 形式の結果を返す
    static char result[64];
    snprintf(result, sizeof(result), "{\"temperature\": %.2f}", temp);
    return result;
}

// MCP ツールの定義
mcp_tool_t my_tools[] = {
    {
        .name = "get_temperature",
        .description = "デバイスの温度を取得する",
        .property_count = 0,
        .properties = NULL,
        .call = get_temperature_callback
    }
};

// MCP サーバーの初期化
mcp_server_t *server = mcp_server_init(
    "esp32_sensor",             // サーバー名
    "ESP32 Sensor MCP Server",  // 説明
    "mqtt://broker.example.com",// MQTT ブローカー URI
    "esp32_client_001",         // クライアント ID
    "username",                 // ユーザー名
    "password",                 // パスワード
    NULL                        // 証明書（オプション）
);

// ツールの登録
mcp_server_register_tool(server, 1, my_tools);

// サーバーの起動
mcp_server_run(server);
```

## ESP-IDF プロジェクトでの MCP サーバーの使用

詳細な使用例については、[ESP32 MCP Demo](https://github.com/mqtt-ai/esp32-mcp-mqtt-tutorial/tree/main/samples/blog_3) プロジェクトをご覧ください。この例では、ESP-IDF プロジェクトに MCP over MQTT C SDK for ESP32 を統合し、MCP サーバーをセットアップし、Python SDK で実装された MCP クライアントを使ってやり取りする方法を示しています。

ESP-IDF でプロジェクトをビルドし、ESP32 デバイスにフラッシュすると、MCP サーバーが自動的に起動します。
