# Paho MQTTを使用したC SDK

このガイドでは、[Paho MQTTを使用したMCP over MQTT C SDK](https://github.com/mqtt-ai/paho-mcp-over-mqtt)を使って、シンプルな**MCP over MQTTサーバー**を作成する方法を説明します。現在、MCPサーバーのみがサポートされています。Python SDKを使用して、対話用のMCPクライアントを作成できます。

## MCPサーバーの作成

[C SDK with Paho MQTT README](https://github.com/mqtt-ai/paho-mcp-over-mqtt)の手順に従い、依存関係とSDKをインストールした後、`demo_mcp_server.c`というファイルを作成し、以下のコードを追加してください。

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
        .description = "デバイスの温度を取得する",
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

## Python SDKでMCPクライアントを作成・実行する

[Python SDK](./mcp-sdk-python.md)のドキュメントを参照し、上記で作成したMCPサーバーに接続し、`get_temperature`ツールを呼び出すMCPクライアントを作成してください。

## CMakeでMCPサーバーをビルド・コンパイルする

[paho-mcp-over-mqttリポジトリ](https://github.com/mqtt-ai/paho-mcp-over-mqtt)にある**CMakeの例**を参考にしてください。

ビルド後、生成された実行ファイルを実行してMCPサーバーを起動します。

```bash
./demo_mcp_server
```
