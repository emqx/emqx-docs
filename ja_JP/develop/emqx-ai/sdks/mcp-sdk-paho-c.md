# C SDK と Paho MQTT

このガイドでは、[Paho MQTT を使用した MCP over MQTT C SDK](https://github.com/mqtt-ai/paho-mcp-over-mqtt) を使って、シンプルな **MCP over MQTT サーバー** を作成する方法を説明します。現時点では MCP サーバーのみがサポートされています。相互作用のために MCP クライアントを作成する場合は、Python SDK をご利用ください。

## MCP サーバーの作成

[C SDK と Paho MQTT の README](https://github.com/mqtt-ai/paho-mcp-over-mqtt) の手順に従い、依存関係と SDK をインストールした後、`demo_mcp_server.c` というファイルを作成し、以下のコードを追加してください。

```c
#include "mcp_server.h"

// 温度取得のコールバック関数
const char* get_temperature_callback(int n_args, property_t *args) {
    // センサーからデータを読み取る
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
    "sensor",                // サーバー名
    "Sensor MCP Server",     // 説明
    "mqtt://broker.example.com", // MQTT ブローカーの URI
    "client_001",            // クライアント ID
    "username",              // ユーザー名
    "password",              // パスワード
    NULL                     // 証明書（任意）
);

// ツールの登録
mcp_server_register_tool(server, 1, my_tools);

// サーバーの起動
mcp_server_run(server);
```

## Python SDK で MCP クライアントを作成・実行する

[Python SDK](./mcp-sdk-python.md) のドキュメントを参照し、上記で作成した MCP サーバーに接続して `get_temperature` ツールを呼び出す MCP クライアントを作成してください。

## CMake で MCP サーバーをビルド・コンパイルする

[paho-mcp-over-mqtt リポジトリ](https://github.com/mqtt-ai/paho-mcp-over-mqtt) にある **CMake の例** を参考にしてください。

ビルド後、生成された実行ファイルを実行して MCP サーバーを起動します。

```bash
./demo_mcp_server
```
