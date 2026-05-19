# MCP Bridge プラグイン

[EMQX MCP Bridge プラグイン](https://github.com/emqx/emqx_mcp_bridge) は、EMQX と MCP（Model Context Protocol）対応デバイスを統合するためのプラグインです。このプラグインを使用することで、MCP対応の大規模言語モデルやAIエージェントを使ってIoTデバイスにアクセスし、制御することが可能になります。

## MCP Bridge プラグインの仕組み

MCP Bridge プラグインはEMQX内にインストールされて動作します。起動後、Streamable HTTP または SSE に基づくMCP接続をMQTTプロトコルに変換するHTTPエンドポイントを公開します。

IoTデバイスはMQTTを使ってEMQXブローカーに接続し、MCP対応の大規模モデルやAIエージェントはMCP Bridge プラグインが公開するHTTPエンドポイントに接続します。

```mermaid
graph LR
    subgraph "EMQX"
        MB[MCP Bridge Plugin]
    end
    subgraph "AI Agents"
        M1[LLM / MCP Client]
        M1 --> |MCP-HTTP| MB
    end
    subgraph "Devices"
        D1[Device 1]
        D2[Device 2]
        D3[Device 3]
        MB --> |MQTT| D1
        MB --> |MQTT| D2
        MB --> |MQTT| D3
    end
```

## MCP over MQTT を使ったデバイスアクセス

デバイス側では、MCP over MQTT プロトコルを使用し、MCPサーバーとして自身のツールや機能を直接公開できます。プラグインはデバイスが登録したツールをツールタイプごとに集約します。MCP Bridge プラグイン内では、MCP over MQTT プロトコルのServer Nameの概念がツールタイプにマッピングされています。

つまり、同じタイプの複数デバイスが登録したツールは、ブリッジプラグインによって単一の論理的なツールに集約され、MCPクライアントから呼び出せるようになります。

この方式は、スマートホーム、産業制御システム、音声対応玩具など、単一または少数のデバイスにクライアントがアクセスするシナリオに適しています。これらのシナリオでは、ユーザーは大規模なデバイス群を管理するのではなく、自身のデバイスにのみアクセスすることが一般的です。

同じタイプの複数デバイスのツールが単一の論理ツールに集約されるため、MCP Bridge プラグインはツール定義に `target-mqtt-client-id` という必須パラメータを注入します。AIエージェントがツールを呼び出す際には、ビジネスロジックに基づいて対象デバイスIDを決定し、このパラメータで指定する必要があります。これにより、MCPリクエストが特定のデバイスにルーティングされます。

```mermaid
graph LR
    subgraph "EMQX"
        MB[MCP Bridge Plugin]
    end
    subgraph "AI Agents"
        M1[LLM / MCP Client]
        M1 --> |MCP tools/call<br>target-mqtt-client-id: aec1| MB
    end
    subgraph "Devices"
        D1[Light: aec1]
        D2[Light: ec82]
        D3[Fan: 3cfa]
        MB --> |MCP over MQTT| D1
        MB -.-> |MCP over MQTT| D2
        MB -.-> |MCP over MQTT| D3
    end
```

## 標準MQTTを使ったデバイスアクセス

デバイスはMCP over MQTTではなく、標準のMQTTプロトコルを使ってEMQXに接続することも可能です。この場合、ユーザーはMCP Bridge プラグイン内にMCPツールを直接実装し、これらの通常のMQTTデバイスに間接的にアクセスできます。

この方式は、スマートシティ、コネクテッドビークル、産業用IoTなど、より柔軟なデバイスアクセスが求められるシナリオに適しています。MCP Bridge プラグイン内では、ユーザー定義の外部サービスやAPIへのアクセス、外部データベースからのデバイス報告データの取得など、任意のビジネスロジックを実装できます。

コードでのMCPツール実装例については、[Create Custom MCP Tools](https://github.com/emqx/emqx_mcp_bridge?tab=readme-ov-file#create-custom-mcp-tools) を参照してください。

```mermaid
graph LR
    subgraph "AI Agents"
        M1[LLM / MCP Client]
    end
    subgraph "Devices"
        D1[Device 1]
        D2[Device 2]
        D3[Device 3]
    end
    subgraph EMQX["EMQX"]
        direction BT
        MB[MCP Bridge Plugin]
        CM[User-Provided Module<br>Tools: tool1,tool2,...]
        MB --> |MQTT| D1
        MB -.-> |MQTT| D2
        MB -.-> |MQTT| D3
        M1 --> |MCP tools/call<br>userid=ee| MB
    end
    subgraph TSDB["Time Series Database"]
        R1[Records:<br>t1,device1,status1<br>t2,device2,status2<br>...]
    end
    subgraph "User-Defined Service"
        UDS[HTTP API]
    end
    CM --> |query| TSDB
    TSDB --> |result| CM
    CM --> |Get the device of<br>userid=ee| UDS
    UDS --> |Device 1| CM
```
