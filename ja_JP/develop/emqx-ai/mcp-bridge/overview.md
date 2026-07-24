# MCP ブリッジプラグイン

[EMQX MCP ブリッジプラグイン](https://github.com/emqx/emqx_mcp_bridge) は、EMQX と MCP（Model Context Protocol）対応デバイスを統合するためのプラグインです。このプラグインを使うことで、MCP 対応の大規模言語モデルや AI エージェントから IoT デバイスにアクセスし、制御することが可能になります。

## MCP ブリッジプラグインの仕組み

MCP ブリッジプラグインは EMQX 内にインストールされて動作します。起動後、Streamable HTTP または SSE に基づく MCP 接続を MQTT プロトコルに変換する HTTP エンドポイントを公開します。

IoT デバイスは MQTT を使って EMQX ブローカーに接続し、MCP 対応の大規模モデルや AI エージェントは MCP ブリッジプラグインが公開する HTTP エンドポイントに接続します。

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

デバイス側では、MCP over MQTT プロトコルを使い、MCP サーバーとして自身のツールや機能を直接公開できます。プラグインはデバイスごとに登録されたツールをツールタイプごとに集約します。MCP ブリッジプラグイン内では、MCP over MQTT プロトコルの Server Name の概念をツールタイプにマッピングしています。

つまり、同じタイプの複数デバイスが登録したツールは、ブリッジプラグインによって単一の論理的なツールとして集約され、MCP クライアントから呼び出せるようになります。

この方式は、スマートホームや産業制御システム、音声対応玩具など、単一または少数のデバイスにクライアントがアクセスするシナリオに適しています。こうした場合、ユーザーは大規模なデバイス群の管理ではなく、自分のデバイスへのアクセスのみを必要とします。

同じタイプの複数デバイスのツールを単一の論理ツールに集約するため、MCP ブリッジプラグインはツール定義に必須パラメータとして `target-mqtt-client-id` を注入します。AI エージェントがツールを呼び出す際は、ビジネスロジックに基づいて対象デバイスの ID を決定し、このパラメータで指定することで、MCP リクエストを特定のデバイスへルーティングできます。

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

## 標準 MQTT を使ったデバイスアクセス

デバイスは MCP over MQTT の代わりに標準 MQTT プロトコルで EMQX に接続することも可能です。この場合、ユーザーは MCP ブリッジプラグイン内に MCP ツールを直接実装し、これらの通常の MQTT デバイスに間接的にアクセスできます。

この方式は、スマートシティ、コネクテッドビークル、産業用 IoT など、より柔軟なデバイスアクセスが求められるシナリオに適しています。MCP ブリッジプラグイン内では、ユーザー定義の外部サービスや API へのアクセス、外部データベースからのデバイス報告データの取得など、任意のビジネスロジックを実装できます。

MCP ツールのコード実装例については、[Create Custom MCP Tools](https://github.com/emqx/emqx_mcp_bridge?tab=readme-ov-file#create-custom-mcp-tools) を参照してください。

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
