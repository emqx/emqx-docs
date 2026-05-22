# MCP ブリッジプラグイン

<<<<<<< HEAD
[EMQX MCP ブリッジプラグイン](https://github.com/emqx/emqx_mcp_bridge)は、EMQXをMCP（Model Context Protocol）対応デバイスと統合するためのプラグインです。このプラグインを使用することで、ユーザーはMCP対応の大規模言語モデルやAIエージェントを使ってIoTデバイスにアクセスし、制御することができます。

## MCP ブリッジプラグインの仕組み

MCP ブリッジプラグインはEMQX内にインストールされて動作します。起動後、Streamable HTTPまたはSSEに基づくMCP接続をMQTTプロトコルに変換するHTTPエンドポイントを公開します。

IoTデバイスはMQTTを使ってEMQXブローカーに接続し、MCP対応の大規模モデルやAIエージェントはMCP ブリッジプラグインが公開するHTTPエンドポイントに接続します。
=======
[EMQX MCP ブリッジプラグイン](https://github.com/emqx/emqx_mcp_bridge) は、EMQX と MCP（Model Context Protocol）対応デバイスを統合するためのプラグインです。このプラグインを使うことで、ユーザーは MCP 対応の大規模言語モデルや AI エージェントを用いて IoT デバイスにアクセスし、制御できます。

## MCP ブリッジプラグインの仕組み

MCP ブリッジプラグインは EMQX 内にインストールされて動作します。起動後、Streamable HTTP または SSE に基づく MCP 接続を MQTT プロトコルに変換する HTTP エンドポイントを公開します。

IoT デバイスは MQTT を使って EMQX ブローカーに接続し、MCP 対応の大規模モデルや AI エージェントは MCP ブリッジプラグインが公開する HTTP エンドポイントに接続します。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
## MCP over MQTTを使ったデバイスへのアクセス

デバイス側では、MCP over MQTTプロトコルを使用し、MCPサーバーとして自身のツールや機能を直接公開できます。プラグインはデバイスが登録したツールをツールタイプごとに集約します。MCP ブリッジプラグインでは、MCP over MQTTプロトコルのServer Nameの概念をツールタイプにマッピングしています。

つまり、同じタイプの複数デバイスが登録したツールは、ブリッジプラグインによって単一の論理的なツールに集約され、MCPクライアントから呼び出せるようになります。

この方式は、スマートホーム、産業制御システム、音声対応玩具など、単一または少数のデバイスにクライアントがアクセスするシナリオに適しています。これらのシナリオでは、ユーザーは大規模なデバイス群を管理するよりも、自分のデバイスにアクセスすることが主な目的です。

同じタイプの複数デバイスのツールが単一の論理ツールに集約されるため、MCP ブリッジプラグインはツール定義に必須パラメータとして`target-mqtt-client-id`を注入します。AIエージェントがツールを呼び出す際には、ビジネスロジックに基づいて対象デバイスIDを判定し、このパラメータで指定する必要があり、これによりMCPリクエストが特定のデバイスにルーティングされます。
=======
## MCP over MQTT を使ったデバイスへのアクセス

デバイス側は MCP over MQTT プロトコルを使用して MCP サーバーとして動作し、自身のツールや機能を直接公開できます。プラグインはデバイスが登録したツールをツールタイプごとに集約します。MCP ブリッジプラグインでは、MCP over MQTT プロトコルの Server Name の概念をツールタイプにマッピングしています。

つまり、同じタイプの複数デバイスが登録したツールは、ブリッジプラグインによって単一の論理ツールとして集約され、MCP クライアントから呼び出せるようになります。

この方式は、スマートホームや産業制御システム、音声対応玩具など、単一または少数のデバイスにアクセスするクライアント向けのシナリオに適しています。これらのシナリオでは、ユーザーは大規模なデバイス群を管理するのではなく、自身のデバイスにのみアクセスすることが一般的です。

同じタイプの複数デバイスのツールを単一の論理ツールに集約するため、MCP ブリッジプラグインはツール定義に必須パラメータとして `target-mqtt-client-id` を注入します。AI エージェントがツールを呼び出す際は、ビジネスロジックに基づいて対象デバイスの ID を特定し、このパラメータで指定する必要があります。これにより MCP リクエストが特定のデバイスにルーティングされます。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
## 標準MQTTを使ったデバイスへのアクセス

デバイスはMCP over MQTTの代わりに標準MQTTプロトコルでEMQXに接続することも可能です。この場合、ユーザーはMCP ブリッジプラグイン内でMCPツールを直接実装し、これらの通常のMQTTデバイスに間接的にアクセスできます。

この方式は、スマートシティ、コネクテッドビークル、産業用IoTなど、より柔軟なデバイスアクセスが求められるシナリオに適しています。MCP ブリッジプラグイン内では、ユーザー定義の外部サービスやAPIへのアクセス、外部データベースからのデバイス報告データの取得など、任意のビジネスロジックを実装可能です。

MCPツールのコード実装例については、[Create Custom MCP Tools](https://github.com/emqx/emqx_mcp_bridge?tab=readme-ov-file#create-custom-mcp-tools)を参照してください。
=======
## 標準 MQTT を使ったデバイスへのアクセス

デバイスは MCP over MQTT ではなく標準 MQTT プロトコルを使って EMQX に接続することも可能です。この場合、ユーザーは MCP ブリッジプラグイン内で MCP ツールを直接実装し、通常の MQTT デバイスに間接的にアクセスできます。

この方式は、スマートシティ、コネクテッドビークル、産業用 IoT など、より柔軟なデバイスアクセスが求められるシナリオに適しています。MCP ブリッジプラグイン内では、ユーザー定義の外部サービスや API へのアクセス、外部データベースからのデバイス報告データの取得など、任意のビジネスロジックを実装可能です。

MCP ツールをコードで実装する方法の例については、[Create Custom MCP Tools](https://github.com/emqx/emqx_mcp_bridge?tab=readme-ov-file#create-custom-mcp-tools) を参照してください。
>>>>>>> origin/release-6.1

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
