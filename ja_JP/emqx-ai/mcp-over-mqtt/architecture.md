# MCP over MQTT アーキテクチャ

MCP over MQTT は、標準のMCPアーキテクチャ（Host、Client、Server）のコアコンセプトを継承しつつ、トランスポート層として中央集権型のMQTTブローカーを導入しています。ブローカーはメッセージのルーティング、サービス登録および検出、認証、認可を可能にします。

このアーキテクチャは、MCPの元々のコンテキスト相互作用モデルを保持しながら、MQTTの軽量かつ広範に適用可能な設計を活用し、IoTやエッジコンピューティングのシナリオにおける多対多通信、ロードバランシング、スケーラビリティの基盤を提供します。

## MQTTトランスポートのコアコンポーネント

MCP over MQTTアーキテクチャでは、中央集権型のMQTTブローカーがメッセージルーターとして導入されており、他のコンポーネント（Host、Client、Server）は標準のMCP設計と一貫しています。

```mermaid
graph LR
    subgraph "Application Host Process"
        H[Host]
        C1[Client 1]
        C2[Client 2]
        C3[Client 3]
        H --> C1
        H --> C2
        H --> C3
    end

    subgraph "MQTT Broker"
        B[Broker]
        C1 --> B
        C2 --> B
        C3 --> B
    end

    subgraph "Servers"
        S1[Server A<br>External APIs]
        R1[("Remote<br>Resource A")]
        B --> S1
        S1 <--> R1
    end

    subgraph "Servers"
        S2[Server B<br>External APIs]
        R2[("Remote<br>Resource B")]
        B --> S2
        S2 <--> R2
    end
```

### Host、Client、および Server

Host、Client、およびServerのコンポーネントは変更されていません（詳細は[MCPコアコンセプト](https://modelcontextprotocol.io/docs/learn/architecture#concepts-of-mcp)を参照）：

- **Host** はクライアントのコンテナおよびコーディネーターとして機能します。
- 各 **Client** はHostによって作成され、Serverと独立した接続を維持します。
- **Server** は専用のコンテキストと機能を提供します。

主な違いは、ClientとServerが直接通信するのではなく、MQTTブローカーを介して通信する点です。ブローカーの存在により、ClientとServer間の関係は1対1から多対多に変わります。

### MQTTブローカーの役割

MQTTブローカーは中央集権型のメッセージルーターとして機能します：

- ClientとServer間のメッセージを転送します。
- サービス登録および検出（リテインドメッセージを介して）をサポートします。
- ClientおよびServerの認証と認可を処理します。

## Serverのスケーリングとロードバランシング

スケーラビリティとロードバランシングを実現するために、MCP Serverは複数のインスタンス（プロセス）を起動できます。各インスタンスはユニークな `server-id` をMQTTクライアントIDとしてブローカーに接続し、すべてのインスタンスは同じ `server-name` を共有します。

**Clientの相互作用フロー：**

1. Clientはサービス検出トピックをサブスクライブし、対象の `server-name` 配下のすべての利用可能な `server-id` を取得します。
2. Clientはカスタムポリシー（ランダムやラウンドロビンなど）に基づいてServerインスタンスを選択し、`initialize` リクエストを送信します。
3. 初期化後、Clientは専用のRPCトピックを通じて選択されたServerインスタンスと通信します。

```mermaid
graph LR

    C1["MCP Client1"]
    C2["MCP Client2"]
    C3["MCP Client3"]
    C4["MCP Client4"]

    subgraph "MCP Server Instances (server-name-a)"
        S1[Server Instance 1]
        S2[Server Instance 2]
    end

    C1 <-- "RPC topic of client-1 and server instance 1" --> S1
    C2 <-- "RPC topic of client-2 and server instance 1" --> S1
    C3 <-- "RPC topic of client-3 and server instance 2" --> S2
    C4 <-- "RPC topic of client-4 and server instance 2" --> S2

```

このアプローチにより、MCPサーバーの高可用性とスケーラビリティが実現されます：

- **スケールアップ時**、既存のMCPクライアントは旧サーバーインスタンスに接続したままで、新規クライアントは新たに追加されたインスタンスで初期化できます。
- **スケールダウン時**、MCPクライアントは再初期化して他の利用可能なサーバーインスタンスに接続できます。
