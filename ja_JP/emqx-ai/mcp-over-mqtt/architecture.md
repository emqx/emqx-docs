# MCP over MQTT アーキテクチャ

MCP over MQTT は、標準のMCPアーキテクチャ（Host、Client、Server）のコアコンセプトを継承しつつ、トランスポート層として中央集権型のMQTTブローカーを導入しています。ブローカーはメッセージのルーティング、サービス登録と検出、認証および認可を可能にします。

このアーキテクチャは、MCPの元々のコンテキストインタラクションモデルを維持しながら、MQTTの軽量かつ広く適用可能な設計を活用し、IoTやエッジコンピューティングのシナリオにおける多対多通信、ロードバランシング、スケーラビリティの基盤を提供します。

## MQTTトランスポートのコアコンポーネント

MCP over MQTTアーキテクチャでは、メッセージルーターとして中央集権型のMQTTブローカーが導入され、その他のコンポーネント（Host、Client、Server）は標準のMCP設計と一貫しています。

```mermaid
graph LR
    subgraph "アプリケーションホストプロセス"
        H[Host]
        C1[Client 1]
        C2[Client 2]
        C3[Client 3]
        H --> C1
        H --> C2
        H --> C3
    end

    subgraph "MQTTブローカー"
        B[Broker]
        C1 --> B
        C2 --> B
        C3 --> B
    end

    subgraph "サーバー群"
        S1[Server A<br>外部API]
        R1[("リモート<br>リソース A")]
        B --> S1
        S1 <--> R1
    end

    subgraph "サーバー群"
        S2[Server B<br>外部API]
        R2[("リモート<br>リソース B")]
        B --> S2
        S2 <--> R2
    end
```

### Host、Client、および Server

Host、Client、および Server のコンポーネントは変更されていません（詳細は[MCPコアコンセプト](https://modelcontextprotocol.io/docs/learn/architecture#concepts-of-mcp)を参照）：

- **Host** はクライアントのコンテナおよびコーディネーターとして機能します。
- 各 **Client** はHostによって作成され、Serverと独立した接続を維持します。
- **Server** は専用のコンテキストと機能を提供します。

主な違いは、ClientとServerが直接通信するのではなく、MQTTブローカーを介して通信する点です。ブローカーの導入により、ClientとServerの関係は1対1から多対多へと変わります。

### MQTTブローカーの役割

MQTTブローカーは中央集権型のメッセージルーターとして機能します：

- ClientとServer間のメッセージを転送します。
- サービス登録と検出をサポートします（保持メッセージを介して）。
- ClientおよびServerの認証と認可を処理します。

## サーバースケーリングとロードバランシング

スケーラビリティとロードバランシングを実現するために、MCP Serverは複数のインスタンス（プロセス）を起動できます。各インスタンスはMQTTクライアントIDとして一意の`server-id`でブローカーに接続し、すべてのインスタンスは同じ`server-name`を共有します。

**Clientのインタラクションフロー:**

1. Clientはサービス検出トピックをサブスクライブし、対象の`server-name`に属するすべての利用可能な`server-id`を取得します。
2. Clientはカスタムポリシー（例：ランダムまたはラウンドロビン）に基づいてServerインスタンスを選択し、`initialize`リクエストを送信します。
3. 初期化後、Clientは専用のRPCトピックを通じて選択されたServerインスタンスと通信します。

```mermaid
graph LR

    C1["MCP Client1"]
    C2["MCP Client2"]
    C3["MCP Client3"]
    C4["MCP Client4"]

    subgraph "MCPサーバーインスタンス (server-name-a)"
        S1[サーバーインスタンス 1]
        S2[サーバーインスタンス 2]
    end

    C1 <-- "client-1 とサーバーインスタンス 1 の RPCトピック" --> S1
    C2 <-- "client-2 とサーバーインスタンス 1 の RPCトピック" --> S1
    C3 <-- "client-3 とサーバーインスタンス 2 の RPCトピック" --> S2
    C4 <-- "client-4 とサーバーインスタンス 2 の RPCトピック" --> S2

```

この方法により、MCPサーバーの高可用性とスケーラビリティが実現されます：

- **スケールアップ時**、既存のMCPクライアントは古いサーバーインスタンスに接続したままで、新しいクライアントは新たに追加されたインスタンスで初期化できます。
- **スケールダウン時**、MCPクライアントは再初期化して他の利用可能なサーバーインスタンスに接続できます。
