# TypeScript SDK

このガイドでは、[@emqx-ai/mcp-mqtt-sdk](https://github.com/emqx/mcp-typescript-sdk) を使用して、MQTT上のMCPサーバーとクライアントを作成する方法を説明します。  
<<<<<<< HEAD
このSDKはブラウザとNode.js環境の両方をサポートし、完全なTypeScriptの型安全性を提供します。

便宜上、このチュートリアルではNode.js環境でデモを実行しますが、VueやReactなどのフレームワークと統合してブラウザ環境で使用することも簡単にできます。

## デモプロジェクトの作成

まず、新しいNode.jsプロジェクトを作成します（Node.js 18以上が必要です）:
=======
このSDKはブラウザおよびNode.js環境の両方をサポートし、完全なTypeScriptの型安全性を提供します。

便宜上、このチュートリアルではNode.js環境でデモを実行しますが、ブラウザ環境に簡単に統合でき、VueやReactなどのフレームワークと共に使用することも可能です。

## デモプロジェクトの作成

まず、新しいNode.jsプロジェクトを作成します（Node.js 18以上が必要です）：
>>>>>>> origin/release-6.1

```bash
mkdir mcp_typescript_demo
cd mcp_typescript_demo
npm init -y
```

## 依存関係のインストール

<<<<<<< HEAD
TypeScript用MCP SDKをインストールします:
=======
TypeScript用MCP SDKをインストールします：
>>>>>>> origin/release-6.1

```bash
# npmを使用する場合
npm install @emqx-ai/mcp-mqtt-sdk
npm install -D typescript @types/node ts-node

<<<<<<< HEAD
# またはyarnを使用する場合
yarn add @emqx-ai/mcp-mqtt-sdk
yarn add -D typescript @types/node ts-node

# またはpnpmを使用する場合
=======
# yarnを使用する場合
yarn add @emqx-ai/mcp-mqtt-sdk
yarn add -D typescript @types/node ts-node

# pnpmを使用する場合
>>>>>>> origin/release-6.1
pnpm add @emqx-ai/mcp-mqtt-sdk
pnpm add -D typescript @types/node ts-node
```

## シンプルなMCPサーバーの作成

<<<<<<< HEAD
`mcp_typescript_demo`プロジェクト内で、計算ツールとリソースを公開するシンプルなMCPサーバーを作成します。  
=======
`mcp_typescript_demo`プロジェクト内に、計算機ツールとリソースを公開するシンプルなMCPサーバーを作成します。  
>>>>>>> origin/release-6.1
`demo_mcp_server.ts`というファイルを作成し、以下のコードを追加してください。

```typescript
// demo_mcp_server.ts
import { McpMqttServer } from "@emqx-ai/mcp-mqtt-sdk";

<<<<<<< HEAD
// MCPサーバーを作成
=======
// MCPサーバーの作成
>>>>>>> origin/release-6.1
const server = new McpMqttServer({
  host: "mqtt://broker.emqx.io:1883",
  serverId: "demo-calculator-server",
  serverName: "demo_server/calculator",
  name: "Calculator MCP Server",
  version: "1.0.0",
  description: "A simple calculator MCP server",
  capabilities: {
    tools: { listChanged: true },
    resources: { listChanged: true },
  },
});

<<<<<<< HEAD
// 加算ツールを追加
=======
// 足し算ツールの追加
>>>>>>> origin/release-6.1
server.tool(
  "add",
  "Add two numbers",
  {
    type: "object",
    properties: {
      a: { type: "number", description: "The first number" },
      b: { type: "number", description: "The second number" },
    },
    required: ["a", "b"],
  },
  async (params: Record<string, any>) => {
    const { a, b } = params as { a: number; b: number };
    const result = a + b;
    return {
      content: [
        {
          type: "text",
          text: `${a} + ${b} = ${result}`,
        },
      ],
    };
  },
);

<<<<<<< HEAD
// 乗算ツールを追加
=======
// 掛け算ツールの追加
>>>>>>> origin/release-6.1
server.tool(
  "multiply",
  "Multiply two numbers",
  {
    type: "object",
    properties: {
      a: { type: "number", description: "The first number" },
      b: { type: "number", description: "The second number" },
    },
    required: ["a", "b"],
  },
  async (params: Record<string, any>) => {
    const { a, b } = params as { a: number; b: number };
    const result = a * b;
    return {
      content: [
        {
          type: "text",
          text: `${a} × ${b} = ${result}`,
        },
      ],
    };
  },
);

<<<<<<< HEAD
// パーソナライズされた挨拶リソースを追加
=======
// パーソナライズされた挨拶リソースの追加
>>>>>>> origin/release-6.1
const names = ["Alice", "Bob", "Charlie", "Diana", "World"];
names.forEach((name) => {
  server.resource(
    `greeting://${name}`,
    `Personalized greeting - ${name}`,
    async () => {
      return {
        contents: [
          {
            uri: `greeting://${name}`,
            mimeType: "text/plain",
            text: `Hello, ${name}! Welcome to our calculator service.`,
          },
        ],
      };
    },
    {
      description: `Generate a personalized greeting message for ${name}`,
      mimeType: "text/plain",
    },
  );
});

<<<<<<< HEAD
// サーバーステータスリソースを追加
=======
// サーバーステータスリソースの追加
>>>>>>> origin/release-6.1
server.resource(
  "status://server",
  "Server status",
  async () => {
    return {
      contents: [
        {
          uri: "status://server",
          mimeType: "application/json",
          text: JSON.stringify(
            {
              name: "Calculator MCP Server",
              status: "running",
              uptime: process.uptime(),
              availableTools: ["add", "multiply"],
              timestamp: new Date().toISOString(),
            },
            null,
            2,
          ),
        },
      ],
    };
  },
  {
    description: "Server runtime status information",
    mimeType: "application/json",
  },
);

// イベントハンドリング
server.on("ready", () => {
  console.log("Calculator MCP Server started");
});

server.on("error", (error) => {
  console.error("Server error:", error);
});

// サーバー起動
async function startServer() {
  try {
    await server.start();
  } catch (error) {
    console.error("Failed to start server:", error);
    process.exit(1);
  }
}

// グレースフルシャットダウン
process.on("SIGINT", async () => {
  console.log("Shutting down server...");
  await server.stop();
  process.exit(0);
});

startServer();
```

## シンプルなMCPクライアントの作成

<<<<<<< HEAD
同じプロジェクト内で、サーバーに接続し利用可能なツールとリソースを一覧表示するシンプルなMCPクライアントを作成します。  
=======
同じプロジェクト内に、サーバーに接続して利用可能なツールとリソースを一覧表示するシンプルなMCPクライアントを作成します。  
>>>>>>> origin/release-6.1
`demo_mcp_client.ts`というファイルを作成し、以下のコードを追加してください。

```typescript
// demo_mcp_client.ts
import { McpMqttClient } from "@emqx-ai/mcp-mqtt-sdk";

<<<<<<< HEAD
// MCPクライアントを作成
=======
// MCPクライアントの作成
>>>>>>> origin/release-6.1
const client = new McpMqttClient({
  host: "mqtt://broker.emqx.io:1883",
  name: "Demo MCP Client",
  version: "1.0.0",
});

async function onServerDiscovered(server: any) {
  console.log(`Discovered server ${server.name}, connecting...`);
  await client.initializeServer(server.serverId);
}

async function onServerConnected(server: any, initResult: any) {
  if (!initResult) {
    console.error(`Failed to connect to ${server.name}`);
    return;
  }

  console.log(`Connected to ${server.name}`);
  const capabilities = initResult.capabilities;

<<<<<<< HEAD
  // ツールを一覧表示
=======
  // ツールの一覧表示
>>>>>>> origin/release-6.1
  if (capabilities.tools) {
    try {
      const tools = await client.listTools(server.serverId);
      console.log(
        `${server.name} tools:`,
        tools.map((t) => t.name),
      );

<<<<<<< HEAD
      // 加算ツールをテスト
=======
      // 足し算ツールのテスト
>>>>>>> origin/release-6.1
      if (tools.some((t) => t.name === "add")) {
        const result = await client.callTool(server.serverId, "add", {
          a: 1,
          b: 2,
        });
        console.log("Result of add(a=1, b=2):", result.content[0]?.text);
      }

<<<<<<< HEAD
      // 乗算ツールをテスト
=======
      // 掛け算ツールのテスト
>>>>>>> origin/release-6.1
      if (tools.some((t) => t.name === "multiply")) {
        const result = await client.callTool(server.serverId, "multiply", {
          a: 3,
          b: 4,
        });
        console.log("Result of multiply(a=3, b=4):", result.content[0]?.text);
      }
    } catch (error) {
      console.error("Tool operation error:", error);
    }
  }

<<<<<<< HEAD
  // リソースを一覧表示および読み取り
=======
  // リソースの一覧表示と読み取り
>>>>>>> origin/release-6.1
  if (capabilities.resources) {
    try {
      const resources = await client.listResources(server.serverId);
      console.log(
        `${server.name} resources:`,
        resources.map((r) => r.uri),
      );

<<<<<<< HEAD
      // サーバーステータスを読み取る
=======
      // サーバーステータスの読み取り
>>>>>>> origin/release-6.1
      if (resources.some((r) => r.uri === "status://server")) {
        const status = await client.readResource(
          server.serverId,
          "status://server",
        );
        console.log("Server status:", status.contents[0]?.text);
      }

<<<<<<< HEAD
      // 動的な挨拶リソースを読み取る
=======
      // 動的な挨拶リソースの読み取り
>>>>>>> origin/release-6.1
      const greeting = await client.readResource(
        server.serverId,
        "greeting://Alice",
      );
      console.log("Greeting resource:", greeting.contents[0]?.text);
    } catch (error) {
      console.error("Resource operation error:", error);
    }
  }
}

async function onServerDisconnected(serverId: string) {
  console.log(`Disconnected from server ${serverId}`);
}

<<<<<<< HEAD
// イベントハンドラを登録
=======
// イベントハンドラの登録
>>>>>>> origin/release-6.1
client.on("serverDiscovered", onServerDiscovered);
client.on("serverInitialized", (server) => {
  // デモ用に初期化結果をモック
  onServerConnected(server, { capabilities: { tools: true, resources: true } });
});
client.on("serverDisconnected", onServerDisconnected);
client.on("error", (error) => {
  console.error("Client error:", error);
});

// クライアント起動
async function startClient() {
  try {
    await client.connect();
    console.log("Demo MCP Client started");

    // 実行を継続
    while (true) {
      // MQTTクライアントがバックグラウンドで動作している間に他の処理をシミュレート
      await new Promise((resolve) => setTimeout(resolve, 20000));
    }
  } catch (error) {
    console.error("Failed to start client:", error);
    process.exit(1);
  }
}

// グレースフルシャットダウン
process.on("SIGINT", async () => {
  console.log("Shutting down client...");
  await client.disconnect();
  process.exit(0);
});

startClient();
```

## プロジェクトの設定

SDKはESモジュールを使用しているため、プロジェクトをモダンなJavaScriptモジュール構文に対応させる必要があります。

<<<<<<< HEAD
`package.json`にモジュールタイプとスクリプトを追加します:
=======
`package.json`にモジュールタイプとスクリプトを追加します：
>>>>>>> origin/release-6.1

```json
{
  "type": "module",
  "scripts": {
    "start:server": "ts-node --esm demo_mcp_server.ts",
    "start:client": "ts-node --esm demo_mcp_client.ts"
  }
}
```

<<<<<<< HEAD
`tsconfig.json`ファイルを作成します:
=======
`tsconfig.json`ファイルを作成します：
>>>>>>> origin/release-6.1

```json
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "ESNext",
    "moduleResolution": "node",
    "esModuleInterop": true,
    "allowSyntheticDefaultImports": true,
    "strict": false,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true
  },
  "ts-node": {
    "esm": true
  }
}
```

## デモの実行

<<<<<<< HEAD
1. クライアントを起動します:
=======
1. クライアントを起動します：
>>>>>>> origin/release-6.1

```bash
npm run start:client
```

<<<<<<< HEAD
2. 新しいターミナルを開き、サーバーを起動します:
=======
2. 新しいターミナルを開き、サーバーを起動します：
>>>>>>> origin/release-6.1

```bash
npm run start:server
```

クライアントがサーバーより先に起動しても、サーバーが利用可能になると自動的に検出して接続します。  
<<<<<<< HEAD
クライアントは利用可能なツールを一覧表示し、`add`ツールをパラメータ `a=1`、`b=2` で呼び出し、`multiply`ツールを `a=3`、`b=4` で呼び出します。

## まとめ

このエンドツーエンドのデモにより、MQTT上の完全なMCPシステムを作成できました。  
これにより、DeepSeek、Claude、GPT、Geminiなどの大規模モデルがMCPプロトコルを介して公開された計算ツールを検出・呼び出しできるようになり、外部サービスとのシームレスな統合とインテリジェントな連携が可能になります。
=======
クライアントは利用可能なツールを一覧表示し、パラメータ `a=1`、`b=2` で `add` ツールを呼び出し、`a=3`、`b=4` で `multiply` ツールを呼び出します。

## まとめ

このエンドツーエンドのデモにより、MQTT上の完全な機能を持つMCPシステムを作成できました。  
これにより、DeepSeek、Claude、GPT、Geminiなどの大規模モデルがMCPプロトコルを介して公開した計算機ツールを検出・呼び出しでき、外部サービスとのシームレスな統合とインテリジェントな連携が可能になります。
>>>>>>> origin/release-6.1
