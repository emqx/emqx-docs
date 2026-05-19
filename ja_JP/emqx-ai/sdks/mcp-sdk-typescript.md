# TypeScript SDK

このガイドでは、[@emqx-ai/mcp-mqtt-sdk](https://github.com/emqx/mcp-typescript-sdk) を使用して、MQTT 上の MCP サーバーとクライアントを作成する方法を説明します。  
この SDK はブラウザと Node.js の両方の環境をサポートし、完全な TypeScript の型安全性を提供します。

便宜上、このチュートリアルでは Node.js 環境でデモを実行しますが、ブラウザ環境に簡単に統合でき、Vue や React などのフレームワークでも利用可能です。

## デモプロジェクトの作成

まず、Node.js プロジェクトを新規作成します（Node.js 18 以上が必要です）:

```bash
mkdir mcp_typescript_demo
cd mcp_typescript_demo
npm init -y
```

## 依存関係のインストール

TypeScript 用 MCP SDK をインストールします:

```bash
# npm を使用する場合
npm install @emqx-ai/mcp-mqtt-sdk
npm install -D typescript @types/node ts-node

# または yarn を使用する場合
yarn add @emqx-ai/mcp-mqtt-sdk
yarn add -D typescript @types/node ts-node

# または pnpm を使用する場合
pnpm add @emqx-ai/mcp-mqtt-sdk
pnpm add -D typescript @types/node ts-node
```

## シンプルな MCP サーバーの作成

`mcp_typescript_demo` プロジェクト内に、計算ツールとリソースを公開するシンプルな MCP サーバーを作成します。  
`demo_mcp_server.ts` というファイルを作成し、以下のコードを追加してください。

```typescript
// demo_mcp_server.ts
import { McpMqttServer } from "@emqx-ai/mcp-mqtt-sdk";

// MCP サーバーの作成
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

// 足し算ツールの追加
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

// 掛け算ツールの追加
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

// パーソナライズされた挨拶リソースの追加
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

// サーバーステータスリソースの追加
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

## シンプルな MCP クライアントの作成

同じプロジェクト内に、サーバーに接続して利用可能なツールとリソースを一覧表示するシンプルな MCP クライアントを作成します。  
`demo_mcp_client.ts` というファイルを作成し、以下のコードを追加してください。

```typescript
// demo_mcp_client.ts
import { McpMqttClient } from "@emqx-ai/mcp-mqtt-sdk";

// MCP クライアントの作成
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

  // ツールの一覧表示
  if (capabilities.tools) {
    try {
      const tools = await client.listTools(server.serverId);
      console.log(
        `${server.name} tools:`,
        tools.map((t) => t.name),
      );

      // 足し算ツールのテスト
      if (tools.some((t) => t.name === "add")) {
        const result = await client.callTool(server.serverId, "add", {
          a: 1,
          b: 2,
        });
        console.log("Result of add(a=1, b=2):", result.content[0]?.text);
      }

      // 掛け算ツールのテスト
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

  // リソースの一覧表示と読み取り
  if (capabilities.resources) {
    try {
      const resources = await client.listResources(server.serverId);
      console.log(
        `${server.name} resources:`,
        resources.map((r) => r.uri),
      );

      // サーバーステータスの読み取り
      if (resources.some((r) => r.uri === "status://server")) {
        const status = await client.readResource(
          server.serverId,
          "status://server",
        );
        console.log("Server status:", status.contents[0]?.text);
      }

      // 動的な挨拶リソースの読み取り
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

// イベントハンドラの登録
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
      // MQTT クライアントがバックグラウンドで動作する間に他の処理をシミュレート
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

SDK は ES モジュールを使用しているため、モダンな JavaScript モジュール構文をサポートするようにプロジェクトを設定します。

`package.json` にモジュールタイプとスクリプトを追加します:

```json
{
  "type": "module",
  "scripts": {
    "start:server": "ts-node --esm demo_mcp_server.ts",
    "start:client": "ts-node --esm demo_mcp_client.ts"
  }
}
```

`tsconfig.json` ファイルを作成します:

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

1. クライアントを起動します:

```bash
npm run start:client
```

2. 新しいターミナルを開き、サーバーを起動します:

```bash
npm run start:server
```

クライアントがサーバーより先に起動しても、サーバーが利用可能になると検出して接続します。  
クライアントは利用可能なツールを一覧表示し、`add` ツールにパラメータ `a=1` と `b=2` を渡して呼び出し、`multiply` ツールに `a=3` と `b=4` を渡して呼び出します。

## まとめ

このエンドツーエンドのデモにより、MQTT 上の MCP システムを完全に機能する形で作成できました。  
これにより、DeepSeek、Claude、GPT、Gemini などの大規模モデルが MCP プロトコルを介して公開された計算ツールを検出・呼び出しでき、外部サービスとのシームレスな統合とインテリジェントな連携が可能になります。
