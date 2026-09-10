# TypeScript SDK

本文将使用 [@emqx-ai/mcp-mqtt-sdk](https://github.com/emqx/mcp-typescript-sdk) 创建一个 MCP over MQTT 服务端和客户端。该 SDK 支持浏览器和 Node.js 环境，提供完整的 TypeScript 类型安全支持。

本文为了方便快速演示，我们将在 Node.js 环境中运行示例。您也可以轻松地将其集成到浏览器环境中，在 Vue、React 等前端框架中使用。

## 创建演示项目

首先创建一个新的 Node.js 项目（需要 Node.js >= 18）：

```bash
mkdir mcp_typescript_demo
cd mcp_typescript_demo
npm init -y
```

## 安装依赖

安装 TypeScript MCP SDK：

```bash
# 使用 npm
npm install @emqx-ai/mcp-mqtt-sdk
npm install -D typescript @types/node ts-node

# 或使用 yarn
yarn add @emqx-ai/mcp-mqtt-sdk
yarn add -D typescript @types/node ts-node

# 或使用 pnpm
pnpm add @emqx-ai/mcp-mqtt-sdk
pnpm add -D typescript @types/node ts-node
```

## 创建简单的 MCP 服务器

在 `mcp_typescript_demo` 项目中，创建一个暴露计算器工具和资源的简单 MCP 服务器。创建一个名为 `demo_mcp_server.ts` 的文件，并添加以下代码：

```typescript
// demo_mcp_server.ts
import { McpMqttServer } from "@emqx-ai/mcp-mqtt-sdk";

// 创建 MCP 服务器
const server = new McpMqttServer({
  host: "mqtt://broker.emqx.io:1883",
  serverId: "demo-calculator-server",
  serverName: "demo_server/calculator",
  name: "Calculator MCP Server",
  version: "1.0.0",
  description: "一个简单的计算器 MCP 服务器",
  capabilities: {
    tools: { listChanged: true },
    resources: { listChanged: true },
  },
});

// 添加加法工具
server.tool(
  "add",
  "两个数字相加",
  {
    type: "object",
    properties: {
      a: { type: "number", description: "第一个数字" },
      b: { type: "number", description: "第二个数字" },
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

// 添加乘法工具
server.tool(
  "multiply",
  "两个数字相乘",
  {
    type: "object",
    properties: {
      a: { type: "number", description: "第一个数字" },
      b: { type: "number", description: "第二个数字" },
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

// 添加具体的问候资源
const names = ["Alice", "Bob", "Charlie", "Diana", "World"];
names.forEach((name) => {
  server.resource(
    `greeting://${name}`,
    `个性化问候 - ${name}`,
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
      description: `生成给${name}的个性化问候消息`,
      mimeType: "text/plain",
    },
  );
});

// 添加服务器状态资源
server.resource(
  "status://server",
  "服务器状态",
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
    description: "服务器运行状态信息",
    mimeType: "application/json",
  },
);

// 事件处理
server.on("ready", () => {
  console.log("Calculator MCP Server 已启动");
});

server.on("error", (error) => {
  console.error("服务器错误:", error);
});

// 启动服务器
async function startServer() {
  try {
    await server.start();
  } catch (error) {
    console.error("启动服务器失败:", error);
    process.exit(1);
  }
}

// 优雅关闭
process.on("SIGINT", async () => {
  console.log("正在关闭服务器...");
  await server.stop();
  process.exit(0);
});

startServer();
```

## 创建简单的 MCP 客户端

在同一个项目中，创建一个连接到服务器并列出可用工具和资源的简单 MCP 客户端。在项目目录中创建一个名为 `demo_mcp_client.ts` 的文件，并添加以下代码：

```typescript
// demo_mcp_client.ts
import { McpMqttClient } from "@emqx-ai/mcp-mqtt-sdk";

// 创建 MCP 客户端
const client = new McpMqttClient({
  host: "mqtt://broker.emqx.io:1883",
  name: "Demo MCP Client",
  version: "1.0.0",
});

async function onServerDiscovered(server: any) {
  console.log(`发现服务器 ${server.name}，正在连接...`);
  await client.initializeServer(server.serverId);
}

async function onServerConnected(server: any, initResult: any) {
  if (!initResult) {
    console.error(`连接到 ${server.name} 失败`);
    return;
  }

  console.log(`已连接到 ${server.name}`);
  const capabilities = initResult.capabilities;

  // 列出工具
  if (capabilities.tools) {
    try {
      const tools = await client.listTools(server.serverId);
      console.log(
        `${server.name} 的工具:`,
        tools.map((t) => t.name),
      );

      // 测试加法工具
      if (tools.some((t) => t.name === "add")) {
        const result = await client.callTool(server.serverId, "add", {
          a: 1,
          b: 2,
        });
        console.log("调用 add(a=1, b=2) 工具结果:", result.content[0]?.text);
      }

      // 测试乘法工具
      if (tools.some((t) => t.name === "multiply")) {
        const result = await client.callTool(server.serverId, "multiply", {
          a: 3,
          b: 4,
        });
        console.log(
          "调用 multiply(a=3, b=4) 工具结果:",
          result.content[0]?.text,
        );
      }
    } catch (error) {
      console.error("工具操作错误:", error);
    }
  }

  // 列出和读取资源
  if (capabilities.resources) {
    try {
      const resources = await client.listResources(server.serverId);
      console.log(
        `${server.name} 的资源:`,
        resources.map((r) => r.uri),
      );

      // 读取服务器状态
      if (resources.some((r) => r.uri === "status://server")) {
        const status = await client.readResource(
          server.serverId,
          "status://server",
        );
        console.log("服务器状态:", status.contents[0]?.text);
      }

      // 读取动态问候资源
      const greeting = await client.readResource(
        server.serverId,
        "greeting://Alice",
      );
      console.log("问候资源:", greeting.contents[0]?.text);
    } catch (error) {
      console.error("资源操作错误:", error);
    }
  }
}

async function onServerDisconnected(serverId: string) {
  console.log(`与服务器 ${serverId} 断开连接`);
}

// 设置事件处理器
client.on("serverDiscovered", onServerDiscovered);
client.on("serverInitialized", (server) => {
  // 这里我们需要获取初始化结果，简化处理
  onServerConnected(server, { capabilities: { tools: true, resources: true } });
});
client.on("serverDisconnected", onServerDisconnected);
client.on("error", (error) => {
  console.error("客户端错误:", error);
});

// 启动客户端
async function startClient() {
  try {
    await client.connect();
    console.log("Demo MCP Client 已启动");

    // 保持运行
    while (true) {
      // 模拟其他工作，同时 MQTT 传输客户端在后台运行
      await new Promise((resolve) => setTimeout(resolve, 20000));
    }
  } catch (error) {
    console.error("启动客户端失败:", error);
    process.exit(1);
  }
}

// 优雅关闭
process.on("SIGINT", async () => {
  console.log("正在关闭客户端...");
  await client.disconnect();
  process.exit(0);
});

startClient();
```

## 配置项目

由于 SDK 使用 ES 模块，需要配置项目支持现代 JavaScript 模块系统。

在 `package.json` 中添加模块类型和运行脚本：

```json
{
  "type": "module",
  "scripts": {
    "start:server": "ts-node --esm demo_mcp_server.ts",
    "start:client": "ts-node --esm demo_mcp_client.ts"
  }
}
```

创建 `tsconfig.json` 配置文件：

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

## 运行演示

1. 首先运行客户端：

```bash
npm run start:client
```

2. 打开新终端并运行服务器：

```bash
npm run start:server
```

即便客户端先于服务器启动，它也会发现服务器并连接到它。客户端将列出可用的工具，并使用参数 `a=1` 和 `b=2` 调用 `add` 工具，使用参数 `a=3` 和 `b=4` 调用 `multiply` 工具。

通过这个完整的演示，您已经成功创建了一个功能完整的 MCP over MQTT 系统。现在，大模型（如 DeepSeek、Claude、GPT、Gemini 等）就可以通过 MCP 协议发现并调用您暴露的计算器工具，实现与外部服务的无缝集成和智能交互。
