# TypeScript SDK

This guide demonstrates how to use [@emqx-ai/mcp-mqtt-sdk](https://github.com/emqx/mcp-typescript-sdk) to create an MCP over MQTT server and client.
 The SDK supports both browser and Node.js environments and provides complete TypeScript type safety.

For convenience, this tutorial runs the demo in a Node.js environment. However, you can easily integrate it into a browser environment and use it with frameworks like Vue or React.

## Create a Demo Project

First, create a new Node.js project (Node.js >= 18 required):

```bash
mkdir mcp_typescript_demo
cd mcp_typescript_demo
npm init -y
```

## Install Dependencies

Install the TypeScript MCP SDK:

```bash
# Using npm
npm install @emqx-ai/mcp-mqtt-sdk
npm install -D typescript @types/node ts-node

# Or using yarn
yarn add @emqx-ai/mcp-mqtt-sdk
yarn add -D typescript @types/node ts-node

# Or using pnpm
pnpm add @emqx-ai/mcp-mqtt-sdk
pnpm add -D typescript @types/node ts-node
```

## Create a Simple MCP Server

In the `mcp_typescript_demo` project, create a simple MCP server exposing calculator tools and resources.
 Create a file named `demo_mcp_server.ts` and add the following code:

```typescript
// demo_mcp_server.ts
import { McpMqttServer } from "@emqx-ai/mcp-mqtt-sdk";

// Create MCP server
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

// Add addition tool
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

// Add multiplication tool
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

// Add personalized greeting resources
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

// Add server status resource
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

// Event handling
server.on("ready", () => {
  console.log("Calculator MCP Server started");
});

server.on("error", (error) => {
  console.error("Server error:", error);
});

// Start server
async function startServer() {
  try {
    await server.start();
  } catch (error) {
    console.error("Failed to start server:", error);
    process.exit(1);
  }
}

// Graceful shutdown
process.on("SIGINT", async () => {
  console.log("Shutting down server...");
  await server.stop();
  process.exit(0);
});

startServer();
```

## Create a Simple MCP Client

In the same project, create a simple MCP client that connects to the server and lists available tools and resources.
Create a file named `demo_mcp_client.ts` and add the following code:

```typescript
// demo_mcp_client.ts
import { McpMqttClient } from "@emqx-ai/mcp-mqtt-sdk";

// Create MCP client
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

  // List tools
  if (capabilities.tools) {
    try {
      const tools = await client.listTools(server.serverId);
      console.log(
        `${server.name} tools:`,
        tools.map((t) => t.name),
      );

      // Test addition tool
      if (tools.some((t) => t.name === "add")) {
        const result = await client.callTool(server.serverId, "add", {
          a: 1,
          b: 2,
        });
        console.log("Result of add(a=1, b=2):", result.content[0]?.text);
      }

      // Test multiplication tool
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

  // List and read resources
  if (capabilities.resources) {
    try {
      const resources = await client.listResources(server.serverId);
      console.log(
        `${server.name} resources:`,
        resources.map((r) => r.uri),
      );

      // Read server status
      if (resources.some((r) => r.uri === "status://server")) {
        const status = await client.readResource(
          server.serverId,
          "status://server",
        );
        console.log("Server status:", status.contents[0]?.text);
      }

      // Read dynamic greeting resource
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

// Register event handlers
client.on("serverDiscovered", onServerDiscovered);
client.on("serverInitialized", (server) => {
  // For demo purposes, mock the initialization result
  onServerConnected(server, { capabilities: { tools: true, resources: true } });
});
client.on("serverDisconnected", onServerDisconnected);
client.on("error", (error) => {
  console.error("Client error:", error);
});

// Start client
async function startClient() {
  try {
    await client.connect();
    console.log("Demo MCP Client started");

    // Keep running
    while (true) {
      // Simulate other work while MQTT client runs in the background
      await new Promise((resolve) => setTimeout(resolve, 20000));
    }
  } catch (error) {
    console.error("Failed to start client:", error);
    process.exit(1);
  }
}

// Graceful shutdown
process.on("SIGINT", async () => {
  console.log("Shutting down client...");
  await client.disconnect();
  process.exit(0);
});

startClient();
```

## Configure the Project

Since the SDK uses ES modules, configure your project to support modern JavaScript module syntax.

Add module type and scripts to `package.json`:

```json
{
  "type": "module",
  "scripts": {
    "start:server": "ts-node --esm demo_mcp_server.ts",
    "start:client": "ts-node --esm demo_mcp_client.ts"
  }
}
```

Create a `tsconfig.json` file:

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

## Run the Demo

1. Start the client:

```bash
npm run start:client
```

2. Open a new terminal and start the server:

```bash
npm run start:server
```

Even if the client starts before the server, it will discover and connect once the server becomes available.
The client will list available tools, call the `add` tool with parameters `a=1` and `b=2`, and call the `multiply` tool with parameters `a=3` and `b=4`.

## Conclusion

With this end-to-end demo, you’ve successfully created a fully functional MCP over MQTT system. Now, large models such as DeepSeek, Claude, GPT, and Gemini can discover and invoke your exposed calculator tools via the MCP protocol, enabling seamless integration and intelligent interaction with external services.