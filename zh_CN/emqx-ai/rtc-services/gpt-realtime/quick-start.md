# 使用 EMQX + GPT-Realtime 构建实时语音智能体

本指南介绍如何使用 GPT-Realtime 模型和 EMQX 快速构建一个实时语音智能体应用。

## 获取临时 API Key

要在浏览器中使用原生 WebRTC 的方式连接 GPT-Realtime，我们需要先获取一个临时的 API Key。我们可以通过 OpenAI 的 REST API 获取该 Key：

```bash
export OPENAI_API_KEY="sk-xxxxxx"
curl -s -X POST https://api.openai.com/v1/realtime/client_secrets -H "Authorization: Bearer $OPENAI_API_KEY" -H "Content-Type: application/json" -d '{"session": {"type": "realtime", "model": "gpt-realtime"}}' | jq .value
```

## 实现实时语音聊天

以下代码展示了如何使用原生 WebRTC 的方式连接 GPT-Realtime 模型，实现实时语音聊天功能：

```javascript
// Put the obtained ephemeral key here
const EPHEMERAL_KEY = "ek_xxxxxx";

// Create a peer connection
const pc = new RTCPeerConnection();

// Set up to play remote audio from the model
audioElement.current = document.createElement("audio");
audioElement.current.autoplay = true;
pc.ontrack = (e) => (audioElement.current.srcObject = e.streams[0]);

// Add local audio track for microphone input in the browser
const ms = await navigator.mediaDevices.getUserMedia({
    audio: true,
});
pc.addTrack(ms.getTracks()[0]);

// Set up data channel for sending and receiving events
const dc = pc.createDataChannel("oai-events");

// Start the session using the Session Description Protocol (SDP)
const offer = await pc.createOffer();
await pc.setLocalDescription(offer);

const sdpResponse = await fetch("https://api.openai.com/v1/realtime/calls", {
    method: "POST",
    body: offer.sdp,
    headers: {
        Authorization: `Bearer ${EPHEMERAL_KEY}`,
        "Content-Type": "application/sdp",
    },
});

const answer = {
    type: "answer",
    sdp: await sdpResponse.text(),
};
await pc.setRemoteDescription(answer);

// Listen for server events
dc.addEventListener("message", (e) => {
    const event = JSON.parse(e.data);
    console.log("Received event:", event);
});
```

上述代码除了创建 WebRTC 语音通道之外，还创建了一个 Data Channel 用于发送和接收 GPT-Realtime 模型的事件。这里我们将所有接收到的事件打印到了控制台，如果测试过程中出现收不到语音等问题，可以在控制台查看具体的错误信息。

## 使用 MCP 控制设备

### 1. 安装、配置 MCP 桥接插件，并启动 MCP Server 以模拟设备

首先我们需要启动 EMQX 并安装 MCP 桥接插件，并启动一个 MCP Server 来模拟智能电灯。具体步骤请参考[使用 EMQX MCP 桥接访问物联网设备](../../mcp-bridge/quick-start.md)。

注意必须在公网环境中启动 EMQX，并且给 MCP 桥接插件配置有效的 SSL 证书，以便 GPT-Realtime 可以通过 HTTPS 访问 MCP 服务。

### 2. 修改前端代码以使用 MCP 工具

为了使用 MCP 工具，我们需要增加一个处理 GPT-Realtime 事件的函数 `handle_event()`。

```javascript
// Listen for server events
dc.addEventListener("message", (e) => {
    const event = JSON.parse(e.data);
    handle_event(event);
});
```

在该函数中，我们需要处理 session.created 事件，以便在会话创建时发送 session.update 事件来启用 MCP 工具。MCP 服务器的地址填写 `https://your-emqx-host:port/mcp`：

```javascript
function handle_event(event) {
    if (event.type === "session.created") {
        // Send client events
        const session_update_event = {
            type: "session.update",
            session: {
                type: "realtime",
                model: "gpt-realtime",
                // can be set to "text"
                output_modalities: ["audio"],
                tools: [
                    {
                        type: "mcp",
                        server_label: "mqtt_mcp_bridge",
                        server_description: "EMQX MCP over MQTT Bridge",
                        server_url: "https://your-emqx-host:port/mcp",
                        require_approval: "never",
                    }
                ],
                tool_choice: "auto",
                // You can still set direct session fields; these override prompt fields if they overlap:
                instructions: "I have a smart light and its client ID is abc123"
            }
        };
        dc.send(JSON.stringify(session_update_event));
    } else if (event.type === "response.done") {
        console.log("Received response done:", event);
    } else {
        console.log("Received event:", event);
    }
}
```

现在，我们在浏览器中访问前端页面与 GPT-Realtime 进行语音对话时，模型就可以通过 MCP 工具访问和控制物联网设备了。

::: tip 注意
GPT-Realtime 仅支持通过 HTTPS 访问 MCP 服务器，请确保：
- MCP 插件安装了有效的、非自签名的 SSL 证书
- URL 必须使用域名而不是 IP 地址，并且确保可以公网访问。
:::

::: tip 注意
GPT-Realtime 需要使用 Streamable HTTP 来访问 MCP 服务器，所以要访问 EMQX MCP 桥接插件的 `/mcp` 路径，而不是 `/sse` 路径。
:::

## 给模型发送消息

前面的代码中，我们使用系统提示词提前告诉了模型我们设备的 Client ID：

```javascript
const session_update_event = {
    type: "session.update",
    session: {
        ...
        instructions: "I have a smart light and its client ID is abc123"
    }
};
```

GPT-Realtime 还支持在对话过程中，通过 WebRTC Data Channel 给模型发送消息，给模型添加上下文信息：

```javascript
// Send client events
const event = {
    type: "conversation.item.create",
    item: {
        type: "message",
        role: "user",
        content: [
            {
                type: "input_text",
                text: "I have a smart light and its client ID is abc123",
            },
        ],
    },
};
dc.send(JSON.stringify(event));
```
