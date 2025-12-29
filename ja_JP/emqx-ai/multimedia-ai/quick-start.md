# Quick Start

This guide shows how to deploy an AI Agent demo based on the EMQX Multimedia Server using Docker Compose. The demo runs in a browser and presents an intelligent doll with real-time voice interaction.

The project highlights the following key capabilities:

- **Real-time audio streaming + ASR + LLM + TTS**
   Audio is transmitted via WebRTC. Frontend speech is streamed to an ASR model for real-time transcription, processed by an LLM to generate responses, and converted back to audio via TTS. The EMQX multimedia service streams the audio response back to the browser using WebRTC.
- **MCP over MQTT**
   The frontend uses the [MCP over MQTT TypeScript SDK](../sdks/mcp-sdk-typescript.md) to expose MCP tools such as photo capture, expression switching, and volume control. The AI Agent interacts with these tools without knowing frontend or device implementation details, enabling clean decoupling.
- **MQTT real-time messaging**
   The frontend communicates directly with the AI Agent over MQTT to support interactions such as tapping or stroking the doll avatar.

## Download the EMQX Multimedia Proxy

```bash
git clone https://github.com/emqx/emqx-multimedia-proxy
cd emqx-multimedia-proxy
```

## Configure Environment Variables

Create a `docker/.env` file:

```
DASHSCOPE_API_KEY='sk-xxxxx'
```

`DASHSCOPE_API_KEY` is an API key from the Alibaba Cloud Large Model Service platform. See
 [How to obtain an Alibaba Cloud large model API key](https://help.aliyun.com/zh/model-studio/get-api-key?) for details.

## Start with Docker Compose

```bash
make compose-run
```

After startup, access the demo at: `http://localhost:4000/index.html`.

## Project Architecture

The `make compose-run` command starts three services:

- a multimedia demo application
- an EMQX broker
- a PostgreSQL database

### Multimedia Demo Application

Started from the `emqx/media-server:latest` image, it includes:

1. **EMQX Multimedia Server**
    Handles audio/video data, integrates ASR and TTS from Alibaba Cloud, and communicates with the AI Agent.
2. **AI Agent**
    Receives ASR text via STDIO or WebSocket, contains core AI logic, invokes the LLM, and calls multimedia APIs to stream TTS audio back to the frontend.
3. **Demo Frontend**
    Displays the voice-interactive doll, uses WebRTC for audio communication, and exposes MCP tools for photo capture, expression switching, and volume control.

Related repositories:

- [EMQX Multimedia Server](https://github.com/emqx/emqx-multimedia-proxy)
- [AI Agent and Demo Frontend](https://github.com/emqx/mcp-ai-companion-demo)

### EMQX Service

Acts as the MQTT broker for messaging and signaling between the multimedia server and the frontend.

### PostgreSQL Database

Intended as the backend database for user and session data. It is not used by the current demo.

::: tip

Use Chrome to access the demo. Due to WebRTC security restrictions, non-TLS sites and TLS sites with self-signed certificates are blocked by default. Add `http://localhost:4000` and `https://localhost:443` to `chrome://flags/#unsafely-treat-insecure-origin-as-secure`.

:::

::: tip Note

The photo capture feature requires the multimedia server to be publicly accessible. The frontend uploads images to the server, which then provides a public download URL to the vision model. In local deployments, the vision model may not be able to access this URL, causing image analysis to fail.

:::