# Quick Start: Building an AI Agent with EMQ + Volcengine Voice Services

This document describes how to quickly deploy an AI agent demo system with voice interaction and device control using Docker Compose. The project simulates smart device capabilities (camera, expressions, volume control, etc.) through a PC browser, demonstrating how the MCP over MQTT protocol enables real-time AI Agent control of devices. The system integrates Volcengine RTC for voice channels, ASR/TTS for speech recognition and synthesis, and CustomLLM mode to connect to custom AI Agent services for multi-turn conversations and tool invocations.

## Architecture Overview

The system consists of three core components:

### Component Overview

| Component | Role | Port | Main Functions |
|------|------|------|----------|
| **web** | MCP Server | 8080 | Frontend UI, exposes hardware control tools (camera/expressions/volume) |
| **app** | MCP Client + AI Agent | 8081 | Provides `/chat-stream` endpoint, handles LLM/VLM inference and MCP tool invocations |
| **volc-server** | Volcengine Proxy | 3002 | Manages RTC rooms/tokens, configures CustomLLM address for Volcengine services to request app |

### Communication Flow

```text
1. Web UI → volc-server: Request scene configuration and RTC credentials
2. Web UI ↔ Volcengine RTC: Establish real-time audio/video connection (ASR/TTS)
3. Volcengine → app: CustomLLM callback to /chat-stream (SSE streaming response)
4. app ↔ Web UI: Invoke MCP tools via MQTT (camera/expressions, etc.)
5. Volcengine → Web UI: TTS synthesized voice playback
```

**Core Capabilities**:

- **MCP over MQTT Protocol**: Enables AI Agent cross-network tool invocation via EMQX Broker (camera, expressions, volume control)
- **Multimodal Understanding**: Integrates VLM vision models, supports visual scenarios like "what am I holding"
- **Real-time Voice Interaction**: Based on Volcengine RTC + ASR/TTS, end-to-end speech recognition and synthesis with low latency
- **Parallel Processing Architecture**: Tool invocation and voice synthesis execute asynchronously for smooth user experience
