# 快速开始

本指南介绍如何使用 Docker Compose 部署一个基于 EMQX 多媒体服务器的 AI Agent 演示项目，该项目会在浏览器页面上展示一个可实时语音交互的智能玩偶形象。

该项目主要展示以下技术要点：

- **实时语音流传输 + 实时 ASR + LLM + 实时 TTS：** 该项目使用 WebRTC 传输语音流，将前端的语音流实时推送给 ASR 模型流式转为文字，再使用 LLM 流式生成文字回复，最后使用 TTS 将文字回复流式转为语音，EMQX 多媒体服务会使用 WebRTC 流式推送给前端。
- **MCP over MQTT：** 前端使用 [MCP over MQTT TypeScript SDK](../sdks/mcp-sdk-typescript.md) 提供了拍照、表情切换、音量调节等 MCP 工具，AI Agent 无需了解前端（或者设备）的具体实现细节和调用方式，实现了与前端逻辑的解耦。
- **MQTT 实时消息通信：** 前端页面可以通过 MQTT 直接与 AI Agent 通信，以实现拍打、抚摸玩偶头像等交互功能。

## 下载 EMQX 多媒体代理项目代码

```bash
git clone https://github.com/emqx/emqx-multimedia-proxy
cd emqx-multimedia-proxy
```

## 准备环境变量

创建 `docker/.env` 文件，内容如下：

```
DASHSCOPE_API_KEY='sk-xxxxx'
```

其中，`DASHSCOPE_API_KEY` 是您在阿里云大模型服务平台上创建的 API KEY，请参考 [获取阿里云大模型 API Key](https://help.aliyun.com/zh/model-studio/get-api-key?) 了解如何获取该密钥。

## 通过 Docker Compose 启动项目

```bash
make compose-run
```

服务启动后，Demo 页面的默认访问地址为：`http://localhost:4000/index.html`。

## 项目组成部分

上面的 `make compose-run` 命令将会启动三个服务实例：一个多媒体演示项目，一个 EMQX 服务，以及一个 PostgreSQL 数据库实例。

### 多媒体演示项目实例

多媒体演示项目实例是从 `emqx/media-server:latest` 镜像拉取并启动的，其中包含了以下三个组件：
1. EMQX 多媒体服务器：负责处理来自设备的音视频数据，集成阿里云模型平台提供的 ASR（自动语音识别）、TTS（文本转语音）等功能，并与 AI Agent 进行通信。
2. AI Agent：通过 STDIO 或 WebSocket 接受多媒体服务器传递的 ASR 结果（文本），包含了 AI 应用的核心业务逻辑，调用 LLM（大语言模型）处理文本自然语言，并调用多媒体服务提供的 API 将文字转为音频流推送给前端页面。
3. Demo 前端页面：展示语音玩偶形象，使用 WebRTC 技术与多媒体服务器点对点音频交互，提供 MCP 工具实现拍照、表情切换、音量调节等功能。

代码仓库地址：
- [EMQX 多媒体服务器](https://github.com/emqx/emqx-multimedia-proxy)
- [AI Agent 和 Demo 前端页面](https://github.com/emqx/mcp-ai-companion-demo)

### EMQX 服务实例

作为 MQTT 消息中间件，负责多媒体服务器与前端页面之间的消息传递和信令交换。

### PostgreSQL 数据库实例

作为多媒体服务的后端数据库，未来将会用于存储用户数据和会话信息，但演示程序目前尚未使用。

:::tip 提示
为避免访问出现权限问题，请使用 Chrome 浏览器访问 Demo 页面。由于浏览器对 WebRTC 协议的安全策略，默认无法访问非 TLS 站点以及自签名证书的 TLS 站点，所以需要在 chrome://flags/#unsafely-treat-insecure-origin-as-secure 中将 `http://localhost:4000` 和 `https://localhost:443` 添加为安全站点。
:::

:::tip 注意
若要使用拍照功能，需要将多媒体服务器部署在可以公网访问的环境中。这是因为拍照功能的工作过程是，前端首先将图片上传到多媒体服务器，多媒体服务器随后会为视觉模型提供一个该图片文件的下载地址，模型分析该图片并返回结果。所以在本地部署测试的情况下，视觉模型可能无法访问该地址导致图像分析失败。
:::
