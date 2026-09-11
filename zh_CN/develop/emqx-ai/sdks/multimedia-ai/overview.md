# 与多媒体服务适配的客户端

多媒体 AI 服务器使用 WebRTC 技术传输音视频流，并且使用 MQTT 协议传输普通消息和 WebRTC 信令消息，实际上客户端无需安装特定的 SDK，任何支持 WebRTC 和 MQTT 协议的客户端均可与多媒体 AI 服务进行交互。常见的客户端包括：

- **Web 浏览器**: 现代浏览器（如 Chrome、Firefox、Edge、Safari）均支持 WebRTC，可以直接通过浏览器访问多媒体 AI 服务。

- **移动应用**: 通过集成 WebRTC SDK（如 [Pion](https://pion.ly)）来实现与多媒体 AI 服务的交互。

- **嵌入式设备**: 物联网设备可以通过集成与设备适配的 WebRTC 库来实现与多媒体 AI 服务的连接，如 [ESP32 WebRTC](https://github.com/espressif/esp-webrtc-solution)。

具体的 WebRTC 信令和 MQTT 消息格式请参考[多媒体 AI 消息协议](../../multimedia-ai/message-protocol.md)。这里我们提供了基于 Web 浏览器的客户端代码示例，演示如何与多媒体服务进行交互：[Typescript WebRTC 示例](./webrtc-typescript.md)。
