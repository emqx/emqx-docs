# MQTT over WebSocket

EMQX 企业版通过内置的 WebSocket 监听器原生支持 MQTT over WebSocket。这使得 MQTT 客户端可以通过 WebSocket 与 MQTT Broker（消息代理服务器） 进行通信，而无需使用原始的 TCP 或 TLS 连接。

MQTT over WebSocket 在功能上与 MQTT over TCP/TLS 完全一致。唯一的区别在于传输层：MQTT 报文被封装在 WebSocket 帧中，并通过 HTTP 或 HTTPS 进行传输。

使用 MQTT over WebSocket 的客户端可以与通过 TCP、TLS 或 QUIC 连接到同一 broker 或集群的其他 MQTT 客户端实现无缝互操作。由于 MQTT 协议本身保持不变，不同传输方式连接的客户端共享相同的主题命名空间和消息路由行为。

该能力在无法建立直接 TCP 连接或 TCP 连接受限的环境中尤为有用，例如 Web 浏览器以及某些企业网络环境。

## 协议栈

使用 MQTT over WebSocket 时，其协议栈如下：

```
MQTT
WebSocket
HTTP / HTTPS
TCP
IP
```

MQTT 协议本身保持不变。所有 MQTT 控制报文（CONNECT、PUBLISH、SUBSCRIBE 等）均以原始形式封装在 WebSocket 帧中进行传输。

## 功能特性

EMQX 企业版中的 MQTT over WebSocket 提供以下特性：

- 完全兼容 MQTT v3.1、v3.1.1 和 v5.0
- 支持 QoS 0、1 和 2
- 支持保留消息和遗嘱消息
- 支持持久会话和离线消息
- 支持共享订阅
- 支持认证与授权机制（Username/Password、JWT、OAuth 等）
- 通过 WSS 提供 TLS 加密

所有 MQTT 语义和 broker 端处理逻辑均与标准 TCP 或 TLS 连接保持一致。

## 配置 WebSocket 监听器

使用 MQTT over WebSocket 需要启用 WebSocket 监听器。

EMQX 企业版提供以下两种监听器：

- WebSocket 监听器（WS）
- 安全 WebSocket 监听器（WSS）

可以通过以下方式配置这些监听器：

- EMQX Dashboard：**管理** -> **监听器**
- 配置文件
- REST API

有关详细的配置说明，请参阅：

- [配置 WebSocket 监听器](../../guides/configuration/listener.md#配置-websocket-监听器)
- [配置安全 WebSocket 监听器](../../guides/configuration/listener.md#配置安全-websocket-监听器)

## 快速开始

要使用 MQTT over WebSocket：

1. 启用 WebSocket（WS）或安全 WebSocket（WSS）监听器。
2. 使用 `ws://` 或 `wss://` 连接到对应的客户端接入端点。
3. 使用支持 MQTT over WebSocket 的 MQTT 客户端库。
4. 将客户端配置为使用 WebSocket 传输方式。

从客户端角度来看，MQTT 的行为保持不变。客户端发送的仍然是标准的 MQTT 控制报文，这些报文会被透明地封装在 WebSocket 帧中进行传输。

在基于浏览器的应用场景中，MQTT over WebSocket 通常与 JavaScript MQTT 客户端库一起使用。

有关分步骤示例，请参阅 [使用 JavaScript SDK 连接](./javascript.md)。

### 示例：浏览器客户端

以下示例演示了浏览器客户端如何通过安全 WebSocket 连接发布和订阅 MQTT 主题：

```javascript
import mqtt from "mqtt";

const client = mqtt.connect("wss://broker.example.com:8084/mqtt", {
  clientId: "web-client-1",
  username: "username",
  password: "password"
});

client.on("connect", () => {
  console.log("Connected over WebSockets");

  client.subscribe("test/topic", () => {
    client.publish("test/topic", "Hello from browser");
  });
});

client.on("message", (topic, message) => {
  console.log(topic, message.toString());
});
```

通过 WebSocket 进行发布和订阅，其行为与通过 MQTT/TCP 或 MQTT/TLS 完全相同。

## 典型使用场景

MQTT over WebSocket 推荐用于以下场景：

- 基于浏览器的应用
- Web 仪表板和前端系统
- 仅允许使用 80 或 443 端口的环境
- 具有严格防火墙或代理策略的企业网络

对于可以使用 TCP 的后端服务和设备连接场景，通常建议优先使用 MQTT over TCP/TLS 以获得最佳性能。

## 性能考量

与 MQTT over TCP/TLS 相比，MQTT over WebSocket 会引入：

- 额外的 HTTP 和 WebSocket 帧封装开销
- 略微增加的延迟
- 轻微降低的吞吐量

在浏览器和 Web 应用场景中，这些差异通常可以忽略不计；但在高吞吐或对延迟敏感的场景下，可能需要进行评估。
