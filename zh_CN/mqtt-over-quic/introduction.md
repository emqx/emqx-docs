# MQTT over QUIC

QUIC 是现代网络的一个新的通用传输层协议。

见: [RFC9000](https://datatracker.ietf.org/doc/html/rfc9000)

## 简介

MQTT over QUIC 是 EMQX 5.0 的一个实验性功能。

相对于 TLS1.2 和 TCP， 您可以测试评估 QUIC 协议将如何改善 MQTT 的客户端和 EMQX 之间的网络连接。

::: tip

是 MQTT over QUIC，而不是 MQTT over HTTP/3。

:::

## 功能

- 支持 0-RTT 握手

  用于建立低延迟的连接。

- 支持客户端地址迁移

  MQTT（over QUIC）客户端可以改变其网络层地址或传输层端口，而不会有任何流量干扰。
  例如，在 NAT 重新绑定的情况下。

- 禁用流量加密

  在 QUIC TLS1.3 握手完成后，MQTT（over QUIC）客户端可以传输未加密的流量。

  见： [draft-banks-quic-disable-encryption](https://datatracker.ietf.org/doc/html/draft-banks-quic-disable-encryption)

  ::: tip

  这是一个 RFC 规范草案。

  :::

## 实现

### 客户端

MQTT 客户端（QUIC 客户端）发起一个 QUIC 连接(Connection)到 EMQX（QUIC 服务器）。

连接握手完成后，MQTT 客户端开始向 EMQX 发送双向流（Bidirectional Stream）。

然后，所有的 MQTT 消息将在这个双向流上进行传输。

流的任何一方关闭流将导致连接关闭，就像 TCP 连接关闭一样。

参考客户端。

[emqtt](https://github.com/emqx/emqtt)

### TLS 1.3 握手

ALPN: MQTT

## 功能启用

QUIC 传输监听器默认是禁用的。

要试验 QUIC 承载，你需要

1. 通过配置启用它

   ```
   listeners.quic.default {
   enabled = true
   bind = "0.0.0.0:14567"
   max_connections = 1024000
   keyfile = "/path/to/etc/certs/key.pem"
   certfile = "/path/to/etc//certs/cert.pem"
   }
   ```

   ::: tip

   `/path/to/etc` 是 emqx 的 etc 目录，如`/etc/emqx/`。

   :::

1. 重新启动 EMQX。

## 未来的工作

到目前为止，EMQX 还没有利用 QUIC 提供的所有功能，如多流复用、流优先级、流量控制和
不可靠的数据报文。

这些功能将在以后的版本中得到解决，并希望能成为 OASIS 标准。

协议栈中有一些固化配置还没有准备好在生产环境使用。
