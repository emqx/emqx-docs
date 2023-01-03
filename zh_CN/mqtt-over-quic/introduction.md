# MQTT over QUIC

QUIC是现代网络的一个新的通用传输层协议。

见: [RFC9000](https://datatracker.ietf.org/doc/html/rfc9000)

## 简介

MQTT over QUIC 是EMQX 5.0的一个实验性功能。

相对于TLS1.2和TCP， 您可以测试评估QUIC协议将如何改善MQTT的客户端和EMQX之间的网络连接。

::: tip

是MQTT over QUIC，而不是 MQTT over HTTP/3。

:::

## 功能

- 支持0-RTT握手

  用于建立低延迟的连接。
  
- 支持客户端地址迁移

  MQTT（over QUIC）客户端可以改变其网络层地址或传输层端口，而不会有任何流量干扰。
  例如，在NAT重新绑定的情况下。
  
- 禁用流量加密

  在QUIC TLS1.3握手完成后，MQTT（over QUIC）客户端可以传输未加密的流量。
  
  见： [draft-banks-quic-disable-encryption](https://datatracker.ietf.org/doc/html/draft-banks-quic-disable-encryption)
  
  ::: tip
  
  这是一个RFC规范草案。
  
  :::

## 实现

### 客户端

MQTT客户端（QUIC客户端）发起一个QUIC连接(Connection)到EMQX（QUIC服务器）。

连接握手完成后，MQTT客户端开始向EMQX发送双向流（Bidirectional Stream）。

然后，所有的MQTT消息将在这个双向流上进行传输。

流的任何一方关闭流将导致连接关闭，就像TCP连接关闭一样。

参考客户端。

[emqtt](https://github.com/emqx/emqtt)

### TLS 1.3握手

ALPN: MQTT

## 功能启用

QUIC传输监听器默认是禁用的。

要试验QUIC承载，你需要 

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

    `/path/to/etc` 是emqx的 etc 目录，如`/etc/emqx/`。

    :::


1. 重新启动EMQX。

## 未来的工作

到目前为止，EMQX还没有利用QUIC提供的所有功能，如多流复用、流优先级、流量控制和
不可靠的数据报文。

这些功能将在以后的版本中得到解决，并希望能成为OASIS标准。

协议栈中有一些固化配置还没有准备好在生产环境使用。
