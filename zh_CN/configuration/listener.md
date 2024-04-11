# 监听器配置

您可以在 EMQX 中配置监听器以接受来自 MQTT 客户端的请求。EMQX 支持多种消息传输协议，具体包括：

- TCP：端口 `1883`
- SSL：端口 `8883`
- WebSocket：端口 `8083`
- 安全 WebSocket（WSS）：端口 `8084`

## 配置 TCP 监听器

TCP 监听器是一种网络服务，它在特定的网络端口上监听传入的 TCP 连接。它在客户端与 EMQX 之间通过 TCP/IP 网络建立和维护连接中发挥重要作用。

在 EMQX 中配置 TCP 监听器，需在 EMQX 安装目录下的 `etc` 文件夹中的 `emqx.conf` 文件添加 `listeners.tcp` 配置项。

例如，若要启用端口 `1883` 上的 TCP 监听器，并设置监听器最多允许 1,024,000 个并发连接，可使用以下配置：

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
}
```

配置说明：

- `listeners.tcp.default` 代表启用该监听器，`default` 为监听器名称，可根据需要更改。

- `bind` 设定监听器的 IP 地址及端口，此处配置为监听所有 IP 地址上的 `1883` 端口的所有传入流量。
- `max_connections` 设置监听器允许的最大并发连接数，默认值为 `infinity`。

## 配置 SSL 监听器

SSL 监听器监听传入的 Secure Sockets Layer (SSL）连接，用于加密客户端与 EMQX 间传输的数据，保护网络通信安全。

在 EMQX 中配置 SSL 监听器，需在 `emqx.conf` 文件中添加 `listeners.ssl` 配置项。

例如，若要在端口 `8883` 上启用 SSL 监听器，同时允许最多 1,024,000 个并发连接，可使用以下配置：

```
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  max_connections = 1024000
  ssl_options {
    cacertfile = "etc/certs/cacert.pem"
    certfile = "etc/certs/cert.pem"
    keyfile = "etc/certs/key.pem"
    verify = verify_none
    fail_if_no_peer_cert = false
  }
}
```

配置说明：

- `listeners.ssl.default` 启用该监听器。

- `bind` 指定监听器的 IP 地址和端口，此处为监听所有 IP 地址上的 `8883` 端口的所有传入流量。
- `max_connections` 设置允许的最大并发连接数，默认为 `infinity`。
- `ssl_options` 为 SSL/TLS 配置选项，包括：
  - `cacertfile`：包含监听器用于验证客户端证书真实性的受信任 CA 证书的 PEM 文件。
  - `certfile`：包含监听器 SSL/TLS 证书链的 PEM 文件。如果证书不是直接由根 CA 颁发，则需将中间 CA 证书附加在监听器证书之后形成证书链。
  - `keyfile`：包含 SSL/TLS 证书对应私钥的 PEM 文件。
  - `verify`：设置 `verify_peer` 验证客户端证书真实性，否则为 `verify_none`。
  - `fail_if_no_peer_cert`：若设置为 `true`，则客户端未发送证书时，服务器会认为连接失败（空证书被视为有效）。

## 配置 WebSocket 监听器

WebSocket 监听器接收并处理通过 WebSocket 协议传入的消息。EMQX 的 WebSocket 支持使客户端能够使用 WebSocket 协议连接到 EMQX 并实时交换数据。

在 EMQX 中配置 WebSocket 监听器，需在 `emqx.conf` 文件中添加 `listeners.ws` 配置项。

例如，若要在端口 `8083` 上启用 WebSocket 监听器，并允许最多 1,024,000 个并发连接，可使用以下配置：

```bash
listeners.ws.default {
  bind = "0.0.0.0:8083"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
}
```

配置说明：

- `listeners.ws.default` 启用该监听器。

- `bind` 指定监听器 IP 地址和端口，此处为监听所有 IP 地址上的 `8083` 端口的所有传入流量。
- `max_connections` 设置允许的最大并发连接数，默认为 `infinity`。
- `websocket.mqtt_path` 设置 WebSocket 的 MQTT 协议路径，默认为 `/mqtt`。

## 配置安全 WebSocket 监听器

安全 WebSocket 监听器通过 SSL 或 TLS 协议加密 WebSocket 客户端与代理之间交换的数据，是保护数据安全的重要措施。

在 EMQX 中配置安全 WebSocket 监听器，需在 `emqx.conf` 文件中添加 `listeners.wss` 配置项。

例如，若要在端口 `8084` 上启用安全 WebSocket 监听器，并允许最多 1,024,000 个并发连接，可使用以下配置：

```
listeners.wss.default {
  bind = "0.0.0.0:8084"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
  ssl_options {
    cacertfile = "etc/certs/cacert.pem"
    certfile = "etc/certs/cert.pem"
    keyfile = "etc/certs/key.pem"
  }
}
```

配置说明：

- `listeners.wss.default` 启用该监听器。

- `bind` 指定监听器的 IP 地址和端口，此处为监听所有 IP 地址上的 `8084` 端口的所有传入流量。
- `max_connections` 设置允许的最大并发连接数，默认为 `infinity`。
- `websocket.mqtt_path` 设置 WebSocket 的 MQTT 协议路径，默认为 `/mqtt`。
- `ssl_options` 包括 SSL/TLS 配置选项，详细说明参见 [配置 SSL 监听器](#配置-ssl-监听器)。

{% emqxce %}

EMQX 提供了更多配置项以更好地满足定制化需求。详细信息参考[配置手册](https://www.emqx.io/docs/zh/v@CE_VERSION@/hocon/)。

{% endemqxce %}

{% emqxee %}

EMQX 提供了更多配置项以更好地满足定制化需求。详细信息参考[配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)。

{% endemqxee %}

::: tip

您也可以通过在 Dashboard 点击左侧导航菜单中的**管理** -> **监听器**来配置监听器。一旦您通过 Dashboard 配置了这些项目，您的设置将覆盖 `emqx.conf` 中的相同配置项。

:::
