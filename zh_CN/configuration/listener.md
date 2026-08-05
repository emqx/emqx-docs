# 监听器配置

您可以在 EMQX 中配置监听器以接受来自 MQTT 客户端的请求。EMQX 支持多种消息传输协议，具体包括：

- TCP：端口 `1883`
- SSL：端口 `8883`
- WebSocket：端口 `8083`
- 安全 WebSocket（WSS）：端口 `8084`

::: tip

您也可以通过在 Dashboard 点击左侧导航菜单中的**管理** -> **监听器**来配置监听器。
注意，如果监听器在  `emqx.conf` 中显式配置，那么在 Dashboard 中进行的修改只能临时生效直到下次 EMQX 重启。

:::

::: tip

EMQX 提供了更多配置项以更好地满足定制化需求。详情请参见 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)。

:::

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

有关 MQTT over WebSocket 的工作原理及其典型使用场景的概述，请参阅 [MQTT over WebSocket](../connect-emqx/mqtt-over-websocket.md)。

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

## WebSocket 监听器的转发客户端地址

WebSocket 与安全 WebSocket 监听器提供以下配置项，用于在监听器位于代理或负载均衡器之后时决定 EMQX 如何获取客户端的源地址：

- `websocket.proxy_address_header`（默认值：`x-forwarded-for`）
- `websocket.proxy_port_header`（默认值：`x-forwarded-port`）
- `websocket.proxy_address_allow`（默认值：`["0.0.0.0/0"]`，6.3.0 起提供）

当 WebSocket 升级请求中携带 `proxy_address_header` 所配置的请求头时，EMQX 会使用该请求头值中第一个（最左侧的）条目作为客户端的源 IP 地址（`proxy_port_header` 对应端口），而不再使用真实 TCP 对端的地址。基于 IP 的授权规则、客户端封禁、连接抖动检测以及审计与追踪日志所看到的客户端源 IP 都来自这个派生地址。

`proxy_address_allow` 限制哪些 TCP 对端（代理）有权通过这些请求头设置客户端地址：只有当真实 TCP 对端地址落在所列 CIDR 范围内时，请求头才会被采信。默认值 `["0.0.0.0/0"]` 信任所有 IPv4 对端，与 6.3.0 之前版本的行为一致；IPv6 对端默认不被信任。可将其设置为您的代理地址以仅信任这些代理，或设置为 `[]` 以完全不采信请求头。

::: warning 仅在受信任代理之后才可信任转发地址请求头

该请求头的值决定了客户端的表观源 IP，因此只有在由受信任的代理设置该请求头时才可信任它：

- 如果监听器可被客户端直接访问（前面没有代理），任何客户端都可以自行发送该请求头，从而任意选择自己的表观源 IP。此时应设置 `proxy_address_allow = []` 以完全不采信请求头；在 6.3.0 之前的版本中，应设置 `proxy_address_header = ""` 和 `proxy_port_header = ""`。
- 如果前面有代理，但代理是将自身观察到的地址**追加**到入站 `X-Forwarded-For` 请求头之后，而不是覆盖或去除它（大多数代理默认为追加行为，例如 NGINX 的 `$proxy_add_x_forwarded_for`），那么 EMQX 读取的最左侧条目仍然是客户端提供的值——即使该代理在 `proxy_address_allow` 中，源 IP 依然可以被伪造。应将代理配置为使用其观察到的地址覆盖该请求头，或改用 [Proxy Protocol](../deploy/cluster/lb.md)。
- 不要试图通过将 `proxy_address_header` 指向一个未使用的请求头名称来“禁用”该机制：客户端可以发送任意名称的请求头。请改用 `proxy_address_allow = []`（或将请求头配置项设置为空字符串）。

当监听器设置了 `proxy_protocol = true` 时，客户端地址来自 Proxy Protocol 握手，不会读取这些请求头。
:::

## 将监听器关联到配置区域

EMQX 中的每个监听器都与一个区域相关联，默认设置为名为 `default` 的逻辑区域。

当监听器关联到特定区域时，连接到该监听器的 MQTT 客户端将继承该区域的设置。

更多信息，请查看配置文件简介中的[区域覆盖](./configuration.md#区域覆盖)部分。
