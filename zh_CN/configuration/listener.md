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

WebSocket 与安全 WebSocket 监听器提供两个配置项，用于在监听器位于代理或负载均衡器之后时决定 EMQX 如何获取客户端的源地址：

- `websocket.proxy_address_header`（默认值：`x-forwarded-for`）
- `websocket.proxy_port_header`（默认值：`x-forwarded-port`）

当 WebSocket 升级请求中携带所配置的请求头时，EMQX 会使用该请求头值中第一个（最左侧的）条目作为客户端的源 IP 地址（或端口），而不再使用真实 TCP 对端的地址。基于 IP 的授权规则、客户端封禁、连接抖动检测以及审计与追踪日志所看到的客户端源 IP 都来自这个派生地址。

::: warning 仅在受信任代理之后才可信任转发地址请求头

该请求头的值决定了客户端的表观源 IP，因此只有在由受信任的代理设置该请求头时才可信任它：

- 如果监听器可被客户端直接访问（前面没有代理），任何客户端都可以自行发送该请求头，从而任意选择自己的表观源 IP。此时应设置 `proxy_address_header = ""` 和 `proxy_port_header = ""`，使 EMQX 始终使用真实的 TCP 对端地址。
- 如果前面有代理，但代理是将自身观察到的地址**追加**到入站 `X-Forwarded-For` 请求头之后，而不是覆盖或去除它（大多数代理默认为追加行为，例如 NGINX 的 `$proxy_add_x_forwarded_for`），那么 EMQX 读取的最左侧条目仍然是客户端提供的值，源 IP 依然可以被伪造。应将代理配置为使用其观察到的地址覆盖该请求头，或改用 [Proxy Protocol](../deploy/cluster/lb.md)，或将上述配置项设置为 `""`。
- 不要试图通过将配置项指向一个未使用的请求头名称来“禁用”该机制：客户端可以发送任意名称的请求头。空字符串是客户端唯一无法提供的值。

当监听器设置了 `proxy_protocol = true` 时，客户端地址来自 Proxy Protocol 握手，不会读取这些请求头。
:::

## 将监听器关联到配置区域

EMQX 中的每个监听器都与一个区域相关联，默认设置为名为 `default` 的逻辑区域。

当监听器关联到特定区域时，连接到该监听器的 MQTT 客户端将继承该区域的设置。

更多信息，请查看配置文件简介中的[区域覆盖](./configuration.md#区域覆盖)部分。

## 挂载点（Mountpoint）

每个监听器都可以配置 `mountpoint`（挂载点）：EMQX 会为通过该监听器连接的客户端使用的主题添加一个主题前缀。该前缀会被添加到 `PUBLISH` 报文、`SUBSCRIBE` 和 `UNSUBSCRIBE` 请求以及遗嘱消息中的主题上，并会从投递给客户端的消息主题中移除。挂载点对客户端透明，常用于在客户端分组之间隔离主题空间，例如多租户部署场景。

```bash
listeners.tcp.demo {
    bind = "0.0.0.0:1883"
    mountpoint = "department-a/"
}
```

挂载点支持占位符 `${clientid}`、`${username}`、`${zone}` 和 `${client_attrs.NAME}`。例如，配置 `mountpoint = "${username}/"` 后，用户名为 `u1` 的客户端订阅 `sensors/#` 时，实际在 Broker 内部创建的订阅为 `u1/sensors/#`。

### 与基于主题前缀的扩展功能不兼容

EMQX 的一些功能通过发布或订阅带有特殊 `$` 前缀的主题来触发。EMQX 会先添加挂载点前缀，再匹配这些特殊前缀。例如，客户端通过配置了挂载点 `mp/` 的监听器连接，并发布到 `$delayed/10/t` 时，Broker 收到的主题为 `mp/$delayed/10/t`，不再以 `$delayed/` 开头。相应功能会被静默绕过：EMQX 将该消息作为普通消息路由到挂载后的字面主题，且不会向客户端报告任何错误。

::: warning 兼容性限制
如果客户端需要使用以下任一功能，请勿在其连接的监听器上配置挂载点：

| 功能 | 主题前缀 |
| --- | --- |
| [延迟发布](../messaging/mqtt-delayed-publish.md) | `$delayed/` |
| [文件传输](../file-transfer/introduction.md) | `$file/`、`$file-async/`、`$file-response/` |
| [消息队列](../message-queue/message-queue-concept.md) | `$queue/` |
| [MQTT 消息流](../mqtt-stream/mqtt-stream-concept.md) | `$stream/` |
| [集群连接](../cluster-linking/introduction.md) | `$LINK/` |

对于集群连接，接受对端集群连接的监听器不能配置挂载点。
:::

[共享订阅](../messaging/mqtt-shared-subscription.md)（`$share/{group}/`）以及[排他订阅](../messaging/mqtt-exclusive-subscription.md)（`$exclusive/`）是例外：它们可以与挂载点配合使用。EMQX 会先解析这些订阅前缀，然后再应用挂载点，因此挂载点只会被添加到内部的实际主题过滤器上。例如，通过配置了挂载点 `mp/` 的监听器订阅 `$share/g/t`，会以主题 `mp/t` 加入共享订阅组 `g`。
