# IPv6

EMQX 全面支持 IPv6，涵盖客户端连接、Dashboard、集群节点间通信以及到外部服务的出站连接。本页介绍如何在 IPv6 环境中配置 EMQX，包括纯 IPv6（单栈）和双栈部署。

## MQTT 监听器

要通过 IPv6 接受 MQTT 客户端连接，需将监听器绑定到 IPv6 地址。当检测到 IPv6 绑定地址时，EMQX 会自动启用 `inet6` socket 选项。

### 双栈（IPv4 和 IPv6）

绑定到 `[::]` 可在同一端口上同时接受 IPv4 和 IPv6 连接：

```bash
listeners.tcp.default {
  bind = "[::]:1883"
}
```

::: tip

在大多数操作系统上，绑定到 `[::]` 默认同时接受 IPv4 和 IPv6 连接（双栈模式）。这是需要同时支持两种协议的环境中最简单的配置方式。

:::

### 仅 IPv6

要将监听器限制为仅接受 IPv6 连接，请设置 `ipv6_v6only = true`：

```bash
listeners.tcp.default {
  bind = "[::]:1883"
  ipv6_v6only = true
}
```

此选项设置 `IPV6_V6ONLY` socket 选项，阻止接受 IPv4 映射的 IPv6 地址。

### 绑定到特定 IPv6 地址

可以绑定到特定的 IPv6 地址：

```bash
listeners.tcp.default {
  bind = "[::1]:1883"
}
```

相同的配置方式适用于 SSL、WebSocket 和安全 WebSocket 监听器：

```bash
listeners.ssl.default {
  bind = "[::]:8883"
  ssl_options {
    certfile = "etc/certs/cert.pem"
    keyfile = "etc/certs/key.pem"
    cacertfile = "etc/certs/cacert.pem"
  }
}

listeners.ws.default {
  bind = "[::]:8083"
}

listeners.wss.default {
  bind = "[::]:8084"
}
```

## Dashboard HTTP/HTTPS 监听器

EMQX Dashboard 的 HTTP/HTTPS 监听器同样支持 IPv6。

### 使用 IPv6 绑定地址

当 `bind` 地址为 IPv6 地址时，EMQX 会自动为 Dashboard 监听器启用 IPv6：

```bash
dashboard.listeners.http {
  bind = "[::]:18083"
}
```

### 使用 `inet6` 标志

如果仅使用端口号绑定（不指定 IP 地址），可以显式启用 IPv6：

```bash
dashboard.listeners.http {
  bind = 18083
  inet6 = true
}
```

| 参数          | 类型    | 默认值  | 说明                                                                   |
| ------------ | ------- | ------- | ---------------------------------------------------------------------- |
| `inet6`      | boolean | `false` | 启用 IPv6 支持。设为 `false` 时，监听器仅接受 IPv4 流量。                   |
| `ipv6_v6only`| boolean | `false` | 禁用 IPv4 到 IPv6 的地址映射。仅在 `inet6` 为 `true` 时生效。              |

## 集群通信

当 EMQX 集群中的节点通过 IPv6 网络通信时，需要配置两个组件：Erlang 分布式协议（用于集群协调）和 Gen RPC 通道（用于节点间数据转发）。

### Erlang 分布式协议

设置 `cluster.proto_dist` 以使用 IPv6 进行节点间通信：

```bash
cluster.proto_dist = inet6_tcp
```

可用选项：

| 值            | 说明                                             |
| ------------ | ------------------------------------------------ |
| `inet_tcp`   | 基于 IPv4 的 TCP（默认）                            |
| `inet6_tcp`  | 基于 IPv6 的 TCP                                   |
| `inet_tls`   | 基于 IPv4 的 TLS，通过 `etc/ssl_dist.conf` 配置      |
| `inet6_tls`  | 基于 IPv6 的 TLS，通过 `etc/ssl_dist.conf` 配置      |

::: warning 重要提示

当使用 IPv6 节点名称（例如 `emqx@::1`）时，**必须**将 `cluster.proto_dist` 设置为 `inet6_tcp` 或 `inet6_tls`。否则节点将无法启动，并报告"not responding to pings"错误。

:::

### Gen RPC

为 Gen RPC 配置 IPv6：

```bash
rpc.listen_address = "::"
rpc.ipv6_only = true
```

| 参数                 | 类型    | 默认值      | 说明                                                                         |
| ------------------- | ------- | ----------- | --------------------------------------------------------------------------- |
| `rpc.listen_address`| string  | `0.0.0.0`   | RPC 服务器监听的 IP 地址。IPv4 使用 `0.0.0.0`，IPv6 使用 `::`。                  |
| `rpc.ipv6_only`     | boolean | `false`     | 当 `listen_address` 为 IPv6 地址时，设为 `true` 可强制 RPC 客户端仅使用 IPv6。     |

## 出站连接

EMQX 会向外部服务发起出站连接，用于 HTTP 认证、Webhook 动作、数据库集成等功能。

### 自动 IPv6 检测

对于基于 HTTP 的连接器（认证后端、Webhook 动作等），EMQX 会自动探测目标主机是否支持 IPv6，并选择合适的地址族。大多数情况下无需手动配置。

### 手动配置

部分连接器类型在其配置中提供了 `ipv6_probe` 开关。启用时（HTTP 连接器默认启用），EMQX 会优先尝试 IPv6 连接。如果您的网络仅支持 IPv4 且 DNS 同时返回 A 和 AAAA 记录，可以禁用此探测以避免连接延迟：

```bash
# 示例：HTTP 认证后端
authentication {
  backend = "http"
  method = "post"
  url = "http://auth-server.example.com:8080/auth"

  # 如不需要，可禁用 IPv6 自动检测
  pool_size = 8
}
```

## 完整的纯 IPv6 配置示例

以下是纯 IPv6 部署的最小化 `emqx.conf` 配置：

```bash
# 使用 IPv6 地址的节点名称
node.name = "emqx@::1"

# 集群分布式协议使用 IPv6
cluster.proto_dist = inet6_tcp

# Gen RPC 使用 IPv6
rpc.listen_address = "::"
rpc.ipv6_only = true

# MQTT 监听器使用 IPv6
listeners.tcp.default {
  bind = "[::]:1883"
  ipv6_v6only = true
}

# Dashboard 使用 IPv6
dashboard.listeners.http {
  bind = "[::]:18083"
}
```

## 故障排查

### 节点无法响应 Ping

**现象**：使用 IPv6 节点名称启动集群节点时，节点报告"not responding to pings"并无法启动。

**原因**：Erlang 分布式协议默认使用 `inet_tcp`（IPv4）。IPv6 节点名称需要使用 `inet6_tcp`。

**解决方案**：在 `emqx.conf` 中设置 `cluster.proto_dist = inet6_tcp`。

### 出站连接报 `enetunreach` 错误

**现象**：出站 HTTP 请求（例如到认证后端的请求）失败，错误为 `enetunreach`（网络不可达）。

**原因**：连接尝试使用 IPv4 访问仅支持 IPv6 的服务，或反之。

**解决方案**：验证目标服务可通过正确的地址族从 EMQX 主机访问。对于 HTTP 连接器，自动 IPv6 探测功能通常可以处理此问题。如果服务使用域名，请确保 DNS 返回正确的记录类型（IPv4 为 A 记录，IPv6 为 AAAA 记录）。

### IPv6 环境下 Dashboard 无法访问

**现象**：在纯 IPv6 环境中运行 EMQX 时，无法访问 Dashboard。

**原因**：Dashboard 监听器默认使用 IPv4（`0.0.0.0:18083`）。

**解决方案**：将 Dashboard 绑定到 IPv6 地址（`bind = "[::]:18083"`），或显式启用 IPv6（`inet6 = true`）。
