# NATS 协议网关

EMQX 5.10 引入了 NATS 网关，它基于 [NATS Protocol](https://docs.nats.io/reference/reference-protocols/nats-protocol) 实现，支持接受 NATS 客户端的连接，并和 MQTT 发布/订阅进行互通。当前支持的特性有：

- 完整的协议报文支持，例如 INFO、CONNECT、PUB/HPUB、SUB/UNSUB、MSG/HMsg、PING/PONG、+OK/-ERR。
- 支持 CONNECT 报文携带 `verbose=true` 开启消息应答。
- 支持 TCP、TLS、Websocket、Websocket over TLS 监听器。
- 支持 NATS 客户端发布订阅、通配符订阅。且 MQTT 发布订阅进行互通。
- 支持 Queue Group 共享订阅。
- 支持 Request/Reply。且支持当请求的主题无订阅者时，快速回复失败消息给请求客户端。

## 快速开始

在 EMQX 5.0 中可以通过 Dashboard 配置并快速启用 NATS 网关。

也可以通过 HTTP API 或 emqx.conf 来启用，例如：

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateway/nats' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "nats",
  "enable": true,
  "mountpoint": "nats/",
  "listeners": [
    {h
      "type": "tcp",
      "name": "default",
      "bind": "4222",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'

```
:::

::: tab Configuration

```properties
gateway.nats {

  mountpoint = "nats/"

  listeners.tcp.default {
    bind = 4222
    acceptors = 16
    max_connections = 1024000
    max_conn_rate = 1000
  }
}
```
:::

::::

::: tip
通过配置文件进行配置网关，需要在每个节点中进行配置；通过 Dashboard 或者 HTTP API 管理则会在整个集群中生效。
:::

NATS 网关支持 TCP/SSL/WS/WSS 类型的监听器，其完整可配置的参数列表可以参考 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)中的网关配置 - 监听器。


## 认证

NATS 协议支持多种认证方式，包括用户名/密码、Token 认证等。NATS 网关支持以下多种认证器类型，例如：
- [内置数据库认证](../access-control/authn/mnesia.md)
- [MySQL 认证](../access-control/authn/mysql.md)
- [MongoDB 认证](../access-control/authn/mongodb.md)
- [PostgreSQL 认证](../access-control/authn/postgresql.md)
- [Redis 认证](../access-control/authn/redis.md)
- [HTTP Server 认证](../access-control/authn/http.md)
- [JWT 认证](../access-control/authn/jwt.md)
- [LDAP 认证](../access-control/authn/ldap.md)

NATS 网关使用 NATS 协议的 CONNECT 报文中的信息来生成客户端的认证信息。默认情况下：

- Client ID：为随机生成的字符串。
- Username：为 CONNECT 报文中的 `user` 字段的值。
- Password：为 CONNECT 报文中的 `pass` 字段的值。

例如，通过 HTTP API 或 emqx.conf 为 NATS 网关创建一个内置数据库认证：

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'POST' \
  'http://127.0.0.1:18083/api/v5/gateway/nats/authentication' \
  -u <your-application-key>:<your-security-key> \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{
  "backend": "built_in_database",
  "mechanism": "password_based",
  "password_hash_algorithm": {
    "name": "sha256",
    "salt_position": "suffix"
  },
  "user_id_type": "username"
}'
```
:::

::: tab Configuration

```properties
gateway.nats {

  authentication {
    backend = built_in_database
    mechanism = password_based
    password_hash_algorithm {
      name = sha256
      salt_position = suffix
    }
    user_id_type = username
  }
}
```
:::

::::


与 MQTT 协议不同，**网关仅支持创建一个认证器，而不是认证器列表（或认证链）**。当不启用任何认证器时，表示允许所有的 NATS 客户端都具有接入的权限。

其他类型的认证器的配置格式参考：[安全- 认证器](../access-control/authn/authn.md)。

## 发布订阅

NATS 协议完全兼容发布订阅的消息模式，并和 MQTT 的发布订阅进行消息互动。NATS 网关的转换规则：

- NATS 协议的 PUB 和 HPUB 报文作为消息发布。
  * 其主题为 PUB 报文中的 `subject` 字段。 例如 Subject 为 `t.a` 会被 NATS 网关转换成功 MQTT 主题 `t/a` 进行发布。
  * 消息内容为 PUB 报文的消息体内容。
  * 当客户端连接 CONNECT 报文中的 `verbose=1` 时，转换消息的 QoS 固定 1；否则为 0。
- NATS 协议的 SUB 报文作为订阅请求。
  * 其主题为 SUB 报文中的 `subject` 字段。例如 Subject 为 `t.a` 会被 NATS 网关转化为 MQTT 主题 `t/a` 进行订阅。
  * 当客户端连接 CONNECT 报文中的 `verbose=1` 时，转换订阅的 QoS 固定 1；否则为 0。
  * 支持通配符，例如 `*.b.>` 会转换为 `+/b/#`。
  * 支持共享订阅。SUB 报文的 Queue Group 会被转换为 MQTT 共享订阅的组名。
- NATS 协议的 UNSUB 报文作为取消订阅请求。其主题为 UNSUB 报文中对应的订阅 ID。

网关内无独立的发布订阅的权限控制，其对主题的权限控制需要统一在 [授权（Authorization）](../access-control/authz/authz.md) 中管理。

## 用户层接口

- 详细配置说明参考：[网关配置 - NATS 网关](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)
- 详细 HTTP API 接口参考：[HTTP API - 网关](https://docs.emqx.com/zh/enterprise/v@EE_MINOR_VERSION@/admin/api-docs)

## 限制

当前，在 EMQX 5.10 中，存在以下实现限制：

- 由于当前网关监听器不支持从 TCP 升级为 TLS 连接，所以暂不支持客户端以 `tls_handshake_first=false` 进行连接。
- 在未配置认证器时，支持未发起 CONNECT 报文的 NATS 客户端进行发布订阅，但目前暂不支持管理匿名客户端。