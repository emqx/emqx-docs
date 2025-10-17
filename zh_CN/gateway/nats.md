# NATS 协议网关

从 EMQX 5.10.0 起，EMQX 引入了基于 [NATS Protocol](https://docs.nats.io/reference/reference-protocols/nats-protocol) 的 NATS 协议网关，用于接入 NATS 客户端并实现与 MQTT 的消息互通。本文档将介绍其功能特性，并指导您如何启用和配置 NATS 网关。

## 功能特性概览

NATS 协议网关当前支持以下主要功能：

### 协议支持

- **完整的 NATS 协议支持**：兼容以下报文类型：
  - 连接与会话管理：`INFO`、`CONNECT`
  - 消息发布与订阅：`PUB`、`HPUB`、`SUB`、`UNSUB`
  - 消息传递与响应：`MSG`、`HMSG`
  - 心跳与状态响应：`PING`、`PONG`、`+OK`、`-ERR`
- **Verbose 模式支持**：支持客户端通过 `CONNECT verbose=true` 开启消息确认响应。

### 协议互通能力（与 MQTT）

- **与 MQTT 的双向消息互通**：
  - NATS 客户端发布的消息可映射为 MQTT 消息发布。
  - MQTT 消息可转发给订阅了相应主题的 NATS 客户端。
- **支持 NATS 通配符订阅**，并自动转换为对应的 MQTT 主题格式。
- **支持 Queue Group（队列组）共享订阅**：NATS 客户端使用 Queue Group 订阅时，将被转换为 MQTT 的共享订阅格式。
- **支持 Request/Reply 模式**，包括以下能力：
  - NATS 客户端发送的请求消息可转为 MQTT 请求消息。
  - 若目标主题无任何订阅者，EMQX 网关可快速返回失败响应。

### 网络与连接能力

- **多种传输协议监听器**：支持 TCP、TLS、WebSocket (WS)、WebSocket over TLS (WSS) 四种类型的连接方式。

## 协议互通与消息映射

NATS 协议完全兼容发布订阅的消息模式，并和 MQTT 的发布订阅进行消息互通。NATS 网关的转换规则：

- NATS 协议的 PUB 和 HPUB 报文作为消息发布。
  * 其主题为 PUB 报文中的 `subject` 字段。 例如 Subject 为 `t.a` 会被 NATS 网关转换成为 MQTT 主题 `t/a` 进行发布。
  * 消息内容为 PUB 报文的消息体内容。
  * 当客户端连接 CONNECT 报文中的 `verbose=1` 时，转换消息的 QoS 固定 1；否则为 0。
- NATS 协议的 SUB 报文作为订阅请求。
  * 其主题为 SUB 报文中的 `subject` 字段。例如 Subject 为 `t.a` 会被 NATS 网关转换为 MQTT 主题 `t/a` 进行订阅。
  * 当客户端连接 CONNECT 报文中的 `verbose=1` 时，转换订阅的 QoS 固定 1；否则为 0。
  * 支持通配符，例如 `*.b.>` 会转换为 `+/b/#`。
  * 支持共享订阅。SUB 报文的 Queue Group 会被转换为 MQTT 共享订阅的组名。
- NATS 协议的 UNSUB 报文作为取消订阅请求。其主题为 UNSUB 报文中对应的订阅 ID。

网关内无独立的发布订阅的权限控制，其对主题的权限控制需要统一在[授权](../access-control/authz/authz.md)中管理。

## 当前功能限制

当前，在 EMQX 5.10.0 中，存在以下实现限制：

- 由于当前网关监听器不支持从 TCP 协议升级为 TLS 协议连接，所以暂不支持客户端以 `tls_handshake_first=false` 进行连接。
- 在未配置认证器时，支持未发起 CONNECT 报文的 NATS 客户端进行发布订阅，但目前暂不支持管理匿名客户端。

## 启用 NATS 网关

在 EMQX 5.10.0 中可以通过 Dashboard 配置并快速启用 NATS 网关，也可以通过 REST API 或配置文件 `base.hocon` 来启用。

::: tip

在集群模式下，使用 Dashboard 或 REST API 进行的网关配置将在所有节点上生效。若希望仅在特定节点上配置网关，可通过修改各节点的 `base.hocon` 配置文件实现。

:::

### 使用 Dashboard 快速启用

在 EMQX Dashboard 左侧导航目录中点击**管理** -> **网关**。**网关**页面上列出了所有支持的网关。找到 **NATS** 并点击**操作**列中的**配置**按钮，您将进入**初始化 NATS** 页面。

为了简化配置过程，EMQX 为**网关**页面上所有必填的字段提供了默认值。如果不需要自定义配置，您只需以下 3 步即可启用 NATS 网关:

1. 接受**基础参数**步骤页面上所有的默认设置，点击**下一步**。
2. 在**监听器**步骤页面中，您可以添加监听器，也可以稍后配置并点击**下一步**。有关监听器的具体配置说明，参考[添加监听器](#添加监听器)。
3. 点击**启用**按钮以激活 NATS 网关。

完成网关激活过程后，您将返回**网关**页面并看到 NATS 网关现在显示为**已启用**状态。

### 使用 REST API 启用

以下为通过 REST API 启用 NATS 网关的示例：

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateway/nats' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "nats",
  "enable": true,
  "mountpoint": "nats/",
  "listeners": [
    {
      "type": "tcp",
      "name": "default",
      "bind": "4222",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'

```
### 使用配置文件启用

以下为通过配置文件启用 NATS 网关的示例：

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
NATS 网关支持 TCP/SSL/WS/WSS 类型的监听器，其完整可配置的参数列表可以参考 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)中的网关配置 - 监听器。

## 自定义您的 NATS 网关

除了默认设置外，EMQX 还提供了各种配置选项，以更好地适应您的特定业务需求。本节提供了关于**网关**页面上可用配置选项的深入概述。

### 基本设置

1. 在**网关**页面，找到 **NATS**，点击**操作**列中的**设置**。

2. 在**设置**页签中，您可以自定义并设置该网关的连接参数、挂载点字符串和客户端连接覆盖等。
   - **服务名称**：网关的唯一标识名称，用于后续引用。默认值：`emq_nats_gateway`。
   
   - **挂载点**: 设置一个字符串，为所有通过网关传输的发布订阅主题添加统一前缀，提供在不同协议之间实现消息路由隔离的方法，例如设置为 `nats/`。此主题前缀由网关管理，客户端在发布和订阅时无需显式添加此前缀。
   
   - **默认心跳间隔**：服务端主动发送 PING 的时间间隔，用于探测客户端是否仍在线。默认值：`60` 秒。
   
   - **心跳超时阀值**：如果客户端在该时间内未回应心跳，将被视为断开连接。
   
   - **最大消息负载大小**：单条 PUB/HPUB 消息的 Payload 最大字节数。默认值：`1048576` 字节。
   
   - **空闲超时时间**: 设置连接客户端在无活动状态下被视为断开连接的持续时间（以秒为单位）。默认值：`30` 秒。
   
   - **启用统计**: 设置是否允许网关收集和报告统计信息。默认为开启。
   
   - **客户端信息覆盖**：客户端连接信息的覆盖策略，支持从 CONNECT 报文中提取客户端认证信息：
     
     ::: tip
     
     在启用认证器的情况下，应明确设置 `username` 和 `password` 的映射字段，以确保认证信息准确传递。
     
     :::
     
     - **用户名**：映射 CONNECT 报文中的 `user` 字段。
     - **密码**：映射 CONNECT 报文中的 `pass` 字段。
     - **客户端 ID**：可设置为 `${generated}` 由服务自动生成，或指定逻辑规则。
   
3. 完成设置后，点击**更新**。

### 添加监听器

您可以点击**监听器**页签进行更多自定义设置，包括编辑、删除监听器，或添加新的监听器。

1. 在**监听器**页签中点击 **+ 添加监听器**。

2. 在弹出的**添加监听器**页面中进行以下配置：

   **基本设置**

   - **名称**: 为监听器设置一个唯一标识符。
   - **类型**: 选择协议类型，对于 NATS，可以选择 `tcp`，`ssl`，`ws`，或 `wss`。
   - **监听地址**: 设置监听器接受传入连接的端口号。

   **监听器设置**

   - **最大连接数**: 设置监听器可以处理的最大并发连接数，默认值：`1024000`。
   - **最大连接速率（监听器）**: 设置监听器每秒钟可以接受的最大新连接数，默认值：`1000`。
   - **代理协议**：是否开启 Proxy Protocol V1/2。默认值：`false`。
   - **代理协议超时**：接收 Proxy Protocol 报文头的超时时间。如果在超时内没有收到 Proxy Protocol 包，EMQX 将关闭 TCP 连接。默认值：`3` 秒。

   **双向认证设置**（仅适用于 SSL 和 WSS 监听器）

   **双向认证**开关默认开启。您需要配置相关的 TLS 证书，TLS 密钥和 CA 证书信息，可以通过输入文件内容或使用**选择文件**按钮上传。详细信息请参阅 [开启 SSL/TLS 连接](../network/emqx-mqtt-tls.md)。

   - **TLS Cert**: 填写服务器中 TLS Cert 的文件路径或直接填写 TLS Cert 文件内的内容。
   - **TLS Key**: 填写服务器中 TLS Cert 的文件路径或直接填写 TLS Cert 文件内的内容。
   - **CA Cert**: 填写服务器中 CA Cert 的文件路径或直接填写 CA Cert 文件内的内容。
   - **强制验证对端证书**: 默认为 `true`。

3. 完成设置后，点击**添加**。

### 配置接入认证

NATS 协议支持多种认证方式，包括用户名/密码、Token 认证等。NATS 网关支持以下多种认证器类型，例如：
- [内置数据库认证](../access-control/authn/mnesia.md)
- [MySQL 认证](../access-control/authn/mysql.md)
- [MongoDB 认证](../access-control/authn/mongodb.md)
- [PostgreSQL 认证](../access-control/authn/postgresql.md)
- [Redis 认证](../access-control/authn/redis.md)
- [HTTP Server 认证](../access-control/authn/http.md)
- [JWT 认证](../access-control/authn/jwt.md)
- [LDAP 认证](../access-control/authn/ldap.md)

与 MQTT 协议不同，网关仅支持创建一个认证器，而不是认证器列表（或认证链）。当不启用任何认证器时，表示允许所有的 NATS 客户端都具有接入的权限。

NATS 网关使用 NATS 协议的 CONNECT 报文中的信息来生成客户端的认证信息。默认情况下：

- Client ID：为随机生成的字符串。
- Username：为 CONNECT 报文中的 `user` 字段的值。
- Password：为 CONNECT 报文中的 `pass` 字段的值。

#### 通过 Dashboard 配置

本节以使用 HTTP 服务进行密码认证为例，说明如何对 NATS 网关进行接入认证的配置。

1. 在 NATS 网关设置中点击**接入认证**页签。
2. 点击 **+ 添加认证**，选择 **Password-Based** 作为**认证方式**，并选择 **HTTP 服务**作为**数据源**，点击**下一步**。然后在**配置参数**中，您可以设置身份验证规则。对于每一项配置的具体解释，您可以参考[使用 HTTP 服务进行密码认证](../access-control/authn/http.md)。
3. 完成配置后，点击**创建**。在**接入认证**中将出现**HTTP 服务**设置页面。
4. 确认您的设置，点击**更新**。

#### 通过 REST API 配置

以下为通过 REST API 或 `base.hocon` 为 NATS 网关创建一个内置数据库认证的示例：

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
#### 通过配置文件配置

以下为通过 `base.hocon` 为 NATS 网关创建一个内置数据库认证的示例：

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
其他类型的认证器的配置格式参考每种 [EMQX 认证器](../access-control/authn/authn.md#emqx-认证器)的使用文档。

### 配置用户层接口

- 详细配置说明参考：[网关配置 - NATS 网关](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)
- 详细 REST API 接口参考：[REST API - 网关](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs)

## 更多信息

要了解有关 NATS 协议网关及其应用场景的更多信息，请参阅博客：[突破连接边界：EMQX 实现 MQTT 和 NATS 协议双向互通](https://www.emqx.com/zh/blog/emqx-nats-gateway)。
