# 认证

身份认证是物联网应用的重要组成部分，可以帮助有效阻止非法客户端的连接。为了提供更好的安全保障，EMQX 支持多种认证机制，此外，EMQX 还支持 TLS 的双向认证（X.509 证书认证）和基于 PSK 的 TLS/DTLS 认证，在一定程度上满足了客户端和服务端之间的身份验证要求。

本章节将向您介绍 EMQX 认证的基本概念和使用方式。

:::tip
EMQX 默认未开启认证功能，即允许所有客户端链接，如在生产环境中使用，请提前配置好至少一种认证方法。
:::

## 认证机制

EMQX 支持的认证机制包括密码认证，JWT 认证，以及基于 MQTT 5.0 协议的增强认证。

### 密码认证

密码认证是最简单，也是使用最多的认证方式。此时，客户端需要提供能够表明身份的凭据，例如用户名、客户端 ID 以及对应的密码，或是 TLS 证书中的一些字段（例如证书公用名称）。这些身份凭据会提前存储到特定数据源（数据库）中，密码通常都会以加盐后散列的形式存储。

EMQX 支持通过密码进行身份验证。启用密码认证后，当客户端尝试连接时，需按要求提供身份凭证信息，EMQX 会在数据库中发起查询，并将返回得到的密码与客户端提供的信息进行匹配，匹配成功后，EMQX 将接受该客户端的连接请求。

<img src="./assets/emqx-authn-flow.png" alt="EMQX 密码认证流程" style="zoom:67%;" />

除简单便捷的内置数据库外，EMQX 还支持通过与多类后端数据库的集成提供密码认证，包括 MySQL、PostgreSQL、MongoDB 和 Redis。

此外，EMQX 也支持通过 HTTP 方式对接用户自己开发的服务，借此实现更复杂的认证逻辑。

### JWT 认证

[JSON Web Token（JWT）](https://jwt.io/) 是一种基于 Token 的认证机制，它不需要服务器来保留客户端的认证信息或会话信息。客户端可以在密码或用户名中携带 Token，EMQX 通过预先配置的密钥或公钥对 JWT 签名进行验证。

此外，如果用户配置了 JWKS 端点，EMQX 也支持通过从 JWKS 端点查询到的公钥列表对 JWT 签名进行验证，从而能够批量为客户端签发认证信息。

### MQTT 5.0 增强认证

[MQTT 5.0 增强认证](https://www.emqx.com/zh/blog/mqtt5-enhanced-authentication)是对密码认证的扩展，增强认证特性允许使用各种更安全的认证机制，例如 SCRAM 认证、Kerberos 认证等。目前 EMQX 具体实施了 SCRAM 认证，并支持将认证数据存储在内置数据库中。

## EMQX 认证器

按照认证方式和数据源来划分，EMQX 内置了以下 8 种认证器：

| 认证方式 | 数据源      | 说明                                                |
| -------- | ----------- | --------------------------------------------------- |
| 密码认证 | 内置数据库  | [使用内置数据库（Mnesia）进行密码认证](./mnesia.md) |
| 密码认证 | MySQL       | [使用 MySQL 进行密码认证](mysql.md)                 |
| 密码认证 | PostgreSQL  | [使用 PostgreSQL 进行密码认证](postgresql.md)       |
| 密码认证 | MongoDB     | [使用 MongoDB 进行密码认证](./mongodb.md)           |
| 密码认证 | Redis       | [使用 Redis 进行密码认证](./redis.md)               |
| 密码认证 | LDAP        | [使用 LDAP 进行密码认证](./ldap.md)                 |
| 密码认证 | HTTP Server | [使用 HTTP 服务进行密码认证](./http.md)             |
| JWT      | --          | [JWT 认证](./jwt.md)                                |
| 增强认证 | 内置数据库  | [MQTT 5.0 增强认证(SCRAM 认证)](./scram.md)         |

## 基本概念

### 认证链

EMQX 允许创建多个认证器构成一条认证链，认证器将按照在链中的位置顺序执行，如果在当前认证器中未检索到身份凭证，将会切换至链上的下一个启用的认证器继续认证。

::: tip

认证链中不允许出现多个相同类型的认证器。同时，只有 MQTT 协议支持认证链，网关仅支持使用单个认证器。

:::

#### 认证流程

以密码认证为例，通常这会产生以下 2 种情况：

1. 当前认证器执行时检索到了匹配的认证信息，例如用户名一致：
   - 密码完全匹配，则客户端认证通过，允许连接。
   - 密码无法匹配，则客户端认证失败，拒绝连接。
2. 当前认证器执行时没有检索到匹配的认证信息，例如数据源中没有查找到数据：
   - 当前认证器之后还有认证器：忽略认证，交由下一认证器继续认证。
   - 当前认证器已经是链中最后一个认证器：客户端认证失败，拒绝连接。

::: tip

请注意，某个认证器查找出错（如数据库错误）或未启用时，将继续执行下一个认证器。

:::

![EMQX 认证链](./assets/authn-chain.png)

#### 应用场景

用户可以创建多个认证器实现链式认证，某些场景下这是有益的，比如客户端数量多、连接速率很高的场景下，用户可能使用 Redis 作为链中的第一个认证器，与 HTTP Server 认证器搭配使用，借助 Redis 高性能优势提供缓存层以实现更高性能的认证能力。

### 监听器认证

默认情况下 EMQX 所有监听器接入的客户端都使用同一种认证方式，从同一个认证数据源中读取数据。但在同时接入多个服务的 EMQX 集群中，用户可能需要根据业务不同为每种接入方式配置不同的认证方式，比如：

- 通过 MQTT over WebSocket 接入的客户端不会颁发永久的用户名密码凭证，而是使用具有时效性的 JWT 进行认证以确保业务安全；
- 通过 MQTT TCP 接入的硬件设备会在初始化时烧录用户名密码或客户端证书，该认证凭证在整个生命周期中不会变化，可以使用密码认证；
- 用于后端服务连接的监听器不需要认证检查，此监听器通常监听于内网地址有足够的安全性。

::: tip

同样，目前只有 MQTT 监听器支持使用认证链，网关监听器仅支持使用单个认证器。

:::

#### 认证器优先级

监听器身份认证器的优先级高于全局身份认证器。监听器会优先匹配自身身份认证器的规则设置，如未设置或已移除，才会使用全局身份认证器。

#### 应用场景

例如，对于启用了 TLS 身份认证的监听器，用户可能不希望应用全局身份认证；此外，如果网络中存在来自不同供应商的客户端设备，也需要启用监听器级别的身份认证来解决设备重名问题。

### 超级用户与权限

通常情况下，认证只是验证了客户端的身份是否合法，而该客户端是否具备发布、订阅某些主题的权限，还需要由授权系统来判断。

EMQX 允许在认证阶段为客户端设置**超级用户**角色以及预设**权限**，用于后续的发布订阅权限检查。

::: tip

权限预设目前仅 JWT 认证支持，允许通过 JWT Payload 携带当前客户端拥有的发布订阅权限列表，并在认证成功后预设到客户端。

:::

超级用户的判定发生在认证阶段，由数据库查询结果、HTTP 响应或者 JWT 声明中的 `is_superuser` 字段来指示。

### 密码散列

通过明文存储密码会有非常高的密码泄漏风险。因此 EMQX 支持多种密码散列算法以满足不同用户的安全性要求，同时我们建议生成一个随机的盐，数据库中存储盐与对密码加盐后散列得到的值（password_hash）。

#### 工作原理

密码散列算法配置在认证时的原理如下：

1. 认证器使用配置的查询语句从数据库中查询符合条件的身份凭证，包括散列密码和盐值；
2. 根据认证器配置的散列算法和查询到的盐值，对客户端连接时提供的密码进行散列；
3. 将第 1 步从数据库查询到的散列密码和第 2 步计算出的散列值进行比较，一致则说明客户端的身份合法。

以下为 EMQX 目前支持的散列算法：

```hcl
# simple algorithms
password_hash_algorithm {
  name = sha256             # plain, md5, sha, sha512
  salt_position = suffix    # prefix, disable
}

# bcrypt
password_hash_algorithm {
  name = bcrypt
}

# pbkdf2
password_hash_algorithm {
  name = pbkdf2
  mac_fun = sha256          # md4, md5, ripemd160, sha, sha224, sha384, sha512
  iterations = 4096
  dk_length = 256           # optional
}
```

### 认证占位符

EMQX 允许使用占位符动态构造认证数据查询语句、HTTP 请求，占位符会在认证器执行时替换为真实的客户端信息，以构造出与当前客户端匹配的查询语句或 HTTP 请求。

以 MySQL 认证器为例，默认的查询 SQL 中使用了 `${username}` 占位符：

```sql
SELECT password_hash, salt FROM mqtt_user where username = ${username} LIMIT 1
```

当用户名为 `emqx_u` 的客户端连接认证时，实际执行认证数据查询 SQL 将被替换为：

```sql
SELECT password_hash, salt FROM mqtt_user where username = 'emqx_u' LIMIT 1
```

目前 EMQX 支持以下占位符：

- `${clientid}`: 将在运行时被替换为客户端 ID。客户端 ID 一般由客户端在 `CONNECT` 报文中显式指定，如果启用了 `use_username_as_clientid` 或 `peer_cert_as_clientid`，则会在连接时被用户名、证书中的字段或证书内容所覆盖。

- `${username}`: 将在运行时被替换为用户名。用户名来自 `CONNECT` 报文中的 `Username` 字段。如果启用了 `peer_cert_as_username`，则会在连接时被证书中的字段或证书内容所覆盖。

- `${password}`: 将在运行时被替换为密码。密码来自 `CONNECT` 报文中的 `Password` 字段。

- `${peerhost}`: 将在运行时被替换为客户端的 IP 地址。EMQX 支持 [Proxy Protocol](http://www.haproxy.org/download/1.8/doc/proxy-protocol.txt)，即使 EMQX 部署在某些 TCP 代理或负载均衡器之后，用户也可以使用此占位符获得真实 IP 地址。

- `${cert_subject}`: 将在运行时被替换为客户端 TLS 证书的主题（Subject），仅适用于 TLS 连接。

- `${cert_common_name}`: 将在运行时被替换为客户端 TLS 证书的通用名称（Common Name），仅适用于 TLS 连接。

## 配置方式

EMQX 提供了 3 种使用认证的配置方式，分别为：Dashboard、配置文件和 HTTP API。

### Dashboard

Dashboard 底层调用了 HTTP API，提供了相对更加易用的可视化操作页面。在 Dashboard 中可以方便的查看认证器状态、调整认证器在认证链中的位置，如下图所示，我们已经成功添加了基于内置数据库和 JWT 两种认证机制。

![Dashboard 认证器列表](./assets/authn-dashboard-2.png)

### 配置文件

EMQX 支持为 MQTT 客户端配置多个认证器以组成认证链 <!--连接到对应概念-->，如以下代码示例中的 `authentication` 字段所示，认证器在数组中的顺序便是在认证链中执行的顺序：

```hcl
# emqx.conf

# Specific global authentication chain for all MQTT listeners
authentication = [
  ...
]

listeners.tcp.default {
  ...
  enable_authn = true
  # Specific authentication chain for the specified MQTT listener
  authentication = [
    ...
  ]
}

gateway.stomp {
  ...
  enable_authn = true
  # Specific global authenticator for all STOMP listeners
  authentication = {
    ...
  }

}
```

不同类型的认证器有着不同的配置项要求。关于各配置项的具体配置方法，可参考配置说明文档 <!--连接到对应文件-->，其中包含了每种认证器的所有配置字段的详细说明。

### HTTP API

<!-- TODO 链接到 API 文档具体 API 上-->

与配置文件相比，HTTP API 支持运行时更新，能够自动将配置改动同步至整个集群，使用起来更加方便。

EMQX 提供的认证 API 允许对认证链和认证器进行管理，例如为全局认证创建一个认证器，以及更新指定认证器的配置。

- `/api/v5/authentication`: 管理 MQTT 全局认证
- `/api/v5/gateway/{protocol}/authentication`: 管理网关的全局认证
- `/api/v5/gateway/{protocol}/listeners/{listener_id}/authentication`: 管理网关监听器认证

#### **认证器 ID**

如果想要对指定认证器进行操作，则需要在上面这些端点后面追加一个认证器 ID，例如 `/api/v5/authentication/{id}`。为了便于维护，这里的 ID 并不是 EMQX 自动生成然后由 API 返回的，而是遵循了一套预先定义的规范：

```bash
<mechanism>:<backend>
```

或者仅仅只有：

```bash
<mechanism>
```

例如：

1. `password_based:built_in_database`
2. `jwt`
3. `scram:built_in_database`

同样，对于监听器 ID，我们也有一套类似的约定，MQTT 监听器 ID 的格式为：

```bash
<transport_protocol>:<name>
```

网关监听器 ID 的格式为：

```bash
<protocol>:<transport_protocol>:<name>
```

我们可以把 MQTT 监听器 ID 看作是默认省略了最前面的协议名。

注意，不管是认证器 ID，还是监听器 ID，当它们在 URL 中使用时，都需要遵循 URL 编码规范。最直接的，我们需要将 `:` 替换为 `%3A`，示例：

```bash
PUT /api/v5/authentication/password_based%3Abuilt_in_database
```

#### **数据操作 API**

对于通过内置数据库存储认证数据的认证方式，例如 [使用内置数据库进行密码认证](./mnesia.md) 和 [MQTT 5.0 增强认证](./scram.md)，EMQX 提供了相关的 HTTP API 来管理认证数据，如创建、更新、删除和查看等操作，具体可阅读 [通过 HTTP API 管理用户](./user_management.md)。

详细的请求方式与参数请参考 [HTTP API](../../admin/api.md)。

