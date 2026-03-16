# CoAP 协议网关

CoAP 网关以 [Publish-Subscribe Broker for the CoAP](https://datatracker.ietf.org/doc/html/draft-ietf-core-coap-pubsub-09) 为标准，实现了发布、订阅、和消息接收功能。

## 快速开始

EMQX 5.0 可以通过 Dashboard 配置并启用 CoAP 网关。

也可以通过 HTTP API 或 emqx.conf 来启用，例如：

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateway/coap' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "coap",
  "enable": true,
  "mountpoint": "coap/",
  "connection_required": false,
  "listeners": [
    {
      "type": "udp",
      "name": "default",
      "bind": "5683",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'
```

:::

::: tab Configuration

```properties
gateway.coap {

  mountpoint = "coap/"

  connection_required = false

  listeners.udp.default {
    bind = "5683"
    max_connections = 1024000
    max_conn_rate = 1000
  }
}
```

:::

::::

::: tip
通过配置文件来配置网关，需要在每个节点上手动同步配置文件；而通过 Dashboard 或者 HTTP API 管理则会自动同步至整个集群。
:::

CoAP 网关支持 UDP、DTLS 类型的监听器，其完整可配置的参数列表可以参考 [EMQX 开源版配置手册](https://docs.emqx.com/zh/emqx/v@CE_VERSION@/hocon/)和 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)中的网关配置 - 监听器。

## 工作模式

CoAP 网关支持 2 种工作模式：
- `无连接模式`：该模式完全遵循 [Publish-Subscribe Broker for the CoAP](https://datatracker.ietf.org/doc/html/draft-ietf-core-coap-pubsub-09) 协议，在该模式下不需要连接创建会话、心跳维持等操作，仅支持：
  * 消息发布
  * 订阅主题
  * 认证

- `连接模式`：该模式下定义了连接认证、会话、和心跳维持等概念。客户端在发布订阅前需要先创建连接，成功连接后客户端将获得会话令牌(Token)，在后续的通信中都需要在 Query String 加入令牌信息。它实现了如下功能:
  * 创建连接
  * 关闭连接
  * 会话保持 (心跳, 可选的)
  * 认证

是否开启 `连接模式` 由 `connection_required` 参数决定，例如

```properties
# emqx.conf
gateway.coap {

  ## true: 开启连接模式
  ## false: 关闭连接模式
  connection_required = true
}
```

:::tip
`连接模式` 仅新增了连接管理相关接口。该模式下仍可以执行发布/订阅、取消订阅等操作，但每次请求需要携带 ClientId 和 Token。
:::


## 认证

客户端 ID、用户名、密码由客户端的[创建连接](#创建连接)请求提供，CoAP 网关支持以下认证器类型：

- [内置数据库认证](../access-control/authn/mnesia.md)
- [MySQL 认证](../access-control/authn/mysql.md)
- [MongoDB 认证](../access-control/authn/mongodb.md)
- [PostgreSQL 认证](../access-control/authn/postgresql.md)
- [Redis 认证](../access-control/authn/redis.md)
- [HTTP Server 认证](../access-control/authn/http.md)
- [JWT 认证](../access-control/authn/jwt.md)
- [LDAP 认证](../access-control/authn/ldap.md)

例如，通过 HTTP API 或 emqx.conf 为 CoAP 网关创建一个内置数据库认证：

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'POST' \
  'http://127.0.0.1:18083/api/v5/gateway/coap/authentication' \
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
gateway.coap {

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


与 MQTT 协议不同，**网关仅支持创建一个认证器，而不是认证器列表（或认证链）**。当不启用任何认证器时，表示所有的 CoAP 客户端都具有接入的权限。

其他类型的认证器的配置格式参考：[安全 - 认证器](../access-control/authn/authn.md)


## 发布订阅

CoAP 网关基于 [Publish-Subscribe Broker for the CoAP](https://datatracker.ietf.org/doc/html/draft-ietf-core-coap-pubsub-09) 协议中定义的请求路径和请求方法
实现了发布订阅的接口。

详情参考下文中的 [消息发布](#消息发布)、[订阅主题](#订阅主题)、[取消订阅](#取消订阅)

网关内无独立的发布订阅的权限控制，其对主题的权限控制需要统一在 [授权（Authorization）](../access-control/authz/authz.md) 中管理。


## 用户层接口

- 详细配置说明参考：[网关配置 - CoAP 网关（开源版）](https://docs.emqx.com/zh/emqx/v@CE_VERSION@/hocon/)和[网关配置 - CoAP 网关（企业版）](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)

- 详细 HTTP API 接口参考：[HTTP API - 网关（开源版）](https://docs.emqx.com/zh/emqx/v@CE_MINOR_VERSION@/admin/api-docs)和 [HTTP API - 网关（企业版）](https://docs.emqx.com/zh/enterprise/v@EE_MINOR_VERSION@/admin/api-docs)

## 客户端库

- [libcoap](https://github.com/obgm/libcoap)
- [californium](https://github.com/eclipse/californium)


## 附录：客户端接入接口说明

### 创建连接

仅在 `连接模式` 下可用。

该接口用于向 CoAP 网关创建客户端连接。当开启 CoAP 网关的认证功能后，网关会对该请求中的 `clientid`, `username`, `password` 进行验证，以防止非法用户登录系统。

**请求参数表：**

- 方法（Method）: `POST`
- 请求路径（URI）：`mqtt/connection{?QueryString*}`，其中 `QueryString` 可用参数为
  - `clientid`：必填参数；UTF8 字符串，网关以该字符串作为该连接的唯一标识。
  - `username`：可选参数，UTF8 字符串，用于连接认证。
  - `password`：可选参数，UTF8 字符串，用于连接认证。
- 消息体（Payload）：为空。

**返回结果：**

- 返回码（Return Code）：
  - `2.01`：连接创建成功，并在消息体中返回本次连接的 Token 字符串。
  - `4.00`：错误的请求格式，并在消息体中返回具体的错误信息。
  - `4.01`：请求格式正确，但登录鉴权失败。并在消息体中返回具体的错误信息。
- 消息体（Payload）：当返回码为 `2.01`时，消息体为 `Token`，否则为 `ErrorMessage`
  - `Token`：用于后续请求使用的令牌字符串。
  - `ErrorMessage`: 错误说明，例如 `Login Failed: not_authorized`

以 `libcoap` 为例：

```bash
# 使用 clientid 为 123， 用户名密码为 admin/public 发起创建连接请求，
# 返回 Token 为 3404490787
coap-client -m post -e "" "coap://127.0.0.1/mqtt/connection?clientid=123&username=admin&password=public"

3404490787
```

::: tip
连接创建成功后，可以使用 Dashboard，HTTP API 或 CLI 检查 CoAP 网关的客户端列表中是否已存在该客户端。
:::


### 断开连接

仅在 `连接模式` 下可用。

该接口用于关闭 CoAP 客户端连接。

**请求参数表：**

- 方法（Method）: `DELETE`
- 请求路径（URI）：`mqtt/connection{?QueryString*}`，其中 `QueryString` 可用参数为
  - `clientid`：必填参数；UTF8 字符串，网关以该字符串作为该连接的唯一标识。
  - `token`：必填参数；使用由创建连接方法返回的 Token 字符串
- 消息体（Payload）：为空。

**返回结果：**

- 返回码（Return Code）：
  - `2.01`：关闭连接成功。
  - `4.00`：错误的请求格式，并在消息体中返回具体的错误信息。
  - `4.01`：请求格式正确，但登录鉴权失败。并在消息体中返回具体的错误信息。
- 消息体（Payload）：当返回码为 `2.01` 时，消息体为空；否则为具体的错误消息

例如：

```bash
coap-client -m delete -e "" "coap://127.0.0.1/mqtt/connection?clientid=123&token=3404490787"
```


### 心跳

仅在 `连接模式` 下可用。

该接口用于维持 CoAP 客户端与网关的连接。当心跳超期后，网关会删除该客户端的会话、订阅关系，并释放所有的资源。

**请求参数表：**

- 方法（Method）: `PUT`
- 请求路径（URI）：`mqtt/connection{?QueryString*}`，其中 `QueryString` 可用参数为
  -  `clientid`：必填参数；UTF8 字符串，网关以该字符串作为该连接的唯一标识。
  - `token`：必填参数；使用由创建连接方法返回的 Token 字符串
- 消息体（Payload）：为空。

**返回结果：**

- 返回码（Return Code）：
  - `2.01`：更新成功。
  - `4.00`：错误的请求格式，并在消息体中返回具体的错误信息。
  - `4.01`：请求格式正确，但登录鉴权失败。并在消息体中返回具体的错误信息。
- 消息体（Payload）：当返回码为 `2.01` 时，消息体为空；否则为具体的错误消息

例如：

```bash
coap-client -m put -e "" "coap://127.0.0.1/mqtt/connection?clientid=123&token=3404490787"
```

:::tip
心跳间隔时间由 CoAP 网关的 `heartbeat` 配置决定，默认为 30 秒
:::


### 消息发布

该接口用于 CoAP 客户端为指定主题发送消息。在 `连接模式` 下需要额外携带身份信息。

**请求参数表：**

- 方法（Method）：`POST`
- 请求路径（URI）：`ps/{+topic}{?QueryString*}`，其中：
  -  `{+topic}` 为需要发布的主题，例如向 `coap/test` 主题发布消息，那么请求路径为 `ps/coap/test`
  - `{?QueryString*}` 为请求参数
    - `clientid`： `连接模式` 下为必填参数，`无连接模式` 下为可选参数。
    - `token`： 仅用于 `连接模式`，必填参数；
    - `retain`：是否作为保留消息进行发布；布尔类型，可选参数，默认为 `false`。
    - `qos`：消息 QoS，用于标识该消息的 QoS 等级，仅影响 MQTT 客户端如何接收该消息；枚举类型，可选值为 `0`、 `1`、 `2`
    - `expiry`：消息超期时间，单位秒；默认为 `0` 表示永不超期
- 消息体（Payload）：消息内容

**返回结果：**

- 返回码（Return Code）：
  - `2.04`：发送成功。
  - `4.00`：错误的请求格式，并在消息体中返回具体的错误信息。
  - `4.01`：请求格式正确，但鉴权失败。并在消息体中返回具体的错误信息。
- 消息体（Payload）：当返回码为 `2.04` 时，消息体为空；否则为具体的错误消息

例如，`无连接模式` 下为 `coap/test` 发送一条消息：

```bash
coap-client -m post -e "Hi, this is libcoap" "coap://127.0.0.1/ps/coap/test"
```

`连接模式` 下则需要携带 `clientid` 和 `token`

```bash
coap-client -m post -e "Hi, this is libcoap" "coap://127.0.0.1/ps/coap/test?clientid=123&token=3404490787"
```


### 订阅主题

该接口用于 CoAP 客户端订阅指定主题。在 `连接模式` 下需要额外携带身份信息。


**请求参数表：**

- 方法（Method）：`GET`
- 选项值（Options）：需设置 `observer` 为 0
- 请求路径（URI）：`ps/{+topic}{?QueryString*}`，其中：
  -  `{+topic}` 为需要订阅主题，例如订阅 `coap/test` 主题，则请求路径为 `ps/coap/test`
  - `{?QueryString*}`为请求参数
    - `clientid`： `连接模式`下为必填参数，`无连接模式`下为可选参数。
    - `token`： 仅用于 `连接模式`，必填参数；
    - `qos`：订阅 QoS，用于指示网关以那种消息类型（`CON` 或 `NON`）来投递后续接收的该消息；枚举类型，可选值为：
      - `0`，`1`，`2`：设置为`qos0`，`qos1` 或 `qos2`。如果设置为 `qos0` 表示该主题上的消息会以 NON 消息进行投递；`qos1`，`qos2` 表示该主题上的消息会以 CON 消息进行投递。

- 消息体（Payload）：空

**返回结果：**

- 返回码（Return Code）：
  - `2.05`：订阅成功。
  - `4.00`：错误的请求格式，并在消息体中返回具体的错误信息。
  - `4.01`：请求格式正确，但鉴权失败。并在消息体中返回具体的错误信息。
- 消息体（Payload）：当返回码为 `2.05` 时，消息体为空；否则为具体的错误消息


例如，`无连接模式` 下订阅主题 `coap/test` ：

```bash
coap-client -m get -s 60 -O 6,0x00 -o - -T "obstoken" "coap://127.0.0.1/ps/coap/test"
```

`连接模式` 下则需要携带 `clientid` 和 `token`：

```bash
coap-client -m get -s 60 -O 6,0x00 -o - -T "obstoken" "coap://127.0.0.1/ps/coap/test?clientid=123&token=3404490787"
```


### 取消订阅

该接口用于 CoAP 客户端取消订阅指定主题。
目前，取消订阅操作仅在 `连接模式` 下可用。

**请求参数表：**

- 方法（Method）：`GET`
- 请求路径（URI）：`ps/{+topic}{?QueryString*}`，其中：
  -  `{+topic}` 为需要取消订阅主题，例如取消订阅 `coap/test` 主题，则请求路径为 `ps/coap/test`
  - `{?QueryString*}`为请求参数
    - `clientid`： `连接模式`下为必填参数，`无连接模式`下为可选参数。
    - `token`： 仅用于 `连接模式`，必填参数；
- 消息体（Payload）：空

**返回结果：**

- 返回码（Return Code）：
  - `2.07`：取消订阅成功。
  - `4.00`：错误的请求格式，并在消息体中返回具体的错误信息。
  - `4.01`：请求格式正确，但鉴权失败。并在消息体中返回具体的错误信息。
- 消息体（Payload）：当返回码为 `2.07` 时，消息体为空；否则为具体的错误消息

例如，`连接模式` 下取消订阅主题 `coap/test` ：

```bash
coap-client -m get -O 6,0x01 "coap://127.0.0.1/ps/coap/test?clientid=123&token=3404490787"
```

### 短参数名称

为节省报文大小，CoAP 网关支持短参数名称。例如，参数 `clientid=barx` 可以写作 `c=bar`。所有支持的短
参数名称见下表：

| 参数名称 |  短参数名称 |
| -------------- | ----------- |
| `clientid`     | `c`         |
| `username`     | `u`         |
| `password`     | `p`         |
| `token`        | `t`         |
| `qos`          | `q`         |
| `retain`       | `r`         |
