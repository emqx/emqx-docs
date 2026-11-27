# MQTT-SN 协议网关

MQTT-SN 网关基于 [MQTT-SN v1.2](https://www.oasis-open.org/committees/download.php/66091/MQTT-SN_spec_v1.2.pdf) 版本实现。

## 快速开始

EMQX 5.0 中，可以通过 Dashboard 配置并启用 MQTT-SN 网关。

也可以通过 HTTP API 或 emqx.conf 来启用，例如：

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateway/mqttsn' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "mqttsn",
  "enable": true,
  "gateway_id": 1,
  "mountpoint": "mqttsn/",
  "listeners": [
    {
      "type": "udp",
      "bind": "1884",
      "name": "default",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'
```
:::

::: tab Configuration

```properties
gateway.mqttsn {

  mountpoint = "mqtt/sn"

  gateway_id = 1

  broadcast = true

  enable_qos3 = true

  listeners.udp.default {
    bind = 1884
    max_connections = 10240000
    max_conn_rate = 1000
  }
}
```
:::

::::

::: tip
注：通过配置文件进行配置网关，需要在每个节点中进行配置；通过 Dashboard 或者 HTTP API 管理则会在整个集群中生效。
:::

MQTT-SN 网关支持 UDP, DTLS 类型的监听器，其完整可配置的参数列表可以参考 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)中的网关配置 - 监听器。

## 认证

MQTT-SN 的 `CONNECT` 报文包含 Client ID，但不包含 Username 和 Password。EMQX 按以下规则生成用于认证和授权的客户端信息：

- 对于发送 `CONNECT` 报文的客户端，Client ID 为报文中 Client ID 字段的值。
- 对于使用 QoS -1 的无连接发布端，Client ID 为 `NegQoS-Client`。
- 默认情况下，这两类客户端的 Username 和 Password 均为空。

可以通过 `clientinfo_override` 配置覆盖 Client ID、Username 或 Password。

认证成功后，EMQX 使用认证后的客户端信息检查发布授权。

例如，通过 HTTP API 或 emqx.conf 为 MQTT-SN 网关创建一个 HTTP 认证：

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'POST' 'http://127.0.0.1:18083/api/v5/gateway/mqttsn/authentication' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "method": "post",
  "url": "http://127.0.0.1:8080",
  "headers": {
    "content-type": "application/json"
  },
  "body": {
    "clientid": "${clientid}"
  },
  "pool_size": 8,
  "connect_timeout": "5s",
  "request_timeout": "5s",
  "enable_pipelining": 100,
  "ssl": {
    "enable": false,
    "verify": "verify_none"
  },
  "backend": "http",
  "mechanism": "password_based",
  "enable": true
}'
```
:::

::: tab Configuration

```properties
gateway.mqttsn {
  authentication {
    backend = "http"
    mechanism = "password_based"
    method = "post"
    connect_timeout = "5s"
    enable_pipelining = 100
    url = "http://127.0.0.1:8080"
    headers {
      "content-type" = "application/json"
    }
    body {
      clientid = "${clientid}"
    }
    pool_size = 8
    request_timeout = "5s"
    ssl.enable = false
  }
}
```
:::

::::


## 发布订阅

MQTT-SN 协议已经定了发布/订阅的行为，MQTT-SN 网关未对其进行额外的定义，例如：
- MQTT-SN 协议的 PUBLISH 报文，作为消息发布，其主题和 QoS 都由该报文指定。
- MQTT-SN 协议的 SUBSCRIBE 报文，作为订阅操作，其主题和 QoS 都由该报文指定。
- MQTT-SN 协议的 UNSUBSCRIBE 报文，作为取消订阅操作，其主题由该报文指定。

网关内无独立的发布订阅的权限控制，其对主题的权限控制需要统一在 [授权（Authorization）](../../guides/access-control/authz/authz.md) 中管理。

从 EMQX 5.9.4 开始，启用 `enable_qos3` 时，EMQX 会对每个来自无连接发布端的 QoS -1 `PUBLISH` 报文执行认证，并在路由消息前检查发布授权。由于消息以 QoS 0 进入 Broker，EMQX 会按 QoS 0 检查发布授权。认证失败或发布授权拒绝该请求时，EMQX 不会路由消息。

::: warning 重要提示

升级到 EMQX 5.9.4 时，如果已启用 `enable_qos3`，请确认认证后端接受为使用 QoS -1 的无连接发布端生成的客户端信息。还需确认授权规则允许认证后的身份以 QoS 0 向目标主题发布消息。

:::

## 用户层接口

- 详细配置说明参考：[网关配置 - MQTT-SN 网关](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)

- 详细 HTTP API 接口参考：[HTTP API - 网关](https://docs.emqx.com/zh/enterprise/v@EE_MINOR_VERSION@/admin/api-docs)

## 客户端库

- [paho.mqtt-sn.embedded-c](https://github.com/eclipse/paho.mqtt-sn.embedded-c)
- [mqtt-sn-tools](https://github.com/njh/mqtt-sn-tools)
