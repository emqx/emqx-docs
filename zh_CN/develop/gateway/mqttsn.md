# MQTT-SN 协议网关

MQTT-SN 网关基于 [MQTT-SN v1.2](https://www.oasis-open.org/committees/download.php/66091/MQTT-SN_spec_v1.2.pdf) 版本实现。

## 快速开始

EMQX 5.0 中，可以通过 Dashboard 配置并启用 MQTT-SN 网关。

也可以通过 REST API 或 base.hocon 来启用，例如：

:::: tabs type:card

::: tab REST API

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
注：通过配置文件进行配置网关，需要在每个节点中进行配置；通过 Dashboard 或者 REST API 管理则会在整个集群中生效。
:::

MQTT-SN 网关支持 UDP, DTLS 类型的监听器，其完整可配置的参数列表可以参考 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)中的网关配置 - 监听器。

## 认证

由于 MQTT-SN 协议的连接报文只定义了 Client ID 的概念，没有 Username 和 Password 。所以 MQTT-SN 网关目前仅支持 [HTTP Server 认证](../../guides/access-control/authn/http.md)

其客户端信息生成规则如下：
- Client ID：为 CONNECT 报文中的 Client ID 字段。
- Username：默认为空
- Password：默认为空

例如，通过 REST API 或 base.hocon 为 MQTT-SN 网关创建一个 HTTP 认证：

:::: tabs type:card

::: tab REST API

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


## 休眠客户端与会话恢复

MQTT-SN 客户端通过发送 Duration 非零的 `DISCONNECT` 报文进入 `asleep` 状态，之后通过发送携带自身 Client ID 的
`PINGREQ` 报文唤醒，网关随即恢复原有会话，并投递休眠期间缓存的消息。由于 MQTT-SN 设备通常位于 NAT 之后，
休眠前后的源 IP 地址和端口可能发生变化。

MQTT-SN 的 `PINGREQ` 报文不包含密码或令牌。因此在会话没有绑定传输层身份的监听器上，网关只能依据 Client ID
进行匹配。这带来一个需要注意的安全影响：

::: warning 没有客户端证书时会话恢复不经过认证
在明文 UDP 监听器上，以及在客户端未提供证书的 DTLS 监听器上，携带已知 Client ID 的 `PINGREQ` 报文可以在未经
认证的情况下恢复该客户端的休眠会话。任何能够访问该监听器并且知道或猜到 Client ID 的人，都可以收取为该设备
缓存的消息。请仅在可以接受该风险的场景中使用这类监听器。
:::

如需将会话恢复绑定到已验证的身份，请使用 DTLS 监听器并同时设置：

- **TLS Verify**（`verify`）为 `verify_peer`，要求客户端提供证书。默认值为 `verify_none`。
- **Fail If No Peer Cert**（`fail_if_no_peer_cert`）为 `true`，拒绝提供空证书的客户端。

两项同时设置后，网关会将每个会话绑定到连接时客户端提供的证书。来自新连接的 `PINGREQ` 只有在提供相同证书时
才能恢复该会话；未提供证书、提供不同证书或提供重新签发证书的唤醒请求都会被拒绝。源 IP 地址和端口的变化不影响
该校验，因此 NAT 重新绑定仍可正常工作。证书轮换后，客户端必须通过 `CONNECT` 重新连接，并完成正常的认证与
会话接管流程。

仅设置 **TLS Verify** 并不足够。当 **Fail If No Peer Cert** 仍为 `false` 时，客户端可以在不提供证书的情况下
连接，该客户端的会话将退回到仅依据 Client ID 恢复的行为。


## 发布订阅

MQTT-SN 协议已经定了发布/订阅的行为，MQTT-SN 网关未对其进行额外的定义，例如：
- MQTT-SN 协议的 PUBLISH 报文，作为消息发布，其主题和 QoS 都由该报文指定。
- MQTT-SN 协议的 SUBSCRIBE 报文，作为订阅操作，其主题和 QoS 都由该报文指定。
- MQTT-SN 协议的 UNSUBSCRIBE 报文，作为取消订阅操作，其主题由该报文指定。

网关内无独立的发布订阅的权限控制，其对主题的权限控制需要统一在 [授权（Authorization）](../../guides/access-control/authz/authz.md) 中管理。

## 用户层接口

- 详细配置说明参考：[网关配置 - MQTT-SN 网关](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)

- 详细 REST API 接口参考：[REST API - 网关](https://docs.emqx.com/zh/enterprise/v@EE_MINOR_VERSION@/admin/api-docs)

## 客户端库

- [paho.mqtt-sn.embedded-c](https://github.com/eclipse/paho.mqtt-sn.embedded-c)
- [mqtt-sn-tools](https://github.com/njh/mqtt-sn-tools)
