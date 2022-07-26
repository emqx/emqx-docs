# MQTT-SN 协议网关

## 简介

MQTT-SN 网关基于 [MQTT-SN v1.2](https://www.oasis-open.org/committees/download.php/66091/MQTT-SN_spec_v1.2.pdf) 版本实现。

## 快速开始

EMQX 5.0 中，可以通过 Dashboard 配置并启用 MQTT-SN 网关。

也可以通过 HTTP-API 来启用，例如：
```
curl -X 'POST' 'http://127.0.0.1:18083/api/v5/gateway' \
  -u admin:public \
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

或在 emqx.conf 中配置启用，例如：

```hocon
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

::: tip
注：通过配置文件进行配置网关，需要在每个节点中进行配置；通过 Dashboard 或者 HTTP API 管理则会在整个集群中生效。
:::

MQTT-SN 网关支持 UDP, DTLS 类型的监听器，其完整可配置的参数列表参考：[网关配置 - 监听器](../admin/cfg.md)

## 认证

由于 MQTT-SN 协议的连接报文只定义了 Client ID 的概念，没有 Username 和 Password 。所以 MQTT-SN 网关目前仅支持 [HTTP Server 认证](../security/authn/http.md)

其客户端信息生成规则如下：
- Client ID：为 CONNECT 报文中的 Client ID 字段。
- Username：默认为空
- Password：默认为空

例如，通过 HTTP-API 为 MQTT-SN 网关创建一个 HTTP 认证：
```bash
curl -X 'POST' 'http://127.0.0.1:18083/api/v5/gateway/mqttsn/authentication' \
  -u admin:public \
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

或通过配置文件，为 MQTT-SN 网关添加一个 HTTP 认证：

```hocon
gateway.mqttsn {
  authentication {
    enable = true
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

## 发布订阅

MQTT-SN 协议已经定了发布/订阅的行为，MQTT-SN 网关未对其进行额外的定义，例如：
- MQTT-SN 协议的 PUBLISH 报文，作为消息发布，其主题和 QoS 都由该报文指定。
- MQTT-SN 协议的 SUBSCRIBE 报文，作为订阅操作，其主题和 QoS 都由该报文指定。
- MQTT-SN 协议的 UNSUBSCRIBE 报文，作为取消订阅操作，其主题由该报文指定。

网关内无独立的发布订阅的权限控制，其对主题的权限控制需要统一在 [授权（Authorization）](../security/authz/authz.md) 中管理。

## 用户层接口

- 详细配置说明参考：[网关配置 - MQTT-SN 网关](../admin/cfg.md)
- 详细 HTTP API 接口参考：[HTTP API - 网关](../admin/api.md)

## 客户端库

- [paho.mqtt-sn.embedded-c](https://github.com/eclipse/paho.mqtt-sn.embedded-c)
- [mqtt-sn-tools](https://github.com/njh/mqtt-sn-tools)
