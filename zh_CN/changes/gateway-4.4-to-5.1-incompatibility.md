# 网关从 EMQX 4.4 到 EMQX 5.1 的不兼容变更

本页介绍了 EMQX 4.4 和 EMQX 5.1 之间网关功能配置的兼容性信息。

## 通用的不兼容变更

### 配置

在 EMQX 4.x 版本中，网关可以通过配置文件进行配置，例如 `etc/plugins/emqx_stomp.conf`，或通过 Dashboard 上的模块（即使用 `POST http://127.0.0.1:18084/api/v4/modules` 接口）进行配置。

在 EMQX 5.1 版本中，所有的网关配置都可以在 `etc/emqx.conf` 中进行，或者通过 `PUT http://127.0.0.1:18083/api/v5/gateways/coap` 进行配置。

EMQX 4.x 中的配置示例：

```
stomp.listener = 61613
stomp.listener.acceptors = 4
stomp.listener.max_connections = 512
#stomp.listener.ssl = off
stomp.default_user.login = guest
stomp.default_user.passcode = guest
stomp.allow_anonymous = true

stomp.frame.max_headers = 10
stomp.frame.max_header_length = 1024
stomp.frame.max_body_length = 8192
```

EMQX 5.x 中的配置示例：

```
gateway.stomp {
    mountpoint = "stomp/"
    frame {
        max_headers = 10
        max_headers_length = 1024
        max_body_length = 65536
    }
    listeners.tcp.default {
        bind = "0.0.0.0:61613"
    }
}
```

### 通过 HTTP API 或 Dashboard 进行管理

在 EMQX 4.x 版本中，没有单独的 HTTP API 和网页用于管理。例如，如果您想查询 MQTT-SN 的设备列表，需要使用 `GET http://127.0.0.1:8081/api/v4/clients?protocol=mqtt-sn`。这与查询 MQTT 设备（甚至其他协议，如 CoAP、LwM2M 等）的接口是合并在一起的。

而在 EMQX 5.x 版本中，我们提供了更多专有的接口来完成这些功能。例如，`GET /api/v5/gateways/mqttsn/clients`，以及更多新增的 HTTP API：

- [Gateways](https://docs.emqx.com/zh/emqx/v5.0/admin/api-docs.html#tag/Gateways)
- [Gateway-Authentication](https://docs.emqx.com/zh/emqx/v5.0/admin/api-docs.html#tag/Gateway-Authentication)
- [Gateway-Clients](https://docs.emqx.com/zh/emqx/v5.0/admin/api-docs.html#tag/Gateway-Clients)

此外，还提供了专用的 Dashboard 页面，用于管理客户端、网关配置、监听器等功能。

### 监听器

在 EMQX 4.x 版本中，每个网关都有自己的监听器配置格式。然而，在 EMQX 5.1 版本中，所有监听器的配置格式已经标准化。

EMQX 4.x 中的配置示例：

```
## etc/plugins/emqx_stomp.conf
stomp.listener = 61613
stomp.listener.acceptors = 4
stomp.listener.max_connections = 512
## etc/plugins/emqx_sn.conf
mqtt.sn.port = 1884
## etc/plugins/emqx_coap.conf
coap.bind.udp.1 = 0.0.0.0:5683
coap.bind.dtls.1 = 0.0.0.0:5684
## etc/plugins/emqx_lwm2m.conf
lwm2m.bind.udp.1 = 0.0.0.0:5683
lwm2m.bind.dtls.1 = 0.0.0.0:5684
```

在 EMQX 5.x 版本中，所有协议网关的监听器配置都采用相同的格式。以 Exproto 网关为例：

```
## etc/emqx.conf
gateway.exproto {
    listeners.tcp.default {
        bind = "0.0.0.0:7993"
    }
    listeners.ssl.default {
        bind = "0.0.0.0:7994"
        cacertfile = ..
        certfile = ..
        keyfile = ..
    }
    listeners.udp.default { ... }
    listeners.dtls.default { ... }
}
```

### 认证

在 EMQX 4.x 版本中，每个网关都使用混合认证来配置 MQTT。

但在 EMQX 5.0 版本中，您需要为每个网关配置单独的认证器。例如：

```
gateway.coap {
    ...
    authentication {
      backend = "http"
      method = "post"
      url = "http://127.0.0.1:8080/auth"
      headers {"content-type" = "application/json"}
      body {password = "${password}", username = "${username}"}
    }
}
```

## 网关协议功能和配置项的不兼容变更

### Stomp

`stomp.default_user.login`, `stomp.default_user.passcode` 和 `stomp.allow_anonymous` 在 EMQX 5.x 中被移除。

### MQTT-SN

- DTLS 类型的监听器在 EMQX 5.1中支持，但在 EMQX 4.x 中不支持。
- `mqtt.sn.username`， `mqtt.sn.password` 和 `mqtt.sn.subs_resume` 没有被移除。
- `mqtt.sn.advertise_duration` 重命名为 `gateway.mqttsn.broadcast`。

### ExProto

之前，ConnectionAdapter 服务的配置格式为：

```
exproto.server.http.port = 9100
exproto.server.https.port = 9101
exproto.server.https.cacertfile = etc/certs/cacert.pem
exproto.server.https.certfile = etc/certs/cert.pem
exproto.server.https.keyfile = etc/certs/key.pem
```

现在为：

```
gateway.exproto {
  server {
    bind = "0.0.0.0:9100"
    ssl_options {verify = "verify_none"}
  }
}
```

之前，ConnectionHandler 在监听器上配置：

```
exproto.listener.protoname.connection_handler_url = http://127.0.0.1:9001
#exproto.listener.protoname.connection_handler_certfile =
#exproto.listener.protoname.connection_handler_cacertfile =
#exproto.listener.protoname.connection_handler_keyfile =
```

现在，配置为：

```
gateway.exproto {
  handler {
    address = "http://127.0.0.1:9001"
    ssl_options {enable = false}
  }
}
```

这意味着在 5.0 版本中，无法为每个监听端口指定不同的 ConnectionHandler 服务地址。

### CoAP

对CoAP 协议的实现规范进行了完全重新设计。

请参考 [CoAP](../gateway/coap.md) 以了解新设计的内容。

### LwM2M

之前，配置项格式为：

```
lwm2m.topics.command = dn/#
lwm2m.topics.response = up/resp
lwm2m.topics.notify = up/notify
lwm2m.topics.register = up/resp
lwm2m.topics.update = up/resp
```

现在，格式为：

```
gateway.lwm2m {
  translators {
    command {qos = 0, topic = "dn/#"}
    notify {qos = 0, topic = "up/notify"}
    register {qos = 0, topic = "up/resp"}
    response {qos = 0, topic = "up/resp"}
    update {qos = 0, topic = "up/update"}
  }
}
```