# MQTT-SN Gateway

## Introduction

The MQTT-SN gateway is based on the [MQTT-SN v1.2](https://www.oasis-open.org/committees/download.php/66091/MQTT-SN_spec_v1.2.pdf).

## Quick Start

In EMQX 5.0, MQTT-SN gateways can be configured and enabled through the Dashboard.

It can also be enabled via the HTTP-API, e.g:
```bash
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

or configured in emqx.conf, e.g:

```properties
gateway.mqttsn {

  mountpoint = "mqtt/sn"

  gateway_id = 1

  broadcast = true

  enable_qos3 = true

  listeners.udp.default {
    bind = 1884
    max_connections = 10240000 max_conn_rate = 1000
  }
}
```

::: tip
Configuring the gateway via emqx.conf requires changes on a per-node basis, but configuring it via Dashboard or the HTTP API will take effect across the cluster.
:::

The MQTT-SN gateway only supports UDP and DTLS type listeners, for a complete list of configurable parameters refer to: [Gateway Configuration - Listeners](../admin/cfg.md)

## Authentication

Since the connection message of MQTT-SN protocol only given the Client ID of Client, there is no Username and Password. Therefore, the MQTT-SN gateway only supports [HTTP Server Authentication](../security/authn/http.md).

The client information generation rules are as follows:
- Client ID: using the Client ID field of CONNECT message.
- Username: undefined
- Password: undefined

For example, to create an HTTP authentication for MQTT-SN gateway via HTTP-API:
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

Or add an HTTP authentication for MQTT-SN gateway via emqx.conf:

```properties
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
## Publish/Subscribe

The MQTT-SN protocol already defines the publish/subscribe behavior, e.g:
- The PUBLISH message of the MQTT-SN protocol is used as a publishing operation, whose topic and QoS are specified by this message.
- The SUBSCRIBE message of the MQTT-SN protocol is used as a subscribing operation, whose topic and QoS are both specified by this message.
- The UNSUBSCRIBE message of the MQTT-SN protocol is used as an unsubscribe operation, whose topic is specified by this message.

There is no special authorization configurations within MQTT-SN gateway, and its permission control for topics needs to be configured [Authorization](../security/authz/authz.md).

## User Interfaces

- Detailed confguration options: [Configuration - Stomp Gateway](../admin/cfg.md)
- Detailed HTTP APIs description: [HTTP API - Gateway](../admin/api.md)

## Client libraries

- [paho.mqtt-sn.embedded-c](https://github.com/eclipse/paho.mqtt-sn.embedded-c)
- [mqtt-sn-tools](https://github.com/njh/mqtt-sn-tools)
