# Gateway Incompatibility Between EMQX 4.4 and EMQX 5.1

This page presents the compatibility information for gateway configurations between EMQX 4.4 and EMQX 5.1.

## Common Incompatibility Changes

### Configuration

In EMQX 4.x, the gateway can be configured through configuration files such as `etc/plugins/emqx_stomp.conf` or via modules on the Dashboard (i.e., using the `POST http://127.0.0.1:18084/api/v4/modules` interface).

In EMQX 5.1, all gateway can be configured in `etc/emqx.conf` or `PUT http://127.0.0.1:18083/api/v5/gateways/coap`.

For example, in EMQX 4.x:

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

In EMQX 5.x:

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

### Management via HTTP API or Dashboard

In EMQX 4.x, there is no separate HTTP API and webpage for management. For example, if you want to query the device list for MQTT-SN, you need to use `GET http://127.0.0.1:8081/api/v4/clients?protocol=mqtt-sn`. It is combined with the query interface for MQTT devices (even other protocols, i.e. CoAP, LwM2M, etc.).

In EMQX 5.x, we provided more proprietary interfaces to accomplish these functions. For example, `GET /api/v5/gateways/mqttsn/clients`, and more newly added HTTP APIs:

- [Gateways](https://docs.emqx.com/en/enterprise/v5.0/admin/api-docs.html#tag/Gateways) 
- [Gateway-Authentication](https://docs.emqx.com/en/enterprise/v5.0/admin/api-docs.html#tag/Gateway-Authentication)
- [Gateway-Clients](https://docs.emqx.com/en/enterprise/v5.0/admin/api-docs.html#tag/Gateway-Clients) 

It also provides dedicated Dashboard pages for managing clients, gateway configurations, listeners, and more.

### Listeners

In EMQX 4.x, each gateway has its own format for listener configuration. However, in EMQX 5.1, the configuration format for all listeners has been standardized. 

For example, in EMQX 4.x

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

In EMQX 5.x, all protocol gateways have the same format for the listener configuration. Take the Exproto gateway as an example:

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

### Authentication

In version EMQX 4.x, each gateway is configured with a hybrid authentication for MQTT.

But in EMQX 5.0, you need to configure a separate authenticator for each gateway. For example:

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

## Incompatibility in Protocol Functionality and Configuration Items

### Stomp

`stomp.default_user.login`, `stomp.default_user.passcode` and `stomp.allow_anonymous` are removed in EMQX 5.x.

### MQTT-SN

- Listeners of DTLS type are supported in EMQX 5.1, but not in EMQX 4.x.
- `mqtt.sn.username`, `mqtt.sn.password` and `mqtt.sn.subs_resume` are not removed.
- `mqtt.sn.advertise_duration` is renamed to `gateway.mqttsn.broadcast`.

### ExProto

Previously, the ConnectionAdapter service configuration format was:

```
exproto.server.http.port = 9100
exproto.server.https.port = 9101
exproto.server.https.cacertfile = etc/certs/cacert.pem
exproto.server.https.certfile = etc/certs/cert.pem
exproto.server.https.keyfile = etc/certs/key.pem
```

Now, it is:

```
gateway.exproto {
  server {
    bind = "0.0.0.0:9100"
    ssl_options {verify = "verify_none"}
  }
}
```

Previously, the ConnectionHandler configuration was on listeners:

```
exproto.listener.protoname.connection_handler_url = http://127.0.0.1:9001
#exproto.listener.protoname.connection_handler_certfile =
#exproto.listener.protoname.connection_handler_cacertfile =
#exproto.listener.protoname.connection_handler_keyfile =
```

Now, the configuration is:

```
gateway.exproto {
  handler {
    address = "http://127.0.0.1:9001"
    ssl_options {enable = false}
  }
}
```

This means that in version 5.0, it is not possible to specify different ConnectionHandler service addresses for each listening port.

### CoAP

The implementation specification for the CoAP protocol is completely redesigned.

Refers to the [CoAP](../gateway/coap.md) for the new design.

### LwM2M

Previously, the option structure was:

```
lwm2m.topics.command = dn/#
lwm2m.topics.response = up/resp
lwm2m.topics.notify = up/notify
lwm2m.topics.register = up/resp
lwm2m.topics.update = up/resp
```

Now, the structure is:

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