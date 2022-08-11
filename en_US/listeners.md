# Listener

## Introduction

Listener is mainly used to configure listening ports and related parameters of different protocols. EMQX Broker supports configuring multiple Listeners to listen to multiple protocols or ports at the same time.

The following are the supported Listeners:

| Listeners                 | Description                                             |
| ------------------------- | ------------------------------------------------------- |
| TCP Listener              | A listener for MQTT which uses TCP                      |
| SSL Listener              | A secure listener for MQTT which uses TLS               |
| Websocket Listener        | A listener for MQTT over WebSockets                     |
| Secure Websocket Listener | A secure listener for MQTT over secure WebSockets (TLS) |
| QUIC Listener             | A secure listener for MQTT over secure WebSockets       |

EMQX Broker provides 4 Listeners by default, and they will occupy the following ports:

| Port            | Description             |
| ----------------| ----------------------- |
| 1883            | MQTT/TCP protocol port  |
| 8883            | MQTT/SSL protocol port  |
| 8083            | MQTT/WS protocol port   |
| 8084            | MQTT/WSS protocol port  |

## Quick Start

### Configuration

The naming rule of the Listener configuration item is `listener.<Protocol>.<Listener Name>.xxx`, and `<Protocol>` is the protocol used by the Listener. `<Listener Name>` can be named arbitrarily, but it is recommended to use all lowercase words, and `xxx` is a specific configuration item. The `<Listener Name>` of Listeners with different protocols can be repeated.

Due to the existence of the default configuration, we can quickly show how to add a new Listener. Taking TCP Listener as an example, we only need to add the following configuration in `emqx.conf`:

```
listeners.tcp.demo.bind = "0.0.0.0:1883"
```


Of course, we can also configure the listener in a more detailed way, for example:

```
listeners.tcp.demo {
    bind = "0.0.0.0:1883"
    max_connections = 1024000
    proxy_protocol = true
}
```

### API

In addition to supporting settings using configuration, listener also support operations such as adding, deleting, modifying, starting, and stopping through HTTP API, for example:

Add a `TCP` listener which named `demo`:

API: `POST http://127.0.0.1:1883/api/v5/listeners`

```
curl -X 'POST' \
     'http://127.0.0.1:18083/api/v5/listeners' \
     -H 'accept: application/json' \
     -H 'Content-Type: application/json' \
     -d '{
  "acceptors": 16,
  "access_rules": [
    "allow all"
  ],
  "bind": "0.0.0.0:1884",
  "current_connections": 10240,
  "name": "demo",
  "max_connections": 204800,
  "mountpoint": "/",
  "proxy_protocol": false,
  "proxy_protocol_timeout": "3s",
  "running": true,
  "tcp_options": {
    "active_n": 100,
    "backlog": 1024,
    "buffer": "4KB",
    "high_watermark": "1MB",
    "nodelay": false,
    "reuseaddr": true,
    "send_timeout": "15s",
    "send_timeout_close": true
  },
  "type": "tcp",
  "zone": "default"
}'
```

Start the `demo` listener:

API: `POST http://127.0.0.1:1883/api/v5/listeners/{type}:{name}/start`

```
curl -X 'POST' \
     'http://127.0.0.1:18083/api/v5/listeners/tcp%3Ademo/start' \
     -H 'accept: */*' \
     -d ''
```

Stop it:

API: `POST http://127.0.0.1:1883/api/v5/listeners/{type}:{name}/stop`


```
curl -X 'POST' \
     'http://127.0.0.1:18083/api/v5/listeners/tcp%3Ademo/stop' \
     -H 'accept: */*' \
     -d ''
```

Delete it:

API: `POST http://127.0.0.1:1883/api/v5/listeners/{type}:{name}`


```
curl -X 'DELETE' \
     'http://127.0.0.1:18083/api/v5/listeners/tcp%3Ademo' \
     -H 'accept: */*'
```

For more API and details, please see [API](api.md)
