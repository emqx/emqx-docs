# 监听器

## 简介

监听器主要用于配置不同协议的监听端口和相关参数，EMQX 支持配置多个监听器以同时监听多个协议或端口。

以下是支持的协议类型：

| 监听器                     | 说明                                             |
| ------------------------- | ------------------------------------------------------- |
| TCP Listener              | A listener for MQTT which uses TCP                      |
| SSL Listener              | A secure listener for MQTT which uses TLS               |
| Websocket Listener        | A listener for MQTT over WebSockets                     |
| Secure Websocket Listener | A secure listener for MQTT over secure WebSockets (TLS) |
| QUIC Listener             | A secure listener for MQTT over secure WebSockets       |

EMQX 默认提供 4 个监听器，它们将占用以下端口：

| 端口             | 说明                    |
| ----------------| ----------------------- |
| 1883            | MQTT/TCP protocol port  |
| 8883            | MQTT/SSL protocol port  |
| 8083            | MQTT/WS protocol port   |
| 8084            | MQTT/WSS protocol port  |

### 通过配置进行添加

监听器配置项的命名规则为 `listener.<Protocol>.<Listener Name>.xxx`， `<Protocol>` 即 Listener 使用的协议。 `<Listener Name>` 可以随意命名，但建议是全小写的英文单词， `xxx` 则是具体的配置项。不同协议的 Listener 的 `<Listener Name>` 可以重复。

由于默认配置的存在，我们能够非常快速地添加新的监听器，以 TCP 监听器为例，我们只需要在 `emqx.conf` 中添加以下一条配置即可：

```
listeners.tcp.demo.bind = "0.0.0.0:1883"
```

当然，我们也可以使用更加详细的方式对监听器进行配置，例如：

```
listeners.tcp.demo {
    bind = "0.0.0.0:1883"
    max_connections = 1024000
    proxy_protocol = true
}
```

### 使用 API 进行操作

监听器除了支持使用配置文件进行设置外，还支持通过 HTTP API 进行添加、删除、修改、启动、停止等操作，例如：

添加一个名为 `demo` 的 TCP 监听器:

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

启动 `demo`:

API: `POST http://127.0.0.1:1883/api/v5/listeners/{type}:{name}/start`

```
curl -X 'POST' \
     'http://127.0.0.1:18083/api/v5/listeners/tcp%3Ademo/start' \
     -H 'accept: */*' \
     -d ''
```

停止:

API: `POST http://127.0.0.1:1883/api/v5/listeners/{type}:{name}/stop`

```
curl -X 'POST' \
     'http://127.0.0.1:18083/api/v5/listeners/tcp%3Ademo/stop' \
     -H 'accept: */*' \
     -d ''
```

删除:

API: `POST http://127.0.0.1:1883/api/v5/listeners/{type}:{name}`

```
curl -X 'DELETE' \
     'http://127.0.0.1:18083/api/v5/listeners/tcp%3Ademo' \
     -H 'accept: */*'
```

更详细的监听器 API 信息，请参见 [API 文档](./admin/api.md)
