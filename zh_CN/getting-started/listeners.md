<a id="org198b209"></a>

# 监听器


<a id="orgd4251a1"></a>

## 简介

监听器主要用于配置不同协议的监听端口和相关参数，EMQX 支持配置多个 Listener 以同时监听多个协议或端口。

以下是支持的协议类型：

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">监听器</th>
<th scope="col" class="org-left">说明</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">TCP Listener</td>
<td class="org-left">A listener for MQTT which uses TCP</td>
</tr>


<tr>
<td class="org-left">SSL Listener</td>
<td class="org-left">A secure listener for MQTT which uses TLS</td>
</tr>


<tr>
<td class="org-left">Websocket Listener</td>
<td class="org-left">A listener for MQTT over WebSockets</td>
</tr>


<tr>
<td class="org-left">Secure Websocket Listener</td>
<td class="org-left">A secure listener for MQTT over secure WebSockets (TLS)</td>
</tr>


<tr>
<td class="org-left">QUIC Listener</td>
<td class="org-left">A listener for MQTT over Quick UDP Internet Connections</td>
</tr>
</tbody>
</table>

EMQX 默认提供 4 个监听器，它们将占用以下端口：

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-right">端口</th>
<th scope="col" class="org-left">说明</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-right">8883</td>
<td class="org-left">MQTT/SSL 协议端口</td>
</tr>


<tr>
<td class="org-right">1883</td>
<td class="org-left">MQTT/TCP 协议端口</td>
</tr>


<tr>
<td class="org-right">8083</td>
<td class="org-left">MQTT/WS 协议端口</td>
</tr>


<tr>
<td class="org-right">8084</td>
<td class="org-left">MQTT/WSS 协议端口</td>
</tr>
</tbody>
</table>


<a id="org69700c5"></a>

## 快速开始


<a id="org67b1907"></a>

### 通过配置进行添加

监听器配置项的命名规则为 `listener.<Protocol>.<Listener Name>.xxx`， `<Protocol>` 即 Listener 使用的协议。 `<Listener Name>` 可以随意命名，但建议是全小写的英文单词， `xxx` 则是具体的配置项。不同协议的 Listener 的 `<Listener Name>` 可以重复。

由于默认配置的存在，我们能够非常快速地展示如何添加新的监听器，以 TCP 监听器为例，我们只需要在 `emqx.conf` 中添加以下一条配置即可：

    listeners.tcp.demo.bind = "0.0.0.0:1883"

当然，我们也可以使用更加详细的方式对监听器进行配置，例如：

    listeners.tcp.demo {
        bind = "0.0.0.0:1883"
        max_connections = 1024000
        proxy_protocol = true
    }


<a id="org796d08a"></a>

### 使用 API 进行操作

监听器除了支持使用配置文件进行设置外，还支持通过 HTTP API 进行添加、删除、修改、启动、停止等操作，例如：

添加一个名为 `demo` 的监听器:

    curl -X 'POST' \
         'http://127.0.0.1:18083/api/v5/listeners/tcp%3Ademo' \
         -H 'accept: application/json' \
         -H 'Content-Type: application/json' \
         -d '{
      "acceptors": 16,
      "access_rules": [
        "allow all"
      ],
      "bind": "0.0.0.0:1884",
      "current_connections": 10240,
      "id": "tcp:demo",
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

启动 `demo`:

    curl -X 'POST' \
         'http://127.0.0.1:18083/api/v5/listeners/tcp%3Ademo/start' \
         -H 'accept: */*' \
         -d ''

停止:

    curl -X 'POST' \
         'http://127.0.0.1:18083/api/v5/listeners/tcp%3Ademo/stop' \
         -H 'accept: */*' \
         -d ''

删除:

    curl -X 'DELETE' \
         'http://127.0.0.1:18083/api/v5/listeners/tcp%3Ademo' \
         -H 'accept: */*'

在 API 文档中有，有更多和更详细的监听器 API 信息，这里就不再一一列举了。
