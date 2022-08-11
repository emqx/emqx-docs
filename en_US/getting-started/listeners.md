<a id="org198b209"></a>

# Listener


<a id="orgd4251a1"></a>

## Introduction

Listener is mainly used to configure listening ports and related parameters of different protocols. EMQX Broker supports configuring multiple Listeners to listen to multiple protocols or ports at the same time. 

The following are the supported Listeners:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col class="org-left" />

<col class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Listener</th>
<th scope="col" class="org-left">Description</th>
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

EMQX Broker provides 4 Listeners by default, and they will occupy the following ports:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col class="org-right" />

<col class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-right">Port</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-right">8883</td>
<td class="org-left">MQTT/SSL protocol port</td>
</tr>


<tr>
<td class="org-right">1883</td>
<td class="org-left">MQTT/TCP protocol port</td>
</tr>


<tr>
<td class="org-right">8083</td>
<td class="org-left">MQTT/WS protocol port</td>
</tr>


<tr>
<td class="org-right">8084</td>
<td class="org-left">MQTT/WSS protocol port</td>
</tr>
</tbody>
</table>


<a id="org69700c5"></a>

## Quick Start


<a id="org67b1907"></a>

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

<a id="org796d08a"></a>

### API

In addition to supporting settings using configuration, listener also support operations such as adding, deleting, modifying, starting, and stopping through HTTP API, for example:

Add a listener which named `demo`:

```
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
```

Start the `demo` listener:

```
    curl -X 'POST' \
         'http://127.0.0.1:18083/api/v5/listeners/tcp%3Ademo/start' \
         -H 'accept: */*' \
         -d ''
```

Stop it:

```
    curl -X 'POST' \
         'http://127.0.0.1:18083/api/v5/listeners/tcp%3Ademo/stop' \
         -H 'accept: */*' \
         -d ''
```

Delete it:

```
    curl -X 'DELETE' \
         'http://127.0.0.1:18083/api/v5/listeners/tcp%3Ademo' \
         -H 'accept: */*'
```

There is more detailed listener API information in the API documentation, you can check it for more information.