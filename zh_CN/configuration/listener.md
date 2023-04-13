# Listeners

In EMQX, listener is configured to receive requests from MQTT clients. EMQX supports the following message transfer protocols, including:

- TCP: port  `1883`
- SSL: port `8883`
- Websocket listener: `8083`
- Secure websocket listener: `8084`

## Configure TCP Listener

TCP listener is a network service that listens for incoming TCP connections on a specific network port. It plays an essential role in establishing and managing connections between clients and EMQX over TCP/IP networks. 

To configure the TCP listener in EMQX, you can add the `listeners.tcp` configuration items in the `emqx.conf` file within the `etc` folder of the EMQX installation directory.

For example, to enable the TCP listener on port `1883`, with a maximum 1,024,000 of concurrent connections allowed by the listener, you can work the code below:

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
}
```

where, <!--did not add the dashboard UI as here is a much simplified version-->

- `listeners.tcp.default` is to enable the listener, and here `default` is the name of the listener, you can change it to your own listener name. 
  - `bind` is to set the IP address and port of the listener, here it will listen to all incoming traffic from any IP address on port `1883`. 
  - `max_connection` is to set the maximum number of concurrent connections allowed by the listener; default value: `infinity`.

## Configure SSL Listener

SSL listener is a network service that listens for incoming SSL (Secure Sockets Layer) connections. In EMQX, it is used to secure network traffic between a client and EMQX by encrypting the data that is transmitted between them.

To configure the SSL listener in EMQX, you can add the `listeners.ssl` configuration items in the `emqx.conf` file within the `etc` folder of the EMQX installation directory.

For example, to enable the SSL listener on port `8883`, with a maximum 1,024,000 of concurrent connections allowed by the listener:

```bash
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  max_connections = 1024000
  ssl_options {
    cacertfile = "etc/certs/cacert.pem"
    certfile = "etc/certs/cert.pem"
    keyfile = "etc/certs/key.pem"
  }
}
```

where:

- `listeners.ssl.default` is to enable the listener. 
  - `bind` is the IP address and port of the listener, here it will listen to all incoming traffic from any IP address on port `8883`. 
  - `max_connection` is the maximum number of concurrent connections allowed by the listener, default value: `infinity`.
  - `ssl_options` is the SSL/TLS configuration option for the listener, it has three properties:
    - `cacertfile`: This sets the path to the file containing the trusted CA (certificate authority) certificates that the listener uses to verify the authenticity of the client certificates.
    - `certfile`: This sets the path to the file containing the SSL/TLS certificate for the listener.
    - `keyfile`: This sets the path to the file containing the private key corresponding to the SSL/TLS certificate.

 

## Configure WebSocket Listener

WebSocket listener is a network service that receives and processes messages over WebSocket. WebSocket support in EMQX allows clients to use the WebSocket protocol to connect to EMQX and exchange data in real-time.

To configure the WebSocket listener in EMQX, you can add the `listeners.ws` configuration items in the `emqx.conf` file within the `etc` folder of the EMQX installation directory.

For example, to enable the WebSocket listener on port `8083`, with a maximum 1,024,000 of concurrent connections allowed by the listener:

```bash
listeners.ws.default {
  bind = "0.0.0.0:8083"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
}
```

where:

- `listeners.ws.default` is to enable the listener. 
  - `bind` is the IP address and port of the listener, here it will listen to all incoming traffic from any IP address on port `8083`. 
  - `max_connection` is the maximum number of concurrent connections allowed by the listener, default value: `infinity`.
  - `websocket.mqtt_path` is to set the path to the WebSocket’s MQTT protocol, which is `/mqtt` by default. 

## Configure Secure WebSocket Listener

A secure WebSocket listener is a WebSocket listener that uses the Secure Sockets Layer (SSL) or Transport Layer Security (TLS) protocol to encrypt the data exchanged between a WebSocket client and the broker. In EMQX, the secure WebSocket listener is an important security measure to protect sensitive data exchanged between WebSocket clients and EMQX> 

To configure the secure WebSocket listener in EMQX, you can add the `listeners.wss` configuration items in the `emqx.conf` file within the `etc` folder of the EMQX installation directory.

For example, to enable the Secure WebSocket listener on port `8084`, with a maximum 1,024,000 of concurrent connections allowed by the listener:

```bash
listeners.wss.default {
  bind = "0.0.0.0:8084"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
  ssl_options {
    cacertfile = "etc/certs/cacert.pem"
    certfile = "etc/certs/cert.pem"
    keyfile = "etc/certs/key.pem"
  }
  }
```

where:

- `listeners.wss.default` is to enable the listener. 
  - `bind` is the IP address and port of the listener, here it will listen to all incoming traffic from any IP address on port `8083`. 
  - `max_connection` is the maximum number of concurrent connections allowed by the listener, default value: `infinity`.
  - `websocket.mqtt_path` is to set the path to the WebSocket’s MQTT protocol, which is `/mqtt` by default. 
    - `ssl_options` is the SSL/TLS configuration option for the listener, it has three properties:
      - `cacertfile`: This sets the path to the file containing the trusted CA (certificate authority) certificates that the listener uses to verify the authenticity of the client certificates.
      - `certfile`: This sets the path to the file containing the SSL/TLS certificate for the listener.
      - `keyfile`: This sets the path to the file containing the private key corresponding to the SSL/TLS certificate.



<!--To add QUIC-->

<!--To add code sample for adding multiple listeners.-->

:::tip

To configure listeners via Dashboard,  click **Configuration** -> **Listener** on the left navigation menu of the Dashboard. Once you configured these items with the Dashboard, your settings will override the same configuration items in `emqx.conf`.

EMQX has offered more configuration items to better serve customized needs, you can continue to read [Configuration Manual](./configuration-manual.md).

:::