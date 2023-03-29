# Dashboard

In EMQX, Dashboard is a web-based graphic interface to manage and monitor EMQX and connected devices in real time. 

For example,  configure a listener for EMQX Dashboard for accepting all incoming connections.

```
dashboard {
  listener {
    bind  =  "0.0.0.0:18083"
    max_connections  =  512
    ssl_options {
      cacertfile = "etc/certs/cacert.pem"
      certfile = "etc/certs/cert.pem"
      keyfile = "etc/certs/key.pem"
    }
  }
}
```

Where,

- `bind  =  "0.0.0.0:18083"`  is to set the network address and port number that the listener will bind to. In this case, the listener will bind to all available network interfaces (`0.0.0.0`) on port `18083`.
- `max_connections  =  512` is to set the maximum number of concurrent connections that the listener will accept. In this case, the maximum number of connections is set to `512`.
- `ssl_options` is the SSL/TLS configuration option for the listener, it has three properties:
  - `cacertfile`: This sets the path to the file containing the trusted CA (certificate authority) certificates that the listener uses to verify the authenticity of the client certificates.
  - `certfile`: This sets the path to the file containing the SSL/TLS certificate for the listener.
  - `keyfile`: This sets the path to the file containing the private key corresponding to the SSL/TLS certificate.

:::tip

To add a listener via Dashboard, click **Configuration** -> **Listeners **on the left navigation menu of the Dashboard. Once you configured these items with the Dashboard, your settings will override the same configuration items in `emqx.conf`.

EMQX has offered more configuration items to better serve customized needs, For more configuration items, see [Configuration Manual](./configuration-manual.md).

:::