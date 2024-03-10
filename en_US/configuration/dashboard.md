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
  - `cacertfile`: PEM file containing the trusted CA (certificate authority) certificates that the listener uses to verify the authenticity of the client certificates.
  - `certfile`: PEM file containing the SSL/TLS certificate chain for the listener. If the certificate is not directly issued by a root CA, the intermediate CA certificates should be appended after the server certificate to form a chain.
  - `keyfile`: PEM file containing the private key corresponding to the SSL/TLS certificate.

{% emqxce %}

:::tip

To add a listener via Dashboard, click **Management** -> **Listeners **on the left navigation menu of the Dashboard. Once you configured these items with the Dashboard, your settings will override the same configuration items in `emqx.conf`.

EMQX has offered more configuration items to serve customized needs better. For details, see [Configuration Manual](https://www.emqx.io/docs/en/v${CE_VERSION}/hocon/).

:::

{% endemqxce %}

{% emqxee %}

:::tip

To add a listener via Dashboard, click **Management** -> **Listeners **on the left navigation menu of the Dashboard. Once you configured these items with the Dashboard, your settings will override the same configuration items in `emqx.conf`.

EMQX has offered more configuration items to serve customized needs better. For details, see [Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

:::

{% endemqxee %}
