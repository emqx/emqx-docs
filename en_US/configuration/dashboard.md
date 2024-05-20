# Dashboard Configuration

In EMQX, Dashboard is a web-based graphic interface to manage and monitor EMQX and connected devices in real time. You can configure the following Dashboard configuration items:

- `listeners`
- `token_expired_time`
- `cors`
- `swagger_support`
- `sso`

For example, to configure `swagger_support` and a listener for the EMQX Dashboard for accepting all incoming connections, you can use the following configuration:

```bash
dashboard {
  listeners {
    http {
      # set 'bind = 0' will disable this listener
      bind = "0.0.0.0:18083"
      max_connections = 512
    }
    https {
      # set 'bind = 0' will disable this listener
      bind = "0.0.0.0:18084"
      ssl_options {
        certfile = "${EMQX_ETC_DIR}/certs/cert.pem"
        keyfile = "${EMQX_ETC_DIR}/certs/key.pem"
      }
    }
  }
  swagger_support = true
  default_password = jEdOgGS6vzQ
}
```

Where,

- `swagger_support = true`: Enable Swagger (OpenAPI) UI available at the endpoint `/api-docs`. Set to `false` to disable.
- `bind = "0.0.0.0:18083"`:  Address and port number that the listener will bind to. In this case, the listener will bind to all available network interfaces (`0.0.0.0`) on port `18083`. set to port number `0` will disable this listener.
- `max_connections = 512`: Set the maximum number of concurrent connections that the listener will accept. In this case, the maximum number of connections is set to `512`.
- `ssl_options.certfile`: Path to the PEM format certificates chain file. Server certificate as the first one, followed by its immediate issuer certificate then the issuer's issuer certificate, and so on. Root CA certificate is optional. The path prefix (only prefix) can be an environment variable.
- `ssl_options.keyfile`: Path to the PEM format private key file.
- `default_password`: The password used to **initialize** the database record for `admin` user. NOTE: Changing this config after EMQX has booted for the first time has no effect. Once initialized, the default password `public` (which comes with the installation) must be changed from dashboard or CLI.

{% emqxce %}

::: tip

EMQX has offered more configuration items to serve customized needs better. For details, see [Configuration Manual](https://www.emqx.io/docs/en/v@CE_VERSION@/hocon/#V-dashboard).

:::

{% endemqxce %}

{% emqxee %}

::: tip

EMQX has offered more configuration items to serve customized needs better. For details, see [Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/#V-dashboard).

:::

{% endemqxee %}
