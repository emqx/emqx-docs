# Dashboard

In EMQX, Dashboard is a web-based graphic interface to manage and monitor EMQX and connected devices in real time. 

For example,  configure a listener for EMQX Dashboard for accepting all incoming connections.

```
dashboard {
  listeners {
    http {
      bind = "0.0.0.0:18083"
      max_connections = 512
    }
  }
}
```

Where,

- `bind  =  "0.0.0.0:18083"`  is to set the network address and port number that the listener will bind to. In this case, the listener will bind to all available network interfaces (`0.0.0.0`) on port `18083`.
- `max_connections  =  512` is to set the maximum number of concurrent connections that the listener will accept. In this case, the maximum number of connections is set to `512`.

See [EMQX Docs - Dashboard](https://www.emqx.io/docs/en/v5.5.1/hocon/#V-dashboard)

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
