# Dashboard 配置

在EMQX中， Dashboard 是一个基于 Web 的图形界面，用于实时管理和监控 EMQX 及连接的设备。您可以配置以下 Dashboard 配置项：

- `listeners`
- `token_expired_time`
- `cors`
- `swagger_support`
- `sso`

例如，要为EMQX Dashboard 配置 `swagger_support` 和一个监听器，以接受所有传入连接，您可以使用以下配置：

```bash
dashboard {
  listeners {
    http {
      bind = "0.0.0.0:18083"
      max_connections = 512
    }
  swagger_support = true
  }
}
```

其中，

- `swagger_support = true` 用于启用所有与 swagger 相关的功能，如生成Swagger API文档。默认情况下，其值始终为 `true`，您可以将值设置为 `false` 以禁用它。
- `bind = "0.0.0.0:18083"` 用于设置监听器绑定的网络地址和端口号。在这种情况下，监听器将绑定到所有可用的网络接口（`0.0.0.0`）上的端口 `18083`。
- `max_connections = 512` 用于设置监听器将接受的最大并发连接数。在这种情况下，最大连接数设置为 `512`。

{% emqxce %}

::: tip

EMQX 提供了更多配置项以更好地满足定制化需求。详细信息请参考[配置手册](https://www.emqx.io/docs/zh/v@CE_VERSION@/hocon/)。

:::

{% endemqxce %}

{% emqxee %}

::: tip

EMQX 提供了更多配置项以更好地满足定制化需求。详细信息请参考[配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)。

:::

{% endemqxee %}
