# 连接抖动配置

在 EMQX 中，连接抖动指的是 MQTT 客户端在短时间内频繁地连接和断开与代理的连接的情况。连接抖动机制用于检测连接抖动客户端并断开客户端的连接。

例如，如果您希望将在1分钟窗口内尝试连接15次的客户端标记为连接抖动客户端，然后禁止检测到的客户端在5分钟内连接到 EMQX，您可以使用以下配置：

```bash
flapping_detect {
  enable = true
  max_count  =  15
  window_time  =  1m
  ban_time  =  5m
}
```

其中，

- `max_count` 用于设置客户端在指定时间窗口（由 `window_time` 定义）内允许的最大连接尝试次数。
- `window_time` 用于设置计算客户端最大连接尝试次数的时间窗口。
- `ban_time` 用于设置在检测到客户端为连接抖动后，禁止客户端连接到 EMQX 的持续时间。

{% emqxce %}

EMQX 还提供了更多配置项以更好地满足定制化需求。更多详细信息请参考[配置手册](https://www.emqx.io/docs/zh/v@CE_VERSION@/hocon/)。

{% endemqxce %}

{% emqxee %}

EMQX 还提供了更多配置项以更好地满足定制化需求。详细信息请参考[配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)。

{% endemqxee %}

::: tip

您也可以通过点击 Dashboard 左侧导航菜单中的 **访问控制** -> **连接抖动**设置，具体参考[连接抖动检测](../access-control/flapping-detect.md)。一旦您通过 Dashboard 配置了这些设置，您的设置将覆盖 `emqx.conf` 中相同的配置项。

:::