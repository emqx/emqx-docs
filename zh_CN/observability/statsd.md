# 集成 StatsD

{% emqxce %}
:::tip
该功能自 5.0.22 版本起已弃用，建议使用 [Prometheus](./prometheus.md) 代替。
:::
{% endemqxce %}

{% emqxee %}
:::tip
该功能自 5.0.2 版本起已弃用，建议使用 [Prometheus](./prometheus.md) 代替。
:::
{% endemqxee %}

EMQX 支持向 [StatsD](https://github.com/statsd/statsd) 服务器周期性推送系统指标。

StatsD 推送功能默认为关闭状态，可以通过在 `etc/emqx.conf` 中添加以下配置来启用：

```bash
statsd {
  ## 开启或关闭 StatsD 数据推送
  enable = false

  ## StatsD 服务器地址
  server = "127.0.0.1:8125"

  ## 数据收集间隔
  sample_time_interval = 10s

  ## 数据推送间隔
  flush_time_interval = 10s
 }

```

修改配置项后需要重启 EMQX 服务器生效。

## 通过 Dashboard 配置

{% emqxce %}

EMQX 在 v5.0.4 后，也支持通过 Dashboard 中的**功能配置** -> **监控** -> **监控集成**直接修改，保存后直接生效，无需重启节点。

{% endemqxce %}

{% emqxee %}

您可在 EMQX Dashboard 设置集成 StatsD，点击左侧导航目录中的**功能配置** -> **监控**，在**监控集成**页签，设置启用 StatsD。

{% endemqxee %}
