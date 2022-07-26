# StatsD

EMQX 可通过`etc/emqx.conf`配置向 [StatsD](https://github.com/statsd/statsd) 服务器定时推送系统指标。


```
statsd {
  ## 开启或关闭 Statsd 数据推送
  enable = false

  ## Statsd 服务器地址
  server = "127.0.0.1:8125"

  ## 数据收集间隔
  sample_time_interval = 10s

  ## 数据推送间隔
  flush_time_interval = 10s
 }

```

修改配置项后需要重启 EMQX 服务器生效。

## Dashboard 更新

EMQX在v5.0.4后，也支持通过 Dashboard 中的**功能配置/监控集成**下直接修改，保存后直接生效，无需重启节点。

