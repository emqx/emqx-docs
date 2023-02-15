# 集成 StatsD

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

EMQX 在 v5.0.4 后，也支持通过 Dashboard 中的 **功能配置** -> **监控** -> **监控集成** 直接修改，保存后直接生效，无需重启节点。
