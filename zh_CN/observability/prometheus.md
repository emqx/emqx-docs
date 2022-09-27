# Prometheus

EMQX 提供以下 Endpoint 供 Prometheus 采集系统指标：
`http://127.0.0.1:18083/api/v5/prometheus/stats`

EMQX 还支持向 `pushgateway` 推送指标，可以通过在 `etc/emqx.conf` 中添加以下配置来启用。默认为关闭状态。

```
prometheus {

  ## Prometheus的URL
  ## @path prometheus.push_gateway_server
  ## @type string()
  ## @default "http://127.0.0.1:9091"
  push_gateway_server: "http://127.0.0.1:9091"


  ## 数据报告间隔。
  ## @path prometheus.interval
  ## @type emqx_schema:duration_ms()
  ## @default 15s
  interval: 15s


  ## 打开Prometheus的数据推送，或者关闭
  ## @path prometheus.enable
  ## @type boolean()
  ## @default false
  enable: true
}
```


## 通过 Dashboard 配置

EMQX 在 v5.0.4 后，也支持通过 Dashboard 中的 **功能配置/监控集成** 直接修改，保存后直接生效，无需重启节点。


## Grafana 数据模板

EMQX 提供了 Grafana Dashboard 的模板文件。这些模板包含了所有 EMQX 监控数据的展示。用户可直接导入到 Grafana 中，进而显示 EMQX 的状态监控图表。

模板文件位于：[Grafana Template](https://github.com/emqx/emqx/tree/master/apps/emqx_prometheus/grafana_template)。
