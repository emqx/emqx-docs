# Prometheus

EMQX REST API 暴露了一个Prometheus指标端点：`http://127.0.0.1:18083/api/v5/prometheus/stats`。

EMQX还支持向pushgateway推送指标，可以通过在`etc/emqx.conf`中添加以下配置来启用。

```yaml
prometheus {

  ## 普罗米修斯服务器的URL
  ## @path prometheus.push_gateway_server
  ## @type string()
  ## @default "http://127.0.0.1:9091"
  push_gateway_server: "http://127.0.0.1:9091"


  ## 数据报告间隔，以毫秒计。
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

## Grafana 数据模板

`emqx_prometheus` 插件提供了 Grafana 的 Dashboard 的模板文件。这些模板包含了所有 EMQX 监控数据的展示。用户可直接导入到 Grafana 中，进行显示 EMQX 的监控状态的图标。

模板文件位于：[emqx_prometheus/grafana_template](https://github.com/emqx/emqx-prometheus/tree/master/grafana_template)。
