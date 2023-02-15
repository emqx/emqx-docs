# 集成 Prometheus

[Prometheus](https://prometheus.io/) 是由 SoundCloud 开源的监控告警解决方案，支持多维数据模型、灵活的查询语言、强大的告警管理等特性。

EMQX 提供以下 Endpoint 供 Prometheus 采集系统指标：`http://127.0.0.1:18083/api/v5/prometheus/stats`

EMQX 还支持向 `pushgateway` 推送指标，此功能默认为关闭状态，可以通过在 `etc/emqx.conf` 中添加以下配置来启用：

```bash
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

## 通过 Grafana 可视化 EMQX 指标

您也可选择提供结合 Prometheus 和 Grafana 实现 EMQX 统计指标可视化。

EMQX 提供了 Grafana 的 Dashboard 模板，可以直接导入到 Grafana 中，查看 EMQX 的指标数据图表。

默认的 Dashboard 模板可以在 [EMQX | Grafana Dashboard](https://grafana.com/grafana/dashboards/17446-emqx/) 中下载，也可以在 EMQX Dashboard 的 **功能设置** -> **监控** -> **监控集成** 配置页面中的帮助页面里下载。

:::tip
完整的 Prometheus Grafana 可视化展示操作步骤请参考 [EMQX+Prometheus+Grafana：MQTT 数据可视化监控实践](https://www.emqx.com/zh/blog/emqx-prometheus-grafana)。
:::
