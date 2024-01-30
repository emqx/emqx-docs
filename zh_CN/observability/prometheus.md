# 集成 Prometheus

[Prometheus](https://prometheus.io/) 是由 SoundCloud 开源的监控告警解决方案，支持多维数据模型、灵活的查询语言、强大的告警管理等特性。

EMQX 提供以下 Endpoint 供 Prometheus 采集系统指标：

- `http://127.0.0.1:18083/api/v5/prometheus/stats`

   EMQX 的基础指标及计数器。

- `http://127.0.0.1:18083/api/v5/prometheus/auth`

  包含访问控制中认证/鉴权的关键指标及计数器。

- `http://127.0.0.1:18083/api/v5/prometheus/data_integration`

  包含规则引擎，连接器，动作，编解码相关指标及计数器。

:::tip 提示:
更多 Prometheus pull Endpoint 相关信息，请参考 API 文档: [Monitor](https://www.emqx.io/docs/zh/v5.4/admin/api-docs.html#tag/Monitor)
:::


## Push Gateway

EMQX 还支持向 `pushgateway` 推送指标，此功能默认为关闭状态，可以通过在 `etc/emqx.conf` 中添加以下配置来启用：

:::tip
向 `pushgateway` 推送指标，目前仅包含 `/api/v5/stats` Endpoint 中的内容。
:::

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

## 节点或集群指标

在调用以上的 Endpoint 获取指标时，均支持使用不同的 URL 查询参数，以获取不同模式的 Metrics 值。


- `mode=node`:

  缺省值，返回当前节点的指标。
  Return metrics from local node. And it is the default behaviour if mode not specified.

- `mode=all_nodes_aggregated`:

  集群指标聚合模式。集群所有运行中节点指标的算术和或逻辑和。
  :::tip 逻辑和：
  对于 "开启状态"，"运行状态" 等指标，将返回逻辑和。
  :::

- `mode=all_nodes_unaggregated`:

  集群指标非聚合模式。集群所有运行中节点的指标。<br>
  对于不同节点的指标，会将节点名加入标签以区分不同节点。
  :::tip Example:
  ```
  ...
  emqx_connections_count{node="emqx@127.0.0.1"} 0
  ...
  ```
  :::


## 通过 Dashboard 配置集成

{% emqxce %}

EMQX 在 v5.0.4 后，也支持通过 Dashboard 中的**管理** -> **监控** -> **监控集成**直接修改，保存后直接生效，无需重启节点。

{% endemqxce %}

{% emqxee %}

您可在 EMQX Dashboard 设置集成 Prometheus，点击左侧导航目录中的**管理** -> **监控**，在**监控集成**页签，设置启用 Prometheus。

{% endemqxee %}

## 通过 Grafana 可视化 EMQX 指标

您也可选择提供结合 Prometheus 和 Grafana 实现 EMQX 统计指标可视化。

EMQX 提供了 Grafana 的 Dashboard 模板，可以直接导入到 Grafana 中，查看 EMQX 的指标数据图表。

默认的 Dashboard 模板可以在 [EMQX | Grafana Dashboard](https://grafana.com/grafana/dashboards/17446-emqx/) 中下载，也可以在 EMQX Dashboard 的 **管理** -> **监控** -> **监控集成** 配置页面中的帮助页面里下载。

:::tip
完整的 Prometheus Grafana 可视化展示操作步骤请参考 [EMQX+Prometheus+Grafana：MQTT 数据可视化监控实践](https://www.emqx.com/zh/blog/emqx-prometheus-grafana)。
:::
