# 集成 Prometheus

EMQX 支持将指标数据集成到第三方服务中来监控指标，例如 Prometheus。[Prometheus](https://prometheus.io/) 是由 SoundCloud 开源的监控告警解决方案，支持多维数据模型、灵活的查询语言、强大的告警管理等特性。还可以结合 Prometheus 和 Grafana 实现 EMQX 统计指标可视化。

使用第三方监控系统可以带来以下优势：

- 提供全面的监控：第三方监控系统可以提供全面的视角。例如，您可以同时获取服务器主机和 MQTT 服务的监控信息。
- 提供直观的监控报告：第三方监控系统通常提供图形和图表等可视化工具，使得监控报告更加直观易懂。例如，您可以使用 Grafana 来可视化 EMQX 的指标。
- 多样的报警通知选项：第三方监控系统通常支持多种报警通知方式，使得在出现问题时，可以及时通知到相关人员。例如，您可以使用 Prometheus Alertmanager 来设置报警规则和通知方式。

EMQX 支持 2 种方式实现 Prometheus 指标监控集成：

- **Pull 模式**：Prometheus 直接通过 EMQX 的 REST API 采集指标。
- **Push 模式**：EMQX 推送指标到 Pushgateway 服务，再由 Prometheus 从 Pushgateway 服务中采集指标。

本页将介绍这两种方式的配置步骤。您可以在 EMQX Dashboard 中点击左侧导航目录中的**管理** -> **监控**，在**监控集成**标签页，选择 **Prometheus** 进行相关的集成配置。您还可以点击页面上的**帮助**按钮查看每个模式的具体配置步骤。

<!-- TODO 5.5 将 Dashboard 上的完整配置步骤合并到文档中 -->

## 配置 Pull 模式集成

EMQX 提供以下 REST API 供 Prometheus 采集系统指标：

- `/api/v5/prometheus/stats`：EMQX 的基础指标及计数器。

- `/api/v5/prometheus/auth`：包含访问控制中认证和鉴权的关键指标及计数器。

- `/api/v5/prometheus/data_integration`：包含规则引擎，连接器，动作，Sink/Source，编解码相关指标及计数器。

在调用以上的 API 来获取指标时，我们可以使用 URL 查询参数 `mode` 来获取不同模式的指标数据。不同参数的含义如下：


:::: tabs type:card

::: tab 单节点模式

`mode=node`

默认模式，会返回当前请求节点的指标。如果没有指定具体的模式，系统会默认返回这种模式的指标。

:::

::: tab 集群聚合模式

`mode=all_nodes_aggregated`

聚合集群指标，返回的是集群中所有运行节点指标的*算术和*或者*逻辑和*。

- 对于像“开启状态”，“运行状态”这类指标系统会返回它们的逻辑和，即所有节点都开启或运行则返回 1，否则返回 0。
- 部分指标在不同节点具有独立性，将不会返回聚合值。例如 CPU 内存的使用量等。系统会将节点名加入标签，以便区分不同节点的指标。例如：

  ```bash
  emqx_vm_cpu_use{node="emqx@172.17.0.2"} 7.6669163995887715
  emqx_vm_cpu_idle{node="emqx@172.17.0.2"} 92.33308360041123

  emqx_vm_cpu_use{node="emqx@172.17.0.3"} 7.676007766679973
  emqx_vm_cpu_idle{node="emqx@172.17.0.3"} 92.32399223332003
  ```

- 部分指标在集群中任意节点的取值应一致。对于集群一致的指标，将直接返回接受 API 请求的节点上的取值。并且不会求和，也不会将节点名加入标签。例如：
  ```bash
  emqx_topics_count 3
  emqx_cert_expiry_at{listener_type="ssl",listener_name="default"} 1904285225
  emqx_cert_expiry_at{listener_type="wss",listener_name="default"} 1904285225
  ```
- 其他指标返回算数和，即返回的指标为所有节点指标的和。

:::

::: tab 集群非聚合模式

`mode=all_nodes_unaggregated`

这是集群指标非聚合模式，返回的是集群中所有运行节点的各自指标。

- 系统会将节点名加入标签，以便区分不同节点的指标。例如：

  ```bash
  emqx_connections_count{node="emqx@127.0.0.1"} 0
  ```

- 部分指标在集群中任意节点的取值应一致。例如“黑名单条数”，“保留消息数”等。对于集群一致的指标，将直接返回接受 API 请求的节点上的取值。并且不会将节点名加入标签。例如：
  ```bash
  emqx_retained_count 3
  ```

:::

::::

更多 Prometheus pull 端点相关信息，请参考 [EMQX 开源版 API 文档](https://docs.emqx.com/zh/emqx/v@CE_MINOR_VERSION@/admin/api-docs.html)和 [EMQX 企业版 API 文档](https://docs.emqx.com/zh/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)。

:::tip 提示

默认情况下，pull 模式 API 是不需要认证的，您可以在在页面上配置**启用基本认证**开关来启用基本认证对接口进行保护。启用后，您需要在 EMQX 上创建 [API 密钥](../admin/api.md#认证)并应用于 Prometheus 配置中才能够获取指标数据。

:::

### 参考的 Prometheus 配置

```yaml
# prometheus.yaml
global:
  scrape_interval:     10s # The default scrape interval is every 10 seconds.
  evaluation_interval: 10s # The default evaluation interval is every 10 seconds.
  # On this machine, every time series will be exported by default.
  external_labels:
    monitor: 'emqx-monitor'
scrape_configs:
  - job_name: 'emqx_stats'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/stats'
    scheme: 'http'
    basic_auth:
      username: ''
      password: ''

  - job_name: 'emqx_auth'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/auth'
    scheme: 'http'
    basic_auth:
      username: ''
      password: ''

  - job_name: 'emqx_data_integration'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/data_integration'
    scheme: 'http'
    basic_auth:
      username: ''
      password: ''
```

## 配置 Push 模式集成

EMQX 支持向 Pushgateway 推送指标，再由 Prometheus 从 Pushgateway 采集指标。此功能默认为关闭状态。如需启用 Pushgateway 服务，您可以在 Dashboard 中的 Prometheus 配置页面上打开**启用 Pushgateway** 按钮。

<img src="./assets/config_pushgateway.png" alt="config_pushgateway" style="zoom:67%;" />

根据您的业务需求配置以下字段：

- **采集间隔**：指定向 Pushgateway 报告监控指标数据的时间间隔。默认值为 `15` 秒。
- **Pushgateway 服务**：输入 Prometheus 服务器的 URL。默认为 `http://127.0.0.1:9091`。
- **Job 名称**：指定包含 EMQX 节点名称和主机名的变量。默认值为 `${name}/instance/${name}~${host}`。例如，当 EMQX 节点名称为 `emqx@127.0.0.1` 时，`name` 变量取值为 `emqx`，`host` 变量取值为 `127.0.0.1`。
- **请求头**：输入推送到 Pushgateway 的监控指标的 HTTP 头的键和值。您可以通过点击**添加**按钮添加请求头列表。类型为字符串，例如，{ Authorization = "some-authz-tokens"}。

:::tip 提示
Push 模式目前仅包含 EMQX 的基础指标及计数器，因此更推荐使用 Pull 模式。
:::

同时，您可以点击**帮助**按钮，参考**使用 Pushgateway** 标签页上的步骤进行配置。

您还可以通过在 `etc/emqx.conf` 中添加以下配置来启用和配置 Pushgateway：

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


  ## 打开 Prometheus 的数据推送，或者关闭
  ## @path prometheus.enable
  ## @type boolean()
  ## @default false
  enable: true
}
```

## 通过 Grafana 可视化 EMQX 指标

EMQX 提供了 Grafana 的 Dashboard 模板，可以直接导入到 Grafana 中查看 EMQX 的指标数据图表。默认的 Dashboard 模板可以在 [EMQX | Grafana Dashboard](https://grafana.com/grafana/dashboards/17446-emqx/) 中下载，也可以在帮助页面里下载。

:::tip 提示
完整的 Prometheus Grafana 可视化展示操作步骤可以参考 [EMQX+Prometheus+Grafana：MQTT 数据可视化监控实践](https://www.emqx.com/zh/blog/emqx-prometheus-grafana)。
:::
