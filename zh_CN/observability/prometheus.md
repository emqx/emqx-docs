# 集成 Prometheus

EMQX 支持将指标数据集成到第三方服务中来监控指标，例如 Prometheus。[Prometheus](https://prometheus.io/) 是由 SoundCloud 开源的监控告警解决方案，支持多维数据模型、灵活的查询语言、强大的告警管理等特性。还可以结合 Prometheus 和 Grafana 实现 EMQX 统计指标可视化。

使用第三方监控系统可以带来以下优势：

- 提供全面的监控：第三方监控系统可以提供全面的视角。例如，您可以同时获取服务器主机和 MQTT 服务的监控信息。
- 提供直观的监控报告：第三方监控系统通常提供图形和图表等可视化工具，使得监控报告更加直观易懂。例如，您可以使用 Grafana 来可视化 EMQX 的指标。
- 多样的报警通知选项：第三方监控系统通常支持多种报警通知方式，使得在出现问题时，可以及时通知到相关人员。例如，您可以使用 Prometheus Alertmanager 来设置报警规则和通知方式。

EMQX 支持两种方式实现 Prometheus 指标监控集成：

- **Pull 模式**：Prometheus 直接通过 EMQX 的 REST API 采集指标。
- **Push 模式**：EMQX 推送指标到 Pushgateway 服务，再由 Prometheus 从 Pushgateway 服务中采集指标。

::: tip
从 EMQX 6.3.0 开始，Prometheus 指标由 `metrics` 功能门控控制。如果手动设置 `EMQX_FEATURES`，启用 `metrics` 时也会自动启用其依赖的 `dashboard` 和 `auth`。更多信息请参见[功能门控](../deploy/feature-gates.md)。
:::

配置 Prometheus 集成步骤如下：

1. 在 EMQX Dashboard 中进入**管理** -> **监控**。
2. 切换到**监控集成**标签页。
3. 选择 **Prometheus** 作为监控平台。

根据所选择的模式，部分配置项仅适用于 Pull 模式，而部分配置项同时适用于两种模式。您可以点击 Dashboard 页面上的**帮助**按钮查看每种模式的详细配置步骤。

<img src="./assets/config_pushgateway.png" alt="config_pushgateway" style="zoom:67%;" />

## Prometheus 配置选项

本节介绍在 Dashboard 中选择 **Prometheus** 时可用的所有配置项。

### 通用选项（同时影响 Pull 和 Push 模式）

#### 延迟区间

用于设置延迟指标的直方图区间边界，多个时间值使用英文逗号分隔。

**格式**

以逗号分隔的时间长度列表，例如：

```
10ms, 100ms, 1s, 5s, 30s
```

**说明**

这些值用于定义 Prometheus 中延迟指标如何划分到不同的直方图区间中。较小的区间间隔可以提供更精细的统计粒度，但可能会增加指标的基数（cardinality）以及存储开销。

该设置会影响延迟直方图指标在内部的生成方式，并适用于：

- Pull 模式指标
- Push 模式指标（通过 Pushgateway）

### Pull 模式选项设置

以下选项仅在 Prometheus 通过 REST API 拉取 EMQX 指标时适用。

#### 启用基本认证

启用或禁用 Prometheus 抓取 API 的 HTTP Basic 认证。

默认情况下，Prometheus Pull 模式接口不需要身份认证。当启用该选项后：

- Prometheus 必须使用 HTTP Basic 认证访问以下接口：
  - `/api/v5/prometheus/stats`
  - `/api/v5/prometheus/auth`
  - `/api/v5/prometheus/data_integration`
- 您需要在 EMQX 中创建一个 [API 密钥](../admin/api.md#创建-api-密钥)。
- 在 `prometheus.yaml` 的 `basic_auth` 部分进行配置。

该选项仅适用于 Pull 模式，不影响 Pushgateway 集成。详情请参阅 [配置 Pull 模式集成](#配置-pull-模式集成)。

#### 命名空间数据抓取速率限制

限制抓取命名空间级指标时的最大请求速率。

在多租户部署场景中，支持按命名空间暴露和聚合指标。详情请参阅 [Prometheus 指标隔离](../multi-tenancy/namespace-overview.md#多租户能力支持情况)。

**格式**：`<requests>/<duration>`

**示例**：`1/5s`，表示每 5 秒最多允许 1 次请求。在时间窗口内的额外请求将被拒绝。

**行为说明**：

- 仅适用于命名空间级指标抓取请求。
- 针对特定命名空间的请求不受限制。
- 仅适用于 Pull 模式。

该选项有助于在大规模或多命名空间部署环境中防止过度负载。

### Push 模式选项设置

Push 模式允许 EMQX 将指标推送到 Pushgateway 实例。默认情况下，Push 模式处于禁用状态。

#### 启用 Pushgateway

启用或禁用向 Pushgateway 推送指标。启用后需配置以下字段：

#### 采集间隔

指定 EMQX 向 Pushgateway 推送指标的时间间隔。默认值为 `15` 秒。

#### Pushgateway 服务

指定 Pushgateway 服务器的 URL。默认值为：`http://127.0.0.1:9091`。

#### Job 名称

指定向 Pushgateway 推送指标时使用的 Job 标签。

您可以使用从 EMQX 节点名称和主机名派生的变量来构造 Job 标签。默认值为：`${name}/instance/${name}~${host}`。

**变量说明：**

- `${name}`：EMQX 节点名称（例如 `emqx`）
- `${host}`：主机 IP 地址（例如 `127.0.0.1`）

例如，当节点名称为：`emqx@127.0.0.1`，则：

- `${name}` = `emqx`
- `${host}` = `127.0.0.1`

#### 请求头

向 Pushgateway 推送指标时可选的 HTTP 请求头。

其值类型为字符串。您可以以键值对形式进行配置，例如：`Authorization = "some-auth-token"`。

点击**添加**可添加更多请求头。

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

更多 Prometheus pull 端点相关信息，请参考 [EMQX 企业版 API 文档](https://docs.emqx.com/zh/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)。

### 认证（可选）

默认情况下，Prometheus Pull 模式接口不需要身份认证。

如果在 EMQX Dashboard 中开启了**启用基本认证**，则 Prometheus 必须使用 HTTP Basic 认证进行访问。

在这种情况下：

1. 在 EMQX 中创建一个 [API 密钥](../admin/api.md#创建-api-密钥)。
2. 在 Prometheus 配置中使用生成的 API Key 和 Secret Key。

在 Prometheus 配置文件中：

```yaml
basic_auth:
  username: '<API_KEY>'
  password: '<SECRET_KEY>'
```

其中：

- `username` 为 API Key。
- `password` 为对应的 Secret Key。

Prometheus 将使用这些凭据抓取 EMQX 指标。

### Prometheus 服务器配置示例

要使 Prometheus 抓取 EMQX 指标，需要配置 Prometheus 服务器。

将以下配置添加到 Prometheus 的配置文件，然后重启 Prometheus 服务。

```yaml
# prometheus.yaml
global:
  scrape_interval:     10s # 默认抓取间隔为 10 秒
  evaluation_interval: 10s # 默认评估间隔为 10 秒
  # 在此机器上，每个时间序列默认都会被导出
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

EMQX 支持向 Pushgateway 推送指标，再由 Prometheus 从 Pushgateway 采集指标。此功能默认为关闭状态。如需启用 Pushgateway 服务，您可以在 Dashboard 中的 Prometheus 配置页面上打开**启用 Pushgateway** 按钮并完成相关选项设置后，点击**保存修改**。

当前 Push 模式仅包含来自 `/api/v5/prometheus/stats` 接口的基础指标和计数器。若需更全面的监控，通常建议使用 Pull 模式。

### 配置文件示例

您还可以通过在 `etc/emqx.conf` 中添加以下配置来启用和配置 Pushgateway。有关更多配置项，请参阅 [配置文件 - Prometheus](../configuration/prometheus.md)。

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
