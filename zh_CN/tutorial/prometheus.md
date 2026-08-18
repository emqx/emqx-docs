---
# 编写日期
date: 2020-02-25 09:15:26
# 作者 Github 名称
author: hjianbo
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---


# Prometheus 监控告警

{% emqxce %}

EMQX 提供 `emqx_prometheus` 插件，用于将系统的监控数据输出到第三方的监控系统中。


以 [Prometheus](https://prometheus.io) 为例：

`emqx_prometheus` 支持将数据推送至 Pushgateway 中，然后再由 Promethues Server 拉取进行存储。

注意：`emqx_prometheus` 不支持 Prometheus 的 Pull 操作。

## 配置


`emqx_prometheus` 插件内部会启动一个定时器，使其每间隔一段时间便采集 EMQX 中的监控数据。

`emqx_prometheus` 推送的监控数据包含的具体字段和含义，参见：[Metrics & Stats](../advanced/metrics-and-stats.md)

配置文件位于 `etc/plugins/emqx_prometheus.conf`，其中：

|  配置项             | 类型    | 可取值    | 默认值                | 说明                           |
| ------------------- | ------- | --------- | --------------------- | ------------------------------ |
| push.gateway.server | string  | -         | http://127.0.0.1:9091 | Prometheus 的 PushGateway 地址 |
| interval            | integer | > 0       | 5000                  | 推送间隔，单位：毫秒           |

### Grafana 数据模板

`emqx_statsd` 插件提供了 Grafana 的 Dashboard 的模板文件。这些模板包含了所有 EMQX 监控数据的展示。用户可直接导入到 Grafana 中，进行显示 EMQX 的监控状态的图标。

模板文件位于：[emqx_statsd/grafana_template](https://github.com/emqx/emqx-statsd/tree/master/grafana_template)。

{% endemqxce %}



{% emqxee %}

> 从 EMQX Enterprise v4.1.0 开始，emqx_statsd 更名为 emqx_prometheus，相关插件名称、目录均有变更。

EMQX Enterprise 支持两种与 Prometheus 集成的方式：

- **推送模式**（全版本）：[`emqx_prometheus`](https://github.com/emqx/emqx-prometheus) 插件定期将指标推送到 Prometheus Pushgateway，再由 Prometheus Server 拉取。
- **拉取模式**（e4.4.34+）：Prometheus Server 直接抓取 EMQX 内置的 HTTP 端点 `/api/v4/emqx_prometheus`，无需 Pushgateway。

## 推送模式：`emqx_prometheus` 插件

```
EMQX → [emqx_prometheus 插件] → Pushgateway → Prometheus Server
```

`emqx_prometheus` 插件内部会启动一个定时器，使其每间隔一段时间便采集 EMQX 中的监控数据，并推送至 Pushgateway。

> 注意：`emqx_prometheus` 插件不支持被 Prometheus 直接抓取，必须通过 Pushgateway 中转。

### 启用

打开 EMQX Dashboard，进入**模块**页面，添加 **[EMQX Prometheus Agent](../modules/prometheus.md)**。

### 配置

配置文件位于 `etc/plugins/emqx_prometheus.conf`，其中：

| 配置项              | 类型    | 默认值                | 说明                           |
| ------------------- | ------- | --------------------- | ------------------------------ |
| push.gateway.server | string  | http://127.0.0.1:9091 | Prometheus 的 PushGateway 地址 |
| interval            | integer | 15000                 | 推送间隔，单位：毫秒           |

`emqx_prometheus` 推送的监控数据包含的具体字段和含义，参见：[Metrics & Stats](../advanced/metrics-and-stats.md)。

### Grafana 数据模板

`emqx_prometheus` 插件提供了 Grafana 的 Dashboard 模板文件，包含所有 EMQX 监控数据的展示。用户可直接导入到 Grafana 中查看 EMQX 监控状态。

模板文件位于：[emqx_prometheus/grafana_template](https://github.com/emqx/emqx-prometheus/tree/master/grafana_template)

## 拉取模式：HTTP 端点

> 从 EMQX Enterprise e4.4.34 开始支持。

```
Prometheus Server → scrape → EMQX HTTP 端点
```

在 Prometheus 中配置抓取以下端点：

```
http://localhost:8081/api/v4/emqx_prometheus?type=prometheus
```

> 注意：默认响应格式为 JSON（`type=json`），需添加 `?type=prometheus` 参数以获取 Prometheus text 格式数据。

## HTTP API 监控指标

> 从 EMQX Enterprise e4.4.34 开始支持。

从 e4.4.34 开始，EMQX Enterprise 新增两个用于监控 HTTP API 请求性能的指标。这些指标会自动包含在 `emqx_prometheus` 插件的数据采集中，无需额外配置，可通过推送模式和拉取模式两种方式获取。

如需直接以 JSON 格式查询请求计数，可使用以下端点：

```
GET /api/v4/http_api_metrics
```

### 指标列表

| 指标名 | 类型 | 标签 | 说明 |
| ------ | ---- | ---- | ---- |
| `emqx_http_api_request_total` | Counter | `result`=`success` \| `failure` | HTTP API 请求总数 |
| `emqx_http_api_request_duration_milliseconds` | Histogram | 无 | HTTP API 请求耗时分布（毫秒） |

### 结果分类

`emqx_http_api_request_total` 指标的 `result` 标签按以下规则分类：

- **success**：HTTP 状态码为 200 且业务返回码 `code=0`。
- **failure**：所有其他情况，包括：
  - 鉴权失败（HTTP 401）
  - 路由不存在（HTTP 404）
  - 服务端异常（HTTP 500）
  - 业务错误码不为 0
  - 权限不足（permission denied）

### 耗时直方图

`emqx_http_api_request_duration_milliseconds` 使用以下 bucket 边界（单位：毫秒）：

`5, 10, 25, 50, 100, 250, 500, 1000`

### 在 Prometheus 中查询

```promql
# 查看 API 请求速率（每秒请求数）
rate(emqx_http_api_request_total[5m])

# 按结果分组查看请求速率
sum by (result) (rate(emqx_http_api_request_total[5m]))

# 请求失败率
sum(rate(emqx_http_api_request_total{result="failure"}[5m]))
/
sum(rate(emqx_http_api_request_total[5m]))

# 请求耗时 P99
histogram_quantile(0.99, rate(emqx_http_api_request_duration_milliseconds_bucket[5m]))

# 请求平均耗时
rate(emqx_http_api_request_duration_milliseconds_sum[5m])
/
rate(emqx_http_api_request_duration_milliseconds_count[5m])
```

## 集群节点间网络健康探测

> 从 EMQX Enterprise e4.4.38 开始支持。

`emqx_erpc_probe` 插件用于监控集群节点间链路的健康状况。每个节点对集群中的其余节点各维持一个独立的探测进程，按 `erpc_probe.probe_interval`（默认 `1s`）周期性发起 `erpc:call(Peer, erlang, node, [], Timeout)` 探测，超时时间为 `erpc_probe.probe_timeout`（默认 `5s`）。

插件通过标准 Prometheus 指标暴露探测结果，这些指标会随 EMQX 的 Prometheus 采集一起暴露（可通过拉取模式或 `emqx_prometheus` 插件获取）：

| 指标名 | 类型 | 标签 | 说明 |
| ------ | ---- | ---- | ---- |
| `emqx_erpc_probe_result_total` | Counter | `peer`、`result`=`ok` \| `timeout` \| `noconnection` \| `system_limit` | 按结果统计探测次数。`timeout` 表示已连接但对端无响应（链路劣化或对端过载）；`noconnection` 表示无分布连接（对端 VM 宕机或完全隔离）；`system_limit` 表示对端进程耗尽 |
| `emqx_erpc_probe_duration_seconds` | Histogram | `peer` | 成功探测的往返时间，用于发现“变慢但尚未超时”的链路（p99） |

配置文件位于 `etc/plugins/emqx_erpc_probe.conf`：

| 配置项 | 默认值 | 说明 |
| ------ | ------ | ---- |
| `erpc_probe.probe_enable` | `on` | 是否启用探测 worker。关闭后指标序列仍存在但保持为 0 |
| `erpc_probe.probe_interval` | `1s` | 对同一对端两次探测之间的最小间隔 |
| `erpc_probe.probe_timeout` | `5s` | 每次 `erpc:call` 探测的超时时间 |
| `erpc_probe.probe_buckets` | `0.001, 0.0025, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5` | 耗时直方图的 bucket 上界（秒），需严格递增 |

::: tip 注意

- 该插件对新安装的集群默认启用（见 `data/loaded_plugins`）。
- 从旧版本升级的集群会保留原有的 `data/loaded_plugins`，插件不会自动启用。可执行 `./bin/emqx ctl plugins load emqx_erpc_probe` 启用，或在 `data/loaded_plugins` 中加入 `{emqx_erpc_probe, true}.` 后重启节点。
- 配置项在插件启动时读取，修改配置后需要重启插件或节点才能生效。

:::

{% endemqxee %}
