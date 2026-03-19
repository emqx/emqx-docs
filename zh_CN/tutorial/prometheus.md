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

EMQX 提供 [emqx_prometheus](https://github.com/emqx/emqx-prometheus) 插件，用于将系统的监控数据输出到第三方的监控系统中。

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

`emqx_prometheus` 插件提供了 Grafana 的 Dashboard 的模板文件。这些模板包含了所有 EMQX 监控数据的展示。用户可直接导入到 Grafana 中，进行显示 EMQX 的监控状态的图标。

模板文件位于：[emqx_prometheus/grafana_template](https://github.com/emqx/emqx-prometheus/tree/master/grafana_template)。

## HTTP API 监控指标

> 从 EMQX Enterprise e4.4.34 开始支持。

EMQX Enterprise 提供 HTTP API 请求的监控指标，通过 Prometheus 端点自动导出。这些指标可用于监控 REST API 的健康状况和性能表现。

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

{% endemqxee %}
