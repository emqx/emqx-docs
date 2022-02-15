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

EMQ X 提供 [emqx_statsd](https://github.com/emqx/emqx-statsd) 插件，用于将系统的监控数据输出到第三方的监控系统中。

以 [Prometheus](https://prometheus.io) 为例：

`emqx_statsd` 支持将数据推送至 Pushgateway 中，然后再由 Promethues Server 拉取进行存储。

注意：`emqx_statsd` 不支持 Prometheus 的 Pull 操作。

## 配置

`emqx_statsd` 插件内部会启动一个定时器，使其每间隔一段时间便采集 EMQ X 中的监控数据。

`emqx_statsd` 推送的监控数据包含的具体字段和含义，参见：[Metrics & Stats](../advanced/metrics-and-stats.md)

配置文件位于 `etc/plugins/emqx_statsd.conf`，其中：

|  配置项             | 类型    | 可取值    | 默认值                | 说明                           |
| ------------------- | ------- | --------- | --------------------- | ------------------------------ |
| push.gateway.server | string  | -         | http://127.0.0.1:9091 | Prometheus 的 PushGateway 地址 |
| interval            | integer | > 0       | 5000                  | 推送间隔，单位：毫秒           |

### Grafana 数据模板

`emqx_statsd` 插件提供了 Grafana 的 Dashboard 的模板文件。这些模板包含了所有 EMQ X 监控数据的展示。用户可直接导入到 Grafana 中，进行显示 EMQ X 的监控状态的图标。

模板文件位于：[emqx_statsd/grafana_template](https://github.com/emqx/emqx-statsd/tree/master/grafana_template)。

{% endemqxce %}




{% emqxee %}

> 从 EMQ X Enterprise v4.1.0 开始，emqx_statsd 更名为 emqx_prometheus，相关插件名称、目录均有变更。

EMQ X 提供 [emqx_prometheus](https://github.com/emqx/emqx-prometheus) 插件，用于将系统的监控数据输出到第三方的监控系统中。

以 [Prometheus](https://prometheus.io) 为例：

`emqx_prometheus` 支持将数据推送至 Pushgateway 中，然后再由 Promethues Server 拉取进行存储。

注意：`emqx_prometheus` 不支持 Prometheus 的 Pull 操作。

## 配置

`emqx_prometheus` 插件内部会启动一个定时器，使其每间隔一段时间便采集 EMQ X 中的监控数据。

`emqx_prometheus` 推送的监控数据包含的具体字段和含义，参见：[Metrics & Stats](../advanced/metrics-and-stats.md)

配置文件位于 `etc/plugins/emqx_prometheus.conf`，其中：

|  配置项             | 类型    | 可取值    | 默认值                | 说明                           |
| ------------------- | ------- | --------- | --------------------- | ------------------------------ |
| push.gateway.server | string  | -         | http://127.0.0.1:9091 | Prometheus 的 PushGateway 地址 |
| interval            | integer | > 0       | 5000                  | 推送间隔，单位：毫秒           |

### Grafana 数据模板

`emqx_prometheus` 插件提供了 Grafana 的 Dashboard 的模板文件。这些模板包含了所有 EMQ X 监控数据的展示。用户可直接导入到 Grafana 中，进行显示 EMQ X 的监控状态的图标。

模板文件位于：[emqx_prometheus/grafana_template](https://github.com/emqx/emqx-prometheus/tree/master/grafana_template)。

{% endemqxee %}
