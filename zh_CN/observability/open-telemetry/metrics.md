# 集成 OpenTelemetry 查看 EMQX 指标

EMQX 内置支持通过 gRPC OTEL 协议将指标直接推送到 OpenTelemetry Collector。然后 Collector 可以将数据路由、过滤并转换到任何想要使用的后端进行存储和可视化。

本页面介绍了如何通过 EMQX Dashboard 将 OpenTelemetry 与 EMQX 集成，并通过 [Prometheus](../../observability/prometheus.md) 查看 EMQX 指标。

## 前置准备

在集成 Opentelemetry 之前，您需要先部署和配置 OpenTelemetry 以及 Prometheus。

- 部署 [OpenTelemetry Collector](https://opentelemetry.io/docs/collector/getting-started)。
- 配置 Collector gRPC接收端口（端口默认为 4317 ），及导出为 Prometheus Metrics 的端口（8889）。

```yaml
# otel-collector-config.yaml
receivers:
  otlp:
    protocols:
      grpc:

exporters:
  prometheus:
    endpoint: "0.0.0.0:8889"

processors:
  batch:

service:
  pipelines:
    metrics:
      receivers: [otlp]
      processors: [batch]
      exporters: [prometheus]
```

- 部署 [Prometheus](https://prometheus.io/docs/prometheus/latest/installation)。
- 配置 Prometheus 拉取 Collector 收集的指标。

```yaml
# prometheus.yaml
scrape_configs:
  - job_name: 'otel-collector'
    scrape_interval: 10s
    static_configs:
      - targets: ['otel-collector:8889'] # emqx metrics
      - targets: ['otel-collector:8888'] # collector metrics
```

## 在 EMQX 中启用 OpenTelemetry 指标

本节指导您如何在 EMQX 中启用 OpenTelemetry 指标。你也可以在 Dashboard **管理** -> **监控** 页面下的 **监控集成** 选项卡中配置 OpenTelemetry 指标集成。

将以下配置添加到 EMQX `cluster.hocon` 文件中（假设 EMQX 在本地机器上运行）：

   ```bash
   opentelemetry {
     exporter { endpoint = "http://localhost:4317" }
     metrics {
        interval = "10s"
     }
   }
   ```

## 通过 Prometheus 查看 EMQX 指标

通过 Prometheus 的控制台（`http://otel-collector:9090`）可以查看到 EMQX 指标：

![OpenTelemetry-Prometheus](./assets/opentelemetry-prometheus.png)
