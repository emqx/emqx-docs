# 集成 OpenTelemetry
[OpenTelemetry](https://opentelemetry.io/docs/what-is-opentelemetry/) 是一个用于管理 metrics、 traces 和 logs 等遥测数据的可观测性框架。它提供了一套标准的 API 和约定，帮助开发人员在应用程序中生成遥测数据，并将其发送到后端存储和可视化工具。

EMQX 支持通过 gRPC OTEL 协议直接把遥测数据推送到 OpenTelemetry Collector，再经 Collector 中转、过滤或转换数据到任意你想要集成的后台中作存储和可视化，例如 Jaeger 和 [Prometheus](../../observability/prometheus.md)。通过与 OpenTelemetry 集成，可以优化 EMQX 的指标收集、消息发布的分布式追踪以及日志的统一收集与上下文关联。这种集成能够帮助用户实现 EMQX 的可视化监控和告警通知，跟踪消息在不同系统和服务之间的流动。这对于持续性能优化、更快地定位问题和进行系统监控有很大的帮助。

::: tip

集成 OpenTelemetry 于 EMQX 5.8.3 版本转为 EMQX 企业版功能。

:::

<img src="./assets/emqx-opentelemetry.jpg" alt="emqx-opentelemetry" style="zoom:67%;" />

本章节介绍了 EMQX 如何将遥测数据与 OpenTelemetry Collector 集成，完整实现对以下可观测性信息的 OpenTelemetry 内置支持：

- [指标（Metrics）](./metrics.md)
- [追踪（Traces）](./traces.md)
- [日志（Logs）](./logs.md)

此外，在 EMQX 5.8.3 以上版本中，还支持基于 OpenTelemetry 的 **端到端追踪** 功能：

- [端到端追踪（e2e Traces）](./e2e-traces.md)
