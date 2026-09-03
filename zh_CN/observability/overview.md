# 日志及可观测性

EMQX 提供内置可观测性功能、外部集成和生产监控指南，帮助您监控和管理部署，并诊断运行问题。本节文档涵盖生产监控、可观测性数据与集成以及诊断工具。

**生产监控：**

- [生产监控最佳实践](./monitoring-best-practices.md)

  设计生产监控和告警，识别可用性、容量、依赖项和消息交付风险，并及时安排维护。

**可观测性与集成：**

- [指标](./metrics-and-stats.md)

  EMQX 为用户提供了丰富的指标来帮助用户与运维人员了解当前服务状态，监测和排除系统的性能问题。您可通过 EMQX Dashboard、HTTP API 和系统主题来获取 EMQX 指标信息。

- [告警](./alarms.md)

  EMQX 内置监控与告警功能，目前支持监控 CPU 占用率、系统与进程的内存占用率、进程数量、规则引擎资源状态、集群脑裂与愈合，并会在发现异常时进行告警。

- [日志](./log.md)

  EMQX 的日志中记录了客户端访问、操作系统或网络异常等问题。您可基于日志信息进行问题排查或系统性能优化。

- [集成 Prometheus](./prometheus.md)

  [Prometheus](https://prometheus.io/) 是由 SoundCloud 开源的监控告警解决方案，支持多维数据模型、灵活的查询语言、强大的告警管理等特性。EMQX 支持集成 Prometheus 用于监测系统指标，同时还支持向 `pushgateway` 推送指标。

- [Broker 健康指标](./broker-health-indicators.md)

  用于监控 EMQX broker 的常用 Prometheus 指标精选参考，按系统、broker、认证与授权、数据集成四个领域组织。请配合 Prometheus 集成文档一起使用，用于决定抓取哪些指标、对哪些指标设置告警以及在仪表盘上展示哪些曲线。

- [集成 Datadog](./datadog.md)

  [Datadog](https://www.datadoghq.com/) 是一款可观测性平台，为应用程序提供统一、实时的可观测性和安全性解决方案。EMQX 支持集成 Datadog 用于了解 EMQX 运行状态、监测和排查系统性能问题，还可以在 Datadog 控制台上查看 EMQX 指标。

**诊断工具：**

- [主题监控](./topic-metrics.md)

  EMQX 提供了主题监控功能，可以统计指定主题下的消息收发数量、速率等指标。您可以通过 Dashboard 的**问题分析** -> **主题监控**页面查看和使用这一功能，也可以通过 HTTP API 完成相应操作。

- [慢订阅统计](./slow-subscribers-statistics.md)

  针对客户端偶尔出现订阅消息时延等情况，EMQX 提供了慢订阅统计功能，进一步提升消息的传输效率。

- [日志追踪](./tracer.md)

  EMQX 5.x 新增了在线日志追踪（Trace）功能，支持用户指定客户端 ID、主题或 IP 实时过滤输出 DEBUG 级别日志，提升问题排查效率。

- [Mria 日志与告警](./mria-alarms.md)

  介绍 Mria 数据库管理系统上报的日志消息与告警，包括如何识别和解读网络分区事件及集群恢复进度。
