# 运维操作

EMQX 通过包括日志在内的一系列的可观测性功能帮助用户进行系统监控和调试支持，具体包括：

- [日志](../getting-started/log.md)

  EMQX 的日志中记录了客户端访问、操作系统或网络异常等问题。您可基于日志信息进行问题排查或系统性能优化。

- [指标](../advanced/metrics-and-stats.md)

  EMQX 为用户提供了丰富的指标来帮助用户与因为人员了解当前服务状态，监测和排除系统的性能问题。

  您可通过 EMQX Dashboard 或 HTTP API 和系统主题来获取 EMQX 指标信息。

- [告警](../advanced/alarms.md)

  EMQX 内置监控与告警功能，目前支持监控 CPU 占用率、系统与进程的内存占用率、进程数量、规则引擎资源状态、网络分区与愈合，并会在发现异常时进行告警。

- [$SYS 系统主题](../advanced/system-topic.md)

  EMQX 周期性发布自身运行状态、消息统计、客户端上下线事件到以 `$SYS/` 开头系统主题。

- [测试/调优](../modules/recon)

  本节介绍了如何启用 EMQX 的 Recon 模块进行测试调优。 

- [主题指标统计](../modules/topic_metrics.md)

  EMQX 提供了主题指标统计功能，可以统计指定主题下的消息收发数量、速率等指标。

- [集成 Prometheus](https://docs.emqx.com/zh/enterprise/v5.0/observability/prometheus.html)

  [Prometheus](https://prometheus.io/)是由 SoundCloud 开源的监控告警解决方案，支持多维数据模型、灵活的查询语言、强大的告警管理等特性。EMQX 支持集成 Prometheus 用于监测系统指标，同时还支持向 `pushgateway` 推送指标。



