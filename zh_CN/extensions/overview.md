# 日志及可观测性

EMQX 通过包括日志在内的一系列的可观测性功能帮助用户进行系统监控和调试支持，具体包括：

- [日志](./log.md)

  EMQX 的日志中记录了客户端访问、操作系统或网络异常等问题。您可基于日志信息进行问题排查或系统性能优化。

- [指标](./metrics-and-stats.md)

  EMQX 为用户提供了丰富的指标来帮助用户与因为人员了解当前服务状态，监测和排除系统的性能问题。

  您可通过 EMQX Dashboard 或 HTTP API 和系统主题来获取 EMQX 指标信息。

- [集成 Prometheus](./prometheus.md)

  [Prometheus](https://prometheus.io/) 是由 SoundCloud 开源的监控告警解决方案，支持多维数据模型、灵活的查询语言、强大的告警管理等特性。EMQX 支持集成 Prometheus 用于监测系统指标，同时还支持向 `pushgateway` 推送指标。

- [集成 StatsD](./statsd.md)

  [StatsD](https://github.com/statsd/statsd) 最初由Etsy开发的，通过在应用程序中添加统计信息收集代码来监视其网站的性能和健康状况。EMQX 支持向服务器周期性推送系统指标。

- [日志追踪](./tracer.md)

  EMQX 5.x 新增了在线日志追踪(Trace)功能，支持用户指定客户端 ID、主题或 IP 实时过滤输出 **DEBUG** 级别日志，提升问题排查效率。

- [告警](./alarms.md)

  EMQX 内置监控与告警功能，目前支持监控 CPU 占用率、系统与进程的内存占用率、进程数量、规则引擎资源状态、集群脑裂与愈合，并会在发现异常时进行告警。

- [主题监控](./topic-metrics.md)

  EMQX 提供了主题监控功能，可以统计指定主题下的消息收发数量、速率等指标。您可以通过 Dashboard 的 **问题分析** -> **主题监控** 页面查看和使用这一功能，也可以通过 HTTP API 完成相应操作。

- [慢订阅统计](./slow-subscribers-statistics.md)

  针对客户端偶尔出现订阅消息时延等情况，EMQX 提供了慢订阅统计功能，进一步提升消息的传输效率。

