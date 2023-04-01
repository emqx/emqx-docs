# Logs and Observability

EMQX provides a series of observability-related features to help with the system monitoring and tuning:

- [Logs](./log.md)

  Logs provide a reliable source of information for troubleshooting and system performance optimization. You can find the record about the access, operating or network issues from EMQX logs.

- [Metrics](./metrics-and-stats.md)

  EMQX provides metrics monitoring functions, based on which the operation and maintenance personnel can monitor the current service status and troubleshoot possible system malfunctions. Users can use the EMQX Dashboard, HTTP API, and system topics to trace the metrics data. 

- [Integrate with Prometheus](./prometheus.md)

  [Prometheus](https://prometheus.io/) is the monitoring solution open-sourced by SoundCloud, featuring its support to multidimensional data model, flexible query language, and powerful alarm management. EMQX supports integrating with Prometheus to collect system metrics and as well as pushing metrics to  `pushgateway`.

- [Log Trace](./tracer.md)

  EMQX 5.x has added the Log Trace feature, allowing users only to enable debug level logs output for specific client IDs, topics or IPs in real-time.

- [Alarm](./alarms.md)

  EMQX has offered a built-in monitoring and alarm functionality for monitoring the CPU occupancy, system and process memory occupancy, number of processes, rule engine resource status, cluster partition and healing, and will raise an alarm in case of system malfunctions.

- [Topic Metrics](./topic-metrics.md)

  EMQX provides a topic monitoring feature(called Topic Metrics) that allows you to count the number of messages sent and received, the rate and other metrics for a given topic. You can view and use this feature through the Dashboard's **Diagnose** -> **Topic Metrics** page, or you can configure through the HTTP API.

- [Slow Subscriptions](./slow-subscribers-statistics.md)

  Typically, EMQX will finish the message transmission within milliseconds, affected mainly by the network. However, there are cases where the latency of subscription messages is very high on the client side. To solve this problem, EMQX provides a Slow subscriptions feature.

