# Logs and Observability

EMQX provides a series of observability-related features to help with system monitoring, management, and diagnosing. All these features can be accessed and configured on Dashboard under the following menu items:

**Monitoring**:

- [Metrics](./metrics-and-stats.md)

  EMQX provides metrics monitoring functions, based on which the operation and maintenance personnel can monitor the current service status and troubleshoot possible system malfunctions. Users can use the EMQX Dashboard, HTTP API, and system topics to trace the metrics data. 

- [Alarm](./alarms.md)

  EMQX has offered a built-in monitoring and alarm functionality for monitoring the CPU occupancy, system and process memory occupancy, number of processes, rule engine resource status, cluster partition and healing, and will raise an alarm in case of system malfunctions.

**Management**:

- [Logs](./log.md)

  Logs provide a reliable source of information for troubleshooting and system performance optimization. You can find the record about the access, operating or network issues from EMQX logs.

- [Audit Log](./audit-log.md)

  Audit Log records the important operation changes in your EMQX cluster in real-time. It is a critical tool for enterprise users to comply with regulatory requirements and ensure data security.

- [Integrate with Prometheus](./prometheus.md)

  [Prometheus](https://prometheus.io/) is the monitoring solution open-sourced by SoundCloud, featuring its support to multidimensional data model, flexible query language, and powerful alarm management. EMQX supports integrating with Prometheus to collect system metrics and as well as pushing metrics to `pushgateway`.

**Diagnose**:

- [Topic Metrics](./topic-metrics.md)

  EMQX provides a topic monitoring feature(called Topic Metrics) that allows you to count the number of messages sent and received, the rate and other metrics for a given topic. You can view and use this feature through the **Diagnose** -> **Topic Metrics** page on Dashboard, or you can configure it through the HTTP API.

- [Slow Subscriptions](./slow-subscribers-statistics.md)

  Typically, EMQX will finish the message transmission within milliseconds, affected mainly by the network. However, there are cases where the latency of subscription messages is very high on the client side. To solve this problem, EMQX provides a Slow subscriptions feature.

- [Log Trace](./tracer.md)

  EMQX 5.x has added the Log Trace feature, allowing users only to enable debug level logs output for specific client IDs, topics or IPs in real-time.



