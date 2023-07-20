# Logs and Observability

EMQX provides a series of observability-related features to help with system monitoring, management, and diagnosing. All these features can be accessed and configured on Dashboard under the following menu items:

**Monitoring**

- [Metrics](../advanced/metrics-and-stats.md)

  EMQX provides metrics monitoring functions, based on which the operation and maintenance personnel can monitor the current service status and troubleshoot possible system malfunctions. Users can use the EMQX Dashboard, HTTP API, and system topics to trace the metrics data.

  [Alarm](../advanced/alarms.md)

  EMQX has offered a built-in monitoring and alarm functionality for monitoring the CPU occupancy, system and process memory occupancy, number of processes, rule engine resource status, cluster partition and healing, and will raise an alarm in case of system malfunctions.

**Management**

- [Log](../getting-started/log.md)

  Logs provide a reliable source of information for troubleshooting and system performance optimization. You can find the record about the access, operating or network issues from EMQX logs.

- [Integragte with Prometheus](../tutorial/prometheus.md)

  [Prometheus](https://prometheus.io/) is the monitoring solution open-sourced by SoundCloud, featuring its support to multidimensional data model, flexible query language, and powerful alarm management. EMQX supports integrating with Prometheus to collect system metrics and as well as pushing metrics to `pushgateway`.

**Diagnose**

- [$SYS Topic](../advanced/system-topic.md)

  The EMQX Broker periodically publishes its running status, message statistics, client online and offline events to the system topic starting with `$SYS/`.

- [Topic Metrics](../modules/topic_metrics.md)

  EMQX provides a topic monitoring feature (called Topic Metrics) that allows you to count the number of messages sent and received, the rate and other metrics for a given topic.
