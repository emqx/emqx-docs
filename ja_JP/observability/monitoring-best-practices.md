# Production Monitoring Best Practices

Production deployments require monitoring beyond EMQX Dashboard. Dashboard shows the current broker state but cannot notify operators if the broker or host becomes unavailable. The monitoring guidance on this page helps you detect loss of service, loss of redundancy, and resource exhaustion early enough to act.

This guidance applies to production deployments of EMQX Enterprise. Treat the example thresholds as starting points and adjust them to your service-level objectives (SLOs), tested capacity, traffic patterns, and recovery time.

## Design a Production Monitoring System

Follow these principles when designing your monitoring system:

1. **Export EMQX metrics to an external monitoring system.**

   [Prometheus Pull mode](./prometheus.md#configure-pull-mode-integration) is recommended for comprehensive monitoring. Scrape every EMQX node directly, rather than through a load balancer, so that a failed or isolated node cannot be hidden by a healthy node. Monitor the Prometheus `up` metric for every target.

2. **Forward EMQX built-in alarms.**

   Configure EMQX built-in alarm thresholds for your environment and send alarm events to an external notification system by using a [Webhook or system topic](./alarms.md#get-alarms). Do not rely on an operator noticing an alarm in the Dashboard.

3. **Run an end-to-end MQTT check from outside the cluster.**

   A synthetic client should connect through the same load balancer, TLS listener, and authentication path as production clients. The client should publish a uniquely identified message, receive it on a subscription, and measure the total latency. This check detects failures that broker metrics alone cannot detect.

4. **Monitor the host or container platform.**

   EMQX does not replace operating system, Kubernetes, or cloud-provider monitoring. Collect CPU throttling, memory pressure, disk capacity and latency, file descriptor use, network errors, container restarts, and time synchronization status.

5. **Collect logs centrally.**

   Send warning, error, and critical logs from every node to storage outside the EMQX cluster. Prefer JSON-formatted logs so that alert rules can match the structured `msg`, `node`, and other context fields. Logs reveal some conditions that are not represented by a metric or built-in alarm.

6. **Keep monitoring independent of EMQX.**

   The monitoring and notification path must remain available when an EMQX node, availability zone, or the entire cluster is unavailable.

::: tip

Collect metrics at a shorter interval than the required detection time. For example, with a 15-second scrape interval and an alert after 2 consecutive failures, you can normally detect an unreachable target in less than 1 minute without paging on a single missed scrape.

:::

## Establish SLOs, Capacity Baselines, and Alert Thresholds

Use the following process instead of copying fixed thresholds into production:

1. Define the user-visible SLOs for connection success, publish-to-delivery success, and latency.
2. Run a representative [performance test](../performance/overview.md) and record resource use, message rates, and latency before saturation.
3. Observe at least 1 normal business cycle and identify daily or weekly peaks.
4. Set warning thresholds below the tested safe capacity, leaving enough time to add capacity or schedule maintenance. Set critical thresholds at the point where immediate action is required.
5. Revisit thresholds after traffic growth, topology changes, upgrades, or changes to persistent sessions and data integrations.

Avoid alerting only on a fixed percentage. Trend and forecast alerts, such as disk exhaustion predicted within 24 hours or connections reaching tested capacity within a week, often provide more useful maintenance lead time.

## Leading Indicators to Monitor

Preventive alerts should detect a deteriorating condition while the cluster is still serving traffic. Use a warning threshold that leaves time for investigation and maintenance, and a critical threshold for conditions that require immediate action. For each condition, the following guidance lists relevant signals and suggested actions for operators.

### Cluster and Runtime Health

**Mria replication pressure**

- **Early-warning condition:** Replication lag or queues stay above their normal peak, or continue growing instead of draining.
- **Relevant signals:** On Replicant nodes, monitor `emqx_mria_lag`, `emqx_mria_message_queue_len`, and `emqx_mria_replayq_len`. On Core nodes, monitor `emqx_mria_server_mql` and `emqx_mria_weight`.
- **Suggested actions:** Correlate the metric change with logs from both the Replicant and its upstream Core node. Look for lag-collection failures, busy distribution ports, long scheduler pauses, Mnesia overload, and Mria replication errors. Then check network latency and loss, CPU, and disk I/O. Reduce write pressure or add Core capacity before Replicants fall further behind.

`emqx_mria_lag` is the number of transactions by which a Replicant shard is behind its upstream Core shard; it is not a duration in seconds. Short spikes can be normal during bursts of writes. Alert when the value remains above the maximum observed during representative peak traffic, or when it and the Mria queue metrics show a sustained positive trend. Group alerts by both node and `shard`, because one shard can be unhealthy while others continue to replicate normally. For details about each Mria metric, see [Monitor and Debug](../deploy/cluster/mria-introduction.md#monitor-and-debug).

**Configuration convergence**

- **Early-warning condition:** `emqx_conf_sync_txid` differs between nodes for longer than a normal configuration rollout.
- **Relevant signals:** `emqx_conf_sync_txid` from every node and configuration synchronization logs.
- **Suggested actions:** Stop further configuration changes, identify the node that is behind, and inspect cluster connectivity and configuration synchronization errors. Restore convergence before maintenance or another configuration change.

**Runtime backlog**

- **Early-warning condition:** Run queue or mailbox sizes stay above their established baseline.
- **Relevant signals:** `emqx_vm_run_queue`, `emqx_vm_mnesia_tm_mailbox_size`, `emqx_vm_broker_pool_max_mailbox_size`, built-in overload alarms, and `busy_dist_port` events.
- **Suggested actions:** Investigate sustained overload, slow storage, or cluster communication before request latency and queues grow further.

### Resource and Capacity

**CPU pressure**

- **Early-warning condition:** CPU remains above the normal peak for 10 to 15 minutes.
- **Relevant signals:** `emqx_vm_cpu_use`, host CPU, load, and container throttling.
- **Suggested actions:** Find the workload or integration causing the increase. Rebalance traffic or add capacity before saturation. EMQX's built-in CPU alarm defaults to 80%.

**Memory pressure**

- **Early-warning condition:** Memory remains above the warning threshold or is growing toward the host or container limit.
- **Relevant signals:** `emqx_vm_used_memory`, `emqx_vm_total_memory`, host or container memory, and EMQX memory alarms.
- **Suggested actions:** Inspect connection, session, queue, and integration growth. Add capacity or reduce the source of growth before the operating system terminates the process. EMQX's built-in system-memory alarm defaults to 70%.

**Overload-protection activity**

- **Early-warning condition:** Overload-protection counters increase, especially connection closures or delay timeouts.
- **Relevant signals:** `emqx_overload_protection_new_conn`, `emqx_overload_protection_delay_timeout`, `emqx_overload_protection_delay_ok`, `emqx_overload_protection_gc`, and `emqx_overload_protection_hibernation`. These metrics are exported only when overload protection is enabled.
- **Suggested actions:** The broker is already mitigating resource pressure. Correlate the events with CPU, memory, run queue, mailboxes, and connection churn. Reduce load or add capacity before more client traffic is affected.

**Disk pressure**

- **Early-warning condition:** Free space falls below the operational reserve or is predicted to run out before the next maintenance window.
- **Relevant signals:** Host or volume free bytes, free inodes, I/O latency, and disk growth rate.
- **Suggested actions:** Remove data according to the retention policy or expand the volume. A common starting point is a warning at 20% free and a critical alert at 10% free.

**Broker capacity**

- **Early-warning condition:** Connections, sessions, subscriptions, or topics approach the tested or licensed operating limit.
- **Relevant signals:** `emqx_connections_count`, `emqx_sessions_count`, `emqx_subscriptions_count`, `emqx_topics_count`, and, in EMQX Enterprise, `emqx_license_max_sessions`.
- **Suggested actions:** Compare growth with capacity-test results. Add nodes or move traffic before reaching the limit. Do not treat the historical `*_max` gauges as configured capacity limits.

### Message Delivery and Dependencies

**Message loss**

- **Early-warning condition:** Unexpected drop counters increase.
- **Relevant signals:** `emqx_messages_dropped_*` and `emqx_delivery_dropped_*`.
- **Suggested actions:** Investigate the specific reason. Queue-full, quota-exceeded, receive-maximum, and expired-message drops can indicate overload or incorrect limits. `no_subscribers` and `no_local` drops can be expected in some applications.

**Authentication and authorization dependency health**

- **Early-warning condition:** An enabled provider or source is not connected (its status is `0`), authentication or authorization latency rises above the normal peak, or authentication failures or authorization denials increase unexpectedly.
- **Relevant signals:** `emqx_authn_enable`, `emqx_authn_status`, `emqx_authn_latency`, `emqx_authn_failed`, `emqx_authz_enable`, `emqx_authz_status`, `emqx_authz_latency`, and `emqx_authz_deny` from `/api/v5/prometheus/auth`.
- **Suggested actions:** Check the external database, HTTP service, LDAP server, network, and connection pool. Correlate a failure spike with client traffic to distinguish a backend problem from invalid credentials, an application change, or an attack.

**Data integration health**

- **Early-warning condition:** An enabled connector or action is disconnected; `emqx_action_queuing` or `emqx_action_inflight` grows instead of draining; or late replies, retries, failures, or drops increase.
- **Relevant signals:** `emqx_connector_enable`, `emqx_connector_status`, `emqx_action_enable`, `emqx_action_status`, `emqx_action_queuing`, `emqx_action_inflight`, and related action metrics from `/api/v5/prometheus/data_integration`, plus EMQX `resource` alarms.
- **Suggested actions:** Check the external service and network, then verify buffer capacity and retry behavior. Growing queues and in-flight requests can provide warning before failures and drops begin.

### Expiry Risks

**Certificate and license expiry**

- **Early-warning condition:** Expiry is within the organization's renewal lead time.
- **Relevant signals:** `emqx_cert_expiry_at` and, in EMQX Enterprise, `emqx_license_expiry_at`.
- **Suggested actions:** Renew and deploy the certificate or license. A common starting point is a warning 30 days before expiry and a critical alert 7 days before expiry.

### Verify Metric Availability

For the descriptions of broker counters displayed in the Dashboard, see [Statistics and Metrics](./metrics-and-stats.md). Basic broker, authentication and authorization, and data integration metrics are exposed through separate Prometheus endpoints. Metric availability can vary by edition and enabled features. Inspect the relevant endpoint in your deployment before creating rules.

## Centralize Logs and Alert Selectively

### Collect Logs Outside the Cluster

Do not keep the only copy of a node's logs on that node. A node failure can make the evidence needed to diagnose it unavailable. Send logs from every node to a central system outside the EMQX cluster and include labels for the cluster, node, node role, EMQX version, and availability zone.

Use [JSON log format](./log.md#log-format) and retain at least warning, error, and critical events. Logs can be collected from console or file output, or exported through [OpenTelemetry](./opentelemetry/logs.md). For configuration and production collection guidance, see [Logs](./log.md).

Monitor the log collector and transport by using their health metrics or an explicit heartbeat that does not depend on application log volume. Do not alert merely because a node produces no logs; an idle or healthy node may have nothing to report at the configured severity.

### Define Targeted Log Alerts

Use the following events and guidance to define log-based alert rules:

| Condition | Log Signal | Alerting Guidance |
| --- | --- | --- |
| Mria lag observation failed | `prometheus_mria_shard_lag_refresh_exception` | Alert if it occurs repeatedly. The exporter caches Mria lag; if a refresh times out, the previous value can continue to be exported and appear stable. |
| Erlang VM or inter-node communication pressure | `busy_dist_port`, `long_schedule`, `long_gc`, and Mnesia overload messages | Alert on a sustained rate or repeated events and correlate them with Mria queues, CPU, and latency. These events can precede client-visible degradation. |
| Mria replication or topology failure | `gap_in_the_tlog` and `mria_lb_split_brain` | Notify the responsible operator immediately. Capture the node, shard, agent, expected sequence number, and actual sequence number from the structured fields. |
| Buffering or message-queue pressure | `data_bridge_buffer_overflow`, `unrecoverable_resource_error`, and `dropped_msg_due_to_mqueue_is_full` | Alert when these events are unexpected or exceed the application's accepted loss rate. Correlate them with action and message-drop counters. |
| Configuration synchronization failure | `sync_data_from_node_failed` and `cluster_rpc_apply_failed` | Alert immediately when a configuration change or node startup is in progress; verify that all nodes have converged on the intended configuration. |

Not every warning-level log requires immediate operator notification. Authentication failures and malformed client traffic, for example, may be expected at low rates. Base alerts on selected `msg` values, severity levels, sustained event rates, or deviations from the normal baseline. Treat unexpected critical events as immediately actionable.

### Account for Log Throttling

EMQX throttles selected repetitive log events. A log query can therefore undercount the original events. Include `log_events_throttled_during_last_period` in dashboards and alerts, and use its `dropped` field to determine which messages were suppressed. For details, see [Log Throttling](./log.md#log-throttling).

## Detect Failures Separately

The indicators in [Leading Indicators to Monitor](#leading-indicators-to-monitor) provide advance warning, but they do not replace failure-detection alerts. The following conditions indicate that service or redundancy has already been lost. Configure alerts to notify the responsible operator immediately when any of these conditions occurs:

- Prometheus `up == 0`
- A failed synthetic MQTT check
- `emqx_cluster_nodes_running` falling below the planned cluster size
- `emqx_cluster_nodes_stopped` increasing
- An unexpected reset of `emqx_vm_uptime_ms`
- An EMQX `partition` alarm

Use the early-warning indicators described in [Leading Indicators to Monitor](#leading-indicators-to-monitor) to detect deterioration early and provide enough time to schedule maintenance before these failures occur.

## Example Prometheus Alert Rules

You can copy the following configuration as a starting point for Prometheus alert rules. Before using it in production:

- The example uses the job names from the [Prometheus server configuration example](./prometheus.md#prometheus-server-configuration-example). Update the `job` matchers if your scrape jobs use different names.
- The cluster-loss rule assumes a planned cluster size of 3 nodes. Replace `3` with the planned size of your cluster.
- Replace the other example thresholds with values appropriate for your deployment.
- If a Prometheus job contains multiple clusters, aggregate the configuration-convergence rule by a cluster label.
- Add an absolute threshold based on your peak-traffic baseline to the Mria trend rules. The example rules detect a queue or lag with a sustained positive slope, but a large, stable backlog should also trigger an alert.
- Add host- or platform-specific rules for disk exhaustion, memory limits, container restarts, and network health.

```yaml
groups:
  - name: emqx-early-warning
    rules:
      - alert: EMQXMRIAReplicationLagGrowing
        expr: deriv(emqx_mria_lag{job="emqx_stats"}[10m]) > 0
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "Mria replication lag is growing on {{ $labels.instance }} shard {{ $labels.shard }}"

      - alert: EMQXMRIAReplicationQueueGrowing
        expr: deriv(emqx_mria_server_mql{job="emqx_stats"}[10m]) > 0 or deriv(emqx_mria_message_queue_len{job="emqx_stats"}[10m]) > 0 or deriv(emqx_mria_replayq_len{job="emqx_stats"}[10m]) > 0
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "A Mria replication queue is growing on {{ $labels.instance }} shard {{ $labels.shard }}"

      - alert: EMQXSustainedHighCPU
        expr: emqx_vm_cpu_use{job="emqx_stats"} > 80
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "EMQX CPU usage is high on {{ $labels.instance }}"

      - alert: EMQXSustainedHighMemory
        expr: 100 * emqx_vm_used_memory{job="emqx_stats"} / emqx_vm_total_memory{job="emqx_stats"} > 70
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "EMQX host memory usage is high on {{ $labels.instance }}"

      - alert: EMQXOverloadProtectionActive
        expr: sum by (instance) (increase(emqx_overload_protection_new_conn{job="emqx_stats"}[5m])) > 0 or sum by (instance) (increase(emqx_overload_protection_delay_timeout{job="emqx_stats"}[5m])) > 0
        labels:
          severity: warning
        annotations:
          summary: "EMQX overload protection is closing or timing out client work on {{ $labels.instance }}"

      - alert: EMQXConfigurationNotConverged
        expr: max(emqx_conf_sync_txid{job="emqx_stats"}) != min(emqx_conf_sync_txid{job="emqx_stats"})
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "EMQX nodes report different configuration transaction IDs"

      - alert: EMQXDeliveryQueueFullDrops
        expr: sum by (instance) (increase(emqx_delivery_dropped_queue_full{job="emqx_stats"}[5m])) > 0
        labels:
          severity: warning
        annotations:
          summary: "EMQX dropped messages because a delivery queue was full"

      - alert: EMQXActionQueueGrowing
        expr: deriv(emqx_action_queuing{job="emqx_data_integration"}[10m]) > 0
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "EMQX data integration action {{ $labels.id }} has a growing queue on {{ $labels.instance }}"

      - alert: EMQXActionFailures
        expr: sum by (instance, id) (increase(emqx_action_failed{job="emqx_data_integration"}[5m])) > 0
        labels:
          severity: warning
        annotations:
          summary: "EMQX data integration action {{ $labels.id }} is failing"

      - alert: EMQXAuthenticationBackendUnavailable
        expr: (emqx_authn_enable{job="emqx_auth"} == 1 and on (instance, id) emqx_authn_status{job="emqx_auth"} == 0) or (emqx_authz_enable{job="emqx_auth"} == 1 and on (instance, type) emqx_authz_status{job="emqx_auth"} == 0)
        for: 2m
        labels:
          severity: warning
        annotations:
          summary: "An enabled EMQX authentication or authorization backend is unavailable on {{ $labels.instance }}"

      - alert: EMQXCertificateExpiresSoon
        expr: emqx_cert_expiry_at{job="emqx_stats"} > 0 and (emqx_cert_expiry_at{job="emqx_stats"} - time()) < 30 * 24 * 60 * 60
        for: 1h
        labels:
          severity: warning
        annotations:
          summary: "EMQX listener certificate expires within 30 days"

  - name: emqx-failure-detection
    rules:
      - alert: EMQXMetricsTargetDown
        expr: up{job="emqx_stats"} == 0
        for: 30s
        labels:
          severity: critical
        annotations:
          summary: "EMQX metrics target {{ $labels.instance }} is unreachable"

      - alert: EMQXClusterLostNode
        expr: min by (job) (emqx_cluster_nodes_running{job="emqx_stats"}) < 3
        for: 2m
        labels:
          severity: critical
        annotations:
          summary: "EMQX cluster has fewer than 3 running nodes"
```

Counter metrics normally only increase. Alert on their rate or increase over a time window, not on their absolute value. Use a `for` duration with resource gauges so that a short traffic spike does not cause an unnecessary alert.

## Make Alerts Actionable

1. **Define alert context and ownership.**

   Each actionable alert should identify the affected cluster and, when applicable, the affected node, current value, and threshold. It should also include a dashboard link, an owner, and a runbook that documents how to investigate, mitigate, and resolve the alert. The runbook should state how to confirm the condition, protect service, restore redundancy, and decide whether to scale, rebalance, restart, or repair.

2. **Test alert delivery and recovery.**

   Test the full alert path before relying on it. In a non-production environment or during an approved test window, deliberately stop a scrape target, lower a test threshold, and disconnect a test integration. Confirm that the alert reaches the correct operator, contains enough context, and clears after recovery.

3. **Prepare maintenance procedures.**

   Use warning alerts to schedule maintenance while redundancy remains available. Before changing the cluster, verify that backups are usable, the remaining nodes can carry the load, and the alerting system is healthy. Relevant procedures include [Backup and Restore](../operations/backup-restore.md), [Node Evacuation and Cluster Load Rebalancing](../deploy/cluster/rebalancing.md), and [EMQX Enterprise Rolling Upgrade](../deploy/rolling-upgrades.md).

## Production Readiness Checklist

- Every EMQX node and its host or container is visible in the external monitoring system.
- Built-in alarms are forwarded outside EMQX and tested.
- Warning, error, and critical logs from every node are stored centrally, and the collection pipeline is monitored.
- An external synthetic MQTT check covers the production client path.
- Alerts for Mria replication, configuration convergence, and runtime backlog have defined owners and runbooks.
- Alerts for overload protection, CPU, memory, disk, and broker capacity have defined owners and runbooks.
- Alerts for authentication and authorization, message drops, and data integrations have defined owners and runbooks.
- Certificate and license expiry alerts have defined owners and runbooks.
- Selected Mria, VM-pressure, buffer-overflow, and configuration-sync log events have rate-based or immediate alerts appropriate to their severity.
- Separate target-down, synthetic MQTT, cluster-size, and partition alerts detect failures and notify the responsible operator immediately.
- Warning thresholds leave enough time for the team's normal maintenance and capacity-provisioning process.
- Dashboards show both the current value and the trend over a relevant business cycle.
- Alert notifications, backup restoration, and rolling-maintenance procedures are tested regularly.
