# Broker Health Indicators

This page is a curated reference of the most useful Prometheus metrics for monitoring an EMQX broker. Use it together with [Integrate with Prometheus](./prometheus.md), which covers how to expose and scrape these metrics.

The indicators are organized into four areas:

1. **System**: operating system and Erlang VM resources.
2. **Broker**: connection and message traffic, plus broker state.
3. **Authentication and Authorization**: connect-time identity checks and per-message ACL decisions.
4. **Data Integration**: rules, actions, connectors, and bridges.

All metrics are exposed on the EMQX Prometheus endpoints (`/api/v5/prometheus/stats`, `/api/v5/prometheus/auth`, and `/api/v5/prometheus/data_integration`). For endpoint details and `mode` query parameters, see [Integrate with Prometheus](./prometheus.md#configure-pull-mode-integration).

::: tip Note on collector defaults

EMQX-native metrics do not depend on the upstream Erlang VM collectors. In EMQX 6.3, the `vm_dist`, `vm_statistics`, `vm_system_info`, and `vm_memory` fields under `prometheus.collectors` default to `enabled`. The `mnesia` and `vm_msacc` fields default to `disabled`.

Legacy flat collector fields, such as `prometheus.vm_statistics_collector`, retain their legacy default of `disabled`. Explicit collector settings take precedence over these defaults.

:::

## System

The signals closest to the hardware layer. When the broker is unhealthy, one of these usually moves first.

### CPU

| Metric | Description |
|--------|-------------|
| `emqx_vm_cpu_use` | Percent CPU used. |
| `emqx_vm_cpu_idle` | Percent CPU idle. |

### Memory

Metrics with the `erlang_vm_memory_` prefix require `prometheus.collectors.vm_memory` to be `enabled`.

| Metric | Description |
|--------|-------------|
| `emqx_vm_total_memory` | Total system memory (bytes). |
| `emqx_vm_used_memory` | Used system memory (bytes). |
| `erlang_vm_memory_bytes` | Total memory allocated by the Erlang VM, with `kind="system"` or `kind="processes"`. |
| `erlang_vm_memory_processes_bytes` | Memory allocated for Erlang processes, with `usage="used"` or `usage="free"`. |
| `erlang_vm_memory_system_bytes` | System memory by usage, including atoms, binaries, loaded code, ETS tables, and other overhead. |
| `erlang_vm_memory_atom_bytes` | Memory allocated for atoms, with `usage="used"` or `usage="free"`. |

### File Descriptors

| Metric | Description |
|--------|-------------|
| `emqx_vm_max_fds` | Soft FD ulimit for the broker process. |

### Erlang Processes and Scheduler Load

| Metric | Description |
|--------|-------------|
| `emqx_vm_run_queue` | Current scheduler run queue length. A sustained non-zero value indicates CPU saturation. |
| `erlang_vm_processes` | Current Erlang process count (requires the `vm_system_info` collector enabled). |
| `erlang_vm_process_limit` | Configured maximum Erlang processes. |

### Internal Mailbox Watchdogs

| Metric | Description |
|--------|-------------|
| `emqx_vm_mnesia_tm_mailbox_size` | Mnesia transaction manager mailbox depth. High values indicate transactional contention. |
| `emqx_vm_broker_pool_max_mailbox_size` | Largest mailbox in the broker dispatch pool. High values indicate subscriber-side backpressure. |

### Uptime

| Metric | Description |
|--------|-------------|
| `emqx_vm_uptime_ms` | Broker uptime in milliseconds. A sudden drop to a small value means the node restarted. |

### Cluster Replication Health (Mria)

| Metric | Description |
|--------|-------------|
| `emqx_mria_lag` | Replication lag per replicant node. |
| `emqx_mria_replicants` | Replicant count. |
| `emqx_mria_bootstrap_time` | Time required for the last bootstrap. |
| `emqx_mria_message_queue_len` | Mria mailbox length. |

### Overload Protection

| Metric | Description |
|--------|-------------|
| `emqx_overload_protection_new_conn` | Connections refused due to overload. |
| `emqx_overload_protection_gc` | Forced garbage collections triggered by overload protection. |
| `emqx_overload_protection_hibernation` | Process hibernations triggered. |
| `emqx_overload_protection_delay_ok` | Successful delay applications. |
| `emqx_overload_protection_delay_timeout` | Delay attempts that timed out. |

## Broker

Core operational signals. Watch the rate of message-related counters, and pay particular attention to the `dropped` series.

### Cluster Topology

| Metric | Description |
|--------|-------------|
| `emqx_cluster_nodes_running` | Running cluster nodes. |
| `emqx_cluster_nodes_stopped` | Stopped cluster nodes. Alert when this is greater than zero. |
| `emqx_conf_sync_txid` | Last cluster configuration transaction ID applied. Diverging values across nodes indicate a sync issue. |

### License (Enterprise)

| Metric | Description |
|--------|-------------|
| `emqx_license_expiry_at` | License expiration time (UNIX epoch seconds). |
| `emqx_license_issued_at` | License issuance time. |
| `emqx_license_max_sessions` | License session cap. |
| `emqx_cert_expiry_at` | Listener certificate expiration time. |

### Connections, Sessions, and Channels

| Metric | Description |
|--------|-------------|
| `emqx_connections_count` | Current connection count. |
| `emqx_connections_max` | Peak connection count since boot. |
| `emqx_live_connections_count` | Currently connected (TCP up) clients. |
| `emqx_live_connections_max` | Peak live connections. |
| `emqx_sessions_count` | Active session count (includes persistent sessions whose client is currently disconnected). |
| `emqx_sessions_max` | Peak session count. |
| `emqx_cluster_sessions_count` | Cluster-wide session count. |
| `emqx_cluster_sessions_max` | Peak cluster-wide session count. |
| `emqx_channels_count` | Channel processes (one per connected client). |
| `emqx_channels_max` | Peak channel count. |

### Subscriptions and Topics

| Metric | Description |
|--------|-------------|
| `emqx_subscriptions_count` | Subscription count. |
| `emqx_subscriptions_max` | Peak subscriptions. |
| `emqx_subscriptions_shared_count` | Shared subscriptions. |
| `emqx_subscriptions_shared_max` | Peak shared subscriptions. |
| `emqx_subscribers_count` | Subscriber processes. |
| `emqx_topics_count` | Distinct topic count. |
| `emqx_topics_max` | Peak topics. |
| `emqx_routes_count` | Route table size. |
| `emqx_routes_max` | Peak route table size. |
| `emqx_durable_subscriptions_count` | Persistent-session subscriptions. |
| `emqx_durable_subscriptions_max` | Peak persistent-session subscriptions. |

### Retained, Delayed, and Banned

| Metric | Description |
|--------|-------------|
| `emqx_retained_count` | Retained message count. |
| `emqx_retained_max` | Peak retained count. |
| `emqx_delayed_count` | Delayed-publish queue depth. |
| `emqx_delayed_max` | Peak delayed queue depth. |
| `emqx_banned_count` | Banned client / username / IP entries. |

### Messages

| Metric | Description |
|--------|-------------|
| `emqx_messages_received` | Application-level messages received from clients. |
| `emqx_messages_sent` | Application-level messages sent to clients. |
| `emqx_messages_publish` | PUBLISH packets dispatched. |
| `emqx_messages_delivered` | Deliveries to subscribers (one published message can produce multiple deliveries). |
| `emqx_messages_acked` | Acknowledgements received from subscribers. |
| `emqx_messages_forward` | Cross-node message forwards. |
| `emqx_messages_retained` | Retained-message events. |
| `emqx_messages_delayed` | Delayed-publish enqueues. |

### Message Drops (the Earliest Sign of Trouble)

| Metric | Description |
|--------|-------------|
| `emqx_messages_dropped` | Total dropped messages. |
| `emqx_messages_dropped_expired` | Dropped because the message-expiry interval was exceeded. |
| `emqx_messages_dropped_no_subscribers` | Dropped because no subscriber matched. |
| `emqx_messages_dropped_quota_exceeded` | Dropped because a per-client quota was hit. |
| `emqx_messages_dropped_receive_maximum` | Dropped because the subscriber's MQTT v5 receive-maximum quota was hit. |

### Per-Subscriber Delivery Drops

| Metric | Description |
|--------|-------------|
| `emqx_delivery_dropped` | Total deliveries dropped. |
| `emqx_delivery_dropped_expired` | Expired before delivery. |
| `emqx_delivery_dropped_no_local` | MQTT v5 no-local rule. |
| `emqx_delivery_dropped_qos` | QoS not supported. |
| `emqx_delivery_dropped_queue_full` | Subscriber mqueue full. |
| `emqx_delivery_dropped_too_large` | Exceeds subscriber's max packet size. |

### Bytes

| Metric | Description |
|--------|-------------|
| `emqx_bytes_received` | Total bytes received. |
| `emqx_bytes_sent` | Total bytes sent. |

### Packet-Level (for Protocol-Debug Dashboards)

| Metric | Description |
|--------|-------------|
| `emqx_packets_received` | Total packets received. |
| `emqx_packets_sent` | Total packets sent. |
| `emqx_packets_connect` | CONNECT packets received. |
| `emqx_packets_connack_sent` | CONNACK packets sent. |
| `emqx_packets_connack_error` | CONNACK with a non-zero reason code (most per-client AUTHN failures show here). |
| `emqx_packets_disconnect_received` | DISCONNECT packets received. |
| `emqx_packets_disconnect_sent` | DISCONNECT packets sent. |
| `emqx_packets_publish_received` | PUBLISH packets received. |
| `emqx_packets_publish_sent` | PUBLISH packets sent. |
| `emqx_packets_publish_error` | PUBLISH that could not be accepted. |
| `emqx_packets_publish_auth_error` | PUBLISH denied by authorization. |
| `emqx_packets_puback_received` | PUBACK packets received (QoS 1). |
| `emqx_packets_puback_sent` | PUBACK packets sent (QoS 1). |
| `emqx_packets_pubrec_received` | PUBREC packets received (QoS 2). |
| `emqx_packets_pubrec_sent` | PUBREC packets sent (QoS 2). |
| `emqx_packets_pubrel_received` | PUBREL packets received (QoS 2). |
| `emqx_packets_pubrel_sent` | PUBREL packets sent (QoS 2). |
| `emqx_packets_pubcomp_received` | PUBCOMP packets received (QoS 2). |
| `emqx_packets_pubcomp_sent` | PUBCOMP packets sent (QoS 2). |
| `emqx_packets_subscribe_received` | SUBSCRIBE packets received. |
| `emqx_packets_suback_sent` | SUBACK packets sent. |
| `emqx_packets_subscribe_error` | SUBSCRIBE packets that failed. |
| `emqx_packets_subscribe_auth_error` | SUBSCRIBE packets denied by authorization. |
| `emqx_packets_unsubscribe_received` | UNSUBSCRIBE packets received. |
| `emqx_packets_unsuback_sent` | UNSUBACK packets sent. |
| `emqx_packets_unsubscribe_error` | UNSUBSCRIBE packets that failed. |
| `emqx_packets_pingreq_received` | PINGREQ packets received. |
| `emqx_packets_pingresp_sent` | PINGRESP packets sent. |

### Client Lifecycle (Hook Trigger Counters)

| Metric | Description |
|--------|-------------|
| `emqx_client_connect` | CONNECT received. |
| `emqx_client_connack` | CONNACK sent. |
| `emqx_client_connected` | `client.connected` hook fired. |
| `emqx_client_disconnected` | `client.disconnected` hook fired. |
| `emqx_client_disconnected_reason` | Disconnect counts labeled by reason. |
| `emqx_client_subscribe` | Subscribe hook fires. |
| `emqx_client_unsubscribe` | Unsubscribe hook fires. |

### Session Lifecycle

| Metric | Description |
|--------|-------------|
| `emqx_session_created` | Sessions created. |
| `emqx_session_resumed` | Persistent sessions resumed. |
| `emqx_session_takenover` | Sessions taken over by a new client. |
| `emqx_session_discarded` | Sessions discarded (clean start over an existing session). |
| `emqx_session_terminated` | Sessions terminated. |

## Authentication and Authorization

Use these metrics when an HTTP, LDAP, or database backend is in the authentication path, to determine whether the broker or the backend is the slow or failing component.

### Connect-Time Authentication Outcomes

| Metric | Description |
|--------|-------------|
| `emqx_authentication_success` | Successful authentication (excluding anonymous). |
| `emqx_authentication_success_anonymous` | Anonymous pass. |
| `emqx_authentication_failure` | Authentication failures. |

### Authorization Decisions

| Metric | Description |
|--------|-------------|
| `emqx_authorization_allow` | Decisions: allow. |
| `emqx_authorization_deny` | Decisions: deny. |
| `emqx_authorization_nomatch` | No matching rule (falls back to the `no_match` configuration). |
| `emqx_authorization_matched_allow` | Matched-allow rule fired. |
| `emqx_authorization_matched_deny` | Matched-deny rule fired. |
| `emqx_authorization_cache_hit` | Cache hits. |
| `emqx_authorization_cache_miss` | Cache misses. |
| `emqx_authorization_superuser` | Superuser-bypass path. |

### Authentication Chain Status

| Metric | Description |
|--------|-------------|
| `emqx_authn_total` | Configured authentication providers. |
| `emqx_authn_enable` | Enabled flag per provider (0 / 1). |
| `emqx_authn_status` | Resource state per provider. |
| `emqx_authn_users_count` | User record count per provider (for password, mnesia, or DB-backed providers). |

### Authentication Per-Provider Runtime Counters

| Metric | Description |
|--------|-------------|
| `emqx_authn_success` | Successful matches per provider. |
| `emqx_authn_failed` | Failures per provider. |
| `emqx_authn_nomatch` | Ignored per provider (chain continues to the next provider). |
| `emqx_authn_latency` | Backend latency per provider. |

### Authorization Source Status

| Metric | Description |
|--------|-------------|
| `emqx_authz_total` | Configured authorization sources. |
| `emqx_authz_enable` | Enabled flag per source (0 / 1). |
| `emqx_authz_status` | Resource state per source. |
| `emqx_authz_rules_count` | Rule record count per source (file, mnesia, or DB-backed). |

### Authorization Per-Source Runtime Counters

| Metric | Description |
|--------|-------------|
| `emqx_authz_allow` | Decisions: allow per source. |
| `emqx_authz_deny` | Decisions: deny per source. |
| `emqx_authz_nomatch` | Ignored per source (chain continues). |
| `emqx_authz_latency` | Backend latency per source. |

### Built-In DB Sizes

| Metric | Description |
|--------|-------------|
| `emqx_authn_builtin_record_count` | User count in the built-in authentication database. |
| `emqx_authz_builtin_record_count` | Rule count in the built-in authorization database. |

## Data Integration

Traffic enters the rule engine, fans out to actions and connectors, and lands at external systems. Each layer exposes its own counters. Reading them in order shows where messages are being lost.

### Inventory

| Metric | Description |
|--------|-------------|
| `emqx_rules_count` | Configured rules. |
| `emqx_actions_count` | Configured actions. |
| `emqx_connectors_count` | Configured connectors. |
| `emqx_schema_registrys_count` | Schema-registry entries. |

### Per-Resource Status

| Metric | Description |
|--------|-------------|
| `emqx_rule_enable` | Rule enable flag (0 / 1). |
| `emqx_action_enable` | Action enable flag (0 / 1). |
| `emqx_action_status` | Action resource state. |
| `emqx_connector_enable` | Connector enable flag (0 / 1). |
| `emqx_connector_status` | Connector resource state. |

### Rule Engine: Per-Rule Counters

| Metric | Description |
|--------|-------------|
| `emqx_rule_matched` | Messages that matched the rule's WHERE clause. |
| `emqx_rule_passed` | Messages that passed the rule. |
| `emqx_rule_failed` | Rule processing failed. |
| `emqx_rule_failed_exception` | Erlang exception during the rule. |
| `emqx_rule_failed_no_result` | SQL produced no result. |

### Rule Engine: Action Sub-Counters

| Metric | Description |
|--------|-------------|
| `emqx_rule_actions_total` | Action invocations from rules. |
| `emqx_rule_actions_success` | Action returned success. |
| `emqx_rule_actions_failed` | Action failed. |
| `emqx_rule_actions_failed_unknown` | Failure with unknown reason. |
| `emqx_rule_actions_failed_out_of_service` | Downstream resource unhealthy. |
| `emqx_rule_actions_discarded` | Action discarded (for example, rate limited). |

### Action Throughput

| Metric | Description |
|--------|-------------|
| `emqx_action_matched` | Messages routed to the action. |
| `emqx_action_received` | Received at the action queue. |
| `emqx_action_success` | Action call succeeded. |
| `emqx_action_failed` | Action call failed. |
| `emqx_action_late_reply` | Response arrived after timeout. |
| `emqx_action_retried` | Retry attempts. |
| `emqx_action_retried_success` | Succeeded after retry. |
| `emqx_action_retried_failed` | Failed after all retries. |

### Action Queue and Inflight

| Metric | Description |
|--------|-------------|
| `emqx_action_inflight` | In-flight requests. |
| `emqx_action_queuing` | Queued (pending dispatch) length. |

### Action Drops

A non-zero rate on any of these series indicates that a downstream system is unhealthy or the action is misconfigured.

| Metric | Description |
|--------|-------------|
| `emqx_action_dropped` | Total dropped at the action layer. |
| `emqx_action_dropped_queue_full` | Queue cap hit. |
| `emqx_action_dropped_resource_stopped` | Target resource stopped. |
| `emqx_action_dropped_resource_not_found` | Target resource missing. |
| `emqx_action_dropped_expired` | Message expired before dispatch. |
| `emqx_action_dropped_other` | Other reasons. |

## Minimal "Broker-is-Sick" Panel

If only a handful of series can fit on a Grafana dashboard, these are the ones that matter most. Most production issues move at least one of them within seconds of the event:

- `rate(emqx_messages_dropped[1m])`: non-zero means the broker is refusing or losing work.
- `rate(emqx_action_dropped[1m])`: the integration layer is losing work.
- `emqx_cluster_nodes_stopped`: greater than zero means a member was lost.
- `rate(emqx_overload_protection_new_conn[1m])`: the broker is actively rejecting new connections.
- `rate(emqx_authentication_failure[1m])`: a spike usually indicates a backend issue or an attack.
- `emqx_vm_run_queue`: sustained above zero means CPU saturation.
- `emqx_mria_lag`: values above a few seconds mean replication is falling behind.
- `emqx_license_expiry_at - time()` (Enterprise): countdown to license expiration.

## Prometheus Metric Compatibility in EMQX 6.3

EMQX 6.3 uses promtool-compliant names for Prometheus VM and Mnesia collector metrics. Update PromQL expressions, recording and alerting rules, and custom Grafana dashboards that use the previous names.

| Before EMQX 6.3 | EMQX 6.3 |
| --- | --- |
| `erlang_mnesia_failed_transactions` | `erlang_mnesia_failed_transactions_total` |
| `erlang_mnesia_committed_transactions` | `erlang_mnesia_committed_transactions_total` |
| `erlang_mnesia_logged_transactions` | `erlang_mnesia_logged_transactions_total` |
| `erlang_mnesia_restarted_transactions` | `erlang_mnesia_restarted_transactions_total` |
| `erlang_vm_memory_atom_bytes_total` | `erlang_vm_memory_atom_bytes` |
| `erlang_vm_memory_bytes_total` | `erlang_vm_memory_bytes` |
| `erlang_vm_memory_processes_bytes_total` | `erlang_vm_memory_processes_bytes` |
| `erlang_vm_memory_system_bytes_total` | `erlang_vm_memory_system_bytes` |
| `erlang_vm_statistics_context_switches` | `erlang_vm_statistics_context_switches_total` |
| `erlang_vm_statistics_garbage_collection_number_of_gcs` | `erlang_vm_statistics_garbage_collection_number_of_gcs_total` |
| `erlang_vm_statistics_garbage_collection_words_reclaimed` | `erlang_vm_statistics_garbage_collection_words_reclaimed_total` |
| `erlang_vm_statistics_garbage_collection_bytes_reclaimed` | `erlang_vm_statistics_garbage_collection_bytes_reclaimed_total` |
| `erlang_vm_statistics_runtime_milliseconds` | `erlang_vm_statistics_runtime_seconds_total` |
| `erlang_vm_statistics_wallclock_time_milliseconds` | `erlang_vm_statistics_wallclock_time_seconds_total` |
| `erlang_vm_port_count` | `erlang_vm_ports` |
| `erlang_vm_process_count` | `erlang_vm_processes` |
| `erlang_vm_atom_count` | `erlang_vm_atoms` |

The `emqx_vm_process_messages_in_queues` metric is no longer exported in EMQX 6.3. Remove queries that use this metric. EMQX 6.3 does not provide a direct replacement.
