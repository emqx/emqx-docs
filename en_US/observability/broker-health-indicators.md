# Broker Health Indicators

This page is a curated reference of the most useful Prometheus metrics for monitoring an EMQX broker. Use it together with [Integrate with Prometheus](./prometheus.md), which covers how to expose and scrape these series.

The indicators are organized into four areas:

1. **System** — operating system and Erlang VM resources.
2. **Broker** — connection and message traffic, plus broker state.
1. **System**: operating system and Erlang VM resources.
2. **Broker**: connection and message traffic, plus broker state.
3. **Authentication and Authorization**: connect-time identity checks and per-message ACL decisions.
4. **Data integration**: rules, actions, connectors, and bridges.
4. **Data integration** — rules, actions, connectors, and bridges.

All metrics are exposed on the EMQX Prometheus endpoints (`/api/v5/prometheus/stats`, `/api/v5/prometheus/auth`, and `/api/v5/prometheus/data_integration`). For endpoint details and `mode` query parameters, see [Integrate with Prometheus](./prometheus.md#configure-pull-mode-integration).

::: tip Note on collector defaults

The metrics prefixed `emqx_` are always on. The richer Erlang VM metrics prefixed `erlang_vm_` come from the upstream Prometheus Erlang exporter and are **disabled by default** in EMQX 6.0 and newer. Turn them on by setting `prometheus.collectors.vm_system_info = enabled` and similar for `vm_memory` and `vm_statistics` if you want process counts, per-allocator memory, or GC / scheduler breakdowns.

:::

## System

The closest-to-the-iron signals. If the broker is unhealthy, one of these usually moves first.

### CPU

- `emqx_vm_cpu_use` — percent CPU used.
- `emqx_vm_cpu_idle` — percent CPU idle.

### Memory

- `emqx_vm_total_memory` — total system memory (bytes).
- `emqx_vm_used_memory` — used system memory (bytes).
- `erlang_vm_memory_processes`, `erlang_vm_memory_atom`, `erlang_vm_memory_binary`, `erlang_vm_memory_ets`, `erlang_vm_memory_code`, `erlang_vm_memory_system` — per-allocator memory breakdown (requires the `vm_memory` collector enabled).

### File descriptors

- `emqx_vm_max_fds` — soft FD ulimit for the broker process.

### Erlang processes and scheduler load

- `emqx_vm_run_queue` — current scheduler run queue length. A sustained non-zero value indicates CPU saturation.
- `emqx_vm_process_messages_in_queues` — sum of all Erlang process mailbox lengths. Large or growing values mean a process is unable to keep up with incoming work.
- `erlang_vm_process_count` — current Erlang process count (requires the `vm_system_info` collector enabled).
- `erlang_vm_process_limit` — configured maximum Erlang processes.

### Internal mailbox watchdogs

- `emqx_vm_mnesia_tm_mailbox_size` — Mnesia transaction manager mailbox depth; high values indicate transactional contention.
- `emqx_vm_broker_pool_max_mailbox_size` — largest mailbox in the broker dispatch pool; high values indicate subscriber-side backpressure.

### Uptime

- `emqx_vm_uptime_ms` — broker uptime in milliseconds. A sudden drop to a small value means the node restarted.

### Cluster replication health (Mria)

- `emqx_mria_lag` — replication lag per replicant node.
- `emqx_mria_replicants` — replicant count.
- `emqx_mria_bootstrap_time` — time required for the last bootstrap.
- `emqx_mria_message_queue_len` — Mria mailbox length.

### Overload protection

- `emqx_overload_protection_new_conn` — connections refused due to overload.
- `emqx_overload_protection_gc` — forced garbage collections triggered by overload protection.
- `emqx_overload_protection_hibernation` — process hibernations triggered.
- `emqx_overload_protection_delay_ok` — successful delay applications.
- `emqx_overload_protection_delay_timeout` — delay attempts that timed out.

## Broker

Headline operational signals. Watch the rate of message-related counters, and pay particular attention to the `dropped` series.

### Cluster topology

- `emqx_cluster_nodes_running` — running cluster nodes.
- `emqx_cluster_nodes_stopped` — stopped cluster nodes. Alert when this is greater than zero.
- `emqx_conf_sync_txid` — last cluster configuration transaction ID applied. Diverging values across nodes indicate a sync issue.

### License (Enterprise)

- `emqx_license_expiry_at` — license expiration time (UNIX epoch seconds).
- `emqx_license_issued_at` — license issuance time.
- `emqx_license_max_sessions` — license session cap.
- `emqx_cert_expiry_at` — listener certificate expiration time.

### Connections, sessions, and channels

- `emqx_connections_count` — current connection count.
- `emqx_connections_max` — peak connection count since boot.
- `emqx_live_connections_count` — currently connected (TCP up) clients.
- `emqx_live_connections_max` — peak live connections.
- `emqx_sessions_count` — active session count (includes persistent sessions whose client is currently disconnected).
- `emqx_sessions_max` — peak session count.
- `emqx_cluster_sessions_count` — cluster-wide session count.
- `emqx_cluster_sessions_max` — peak cluster-wide session count.
- `emqx_channels_count` — channel processes (one per connected client).
- `emqx_channels_max` — peak channel count.

### Subscriptions and topics

- `emqx_subscriptions_count` — subscription count.
- `emqx_subscriptions_max` — peak subscriptions.
- `emqx_subscriptions_shared_count` — shared subscriptions.
- `emqx_subscriptions_shared_max` — peak shared subscriptions.
- `emqx_subscribers_count` — subscriber processes.
- `emqx_topics_count` — distinct topic count.
- `emqx_topics_max` — peak topics.
- `emqx_routes_count` — route table size.
- `emqx_routes_max` — peak route table size.
- `emqx_durable_subscriptions_count` — persistent-session subscriptions.
- `emqx_durable_subscriptions_max` — peak persistent-session subscriptions.

### Retained, delayed, and banned

- `emqx_retained_count` — retained message count.
- `emqx_retained_max` — peak retained count.
- `emqx_delayed_count` — delayed-publish queue depth.
- `emqx_delayed_max` — peak delayed queue depth.
- `emqx_banned_count` — banned client / username / IP entries.

### Messages

- `emqx_messages_received` — application-level messages received from clients.
- `emqx_messages_sent` — application-level messages sent to clients.
- `emqx_messages_publish` — PUBLISH packets dispatched.
- `emqx_messages_delivered` — deliveries to subscribers (one published message can produce multiple deliveries).
- `emqx_messages_acked` — acknowledgements received from subscribers.
- `emqx_messages_forward` — cross-node message forwards.
- `emqx_messages_retained` — retained-message events.
- `emqx_messages_delayed` — delayed-publish enqueues.

### Message drops (the earliest sign of trouble)

- `emqx_messages_dropped` — total dropped messages.
- `emqx_messages_dropped_expired` — dropped because the message-expiry interval was exceeded.
- `emqx_messages_dropped_no_subscribers` — dropped because no subscriber matched.
- `emqx_messages_dropped_quota_exceeded` — dropped because a per-client quota was hit.
- `emqx_messages_dropped_receive_maximum` — dropped because the subscriber's MQTT v5 receive-maximum quota was hit.

### Per-subscriber delivery drops

- `emqx_delivery_dropped` — total deliveries dropped.
- `emqx_delivery_dropped_expired` — expired before delivery.
- `emqx_delivery_dropped_no_local` — MQTT v5 no-local rule.
- `emqx_delivery_dropped_qos` — QoS-not-supported.
- `emqx_delivery_dropped_queue_full` — subscriber mqueue full.
- `emqx_delivery_dropped_too_large` — exceeds subscriber's max packet size.

### Bytes

- `emqx_bytes_received` — total bytes received.
- `emqx_bytes_sent` — total bytes sent.

### Packet-level (for protocol-debug dashboards)

- `emqx_packets_received` / `emqx_packets_sent` — total packets.
- `emqx_packets_connect` — CONNECT packets received.
- `emqx_packets_connack_sent` — CONNACK packets sent.
- `emqx_packets_connack_error` — CONNACK with a non-zero reason code (most per-client AUTHN failures show here).
- `emqx_packets_disconnect_received` / `emqx_packets_disconnect_sent` — DISCONNECT in / out.
- `emqx_packets_publish_received` / `emqx_packets_publish_sent` — PUBLISH in / out.
- `emqx_packets_publish_error` — PUBLISH that could not be accepted.
- `emqx_packets_publish_auth_error` — PUBLISH denied by authorization.
- `emqx_packets_puback_*`, `emqx_packets_pubrec_*`, `emqx_packets_pubrel_*`, `emqx_packets_pubcomp_*` — QoS 1 / 2 acknowledgement counters.
- `emqx_packets_subscribe_received` / `emqx_packets_suback_sent` / `emqx_packets_subscribe_error` / `emqx_packets_subscribe_auth_error` — SUBSCRIBE accept / fail / AUTHZ-denied.
- `emqx_packets_unsubscribe_received` / `emqx_packets_unsuback_sent` / `emqx_packets_unsubscribe_error`.
- `emqx_packets_pingreq_received` / `emqx_packets_pingresp_sent` — keepalive activity.

### Client lifecycle (hook trigger counters)

- `emqx_client_connect` — CONNECT received.
- `emqx_client_connack` — CONNACK sent.
- `emqx_client_connected` — `client.connected` hook fired.
- `emqx_client_disconnected` — `client.disconnected` hook fired.
- `emqx_client_disconnected_reason` — disconnect counts labeled by reason.
- `emqx_client_subscribe` / `emqx_client_unsubscribe` — subscribe hook fires.

### Session lifecycle

- `emqx_session_created` — sessions created.
- `emqx_session_resumed` — persistent sessions resumed.
- `emqx_session_takenover` — sessions taken over by a new client.
- `emqx_session_discarded` — sessions discarded (clean start over an existing session).
- `emqx_session_terminated` — sessions terminated.

## Authentication and Authorization

Useful when an HTTP, LDAP, or database backend is in the auth path and you need to see whether the broker or the backend is the slow or failing party.

### Connect-time authentication outcomes

- `emqx_authentication_success` — successful authentication (excluding anonymous).
- `emqx_authentication_success_anonymous` — anonymous pass.
- `emqx_authentication_failure` — authentication failures.

### Authorization decisions

- `emqx_authorization_allow` — decisions = allow.
- `emqx_authorization_deny` — decisions = deny.
- `emqx_authorization_nomatch` — no matching rule (falls back to the `no_match` configuration).
- `emqx_authorization_matched_allow` — matched-allow rule fired.
- `emqx_authorization_matched_deny` — matched-deny rule fired.
- `emqx_authorization_cache_hit` — cache hits.
- `emqx_authorization_cache_miss` — cache misses.
- `emqx_authorization_superuser` — superuser-bypass path.

### Authentication chain status

- `emqx_authn_total` — configured authentication providers.
- `emqx_authn_enable` — enabled flag per provider (0 / 1).
- `emqx_authn_status` — resource state per provider.
- `emqx_authn_users_count` — user record count per provider (for password, mnesia, or DB-backed providers).

### Authentication per-provider runtime counters

- `emqx_authn_success` — successful matches per provider.
- `emqx_authn_failed` — failures per provider.
- `emqx_authn_nomatch` — ignored (chain continues to the next provider).
- `emqx_authn_latency` — backend latency per provider.

### Authorization source status

- `emqx_authz_total` — configured authorization sources.
- `emqx_authz_enable` — enabled flag per source (0 / 1).
- `emqx_authz_status` — resource state per source.
- `emqx_authz_rules_count` — rule record count per source (file, mnesia, or DB-backed).

### Authorization per-source runtime counters

- `emqx_authz_allow` — decisions = allow per source.
- `emqx_authz_deny` — decisions = deny per source.
- `emqx_authz_nomatch` — ignored per source (chain continues).
- `emqx_authz_latency` — backend latency per source.

### Built-in DB sizes

- `emqx_authn_builtin_record_count` — user count in the built-in authentication database.
- `emqx_authz_builtin_record_count` — rule count in the built-in authorization database.

## Data Integration

The pipeline view: traffic enters the rule engine, fans out to connectors and actions, and lands at external systems. Each layer exposes its own counters; reading them in order shows where flow is being lost.

### Inventory

- `emqx_rules_count` — configured rules.
- `emqx_actions_count` — configured actions.
- `emqx_connectors_count` — configured connectors.
- `emqx_schema_registrys_count` — schema-registry entries.

### Per-resource status

- `emqx_rule_enable` — rule enable flag (0 / 1).
- `emqx_action_enable` — action enable flag (0 / 1).
- `emqx_action_status` — action resource state.
- `emqx_connector_enable` — connector enable flag (0 / 1).
- `emqx_connector_status` — connector resource state.

### Rule engine — per-rule counters

- `emqx_rule_matched` — messages that matched the rule's WHERE clause.
- `emqx_rule_passed` — messages that passed the rule.
- `emqx_rule_failed` — rule processing failed.
- `emqx_rule_failed_exception` — Erlang exception during the rule.
- `emqx_rule_failed_no_result` — SQL produced no result.

### Rule engine — action sub-counters

- `emqx_rule_actions_total` — action invocations from rules.
- `emqx_rule_actions_success` — action returned success.
- `emqx_rule_actions_failed` — action failed.
- `emqx_rule_actions_failed_unknown` — failure with unknown reason.
- `emqx_rule_actions_failed_out_of_service` — downstream resource unhealthy.
- `emqx_rule_actions_discarded` — action discarded (for example, rate limited).

### Action throughput

- `emqx_action_matched` — messages routed to the action.
- `emqx_action_received` — received at the action queue.
- `emqx_action_success` — action call succeeded.
- `emqx_action_failed` — action call failed.
- `emqx_action_late_reply` — response arrived after timeout.
- `emqx_action_retried` — retry attempts.
- `emqx_action_retried_success` — succeeded after retry.
- `emqx_action_retried_failed` — failed after all retries.

### Action queue and inflight

- `emqx_action_inflight` — in-flight requests.
- `emqx_action_queuing` — queued (pending dispatch) length.

### Action drops

A non-zero rate on any of these series is a strong signal that a downstream system is unhealthy or that the action's configuration is wrong.

- `emqx_action_dropped` — total dropped at the action layer.
- `emqx_action_dropped_queue_full` — queue cap hit.
- `emqx_action_dropped_resource_stopped` — target resource stopped.
- `emqx_action_dropped_resource_not_found` — target resource missing.
- `emqx_action_dropped_expired` — message expired before dispatch.
- `emqx_action_dropped_other` — other reasons.

## Minimal "Broker-is-Sick" Panel

If only a handful of series can fit on a single page, these are the load-bearing ones. Most production issues move at least one of them within seconds of the event:

- `rate(emqx_messages_dropped[1m])` — non-zero means the broker is refusing or losing work.
- `rate(emqx_action_dropped[1m])` — the integration layer is losing work.
- `emqx_cluster_nodes_stopped` — greater than zero means a member was lost.
- `rate(emqx_overload_protection_new_conn[1m])` — the broker is actively rejecting new connections.
- `rate(emqx_authentication_failure[1m])` — an authentication failure spike usually indicates a backend issue or an attack.
- `emqx_vm_run_queue` — sustained above zero means CPU saturation.
- `emqx_vm_process_messages_in_queues` — large values indicate process-mailbox backlog.
- `emqx_mria_lag` — values above a few seconds mean replication is falling behind.
- `emqx_license_expiry_at - time()` (Enterprise) — countdown to license expiration.
