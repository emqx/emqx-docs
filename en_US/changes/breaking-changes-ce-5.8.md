# Incompatible Changes in EMQX 5.8

## v5.8.0

- [#13080](https://github.com/emqx/emqx/pull/13080) Change `mqtt.retry_interval` config default to `infinity`.

  Previously, the default value for `retry_interval` was 30 seconds.

  The new default has been changed to 'infinity'. With this update, EMQX will not automatically retry message deliveries by default.

  Aligning with MQTT specification standards, in-session message delivery retries are not typically compliant.
  We recognize that some users depend on this feature, so the option to configure retries remains available for backward compatibility.

- [#13190](https://github.com/emqx/emqx/pull/13190) Stop releasing on CentOS 7 and Ubuntu 18.

- [#13248](https://github.com/emqx/emqx/pull/13248) `builtin` durable storage backend has been replaced with the following two backends:

  - `builtin_local`: A durable storage backend that doesn't support replication.
  It can't be used in a multi-node cluster.
  This backend is available in both open source and enterprise editions.
  - `builtin_raft`: A durable storage backend that uses Raft algorithm for replication.
  This backend is available only in the enterprise edition.

  The following Prometheus metrics have been renamed:

  - `emqx_ds_egress_batches` -> `emqx_ds_buffer_batches`
  - `emqx_ds_egress_batches_retry` -> `emqx_ds_buffer_batches_retry`
  - `emqx_ds_egress_batches_failed` -> `emqx_ds_buffer_batches_failed`
  - `emqx_ds_egress_messages` -> `emqx_ds_buffer_messages`
  - `emqx_ds_egress_bytes` -> `emqx_ds_buffer_bytes`
  - `emqx_ds_egress_flush_time` -> `emqx_ds_buffer_flush_time`

- [#13526](https://github.com/emqx/emqx/pull/13526) - Core-replicant feature has been removed from the Open-Source Edition.
  Starting from release 5.8, all nodes running Open-Source Edition will assume Core role.
  This change doesn't affect Enterprise Edition users.

  - Obsolete and unused `cluster.core_nodes` configuration parameter has been removed.
