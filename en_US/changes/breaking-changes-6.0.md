# Incompatible Changes between EMQX 5.x and EMQX 6.0

## Deprecated Packages

- [#15939](https://github.com/emqx/emqx/pull/15939) Stop releasing packages for systems which have already reached end-of-life:
  - Enterprise Linux (CentOS) 7
  - Ubuntu 20.04
  - macOS 13 (Ventura)

## Durable Sessions

If durable sessions feature was not previously enabled, the following information can be ignored.

6.0 release changes the internal representation of the durable sessions and messages.
If the cluster was previously running on version 5.x with the feature enabled,
it must be recreated from the clean state.

- [#15496](https://github.com/emqx/emqx/pull/15496) State of the durable sessions has been moved from Mnesia to a new database based on EMQX durable storage.
  As a consequence, state of the durable sessions created prior to 6.0.0 release will be lost during the move.

  This solves a problem with session state corruption that could occur due to insufficient transaction isolation of Mnesia (as reported in [#14039](https://github.com/emqx/emqx/issues/14039)).
  This change also improves general performance of durable sessions thanks to sharding and more efficient data representation.


## Will message behavior

Authorization checks that decide whether the durable session is eligible to publish the will message now run at the moment of client disconnection.
Previously they ran after expiration of `Will-Delay-Interval`.

## Configuration Changes

- `durable_sessions.heartbeat_interval` parameter has been renamed to `durable_sessions.checkpoint_interval`.

- `durable_sessions.idle_poll_interval` and `durable_sessions.renew_streams_interval` parameters have been removed, as sessions have become fully event-based.

- `durable_sessions.session_gc_interval` and `durable_sessions.session_gc_batch_size` parameters have been removed as obsolete.

- `durable_storage.messages.n_sites` parameter has been renamed to `durable_storage.n_sites`.
  This parameter has become common for all durable storages.

- Added configuration for new durable storages: `durable_storage.sessions` and `durable_storage.timers`.
- [#15613](https://github.com/emqx/emqx/pull/15613) Stopped releasing packages for Debian 10.

- [#15635](https://github.com/emqx/emqx/pull/15635) The `parameters.strategy` field in the RocketMQ Action no longer accepts key templates (which implicitly selected the `key_dispatch` strategy).
  Instead, users must explicitly set `parameters.strategy = key_dispatch` and provide the key template in `parameters.key`.

- [#15734](https://github.com/emqx/emqx/pull/15734) Improved reliability and throughput of durable sessions.

## Rate Limit

- [#15743](https://github.com/emqx/emqx/pull/15743) Listener connection rate limits (`max_conn_rate` and `max_conn_burst`) are now enforced per listener rather than per acceptor, restoring the pre-5.9.0 behavior. As a result, configurations from versions 5.9.0, 5.9.1 and 5.10.0 are incompatible: specified rates must be scaled up by the number of acceptors configured for respective listeners.
