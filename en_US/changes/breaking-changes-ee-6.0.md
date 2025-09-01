# Incompatible Changes in EMQX 6.0

## e6.0.0

- [#15753](https://github.com/emqx/emqx/pull/15753) Listener connection rate limits (`max_conn_rate` and `max_conn_burst`) are now enforced per listener rather than per acceptor, restoring the pre-5.9.0 behavior. As a result, configurations from versions 5.9.0 and 5.9.1 are incompatible: specified rates must be scaled up by the number of acceptors configured for respective listeners.

- [#15752](https://github.com/emqx/emqx/pull/15752) Listener connection rate limits (`max_conn_rate` and `max_conn_burst`) are now enforced per listener rather than per acceptor, restoring the pre-5.9.0 behavior. As a result, configurations from versions 5.9.0, 5.9.1 and 5.10.0 are incompatible: specified rates must be scaled up by the number of acceptors configured for respective listeners.

- [#15743](https://github.com/emqx/emqx/pull/15743) Listener connection rate limits (`max_conn_rate` and `max_conn_burst`) are now enforced per listener rather than per acceptor, restoring the pre-5.9.0 behavior. As a result, configurations from versions 5.9.0, 5.9.1 and 5.10.0 are incompatible: specified rates must be scaled up by the number of acceptors configured for respective listeners.

- [#15496](https://github.com/emqx/emqx/pull/15496) State of the durable sessions has been moved from Mnesia to a new database based on EMQX durable storage.
  As a consequence, state of the durable sessions created prior to 6.0.0 release will be lost during the move.

  This solves a problem with session state corruption that could occur due to insufficient transaction isolation of Mnesia (as reported in [#14039](https://github.com/emqx/emqx/issues/14039)).
  This change also improves general performance of durable sessions thanks to sharding and more efficient data representation.

  ### Will message behavior

  Authorization checks that decide whether the durable session is eligible to publish the will message now run at the moment of client disconnection.
  Previously they ran after expiration of `Will-Delay-Interval`.

  ### Configuration changes

  - `durable_sessions.heartbeat_interval` parameter has been renamed to `durable_sessions.checkpoint_interval`.

  - `durable_sessions.idle_poll_interval` and `durable_sessions.renew_streams_interval` parameters have been removed, as sessions have become fully event-based.

  - `durable_sessions.session_gc_interval` and `durable_sessions.session_gc_batch_size` parameters have been removed as obsolete.

  - `durable_storage.messages.n_sites` parameter has been renamed to `durable_storage.n_sites`.
    This parameter has become common for all durable storages.

  - Added configuration for new durable storages: `durable_storage.sessions` and `durable_storage.timers`.


