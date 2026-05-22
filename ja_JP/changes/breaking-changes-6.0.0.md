<<<<<<< HEAD
# Incompatible Changes from EMQX 5.x to EMQX 6.0
=======
# Incompatible Changes between EMQX 5.x and EMQX 6.0
>>>>>>> origin/release-6.1

## Deprecated Packages

- [#15939](https://github.com/emqx/emqx/pull/15939) Stopped releasing packages for systems that have already reached end-of-life:
  - Debian 10 (Buster)
  - Enterprise Linux (CentOS) 7
  - Ubuntu 18.04
  - Ubuntu 20.04
  - macOS 13 (Ventura)

- [#16050](https://github.com/emqx/emqx/pull/16050) Stopped releasing packages for Amazon Linux 2. It will reach end-of-life on June 30, 2026.

## Durable Sessions

If the durable sessions feature was not enabled before, you can ignore this section.

In EMQX 6.0, the internal representation of durable sessions and their messages has changed. 
Clusters previously running on version 5.x with durable sessions enabled must be recreated from a clean state when upgrading to 6.0.

For detailed upgrade instructions, see the [rolling upgrade documentation](../deploy/rolling-upgrades.md#emqx-enterprise-rolling-upgrade).

- [#15496](https://github.com/emqx/emqx/pull/15496) The state of durable sessions has been migrated from Mnesia to a new database built on EMQX durable storage.
  - As a result, all durable session states created before 6.0.0 will be lost during the migration.
  - This change resolves potential session state corruption caused by Mnesia’s limited transaction isolation (see [#14039](https://github.com/emqx/emqx/issues/14039)).
  - It also improves the performance and scalability of durable sessions through sharding and a more efficient data representation.

## Will Message Behavior

Authorization checks for durable sessions are now performed at the moment of client disconnection to determine whether the will message may be published.

Previously, these checks were deferred until after the configured `Will-Delay-Interval` had expired.

## Configuration Changes

**Durable Sessions**

- `durable_storage.messages.n_sites` parameter has been renamed to `durable_storage.n_sites`. This parameter has become common for all durable storage.
- `durable_storage.sessions` and `durable_storage.timers` have been added.
- [#15734](https://github.com/emqx/emqx/pull/15734) Improved the reliability and throughput of durable sessions.

**Durable Storage**

- `durable_storage.messages.n_sites` has been renamed to `durable_storage.n_sites`, which now applies to all durable storage types.
- Added new configuration entries for `durable_storage.sessions` and `durable_storage.timers`.

**RocketMQ**

- [#15635](https://github.com/emqx/emqx/pull/15635) The `parameters.strategy` field no longer accepts key templates (which previously implied the `key_dispatch` strategy).
  Instead, set `parameters.strategy = key_dispatch` explicitly and specify the key template in `parameters.key`.

**Platform Support**

- [#15613](https://github.com/emqx/emqx/pull/15613) Discontinued package builds for Debian 10.

## Rate Limit

- [#15743](https://github.com/emqx/emqx/pull/15743) Listener connection rate limits (`max_conn_rate` and `max_conn_burst`) are now enforced per listener rather than per acceptor, restoring the behavior before 5.9.0. As a result, configurations from versions 5.9.0, 5.9.1, and 5.10.0 are incompatible: the specified rate values must be scaled up by the number of acceptors configured for each listener to preserve the same effective limits.
