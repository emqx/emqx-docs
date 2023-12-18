# Releases

## e5.4.0

### Enhancements

- [#11884](https://github.com/emqx/emqx/pull/11884) Modified the Prometheus API and configuration to:
  - Restructure configuration sections to group related settings, improving readability and maintainability
  - Introduced `enable_basic_auth` configuration for basic authentication on the scrape API endpoint, enhancing security
  - Maintained backwards compatibility while refactoring code, avoiding breaking changes

- [#11896](https://github.com/emqx/emqx/pull/11896) Support configuring authentication-related sensitive fields in bridges (i.e. passwords, tokens, secret keys) via secrets stored as files in the file system, through special `file://` prefix.

- [#11921](https://github.com/emqx/emqx/pull/11921) Introduced Open Telemetry Logs Handler that allows to format log events according to Open Telemetry log data model and
  export them to the configured Open Telemetry collector or back-end.

- [#11935](https://github.com/emqx/emqx/pull/11935) Switch to the new `v2` routing store schema by default. New schema improves both subscription and routing performance, especially so for scenarios with concurrent subscriptions to topic filters sharing common wildcard prefixes, at the cost of slightly increased memory usage. This schema also eliminates the need for a separate index, thus inconsistencies in the routing state rarely encountered in previous versions should no longer be possible.

  If a cluster is rolling upgraded from older version, the cluster will continue to use `v1` store until a full cluster (non-rolling) restart happens.

  The former schema can still be forced by setting `broker.routing.storage_schema` configuration option to `v1` and conducting full non-rolling cluster restart as well.

- [#11984](https://github.com/emqx/emqx/pull/11984) Implemented Open Telemetry distributed tracing feature.

- [#12017](https://github.com/emqx/emqx/pull/12017) Implemented HTTP API for configuration and user data import/export.

- [#12040](https://github.com/emqx/emqx/pull/12040) Upgrade QUIC stack, more features on the way!


- [#12089](https://github.com/emqx/emqx/pull/12089) Added a technical preview of the new persistent session implementation based on RocksDB.
  Please note that this feature is in alpha stage and must not be enabled in the production systems.

  Features missing in the early preview version of the new persistent session implementation:

  - Shard failover
  - Retained messages
  - Will message handling
  - Shared subscriptions
  - Subscription IDs

- [#11766](https://github.com/emqx/emqx/pull/11766) Implemented a preliminary Role-Based Access Control for the REST API.

  In this version, there are three predefined roles:
  - Administrator: This role could access all resources.

  - Viewer: This role can only view resources and data, corresponding to all GET requests in the REST API.

  - Publisher: This role is special for MQTT messages publish, it can only access publish-related endpoints.

- [#11773](https://github.com/emqx/emqx/pull/11773) Support audit log filter via dashboard (http api).

- [#11778](https://github.com/emqx/emqx/pull/11778) Support Azure Entra Id for saml single sign on.

- [#11795](https://github.com/emqx/emqx/pull/11795) Integrated Nari Syskeeper 2000 as a new bridge backend.

- [#11811](https://github.com/emqx/emqx/pull/11811) Improve the format for the REST API key bootstrap file to support initialize key with a role.

  The new form is:`api_key:api_secret:role`.

  `role` is optional and its default value is `administrator`.

- [#11852](https://github.com/emqx/emqx/pull/11852) Introduced a new gateway for vehicles to access EMQX through the GBT32960 protocol.

- [#11883](https://github.com/emqx/emqx/pull/11883) Introduced a new gateway for vehicles to access EMQX through the JT/T 808 protocol.

- [#11885](https://github.com/emqx/emqx/pull/11885) Introduced a new gateway for Electric vehicle (EV) charging stations to access EMQX through the OCPP (Open Charge Point Protocol).

- [#11971](https://github.com/emqx/emqx/pull/11971) Made `/api/v5/load_rebalance/availability_check` public, i.e. not requiring authentication. This simplifies load balancer setup.

  Made rebalance/evacuation more graceful during the wait health check phase. The connections to nodes marked for eviction are now not prohibited during this phase.
  During this phase it is unknown whether these nodes are all marked unhealthy by the load balancer, so prohibiting connections to them may cause multiple unssuccessful attempts to reconnect.

- [#12013](https://github.com/emqx/emqx/pull/12013) The bridges for PostgreSQL, Timescale and Matrix have been split so they are available via the connectors and actions APIs. They are still backwards compatible with the old bridge API.

- [#12016](https://github.com/emqx/emqx/pull/12016) Enhanced license key management.

  EMQX can now load the license key from a specified file. This is enabled by setting the `license.key` configuration to a file path, which should be prefixed with `"file://"`.
  Also added the ability to revert to the default trial license by setting `license.key = default`. This option simplifies the process of returning to the trial license if needed.

- [#12129](https://github.com/emqx/emqx/pull/12129) Default license renewal.

  Replaced old license issued in Jan 2023.
  New license supports up to 25 concurrent connections.



### Bug Fixes

- [#10976](https://github.com/emqx/emqx/pull/10976) Fix topic-filter overlapping handling in shared subscription.
  In the previous implementation, the storage method for subscription options did not provide adequate support for shared subscriptions. This resulted in message routing failures and leakage of routing tables between nodes during the "subscribe-unsubscribe" process with specific order and topics.

- [#12048](https://github.com/emqx/emqx/pull/12048) Fix COAP gateway bug that caused it to ignore subscription options.

- [#12078](https://github.com/emqx/emqx/pull/12078) Upgrade grpc-erl to 0.6.12

  grpc-erl 0.6.12 fixes a potential deadlock that was possible because grpc client started dependent apps lazily.

- [#12081](https://github.com/emqx/emqx/pull/12081) Updated `gen_rpc` library to version 3.3.0. The new version includes
  several performance improvements:

  - Avoid allocating extra memory for the packets before they are sent
  to the wire in some cases

  - Bypass network for the local calls

- [#12111](https://github.com/emqx/emqx/pull/12111) Fix an issue where API tokens were sometimes unavailable by using sync_transaction function to ensure all updates are consistently synchronized to the replica node.

- [#12121](https://github.com/emqx/emqx/pull/12121) Fixed occasionally return stale view when updating configurations on different nodes concurrently

- [#12158](https://github.com/emqx/emqx/pull/12158) Fix the issue that the rule engine cannot connect to `upstash` Redis.

  Before the fix, after establishing a TCP connection with the Redis service, the Redis driver of EMQX used [Inline Commands](https://redis.io/docs/reference/protocol-spec/#inline-commands) to send AUTH and SELECT commands. However, the `upstash` Redis service does not support Inline Commands, which causes the rule engine to fail to connect to the `upstash` Redis service.
  After the fix, the Redis driver of EMQX uses RESP (REdis Serialization Protocol) to send AUTH and SELECT commands.

- [#12176](https://github.com/emqx/emqx/pull/12176) Ack the DISCONNECT packet to MQTT-SN client regardless of whether the connection has been successfully established.
