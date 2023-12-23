# Releases

## e5.4.0

### Enhancements

- [#11884](https://github.com/emqx/emqx/pull/11884) Modified the Prometheus API and configuration to implement the following improvements:

  - Restructured configuration sections to group-related settings, improving readability and maintainability.
  - Introduced `enable_basic_auth` configuration for basic authentication on the scrape API endpoint, enhancing security.
  - Maintained backward compatibility while refactoring code, avoiding breaking changes.

- [#11896](https://github.com/emqx/emqx/pull/11896) Introduced an enhancement for configuring sensitive authentication fields in bridges, such as passwords, tokens, and secret keys. This improvement allows the use of secrets stored as files in the file system. These secrets can be securely referenced in configuration files using the special `file://` prefix, enhancing the security of sensitive data handling in bridge configurations.

- [#11921](https://github.com/emqx/emqx/pull/11921) Introduced Open Telemetry Logs Handler that allows to format log events in alignment with the Open Telemetry log data model. This handler facilitates the exportation of formatted log events to a configured Open Telemetry collector or back-end, thereby enhancing log management and integration capabilities.

- [#11935](https://github.com/emqx/emqx/pull/11935) Switched to the new `v2` routing store schema by default. The new schema improves both subscription and routing performance, especially in scenarios with concurrent subscriptions to topic filters sharing common wildcard prefixes. However, it does come with a minor increase in memory usage. This schema also eliminates the need for a separate index, thus inconsistencies in the routing state rarely encountered in previous versions should no longer be possible.

  If a cluster is rolling upgraded from an older version, the cluster will continue to use `v1` store until a full cluster (non-rolling) restart happens.

  Users can still opt for the previous schema by configuring the `broker.routing.storage_schema` option to `v1`. However, this also requires a complete, non-rolling restart of the cluster to take effect.

- [#11984](https://github.com/emqx/emqx/pull/11984) Implemented Open Telemetry distributed tracing feature.

- [#12017](https://github.com/emqx/emqx/pull/12017) Implemented a dedicated REST API for the import and export of configuration and user data. 

- [#12040](https://github.com/emqx/emqx/pull/12040) Upgraded QUIC protocol stack.

- [#12201](https://github.com/emqx/emqx/pull/11994) Added support for hot updates to TCP/SSL/WS/WSS MQTT listener configurations. This feature allows you to modify most configuration parameters without restarting the listener and disconnecting the clients. However, there are some limitations:

  - For TCP/SSL listeners, changes to the following parameters will still require a listener restart and client reconnection:

    - `bind`
    - `tcp_options.backlog`

  - For WS/WSS (WebSocket) listeners, modifying transport-related parameters (listed below) will result in the listening socket being reopened, but established connections will remain uninterrupted.
    - `bind`
    - `tcp_options.*`
    
    - `ssl_options.*`

- [#11608](https://github.com/emqx/emqx/pull/11608) Integrated LDAP bind operation as a new authenticator, providing a more flexible and secure method for user authentication.

- [#11766](https://github.com/emqx/emqx/pull/11766) Implemented a preliminary Role-Based Access Control for the REST API. In this version, there are three predefined roles:

  - Administrator: This role can access all resources.

  - Viewer: This role can only view resources and data, corresponding to all GET requests in the REST API.

  - Publisher: Specifically tailored for MQTT message publishing, this role is confined to accessing endpoints related to message publication.

- [#11773](https://github.com/emqx/emqx/pull/11773) Implemented Dashboard support for audit log management. Users can utilize this page to view all change operations performed on EMQX devices and data, such as kicking out devices, creating/deleting rules, etc.

- [#11778](https://github.com/emqx/emqx/pull/11778) Integrated Microsoft Entra Identity (formerly known as Azure Active Directory) support into the SAML single sign-on (SSO) process.


- [#11811](https://github.com/emqx/emqx/pull/11811) Improved the format for the REST API key bootstrap file to support initializing key with a role.

  The new form is:`api_key:api_secret:role`.

  `role` is optional and its default value is `administrator`.

- [#11852](https://github.com/emqx/emqx/pull/11852) Introduced a new GB/T 32960 gateway, enabling vehicles to connect with EMQX via the GBT32960 vehicular networking protocol. 

- [#11883](https://github.com/emqx/emqx/pull/11883) Introduced a new JT/T808 gateway, enabling vehicles to connect with EMQX via the JT/T 808 vehicular networking protocol. 

- [#11885](https://github.com/emqx/emqx/pull/11885) Introduced a new OCPP gateway for Electric vehicle (EV) charging stations to access EMQX through the OCPP (Open Charge Point Protocol). 

- [#11971](https://github.com/emqx/emqx/pull/11971) Made `/api/v5/load_rebalance/availability_check` public, meaning it no longer requires authentication. This change simplifies the setup of load balancers.

  It improved the gracefulness of the rebalance/evacuation process during the wait health check phase. The connections to nodes marked for eviction are now not prohibited during this phase.
  During this phase it is unknown whether these nodes are all marked unhealthy by the load balancer, so prohibiting connections to them may cause multiple unsuccessful reconnection attempts.

- [#12013](https://github.com/emqx/emqx/pull/12013) The data bridging design has been adjusted to split it into connectors and actions (Sinks). Connectors are used to manage the integration of data with external systems and can be reused across multiple actions, while actions are used to configure how data is processed. This design provides greater flexibility and scalability, resulting in clearer data integration configuration and management. 

  The adjusted data bridges includes PostgreSQL, Timescale, and Matrix, which have now been split into connectors and actions APIs, but they remain backward compatible with the old data bridge API.

- [#12016](https://github.com/emqx/emqx/pull/12016) Enhanced license key management.

  EMQX can now load the license key from a specified file. This is enabled by setting the `license.key` configuration to a file path, which should be prefixed with `"file://"`.
  Also added the ability to revert to the default trial license by setting `license.key = default`. This option simplifies the process of returning to the trial license if needed.

- [#12129](https://github.com/emqx/emqx/pull/12129) Renewed the default license, replacing the old license issued in January 2023.  At the same time, the license capacity has been adjusted from 100 concurrent connections to 25 concurrent connections.

### Bug Fixes

- [#10976](https://github.com/emqx/emqx/pull/10976) Fixed topic-filter overlapping handling in shared subscription.
  In the previous implementation, the storage method for subscription options did not provide adequate support for shared subscriptions. This resulted in message routing failures and leakage of routing tables between nodes during the "subscribe-unsubscribe" process with specific order and topics.

- [#12048](https://github.com/emqx/emqx/pull/12048) Fixed COAP gateway bug that caused it to ignore subscription options.

- [#12078](https://github.com/emqx/emqx/pull/12078) Upgraded grpc-erl to 0.6.12. This update addresses a potential deadlock issue where the grpc client started dependent apps lazily.

- [#12081](https://github.com/emqx/emqx/pull/12081) Updated `gen_rpc` library to version 3.3.1. The new version includes several performance improvements:
  
  - Avoiding allocating extra memory for the packets before they are sent to the wire in some cases.
  
  - Bypassing network for the local calls.

  - Avoid senstive data leaking in debug logs [#12202](https://github.com/emqx/emqx/pull/12202)
- [#12111](https://github.com/emqx/emqx/pull/12111) Fixed an issue when API tokens were sometimes unavailable immediately after login due to race condition.

- [#12121](https://github.com/emqx/emqx/pull/12121) Fixed an issue where nodes in the cluster would occasionally return a stale view when updating configurations on different nodes concurrently.

- [#12158](https://github.com/emqx/emqx/pull/12158) Fixed an issue when the rule engine cannot connect to Redis hosted by Upstash.

  Before the fix, after establishing a TCP connection with the Redis service, the Redis driver of EMQX used [Inline Commands](https://redis.io/docs/reference/protocol-spec/#inline-commands) to send AUTH and SELECT commands. However, the `upstash` Redis service does not support Inline Commands, which causes the rule engine to fail to connect to the `upstash` Redis service.
  After the fix, the Redis driver of EMQX uses RESP (REdis Serialization Protocol) to send AUTH and SELECT commands.

- [#12176](https://github.com/emqx/emqx/pull/12176) Always acknowledge `DISCONNECT` packet to MQTT-SN client regardless of whether the connection has been successfully established before.

- [#12180](https://github.com/emqx/emqx/pull/12180) Fix an issue where DTLS enabled MQTT-SN gateways could not be started, caused by incompatibility of default listener configuration with the DTLS implementation.
- [#12219](https://github.com/emqx/emqx/pull/12219) Fix file transfer S3 config secret deobfuscation issue while performing config updates from dashboard.
