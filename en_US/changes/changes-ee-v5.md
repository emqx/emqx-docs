# Version 5

## 5.4.1

*Release Date: 2024-01-09*

### Bug Fixes

- [#12234](https://github.com/emqx/emqx/pull/12234) Resolved compatibility issues with Open Telemetry configurations defined in `emqx.conf` from versions before EMQX 5.4.0, ensuring smooth integration of legacy configurations with the latest EMQX release.

- [#12236](https://github.com/emqx/emqx/pull/12236) Fixed client ID generation in MQTT broker data integration to comply with MQTT 3.1 specification of 23-byte limit. Client ID is now prefixed with user-assigned Connector name, followed by the first 8 bytes of node name's SHA hash and pool member ID. If the resulting ID exceeds 23 bytes, additional SHA hash and truncation for the first 23 characters are applied to ensure compliance.
- [#12238](https://github.com/emqx/emqx/pull/12238) Resolved compatibility issue with the error format configurations introduced in the HTTP Action feature of EMQX version 5.3.2.

- [#12240](https://github.com/emqx/emqx/pull/12240) Modified the `/file_transfer` API to return file transfer configurations in their original raw format. This change prevents the conversion of time units, such as "1h", to seconds, ensuring that callers receive the initially configured values. This modification aligns with other getter APIs, maintaining consistency in data representation.

- [#12241](https://github.com/emqx/emqx/pull/12241) Fixed a bug where configuring additional HTTP headers for S3 API interactions disrupted file transfers using the S3 storage backend, ensuring stable and uninterrupted file transfer operations.

- [#12246](https://github.com/emqx/emqx/pull/12246) Stopped exposing port 11883 by default in Docker and removed it from Helm charts, as this port is no longer in use. 

- [#12249](https://github.com/emqx/emqx/pull/12249) Fixed an issue in the `/configs` API where attempting to modify a read-only configuration value resulted in a garbled response message.

- [#12250](https://github.com/emqx/emqx/pull/12250) Resolved an issue where the `file_transfer` configuration's `secret_access_key` value was erroneously being updated to masked stars (`*****`), ensuring that the original key value remains unaltered and secure.
- [#12256](https://github.com/emqx/emqx/pull/12256) Fixed an issue that prevented establishing connections to MySQL resources without a password.

- [#12264](https://github.com/emqx/emqx/pull/12264) Fix rolling upgrade when replica nodes join cluster with core nodes prior to version 5.4.

## 5.4.0

*Release Date: 2023-12-23*

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

## 5.3.2

*Release Date: 2023-12-01*

### Enhancements

- [#11752](https://github.com/emqx/emqx/pull/11752) Changed default RPC driver from `gen_rpc` to `rpc` for core-replica database synchronization.

  This improves core-replica data replication latency.

- [#11785](https://github.com/emqx/emqx/pull/11785) Allowed users with the "Viewer" role to change their own passwords. However, those with the "Viewer" role do not have permission to change the passwords of other users.
- [#11787](https://github.com/emqx/emqx/pull/11787) Improved the performance of the `emqx` command.

- [#11790](https://github.com/emqx/emqx/pull/11790) Added validation to Redis commands in Redis authorization source.
  Additionally, this improvement refines the parsing of Redis commands during authentication and authorization processes.  The parsing now aligns with `redis-cli` compatibility standards and supports quoted arguments.
  
- [#11541](https://github.com/emqx/emqx/pull/11541) Enhanced file transfer capabilities. Now, clients can use an asynchronous method for file transfer by sending commands to the `$file-async/...` topic and subscribing to command execution results on the `$file-response/{clientId}` topic. This improvement simplifies the use of the file transfer feature, particularly suitable for clients using MQTT v3.1/v3.1.1 or those employing MQTT bridging. For more details, please refer to [EIP-0021](https://github.com/emqx/eip).

### Bug Fixes

- [#11757](https://github.com/emqx/emqx/pull/11757) Fixed the error response code when downloading non-existent trace files. Now the response returns `404` instead of `500`.

- [#11762](https://github.com/emqx/emqx/pull/11762) Fixed an issue in EMQX's `built_in_database` authorization source. With this update, all Access Control List (ACL) records are completely removed when an authorization source is deleted. This resolves the issue of residual records remaining in the database when re-creating authorization sources.

- [#11771](https://github.com/emqx/emqx/pull/11771) Fixed validation of Bcrypt salt rounds in authentication management through the API/Dashboard.

- [#11780](https://github.com/emqx/emqx/pull/11780) Fixed validation of the `iterations` field of the `pbkdf2` password hashing algorithm. Now, `iterations` must be strictly positive. Previously, it could be set to 0, which led to a nonfunctional authenticator.

- [#11791](https://github.com/emqx/emqx/pull/11791) Fixed an issue in the EMQX CoAP Gateway where heartbeats were not effectively maintaining the connection's active status. This fix ensures that the heartbeat mechanism properly sustains the liveliness of CoAP Gateway connections.

- [#11797](https://github.com/emqx/emqx/pull/11797) Modified HTTP API behavior for APIs managing the `built_in_database` authorization source. They will now return a `404` status code if `built_in_database` is not set as the authorization source, replacing the former `20X` response.

- [#11965](https://github.com/emqx/emqx/pull/11965) Improved the termination of EMQX services to ensure a graceful stop even in the presence of an unavailable MongoDB resource. 

- [#11975](https://github.com/emqx/emqx/pull/11975) This fix addresses an issue where redundant error logs were generated due to a race condition during simultaneous socket closure by a peer and the server. Previously, concurrent socket close events triggered by the operating system and EMQX resulted in unnecessary error logging. The implemented fix improves event handling to eliminate unnecessary error messages.

- [#11987](https://github.com/emqx/emqx/pull/11987) Fixed a bug where attempting to set the `active_n` option on a TCP/SSL socket could lead to a connection crash. 

  The problem occurred if the socket had already been closed by the time the connection process attempted to apply the `active_n` setting, resulting in a `case_clause` crash.

- [#11731](https://github.com/emqx/emqx/pull/11731) Added hot configuration support for the file transfer feature.

- [#11754](https://github.com/emqx/emqx/pull/11754) Improved the log formatting specifically for the Postgres bridge in EMQX. It addresses issues related to Unicode characters in error messages returned by the driver.


## 5.3.1

*Release Date: 2023-11-14*

### Enhancements

- [#11637](https://github.com/emqx/emqx/pull/11637) Added extra diagnostic checks to help debug issues when mnesia is stuck waiting for tables. Library Updates: `ekka` has been upgraded to version 0.15.15, and `mria` to version 0.6.4.
- [#11581](https://github.com/emqx/emqx/pull/11581) Feature Preview: Planned for EMQX v5.4.0, introducing the concepts of *Connector* and *Action* base on data bridge. The existing data bridge will be gradually migrated to Connector and Action. Connector are designed to manage the integration with external systems, while Actions are solely used to configure the data processing methods. Connector can be reused across multiple Actions, providing greater flexibility and scalability. Currently, the migration has been completed for Kafka producer and Azure Event Hub producer.
- The Dashboard now supports MQTT 5.0 publish attribute settings for the rule engine's message republish action, allowing users more flexibility in publishing messages.

### Bug Fixes

- [#11565](https://github.com/emqx/emqx/pull/11565) Upgraded jq library from v0.3.10 to v0.3.11. In this version, jq_port programs are initiated on-demand and will not appear in users' processes unless the jq function in EMQX is used. Additionally, idle jq_port programs will auto-terminate after a set period. Note: Most EMQX users are running jq in NIF mode and will not be affected by this update.

- [#11676](https://github.com/emqx/emqx/pull/11676) Hid a few pieces of sensitive information from debug-level logs.

- [#11697](https://github.com/emqx/emqx/pull/11697) Disabled outdated TLS versions and cipher suites in the EMQX backplane network (`gen_rpc`). Added support for tlsv1.3 on the backplane and introduced new configuration parameters: `EMQX_RPC__TLS_VERSIONS` and `EMQX_RPC__CIPHERS`.
  
  The corresponding `gen_rpc` PR: https://github.com/emqx/gen_rpc/pull/36
  
- [#11734](https://github.com/emqx/emqx/pull/11734) Fixed clustering in IPv6 network. Added new configurations `rpc.listen_address` and `rpc.ipv6_only` to allow EMQX cluster RPC server and client to use IPv6.

- [#11747](https://github.com/emqx/emqx/pull/11747) Updated QUIC stack to msquic 2.2.3.


- [#11796](https://github.com/emqx/emqx/pull/11796) Fixed rpc schema to ensure that client/server uses same transport driver.


- [#11798](https://github.com/emqx/emqx/pull/11798) Fixed the issue where the node could not start after executing `./bin/emqx data import [FILE]`.

  The connection between `apikey_key` and `apikey_name` is also enhanced for better consistency and unique identification.
  - `apikey_key`: When generating an API key via the dashboard, `apikey_key` will now create a unique value derived from the provided human-readable `apikey_name`. 
  - `apikey_name` Conversely, when using a bootstrap file to generate an API key, `apikey_name` will be generated as a unique value based on the associated `apikey_key`. 

- [#11813](https://github.com/emqx/emqx/pull/11813) Fixed the schema to ensure that RPC client SSL port aligns with the configured server port. This fix also guarantees that the RPC ports are correctly opened in the Helm chart. 

- [#11819](https://github.com/emqx/emqx/pull/11819) Upgraded opentelemetry library to v1.3.1-emqx. This opentelemetry release fixes invalid metrics timestamps in the exported metrics.

- [#11861](https://github.com/emqx/emqx/pull/11861) Fixed excessive warning message printed in remote console shell.

- [#11722](https://github.com/emqx/emqx/pull/11722) Fixed an issue where a Kafka Producer bridge with `sync` query mode would not buffer messages when in the `connecting` state.
- [#11724](https://github.com/emqx/emqx/pull/11724) Fixed a metrics-related issue where messages sent to Kafka would be counted as failed even when they were successfully transmitted afterward due to internal buffering.
- [#11728](https://github.com/emqx/emqx/pull/11728) Enhanced the LDAP filter string parser with the following improvements:
  - Automatic escaping of special characters within filter strings.
  - Fixed a bug that previously prevented the use of `dn` as a filter value.
- [#11733](https://github.com/emqx/emqx/pull/11733) Resolved an incompatibility issue that caused crashes during session takeover or channel eviction when the session was located on a remote node running EMQX v5.2.x or an earlier version.
- [#11750](https://github.com/emqx/emqx/pull/11750) Eliminated logging and tracing of HTTP request bodies in HTTP authentification and HTTP bridges.
- [#11760](https://github.com/emqx/emqx/pull/11760) Simplified the CQL query used for the Cassandra bridge health check, which was previously generating warnings in the Cassandra server logs.

- [#11886](https://github.com/emqx/emqx/pull/11886) Fixed backward plugin compatibility.

  Currently, EMQX validates hook point names, and invalid hook points cannot be used for hook registration. However, some older versions of plugin templates used misspelled hook points, and actual plugins in use may also have this issue. To maintain compatibility with these older plugins, we allow the use of the old hook points for hook registration, but we issue deprecated warnings for them. As before, these hooks will not be called.

- [#11897](https://github.com/emqx/emqx/pull/11897) Fixed the issue of waiting for a loop race condition during node configuration synchronization when cluster nodes are started approximately at the same time.

## 5.3.0

*Release Date: 2023-09-29*

### Enhancements

- [#11597](https://github.com/emqx/emqx/pull/11597) Upgraded ekka to 0.15.13, which incorporates the following changes:
  - Upgraded Mria to 0.6.2.
  - Introduced the ability to configure the bootstrap data sync batch size, as detailed in [Mria PR](https://github.com/emqx/mria/pull/159).
  - Enhanced the reliability of mria_membership processes, as described in [Mria PR](https://github.com/emqx/mria/pull/156).
  - Fix log message formatting error.
  - Added `node.default_bootstrap_batch_size` option to EMQX configuration.
  Increasing the value of this option can greatly reduce a replicant node startup time, especially when the EMQX cluster interconnect network latency is high and the EMQX built-in database holds a large amount of data, e.g. when the number of subscriptions is high.
- [#11620](https://github.com/emqx/emqx/pull/11620) Added a new rule-engine SQL function `bytesize` to get the size of a byte-string. e.g. `SELECT * FROM "t/#" WHERE bytesize(payload) > 10`.
- [#11642](https://github.com/emqx/emqx/pull/11642) Updated to quicer version 0.0.200 in preparation for enabling openssl3 support for QUIC transport.

- [#11610](https://github.com/emqx/emqx/pull/11610) Implemented a preliminary Role-Based Access Control for the Dashboard.

  In this version, there are two predefined roles:
  - Administrator: This role could access all resources.
  
  - Viewer: This role can only view resources and data, corresponding to all GET requests in the REST API.
  
- [#11631](https://github.com/emqx/emqx/pull/11631) Added Single Sign-On (SSO) feature and integrated with LDAP.

- [#11656](https://github.com/emqx/emqx/pull/11656) Integrated the SAML 2.0 Support for SSO.

- [#11599](https://github.com/emqx/emqx/pull/11599) Supported audit logs to record operations from CLI, REST API, and Dashboard in separate log files.

### Bug Fixes

- [#11682](https://github.com/emqx/emqx/pull/11682) Fixed an issue where logging would stop if "Rotation Size" would be set to `infinity` on file log handlers.
- [#11567](https://github.com/emqx/emqx/pull/11567) Improve EMQX graceful shutdown (`emqx stop` command):
  - Increase timeout from 1 to 2 minutes.
  - Printed an error message if EMQX can't stop gracefully within the configured timeout.
  - Print periodic status messages while EMQX is shutting down.
- [#11584](https://github.com/emqx/emqx/pull/11584) Fixed telemetry reporting error on Windows when os_mon module is unavailable.
- [#11605](https://github.com/emqx/emqx/pull/11605) Lowered CMD_overridden log severity from warning to info.
- [#11622](https://github.com/emqx/emqx/pull/11622) Upgraded rpc library gen_rpc from 2.8.1 to 3.1.0.
- [#11623](https://github.com/emqx/emqx/pull/11623) Upgraded library `esockd` from 5.9.6 to 5.9.7. This upgrade included:
  * Enhancements regarding proxy protocol error and timeout. [esockd pr#178](https://github.com/emqx/esockd/pull/178)
  * Lowered `ssl_error` exceptions to info-level logging. [esockd pr#180](https://github.com/emqx/esockd/pull/180)
  * Malformed MQTT packet parsing exception log level is lowered from `error` to `info`.
  * In command `emqx ctl listeners` output, the `shutdown_count` counter is incremented
  when TLS handshake failure (`ssl_error`) or Malformed packet (`frame_error`) happens.
- [#11661](https://github.com/emqx/emqx/pull/11661) Fixed log formatter when log.HANDLER.formatter is set to 'json'. The bug was introduced in v5.0.4 where the log line was no longer a valid JSON, but prefixed with timestamp string and level name.
- [#11627](https://github.com/emqx/emqx/pull/11627) Fixed resources cleanup in HStreamdB bridge. Prior to this fix, HStreamDB bridge might report errors during bridge configuration updates, since hstreamdb client/producer were not stopped properly.

## 5.2.1

*Release Date: 2023-09-20*

### Enhancements

- [#11487](https://github.com/emqx/emqx/pull/11487) The bcrypt work factor is limited to the range 5-10, because higher values consume too much CPU resources.
  Bcrypt library is updated to allow parallel hash evaluation.

- [#11568](https://github.com/emqx/emqx/pull/11568) Added support for defining templates for MQTT 5.0 publish properties and user properties in Republish rule action.

- [#11612](https://github.com/emqx/emqx/pull/11612) During node evacuation, evacuate all disconnected sessions, not only those started with `clean_start` set to `false`.

- [#11532](https://github.com/emqx/emqx/pull/11532) Improved error messaging for better clarity when parsing invalid packets.

### Bug Fixes

- [#11493](https://github.com/emqx/emqx/pull/11493) Fixed response examples for `/api/v5/publish` bad request in RESP API documentation. Previously the documentation example said that the bad request response could return a list in the body which was not actually the case.

- [#11499](https://github.com/emqx/emqx/pull/11499) Upgraded Erlang/OTP to version 25.3.2-2, which now excludes sensitive data from mnesia_hook log messages.

- [#11506](https://github.com/emqx/emqx/pull/11506) Previously, attempting to download a non-existent trace log file would result in downloading an empty file. After implementing this fix, when attempting to download an empty trace log file using the GET request `/api/v5/trace/clientempty/download`, the server will now respond with a 404 status code and the following JSON message: `{"code":"NOT_FOUND","message":"Trace is empty"}`. This response will be triggered if no events matching the trace condition are found in the log file. 

- [#11522](https://github.com/emqx/emqx/pull/11522) Improved rule engine schema registry error message when schema name exceeds the permissible length.

- [#11531](https://github.com/emqx/emqx/pull/11531) Fixed an issue where authorization cache cleaning CLI was not working properly for specific client ID.

- [#11564](https://github.com/emqx/emqx/pull/11564) Fixed cluster partition autoheal functionality. Implemented autohealing for the clusters that split into multiple partitions.
  
- [#11568](https://github.com/emqx/emqx/pull/11568) Fixed an issue where an ill-defined built-in rule action config could be interpreted as a custom user function.

- [#11394](https://github.com/emqx/emqx/pull/11394) Upgraded Kafka producer client `wolff` from 1.7.6 to 1.7.7. This fixed a potential race condition that might cause all Kafka producers to crash if some failed to initialize.
  
- [#11401](https://github.com/emqx/emqx/pull/11401) Fixed the behavior of the rule SQL `mongo_date` function in SQL statement testing in the EMQX Dashboard. The rule SQL `mongo_date` function now returns a string with the format `ISODate(*)`, where * is an ISO date string when running rules in test mode. This format aligns with how MongoDB stores dates.

- [#11547](https://github.com/emqx/emqx/pull/11547) Fixed several emqx_bridge issues:
  - Fixed Cassandra bridge connect error occurring when the bridge is configured without username/password
  (Cassandra doesn't require user credentials when it is configured with `authenticator: AllowAllAuthenticator`.)
  - Fixed SQL Server bridge connect error caused by an empty password.
  - Made `username` a required field in Oracle bridge.
  - Fixed IoTDB bridge error caused by setting base URL without a scheme (e.g. `<host>:<port>`).
  
- [#11630](https://github.com/emqx/emqx/pull/11630) Fixed an issue where the core node could get stuck in the `mria_schema:bootstrap/0` state, preventing new nodes from joining the cluster.

## 5.2.0

*Release Date: 2023-09-07*

### Enhancements

- [#10697](https://github.com/emqx/emqx/pull/10697) This enhancement enables the configuration of the `minReadySeconds` for the StatefulSet. This feature allows for the introduction of a time gap between the restarts of individual pods triggered by upgrade or restart commands.

- [#11124](https://github.com/emqx/emqx/pull/11124) Released packages for Amazon Linux 2023.

- [#11289](https://github.com/emqx/emqx/pull/11289) Released packages for Debian 12.

- [#11290](https://github.com/emqx/emqx/pull/11290) Updated the `jq` dependency to version 0.3.10, which includes an update to the `oniguruma` library to version 6.9.8 with a few minor security fixes.

- [#11291](https://github.com/emqx/emqx/pull/11291) Updated RocksDB version to 1.8.0-emqx-1 via ekka update to 0.15.6.

- [#11390](https://github.com/emqx/emqx/pull/11390) Added `node.broker_pool_size`, `node.generic_pool_size`, `node.channel_cleanup_batch_size` options to EMQX configuration. Tuning these options can significantly improve the performance if cluster interconnect network latency is high.

- [#11429](https://github.com/emqx/emqx/pull/11429) Added an option to configure detection of the legacy protocol in MondoDB connectors and bridges.

- [#11436](https://github.com/emqx/emqx/pull/11436) Added a new API endpoint `DELETE/banned` for clearing all `banned` data.

- [#11438](https://github.com/emqx/emqx/pull/11438) Changed the type of the `mqtt.max_packet_size` from string to byteSize for a better representation of the valid numeric range. Strings will still be accepted for backward compatibility.
  
- [#11469](https://github.com/emqx/emqx/pull/11469) Added support for specifying username in Redis authentication.

- [#11496](https://github.com/emqx/emqx/pull/11496) Disabled the Erlang VM Prometheus exporter by default to improve performance and security.

- [#11497](https://github.com/emqx/emqx/pull/11497) Enhanced broker metrics collection and export by adding new metrics for messages, overload protection, authorization, authentication, and improving naming consistency for OpenTelemetry.
  
- [#10647](https://github.com/emqx/emqx/pull/10647) Implemented [GreptimeDB](https://github.com/GreptimeTeam/greptimedb) data integration.

- [#11261](https://github.com/emqx/emqx/pull/11261) Implemented Amazon Kinesis Data Streams producer data integration.

- [#11329](https://github.com/emqx/emqx/pull/11329) Implemented Azure Event Hub Producer data integration.

- [#11363](https://github.com/emqx/emqx/pull/11363) Added TLS connection support to the RabbitMQ bridge.

- [#11367](https://github.com/emqx/emqx/pull/11367) Ported GCP IoT Hub authentication support from EMQX 4.4.

- [#11386](https://github.com/emqx/emqx/pull/11386) Integrated LDAP as a new authenticator.

- [#11392](https://github.com/emqx/emqx/pull/11392) Integrated LDAP as an authorization source.

- [#11402](https://github.com/emqx/emqx/pull/11402) Added support for using placeholders to define MQTT Topic in Kafka Consumer bridge topic mappings. This allows dynamically setting the MQTT Topic.

- [#11403](https://github.com/emqx/emqx/pull/11403) Added support for defining message attributes and ordering key templates for GCP PubSub Producer bridge.

  Also updated our HOCON library to fix an issue where objects in an array were concatenated even if they were laid on different lines.

- [#11459](https://github.com/emqx/emqx/pull/11459) Added the option to configure health check interval for Kafka bridges.

- [#11478](https://github.com/emqx/emqx/pull/11478) Added HStreamDB bridge support (both TCP and TLS connection allowed), adapted to the HStreamDB `v0.16.1`.

  Updated driver to `0.4.5+v0.16.1` in [PR#11530](https://github.com/emqx/emqx/pull/11530).

- [#11389](https://github.com/emqx/emqx/pull/11389) Improved retained message publishing latency by consolidating multiple index update operations into a single Mnesia activity, leveraging the new APIs introduced in Mria 0.6.0.

- [#11396](https://github.com/emqx/emqx/pull/11396) Introduced topic index for the rule engine runtime to speed up matching messages' topics to topic filters configured in rule definitions by avoiding full scan of the rule set, significantly improving EMQX's performance when handling a substantial number of rules.

- [#11399](https://github.com/emqx/emqx/pull/11399) Improved the placeholder syntax in the rule engine. The republishing actions support placeholder syntax to
  dynamically fill in the content of strings in the payload variable. The format of the placeholder syntax is `\${key}`.
  Before this improvement, the `key` in `\${key}` could only contain letters, numbers, and underscores. Now the `key` supports any UTF8 characters.
  
- [#11405](https://github.com/emqx/emqx/pull/11405) Made the error message for `date_to_unix_ts` function more understandable.

- [#11490](https://github.com/emqx/emqx/pull/11490) Added fast error handling for undefined passwords in various authentication backends. This improves the consistency and user-friendliness of the authentication process.

### Bug Fixes

- [#11065](https://github.com/emqx/emqx/pull/11065) Silenced irrelevant error messages during EMQX shutdown.

- [#11279](https://github.com/emqx/emqx/pull/11279) Fixed an issue where clients could not send messages with large payloads when debug/trace logging was enabled in EMQX.

- [#11296](https://github.com/emqx/emqx/pull/11296) Added support for importing additional configurations from EMQX backup file using the `emqx ctl import` command):
  
  - rule_engine (previously not imported due to the bug)
  - topic_metrics (previously not implemented)
  - slow_subs (previously not implemented)
  
- [#11327](https://github.com/emqx/emqx/pull/11327) Updated ekka to version 0.15.8, mria to version 0.15.8, and optvar to 1.0.5. This fixes occasional assertion failures.

- [#11346](https://github.com/emqx/emqx/pull/11346) Updated ekka to version 0.15.9. This fixes dangling etcd locks that occurred when acquiring the lock failed with a timeout.
  
- [#11347](https://github.com/emqx/emqx/pull/11347) Ensured that OCSP request path is properly URL encoded.

- [#11352](https://github.com/emqx/emqx/pull/11352) Fixed a [crash issue](https://github.com/emqx/emqx/issues/11345) that occurred when starting on Windows or any other platform without RocksDB support.


- [#11388](https://github.com/emqx/emqx/pull/11388) Increased `emqx_router_sup` restart intensity to improve tolerance for occasional crashes that can occur under normal conditions, without necessitating the shutdown of the entire EMQX application.
  For example, mria write/delete call delegated from a replicant to a core node by `emqx_router_helper` may fail,
  if the core node undergoes stopping, restarting, or is in an unready state. The modified restart intensity ensures that the system remains stable and operational.
  

  This fixes issues found when trying to upgrade from 5.1.3 where that option was set in the configuration files or persisted in EMQX Operator settings.

- [#11424](https://github.com/emqx/emqx/pull/11424) Added a check for the maximum value of the timestamp in the API to ensure it is a valid Unix timestamp.

- [#11445](https://github.com/emqx/emqx/pull/11445) Removed os_mon application monitor support on Windows platforms to prevent VM crashes. Functionality remains on non-Windows platforms.
  
- [#11454](https://github.com/emqx/emqx/pull/11454) Fixed crashing when debugging/tracing with large payloads (introduced in [#11279](https://github.com/emqx/emqx/pull/11279)).

- [#11456](https://github.com/emqx/emqx/pull/11456) Removed validation that enforced non-empty PEM for the CA cert file, allowing the CA certificate file PEM to be empty.
  
- [#11466](https://github.com/emqx/emqx/pull/11466) Fixed a crash that occurred when setting the `ssl_options.ciphers` configuration option to an empty string ("").

- [#11480](https://github.com/emqx/emqx/pull/11480) Improves the error handling and testing of SQL functions in the rule engine when rule functions receive bad arguments.

- [#11520](https://github.com/emqx/emqx/pull/11520) Fixed issue where `packets_connack_sent` metric was not incremented on CONNACK packets sent with non-zero `ack_flag`.

- [#11523](https://github.com/emqx/emqx/pull/11523) Corrected a misleading prompt when specifying invalid certificates/keys for the `/configs` API.

- [#11534](https://github.com/emqx/emqx/pull/11534) Fixed the increment on data bridge statistics when the bridge is unhealthy. Now, messages sent to unhealthy bridges are counted as dropped messages.

- [#11540](https://github.com/emqx/emqx/pull/11540) Improved HTTP response when attempting to create a bridge with an invalid name.

- [#11548](https://github.com/emqx/emqx/pull/11548) Fixed an issue that prevented the plugin order from being updated across the entire cluster.

- [#11366](https://github.com/emqx/emqx/pull/11366) Fixed an issue that could prevent a pod from starting if some bridge configurations were specified in `bootstrapConfig` using EMQX Operator.

- [#11453](https://github.com/emqx/emqx/pull/11453) Fixed an issue that would yield false negatives when testing the connectivity of InfluxDB bridges.

- [#11461](https://github.com/emqx/emqx/pull/11461) Aligned the timeout for testing bridge connectivity more closely with the configured health check timeout.

- [#11492](https://github.com/emqx/emqx/pull/11492) Fixed an issue that would yield false negatives when testing the connectivity of GreptimeDB bridges.


- [#11508](https://github.com/emqx/emqx/pull/11508) Fixed error handling in Kafka bridge when headers are translated to an invalid value.

- [#11513](https://github.com/emqx/emqx/pull/11513) Fixed a bug that prevented the Kafka Producer bridge from using the correct template for the `timestamp` field.

- [#11527](https://github.com/emqx/emqx/pull/11527) Fixed an issue related to Kafka header template handling. The issue occurs when placeholders are resolved into an array of key-value pairs (e.g.: `[{"key": "foo", "value": "bar"}]`).

## 5.1.1

*Release Date: 2023-07-27*

### Enhancements

- [#10667](https://github.com/emqx/emqx/pull/10667) The MongoDB connector and bridge have been refactored into a separate app to improve the code structure.

- [#11115](https://github.com/emqx/emqx/pull/11115) Added info logs to indicate when buffered messages are dropped due to time-to-live (TTL) expiration.

- [#11133](https://github.com/emqx/emqx/pull/11133) Renamed `deliver_rate` to `delivery_rate` in the configuration of `retainer`, while being compatible with the previous `deliver_rate`.

- [#11137](https://github.com/emqx/emqx/pull/11137) Refactored the Dashboard listener configuration to use a nested `ssl_options` field for SSL settings.

- [#11138](https://github.com/emqx/emqx/pull/11138) Changed the default value of k8s `api_server` from `http://127.0.0.1:9091` to `https://kubernetes.default.svc:443`.
  
  - `emqx_ctl conf show cluster` no longer displays irrelevant configuration items when `discovery_strategy=static`.
  Configuration information related to `etcd/k8s/dns` will not be shown.
  - Removed `zones `(deprecated config key) from `emqx_ctl conf show_keys`.
  
- [#11165](https://github.com/emqx/emqx/pull/11165) Removed the `/configs/limiter` API from `swagger.json`. Only the API documentation was removed,
  and the `/configs/limiter` API functionalities remain unchanged.

- [#11166](https://github.com/emqx/emqx/pull/11166) Added 3 random SQL functions to the rule engine:
  
  - `random()`: Generates a random number between 0 and 1 (0.0 =< X < 1.0).
  - `uuid_v4()`: Generates a random UUID (version 4) string.
  - `uuid_v4_no_hyphen()`: Generates a random UUID (version 4) string without hyphens.
  
- [#11180](https://github.com/emqx/emqx/pull/11180) Added a new configuration API `/configs` (GET/PUT) that supports reloading the HOCON format configuration file.

- [#11226](https://github.com/emqx/emqx/pull/11226) Unified the listener switch to `enable`, while being compatible with the previous `enabled`.

- [#11249](https://github.com/emqx/emqx/pull/11249) Added `/license/setting` REST API endpoint to read and update licensed connections usage alarm watermark.

- [#11251](https://github.com/emqx/emqx/pull/11251) Added the `/cluster/topology` REST API endpoint:

  A `GET` request to this endpoint returns the cluster topology, showing connections between RLOG core and replicant nodes.

- [#11253](https://github.com/emqx/emqx/pull/11253) The Webhook/HTTP bridge has been refactored into its own Erlang application. This allows for more flexibility in the future and allows the bridge to be run as a standalone application.

- [#11079](https://github.com/emqx/emqx/pull/11079) Added support for custom headers in messages for Kafka bridge producer mode.

- [#11132](https://github.com/emqx/emqx/pull/11132) Added support for MQTT action authorization based on QoS level and Retain flag values.
  Now, EMQX can verify whether clients have the permission to publish/subscribe using specific QoS levels, and whether they have the permission to publish retained messages.

- [#11207](https://github.com/emqx/emqx/pull/11207) Updated the driver versions of multiple data bridges to enhance security and ensure that sensitive data will not be leaked. This includes:
  
  - TDengine
  - MongoDB
  - MySQL
  - Clickhouse

- [#11241](https://github.com/emqx/emqx/pull/11241) Schema Registry has been refactored into its own Erlang application. This allows for more flexibility in the future.

- [#11020](https://github.com/emqx/emqx/pull/11020) Upgraded emqtt dependency to prevent sensitive data leakage in the debug log.

- [#11135](https://github.com/emqx/emqx/pull/11135) Improved time offset parser in rule engine and return uniform error codes.

- [#11236](https://github.com/emqx/emqx/pull/11236) Improved the speed of clients querying in REST API `/clients` endpoint with default parameters.

### Bug Fixes

- [#11004](https://github.com/emqx/emqx/pull/11004) Wildcards are no longer allowed for the destination topic in topic rewrite.

- [#11026](https://github.com/emqx/emqx/pull/11026) Addressed an inconsistency in the usage of `div` and `mod` operations within the rule engine. Previously, the `div'` operation could only be used as an infix operation, and `mod` could only be applied through a function call. Now, both `div` and `mod` can be used via function call syntax and infix syntax.

- [#11037](https://github.com/emqx/emqx/pull/11037) When starting an HTTP connector, EMQX now returns a descriptive error in case the system is unable to connect to the remote target system.

- [#11039](https://github.com/emqx/emqx/pull/11039) Fixed database number validation for Redis connector. Previously, negative numbers were accepted as valid database numbers.

- [#11074](https://github.com/emqx/emqx/pull/11074) Fixed a bug to adhere to Protocol spec MQTT-5.0 [MQTT-3.8.3-4].

- [#11077](https://github.com/emqx/emqx/pull/11077) Fixed a crash when updating listener binding with a non-integer port.

- [#11094](https://github.com/emqx/emqx/pull/11094) Fixed an issue where connection errors in Kafka Producer would not be reported when reconnecting the bridge.

- [#11103](https://github.com/emqx/emqx/pull/11103) Updated `erlcloud` dependency.

- [#11106](https://github.com/emqx/emqx/pull/11106) Added validation for the maximum number of `worker_pool_size` of a bridge resource.

  Now the maximum amount is 1024 to avoid large memory consumption from an unreasonable number of workers.

- [#11118](https://github.com/emqx/emqx/pull/11118) Ensured that validation errors in REST API responses are slightly less confusing. Now, if there are out-of-range errors, they will be presented as `{"value": 42, "reason": {"expected": "1..10"}, ...}`, replacing the previous usage of `expected_type` with `expected`.

- [#11126](https://github.com/emqx/emqx/pull/11126) Rule metrics for async mode bridges will set failure counters correctly now.

- [#11134](https://github.com/emqx/emqx/pull/11134) Fixed the value of the uppercase `authorization` header not being obfuscated in the log.

- [#11139](https://github.com/emqx/emqx/pull/11139) The Redis connector has been refactored into its own Erlang application to improve the code structure.

- [#11145](https://github.com/emqx/emqx/pull/11145) Added several fixes and improvements in Ekka and Mria.

  Ekka:
  - Improved cluster discovery log messages to consistently describe actual events.
  [Ekka PR](https://github.com/emqx/ekka/pull/204)
  - Removed deprecated cluster auto-clean configuration parameter (it has been moved to Mria).
  [Ekka PR](https://github.com/emqx/ekka/pull/203)

  Mria:
  - Ping now only runs on replicant nodes. Previously, `mria_lb` was trying to ping both stopped and running
  replicant nodes, which could result in timeout errors.
  [Mria PR](https://github.com/emqx/mria/pull/146)
  - Used `null_copies` storage when copying `$mria_rlog_sync` table.
  This fix has no effect on EMQX for now, as `$mria_rlog_sync` is only used in `mria:sync_transaction/2,3,4`,
  which is not utilized by EMQX.
  [Mria PR](https://github.com/emqx/mria/pull/144)

- [#11148](https://github.com/emqx/emqx/pull/11148) Fixed an issue when nodes tried to synchronize configuration update operations to a node which has already left the cluster.

- [#11150](https://github.com/emqx/emqx/pull/11150) Wait for Mria table when emqx_psk app is being started to ensure that PSK data is synced to replicant nodes even if they don't have init PSK file.
  
- [#11151](https://github.com/emqx/emqx/pull/11151) The MySQL connector has been refactored into its own Erlang application to improve the code structure.

- [#11158](https://github.com/emqx/emqx/pull/11158) Wait for Mria table when the mnesia backend of retainer starts to avoid a possible error of the retainer when joining a cluster.

- [#11162](https://github.com/emqx/emqx/pull/11162) Fixed an issue in webhook bridge where, in async query mode, HTTP status codes like 4XX and 5XX would be treated as successes in the bridge metrics.

- [#11164](https://github.com/emqx/emqx/pull/11164) Reintroduced support for nested (i.e.: `${payload.a.b.c}`) placeholders for extracting data from rule action messages without the need for calling `json_decode(payload)` first.

- [#11172](https://github.com/emqx/emqx/pull/11172) Fixed the `payload` field in rule engine SQL being duplicated in the below situations:
  
  - When using a `foreach` sentence without the `as` sub-expression and selecting all fields (using the `*` or omitting the `do` sub-expression).
  
  For example:
  
  `FOREACH payload.sensors FROM "t/#"`
  - When selecting the `payload` field and all fields.
  
  For example:
  
  `SELECT payload.sensors, * FROM "t/#"`
  
- [#11174](https://github.com/emqx/emqx/pull/11174) Fixed the encoding of the `server` key coming from an ingress MQTT bridge.

  Before the fix, it was encoded as a list of integers corresponding to the ASCII characters of the server string.

- [#11184](https://github.com/emqx/emqx/pull/11184) Config value for `mqtt.max_packet_size` now has a max value of 256MB as defined by the protocol.

- [#11192](https://github.com/emqx/emqx/pull/11192) Fixed an issue with producing invalid HOCON file when an atom type was used. Also removed unnecessary `"` around keys and latin1 strings from HOCON file.
  
- [#11195](https://github.com/emqx/emqx/pull/11195) Fixed an issue where the REST API could create duplicate subscriptions for specified clients of the Stomp gateway.

- [#11206](https://github.com/emqx/emqx/pull/11206) Made the `username` and `password` params of CoAP client optional in connection mode.

- [#11208](https://github.com/emqx/emqx/pull/11208) Fixed the issue of abnormal data statistics for LwM2M clients.

- [#11211](https://github.com/emqx/emqx/pull/11211) HTTP API `DELETE` operations on non-existent resources now consistently returns `404`.

- [#11214](https://github.com/emqx/emqx/pull/11214) Fixed a bug where node configuration may fail to synchronize correctly when the node joins the cluster.

- [#11229](https://github.com/emqx/emqx/pull/11229) Fixed an issue that prevented plugins from starting/stopping after changing configuration via `emqx ctl conf load`.

- [#11237](https://github.com/emqx/emqx/pull/11237) The `headers` default value in /prometheus API should be a map instead of a list.

- [#11250](https://github.com/emqx/emqx/pull/11250) Fixed a bug when the order of MQTT packets withing a WebSocket packet will be reversed.


- [#11271](https://github.com/emqx/emqx/pull/11271) Ensured that the range of all percentage type configurations is from 0% to 100% in the REST API and configuration. For example, `sysom.os.sysmem_high_watermark=101%` is invalid now.

- [#11272](https://github.com/emqx/emqx/pull/11272) Fixed a typo in the log, where an abnormal `PUBREL` packet was mistakenly referred to as `pubrec`.

- [#11281](https://github.com/emqx/emqx/pull/11281) Restored support for the special `$queue/` shared subscription topic prefix.

- [#11294](https://github.com/emqx/emqx/pull/11294) Fixed `emqx ctl cluster join`, `leave`, and `status` commands.

- [#11306](https://github.com/emqx/emqx/pull/11306) Fixed rule action metrics inconsistency where dropped requests were not accounted for.

- [#11309](https://github.com/emqx/emqx/pull/11309) Improved startup order of EMQX applications. Simplified build scripts and improved code reuse.
  
- [#11322](https://github.com/emqx/emqx/pull/11322) Added support for importing additional configurations from EMQX backup file (`emqx ctl import` command):
  
  - rule_engine (previously not imported due to the bug)
  - topic_metrics (previously not implemented)
  - slow_subs (previously not implemented).
  
- [#10645](https://github.com/emqx/emqx/pull/10645) Changed health check for Oracle Database, PostgreSQL, MySQL and Kafka Producer data bridges to ensure target table/topic exists.

- [#11107](https://github.com/emqx/emqx/pull/11107) MongoDB bridge health check now returns the failure reason.

- [#11139](https://github.com/emqx/emqx/pull/11139) The Redis bridge has been refactored into its own Erlang application to improve the code structure and to make it easier to maintain.

- [#11151](https://github.com/emqx/emqx/pull/11151) The MySQL bridge has been refactored into its own Erlang application to improve the code structure and to make it easier to maintain.

- [#11163](https://github.com/emqx/emqx/pull/11163) Hid `topology.pool_size` in MondoDB bridges and fixed it to 1 to avoid confusion.

- [#11175](https://github.com/emqx/emqx/pull/11175) Now when using a nonexistent hostname for connecting to MySQL, a 400 error is returned rather than 503 in the REST API.

- [#11198](https://github.com/emqx/emqx/pull/11198) Fixed global rebalance status evaluation on replicant nodes. Previously, `/api/v5/load_rebalance/global_status` API method could return incomplete results if handled by a replicant node.
  
- [#11223](https://github.com/emqx/emqx/pull/11223) In InfluxDB bridging, mixing decimals and integers in a field may lead to serialization failure in the Influx Line Protocol, resulting in the inability to write to the InfluxDB bridge (when the decimal point is 0, InfluxDB mistakenly interprets it as an integer).

  See also: [InfluxDB v2.7 Line-Protocol](https://docs.influxdata.com/influxdb/v2.7/reference/syntax/line-protocol/#float).

- [#11225](https://github.com/emqx/emqx/pull/11225) The `username` field in PostgreSQL/Timescale/MatrixDB bridges configuration is now a required one.

- [#11242](https://github.com/emqx/emqx/pull/11242) Restarted emqx_ee_schema_registry when a node joins a cluster. As emqx_ee_schema_registry uses Mria tables, a node joining a cluster needs to restart this application in order to start relevant Mria shard processes, ensuring a correct behaviour in Core/Replicant mode.

- [#11266](https://github.com/emqx/emqx/pull/11266) Fixed and improved support for TDengine `insert` syntax:

  1. Added support for inserting into multi-table in the template.

     For example:

     `insert into table_1 values (${ts}, '${id}', '${topic}')
     table_2 values (${ts}, '${id}', '${topic}')`

  2. Added support for mixing prefixes/suffixes and placeholders in the template.

     For example:

     `insert into table_${topic} values (${ts}, '${id}', '${topic}')`

     Note: This is a breaking change. Previously, the values of string type were quoted automatically, but now they must be quoted explicitly.

     For example:

     `insert into table values (${ts}, '${a_string}')`

- [#11307](https://github.com/emqx/emqx/pull/11307) Fixed check for table existence to return a more friendly message in the Oracle bridge.

- [#11316](https://github.com/emqx/emqx/pull/11316) Fixed Pool Size value not being considered in Oracle Bridge.

- [#11326](https://github.com/emqx/emqx/pull/11326) Fixed return error checking on table validation in the Oracle bridge.


## 5.1.0

*Release Date: 2023-06-21*

### Enhancements

-   [#11035](https://github.com/emqx/emqx/pull/11035) Upgraded Cassandra driver to avoid username and password leakage in data bridge logs.
-   [#10584](https://github.com/emqx/emqx/pull/10584) Added log level configuration to SSL communication
-   [#10678](https://github.com/emqx/emqx/pull/10678) Optimized counter increment calls to avoid work if increment is zero.
-   [#10690](https://github.com/emqx/emqx/pull/10690) Added a retry mechanism to webhook bridge that attempts to improve throughput.
    This optimization retries request failures without blocking the buffering layer, which can improve throughput in situations of high messaging rate.
-   [#10702](https://github.com/emqx/emqx/pull/10702) Introduced a more straightforward configuration option `keepalive_multiplier` and deprecate the old `keepalive_backoff` configuration. After this enhancement, EMQX checks the client's keepalive timeout status period by multiplying the "Client Requested Keepalive Interval" with `keepalive_multiplier`.
-   [#10698](https://github.com/emqx/emqx/pull/10698) Optimized memory usage when accessing the configuration during runtime.
-   [#10778](https://github.com/emqx/emqx/pull/10778) Refactored Pulsar Producer bridge to avoid leaking resources in case bridge crashed during initialization phase.
-   [#10813](https://github.com/emqx/emqx/pull/10813) Refactored Kafka Producer and Consumer bridges to avoid leaking resources in case bridge crashed during initialization phase.
-   [#10858](https://github.com/emqx/emqx/pull/10858) A new utility function timezone_to_offset_seconds/1 has been added to the rule engine SQL language. This function converts a timezone string (for example, "+02:00", "Z" and "local") to the corresponding offset in seconds.
-   [#10841](https://github.com/emqx/emqx/pull/10841) Added a schema validation to ensure message key is not empty when "key_dispatch" strategy is selected in Kafka and Pulsar Producer bridges.
-   [#10754](https://github.com/emqx/emqx/pull/10754) The MQTT bridge has been enhanced to utilize connection pooling and leverage available parallelism, substantially improving throughput.
    As a consequence, single MQTT bridge now uses a pool of `clientid`s to connect to the remote broker.
-   [#10782](https://github.com/emqx/emqx/pull/10782) Added a new `deliver_rate` option to the retainer configuration, which can limit the maximum delivery rate per session in the retainer.
-   [#10877](https://github.com/emqx/emqx/pull/10877) Upgraded RocketMQ driver to enhance security for sensitive data.
-   [#10598](https://github.com/emqx/emqx/pull/10598) Provided a callback method of Unary type in ExProto to avoid possible message disorder issues.
-   [#10895](https://github.com/emqx/emqx/pull/10895) Refactored most of the bridges to avoid resource leaks in case bridge crashed during initialization phase.
-   [#10790](https://github.com/emqx/emqx/pull/10790) Optimized access to configuration in runtime by reducing overhead of reading configuration per zone.
-   [#10892](https://github.com/emqx/emqx/pull/10892) Added the requirement for setting SID or Service Name in Oracle Database bridge creation.
-   [#10910](https://github.com/emqx/emqx/pull/10910) The data bridge resource option `auto_restart_interval` was deprecated in favor of `health_check_interval`, and `request_timeout` was renamed to `request_ttl`. Also, the default `request_ttl` value went from 15 seconds to 45 seconds.
    The previous existence of both `auto_restart_interval` and `health_check_interval` was a source of confusion, as both parameters influenced the recovery of data bridges under failures. An inconsistent configuration of those two parameters could lead to messages being expired without a chance to retry. Now, `health_check_interval` is used both to control the interval of health checks that may transition the data bridge into `disconnected` or `connecting` states, as well as recovering from `disconnected`.
-   [#10929](https://github.com/emqx/emqx/pull/10929) Upgraded Erlang/OTP to 25.3.2-1.
-   [#10909](https://github.com/emqx/emqx/pull/10909) Removed the deprecated HTTP APIs for gateways.
-   [#10908](https://github.com/emqx/emqx/pull/10908) Refactored the RocketMQ bridge to avoid resources leaks in case bridge crashed during initialization phase.
-   [#10924](https://github.com/emqx/emqx/pull/10924) Refactored Influxdb bridge connector to avoid resource leaks in case bridge crashed during initialization phase.
-   [#10944](https://github.com/emqx/emqx/pull/10944) Improved the GCP PubSub bridge to avoid a potential issue that the bridge could fail to send messsages after node restart.
-   [#10933](https://github.com/emqx/emqx/pull/10933) Added support for configuring TCP keep-alive in MQTT/TCP and MQTT/SSL listeners.
-   [#10948](https://github.com/emqx/emqx/pull/10948) Added `live_connections` field for some HTTP APIs, i.e:
    -   `/monitor_current`, `/monitor_current/nodes/{node}`
    -   `/monitor/nodes/{node}`, `/monitor`
    -   `/node/{node}`, `/nodes`
-   [#10941](https://github.com/emqx/emqx/pull/10941) Improved the collection speed of Prometheus metrics when setting `prometheus.vm_dist_collector=disabled` and metric `erlang_vm_statistics_run_queues_length_total` is renamed to `erlang_vm_statistics_run_queues_length`
-   [#10985](https://github.com/emqx/emqx/pull/10985) Renamed `emqx ctl` command `cluster_call` to `conf cluster_sync`. The old command `cluster_call` is still a valid command, but not included in usage info.
-   [#10988](https://github.com/emqx/emqx/pull/10988) Improved log security when data bridge creation fails to ensure sensitive data is always obfuscated.
-   [#10926](https://github.com/emqx/emqx/pull/10926) Allowed `enable` as well as `enabled` as the state flag for listeners.
    Prior to this change, listener can be enable/disabled by setting the `true` or `false` on the `enabled` config. This is slightly different naming comparing to other state flags in the system. Now the `enable` flag is added as an alias in listener config.
-   [#10970](https://github.com/emqx/emqx/pull/10970) A query_mode parameter has been added to the Kafka producer bridge. This parameter allows you to specify if the bridge should use the asynchronous or synchronous mode when sending data to Kafka. The default is asynchronous mode.
-   [#10676](https://github.com/emqx/emqx/pull/10676) Added CLI commands `emqx ctl export` and `emqx ctl import` for importing/exporting configuration and user data. This allows exporting configurations and built-in database data from a running EMQX cluster and importing them into the same or another running EMQX cluster.
-   [#11003](https://github.com/emqx/emqx/pull/11003) Added an option to configure TCP keepalive in Kafka bridge.
-   [#10961](https://github.com/emqx/emqx/pull/10961) Added support for unlimited max connections for gateway listeners by allowing infinity as a valid value for the `max_connections` field in the configuration and HTTP API.
-   [#11019](https://github.com/emqx/emqx/pull/11019) Improved log security for JWT, now it will be obfuscated before print.
-   [#11024](https://github.com/emqx/emqx/pull/11024) Added a small improvement to reduce the chance of seeing the `connecting` state when creating/updating a Pulsar Producer bridge.
-   [#11034](https://github.com/emqx/emqx/pull/11034) Hid the broker config and changed the `broker.shared_subscription_strategy` to `mqtt.shared_subscription_strategy` as it belongs to `mqtt`.
-   [#11045](https://github.com/emqx/emqx/pull/11045) The listener's authentication and zone related apis have been officially removed in version `5.1.0`.
-   [#11062](https://github.com/emqx/emqx/pull/11062) Renamed config `log.file.to` to `log.file.path`.

### Bug Fixes

-   [#11018](https://github.com/emqx/emqx/pull/11018) Fixed multiple issues with the Stomp gateway, including:
    -   Fixed an issue where `is_superuser` was not working correctly.
    -   Fixed an issue where the mountpoint was not being removed in message delivery.
    -   After a message or subscription request fails, the Stomp client should be disconnected
        immediately after replying with an ERROR message.
-   [#11051](https://github.com/emqx/emqx/pull/11051) Added validation to ensure that certificate `depth` (listener SSL option) is a non negative integer.
-   [#10563](https://github.com/emqx/emqx/pull/10563) Corrected an issue where the no_local flag was not functioning correctly in subscription.
-   [#10653](https://github.com/emqx/emqx/pull/10653) Stored gateway authentication TLS certificates and keys in the data directory to fix the problem of memory leakage.
-   [#10682](https://github.com/emqx/emqx/pull/10682) Fixed the timestamp for the will message is incorrectly assigned at the session creation time, now this timestamp is the disconnected time of the session.
-   [#10701](https://github.com/emqx/emqx/pull/10701) RPM package for Amazon Linux 2 did not support TLS v1.3 as it was assembled with Erlang/OTP built with openssl 1.0.
-   [#10677](https://github.com/emqx/emqx/pull/10677) Fixed an issue in the Rule API where attempting to delete a non-existent rule resulted in a 404 HTTP error code response.
-   [#10715](https://github.com/emqx/emqx/pull/10715) Support for getting the client certificate in the client.connected hook. Previously, this data was removed after the connection was established to reduce memory usage.
-   [#10737](https://github.com/emqx/emqx/pull/10737) Fixed the issue where the HTTP API interface of Gateway cannot handle ClientIDs with special characters, such as: `!@#$%^&*()_+{}:"<>?/`.
-   [#10809](https://github.com/emqx/emqx/pull/10809) Addressed `** ERROR ** Mnesia post_commit hook failed: error:badarg` error messages happening during node shutdown or restart. Mria pull request: [https://github.com/emqx/mria/pull/142](https://github.com/emqx/mria/pull/142)
-   [#10807](https://github.com/emqx/emqx/pull/10807) The debug-level logs related to license checks will no longer be printed. These logs were generated too frequently and could interfere with log recording.
-   [#10818](https://github.com/emqx/emqx/pull/10818) Fixed `emqx_ctl traces` command error where the `traces start` command in the `emqx_mgmt_cli` module was not working properly with some filters.
-   [#10600](https://github.com/emqx/emqx/pull/10600) Deleted emqx_statsd application.
-   [#10820](https://github.com/emqx/emqx/pull/10820) Fixed the issue where newly added nodes in the cluster would not apply the new license after a cluster license update and would continue to use the old license.
    Sometimes the new node must start with a outdated license. e.g. use emqx-operator deployed and needed to scale up after license expired. At the time the cluster's license key already updated by API/CLI, but the new node won't use it.
-   [#10851](https://github.com/emqx/emqx/pull/10851) Obfuscated sensitive data in the bad API logging.
-   [#10884](https://github.com/emqx/emqx/pull/10884) Fixed an issue where trying to get rule info or metrics could result in a crash when a node is joining a cluster.
-   [#10887](https://github.com/emqx/emqx/pull/10887) Fixed a potential issue where requests to bridges might take a long time to be retried.
    This only affected low throughput scenarios, where the buffering layer could take a long time to detect connectivity and driver problems.
-   [#10878](https://github.com/emqx/emqx/pull/10878) Fixed a vulnerability in the RabbitMQ bridge, which could potentially expose passwords to log files.
-   [#10871](https://github.com/emqx/emqx/pull/10871) Fixed an issue where the Dashboard shows that the connection still exists after a CoAP connection is disconnected, but deletion and message posting requests do not take effect.
-   [#10880](https://github.com/emqx/emqx/pull/10880) Added a new REST API `POST /clients/kickout/bulk` for kicking out multiple clients in bulk.
-   [#10913](https://github.com/emqx/emqx/pull/10913) Fixed an issue where the plugin status REST API of a node would still include the cluster node status after the node left the cluster.
-   [#10923](https://github.com/emqx/emqx/pull/10923) Fixed a race-condition in channel info registration.
    Prior to this fix, when system is under heavy load, it might happen that a client is disconnected (or has its session expired) but still can be found in the clients page in dashboard. One of the possible reasons is a race condition fixed in this PR: the connection is killed in the middle of channel data registration.
-   [#10930](https://github.com/emqx/emqx/pull/10930) Added a schema validation for duration data type to avoid invalid values.
    Before this fix, it was possible to use absurd values in the schema that would exceed the system limit, causing a crash.
-   [#10952](https://github.com/emqx/emqx/pull/10952) Disallow enabling `fail_if_no_peer_cert` in listener SSL options if `verify = verify_none` is set.
    Setting `fail_if_no_peer_cert = true` and `verify = verify_none` caused connection errors due to incompatible options. This fix validates the options when creating or updating a listener to avoid these errors.

    Note: any old listener configuration with `fail_if_no_peer_cert = true` and `verify = verify_none` that was previously allowed will fail to load after applying this fix and must be manually fixed.
-   [#10951](https://github.com/emqx/emqx/pull/10951) Fixed the issue in MQTT-SN gateway when the `mountpoint` did not take effect on message publishing.
-   [#10943](https://github.com/emqx/emqx/pull/10943) Deprecated UDP mcast mechanism for cluster discovery.
    This feature has been planed for deprecation since 5.0 mainly due to the lack of actual production use. This feature code is not yet removed in 5.1, but the document interface is demoted.
-   [#10902](https://github.com/emqx/emqx/pull/10902) Avoid syncing cluser.hocon file from the nodes running a newer version than the self-node.
    During cluster rolling upgrade, if an older version node has to restart due to whatever reason, if it copies the `cluster.hocon` file from a newer version node, it may fail to start. After this fix, the older version node will not copy the `cluster.hocon` file from a newer, so it will use its own `cluster.hocon` file to start.
-   [#10967](https://github.com/emqx/emqx/pull/10967) Fixed error message formatting in rebalance API: previously they could be displayed as unclear dumps of internal Erlang structures.
    Added `wait_health_check` option to node evacuation CLI and API. This is a time interval when the node reports "unhealthy status" without beginning actual evacuation. We need this to allow a Load Balancer (if any) to remove the evacuated node from balancing and not forward (re)connecting clients to the evacuated node.
-   [#10911](https://github.com/emqx/emqx/pull/10911) The error message and log entry that appear when one tries to create a bridge with a name the exceeds 255 bytes is now easier to understand.
-   [#10983](https://github.com/emqx/emqx/pull/10983) Fixed the issue when mqtt clients could not connect over TLS if the listener was configured to use TLS v1.3 only.
    The problem was that TLS connection was trying to use options incompatible with TLS v1.3.
-   [#10977](https://github.com/emqx/emqx/pull/10977) Fixed the delay in updating subscription count metric and corrected configuration issues in Stomp gateway.
-   [#10950](https://github.com/emqx/emqx/pull/10950) Fixed the issue where the `enable_qos` option does not take effect in the MQTT-SN gateway.
-   [#10999](https://github.com/emqx/emqx/pull/10999) Changed schema validation for Kafka fields 'Partition Count Refresh Interval' and 'Offset Commit Interval' to avoid accepting values larger then maximum allowed.
-   [#10997](https://github.com/emqx/emqx/pull/10997) The ClickHouse bridge had a problem that could cause messages to be dropped when the ClickHouse server is closed while sending messages even when the request_ttl is set to infinity. This has been fixed by treating errors due to a closed connection as recoverable errors.
-   [#10994](https://github.com/emqx/emqx/pull/10994) Redacted `proxy-authorization` headers as used by HTTP connector to avoid leaking secrets into log files.
-   [#10996](https://github.com/emqx/emqx/pull/10996) For any unknown HTTP/API request, the default response is a 404 error rather than the dashboard's index.html.
-   [#11005](https://github.com/emqx/emqx/pull/11005) Fixed the issue where the `method` field cannot be correctly printed in the trace logs of AuthN HTTP.
-   [#11006](https://github.com/emqx/emqx/pull/11006) Fixed QUIC listeners's default cert file paths.
    Prior to this change, the default cert file paths are prefixed with environment variable `${EMQX_ETC_DIR}` which were not interpolated before used in QUIC listeners.
-   [#10998](https://github.com/emqx/emqx/pull/10998) Do not allow `batch_size` option for MongoDB bridge resource. MongoDB connector currently does not support batching, the `batch_size` config value is forced to be 1 if provided.
-   [#10955](https://github.com/emqx/emqx/pull/10955) Fixed the issue in MQTT-SN gateway where deleting Predefined Topics configuration does not work.
-   [#11025](https://github.com/emqx/emqx/pull/11025) Fixed a `case_clause` error that could arise in race conditions in Pulsar Producer bridge.
-   [#11030](https://github.com/emqx/emqx/pull/11030) Improved error messages when a validation error occurs while using the Listeners HTTP API.
-   [#11033](https://github.com/emqx/emqx/pull/11033) Deprecated the `mountpoint` field in `AuthenticateRequest` in ExProto gateway.
    This field was introduced in e4.x, but in fact, in e5.0 we have provided
    `gateway.exproto.mountpoint` for configuration, so there is no need to override
    it through the Authenticate request.

    Additionally, updates the default value of `subscriptions_max`, `inflight_max`,
    `mqueue_max` to `infinity`.
-   [#11040](https://github.com/emqx/emqx/pull/11040) Fixed a health check issue for Kafka Producer that could lead to loss of messages when the connection to Kafka's brokers were down.
-   [#11038](https://github.com/emqx/emqx/pull/11038) Fixed a health check issue for Pulsar Producer that could lead to loss of messages when the connection to Pulsar's brokers were down.
-   [#11042](https://github.com/emqx/emqx/pull/11042) Fixed crash on REST API `GET /listeners` when listener's `max_connections` is set to a string.
-   [#11028](https://github.com/emqx/emqx/pull/11028) Disallowed using multiple TLS versions in the listener config that include tlsv1.3 but exclude tlsv1.2.
    Using TLS configuration with such version gap caused connection errors.
    Additionally, drop and log TLS options that are incompatible with the selected TLS version(s).

    Note: any old listener configuration with the version gap described above will fail to load
    after applying this fix and must be manually fixed.
-   [#11031](https://github.com/emqx/emqx/pull/11031) Fixed credential validation when creating bridge and checking status for InfluxDB Bridges.
-   [#11056](https://github.com/emqx/emqx/pull/11056) Fixed the issue where newly created listeners sometimes do not start properly.
    When you delete a system default listener and add a new one named 'default', it will not start correctly.
    -   Fixed the bug where configuration failure on certain nodes can cause Dashboard unavailability.
-   [#11070](https://github.com/emqx/emqx/pull/11070) Fixed the problem that the `cluster.autoclean` configuration item does not take effect.
-   [#11092](https://github.com/emqx/emqx/pull/11092) and [#11100](https://github.com/emqx/emqx/pull/11100) Fixed problem when replicat nodes were unable to connect to the core node due to timeout in `mria_lb:core_nodes()` call.
    Relevant mria pull request: [https://github.com/emqx/mria/pull/143](https://github.com/emqx/mria/pull/143)




## 5.0.4

_Release Date: 2023-05-26_

### Enhancements

- [#10389](https://github.com/emqx/emqx/pull/10389) Unified the configuration formats for `cluster.core_nodes` and `cluster.statics.seeds`. Now they both support formats in array `["emqx1@127.0.0.1", "emqx2@127.0.0.1"]` and the comma-separated string `"emqx1@127.0.0.1,emqx2@127.0.0.1"`.

- [#10392](https://github.com/emqx/emqx/pull/10392) Introduced a new function to convert a formatted date to an integer timestamp: date_to_unix_ts/3.

  `date_to_unix_ts(TimeUnit, FormatString, InputDateTimeString)`

- [#10426](https://github.com/emqx/emqx/pull/10426) Optimized the configuration priority mechanism to fix the issue where the configuration changes made to `etc/emqx.conf` do not take effect after restarting EMQX.

  More information about the new mechanism: [Configure Override Rules](https://www.emqx.io/docs/en/v5.0/configuration/configuration.html#configure-override-rules)

- [#10457](https://github.com/emqx/emqx/pull/10457) Deprecated the integration with StatsD.

- [#10458](https://github.com/emqx/emqx/pull/10458) Set the level of plugin configuration options to low, users usually manage the plugins through the dashboard, rarely modify them manually, so we lowered the level.

- [#10491](https://github.com/emqx/emqx/pull/10491) Renamed `etcd.ssl` to `etcd.ssl_options` to keep all SSL options consistent in the configuration file.

- [#10512](https://github.com/emqx/emqx/pull/10512) Improved the storage format of Unicode characters in data files, Now we can store Unicode characters. For example: `SELECT * FROM "t/1" WHERE clientid = "-测试专用-"`.

- [#10568](https://github.com/emqx/emqx/pull/10568) Added `shutdown_count` printout to `emqx ctl listeners` command.

- [#10588](https://github.com/emqx/emqx/pull/10588) Increased the time precision of trace logs from second to microsecond. For example, change from `2023-05-02T08:43:50+00:00` to `2023-05-02T08:43:50.237945+00:00`.

- [#10623](https://github.com/emqx/emqx/pull/10623) Renamed `max_message_queue_len` to `max_mailbox_size` in the `force_shutdown` configuration. The old name is kept as an alias, so this change is backward compatible.

- [#10713](https://github.com/emqx/emqx/pull/10713) Hide the `resource_option.request_timeout` of the webhook and it will use the value of `http` `request_timeout`.

- [#10075](https://github.com/emqx/emqx/pull/10075) Added node rebalance/node evacuation functionality. See also: [EIP doc](https://github.com/emqx/eip/blob/main/active/0020-node-rebalance.md)

- [#10378](https://github.com/emqx/emqx/pull/10378) Implemented Pulsar Producer Bridge and only producer role is supported now.

- [#10408](https://github.com/emqx/emqx/pull/10408) Introduced 3 built-in functions in the rule engine SQL-like language for creating values of the MongoDB date type.

- [#10409](https://github.com/emqx/emqx/pull/10409) [#10337](#10337) Supported [Protocol Buffers](https://protobuf.dev/) and [Apache Avro](https://avro.apache.org/) schemas in Schema Registry.

- [#10425](https://github.com/emqx/emqx/pull/10425) Implemented OpenTSDB data bridge.

- [#10498](https://github.com/emqx/emqx/pull/10498) Implemented Oracle Database Bridge.

- [#10560](https://github.com/emqx/emqx/pull/10560) Added enterprise data bridge for Apache IoTDB.

- [#10417](https://github.com/emqx/emqx/pull/10417) Improved get config items performance by eliminating temporary references.

- [#10430](https://github.com/emqx/emqx/pull/10430) Simplified the configuration of the `retainer` feature. Marked `flow_control` as a non-importance field.

- [#10511](https://github.com/emqx/emqx/pull/10511) Improved the security and privacy of some resource logs by masking sensitive information in the log.

- [#10525](https://github.com/emqx/emqx/pull/10525) Reduced resource usage per MQTT packet handling.

- [#10528](https://github.com/emqx/emqx/pull/10528) Reduced memory footprint in hot code path. The hot path includes the code that is frequently executed in core functionalities such as message handling, connection management, authentication, and authorization.

- [#10591](https://github.com/emqx/emqx/pull/10591) [#10625](https://github.com/emqx/emqx/pull/10625) Improved the configuration of the limiter.

  - Reduced the complexity of the limiter's configuration.

  - Updated the `configs/limiter` API to suit this refactor.

  - Reduced the memory usage of the limiter configuration.

- [#10487](https://github.com/emqx/emqx/pull/10487) Optimized the instance of limiter for whose rate is `infinity` to reduce memory and CPU usage.

- [#10490](https://github.com/emqx/emqx/pull/10490) Removed the default limit of connect rate which used to be `1000/s`.

- [#10077](https://github.com/emqx/emqx/pull/10077) Added support for QUIC TLS password-protected certificate file.

### Bug Fixes

- [#10340](https://github.com/emqx/emqx/pull/10340) Fixed the issue that could lead to crash logs being printed when stopping EMQX via `systemd`.

- [#10369](https://github.com/emqx/emqx/pull/10369) Fixed error in `/api/v5/monitor_current` API endpoint that happens when some EMQX nodes are down.

  Prior to this fix, sometimes the request returned HTTP code 500 and the following message:

  `{"code":"INTERNAL_ERROR","message":"error, badarg, [{erlang,'++',[{error,nodedown},[{node,'emqx@10.42.0.150'}]], ...`

- [#10407](https://github.com/emqx/emqx/pull/10407) Fixed the crash issue of the alarm system.

  - Leverage Mnesia dirty operations and circumvent extraneous calls to enhance 'emqx_alarm' performance.

  - Use 'emqx_resource_manager' for reactivating alarms that have already been triggered.

  - Implement the newly developed, fail-safe 'emqx_alarm' API to control the activation and deactivation of alarms, thus preventing 'emqx_resource_manager' from crashing due to alarm timeouts.

  - The alarm system is susceptible to crashing under these concurrent conditions:

    - A significant number of resources fail, such as when bridges continuously attempt to trigger alarms due to recurring errors.

    - The system is under an extremely high load.

- [#10420](https://github.com/emqx/emqx/pull/10420) Fixed HTTP path handling when composing the URL for the HTTP requests in authentication and authorization modules.

  - Avoid unnecessary URL normalization since we cannot assume that external servers treat original and normalized URLs equally. This led to bugs like [#10411](https://github.com/emqx/emqx/issues/10411).

  - Fixed the issue that path segments could be HTTP encoded twice.

- [#10422](https://github.com/emqx/emqx/pull/10422) Fixed a bug where external plugins could not be configured via environment variables in a lone-node cluster.

- [#10448](https://github.com/emqx/emqx/pull/10448) Fixed a compatibility issue of limiter configuration introduced by e5.0.3 which broke the upgrade from previous versions if the `capacity` is `infinity`.

  In e5.0.3 we have replaced `capacity` with `burst`. After this fix, a `capacity = infinity` config will be automatically converted to equivalent `burst = 0`.

- [#10462](https://github.com/emqx/emqx/pull/10462) Deprecated config `broker.shared_dispatch_ack_enabled`. This was designed to avoid dispatching messages to a shared-subscription session that has the client disconnected. However, since e5.0.0, this feature is no longer helpful because the shared-subscription messages in an expired session will be redispatched to other sessions in the group. See also: <https://github.com/emqx/emqx/pull/9104> .

- [#10463](https://github.com/emqx/emqx/pull/10463) Improved bridges API error handling. If Webhook bridge URL is not valid, the bridges API will return '400' error instead of '500'.

- [#10484](https://github.com/emqx/emqx/pull/10484) Fixed the issue that the priority of the configuration cannot be set during the rolling upgrade. For example, when authorization is modified in e5.0.2 and then upgraded e5.0.3 through the rolling upgrade, the authorization will be restored to the default.

- [#10495](https://github.com/emqx/emqx/pull/10495) Added the limiter API `/configs/limiter` which was deleted by mistake back.

- [#10500](https://github.com/emqx/emqx/pull/10500) Added several fixes, enhancements, and features in Mria:

  - Protect `mria:join/1,2` with a global lock to prevent conflicts between two nodes trying to join each other simultaneously [Mria PR](https://github.com/emqx/mria/pull/137)

  - Implement new function `mria:sync_transaction/4,3,2`, which blocks the caller until a transaction is imported to the local node (if the local node is a replicant, otherwise, it behaves exactly the same as `mria:transaction/3,2`) [Mria PR](https://github.com/emqx/mria/pull/136)

  - Optimize `mria:running_nodes/0` [Mria PR](https://github.com/emqx/mria/pull/135)

  - Optimize `mria:ro_transaction/2` when called on a replicant node [Mria PR](https://github.com/emqx/mria/pull/134).

- [#10518](https://github.com/emqx/emqx/pull/10518) Added the following fixes and features in Mria:

  - Call `mria_rlog:role/1` safely in mria_membership to ensure that mria_membership gen_server won't crash if RPC to another node fails [Mria PR](https://github.com/emqx/mria/pull/139)

  - Add an extra field to `?rlog_sync` table to facilitate extending this functionality in future [Mria PR](https://github.com/emqx/mria/pull/138).

- [#10556](https://github.com/emqx/emqx/pull/10556) Wrapped potentially sensitive data in `emqx_connector_http` if `Authorization` headers are being passed at initialization.

- [#10571](https://github.com/emqx/emqx/pull/10571) Stopped emitting useless crash report when EMQX stops.

- [#10659](https://github.com/emqx/emqx/pull/10659) Fixed the issue where EMQX cannot start when `sysmon.os.mem_check_interval` is disabled.

- [#10717](https://github.com/emqx/emqx/pull/10717) Fixed an issue where the buffering layer processes could use a lot of CPU when inflight window is full.

- [#10724](https://github.com/emqx/emqx/pull/10724) A summary has been added for all endpoints in the HTTP API documentation (accessible at "http://<emqx_host_name\>:18083/api-docs").

- [#10726](https://github.com/emqx/emqx/pull/10726) Health Check Interval and Auto Restart Interval now support the range from 1ms to 1 hour.

- [#10728](https://github.com/emqx/emqx/pull/10728) Fixed an issue where the rule engine was unable to access variables exported by `FOREACH` - `DO` clause.

  Given a payload: `{"date": "2023-05-06", "array": ["a"]}`, as well as the following SQL statement:

  `FOREACH   payload.date as date, payload.array as elem DO   date, elem FROM "t/#"  -- {"date": "2023-05-06", "array": ["a"]}`

  Prior to the fix, the `date` variable exported by `FOREACH` could not be accessed in the `DO` clause of the above SQL, resulting in the following output for the SQL statement: `[{"elem": "a","date": "undefined"}]`.

- [#10742](https://github.com/emqx/emqx/pull/10742) Correctness check of the rules is enforced before saving the authorization file source. Previously, Saving wrong rules could lead to EMQX restart failure.

- [#10743](https://github.com/emqx/emqx/pull/10743) Fixed an issue where trying to get bridge info or metrics could result in a crash when a node is joining a cluster.

- [#10755](https://github.com/emqx/emqx/pull/10755) Fixed data bridge resource update race condition.

  In the 'delete + create' process for EMQX resource updates, long bridge creation times could cause dashboard request timeouts. If a bridge resource update was initiated before completion of its creation, it led to an erroneous deletion from the runtime, despite being present in the config file.

  This fix addresses the race condition in bridge resource updates, ensuring the accurate identification and addition of new resources, and maintaining consistency between runtime and configuration file statuses.

- [#10761](https://github.com/emqx/emqx/pull/10761) Fixed the issue where the default value of SSL certificate for Dashboard Listener was not correctly interpolated, which caused HTTPS to be inaccessible when `verify_peer` and `cacertfile` were using the default configuration.

- [#10672](https://github.com/emqx/emqx/pull/10672) Fixed the issue where the lack of a default value for `ssl_options` in listeners results in startup failure. For example, such command(`EMQX_LISTENERS__WSS__DEFAULT__BIND='0.0.0.0:8089' ./bin/emqx console`) would have caused a crash before.

- [#10738](https://github.com/emqx/emqx/pull/10738) TDEngine data bridge now supports "Supertable" and "Create Tables Automatically". Before this fix, an insert with a supertable in the template will fail, like this:

  - `insert into ${clientid} using msg TAGS (${clientid}) values (${ts},${msg})`.

- [#10746](https://github.com/emqx/emqx/pull/10746) Add missing support of the event `$events/delivery_dropped` into the rule engine test API `rule_test`.

- [#10747](https://github.com/emqx/emqx/pull/10747) Ported some time formating fixes in Rule-Engine functions from version 4.4.

- [#10760](https://github.com/emqx/emqx/pull/10760) Fix "internal error 500" when getting bridge statistics page while a node is joining the cluster.

- [#10801](https://github.com/emqx/emqx/pull/10801) Avoid double percent-decode for topic name in API `/topics/{topic}` and `/topics`.

- [#10817](https://github.com/emqx/emqx/pull/10817) Fix a config value handling for bridge resource option `auto_restart_interval`, now it can be set to `infinity`.

## 5.0.3

_Release Date: 2023-05-08_

### Enhancements

- [#10128](https://github.com/emqx/emqx/pull/10128) Add support for OCSP stapling for SSL MQTT listeners.

- [#10156](https://github.com/emqx/emqx/pull/10156) Change the configuration overlay order:

  If it is a new installation of EMQX, `emqx.conf` + Environment variables overlays on top of API Updated Configs (`cluster.hocon`)

  If EMQX is upgraded from an older version (i.e., the `cluster-override.conf` file still exists in EMQX's `data` directory), then it’s the same as before, that is `cluster-override.conf` overlays on top of `emqx.conf` + Environment variables.

  Please note that `data/configs/cluster-override.conf` is considered deprecated. After upgrade, you are encouraged to update `emqx.conf` to delete configs which are overridden by `cluster-override.conf` and move the configs in `cluster-override.conf` to `cluster.hocon`.
  After upgrade, EMQX will continue to read `local-override.conf` (if it exists) as before, but you are encouraged to merge the configs to `emqx.conf`.

- [#10164](https://github.com/emqx/emqx/pull/10164) Add CRL check support for TLS MQTT listeners.

- [#10207](https://github.com/emqx/emqx/pull/10207) Improve OpenAPI (swagger) document readability. Prior to this change, there were a few `summary` docs which are lengthy and lack of translation, now it makes use of the more concise `label` field from schema i18n database instead.

- [#10210](https://github.com/emqx/emqx/pull/10210) Eliminated a few harmless error level logs.
  Prior to this change, there might be some Mnesia callback (hook) failures occasionally occurring when stopping/restarting Mria.
  Now the callbacks (hooks) are unregistered prior to stop. See also [Mria PR](https://github.com/emqx/mria/pull/133).

- [#10224](https://github.com/emqx/emqx/pull/10224) Add the option to customize `clusterIP` in Helm chart, so that a user may set it to a fixed IP.

- [#10263](https://github.com/emqx/emqx/pull/10263) Add command `eval-ex` for Elixir expression evaluation.

- [#10278](https://github.com/emqx/emqx/pull/10278) Refactor the directory structure of all gateways.

- [#10206](https://github.com/emqx/emqx/pull/10206) Support async query mode for all data bridges.

  Prior to this change, setting the query mode of a resource such as a bridge to sync would force the buffer to call the underlying connector in a synchronous way, even if it supports async calls.

- [#10306](https://github.com/emqx/emqx/pull/10306) Add support for async query mode for most bridges.

  This is a follow-up change after [#10206](https://github.com/emqx/emqx/pull/10206). Before this change, some bridges (Cassandra, MongoDB, MySQL, Postgres, Redis, RocketMQ, TDengine) were only allowed to be created with a sync query mode. Now async mode is also supported.

- [#10318](https://github.com/emqx/emqx/pull/10318) Prior to this enhancement, only double quotes (") were allowed in rule engine SQL language's FROM clause. Now it also supports single quotes (').

- [#10336](https://github.com/emqx/emqx/pull/10336) Add `/rule_engine` API endpoint to manage configuration of rule engine.

- [#10354](https://github.com/emqx/emqx/pull/10354) More specific error messages when configure with `bad max_heap_size` value. Log current value and the max value when the `message_queue_too_long` error is thrown.

- [#10358](https://github.com/emqx/emqx/pull/10358) Hide `flapping_detect/conn_congestion/stats` configuration. Deprecate `flapping_detect.enable`.

- [#10359](https://github.com/emqx/emqx/pull/10359) Metrics now are not implicitly collected in places where API handlers don't make any use of them. Instead, a separate backplane RPC gathers cluster-wide metrics.

- [#10373](https://github.com/emqx/emqx/pull/10373) Deprecate the `trace.payload_encode` configuration. Add `payload_encode=[text,hidden,hex]` option when creating a trace via HTTP API.

- [#10381](https://github.com/emqx/emqx/pull/10381) Hide the `auto_subscribe` configuration items so that they can be modified later only through the HTTP API.

- [#10391](https://github.com/emqx/emqx/pull/10391) Hide a large number of advanced options to simplify the configuration file.

  That includes `rewrite`, `topic_metric`, `persistent_session_store`, `overload_protection`,
  `flapping_detect`, `conn_congestion`, `stats,auto_subscribe`, `broker_perf`,
  `shared_subscription_group`, `slow_subs`, `ssl_options.user_lookup_fun` and some advance items
  in `node` and `dashboard` section, [#10358](https://github.com/emqx/emqx/pull/10358),
  [#10381](https://github.com/emqx/emqx/pull/10381), [#10385](https://github.com/emqx/emqx/pull/10385).

- [#10404](https://github.com/emqx/emqx/pull/10404) Change the default queue mode for buffer workers to `memory_only`. Before this change, the default queue mode was `volatile_offload`. When under high message rate pressure and when the resource is not keeping up with such rate, the buffer performance degraded a lot due to the constant disk operations.

- [#10140](https://github.com/emqx/emqx/pull/10140) Integrate Cassandra into bridges as a new backend. At the current stage only support Cassandra version 3.x, not yet 4.x.

- [#10143](https://github.com/emqx/emqx/pull/10143) Add RocketMQ data integration bridge.

- [#10165](https://github.com/emqx/emqx/pull/10165) Support escaped special characters in InfluxDB data bridge `write_syntax`. This update allows to use escaped special characters in string elements in accordance with InfluxDB line protocol.

- [#10211](https://github.com/emqx/emqx/pull/10211) Hide `broker.broker_perf` config and API documents. The two configs `route_lock_type` and `trie_compaction` are rarely used and requires a full cluster restart to take effect. They are not suitable for being exposed to users. Detailed changes can be found here: <https://gist.github.com/zmstone/01ad5754b9beaeaf3f5b86d14d49a0b7/revisions>.

- [#10294](https://github.com/emqx/emqx/pull/10294) When configuring a MongoDB bridge, you can now use the `${field}` syntax to reference fields in the message. This enables you to select the collection to insert data into dynamically.

- [#10363](https://github.com/emqx/emqx/pull/10363) Implement Microsoft SQL Server bridge.

- [#10573](https://github.com/emqx/emqx/pull/10573) Improved performance of Webhook bridge when using synchronous query mode. This also should improve the performance of other bridges when they are configured with no batching.

### Bug Fixes

- [#10145](https://github.com/emqx/emqx/pull/10145) Add field `status_reason` to `GET /bridges/:id` response in case this bridge is in status `disconnected` if internal health-check reports an error condition. Include this same error condition in message when creating an alarm for a failing bridge.

- [#10172](https://github.com/emqx/emqx/pull/10172) Fix the incorrect regular expression in default ACL rule to allow specify username(dashboard) to subscribe `$SYS/#`.

- [#10174](https://github.com/emqx/emqx/pull/10174) Upgrade library `esockd` from 5.9.4 to 5.9.6. Fix an unnecessary error level logging when a connection is closed before proxy protocol header is sent by the proxy.

- [#10195](https://github.com/emqx/emqx/pull/10195) Add labels to API schemas where description contains raw HTML, which would break formatting of generated documentation otherwise.

- [#10196](https://github.com/emqx/emqx/pull/10196) Use lower-case for schema summaries and descriptions to be used in menu of generated online documentation.

- [#10209](https://github.com/emqx/emqx/pull/10209) Fix bug where a last will testament (LWT) message could be published when kicking out a banned client.

- [#10225](https://github.com/emqx/emqx/pull/10225) Allow installing a plugin if its name matches the beginning of another (already installed) plugin name. For example: if plugin `emqx_plugin_template_a` is installed, it must not block installing plugin `emqx_plugin_template`.

- [#10226](https://github.com/emqx/emqx/pull/10226) Handle validation error in `/bridges` API and return `400` instead of `500`.

- [#10242](https://github.com/emqx/emqx/pull/10242) Fixed a log data field name clash. Prior to this fix, some debug logs may report a wrong Erlang PID which may affect troubleshooting session takeover issues.

- [#10257](https://github.com/emqx/emqx/pull/10257) Fixed the issue where `auto_observe` was not working in LwM2M Gateway.

  Before the fix, `OBSERVE` requests were sent without a token, causing failures that LwM2M clients could not handle.

  After the fix, LwM2M Gateway can correctly observe the resource list carried by client, furthermore, unknown resources will be ignored and printing the following warning log:

  ```
  2023-03-28T18:50:27.771123+08:00 [warning] msg: ignore_observer_resource, mfa: emqx_lwm2m_session:observe_object_list/3, line: 522, peername: 127.0.0.1:56830, clientid: testlwm2mclient, object_id: 31024, reason: no_xml_definition
  ```

- [#10286](https://github.com/emqx/emqx/pull/10286) Enhance logging behaviour during boot failure. When EMQX fails to start due to corrupted configuration files, excessive logging is eliminated and no crash dump file is generated.

- [#10297](https://github.com/emqx/emqx/pull/10297) Keeps `eval` command backward compatible with v4 by evaluating only Erlang expressions, even on Elixir node. For Elixir expressions, use `eval-ex` command.

- [#10300](https://github.com/emqx/emqx/pull/10300) Fixed issue with Elixir builds that prevented plugins from being configured via environment variables.

- [#10315](https://github.com/emqx/emqx/pull/10315) Fix crash checking `limit` and `page` parameters in `/mqtt/delayed/messages` API call.

- [#10317](https://github.com/emqx/emqx/pull/10317) Do not expose listener level authentications before extensive verification.

- [#10323](https://github.com/emqx/emqx/pull/10323) For security reasons, the value of the password field in the API examples is replaced with `******`.

- [#10410](https://github.com/emqx/emqx/pull/10410) Fix config check failed when gateways are configured in emqx.conf.
  This issue was first introduced in v5.0.22 via [#10278](https://github.com/emqx/emqx/pull/10278), the boot-time config check was missing.

- [#10533](https://github.com/emqx/emqx/pull/10533) Fixed an issue that could cause (otherwise harmless) noise in the logs.

  During some particularly slow synchronous calls to bridges, some late replies could be sent to connections processes that were no longer expecting a reply, and then emit an error log like:

  ```
  2023-04-19T18:24:35.350233+00:00 [error] msg: unexpected_info, mfa: emqx_channel:handle_info/2, line: 1278, peername: 172.22.0.1:36384, clientid: caribdis_bench_sub_1137967633_4788, info: {#Ref<0.408802983.1941504010.189402>,{ok,200,[{<<"cache-control">>,<<"max-age=0, ...">>}}
  ```

  Those logs are harmless, but they could flood and worry the users without need.

- [#10449](https://github.com/emqx/emqx/pull/10449) Validate the `ssl_options` and `header` configurations when creating authentication http (`authn_http`). Prior to this, incorrect `ssl` configuration could result in successful creation but the entire authn being unusable.

- [#10548](https://github.com/emqx/emqx/pull/10548) Fixed a race condition in the HTTP driver that would result in an error rather than a retry of the request.
  Related fix in the driver: [emqx/ehttpc#45](https://github.com/emqx/ehttpc/pull/45)

- [#10201](https://github.com/emqx/emqx/pull/10201) In TDengine data bridge, removed the redundant database name from the SQL template.

- [#10270](https://github.com/emqx/emqx/pull/10270) ClickHouse data bridge has got a fix that makes the error message better when users click the test button in the settings dialog.

- [#10324](https://github.com/emqx/emqx/pull/10324) Previously, when attempting to reconnect to a misconfigured ClickHouse bridge through the dashboard, users would not receive an error message. This issue is now resolved, and error messages will now be displayed.

- [#10438](https://github.com/emqx/emqx/pull/10438) Fix some configuration item terminology errors in the DynamoDB data bridge:

  - Changed `database` to `table`
  - Changed `username` to `aws_access_key_id`
  - Changed `password` to `aws_secret_access_key`

## 5.0.2

_Release Date: 2023-04-12_

### Enhancements

- [#10022](https://github.com/emqx/emqx/pull/10022) Release installation packages for Rocky Linux 9 (compatible with Red Hat Enterprise Linux 9) and macOS 12 for Intel platform.

- [#10139](https://github.com/emqx/emqx/pull/10139) Add `extraVolumeMounts` to EMQX Helm Chart, you can mount user's own files to EMQX instance, such as ACL rule files mentioned in [#9052](https://github.com/emqx/emqx/issues/9052).

- [#9893](https://github.com/emqx/emqx/pull/9893) When connecting with the flag `clean_start=false`, EMQX will filter out messages that published by clients banned by the blacklist feature in the session.
  Previously, messages sent by clients banned by the blacklist feature could still be delivered to subscribers in this case.

- [#9986](https://github.com/emqx/emqx/pull/9986) Add MQTT ingress to helm charts and remove obsolete mgmt references.

- [#9564](https://github.com/emqx/emqx/pull/9564) Implement Kafka Consumer Bridge, which supports consuming messages from Kafka and publishing them to MQTT topics.

- [#9881](https://github.com/emqx/emqx/pull/9881) Improve error logging related to health checks for InfluxDB connections.

- [#9985](https://github.com/emqx/emqx/pull/9985) Implement ClickHouse Data Bridge

- [#10123](https://github.com/emqx/emqx/pull/10123) Improve the performance of `/bridges` API.
  Earlier, when the number of nodes in the cluster was large or the node was busy, the API may had a request timeout.

- [#9998](https://github.com/emqx/emqx/pull/9998) Obfuscate request body in error log when using HTTP service for client authentication for security reasons.

- [#10026](https://github.com/emqx/emqx/pull/10026) Metrics are now only exposed via the `/bridges/:id/metrics` endpoint, and no longer returned in other API operations.

- [#10052](https://github.com/emqx/emqx/pull/10052) Improve startup failure logs in daemon mode.

### Bug Fixes

- [#10013](https://github.com/emqx/emqx/pull/10013) Fix return type structure for error case in API schema for `/gateways/:name/clients`.

- [#10014](https://github.com/emqx/emqx/pull/10014) Ensure Monitor API `/monitor(_current)/nodes/:node` returns `404` instead of `400` if node does not exist.

- [#10027](https://github.com/emqx/emqx/pull/10027) Allow setting node name via environment variable `EMQX_NODE__NAME` in Docker.

- [#10050](https://github.com/emqx/emqx/pull/10050) Ensure Bridge API returns `404` status code consistently for resources that don't exist.

- [#10055](https://github.com/emqx/emqx/pull/10055) The configuration parameter `mqtt.max_awaiting_rel` was not functional and has now been corrected.

- [#10056](https://github.com/emqx/emqx/pull/10056) Fix `/bridges` API status code.
  Return `400` instead of `403` in case of removing a data bridge that is dependent on an active rule.
  Return `400` instead of `403` in case of calling operations (start|stop|restart) when Data-Bridging is not enabled.

- [#10066](https://github.com/emqx/emqx/pull/10066) Improve error messages for `/briges_probe` and `[/node/:node]/bridges/:id/:operation` API calls to make them more readable. And set HTTP status code to `400` instead of `500`.

- [#10074](https://github.com/emqx/emqx/pull/10074) Check if type in `PUT /authorization/sources/:type` matches `type` given in the request body.

- [#10079](https://github.com/emqx/emqx/pull/10079) Fix wrong description about `shared_subscription_strategy`.

- [#10085](https://github.com/emqx/emqx/pull/10085) Consistently return `404` for all requests on non-existent source in `/authorization/sources/:source[/*]`.

- [#10098](https://github.com/emqx/emqx/pull/10098) Fix an issue where the MongoDB connector crashed when MongoDB authorization was configured.

- [#10100](https://github.com/emqx/emqx/pull/10100) Fix channel crash for slow clients with enhanced authentication.  
  Previously, when the client was using enhanced authentication, but the Auth message was sent slowly or the Auth message was lost, the client process would crash.

- [#10107](https://github.com/emqx/emqx/pull/10107) For operations on Bridges API if `bridge-id` is unknown we now return `404` instead of `400`.

- [#10117](https://github.com/emqx/emqx/pull/10117) Fix an error occurring when a joining node doesn't have plugins that are installed on other nodes in the cluster.
  After this fix, the joining node will copy all the necessary plugins from other nodes.

- [#10118](https://github.com/emqx/emqx/pull/10118) Fix problems related to manual joining of EMQX replicant nodes to the cluster.

- [#10119](https://github.com/emqx/emqx/pull/10119) Fix crash when `statsd.server` is set to an empty string.

- [#10124](https://github.com/emqx/emqx/pull/10124) The default heartbeat period for MongoDB has been increased to reduce the risk of too excessive logging to the MongoDB log file.

- [#10130](https://github.com/emqx/emqx/pull/10130) Fix garbled config display in dashboard when the value is originally from environment variables.

- [#10132](https://github.com/emqx/emqx/pull/10132) Fix some error logs generated by `systemctl stop emqx` command.  
  Prior to the fix, the command was not stopping `jq` and `os_mon` applications properly.

- [#10144](https://github.com/emqx/emqx/pull/10144) Fix an issue where emqx cli failed to set the Erlang cookie when the emqx directory was read-only.

- [#10154](https://github.com/emqx/emqx/pull/10154) Change the default `resume_interval` for bridges and connectors to be the minimum of `health_check_interval` and `request_timeout / 3` to resolve issue of request timeout.

- [#10157](https://github.com/emqx/emqx/pull/10157) Fix default rate limit configuration not being applied correctly when creating a new listener.

- [#10237](https://github.com/emqx/emqx/pull/10237) Ensure we return `404` status code for unknown node names in `/nodes/:node[/metrics|/stats]` API.

- [#10251](https://github.com/emqx/emqx/pull/10251) Fix an issue where rule dependencies were not prompted when deleting an ingress-type bridge in use.

- [#10313](https://github.com/emqx/emqx/pull/10313) Ensure that when the core or replicant node starting, the `cluster-override.conf` file is only copied from the core node.

- [#10327](https://github.com/emqx/emqx/pull/10327) Don't increase “actions.failed.unknown” rule metrics counter upon receiving unrecoverable data bridge errors.

- [#10095](https://github.com/emqx/emqx/pull/10095) Fix an issue where when the MySQL connector was in batch mode, clients would keep querying the server with unnecessary `PREPARE` statements on each batch, possibly causing server resource exhaustion.
  Footer

## 5.0.1

_Release Date: 2023-03-10_

### Enhancements

- [#10019](https://github.com/emqx/emqx/pull/10019) Add low-level tuning settings for QUIC listeners.
- [#10059](https://github.com/emqx/emqx/pull/10059) Errors returned by rule engine API are formatted in a more human-readable way rather than dumping the raw error including the stack trace.
- [#9213](https://github.com/emqx/emqx/pull/9213) Add pod disruption budget to helm chart
- [#9949](https://github.com/emqx/emqx/pull/9949) QUIC transport Multistreams support and QUIC TLS ca-cert support.
- [#9932](https://github.com/emqx/emqx/pull/9932) Integrate `TDengine` into `bridges` as a new backend.
- [#9967](https://github.com/emqx/emqx/pull/9967) New common TLS option 'hibernate_after' to reduce memory footprint per idle connection, default: 5s.

### Bug Fixes

- [#10009](https://github.com/emqx/emqx/pull/10009) Validate `bytes` param to `GET /trace/:name/log` to not exceed signed 32bit integer.

- [#10015](https://github.com/emqx/emqx/pull/10015) To prevent errors caused by an incorrect EMQX node cookie provided from an environment variable,
  we have implemented a fail-fast mechanism.
  Previously, when an incorrect cookie was provided, the command would still attempt to ping the node,
  leading to the error message 'Node xxx not responding to pings'.
  With the new implementation, if a mismatched cookie is detected,
  a message will be logged to indicate that the cookie is incorrect,
  and the command will terminate with an error code of 1 without trying to ping the node.

- [#10020](https://github.com/emqx/emqx/pull/10020) Fix bridge metrics when running in async mode with batching enabled (`batch_size` > 1).

- [#10021](https://github.com/emqx/emqx/pull/10021) Fix error message when the target node of `emqx_ctl cluster join` command is not running.

- [#10032](https://github.com/emqx/emqx/pull/10032) When resources on some nodes in the cluster are still in the 'initializing/connecting' state, the `bridges/` API will crash due to missing Metrics information for those resources. This fix will ignore resources that do not have Metrics information.

- [#10041](https://github.com/emqx/emqx/pull/10041) For InfluxDB bridge, added integer value placeholder annotation hint to `write_syntax` documentation.
  Also supported setting a constant value for the `timestamp` field.

- [#10042](https://github.com/emqx/emqx/pull/10042) Improve behavior of the `replicant` nodes when the `core` cluster becomes partitioned (for example when a core node leaves the cluster).
  Previously, the replicant nodes were unable to rebalance connections to the core nodes, until the core cluster became whole again.
  This was indicated by the error messages: `[error] line: 182, mfa: mria_lb:list_core_nodes/1, msg: mria_lb_core_discovery divergent cluster`.

- [#10054](https://github.com/emqx/emqx/pull/10054) Fix the problem that the obfuscated password is used when using the `/bridges_probe` API to test the connection in Data-Bridge.

- [#10058](https://github.com/emqx/emqx/pull/10058) Deprecate unused QUIC TLS options.
  Only following TLS options are kept for the QUIC listeners:

  - cacertfile
  - certfile
  - keyfile
  - verify

- [#10076](https://github.com/emqx/emqx/pull/10076) Fix webhook bridge error handling: connection timeout should be a retriable error.
  Prior to this fix, connection timeout was classified as unrecoverable error and led to the request being dropped.

- [#10078](https://github.com/emqx/emqx/pull/10078) Fix an issue that invalid QUIC listener setting could cause a segfault.

- [#10084](https://github.com/emqx/emqx/pull/10084) Fix the problem when joining core nodes running different EMQX versions into a cluster.

  [Mria PR](https://github.com/emqx/mria/pull/127)

- [#10086](https://github.com/emqx/emqx/pull/10086) Upgrade HTTP client ehttpc to `0.4.7`.
  Prior to this upgrade, HTTP clients for authentication, authorization, and webhook may crash
  if `body` is empty but content-type HTTP header is set.
  For more details see [ehttpc PR#44](https://github.com/emqx/ehttpc/pull/44).

- [#9939](https://github.com/emqx/emqx/pull/9939) Allow 'emqx ctl cluster' command to be issued before Mnesia starts.
  Prior to this change, EMQX `replicant` could not use `manual` discovery strategy.
  Now it's possible to join cluster using 'manual' strategy.

- [#9958](https://github.com/emqx/emqx/pull/9958) Fix the error code and error message returned by the `clients` API when the Client ID does not exist.

- [#9961](https://github.com/emqx/emqx/pull/9961) Avoid parsing config files for node name and cookie when executing non-boot commands in bin/emqx.

- [#9974](https://github.com/emqx/emqx/pull/9974) Report memory usage to statsd and Prometheus using the same data source as dashboard.
  Prior to this fix, the memory usage data source was collected from an outdated source which did not work well in containers.

- [#9997](https://github.com/emqx/emqx/pull/9997) Fix Swagger API schema generation. `deprecated` metadata field is now always boolean, as [Swagger specification](https://swagger.io/specification/) suggests.

- [#10007](https://github.com/emqx/emqx/pull/10007) Change Kafka bridge's config `memory_overload_protection` default value from `true` to `false`.
  EMQX logs case when messages get dropped due to overload protection, and this is also reflected in counters.
  However, since there is by default no alerting based on the logs and counters,
  setting it to `true` may cause messages being dropped without notice.
  At the time being, the better option is to let sysadmin set it explicitly so they are fully aware of the benefits and risks.

- [#10087](https://github.com/emqx/emqx/pull/10087) Use default template `${timestamp}` if the `timestamp` config is empty (undefined) when inserting data in InfluxDB.
  Prior to this change, InfluxDB bridge inserted a wrong timestamp when template is not provided.

## 5.0.0

_Release Date: 2023-02-03_

EMQX Enterprise 5.0 is a completely new release. In this version, we have implemented a new cluster architecture and refined the main features, as well as introduced many new features.

### **New Core + Replica clustering architecture**

EMQX Enterprise 5.0 adopts a brand-new Core + Replica clustering architecture, offering better scalability and reliability.

- A single cluster can now support up to 23 nodes and 100+ million MQTT connections, a [10x increase](https://www.emqx.com/en/blog/how-emqx-5-0-achieves-100-million-mqtt-connections) compared with EMQX Enterprise 4.x.
- The stateless nature of Replicant nodes ensures a stable cluster performance during dynamic scaling.
- Reduce the risk of split-brain under large-scale deployment and minimize the impact of split-brain on business.

The [EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator) has been adapted for this new clustering architecture so that you can deploy a scalable and stateless MQTT service with EMQX effortlessly.

### Support MQTT over QUIC

By adopting QUIC as a transport layer, EMQX Enterprise 5.0 now supports the MQTT over QUIC transmission protocol.

MQTT over QUIC (Quick UDP Internet Connections) is a new transport protocol that aims to provide low-latency, reliable, and secure data communication in IoT (Internet of Things) networks. As demonstrated from some performance tests, MQTT over QUIC can overcome some of the challenges of traditional networking protocols, such as slow and unreliable data transmission, high latency, and security vulnerabilities. Therefore, it is said that MQTT over QUIC has the potential to revolutionize the way how data is collected in the Internet of Vehicles (IoV) and mobile data acquisition scenarios.

With EMQX Enterprise 5.0, users can now create an MQTT over QUIC listener and use the EMQ SDK to connect to IoT devices. EMQ is submitting a draft to the MQTT protocol as a member of OASIS.

For more information, please refer to [MQTT over QUIC: Next-Generation IoT Standard Protocol](https://www.emqx.com/en/blog/getting-started-with-mqtt-over-quic-from-scratch).

### Visualized **rules orchestration and bidirectional data integration**

EMQX Enterprise 5.0 offers real-time processing capability for IoT data and supports integration with third-party data systems in a more flexible and low-code way.

**Visualized orchestration rules with Flows editor**

EMQX Enterprise 5.0 has added a visualized Flows page in EMQX Dashboard to facilitate the management of rules. In the Flows editor, users can now easily view and monitor the data filtering, processing, and bridging flow. With this Flow editor to visualize the connection between the IoT hardware and data flow, developers can now focus on works with business significance.

EMQX plans to support rules orchestration and data bridge creation by drag and drop in future releases.

**More flexible bidirectional data integration**

Besides bridging device data to external systems, EMQX Enterprise 5.0 also supports bridging data from external data systems to specific clients after rule processing, for example, other MQTT services and Kafka.

Bidirectional data integration is suitable for sending messages from the cloud to the device, with our support for real-time processing and delivering under large-scale messages, EMQX 5.0 offers more possibilities for IoT service application scenarios.

**Disk-based buffer queue**

EMQX Enterprise 5.0 also provides a buffer queue feature to better support our data bridging services. With this buffer queue feature, messages generated under abnormal connections can be cached for the moment and continue to be sent after the connection is resumed.

This buffer queue feature helps to ensure excellent reliability for data integration and greatly improves business availability.

**Supported data integrations**

EMQX Enterprise 5.0 currently supports integration with the following data systems:

1. Webhook
2. MQTT
3. Kafka
4. InfluxDB
5. MySQL
6. Redis
7. GCP Pub/Sub
8. MongoDB

We plan to add support to more data systems, please stay tuned.

### **Improved security management**

**More flexible access control**

EMQX Enterprise 5.0 provides authentication options such as password-based authentication, LDAP, JWT, PSK, and X.509 certificates. It also provides authorization checking for message publishing and subscriptions.

Besides the configuration files, users can also configure their access control with EMQX Dashboard, a more flexible and user-friendly method. With this Dashboard configuration option, you can enable access control for EMQX clusters without rebooting.

EMQX also offers statistical metrics at both cluster and node levels to help our users better monitor access control running status, including:

- Allow: Number of authentication/authorization passed
- Deny: Number of authentication/authorization failed
- No match: Number of client authentication/authorization data not found
- Rate: Rate of request

**Overload protection with Limiter**

EMQX introduces the overload protection mechanism and a new Limiter feature. This Limiter delivers a more accurate and layered rate control and ensures that the system operates under the expected workload, because it supports limiting client behavior at the client, listener, or node levels.

The combination of these 2 features prevents the clients from becoming too busy or receiving excessive request traffic and ensures stable system operation.

### User-friendly EMQX Dashboard with better observability

In EMQX Enterprise 5.0, we have redesigned the EMQX Dashboard with a new UI design style, enhancing the visual experience and supporting more powerful and user-friendly features. Users can manage client connections, authenticate/authorize various subscribe/publish requests, and integrate with different data systems via data bridges and rule engine with our brand-new EMQX Dashboard.

**The main improvements are as follows：**

- Access control management
- Introduce the Flows editor to visualize the data integration
- More powerful hot configuration
- More diagnostic tools, such as slow subscriptions and log trace
- Powerful data managing capability: users can manage retained messages or postpone the publishing schedules with Dashboard

### More flexible extensions

With EMQX Enterprise 5.0, users can now compile, distribute, and install extension plugins with standalone plugin packages. You can upload these packages via the Dashboard to finish the configuration with no need to reboot the EMQX cluster.

A standard plugin will come with complete documentation and a website URL, so users can easily follow the instructions to use the plugins and communicate with the developers.

**Easy-to-use ExHook/gRPC**

Users can create multiple ExHooks at the same time. With the relevant metrics, users can view the detailed usage statics and hooks (and their arguments) registered under each Exhook, so they can better understand the load of the ExHook extensions.

**More "native" multi-protocol connectivity**

The gateway is re-implemented in a unified design layer. EMQX Enterprise 5.0 provides unique client management pages and security authentication configurations for each protocol feature, so users can manage the access in a more protocol-native manner.

As each gateway can be configured with its authentication methods, the authentication credentials of different gateway devices can now be isolated from each other to meet advanced security requirements.
