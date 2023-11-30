# Releases

## e5.3.2

## Enhancements

- [#11752](https://github.com/emqx/emqx/pull/11752) Changed default RPC driver from `gen_rpc` to `rpc` for core-replica database synchronization.

  This improves core-replica data replication latency.

- [#11785](https://github.com/emqx/emqx/pull/11785) Allowed users with the "Viewer" role to change their own passwords. However, those with the "Viewer" role do not have permission to change the passwords of other users.
- [#11787](https://github.com/emqx/emqx/pull/11787) Improved the performance of the `emqx` command.

- [#11790](https://github.com/emqx/emqx/pull/11790) Added validation of Redis commands configured in Redis authorization source.
  Additionally, this improvement refines the parsing of Redis commands during authentication and authorization processes.  The parsing now aligns with `redis-cli` compatibility standards and supports quoted arguments.
  
- [#11541](https://github.com/emqx/emqx/pull/11541) Enhances file transfer capabilities in EMQX by introducing an additional interaction method. Now clients can send file transfer commands to `$file-async/...` topic instead of `$file/...`. Command execution results are delivered as messages to the `$file-response/{clientId}` topic. 
  This enhancement simplifies file transfer feature usage in certain cases, for example, when a client uses MQTTv3 or when the broker is behind an MQTT bridge.
  See the [EIP-0021](https://github.com/emqx/eip) for more details.

## Bug Fixes

- [#11757](https://github.com/emqx/emqx/pull/11757) Fixed the error response code when downloading non-existent trace files. Now the response returns `404` instead of `500`.

- [#11762](https://github.com/emqx/emqx/pull/11762) Fixed an issue in the `built_in_database` authorization source of EMQX. With this fix, all Access Control List (ACL) records are now thoroughly removed upon the destruction of an authorization source. This addresses the previous concern where residual records remained in the database, potentially leading to complications when re-creating authorization sources.

- [#11771](https://github.com/emqx/emqx/pull/11771) Fixed validation of Bcrypt salt rounds in authentication management through the API/Dashboard.

- [#11780](https://github.com/emqx/emqx/pull/11780) Fixed validation of the `iterations` field of the `pbkdf2` password hashing algorithm. Now, `iterations` must be strictly positive. Previously, it could be set to 0, which led to a nonfunctional authenticator.

- [#11791](https://github.com/emqx/emqx/pull/11791) Fixed an issue in the EMQX CoAP Gateway where heartbeats were not effectively maintaining the connection's active status. This fix ensures that the heartbeat mechanism properly sustains the liveliness of CoAP Gateway connections.

- [#11797](https://github.com/emqx/emqx/pull/11797) Modified HTTP API behavior for APIs managing the `built_in_database` authorization source. They will now return a `404` status code if `built_in_database` is not set as the authorization source, replacing the former `20X` response.

- [#11965](https://github.com/emqx/emqx/pull/11965) Improved the termination of EMQX services to ensure a graceful stop even in the presence of an unavailable MongoDB resource. 

- [#11975](https://github.com/emqx/emqx/pull/11975) This fix addresses an issue where redundant error logs were generated due to a race condition during simultaneous socket closure by a peer and the server. Previously, concurrent socket close events triggered by the operating system and EMQX resulted in unnecessary error logging. The implemented fix improves event handling to eliminate unnecessary error messages.

- [#11987](https://github.com/emqx/emqx/pull/11987) Fixed a bug where attempting to set the `active_n` option on a TCP/SSL socket could lead to a connection crash. 

  The problem occurred if the socket had already been closed by the time the connection process attempted to apply the `active_n` setting, resulting in a `case_clause` crash.

- [#11731](https://github.com/emqx/emqx/pull/11731) Added file_transfer feature configs to hot-config schema.

- [#11754](https://github.com/emqx/emqx/pull/11754) Improved the log formatting specifically for the Postgres bridge in EMQX. It addresses issues related to Unicode characters in error messages returned by the driver.


## e5.3.1

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

## e5.3.0

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
