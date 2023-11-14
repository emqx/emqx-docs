# Releases

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

  Currently, EMQX validates hookpoint names, so invalid hookspoints cannot be used for registering hooks. However, older versions of plugin templates used some misspelled hookpoints, and so could the real plugins. We allow the old hookpoints to be used for registering hooks, but issue a warning that they are deprecated. As before, these hooks are never called.

- [#11897](https://github.com/emqx/emqx/pull/11897) Fix config sync wait-loop race condition when cluster nodes boot around the same time.

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
