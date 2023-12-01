# Releases

## v5.3.2

## Enhancements

- [#11725](https://github.com/emqx/emqx/pull/11725) Introduced the LDAP as a new authentication and authorization backend.

- [#11752](https://github.com/emqx/emqx/pull/11752) Changed default RPC driver from `gen_rpc` to `rpc` for core-replica database synchronization.

  This improves core-replica data replication latency.

- [#11785](https://github.com/emqx/emqx/pull/11785) Allowed users with the "Viewer" role to change their own passwords. However, those with the "Viewer" role do not have permission to change the passwords of other users.

- [#11787](https://github.com/emqx/emqx/pull/11787) Improved the performance of the `emqx` command.

- [#11790](https://github.com/emqx/emqx/pull/11790) Added validation to Redis commands in Redis authorization source. Additionally, this improvement refines the parsing of Redis commands during authentication and authorization processes. The parsing now aligns with `redis-cli` compatibility standards and supports quoted arguments.

## Bug Fixes

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

## v5.3.1

### Enhancements

- [#11637](https://github.com/emqx/emqx/pull/11637) Added extra diagnostic checks to help debug issues when mnesia is stuck waiting for tables. Library Updates: `ekka` has been upgraded to version 0.15.15, and `mria` to version 0.6.4.

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


- [#11733](https://github.com/emqx/emqx/pull/11733) Resolved an incompatibility issue that caused crashes during session takeover or channel eviction when the session was located on a remote node running EMQX v5.2.x or an earlier version.
- [#11750](https://github.com/emqx/emqx/pull/11750) Eliminated logging and tracing of HTTP request bodies in HTTP authentification and HTTP bridges.
- [#11886](https://github.com/emqx/emqx/pull/11886) Fixed backward plugin compatibility. Currently, EMQX validates hook point names, and invalid hook points cannot be used for hook registration. However, some older versions of plugin templates used misspelled hook points, and actual plugins in use may also have this issue. To maintain compatibility with these older plugins, we allow the use of the old hook points for hook registration, but we issue deprecated warnings for them. As before, these hooks will not be called.
- [#11897](https://github.com/emqx/emqx/pull/11897) Fixed the issue of waiting for a loop race condition during node configuration synchronization when cluster nodes are started approximately at the same time.


## v5.3.0

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
- [#11667](https://github.com/emqx/emqx/pull/11667) Disabled access to the `logout` endpoint by the API key. This endpoint is for the Dashboard only.