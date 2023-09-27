# Releases

## e5.3.0

### Enhancements

- [#11568](https://github.com/emqx/emqx/pull/11568) Added support for defining templates for MQTT publish properties in Republish rule action.

- [#11597](https://github.com/emqx/emqx/pull/11597) - Upgrade ekka to 0.15.13, which includes the following changes:
  - upgrade Mria to 0.6.2:
  - make bootstrap batch size configurable, [Mria PR](https://github.com/emqx/mria/pull/159)
  - improve robustness of mria_membership processes, [Mria PR](https://github.com/emqx/mria/pull/156)
  - fix log message formatting error
  - Add `node.default_bootstrap_batch_size` option to EMQX configuration.
  Increasing the value of this  option can greatly reduce a replicant node startup time, especially when EMQX cluster interconnect network latency is high and EMQX built-in database holds large amount of data, e.g. when the number of subscriptions is high.

- [#11620](https://github.com/emqx/emqx/pull/11620) Add a new rule-engine SQL function `bytesize` to get the size of a byte-string. e.g. ``SELECT * FROM "t/#" WHERE bytesize(payload) > 10`

- [#11642](https://github.com/emqx/emqx/pull/11642) Updated to quicer version 0.0.200 in preparation for enabling openssl3 support for QUIC transport.

- [#11608](https://github.com/emqx/emqx/pull/11608) Integrated the LDAP bind operation as a new authenticator.

- [#11610](https://github.com/emqx/emqx/pull/11610) Implemented a preliminary Role-Based Access Control for the Dashboard.

  In this version, there are two predefined roles:
  - superuser

  This role could access all resources.
  - viewer

  This role can only view resources and data, corresponding to all GET requests in the REST API.

- [#11612](https://github.com/emqx/emqx/pull/11612) During node evacuation, evacuate all disconnected sessions, not only those started with `clean_start` set to `false`.



### Bug Fixes

- [#11682](https://github.com/emqx/emqx/pull/11682) Fixed an issue where logging would stop if "Rotation Size" would be set to `infinity` on file log handlers.

- [#11567](https://github.com/emqx/emqx/pull/11567) Improve EMQX graceful shutdown (`emqx stop` command):
  - increase timeout from 1 to 2 minutes
  - print an error message if EMQX can't stop gracefully within the configured timeout
  - print periodic status messages while EMQX is shutting down

- [#11568](https://github.com/emqx/emqx/pull/11568) Fixed an issue where an ill-defined builtin rule action config could be interpreted as a custom user function.

- [#11584](https://github.com/emqx/emqx/pull/11584) Fixed telemetry reporting error on Windows when os_mon module is unavailable.

- [#11605](https://github.com/emqx/emqx/pull/11605) Lower CMD_overridden log severity from warning to info.

- [#11622](https://github.com/emqx/emqx/pull/11622) Upgrade rpc library gen_rpc from 2.8.1 to 3.1.0

- [#11623](https://github.com/emqx/emqx/pull/11623) Upgrade library `esockd` from 5.9.6 to 5.9.7

  This upgrade included
  * Enhancements regarding proxy protocol error and timeout. [esockd pr#178](https://github.com/emqx/esockd/pull/178)
  * Lowered `ssl_error` exceptions to info level logging. [esockd pr#180](https://github.com/emqx/esockd/pull/180)
  * Malformed MQTT packet parsing exception log level is lowered from `error` to `info`.
  * In command `emqx ctl listeners` output, the `shutdown_count` counter is incremented
  when TLS handshake failure (`ssl_error`) or Malformed packet (`frame_error`) happens.

- [#11661](https://github.com/emqx/emqx/pull/11661) Fix log formatter when log.HANDLER.formatter is set to 'json'.

  The bug was introduced in v5.0.4 where the log line was no longer a valid JSON, but prefixed with timestamp string and level name.

- [#11627](https://github.com/emqx/emqx/pull/11627) Fix resources cleanup in HStreamdB bridge.

  Prior to this fix, HStreamDB bridge might report errors during bridge configuration updates, since hstreamdb client/producer were not stopped properly.ha
