# Releases

## v5.2.0

### Enhancements

- [#11469](https://github.com/emqx/emqx/pull/11469) Added support for specifying username in Redis authentication.
- [#11487](https://github.com/emqx/emqx/pull/11487) The bcrypt work factor is limited to the range 5-10, because higher values consume too much CPU resources.
  Bcrypt library is updated to allow parallel hash evaluation.
- [#11496](https://github.com/emqx/emqx/pull/11496) Disabled the Erlang VM Prometheus exporter by default to improve performance and security.
- [#11497](https://github.com/emqx/emqx/pull/11497) Enhanced broker metrics collection and export by adding new metrics for messages, overload protection, authorization, and authentication, and also by improving naming consistency for OpenTelemetry.
- [#11490](https://github.com/emqx/emqx/pull/11490) Added fast error handling for undefined passwords in various authentication backends. This improves the consistency and user-friendliness of the authentication process.
- [#11532](https://github.com/emqx/emqx/pull/11532) Improved error messaging for better clarity when parsing invalid packets.
- [#11568](https://github.com/emqx/emqx/pull/11568) Added support for defining templates for MQTT 5.0 publish properties and user properties in Republish rule action.

### Bug Fixes

- [#11466](https://github.com/emqx/emqx/pull/11466) Fixed a crash that occurred when setting the `ssl_options.ciphers` configuration option to an empty string ("").

- [#11480](https://github.com/emqx/emqx/pull/11480) Improved the error handling and testing of SQL functions in the rule engine when rule functions received bad arguments.

- [#11493](https://github.com/emqx/emqx/pull/11493) Fixed response examples for `/api/v5/publish` bad request in RESP API documentation. Previously the documentation example said that the bad request response could return a list in the body which was not actually the case.

- [#11506](https://github.com/emqx/emqx/pull/11506) Previously, attempting to download a non-existent trace log file would result in downloading an empty file. After implementing this fix, when attempting to download an empty trace log file using the GET request `/api/v5/trace/clientempty/download`, the server will now respond with a 404 status code and the following JSON message: `{"code":"NOT_FOUND","message":"Trace is empty"}`. This response will be triggered if no events matching the trace condition are found in the log file. 

- [#11520](https://github.com/emqx/emqx/pull/11520) Fixed an issue where `packets_connack_sent` metric was not incremented on CONNACK packets sent with non-zero `ack_flag`.

- [#11522](https://github.com/emqx/emqx/pull/11522) Improved rule engine schema registry error message when schema name exceeds the permissible length.

- [#11523](https://github.com/emqx/emqx/pull/11523) Corrected a misleading prompt when specifying invalid certificates/keys for the `/configs` API.

- [#11531](https://github.com/emqx/emqx/pull/11531) Fixed an issue where authorization cache cleaning CLI was not working properly for specific client ID.

- [#11534](https://github.com/emqx/emqx/pull/11534) Fixed the increment on data bridge statistics when the bridge is unhealthy. Now, messages sent to unhealthy bridges are counted as dropped messages.

- [#11540](https://github.com/emqx/emqx/pull/11540) Improved HTTP response when attempting to create a bridge with an invalid name.

- [#11548](https://github.com/emqx/emqx/pull/11548) Fixed an issue that prevented the plugin order from being updated across the entire cluster.

- [#11564](https://github.com/emqx/emqx/pull/11564) Fixed cluster partition autoheal functionality. Implemented autohealing for the clusters that split into multiple partitions.

- [#11568](https://github.com/emqx/emqx/pull/11568) Fixed an issue where an ill-defined built-in rule action config could be interpreted as a custom user function.

- [#11586](https://github.com/emqx/emqx/pull/11586) Fixed an issue that could lead to crashes during session takeover when rolling upgrading from a previous version to `v5.2.0`.

  

