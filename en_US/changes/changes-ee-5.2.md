# Releases

## e5.2.1

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

- [#11522](https://github.com/emqx/emqx/pull/11522) Improved error message for rule engine schema registry when schema name exceeds the permissible length.

- [#11531](https://github.com/emqx/emqx/pull/11531) Fixed an issue where authorization cache cleaning cli was not working properly for specific client ID.

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

## e5.2.0

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
