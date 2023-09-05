# Releases

## e5.2.0

### Enhancements

- [#10697](https://github.com/emqx/emqx/pull/10697) This change allows to set the minReadySeconds for the StatefulSet. This allows to add a gap between the restarts of each pod by upgrade or restart command.

- [#11124](https://github.com/emqx/emqx/pull/11124) Release packages for Amazon Linux 2023

- [#11289](https://github.com/emqx/emqx/pull/11289) Release packages for Debian 12.

- [#11290](https://github.com/emqx/emqx/pull/11290) Updated `jq` dependency to version 0.3.10 which includes `oniguruma` library update to version 6.9.8 with few minor security fixes.

- [#11291](https://github.com/emqx/emqx/pull/11291) Updated RocksDB version to 1.8.0-emqx-1 via ekka update to 0.15.6.

- [#11390](https://github.com/emqx/emqx/pull/11390) Added `node.broker_pool_size`, `node.generic_pool_size`, `node.channel_cleanup_batch_size` options to EMQX configuration.

  Tuning these options can significantly improve performance if cluster interconnect network latency is high.

- [#11429](https://github.com/emqx/emqx/pull/11429) Added option to configure detection of legacy protocol in MondoDB connectors and bridges.

- [#11436](https://github.com/emqx/emqx/pull/11436) Added a new API endpoint `DELETE  /banned` to clear all `banned` data.

- [#11438](https://github.com/emqx/emqx/pull/11438) Changed the type of the `mqtt.max_packet_size` from string to byteSize to better represent the valid numeric range.
  Strings will still be accepted for backwards compatibility.

- [#11446](https://github.com/emqx/emqx/pull/11446) Refactored datetime-related modules and functions to simplify the code.

- [#11469](https://github.com/emqx/emqx/pull/11469) Added support for specifying username in Redis authentication.

- [#11496](https://github.com/emqx/emqx/pull/11496) Disabled the Erlang VM Prometheus exporter by default to improve performance and security.

- [#11497](https://github.com/emqx/emqx/pull/11497) Enhanced broker metrics collection and export by adding new metrics for messages, overload protection, authorization, authentication,
  and improving naming consistency for OpenTelemetry.

- [#10647](https://github.com/emqx/emqx/pull/10647) Added enterprise data bridge for [GreptimeDB](https://github.com/GreptimeTeam/greptimedb).

- [#11261](https://github.com/emqx/emqx/pull/11261) Implemented Amazon Kinesis Data Streams producer data integration bridge .

- [#11329](https://github.com/emqx/emqx/pull/11329) Implemented Azure Event Hub Producer data integration bridge.

- [#11363](https://github.com/emqx/emqx/pull/11363) Added TLS connection support to RabbitMQ bridge.

- [#11367](https://github.com/emqx/emqx/pull/11367) Ported GCP IoT Hub authentication support from EMQX 4.4.

- [#11386](https://github.com/emqx/emqx/pull/11386) Integrated LDAP as a new authenticator.

- [#11392](https://github.com/emqx/emqx/pull/11392) Integrated LDAP as an authorization source.

- [#11402](https://github.com/emqx/emqx/pull/11402) Added support for using placeholders to define MQTT Topic in Kafka Consumer bridge topic mappings.

- [#11403](https://github.com/emqx/emqx/pull/11403) Added support for defining message attributes and ordering key templates for GCP PubSub Producer bridge.

  Also updated our HOCON library to fix an issue where objects in an array were being concatenated even if they lay on different lines.

- [#11459](https://github.com/emqx/emqx/pull/11459) Added the option to configure health check interval for Kafka bridges.

- [#11478](https://github.com/emqx/emqx/pull/11478) Added HStreamDB bridge support (both TCP and TLS connection allowed), adapted to the HStreamDB `v0.16.1`.

  Updated driver to `0.4.5+v0.16.1` in [PR#11530](https://github.com/emqx/emqx/pull/11530).

- [#11389](https://github.com/emqx/emqx/pull/11389) Improved retained message publishing latency by consolidating multiple index update operations into a single mnesia activity, leveraging the new APIs introduced in mria 0.6.0.

- [#11396](https://github.com/emqx/emqx/pull/11396) Introduced topic index for the rule engine runtime that significantly improves the performance of EMQX with a non-trivial number of rules consuming messages matching different topic filters.

- [#11399](https://github.com/emqx/emqx/pull/11399) Improved the placeholder syntax of rule engine.

  The parameters of actions support placeholder syntax to
  dynamically fill in the content of strings. The format of the
  placeholder syntax is `${key}`.
  Before this improvement, the `key` in `${key}` could only contain
  letters, numbers, and underscores. Now the `key` supports any UTF8
  characters.

- [#11405](https://github.com/emqx/emqx/pull/11405) Improved the error reason of the `date_to_unix_ts` to make more sense.

- [#11490](https://github.com/emqx/emqx/pull/11490) Return error faster when the password is absent in password-based authentication.

### Bug Fixes

- [#11065](https://github.com/emqx/emqx/pull/11065) Avoid logging irrelevant error messages during EMQX shutdown.

- [#11279](https://github.com/emqx/emqx/pull/11279) Fixed issue when client cannot send large payloads with debug/trace logging enabled in emqx.

- [#11296](https://github.com/emqx/emqx/pull/11296) Support for importing additional configurations from EMQX backup file (`emqx ctl import` command):
  - rule_engine (previously not imported due to the bug)
  - topic_metrics (previously not implemented)
  - slow_subs (previously not implemented).

- [#11327](https://github.com/emqx/emqx/pull/11327) Updated ekka to version 0.15.8, mria to version 0.15.8, and optvar to 1.0.5.
  This fixes occasional assertion failures:
  `{{badmatch,noproc},[{optvar,read,2,[{file,"optvar.erl"},{line,140}]},{optvar,read,1,[{file,"optvar.erl"},{line,124}]},...`

- [#11346](https://github.com/emqx/emqx/pull/11346) Updated ekka to version 0.15.9.
  This fixes dangling etcd locks occurred if acquiring the lock failed with a timeout.

- [#11347](https://github.com/emqx/emqx/pull/11347) Ensure that OCSP request path is properly URL encoded.

- [#11352](https://github.com/emqx/emqx/pull/11352) Fixed [crash issue](https://github.com/emqx/emqx/issues/11345) when starting on Windows or any other platform without RocksDB support.

- [#11372](https://github.com/emqx/emqx/pull/11372) Removed the recently introduced `cacerts` option from TLS client schema due to incompatibilities with some cluster discovery mechanisms.

- [#11388](https://github.com/emqx/emqx/pull/11388) Increased `emqx_router_sup` restart intensity.

  The goal is to tolerate occasional crashes that can happen under relatively normal conditions
  and don't seem critical to shutdown the whole app (emqx).
  For example, mria write/delete call delegated from a replicant to a core node by `emqx_router_helper` may fail,
  if the core node is being stopped / restarted / not ready.

- [#11410](https://github.com/emqx/emqx/pull/11410) Reintroduced `cacerts` TLS client option as a deprecated option.

  This fixes issues found when trying to upgrade from 5.1.3 where that option is set in the configuration files or persisted in EMQX Operator settings.

- [#11424](https://github.com/emqx/emqx/pull/11424) Added a check for the maximum value of the timestamp in the API to ensure it is a valid Unix timestamp.

- [#11445](https://github.com/emqx/emqx/pull/11445) Removed os_mon application monitor support on Windows platforms to prevent VM crashes.
  Functionality remains on non-Windows platforms.

- [#11454](https://github.com/emqx/emqx/pull/11454) Fixed crashing when debugging/tracing with large payloads(introduce when [#11279](https://github.com/emqx/emqx/pull/11279))

- [#11456](https://github.com/emqx/emqx/pull/11456) Removed validation that enforced non-empty PEM for CA cert file.
  CA certificate file PEM can now be empty.

- [#11466](https://github.com/emqx/emqx/pull/11466) Fixed a crash that occurred when setting the `ssl_options.ciphers` configuration option to an empty string ("").

- [#11480](https://github.com/emqx/emqx/pull/11480) Return more user-friendly messages when rule functions are fed bad arguments.

- [#11520](https://github.com/emqx/emqx/pull/11520) Fixed issue where packets_connack_sent metric was not incremented on CONNACK packets sent with non-zero ack_flag

- [#11523](https://github.com/emqx/emqx/pull/11523) Fixes misunderstood prompt when invalid certificates/keys were specified for the `/configs` API.

- [#11534](https://github.com/emqx/emqx/pull/11534) Fixed increment on data bridge statistics when bridge is unhealthy. Now, messages sent to unhealthy bridges are being counted as dropped messages.

- [#11540](https://github.com/emqx/emqx/pull/11540) Improved HTTP response when attempting to create a bridge with an invalid name.

- [#11548](https://github.com/emqx/emqx/pull/11548) Fixed an issue that prevented the plugin order to be updated on the whole cluster.

- [#11366](https://github.com/emqx/emqx/pull/11366) Fixed an issue that could prevent a pod from starting if some bridge configuration were specified in `bootstrapConfig` using EMQX Operator.

- [#11444](https://github.com/emqx/emqx/pull/11444) Fixed error information when Kinesis bridge fails to connect to endpoint.

- [#11452](https://github.com/emqx/emqx/pull/11452) Updated the default payload template for Kinesis to store the entire message when no template is provided.

- [#11453](https://github.com/emqx/emqx/pull/11453) Fixed an issue which would yield false negatives when testing the connectivity of InfluxDB bridges.

- [#11461](https://github.com/emqx/emqx/pull/11461) Made the timeout for testing bridges connectivity follow more closely the configured health check timeout.

- [#11492](https://github.com/emqx/emqx/pull/11492) Fixed an issue which would yield false negatives when testing the connectivity of GreptimeDB bridges.

- [#11494](https://github.com/emqx/emqx/pull/11494) Added schema validator to reflect Amazon Kinesis static constraint: batch request can support up to 500 records (max batch size).

- [#11508](https://github.com/emqx/emqx/pull/11508) Fixed error handling in Kafka bridge when headers translate to an invalid value.

- [#11513](https://github.com/emqx/emqx/pull/11513) Fixed a bug which prevented the Kafka Producer bridge from using the correct template for the `timestamp` field.

- [#11527](https://github.com/emqx/emqx/pull/11527) Fixed an issue with Kafka header handling when placeholders resolve to an array of key-value pairs (e.g.: `[{"key": "foo", "value": "bar"}]`).
