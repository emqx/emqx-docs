# Releases

## e5.1.1

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

### [Known Issues](https://github.com/emqx/emqx-docs/blob/release-5.1/en_US/changes/known-issues-5.1.1.md)


## e5.1.0

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

### [Known Issues](https://github.com/emqx/emqx-docs/blob/release-5.1/en_US/changes/known-issues-5.1.0.md)

