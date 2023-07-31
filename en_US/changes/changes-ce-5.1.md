# Releases

## v5.1.3

### Bug Fixes

- [#11306](https://github.com/emqx/emqx/pull/11306) Fixed rule action metrics inconsistency where dropped requests were not accounted for.
- [#11327](https://github.com/emqx/emqx/pull/11327) Updated ekka to version 0.15.8, mria to version 0.5.10, and optvar to 1.0.5.
  This fixes occasional assertion failures:
  `{{badmatch,noproc},[{optvar,read,2,[{file,"optvar.erl"},{line,140}]},{optvar,read,1,[{file,"optvar.erl"},{line,124}]},...`
- [#11337](https://github.com/emqx/emqx/pull/11337) Fixed HTTP API error when a publish topic rewrite rule targets a topic with wildcards. Now it returns error 400 (Bad Match) instead of error 500 (Internal Error).
- [#11346](https://github.com/emqx/emqx/pull/11346) Updated ekka to version 0.15.9.
  This fixes dangling etcd locks occurred if acquiring the lock failed with a timeout.
- [#11352](https://github.com/emqx/emqx/pull/11352) Fixed this [#11345](https://github.com/emqx/emqx/issues/11345) crash issue when starting on Windows or any other platform without RocksDB support.

## v5.1.2

### Enhancements

- [#11124](https://github.com/emqx/emqx/pull/11124) Released packages for Amazon Linux 2023.
- [#11226](https://github.com/emqx/emqx/pull/11226) Unified the listener switch to `enable`, while being compatible with the previous `enabled`.
- [#11249](https://github.com/emqx/emqx/pull/11249) Supported REST API for setting alarm watermark of license.
- [#11251](https://github.com/emqx/emqx/pull/11251) Added `/cluster/topology` REST API endpoint. `GET` request to the endpoint returns the cluster topology: connections between RLOG core and replicant nodes.
- [#11253](https://github.com/emqx/emqx/pull/11253) The Webhook/HTTP bridge has been refactored to its own Erlang application. This allows for more flexibility in the future, and also allows for the bridge to be run as a standalone application.
- [#11289](https://github.com/emqx/emqx/pull/11289) Released packages for Debian 12.
- [#11290](https://github.com/emqx/emqx/pull/11290) Updated `jq` dependency to version 0.3.10 which included `oniguruma` library update to version 6.9.8 with few minor security fixes.
- [#11291](https://github.com/emqx/emqx/pull/11291) Updated RocksDB version to 1.8.0-emqx-1 via ekka update to 0.15.6.
- [#11236](https://github.com/emqx/emqx/pull/11236) Improved the speed of clients querying in HTTP API `/clients` endpoint with default parameters.

### Bug Fixes

- [#11065](https://github.com/emqx/emqx/pull/11065) Avoided logging irrelevant error messages during EMQX shutdown.
- [#11077](https://github.com/emqx/emqx/pull/11077) Fixed crash when updating binding with a non-integer port.
- [#11184](https://github.com/emqx/emqx/pull/11184) The maximum config value for `max_packet_size` has been set to 256MB defined by protocol. This is now enforced and any configuration with a value greater than that will break.
- [#11192](https://github.com/emqx/emqx/pull/11192) Fixed produces valid HOCON file when atom type is used. Removed unnecessary `"` from HOCON file.
- [#11195](https://github.com/emqx/emqx/pull/11195) Avoided to create duplicated subscription by HTTP API or client in Stomp gateway.
- [#11206](https://github.com/emqx/emqx/pull/11206) Made the username and password params of CoAP client to optional in connection mode.
- [#11208](https://github.com/emqx/emqx/pull/11208) Fixed the issue of abnormal data statistics for LwM2M client.
- [#11211](https://github.com/emqx/emqx/pull/11211) Fixed the error handling of the REST APIs and now code `404` is returned for `DELETE` operations on non-existent resources.
- [#11214](https://github.com/emqx/emqx/pull/11214) Fixed a bug where node configuration might fail to synchronize correctly when joining the cluster.
- [#11229](https://github.com/emqx/emqx/pull/11229) Fixed an issue preventing plugins from starting/stopping after changing configuration via `emqx ctl conf load`.
- [#11237](https://github.com/emqx/emqx/pull/11237) The `headers` default value in /prometheus API should be a map instead of a list.
- [#11250](https://github.com/emqx/emqx/pull/11250) Fixed a bug where a WebSocket packet containing more than one MQTT packet would have their order reversed.
- [#11271](https://github.com/emqx/emqx/pull/11271) Ensured that the range of percentage type is from 0% to 100%  in the REST API and configuration.
- [#11272](https://github.com/emqx/emqx/pull/11272) Fixed a typo in the log, when EMQX received an abnormal `PUBREL` packet, the `pubrel` was mistakenly typo as `pubrec`.
- [#11281](https://github.com/emqx/emqx/pull/11281) Restored support for the special `$queue/` shared subscription.
- [#11294](https://github.com/emqx/emqx/pull/11294) Fixed `emqx_ctl cluster join`, `leave`, and `status` commands.
- [#11296](https://github.com/emqx/emqx/pull/11296) Imported additional configurations from EMQX backup file (`emqx ctl import` command):
  - rule_engine (previously not imported due to the bug)
  - topic_metrics (previously not implemented)
  - slow_subs (previously not implemented).
- [#11309](https://github.com/emqx/emqx/pull/11309) Improved startup order of EMQX applications. Simplified build scripts and improved code reuse.
- [#11322](https://github.com/emqx/emqx/pull/11322) Imported additional configurations from EMQX backup file (`emqx ctl import` command):
  - rule_engine (previously not imported due to the bug)
  - topic_metrics (previously not implemented)
  - slow_subs (previously not implemented).

## v5.1.1

### Enhancements

- [#10667](https://github.com/emqx/emqx/pull/10667) The MongoDB connector and bridge have been refactored to a separate app to improve code structure.
- [#11115](https://github.com/emqx/emqx/pull/11115) Added info logs to indicate when buffered messages are dropped due to time-to-live (TTL) expiration.
- [#11133](https://github.com/emqx/emqx/pull/11133) Renamed `deliver_rate` to `delivery_rate` in the configuration of `retainer`.
- [#11137](https://github.com/emqx/emqx/pull/11137) Refactored the dashboard listener configuration to use a nested `ssl_options` field for SSL settings.
- [#11138](https://github.com/emqx/emqx/pull/11138) Changed k8s `api_server` default value from `http://127.0.0.1:9091` to `https://kubernetes.default.svc:443`
  - `emqx_ctl conf show cluster` no longer displays irrelevant configuration items, such as when `discovery_strategy=static`, it will not display configuration information related to `etcd/k8s/dns`.
  - Removed `zones`(deprecated config key) from `emqx_ctl conf show_keys`.
- [#11165](https://github.com/emqx/emqx/pull/11165) Removed `/configs/limiter` API from `swagger.json`, only the API documentation was removed,
  and the `/configs/limiter` API functionalities have not been changed.
- [#11166](https://github.com/emqx/emqx/pull/11166) Added 3 random SQL functions to the rule engine.
  - `random()`: Generates a random number between 0 and 1 (0.0 =< X < 1.0).
  - `uuid_v4()`: Generates a random UUID (version 4) string.
  - `uuid_v4_no_hyphen()`: Generates a random UUID (version 4) string without hyphens.
- [#11180](https://github.com/emqx/emqx/pull/11180) Added a new configuration API `/configs`(GET/PUT) that supports reloading the hocon format configuration file.
- [#11020](https://github.com/emqx/emqx/pull/11020) Upgraded emqtt dependency to avoid sensitive data leakage in the debug log.
- [#11135](https://github.com/emqx/emqx/pull/11135) Improved time offset parser in the rule engine and return uniform error codes.

### Bug Fixes

- [#11004](https://github.com/emqx/emqx/pull/11004) Wildcards are no longer allowed for the destination topic in topic rewrite.

- [#11026](https://github.com/emqx/emqx/pull/11026) Addressed an inconsistency in the usage of `div` and `mod` operations within the rule engine. Previously, the `div` operation could only be used as an infix operation, and `mod` could only be applied through a function call. Now, both `div` and `mod` can be used via function call syntax and infix syntax.

- [#11037](https://github.com/emqx/emqx/pull/11037) When starting an HTTP connector, EMQX now returns a descriptive error in case the system is unable to connect to the remote target system.

- [#11039](https://github.com/emqx/emqx/pull/11039) Fixed database number validation for Redis connector. Previously, negative numbers were accepted as valid database numbers.

- [#11074](https://github.com/emqx/emqx/pull/11074) Fixed a bug to adhere to Protocol spec MQTT-5.0 [MQTT-3.8.3-4].

- [#11094](https://github.com/emqx/emqx/pull/11094) Fixed an issue where connection errors in Kafka Producer would not be reported when reconnecting the bridge.

- [#11103](https://github.com/emqx/emqx/pull/11103) Updated `erlcloud` dependency.

- [#11106](https://github.com/emqx/emqx/pull/11106) Added validation for the maximum number of `worker_pool_size` of a bridge resource.

  Now the maximum amount is 1024 to avoid large memory consumption from an unreasonable number of workers.

- [#11118](https://github.com/emqx/emqx/pull/11118) Ensured that validation errors in REST API responses are slightly less confusing. Now, if there are out-of-range errors, they will be presented as `{"value": 42, "reason": {"expected": "1..10"}, ...}`, replacing the previous usage of `expected_type` with `expected`.

- [#11126](https://github.com/emqx/emqx/pull/11126) Rule metrics for async mode bridges will set failure counters correctly now.

- [#11134](https://github.com/emqx/emqx/pull/11134) Fix the value of the uppercase `authorization` header is not obfuscated.

- [#11139](https://github.com/emqx/emqx/pull/11139) The Redis connector has been refactored to its own Erlang application to improve the code structure.

- [#11145](https://github.com/emqx/emqx/pull/11145) Added several fixes and improvements in Ekka and Mria.

  Ekka:

  - Improved cluster discovery log messages to consistently describe actual events. [Ekka PR](https://github.com/emqx/ekka/pull/204)
  - Removed deprecated cluster auto-clean configuration parameter (it has been moved to Mria). [Ekka PR](https://github.com/emqx/ekka/pull/203)

  Mria:

  - Ping now only runs on replicant nodes. Previously, `mria_lb` was trying to ping both stopped and running replicant nodes, which could result in timeout errors. [Mria PR](https://github.com/emqx/mria/pull/146)
  - Used `null_copies` storage when copying `$mria_rlog_sync` table. This fix has no effect on EMQX for now, as `$mria_rlog_sync` is only used in `mria:sync_transaction/2,3,4`, which is not utilized by EMQX. [Mria PR](https://github.com/emqx/mria/pull/144)

- [#11148](https://github.com/emqx/emqx/pull/11148) Fixed an issue when nodes tried to synchronize configuration update operations to a node which has already left the cluster.

- [#11150](https://github.com/emqx/emqx/pull/11150) Wait for Mria table when emqx_psk app is being started to ensure that PSK data is synced to replicant nodes even if they don't have init PSK file.

- [#11151](https://github.com/emqx/emqx/pull/11151) The MySQL connector has been refactored into its own Erlang application to improve the code structure.

- [#11158](https://github.com/emqx/emqx/pull/11158) Wait for Mria table when the mnesia backend of retainer starts to avoid a possible error of the retainer when joining a cluster.

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

## v5.1.0

### Enhancements

- [#10858](https://github.com/emqx/emqx/pull/10858) A new utility function timezone_to_offset_seconds/1 has been added to the rule engine SQL language. This function converts a timezone string (for example, "+02:00", "Z" and "local") to the corresponding offset in seconds.
- [#10754](https://github.com/emqx/emqx/pull/10754) The MQTT bridge has been enhanced to utilize connection pooling and leverage available parallelism, substantially improving throughput.
  As a consequence, single MQTT bridge now uses a pool of `clientid`s to connect to the remote broker.
- [#10782](https://github.com/emqx/emqx/pull/10782) Added a new `deliver_rate` option to the retainer configuration, which can limit the maximum delivery rate per session in the retainer.
- [#10598](https://github.com/emqx/emqx/pull/10598) Provided a callback method of Unary type in ExProto to avoid possible message disorder issues.
- [#10790](https://github.com/emqx/emqx/pull/10790) Reduced the overhead during configuration reads by optimizing the configuration read mechanism.
- [#10910](https://github.com/emqx/emqx/pull/10910) The data bridge resource option `auto_restart_interval` was deprecated in favor of `health_check_interval`, and `request_timeout` was renamed to `request_ttl`. Also, the default `request_ttl` value went from 15 seconds to 45 seconds.
  The previous existence of both `auto_restart_interval` and `health_check_interval` was a source of confusion, as both parameters influenced the recovery of data bridges under failures. An inconsistent configuration of those two parameters could lead to messages being expired without a chance to retry. Now, `health_check_interval` is used both to control the periodicity of health checks that may transition the data bridge into `disconnected` or `connecting` states, as well as recovering from `disconnected`.
- [#10929](https://github.com/emqx/emqx/pull/10929) Upgraded Erlang/OTP to 25.3.2-1.
- [#10909](https://github.com/emqx/emqx/pull/10909) Removed the deprecated HTTP APIs for gateways.
- [#10933](https://github.com/emqx/emqx/pull/10933) Added support for configuring TCP keep-alive in MQTT/TCP and MQTT/SSL listeners.
- \#10948 Added `live_connections` field for some HTTP APIs, i.e:
  - `/monitor_current`, `/monitor_current/nodes/{node}`
  - `/monitor/nodes/{node}`, `/monitor`
  - `/node/{node}`, `/nodes`
- [#10941](https://github.com/emqx/emqx/pull/10941) Improved the collection speed of Prometheus metrics when setting `prometheus.vm_dist_collector=disabled` and metric `erlang_vm_statistics_run_queues_length_total` is renamed to `erlang_vm_statistics_run_queues_length`
- [#10985](https://github.com/emqx/emqx/pull/10985) Renamed `emqx ctl` command `cluster_call` to `conf cluster_sync`. The old command `cluster_call` is still a valid command, but not included in usage info.
- [#10988](https://github.com/emqx/emqx/pull/10988) Improved log security when data bridge creation fails to ensure sensitive data is always obfuscated.
- [#10926](https://github.com/emqx/emqx/pull/10926) Allowed `enable` as well as `enabled` as the state flag for listeners.
  Prior to this change, listener can be enable/disabled by setting the `true` or `false` on the `enabled` config. This is slightly different naming comparing to other state flags in the system. Now the `enable` flag is added as an alias in listener config.
- [#10970](https://github.com/emqx/emqx/pull/10970) A query_mode parameter has been added to the Kafka producer bridge. This parameter allows you to specify if the bridge should use the asynchronous or synchronous mode when sending data to Kafka. The default is asynchronous mode.
- [#10676](https://github.com/emqx/emqx/pull/10676) Added CLI commands `emqx ctl export` and `emqx ctl import` for importing/exporting configuration and user data. This allows exporting configurations and built-in database data from a running EMQX cluster and importing them into the same or another running EMQX cluster.
- [#10961](https://github.com/emqx/emqx/pull/10961) Added support for unlimited max connections for gateway listeners by allowing infinity as a valid value for the `max_connections` field in the configuration and HTTP API.
- [#11019](https://github.com/emqx/emqx/pull/11019) Improved log security for JWT, now it will be obfuscated before print.
- [#11034](https://github.com/emqx/emqx/pull/11034) Hid the broker config and changed the `broker.shared_subscription_strategy` to `mqtt.shared_subscription_strategy` as it belongs to `mqtt`.
- [#11045](https://github.com/emqx/emqx/pull/11045) The listener's authentication and zone related apis have been officially removed in version `5.1.0`.
- [#11062](https://github.com/emqx/emqx/pull/11062) Renamed config `log.file.to` to `log.file.path`.
- [#10833](https://github.com/emqx/emqx/pull/10833) Only include enabled authenticators and authorizers in telemetry report, not all of them.

### Bug Fixes

- [#11018](https://github.com/emqx/emqx/pull/11018) Fixed multiple issues with the Stomp gateway, including:

  - Fixed an issue where `is_superuser` was not working correctly.
  - Fixed an issue where the mountpoint was not being removed in message delivery.
  - After a message or subscription request fails, the Stomp client should be disconnected
    immediately after replying with an ERROR message.

- [#11051](https://github.com/emqx/emqx/pull/11051) Added validation to ensure that certificate `depth` (listener SSL option) is a non negative integer.

- [#10884](https://github.com/emqx/emqx/pull/10884) Fixed an issue where trying to get rule info or metrics could result in a crash when a node is joining a cluster.

- [#10887](https://github.com/emqx/emqx/pull/10887) Fixed a potential issue where requests to bridges might take a long time to be retried.
  This only affected low throughput scenarios, where the buffering layer could take a long time to detect connectivity and driver problems.

- [#10871](https://github.com/emqx/emqx/pull/10871) Fixed an issue where the Dashboard shows that the connection still exists after a CoAP connection is disconnected, but deletion and message posting requests do not take effect.

- [#10880](https://github.com/emqx/emqx/pull/10880) Added a new REST API `POST /clients/kickout/bulk` for kicking out multiple clients in bulk.

- [#10923](https://github.com/emqx/emqx/pull/10923) Fixed a race-condition in channel info registration.
  Prior to this fix, when system is under heavy load, it might happen that a client is disconnected (or has its session expired) but still can be found in the clients page in dashboard. One of the possible reasons is a race condition fixed in this PR: the connection is killed in the middle of channel data registration.

- [#10930](https://github.com/emqx/emqx/pull/10930) Added a schema validation for duration data type to avoid invalid values.
  Before this fix, it was possible to use absurd values in the schema that would exceed the system limit, causing a crash.

- [#10952](https://github.com/emqx/emqx/pull/10952) Disallow enabling `fail_if_no_peer_cert` in listener SSL options if `verify = verify_none` is set.
  Setting `fail_if_no_peer_cert = true` and `verify = verify_none` caused connection errors due to incompatible options. This fix validates the options when creating or updating a listener to avoid these errors.

  Note: any old listener configuration with `fail_if_no_peer_cert = true` and `verify = verify_none` that was previously allowed will fail to load after applying this fix and must be manually fixed.

- [#10951](https://github.com/emqx/emqx/pull/10951) Fixed the issue in MQTT-SN gateway where the `mountpoint` does not take effect on message publishing.

- [#10943](https://github.com/emqx/emqx/pull/10943) Deprecated UDP mcast mechanism for cluster discovery.
  This feature has been planed for deprecation since 5.0 mainly due to the lack of actual production use. This feature code is not yet removed in 5.1, but the document interface is demoted.

- [#10902](https://github.com/emqx/emqx/pull/10902) Avoid syncing cluser.hocon file from the nodes running a newer version than the self-node.
  During cluster rolling upgrade, if an older version node has to restart due to whatever reason, if it copies the `cluster.hocon` file from a newer version node, it may fail to start. After this fix, the older version node will not copy the `cluster.hocon` file from a newer, so it will use its own `cluster.hocon` file to start.

- [#10911](https://github.com/emqx/emqx/pull/10911) The error message and log entry that appear when one tries to create a bridge with a name the exceeds 255 bytes is now easier to understand.

- [#10983](https://github.com/emqx/emqx/pull/10983) Fixed the issue when mqtt clients could not connect over TLS if the listener was configured to use TLS v1.3 only.
  The problem was that TLS connection was trying to use options incompatible with TLS v1.3.

- [#10977](https://github.com/emqx/emqx/pull/10977) Fixed the delay in updating subscription count metric and corrected configuration issues in Stomp gateway.

- [#10950](https://github.com/emqx/emqx/pull/10950) Fixed the issue where the `enable_qos` option does not take effect in the MQTT-SN gateway.

- [#10994](https://github.com/emqx/emqx/pull/10994) Redacted `proxy-authorization` headers as used by HTTP connector to avoid leaking secrets into log files.

- [#10996](https://github.com/emqx/emqx/pull/10996) For any unknown HTTP/API request, the default response is a 404 error rather than the dashboard's index.html.

- [#11005](https://github.com/emqx/emqx/pull/11005) Fixed the issue where the `method` field cannot be correctly printed in the trace logs of AuthN HTTP.

- [#10955](https://github.com/emqx/emqx/pull/10955) Fixed the issue in MQTT-SN gateway where deleting Predefined Topics configuration does not work.

- [#11030](https://github.com/emqx/emqx/pull/11030) Improved error messages when a validation error occurs while using the Listeners HTTP API.

- [#11033](https://github.com/emqx/emqx/pull/11033) Deprecated the `mountpoint` field in `AuthenticateRequest` in ExProto gateway.
  This field was introduced in e4.x, but in fact, in e5.0 we have provided
  `gateway.exproto.mountpoint` for configuration, so there is no need to override
  it through the Authenticate request.

  Additionally, updates the default value of `subscriptions_max`, `inflight_max`,
  `mqueue_max` to `infinity`.

- [#11042](https://github.com/emqx/emqx/pull/11042) Fixed crash on REST API `GET /listeners` when listener's `max_connections` is set to a string.

- [#11028](https://github.com/emqx/emqx/pull/11028) Disallowed using multiple TLS versions in the listener config that include tlsv1.3 but exclude tlsv1.2.
  Using TLS configuration with such version gap caused connection errors.
  Additionally, drop and log TLS options that are incompatible with the selected TLS version(s).

  Note: any old listener configuration with the version gap described above will fail to load
  after applying this fix and must be manually fixed.

- [#11056](https://github.com/emqx/emqx/pull/11056) Fixed the issue where newly created listeners sometimes do not start properly.
  When you delete a system default listener and add a new one named 'default', it will not start correctly.

  - Fixed the bug where configuration failure on certain nodes can cause Dashboard unavailability.

- [#11070](https://github.com/emqx/emqx/pull/11070) Fixed the problem that the `cluster.autoclean` configuration item does not take effect.

- [#11092](https://github.com/emqx/emqx/pull/11092) and [#11100](https://github.com/emqx/emqx/pull/11100) Fixed problem when replicant nodes were unable to connect to the core node due to timeout in `mria_lb:core_nodes()` call.
  Relevant mria pull request: [emqx/mria#143](https://github.com/emqx/mria/pull/143)

## [Known Issues](https://github.com/emqx/emqx-docs/blob/release-5.1/en_US/changes/known-issues-5.1.0.md)