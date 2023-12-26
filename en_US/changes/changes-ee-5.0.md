# v5.0

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
