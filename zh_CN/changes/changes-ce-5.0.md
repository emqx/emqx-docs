# v5.0

## 5.0.26

*发布日期: 2023-05-29*

### Enhancements

- [#10584](https://github.com/emqx/emqx/pull/10584) Add log level configuration to SSL communication

- [#10702](https://github.com/emqx/emqx/pull/10702) Introduce a more straightforward configuration option `keepalive_multiplier` and
  deprecate the old `keepalive_backoff` configuration.
  After this enhancement, EMQX checks the client's keepalive timeout status
  period by multiplying the "Client Requested Keepalive Interval" with `keepalive_multiplier`.

- [#10713](https://github.com/emqx/emqx/pull/10713) We hide the request_timeout in resource_option of the webhook to keep it consistent with the http request_timeout of the webhook.
  From now on, when configuring a webhook through API or configuration files,
  it is no longer necessary to configure the request_timeout of the resource. Only configuring the http request_timeout is sufficient, and the request_timeout in the resource will automatically be consistent with the http request_timeout.

- [#10511](https://github.com/emqx/emqx/pull/10511) Improve the security and privacy of some resource logs by masking sensitive information in the data.

- [#10678](https://github.com/emqx/emqx/pull/10678) Optimized counter increment calls to avoid work if increment is zero.

- [#10690](https://github.com/emqx/emqx/pull/10690) Added a retry mechanism to webhook bridge that attempts to improve throughput.

  This optimization retries request failures without blocking the buffering layer, which can improve throughput in situations of high messaging rate.

- [#10698](https://github.com/emqx/emqx/pull/10698) Optimize memory usage when accessing the configuration during runtime.

### Bug Fixes

- [#10340](https://github.com/emqx/emqx/pull/10340) Fixed the issue that could lead to crash logs being printed when stopping EMQX via systemd.

- [#10563](https://github.com/emqx/emqx/pull/10563) Corrected an issue where the no_local flag was not functioning correctly.

- [#10600](https://github.com/emqx/emqx/pull/10600) Deleted emqx_statsd application.

- [#10653](https://github.com/emqx/emqx/pull/10653) Store gateway authentication TLS certificates and keys in the data directory.

- [#10677](https://github.com/emqx/emqx/pull/10677) In Rule API, reapond with 404 HTTP error code when trying to delete a rule that does not exist.

- [#10682](https://github.com/emqx/emqx/pull/10682) Fix the timestamp for the will message is incorrectly assigned at the session creation time, now this timestamp is the disconnected time of the session.

- [#10701](https://github.com/emqx/emqx/pull/10701) RPM package for Amazon Linux 2 did not support TLS v1.3 as it was assembled with Erlang/OTP built with openssl 1.0.

- [#10715](https://github.com/emqx/emqx/pull/10715) Postpone trimming the connection information structure until after `client.connected` hooks have been executed. These hooks once again have access to the client's peer certificate.

- [#10717](https://github.com/emqx/emqx/pull/10717) Fixed an issue where the buffering layer processes could use a lot of CPU when inflight window is full.

- [#10724](https://github.com/emqx/emqx/pull/10724) A summary has been added for all endpoints in the HTTP API documentation (accessible at "http://emqx_host_name:18083/api-docs").

- [#10726](https://github.com/emqx/emqx/pull/10726) Validate Health Check Interval and Auto Restart Interval against the range from 1ms to 1 hour.

- [#10728](https://github.com/emqx/emqx/pull/10728) Fixed an issue where the rule engine was unable to access variables exported by `FOREACH` in the `DO` clause.

  Given a payload: `{"date": "2023-05-06", "array": ["a"]}`, as well as the following SQL statement:

  ```
  FOREACH payload.date as date, payload.array as elem
  DO date, elem
  FROM "t/#"
  ```

  Prior to the fix, the `date` variable exported by `FOREACH` could not be accessed in the `DO` clause of the above SQL, resulting in the following output for the SQL statement:
  `[{"elem": "a","date": "undefined"}]`.
  After the fix, the output of the SQL statement is: `[{"elem": "a","date": "2023-05-06"}]`

- [#10737](https://github.com/emqx/emqx/pull/10737) Fix the issue where the HTTP API interface of Gateway cannot handle ClientIDs with
  special characters, such as: `!@#$%^&*()_+{}:"<>?/`.

- [#10742](https://github.com/emqx/emqx/pull/10742) Check the correctness of the rules before saving the authorization file source.
  Previously, Saving wrong rules could lead to restart failure.

- [#10743](https://github.com/emqx/emqx/pull/10743) Fixes an issue where trying to get a bridge info or metrics could result in a crash when a node is joining a cluster.

- [#10746](https://github.com/emqx/emqx/pull/10746) Add missing support of the event `$events/delivery_dropped` into the rule engine test API `rule_test`.

- [#10747](https://github.com/emqx/emqx/pull/10747) Refactor date and time functions, `format_date` and `date_to_unix_ts`, in the rule engine to fix the implementation problem.

- [#10755](https://github.com/emqx/emqx/pull/10755) Fixed data bridge resource update race condition.

  In the 'delete + create' process for EMQX resource updates,
  long bridge creation times could cause dashboard request timeouts.
  If a bridge resource update was initiated before completion of its creation,
  it led to an erroneous deletion from the runtime, despite being present in the config file.

  This fix addresses the race condition in bridge resource updates,
  ensuring the accurate identification and addition of new resources,
  maintaining consistency between runtime and configuration file statuses.

- [#10760](https://github.com/emqx/emqx/pull/10760) Fix Internal Error 500 that occurred sometimes when bridge statistics page was updated while a node was (re)joining the cluster.

- [#10761](https://github.com/emqx/emqx/pull/10761) Fixing the issue where the default value of SSL certificate for Dashboard Listener was not correctly interpolated, which caused HTTPS to be inaccessible when verify_peer and cacertfile were using the default configuration.

- [#10785](https://github.com/emqx/emqx/pull/10785) Ensure `EMQX_LOG_DIR` is set by Windows boot script.

  The environment variable `EMQX_LOG_DIR` was missing in v5.0.25, caused EMQX Windows package fail to boot unless set by sysadmin.

- [#10801](https://github.com/emqx/emqx/pull/10801) Avoid duplicated percent decode the topic name in API `/topics/{topic}` and `/topics`.

- [#10809](https://github.com/emqx/emqx/pull/10809) Address `** ERROR ** Mnesia post_commit hook failed: error:badarg` error messages happening during node shutdown or restart.
  Mria pull request: https://github.com/emqx/mria/pull/142

- [#10817](https://github.com/emqx/emqx/pull/10817) Fix the error of not being able to configure `auto_restart_interval` as infinity

- [#10818](https://github.com/emqx/emqx/pull/10818) Fixing `emqx_ctl traces` command.

- [#10820](https://github.com/emqx/emqx/pull/10820) In case the cluster updated license before the new node join in. The new node will not apply the updated license.
  After this change, the new joined node will use the cluster's license key.

  Sometimes the new node must start with a outdated license.
  e.g. use emqx-operator deployed and needed to scale up after license expired.
  At the time the cluster's license key already updated by API/CLI, but the new node won't use it.

- [#10833](https://github.com/emqx/emqx/pull/10833) Only include enabled authenticators and authorizers in telemetry report, not all of them.

- [#10851](https://github.com/emqx/emqx/pull/10851) Obfuscated sensitive data in the bad API logging.

## 5.0.25

*发布日期: 2023-05-12*

### Enhancements

- [#10568](https://github.com/emqx/emqx/pull/10568) Add shutdown counter information to `emqx ctl listeners` command

- [#10571](https://github.com/emqx/emqx/pull/10571) Do not emit useless crash report when EMQX stops.
  Previously, when EMQX (and `emqx_topic_metrics` in particular) stopped and removed underlying tables, some messages were still being handled and crashed.

- [#10588](https://github.com/emqx/emqx/pull/10588) Increase the time precision of trace logs from second to microsecond.
  For example, change from `2023-05-02T08:43:50+00:00` to `2023-05-02T08:43:50.237945+00:00`.

- [#10623](https://github.com/emqx/emqx/pull/10623) Renamed `max_message_queue_len` to `max_mailbox_size` in the `force_shutdown` configuration. Old name is kept as an alias, so this change is backward compatible.

- [#10417](https://github.com/emqx/emqx/pull/10417) Improve get config performance by eliminating temporary references.

- [#10525](https://github.com/emqx/emqx/pull/10525) Reduce resource usage per MQTT packet handling.

- [#10528](https://github.com/emqx/emqx/pull/10528) Reduce memory footprint in hot code path.

- [#10573](https://github.com/emqx/emqx/pull/10573) Improved performance of Webhook bridge when using synchronous query mode.
  This also should improve the performance of other bridges when they are configured with no batching.

- [#10591](https://github.com/emqx/emqx/pull/10591) Improve the configuration of the limiter.

  - Simplify the memory representation of the limiter configuration.
  - Make sure the node-level limiter can really work when the listener's limiter configuration is omitted.

- [#10625](https://github.com/emqx/emqx/pull/10625) Simplify limiter configuration.
  - Reduce the complexity of the limiter's configuration.
    e.g. now users can use `limiter.messages_rate = 1000/s` to quickly set the node-level limit for the message publish.
  - Update the `configs/limiter` API to suit this refactor.

### Bug Fixes

- [#10548](https://github.com/emqx/emqx/pull/10548) Fixed a race condition in the HTTP driver that would result in an error rather than a retry of the request.
  Related fix in the driver: https://github.com/emqx/ehttpc/pull/45

- [#10556](https://github.com/emqx/emqx/pull/10556) Wrap potentially sensitive data in `emqx_connector_http` if `Authorization` headers are being passed at initialization.

- [#10659](https://github.com/emqx/emqx/pull/10659) Fix the issue where emqx cannot start when `sysmon.os.mem_check_interval` is disabled.

## 5.0.24

*发布日期: 2023-04-26*

### Enhancements

- [#10457](https://github.com/emqx/emqx/pull/10457) Deprecates the integration with StatsD.

  There seemd to be no user using StatsD integration, so we have decided to hide this feature
  for now. We will either remove or revive it based on requirements in the future.

- [#10458](https://github.com/emqx/emqx/pull/10458) Set the level of plugin configuration options to low level,
  in most cases, users only need to manage plugins on the dashboard
  without the need for manual modification, so we lowered the level.

- [#10491](https://github.com/emqx/emqx/pull/10491) Rename `etcd.ssl` to `etcd.ssl_options` to keep all of SSL options consistent in the configuration file.

- [#10512](https://github.com/emqx/emqx/pull/10512) Improved the storage format of Unicode characters in data files,
  Now we can store Unicode characters normally.
  For example: "SELECT \* FROM \"t/1\" WHERE clientid = \"-测试专用-\""

- [#10487](https://github.com/emqx/emqx/pull/10487) Optimize the instance of limiter for whose rate is `infinity` to reduce memory and CPU usage.

- [#10490](https://github.com/emqx/emqx/pull/10490) Remove the default limit of connect rate which used to be `1000/s`

### Bug Fixes

- [#10407](https://github.com/emqx/emqx/pull/10407) Improve 'emqx_alarm' performance by using Mnesia dirty operations and avoiding
  unnecessary calls from 'emqx_resource_manager' to reactivate alarms that have been already activated.
  Use new safe 'emqx_alarm' API to activate/deactivate alarms to ensure that emqx_resource_manager
  doesn't crash because of alarm timeouts.
  The crashes were possible when the following conditions co-occurred:

  - a relatively high number of failing resources, e.g. bridges tried to activate alarms on re-occurring errors;
  - the system experienced a very high load.

- [#10420](https://github.com/emqx/emqx/pull/10420) Fix HTTP path handling when composing the URL for the HTTP requests in authentication and authorization modules.

  - Avoid unnecessary URL normalization since we cannot assume that external servers treat original and normalized URLs equally. This led to bugs like [#10411](https://github.com/emqx/emqx/issues/10411).
  - Fix the issue that path segments could be HTTP encoded twice.

- [#10422](https://github.com/emqx/emqx/pull/10422) Fixed a bug where external plugins could not be configured via environment variables in a lone-node cluster.

- [#10448](https://github.com/emqx/emqx/pull/10448) Fix a compatibility issue of limiter configuration introduced by v5.0.23 which broke the upgrade from previous versions if the `capacity` is `infinity`.

  In v5.0.23 we have replaced `capacity` with `burst`. After this fix, a `capacity = infinity` config will be automatically converted to equivalent `burst = 0`.

- [#10449](https://github.com/emqx/emqx/pull/10449) Validate the ssl_options and header configurations when creating authentication http (`authn_http`).
  Prior to this, incorrect `ssl` configuration could result in successful creation but the entire authn being unusable.

- [#10455](https://github.com/emqx/emqx/pull/10455) Fixed an issue that could cause (otherwise harmless) noise in the logs.

  During some particularly slow synchronous calls to bridges, some late replies could be sent to connections processes that were no longer expecting a reply, and then emit an error log like:

  ```
  2023-04-19T18:24:35.350233+00:00 [error] msg: unexpected_info, mfa: emqx_channel:handle_info/2, line: 1278, peername: 172.22.0.1:36384, clientid: caribdis_bench_sub_1137967633_4788, info: {#Ref<0.408802983.1941504010.189402>,{ok,200,[{<<"cache-control">>,<<"max-age=0, ...">>}}
  ```

  Those logs are harmless, but they could flood and worry the users without need.

- [#10462](https://github.com/emqx/emqx/pull/10462) Deprecate config `broker.shared_dispatch_ack_enabled`.
  This was designed to avoid dispatching messages to a shared-subscription session which has the client disconnected.
  However since v5.0.9, this feature is no longer useful because the shared-subscrption messages in a expired session will be redispatched to other sessions in the group.
  See also: https://github.com/emqx/emqx/pull/9104

- [#10463](https://github.com/emqx/emqx/pull/10463) Improve bridges API error handling.
  If Webhook bridge URL is not valid, bridges API will return '400' error instead of '500'.

- [#10484](https://github.com/emqx/emqx/pull/10484) Fix the issue that the priority of the configuration cannot be set during rolling upgrade.
  For example, when authorization is modified in v5.0.21 and then upgraded v5.0.23 through rolling upgrade,
  the authorization will be restored to the default.

- [#10495](https://github.com/emqx/emqx/pull/10495) Add the limiter API `/configs/limiter` which was deleted by mistake back.

- [#10500](https://github.com/emqx/emqx/pull/10500) Add several fixes, enhancements and features in Mria:

  - protect `mria:join/1,2` with a global lock to prevent conflicts between
    two nodes trying to join each other simultaneously
    [Mria PR](https://github.com/emqx/mria/pull/137)
  - implement new function `mria:sync_transaction/4,3,2`, which blocks the caller until
    a transaction is imported to the local node (if the local node is a replicant, otherwise,
    it behaves exactly the same as `mria:transaction/3,2`)
    [Mria PR](https://github.com/emqx/mria/pull/136)
  - optimize `mria:running_nodes/0`
    [Mria PR](https://github.com/emqx/mria/pull/135)
  - optimize `mria:ro_transaction/2` when called on a replicant node
    [Mria PR](https://github.com/emqx/mria/pull/134).

- [#10518](https://github.com/emqx/emqx/pull/10518) Add the following fixes and features in Mria:
  - call `mria_rlog:role/1` safely in mria_membership to ensure that mria_membership
    gen_server won't crash if RPC to another node fails
    [Mria PR](https://github.com/emqx/mria/pull/139)
  - Add extra field to ?rlog_sync table to facilitate extending this functionality in future
    [Mria PR](https://github.com/emqx/mria/pull/138).

## 5.0.23

*发布日期: 2023-04-18*

### Enhancements

- [#10156](https://github.com/emqx/emqx/pull/10156) Change the priority of the configuration:

  1. If it is a new installation of EMQX, the priority of configuration is `ENV > emqx.conf > HTTP API`.
  2. If EMQX is upgraded from an old version (i.e., the cluster-override.conf file still exists in EMQX's data directory), then the configuration priority remains the same as before. That is, `HTTP API > ENV > emqx.conf`.

  Deprecated data/configs/local-override.conf.

  Stabilizing the HTTP API for hot updates.

- [#10354](https://github.com/emqx/emqx/pull/10354) More specific error messages when configure with bad max_heap_size value.
  Log current value and the max value when the `message_queue_too_long` error is thrown.

- [#10359](https://github.com/emqx/emqx/pull/10359) Metrics now are not implicitly collected in places where API handlers don't make any use of them. Instead, a separate backplane RPC gathers cluster-wide metrics.

- [#10373](https://github.com/emqx/emqx/pull/10373) Deprecate the trace.payload_encode configuration.
  Add payload_encode=[text,hidden,hex] option when creating a trace via HTTP API.

- [#10389](https://github.com/emqx/emqx/pull/10389) Unify the config formats for `cluster.core_nodes` and `cluster.statics.seeds`.
  Now they both support formats in array `["emqx1@127.0.0.1", "emqx2@127.0.0.1"]` or semicolon-separated string `"emqx1@127.0.0.1,emqx2@127.0.0.1"`.

- [#10391](https://github.com/emqx/emqx/pull/10391) Hide a large number of advanced options to simplify the configuration file.

  That includes `rewrite`, `topic_metric`, `persistent_session_store`, `overload_protection`,
  `flapping_detect`, `conn_congestion`, `stats,auto_subscribe`, `broker_perf`,
  `shared_subscription_group`, `slow_subs`, `ssl_options.user_lookup_fun` and some advance items
  in `node` and `dashboard` section, [#10358](https://github.com/emqx/emqx/pull/10358),
  [#10381](https://github.com/emqx/emqx/pull/10381), [#10385](https://github.com/emqx/emqx/pull/10385).

- [#10392](https://github.com/emqx/emqx/pull/10392) A new function to convert a formatted date to an integer timestamp has been added: date_to_unix_ts/3

- [#10404](https://github.com/emqx/emqx/pull/10404) Change the default queue mode for buffer workers to `memory_only`.
  Before this change, the default queue mode was `volatile_offload`. When under high message rate pressure and when the resource is not keeping up with such rate, the buffer performance degraded a lot due to the constant disk operations.

- [#10426](https://github.com/emqx/emqx/pull/10426) Optimize the configuration priority mechanism to fix the issue where the configuration
  changes made to `etc/emqx.conf` do not take effect after restarting EMQX.

  More introduction about the new mechanism: [Configure Override Rules](https://www.emqx.io/docs/en/v5.0/configuration/configuration.html#configure-override-rules)

- [#10376](https://github.com/emqx/emqx/pull/10376) Simplify the configuration of the limiter feature and optimize some codes

  - Rename `message_in` to `messages`
  - Rename `bytes_in` to `bytes`
  - Use `burst` instead of `capacity`
  - Hide non-importance fields
  - Optimize limiter instances in different rate settings

- [#10430](https://github.com/emqx/emqx/pull/10430) Simplify the configuration of the `retainer` feature.
  - Mark `flow_control` as non-importance field.

### Bug Fixes

- [#10369](https://github.com/emqx/emqx/pull/10369) Fix error in `/api/v5/monitor_current` API endpoint that happens when some EMQX nodes are down.

  Prior to this fix, sometimes the request returned HTTP code 500 and the following message:

  ```
  {"code":"INTERNAL_ERROR","message":"error, badarg, [{erlang,'++',[{error,nodedown},[{node,'emqx@10.42.0.150'}]], ...
  ```

- [#10410](https://github.com/emqx/emqx/pull/10410) Fix config check failed when gateways are configured in emqx.conf.
  This issue was first introduced in v5.0.22 via [#10278](https://github.com/emqx/emqx/pull/10278), the boot-time config check was missing.

## 5.0.22

*发布日期: 2023-04-13*

### Enhancements

- [#10077](https://github.com/emqx/emqx/pull/10077) Add support for QUIC TLS password protected certificate file.

- [#10128](https://github.com/emqx/emqx/pull/10128) Add support for OCSP stapling for SSL MQTT listeners.

- [#10164](https://github.com/emqx/emqx/pull/10164) Add CRL check support for TLS MQTT listeners.

- [#10206](https://github.com/emqx/emqx/pull/10206) Decouple the query mode from the underlying call mode for buffer
  workers.

  Prior to this change, setting the query mode of a resource
  such as a bridge to `sync` would force the buffer to call the
  underlying connector in a synchronous way, even if it supports async
  calls.

- [#10207](https://github.com/emqx/emqx/pull/10207) Use 'label' from i18n file as 'summary' in OpenAPI spec.

- [#10210](https://github.com/emqx/emqx/pull/10210) Unregister Mnesia post commit hook when Mria is being stopped.
  This fixes hook failures occasionally occurring on stopping/restarting Mria.

  [Mria PR](https://github.com/emqx/mria/pull/133)

- [#10224](https://github.com/emqx/emqx/pull/10224) Add the option to customize `clusterIP` in Helm chart, so that a user may set it to a fixed IP.

- [#10263](https://github.com/emqx/emqx/pull/10263) Add command 'eval-ex' for Elixir expression evaluation.

- [#10278](https://github.com/emqx/emqx/pull/10278) Refactor the directory structure of all gateways.

- [#10306](https://github.com/emqx/emqx/pull/10306) Add support for `async` query mode for most bridges.

  Before this change, some bridges (Cassandra, MongoDB, MySQL, Postgres, Redis, RocketMQ, TDengine) were only allowed to be created with a `sync` query mode.

- [#10318](https://github.com/emqx/emqx/pull/10318) Now, the rule engine language's FROM clause supports both strings enclosed in double quotes (") and single quotes (').

- [#10336](https://github.com/emqx/emqx/pull/10336) Add `/rule_engine` API endpoint to manage configuration of rule engine.

### Bug Fixes

- [#10145](https://github.com/emqx/emqx/pull/10145) Fix `bridges` API to report error conditions for a failing bridge as
  `status_reason`. Also when creating an alarm for a failing resource we include
  this error condition with the alarm's message.

- [#10154](https://github.com/emqx/emqx/pull/10154) Change the default `resume_interval` for bridges and connectors to be
  the minimum of `health_check_interval` and `request_timeout / 3`.
  Also exposes it as a hidden configuration to allow fine tuning.

  Before this change, the default values for `resume_interval` meant
  that, if a buffer ever got blocked due to resource errors or high
  message volumes, then, by the time the buffer would try to resume its
  normal operations, almost all requests would have timed out.

- [#10172](https://github.com/emqx/emqx/pull/10172) Fix the incorrect default ACL rule, which was:

  ```
  {allow, {username, "^dashboard?"}, subscribe, ["$SYS/#"]}.
  ```

  However, it should use `{re, "^dashboard$"}` to perform a regular expression match:

  ```
  {allow, {username, {re,"^dashboard$"}}, subscribe, ["$SYS/#"]}.
  ```

- [#10174](https://github.com/emqx/emqx/pull/10174) Upgrade library `esockd` from 5.9.4 to 5.9.6.
  Fix an unnecessary error level logging when a connection is closed before proxy protocol header is sent by the proxy.

- [#10195](https://github.com/emqx/emqx/pull/10195) Add labels to API schemas where description contains HTML and breaks formatting of generated documentation otherwise.

- [#10196](https://github.com/emqx/emqx/pull/10196) Use lower-case for schema summaries and descritptions to be used in menu of generated online documentation.

- [#10209](https://github.com/emqx/emqx/pull/10209) Fix bug where a last will testament (LWT) message could be published
  when kicking out a banned client.

- [#10211](https://github.com/emqx/emqx/pull/10211) Hide `broker.broker_perf` config and API documents.
  The two configs `route_lock_type` and `trie_compaction` are rarely used and requires a full cluster restart to take effect. They are not suitable for being exposed to users.
  Detailed changes can be found here: https://gist.github.com/zmstone/01ad5754b9beaeaf3f5b86d14d49a0b7/revisions

- [#10225](https://github.com/emqx/emqx/pull/10225) Allow installing a plugin if its name matches the beginning of another (already installed) plugin name.
  For example: if plugin "emqx_plugin_template_a" is installed, it must not block installing plugin "emqx_plugin_template".

- [#10226](https://github.com/emqx/emqx/pull/10226) Don't crash on validation error in `/bridges` API, return `400` instead.

- [#10237](https://github.com/emqx/emqx/pull/10237) Ensure we return `404` status code for unknown node names in `/nodes/:node[/metrics|/stats]` API.

- [#10242](https://github.com/emqx/emqx/pull/10242) Fixed a log data field name clash.
  Piror to this fix, some debug logs may report a wrong Erlang PID which may affect troubleshooting session takeover issues.

- [#10251](https://github.com/emqx/emqx/pull/10251) Consider bridges referenced in `FROM` rule clauses as dependencies.

  Before this fix, when one tried to delete an ingress rule referenced in an action like `select * from "$bridges/mqtt:ingress"`, the UI would not trigger a warning about dependent rule actions.

- [#10257](https://github.com/emqx/emqx/pull/10257) Fixed the issue where `auto_observe` was not working in LwM2M Gateway.

  Before the fix, OBSERVE requests were sent without a token, causing failures
  that LwM2M clients could not handle.

  After the fix, LwM2M Gateway can correctly observe the resource list carried by
  client, furthermore, unknown resources will be ignored and printing the following
  warning log:

  ```
  2023-03-28T18:50:27.771123+08:00 [warning] msg: ignore_observer_resource, mfa: emqx_lwm2m_session:observe_object_list/3, line: 522, peername: 127.0.0.1:56830, clientid: testlwm2mclient, object_id: 31024, reason: no_xml_definition
  ```

- [#10286](https://github.com/emqx/emqx/pull/10286) Enhance logging behaviour during boot failure.
  When EMQX fails to start due to corrupted configuration files, excessive logging is eliminated and no crash dump file is generated.

- [#10297](https://github.com/emqx/emqx/pull/10297) Keeps `eval` command backward compatible with v4 by evaluating only Erlang expressions, even on Elixir node. For Elixir expressions, use `eval-ex` command.

- [#10300](https://github.com/emqx/emqx/pull/10300) Fixed an issue where a build made with Elixir could not receive uploaded plugins until the `plugins` folder was created manually to receive the uploaded files.

- [#10313](https://github.com/emqx/emqx/pull/10313) Ensure that when the core or replicant node starting, the `cluster-override.conf` file is only copied from the core node.
  Previously, when sorting nodes by startup time, the core node may have copied this file from the replicant node.

- [#10314](https://github.com/emqx/emqx/pull/10314) Fix /monitor_current API so that it only looks at the current node.
  Fix /stats API to not crash when one or more nodes in the cluster are down.

- [#10315](https://github.com/emqx/emqx/pull/10315) Fix crash checking `limit` and `page` parameters in `/mqtt/delayed/messages` API call.

- [#10317](https://github.com/emqx/emqx/pull/10317) Do not expose listener level authentications before extensive verification.

- [#10323](https://github.com/emqx/emqx/pull/10323) For security reasons, the value of the `password` field in the API examples is replaced with `******`.

- [#10327](https://github.com/emqx/emqx/pull/10327) Don't increment 'actions.failed.unknown' rule metrics counter upon receiving unrecoverable bridge errors.
  This counter is displayed on the dashboard's rule overview tab ('Action statistics' - 'Unknown').
  The fix is only applicable for synchronous bridges, as all rule actions for asynchronous bridges
  are counted as successful (they increment 'actions.success' which is displayed as 'Action statistics' - 'Success').

## 5.0.21

*发布日期: 2023-03-24*

### Enhancements

- [#10022](https://github.com/emqx/emqx/pull/10022) Start releasing Rocky Linux 9 (compatible with Enterprise Linux 9) and MacOS 12 packages

- [#10139](https://github.com/emqx/emqx/pull/10139) Add `extraVolumeMounts` to EMQX Helm Chart, it will have the ability to mount the user-own files into the EMQX instance, for example, ACL rule files as mentioned in [#9052](https://github.com/emqx/emqx/issues/9052)

  Done of [#10116](https://github.com/emqx/emqx/issues/10116)

- [#9893](https://github.com/emqx/emqx/pull/9893) When connecting with the flag `clean_start=false`, EMQX will filter out messages that published by banned clients.
  Previously, the messages sent by banned clients may still be delivered to subscribers in this scenario.

- [#9986](https://github.com/emqx/emqx/pull/9986) For helm charts, add MQTT ingress bridge; and removed stale `mgmt` references.

- [#10123](https://github.com/emqx/emqx/pull/10123) Improve the performance of `/bridges` API.
  Earlier, when the number of nodes in the cluster was large or the node was busy, the API may have a request timeout.

- [#9998](https://github.com/emqx/emqx/pull/9998) Redact the HTTP request body in the authentication error logs for security reasons.

### Bug Fixes

- [#10013](https://github.com/emqx/emqx/pull/10013) Fix return type structure for error case in API schema for `/gateways/:name/clients`.
  u
- [#10014](https://github.com/emqx/emqx/pull/10014) In dashboard API for `/monitor(_current)/nodes/:node` return `404` instead of `400` if node does not exist.

- [#10026](https://github.com/emqx/emqx/pull/10026) Metrics are now only exposed via the /bridges/:id/metrics endpoint. Metrics are no longer returned in other API operations such as getting the list of all bridges, or in the response when a bridge has been created.

- [#10027](https://github.com/emqx/emqx/pull/10027) Allow setting node name from `EMQX_NODE__NAME` when running in docker.
  Prior to this fix, only `EMQX_NODE_NAME` is allowed.

- [#10050](https://github.com/emqx/emqx/pull/10050) Ensure Bridge API returns `404` status code consistently for resources that don't exist.

- [#10052](https://github.com/emqx/emqx/pull/10052) Improve daemon mode startup failure logs.

  Before this change, it was difficult for users to understand the reason for EMQX 'start' command failed to boot the node.
  The only information they received was that the node did not start within the expected time frame,
  and they were instructed to boot the node with 'console' command in the hope of obtaining some logs.
  However, the node might actually be running, which could cause 'console' mode to fail for a different reason.

  With this new change, when daemon mode fails to boot, a diagnosis is issued. Here are the possible scenarios:

  - If the node cannot be found from `ps -ef`, the user is instructed to find information in log files `erlang.log.*`.
  - If the node is found to be running but not responding to pings, the user is advised to check if the host name is resolvable and reachable.
  - If the node is responding to pings, but the EMQX app is not running, it is likely a bug. In this case, the user is advised to report a Github issue.

- [#10055](https://github.com/emqx/emqx/pull/10055) Fix: configuration parameter `mqtt.max_awaiting_rel` had no effect.

- [#10056](https://github.com/emqx/emqx/pull/10056) Fix `/bridges` API status code.

  - Return `400` instead of `403` in case of removing a data bridge that is dependent on an active rule.
  - Return `400` instead of `403` in case of calling operations (start|stop|restart) when Data-Bridging is not enabled.

- [#10066](https://github.com/emqx/emqx/pull/10066) Improve error messages for `/briges_probe` and `[/node/:node]/bridges/:id/:operation` API calls to make them more readable. And set HTTP status code to `400` instead of `500`.

- [#10074](https://github.com/emqx/emqx/pull/10074) Check if type in `PUT /authorization/sources/:type` matches `type` given in body of request.

- [#10079](https://github.com/emqx/emqx/pull/10079) Fix description of `shared_subscription_strategy`.

- [#10085](https://github.com/emqx/emqx/pull/10085) Consistently return `404` for all requests on non existent source in `/authorization/sources/:source[/*]`.

- [#10098](https://github.com/emqx/emqx/pull/10098) A crash with an error in the log file that happened when the MongoDB authorization module queried the database has been fixed.

- [#10100](https://github.com/emqx/emqx/pull/10100) Fix channel crash for slow clients with enhanced authentication.
  Previously, when the client was using enhanced authentication, but the Auth message was sent slowly or the Auth message was lost, the client process would crash.

- [#10107](https://github.com/emqx/emqx/pull/10107) For operations on `bridges API` if `bridge-id` is unknown we now return `404`
  instead of `400`. Also a bug was fixed that caused a crash if that was a node
  operation. Additionally we now also check if the given bridge is enabled when
  doing the cluster operation `start` . Affected endpoints:

  - [cluster] `/bridges/:id/:operation`,
  - [node] `/nodes/:node/bridges/:id/:operation`, where `operation` is one of
    `[start|stop|restart]`.
    Moreover, for a node operation, EMQX checks if node name is in our cluster and
    return `404` instead of `501`.

- [#10117](https://github.com/emqx/emqx/pull/10117) Fix an error occurring when a joining node doesn't have plugins that are installed on other nodes in the cluster.
  After this fix, the joining node will copy all the necessary plugins from other nodes.

- [#10118](https://github.com/emqx/emqx/pull/10118) Fix problems related to manual joining of EMQX replicant nodes to the cluster.
  Previously, after manually executing joining and then leaving the cluster, the `replicant` node can only run normally after restarting the node after joining the cluster again.

  [Mria PR](https://github.com/emqx/mria/pull/128)

- [#10119](https://github.com/emqx/emqx/pull/10119) Fix crash when `statsd.server` is set to an empty string.

- [#10124](https://github.com/emqx/emqx/pull/10124) The default heartbeat period for MongoDB has been increased to reduce the risk of too excessive logging to the MongoDB log file.

- [#10130](https://github.com/emqx/emqx/pull/10130) Fix garbled config display in dashboard when the value is originally from environment variables.
  For example, `env EMQX_STATSD__SERVER='127.0.0.1:8124' . /bin/emqx start` results in unreadable string (not '127.0.0.1:8124') displayed in Dashboard's Statsd settings page.
  Related PR: [HOCON#234](https://github.com/emqx/hocon/pull/234).

- [#10132](https://github.com/emqx/emqx/pull/10132) Fix some error logs generated by `systemctl stop emqx` command.
  Prior to the fix, the command was not stopping jq and os_mon applications properly.

- [#10144](https://github.com/emqx/emqx/pull/10144) Add `-setcookie` emulator flag when invoking `emqx ctl` to prevent problems with emqx cli when home directory is read only. Fixes [#10142](https://github.com/emqx/emqx/issues/10142).

- [#10157](https://github.com/emqx/emqx/pull/10157) Fixed default rate limit configuration not being applied correctly when creating a new listener.

## 5.0.20

*发布日期: 2023-03-10*

### Enhancements

- [#10059](https://github.com/emqx/emqx/pull/10059) Errors returned by rule engine API are formatted in a more human readable way rather than dumping the raw error including the stacktrace.

### Bug Fixes

- [#10032](https://github.com/emqx/emqx/pull/10032) When resources on some nodes in the cluster are still in the 'initializing/connecting' state, the `bridges/` API will crash due to missing Metrics information for those resources. This fix will ignore resources that do not have Metrics information.

- [#10044](https://github.com/emqx/emqx/pull/10044) Fix node information formatter for stopped nodes in the cluster. The bug was introduced by v5.0.18.

- [#10054](https://github.com/emqx/emqx/pull/10054) Fix the problem that the obfuscated password is used when using the `/bridges_probe` API to test the connection in Data-Bridge.

- [#10058](https://github.com/emqx/emqx/pull/10058) Deprecate unused QUIC TLS options.
  Only following TLS options are kept for the QUIC listeners:

  - cacertfile
  - certfile
  - keyfile
  - verify

- [#10076](https://github.com/emqx/emqx/pull/10076) Fix webhook bridge error handling: connection timeout should be a retriable error.
  Prior to this fix, connection timeout was classified as unrecoverable error and led to request being dropped.

- [#10078](https://github.com/emqx/emqx/pull/10078) Fix an issue that invalid QUIC listener setting could casue segfault.

- [#10084](https://github.com/emqx/emqx/pull/10084) Fix problem when joining core nodes running different EMQX versions into a cluster.

  [Mria PR](https://github.com/emqx/mria/pull/127)

- [#10086](https://github.com/emqx/emqx/pull/10086) Upgrade HTTP client ehttpc to `0.4.7`.
  Prior to this upgrade, HTTP clients for authentication, authorization and webhook may crash
  if `body` is empty but content-type HTTP header is set.
  For more details see [ehttpc PR#44](https://github.com/emqx/ehttpc/pull/44).

## 5.0.19

*发布日期: 2023-03-01*

### Bug Fixes

- [#10032](https://github.com/emqx/emqx/pull/10032) When the resource manager is busy trying to establish a connection with the remote, the resource might yet lack any metrics information. Prior to this fix, the `bridges/` API handler crashed in such circumstances.

- [#10037](https://github.com/emqx/emqx/pull/10037) Fix Swagger API doc rendering crash.
  In version 5.0.18, a bug was introduced that resulted in duplicated field names in the configuration schema. This, in turn, caused the Swagger schema generated to become invalid.

- [#10041](https://github.com/emqx/emqx/pull/10041) For influxdb bridge, added integer value placeholder annotation hint to `write_syntax` documentation.
  Also supported setting a constant value for the `timestamp` field.

- [#10042](https://github.com/emqx/emqx/pull/10042) Improve behavior of the `replicant` nodes when the `core` cluster becomes partitioned (for example when a core node leaves the cluster).
  Previously, the replicant nodes were unable to rebalance connections to the core nodes, until the core cluster became whole again.
  This was indicated by the error messages: `[error] line: 182, mfa: mria_lb:list_core_nodes/1, msg: mria_lb_core_discovery divergent cluster`.

  [Mria PR](https://github.com/emqx/mria/pull/123/files)

- [#10043](https://github.com/emqx/emqx/pull/10043) Fixed two bugs introduced in v5.0.18.

  - The environment varialbe `SSL_DIST_OPTFILE` was not set correctly for non-boot commands.
  - When cookie is overridden from environment variable, EMQX node is unable to start.

- [#10044](https://github.com/emqx/emqx/pull/10044) Fix node information formatter for stopped nodes in the cluster.

## 5.0.18

*发布日期: 2023-02-24*

### Enhancements

- [#10019](https://github.com/emqx/emqx/pull/10019) Add low level tuning settings for QUIC listeners.

- [#9213](https://github.com/emqx/emqx/pull/9213) Add pod disruption budget to helm chart

- [#9949](https://github.com/emqx/emqx/pull/9949) QUIC transport Multistreams support and QUIC TLS cacert support.

- [#9967](https://github.com/emqx/emqx/pull/9967) New common TLS option 'hibernate_after' to reduce memory footprint per idle connecion, default: 5s.

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

- [#9939](https://github.com/emqx/emqx/pull/9939) Allow 'emqx ctl cluster' command to be issued before Mnesia starts.
  Prior to this change, EMQX `replicant` could not use `manual` discovery strategy.
  Now it's possible to join cluster using 'manual' strategy.

- [#9958](https://github.com/emqx/emqx/pull/9958) Fix bad http response format when client ID is not found in `clients` APIs

- [#9961](https://github.com/emqx/emqx/pull/9961) Avoid parsing config files for node name and cookie when executing non-boot commands in bin/emqx.

- [#9974](https://github.com/emqx/emqx/pull/9974) Report memory usage to statsd and prometheus using the same data source as dashboard.
  Prior to this fix, the memory usage data source was collected from an outdated source which did not work well in containers.

- [#9978](https://github.com/emqx/emqx/pull/9978) Fixed configuration issue when choosing to use SSL for a Postgres connection (`authn`, `authz` and bridge).
  The connection could fail to complete with a previously working configuration after an upgrade from 5.0.13 to newer EMQX versions.

- [#9997](https://github.com/emqx/emqx/pull/9997) Fix Swagger API schema generation. `deprecated` metadata field is now always boolean, as [Swagger specification](https://swagger.io/specification/) suggests.

## 5.0.17

*发布日期: 2023-02-13*

### Enhancements

- [#9802](https://github.com/emqx/emqx/pull/9802) Support HAProxy protocol for HTTP API.

- [#9871](https://github.com/emqx/emqx/pull/9871) Allow the placeholder to be anywhere in the topic for `authz` rules.
  e.g:
  `{allow, {username, "who"}, publish, ["t/foo${username}boo/${clientid}xxx"]}.`

- [#9910](https://github.com/emqx/emqx/pull/9910) Add `start` operation to bridges API to allow manual reconnect after failure.

- [#9917](https://github.com/emqx/emqx/pull/9917) Stop building -alpine docker image because it's size is larger than the regular one based on debian slim

- [#9930](https://github.com/emqx/emqx/pull/9930) Expose the stats `live_connections.count` and `live_connections.max` to Prometheus.

- [#9936](https://github.com/emqx/emqx/pull/9936) Disable disksup (part of os_mon) in releases by default, no warnings are issued when a disk error occurs.

- [#9954](https://github.com/emqx/emqx/pull/9954) Improve bridge performance

### Bug fixes

- [#9864](https://github.com/emqx/emqx/pull/9864) Fix the exclusive topics aren't removed when the session has already been cleaned.

- [#9875](https://github.com/emqx/emqx/pull/9875) Return `400` if a broken plugin package is uploaded from HTTP API, also cleanup if plugin is not accepted.

- [#9916](https://github.com/emqx/emqx/pull/9916) Fix MQTT bridge fails to verify TLS wildcard server certificate.

- [#9922](https://github.com/emqx/emqx/pull/9922) Fix the issue with the bridge resource buffer where it might become stuck if enough async queries fill the inflight window full before failing with retryable errors.

- [#9923](https://github.com/emqx/emqx/pull/9923) Fix REPORT_CB/2 CRASH error logs when errors happen during boot-up or shutdown.

- [#9938](https://github.com/emqx/emqx/pull/9938) Report some egress MQTT bridge errors as recoverable, and thus retryable.

- [#9946](https://github.com/emqx/emqx/pull/9946) Add back `reconnect_interval` as deprecated field for MQTT bridge.
  The field was removed from v5.0.16/e5.0.0 by mistake, caused new version unable to start on old config.
  Now it's added back as deprecated (config value is ignored if provided).

- [#9951](https://github.com/emqx/emqx/pull/9951) Propagate errors from operations (`start|stop|restart`) on bridges API if called for all nodes.

- [#9952](https://github.com/emqx/emqx/pull/9952) Disallow subscribing with QoS 2 for ingress MQTT bridges.
  Allow user to configure `clean_start` option for ingress MQTT bridges, however.

## 5.0.16

*发布日期: 2023-02-02*

### Bug fixes

- [#9824](https://github.com/emqx/emqx/pull/9824) The `topics/{topic}` API endpoint would return `500 - Internal Error` if a topic had multiple routes. This is fixed by returning a list of routes.

- [#9832](https://github.com/emqx/emqx/pull/9832) Improve error log when bridge in 'sync' mode timed out to get response.

- [#9834](https://github.com/emqx/emqx/pull/9834) Allow `mqtt.idle_timeout` to be set to `infinity`

- [#9839](https://github.com/emqx/emqx/pull/9839) Make sure that the content of an authorization header that users have specified for a webhook bridge is not printed to log files.

- [#9884](https://github.com/emqx/emqx/pull/9884) Do not resume all buffer workers on successful health check of any individual resource.
  Previously after any successful healthcheck, all buffer workers (for all resources) were resumed

## 5.0.15

*发布日期: 2023-01-20*

### Enhancements

- [#9569](https://github.com/emqx/emqx/pull/9569) Refactor `/authorization/sources/built_in_database/` by adding `rules/` to the path.

- [#9585](https://github.com/emqx/emqx/pull/9585) `/bridges_probe` API endpoint to test params for creating a new data bridge.

- [#9586](https://github.com/emqx/emqx/pull/9586) Basic auth is no longer allowed for API calls, must use API key instead.

- [#9628](https://github.com/emqx/emqx/pull/9628) Expose additional resource configuration parameters: `start_after_created` and `start_timeout`.

- [#9722](https://github.com/emqx/emqx/pull/9722) Add the following configuration options for Pushing metrics to Prometheus Push Gateway:

  - `headers`: Allows custom HTTP request headers.
  - `job_name`: allows to customize the name of the Job pushed to Push Gateway.

- [#9725](https://github.com/emqx/emqx/pull/9725) Remove the config `auto_reconnect` from the emqx_authz, emqx_authn and data-bridge componets.
  This is because we have another config with similar functions: `resource_opts.auto_restart_interval`。

  The functions of these two config are difficult to distinguish, which will lead to confusion.
  After this change, `auto_reconnect` will not be configurable (always be true), and the underlying
  drivers that support this config will automatically reconnect the abnormally disconnected
  connection every `2s`.

  And the config `resource_opts.auto_restart_interval` is still available for user.
  It is the time interval that emqx restarts the resource when the connection cannot be
  established for some reason.

- [#9736](https://github.com/emqx/emqx/pull/9736) Refactor of /bridges API to make it more consistent with other APIs:

  - bridge enable/disable is now done via the endpoint `/bridges/{id}/enable/[true,false]`
  - `/bridges/{id}/operation/{operation}` endpoints are now `/bridges/{id}/{operation}`
  - metrics are moved out from the GET `/bridges/{id}` response and can now be fetched via `/bridges/{id}/metrics`
  - the `bridges/{id}/reset_metrics` endpoint is now `/bridges/{id}/metrics/reset`

- [#9774](https://github.com/emqx/emqx/pull/9774) Add a password complexity requirement when adding or modifying Dashboard users via the API.
  Now password must contain at least 2 of alphabetic, numeric and special characters,
  and must be 8 to 64 characters long.

### Bug fixes

- [#9626](https://github.com/emqx/emqx/pull/9626) Return authorization settings with default values.
  The authorization cache is enabled by default, but due to the missing default value in `GET` response of `/authorization/settings`, it seemed to be disabled from the dashboard.

- [#9680](https://github.com/emqx/emqx/pull/9680) Fix the problem that username and password authentication is mandatory in Influxdb v1 write API.

- [#9726](https://github.com/emqx/emqx/pull/9726) Client fuzzy search API results were missing information which could tell if more results are available in the next pages, this is now fixed by providing `hasnext` flag in the response.

- [#9735](https://github.com/emqx/emqx/pull/9735) Password information has been removed from information log messages for http, ldap, mongo, mqtt, mysql, pgsql and redis.

- [#9748](https://github.com/emqx/emqx/pull/9748) Listeners not configured with `max_connections` will cause the cluster `/listeners` API to return 500 error.

- [#9749](https://github.com/emqx/emqx/pull/9749) In some cases search APIs could respond with an incorrect `count` value in the metadata, that is usually much bigger than expected, this is now fixed.

- [#9750](https://github.com/emqx/emqx/pull/9750) Reload overriding configs after boot.
  Prior to this change, two configs were allow to change from dashboard, but will not take effect after reboot:

  - Logging (such as level)
  - Prometheus configs

- [#9751](https://github.com/emqx/emqx/pull/9751) Fix that obsoleted cert file will not be deleted after the listener is updated/deleted

- [#9763](https://github.com/emqx/emqx/pull/9763) Fix an authentication exception when password is not provided

- [#9765](https://github.com/emqx/emqx/pull/9765) Parse decimals as password from environment variable overrides correctly.
  Prior to this change, config values for passwords are not allowed to be decimals.
  e.g. `EMQX_FOOBAR__PASSWORD=12344` or `emqx.foobar.password=1234`
  would result in a type check error, unless quoted as:
  `EMQX_FOOBAR__PASSWORD='"12344"'` or `emqx.foobar.password="1234"`.
  After this fix, the value does not have to be quoted.

- [#9769](https://github.com/emqx/emqx/pull/9769) Fix Erlang shell prompt version prefix. e5.0.15 -> v5.0.15

- [#9780](https://github.com/emqx/emqx/pull/9780) When creating disk queue directory for resource worker, substitute ':' with '-' in worker id.

- [#9781](https://github.com/emqx/emqx/pull/9781) Trace files were left on a node when creating a zip file for download. They are now removed when the file is sent. Also, concurrent downloads will no longer interfere with each other.

- [#9785](https://github.com/emqx/emqx/pull/9785) Stop authentication hook chain if `emqx_authentication` provides a definitive result.

- [#9787](https://github.com/emqx/emqx/pull/9787) Fix a compatible problem for the `webhook` bridge configuration which was created before the v5.0.12.

## 5.0.14

*发布日期: 2023-01-11*

### Enhancements

- https://github.com/emqx/emqx/pull/8329 The MongoDB library has been upgraded to support MongoDB 5.1+

- https://github.com/emqx/emqx/pull/9593 Obfuscated sensitive data in the response when querying bridges information by API.

- https://github.com/emqx/emqx/pull/9614 Make it possible to configure host:port from environment variables without quotes.
  Prior to this change, when overriding a host:port config value from the environment variable, one has to quote it as:
  env EMQX_BRIDGES**MQTT**XYZ**SERVER='"localhost:1883"'.
  Now it's possible to set it without quote as env EMQX_BRIDGES**MQTT**XYZ**SERVER='localhost:1883'.

- https://github.com/emqx/emqx/pull/9642 Deprecates enable_batch and enable_queue options for bridges/resources. After this change, queuing is always enabled for bridges, and batching is controlled by the batch_size option: batch_size > 1 means batching will be enabled.

- https://github.com/emqx/emqx/pull/9671 Implement sliding window average metrics.

- https://github.com/emqx/emqx/pull/9674 Made rule engine behavior more consistent with bridge behavior regarding metrics: if a rule engine is disabled, its metrics are now reset.

- https://github.com/emqx/emqx/pull/9675 HTTP client library ehttpc upgraded from 0.4.2 to 0.4.3.
  Library eredis_cluster which manages clients to Redis clusters upgraded from 0.7.1 to 0.7.5.

- https://github.com/emqx/emqx/pull/9713 Introduce api_key.bootstrap_file to initialize the api key at boot time.
  Deprecate dashboard.bootstrap_users_file.
  Limit the maximum number of api keys to 100 instead of 30.

### Bug fixes

- https://github.com/emqx/emqx/pull/8648 When deleting a non-existing bridge the server gave a successful response. This has been fixed so that the server instead gives an error response when the user attempts to delete a non-existing bridge.

- https://github.com/emqx/emqx/pull/9637 Fix the expiry_interval fields of the clients HTTP API to measure in seconds.

- https://github.com/emqx/emqx/pull/9638 Fix the problem of data loss and bad match when the MySQL driver is disconnected.

- https://github.com/emqx/emqx/pull/9641 Fix an issue where testing the GCP PubSub could leak memory and an issue where its JWT token would fail to refresh a second time.

- https://github.com/emqx/emqx/pull/9642 Fix some issues that could lead to wrong bridge metrics.
  Fix an issue that could lead to message loss and wrong metrics with the Kafka Producer bridge when Kafka or the connection to it is down.
  Fix some issues that could lead to the same message being delivered more than once when using batching for bridges and when the batch was retried.

- https://github.com/emqx/emqx/pull/9667 Remove the possibility to set clientid for /publish and /publish/bulk HTTP APIs. This is to reduce the risk of security confusion.

- https://github.com/emqx/emqx/pull/9687 Fix the problem that sending messages to data-bridges failed because of incorrect handling of some data-bridges without local_topic field configured.
  Before this change, if some bridges have configured the local_topic field but others have not, a function_clause error will occur when forwarding messages to the data-bridges.

- https://github.com/emqx/emqx/pull/9689 Fix handling of HTTP authorization result when a request failure (e.g.: HTTP resource is down) would cause a function_clause error.

- https://github.com/emqx/emqx/pull/9703 Set the default value of the qos field of the HTTP API /clients/:clientid/subscribe to 0.
  Before this fix, the qos field has no default value, which leads to a function_clause error
  when querying this API.

- https://github.com/emqx/emqx/pull/9705 Remove the default value of Webhook.
  Before this repair, the default value of the body field of Webhook is ${payload},
  but there is no payload field in the available fields of other events except the message
  publishing in the rule, so in this case, the webhook will send a string with the
  message body as "undefined" to the HTTP service.
  This fix removes the default value of the body field. When the body field is
  not configured, Webhook will send all available fields of the current event in
  the format of a JSON object.

- https://github.com/emqx/emqx/pull/9712 Fixed the problem of '404 Not Found' when calling the HTTP API '/clients/:clientid/subscribe/bulk'
  from the plug-ins and data-bridges on handling the 'client.connected' event.

- https://github.com/emqx/emqx/pull/9714 Fix /mqtt/auto_subscribe API's bad swagger schema, and make sure swagger always checks if the schema is correct.

- https://github.com/emqx/emqx/pull/9716 MQTT bridge config compatibility fix. The config created before v5.0.12 may encounter a compatibility issue after upgrading to v5.0.13.

- https://github.com/emqx/emqx/pull/9717 Prior to this fix, if it always times out when trying to connect a bridge server, it's not possible to change other configs even when the bridge is disabled.

## 5.0.13

*发布日期: 2022-12-27*

### Enhancements

- Add `limiter` update API [#9133](https://github.com/emqx/emqx/pull/9133).

- Avoid creating temporary zip files when syncing data directory during cluster startup [#9429](https://github.com/emqx/emqx/pull/9429).

- Refactor: move `/mqtt/sys_topics` to generic `/configs/sys_topics` [#9511](https://github.com/emqx/emqx/pull/9511).

- Refactor: use `POST` not `PUT` for `/users/{name}/change_pwd` [#9533](https://github.com/emqx/emqx/pull/9533).

- Add compression functions `zip`, `gzip`, `zip_compress` in Rule-Engine and corresponding decompression functions [#9573](https://github.com/emqx/emqx/pull/9573).

- Return `204` instead of `200` for `PUT /authenticator/:id` [#9434](https://github.com/emqx/emqx/pull/9434/).

- Added the option to customize the clientid prefix of egress MQTT bridges. [#9609](https://github.com/emqx/emqx/pull/9609)

- Ensure the default expiration time of `banned` is large enough [#9599](https://github.com/emqx/emqx/pull/9599/).

### Bug fixes

- Trigger `message.dropped` hook when QoS2 message is resend by client with a same packet id, or 'awaiting_rel' queue is full [#9487](https://github.com/emqx/emqx/pull/9487).

- Fix shared subscription 'sticky' strategy [#9578](https://github.com/emqx/emqx/pull/9578).
  Prior to this change, a 'sticky' subscriber may continue to receive messages after unsubscribing.

- Add check to ensure that a given key is among the prepared statements on query in the mysql connector [#9571](https://github.com/emqx/emqx/pull/9571).

- Fix password leak to logs for connectors [#9608](https://github.com/emqx/emqx/pull/9608).

## 5.0.12

*发布日期: 2022-12-14*

### Highlights

- This version included a refactoring of MQTT bridge config. The older version config file created from v5.0.11 or earlier will be converted to according to the new schema. Please note though, the request body of `/bridges` API to configure MQTT brdige is changed in a incompatible way.
- Start releasing packages for Apple M1/M2 (MacOS-12).
- Start releasing packages for Amazon Linux 2 (e.g. emqx-5.0.12-amzn2-amd64.rpm).
- Retained message index performance improvement.

### Enhancements

- Disable global garbage collection by `node.global_gc_interval = disabled` [#9418](https://github.com/emqx/emqx/pull/9418)。

- Improve the CLI to avoid waste atom table when typing erros [#9416](https://github.com/emqx/emqx/pull/9416).

- Start building MacOS packages for Apple Silicon hadrdware [#9423](https://github.com/emqx/emqx/pull/9423).

- Remove support for setting shared subscriptions using the non-standard `$queue` feature [#9412](https://github.com/emqx/emqx/pull/9412).
  Shared subscriptions are now part of the MQTT spec. Use `$share` instead.

- Refactor authn API by replacing `POST /authentication/{id}/move` with `PUT /authentication/{id}/position/{position}`. [#9419](https://github.com/emqx/emqx/pull/9419).
  Same is done for `/listeners/{listener_id}/authentication/id/...`.

- Redesign `/rules` API to make `metrics` a dedicated resources rather than being included with every response [#9461](https://github.com/emqx/emqx/pull/9461).

- Add more PSK ciphers support [#9505](https://github.com/emqx/emqx/pull/9505).

- Improve `emqx_retainer` write performance: get rid of transactions on write [#9372](https://github.com/emqx/emqx/pull/9372).

- HTTP client library `ehttpc` upgraded from `0.4.0` to `0.4.2` [#9520](https://github.com/emqx/emqx/pull/9520).

- Add `handshake_timeout` option to MQTT SSL listener [#9502](https://github.com/emqx/emqx/pull/9502).

- Upgrade dashboard to [v1.1.3](https://github.com/emqx/emqx-dashboard-web-new/releases/tag/v1.1.3).

- Users can define the `externalTrafficPolicy` of service in EMQX Helm Chart [#9527](https://github.com/emqx/emqx/pull/9527).

- Return `204` instead of `200` for `POST /gateway/lwm2m/clients/{clientid}/{read,write,observe}` [#9480](https://github.com/emqx/emqx/pull/9480).

- Make possible to create an authentication entirely from environment variable [#9547](https://github.com/emqx/emqx/pull/9547).
  As an example, one can now enable MySQL auth with:
  `env EMQX_AUTHENTICATION__1='{mechanism="password_based",backend="mysql",server="localhost:3306",database="emqx",username="emqx",password="******",query="SELECT password_hash,salt FROM mqtt_user WHERE username=${username} LIMIT 1",enable=true}'`.
  Prior to this change, overrides only work on top of existing authentication, for example, if there is already MySQL auth configured in `emqx.conf`
  but we want to disable it, we can do it with `env EMQX_AUTHENTICATION__1__ENABLE=false`.

- Start building packages for Amazon Linux 2 [#9537](https://github.com/emqx/emqx/pull/9537).

### Bug fixes

- Fix that the obsolete SSL files aren't deleted after the ExHook config update [#9432](https://github.com/emqx/emqx/pull/9432).

- Fix doc and schema for `/trace` API [#9468](https://github.com/emqx/emqx/pull/9468).

- Return `404` for `/telemetry/data` in case it's disabled [#9464](https://github.com/emqx/emqx/pull/9464).

- Fix some potential MQTT packet parse errors [#9477](https://github.com/emqx/emqx/pull/9477).

- Fixed EMQX Helm Chart deployment error [#9509](https://github.com/emqx/emqx/pull/9509).

  - Fixed the `Discovery error: no such service` error occurred during helm chart deployment, resulting in an abnormal discovery of cluster nodes.
  - Fixed issue that caused EMQX Helm Chart to fail when modifying some of EMQX's configuration items via environment variables.

- Fix shadowing `'client.authenticate'` callbacks by `emqx_authenticator`. Now `emqx_authenticator`
  passes execution to the further callbacks if none of the authenticators matches [#9496](https://github.com/emqx/emqx/pull/9496).

- Return `400` if query param `node` is not a known node in `/trace/:id/download?node={node}` [#9478](https://github.com/emqx/emqx/pull/9478).

- `POST /traces` to return `409` in case of duplicate [#9494](https://github.com/emqx/emqx/pull/9494).

- Fix bridging function, when both ingress and egress bridges are configured, egress bridge does not work [#9523](https://github.com/emqx/emqx/pull/9523).

- Fix EMQX Helm Chart using incorrect secret values when custom credentials are provided [#9536](https://github.com/emqx/emqx/pull/9536).

## 5.0.11

*发布日期: 2022-11-27*

### Enhancements

- Security enhancement for retained messages [#9326](https://github.com/emqx/emqx/pull/9326).
  The retained messages will not be published if the publisher client is banned.

- Security enhancement for the `subscribe` API [#9355](https://github.com/emqx/emqx/pull/9355).

- Enhance the `banned` feature [#9367](https://github.com/emqx/emqx/pull/9367).
  Now the corresponding session will be kicked when client is banned by `clientid`.

- Redesign `/gateways` API [9364](https://github.com/emqx/emqx/pull/9364).
  Use `PUT /gateways/{name}` instead of `POST /gateways`, gateway gets 'loaded'
  automatically if needed. Use `PUT /gateways/{name}/enable/{true|false}` to
  enable or disable gateway. No more `DELETE /gateways/{name}`.

- Support `statsd {tags: {"user-defined-tag" = "tag-value"}` configure and improve stability of `emqx_statsd` [#9363](http://github.com/emqx/emqx/pull/9363).

- Improve node name generation rules to avoid potential atom table overflow risk [#9387](https://github.com/emqx/emqx/pull/9387).

- Set the default value for the maximum level of a topic to 128 [#9406](https://github.com/emqx/emqx/pull/9406).

- Keep MQTT v5 User-Property pairs from bridge ingested MQTT messsages to bridge target [#9398](https://github.com/emqx/emqx/pull/9398).

### Bug fixes

- Fix `ssl.existingName` option of helm chart not working [#9307](https://github.com/emqx/emqx/issues/9307).

- Fix create trace sometime failed by end_at time has already passed. [#9303](https://github.com/emqx/emqx/pull/9303)

- Return 404 for status of unknown authenticator in `/authenticator/{id}/status` [#9328](https://github.com/emqx/emqx/pull/9328).

- Fix that JWT ACL rules are only applied if an `exp` claim is set [#9368](https://github.com/emqx/emqx/pull/9368).

- Fix that `/configs/global_zone` API cannot get the default value of the configuration [#9392](https://github.com/emqx/emqx/pull/9392).

- Fix mountpoint not working for will-msg [#9399](https://github.com/emqx/emqx/pull/9399).

## 5.0.10

*发布日期: 2022-11-09*

Release had to be recreated due to an issue in GitHub action which failed to upload/publish packages.
Previous commit: 34a6c6c88

### Enhancements

- Improve `/nodes` API responsiveness [#9221](https://github.com/emqx/emqx/pull/9221).

- Improve the integration of the `banned` and the `delayed` feature [#9326](https://github.com/emqx/emqx/pull/9326).
  Now when publishing a delayed message will check first if its source client is banned, if true, this publish will be ignored.

- Update `gen_rpc` library to version 3.0 [#9187](https://github.com/emqx/emqx/pull/9187).

- Improve memory usage on core nodes when bootstrapping a replicant [#9236](https://github.com/emqx/emqx/pull/9236).

- Improve stability of Prometheus Push Gateway and log errors when POST fails [#9235](http://github.com/emqx/emqx/pull/9235).

- Now it is possible to opt out VM internal metrics in prometheus stats [#9222](https://github.com/emqx/emqx/pull/9222).
  When system load is high, reporting too much metrics data may cause the prometheus stats API timeout.

- Improve security when converting types such as `binary` `lists` to `atom` types [#9279](https://github.com/emqx/emqx/pull/9279), [#9286](https://github.com/emqx/emqx/pull/9286).

- Add `/trace/:name/log_detail` HTTP API to return trace file's size and mtime [#9152](https://github.com/emqx/emqx/pull/9152).

- Add `/status` HTTP API endpoint to api documentation [#9230](https://github.com/emqx/emqx/pull/9230).

- Binary packages for all platforms are now built on Erlang/OTP version 24.3.4.2 [#9293](https://github.com/emqx/emqx/pull/9293).

### Bug fixes

- Fix error log message when `mechanism` is missing in authentication config [#8924](https://github.com/emqx/emqx/pull/8924).

- Fix HTTP 500 issue when unknown `status` parameter is used in `/gateway` API call [#9225](https://github.com/emqx/emqx/pull/9225).

- Fixed the HTTP response status code for the `/status` endpoint [#9211](https://github.com/emqx/emqx/pull/9211).
  Before the fix, it always returned `200` even if the EMQX application was not running. Now it returns `503` in that case.

- Fix message delivery related event encoding [#9228](https://github.com/emqx/emqx/pull/9228).
  This bug was introduced in v5.0.9. For Rule-Engine's input events like `$events/message_delivered`
  and `$events/message_dropped`, if the message was delivered to a shared-subscription,
  the encoding (to JSON) of the event will fail.

- Fix bad HTTP response status code for `/gateways` API, when Gateway name is unknown, it should return `404` instead of `400` [#9268](https://github.com/emqx/emqx/pull/9268).

- Fix incorrect topic authorize checking of delayed messages [#9290](https://github.com/emqx/emqx/pull/9290).
  Now will determine the actual topic of the delayed messages, e.g. `$delayed/1/t/foo` will be treated as `t/foo` in authorize checks.

- Add property `code` to error response for `/authentication/sources/:type` [9299](https://github.com/emqx/emqx/pull/9299).

- Align documentation for `/authentication/sources` with what we actually send [9299](https://github.com/emqx/emqx/pull/9299).

- Fix query string parameter 'node' to `/configs` resource being ignored, return 404 if node does not exist [#9310](https://github.com/emqx/emqx/pull/9310/).

- Avoid re-dispatching shared-subscription session messages when a session is kicked or taken-over (to a new session) [#9123](https://github.com/emqx/emqx/pull/9123).

## 5.0.9

*发布日期: 2022-10-24*

### Enhancements

- Add `cert_common_name` and `cert_subject` placeholder support for authz_http and authz_mongo [#8973](https://github.com/emqx/emqx/pull/8973).

- Use milliseconds internally in emqx_delayed to store the publish time, improving precision [#9060](https://github.com/emqx/emqx/pull/9060).

- More rigorous checking of flapping to improve stability of the system [#9136](https://github.com/emqx/emqx/pull/9136).

- No message(s) echo for the message publish APIs [#9155](https://github.com/emqx/emqx/pull/9155).
  Prior to this fix, the message publish APIs (`api/v5/publish` and `api/v5/publish/bulk`) echos the message back to the client in HTTP body.
  This change fixed it to only send back the message ID.

### Bug fixes

- Check ACLs for last will testament topic before publishing the message [#8930](https://github.com/emqx/emqx/pull/8930).

- Fix GET /listeners API crash when some nodes (in a cluster) is still loading the configs [#9002](https://github.com/emqx/emqx/pull/9002).

- Fix empty variable interpolation in authentication and authorization [#8963](https://github.com/emqx/emqx/pull/8963).
  Placeholders for undefined variables are rendered now as empty strings and do not cause errors anymore.

- Fix the latency statistics error of the slow subscription stats [#8986](https://github.com/emqx/emqx/pull/8986).
  Prior to this change when `stats_type` is `internal` or `response`, the begin time stamp was taken at wrong precision.

- Fix shared subscription message re-dispatches [#9104](https://github.com/emqx/emqx/pull/9104).

  - When discarding QoS 2 inflight messages, there were excessive logs
  - For wildcard deliveries, the re-dispatch used the wrong topic (the publishing topic,
    but not the subscribing topic), caused messages to be lost when dispatching.

- Upgrade http client `gun` from 1.3.7 to [1.3.9](https://github.com/emqx/gun/tree/1.3.9)
  Prior to this fix, long-lived HTTPS connections for HTTP auth or webhook integrations
  may stall indefinitely, causing massive timeouts for HTTP requests.

## 5.0.8

*发布日期: 2022-09-17*

### Changes Worth Mentioning

### Enhancements

- An independent RPC implementation is used between nodes to forward shared subscription messages instead of Erlang's own RPC to reduce the cluster pressure when the shared subscription load is high. [#8893](https://github.com/emqx/emqx/pull/8893)
- Print a warning message when boot with the default (insecure) Erlang cookie. [#8905](https://github.com/emqx/emqx/pull/8905)
- The configuration in `local-override.conf` will not allow synchronous updates in the cluster to the entire cluster at runtime. [#8851](https://github.com/emqx/emqx/pull/8851)
- Add `POST /listeners` interface for creating listeners. [#8876](https://github.com/emqx/emqx/pull/8876)
- Change `/gateway` API path to plural form. [#8823](https://github.com/emqx/emqx/pull/8823)
- The `exp`, `nbf` and `iat` claims in JWT authentication support non-integer timestamps. [#8867](https://github.com/emqx/emqx/pull/8867)
- Improve the request performance of ExProto and gRPC Server. [#8866](https://github.com/emqx/emqx/pull/8866)

### Bug Fixes

- Fix the issue that password authentication using Redis as the data source directly terminates the authentication when no authentication data is retrieved. [#8934](https://github.com/emqx/emqx/pull/8934)
- Fix inaccurate delayed publish due to OS time changes. [#8926](https://github.com/emqx/emqx/pull/8926)
- Fix the issue that EMQX could not be started after disabling the retained message feature. [#8911](https://github.com/emqx/emqx/pull/8911)
- Fix the slow response of updating the configuration when a node in the cluster is down. [#8857](https://github.com/emqx/emqx/pull/8857)
- Fix the issue that the authorization would terminate the execution of the `client.authorize` hook when no rules were matched. [#8780](https://github.com/emqx/emqx/pull/8780)
- Fix the issue that Payload must be configured in MQTT Bridge. [#8949](https://github.com/emqx/emqx/pull/8949)
- Fix the issue that the log directory cannot be configured through environment variables. [#8892](https://github.com/emqx/emqx/pull/8892)
- Fix the issue that the CoAP gateway introduced an extra `/` prefix when parsing topics. [#8658](https://github.com/emqx/emqx/pull/8658)
- Fix the issue that MQTT Bridge returned the content of the TLS file in the API response. [#8872](https://github.com/emqx/emqx/pull/8872), [#8958](https://github.com/emqx/emqx/pull/8958)
- Fix the issue that client authentication failure would trigger the release of will messages. [#8887](https://github.com/emqx/emqx/pull/8887)
- Fix ExProto's imperfect Keep Alive check mechanism that could cause the client to never expire. [#8866](https://github.com/emqx/emqx/pull/8866)

### Dependency Upgrades

- `grpc-erl` upgrades from `0.6.6` to `0.6.7`, [#8866](https://github.com/emqx/emqx/pull/8866)

## 5.0.7

*发布日期: 2022-09-01*

### Enhancements

- Simplify TLS cipher suite configuration
- Add confirmation before listener closes on Dashboard
- Unify the configuration of TLS on Dashboard
- Supports viewing EMQX version and node role on Dashboard overview page
- Increase restrictions on plugin file types

### Bug fixes

- Fix the issue that Mria transactions of replicated nodes could not be executed when nodes of mixed versions formed a cluster
- Fix the issue that the Authorization settings page of Dashboard could not display data
- Fix the issue that the monitoring topic data could not be reset on Dashboard

### Other changes

- Remove will message related fields in the response of the client query interface

## 5.0.6

*发布日期: 2022-08-22*

### Bug fixes

- Fix incorrect display of node status on Dashboard

## 5.0.4

*发布日期: 2023-05-26*

#### 增强功能

- [#10389](https://github.com/emqx/emqx/pull/10389) 统一 `cluster.core_nodes` 和 `cluster.statics.seeds` 配置格式，同时支持数组 `["emqx1@127.0.0.1", "emqx2@127.0.0.1"]` 或逗号分隔的字符串 `"emqx1@127.0.0.1,emqx2@127.0.0.1"` 两种格式。

- [#10392](https://github.com/emqx/emqx/pull/10392) 新增函数 date_to_unix_ts/3，用于将格式化的日期转换成整数类型的时间戳：

  `date_to_unix_ts(TimeUnit, FormatString, InputDateTimeString)`

- [#10426](https://github.com/emqx/emqx/pull/10426) 优化配置优先级机制，修复重启 EMQX 后 `etc/emqx.conf` 中的配置不生效的问题。有关配置优先级请参考[配置重写规则](https://docs.emqx.com/zh/enterprise/v5.0/configuration/configuration.html#%E9%85%8D%E7%BD%AE%E8%A6%86%E7%9B%96%E8%A7%84%E5%88%99)

- [#10457](https://github.com/emqx/emqx/pull/10457) 移除 StatsD 指标监控功能。

- [#10458](https://github.com/emqx/emqx/pull/10458) 调整插件配置在示例配置文件中的位置，大多数情况下用户都在 Dashboard 上管理插件，不需要手动修改配置，因此我们将插件配置调整到更靠后的位置。

- [#10491](https://github.com/emqx/emqx/pull/10491) 将 `etcd.ssl` 重命名为 `etcd.ssl_options` ，使其与配置文件中其他 SSL 相关配置项保持一致的命名。

- [#10512](https://github.com/emqx/emqx/pull/10512) 改进了数据文件存储格式，现已支持正常存储 Unicode 字符，例如包含 Unicode 字符的规则 SQL `SELECT * FROM "t/1" WHERE clientid = "-测试专用-"`。

- [#10568](https://github.com/emqx/emqx/pull/10568) 为 `emqx ctl listeners` 命令返回结果添加连接关闭计数指标 `shutdown_count`。

- [#10588](https://github.com/emqx/emqx/pull/10588) 将日志跟踪记录的时间精度从秒提高到微秒。例如，从 `2023-05-02T08:43:50+00:00` 到 `2023-05-02T08:43:50.237945+00:00` 。

- [#10623](https://github.com/emqx/emqx/pull/10623) 在 `force_shutdown` 配置中，将 `max_message_queue_len` 重命名为 `max_mailbox_size` ，旧名称作为别名被保留，保证向后兼容性。

- [#10713](https://github.com/emqx/emqx/pull/10713) 为减少歧义与配置复杂度，隐藏 WebHook `resource_option.request_timeout` 配置项，并使用 `http` `request_timeout` 来设置该值。

- [#10075](https://github.com/emqx/emqx/pull/10075) 新增节点再平衡/节点疏散功能，功能设计与使用请参考 [EIP 文档](https://github.com/emqx/eip/blob/main/active/0020-node-rebalance.md)。

- [#10378](https://github.com/emqx/emqx/pull/10378) 新增 Pulsar 数据桥接，目前仅支持单向数据集成即 Pulsar 生产者角色。

- [#10408](https://github.com/emqx/emqx/pull/10408) 规则 SQL 新增三个内置函数，用于写入日期类型的值到 MongoDB 中。

- [#10409](https://github.com/emqx/emqx/pull/10409) [#10337](https://github.com/emqx/emqx/pull/10337) 规则引擎新增 [Protocol Buffer](https://protobuf.dev/)  和 [Apache Avro](https://avro.apache.org/) 格式消息编解码功能。

- [#10425](https://github.com/emqx/emqx/pull/10425) 新增 OpenTSDB 数据桥接。

- [#10498](https://github.com/emqx/emqx/pull/10498) 新增 Oracle 数据桥接。

- [#10560](https://github.com/emqx/emqx/pull/10560) 新增 Apache IoTDB 数据桥接。

- [#10417](https://github.com/emqx/emqx/pull/10417) 通过解除临时引用来提高配置项读取性能。

- [#10430](https://github.com/emqx/emqx/pull/10430) 简化 `retainer` 的配置，将 `flow_control` 标记为不重要的字段。

- [#10511](https://github.com/emqx/emqx/pull/10511) 对资源相关的日志进行脱敏以提升隐私与安全性。

- [#10525](https://github.com/emqx/emqx/pull/10525) 提高 MQTT 数据包处理的性能。

- [#10528](https://github.com/emqx/emqx/pull/10528) 在代码的热路径（hot code path）中减少内存占用。热路径包括在消息处理、连接管理、认证授权等核心功能中被频繁执行的代码。

- [#10591](https://github.com/emqx/emqx/pull/10591) [#10625](https://github.com/emqx/emqx/pull/10625) 改进并简化速率限制器的配置：

  - 降低速率限制器配置的复杂度。

  - 更新 `configs/limiter` API。

  - 减少速率限制器配置内存占用。

- [#10487](https://github.com/emqx/emqx/pull/10487) 优化不设置速率限制(`infinity`)时速率限制器的性能表现，减少了内存和 CPU 的使用量。

- [#10490](https://github.com/emqx/emqx/pull/10490) 移除默认 `1000/s` 的连接速率限制。

- [#10077](https://github.com/emqx/emqx/pull/10077) QUIC TLS 现已支持密码保护证书文件。

#### 错误修复

- [#10340](https://github.com/emqx/emqx/pull/10340) 修复通过 `systemd` 停止 EMQX 时可能导致的日志打印崩溃问题。

- [#10369](https://github.com/emqx/emqx/pull/10369) 修复 `/api/v5/monitor_current` API，修复前，当某些 EMQX 节点出现故障时，该 API 请求会返回 500 和以下信息：

  `{"code":"INTERNAL_ERROR","message":"error, badarg, [{erlang,'++',[{error,nodedown},[{node,'emqx@10.42.0.150'}]], ...`

- [#10407](https://github.com/emqx/emqx/pull/10407) 修复告警崩溃问题

  - 通过使用 Mnesia dirty 操作和避免不必要的调用。

  - 使用 'emqx_resource_manager' 来重新激活已经激活的告警，提高'emqx_alarm'性能。

  - 使用新的安全的 'emqx_alarm' API 来激活/停用告警，确保 emqx_resource_manager 不会因为警报超时而崩溃。

  - 当以下条件同时出现时告警系统容易崩溃：

    - 失败的资源数量相对较多，例如，桥接试图激活重复出现的错误的警报。

    - 系统经历了一个非常高的负载。

- [#10420](https://github.com/emqx/emqx/pull/10420) 修复认证和授权 HTTP 路径可能被多次编码的问题：

  - 由于无法假定外部服务器对原始和规范化 URL 的处理方式，因此将尽量避免不必要的 URL 编码，以免导致类似 [#10411](https://github.com/emqx/emqx/issues/10411) 的 bug。

- [#10422](https://github.com/emqx/emqx/pull/10422) 修正单节点集群中，外部插件无法通过环境变量进行配置的问题。

- [#10448](https://github.com/emqx/emqx/pull/10448) 修复由 e5.0.3 引入的速率限制器配置（用 `burst` 代替 `capacity` ）的兼容性问题，修复前，如 `capacity` 设为 `infinity` ，将无法从之前的版本进行升级。修复后，`capacity = infinity` 配置将自动转换为等同的 `burst = 0` 。

- [#10462](https://github.com/emqx/emqx/pull/10462) 废弃已经无效的 `broker.shared_dispatch_ack_enabled` 配置项 。该配置项旨在避免向存在连接断开客户端的共享订阅组中派发回消息。但实际从 e5.0.0 版本开始，消息会被重新派发到组内的其他已经连接的会话中，该配置已经失效。请参考: <https://github.com/emqx/emqx/pull/9104> 。

- [#10463](https://github.com/emqx/emqx/pull/10463) 修复数据桥接 API 的错误处理问题，例如 WebHook 桥接 URL 无效时， API 将返回 '400' 而非 '500'。

- [#10484](https://github.com/emqx/emqx/pull/10484) 修复滚动升级时无法设置配置优先级的问题。例如，在 e5.0.2 中修改了授权的优先级，当通过滚动升级升级到 e5.0.3 时，授权的优先级将被恢复为默认。

- [#10495](https://github.com/emqx/emqx/pull/10495) 恢复了被误删的速率限制器 API `/configs/limiter`。

- [#10500](https://github.com/emqx/emqx/pull/10500) 修复并增强了 Mria 中的一些功能：

  - 通过全局锁来保护 `mria:join/1,2` 函数，避免两个同时试图加入对方的节点之间的冲突 [Mria PR](https://github.com/emqx/mria/pull/137)

  - 提供了新函数 `mria:sync_transaction/4,3,2`，该函数会阻止调用者，直到事务被导入到本地节点（当本地节点是复制者时有效，否则它的行为与`mria:transaction/3,2` 函数完全一样）。

  - 优化  `mria:running_nodes/0`  函数 [Mria PR](https://github.com/emqx/mria/pull/135)

  - 优化单个复制节点调用  `mria:ro_transaction/2`  函数时的性能 [Mria PR](https://github.com/emqx/mria/pull/134).

- [#10518](https://github.com/emqx/emqx/pull/10518) 在 Mria 中增加以下修复和功能：

  - 在 mria_membership 中安全地调用 `mria_rlog:role/1`，以确保 mria_membership gen_server 在 RPC 到另一个节点失败时不会崩溃 [Mria PR](https://github.com/emqx/mria/pull/139)。

  - 在 `?rlog_sync` 表中添加额外的字段，以方便在未来的扩展这一功能 [Mria PR](https://github.com/emqx/mria/pull/138)。

- [#10556](https://github.com/emqx/emqx/pull/10556) 加密`emqx_connector_http`  携带的  `Authorization header`。

- [#10571](https://github.com/emqx/emqx/pull/10571) 尝试通过 API 获取主题统计中不存在的主题时，不会产生无意义的崩溃报告。

- [#10659](https://github.com/emqx/emqx/pull/10659) 修复当 `sysmon.os.mem_check_interval` 被禁用时，EMQX 无法启动的问题。

- [#10717](https://github.com/emqx/emqx/pull/10717) 修复当飞行窗口拥塞时，缓冲层进程 CPU 占用过高的问题。

- [#10724](https://github.com/emqx/emqx/pull/10724) 为 HTTP API 文档所有 API 添加摘要字段以便查找 API（访问地址为 "http://<emqx_host_name\>:18083/api-docs")。

- [#10726](https://github.com/emqx/emqx/pull/10726) 对健康检查间隔和自动重启间隔的范围进行验证，支持 1 ms 到 1 小时的时间范围。

- [#10728](https://github.com/emqx/emqx/pull/10728) 修复规则引擎 SQL `FOREACH - DO`语句执行问题，例如在以下 SQL 语句中：

  `FOREACH   payload.date as date, payload.array as elem DO   date, elem FROM "t/#"  -- {"date": "2023-05-06", "array": ["a"]}`

  修复前，由 `FOREACH` 导出的 `date` 变量无法在 `DO` 子句中访问，导致该 SQL 输出如下： `[{"elem": "a","date": "undefined"}]` 。

  修复后，该 SQL 语句将能正确输出：`[{"elem": "a","date": "2023-05-06"}]`

- [#10742](https://github.com/emqx/emqx/pull/10742) 在保存授权文件前对规则进行正确性检查，避免由于错误规则导致 EMQX 无法启动。

- [#10743](https://github.com/emqx/emqx/pull/10743) 修复节点加入集群后，尝试获取桥接信息或指标时可能导致的崩溃问题。

- [#10755](https://github.com/emqx/emqx/pull/10755) 修复了数据桥接资源更新中的竞争条件。

  在 EMQX 的资源更新中，进行了"删除+创建"的操作。如果桥接器的创建时间过长，可能导致 Dashboard 请求超时。如果在桥接创建完成之前进行更新操作，可能会导致桥接被删除。

  此修复解决了桥接器资源更新中的竞争条件，确保准确识别和添加新数据桥接，并保持运行时和配置文件状态的一致性。

- [#10761](https://github.com/emqx/emqx/pull/10761) 修复了 Dashboard Listener 的 SSL 证书的默认值没能被正确设置的问题，当 `verify_peer` 和 `cacertfile` 使用默认配置时，将导致 HTTPS 无法访问。

- [#10672](https://github.com/emqx/emqx/pull/10672) 修复当监听器中缺少 `ssl_options` 的默认值导致启动失败的问题。修复前， `EMQX_LISTENERS__WSS__DEFAULT__BIND='0.0.0.0:8089' ./bin/emqx console` 类似的命令将导致 EMQX 崩溃。

- [#10738](https://github.com/emqx/emqx/pull/10738) 修复 TDengine 数据桥不支持子表以及自动建表问题。在此之前，子表插入 SQL 语句将执行失败：

  - `insert into ${clientid} using msg TAGS (${clientid}) values (${ts},${msg})`

* [#10746](https://github.com/emqx/emqx/pull/10746)  在规则引擎的测试 API `rule_test` 中增加对事件`$events/delivery_dropped`的支持。
* [#10747](https://github.com/emqx/emqx/pull/10747)  移植 4.4 版本中一些规则引擎的时间格式化函数的修复到当前版本。
* [#10760](https://github.com/emqx/emqx/pull/10760)  修复在节点加入集群是访问数据桥接数据统计页面是出现 "internal error 500" 错误的问题。
* [#10801](https://github.com/emqx/emqx/pull/10801)  避免在 API `/topics/{topic}`  和  `/topics` 中对主题名进行双重百分号解码。
* [#10817](https://github.com/emqx/emqx/pull/10817)  修复对桥接配置项`auto_restart_interval`的值的处理，现在支持设置为`infinity`。

## 5.0.4

*发布日期: 2022-07-28*

### Enhancements

- Rules in data Integration support paging and searching. Note that the `GET /rules` API will return the page meta information after the update, i.e. `{"data": [RuleObj1, RuleObj2], "meta": {"count": 2, "limit": 100, "page": 1}}`. [#8472](https://github.com/emqx/emqx/pull/8472)
- Improve the health check of WebHook in data integration, if TLS is enabled, it will now check if the TLS handshake was successful. [#8443](https://github.com/emqx/emqx/pull/8443)
- Falls back to using Mnesia to persist sessions when RocksDB is unavailable. [#8528](https://github.com/emqx/emqx/pull/8528)
- Support for updating thresholds of alarms at runtime via the HTTP API. [#8532](https://github.com/emqx/emqx/pull/8532)
- Log trace will show the detailed authentication process. [#8554](https://github.com/emqx/emqx/pull/8554)
- Supports listening on IPv6 addresses, for example: `[::1]:1883` or `::1:1883`. [#8547](https://github.com/emqx/emqx/pull/8547)
- Updated the Listener API's request and response formats to align with the behavior of other APIs. This will introduce some incompatible updates, see [#8571](https://github.com/emqx/emqx/pull/8571)。
- Dashboard will prompt to change the default password.
- Optimize Dashboard's overview page for rules and data bridges of data integration.
- Add result statistics for data integration rules on Dashboard.
- Optimize the charts in the Dashboard homepage.
- Add MQTT 5.0 subscription options display to subscription list on Dashboard.

### Bug fixes

- Fix the issue that when the log type format is set to `json`, the configuration of the maximum length of a single log is invalid. [#8518](http://github.com/emqx/emqx/pull/8518)
- Fix the issue that `jq` in data integration cannot be used when the path of the EMQX installation directory contains spaces. [#8455](https://github.com/emqx/emqx/pull/8455)
- Fix the issue that super user does not take effect. [#8452](https://github.com/emqx/emqx/pull/8452)
- Aligned system topic format for stats with metrics. [#8464](https://github.com/emqx/emqx/pull/8464)
- Fix the issue that the creation time of the rules in the data integration was not persistent, causing it to be updated to the EMQX startup time every time. [#8443](https://github.com/emqx/emqx/pull/8443)
- Fix an issue where the `cluster-override.conf` file would be emptied when there was an error updating the configuration via the HTTP API, causing all modified configurations to be lost. [#8443](https://github.com/emqx/emqx/pull/8443)
- Fix the issue that the Sentinel field was not required to be set when using Redis sentinel mode in authentication and authorization. [#8458](https://github.com/emqx/emqx/pull/8458)
- Fix formatting errors in OpenAPI documentation. [#8517](https://github.com/emqx/emqx/pull/8517)
- Fix the issue that multilingual hook extensions might not be dispatched in the order in which client events were fired. [#8530](https://github.com/emqx/emqx/pull/8530)
- Fix authentication placeholders `cert_subject` and `cert_common_name` not being available. [#8531](https://github.com/emqx/emqx/pull/8531)
- Fix TCP connection process leak in WebHook. [ehttpc#34](https://github.com/emqx/ehttpc/pull/34), [#8580](https://github.com/emqx/emqx/pull/8580)
- Fix CLI not printing listeners that only listen on ports. [#8547](https://github.com/emqx/emqx/pull/8547)
- Fix incorrect TLS field checking in JWKS authentication. [#8458](https://github.com/emqx/emqx/pull/8458)
- Fix listener API not returning connection information on all nodes in the cluster. [#8538](https://github.com/emqx/emqx/pull/8538)
- Fix the issue that replicant nodes might not receive Mnesia events, causing operations such as configuration updates to fail. [#8502](https://github.com/emqx/emqx/pull/8502)

## 5.0.3

*发布日期: 2023-05-08*

### 优化

- [#10128](https://github.com/emqx/emqx/pull/10128) SSL MQTT 监听器增加对 OCSP Stapling 的支持。

- [#10156](https://github.com/emqx/emqx/pull/10156) 调整配置文件覆盖顺序机制。

  对于新安装的 EMQX，emqx.conf 和环境变量中的配置会覆盖 API 传入的配置（即 `cluster.hocon` 中的配置）

  对于从旧版本升级的 EMQX（即 `data` 文件夹中包含 `cluster-override.conf` 文件），保留之前的覆盖规则， 即 `cluster-override.conf` 中的配置会覆盖 `emqx.conf` 和环境变量的配置。

  注意：`data/configs/cluster-override.conf` 已弃用。升级后，建议您在 `emqx.conf` 中重新配置之前被 `cluster-override.conf` 覆盖的配置项，并将 cluster-override.conf 中的配置迁移到 `cluster.hocon` 中。

  升级后，EMQX 将像以前一样继续读取 `local-override.conf` (如果存在的话)，但建议您将配置合并到 `emqx.conf` 中。

- [#10164](https://github.com/emqx/emqx/pull/10164) TLS MQTT 监听器增加对 CRL 检查的支持。

- [#10207](https://github.com/emqx/emqx/pull/10207) 提高 OpenAPI (swagger) 文档的可读性。 在此更改之前，文档中有一些 `Summary` 字段冗长且缺乏翻译，现在使用了 i18n 数据库中更简洁的 `label` 字段。

- [#10210](https://github.com/emqx/emqx/pull/10210) 解决停止/重启 Mria 时 Mnesia callback 可能出现的问题。优化后，当 Mria 被停止前会取消 Mnesia callback 的注册。详情见 [Mria PR](https://github.com/emqx/mria/pull/133)。

- [#10224](https://github.com/emqx/emqx/pull/10224) 在 Helm 图表中增加自定义 `clusterIP` 选项，用户可以将其设置为固定 IP。

- [#10263](https://github.com/emqx/emqx/pull/10263) 添加用于评估 Elixir 表达式的命令 `eval-ex`。

- [#10278](https://github.com/emqx/emqx/pull/10278) 重构所有网关的目录结构。

- [#10206](https://github.com/emqx/emqx/pull/10206) 所有数据桥接支持异步查询模式。

  优化前，如将某项资源（如数据桥接）的查询模式设为 sync（同步模式），缓存将以同步的模式调用底层连接器，即使它支持异步调用。

- [#10306](https://github.com/emqx/emqx/pull/10306) 大多数数据桥接支持 async 查询模式。
  这是 [#10206](https://github.com/emqx/emqx/pull/10206) 的后续优化, 优化前，Cassandra、MongoDB、MySQL、Postgres、Redis、RocketMQ、TDengine 等数据桥接只支持同步查询模式。

- [#10318](https://github.com/emqx/emqx/pull/10318) 规则引擎中的 FROM 语句新增支持由单引号（'）包裹的字符串。

- [#10336](https://github.com/emqx/emqx/pull/10336) 添加 API Endpoint `/rule_engine`，用以管理规则引擎的配置。

- [#10354](https://github.com/emqx/emqx/pull/10354) 优化 `max_heap_size` 配置错误时的报错信息。当发生 `message_queue_too_long` 报错时，会在日志文件中记录当前值和最大值。

- [#10358](https://github.com/emqx/emqx/pull/10358) 隐藏 `flapping_detect/conn_congestion/stats` 配置。弃用 `flapping_detect.enable` 配置项。

- [#10359](https://github.com/emqx/emqx/pull/10359) 通过独立的 RPC 收集针对集群级别的指标，不再隐式收集不被 API 调用的指标。

- [#10373](https://github.com/emqx/emqx/pull/10373) 废弃 `trace.payload_encode` 配置项。可以在通过 HTTP API 创建的日志追踪时使用 `trace.payload_encode = [text, hidden, hex]` 字段替代。

- [#10381](https://github.com/emqx/emqx/pull/10381) 隐藏 `auto_subscribe` 配置项，后续将只能通过 HTTP API 来修改自动订阅规则。

- [#10391](https://github.com/emqx/emqx/pull/10391) 简化配置文件并隐藏大量的配置项，包括 `rewrite`、`topic_metric`、`persistent_session_store`、`overload_protection`、`flapping_detect`、`conn_congestion`、`stats`、`auto_subscribe`、`broker_perf`、`shared_subscription_group`、`slow_subs`、`ssl_options.user_lookup_fun`、 `node` 和 `dashboard` 相关的部分高级配置项，[#10358](https://github.com/emqx/emqx/pull/10358), [#10381](https://github.com/emqx/emqx/pull/10381), [#10385](https://github.com/emqx/emqx/pull/10385)。

- [#10404](https://github.com/emqx/emqx/pull/10404) 将缓冲区工作线程的默认队列模式更改为 `memory_only`。在此优化前，默认队列模式为 `volatile_offload`，当消息速率很高，资源无法满足该需求时，缓冲区性能会由于频繁的磁盘操作而受影响。

- [#10140](https://github.com/emqx/emqx/pull/10140) 新增 Cassandra 数据桥接，目前仅支持 Cassandra 3.x 版本，暂不支持 4.x 版本。

- [#10143](https://github.com/emqx/emqx/pull/10143) 新增 RocketMQ 数据桥接。

- [#10165](https://github.com/emqx/emqx/pull/10165) InfluxDB 数据桥接中的 `write_syntax` 中支持转义特殊字符。优化后，用户可根据 InfluxDB Line Protocol 在字符串中使用经转义的特殊字符。

- [#10211](https://github.com/emqx/emqx/pull/10211) 隐藏 `broker.broker_perf` 配置和相关 API 文档。其中的两个配置项 `route_lock_type` 和 `trie_compaction` 很少使用，而且需要重新启动整个集群才能生效，不必要暴露给用户。更多信息可阅读：<https://gist.github.com/zmstone/01ad5754b9beaeaf3f5b86d14d49a0b7/revisions>。

- [#10294](https://github.com/emqx/emqx/pull/10294) 配置 MongoDB 数据桥接时，现支持通过占位符 `${field}` 语法来引用消息中的字段，从而动态地选择要插入的数据集合。

- [#10363](https://github.com/emqx/emqx/pull/10363) 新增 Microsoft SQL Server 数桥接。

- [#10573](https://github.com/emqx/emqx/pull/10573) 提升了 WebHook 在同步请求模式下的性能表现，以及其他数据桥接在未配置批处理时的性能表现。

### 修复

- [#10145](https://github.com/emqx/emqx/pull/10145) 在针对 `GET /bridges/:id` 的 API 调用响应中，如桥接的状态为断开，且内部健康检查返回错误，添加 `status_reason` 字段说明错误原因。在相应的告警信息中，同样增加 `status_reason` 字段说明断开原因。

- [#10172](https://github.com/emqx/emqx/pull/10172) 修复默认 ACL 规则中不正确的正则表达式，从而允许 dashboard 用户名订阅 `$SYS/#` 主题。

- [#10174](https://github.com/emqx/emqx/pull/10174) 将库 `esockd` 从 5.9.4 升级至 5.9.6。如连接在代理发送代理协议头之前关闭，将不再产生的一条错误级别日志。

- [#10195](https://github.com/emqx/emqx/pull/10195) 对包含 HTML 的 API Schema 添加标签，解决之前会破坏生成文档格式的问题。

- [#10196](https://github.com/emqx/emqx/pull/10196) 针对用于生成在线文档菜单中的模式摘要和描述，使用小写字母。

- [#10209](https://github.com/emqx/emqx/pull/10209) 修复在断开禁用客户端时，此类客户端仍可发布遗嘱消息的错误。

- [#10225](https://github.com/emqx/emqx/pull/10225) 对于名称与已安装的插件开头相同的插件，用户仍可继续安装。例如：如果插件 `emqx_plugin_template_a` 已安装，用户仍可安装名为 `emqx_plugin_template` 的插件。

- [#10226](https://github.com/emqx/emqx/pull/10226) 在 `/bridges` API 验证错误时，返回 `400` 而非 `500`。

- [#10242](https://github.com/emqx/emqx/pull/10242) 修复日志中数据字段名称冲突。修复前，一些调试日志可能会报告错误的 Erlang PID，影响解决会话接管类问题。

- [#10257](https://github.com/emqx/emqx/pull/10257) 修复 LwM2M 网关中 `auto_observe` 无法正常工作的问题。

  修复前，在发送 `OBSERVE` 请求时没有发送 token，导致 LwM2M 网关无法处理客户端请求。

  修复后，LwM2M 网关可以正确观察到客户端携带的资源列表，此外，未知资源将被忽并打印以下警告日志：

  ```
  2023-03-28T18:50:27.771123+08:00 [warning] msg: ignore_observer_resource, mfa: emqx_lwm2m_session:observe_object_list/3, line: 522, peername: 127.0.0.1:56830, clientid: testlwm2mclient, object_id: 31024, reason: no_xml_definition
  ```

- [#10286](https://github.com/emqx/emqx/pull/10286) 优化 EMQX 启动失败时的日志记录行为。当 EMQX 由于配置文件破坏无法启动时，不会再产生过多的日志记录，也不会再产生崩溃转储文件。

- [#10297](https://github.com/emqx/emqx/pull/10297) 通过仅评估 Erlang 表达式实现 `eval` 命令与 v4 版本的向后兼容，该更新同样适用于 Elixir 节点。对于 Elixir 表达式，请使用 `eval-ex` 命令。

- [#10300](https://github.com/emqx/emqx/pull/10300) 针对通过 Elixir 构建的项目，修复无法通过环境变量配置插件的问题。

- [#10315](https://github.com/emqx/emqx/pull/10315) 修复在 `/mqtt/delayed/messages` API 调用中，在检查 `limit` 和 `page` 参数时的崩溃问题。

- [#10317](https://github.com/emqx/emqx/pull/10317) 在经过充分验证前，隐藏监听器级别的认证信息。

- [#10323](https://github.com/emqx/emqx/pull/10323) 出于安全原因，API 示例中 `password` 字段的值被替换为 `******`。

- [#10410](https://github.com/emqx/emqx/pull/10410) 针对由`emqx.conf` 配置的网关，修复配置检查失败问题。
  此问题最早在 v5.0.22 版本中由 [#10278](https://github.com/emqx/emqx/pull/10278)引入，启动时缺少配置检查。

- [#10533](https://github.com/emqx/emqx/pull/10533) 修复可能导致日志中的出现无害的噪音的问题。

  在对数据桥接进行同步调用时，一些迟到的回复可能会发送到不再期待回复的连接进程，导致产生错误日志，如：

  ```
  2023-04-19T18:24:35.350233+00:00 [error] msg: unexpected_info, mfa: emqx_channel:handle_info/2, line: 1278, peername: 172.22.0.1:36384, clientid: caribdis_bench_sub_1137967633_4788, info: {#Ref<0.408802983.1941504010.189402>,{ok,200,[{<<"cache-control">>,<<"max-age=0, ...">>}}
  ```

  这些日志是无害的，但它们可能会泛滥成灾，引起用户不必要的担心。

- [#10449](https://github.com/emqx/emqx/pull/10449) 在通过 HTTP 服务（`authn_http`）创建身份验证时，将进行 `ssl_options` 和 `header` 配置验证。在此修复前，用户通过错误的 ssl 配置也可成功创建身份验证，但该验证整体不生效。

- [#10548](https://github.com/emqx/emqx/pull/10548) 修复了 HTTP 驱动程序在竞争条件下会导致错误而不去重试的问题。
  相关的驱动程序修复：[emqx/ehttpc#45](https://github.com/emqx/ehttpc/pull/45)

- [#10201](https://github.com/emqx/emqx/pull/10201) 在 TDengine 数据桥接中，移除 SQL 模板中冗余的数据库名称。

- [#10270](https://github.com/emqx/emqx/pull/10270) 在创建 ClickHouse 数据桥接时，优化用户点击测试按钮时的错误信息。

- [#10324](https://github.com/emqx/emqx/pull/10324) 针对配置有误的 ClickHouse 数据桥接，当用户尝试通过 Dashboard 重连时，将收到报错。修复前，用户不会收到报错。

- [#10438](https://github.com/emqx/emqx/pull/10438) 修复 DynamoDB 数据桥接中的部分术语使用错误：

  - 将 `database` 修改为 `table`
  - 将 `username` 修改为 `aws_access_key_id`
  - 将 `password` 修改为 `aws_secret_access_key`

## 5.0.3

*发布日期: 2022-07-07*

## 5.0.3

### Bug fixes

- Websocket listener failed to read headers `X-Forwared-For` and `X-Forwarded-Port` [8415](https://github.com/emqx/emqx/pull/8415)
- Deleted `cluster_singleton` from MQTT bridge config document. This config is no longer applicable in 5.0 [8407](https://github.com/emqx/emqx/pull/8407)
- Fix `emqx/emqx:latest` docker image publish to use the Erlang flavor, but not Elixir flavor [8414](https://github.com/emqx/emqx/pull/8414)
- Changed the `exp` field in JWT auth to be optional rather than required to fix backwards compatability with 4.X releases. [8425](https://github.com/emqx/emqx/pull/8425)

### Enhancements

- Improve the speed of dashboard's HTTP API routing rule generation, which sometimes causes timeout [8438](https://github.com/emqx/emqx/pull/8438)

## 5.0.2

*发布日期: 2023-04-12*

### 优化

- [#10022](https://github.com/emqx/emqx/pull/10022) 发布 Rocky Linux 9 (兼容 Red Hat Enterprise Linux 9) 以及 macOS 12 Intel 平台的安装包。

- [#10139](https://github.com/emqx/emqx/pull/10139) 在 EMQX Helm Chart 中添加 `extraVolumeMounts`，可以挂载用户自己的文件到 EMQX 实例中，例如在 [#9052](https://github.com/emqx/emqx/issues/9052) 中提到的 ACL 规则文件。

- [#9893](https://github.com/emqx/emqx/pull/9893) 当使用 `clean_start=false` 标志连接时，EMQX 将过滤掉会话中被被黑名单功能禁止的客户端发布的消息。以前，在这种情况下，被黑名单功能禁止的客户端发送的消息仍可能被传递给订阅者。

- [#9986](https://github.com/emqx/emqx/pull/9986) 在 helm charts 中增加 MQTT ingress 并删除过时的 `mgmt` 引用。

- [#9564](https://github.com/emqx/emqx/pull/9564) 数据桥接新增 Kafka Consumer，支持从 Kafka 消费消息并将它们发布到 MQTT 主题。

- [#9881](https://github.com/emqx/emqx/pull/9881) 改进了与 InfluxDB 连接的健康检查相关的错误日志。

- [#9985](https://github.com/emqx/emqx/pull/9985) 添加 ClickHouse 数据桥接。

- [#10123](https://github.com/emqx/emqx/pull/10123) 改进了 `/bridges` API 的性能。避免了当集群节点数量较多时可能出现的请求响应超时。

- [#9998](https://github.com/emqx/emqx/pull/9998) 出于安全原因，在使用 HTTP 服务进行客户端认证时，对错误日志中的请求体进行脱敏处理。

- [#10026](https://github.com/emqx/emqx/pull/10026) 仅在 `/bridges/:id/metrics` API 中返回指标数据。

- [#10052](https://github.com/emqx/emqx/pull/10052) 改进守护进程模式下启动失败后的日志。

### 修复

- [#10013](https://github.com/emqx/emqx/pull/10013) 修复 `/gateways/:name/clients` API 在错误情况下的返回类型结构。

- [#10014](https://github.com/emqx/emqx/pull/10014) 当节点不存在时，`/monitor(_current)/nodes/:node` API 返回 `404` 错误代码而不是 `400` 。

- [#10027](https://github.com/emqx/emqx/pull/10027) 允许在 Docker 中通过 `EMQX_NODE__NAME` 设置节点名称。

- [#10050](https://github.com/emqx/emqx/pull/10050) 当调用 Bridge API 时若资源不能存在则返回 `404` 状态码。

- [#10055](https://github.com/emqx/emqx/pull/10055) 修复配置项 `mqtt.max_awaiting_rel` 设置无效的问题。

- [#10056](https://github.com/emqx/emqx/pull/10056) 修复 `/bridges` API 状态码返回错误。当被删除的 Bridge 存在依赖，或 Bridge 未启用时进行启用、停止、重启等操作时返回 `400` 状态码。

- [#10066](https://github.com/emqx/emqx/pull/10066) 优化 `/briges_probe` 和 `[/node/:node]/bridges/:id/:operation` API 调用的错误消息，使其更易理解。并修正错误状态码为`400`。

- [#10074](https://github.com/emqx/emqx/pull/10074) 增加 `PUT /authorization/sources/:type` 请求参数 `type` 的值与请求体中的实际类型的一致性检查。

- [#10079](https://github.com/emqx/emqx/pull/10079) 修复文档中关于 `shared_subscription_strategy` 的描述错误。

- [#10085](https://github.com/emqx/emqx/pull/10085) 对于 `/authorization/sources/:source[/*]` API 中不存在的资源的所有请求，始终返回 `404`。

- [#10098](https://github.com/emqx/emqx/pull/10098) 修复了当配置 MongoDB 授权时 MongoDB 连接器崩溃的问题。

- [#10100](https://github.com/emqx/emqx/pull/10100) 修复了当客户端使用增强认证时，认证消息发送缓慢或者消息丢失时客户端进程崩溃的问题。

- [#10107](https://github.com/emqx/emqx/pull/10107) 当调用 `bridges API` 时如果 `bridge-id` 不存在则返回 `404` 状态码。

- [#10117](https://github.com/emqx/emqx/pull/10117) 修复当加入的节点没有安装在集群其他节点上的插件时发生的错误。修复后，加入的节点将从其他节点复制所有必要的插件。

- [#10118](https://github.com/emqx/emqx/pull/10118) 修复手动添加 EMQX 副本类型节点到集群的相关问题。

- [#10119](https://github.com/emqx/emqx/pull/10119) 修复了当 `statsd.server` 设置为空字符串时会崩溃的问题。

- [#10124](https://github.com/emqx/emqx/pull/10124) 调整 MongoDB 的默认心跳周期，以降低日志文件记录过多的风险。

- [#10130](https://github.com/emqx/emqx/pull/10130) 修复了通过环境变量设置的值，在 Dashboard 中显示乱码的问题。

- [#10132](https://github.com/emqx/emqx/pull/10132) 修复了 `systemctl stop emqx` 命令无法正确停止 `jq` 和 `os_mon` 应用的问题。

- [#10144](https://github.com/emqx/emqx/pull/10144) 修复了当 emqx 目录只读时， emqx cli 设置 Erlang cookie 失败的问题。

- [#10154](https://github.com/emqx/emqx/pull/10154) 将数据桥接和连接器的 `resume_interval` 参数值设为 `health_check_interval` 和 `request_timeout / 3` 中的较小值，以解决请求超时的问题。

- [#10157](https://github.com/emqx/emqx/pull/10157) 修复在创建新的监听器时默认速率限制不生效的问题。

- [#10237](https://github.com/emqx/emqx/pull/10237) 当调用 `/nodes/:node[/metrics|/stats]` API，若节点不存在则返回 `404` 状态码。

- [#10251](https://github.com/emqx/emqx/pull/10251) 修复了当删除一个使用中的 ingress 类型的桥接时，未提示存在规则依赖的问题。

- [#10313](https://github.com/emqx/emqx/pull/10313) 确保在核心节点或副本节点启动时，仅从核心节点复制 `cluster-override.conf` 文件。

- [#10327](https://github.com/emqx/emqx/pull/10327) 数据桥接出现不可恢复的错误不再计入到 `actions.failed.unknown` 指标中。

- [#10095](https://github.com/emqx/emqx/pull/10095) 修复当 MySQL 连接器处于批处理模式时，会发生客户端在每个批次上不断使用不必要的 `PREPARE` 语句查询服务器，可能会导致服务器资源耗尽的问题。

## 5.0.2

*发布日期: 2022-07-02*

Announcemnet: EMQX team has decided to stop supporting relup for opensouce edition.
Going forward, it will be an enterprise only feature.

Main reason: relup requires carefully crafted upgrade instructions from ALL previous versions.

For example, 4.3 is now at 4.3.16, we have `4.3.0->4.3.16`, `4.3.1->4.3.16`, ... 16 such upgrade paths in total to maintain.
This had been the biggest obstacle for EMQX team to act agile enought in deliverying enhancements and fixes.

### Bug fixes

- Fixed a typo in `bin/emqx` which affects macOS release when trying to enable Erlang distribution over TLS [8398](https://github.com/emqx/emqx/pull/8398)

## 5.0.1

*发布日期: 2023-03-10*

### 增强

- [#10019](https://github.com/emqx/emqx/pull/10019) 为 QUIC 监听器添加更多底层调优选项。
- [#10059](https://github.com/emqx/emqx/pull/10059) 规则引擎 API 在出错时返回用户可读的错误信息而不是原始的异常堆栈信息。
- [#9213](https://github.com/emqx/emqx/pull/9213) 在 Helm Chart 中支持 PodDisruptionBudget。
- [#9949](https://github.com/emqx/emqx/pull/9949) QUIC 支持多流传输与 TLS。
- [#9932](https://github.com/emqx/emqx/pull/9932) 添加 TDengine 数据桥接。
- [#9967](https://github.com/emqx/emqx/pull/9967) 新增 TLS 配置项 `hibernate_after`，通过在闲置一段时间后休眠 TLS 进程以减少其内存占用，默认： 5s 。

### 修复

- [#10009](https://github.com/emqx/emqx/pull/10009) `GET /trace/:name/log` API 添加 `bytes` 参数校验，长度不超过 32 位有符号整数。
- [#10015](https://github.com/emqx/emqx/pull/10015) 执行 CLI 时，如果节点 cookie 配置错误则快速抛出错误。
  在此修复前，即使 cookie 配置错误，EMQX 命令仍然会尝试去 ping EMQX 节点，
  并得到一个 'Node xxx not responding to pings' 的错误。
  修复后，如果发现 cookie 不一致，立即打印不一致的错误信息并退出。
- [#10020](https://github.com/emqx/emqx/pull/10020) 修复使用异步和批量配置的桥接计数不准确的问题。
- [#10021](https://github.com/emqx/emqx/pull/10021) 修正执行`emqx_ctl cluster join`命令时，目标节点未运行时的错误信息。
- [#10032](https://github.com/emqx/emqx/pull/10032) 当集群中某些节点上的资源仍处于 **初始化/连接中** 状态时，调用 `/bridges` API 获取 metrics 时将因为没有 metrics 数据而崩溃，此修复将忽略没有 metrics 数据的资源。
- [#10041](https://github.com/emqx/emqx/pull/10041) 为 InfluxDB 桥接配置项 `write_syntax` 描述文档添加了整数占位符注释说明。
  另外在配置中支持 `timestamp` 使用一个常量。
- [#10042](https://github.com/emqx/emqx/pull/10042) 当 core 节点离开集群时，改进 replicant 节点的行为。
  以前，直到所有 core 节点全部起来前， replicant 节点无法重新平衡与 core 节点的连接。
  该情况会打印以下日志：
  `[error] line: 182, mfa: mria_lb:list_core_nodes/1, msg: mria_lb_core_discovery divergent cluster`。
- [#10054](https://github.com/emqx/emqx/pull/10054) 修复了对于已有的 Data Bridge 进行测试连接时需要手工输入一遍密码的问题。
- [#10058](https://github.com/emqx/emqx/pull/10058) 优化未使用的 QUIC TLS 选项。
  QUIC 监听器只保留以下 TLS 选项:
  - cacertfile
  - certfile
  - keyfile
  - verify
- [#10076](https://github.com/emqx/emqx/pull/10076) 修复 Webhook 桥接的一个异常处理：连接超时错误发生后，发生错误的请求可以被重试。
  在此修复前，连接超时后，被当作不可重试类型的错误处理，导致请求被丢弃。
- [#10078](https://github.com/emqx/emqx/pull/10078) 修复了无效的 QUIC 监听器设置可能导致 segfault 的问题。
- [#10084](https://github.com/emqx/emqx/pull/10084) 修正将运行不同 EMQX 版本的 core 节点加入集群的问题。
- [#10086](https://github.com/emqx/emqx/pull/10086) HTTP 客户端库 `ehttpc` 升级到 0.4.7。
  在升级前，如果 HTTP 客户端，例如 认证，授权，webhook 等配置中使用了 content-type HTTP 头，但是没有配置 body，则可能会发生异常。
  详情见 [ehttpc PR#44](https://github.com/emqx/ehttpc/pull/44)。
- [#9939](https://github.com/emqx/emqx/pull/9939) 允许 `emqx ctl cluster join` 命令在 Mnesia 启动前调用。
  在此修复前， EMQX 的 `replicant` 类型节点无法使用 `manual` 集群发现策略。
- [#9958](https://github.com/emqx/emqx/pull/9958) 修复 `clients` API 在 Client ID 不存在时返回的错误码和错误提示。
- [#9961](https://github.com/emqx/emqx/pull/9961) 在 bin/emqx 脚本中，避免在运行非启动命令时解析 emqx.conf 来获取节点名称和 cookie。
- [#9974](https://github.com/emqx/emqx/pull/9974) Statsd 和 prometheus 使用跟 Dashboard 相同的内存用量数据源。
  在此修复前，内存的总量和用量统计使用了过时的（在容器环境中不准确）的数据源。
- [#9997](https://github.com/emqx/emqx/pull/9997) 修复生成 Swagger API 时没有遵循[标准](https://swagger.io/specification/)将元数据字段 `deprecated` 设置为布尔值的问题。
- [#10007](https://github.com/emqx/emqx/pull/10007) Kafka 桥接的配置参数 `memory_overload_protection` 默认值从 `true` 改成了 `false`。
  尽管内存过载后消息被丢弃会产生日志和计数，如果没有基于这些日志或计数的告警，系统管理员可能无法及时发现消息被丢弃。
  当前更好的选择是：让管理员显式的配置该项，迫使他们理解这个配置的好处以及风险。
- [#10087](https://github.com/emqx/emqx/pull/10087) 往 InfluxDB 中插入数据时，如果时间戳为空（未定义），则使用默认的占位符 `${timestamp}`。
  在此修复前，如果时间戳字段没有设置，InfluxDB 桥接使用了一个错误的时间戳。

## 5.0.1

*发布日期: 2022-07-01*

### Enhancements

- Removed management API auth for prometheus scraping endpoint /api/v5/prometheus/stats [8299](https://github.com/emqx/emqx/pull/8299)
- Added more TCP options for exhook (gRPC) connections. [8317](https://github.com/emqx/emqx/pull/8317)
- HTTP Servers used for authentication and authorization will now indicate the result via the response body. [8374](https://github.com/emqx/emqx/pull/8374) [8377](https://github.com/emqx/emqx/pull/8377)
- Bulk subscribe/unsubscribe APIs [8356](https://github.com/emqx/emqx/pull/8356)
- Added exclusive subscription [8315](https://github.com/emqx/emqx/pull/8315)
- Provide authentication counter metrics [8352](https://github.com/emqx/emqx/pull/8352) [8375](https://github.com/emqx/emqx/pull/8375)
- Do not allow admin user self-deletion [8286](https://github.com/emqx/emqx/pull/8286)
- After restart, ensure to copy `cluster-override.conf` from the clustered node which has the greatest `tnxid`. [8333](https://github.com/emqx/emqx/pull/8333)

### Bug fixes

- A bug fix ported from 4.x: allow deleting subscriptions from `client.subscribe` hookpoint callback result. [8304](https://github.com/emqx/emqx/pull/8304) [8347](https://github.com/emqx/emqx/pull/8377)
- Fixed Erlang distribution over TLS [8309](https://github.com/emqx/emqx/pull/8309)
- Made possible to override authentication configs from environment variables [8323](https://github.com/emqx/emqx/pull/8309)
- Made authentication passwords in Mnesia database backward compatible to 4.x, so we can support data migration better. [8351](https://github.com/emqx/emqx/pull/8351)
- Fix plugins upload for rpm/deb installations [8379](https://github.com/emqx/emqx/pull/8379)
- Sync data/authz/acl.conf and data/certs from clustered nodes after a new node joins the cluster [8369](https://github.com/emqx/emqx/pull/8369)
- Ensure auto-retry of failed resources [8371](https://github.com/emqx/emqx/pull/8371)
- Fix the issue that the count of `packets.connack.auth_error` is inaccurate when the client uses a protocol version below MQTT v5.0 to access [8178](https://github.com/emqx/emqx/pull/8178)

### Others

- Rate limiter interface is hidden so far, it's subject to a UX redesign.
- QUIC library upgraded to 0.0.14.
- Now the default packages will be released withot otp version number in the package name.
- Renamed config exmpale file name in `etc` dir.

## 5.0.0

*发布日期: 2023-02-03*

EMQX 企业版 5.0 是一个全新的版本。在这个版本中，我们采用了全新的集群架构，并对主要功能进行了改进，同时还引入了大量新功能。

### 集群水平扩展性与可靠性

基于开创性的 Core + Replica 集群架构，EMQX 企业版 5.0 获得了更好的集群水平扩展性和可靠性：

- 单个集群支持至多 23 个节点并能够承载超过 1 亿 MQTT 连接，相比 4.x 版本实现了 [10 倍的接入能力提升](https://www.emqx.com/zh/blog/how-emqx-5-0-achieves-100-million-mqtt-connections)。
- 使用 Core-Replicant 模式部署时，由于 Replicant 节点是无状态的，动态伸缩增减 Replicant 节点数量不会影响集群稳定性。
- 大规模部署时节点脑裂风险以及脑裂后对业务产生的影响显著降低，为企业用户提供更加稳定可靠的服务。

目前 [EMQX Kubernetes Operator](https://www.emqx.com/zh/emqx-kubernetes-operator) 已经针对新集群架构进行了适配，结合新集群架构能够将 EMQX 部署为一个弹性的、无状态的 MQTT 服务。

### MQTT over QUIC，下一代物联网传输协议

QUIC 是下一代互联网协议 HTTP/3 的底层传输协议，它的出现能够大大扩充现有物联网的应用场景。

EMQX 企业版 5.0 将 QUIC 作为传输层引入到 MQTT 中，以满足多样化的应用场景并统一接入设备进行管理。

MQTT over QUIC 适用于位置不固定、或是需要频繁断连不适合做长连接的设备。

多项与 TCP/TLS 测试的对比表明，在弱网与多变的网络环境下，QUIC 都能够满足其高质量、稳定的消息通信需求，弥补了现有 TCP/TLS 传输层的不足。车联网、移动数据采集等场景的用户将从中受益。

现在，用户可以创建一个 MQTT over QUIC 监听器并使用 EMQ 提供的 SDK 接入物联网设备，EMQ 也正在以 OASIS 成员身份推动 MQTT over QUIC 的标准化落地。

更多信息请参考： [从零开始上手 MQTT over QUIC：快速体验下一代物联网标准协议](https://www.emqx.com/zh/blog/getting-started-with-mqtt-over-quic-from-scratch)

### 可视化双向数据集成

EMQX 企业版 5.0 数据集成包括规则引擎与数据桥接功能，能够以灵活、低代码的配置方式进行物联网数据的实时处理和与第三方数据系统的集成。

**Flow Editor 可视化编排规则处理数据流**

通过可视化查看 Flows 页面，改善规则数量较多情况下的维护和管理难度。现在，用户可以清晰看到数据筛选、处理与桥接的整个步骤，并实时监控这一链路中每个步骤的状态。

后续版本 EMQX 将允许用户在 Dashboard 上以拖拽的方式自由编排规则和数据桥接，通过可视化界面将物联网硬件数据流轻松连接在一起，使开发者专注于自有业务开发。

**更灵活的双向数据集成**

除了将设备数据桥接至外部系统外，还能从外部数据系统如另一个 MQTT 服务、Kafka 中桥接数据至 EMQX，并经过规则处理后发送到指定客户端。

双向数据集成适用于云端下发场景，在支撑持续大规模消息实时处理与下发，为物联网业务开发提供了更多的可能性。

**数据桥接磁盘缓存**

为数据桥接集成增加了磁盘缓存功能，数据桥接连接异常的情况下能够将期间产生的消息缓存起来，待连接恢复后继续发送，这为数据集成提供了极佳的可靠性，大大提升业务可用性。

**数据集成支持情况**

EMQX 企业版 5.0 中首批已获支持的数据集成包括 Webhook、MQTT、Kafka、InfluxDB、MySQL、Redis、GCP PubSub 以及 MongoDB，更多数据集成将在后续的维护版本（5.0.x 版本）中陆续加入。

### 全面的安全保障

**更灵活的访问控制**

EMQX 企业版 5.0 提供了用户名/密码、LDAP、JWT、PSK 和 X.509 证书等多种身份认证，以及消息发布订阅授权检查功能。

访问控制功能可以通过 Dashboard 进行配置，无需重启操作即可为整个集群启用访问控制安全设置，拥有更灵活、易用的操作体验。

认证授权还具有详细的统计指标，可以分别统计集群以及单个节点上认证器和授权检查器的执行情况，包括以下数据：

- 允许数：允许通过认证或授权检查的次数
- 拒绝数：拒绝通过认证或授权检查的次数
- 不匹配数：没有找到用户凭证或权限列表的次数
- 当前速率：当前执行速度

通过认证授权统计指标，用户可以及时发现如大量的失败认证/授权检查，及时感知安全系统的异常情况。

**过载保护与速率限制**

引入了过载保护功能，新的速率限制系统也提供了精度更高的分层速率控制支持，能够从客户端、监听器以及节点多个层面限制客户端行为确保应用按照用户预期的负载运行。

两者结合可以避免客户端过于繁忙以及过多的请求流量导致服务不稳定或故障。

### 全新的 Dashboard

采用全新 UI 设计风格的 Dashboard，优化了关键数据和指标数据的显示方式与内容，在提升视觉体验的同时，也提供了更全面、强大、易用的内置功能，为用户使用 EMQX 进行更多物联网业务开发提供了便利。

主要改进如下：

- 提供了访问控制管理页面
- 改进的数据集成管理 UI，支持可视化数据集成查看
- 更强大的热配置功能
- 更多诊断工具，例如慢订阅和在线追踪
- 更多数据管理：能够在 Dashboard 上管理保留消息、延迟发布数据

### 更好的扩展性

**新的插件机制** 允许通过插件包的的形式编译、分发、安装插件，当用户需要扩展功能时，可以下载独立的插件安装包，在 Web 界面完成上传即可进行安装，整个过程甚至不需要重启 EMQX 集群。

同时，一个规范的插件将会随身附带使用说明、插件主页地址等信息，普通用户可以依照说明快速将插件用起来，也为插件开发者提供了与用户沟通的渠道。

**更易用的 ExHook/gRPC 扩展** 允许创建多个 ExHook 实例，并为每个实例提供了详细的使用情况统计，同时还可以查看每个 Exhook 实例注册的钩子以及钩子参数，能够更好地感知 Exhook 扩展负载情况。

**更"原生"的多协议接入**

网关以统一的设计语言重新实现，针对每种协议特性提供不同的客户端管理页面、安全认证配置，让用户以更原生的方式接入。

由于每个网关都可以配置自己独立的认证，不同网关设备的认证信息现在可以相互隔离，满足更多场景的需求。

## 5.0.0

*发布日期: 2022-06-17*

### Horizontal Scalability

An extension to the Mnesia database was introduced, named 'Mria'.

EMQX nodes can continue to form a cluster (named 'core') exactly as in previous versions.

In version 5, Mria allows more nodes to join the cluster as 'replicants'.

The difference is, replicant nodes apply changes in the routing table (and configurations etc.) asynchronously, and the replicant nodes can be scaled up and down without affecting the data consistency.

This reduces the networking overheads of forming a full-mesh cluster.

In our most recent test, we managed to achieve 100 million MQTT clients connecting to a 23 nodes Mria cluster.

It's a 3 times larger cluster size compared to a typical version 4 cluster,

and 10 times more capacity comparing to our previous [10 million achievement](https://www.emqx.com/en/resources/emqx-v-4-3-0-ten-million-connections-performance-test-report)

### Reliability

With the help of the Mria clustering topology, the risk of suffering a split-view of the cluster due to network partitioning, and improves cluster stability in general.

- Only core nodes are involved in transactional writes
- Data is fully replicated to replicant nodes so they can read from local copy (i.e., from RAM)
- While core nodes are fully functional MQTT broker, one can also opt to dedicate them as pure data base nodes

### The new Dashboard

Version 5 comes with a fresh new design for the dashboard. Powered by `vue.js` at the frontend, and OpenAPI at the backend, it provides the most easy-to-use MQTT broker management UI.

Key improvements compared to version 4:

- Access control (authentication and authorization) management
- Improved rule engine and action management UI
- Visual aid view to display data flow e.g., from MQTT topics to destination data sink
- Online config update
- Gateway/Extension management
- More diagnosis tools, such as slow subscriptions and online tracing

### Typesafe Config

Starting from version 5, EMQX will use [HOCON](https://lightbend.github.io/config/) for the configuration syntax.

With a schema on top, the configs are now type-checked.

HOCON supports Nginx-like configuration layouts, and provides a native array syntax.

But don't worry, the old `cuttlefish` syntax is still supported if you prefer to keep everything flat like in version 4.

The EMQX team have put huge effort into providing sane-default values, as a result, the default configuration file is now less than 100 lines including comments.

Provided side-by-side, there is a full-blown example configuration (generated from the schema) to help us quickly find examples to copy from.

### Operability

In version 5, the interface descriptions for the Dashboard-enabled management APIs follow the OpenAPI specification 3.0. Behind the scene, the specification is actually generated from the configuration schema, the same schema which type-checks the files.

Now we have a single source of schema, which is used to guard both the configuration file and HTTP interface, also to generate config and API documents.

Another nice thing about Swagger, is that it comes with the Swagger-UI in which we can click a "try it out" button to test the API directly from the web browser, or copy-paste the `curl` command example to a console to test it out.

Another major change in operability compared to version 4 is that the changes are persisted back on disk as configuration file (in HOCON syntax).

The immediate benefit from unifying the config file interface and HTTP API interface is the hot-configuration.

Hot-configuration was a feature only available in EMQX Enterprise edition, it allows us to change a lot of configurations from Dashboard or through the APIs at runtime without having to restart the service.

In enterprise edition version 4, the hot-values were stored in the database, but not in config files, which was not very convenient for users who want to change the overridden values from the config file interface.

### Observability

The dashboard comes with more detailed monitoring metrics. We can now view up to 7 days of historical metrics data on Dashboard, and integrate with Prometheus with one click. Add log tracking and slow subscription diagnostic tools to effectively improve the user experience when troubleshooting and diagnosing abnormal client behaviors.

Another enhancement in observability is the introduction of structured logging, now most of the logs emitted from EMQX have a `msg` field. The text of this filed is an underscore-separated words, to make it more search-friendly for humans but also helps the log indexing tools to index the logs.

### Data Integration (The Old Rule Engine)

As you may notice from the dashboard, the old 'Rule Engine' has been renamed to 'Data integration'.

It includes two major functionalities: rules and data bridges.

Rule is a data processing language in SQL syntax (in addition a [`jq` language support](https://www.youtube.com/watch?v=_GwF8zvhNcQ)), which helps to filter and transform IoT messages.

Data bridge provides the ability to ingest data into EMQX or export data outside of EMQX.

Through the dashboard, users can now clearly see how IoT data is processed with rules and how the data flows from/to external data services.

Another important change: now rules and actions are configs (in config files), but not data in Mnesia (the built-in database, which is quite opaque for non-Erlang developers), meaning we'll be able to configure rules with various deployment/orchestration tools a lot easier.

### Access Control

Now we can manage authentication and authorization from the dashboard without having to restart the broker.

Previously provided as plugins, authentication and authorization (ACL) configurations are scattered in different files, and changes will often require a restart of the broker to take effect.
In version 5, all the commonly used security configurations are grouped together. There is also a user management UI provided in the dashboard, allowing you to manage users and access rules on the fly.

We can even configure different authentication rules per listener.

### Gateway

The gateway is re-implemented with a unified design language, providing independent management interfaces and security authentication capabilities for various protocols with different client attributes and life cycles, allowing users to access in a more native way. Since each gateway can be configured with its own independent authentication, authentication information of different gateway devices can now be isolated from each other to meet the needs of more scenarios.

### MQTT over QUIC

QUIC [RFC 9000](https://datatracker.ietf.org/doc/html/rfc9000), the next generation transport layer for the internet, brings amazing opportunities (and challenges too) to IoT.

EMQX team cannot wait to start experimenting MQTT on top of it.

Coming with the 5.0 release, one can configure a QUIC listener to experiment with MQTT over QUIC.

### Extension

EMQX 5.0 keeps and to a certain extent enhances the ability of plugin extensions. The plugins can now be compiled, distributed as a standalone package, and then uploaded on the Dashboard to complete the installation for the entire cluster, so they do not have to repeat the steps for each node.

### Hello Elixir

EMQX 5 is still a rebar3 project, but it now compiles with mix, if you know Elixir, you know what it means. Happy hacking.
