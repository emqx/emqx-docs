# Releases

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