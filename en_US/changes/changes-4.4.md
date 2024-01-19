# Releases

## 4.4.19

*Release Date: 2023-06-27*

### Enhancements

- Added support for TCP keep-alive in MQTT/TCP and MQTT/SSL listeners [#10854](https://github.com/emqx/emqx/pull/10854).

  A new configuration option has been added: `zone.<zone-name>.tcp_keepalive = Idle,Interval,Probes`. Users can enable the TCP layer's Keep Alive feature and specify time parameters using this configuration. This configuration is only effective on Linux and MacOS systems.

- Improved error logs related to Proxy Protocol [emqx/esockd#177](https://github.com/emqx/esockd/pull/177).

  The sample logs before this improvement:
  ```
  2023-04-20T14:56:51.671735+08:00 [error] supervisor: 'esockd_connection_sup - <0.2537.0>', errorContext: connection_shutdown, reason: {invalid_proxy_info,<<"f\n">>}, offender: [{pid,<0.3192.0>},{name,connection},{mfargs,{...}}]

  2023-04-20T14:57:01.348275+08:00 [error] supervisor: 'esockd_connection_sup - <0.2537.0>', errorContext: connection_shutdown, reason: {proxy_proto_timeout,5000}, offender: [{pid,<0.3194.0>},{name,connection},{mfargs,{...}}]
  ```
  After the improvement:
  ```
  2023-04-20T18:07:06.180134+08:00 [error] [esockd_proxy_protocol] The listener 127.0.0.1:8883 is working in proxy protocol mode, but received invalid proxy_protocol header, raw_bytes=<<"f\n">>

  2023-04-20T18:10:17.205436+08:00 [error] [esockd_proxy_protocol] The listener 127.0.0.1:8883 is working in proxy protocol mode, but timed out while waiting for proxy_protocol header
  ```

- Added a new feature to enable partial certificate chain validation for TLS listeners [#10553](https://github.com/emqx/emqx/pull/10553).

  For details please checkout the `listener.ssl.external.partial_chain` in the `emqx.conf` config file.

- Added a new feature to enable client certificate extended key usage validation for TLS listeners [#10669](https://github.com/emqx/emqx/pull/10669).

  For details please checkout the `listener.ssl.external.verify_peer_ext_key_usage` in the `emqx.conf` config file.

- Added the `live_connections` field in the HTTP API `/api/v4/nodes` response [#10859](https://github.com/emqx/emqx/pull/10859).

  Previously, this interface had a `connections` field, which represented the number of active connections on the current node that had not expired. This means that even if the MQTT connection has been disconnected, as long as the client has a persistent session, it would still be counted in the `connections` field. The newly added `live_connections` field specifically counts the number of clients with MQTT connections that have not been disconnected.

- Added 3 random SQL functions to the rule engine [#11113](https://github.com/emqx/emqx/pull/11113).

  - random(): Generates a random number between 0 and 1 (0.0 =< X < 1.0).
  - uuid_v4(): Generates a random UUID (version 4) string.
  - uuid_v4_no_hyphen(): Generates a random UUID (version 4) string without hyphens.

- Added numerical range validation (23-65535) for the `mqtt.max_clientid_len` configuration parameter [#11096](https://github.com/emqx/emqx/pull/11096).

### Bug fixes

- Fixed an issue where the rule engine was unable to access variables exported by `FOREACH` in the `DO` clause [#10620](https://github.com/emqx/emqx/pull/10620).

  Given a payload: `{"date": "2023-05-06", "array": ["a"]}`, as well as the following SQL statement:
  ```
  FOREACH payload.date as date, payload.array as elem
  DO date, elem
  FROM "t/#"
  ```
  Prior to the fix, the `date` variable exported by `FOREACH` could not be accessed in the `DO` clause of the above SQL, resulting in the following output for the SQL statement:
  `[{"elem": "a","date": "undefined"}]`.
  After the fix, the output of the SQL statement is: `[{"elem": "a","date": "2023-05-06"}]`

- Fixed the issue where the cache of rules failed to update in certain cases [#11072](https://github.com/emqx/emqx/pull/11072).

  Prior to the fix, after manually updating the rules, there could be instances where the cache update did not synchronize to certain nodes. This would result in inconsistent rule execution states across different nodes.

- Fixed an issue where the WebHook plugin failed to execute the `on_client_connack` hook [#10710](https://github.com/emqx/emqx/pull/10710).

  See https://github.com/emqx/emqx/issues/10628 for more details.

## 4.4.18

*Release Date: 2023-04-28*

### Enhancements

- Improved the placeholder syntax of the rule engine [#10470](https://github.com/emqx/emqx/pull/10470).

  The parameters of certain actions support using placeholder syntax to dynamically fill in the content of strings. The format of the placeholder syntax is `${key}`.\
  Prior to the improvement, the `key` in `${key}` could only contain letters, numbers, and underscores. Now the `key` supports any UTF8 character after the improvement.


## 4.4.17

*Release Date: 2023-04-13*

### Enhancements

- When the listener enabled with `Proxy Protocol` receives a TCP port probe, no error logs will be printed anymore [emqx/esockd#172](https://github.com/emqx/esockd/pull/172).

  Before the fix, if the listener had enabled the proxy protocol (`listener.tcp.external.proxy_protocol=on`), but the connection was disconnected after the TCP handshake was completed and before the proxy information was received, the following error log would be printed:

  ```
  [error] supervisor: 'esockd_connection_sup - <0.3265.0>', errorContext: connection_shutdown, reason: {recv_proxy_info_error,tcp_closed}, offender:
  ```
  After the fix, no logs will be printed, but you can still view the error reason statistics through the `emqx_ctl listeners` command.

- Improved the error logs of the listener for file descriptor exhaustion [emqx/esockd#173](https://github.com/emqx/esockd/pull/173).

  Before the improvement, the log was:
  ```
  [error] Accept error on 0.0.0.0:1883: emfile
  ```
  After the improvement, the log became:
  ```
  [error] Accept error on 0.0.0.0:1883: EMFILE (Too many open files)
  ```

- Improved the performance of the rule engine when there are many rules [#10283](https://github.com/emqx/emqx/pull/10283)

  Before the improvement, when there were many rules, the rule engine would consume a lot of CPU time on rule queries and matching, becoming a performance bottleneck.
  In this optimization, by simply adding a cache to the rule list, the rule execution efficiency in this scenario was greatly improved.
  In our test, we created 700 rules that did not perform any actions (bound to the "do_nothing" debugging action) on a 32-core 32G virtual machine, and sent MQTT messages to EMQX at a rate of 1000 messages per second (that is, the rule trigger frequency was 700 * 1000 times per second).
  In the above scenario, the CPU usage of the optimized rule engine dropped to 55% ~ 60% of the previous level.

### Fixes

- Fixed the issue where `Erlang distribution` could not use TLS [#9981](https://github.com/emqx/emqx/pull/9981).

  For more information on `Erlang distribution`, see [here](https://www.emqx.io/docs/en/v4.4/advanced/cluster.html).

- Fixed the issue where MQTT bridging could not verify TLS certificates with wildcard domains on the peer side [#10094](https://github.com/emqx/emqx/pull/10094).

- Fixed the issue where EMQX could not timely clear the information of disconnected MQTT connections when there were too many messages backlogged in the retainer. [#10189](https://github.com/emqx/emqx/pull/10189).

  Before the fix, the `emqx_retainer` plugin and the EMQX connection information cleanup task shared a process pool. Therefore, if the process pool was blocked by a large number of retain message distribution tasks, many disconnected MQTT connection information would not be cleared in time. See [#9409](https://github.com/emqx/emqx/issues/9409) for details.
  After the fix, the `emqx_retainer` plugin uses a separate process pool to avoid this problem.

- Fixed the issue where the path of the template file `service-monitor.yaml` in the Helm Chart was incorrect. [#10229](https://github.com/emqx/emqx/pull/10229)

## 4.4.16

*Release Date: 2023-03-10*

### Enhancements

- Change "EMQX" to "EMQX" from the outputs of CLIs and names of plugins [#10099](https://github.com/emqx/emqx/pull/10099).

### Bug fixes

- Avoid changing the payload of MQTT messages when printing debug logs [#10091](https://github.com/emqx/emqx/pull/10091).
  Before this fix, if EMQX receives a message with Payload "e\ne\nc\nc\n2\n\n\n", the log message will be as follows:
  ```
  2023-03-08T13:28:04.320622+08:00 [debug] mqttx_e34bd582@127.0.0.1:54020 [MQTT] RECV PUBLISH(Q1, R0, D0, Topic=t/1, PacketId=39467, Payload=e, e, c, c, 2, , , )
  ```
  This is the corresponding log message now:
  ```
  2023-03-08T14:26:50.935575+08:00 [debug] mqttx_e34bd582@127.0.0.1:54020 [MQTT] RECV PUBLISH(Q1, R0, D0, Topic=t/1, PacketId=39467, Payload=<<"e\ne\nc\nc\n2\n\n\n">>)
  ```

## 4.4.15

*Release Date: 2023-03-03*

This version update includes 8 enhancements and 13 fixes.
Among the enhancements, there are new exciting new features worth highlighting:

- Upgrade the MongoDB client library of EMQX to support MongoDB 5.1 and above.
- Dashboard supports the proxy protocol of HAProxy.
- Release the Ubuntu 22.04 installation package.

### Enhancements

- The MongoDB library has been upgraded to support MongoDB version 5.1 and greater.

- Support proxy protocol of HAProxy for dashboard API [9803](https://github.com/emqx/emqx/pull/9803).

- Added Ubuntu 22.04 package release [#9831](https://github.com/emqx/emqx/pull/9831).

- Improve the integration of the `banned` and the `delayed` feature [#9790](https://github.com/emqx/emqx/pull/9790).
  Now when publishing a delayed message will check first if its source client is banned, if true, this publish will be ignored.

- Security enhancement for retained messages [#9790](https://github.com/emqx/emqx/pull/9790).
  The retained messages will not be published if the publisher client is banned.

- Now the corresponding session will be kicked when client is banned by `clientid` [#9904](https://github.com/emqx/emqx/pull/9904).

- Add more debug logs for authentication and ACL [#9943](https://github.com/emqx/emqx/pull/9943).

- Expose the stats `live_connections.count` and `live_connections.max` to Prometheus [#9929](https://github.com/emqx/emqx/pull/9929).

### Bug fixes

- Fixed an error when forward MQTT messages with User-Property using the `republish` action [#9942](https://github.com/emqx/emqx/pull/9942).

- fix some issues in descriptions of the actions, resources and emqx-modules [#9931](https://github.com/emqx/emqx/pull/9931).

- fix there's no error logs when query the JWKS server failed [#9931](https://github.com/emqx/emqx/pull/9931).

- The returned client lists of HTTP query `GET /api/v4/clients?_page=2&_limit=20` to different nodes might be inconsistent [#9926](https://github.com/emqx/emqx/pull/9926).

- Fix the problem that new MQTT TLS connections failed to establish after release hot upgrade [#9810](https://github.com/emqx/emqx/pull/9810).
  For more detailed information please see: [emqx/esockd#170](https://github.com/emqx/esockd/pull/170).

- Fix a problem in the log message format of MQTT packets [#9858](https://github.com/emqx/emqx/pull/9858).
  Before this fix, a comma was missing between the flags (DUP) of the fixed header
  and the fields (ClientId) of the variable header:
  ```
  2023-01-29T13:40:36.567692+08:00 [debug] 127.0.0.1:50393 [MQTT] RECV CONNECT(Q0, R0, D0ClientId=test_client, ... Password=undefined)
  ```

- Avoid crash logs in CoAP gateway when receiving liveness checking packets from Load Balancer [#9869](https://github.com/emqx/emqx/pull/9869).

- Fix the exclusive topics aren't removed when the session has already been cleaned [#9868](https://github.com/emqx/emqx/pull/9868).

- fix the EMQX reports `{case_clause,{error,closed}}` error log message when websocket connections interrupted [emqx/cowboy#8](https://github.com/emqx/cowboy/pull/8).

- fix sometimes the rules cannot be enabled automatically after EMQX is restarted [#9911](https://github.com/emqx/emqx/pull/9911).

- fix the `{badarg,[{ets,lookup,[gproc,{shared, ...` error logs during shutdown [#9919](https://github.com/emqx/emqx/pull/9919).

- Delete the files directory when `resources` were deleted to avoid files leaking [#10039](https://github.com/emqx/emqx/pull/10039).

## 4.4.11

*Release Date: 2022-11-26*

This release included 18 enhancements and 14 bug fixes.
Among the enhancements, therer are new exciting new features worth highlighting.

- OCSP (Online Certificate Status Protocol) Stapling.
- CRL (Certificate Revocation List) cache.
- OTP upgrade from 24.1.5-3 to 24.3.4.2-1.
- Customizable client aliases to make it easier to when creating customized authentication and authorization.

It is possible to hot-upgrade from older version v4.4 to this version.
Please note though, in order to start making use of the new features such as OCSP Stapling, CRL cache,
a node restart (and configuration change) is required.

### Enhancements

- OTP upgrade from 24.1.5-3 to 24.3.4.2-1 [#9265](https://github.com/emqx/emqx/pull/9265).
  Change highlights:
    - Erlang/OTP [SSL library vulnerability fix](https://nvd.nist.gov/vuln/detail/CVE-2022-37026)
    - Added support for OCSP (Online Certificate Status Protocol) Stapling
    - Added CRL (Certificate Revocation List) cache auto refresh

- Added support for OCSP stapling and CRL
  caching [#9297](https://github.com/emqx/emqx/pull/9297).

- Added support for specifying custom modules for adding clientid and common name
  aliases [#9297](https://github.com/emqx/emqx/pull/9297).
  Now you can implement a simple callback to enrich clients with aliases, and then use the aliases
  in the authentication and authorization (ACL) rules' place holders (`%cida` for clientid alias
  and `%cna` for username alias).

- Added support for specifying custom modules for custom authentication [#9297](https://github.com/emqx/emqx/pull/9297).
  To support simple authentication rules, it is no longer necessary to implement a full-blown plugin.

- Added a JWT management for Rule-Engine, for creating and refreshing JWT tokens in rule engine actions [#9241](https://github.com/emqx/emqx/pull/9241).
  This feature is so far only used in EMQX Enterprise Google PubSub integration.
  Can be used as webhook integration's JWT authentication against the webhook service endpoint.

- Make sure listener's `tls_versions` config value is one or more of `tlsv1`, `tlsv1.1`, `tlsv1.2`, `tlsv1.3` [#9260](https://github.com/emqx/emqx/pull/9260).

- Remove useless information from the dashboard listener failure log [#9260](https://github.com/emqx/emqx/pull/9260).

- We now trigger the `'message.acked'` hook after the CoAP gateway sends a message to the device and receives the ACK from the device [#9264](https://github.com/emqx/emqx/pull/9264).
  With this change, the CoAP gateway can be combined with the offline message caching function (in the
  emqx enterprise), so that CoAP devices are able to read the missed messages from the database when
  it is online again.

- Support to use placeholders like `${var}` in the HTTP `Headers` of rule-engine's Webhook actions [#9239](https://github.com/emqx/emqx/pull/9239).

- Asynchronously refresh the resources and rules during emqx boot-up [#9199](https://github.com/emqx/emqx/pull/9199).
  This is to avoid slowing down the boot if some resources spend long time establishing the connection.

- Add a warning log if the ACL check failed for subscription [#9124](https://github.com/emqx/emqx/pull/9124).
  This is to make the ACL deny logging for subscription behave the same as for publish.

- JWT ACL claim supports `all` action to imply the rules applie to both `pub` and `sub` [#9044](https://github.com/emqx/emqx/pull/9044).

- Added a log censor to avoid logging sensitive data [#9189](https://github.com/emqx/emqx/pull/9189).
  If the data to be logged is a map or key-value list which contains sensitive key words such as `password`, the value is obfuscated as `******`.

- Enhanced log security in ACL modules, sensitive data will be obscured [#9242](https://github.com/emqx/emqx/pull/9242).

- Add `management.bootstrap_apps_file` configuration to bulk import default app/secret when EMQX initializes the database [#9273](https://github.com/emqx/emqx/pull/9273).

- Added two new configs for deterministic order of authentication and ACL checks [#9283](https://github.com/emqx/emqx/pull/9283).
  The two new global config names are `auth_order` and `acl_order`.
  When multiple ACL or auth plugins (or modules) are enabled, without this config, the order (in which each backend is queried)
  is determined by the start/restart order of the plugin (or module).
  Meaning, if a plugin (or module) is restarted after initial boot, it may get ordered to the end of the list.
  With this config, you may set the order with a comma-speapated ACL or auth plugin names (or aliases).
  For example: `acl_order = jwt,http`, this will make sure `jwt` is always checked before `http`,
  meaning if JWT is not found (or no `acl` cliam) for a client, then the ACL check will fallback to use the HTTP backend.

- Added configurations to enable more `client.disconnected` events (and counter bumps) [#9267](https://github.com/emqx/emqx/pull/9267).
  Prior to this change, the `client.disconnected` event (and counter bump) is triggered when a client
  performs a 'normal' disconnect, or is 'kicked' by system admin, but NOT triggered when a
  stale connection had to be 'discarded' (for clean session) or 'takeovered' (for non-clean session) by new connection.
  Now it is possible to set configs `broker.client_disconnect_discarded` and `broker.client_disconnect_takeovered` to `on` to enable the event in these scenarios.

- For Rule-Engine resource creation failure, delay before the first retry [#9313](https://github.com/emqx/emqx/pull/9313).
  Prior to this change, the retry delay was added *after* the retry failure.

### Bug fixes

- Fix get trace list crash when trace not initialize. [#9156](https://github.com/emqx/emqx/pull/9156)

- Fix create trace sometime failed by end_at time has already passed. [#9156](https://github.com/emqx/emqx/pull/9156)

- Fix that after uploading a backup file with an non-ASCII filename, HTTP API `GET /data/export` fails with status code 500 [#9224](https://github.com/emqx/emqx/pull/9224).

- Improve the display of rule's 'Maximum Speed' counter to only reserve 2 decimal places [#9185](https://github.com/emqx/emqx/pull/9185).
  This is to avoid displaying floats like `0.30000000000000004` on the dashboard.

- Fix the issue that emqx prints too many error logs when connecting to mongodb but auth failed [#9184](https://github.com/emqx/emqx/pull/9184).

- Fix that after receiving publish in `idle mode` the emqx-sn gateway may panic [#9024](https://github.com/emqx/emqx/pull/9024).

- "Pause due to rate limit" log level demoted from warning to notice [#9134](https://github.com/emqx/emqx/pull/9134).

- Restore old `emqx_auth_jwt` module API, so the hook callback functions registered in older version will not be invalidated after hot-upgrade [#9144](https://github.com/emqx/emqx/pull/9144).

- Fixed the response status code for the `/status` endpoint [#9210](https://github.com/emqx/emqx/pull/9210).
  Before the fix, it always returned `200` even if the EMQX application was not running.  Now it returns `503` in that case.

- Fix message delivery related event encoding [#9226](https://github.com/emqx/emqx/pull/9226)
  For rule-engine's input events like `$events/message_delivered`, and `$events/message_dropped`,
  if the message was delivered to a shared-subscription, the encoding (to JSON) of the event will fail.
  Affected versions: `v4.3.21`, `v4.4.10`, `e4.3.16` and `e4.4.10`.

- Make sure Rule-Engine API supports Percent-encoding `rule_id` and `resource_id` in HTTP request path [#9190](https://github.com/emqx/emqx/pull/9190).
  Note that the `id` in `POST /api/v4/rules` should be literals (not encoded) when creating a `rule` or `resource`.
  See docs [Create Rule](https://docs.emqx.com/en/enterprise/v4.4/advanced/http-api.html#post-api-v4-rules) [Create Resource](https://docs.emqx.com/en/enterprise/v4.4/advanced/http-api.html#post-api-v4-resources).

- Calling 'DELETE /alarms/deactivated' now deletes deactived alarms on all nodes, including remote nodes, not just the local node [#9280](https://github.com/emqx/emqx/pull/9280).

- When republishing messages or bridge messages to other brokers, check the validity of the topic and make sure it does not have topic wildcards [#9291](https://github.com/emqx/emqx/pull/9291).

- Disable authorization for `api/v4/emqx_prometheus` endpoint on management api listener (default 8081) [#9294](https://github.com/emqx/emqx/pull/9294).

## 4.4.10

*Release Date: 2022-10-14*

### Enhancements

- TLS listener memory usage optimization [#9005](https://github.com/emqx/emqx/pull/9005).
  New config `listener.ssl.$NAME.hibernate_after` to hibernate TLS connection process after idling.
  Hibernation can reduce RAM usage significantly, but may cost more CPU.
  This configuration is by default disabled.
  Our preliminary test shows a 50% of RAM usage decline when configured to '5s'.

- TLS listener default buffer size to 4KB [#9007](https://github.com/emqx/emqx/pull/9007)
  Eliminate uncertainty that the buffer size is set by OS default.

- Disable authorization for `api/v4/emqx_prometheus` endpoint [#8955](https://github.com/emqx/emqx/pull/8955).

- Added a test to prevent a last will testament message to be
  published when a client is denied connection [#8894](https://github.com/emqx/emqx/pull/8894).

- More rigorous checking of flapping to improve stability of the system [#9045](https://github.com/emqx/emqx/pull/9045).
  Previsouly only normal disconnects are counted, now the connection rejections (e.g. authentication failure) is also included.
  Find more about flapping detection in [EMQX document](https://www.emqx.io/docs/en/v4.3/configuration/configuration.html#flapping-detect-policy)

- QoS1 and QoS2 messages in session's buffer are re-dispatched to other members in the group
  when the session terminates [#9094](https://github.com/emqx/emqx/pull/9094).
  Prior to this enhancement, one would have to set `broker.shared_dispatch_ack_enabled` to `true`
  to prevent sessions from buffering messages, however this acknowledgement costs extra resources.

- Fix delayed publish timing inaccuracy caused by OS time change [#8908](https://github.com/emqx/emqx/pull/8908).

### Bug fixes

- Fix the latency statistics error of the slow subscription module when `stats_type` is `internal` or `response` [#8981](https://github.com/emqx/emqx/pull/8981).

- Fix HTTP client library to handle SSL socket passive signal [#9145](https://github.com/emqx/emqx/pull/9145).

- Hide redis password in error logs [#9071](https://github.com/emqx/emqx/pull/9071)
  In this change, it also included more changes in redis client:
  - Improve redis connection error logging [eredis#19](https://github.com/emqx/eredis/pull/19).
    Also added support for eredis to accept an anonymous function as password instead of
    passing around plaintext args which may get dumpped to crash logs (hard to predict where).
    This change also added `format_status` callback for `gen_server` states which hold plaintext
    password so the process termination log and `sys:get_status` will print '******' instead of
    the password to console.
  - Avoid pool name clashing [eredis_cluster#22](https://github.com/emqx/eredis_cluster/pull/22).
    Same `format_status` callback is added here too for `gen_server`s which hold password in
    their state.

- Fix shared subscription message re-dispatches [#9094](https://github.com/emqx/emqx/pull/9094).
  - When discarding QoS 2 inflight messages, there were excessive logs
  - For wildcard deliveries, the re-dispatch used the wrong topic (the publishing topic,
    but not the subscribing topic), caused messages to be lost when dispatching.

- Fix shared subscription group member unsubscribe issue when 'sticky' strategy is used.
  Prior to this fix, if a previously picked member unsubscribes from the group (without reconnect)
  the message is still dispatched to it.
  This issue only occurs when unsubscribe with the session kept.
  Fixed in [#9119](https://github.com/emqx/emqx/pull/9119)

- Fix shared subscription 'sticky' strategy when there is no local subscriptions at all.
  Prior to this change, it may take a few rounds to randomly pick group members until a local subscriber
  is hit (and then start sticking to it).
  After this fix, it will start sticking to whichever randomly picked member even when it is a
  subscriber from another node in the cluster.
  Fixed in [#9122](https://github.com/emqx/emqx/pull/9122)

- Fix rule engine fallback actions metrics reset [#9125](https://github.com/emqx/emqx/pull/9125).

## 4.4.9

*Release Date: 2022-09-17*

### Enhancements

- The `exp`, `nbf` and `iat` claims in JWT authentication support non-integer timestamps

### Bug fixes

- Fix rule engine update behaviour which may initialize actions for disabled rules
- Fix the issue that the IP address bound to the Dashboard listener did not take effect
- Fix the issue that shared subscriptions might get stuck in an infinite loop when `shared_dispatch_ack_enabled` is set to true
- Fix the issue that the rule engine SQL crashes when subject matching null values

## 4.4.8

*Release Date: 2022-08-29*

### Enhancements

- Add `GET /trace/:name/detail` API to view log trace file information
- Improve the log when LwM2M packet parsing fails
- Improve the rule engine error log, the log will contain the rule ID when the action execution fails
- Improve log when `loaded_modules` and `loaded_plugins` files do not exist
- Add a guide for changing the default password on Dashboard

### Bug fixes

- Fix `client.disconnected` event not trigger in some cases
- Fix the issue that the built-in database authentication did not distinguish the pagination statistics of the authentication data of the client ID and username
- Fix Redis driver process leak problem
- Fix rule engine MQTT bridge to AWS IOT connection timeout issue
- Fix `GET /listener` request crashing when listener is not ready
- Fix the issue that the comparison between any variable and null value in the rule engine SQL always returns false after v4.4.1
- Fix incorrectly managing `emqx_modules` applications as plugins
- Fix the issue that when the execution priority of ExHook is higher than that of the rule engine, the topic filtered by the ExHook Message Hook will not trigger the rule engine
- Fix the issue that the ExHook management process was forcibly killed due to the supervisor shutdown timeout
- Fix the issue that the Client ID parameter in ExProto `client.connect` hook is not defined
- Fix ExProto not triggering disconnect event when client is kicked

## 4.4.7

*Release Date: 2022-08-11*

### Important Changes

- As of version 4.4.7, we will no longer provide packages for macOS 10

### Enhancements

- Allows the connection process to be configured to be garbage collected after the TLS handshake is complete to reduce memory footprint, which can reduce memory consumption by about 35% per SSL connection, but increases CPU consumption accordingly
- Allows configuring the log level of the TLS handshake log to view the detailed handshake process

## 4.4.6

*Release Date: 2022-07-29*

### Enhancement

- Supports searching and paging of rules in rule engine
- Provides CLI `./bin/emqx check_conf` to actively check if the configuration is correct
- Optimizing Shared Subscription Performance

### Bug fixes

- Fix the issue that once the old version of EMQX is uninstalled after hot upgrade, EMQX will not be able to start again
- Fix the issue that the keep-alive check for UDP clients in the Multilingual Protocol Extension was incorrect, causing clients not to expire
- Fix the issue that the client information in the Multilingual Protocol Extension was not updated in time
- Fix the issue that when the client specified Clean Session as false to reconnect, the shared subscription message in the flight window would be re-dispatched to the old session process
- Fix the issue that the `emqx_lua_hook` plugin cannot cancel the message publishing

## 4.4.5

*Release Date: 2022-06-30*

### Enhancement

- QoS and Retain flag in rule engine's message republish actions can now use placeholders
- Supports exclusive subscriptions, that is, only one subscriber is allowed for a topic
- Dashboard and management API's HTTPS listeners can now use password-protected private key files, providing `key_password` configuration item
- Support for placeholders `%u` and `%c` in topic rewrite rules
- Support setting MQTT 5.0 properties in the API request for message publishing, such as message expiry interval, response topic, etc.
- Optimize the UI when creating rule engine resources, such as folding some uncommon options, etc.
- Opened 4 TCP-related configuration items: KeepAlive, TCP_NODELAY, SO_RCVBUF and SO_SNDBUF for the underlying gRPC connection of ExHook

### Bug fixes

- Fix the issue of inaccurate memory calculation in Linux OS, and calculate the memory usage of the current OS instead of the memory usage of EMQX
- Fix the issue that the old disconnect event of ExHook would be triggered later than the new connect event when the client reconnects
- Fix the issue that the execution order of topic rewriting and delayed publish is not fixed, now it is fixed to execute topic rewriting first
- Fix the issue that rule engine could not encode MQTT 5.0 user properties
- Fix the issue that the count of `connack.auth_error` is inaccurate when the client uses a protocol version below MQTT v5.0 to access
- Fix the issue that the UDP listeners of LwM2M and CoAP gateways could not bind to the specified network interface
- Fix Dashboard not starting after removing the default Dashboard user in the configuration file
- Fix `client.subscribe` hook not being able to reject subscriptions
- If the placeholder in the ACL rule is not replaced, the client's publish or subscribe operation will be rejected

## 4.4.4

*Release Date: 2022-06-01*

### Enhancement

- Add more time transformation functions to the SQL of rule engine
- Add the `float2str/2` function to the SQL of rule engine to support specifying the output precision of floating point numbers
- Support for using JWT for authorization, now MQTT clients can authorize using specific claims that include a pub-sub whitelist
- Improved authentication related metrics to make it easier to understand, now `client.authenticate = client.auth.success + client.auth.failure`
- Support binding the listener of the REST API to a specified network interface
- Support multi-condition query and fuzzy query for user data in authentication and authorization using built-in database as data source
- Supports querying clients using the length of the message queue and the number of dropped messages as conditions
- Support to configure the log time format to be compatible with the time format in older versions
- When `use_username_as_clientid` is configured to `true` and the client connects without specifying a `username`, the connection is now rejected with a reason code `0x85`
- Full randomisation of app secrets (previously partially randomised)
- When using CLI for backup and recovery, it is no longer required that the backup file must be located in the `backup` folder of the EMQX data directory
- Hot upgrades between incompatible versions will now be rejected
- Allow white spaces in EMQX's installation path
- Boot script fail fast on invalid node name (improve error message readability)

### Bug fixes

- Fix the issue that rule engine's SQL function `hexstr_to_bin/1` could not handle half-byte
- Fix the issue that the alarm was not cleared when the rule engine resource was deleted
- Fix Dashboard HTTPS listener's `verify` option not taking effect
- Fix the issue that messages were lost when the peer session was terminated during the delivery of QoS 1 messages through shared subscriptions
- Fix the issue that when the log tracer encounters large packets, the heap size grows too fast and triggers the policy of forcibly closeing the connection process
- Fix the issue that the MQTT-SN client would be disconnected when retransmitting QoS 2 messages
- Fix the issue that the subscriber's connection was disconnected due to the wrong user properties type in the message publishing API `api/v4/mqtt/publish`
- Fix the issue that some authentication algorithms were unavailable due to the PostgreSQL driver not adapting to OTP 24
- Fix the issue that the returned results did not match the query conditions when querying subscriptions with multiple conditions
- Fix rule engine resource connection test not working
- Fix multiple Dashboard display issues

## 4.4.3

*Release Date: 2022-04-18*

### Enhancement

- Rule engine supports resetting metrics of the specified rule
- Add connection confirmation and authorization completion events to the rule engine
- Rule engine supports copying rule for fast reuse
- SQL in rule engine supports zip, gzip and other compression and decompression functions
- Improve the error message when rule engine fails to parse payload
- Improve the connection test for some resources in rule engine
- Support setting execution priority for ExHook
- ExHook callback interface adds a Protobuf field `RequestMeta meta` to return the EMQX cluster name
- Support `local` policy for shared subscriptions, which will preferentially send messages to shared subscribers under the node where messages flow in. In some scenarios, the efficiency of shared message scheduling will be improved, especially when the MQTT bridge is configured as a shared subscription
- `RSA-PSK-AES256-GCM-SHA384`, `RSA-PSK-AES256-CBC-SHA384`, `RSA-PSK-AES128-GCM-SHA256` and `RSA-PSK-AES128-CBC- SHA256` four new TLS PSK cipher suites are supported, removing two insecure cipher suites `PSK-3DES-EDE-CBC-SHA` and `PSK-RC4-SHA` from the default configuration
- Diagnostic logging for `wait_for_table` of mnesia
  - Prints check points of mnesia internal stats
  - Prints check points of per table loading stats, help to locate the problem of long table loading time.
- Subscribing to an empty topic is prohibited in strict mode
- Generate default files when `loaded_modules` and `loaded_plugins` files do not exist

### Bug fixes

- Fix the issue that the TLS configuration item `server_name_indication` is set to disable and does not take effect
- Fix potential process leak issue in MongoDB driver
- Fix the issue that the password of the default Dashboard user modified via the CLI command would be reset after the node leaves the cluster
- Silence grep and sed warnings in `docker-entrypoint.sh`
- Fix the backup file cannot be deleted and downloaded when the API path contains ISO8859-1 escape characters
- Fix the issue that the Redis driver would crash when DNS resolution failed, etc
- Fix the issue that the headers field configuration in the `Data to Web Server` action of the rule engine did not take effect
- Fix the issue that the MQTT Bridge plugin cannot be started when only the subscription topic is configured but QoS is not configured
- When creating a rule, if a rule with the same ID already exists, the rules engine will now report an error instead of replacing the existing rule
- Fix the issue that the HTTP driver process pool may not be deleted

## 4.4.2

*Release Date: 2022-04-01*
### Important changes

- For Docker images, the configuration directory `/opt/emqx/etc` has been removed from the VOLUME list, making it easier for users to rebuild images with changed configurations.
- CentOS 7 Erlang runtime rebuilt on OpenSSL-1.1.1n (previously 1.0), prior to v4.3.13, EMQX will fail to handshake and trigger `malformed_handshake_data` exception when clients use certain cipher suites.
- CentOS 8 Erlang runtime system rebuilt on RockyLinux 8. `centos8` will remain in the package name for backward compatibility.

### Enhancement

- Windows package support for building on Erlang/OTP 24.
- Add command line interface `emqx_ctl pem_cache clean` to allow forcibly clear x509 certificate cache to reload immediately after certificate file update.
- Refactored ExProto so that anonymous clients can also be displayed on Dashboard.
- Topic configuration items in bridges can now use `${node}` placeholders.
- Add validation of UTF-8 strings in MQTT packets in strict mode. When set to `true`, invalid UTF-8 strings will cause the client to disconnect.
- MQTT-SN gateway supports initiative to synchronize registered topics after session resumed.
- Improve the writing precision of rule engine floating point data from 10 decimal places to 17 decimal places.
- EMQX will prompt how to modify the initial password of Dashboard at startup.

### Bug fixes

- Fix the issue that the el8 installation package cannot be started on Amazon Linux 2022, the error content is `errno=13 Permission denied`.
- Fix an issue where the client could not reconnect if the connection process was blocked in some cases. Now waiting for more than 15 seconds without a response will force the old connection process to be closed.
- Fix the issue of query resource request timeout when rule engine resource is unavailable.
- Fix the issue of `{error, eexist}` error when re-run after hot upgrade failed.
- Fix an issue where publishing to a non-existing topic alias would crash the connection.
- Fix 500 error when querying lwm2m client list on another node via HTTP API.
- Fix HTTP API for subscribing topics crashes when invalid QoS are passed in.
- Fix the issue that the connection count was not updated because the related resources were not released when the connection process accessed through the ExProto exited abnormally.
- Fix an issue where the value of `server_keepalive` configuration item would be incorrectly applied to MQTT v3.1.1 clients.
- Fix Stomp client not firing `$event/client_connection` event messages.
- Fix the issue that the system memory alarm was incorrectly activated when EMQX was started.
- Fixed an issue where messages that failed to be delivered due to unregistered topics were not retransmitted when topics were successfully registered with the MQTT-SN client.
- Fix EMQX startup output error log when duplicate plugins are configured in `loaded_plugins` file.
- Fix MongoDB related features outputting excessive error logs when configured incorrectly.
- Add format check for Dashboard User and AppID, special characters such as `/` are not allowed.
- Corrected the reason code in the DISCONNECT packet returned when kicking the client to `0x98`.
- Auto subscriptions will ignore empty topics.

## 4.4.1

*Release Date: 2022-02-21*

This patch release only includes a CI change for the Windows package.

## 4.4.0

*Release Date: 2022-02-18*

NOTE:

- 4.4.0 is in sync with: 4.3.12.
- The build of Windows package has some issues in the current version, we will fix it in the next version

The compare base of this change set is 4.3.12

### Important changes

- **For Debian/Ubuntu users**, Debian/Ubuntu package (deb) installed EMQX is now started from systemd.
  This is to use systemd's supervision functionality to ensure that EMQX service restarts after a crash.
  The package installation service upgrade from init.d to systemd has been verified,
  it is still recommended that you verify and confirm again before deploying to the production environment,
  at least to ensure that systemd is available in your system

- Package name scheme changed comparing to 4.3.
  4.3 format: emqx-centos8-4.3.8-amd64.zip
  4.4 format: emqx-4.4.0-rc.1-otp24.1.5-3-el8-amd64.zip
  * Erlang/OTP version is included in the package name,
    providing the possibility to release EMQX on multiple Erlang/OTP versions
  * `centos` is renamed to `el`. This is mainly due to centos8 being dead (replaced with rockylinux8)

- MongoDB authentication supports DNS SRV and TXT Records resolution, which can seamlessly connect with MongoDB Altas

- Support dynamic modification of MQTT Keep Alive to adapt to different energy consumption strategies.

- Support 4.3 to 4.4 rolling upgrade of clustered nodes. See upgrade document for more dtails.

- TLS for cluster backplane (RPC) connections. See clustering document for more details.

- Add new feature for `slow subscription` to count the time spent in the process of message transmission, and record and display the time-consuming clients and topics.

- Add new feature for `online log tracing` to support real-time tracing of client events and viewing on the dashboard.

### Minor changes

- Bumped default boot wait time from 15 seconds to 150 seconds
  because in some simulated environments it may take up to 70 seconds to boot in build CI

- Dashboard supports relative paths and custom access paths

- Supports configuring whether to forward retained messages with empty payload to suit users
  who are still using MQTT v3.1. The relevant configurable item is `retainer.stop_publish_clear_msg`

- Multi-language hook extension (ExHook) supports dynamic cancellation of subsequent forwarding of client messages

- Rule engine SQL supports the use of single quotes in `FROM` clauses, for example: `SELECT * FROM 't/#'`

- Change the default value of the `max_topic_levels` configurable item to 128.
  Previously, it had no limit (configured to 0), which may be a potential DoS threat

- Improve the error log content when the Proxy Protocol message is received without `proxy_protocol` configured.

- Add additional message attributes to the message reported by the gateway.
  Messages from gateways such as CoAP, LwM2M, Stomp, ExProto, etc., when converted to EMQX messages,
  add fields such as protocol name, protocol version, user name, client IP, etc.,
  which can be used for multi-language hook extension (ExHook)

- HTTP client performance improvement

- Add openssl-1.1 to RPM dependency

## 4.3.22

*Release Date: 2022-11-26*

### Enhancements

- Make sure listener's `tls_versions` config value is one or more of `tlsv1`, `tlsv1.1`, `tlsv1.2`, `tlsv1.3` [#9260](https://github.com/emqx/emqx/pull/9260).

- Remove useless information from the dashboard listener failure log [#9260](https://github.com/emqx/emqx/pull/9260).

- We now trigger the `'message.acked'` hook after the CoAP gateway sends a message to the device and receives the ACK from the device [#9264](https://github.com/emqx/emqx/pull/9264).
  With this change, the CoAP gateway can be combined with the offline message caching function (in the
  emqx enterprise), so that CoAP devices are able to read the missed messages from the database when
  it is online again.

- Support to use placeholders like `${var}` in the HTTP `Headers` of rule-engine's Webhook actions [#9239](https://github.com/emqx/emqx/pull/9239).

- Asynchronously refresh the resources and rules during emqx boot-up [#9199](https://github.com/emqx/emqx/pull/9199).
  This is to avoid slowing down the boot if some resources spend long time establishing the connection.

- Add a warning log if the ACL check failed for subscription [#9124](https://github.com/emqx/emqx/pull/9124).
  This is to make the ACL deny logging for subscription behave the same as for publish.

- JWT ACL claim supports `all` action to imply the rules applie to both `pub` and `sub` [#9044](https://github.com/emqx/emqx/pull/9044).

- Added a log censor to avoid logging sensitive data [#9189](https://github.com/emqx/emqx/pull/9189).
  If the data to be logged is a map or key-value list which contains sensitive key words such as `password`, the value is obfuscated as `******`.

- Enhanced log security in ACL modules, sensitive data will be obscured [#9242](https://github.com/emqx/emqx/pull/9242).

- Add `management.bootstrap_apps_file` configuration to bulk import default app/secret when EMQX initializes the database [#9273](https://github.com/emqx/emqx/pull/9273).

- Added two new configs for deterministic order of authentication and ACL checks [#9283](https://github.com/emqx/emqx/pull/9283).
  The two new global config names are `auth_order` and `acl_order`.
  When multiple ACL or auth plugins (or modules) are enabled, without this config, the order (in which each backend is queried)
  is determined by the start/restart order of the plugin (or module).
  Meaning, if a plugin (or module) is restarted after initial boot, it may get ordered to the end of the list.
  With this config, you may set the order with a comma-speapated ACL or auth plugin names (or aliases).
  For example: `acl_order = jwt,http`, this will make sure `jwt` is always checked before `http`,
  meaning if JWT is not found (or no `acl` cliam) for a client, then the ACL check will fallback to use the HTTP backend.

- Added configurations to enable more `client.disconnected` events (and counter bumps) [#9267](https://github.com/emqx/emqx/pull/9267).
  Prior to this change, the `client.disconnected` event (and counter bump) is triggered when a client
  performs a 'normal' disconnect, or is 'kicked' by system admin, but NOT triggered when a
  stale connection had to be 'discarded' (for clean session) or 'takeovered' (for non-clean session) by new connection.
  Now it is possible to set configs `broker.client_disconnect_discarded` and `broker.client_disconnect_takeovered` to `on` to enable the event in these scenarios.

- For Rule-Engine resource creation failure, delay before the first retry [#9313](https://github.com/emqx/emqx/pull/9313).
  Prior to this change, the retry delay was added *after* the retry failure.

### Bug fixes

- Fix that after uploading a backup file with an non-ASCII filename, HTTP API `GET /data/export` fails with status code 500 [#9224](https://github.com/emqx/emqx/pull/9224).

- Improve the display of rule's 'Maximum Speed' counter to only reserve 2 decimal places [#9185](https://github.com/emqx/emqx/pull/9185).
  This is to avoid displaying floats like `0.30000000000000004` on the dashboard.

- Fix the issue that emqx prints too many error logs when connecting to mongodb but auth failed [#9184](https://github.com/emqx/emqx/pull/9184).

- Fix that after receiving publish in `idle mode` the emqx-sn gateway may panic [#9024](https://github.com/emqx/emqx/pull/9024).

- "Pause due to rate limit" log level demoted from warning to notice [#9134](https://github.com/emqx/emqx/pull/9134).

- Restore old `emqx_auth_jwt` module API, so the hook callback functions registered in older version will not be invalidated after hot-upgrade [#9144](https://github.com/emqx/emqx/pull/9144).

- Fixed the response status code for the `/status` endpoint [#9210](https://github.com/emqx/emqx/pull/9210).
  Before the fix, it always returned `200` even if the EMQX application was not running.  Now it returns `503` in that case.

- Fix message delivery related event encoding [#9226](https://github.com/emqx/emqx/pull/9226)
  For rule-engine's input events like `$events/message_delivered`, and `$events/message_dropped`,
  if the message was delivered to a shared-subscription, the encoding (to JSON) of the event will fail.
  Affected versions: `v4.3.21`, `v4.4.10`, `e4.3.16` and `e4.4.10`.

- Make sure Rule-Engine API supports Percent-encoding `rule_id` and `resource_id` in HTTP request path [#9190](https://github.com/emqx/emqx/pull/9190).
  Note that the `id` in `POST /api/v4/rules` should be literals (not encoded) when creating a `rule` or `resource`.
  See docs [Create Rule](https://docs.emqx.com/en/enterprise/v4.4/advanced/http-api.html#post-api-v4-rules) [Create Resource](https://docs.emqx.com/en/enterprise/v4.4/advanced/http-api.html#post-api-v4-resources).

- Calling 'DELETE /alarms/deactivated' now deletes deactived alarms on all nodes, including remote nodes, not just the local node [#9280](https://github.com/emqx/emqx/pull/9280).

- When republishing messages or bridge messages to other brokers, check the validity of the topic and make sure it does not have topic wildcards [#9291](https://github.com/emqx/emqx/pull/9291).

- Disable authorization for `api/v4/emqx_prometheus` endpoint on management api listener (default 8081) [#9294](https://github.com/emqx/emqx/pull/9294).

## 4.3.21

*Release Date: 2022-09-14*

### Enhancements

- TLS listener memory usage optimization [#9005](https://github.com/emqx/emqx/pull/9005).
  New config `listener.ssl.$NAME.hibernate_after` to hibernate TLS connection process after idling.
  Hibernation can reduce RAM usage significantly, but may cost more CPU.
  This configuration is by default disabled.
  Our preliminary test shows a 50% of RAM usage decline when configured to '5s'.

- TLS listener default buffer size to 4KB [#9007](https://github.com/emqx/emqx/pull/9007)
  Eliminate uncertainty that the buffer size is set by OS default.

- Disable authorization for `api/v4/emqx_prometheus` endpoint [#8955](https://github.com/emqx/emqx/pull/8955).

- Added a test to prevent a last will testament message to be
  published when a client is denied connection [#8894](https://github.com/emqx/emqx/pull/8894).

- More rigorous checking of flapping to improve stability of the system [#9045](https://github.com/emqx/emqx/pull/9045).

- QoS1 and QoS2 messages in session's buffer are re-dispatched to other members in the group
  when the session terminates [#9094](https://github.com/emqx/emqx/pull/9094).
  Prior to this enhancement, one would have to set `broker.shared_dispatch_ack_enabled` to `true`
  to prevent sessions from buffering messages, however this acknowledgement costs extra resources.

- Fix delayed publish timing inaccuracy caused by OS time change [#8908](https://github.com/emqx/emqx/pull/8908).

### Bug fixes

- Fix HTTP client library to handle SSL socket passive signal [#9145](https://github.com/emqx/emqx/pull/9145).

- Hide redis password in error logs [#9071](https://github.com/emqx/emqx/pull/9071)
  In this change, it also included more changes in redis client:
  - Improve redis connection error logging [eredis#19](https://github.com/emqx/eredis/pull/19).
    Also added support for eredis to accept an anonymous function as password instead of
    passing around plaintext args which may get dumpped to crash logs (hard to predict where).
    This change also added `format_status` callback for `gen_server` states which hold plaintext
    password so the process termination log and `sys:get_status` will print '******' instead of
    the password to console.
  - Avoid pool name clashing [eredis_cluster#22](https://github.com/emqx/eredis_cluster/pull/22).
    Same `format_status` callback is added here too for `gen_server`s which hold password in
    their state.

- Fix shared subscription message re-dispatches [#9094](https://github.com/emqx/emqx/pull/9094).
  - When discarding QoS 2 inflight messages, there were excessive logs
  - For wildcard deliveries, the re-dispatch used the wrong topic (the publishing topic,
    but not the subscribing topic), caused messages to be lost when dispatching.

- Fix shared subscription group member unsubscribe issue when 'sticky' strategy is used.
  Prior to this fix, if a previously picked member unsubscribes from the group (without reconnect)
  the message is still dispatched to it.
  This issue only occurs when unsubscribe with the session kept.
  Fixed in [#9119](https://github.com/emqx/emqx/pull/9119)

- Fix shared subscription 'sticky' strategy when there is no local subscriptions at all.
  Prior to this change, it may take a few rounds to randomly pick group members until a local subscriber
  is hit (and then start sticking to it).
  After this fix, it will start sticking to whichever randomly picked member even when it is a
  subscriber from another node in the cluster.
  Fixed in [#9122](https://github.com/emqx/emqx/pull/9122)

- Fix rule engine fallback actions metrics reset [#9125](https://github.com/emqx/emqx/pull/9125).

## 4.3.20

*Release Date: 2022-09-17*

### Enhancements

- The `exp`, `nbf` and `iat` claims in JWT authentication support non-integer timestamps

### Bug fixes

- Fix rule engine update behaviour which may initialize actions for disabled rules
- Fix the issue that the IP address bound to the Dashboard listener did not take effect
- Fix the issue that shared subscriptions might get stuck in an infinite loop when `shared_dispatch_ack_enabled` is set to true
- Fix the issue that the rule engine SQL crashes when subject matching null values

## 4.3.19

*Release Date: 2022-08-29*

### Enhancements

- Improve the log when LwM2M packet parsing fails
- Improve the rule engine error log, the log will contain the rule ID when the action execution fails
- Improve log when `loaded_modules` and `loaded_plugins` files do not exist
- Add a guide for changing the default password on Dashboard

### Bug fixes

- Fix `client.disconnected` event not trigger in some cases
- Fix the issue that the built-in database authentication did not distinguish the pagination statistics of the authentication data of the client ID and username
- Fix Redis driver process leak problem
- Fix rule engine MQTT bridge to AWS IOT connection timeout issue
- Fix `GET /listener` request crashing when listener is not ready
- Fix the issue that the comparison between any variable and null value in the rule engine SQL always returns false after v4.3.12
- Fix incorrectly managing `emqx_modules` applications as plugins
- Fix the issue that when the execution priority of ExHook is higher than that of the rule engine, the topic filtered by the ExHook Message Hook will not trigger the rule engine
- Fix the issue that the ExHook management process was forcibly killed due to the supervisor shutdown timeout
- Fix the issue that the Client ID parameter in ExProto `client.connect` hook is not defined
- Fix ExProto not triggering disconnect event when client is kicked


## 4.3.18

*Release Date: 2022-08-11*

### Important Changes

- Upgraded the OTP version used to solve the low probability of random process unresponsiveness caused by OTP bugs. Users who are still using 4.3 are recommended to upgrade to this version
- From the next release, we will stop supporting macOS 10 and provide an installation package for macOS 11

### Enhancements

- Allows the connection process to be configured to be garbage collected after the TLS handshake is complete to reduce memory footprint, which can reduce memory consumption by about 35% per SSL connection, but increases CPU consumption accordingly
- Allows configuring the log level of the TLS handshake log to view the detailed handshake process

## 4.3.17

*Release Date: 2022-07-29*

### Enhancement

- Supports searching and paging of rules in rule engine
- Provides CLI `./bin/emqx check_conf` to actively check if the configuration is correct
- Optimizing Shared Subscription Performance

### Bug fixes

- Fix the issue that once the old version of EMQX is uninstalled after hot upgrade, EMQX will not be able to start again
- Fix the issue that the keep-alive check for UDP clients in the Multilingual Protocol Extension was incorrect, causing clients not to expire
- Fix the issue that the client information in the Multilingual Protocol Extension was not updated in time
- Fix the issue that when the client specified Clean Session as false to reconnect, the shared subscription message in the flight window would be re-dispatched to the old session process
- Fix the issue that the `emqx_lua_hook` plugin cannot cancel the message publishing

## 4.3.16

*Release Date: 2022-06-30*

### Enhancement

- QoS and Retain flag in rule engine's message republish actions can now use placeholders
- Supports exclusive subscriptions, that is, only one subscriber is allowed for a topic
- Dashboard and management API's HTTPS listeners can now use password-protected private key files, providing `key_password` configuration item
- Support for placeholders `%u` and `%c` in topic rewrite rules
- Support setting MQTT 5.0 properties in the API request for message publishing, such as message expiry interval, response topic, etc.
- Optimize the UI when creating rule engine resources, such as folding some uncommon options, etc.
- Opened 4 TCP-related configuration items: KeepAlive, TCP_NODELAY, SO_RCVBUF and SO_SNDBUF for the underlying gRPC connection of ExHook

### Bug fixes

- Fix the issue of inaccurate memory calculation in Linux OS, and calculate the memory usage of the current OS instead of the memory usage of EMQX
- Fix the issue that the old disconnect event of ExHook would be triggered later than the new connect event when the client reconnects
- Fix the issue that the execution order of topic rewriting and delayed publish is not fixed, now it is fixed to execute topic rewriting first
- Fix the issue that rule engine could not encode MQTT 5.0 user properties
- Fix the issue that the count of `connack.auth_error` is inaccurate when the client uses a protocol version below MQTT v5.0 to access
- Fix the issue that the UDP listeners of LwM2M and CoAP gateways could not bind to the specified network interface
- Fix Dashboard not starting after removing the default Dashboard user in the configuration file
- Fix `client.subscribe` hook not being able to reject subscriptions
- If the placeholder in the ACL rule is not replaced, the client's publish or subscribe operation will be rejected

## 4.3.15

*Release Date: 2022-06-01*

### Enhancement

- Add more time transformation functions to the SQL of rule engine
- Add the `float2str/2` function to the SQL of rule engine to support specifying the output precision of floating point numbers
- Support for using JWT for authorization, now MQTT clients can authorize using specific claims that include a pub-sub whitelist
- Improved authentication related metrics to make it easier to understand, now `client.authenticate = client.auth.success + client.auth.failure`
- Support binding the listener of the REST API to a specified network interface
- Support multi-condition query and fuzzy query for user data in authentication and authorization using built-in database as data source
- Supports querying clients using the length of the message queue and the number of dropped messages as conditions
- Support to configure the log time format to be compatible with the time format in older versions
- When `use_username_as_clientid` is configured to `true` and the client connects without specifying a `username`, the connection is now rejected with a reason code `0x85`
- Full randomisation of app secrets (previously partially randomised)
- Hot upgrades between incompatible versions will now be rejected
- Allow white spaces in EMQX's installation path
- Boot script fail fast on invalid node name (improve error message readability)

### Bug fixes

- Fix the issue that rule engine's SQL function `hexstr_to_bin/1` could not handle half-byte
- Fix the issue that the alarm was not cleared when the rule engine resource was deleted
- Fix Dashboard HTTPS listener's `verify` option not taking effect
- Fix the issue that messages were lost when the peer session was terminated during the delivery of QoS 1 messages through shared subscriptions
- Fix the issue that when the log tracer encounters large packets, the heap size grows too fast and triggers the policy of forcibly closeing the connection process
- Fix the issue that the MQTT-SN client would be disconnected when retransmitting QoS 2 messages
- Fix the issue that the returned results did not match the query conditions when querying subscriptions with multiple conditions
- Fix rule engine resource connection test not working
- Fix multiple Dashboard display issues

## 4.3.14

*Release Date: 2022-04-18*

### Enhancement

- Rule engine supports copying rule for fast reuse
- SQL in rule engine supports zip, gzip and other compression and decompression functions
- Improve the error message when rule engine fails to parse payload
- Improve the connection test for some resources in rule engine
- Support setting execution priority for ExHook
- ExHook callback interface adds a Protobuf field `RequestMeta meta` to return the EMQX cluster name
- Support `local` policy for shared subscriptions, which will preferentially send messages to shared subscribers under the node where messages flow in. In some scenarios, the efficiency of shared message scheduling will be improved, especially when the MQTT bridge is configured as a shared subscription
- `RSA-PSK-AES256-GCM-SHA384`, `RSA-PSK-AES256-CBC-SHA384`, `RSA-PSK-AES128-GCM-SHA256` and `RSA-PSK-AES128-CBC- SHA256` four new TLS PSK cipher suites are supported, removing two insecure cipher suites `PSK-3DES-EDE-CBC-SHA` and `PSK-RC4-SHA` from the default configuration
- Diagnostic logging for `wait_for_table` of mnesia
  - Prints check points of mnesia internal stats
  - Prints check points of per table loading stats, help to locate the problem of long table loading time.
- Subscribing to an empty topic is prohibited in strict mode
- Generate default files when `loaded_modules` and `loaded_plugins` files do not exist

### Bug fixes

- Fix the issue that the TLS configuration item `server_name_indication` is set to disable and does not take effect
- Fix potential process leak issue in MongoDB driver
- Fix the issue that the password of the default Dashboard user modified via the CLI command would be reset after the node leaves the cluster
- Silence grep and sed warnings in `docker-entrypoint.sh`
- Fix the backup file cannot be deleted and downloaded when the API path contains ISO8859-1 escape characters
- Fix the issue that the Redis driver would crash when DNS resolution failed, etc
- Fix the issue that the headers field configuration in the `Data to Web Server` action of the rule engine did not take effect
- Fix the issue that the MQTT Bridge plugin cannot be started when only the subscription topic is configured but QoS is not configured
- When creating a rule, if a rule with the same ID already exists, the rules engine will now report an error instead of replacing the existing rule
- Fix the issue that the HTTP driver process pool may not be deleted

## 4.3.13

*Release Date: 2022-04-01*

### Important changes

- For Docker images, the configuration directory `/opt/emqx/etc` has been removed from the VOLUME list, making it easier for users to rebuild images with changed configurations.
- CentOS 7 Erlang runtime rebuilt on OpenSSL-1.1.1n (previously 1.0), prior to v4.3.13, EMQX will fail to handshake and trigger `malformed_handshake_data` exception when clients use certain cipher suites.
- CentOS 8 Erlang runtime system rebuilt on RockyLinux 8. `centos8` will remain in the package name for backward compatibility.

### Enhancement

- Add command line interface `emqx_ctl pem_cache clean` to allow forcibly clear x509 certificate cache to reload immediately after certificate file update.
- Refactored ExProto so that anonymous clients can also be displayed on Dashboard.
- Topic configuration items in bridges can now use `${node}` placeholders.
- Add validation of UTF-8 strings in MQTT packets in strict mode. When set to `true`, invalid UTF-8 strings will cause the client to disconnect.
- MQTT-SN gateway supports initiative to synchronize registered topics after session resumed.
- Improve the writing precision of rule engine floating point data from 10 decimal places to 17 decimal places.
- EMQX will prompt how to modify the initial password of Dashboard at startup.

### Bug fixes

- Fix the issue that the el8 installation package cannot be started on Amazon Linux 2022, the error content is `errno=13 Permission denied`.
- Fix an issue where the client could not reconnect if the connection process was blocked in some cases. Now waiting for more than 15 seconds without a response will force the old connection process to be closed.
- Fix the issue of query resource request timeout when rule engine resource is unavailable.
- Fix the issue of `{error, eexist}` error when re-run after hot upgrade failed.
- Fix an issue where publishing to a non-existing topic alias would crash the connection.
- Fix 500 error when querying lwm2m client list on another node via HTTP API.
- Fix HTTP API for subscribing topics crashes when invalid QoS are passed in.
- Fix the issue that the connection count was not updated because the related resources were not released when the connection process accessed through the ExProto exited abnormally.
- Fix an issue where the value of `server_keepalive` configuration item would be incorrectly applied to MQTT v3.1.1 clients.
- Fix Stomp client not firing `$event/client_connection` event messages.
- Fix the issue that the system memory alarm was incorrectly activated when EMQX was started.
- Fixed an issue where messages that failed to be delivered due to unregistered topics were not retransmitted when topics were successfully registered with the MQTT-SN client.
- Fix EMQX startup output error log when duplicate plugins are configured in `loaded_plugins` file.
- Fix MongoDB related features outputting excessive error logs when configured incorrectly.
- Add format check for Dashboard User and AppID, special characters such as `/` are not allowed.
- Corrected the reason code in the DISCONNECT packet returned when kicking the client to `0x98`.
- Auto subscriptions will ignore empty topics.

## 4.3.12

*Release Date: 2022-02-11*

### Enhancement

- Rule engine supports the configuration of rules and actions for the event of abnormal loss of client messages to enhance the user's custom processing capabilities in this scenario
- Improve the relevant metrics during the execution of the rule engine SQL matching
- Fuzzy search on client supports special characters such as `*`, `(`, `)`
- Improve ACL-related metrics to solve the issue that the count does not increase due to hitting the ACL cache
- Added `connected_at` field to webhook event notifications
- Log client state before terminating client due to holding the lock too long

### Bug fixes

- Fixed the issue that the metrics interface does not return authentication metrics such as `client.acl.deny` by default
- Fixed the issue that the subscription query interface did not return paginated data
- Fix the issue of parsing failure when STOMP handles TCP sticky packets
- Fix the issue where the session creation time option was not available when filtering clients
- Fix the issue where memory alarms might not be triggered after restarting
- Fix the crash of import data when user data exists in `emqx_auth_mnesia` plugin

## 4.3.11

*Release Date: 2021-12-17*

EMQX 4.3.11 is released now, it mainly includes the following changes:

### Enhancement

- Support the configuration of whether to continue to deliver empty retained messages to suit users who are still using the MQTT v3.1 protocol

### Bug fixes

- Fix the issue of incorrect calculation of memory usage
- Fix the issue that the Path option of Webhook Action in rule engine doesn't support the use of ${Variable}
- Fix the issue that the connection failure log will continue to be printed when stopping MQTT Bridge plugin in some cases

## 4.3.10

*Release Date: 2021-11-11*

EMQX 4.3.10 is released now, it mainly includes the following changes:

**Bug fixes (Important):**

- Fix STOMP gateway live-upgrade failure

  Github PR: [emqx#6110](https://github.com/emqx/emqx/pull/6110)

- Fix the issue that emqx cannot start after updating the listener configuration through Dashboard

  Github PR: [emqx#6121](https://github.com/emqx/emqx/pull/6121)

**Enhancement:**

- Introduced pushback for MQTT clients

  Github PR: [emqx#6065](https://github.com/emqx/emqx/pull/6065)

## 4.3.9

*Release Date: 2021-11-04*

EMQX 4.3.9 is released now, it mainly includes the following changes:

**Bug fixes (Important):**

- Fix the issue that calls between clusters may cause the client process to lose response

  Github PR: [emqx#6062](https://github.com/emqx/emqx/pull/6062)

- WebHook's HTTP client SSL configuration parse

  Github PR: [emqx#5696](https://github.com/emqx/emqx/pull/5696)

- MongoDB resources allow host names

  Github PR: [emqx#6035](https://github.com/emqx/emqx/pull/6035)

- Performance improvement for built-in database ACL (emqx_auth_mnesia)

  Github PR: [emqx#5885](https://github.com/emqx/emqx/pull/5885)

- Fix the issue that the authentication based on the built-in database incorrectly transcodes the HTTP request parameters

  Github PR: [emqx#5674](https://github.com/emqx/emqx/pull/5674)

- Fix the issue that resources cannot be released after the rule engine disables the rules in cluster

  Github PR: [emqx#5731](https://github.com/emqx/emqx/pull/5731)

- Fix some issues of STOMP gateway

  Github PR: [emqx#6040](https://github.com/emqx/emqx/pull/6040)

**Bug fixes (Minor):**

- Fixed the issue that the Client ID containing "\" characters could not be searched in a fuzzy manner

  Github PR: [emqx#5978](https://github.com/emqx/emqx/pull/5978)

- Fix the issue that variable byte integers may be larger than 4 bytes

  Github PR: [emqx#5826](https://github.com/emqx/emqx/pull/5826)

**Enhancement:**

- Improve client kick (forced step-down)

  Github PR: [emqx#6030](https://github.com/emqx/emqx/pull/6030)

- Add support for new cipher suites for LwM2M gateway

  Github PR: [emqx#5970](https://github.com/emqx/emqx/pull/5970)

- Introduced interleaving for priority queues (to avoid low priority queue stavation)

  Github PR: [emqx#5666](https://github.com/emqx/emqx/pull/5666)

- HTTP authentication plugin disable superuser requests by default

  Github PR: [emqx#5567](https://github.com/emqx/emqx/pull/5567)

## 4.3.8

*Release Date: 2021-08-10*

EMQX 4.3.8 is released now, it mainly includes the following changes:

**Bug fixes:**

- Fix the rule engine rule import fails

  Github PR: [emqx#5512](https://github.com/emqx/emqx/pull/5512)

- Fix the Path field in the Webhook action of rule engine cannot be used

  Github PR: [emqx#5468](https://github.com/emqx/emqx/pull/5468)

- Fix the issue that the Force Shutdown mechanism cannot take effect when the process is suspended

  Github PR: [emqx#5460](https://github.com/emqx/emqx/pull/5460)

- Fix the issue that the k8s deployment EMQX cluster cannot be restarted correctly in some cases

  Github PR: [emqx#5646](https://github.com/emqx/emqx/pull/5646), [emqx#5428](https://github.com/emqx/emqx/pull/5428)

- Fix exproto cross-node process call error

  Github PR: [emqx#5436](https://github.com/emqx/emqx/pull/5436)

**Enhancement:**

- Add automatic reconnection mechanism and related configuration items for request timeout for exhook to enhance reliability

  Github PR: [emqx#5447](https://github.com/emqx/emqx/pull/5447)

- Add disconnect retry mechanism for exproto

  Github PR: [emqx#5436](https://github.com/emqx/emqx/pull/5436)

> Note: Starting from this version, CentoOS 7 requires the use of openssl 1.1.1. For the installation method of openssl upgrade, please refer to: [FAQ - Incorrect OpenSSL Vesion](https://docs.emqx.io/en/broker/v4.3/faq/error.html#incorrect-openssl-version)

## 4.3.7

*Release Date: 2021-08-09*

EMQX 4.3.7 is released now, it mainly includes the following changes:

**Bug fixes:**

- Fix the issue that the current HTTP KeepAlive behavior may cause some servers to disconnect

  Github PR: [emqx#5395](https://github.com/emqx/emqx/pull/5395)

- Fix the issue that the command line interface cannot print certain characters

  Github PR: [emqx#5411](https://github.com/emqx/emqx/pull/5411)

- Fix the issue of coding error when LwM2M gateway sends integer numbers

  Github PR: [emqx#5425](https://github.com/emqx/emqx/pull/5425)

## 4.3.6

*Release Date: 2021-07-28*

EMQX 4.3.6 is released now, it mainly includes the following changes:

**Enhancement:**

- Support disable HTTP Pipelining

  Github PR: [emqx#5279](https://github.com/emqx/emqx/pull/5279)

- ACL supports IP address list

  Github PR: [emqx#5328](https://github.com/emqx/emqx/pull/5328)

## 4.3.5

*Release Date: 2021-06-28*

EMQX 4.3.5 is released now, it mainly includes the following changes:

**Bug fixes:**

- Fix the issue that messages may be lost after canceling the subscription when multiple shared subscriptions are established by the same client

  Github PR: [emqx#5098](https://github.com/emqx/emqx/pull/5098)

## 4.3.4

*Release Date: 2021-06-23*

EMQX 4.3.4 is released now, it mainly includes the following changes:

**Bug fixes:**

- CoAP gateway cannot resolve certain URIs

  Github Issue: [emqx#5062](https://github.com/emqx/emqx/issues/5062)
  Github PR: [emqx#5059](https://github.com/emqx/emqx/pull/5059)

- Multi-language extension hooks may fail to start

  Github PR: [emqx#5004](https://github.com/emqx/emqx/pull/5004)

- When the rule engine deletes a resource, if there is a rule that depends on the resource, it will cause a crash

  Github PR: [emqx#4996](https://github.com/emqx/emqx/pull/4996)

- HTTP authentication and Webhook do not support Query String

  Github PR: [emqx#4981](https://github.com/emqx/emqx/pull/4981)

- Ensure the forwarding order of messages between nodes in the default configuration

  Github PR: [emqx#4979](https://github.com/emqx/emqx/pull/4979)

## 4.3.3

*Release Date: 2021-06-05*

EMQX 4.3.3 is released now, it mainly includes the following changes:

### emqx

**Enhancement:**

- Data dump support to get imported data from HTTP request

  Github PR: [emqx#4900](https://github.com/emqx/emqx/pull/4900)

**Bug fixes:**

- Fix the crash caused by not configuring the JWKS endpoint

  Github PR: [emqx#4916](https://github.com/emqx/emqx/pull/4916)

- Fix MQTT-SN subscription in cluster environment

  Github PR: [emqx#4915](https://github.com/emqx/emqx/pull/4915)

- Fix Webhook cannot use TLS

  Github PR: [emqx#4908](https://github.com/emqx/emqx/pull/4908)

- Fix the issue that the parameter error in the client's multi-condition query may cause a crash

  Github PR: [emqx#4916](https://github.com/emqx/emqx/pull/4916)

- Fix incorrect calculation of memory usage

  Github PR: [emqx#4891](https://github.com/emqx/emqx/pull/4891)

## 4.3.2

*Release Date: 2021-05-28*

EMQX 4.3.2 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Topic metrics monitoring cannot be used in the cluster environment

  Github PR: [emqx#4870](https://github.com/emqx/emqx/pull/4870)

- Fix some errors in message parsing

  Github PR: [emqx#4858](https://github.com/emqx/emqx/pull/4858)

- Fix the conflict between MQTT-SN sleep mode and KeepAlive mechanism

  Github PR: [emqx#4842](https://github.com/emqx/emqx/pull/4842)

- Broker may crash when a large number of clients are offline

  Github Issue: [emqx#4823](https://github.com/emqx/emqx/issues/4823)
  Github PR: [emqx#4824](https://github.com/emqx/emqx/pull/4824)

- Mark the resource as unavailable when the rule engine fails to refresh the resource

  Github PR: [emqx#4821](https://github.com/emqx/emqx/pull/4821)

## 4.3.1

*Release Date: 2021-05-14*

EMQX 4.3.1 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- CPU consumption increases exponentially with the number of topic levels after route compression

  Github PR: [emqx#4800](https://github.com/emqx/emqx/pull/4800)

- Fix poor large frame concatenation performance

  Github Issue: [emqx#4787](https://github.com/emqx/emqx/issues/4787)
  Github PR: [emqx#4802](https://github.com/emqx/emqx/pull/4802)

- Newly added shared subscription strategy is unavailable

  Github Issue: [emqx#4808](https://github.com/emqx/emqx/issues/4808)
  Github PR: [emqx#4809](https://github.com/emqx/emqx/pull/4809)

- Fixed the incorrect implementation of metrics/stats for retained messages and messages of delayed publish

  Github PR: [emqx#4778](https://github.com/emqx/emqx/pull/4778), [emqx#4778](https://github.com/emqx/emqx/pull/4799)

- Ensure line breaks between json logs

  Github PR: [emqx#4778](https://github.com/emqx/emqx/pull/4771)

## 4.3.0

*Release Date: 2021-05-08*

### Features and Enhancement

#### Building

- Support Erlang/OTP 23
- The new installation package only supports macOS 10.14 and above
- Project adjusted to umbrella structure
- Support using Elixir to build plugins

#### Performance improvement

- The underlying implementation of the multi-language extension function is changed from erlport to gRPC
- Support routing table compression, reduce memory usage, enhance subscription performance, publishing performance will be slightly affected, so disable option is provided
- Improve wildcard subscription performance
- Improve processing performance when a large number of clients are offline

#### Security

- Protect EMQX Broker from cross-site WebSocket hijacking attacks
- SSL supports `verify` and `server_name_indication` configuration
- Support the configuration of the maximum length of the certificate chain and the password of the private key file
- Use TLS v1.3 by default, TLS v1.3 configs has no effect if started on OTP 22
- JWT authentication supports JWKS

#### Other

- Added update resource functionality for rule engine
- Rule engine SQL function supports conversion between unix timestamp and rfc3339 format time
- Keep retrying the resources that failed to connect after the EMQX Broker is started
- Websocket listener supports selecting supported subprotocols from the subprotocols list
- WebSocket connection supports obtaining real IP and Port
- Support the default authentication method caching_sha2_password of MySQL 8.0
- The starting point is randomly selected when the shared subscription distribution strategy is configured as `round_robin`
- Shared subscription supports hash distribution of messages by source topic
- Support import and export of Authentication & ACL information in Mnesia
- Allow to use base64 encoded client certificate or MD5 value of client certificate as username or Client ID
- MQTT listener restart from API/CLI
- API/CLI to force evict ACL cache
- Added observer_cli
- Support cluster metrics for Prometheus
- Redis sentinel mode supports SSL connection
- Support single-line log output, and support rfc3339 time format
- `emqx_auth_clientid` and `emqx_auth_username` are merged to `emqx_auth_mnesia`. Please refer to [doc](https://docs.emqx.io/en/broker/v4.3/advanced/data-import-and-export.html) to export data from older versions, and import to v4.3
- By default, docker only logs to console, set EMQX_LOG__TO=file to switch to file
- Support Json format log
- Support IPv6 auto probe
- Environment variable override configuration files can be used for all distributions (previously only for docker)
- Certificate upload from dashboard has been made available for EMQX (previously only for EMQX Enterprise)

### Bugs Fix

#### MQTT Protocol

- Fix the processing of MQTT heartbeat packets
- Fix MQTT packet receiving count issue
- Limit the maximum effective size of the flight window to 65535
- The value of the `Keep Alive` field in the Dashboard is not synchronized when `Server Keep Alive` was in effect

#### Gateway

- ACL configuration in CoAP connection does not take effect
- CoAP clients using the same ClientID can access at the same time
- The sleep mode of MQTT-SN is unavailable
- The MQTT-SN gateway will discard DISCONNECT packets in sleep mode
- The LwM2M gateway encodes and decodes numbers into unsigned integers

#### Resource

- The MySQL authentication SSL/TLS connection function is not available
- Fix Redis reconnection failure

#### Other Fixes

- ekka_locker's memory may grow infinitely under extreme conditions
- The `max_inflight_size` configuration item in the MQTT bridge function does not take effect
- Fix the issue of inflight in MQTT bridge
- Fixed the error of indicator statistics in the MQTT bridge function and the problem of multiple unit conversions in the `retry_interval` field
- Incorrect calculation of alarm duration
- The long Client ID cannot be tracked
- The query client information may crash
- The inconsistency between topic rewriting and ACL execution order when publishing and subscribing
- The WebSocket connection cannot use the peer certificate as the username
- The authentication data cannot be imported
- EMQX may fail to start in Docker
- Fixed delayed connection process OOM kill
- The MQTT-SN connection with Clean Session being false did not publish a will message when it was disconnected abnormally

## 4.2.14

*Release Date: 2021-08-02*

EMQX 4.2.14 is released now, it mainly includes the following changes:

**Bug fixes:**

- Fix the issue of unavailable hot upgrade

## 4.2.13

*Release Date: 2021-06-28*

EMQX 4.2.13 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue that messages may be lost after canceling the subscription when multiple shared subscriptions are established by the same client

  GitHub PR: [emqx#5104](https://github.com/emqx/emqx/pull/5104)

### emqx-auth-http

**Bug fixes:**

- Fix the issue of printing crash log after request timeout

  GitHub PR: [emqx-auth-http#263](https://github.com/emqx/emqx-auth-http/pull/263)

- Support Query String Parameters

  GitHub PR: [emqx-auth-http#264](https://github.com/emqx/emqx-auth-http/pull/264)

### emqx-web-hook

**Bug fixes:**

- Support Query String Parameters

  GitHub PR: [emqx-web-hook#284](https://github.com/emqx/emqx-web-hook/pull/284)

## 4.2.12

*Release Date: 2021-05-06*

EMQX 4.2.12 is released now, it mainly includes the following changes:

### emqx

**Bug Fix**:

Fixed a wait-for-table timeout bug which may lead to premature EMQX start up when there is a lot of Mnesia data to load.

GitHub PR: [emqx#4724](https://github.com/emqx/emqx/pull/4724)

**Performance Improvement**:

Optimisation for massive concurrent subscribe/unsubscribe requests handling

GitHub PR: [emqx#4732](https://github.com/emqx/emqx/pull/4732)
GitHub PR: [emqx#4738](https://github.com/emqx/emqx/pull/4738)

## 4.2.11

*Release Date: 2021-04-16*

EMQX 4.2.11 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue that the WebSocket connection cannot use the peer certificate as the username

  Github PR: [emqx#4574](https://github.com/emqx/emqx/pull/4574)

### emqx-management

**Bug fixes:**

- Fix the issue of authentication data export and import

  Github PR: [emqx-management#320](https://github.com/emqx/emqx-management/pull/320)

## 4.2.10

*Release Date: 2021-03-26*

EMQX 4.2.10 is released now, it mainly includes the following changes:

### emqx-management

**Bug fixes:**

- When export data, use base64 encoding for `emqx_auth_clientid`

  Github PR: [emqx-management#314](https://github.com/emqx/emqx-management/pull/314)

- Fix MQTT bridge inflight reference booking error

  Github PR: [emqx-bridge-mqtt#132](https://github.com/emqx/emqx-bridge-mqtt/pull/132)

## 4.2.9

*Release Date: 2021-03-26*

EMQX 4.2.9 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix MQTT packet receiving count issue

  Github PR: [emqx#4425](https://github.com/emqx/emqx/pull/4425)

- Fix the processing of heartbeat packets

  Github Issue: [emqx#4370](https://github.com/emqx/emqx/issues/4370)
  Github PR: [emqx#4425](https://github.com/emqx/emqx/pull/4425)

### emqx-auth-mnesia

**Bug fixes:**

- Fixed database storage issues and CLI issues

  Github PR: [emqx-auth-mnesia#54](https://github.com/emqx/emqx-auth-mnesia/pull/54)

- Fix the issue that the `password_hash` configuration does not take effect

  Github PR: [emqx-auth-mnesia#56](https://github.com/emqx/emqx-auth-mnesia/pull/56)

## 4.2.8

*Release Date: 2021-03-10*

EMQX 4.2.8 is released now, it fixes a bug in MQTT message parser.

## 4.2.7

*Release Date: 2021-01-28*

EMQX 4.2.7 is released now, it mainly includes the following changes:

### emqx-auth-http

**Bug fixes:**

- Fix the issue that the HTTP long connection is disconnected when the Keepalive timeout period or the maximum number of requests is reached, causing the request to be lost

  Github PR: [emqx-auth-http#245](https://github.com/emqx/emqx-auth-http/pull/245)

### emqx-web-hook

**Bug fixes:**

- Fix the issue that the HTTP long connection is disconnected when the Keepalive timeout period or the maximum number of requests is reached, causing the request to be lost

  Github PR: [emqx-web-hook#272](https://github.com/emqx/emqx-web-hook/pull/272)

- Fix SSL certificate configuration issue

  Github PR: [emqx-web-hook#264](https://github.com/emqx/emqx-web-hook/pull/264)

### emqx-auth-redis

**Bug fixes:**

- Fix Redis reconnection failure

  Github PR: [emqx-auth-redis#195](https://github.com/emqx/emqx-auth-redis/pull/195)

## 4.2.6

*Release Date: 2021-01-16*

EMQX 4.2.6 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the wrong port used by remsh

  Github PR: [emqx#4016](https://github.com/emqx/emqx/pull/4016)

### emqx-auth-http

**Enhancement:**

- The host field in the HTTP request header uses the original URL configured by the user

  Github PR: [emqx-auth-http#240](https://github.com/emqx/emqx-auth-http/pull/240)

**Bug fixes:**

- Fix the issue of unavailable GET request

  Github PR: [emqx-auth-http#238](https://github.com/emqx/emqx-auth-http/pull/238)

### emqx-web-hook

**Enhancement:**

- The host field in the HTTP request header uses the original URL configured by the user

  Github PR: [emqx-web-hook#256](https://github.com/emqx/emqx-web-hook/pull/256)

- Use the default port when the port is not included in the URL

  Github PR: [emqx-web-hook#253](https://github.com/emqx/emqx-web-hook/pull/253)

**Bug fixes:**

- Fix the issue of SSL configuration item parsing error

  Github PR: [emqx-web-hook#252](https://github.com/emqx/emqx-web-hook/pull/252)

- Fix the issue of unavailable GET request

  Github PR: [emqx-web-hook#254](https://github.com/emqx/emqx-web-hook/pull/254)

### emqx-management

**Bug fixes:**

- Increase the duration field of the alarm, fix the issue that the front-end may cause calculation errors due to time inconsistency

  Github PR: [emqx-management#304](https://github.com/emqx/emqx-management/pull/304)

### emqx-dashboard

**Enhancement:**

- Improve the display of alarm duration

  Github PR: [emqx-dashboard#271](https://github.com/emqx/emqx-dashboard/pull/271)

### ehttpc

**Bug fixes:**

- Fix the issue of gen_server:call/3 timeout causing the reply to be sent to the message queue of the calling process

  Github PR: [ehttpc#2](https://github.com/emqx/ehttpc/pull/2)

## 4.2.5

*Release Date: 2020-12-23*

EMQX 4.2.5 is released now, it mainly includes the following changes:

### emqx-auth-http

- Fix wrong field name in HTTP request header

  Github PR: [emqx-auth-http#229](https://github.com/emqx/emqx-auth-http/pull/229)

### emqx-web-hook

- Update the underlying HTTP client driver to solve the issue of unresponsive connection process caused by driver stuck

  Github PR: [emqx-web-hook#240](https://github.com/emqx/emqx-web-hook/pull/240)

## 4.2.4

*Release Date: 2020-12-11*

EMQX 4.2.4 is released now, it mainly includes the following changes:

### emqx

- Support configuration of SSL/TLS certificate chain length and password of keyfile

  Github PR: [emqx#3901](https://github.com/emqx/emqx/pull/3901)

### emqx-auth-http

- Update the underlying HTTP client driver to solve the issue of unresponsive connection process caused by driver stuck

  Github PR: [emqx-auth-http#213](https://github.com/emqx/emqx-auth-http/pull/213)

### emqx-auth-mongo

- Fix the type error problem caused by no matching query results

  Github PR: [emqx-auth-mongo#240](https://github.com/emqx/emqx-auth-mongo/pull/240)

### emqx-auth-redis

- Fix the issue that the redis driver did not report an error when it failed to start in cluster mode

  Github PR: [emqx-auth-redis#187](https://github.com/emqx/emqx-auth-redis/pull/187)

### emqx-rel

- Supports pulling private image through secret for helm chart

  Github PR: [emqx-rel#626](https://github.com/emqx/emqx-rel/pull/626)

- Improve security

  Github PR: [emqx-rel#612](https://github.com/emqx/emqx-rel/pull/612)

## 4.2.3

*Release Date: 2020-11-13*

EMQX 4.2.3 is released now, it mainly includes the following changes:

### emqx-web-hook

- Support for inserting variables in the requested URL path

  Github PR: [emqx-web-hook#225](https://github.com/emqx/emqx-web-hook/pull/225)
  Github Issue: [emqx-web-hook#224](https://github.com/emqx/emqx-web-hook/issues/224)

- Support setting Content Type

  Github PR: [emqx-web-hook#230](https://github.com/emqx/emqx-web-hook/pull/230)

### emqx-rel

- Fix the issue of compilation failure when LC_ALL is not equal to en_US.UTF-8

  Github PR: [emqx-rel#605](https://github.com/emqx/emqx-rel/pull/605)
  Github Issue: [emqx-rel#604](https://github.com/emqx/emqx-rel/issues/604)

- Support creating and running other services before emqx container starts

  Github PR: [emqx-rel#608](https://github.com/emqx/emqx-rel/pull/608)

- Allows to set a variable number of configuration items for the container

  Github PR: [emqx-rel#609](https://github.com/emqx/emqx-rel/pull/609)

### emqx-sn

- Fix the issue that trying to publish a will message will cause a crash

  Github PR: [emqx-sn#169](https://github.com/emqx/emqx-sn/pull/169)

- Fix the issue that setting an inappropriate value for the from field

  Github PR: [emqx-sn#170](https://github.com/emqx/emqx-sn/pull/170)

### emqx-rule-engine

- Keep backward compatible

  Github PR: [emqx-rule-engine#189](https://github.com/emqx/emqx-rule-engine/pull/189)

- Fix statistics of `messages.received` metric

  Github PR: [emqx-rule-engine#193](https://github.com/emqx/emqx-rule-engine/pull/193)

### emqx-management

- Fix statistics of `messages.received` metric

  Github PR: [emqx-management#284](https://github.com/emqx/emqx-management/pull/284)

- Enable data import and export functions to be used in a cluster environment

  Github PR: [emqx-management#288](https://github.com/emqx/emqx-management/pull/288)

### emqx-redis

- Supports one-way and two-way SSL/TLS of Redis 6.0

  Github PR: [emqx-auth-redis#180](https://github.com/emqx/emqx-auth-redis/pull/180)

## 4.2.2

*Release Date: 2020-10-24*

EMQX 4.2.2 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue of inaccurate topic statistics rate calculation

  Github PR: [emqx#3784](https://github.com/emqx/emqx/pull/3784)

### emqx-web-hook

**Enhancement:**

- Support `GET` and `DELETE` methods

  Github PR: [emqx-web-hook#220](https://github.com/emqx/emqx-web-hook/pull/220)

- Add `node` and `disconnected_at` fields

  Github PR: [emqx-web-hook#215](https://github.com/emqx/emqx-web-hook/pull/215)

### emqx-auth-pgsql

**Bug fixes:**

- Fix the issue that `%a` placeholder doesn't take effect

  Github PR: [emqx-auth-pgsql#208](https://github.com/emqx/emqx-auth-pgsql/pull/208)

### emqx-auth-mysql

**Bug fixes:**

- Fix the issue that `%a` placeholder doesn't take effect

  Github PR: [emqx-auth-mysql#245](https://github.com/emqx/emqx-auth-mysql/pull/245)

## 4.2.1

*Release Date: 2020-09-29*

EMQX 4.2.1 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue that the No Local logic is not handled correctly, which causes messages to accumulate in the flight window and message queue

  Github PR: [emqx#3741](https://github.com/emqx/emqx/pull/3741)
  Github Issue: [emqx#3738](https://github.com/emqx/emqx/issues/3738)

### emqx-bridge-mqtt

**Bug fixes:**

- Fix the issue that the rule engine MQTT subscription cannot receive messages

  Github PR: [emqx-bridge-mqtt#108](https://github.com/emqx/emqx-bridge-mqtt/pull/108)

### emqx-dashboard

**Bug fixes:**

- Fix display error of long Client ID

  Github PR: [emqx-dashboard#262](https://github.com/emqx/emqx-dashboard/pull/262)

### emqx-auth-mongo

**Bug fixes:**

- Fix the issue that only the first match is processed when the ACL query statement has multiple matching results

  Github PR: [emqx-auth-mongo#231](https://github.com/emqx/emqx-auth-mongo/pull/231)

## 4.2.0

*Release Date: 2020-09-11*

EMQX 4.2.0 is released now, it mainly includes the following changes:

**Feature:**

- Support the use of third-party languages to write extension plugins to access other non-MQTT protocols, and currently supports Java and Python two programming languages. Visit [Read Me](https://github.com/emqx/emqx-exproto/blob/master/README.md) for more information
- Support hot upgrade between revisions
- A new telemetry function is added to collect information about the usage of EMQX Broker to help us improve the product. This function is enabled by default and supports manual disabled. Visit [EMQX Telemetry](https://docs.emqx.io/broker/latest/en/advanced/telemetry.html) for more telemetry related information.
- Support message flow control in the form of quotas

**Enhancement:**

- The rule engine supports the creation of subscriptions for MQTT bridges
- The rule engine supports a more powerful SQL syntax
- Plugins such as MySQL and PostgreSQL fully support IPv6, SSL/TLS
- Support CentOS 8, Ubuntu 20.04 operating system and ARM64 system architecture
- Webhook supports configuring custom HTTP headers
- A more friendly alarm mechanism, providing developers with HTTP API
- Optimize retained message performance

**Change:**

- Subsequent versions will no longer support Debian 8, Ubuntu 14.04 and Raspbian 8 operating systems
- The `emqx-statsd` plugin is officially renamed to `emqx-prometheus`
- Publish and subscribe support independent configuration of topic rewrite rules
- Allow users to configure whether to allow WebSocket messages to contain multiple MQTT messages to be compatible with some clients
- Adjust RPC port discovery strategy
- ***Incompatible changes:*** The API path provided by the `emqx-auth-mnesia` plugin has been adjusted to `api/v4/mqtt_user` and `api/v4/mqtt_acl`
- The `emqx-auth-http` plugin disables super user authentication requests by default
- `emqx-bridge-mqtt` disables bridge mode by default

**Bug fixes:**

- Fix the issue of abnormal memory growth caused by the topic metrics feature
- Fix the issue that the LwM2M plugin did not correctly obtain the protocol version
- Fix the issue that the command line interface cannot be used when running multiple emqx instances on one machine
- Fix the issue that Websocket connection does not support IPv6

## 4.1.4

*Release Date: 2020-08-28*

EMQX 4.1.4 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue of abnormal memory growth caused by the topic metrics feature

  Github PR: [emqx#3680](https://github.com/emqx/emqx/pull/3680)

### emqx-bridge-mqtt

**Enhancements:**

- The clientid configuration item supports `${node}` placeholders to optimize the user experience under the cluster

  Github PR: [emqx-bridge-mqtt#99](https://github.com/emqx/emqx-bridge-mqtt/pull/99)

### emqx-management

**Bug fixes:**

- Fix the issue that the data migration function is not available under Windows

  Github PR: [emqx-management#262](https://github.com/emqx/emqx-management/pull/262)

### emqx-lua-hook

**Bug fixes:**

- Fix the issue that the Username field cannot be obtained

  Github PR: [emqx-lua-hook#115](https://github.com/emqx/emqx-lua-hook/pull/115)

## 4.1.3

*Release Date: 2020-08-04*

EMQX 4.1.3 is released now, it mainly includes the following changes:

### emqx-management

**Bug fixes:**

- Add type checking for the payload field in PUBLISH API

  Github PR: [emqx/emqx-management#250](https://github.com/emqx/emqx-management/pull/250)

### emqx-retainer

**Bug fixes:**

- Fix the issue that the retained message will not be sent when the subscription topic contains both '+' and '#' 

  Github PR: [emqx/emqx-retainer#146](https://github.com/emqx/emqx-retainer/pull/146)

## 4.1.2

*Release Date: 2020-07-23*

EMQX 4.1.2 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue that the topic alias is not used to replace the topic

  Github PR: [emqx/emqx#3616](https://github.com/emqx/emqx/pull/3616)

- Fix the issue that some operations take up too much CPU

  Github PR: [emqx/emqx#3581](https://github.com/emqx/emqx/pull/3581)

### emqx-rel

**Bug fixes:**

- Fix the issue that the console no longer outputs the log after the log is filled with all log files when running emqx by docker

  Github PR: [emqx/emqx-rel#559](https://github.com/emqx/emqx-rel/pull/559)

## 4.1.1

*Release Date: 2020-07-03*

EMQX 4.1.1 is released now, it mainly includes the following changes:

### emqx-retainer

**Bug fixes:**

- Fix performance issues

  Github PR: [emqx/emqx-retainer#141](https://github.com/emqx/emqx-retainer/pull/141)

### emqx-bridge-mqtt

**Bug fixes:**

- Change mount point to optional configuration

  Github PR: [emqx/emqx-bridge-mqtt#84](https://github.com/emqx/emqx-bridge-mqtt/pull/84)

### emqx-rel

**Bug fixes:**

- Hiding sensitive env from docker's logging out

  Github Issue: [emqx/emqx-rel#524](https://github.com/emqx/emqx-rel/pull/524)

  Github PR: [emqx/emqx-rel#542](https://github.com/emqx/emqx-rel/pull/542)

  Thanks: [emqx/emqx-rel#525](https://github.com/emqx/emqx-rel/pull/525) - [daadu](https://github.com/daadu)

### emqx-lua-hook

**Bug fixes:**

- Fix the issue that there is no unload script and CLI when the plugin is unloaded

  Github PR: [emqx/emqx-lua-hook#106](https://github.com/emqx/emqx-lua-hook/pull/106)

## 4.1.0

*Release Date: 2020-06-04*

EMQX 4.1.0 is released now, it mainly includes the following changes:

**Enhancements:**

  - Support multi-language extension and provide SDK, supported languages: Python, Java
  - Support topic based metrics
  - Supports loading the latest configuration when the plugin starts
  - Support for topic aliases when messages are forwarded
  - Add subscription option configuration for proxy subscriptions
  - Support fuzzy query and multi condition query of client list
  - Support fuzzy query of subscription list
  - Support to add simple authentication information on Dashboard
  - Support data migration between versions
  - MQTT AUTH Packet is supported. At present, only SCRAM-SHA-1 authentication mechanism is supported and users can expand it by themselves
  - Support for obtaining network addresses and ports when using the proxy protocol
  - Add authentication plugin based on Mnesia database (completely replace `emqx-auth-clientid` and `emqx-auth-username` plugins in subsequent versions)
  - Support for editing rules in rule engine
  - Support comment configuration items when running EMQX through Docker
  - LwM2M gateway plugin supports IPv6 and listens to multiple ports at the same time
  - CoAP gateway plugin supports IPv6
  - JWT authentication plugin supports configuration of jwerl signature format

**Bug fixes:**

  - Fix the issue that EMQX could not start when `etc/emqx.conf` was read-only
  - Fix the issue that the connection process crashes in some cases
  - Fix the issue that the browser doesn't support the current SSL/TLS certificates
  - Fix the issue that the MQTT bridge plugin doesn't send heartbeat packets by default
  - Fix the issue that the abnormal login detection function does not delete the expired data, resulting in memory growth
  - Fix the issue that the built-in ACL module did not clear the ACL cache when reloading
  - Fix the issue that `client.disconnected` event in WebHook plugin goes wrong in some cases
  - Fix the issue that MQTT-SN gateway plugin doesn't support specifying listening IP address and supports IPv6

## 4.0.7

*Release Date: 2020-05-12*

EMQX 4.0.7 is released now, which mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue that the browser doesn't support the current SSL / TLS certificates

  Github PR: [emqx/emqx#3448](https://github.com/emqx/emqx/pull/3448)
  
- Fix the issue of connection process crashing in some cases, which is the feedback from [Github issue#3455](https://github.com/emqx/emqx/issues/3455) 

  Github PR: [emqx/emqx#3458](https://github.com/emqx/emqx/pull/3458)

### emqx-web-hook

**Bug fixes:**

- Fix the issue that the `client.disconnected` event went wrong in some cases

  Github PR: [emqx/emqx-web-hook#187](https://github.com/emqx/emqx-web-hook/pull/187)

## 4.0.6

*Release Date: 2020-04-22*

EMQX 4.0.6 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue that abnormal client detection did not delete expired data

  Github PR: [emqx/emqx#3407](https://github.com/emqx/emqx/pull/3407)
  
- Fix the issue that the Proxy Protocol function does not work when using WebSocket

  Github PR: [emqx/emqx#3372](https://github.com/emqx/emqx/pull/3372)
  
### emqx-bridge-mqtt

**Bug fixes:**

- Fix the issue that PINREQ packets will not be sent by default

  Github PR: [emqx/emqx-bridge-mqtt#67](https://github.com/emqx/emqx-bridge-mqtt/pull/67)

### emqx-rule-engine

**Bug fixes:**

- Fix wrong type of rule engine timestamp

  Github Commit: [emqx/emqx-rule-engine#27ca37](https://github.com/emqx/emqx-rule-engine/commit/27ca3768602c107af71ea6b20f4518bb0f70404d)

- Fix the feature of testing SQL statements in the rule engine

  Github Commit: [emqx/emqx-rule-engine#33fcba](https://github.com/emqx/emqx-rule-engine/commit/33fcba394e59fef495e2fe54883297c8d3d893e5)

## 4.0.5

*Release Date: 2020-03-17*

EMQX 4.0.5 is released now. This version mainly focuses on bug fixes.

### emqx
**Bug fixes:**

- Fix GC policy

  Github PR: [emqx/emqx#3317](https://github.com/emqx/emqx/pull/3317)
  
- Fix the issue that the value of the `Maximum-QoS` property was set incorrectly

  Github issue: [emqx/emqx#3304](https://github.com/emqx/emqx/issues/3304), [emqx/emqx#3315](https://github.com/emqx/emqx/issues/3315)
  Github PR: [emqx/emqx#3321](https://github.com/emqx/emqx/pull/3321)
  
- Fix the issue that CPU usage would increase abnormally every 15 seconds when EMQX Broker was running in the docker environment

	Github issue: [emqx/emqx#3274](https://github.com/emqx/emqx/pull/3274)
  Github PR: [emqx/emqx-rel#462](https://github.com/emqx/emqx-rel/pull/462)
  
- Fixed the issue that configuration items named "node.*" don't take effect in `emqx.conf`

  Github issue: [emqx/emqx#3302](https://github.com/emqx/emqx/pull/3302)
  Github PR: [emqx/emqx-rel#463](https://github.com/emqx/emqx-rel/pull/463)

### emqx-rule-engine (plugin)

**Bug fixes:**

- Fix the issue that the rule engine does not support Payload with UTF-8 string

  Github issue: [emqx/emqx#3287](https://github.com/emqx/emqx/issues/3287)
  Github PR: [emqx/emqx#3299](https://github.com/emqx/emqx/pull/3299)

### emqx-sn (plugin)

**Bug fixes:**

- Fix MQTT-SN subscription loss

  Github issue: [emqx/emqx#3275](https://github.com/emqx/emqx/issues/3275)
  Github PR: [emqx/emqx-sn#156](https://github.com/emqx/emqx-sn/pull/156)

## 4.0.4

*Release Date: 2020-03-21*

EMQX 4.0.4 is released now. This version mainly focuses on bug fixes.

### emqx

**Bug fixes:**

  - Fix the issue that the `acl_deny_action` configuration item does not
    working
    
    Github issue:
    [emqx/emqx\#3266](https://github.com/emqx/emqx/issues/3266)
    
    Github PR: [emqx/emqx\#3286](https://github.com/emqx/emqx/pull/3286)

  - Fix wrong type of `mountpoint` configuration item
    
    Github issue:
    [emqx/emqx\#3271](https://github.com/emqx/emqx/issues/3271)
    
    Github PR: [emqx/emqx\#3272](https://github.com/emqx/emqx/pull/3272)

  - Fix the issue that the `peer_cert_as_username` configuration item
    does not working
    
    Github issue:
    [emqx/emqx\#3281](https://github.com/emqx/emqx/issues/3281)
    
    Github PR: [emqx/emqx\#3291](https://github.com/emqx/emqx/pull/3291)

  - Fix the issue that the error log is still printed after the
    connection is closed normally
    
    Github PR: [emqx/emqx\#3290](https://github.com/emqx/emqx/pull/3290)

### emqx-dashboard (plugin)

**Bug fixes:**

  - Fix blank issue in Dashboard node drop-down list
    
    Github issue:
    [emqx/emqx\#3278](https://github.com/emqx/emqx/issues/3278)
    
    Github PR:
    [emqx/emqx-dashboard\#206](https://github.com/emqx/emqx-dashboard/pull/206)

### emqx-retainer (plugin)

**Bug fixes:**

  - The behavior after the maximum number of retained messages has been
    stored is corrected by the inability to store any retained messages
    to retained messages that can replace existing topics
    
    Github PR:
    [emqx/emqx-retainer\#136](https://github.com/emqx/emqx-retainer/pull/136)

## 4.0.3

*Release Date: 2020-02-21*

EMQX 4.0.3 is released now. This version mainly focuses on bug fixes.

### emqx

**Enhancements:**

  - Add an option to allow client bypass auth plugins
    
    Github PR: [emqx/emqx\#3253](https://github.com/emqx/emqx/pull/3253)

**Bug fixes:**

  - Fix the issue of printing unnecessary error logs under some
    competitive conditions
    
    Github PR: [emqx/emqx\#3246](https://github.com/emqx/emqx/pull/3253)

### emqx-management (plugin)

**Bug fixes:**

  - Remove fields and functions that are no longer in use and fix wrong
    field values
    
    Github PR:
    [emqx/emqx-management\#176](https://github.com/emqx/emqx-management/pull/176)

  - Fix the issue that the client list cannot be returned correctly in
    the cluster
    
    Github PR:
    [emqx/emqx-management\#173](https://github.com/emqx/emqx-management/pull/173)

  - Fix HTTPS Listening Options
    
    Github PR:
    [emqx/emqx-management\#172](https://github.com/emqx/emqx-management/pull/172)

  - Fix the return format of the application list
    
    Github PR:
    [emqx/emqx-management\#169](https://github.com/emqx/emqx-management/pull/169)

## 4.0.2

*Release Date: 2020-02-07*

EMQX 4.0.2 is released now. This version mainly focuses on bug fixes
and performance optimizes.

### emqx

**Enhancements:**

  - Enhance performance of json encode/decode
    
    Github PR:
    [emqx/emqx\#3213](https://github.com/emqx/emqx/pull/3213),
    [emqx/emqx\#3230](https://github.com/emqx/emqx/pull/3230),
    [emqx/emqx\#3235](https://github.com/emqx/emqx/pull/3235)

  - Compress the generated object code
    
    Github PR: [emqx/emqx\#3214](https://github.com/emqx/emqx/pull/3214)

**Bug fixes:**

  - Fix the issue that DISCONNECT packet will not be sent in some cases
    
    Github PR: [emqx/emqx\#3208](https://github.com/emqx/emqx/pull/3208)

  - Fix the issue that the connection will be closed when broker
    received the same Packet ID
    
    Github PR: [emqx/emqx\#3233](https://github.com/emqx/emqx/pull/3233)

### emqx-stomp (plugin)

**Bug fixes:**

  - Fix the issue that the maximum number of connections doesn't take
    effect
    
    Github PR:
    [emqx/emqx-stomp\#93](https://github.com/emqx/emqx-stomp/pull/93)

### emqx-auth-redis (plugin)

**Bug fixes:**

  - Fix the issue that internal module start failed
    
    Github PR:
    [emqx/emqx-auth-redis\#151](https://github.com/emqx/emqx-auth-redis/pull/151)

### cowboy (dependency)

**Bug fixes:**

  - Fix the issue that will message will not be sent in some cases when
    using Websocket connection
    
    Github Issue:
    [emqx/emqx\#3221](https://github.com/emqx/emqx/issues/3221)
    
    Github Commit:
    [emqx/cowboy\#3b6bda](https://github.com/emqx/cowboy/commit/3b6bdaf4f2e3c5b793a0c3cada2c3b74c3d5e885)

## 4.0.1

*Release Date: 2020-01-17*

EMQX 4.0.1 is released now. This version mainly focuses on bug fixes
and performance optimizes.

### emqx

**Enhancements:**

  - force\_shutdown\_policy defaults to disable
    
    Github PR: [emqx/emqx\#3184](https://github.com/emqx/emqx/pull/3184)

  - Support timed global GC and provide configuration items
    
    Github PR: [emqx/emqx\#3190](https://github.com/emqx/emqx/pull/3190)

  - Tune the default value of `force_gc_policy`
    
    Github PR:
    [emqx/emqx\#3192](https://github.com/emqx/emqx/pull/3192),
    [emqx/emqx\#3201](https://github.com/emqx/emqx/pull/3201)

  - Tune and optimize the Erlang VM
    
    Github PR:
    [emqx/emqx\#3195](https://github.com/emqx/emqx/pull/3195),
    [emqx/emqx\#3197](https://github.com/emqx/emqx/pull/3197)

**Bug fixes:**

  - Fix the issue that the feature of ban is abnormal due to using the
    wrong unit
    
    Github PR: [emqx/emqx\#3188](https://github.com/emqx/emqx/pull/3188)

  - Fix the handling of `Retain As Publish` and keep the value of
    `Retain` in bridge mode
    
    Github PR: [emqx/emqx\#3189](https://github.com/emqx/emqx/pull/3189)

  - Fix the issue of unable to use multiple websocket listening ports
    
    Github PR: [emqx/emqx\#3196](https://github.com/emqx/emqx/pull/3196)

  - Fix the issue that EMQX may not send DISCONNECT packet when session
    is takeovered
    
    Github PR: [emqx/emqx\#3208](https://github.com/emqx/emqx/pull/3208)

### emqx-rule-engine

**Enhancements:**

  - Provide more arrays functions of SQL
    
    Github PR:
    [emqx/emqx-rule-engine\#136](https://github.com/emqx/emqx-rule-engine/pull/136)

  - Reduce performance impact when no rules are configured
    
    Github PR:
    [emqx/emqx-rule-engine\#138](https://github.com/emqx/emqx-rule-engine/pull/138)

### emqx-web-hook

**Bug fixes:**

  - Fix crash due to parameter mismatch
    
    Github PR:
    [emqx/emqx-web-hook\#167](https://github.com/emqx/emqx-web-hook/pull/167)

## 4.0.0

*Release Date: 2020-01-17*

EMQX 4.0.0 is now released. In this version we significantly improved
the throughput performance by refactoring the session and channel,
improved the extensibility by adding more hooks and counters, redesigned
rule engine SQL that filter messages/events mainly by topics, and also
lots of improvements in edge.

### General

**Enhancements:**

  - Greatly improve message throughput performance, and reduce CPU and
    memory usage.
  - Optimize handling of MQTT 5.0 packets
  - Rule engine supports new SQL
  - Modify metrics naming and add more metrics
  - Modify parameters of hooks and add more hooks
  - emqtt provides command line interfaces to publish and subscribe

**Bug fixes:**

  - Fix the issue that failure of SSL handshake could cause crash
  - Fix the issue that `max_subscriptions` don't working
  - Fix message out-of-order issues when forwarding across clusters
  - Fix the issue that REST API and CLI cannot get multiple routes for a
    topic

### REST API

**Enhancements:**

  - Support IPv6
  - The default listening port for the HTTP API server is changed from
    8080 to 8081
  - Remove all REST API related with sessions
  - `connections` APIs change to `clients` APIs，the new APIs support
    features in sessions
  - Support return the real topic of shared subscription in querying
    subscriptions API
  - Support to configure the default AppID and AppSecret
  - The HTTP API for publishing message now supports base64 encoded
    payload

**Bug fixes:**

  - Fix the issue that encoded URI isn't handled correctly

### Authentication

**Enhancements:**

  - HTTP authentication plugin supports users to defining HTTP request
    headers in profile
  - Clientid and username authentication plugin Resupport to configure
    the default clientid and username in profile

## 3.2.7

_Release Date: 2019-12-03_

EMQX 3.2.7 is now available. This version resupports to configure the default `username` and `clientid` through the configuration file.

### emqx-auth-username (plugin)

Enhancements:

- Resupport to configure the default `username` through the configuration file

Github PR: [emqx/emqx-auth-username#127](https://github.com/emqx/emqx-auth-username/pull/127)

### emqx-auth-clientid (plugin)

Enhancements:

- Resupport to configure the default `clientid` through the configuration file

Github PR: [emqx/emqx-auth-clientid#123](https://github.com/emqx/emqx-auth-clientid/pull/123)

## 3.2.6

_Release Date: 2019-11-23_

EMQX 3.2.6 is now available. This version focuses on feature improvements and bug fixes.

### emqx (major)

Bug fixes:

- Fix the issue that messages maybe disordered when forwarding messages to remote nodes via `gen_rpc`

Github PR: [emqx/emqx#3049](https://github.com/emqx/emqx/pull/3049)

- Fix `emqx` crash caused by the crash of auth plugin

Github PR: [emqx/emqx#3048](https://github.com/emqx/emqx/pull/3048)

## 3.2.5

_Release Date: 2019-11-15_

EMQX 3.2.5 is now available. This version focuses on bug fixes.

### emqx-rule-engine (plugin)

Bug fixes:

- Support rule SQL: FOREACH/DO/INCASE

Github Commit: [emqx/emqx-rule-engine#a962e3](https://github.com/emqx/emqx-rule-engine/commit/a962e364cfde9a7f9bbde3d4d6613625b8d00ce7)

- Support rule SQL: CASE/WHEN

Github Commit: [emqx/emqx-rule-engine#40e68e](https://github.com/emqx/emqx-rule-engine/commit/40e68e9607198613cc93d001488d40b2bfb4f23e)

- Support comparing atom to binary in WHERE SQL clause

Github Commit: [emqx/emqx-rule-engine#b240cc](https://github.com/emqx/emqx-rule-engine/commit/b240cc0434815bafb5cfcd366692257336d26e8c)

- Fix column validation failure in select and foreach

Github Commit: [emqx/emqx-rule-engine#6a1267](https://github.com/emqx/emqx-rule-engine/commit/6a1267cb1530d00972899ecb3abb7a3220e28175)

- Fix race-conditions when re-build rules

Github Commit: [emqx/emqx-rule-engine#af8967](https://github.com/emqx/emqx-rule-engine/commit/af8967793d4f554134955c620d9e31b8c3876445)

- Fix incorrect publish message by adding default flags in republish action

Github Commit: [emqx/emqx-rule-engine#60e45c](https://github.com/emqx/emqx-rule-engine/commit/60e45c28596a6cb42437043fbba5509502a3cf41)

### minirest (plugin)

Bug fixes:

- Fix missing error data in log

Github PR: [emqx/minirest#20](https://github.com/emqx/minirest/pull/20)

### emqx-web-hook (plugin)

Bug fixes:

- Fix bad match

Github Commit: [emqx/emqx-web-hook#3dd041](https://github.com/emqx/emqx-web-hook/commit/3dd041afaf39eabe71ab473648d57f4b55735224)

## 3.2.4

_Release Date: 2019-10-28_

EMQX 3.2.4 is now available. This version mainly adds IPv6 support for Dashbaord and REST APIs, and fixes some bugs.

Bug fixes:

- Fix the issue that 'max_subscriptions' don't working

Github PR: [emqx/emqx#2922](https://github.com/emqx/emqx/pull/2922)

Github Issue: [emqx/emqx#2908](https://github.com/emqx/emqx/issues/2908)

### emqx-auth-mysql (plugin)

Bug fixes:

- Gets the value corresponding to placeholders more securely

Github PR: [emqx/emqx-auth-mysql#180](https://github.com/emqx/emqx-auth-mysql/pull/180)

Github Issue: [emqx/emqx#2937](https://github.com/emqx/emqx/issues/2937)

### emqx-dashboard (plugin)

Enhancements:

- Support for IPv6 access to Dashbaord

Github PR: [emqx/emqx-dashboard#161](https://github.com/emqx/emqx-dashboard/pull/161)

### emqx-management (plugin)

Enhancements:

- REST API supports IPv6

Github PR: [emqx/emqx-management#134](https://github.com/emqx/emqx-management/pull/134)

### emqx-delay-publish (plugin)

Bug fixes:

- Fix the issue that delayed messages are published disorderly, thanks contribution of [soldag](https://github.com/soldag)

Github PR: [emqx/emqx-delay-publish#49](https://github.com/emqx/emqx-delay-publish/pull/49)

Github Issue: [emqx/emqx-delay-publish#15](https://github.com/emqx/emqx-delay-publish/issues/15)

### emqx-rule-engine (plugin)

Enhancements:

- Improved the SQL syntax for decoding the payload of JSON format

Github Repository: [emqx/emqx-rule-engine](https://github.com/emqx/emqx-rule-engine)

## 3.2.3

_Release Date: 2019-09-16_

EMQX 3.2.3 is now available, and this version focuses on bug fixes.

Bug fixes:

- Fix the issue that the alarm of CPU usage triggered abnormally when emqx container is running

GitHub Commit: [emqx/emqx#9cdaa7](https://github.com/emqx/emqx/commit/9cdaa71a66c44d6bfd7606f8e64bc6670f619cdf)

- Fix the issue that the mechanism of message expiration doesn't take effect

Github Commit: [emqx/emqx#31671f](https://github.com/emqx/emqx/commit/31671f5ee5516e04ca6c648679f030b790c84fd9)

- Fix the issue thar placeholder like '%c' in mountpoint doesn't take effect

Github Commit: [emqx/emqx#58ba22](https://github.com/emqx/emqx/commit/58ba22dfc79ce81ac74fffae60a624d2238585ca)

### emqx-dashboard (plugin)

Bug fixes:

- Fix the issue that the function of SSL is unavailable

Github Commit: [emqx/emqx-dashboard#272a42](https://github.com/emqx/emqx-dashboard/commit/272a42b5ac7b28f52e5e71fae540e47278fac9d5)

## 3.2.2

_Release Date: 2019-08-03_

EMQX 3.2.2 is now available, and this version focuses on bug fixes.

Enhancements:

- Extends configurations of `gen_rpc`

Github PR: [emqx/emqx#2732](https://github.com/emqx/emqx/pull/2732)

### emqx-rule-engine (plugin)

Bug fixes:

- Fix the issue testing URL connectivity

Github PR: [emqx/emqx-rule-engine#88](https://github.com/emqx/emqx-rule-engine/pull/88)

### emqx-dashboard (plugin)

Enhancements:

- Add help page

### ekka (dependency)

Bug fixes:

- Fix the issue that releasing lock could causes crash

Github PR: [emqx/ekka#60](https://github.com/emqx/ekka/pull/60)

## 3.2.1

_Release Date: 2019-07-20_

EMQX 3.2.1 is now available. We've enhanced performance and fixed bugs.

Enhancements:

- Optimize the performance of `gen_rpc`

Github PR: [emqx/emqx#2694](https://github.com/emqx/emqx/pull/2694)

- Support using hostname to automatically discover k8s cluster

Github PR: [emqx/emqx#2699](https://github.com/emqx/emqx/pull/2699)

- Change the default uptime heartbeat interval to 30s

Github PR: [emqx/emqx#2696](https://github.com/emqx/emqx/pull/2696)

Bug fixes:

- Fix the issue that encouter crash when Websocket sessions go offline abnormally

Github PR: [emqx/emqx#2697](https://github.com/emqx/emqx/pull/2697)

- Fix the issue that ws_channel is still online when session closed on exception

Github PR: [emqx/emqx#2704](https://github.com/emqx/emqx/pull/2704)

### emqx-rule-engine (plugin)

Enhancements:

- Improve parameters for republish action

Github PR: [emqx/emqx-rule-engine#81](https://github.com/emqx/emqx-rule-engine/pull/81)

Bug fixes:

- Fix the issue that fail to select payload fields using '.'

Github PR: [emqx/emqx-rule-engine#83](https://github.com/emqx/emqx-rule-engine/pull/83)

### emqx-dashboard (plugin)

Bug fixes:

- Fix the issue rendering resources list incorrectly in Dashboard on Safari

Github PR: [emqx/emqx-dashboard#124](https://github.com/emqx/emqx-dashboard/pull/124) , [emqx/emqx-dashboard#125](https://github.com/emqx/emqx-dashboard/pull/125) , [emqx/emqx-dashboard#126](https://github.com/emqx/emqx-dashboard/pull/126)

### emqx-lwm2m (plugin)

Enhancements:

- Compatible with client login using LwM2M v1.1

Github Commit: [emqx/emqx-lwm2m#1c03bf](https://github.com/emqx/emqx-lwm2m/commit/1c03bf3b6a9cae7ed52f87ee219e9dd9d8824892)

### emqx-rel (build project)

Enhancements:

- Support building `emqx-rel` with built-in rebar3

Github PR: [emqx/emqx-rel#394](https://github.com/emqx/emqx-rel/pull/394)

- Delay EMQX windows service auto start

Github PR: [emqx/emqx-rel#395](https://github.com/emqx/emqx-rel/pull/395)

## 3.2.0

_Release Date: 2019-07-12_

EMQX 3.2.0 is mainly for improvements of rule engine.

### Rule Engine

Improve rule engine and ui of dashboard, support more actions.

### Project building

Support rebar3 to build project.

### MQTT Broker Bridge

Bridging to MQTT Broker is now provided by emqx-bridge-mqtt (plugin) instead.

### HTTP Plugin

Support HTTPs.

### Cluster (ekka)

Improve stability of emqx cluster.

### Other Plugins and Dependencies

Fix Windows service registering issue.

## 3.1.2

_Release Date: 2019-06-06_

EMQX 3.1.2 is now available. We've fixed bugs and improved stability.

### EMQX Core

Bug fixes:

- Fix [emqx/emqx: issue #2595](https://github.com/emqx/emqx/issues/2595)

Github PR: [emqx/emqx#2601](https://github.com/emqx/emqx/pull/2601)

- Fix the issue that failed when setting the log level

Github PR: [emqx/emqx#2600](https://github.com/emqx/emqx/pull/2600)

- Fix the issue that doesn't match the return value

Github PR: [emqx/emqx#2560](https://github.com/emqx/emqx/pull/2560)

- Hotfix for `emqx_sn` and `emqx_coap` plugins

Github PR: [emqx/emqx#2556](https://github.com/emqx/emqx/pull/2556)

### emqx-coap (plugin)

Bug fixes:

- Fix the issue that messages can't be published

Github PR: [emqx/emqx-coap#120](https://github.com/emqx/emqx-coap/pull/120)

### ekka (deps)

Bug fixes:

- Fix the issue makes `emqx_sm_locker` crash

Github PR: [emqx/ekka#54](https://github.com/emqx/ekka/pull/54)

- Fix the issue that k8s can't use dns cluster

Github PR: [emqx/ekka#53](https://github.com/emqx/ekka/pull/53)

- Fix the issue that etcd cluster is unusable

Github PR: [emqx/ekka#52](https://github.com/emqx/ekka/pull/52)

## 3.1.1

_Release Date: 2019-05-10_

EMQX 3.1.1 is now available. In this version we've fixed bugs and improved stability.

### EMQX Core

Enhancements:

- Enlarge the maximum number of characters printed by each log event

Github PR: [emqx/emqx#2509](https://github.com/emqx/emqx/pull/2509)

- `force_shutdown_policy` will use a different value according to digits of system

Github PR: [emqx/emqx#2515](https://github.com/emqx/emqx/pull/2515)

Bug fixes:

- Configure and use `long_gc` 与 `long_schedule` correctly

Github PR: [emqx/emqx#2504](https://github.com/emqx/emqx/pull/2504) , [emqx/emqx#2513](https://github.com/emqx/emqx/pull/2513)

- Fix the issue `suboptions/count` not been updated

Github PR: [emqx/emqx#2507](https://github.com/emqx/emqx/pull/2507)

### emqx-lwm2m (plugin)

Bug fixes:

- Fix the issue that mountpoint didn't take effect

Github PR: [emqx/emqx-lwm2m#34](https://github.com/emqx/emqx-lwm2m/pull/34)

- Fix the issue that message couldn't be forwarded by `emqx-web-hook`

Github PR: [emqx/emqx-lwm2m#35](https://github.com/emqx/emqx-lwm2m/pull/35)

## 3.1.0

_Release Date: 2019-04-26_

EMQX 3.1.0 is now available. The rule engine has become stable and production ready. We've also introduced an emqx-edge manager - the `Storm` , and improved some code for flapping.

### EMQX Core

Enhancements:

- Add emqx_ct_helpers as deps and refactor test suites

Github PR: [emqx/emqx#2480](https://github.com/emqx/emqx/pull/2480)

- Refactor flapping code

Github PR: [emqx/emqx#2476](https://github.com/emqx/emqx/pull/2476)

### emqx-management (plugin)

Bug fixes:

- Fixed listeners acceptors is undefined

Github PR: [emqx/emqx-management#76](https://github.com/emqx/emqx-management/pull/76)

### emqx-rule-engine (plugin)

Enhancements:

- Support validation of rule action params

Github PR: [emqx/emqx-rule-engine#b28318](https://github.com/emqx/emqx-rule-engine/commit/b283184dcbb207e8d58ac308c027a093a4f4ab88)

- Check dependency when deleting resources

Github PR: [emqx/emqx-rule-engine#fa75b9](https://github.com/emqx/emqx-rule-engine/commit/fa75b952efb7951bc57242adc8e953dbbba6b2ed)

- Remove `from` param from republish action

Github PR: [emqx/emqx-rule-engine#8721eb](https://github.com/emqx/emqx-rule-engine/commit/8721ebe583d5426f239b5b1f044fe381bf4ea0b7)

- Fix where clause of SQL cannot handle integers

Github PR: [emqx/emqx-rule-engine#c9c761](https://github.com/emqx/emqx-rule-engine/commit/c9c7616f86019657861dff408854e9c5238d666b)

### emqx-storm (plugin)

Enhancements:

- Support edge storm

Github Repository: [emqx/emqx-storm](https://github.com/emqx/emqx-storm)

## 3.0.1

_Release Date: 2019-01-25_

The EMQX 3.0.1 is now available. Many improvements and bug fixes has been made.

### EMQX Core

Enhancements:

- Add +L vm args for reducing some memory for emqx edge

Github PR: [emqx/emqx#2110](https://github.com/emqx/emqx/pull/2110)

- Change logger level in a single command

Github PR: [emqx/emqx#2115](https://github.com/emqx/emqx/pull/2115)

- Refactor the emqx bridge; Support bridge message persistence.

Github PR: [emqx/emqx#2160](https://github.com/emqx/emqx/pull/2160) , [emqx/emqx#2117](https://github.com/emqx/emqx/pull/2117) , [emqx/emqx#2113](https://github.com/emqx/emqx/pull/2113) , [emqx/emqx#2108](https://github.com/emqx/emqx/pull/2108) , [emqx/emqx#2053](https://github.com/emqx/emqx/pull/2053)

- Optimize route matching

Github PR: [emqx/emqx#2124](https://github.com/emqx/emqx/pull/2124)

- Improve the design of 'emqx_client' module

Github PR: [emqx/emqx#2137](https://github.com/emqx/emqx/pull/2137)

- Improve the design of 'emqx_pool' module

Github PR: [emqx/emqx#2138](https://github.com/emqx/emqx/pull/2138)

- Improve shared subscribe dispatch implementation

Github PR: [emqx/emqx#2144](https://github.com/emqx/emqx/pull/2144)

- Re-generate the configuration when restarting emqx

Github PR: [emqx/emqx#2175](https://github.com/emqx/emqx/pull/2175)

Bug Fixes:

- Fix crash if peer closed the connection

Github PR: [emqx/emqx#2120](https://github.com/emqx/emqx/pull/2120)

- Fix the bug that send will message unexpectedly

Github PR: [emqx/emqx#2156](https://github.com/emqx/emqx/pull/2156)

### emqx-lwm2m (plugin)

Bug Fixes:

- Remove authentication for LwM2M

GitHub PR: [emqx/emqx-lwm2m#14](https://github.com/emqx/emqx-lwm2m/pull/14)

### emqx-auth-username (plugin)

Enhancements:

- Support optional encryption modes

GitHub PR: [emqx/emqx-auth-usernmae#64](https://github.com/emqx/emqx-auth-username/pull/64)

### emqx-auth-clientid (plugin)

Enhancements:

- Support optional encryption modes

GitHub PR: [emqx/emqx-auth-clientid#52](https://github.com/emqx/emqx-auth-username/pull/52)

### emqx-management (plugin)

Enhancements:

- Add a new CLI 'plugins reload \<Name>'; Re-generate the configuration when reloading emqx plugin

Github PR: [emqx/emqx-management#30](https://github.com/emqx/emqx-management/pull/30)

## 3.0.0

_Release Date: 2018-12-22_

The EMQX 3.0.0 is now available. In this release, we have re-designed the ETS tables for subscripions, and enhanced the performance by refactoring some modules and tuning the erlang vm args.

### EMQX Core

Enhancements:

- Move addtional vm args to a separate vm.args file

Github PR: [emqx/emqx#2033](https://github.com/emqx/emqx/pull/2033) , [emqx/emqx#2057](https://github.com/emqx/emqx/pull/2057) , [emqx/emqx#2070](https://github.com/emqx/emqx/pull/2070)

- Add will topic validation and acl check

Github PR: [emqx/emqx#2075](https://github.com/emqx/emqx/pull/2075)

- Add option to disconnect client in case of ACL denied

Github PR: [emqx/emqx#2059](https://github.com/emqx/emqx/pull/2059)

- Implement a new session supervisor

Github PR: [emqx/emqx#2077](https://github.com/emqx/emqx/pull/2077)

- Add 'active_n' option to optimize the CPU usage of emqx_connection

Github PR: [emqx/emqx#2060](https://github.com/emqx/emqx/pull/2060)

- Supports batch processing 'DOWN' events

Github PR: [emqx/emqx#2060](https://github.com/emqx/emqx/pull/2060)

- Add sharding for subscription tables

Github PR: [emqx/emqx#2044](https://github.com/emqx/emqx/pull/2044)

- Implement a new 'emqx_gc' module

Github PR: [emqx/emqx#2090](https://github.com/emqx/emqx/pull/2090)

Bug Fixes:

- Fix bug for Topic Alias Maximum

Github PR: [emqx/emqx#2074](https://github.com/emqx/emqx/pull/2074)

- Fix a bug that would not send a will message in some cases

Github PR: [emqx/emqx#2068](https://github.com/emqx/emqx/pull/2068)

### emqx-auth-ldap (plugin)

Enhancements:

- Better design

GitHub PR: [emqx/emqx-auth-ldap#46](https://github.com/emqx/emqx-auth-ldap/pull/46)

### emqx-lua-hook (plugin)

Bug Fixes:

- Make all test cases pass

GitHub PR: [emqx/emqx-lua-hook#45](https://github.com/emqx/emqx-lua-hook/pull/45)

### emqx-management (plugin)

Enhancements:

- Add test cases for rest api and better design for the format of response

GitHub PR: [emqx/emqx-management#21](https://github.com/emqx/emqx-management/pull/21)

## 2.3.11

_Release Date: 2018-07-23_

### Bugfix and Enhancements

Fix the getting config REST API which throws exceptions.

Support to restart listeners when emqttd is running.

Specify a fixed tag for the dependency libraries.

### emq-auth-jwt

Fix token verification with jwerl 1.0.0

### emq-auth-mongo

Support $all variable in ACL query. (emq-auth-mongo#123)

Support both clientid and username variables in all queries. (emq-auth-mongo#123)

## 2.3.10

_Release Date: 2018-06-27_

### Bugfix and Enhancements

Upgrade the esockd library to v5.2.2

### emq-auth-http

Ignore auth on ignore in body, allows for chaining methods

## 2.3.9

_Release Date: 2018-05-20_

### Bugfix and Enhancements

Bugfix: check params for REST publish API (#1599)

Upgrade the mongodb library to v3.0.5

### esockd

Bugfix: proxy protocol - set socket to binary mode (#78)

## 2.3.8

_Release Date: 2018-05-11_

### Bugfix and Enhancements

Bugfix: unregister users CLI when unload emq_auth_username (#1588)

Bugfix: Should be an info level when change CleanSession (#1590)

Bugfix: emqttd_ctl crashed when emq_auth_usename doesn't exist (#1588)

### emq-auth-mongo

Improve: Support authentication database (authSource) (#116)

## 2.3.7

_Release Date: 2018-04-22_

### Bugfix and Enhancements

Bugfix: fixed spec of function setstats/3 (#1575)

Bugfix: clean dead persistent session on connect (#1575)

Bugfix: dup flag not set when re-deliver (#1575)

Bugfix: Upgrade the lager_console_backend config (#1575)

Improve: Support set k8s namespace (#1575)

Upgrade the ekka library to v0.2.3 (#1575)

Improve: move PIPE_DIR dir from /tmp/${WHOAMI}_erl_pipes/$NAME/ to /$RUNNER_DATA_DIR/${WHOAMI}\_erl_pipes/$NAME/ (emq-relx#188)

### emq-auth-http

Improve: Retry 3 times when httpc:request occurred socket_closed_remotely error (emq-auth-http#70)

## 2.3.6

_Release Date: 2018-03-25_

### Bugfix and Enhancements

Security: LWT message checking the ACL (#1524)

Bugfix: Retain msgs should not be sent to existing subscriptions (#1529)

### emq-auth-jwt

Validate JWT token using a expired field (#29)

## 2.3.5

_Release Date: 2018-03-03_

### Bugfix and Enhancements

Feature: Add etc/ssl_dist.conf file for erlang SSL distribution (emq-relx#178)

Feature: Add node.ssl_dist_optfile option and etc/ssl_dist.conf file (#1512)

Feature: Support Erlang Distribution over TLS (#1512)

Improve: Tune off the 'tune_buffer' option for external MQTT connections (#1512)

### emq-sn

Clean registered topics if mqtt-sn client send a 2nd CONNECT in connected state (#76)

Upgrade the esockd library to v5.2.1 (#76)

### emq-auth-http

Remove 'password' param from ACL and superuser requests (#66)

## 2.3.4

_Release Date: 2018-01-29_

### Bugfix and Enhancements

Feature: Forward real client IP using a reverse proxy for websocket (#1335)

Feature: EMQ node.name with link local ipv6 address not responding to ping (#1460)

Feature: Add PROTO_DIST_ARG flag to support clustering via IPv6 address. (#1460)

Bugfix: retain bit is not set when publishing to clients (when it should be set). (#1461)

Bugfix: Can't search topic on web dashboard (#1473)

### emq-sn

Bugfix: CONNACK is not always sent to the client (emq-sn#67)

Bugfix: Setting the port to ::1:2000 causes error (emq-sn#66)

## 2.3.3

_Release Date: 2018-01-08_

### Bugfix and Enhancements

Add a full documentation for emq.conf and plugins.

Repair a dead link in README - missing emq-lwm2m. (#1430)

Subscriber with wildcard topic does not receive retained messages with sub topic has $ sign (#1398)

Web Interface with NGINX Reverse Proxy not working. (#953)

### emq-dashboard

Add dashboard.default_user.login , dashboard.default_user.password options to support configuring default admin.

### emq-modules

The emq-modules rewrite config is not right. (#35)

### emq-docker

Upgrade alpine to 3.7 (#31)

### emq-packages

Support ARM Platform (#12)

## 2.3.2

_Release Date: 2017-12-26_

### Bugfix and Enhancements

Support X.509 certificate based authentication (#1388)

Add proxy_protocol, proxy_protocol_timeout options for ws/wss listener.

Cluster discovery etcd nodes key must be created manually. (#1402)

Will read an incorrect password at the last line of emq_auth_username.conf (#1372)

How can i use SSL/TLS certificate based client authentication? (#794)

Upgrade the esockd library to v5.2.

### esockd

Improve the parser of proxy protocol v2.

Add 'send_timeout', 'send_timeout_close' options.

Rename esockd_transport:port_command/2 function to async_send/2.

Add test case for esockd_transport:async_send/2 function.

Add esockd_transport:peer_cert_subject/1, peer_cert_common_name/1 functions.

### emq-auth-mysql

Update depends on emqtt/mysql-otp.

Fixed the issue that Cannot connect to MySQL 5.7 (#67).

### emq-relx

Fix mergeconf/3 appending line break error. (#152)

### emq-sn

Fix crash in emq_sn_gateway:transform() function which handles SUBACK. (#57)

Define macro SN_RC_MQTT_FAILURE. (#59)

### emq-web-hook

Filter auth_failure client for disconnected hook. (#30)

## 2.3.1

_Release Date: 2017-12-03_

### Bugfix and Enhancements

Remove the unnecessary transactions to optimize session management.

Should not exit arbitrarily when clientid conflicts in mnesia.

Change the default value of 'mqtt.session.enable_stats' to 'on'.

The DUP flag should be set to 0 for all QoS0 messages. (emqttd#1319)

Fix the 'no function clause' exception. (emqttd#1293)

The retained flags should be propagated for bridge. (emqttd#1293)

The management API should listen on 0.0.0.0:8080. (emqttd#1353)

Fast close the invalid websocket in init/1 function.

erlang:demonitor/1 the reference when erasing a monitor. (emqttd#1340)

### emq-retainer

Don't clean the retain flag after the retained message is stored.

Add three CLIs for the retainer plugin. (emq-retainer#38)

### emq-dashboard

Refactor(priv/www): improve the routing page. (emq-dashboard#185)

### emq-modules

Turn off the subscription module by default. (emq-modules#26)

### emq-sn

Add an integration test case for sleeping device.

Do not send will topic if client is kicked out.

Prevent crash information in log when emq_sn_gateway getting timeout, since it is a possible procedure.

### emq-relx

Support node cookie value with = characters. (emq-relx#146)

### mochiweb

Improve Req:get(peername) funciton to support x-forwarded-for and x-remote-port . (emqtt/mochiweb#9)

## 2.3.0 "Passenger's Log"

_Release Date: 2017-11-20_

EMQ 2.3.0 is available now! EMQ R2.3.0 improved the PubSub design to avoid race-condition issue and optimized the message routing efficiency. The self-signed certificates for SSL released with EMQ has been updated. This release also comes with a new dashboard theme and improvement of API design.

### Bugfix and Enhancements

Fixed the issue that Retained message is not sent for Subscribe to existing topic. (emqttd#1314)

Fixed the issue that The DUP flag MUST be set to 0 for all QoS0 messages.(emqttd#1319)

Improve the pubsub design and fix the race-condition issue. (emqttd#PR1342)

Crash on macOS High Sierra (emqttd#1297)

### emq-dashboard Plugin (emq-dashboard#PR174)

Upgraded the 'subscriptions' RESTful API.

Improvement of the auth failure log. (emq-dashboard#59)

### emq-coap Plugin (emq-coap#PR61)

Replaced coap_client with er_coap_client.

Fixed: correct the output format of coap_discover() to enable ".well-known/core".

Refactor the coap_discover method.

### emq-relx

Upgraded the bin/nodetool script to fix the rpcterms command.

### emq-web-hook Plugin

Fixed the emq_web_hook plugin getting username from client.connected hook. (emq-web-hook#19)

### emq-auth-jwt Plugin(emq-auth-jwt#PR15)

Added test cases for emq_auth_jwt.

Fixed jwt:decode/2 functions's return type.

### emq-auth-mongo Plugin(emq-auth-mongo#PR92)

Updated the default MongoDB server configuration.

## 2.2 "Nostalgia"

_Release Date: 2017-07-08_

_Release Name: Nostalgia_

EMQ 2.2.0 is available now! EMQ R2.2 supports CoAP(RFC 7252), MQTT-SN protocols completely, and it is extensible with Web Hook, Lua Hook and Elixir Hook.

Feature: Add 'listeners restart/stop' CLI command (emqttd#1135)

Bugfix: Exit Code from emqttd_ctl (emqttd#1133)

Bugfix: Fix spec errors found by dialyzer (emqttd#1136)

Bugfix: Catch exceptions thrown from rpc:call/4 (emq-dashboard#128)

Bugfix: Topic has been decoded by gen-coap, no conversion needed (emq-coap#43)

## 2.1.2

_Release Date: 2017-04-21_

Fix emqttd_ctl sessions list CLI

Newline character in emq.conf causing error;(emqttd#1000)

Fix crash caused by duplicated PUBREC packet (emqttd#1004)

Unload the 'session.created' and 'session.teminated' hooks (emq-plugin-template)

## 2.1.1

_Release Date: 2017-04-14_

Localhost:8083/status returns 404 when AWS LB check the health of EMQ (emqttd#984)

Https listener not working in 2.1.0 as in 2.0.7 (emq-dashboard#105)

Fix mqtt-sn Gateway not working (emq-sn#12)

Upgrade emq-sn Plugin (emq-sn#11)

Upgrade emq-coap Plugin (emq-coap#21)

## 2.1.0

_Release Date: 2017-04-07_

The stable release of 2.1 version.

Trouble with auth.mysql.acl_query (emq-auth-mysql#38)

Filter the empty fields in ACL table (emq-auth-mysql#39)

## 2.0.7

_Release Date: 2017-01-20_

The Last Maintenance Release for EMQ 2.0, and support to build RPM/DEB Packages.

Create the emq-package project: [https://github.com/emqtt/emq-package](https://github.com/emqtt/emq-package)

emq-auth-http#9: Update the priv/emq_auth_http.schema, cuttlefish:unset() if no super_req/acl_req config exists

emq-auth-mongo#31: cuttlefish:unset() if no ACL/super config exists

emq-dashboard#91: Fix the exception caused by binary payload

emq-relx#21: Improve the binemqttd.cmd batch script for windows platform

emqttd#873: Documentation: installing-from-source

emqttd#870: Documentation: The word in Documents is wrong

emqttd#864: Hook 'client.unsubscribe' need to handle 'stop'

emqttd#856: Support variables in etc/emq.conf: {{ runner_etc_dir }}, {{ runner_etc_dir }}, {{ runner_data_dir }}

## 2.0.6

_Release Date: 2017-01-08_

Upgrade the [esockd](https://github.com/emqtt/esockd) library to v4.1.1

esockd#41: Fast close the TCP socket if ssl:ssl_accept failed

emq-relx#15: The EMQ 2.0 broker cannot run on Windows.

emq-auth-mongo#31: Mongodb ACL Cannot work?

## 2.0.5

_Release Date: 2016-12-24_

emq-auth-http#9: Disable ACL support

emq-auth-mongo#29: Disable ACL support

emq-auth-mongo#30: {datatype, flag}

## 2.0.4

_Release Date: 2016-12-16_

emqttd#822: Test cases for SSL connections

emqttd#818: trap_exit to link WebSocket process

emqttd#799: Can't publish via HTTPS

## 2.0.3

_Release Date: 2016-12-12_

emqttd#796: Unable to forbidden tcp lisener

emqttd#814: Cannot remove a 'DOWN' node from the cluster

emqttd#813: Change parameters order

emqttd#795: Fix metrics of websocket connections

emq-dashboard#88: Rename the default topic from “/World” to “world”

emq-dashboard#86: Lookup all online clients

emq-dashboard#85: Comment the default listener port

emq-mod-retainer#3: Retained messages get lost after EMQTT broker restart.

## 2.0.2

_Release Date: 2016-12-05_

emqttd#787: Stop plugins before the broker stopped, clean routes when a node down

emqttd#790: Unable to start emqttd service if username/password contains special characters

emq-auth-clientid#4: Improve the configuration of emq_auth_clientid.conf to resolve emqttd#790

emq-auth-username#4: Improve the configuration of emq_auth_username.conf to resolve emqttd#790

## 2.0.1

_Release Date: 2016-11-30_

emqttd#781: Update README for EMQ 2.0

emq_dashboard#84: Show the Cluster Status of Node

emq_dashboard#79: disc_copies to store mqtt_admin table

emq_auth_clientid: disc_copies to store mqtt_auth_clientid table

emq_auth_username: disc_copies to store mqtt_auth_username table

emq_mod_subscription#3: Remove emq_mod_subscription table and module.subscription.backend config

emq_plugin_template#5: Unregister Auth/ACL modules when the plugin unloaded

## 2.0 "West of West Lake"

_Release Date: 2016-11-24_

_Release Name: West of West Lake_

The _EMQ_ Version 2.0, named "West of West Lake", has been released with a lot of improvements and enhancements, and is ready to deploy in production now.

1. First of all, the _EMQ_ broker now supports Shared Subscription and Local Subscription .
2. Supports CoAP(RFC 7252) and MQTT-SN protocol/gateway.
3. Adopt a more user-friendly k = v syntax for the new configuration file.
4. Add more hooks and new plugins, integrate with HTTP, LDAP, Redis, MySQL, PostgreSQL and MongoDB.
5. Cross-platform Builds and Deployment. Run the broker on Linux, Unix, Windows, Raspberry Pi and ARM platform.

### Shared Subscription

Shared Subscription supports Load balancing to distribute MQTT messages between multiple subscribers in the same group:

```bash
---------
|       | --Msg1--> Subscriber1
Publisher--Msg1,Msg2,Msg3-->|  EMQ  | --Msg2--> Subscriber2
|       | --Msg3--> Subscriber3
---------
```

Create a shared subscription with $queue/ or $share/\<group>/ prefix:

Prefix          |  Examples
----------------|---------------------------------------
$queue/         |  mosquitto_sub -t '$queue/topic  
$share/\<group>/ |  mosquitto_sub -t '$share/group/topic

### Local Subscription

The Local Subscription will not create global routes on clustered nodes, and only dispatch MQTT messages on local node.

Usage: subscribe a topic with $local/ prefix.

### erlang.mk and relx

The _EMQ_ 2.0 adopts [erlang.mk](https://erlang.mk) and [relx](https://github.com/erlware/relx) tools to build the whole projects on Linux, Unix and Windows.

### CoAP Support

The _EMQ_ 2.0 supports CoAP(RFC7252) protocol/gateway now, and supports communication between CoAP, MQTT-SN and MQTT clients.

CoAP Protocol Plugin: [https://github.com/emqtt/emqttd_coap](https://github.com/emqtt/emqttd_coap)

### MQTT-SN Support

The _EMQ_ 2.0 now supports MQTT-SN protocol/gateway.

MQTT-SN Plugin: [https://github.com/emqtt/emq_sn](https://github.com/emqtt/emq_sn)

### New Configuration File

The release integrated with cuttlefish library, and adopted a more user-friendly k = v syntax for the new configuration file:

```bash
## Node name
node.name = emqttd@127.0.0.1
...
## Max ClientId Length Allowed.
mqtt.max_clientid_len = 1024
...
```

The new configuration files will be preprocessed and translated to an Erlang app.config before the EMQ broker started:

```bash
----------------------                                          2.0/schema/*.schema      -------------------
    | etc/emq.conf       |                   -----------------              \|/              | data/app.config |
    |       +            | --> mergeconf --> | data/app.conf | -->  cuttlefish generate  --> |                 |
    | etc/plugins/*.conf |                   -----------------                               | data/vm.args    |
    ----------------------                                                                   -------------------
```

### OS Environment Variables

| EMQ_NODE_NAME   | Erlang node name                      |
| --------------- | ------------------------------------- |
| EMQ_NODE_COOKIE | Cookie for distributed erlang node    |
| EMQ_MAX_PORTS   | Maximum number of opened sockets      |
| EMQ_TCP_PORT    | MQTT TCP Listener Port, Default: 1883 |
| EMQ_SSL_PORT    | MQTT SSL Listener Port, Default: 8883 |
| EMQ_HTTP_PORT   | HTTP/WebSocket Port, Default: 8083    |
| EMQ_HTTPS_PORT  | HTTPS/WebSocket Port, Default: 8084   |

### Docker Image

We released an official Docker Image for _EMQ_ 2.0. The open source project for Dockerfile: [https://github.com/emqtt/emq_docker](https://github.com/emqtt/emq_docker) .

### Full Support for Windows

The _EMQ_ 2.0 fully supports Windows platform. You can run 'emqttd_ctl' command and cluster two nodes on Windows now.

### Bugfix and Enhancements

- 764: add mqtt.cache_acl option

- 667: Configuring emqttd from environment variables

- 722: mqtt/superuser calls two times emqtt_auth_http

- 754: "-heart" option for EMQ 2.0

- 741: emq_auth_redis cannot use hostname as server address

### Plugins

| Plugin                                                                  | Description                   |
| ----------------------------------------------------------------------- | ----------------------------- |
| [emq_dashboard](https://github.com/emqtt/emqttd_dashboard)            | Web Dashboard                 |
| [emq_auth_clientid](https://github.com/emqtt/emq_auth_clientid)       | ClientId Auth Plugin          |
| [emq_auth_username](https://github.com/emqtt/emq_auth_username)       | Username/Password Auth Plugin |
| [emq_auth_ldap](https://github.com/emqtt/emq_auth_ldap)               | LDAP Auth                     |
| [emq_auth_http](https://github.com/emqtt/emq_auth_http)               | HTTP Auth/ACL Plugin          |
| [emq_auth_mysql](https://github.com/emqtt/emq_auth_mysql)             | MySQL Auth/ACL Plugin         |
| [emq_auth_pgsql](https://github.com/emqtt/emq_auth_pgsql)             | PostgreSQL Auth/ACL Plugin    |
| [emq_auth_redis](https://github.com/emqtt/emq_auth_redis)             | Redis Auth/ACL Plugin         |
| [emq_auth_mongo](https://github.com/emqtt/emq_auth_mongo)             | MongoDB Auth/ACL Plugin       |
| [emq_mod_presence](https://github.com/emqtt/emq_mod_presence)         | Presence Module               |
| [emq_mod_retainer](https://github.com/emqtt/emq_mod_retainer)         | Retainer Module               |
| [emq_mod_rewrite](https://github.com/emqtt/emq_mod_rewrite)           | Topic Rewrite Module          |
| [emq_mod_subscription](https://github.com/emqtt/emq_mod_subscription) | Subscription Module           |
| [emq_coap](https://github.com/emqtt/emq_coap)                         | CoAP Protocol Plugin          |
| [emq_sn](https://github.com/emqtt/emq_sn)                             | MQTT-SN Protocol Plugin       |
| [emq_stomp](https://github.com/emqtt/emq_stomp)                       | STOMP Protocol Plugin         |
| [emq_sockjs](https://github.com/emqtt/emq_sockjs)                     | STOMP over SockJS Plugin      |
| [emq_recon](https://github.com/emqtt/emq_recon)                       | Recon Plugin                  |
| [emq_reloader](https://github.com/emqtt/emq_reloader)                 | Reloader Plugin               |
| [emq_plugin_template](https://github.com/emqtt/emq_plugin_template)   | Template Plugin               |

## 1.1.3

_Release Date: 2016-08-19_

Support './bin/emqttd_ctl users list' CLI (#621)

Cannot publish payloads with a size of the order 64K using WebSockets (#643)

Optimize the procedures that retrieve the Broker version and Borker description in the tick timer (PR#627)

Fix SSL certfile, keyfile config (#651)

## 1.1.2

## 1.1.2

_Release Date: 2016-06-30_

Upgrade mysql-otp driver to 1.2.0 (#564, #523, #586, #596)

Fix WebSocket Client Leak (PR #612)

java.io.EOFException using paho java client (#551)

Send message from paho java client to javascript client (#552)

Compatible with the Qos0 PUBREL packet (#575)

Empty clientId with non-clean session accepted (#599)

Update docs to fix typos (#601, #607)

## 1.1.1

_Release Date: 2016-06-04_

Compatible with the Qos0 PUBREL packet (#575)

phpMqtt Client Compatibility (#572)

java.io.EOFException using paho java client (#551)

## 1.1

_Release Date: 2016-06-01_

### Highlights

Upgrade eSockd library to 4.0 and Support IPv6

Support to listen on specific IP Address:

```bash
{mqtt, {"192.168.1.20", 1883}, [
        ...
    ]},
```

Add MongoDB, HTTP Authentication/ACL Plugins

Upgrade MySQL, PostgreSQL, Redis Plugins to support superuser authentication and avoid SQL Injection

### Enhancements

Allow human-friendly IP addresses (PR#395)

File operation error: emfile (#445)

emqttd_plugin_mongo not found in emqttd (#489)

emqttd_plugin_mongo Error While Loading in emqttd (#505)

Feature request: HTTP Authentication (#541)

Compatible with the Qos0 PUBREL packet (#575)

### Bugfix

Bugfix: function_clause exception occurs when registering a duplicated authentication module (#542)

Bugfix: ./emqttd_top msg_q result: {"init terminating in do_boot",{undef,[{etop,start,[],[]},{init,start_it,1,[]},{init,start_em,1,[]}]}} (#557)

### Tests

111 common test cases.

### Dashboard Plugin

WebSocket Page: Support 'Clean Session', Qos, Retained parameters (emqttd_dashboard#52)

Upgrade eSockd library to 4.0, Show OTP Release on Overview Page (emqttd_dashboard#61)

Changing dashboard credentials for username authentication (emqttd_dashboard#56)

Add './bin/emqttd_ctl admins' CLI, support to add/delete admins

### HTTP Auth Plugin

Authentication/ACL by HTTP API: [https://github.com/emqtt/emqttd_auth_http](https://github.com/emqtt/emqttd_auth_http)

### MongoDB Plugin

Upgrade Erlang MongoDB driver to v1.0.0

Support superuser authentication

Support ACL (emqttd_plugin_mongo#3)

### MySQL Plugin

Support superuser authentication

Use parameterized query to avoid SQL Injection

### Postgre Plugin

Support superuser authentication

Use parameterized query to avoid SQL Injection

### Redis Plugin

Support superuser authentication

Support ClientId authentication by '%c' variable

### Reloader Plugin

Reload modified modules during development automatically.

## 1.0.3

_Release Date: 2016-05-23_

eSockd 3.2

MochiWeb 4.0.1

## 1.0.2

_Release Date: 2016-05-04_

Issue#534 - './bin/emqttd_ctl vm' - add 'port/count', 'port/limit' statistics

Issue#535 - emqttd_client should be terminated properly even if exception happened when sending data

PR#519 - The erlang '-name' requires the fully qualified host name

emqttd_reloader plugin - help reload modified modules during development.

## 1.0.1

_Release Date: 2016-04-16_

PR#515 - Fix '$queue' pubsub, add 'pubsub_queue' test and update docs

## 1.0 (The Seven Mile Journey)

_Release Date: 2016-04-13_

_Release Name: The Seven Mile Journey_

We finally released Version 1.0 (The Seven Mile Journey) with full documentation after two years' development and more than fifty iterations.

The emqttd 1.0 implements a fully-featured, scalable, distributed and extensible open-source MQTT broker for IoT, M2M and Mobile applications:

1. Full MQTT V3.1/3.1.1 Protocol Specifications Support
2. Massively scalable - Scaling to 1 million connections on a single server
3. Distributed - Route MQTT Messages among clustered or bridged broker nodes
4. Extensible - LDAP, MySQL, PostgreSQL, Redis Authentication/ACL Plugins

### Bugfix and Enhancements

Possible race condition using emqttd_cm (#486)

Improve the design of retained message expiration (#503)

Do not expire the retained messages from $SYS/# topics (#500)

### Documentation

[https://docs.emqx.io/broker/v2/en/index.html](https://docs.emqx.io/broker/v2/en/index.html)

[http://docs.emqtt.com/](http://docs.emqtt.com/)

### Thanks

Thank Ericsson for the Great Erlang/OTP Platform ( [http://erlang.org/](http://erlang.org/) )!

Contributors on GitHub: @callbay @lsxredrain @hejin1026 @desoulter @turtleDeng @Hades32 @huangdan @phanimahesh @dvliman @Prots @joaohf

Partners: EACG ( [http://eacg.de/](http://eacg.de/) )

Favorite Band: The Seven Mile Journey ( [http://www.thesevenmilejourney.dk/](http://www.thesevenmilejourney.dk/) )

## 0.2.0

_Release Date: 2014-12-07_

rewrite the project, integrate with esockd, mochiweb

support MQTT 3.1.1

support HTTP to publish message

## 0.1.5

_Release Date: 2013-01-05_

Bugfix: remove QOS_1 match when handle PUBREL request

Bugfix: reverse word in emqtt_topic:words/1 function

## 0.1.4

_Release Date: 2013-01-04_

Bugfix: fix "mosquitto_sub -q 2 ......" bug

Bugfix: fix keep alive bug

## 0.1.3

_Release Date: 2013-01-04_

Feature: Support QOS2 PUBREC, PUBREL, PUBCOMP messages

Bugfix: fix emqtt_frame to encode/decoe PUBREC/PUBREL messages

## 0.1.2

_Release Date: 2012-12-27_

Feature: release support like riak

Bugfix: use ?INFO/?ERROR to print log in tcp_listener.erl

## 0.1.1

_Release Date: 2012-09-24_

Feature: use rebar to generate release

Feature: support retained messages

Bugfix: send will msg when network error

## 0.1.0

_Release Date: 2012-09-21_

The first public release.
