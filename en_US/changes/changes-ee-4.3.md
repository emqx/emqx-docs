# Releases

## e4.3.18

*Release Date: 2022-12-29*

### Enhancements

- Fix the "ORA-01000: maximum open cursors exceeded" problem when sending data via the emqx oracle resource [#1560](https://github.com/emqx/emqx-enterprise/pull/1560).

- Add more PSK ciphers support [#1619](https://github.com/emqx/emqx-enterprise/pull/1619).

- Upgrade Erlang/OTP from 23.3.4.9-3 to 23.3.4.18-1 [#1660](https://github.com/emqx/emqx-enterprise/pull/1660).

### Bug fixes

- Fixed an issue where the configuration would not be reloaded on all nodes in a cluster after importing a backup configuration. [#1486](https://github.com/emqx/emqx-enterprise/pull/1486)

- Fixed an issue where the HTTP API would fail to download a backup configuration file when downloading it from a node where it does not reside in. [#1486](https://github.com/emqx/emqx-enterprise/pull/1486)

- Add validations for the config fields of Kafka resource [#1511](https://github.com/emqx/emqx-enterprise/pull/1511).
  Before this change, when creating Kafka resources, some of the config fields had no validity check, such
  as duration and byte size. Even if arbitrary strings were passed to these fields, the resource
  could be created successfully, then later leads to runtime error (until resource is updated).

- Avoid delete offline message twice [#1522](https://github.com/emqx/emqx-enterprise/pull/1522).
  EMQX while delete offline message in external database when subscriber send a PUBACK or PUBREC packet.
  But a message with `retain = true` will be stored twice (in retainer and external database) in case retain message and offline message are used in same time.
  The reduplicated PUBACK and PUBREC will trigger deleted action twice. And the action-metrics will also increase caused by Rule-SQL execution suceeeed.
  In most cases this does not generate any exceptions or errors, and only a few databases will report that the message to be deleted does not exist on the second delete.
  This change will avoid redundant offline message deletion operations.

- Upgrade http client library `ehttpc` from `0.2.1` to `0.4.2` [#1587](https://github.com/emqx/emqx-enterprise/pull/1587).

- Added topic validation for `emqx_mod_rewrite`. The dest topics contains wildcards are not allowed to publish [#1589](https://github.com/emqx/emqx-enterprise/pull/1589).

- The offline message clickhouse action prints an info level log: `Destroyed .. Successfully` when enabling a rule [#1594](https://github.com/emqx/emqx-enterprise/pull/1594).

- Now the rules can be created even though the corresponding resources are not ready [#1620](https://github.com/emqx/emqx-enterprise/pull/1620).
  Before this change, one cannot create rules without getting the resources connected. We made it
  possible in this change, but the newly created rule will be in `disabled` state.

- Fixed `cluster/invite_node` crash if node name is not provided [#1531](https://github.com/emqx/emqx-enterprise/pull/1531).

- Fixed broken error message for bad rpc in `/load_rebalance/{node}/evacuation/start` [#1572](https://github.com/emqx/emqx-enterprise/pull/1572).

- Fixed load bootstrap file when no bootstrap user in `mqtt_app` [#1600](https://github.com/emqx/emqx-enterprise/pull/1600).

- The expired 'awaiting_rel' queue is not cleared after persistent session MQTT client disconnected [#1574](https://github.com/emqx/emqx-enterprise/pull/1574).
  Before this change, if the 'awaiting_rel' queue is full when the MQTT client reconnect to the broker and publish a QoS2 message, the client will get
  disconnected by the broker with reason code RC_RECEIVE_MAXIMUM_EXCEEDED(0x93),
  even if the packet IDs in the 'awaiting_rel' queue have already expired.

- Trigger `message.dropped` hook when QoS2 message is resend by client with a same packet id, or 'awaiting_rel' queue is full [#1605](https://github.com/emqx/emqx-enterprise/pull/1605).

- After a reconnect, the unacknowledged QoS1/QoS2 messages in non-clean session were not retransmitted periodically as before the reconnect [#1617](https://github.com/emqx/emqx-enterprise/pull/1617).
  The configuration `zone.<zone-name>.retry_interval` specifies the retransmission interval of
  unacknowledged QoS1/QoS2 messages (defaults to 30s).
  Prior to this fix, unacknowledged messages buffered in the session are re-sent only once after session take-over, but not retried at configured interval.

- Fix Rule-Engine action `Data to InfluxDB` exection failed since hot-upgrade from `e4.3.0..e4.3.10` to `e4.3.11..e4.3.17` [#1601](https://github.com/emqx/emqx-enterprise/pull/1601).

- Added validation for Kafka action parameters, Segment Bytes should not be greater than Max Bytes [#1608](https://github.com/emqx/emqx-enterprise/pull/1608).

- Added validation for the duration and bytesize parameters of Pulsar actions [#1631](https://github.com/emqx/emqx-enterprise/pull/1631).

- Authentication for RocketMQ resource not working [#1561](https://github.com/emqx/emqx-enterprise/pull/1561).
  In this change we moved the configuration fields `access_key`, `secret_key`
  and `security_token` from the `data_to_rocket` action to the `bridge_rocket`
  resource. And we also added a new field `namespace` for RocketMQ services in
  Aliyun cloud.

- When dashboard creates a new user, the password length must match 3-32 and the format is `^[A-Za-z0-9]+[A-Za-z0-9-_]*$` [#1599](https://github.com/emqx/emqx-enterprise/pull/1599).

- When resource creation is too slow, there may be some temporary probing connections left [#1641](https://github.com/emqx/emqx-enterprise/pull/1641).

- Add the `SNI` field for SSL connection configuration of Kafka resource [#1647](https://github.com/emqx/emqx-enterprise/pull/1647).

- Fixed the issue that the MongoDB resource connection process was slow when authentication was enabled [#1669](https://github.com/emqx/emqx-enterprise/pull/1669).

- Fixed the issue that after the release hot upgrade, EMQX occasionally alarms resources down, and the alarms could not be automatically cleared [#1668](https://github.com/emqx/emqx-enterprise/pull/1668).

## e4.3.17

*Release Date: 2022-11-26*

### Enhancements

- Support to use placeholders like `${var}` in the `Collection` field of Rule-Engine's MongoDB actions [#1503](https://github.com/emqx/emqx-enterprise/pull/1503).

- Add a format check to the `host` field of the InfluxDB resource in Rule-Engine [#1426](https://github.com/emqx/emqx-enterprise/pull/1426).
  The host field should be an ip/domain without scheme and port.

- Make sure listener's `tls_versions` config value is one or more of `tlsv1`, `tlsv1.1`, `tlsv1.2`, `tlsv1.3` [#9260](https://github.com/emqx/emqx/pull/9260).

- Remove useless information from the dashboard listener failure log [#9260](https://github.com/emqx/emqx/pull/9260).

- We now trigger the `'message.acked'` hook after the CoAP gateway sends a message to the device and receives the ACK from the device [#9264](https://github.com/emqx/emqx/pull/9264).
  With this change, the CoAP gateway can be combined with the offline message caching function (in the
  emqx enterprise), so that CoAP devices are able to read the missed messages from the database when
  it is online again.

- Support to use placeholders like `${var}` in the HTTP `Headers` of Rule-Engine's Webhook actions [#9239](https://github.com/emqx/emqx/pull/9239).

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

- Fixed the option to choose the `reset_by_subscriber` offset reset
  policy in Kafka Consumer [#1463](https://github.com/emqx/emqx-enterprise/pull/1463).

- Added the missing `tlsv1.3` option to `tls_versions` in hot-config [#1532](https://github.com/emqx/emqx-enterprise/pull/1532).

- Made Rule-Engine able to connect SQL server when its listening port is not the default (`1433`) [#1464](https://github.com/emqx/emqx-enterprise/pull/1464).

- Make sure Schema-Registry API supports Percent-encoding `name` in HTTP request URI [#1497](https://github.com/emqx/emqx-enterprise/issues/1497).
  Note that the `name` in `POST /api/v4/schemas` request body should not be percent-encoded as it's a JSON field value.

- Fix an upgrade issue for JWT authentication plugin [#1554](https://github.com/emqx/emqx-enterprise/pull/1554).
  When upgrading from e4.3.9 or earlier, an EMQX internal resource which holds the keys will have to be restarted,
  during the restart, clients may fail to be authenticated.

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
  For Rule-Engine's input events like `$events/message_delivered`, and `$events/message_dropped`,
  if the message was delivered to a shared-subscription, the encoding (to JSON) of the event will fail.
  Affected versions: `v4.3.21`, `v4.4.10`, `e4.3.16` and `e4.4.10`.

- Make sure Rule-Engine API supports Percent-encoding `rule_id` and `resource_id` in HTTP request path [#9190](https://github.com/emqx/emqx/pull/9190).
  Note that the `id` in `POST /api/v4/rules` should be literals (not encoded) when creating a `rule` or `resource`.
  See docs [Create Rule](https://docs.emqx.com/en/enterprise/v4.4/advanced/http-api.html#post-api-v4-rules) [Create Resource](https://docs.emqx.com/en/enterprise/v4.4/advanced/http-api.html#post-api-v4-resources).

- Calling 'DELETE /alarms/deactivated' now deletes deactived alarms on all nodes, including remote nodes, not just the local node [#9280](https://github.com/emqx/emqx/pull/9280).

- When republishing messages or bridge messages to other brokers, check the validity of the topic and make sure it does not have topic wildcards [#9291](https://github.com/emqx/emqx/pull/9291).

- Disable authorization for `api/v4/emqx_prometheus` endpoint on management api listener (default 8081) [#9294](https://github.com/emqx/emqx/pull/9294).

## e4.3.16

*Release Date: 2022-10-14*

### Enhancements

- Added more Kafka action parameter checks
  - TCP send buffer size and max batch size parameters are not allowed to be left blank from the configration UI.
  - The combination of "Produce Strategy" set to 'key_dispatch' and the "Key" set to "none" is now not allowed,
    the dashboard will get an error with text like: "with strategy set to 'key_dispatch', key is not allowed to be 'none'"

- TLS listener memory usage optimization [#9005](https://github.com/emqx/emqx/pull/9005).
  New config `listener.ssl.$NAME.hibernate_after` to hibernate TLS connection process after idling.
  Hibernation can reduce RAM usage significantly, but may cost more CPU.
  This configuration is by default disabled.
  Our preliminary test shows a 50% of RAM usage decline when configured to '5s'.

- TLS listener default buffer size to 4KB [#9007](https://github.com/emqx/emqx/pull/9007).
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

- Fix `load_modules` reset after new node joins the cluster.
  Prior to this fix, if `load_modules` for a cluster has been changed, adding a new node to the cluster with default modules
  would cause the other nodes to reset to default too.
  In this fix, the node which is going to join the cluster will copy the `loaded_modules` from the oldest node in the cluster.

- Fix getting subscriptions from backends successfully with QoS values out of range [0, 2].
  Before this change, when we add subscriptions for clients from backends like Redis or MySQL, we won't validate the QoS.
  For example if the QoS is an integer -1, the topic was still subscribed successfully with QoS -1,
  if we send a message to this topic, then an error will occur and the MQTT connection will crash.
  After this change QoS will be clamped into range [0, 2].

- Fix Rule-Engine increased 'success' counter when get subscriptions from Redis failed (due to query Redis timeout).

- Fix Rule-Engine increased 'success' counter when saving offline messages with QoS = 0.
  We don't allow saving offline messages to backends with QoS = 0, so we need to increase the 'failed' counter instead of the 'success' counter in this case.

- Fix the `verify` field is missing from the SSL settings of redis-cluster and redis-sentinel resources.

- Fixed Redis resource liveness problem issue. Prior to this fix, the resource is considered alive when connection can be established.
  The fix is to perform a PING query to make sure the service is alive.

- Fix the redis-cluster resource prints too many error logs when redis servers are not avaliable.

- Fixed an internal Redis resource ID clashing. This clashing may cause resources in use getting deleted when deleting another resource.

- Mask secret/password in the resource/module creation UI.

- Fix HTTP client library to handle SSL socket passive signal [#9145](https://github.com/emqx/emqx/pull/9145).

- Hide redis password in error logs [#9071](https://github.com/emqx/emqx/pull/9071).
  More changes in redis client included in this release:
  - Improve redis connection error logging [eredis#19](https://github.com/emqx/eredis/pull/19).
    Also added support for eredis to accept an anonymous function as password instead of
    passing around plaintext args which may get dumpped to crash logs (hard to predict where).
    This change also added `format_status` callback for `gen_server` states which hold plaintext
    password so the process termination log and `sys:get_status` will print '******' instead of
    the password to console.
  - Avoid pool name clashing [eredis_cluster#22](https://github.com/emqx/eredis_cluster/pull/22.
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

## e4.3.15

*Release Date: 2022-09-17*

### Enhancements

- The `exp`, `nbf` and `iat` claims in JWT authentication support non-integer timestamps

### Bug fixes

- Fix rule engine update behaviour which may initialize actions for disabled rules
- Fix inaccurate delayed publish due to OS time changes
- Fix the issue that the IP address bound to the Dashboard listener did not take effect
- Fix the issue that shared subscriptions might get stuck in an infinite loop when `shared_dispatch_ack_enabled` is set to true
- Fix the issue that the rule engine SQL crashes when subject matching null values

## e4.3.14

*Release Date: 2022-08-29*

### Enhancements

- Improve the log when LwM2M packet parsing fails
- Improve the rule engine error log, the log will contain the rule ID when the action execution fails
- Improve log when `loaded_modules` and `loaded_plugins` files do not exist
- Add a guide for changing the default password on Dashboard
- Improved import performance for Protobuf Schema files

### Bug fixes

- Fix `client.disconnected` event not trigger in some cases
- Fix the issue that the JWK authentication module could not be started later when the JWKS service was not ready in time
- Fix the issue that setting the listener port via an environment variable would prevent either listener from being stopped
- Fix the issue that the built-in database authentication did not distinguish the pagination statistics of the authentication data of the client ID and username
- Fix the issue that the module status would be reset after EMQX restarts after hot upgrade
- Fix Redis driver process leak problem
- Fix rule engine MQTT bridge to AWS IOT connection timeout issue
- Fix `GET /listener` request crashing when listener is not ready
- Fix the issue that the comparison between any variable and null value in the rule engine SQL always returns false after e4.3.7
- Fix the issue that when the execution priority of ExHook is higher than that of the rule engine, the topic filtered by the ExHook Message Hook will not trigger the rule engine
- Fix the issue that the write request of TDEngine may fail because the peer end closes the network connection
- Fix the issue that the configuration of the MQTT-SN module other than the listener would not take effect
- Fix the issue that the ExHook management process was forcibly killed due to the supervisor shutdown timeout
- Fix the issue that the Client ID parameter in ExProto `client.connect` hook is not defined
- Fix ExProto not triggering disconnect event when client is kicked

## e4.3.13

*Release Date: 2022-08-11*

### Important Changes

- Upgraded the OTP version used to solve the low probability of random process unresponsiveness caused by OTP bugs. Users who are still using 4.3 are recommended to upgrade to this version
- From the next release, we will stop supporting macOS 10 and provide an installation package for macOS 11

### Enhancements

- Allows the connection process to be configured to be garbage collected after the TLS handshake is complete to reduce memory footprint, which can reduce memory consumption by about 35% per SSL connection, but increases CPU consumption accordingly
- Allows configuring the log level of the TLS handshake log to view the detailed handshake process

### Bug fixes

- Fix the issue that EMQX could not be started when deployed through Helm Chart after unmounting the `loaded_modules` file in ConfigMap

## e4.3.12

*Release Date: 2022-07-29*

### Enhancement

- Rules engine supports RocketMQ with ACL enabled
- Supports searching and paging of rules in rule engine
- Provides CLI `./bin/emqx check_conf` to actively check if the configuration is correct
- Optimize the write performance of TDEngine in the rule engine
- Optimizing Shared Subscription Performance
- Add `db_name` field to the action of rule engine writing data to TDEngine to improve support for super table

### Bug fixes

- Fix the issue that the action count is wrong when the rule engine writes to TDEngine
- Fix the issue that the process pool size setting does not take effect when the rule engine writes to HStreamDB
- Fix the issue that an error was reported when querying the subscription list after the GB/T 32960 plugin was enabled
- Fix the issue that incompatible configuration items when restoring a 4.2 backup to 4.3 with hot configuration enabled
- Fix the issue that once the old version of EMQX is uninstalled after hot upgrade, EMQX will not be able to start again
- Fix the issue that the keep-alive check for UDP clients in the Multilingual Protocol Extension was incorrect, causing clients not to expire
- Fix the issue that the client information in the Multilingual Protocol Extension was not updated in time
- Fix the issue that the license update does not take effect when running after hot upgrade to e4.3.10 and later versions
- Fix the issue that when the client specified Clean Session as false to reconnect, the shared subscription message in the flight window would be re-dispatched to the old session process
- Fix the issue that the new node did not use the cluster license after joining the cluster
- Fix the issue that the `emqx_lua_hook` plugin cannot cancel the message publishing

## e4.3.11

*Release Date: 2022-06-30*

### Enhancement

- Rule engine supports persisting data to HStreamDB
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
- Improve the JWT authentication module's judgment logic for startup status in a cluster environment
- Extend the timeout of DynamoDB resource status query of rule engine to avoid the problem that some overseas resources may not be available
- Fix the issue that the timestamp is not updated when the PostgreSQL data storage plugin persists messages and updates message consumption
- Fix the issue that the rules engine's Tablestore, Lindorm and InfluxDB resources did not check connection status when creating
- Fix rule engine not checking connection status when creating Tablestore, Lindorm and InfluxDB resources
- Fix rule engine not updating metrics correctly when writing to TDEngine resource fails
- Fix the issue that the Cassandra message store plugin persisted retained messages to multiple tables, causing duplicate messages
- Fix the issue that Kafka resources exported from versions prior to 4.3.0 (only when the Produce policy is set to `first_key_dispatch`) could not be imported into 4.3.0 and later
- Fix the issue that the execution order of topic rewriting and delayed publish is not fixed, now it is fixed to execute topic rewriting first
- Improve the null value handling when rule engine persists data to InfluxDB and Tablestore, now null values will not be written
- Fix the issue that rule engine could not encode MQTT 5.0 user properties
- Fix the issue that the count of `connack.auth_error` is inaccurate when the client uses a protocol version below MQTT v5.0 to access
- Fix the issue that the UDP listeners of LwM2M and CoAP gateways could not bind to the specified network interface
- Fix Dashboard not starting after removing the default Dashboard user in the configuration file
- Fix `client.subscribe` hook not being able to reject subscriptions
- If the placeholder in the ACL rule is not replaced, the client's publish or subscribe operation will be rejected
- Fix the issue that TLS was enabled but no TLS connection was actually established with Pulsar

## e4.3.10

*Release Date: 2022-06-01*

### Enhancement

- Add more time transformation functions to the SQL of rule engine
- Add the `float2str/2` function to the SQL of rule engine to support specifying the output precision of floating point numbers
- Rule engine supports connecting to Pulsar using Basic and JWT authentication
- Add `service_name` option to Oracle resource of rule engine to support Oracle Database RAC
- Support for using JWT for authorization, now MQTT clients can authorize using specific claims that include a pub-sub whitelist
- Improved authentication related metrics to make it easier to understand, now `client.authenticate = client.auth.success + client.auth.failure`
- Support binding the listener of the REST API to a specified network interface
- Upload license will be automatically synchronized to the entire cluster, no need for each node to upload separately, provide HTTP API
- Support multi-condition query and fuzzy query for user data in authentication and authorization using built-in database as data source
- Supports querying clients using the length of the message queue and the number of dropped messages as conditions
- Support to configure the log time format to be compatible with the time format in older versions
- When `use_username_as_clientid` is configured to `true` and the client connects without specifying a `username`, the connection is now rejected with a reason code `0x85`
- Full randomisation of app secrets (previously partially randomised)
- Hot upgrades between incompatible versions will now be rejected
- Allow white spaces in EMQX's installation path
- Boot script fail fast on invalid node name (improve error message readability)

### Bug fixes

- Fix the issue that the client could not get the message after going online when using the PostgreSQL offline message plugin
- Fix the issue that the rules engine could not successfully establish a TLS connection with Pulsar in some cases
- Fix the issue that rule engine's SQL function `hexstr_to_bin/1` could not handle half-byte
- Fix the issue that the alarm was not cleared when the rule engine resource was deleted
- Fix Dashboard HTTPS listener's `verify` option not taking effect
- Fix the issue that messages were lost when the peer session was terminated during the delivery of QoS 1 messages through shared subscriptions
- Fix the issue that when the log tracer encounters large packets, the heap size grows too fast and triggers the policy of forcibly closeing the connection process
- Fix the issue that the relevant hooks were not properly uninstalled when the module was disabled, resulting in abnormal functions
- Fix the issue that the MQTT-SN client would be disconnected when retransmitting QoS 2 messages
- Fix the issue that modules that were turned off in the backup file would be automatically enabled after restoring the backup
- Fix the issue that the returned results did not match the query conditions when querying subscriptions with multiple conditions
- Fix rule engine resource connection test not working
- Fix multiple Dashboard display issues

## e4.3.9

*Release Date: 2022-04-18*

### Enhancement

- Schema registry now supports decoding arbitrary binary payloads to JSON data using gRPC services
- Support for connecting to Pulsar using TLS
- Add `mongo_date` function for SQL in rule engine, which supports saving timestamps as MongoDB Date objects
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
- Fix the issue that the MQTT Bridge plugin cannot be started when only the subscription topic is configured but QoS is not configured
- When creating a rule, if a rule with the same ID already exists, the rules engine will now report an error instead of replacing the existing rule
- Fix the issue that the HTTP driver process pool may not be deleted
- Fix the issue that the module parameters could not be updated again after failing to update
- Fix the incorrect type of some fields in the GB/T 32960 access gateway module in Dashboard
- Fix the issue that the configuration of Bridge resources such as Kafka and Pulsar could not be updated
- Fix the issue that JT/T 808 client authentication fails when anonymous authentication is enabled

## e4.3.8

*Release Date: 2022-04-01*

### Important changes

- For Docker images, the configuration directory `/opt/emqx/etc` has been removed from the VOLUME list, making it easier for users to rebuild images with changed configurations.
- CentOS 7 Erlang runtime rebuilt on OpenSSL-1.1.1n (previously 1.0), prior to v4.3.13, EMQX will fail to handshake and trigger `malformed_handshake_data` exception when clients use certain cipher suites.
- CentOS 8 Erlang runtime system rebuilt on RockyLinux 8. `centos8` will remain in the package name for backward compatibility.

### Enhancement

- Add Pulsar proxy support for rule engine bridging data to Pulsar.
- Add OOM protection for Kafka producers.
- Add command line interface `emqx_ctl pem_cache clean` to allow forcibly clear x509 certificate cache to reload immediately after certificate file update.
- Refactored ExProto so that anonymous clients can also be displayed on Dashboard.
- Topic configuration items in bridges can now use `${node}` placeholders.
- Add validation of UTF-8 strings in MQTT packets in strict mode. When set to `true`, invalid UTF-8 strings will cause the client to disconnect.
- MQTT-SN gateway supports initiative to synchronize registered topics after session resumed.
- Improve the writing precision of rule engine floating point data from 10 decimal places to 17 decimal places.
- EMQX will prompt how to modify the initial password of Dashboard at startup.

### Bug fixes

- Fix `MQTT Subscriber` module not being able to use two-way SSL connection.
- Fix the issue that `PSKFile` module failed to start.
- Fix the issue that `Kafka Consumer Group` module could not process binary data.
- Fix the issue that `Log Trace` could not be stopped.
- Fix the issue that the alternate action could not be triggered when the action of the rule engine persisting data to Oracle and Lindorm (only synchronous operation) failed to execute.
- Fix an issue where rule engine data persistence to Oracle failed but the success count still increased.
- Fix the issue that some zone configurations could not be cleared.
- Fix an issue where changes to some monitoring and alarm configurations were invalid after restarting.
- Fix the issue that `Schema Registry` is not available in the cluster environment.
- Fix the issue that the LwM2M client list query API returned incorrect data in a cluster environment, which resulted in the inability to access the LwM2M gateway module management page.
- Fix the issue that the JT/T 808 location report frame was parsed incorrectly.
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

## e4.3.7

*Release Date: 2022-02-11*

### Important

A cluster-wide total connections calculation bug was fixed in in EMQX Enterprise 4.3.7. Previously only the individual node's local number of connections were checked against the max number of connections allowed by the license. After this fix, the total number of connections is aggregated cluster-wide every 5 seconds. An extra 10% overrun is allowed to compensate the delays in aggregation.

Users planning to upgrade should be aware of the possibility that this change may cause clients to reach the license limit and not be able to connect.

### Enhancement

- Support alarm about the usage rate of license connections. By default, the number of connections reaches 80% of the allowed number of licenses, and the alarm is raised. When it is less than 75%, the alarm is cleared. User can also customize in `emqx.conf`: `license.connection_high_watermark_alarm` , `license.connection_low_watermark_alarm`
- Support alarm about license expiration, when the validity period is less than 30 days, the alarm will be raised
- Rule engine supports the configuration of rules and actions for the event of abnormal loss of client messages to enhance the user's custom processing capabilities in this scenario
- Improve the relevant metrics during the execution of the rule engine SQL matching
- Fuzzy search on client supports special characters such as `*`, `(`, `)`
- Improve ACL-related metrics to solve the issue that the count does not increase due to hitting the ACL cache
- Added `connected_at` field to webhook event notifications
- Log client state before terminating client due to holding the lock too long

### Bug fixes

- Fix the issue that data import and export were not available in some cases
- The module update mechanism is improved to solve the issue that the module is unavailable after the update fails
- Fix the issue that the rule engine did not perform type checking when executing the size comparison statement
- Fix the issue that the related counts are cleared after updating the rule engine action
- Fixed the issue that the metrics interface does not return authentication metrics such as `client.acl.deny` by default
- Fixed the issue that the subscription query interface did not return paginated data
- Fix the issue of parsing failure when STOMP handles TCP sticky packets
- Fix the issue where the session creation time option was not available when filtering clients
- Fix the issue where memory alarms might not be triggered after restarting
- Fix the crash of import data when user data exists in `emqx_auth_mnesia` plugin

## e4.3.6

*Release Date: 2021-12-17*

### Enhancement

- Rule engine supports Ali Lindorm database now
- Support the configuration of whether to continue to deliver empty retained messages to suit users who are still using the MQTT v3.1 protocol
- Optimize the use and interaction of the built-in access control file module

### Bug fixes

- Fix the issue of incorrect calculation of memory usage
- Fix the issue that the Path option of Webhook Action in rule engine doesn't support the use of ${Variable}
- Fix the issue of garbled data when writing data to RocketMQ asynchronously
- Fix the issue of inaccurate statistics of RocketMQ
- Fix the issue that the connection failure log will continue to be printed when updating or deleting MQTT Bridge and MQTT Subscribe resources in some cases
- Fix the issue that the rule engine may have a higher failure rate when saving data to MySQL
- Fix the issue that the Clickhouse-based offline messaging feature of the rule engine is unavailable
- Fix the issue that the Max Returned Count option in the MongoDB-based offline message feature of the rule engine cannot be used
- Fix the issue of partial hot configuration failure

## e4.3.5

*Release Date: 2021-11-05*

### Enhancement

- Improve client kick (forced step-down)
- Add support for new cipher suites for LwM2M gateway
- Introduced interleaving for priority queues (to avoid low priority queue stavation)
- HTTP authentication plugin disable superuser requests by default
- Improve InfluxDB write performance
- InfluxDB Tag Name and Field Name support the use of placeholders

### Important fixes

- Fix the issue that calls between clusters may cause the client process to lose response
- WebHook's HTTP client SSL configuration parse
- MongoDB resources allow host names
- Performance improvement for built-in database ACL (emqx_auth_mnesia)
- Fix the issue that the authentication based on the built-in database incorrectly transcodes the HTTP request parameters
- Fix the issue that the MySQL authentication module may fail to start
- Fix some issues of STOMP gateway

### Minor fixes

- Fix the wrong status of Kafka and MongoDB resources of the rule engine
- Fixed the issue that the Client ID containing "\" characters could not be searched in a fuzzy manner
- Fix the issue that variable byte integers may be larger than 4 bytes
- Fix the issue that duplicate modules may be added
- Fix the issue that the Listener cannot be restarted on the Dashboard

## e4.3.4

*Release Date: 2021-09-18*

### Enhancement

- Rule engine Data bridge to pursar supports data compression
- Extend the timeout interval for creating rules

### Bug fixes

- Fix rule engine data saved to InfluxDB performance issues
- Fixes an issue where WebHook inability to configure sni caused HTTPS to not be available in some cases
- Fix an issue where resources cannot be released after the rule is shut down by the rule engine
- Fixes an issue where the rule engine offline messages cannot be deleted after receiving them in some cases


## e4.3.3

*Release Date: 2021-08-16*

### Enhancement

- Save offline messages to Redis to support clearing residual data
- ExHook to add automatic reconnection mechanism, timeout parameters and alternative actions

### Bug fixes

- Fix the rule engine can't connect to InfluxDB using HTTPS
- Fix the rule engine data saving to InfluxDB action cannot use placeholder
- Fix the rule engine data bridging to WebServer can not use Path
- Repair grpc-client timeout processing logic.
- Fix ExProto bug, add retry logic, reduce some unnecessary printing


## e4.3.2

*Release Date: 2021-07-17*

### Enhancement

- The client adds more fields to the on/offline message

### Bug fixes

- Fix an issue where the LwM2M Gateway Management page could not be opened
- Fix an issue where custom fields cannot be resolved in a resolution location escalation for the JT/T808 gateway
- Fix an acl.conf file format error that invalidated the ACL rule
- Fix an issue where creating a auth_ldap authentication module failed
- Fix an issue that cannot be stopped in the case of a multilingual protocol resolution exception
- Fix an issue where the Rule Engine was unable to create Oracle resources
- Fix an issue where the Rule Engine failed to synchronize bulk writes to SQL Server

## e4.3.1

*Release Date: 2021-06-05*

### Enhancement

- New Rule Engine Description of the TimeRange field in Offline Message Save

### Bug fixes

- Rule engine Problems where data cannot be written in the case of An OpenTSDB exception
- An issue with the wrong display in the hotly upgraded version
- MQTT-SN protocol 'cleansession'false' client loses the topicid when recovering the session
- There is a problem with the preset module modifying the configuration after the restart
- Dashboard rule engine editing shows an error issue
- Dashboard navigation breadcrumbs show problems


## e4.3.0

*Release Date: 2021-05-19*

### Enhancement

- Rule engine supports Kafka to add partitions
- Rule engine supports offline message and auto-subscription using ClickHouse Storage
- The batch and async mode is enabled by default for the actions of the rule engine, if the actions support batch and asnyc
- Refactoring and improving the performance of data-to-InfluxDB
- Using Kafka to send MQTTmessage to support the set payload format

### Bug fixes

- The rule engine will make mistakes when editing actions
- Fix i18n translate of module in Dashboard
- The rule engine supports writing `null` to the database
