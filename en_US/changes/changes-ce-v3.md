# Version 0.1 to 3.x

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
