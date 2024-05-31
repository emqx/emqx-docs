# 版本发布 (Changes)

## 2.3.11 版本

_发布日期: 2018-07-23_

### Bugfix and Enhancements

Fix the getting config REST API which throws exceptions.

Support to restart listeners when emqttd is running.

Specify a fixed tag for the dependency libraries.

### emq-auth-jwt

Fix token verification with jwerl 1.0.0

### emq-auth-mongo

Support $all variable in ACL query. (emq-auth-mongo#123)

Support both clientid and username variables in all queries. (emq-auth-mongo#123)

## 2.3.10 版本

_发布日期: 2018-06-27_

### Bugfix and Enhancements

Upgrade the esockd library to v5.2.2

### emq-auth-http

Ignore auth on ignore in body, allows for chaining methods

## 2.3.9 版本

_发布日期: 2018-05-20_

### Bugfix and Enhancements

Bugfix: check params for REST publish API (#1599)

Upgrade the mongodb library to v3.0.5

### esockd

Bugfix: proxy protocol - set socket to binary mode (#78)

## 2.3.8 版本

_发布日期: 2018-05-11_

### Bugfix and Enhancements

Bugfix: unregister users CLI when unload emq_auth_username (#1588)

Bugfix: Should be an info level when change CleanSession (#1590)

Bugfix: emqttd_ctl crashed when emq_auth_usename doesn't exist (#1588)

### emq-auth-mongo

Improve: Support authentication database (authSource) (#116)

## 2.3.7 版本

_发布日期: 2018-04-22_

### Bugfix and Enhancements

Bugfix: fixed spec of function setstats/3 (#1575)

Bugfix: clean dead persistent session on connect (#1575)

Bugfix: dup flag not set when re-deliver (#1575)

Bugfix: Upgrade the lager_console_backend config (#1575)

Improve: Support set k8s namespace (#1575)

Upgrade the ekka library to v0.2.3 (#1575)

Improve: move PIPE_DIR dir from /tmp/\${WHOAMI}\_erl_pipes/\$NAME/ to /\$RUNNER_DATA_DIR/\${WHOAMI}\_erl_pipes/$NAME/ (emq-relx#188)

### emq-auth-http

Improve: Retry 3 times when httpc:request occurred socket_closed_remotely error (emq-auth-http#70)

## 2.3.6 版本

_发布日期: 2018-03-25_

### Bugfix and Enhancements

Security: LWT message checking the ACL (#1524)

Bugfix: Retain msgs should not be sent to existing subscriptions (#1529)

### emq-auth-jwt

Validate JWT token using a expired field (#29)

## 2.3.5 版本

_发布日期: 2018-03-03_

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

## 2.3.4 版本

_发布日期: 2018-01-29_

### Bugfix and Enhancements

Feature: Forward real client IP using a reverse proxy for websocket (#1335)

Feature: EMQ node.name with link local ipv6 address not responding to ping (#1460)

Feature: Add PROTO_DIST_ARG flag to support clustering via IPv6 address. (#1460)

Bugfix: retain bit is not set when publishing to clients (when it should be set). (#1461)

Bugfix: Can't search topic on web dashboard (#1473)

### emq-sn

Bugfix: CONNACK is not always sent to the client (emq-sn#67)

Bugfix: Setting the port to ::1:2000 causes error (emq-sn#66)

## 2.3.3 版本

_发布日期: 2018-01-08_

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

## 2.3.2 版本

_发布日期: 2017-12-26_

### Bugfix and Enhancements

Support X.509 certificate based authentication (#1388)

Add proxy_protocol, proxy_protocol_timeout options for ws/wss listener.

Cluster discovery etcd nodes key must be created manually. (#1402)

Will read an incorrect password at the last line of emq_auth_username.conf (#1372)

How can I use SSL/TLS certificate based client authentication? (#794)

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

## 2.3.1 版本

_发布日期: 2017-12-03_

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

## 2.3.0 版本 "Passenger's Log"

_发布日期: 2017-11-20_

EMQ 2.3.0 版本正式发布，改进了 PubSub 设计与消息路由性能，更新 EMQ 自带的自签名 SSL 证书，改进 Dashboard 界面与 API 设计。

### Bugfix and Enhancements

Fix the issue that Retained message is not sent for Subscribe to existing topic. (emqttd#1314)

Fix the issue that The DUP flag MUST be set to 0 for all QoS0 messages.(emqttd#1319)

Improve the pubsub design and fix the race-condition issue. (emqttd#PR1342)

Crash on macOS High Sierra (emqttd#1297)

### emq-dashboard Plugin (emq-dashboard#PR174)

Upgraded the 'subscriptions' RESTful API.

Improvement of the auth failure log. (emq-dashboard#59)

### emq-coap Plugin (emq-coap#PR61)

Replaced coap_client with er_coap_client.

Fix: correct the output format of coap_discover() to enable ".well-known/core".

Refactor the coap_discover method.

### emq-relx

Upgraded the bin/nodetool script to fix the rpcterms command.

### emq-web-hook Plugin

Fix the emq_web_hook plugin getting username from client.connected hook. (emq-web-hook#19)

### emq-auth-jwt Plugin(emq-auth-jwt#PR15)

Added test cases for emq_auth_jwt.

Fix jwt:decode/2 functions's return type.

### emq-auth-mongo Plugin(emq-auth-mongo#PR92)

Updated the default MongoDB server configuration.

## 2.3-rc.2 版本

_发布日期: 2017-10-22_

#### Bugfix

Change the default logging level of trace CLI. (emqttd#1306)

### emq-dashboard Plugin (emq-dashboard#164)

Fix the 'Status' filters of plugins's management.

Fix the URL Redirection when deleting an user.

Compatible with IE,Safari,360 Browsers.

## 2.3-rc.1 版本

_发布日期: 2017-10-12_

#### Bugfix

Fix the issue that invalid clients can publish will message. (emqttd#1230)

Fix Dashboard showing no stats data (emqttd#1263)

Fix a rare occurred building failure (emqttd#1284)

Support Persistence Logs for longer time (emqttd#1275)

Fix for users APIs (emqttd#1289)

Changed passwd_hash/2 function's return type (emqttd#1289)

### emq-dashboard Plugin (emq-dashboard#154)

Improved the Dashboard Interface of Monitoring/Management/Tools.

Allow switching dashboard themes.

Supoort both EN and CN languages.

## 2.3-beta.4 版本

_发布日期: 2017-09-13_

### Highlights

Released a new sexy dashboard.

Add more RESTful APIs for manangement and monitoring.

Configuring the broker through CLI or API without having to restart.

### Bugfix

Job for emqttd.service failed because the control process exited with error code. (emqttd#1238)

Travis-CI Build Failing (emqttd#1221)

Https listener of Dashboard plugin won't work (emqttd#1220)

Service not starting on Debian 8 Jessie (emqttd#1228)

### emq-dashboard

1. Support switching to other clustered node.
2. Configure and reboot the plugins on the dashboard.
3. A login page to replace the basic authentication popup window.

### emq-coap

1.Try to clarify the relationship between coap and mqtt in EMQ. (emq-coap#54).

2.Fix crashes in coap concurrent test(gen-coap#3).

## 2.3-beta.3 版本

_发布日期: 2017-08-21_

## 2.3-beta.3 版本

_发布日期: 2017-08-21_

### Enhancements

Add HTTP API for hot configuration.

### Bugfix

1. Parse 'auth.mysql.password_hash' error when hot configuration reload (emq-auth-mysql#68)
2. Set 'auth.pgsql.server' error when hot configuration reload (emq-auth-pgsql#67)
3. Set 'auth.redis.server' and 'auth.redis.password_hash' error when hot configuration reload (emq-auth-redis#47)
4. Fix the issue that when deleting retained message subscribed clients are not notified (emqttd#1207)
5. Support more parameters for hot configuration reload:

- mqtt.websocket_protocol_header = on
- mqtt.mqueue.low_watermark = 20%
- mqtt.mqueue.high_watermark = 60%
- mqtt.client.idle_timeout = 30s
- mqtt.client.enable_stats = off

## 2.3-beta.2 版本

_发布日期: 2017-08-12_

EMQ R2.3-beta.2 版本发布！该版本新增 HTTP 管理 API，支持配置 Keepalive 检测周期，支持配置参数热更新。

目前支持配置热更新的插件有:

- emq-stomp
- emq-coap
- emq-sn
- emq-lwm2m
- emq-retainer
- emq-recon
- emq-web-hook
- emq-auth-jwt
- emq-auth-http
- emq-auth-mongo
- emq-auth-mysql
- emq-auth-pgsql
- emq-auth-redis

::: tip
为支持命令行更新配置参数，部分认证插件参数值采用','替代了空格分隔符。
:::

### Enhancements

1. Introduce new HTTP management API.
2. Add ClientId parameter for HTTP Publish API.
3. Allow configuring keepalive backoff.
4. Remove the fullsweep_after option to lower CPU usage.
5. Authorize HTTP Publish API with clientId.

### emq-sn Plugin (emq-sn#49)

1. Support CONNECT message in connected/wait_for_will_topic/wait_for_will_msg states.
2. Clean registered topic for a restarted client.
3. Bug fix of not clearing buffered PUBLISH messages received during asleep state as those messages are sent to client when client wakes up.

### emq-auth-ldap Plugin (emq-auth-ldap#21)

Improve the design LDAP authentication.

### emq-coap Plugin (emq-coap#51)

Support CoAP PubSub Specification ( [ https://www.ietf.org/id/draft-ietf-core-coap-pubsub-02.txt ](https://www.ietf.org/id/draft-ietf-core-coap-pubsub-02.txt) )

## 2.3-beta.1 版本

_发布日期: 2017-07-24_

EMQ R2.3-beta.1 版本发布！该版本正式支持集群节点自动发现与集群脑裂自动愈合，支持基于 IP Multicast、Etcd、Kubernetes 等多种策略自动构建集群。

### 节点发现与自动集群

EMQ R2.3 版本支持多种策略的节点自动发现与集群:

| 策略   | 说明                    |
| ------ | ----------------------- |
| static | 静态节点列表自动集群    |
| mcast  | UDP 组播方式自动集群    |
| dns    | DNS A 记录自动集群      |
| etcd   | 通过 etcd 自动集群      |
| k8s    | Kubernetes 服务自动集群 |

### 集群脑裂与自动愈合

EMQ R2.3 版本正式支持集群脑裂自动愈合(Network Partition Autoheal):

    cluster.autoheal = on

集群脑裂自动恢复流程:

1. 节点收到 Mnesia 库的'inconsistent_database'事件 3 秒后进行集群脑裂确认；
2. 节点确认集群脑裂发生后，向 Leader 节点(集群中最早启动节点)上报脑裂消息；
3. Leader 节点延迟一段时间后，在全部节点在线状态下创建脑裂视图(SplitView)；
4. Leader 节点在多数派(Majority)分区选择集群自愈的 Coordinator 节点；
5. Coordinator 节点重启少数派(minority)分区节点恢复集群。

### 节点宕机与自动清除

EMQ R2.3 版本支持从集群自动删除宕机节点(Autoclean):

    cluster.autoclean = 5m

### LWM2M 协议支持

EMQ R2.3 版本正式支持 LWM2M 协议网关，实现了 LWM2M 协议的大部分功能。MQTT 客户端可以通过 EMQ-LWM2M 访问支持 LWM2M 的设备。设备也可以往 EMQ-LWM2M 上报 notification，为 EMQ 后端的服务采集数据。

LWM2M 是由 Open Mobile Alliance(OMA)定义的一套适用于物联网的协议，它提供了设备管理和通讯的功能。LWM2M 使用 CoAP 作为底层的传输协议，承载在 UDP 或者 SMS 上

### JSON Web Token 认证支持

EMQ R2.3 版本支持基于 JWT(JSON Web Token)的 MQTT 客户端认证。

### Retainer 插件

Retainer 插件支持'disc_only'模式存储 retained 消息。

### Debian 9 安装包

EMQ R2.3 支持 Debian 9 系统安装包。

### Erlang/OTP R20

EMQ R2.3 版本兼容 Erlang/OTP R20，全部程序包基于 Erlang/OTP R20 构建。

## 2.2 正式版 "Nostalgia"

_发布日期: 2017-07-08_

_版本别名: Nostalgia_

EMQ-2.2.0 版本正式发布！EMQ R2.2 版本完整支持 CoAP(RFC 7252)、MQTT-SN 协议，支持 Web Hook、Lua Hook、Proxy Protocol V2，支持 Elixir 语言插件开发。

Feature: Add 'listeners restart/stop' CLI command (emqttd#1135)

Bugfix: Exit Code from emqttd_ctl (emqttd#1133)

Bugfix: Fix spec errors found by dialyzer (emqttd#1136)

Bugfix: Catch exceptions thrown from rpc:call/4 (emq-dashboard#128)

Bugfix: Topic has been decoded by gen-coap, no conversion needed (emq-coap#43)

## 2.2-rc.2 版本

_发布日期: 2017-07-03_

::: warning
2.2-rc.2 版本源码编译需要 Erlang/OTP R19.3+
:::

### 问题与改进

Compatible with Erlang/OTP R20 (emq-relx#77)

CoAP gateway plugin supports coap-style publish & subscribe pattern. (emq_coap#33)

MQTT-SN gateway plugin supports sleeping device (emq_sn#32)

Upgrade esockd and mochiweb libraries to support restarting a listener

## 2.2-rc.1 版本

_发布日期: 2017-06-14_

### 问题与改进

Add a new listener for HTTP REST API (emqttd#1094)

Fix the race condition issue caused by unregister_session/1 (emqttd#1096)

Fix the issue that we cannot remove a down node from the cluster (emqttd#1100)

Passed org.eclipse.paho.mqtt_sn.testing/interoperability tests (emq_sn#29)

Fix the issue that send http request and return non-200 status code, but AUTH/ACL result is denied (emq-auth-http#33)

Fix the issue that fail to stop listener (emq_stomp#24)

Support using systemctl to manage emqttd service on CentOS

## 2.2-beta.3 版本

_发布日期: 2017-05-27_

### 问题与改进

Call emit_stats when force GC (emqttd#1071)

Update the default value of 'mqtt.mqueue.max_length' to 1000 (emqttd#1074)

Update emq-auth-mongo READEME (emq-auth-mongo#66)

Update default password field (emq-auth-mongo#67)

Upgrade the mongodb library to v3.0.3

Remove ‘check password===undefined && userName!== undefined’ (emq-dashboard#120)

### emq_auth_redis 插件

认证支持 HGET 查询

### emq_auth_mongo 插件

支持 mongodb 集群、Replica Set

### 文档更新

更新 Windows 源码编译安装

## 2.2-beta.2 版本

_发布日期: 2017-05-20_

### 问题与改进

Add a 'websocket_protocol_header' option to handle WebSocket connection from WeChat (emqttd#1060)

Assign username and password to MQTT-SN's CONNECT message (emqttd#1041)

Allow for Content-Type:application/json in HTTP Publish API (emqttd#1045)

emqttd_http.erl:data conversion (emqttd#1059)

Seperate emq_sn from emqttd (emq-sn#24)

Check St0's type, making it easier to debug crash problems (emq-lua-hook#6)

Fix error: load xxx.lua (emq-lua-hook#8)

Leave luerl alone as a rebar project (emq-lue-hook#9)

Display websocket data in reverse order (emq-dashboard#118)

priv/www/assets/js/dashboard.js:Fixed a typo (emq-dashboard#118)

### Update README

Update README of emq-auth-pgsql: add the 'ssl_opts' configuration (emq-auth-pgsql#56)

Update README of emq-auth-mysql: fix the 'passwd_hash' typo (emq-auth-mysql#54)

Update README of emq-auth-mongo: change 'aclquery' to 'acl_query' (emq-auth-mongo#63)

### Elixir Plugin

Add a new plugin [ emq-elixir-plugin ](https://github.com/emqtt/emq-elixir-plugin) to support Elixir language.

## 2.2-beta.1 版本

_发布日期: 2017-05-05_

EMQ 2.2-beta.1 版本正式发布！EMQ2.2 版本发布主要新功能包括:

1. 支持 MQTT 协议多监听器配置，支持 HAProxy 的 Proxy Protocol V1/V2
2. 新增 Web Hook 插件(emq-web-hook)、Lua Hook 插件(emq-lua-hook)

### MQTT 协议监听器配置

一个 EMQ 节点可配置多个 MQTT 协议监听端口，例如下述配置 external, internal 监听器，分别用于设备连接与内部通信:

                             -------
    -- Ex，支持Web Hook、Lua Hook、ernal TCP 1883 --> |     |
                             | EMQ | -- Internal TCP 2883 --> Service
    -- External SSL 8883-->  |     |
                             -------

EMQ 2.2 版本 etc/emq.conf 监听器配置方式:

    listener.tcp.${name}= 127.0.0.1:2883

    listener.tcp.${name}.acceptors = 16

    listener.tcp.${name}.max_clients = 102400

### Proxy Protocol V1/2 支持

EMQ 集群通常部署在负载均衡器(LB)后面，典型架构:

                  -----
                  |   |
                  | L | --TCP 1883--> EMQ
    --SSL 8883--> |   |                |
                  | B | --TCP 1883--> EMQ
                  |   |
                  -----

HAProxy、NGINX 等常用的负载均衡器(LB)，一般通过 Proxy Protocol 协议传递 TCP 连接源地址、源端口给 EMQ。

EMQ 2.2 版本的监听器开启 Proxy Protocol 支持:

    ## Proxy Protocol V1/2
    ## listener.tcp.${name}.proxy_protocol = on
    ## listener.tcp.${name}.proxy_protocol_timeout = 3s

### Web Hook 插件

新增 WebHook 插件: [ emq-web-hook ](https://github.com/emqtt/emq-web-hook) ，支持在 MQTT 客户端上下线、消息发布订阅时触发 WebHook 回调。

### Lua Hook 插件

新增 Lua Hook 插件: [ emq-lua-hook ](https://github.com/emqtt/emq-lua-hook) ，支持 Lua 脚本注册 EMQ 扩展钩子来开发插件。

### 改进认证链设计

EMQ 2.2 版本改进认证链设计，当前认证模块返回 ignore(例如用户名不存在等情况下)，认证请求将继续转发后面认证模块:

               -------------           ------------           -------------
    Client --> | Redis认证 | -ignore-> | HTTP认证 | -ignore-> | MySQL认证 |
               -------------           ------------           -------------
                     |                       |                       |
                    \|/                     \|/                     \|/
               allow | deny            allow | deny            allow | deny

### 支持 bcrypt 密码 Hash

EMQ 2.2 版本支持 bcrypt 密码 Hash 方式，例如 Redis 认证插件配置:

    auth.redis.password_hash = bcrypt

### etc/emq.conf 配置变更

'mqtt.queue. _' 配置变更为 'mqtt.mqueue._ '

### emq-dashboard

WebSocket 页面支持 Unsubscribe

## 2.1.2 版本

_发布日期: 2017-04-21_

Fix emqttd_ctl sessions list CLI

Newline character in emq.conf causing error;(emqttd#1000)

Fix crash caused by duplicated PUBREC packet (emqttd#1004)

Unload the 'session.created' and 'session.teminated' hooks (emq-plugin-template)

## 2.1.1 版本

_发布日期: 2017-04-14_

Localhost:8083/status returns 404 when AWS LB check the health of EMQ (emqttd#984)

Https listener not working in 2.1.0 as in 2.0.7 (emq-dashboard#105)

Fix mqtt-sn Gateway not working (emq-sn#12)

Upgrade emq-sn Plugin (emq-sn#11)

Upgrade emq-coap Plugin (emq-coap#21)

## 2.1.0 版本

_发布日期: 2017-04-07_

The stable release of 2.1 version.

Trouble with auth.mysql.acl_query (emq-auth-mysql#38)

Filter the empty fields in ACL table (emq-auth-mysql#39)

## 2.1.0-rc.2 版本

_发布日期: 2017-03-31_

Support pbkdf2 hash (emq-auth-mongo#46)

Kickout the conflict WebSocket connection (emqttd#963)

Correct licence in app.src (emqttd#958)

SSL options to connect to pgsql (emq-auth-pgsql#41)

## 2.1.0-rc.1 版本

_发布日期: 2017-03-24_

EMQ fails to start if run under a different linux user than that which first ran it (emqttd#842)

Depend on emqtt/pbkdf2 to fix the building errors of Travis CI (emqttd#957)

Depend on goldrush and emqtt/pbkdf2 to resolve the building errors (emqttd#956)

Fix 'rebar command not found' (emq-relx#33)

Compile error in v2.1.0-beta.2 (emq-relx#32)

Support salt with passwords (emq-auth-mongo#11)

Change the default storage_type to 'ram' (emq-retainer#13)

## 2.1.0-beta.2 版本

_发布日期: 2017-03-13_

Cannot find AwaitingAck (emqttd#597)

EMQ V2.1 crash when public with QoS = 2 (emqttd#919)

Support pbkdf2 hash (emqttd#940)

Add src/emqttd.app.src to be compatible with rebar3 (emqttd#920)

Add more test cases (emqttd#944)

CRASH REPORT Process \<0.1498.0> with 0 neighbours crashed with reason: {ssl_error,{tls_alert,"certificate unknown"}} in esockd_connection:upgrade (emqttd#915)

'auth.redis.password_hash = plain' by default (emq-auth-redis#20)

## 2.1.0-beta.1 版本

_发布日期: 2017-02-24_

EMQ 2.1.0-beta.1 版本发布。

::: warning
2.1.x 版本源码编译需要 Erlang/OTP R19+
:::

EMQ 正式采用 [ Semantic Versioning 2.0.0 ](http://semver.org) 规范创建发布版本号，按'Tick-Tock'方式按月发布迭代版本。奇数版本问题修复与性能改进，偶数版本架构改进和新功能布。

### GC 优化

1. WebSocket、Client、Session 进程空置一段时间后自动 Hibernate 与 GC。
2. 新增'mqtt.conn.force_gc_count'配置，Client、Session 进程处理一定数量消息后强制 GC。
3. 大幅降低 WebSocket、Client、Session 进程 fullsweep_after 设置，强制进程深度 GC。

### API 改进

Hooks API 支持注册带 Tag 的回调函数，解决相同模块函数多次 Hook 注册问题。

### 问题修复

emqttd#916: Add 'mqtt_msg_from()' type

emq-auth-http#15: ACL endpoint isnt called

## 2.1-beta 版本

_发布日期: 2017-02-18_

EMQ v2.1-beta 版本正式发布，改进 Session/Inflight 窗口设计，一个定时器负责全部 Inflight QoS1/2 消息重传，大幅降低高消息吞吐情况下的 CPU 占用。

### Client, Session 统计信息

支持对单个 Client、Session 进程进行统计，etc/emq.conf 配置文件中设置'enable_stats'开启:

    mqtt.client.enable_stats = 60s

    mqtt.session.enable_stats = 60s

### 新增 missed 统计指标

EMQ 收到客户端 PUBACK、PUBREC、PUBREL、PUBCOMP 报文，但在 Inflight 窗口无法找到对应消息时，计入 missed 统计指标:

    packets/puback/missed

    packets/pubrec/missed

    packets/pubrel/missed

    packets/pubcomp/missed

### Syslog 日志集成

支持输出 EMQ 日志到 Syslog，etc/emq.config 配置项:

    ## Syslog. Enum: on, off
    log.syslog = on

    ##  syslog level. Enum: debug, info, notice, warning, error, critical, alert, emergency
    log.syslog.level = error

### Tune QoS 支持

支持订阅端升级 QoS，etc/emq.conf 配置项:

    mqtt.session.upgrade_qos = on

### 'acl reload'管理命令

Reload acl.conf without restarting emqttd service (#885)

### 配置项变更

1. 变更 mqtt.client_idle_timeout 为 mqtt.client.idle_timeout
2. 新增 mqtt.client.enable_stats 配置项
3. 新增 mqtt.session.upgrade_qos 配置项
4. 删除 mqtt.session.collect_interval 配置项
5. 新增 mqtt.session.enable_stats 配置项
6. 变更 mqtt.session.expired_after 为 mqtt.session.expiry_interval

### 合并扩展模块到 emq_modules 项目

合并 emq_mod_presence, emq_mod_subscription, emq_mod_rewrite 到 emq_modules 项目

变更 emq_mod_retainer 为 emq_retainer 项目

### Dashboard 插件

Overview 页面增加 missed 相关统计指标。 Client 页面增加 SendMsg、RecvMsg 统计指标。 Session 页面增加 DeliverMsg、EnqueueMsg 指标。

### recon 插件

变更 recon.gc_interval 配置项类型为 duration

### reloader 插件

变更 reloader.interval 配置项类型为 duration

## 2.0.7 版本

_发布日期: 2017-01-20_

The Last Maintenance Release for EMQ 2.0, and support to build RPM/DEB Packages.

emq-auth-http#9: Update the priv/emq_auth_http.schema, cuttlefish:unset() if no super_req/acl_req config exists

emq-auth-mongo#31: cuttlefish:unset() if no ACL/super config exists

emq-dashboard#91: Fix the exception caused by binary payload

emq-relx#21: Improve the binemqttd.cmd batch script for windows

emqttd#873: Documentation: installing-from-source

emqttd#870: Documentation: The word in Documents is wrong

emqttd#864: Hook 'client.unsubscribe' need to handle 'stop'

emqttd#856: Support variables in etc/emq.conf: {{ runner_etc_dir }}, {{ runner_etc_dir }}, {{ runner_data_dir }}

## 2.0.6 版本

_发布日期: 2017-01-08_

Upgrade the [ esockd ](https://github.com/emqtt/esockd) library to v4.1.1

esockd#41: Fast close the TCP socket if ssl:ssl_accept failed

emq-relx#15: The EMQ 2.0 broker cannot run on Windows.

emq-auth-mongo#31: Mongodb ACL Cannot work?

## 2.0.5 版本

_发布日期: 2016-12-24_

emq-auth-http#9: Disable ACL support

emq-auth-mongo#29: Disable ACL support

emq-auth-mongo#30: {datatype, flag}

## 2.0.4 版本

_发布日期: 2016-12-16_

emqttd#822: Test cases for SSL connections

emqttd#818: trap_exit to link WebSocket process

emqttd#799: Can't publish via HTTPS

## 2.0.3 版本

_发布日期: 2016-12-12_

emqttd#796: Unable to forbidden tcp lisener

emqttd#814: Cannot remove a 'DOWN' node from the cluster

emqttd#813: Change parameters order

emqttd#795: Fix metrics of websocket connections

emq-dashboard#88: Rename the default topic from “/World” to “world”

emq-dashboard#86: Lookup all online clients

emq-dashboard#85: Comment the default listener port

emq-mod-retainer#3: Retained messages get lost after EMQTT broker restart.

## 2.0.2 版本

_发布日期: 2016-12-05_

emqttd#787: Stop plugins before the broker stopped, clean routes when a node down

emqttd#790: Unable to start emqttd service if username/password contains special characters

emq-auth-clientid#4: Improve the configuration of emq_auth_clientid.conf to resolve emqttd#790

emq-auth-username#4: Improve the configuration of emq_auth_username.conf to resolve emqttd#790

## 2.0.1 版本

_发布日期: 2016-11-30_

emqttd#781: 更新项目 README 到 2.0 版本

emq_dashboard#84: 显示节点集群状态

emq_dashboard#79: 集群节点采用 disc_copies 存储 mqtt_admin 表

emq_auth_clientid: 集群节点采用 disc_copies 存储 mqtt_auth_clientid 表

emq_auth_username: 集群节点采用 disc_copies 存储 mqtt_auth_username 表

emq_mod_subscription#3: 删除 emq_mod_subscription 表与 module.subscription.backend 配置

emq_plugin_template#5: 插件停止时注销认证/ACL 模块

## 2.0 正式版 "西湖以西"

_发布日期: 2016-11-24_

_版本别名: 西湖以西(West of West Lake)_

EMQ-2.0 版本正式发布！EMQ-1.0 版本产品环境下已支持 900K 并发连接，EMQ-2.0 版本重构了整个项目架构并正式支持共享订阅功能:

1. 支持共享订阅(Shared Subscription)与本地订阅(Local Subscription)，解决 MQTT 协议负载平衡消费问题；
2. 支持 CoAP(RFC 7252)、MQTT-SN 协议和网关，支持 CoAP、MQTT-SN 客户端与 MQTT 客户端互通；
3. 重构配置文件格式与加载方式，支持用户友好的'K = V'文件格式，支持操作系统环境变量；
4. 增加了扩展钩子和大量的认证插件，支持与大部分数据库或 NoSQL 的认证集成；
5. 支持全平台编译部署，Linux/Unix/Windows 以及 ARM 平台网关，支持 Docker 镜像制作。

### 共享订阅(Shared Subscription)

共享订阅(Shared Subscription)支持在多订阅者间采用分组负载平衡方式派发消息:

                                ---------
                                |       | --Msg1--> Subscriber1
    Publisher--Msg1,Msg2,Msg3-->|  EMQ  | --Msg2--> Subscriber2
                                |       | --Msg3--> Subscriber3
                                ---------

使用方式: 订阅者在主题(Topic)前增加'$queue'或'$share/\<group>/'前缀。

### 本地订阅(Local Subscription)

本地订阅(Local Subscription)只在本节点创建订阅与路由表，不会在集群节点间广播全局路由，非常适合物联网数据采集应用。

使用方式: 订阅者在主题(Topic)前增加'$local/'前缀。

### erlang.mk 与 relx

2.0 版本分离 [ emqttd ](https://github.com/emqtt/emqttd) 主项目和发布项目 [ emq-relx ](https://github.com/emqtt/emq-relx) , 采用 [ erlang.mk ](https://erlang.mk) 和 [ relx ](https://github.com/erlware/relx) 编译发布工具替换 1.x 版本使用的 rebar，项目可以跨平台在 Linux/Unix/Windows 系统下编译。

### CoAP 协议支持

2.0 版本支持 CoAP 协议(RFC7252)，支持 CoAP 网关与 MQTT 客户端互通。

CoAP 插件: [ https://github.com/emqtt/emq_coap ](https://github.com/emqtt/emq_coap)

### MQTT-SN 协议支持

2.0 版本支持 MQTT-SN 协议，支持 MQTT-SN 网关与 MQTT 客户端互通。

MQTT-SN 插件: [ https://github.com/emqtt/emq_sn ](https://github.com/emqtt/emq_sn)

### 'K = V'格式配置文件

2.0 版本支持用户友好的'K = V'格式配置文件 etc/emq.conf:

    node.name = emqttd@127.0.0.1

    ...

    mqtt.listener.tcp = 1883

    ...

### 操作系统环境变量

2.0 版本支持操作系统环境变量。启动时通过环境变量设置 EMQ 节点名称、安全 Cookie 以及 TCP 端口号:

    EMQ_NODE_NAME=emqttd@127.0.0.1
    EMQ_NODE_COOKIE=emq_dist_cookie
    EMQ_MAX_PORTS=65536
    EMQ_TCP_PORT=1883
    EMQ_SSL_PORT=8883
    EMQ_HTTP_PORT=8083
    EMQ_HTTPS_PORT=8084

### Docker 镜像支持

EMQ-2.0 版本支持 Docker 镜像制作，Dockerfile 开源在: [ https://github.com/emqtt/emq_docker ](https://github.com/emqtt/emq_docker)

### Windows 平台支持

2.0 版本完整支持 Windows 平台的编译、发布与运行，支持 Windows 平台下的'emqttd_ctl'控制命令，支持在 Windows 节点间的集群。

### 问题与改进

#764: add mqtt.cache_acl option

#667: Configuring emqttd from environment variables

#722: mqtt/superuser calls two times emqtt_auth_http

#754: "-heart" option for EMQ 2.0

#741: emq_auth_redis cannot use hostname as server address

### 扩展插件

2.0 版本发布的认证与扩展插件列表:

| 插件                                                                    | 说明                       |
| ----------------------------------------------------------------------- | -------------------------- |
| [ emq_dashboard ](https://github.com/emqtt/emqttd_dashboard)            | Web 控制台插件(默认加载)   |
| [ emq_auth_clientid ](https://github.com/emqtt/emq_auth_clientid)       | ClientId 认证插件          |
| [ emq_auth_username ](https://github.com/emqtt/emq_auth_username)       | 用户名、密码认证插件       |
| [ emq_auth_ldap ](https://github.com/emqtt/emq_auth_ldap)               | LDAP 认证/访问控制         |
| [ emq_auth_http ](https://github.com/emqtt/emq_auth_http)               | HTTP 认证/访问控制         |
| [ emq_auth_mysql ](https://github.com/emqtt/emq_auth_mysql)             | MySQL 认证/访问控制        |
| [ emq_auth_pgsql ](https://github.com/emqtt/emq_auth_pgsql)             | PostgreSQL 认证/访问控制   |
| [ emq_auth_redis ](https://github.com/emqtt/emq_auth_redis)             | Redis 认证/访问控制        |
| [ emq_auth_mongo ](https://github.com/emqtt/emq_auth_mongo)             | MongoDB 认证/访问控制      |
| [ emq_mod_rewrite ](https://github.com/emqtt/emq_mod_rewrite)           | 重写主题(Topic)插件        |
| [ emq_mod_retainer ](https://github.com/emqtt/emq_mod_retainer)         | Retain 消息存储模块        |
| [ emq_mod_presence ](https://github.com/emqtt/emq_mod_presence)         | 客户端上下线状态消息发布   |
| [ emq_mod_subscription ](https://github.com/emqtt/emq_mod_subscription) | 客户端上线自动主题订阅     |
| [ emq_coap ](https://github.com/emqtt/emq_coap)                         | CoAP 协议支持              |
| [ emq_sn ](https://github.com/emqtt/emq_sn)                             | MQTT-SN 协议支持           |
| [ emq_stomp ](https://github.com/emqtt/emq_stomp)                       | Stomp 协议支持             |
| [ emq_sockjs ](https://github.com/emqtt/emq_sockjs)                     | Stomp over SockJS 协议支持 |
| [ emq_recon ](https://github.com/emqtt/emq_recon)                       | Recon 性能调试             |
| [ emq_reloader ](https://github.com/emqtt/emq_reloader)                 | Reloader 代码热加载插件    |
| [ emq_plugin_template ](https://github.com/emqtt/emq_plugin_template)   | 插件开发模版               |

## 2.0-rc.3 版本

## 2.0-rc.3 版本

_发布日期: 2016-11-01_

1. 将 Presence、Retainer、Subscription 三个扩展模块改为独立插件:

| [ emq_mod_retainer ](https://github.com/emqtt/emq_mod_retainer)         | Retain 消息存储模块      |
| ----------------------------------------------------------------------- | ------------------------ |
| [ emq_mod_presence ](https://github.com/emqtt/emq_mod_presence)         | 客户端上下线状态消息发布 |
| [ emq_mod_subscription ](https://github.com/emqtt/emq_mod_subscription) | 客户端上线自动主题订阅   |

2. 更新 EMQ 自带的自签名 SSL 证书，修复 SSL 双向认证配置文件错误
3. Bugfix: Fixed a typo (#716)
4. Bugfix: emqttd_http can not use emq_auth_http? #739
5. Bugfix: emq_auth_redis cannot use hostname as server address (#741)
6. 升级 Redis, MySQL, Postgre, MongoDB 插件，支持主机名或域名配置

## 2.0-rc.2 版本

_发布日期: 2016-10-19_

1. 集成 cuttlefish 库，支持'K = V'通用配置文件格式，重构 EMQ 与全部插件配置文件:


    node.name = emqttd@127.0.0.1

    ...

    mqtt.listener.tcp = 1883

    ...

2. 支持操作系统环境变量。启动时通过环境变量设置 EMQ 节点名称、Cookie 以及 TCP 端口号:


    EMQ_NODE_NAME
    EMQ_NODE_COOKIE
    EMQ_MAX_PORTS
    EMQ_TCP_PORT
    EMQ_SSL_PORT
    EMQ_HTTP_PORT
    EMQ_HTTPS_PORT

3. 重构认证模块、ACL 模块与扩展模块，更新全部插件项目名称以及配置文件。

## 2.0-rc.1 版本

_发布日期: 2016-10-03_

1. 超级用户认证成功后，发布订阅时不进行 ACL 鉴权 (#696)

2. MQTT 客户端认证失败后，EMQ 服务器主动关闭 TCP 连接 (#707)

3. 改进插件管理设计，新增插件无需修改 rel/sys.config 配置

4. 改进全部插件 Makefile 的 emqttd 依赖:


    BUILD_DEPS = emqttd
    dep_emqttd = git https://github.com/emqtt/emqttd emq20

5. 重新设计 Redis 插件的 ACL 鉴权模块

## 2.0-beta.3 版本

_发布日期: 2016-09-18_

### 共享订阅(Shared Subscription)

Shared Suscriptions (#639, #416):

    mosquitto_sub -t '$queue/topic'
    mosquitto_sub -t '$share/group/topic'

### 本地订阅(Local Subscription)

Local Subscriptions that will not create global routes:

    mosquitto_sub -t '$local/topic'

### 问题修复

Error on Loading emqttd_auth_http (#691)

Remove 'emqttd' application from dependencies (emqttd_coap PR#3)

## 2.0-beta.2 版本

_发布日期: 2016-09-10_

### CoAP 协议支持

CoAP 协议支持插件(Beta): [ https://github.com/emqtt/emqttd_coap ](https://github.com/emqtt/emqttd_coap)

### API Breaking Changes

'\$u', '\$c' variables in emqttd.conf and modules/acl.conf changed to '%u', '%c'

Improve the design of mqtt retained message, replace emqttd_retainer with emqttd_mod_retainer.

Add 'session.subscribed', 'session.unsubscribed' hooks, remove 'client.subscribe.after' hook

Tab 'retained_message' -> 'mqtt_retained'

### Bugfix

[2.0 beta1] FORMAT ERROR: "~s PUBLISH to ~s: ~p" (PR #671)

Fixing issues in cluster mode. (PR #681)

Fixing issues with unsubscribe hook (PR #673)

## 2.0-beta.1 版本

_发布日期: 2016-08-30_

_版本别名: 西湖以西(West of West Lake)_

EMQ 2.0-beta1 预览版本(Preview Release)发布。EMQ 2.0 版本改进了项目结构、发布方式、Git 分支结构以及配置文件格式，以奠定 EMQ 消息服务器项目长期演进基础。

::: tip
1.x 版本产品部署用户请勿升级到该版本，2.0 正式版本发布前会有 API 变更。
:::

### 项目简称 - EMQ

项目简称变更为 EMQ(Erlang/Enterprise/Elastic MQTT Broker)，E 含义 Erlang/OTP 平台、企业(Enterprise)、弹性(Elastic)。

### 项目发布方式

2.0 版本后采用预览版(Preview Release) + 候选版本(Release Candidate)版本方式迭代发布，2.0 版本将陆续发布 beta1, beta2, beta3, rc1, rc2 等迭代，直到 2.0 正式版本发布。

### 应用与发布

2.0 版本后 [ emqttd ](https://github.com/emqtt/emqttd) 项目只包括消息服务器应用源码，分离发布(rel)为独立项目: [ emqttd_relx ](https://github.com/emqtt/emqttd-relx) ，以解决 1.0 版本的插件(plugins)与 emqttd 应用编译依赖问题。

源码编译请 clone [ emqttd_relx ](https://github.com/emqtt/emqttd-relx) :

    git clone https://github.com/emqtt/emqttd-relx.git

    cd emqttd-relx && make

    cd _rel/emqttd && ./bin/emqttd console

### erlang.mk 与 relx

2.0 版本发布项目 [ emqttd_relx ](https://github.com/emqtt/emqttd-relx) 采用 [ erlang.mk ](https://erlang.mk) 和 [ relx ](https://github.com/erlware/relx) 编译发布工具替换 1.x 版本使用的 rebar。原因: [ https://erlang.mk/guide/why.html ](https://erlang.mk/guide/why.html)

### Git 分支结构

| stable     | 1.x 稳定版本分支 |
| ---------- | ---------------- |
| master     | 2.x 主版本分支   |
| emq10      | 1.x 版本开发分支 |
| emq20      | 2.x 版本开发分支 |
| emq30      | 3.x 版本开发分支 |
| issue#{id} | Issue 修复分支   |

etc/emqttd.conf 配置文件 ---------=-------------

2.0 版本改进项目配置文件格式，采用 rebar.config、relx.config 类似格式，提高配置文件的可读性和可编辑性。

etc/emqttd.conf 配置示例:

    %% Max ClientId Length Allowed.
    {mqtt_max_clientid_len, 512}.

    %% Max Packet Size Allowed, 64K by default.
    {mqtt_max_packet_size, 65536}.

    %% Client Idle Timeout.
    {mqtt_client_idle_timeout, 30}. % Second

### MQTT-SN 协议支持

2.0-beta1 版本正式发布 [ emqttd_sn ](http://github.com/emqtt/emqttd_sn) 项目支持 MQTT-SN 协议，插件加载方式启用 emqttd_sn 项目，MQTT-SN 默认 UDP 端口: 1884:

    ./bin/emqttd_ctl plugins load emqttd_sn

### 改进插件架构

2.0 版本从 emqttd 项目删除 plugins/目录，插件作为一个普通的 Erlang 应用，直接依赖(deps)方式在编译到 lib 目录，插件配置文件统一放置在 etc/plugins/目录中:

    ▾ emqttd-relx/
      ▾ etc/
        ▸ modules/
        ▾ plugins/
            emqtt_coap.conf
            emqttd.conf
            emqttd_auth_http.conf
            emqttd_auth_mongo.conf
            emqttd_auth_mysql.conf
            emqttd_auth_pgsql.conf
            emqttd_auth_redis.conf
            emqttd_coap.conf
            emqttd_dashboard.conf
            emqttd_plugin_template.conf
            emqttd_recon.conf
            emqttd_reloader.conf
            emqttd_sn.conf
            emqttd_stomp.conf

### 2.0 版本项目文档

2.0 版本中文文档: [ http://emqtt.com/docs/v2/index.html ](http://emqtt.com/docs/v2/index.html) 或 [ http://docs.emqtt.cn/zh_CN/emq20 ](http://docs.emqtt.cn/zh_CN/emq20)

2.0 版本英文文档: [ https://developer.emqx.io/docs/emq/v2/en/index.html ](https://developer.emqx.io/docs/emq/v2/en/index.html) 或 [ http://docs.emqtt.com/ ](http://docs.emqtt.com/)

### 发布订阅流程

![image](./_static/images/publish.png)

## 1.1.3 版本

_发布日期: 2016-08-19_

Support './bin/emqttd_ctl users list' CLI (#621)

Cannot publish payloads with a size of the order 64K using WebSockets (#643)

Optimize the procedures that retrieve the Broker version and Borker description in the tick timer (PR#627)

Fix SSL certfile, keyfile config (#651)

## 1.1.2 版本

_发布日期: 2016-06-30_

Upgrade mysql-otp driver to 1.2.0 (#564, #523, #586, #596)

Fix WebSocket Client Leak (PR #612)

java.io.EOFException using paho java client (#551)

Send message from paho java client to javascript client (#552)

Compatible with the Qos0 PUBREL packet (#575)

Empty clientId with non-clean session accepted (#599)

Update docs to fix typos (#601, #607)

## 1.1.1 版本

_发布日期: 2016-06-04_

Compatible with the Qos0 PUBREL packet (#575)

phpMqtt Client Compatibility (#572)

java.io.EOFException using paho java client (#551)

## 1.1 版本

_发布日期: 2016-06-01_

1.1 版本升级 eSockd 库到 4.0，支持 IPv6 与监听特定 IP 地址。新增 MongoDB 认证插件、HTTP 认证插件与 Reloader 插件。升级 MySQL、PostgreSQL、Redis 认证插件，采用参数化查询避免 SQL 注入，并支持超级用户(superuser)认证。

### 问题与改进

Allow human-friendly IP addresses (PR#395)

File operation error: emfile (#445)

emqttd_plugin_mongo not found in emqttd (#489)

emqttd_plugin_mongo Error While Loading in emqttd (#505)

Feature request: HTTP Authentication (#541)

Compatible with the Qos0 PUBREL packet (#575)

Bugfix: function_clause exception occurs when registering a duplicated authentication module (#542)

Bugfix: ./emqttd_top msg_q result: {"init terminating in do_boot",{undef,[{etop,start,[],[]},{init,start_it,1,[]},{init,start_em,1,[]}]}} (#557)

### Dashboard 插件

WebSocket 连接页面支持 Clean Session, Qos, Retained 参数设置 (emqttd_dashboard#52)

升级 eSockd 库到 4.0 版本，Overview 页面显示 OTP 版本 (emqttd_dashboard#61)

Changing dashboard credentials for username authentication (emqttd_dashboard#56)

新增'./bin/emqttd_ctl admins'管理命令，支持通过命令行重新设置 admin 密码

### HTTP 认证插件

支持通过 HTTP API 认证/鉴权 MQTT 客户端: [ https://github.com/emqtt/emqttd_auth_http ](https://github.com/emqtt/emqttd_auth_http)

### MongoDB 认证插件

升级 Erlang Mongodb 驱动到 v1.0.0 (emqttd_plugin_mongo#1)

支持超级用户认证

支持基于 MongoDB 的 ACL (emqttd_plugin_mongo#3)

### MySQL 认证插件

支持超级用户认证

采用参数化查询避免 SQL 注入

### Postgre 认证插件

支持超级用户认证

采用参数化查询避免 SQL 注入

### Redis 认证插件

支持超级用户认证

支持 ClientId 认证/ACL (emqttd_plugin_redis#4)

### Reloader 插件

开发调试代码热升级插件: [ https://github.com/emqtt/emqttd_reloader ](https://github.com/emqtt/emqttd_reloader)

## 1.0.2 版本

_发布日期: 2016-05-04_

Issue#534 - './bin/emqttd_ctl vm' - add 'port/count', 'port/limit' statistics

Issue#535 - emqttd_client should be terminated properly even if exception happened when sending data

PR#519 - The erlang '-name' requires the fully qualified host name

emqttd_reloader plugin - help reload modified modules during development.

## 1.0.1 版本

_发布日期: 2016-04-16_

PR#515 - Fix '$queue' pubsub, add 'pubsub_queue' test and update docs

## 1.0 (七英里) 版本

_发布日期: 2016-04-13_

_版本别名: 七英里(The Seven Mile Journey)_

经过两年开发，五十个版本迭代，我们正式发布 1.0(七英里)版本，和完整的中英文项目文档。

1.0 版本基本实现了设计目标: 稳定承载来自移动互联网或物联网终端的大量并发 MQTT 连接，并实现在大数量的终端间快速低延时的 MQTT 消息路由。

1. 完整支持 MQTT V3.1.1 协议，扩展支持 WebSocket、Stomp 或私有 TCP 等多协议。
2. 稳定承载大规模的并发 MQTT 客户端连接，单服务器节点支持 50 万到 100 万连接。
3. 分布式节点集群或桥接，快速低延时的消息路由，单集群支持 1000 万规模的路由。
4. 支持消息服务器内扩展，支持定制多种认证方式，插件方式存储消息到后端数据库。

### 问题与改进

1.0 版本主要发布完整项目文档，相比 0.17.1 版本很少代码变更:

Possible race condition using emqttd_cm (#486)

Improve the design of retained message expiration (#503)

Should not expire the retained messages from \$SYS/# topics (#500)

### 项目文档

1.0 版本中文文档: [ http://emqtt.com/docs/ ](http://emqtt.com/docs/) 或 [ http://docs.emqtt.cn ](http://docs.emqtt.cn)

1.0 版本英文文档: [ https://developer.emqx.io/docs/emq/v1/en/index.html ](https://developer.emqx.io/docs/emq/v1/en/index.html) 或 [ http://docs.emqtt.com/ ](http://docs.emqtt.com/)

### 官方站点

中文站点: [ http://emqtt.com ](http://emqtt.com)

英文站点: [ https://www.emqx.io/ ](https://www.emqx.io/)

### 致谢

爱立信与 Erlang/OTP 语言平台团队( [ http://www.erlang.org/ ](http://www.erlang.org/) )!

贡献者(GitHub 帐户): @callbay @lsxredrain @hejin1026 @desoulter @turtleDeng @Hades32 @huangdan @phanimahesh @dvliman @Prots @joaohf

公司: 开源中国，鲁能电力，太极计算机，电信天翼云直播，研色科技，杭州华思

乐队: 七英里(The Seven Mile Journey)，腰乐队，万能青年旅店

## 0.17.1-beta 版本

_发布日期: 2016-03-22_

### Enhancements

Time unit of session 'expired_after' changed to minute. (#479)

### Dashboard

Code Review and improve the design of Dashboard.

## 0.17.0-beta 版本

_发布日期: 2016-03-15_

### Highlights

Installation and Configuration Guide released on [ http://docs.emqtt.com ](http://docs.emqtt.com)

Improve and Consolidate the design of Hook, Server, PubSub and Router

Upgrade the [Web Dashboard](https://github.com/emqtt/emqttd_dashboard) to support pagination

Bridge emqttd broker to another emqttd broker & emqttd to mosquitto bridge (#438)

### Enhancements

emqttd_ctl: better error message (#450)

./bin/emqttd_ctl: add 'routes' command

` ` routes list # List all routes routes show \<Topic> # Show a route ` `

Add 'backend_subscription' table and support static subscriptions (emqttd_backend)

Add 'retained_message' table and refactor emqttd_retainer module (emqttd_backend)

A New Hook and Callback Design (emqttd_hook)

Add PubSub, Hooks APIs to emqttd module (emqttd)

Move start_listeners/0, stop_listeners/0 APIs to emqttd_app module (emqttd_app)

### Tests

Add 100+ common test cases.

### Plugins

Upgrade Dashboard, Redis, Stomp and Template Plugins

## 0.16.0-beta 版本

_发布日期: 2016-02-16_

### Highlights

Licensed under the Apache License, Version 2.0 Now.

Improve the design of cluster, support to join or leave the cluster (#449):

` ` $ ./bin/emqttd_ctl cluster cluster join \<Node> #Join the cluster cluster leave #Leave the cluster cluster remove \<Node> #Remove the node from cluster cluster status #Cluster status ` `

Improve the design of Trie and Route, only the wildcard topics stored in Trie.

Common Test to replace EUnit.

### Enhancements

mqtt_message record: add 'sender' field (#440)

refactor the emqttd, emqttd_time, emqttd_opts, emqttd_node modules.

### Bugfix

noproc error when call to gen_server2:call(false, {add_route,Topic,\<0.685.0>}, infinity) (#446)

#### Plugins

Changed the license of all plugins.

## 0.15.0-beta 版本

_发布日期: 2016-01-31_

### Highlights

Optimize for Push Application, 500K+ Subscribers to a Topic.

Optimization for Route ETS insertion (#427)

Priority Message Queue for Persistent Session (#432)

Add Redis, MongoDB Plugins (#417)

### Enhancements

Username/Password Authentication: Support to configure default users (#428)

Improve CLI Commands: pubsub, bridges, trace (#429)

emqttd_mod_subscription: fix client_connected/3

emqttd_auth_mod: add passwd_hash/2 function

priority_queue: add plen/2, out/2 functions

### Bugfix

Fix dequeue/1 of emqttd_bridge...

Add emqttd:seed_now/0 function

### Plugins

emqttd_plubin_mysql: Changed mysql driver to mysql-otp

emqttd_plugin_pgsql: Integrate with ecpool

emqttd_plugin_redis: First release

emqttd_plugin_mongo: First release

## 0.14.1-beta 版本

_发布日期: 2015-12-28_

Bugfix: emqttd_ws_client.erl: Unexpected Info: {'EXIT',\<0.27792.18>,{shutdown,destroy}} (#413)

Improve: fix spec errors found by dialyzer

## 0.14.0-beta 版本

_发布日期: 2015-12-18_

### Highlights

Scaling to 1.3 Million Concurrent MQTT Connections on a 12 Core, 32G CentOS server.

New PubSub, Router Design (#402). Prepare for scaling to 10 millions on one cluster.

### Enhancements

Improve the gproc_pool usage with a general emqttd_pool_sup

Improve the design of emqttd_pubsub, add a new emqttd_router module

Improve the design of the whole supervisor tree

Route aging mechanism to remove the topics that have no subscriptions

Improve the dashboard, mysql, pgsql, stomp, sockjs plugins

Add 'topics', 'subscriptions' admin commands

Avoid using mnesia table index and mnesia:index_read API to lower CPU usage

Subscribe timeout exception (#366)

Long Delay on Multiple Topic Subscription (#365)

Subscriptions persistence (#344)

emqttd_ctl: 'subscriptions' command to force clients to subscribe some topics (#361)

### Bugfix

emqttd_sm: spec of lookup_session/1 is not right BUG (#411)

Observer application should be removed from reltool.config for 'wx' app is not available (#410)

### Benchmark

1.3 million concurrent MQTT connections on a 12 Core, 32G CentOS Server, consume about 15G Memory and 200% CPU.

## 0.13.1-beta 版本

_发布日期: 2015-11-28_

Bugfix: Plugin pathes error under windows (#387)

Improve: Too many error logs "[error] Session ..... Unexpected EXIT: client_pid=\<0.14137.35>, exit_pid=\<0.30829.22>, reason=nop..." (#383)

Improve: Define QOS0/1/2, Pooler Error (PR#382)

Improve: High CPU load when 400K unstable mobile connections (#377)

BugFix: emqttd_plugin_pgsql - error using same query with latest update plugin (pgsql#5)

## 0.13.0-beta 版本

_发布日期: 2015-11-08_

### Highlights

Rate Limiting based on [Token Bucket](https://en.wikipedia.org/wiki/Token_bucket) and [Leaky Bucket](https://en.wikipedia.org/wiki/Leaky_bucket#The_Leaky_Bucket_Algorithm_as_a_Meter) Algorithm

Upgrade eSockd and MochiWeb libraries to support Parameterized Connection Module

Improve emqttd_client to support fully asynchronous socket networking

### Enhancements

Protocol Compliant - Session Present Flag (#163)

Compilation fails if repo is cloned with a different name (#348)

emqttd_client: replace gen_tcp:send with port_command (#358)

TCP sndbuf, recbuf, buffer tuning (#359)

emqttd_client.erl to handle 'inet_async', 'inet_reply' properly (#360)

Refator the [client/session management design](https://github.com/emqtt/emqttd/blob/master/doc/design/ClientSession.md)

### Bugfix

Cannot kick transient client out when clientId collision (#357)

Fix the order of emqttd_app:start_server/1 (#367)

emqttd_session:subscribe/2 will crash (#374)

### Benchmark

[benchmark for 0.13.0 release](https://github.com/emqtt/emqttd/wiki/benchmark-for-0.13.0-release)

3.1G memory and 50+ CPU/core:

    Connections: 250K
    Subscribers: 250K
    Topics:      50K
    Qos1 Messages/Sec In:  4K
    Qos1 Messages/Sec Out: 20K
    Traffic In(bps):  12M+
    Traffic Out(bps): 56M+

## 0.12.3-beta 版本

_发布日期: 2015-10-22_

Bugfix: emqttd_sysmon crasher for 'undefined' process_info (#350)

Bugfix: emqttd_client: catch parser exception (#353)

## 0.12.2-beta 版本

_发布日期: 2015-10-16_

Bugfix: Retained messages should not be expired if 'broker.retained.expired_after = 0' (#346)

## 0.12.1-beta 版本

_发布日期: 2015-10-15_

Highlight: Release for Bugfix and Code Refactor.

Feature: Retained message expiration (#182)

Improve: '\$SYS/#' publish will not match '#' or '+/#' (#68)

Improve: Add more metrics and ignore '\$SYS/#' publish (#266)

Improve: emqttd_sm should be optimized for clustered nodes may be crashed (#282)

Improve: Refactor emqttd_sysmon and suppress 'monitor' messages (#328)

Task: benchmark for 0.12.0 release (#225)

Benchmark: About 900K concurrent connections established on a 20Core, 32G CentOS server.

## 0.12.0-beta 版本

_发布日期: 2015-10-08_

### Highlights

Enhance the **emqttd_ctl** module to allow plugins to register new commands (#256)

Add [emqttd_recon plugin](https://github.com/emqtt/emqttd_recon) to debug/optimize the broker (#235)

Add **'./bin/emqttd_ctl broker pubsub'** command to check the status of core pubsub processes

Add **'./bin/emqttd_top'** command(like etop) to show the top 'msg_q', 'reductions', 'memory' or 'runtime' processes

'rel/files/emqttd.config.production' for production deployment(default)

'rel/files/emqttd.config.development' for development deployment

### Enhancements

Qos1/2 messages will not be dropped under unstable mobile network (#264)

**emqttd_session:subscribe/2, emqttd_session:unsubscribe/2** APIs should be asynchronous (#292)

**etc/emqttd.config** : 'idle_timeout' option to close the idle client(socket connected but no 'CONNECT' frame received)

**etc/emqttd.config** : 'unack_retry_interval' option for redelivering Qos1/2 messages

How to monitor large 'message_queue_len' (#283)

### Bugfix

Behaviour emqttd_auth_mod is missing init callback (#318)

### Benchmark

Write a new [benchmark tool](https://github.com/emqtt/emqtt_benchmark) to benchmark this release

Hw requirements - 5K users, 25-50 msgs/sec, QoS=1 (#209)

Supported Number of Connections Greatly Reduced When Clients are Subscribing (#324)

## 0.11.0-beta 版本

_发布日期: 2015-09-25_

Highlight: Rebar to manage plugin dependencies.

Highlight: [Stomp](https://github.com/emqtt/emqttd_stomp) and [SockJS](https://github.com/emqtt/emqttd_sockjs) Plugins!

Improve: add rel/files/emqttd.config.development|production.

Improve: rel/reltool.config.script to release deps of plugin.

Improve: persist mnesia schema on slave nodes.

Improve: use timer:seconds/1 api.

Improve: The binary release will be compiled with R18.1 now.

Bugfix: issue#306 - emqttd_cm should unregister the duplicated client

Bugfix: issue#310 - usage of emqttd_ctl error: 'session list' should be 'sessions list'

Bugfix: issue#311 - './bin/emqttd_ctl sessions list' error

Bugfix: issue#312 - unsubcribe will lead to crash if emqttd_plugin_template plugin loaded

## 0.10.4-beta 版本

_发布日期: 2015-09-18_

Optimize session management and upgrade eSockd library to 2.7.1

[Benchmark for 0.10.4 release](https://github.com/emqtt/emqttd/wiki/benchmark-for-0.10.4-release)

Improve: issue#294 - [error] failed to start connection on 0.0.0.0:1883 - enotconn

Improve: issue#297 - How do I allow user with some pattern to access topic with some pattern?

Bugfix: issue#291 - "./bin/emqttd attach ..." cannot work

Bugfix: issue#284 - Should not use erlang:list_to_atom/1 in emqttd_vm.erl

## 0.10.3-beta 版本

_发布日期: 2015-08-30_

Bugfix: issue#271 - add emqttd_ws_client:subscribe/2 function

Bugfix: issue#269 - bin/emqttd Syntax error on ubuntu

Improve: issue#265 - client under unstable mobile network generate a lot of logs

## 0.10.2-beta 版本

_发布日期: 2015-08-26_

Improve: issue#257 - After the node name changed, the broker cannot restart for mnesia schema error.

## 0.10.1-beta 版本

_发布日期: 2015-08-25_

Bugfix: issue#259 - when clustered the emqttd_dashboard port is close, and the 'emqttd' application cannot stop normally.

Feature: issue#262 - Add ' [ http://host:8083/mqtt/status ](http://host:8083/mqtt/status) ' Page for health check

## 0.10.0-beta 版本

_发布日期: 2015-08-20_

[Web Dashboard](https://github.com/emqtt/emqttd_dashboard) and [MySQL](https://github.com/emqtt/emqttd_plugin_mysql), [PostgreSQL](https://github.com/emqtt/emqttd_plugin_pgsql) Authentication/ACL Plugins!

Highlight: Web Dashboard to monitor Statistics, Metrics, Clients, Sessions and Topics of the broker.

Highlight: JSON/HTTP API to query all clients connected to broker.

Highlight: A new [Plugin Design](https://github.com/emqtt/emqttd/wiki/Plugin%20Design) and a [Template project](https://github.com/emqtt/emqttd_plugin_template) for plugin development.

Highlight: Authentication/ACL with MySQL, PostreSQl databases (#194, #172)

Feature: Session Statistics including inflight_queue, message_queue, message_dropped, awaiting_rel, awaiting_ack, awaiting_comp (#213)

Feature: Cookie based authentication for MQTT over websocket connections (#231)

Feature: Get all clients connected to the broker (#228, #230, #148, #129)

Feature: "./bin/emqttd_ctl clients show ClientId" to query client status (#226)

Feature: "./bin/emqttd_ctl clients kick ClientId" to kick out a client

Feature: "./bin/emqttd_ctl sessions list" to show all sessions

Feature: "./bin/emqttd_ctl sessions show ClientId" to show a session

Feature: Erlang VM metrics monitor with Web Dashboard (#59)

Improve: Too many "inflight queue is full!" log when session is overloaded (#247)

Improve: There are two many "MQueue(~s) drop ~s" logs if the message queue of session is small (#244)

Improve: gen_server2(from RabbitMQ) to improve emqttd_session, emqttd_pubsub

Improve: Makefile to build plugins

Bugfix: emqttd_broker:unhook/2 cannot work (#238)

Bugfix: emqttd plugin cannot include_lib("emqttd/include/emqttd.hrl") (#233)

Bugfix: Too many 'Session ~s cannot find PUBACK' logs (#212)

Bugfix: emqttd_pooler cannot work

## 0.9.3-alpha 版本

_发布日期: 2015-07-25_

Wiki: [Bridge](https://github.com/emqtt/emqttd/wiki/Bridge)

Improve: emqttd_protocol.hrl to define 'QOS_I'

Improve: emqttd_pubsub to add subscribe/2 API

Improve: ./bin/emqttd_ctl to support new bridges command

Bugfix: issue #206 - Cannot bridge two nodes

## 0.9.2-alpha 版本

_发布日期: 2015-07-18_

Improve: issue #196 - Add New Hook 'client.subscribe.after'

## 0.9.1-alpha 版本

_发布日期: 2015-07-10_

Bugfix: issue #189 - MQTT over WebSocket(SSL) cannot work?

Bugfix: issue #193 - 'client.ack' hook should be renamed to 'message.acked', and called by emqttd_broker:foreach_hooks

## 0.9.0-alpha 版本

_发布日期: 2015-07-09_

[Session, Queue, Inflight Window, Hooks, Global MessageId and More Protocol Compliant](https://github.com/emqtt/emqttd/releases/tag/0.9.0-alpha) Now!

Feature: Session/Queue/Inflight Window Design (#145).

Feature: Support to resume a persistent session on other clustered node.

Feature: Support alarm management.

Feature: emqttd_guid to generate global unique message id.

Feature: Hooks for message pub/ack.

Feature: Protocol compliant - message ordering, timeout and retry.

Improve: Every client will start_link a session process, whether or not the client is persistent.

Improve: etc/emqttd.config to support more session, queue configuration.

Improve: issue #179 - Max offline message queue {max_queue, 100} meaning.

Improve: issue #180 - Should change project structure for other projects maybe depend on 'emqttd'. Merge emqtt, emqttd apps.

Improve: issue #185 - PacketId and MessageId: the broker should generate global unique message id.

Improve: issue #187 - etc/emqttd.config to support https listener

Improve: issue #186 - emqttd_cm to store client details

Improve: issue #174 - add 'from' field to mqtt_message record.

Improve: issue #170 - \$SYS Topics should support alarms.

Improve: issue #169 - Add More [Hooks](https://github.com/emqtt/emqttd/wiki/Hooks-Design)

Improve: issue #167 - Inflight window to assure message ordering.

Improve: issue #166 - Message delivery timeout and retry.

Improve: issue #143 - Qos1, Qos2 PubSub message timeout.

Improve: issue #122 - Labeling message with unique id. emqttd_guid module to generate global unique msgid.

Improve: emqttd_bridge to support pending message queue, and fix the wrong Qos design.

Improve: mqtt_message record to add 'msgid', 'from' and 'sys' fields.

Change: Add emqttd_mqueue, emqttd_guid, emqttd_alarm modules.

Bugfix: issue #184 - emqttd_stats:setstats is not right.

Bugfix: Closed issues #181, #119.

Tests: fix the parser, acl test cases.

## 0.8.6-beta 版本

_发布日期: 2015-06-17_

Bugfix: issue #175 - publish Will message when websocket is closed without 'DISCONNECT' packet

## 0.8.5-beta 版本

_发布日期: 2015-06-10_

Bugfix: issue #53 - client will receive duplicate messages when overlapping subscription

## 0.8.4-beta 版本

_发布日期: 2015-06-08_

Bugfix: issue #165 - duplicated message when publish 'retained' message to persistent client

## 0.8.3-beta 版本

_发布日期: 2015-06-05_

Bugfix: issue #158 - should queue:in new message after old one dropped

Bugfix: issue #155 - emqtt_parser.erl: parse_topics/3 should reverse topics

Bugfix: issue #149 - Forget to merge plugins/emqttd_auth_mysql from 'dev' branch to 'master' in 0.8.x release

## 0.8.2-alpha 版本

_发布日期: 2015-06-01_

Bugfix: issue #147 - WebSocket client cannot subscribe queue '$Q/queue/${clientId}'

Bugfix: issue #146 - emqttd_auth_ldap: fill(Username, UserDn) is not right

## 0.8.1-alpha 版本

_发布日期: 2015-05-28_

Client [Presence](https://github.com/emqtt/emqttd/wiki/Presence) Support and [\$SYS Topics](https://github.com/emqtt/emqttd/wiki/\$SYS-Topics) Redesigned!

Bugfix: issue #138 - when client disconnected normally, broker will not publish disconnected \$SYS message

Bugfix: fix websocket url in emqttd/priv/www/websocket.html

Improve: etc/emqttd.config to allow websocket connections from any hosts

Improve: rel/reltool.config to exclude unnecessary apps.

## 0.8.0-alpha 版本

_发布日期: 2015-05-25_

[Hooks](https://github.com/emqtt/emqttd/wiki/Hooks%20Design), Modules and [Plugins](https://github.com/emqtt/emqttd/wiki/Plugin%20Design) to extend the broker Now!

Plugin: emqttd_auth_mysql - MySQL authentication plugin (issues #116, #120)

Plugin: emqttd_auth_ldap - LDAP authentication plugin

Feature: emqttd_broker to support Hooks API

Feature: issue #111 - Support 'Forced Subscriptions' by emqttd_mod_autosub module

Feature: issue #126 - Support 'Rewrite rules' by emqttd_mod_rewrite module

Improve: Support hooks, modules to extend the broker

Improve: issue #76 - dialyzer check

Improve: 'Get Started', 'User Guide', 'Developer Guide' Wiki

Improve: emqtt_topic to add join/1, feed_var/3, is_queue/1

Improve: emqttd_pooler to execute common tasks

Improve: add emqttd_sm_sup module, and use 'hash' gproc_pool to manage sessions

Tests: add more test cases for 'emqttd' app

## 0.7.1-alpha 版本

_发布日期: 2015-05-04_

Add doc/design/_ and merge doc/_ to github Wiki

Bugfix: issue #121 - emqttd cluster issuse

Bugfix: issue #123 - emqttd:unload_all_plugins/0 cannot unload any plugin

Bugfix: fix errors found by dialyzer

## 0.7.0-alpha 版本

_发布日期: 2015-05-02_

[MQTT over WebSocket(SSL)](https://github.com/emqtt/emqttd/wiki/MQTT-Over-WebSocket) Now!

[Plugin Achitecture](https://github.com/emqtt/emqttd/wiki/Plugin%20Design) based on OTP application

[Trace MQTT Packets or Messages](https://github.com/emqtt/emqttd/wiki/Trace%20Design) to log files

Feature: issue #40, #115 - WebSocket/SSL Support

Feature: issue #49, #105 - Plugin Architecture Support

Feature: issue #93 - Trace API Design

Improve: issue #109 - emqttd_broker should add subscribe, notify API

Improve: update README.md to add 'Goals', 'Contributors' chapters

Change: rename etc/app.config to etc/emqttd.config

Change: etc/emqttd.config changed

Bugfix: critical issue #54 - error when resume session!

Bugfix: issue #118 - error report when UNSUBSCRIBE with no topics

Bugfix: issue #117 - sys_interval = 0 config cannot work

Bugfix: issue #112 - Makefile to support build plugins

Bugfix: issue #96 - "make clean" cannot work

## 0.6.2-alpha 版本

_发布日期: 2015-04-24_

Bugfix: critical issue #54, #104, #106 - error when resume session

Improve: add emqttd_cm_sup module, and use 'hash' gproc_pool to register/unregister client ids

Improve: kick old client out when session is duplicated.

Improve: move mnesia dir config from etc/app.config to etc/vm.args

## 0.6.1-alpha 版本

_发布日期: 2015-04-20_

Integrate with [gproc library](https://github.com/uwiger/gproc) to support pool

Feature: issues#91 - should use worker_pool to handle some async work?

Feature: issues#95 - Topic filters in ACL rule should support 'eq' tag

Improve: issues#84 - emqttd_pubsub is redesigned again to protect mnesia transaction

Improve: issues#74 - ACL Support and update [ACL Design Wiki](https://github.com/emqtt/emqttd/wiki/ACL-Design)

## 0.6.0-alpha 版本

_发布日期: 2015-04-17_

ACL Support Now: [ACL-Design Wiki](https://github.com/emqtt/emqttd/wiki/ACL-Design)

Authentication with username, clientid Now: [Authentication Wiki](https://github.com/emqtt/emqttd/wiki/Authentication)

Seperate common MQTT library to 'emqtt' application

Redesign message pubsub, route and retain modules

Redesign mnesia database cluster

Feature: issues#47 - authentication, authorization support

Feature: issues#92 - merge emqttd_acl and emqttd_auth to emqttd_access_control

Feature: emqttd_acl_mod, emqttd_auth_mod behaviour to extend ACL, authentication

Feature: issues#85 - lager:info to log subscribe, unsubscribe actions

Feature: issues#77 - authentication with clientid, ipaddress

Improve: issues#90 - fix lager_file_backend log format, and rotate 10 log files

Improve: issues#88 - use '-mneisa_create', '-mnesia_replicate' attributes to init mneisa

Improve: issues#87 - record mqtt_user and mqtt_client is duplicated

Improve: issues#81 - redesign nodes cluster to support disc_copies mnesia tables

Improve: issues#80 - redesign emqttd_cm to handle more concurrent connections

Improve: issues#70 - how to handle connection flood? Now could support 2K+ CONNECT/sec

Change: redesign mnesia tables: message, topic, subscriber, trie, trie_node

Bugfix: issues#83 - emqttd_broker stats cannot work

Bugfix: issues#75 - careless about function name when emqttd_pubsub handle getstats message

## 0.5.5-beta 版本

_发布日期: 2015-04-09_

Bugfix: issue #75 - careless about function name when emqttd_pubsub handle getstats message.

Bugfix: issue #79 - cannot find topic_subscriber table after cluster with other nodes.

## 0.5.4-alpha 版本

_发布日期: 2015-03-22_

Benchmark this release on a ubuntu/14.04 server with 8 cores, 32G memory from QingCloud.com:

> 200K Connections, 30K Messages/Sec, 20Mbps In/Out Traffic, 200K Topics, 200K Subscribers, Consumed 7G memory, 40% CPU/core

Benchmark code: [ https://github.com/emqtt/emqttd_benchmark ](https://github.com/emqtt/emqttd_benchmark)

Change: rewrite emqttd_pubsub to handle more concurrent subscribe requests.

Change: ./bin/emqttd_ctl add 'stats', 'metrics' commands.

Bugfix: issue #71, #72

## 0.5.3-alpha 版本

_发布日期: 2015-03-19_

Bugfix: issues#72 - emqttd_cm, emqtt_sm ets:match_delete/2 with wrong pattern

## 0.5.2-alpha 版本

_发布日期: 2015-03-18_

Change: upgrade esockd to 2.1.0-alpha, do not tune socket buffer for mqtt connection.

## 0.5.1-alpha 版本

_发布日期: 2015-03-13_

Change: upgrade esockd to v1.2.0-beta, rename 'acceptor_pool' to 'acceptors'

## 0.5.0-alpha 版本

_发布日期: 2015-03-12_

RENAME 'emqtt' to 'emqttd'!

Support [Broker Bridge](https://github.com/emqtt/emqttd/wiki/Bridge-Design) Now!

Change: rename project from 'emqtt' to 'emqttd'

Change: lager:debug to dump RECV/SENT packets

Feature: emqttd_bridge, emqttd_bridge_sup to support broker bridge

Feature: emqtt_event to publish client connected/disconnected message to \$SYS topics

Feature: ./bin/emqttd_ctl add more commands: listeners, broker, bridges, start_bridge, stop_bridge...

Feature: issue#57 - support to configure max packet size

Feature: issue#68 - if sys_interval = 0, emqttd_broker will not publish messages to \$SYS/brokers/#

Bugfix: issue#67 - subscribe '#' to receive all messages

Bugfix: issue#64 - emqtt_app start/2: should wait_for_databases

Test: emqttd_topic_tests add more '\_match_test'

## 0.4.0-alpha 版本

_发布日期: 2015-03-10_

Support [\$SYS Topics of Broker](https://github.com/emqtt/emqttd/wiki/\$SYS-Topics-of-Broker) Now!

Feature: emqtt_broker to publish version, uptime, datetime to \$SYS/brokers/# topics

Feature: emqtt_broker to publish count of clients, sessions, suscribers to \$SYS/brokers/# topics

Feature: emqtt_metrics to publish bytes, packets, messages metrics to \$SYS/brokers/# topics

Feature: add include/emqtt_systop.hrl

Change: emqtt_cm to count current clients

Change: emqtt_sm to count current sessions

Change: emqtt_pubsub to count current topics and suscribers

Change: emqtt_pubsub to add create/1 API

Change: emqtt_pubsub dispatch/2 to return number of subscribers

Change: emqtt_pubsub to count 'dropped' messages

Change: emqtt_opts to add merge/2 function

Test: add emqtt_serialiser_tests.erl

## 0.3.4-beta 版本

_发布日期: 2015-03-08_

Bugfix: emqtt_serialiser.erl cannot serialise UNSUBACK packets

## 0.3.3-beta 版本

_发布日期: 2015-03-07_

Bugfix: emqtt_serialiser.erl cannot serialise PINGRESP issue#60

## 0.3.2-beta 版本

_发布日期: 2015-03-05_

Improve: merge emqttc serialiser, parser, packet

Add: emqtt_opts to merge socket options

## 0.3.1-beta 版本

_发布日期: 2015-03-02_

Feature: SSL Socket Support

Feature: issue#44 HTTP API should add Qos parameter

Bugfix: issue#52 emqtt_session crash

Bugfix: issue#53 sslsocket keepalive error

Upgrade: esockd to v0.2.0

Upgrade: mochiweb to v3.0.0

## 0.3.0-beta 版本

_发布日期: 2015-01-19_

Feature: HTTP POST API to support 'qos', 'retain' parameters

Feature: \$SYS system topics support

Change: Rewrite emqtt_topic.erl, use '', '#', '+' to replace <<"">>, <<"#">>, <<"+">>

Change: fix emqtt_pubsub.erl to match '#', '+'

Tests: emqtt_topic_tests.erl add more test cases

## 0.3.0-alpha 版本

_发布日期: 2015-01-08_

NOTICE: Full MQTT 3.1.1 support now!

Feature: Passed org.eclipse.paho.mqtt.testing/interoperability tests

Feature: Qos0, Qos1 and Qos2 publish and suscribe

Feature: session(clean_sess=false) management and offline messages

Feature: redeliver awaiting puback/pubrec messages(doc: Chapter 4.4)

Feature: retain messages, add emqtt_server module

Feature: MQTT 3.1.1 null client_id support

Bugfix: keepalive timeout to send will message

Improve: overlapping subscription support

Improve: add emqtt_packet:dump to dump packets

Test: passed org.eclipse.paho.mqtt.testing/interoperability

Test: simple cluster test

Closed Issues: #22, #24, #27, #28, #29, #30, #31, #32, #33, #34, #36, #37, #38, #39, #41, #42, #43

## 0.2.1-beta 版本

_发布日期: 2015-01-08_

pull request 26: Use binaries for topic paths and fix wildcard topics

emqtt_pubsub.erl: fix wildcard topic match bug caused by binary topic in 0.2.0

Makefile: deps -> get-deps

rebar.config: fix mochiweb git url

tag emqtt release accoding to [Semantic Versioning](http://semver.org/)

max clientId length is 1024 now.

## 0.2.0 版本

_发布日期: 2014-12-07_

rewrite the project, integrate with esockd, mochiweb

support MQTT 3.1.1

support HTTP to publish message

## 0.1.5 版本

_发布日期: 2013-01-05_

Bugfix: remove QOS_1 match when handle PUBREL request

Bugfix: reverse word in emqtt_topic:words/1 function

## 0.1.4 版本

_发布日期: 2013-01-04_

Bugfix: fix "mosquitto_sub -q 2 ......" bug

Bugfix: fix keep alive bug

## 0.1.3 版本

_发布日期: 2013-01-04_

Feature: support QOS2 PUBREC, PUBREL,PUBCOMP messages

Bugfix: fix emqtt_frame to encode/decoe PUBREC/PUBREL messages

## 0.1.2 版本

_发布日期: 2012-12-27_

Feature: release support like riak

Bugfix: use ?INFO/?ERROR to print log in tcp_listener.erl

## 0.1.1 版本

_发布日期: 2012-09-24_

Feature: use rebar to generate release

Feature: support retained messages

Bugfix: send will msg when network error

## 0.1.0 版本

_发布日期: 2012-09-21_

The first public release.
