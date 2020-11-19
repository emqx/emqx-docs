# Changes

## Version 2.3.11

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

## Version 2.3.10

_Release Date: 2018-06-27_

### Bugfix and Enhancements

Upgrade the esockd library to v5.2.2

### emq-auth-http

Ignore auth on ignore in body, allows for chaining methods

## Version 2.3.9

_Release Date: 2018-05-20_

### Bugfix and Enhancements

Bugfix: check params for REST publish API (#1599)

Upgrade the mongodb library to v3.0.5

### esockd

Bugfix: proxy protocol - set socket to binary mode (#78)

## Version 2.3.8

_Release Date: 2018-05-11_

### Bugfix and Enhancements

Bugfix: unregister users CLI when unload emq_auth_username (#1588)

Bugfix: Should be an info level when change CleanSession (#1590)

Bugfix: emqttd_ctl crashed when emq_auth_usename doesn't exist (#1588)

### emq-auth-mongo

Improve: Support authentication database (authSource) (#116)

## Version 2.3.7

_Release Date: 2018-04-22_

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

## Version 2.3.6

_Release Date: 2018-03-25_

### Bugfix and Enhancements

Security: LWT message checking the ACL (#1524)

Bugfix: Retain msgs should not be sent to existing subscriptions (#1529)

### emq-auth-jwt

Validate JWT token using a expired field (#29)

## Version 2.3.5

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

## Version 2.3.4

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

## Version 2.3.3

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

## Version 2.3.2

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

## Version 2.3.1

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

## Version 2.3.0 "Passenger's Log"

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

## Version 2.3-rc.2

_Release Date: 2017-10-22_

#### Bugfix

Change the default logging level of trace CLI. (emqttd#1306)

### emq-dashboard Plugin (emq-dashboard#164)

Fix the 'Status' filters of plugins's management.

Fix the URL Redirection when deleting an user.

Compatible with IE,Safari,360 Browsers.

## Version 2.3-rc.1

_Release Date: 2017-10-12_

#### Bugfix

Fixed the issue that invalid clients can publish will message. (emqttd#1230)

Fixed Dashboard showing no stats data (emqttd#1263)

Fixed a rare occurred building failure (emqttd#1284)

Support Persistence Logs for longer time (emqttd#1275)

Fix for users APIs (emqttd#1289)

Changed passwd_hash/2 function's return type (emqttd#1289)

### emq-dashboard Plugin (emq-dashboard#154)

Improved the Dashboard Interface of Monitoring/Management/Tools.

Allow switching dashboard themes.

Supoort both EN and CN languages.

## Version 2.3-beta.4

_Release Date: 2017-09-13_

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

## Version 2.3-beta.3

_Release Date: 2017-08-21_

### Enhancements

Add HTTP API for hot configuration.

### Bugfix

1. Parse 'auth.mysql.password_hash' error when hot configuration reload (emq-auth-mysql#68)
2. Set 'auth.pgsql.server' error when hot configuration reload (emq-auth-pgsql#67)
3. Set 'auth.redis.server' and 'auth.redis.password_hash' error when hot configuration reload (emq-auth-redis#47)
4. Fixed the issue that when deleting retained message subscribed clients are not notified (emqttd#1207)
5. Support more parameters for hot configuration reload:

- mqtt.websocket_protocol_header = on
- mqtt.mqueue.low_watermark = 20%
- mqtt.mqueue.high_watermark = 60%
- mqtt.client.idle_timeout = 30s
- mqtt.client.enable_stats = off

## Version 2.3-beta.2

_Release Date: 2017-08-12_

EMQ R2.3-beta.2, a development release, is available now! This release introduces new HTTP Managment API, and supports Hot configuration of some parameters and plugins.

The plugins which support Hot configuration:

- emq-stomp
- emq-coap
- emq-sn
- emq-lwm2m
- emq-dashboard
- emq-retainer
- emq-recon
- emq-web-hook
- emq-auth-jwt
- emq-auth-http
- emq-auth-mongo
- emq-auth-mysql
- emq-auth-pgsql
- emq-auth-redis

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

## Version 2.3-beta.1

_Release Date: 2017-07-24_

EMQ R2.3-beta.1 is available now! This release supports automatic cluster node discovery and network partition autoheal. It supports automatically forming clusters of Erlang nodes using different strategies, such as IP Multicast, Etcd and Kubernetes.

### Node Discovery and Autocluster

EMQ R2.3 supports node discovery and autocluster with various strategies:

| Strategy | Description                     |
| -------- | ------------------------------- |
| static   | Autocluster by static node list |
| mcast    | Autocluster by UDP Multicast    |
| dns      | Autocluster by DNS A Record     |
| etcd     | Autocluster using etcd          |
| k8s      | Autocluster on Kubernetes       |

### Network Partition and Autoheal

Enable autoheal of Network Partition by default:

    cluster.autoheal = on

When network partition occurs, the following steps are performed to heal the cluster if autoheal is enabled:

1. Node reports the partitions to a leader node which has the oldest guid.
2. Leader node create a global netsplit view and choose one node in the majority as coordinator.
3. Leader node requests the coordinator to autoheal the network partition.
4. Coordinator node reboots all the nodes in the minority side.

### Node down and Autoclean

A down node will be removed from the cluster if autoclean is enabled:

    cluster.autoclean = 5m

### LWM2M Protocol Support

EMQ-LWM2M is a gateway plugin for EMQ，which implements most LWM2M features. MQTT client is able to access LWM2M device through emq-lwm2m plugin, by sending a command and reading its response.

Lightweight M2M (LWM2M) is a set of protocols defined by the Open Mobile Alliance (OMA) for machine-to-machine (M2M) or Internet of Things (IoT) device management and communications

### JWT Authentication

EMQ R2.3 supports JWT(JSON Web Token) Authentication with [ emq-auth-jwt ](https://github.com/emqtt/emq-auth-jwt) plugin.

### Retainer Plugin

Retainer Plugin support 'disc_only' mode to store MQTT retained messages.

### Debian 9 Package

EMQ R2.3 released binary package for Debian 9.

### Erlang/OTP R20

EMQ R2.3 is compatible with Erlang/OTP R20, and all the binary packages are built on Erlang/OTP R20.

## Version 2.2 "Nostalgia"

_Release Date: 2017-07-08_

_Release Name: Nostalgia_

EMQ 2.2.0 is available now! EMQ R2.2 supports CoAP(RFC 7252), MQTT-SN protocols completely, and it is extensible with Web Hook, Lua Hook and Elixir Hook.

Feature: Add 'listeners restart/stop' CLI command (emqttd#1135)

Bugfix: Exit Code from emqttd_ctl (emqttd#1133)

Bugfix: Fix spec errors found by dialyzer (emqttd#1136)

Bugfix: Catch exceptions thrown from rpc:call/4 (emq-dashboard#128)

Bugfix: Topic has been decoded by gen-coap, no conversion needed (emq-coap#43)

## Version 2.2-rc.2

_Release Date: 2017-07-03_

::: warning Warning
2.2-rc.2 requires Erlang/OTP R19.3+ to build.
:::

### Bugfix and Enhancements

Compatible with Erlang/OTP R20 (emq-relx#77)

CoAP gateway plugin supports coap-style publish & subscribe pattern. (emq_coap#33)

MQTT-SN gateway plugin supports sleeping device (emq_sn#32)

Upgrade esockd and mochiweb libraries to support restarting a listener

## Version 2.2-rc.1

_Release Date: 2017-06-14_

### Bugfix and Enhancements

Add a new listener for HTTP REST API (emqttd#1094)

Fix the race condition issue caused by unregister_session/1 (emqttd#1096)

Fix the issue that we cannot remove a down node from the cluster (emqttd#1100)

Passed org.eclipse.paho.mqtt_sn.testing/interoperability tests (emq_sn#29)

Fixed the issue that send http request and return non-200 status code, but AUTH/ACL result is denied (emq-auth-http#33)

Fixed the issue that fail to stop listener (emq_stomp#24)

Support using systemctl to manage emqttd service on CentOS

## Version 2.2-beta.3

_Release Date: 2017-05-27_

### Bugfix and Enhancements

Call emit_stats when force GC (emqttd#1071)

Update the default value of 'mqtt.mqueue.max_length' to 1000 (emqttd#1074)

Update emq-auth-mongo READEME (emq-auth-mongo#66)

Update default password field (emq-auth-mongo#67)

Upgrade the mongodb library to v3.0.3

Remove ‘check password===undefined && userName!== undefined’ (emq-dashboard#120)

### emq_auth_redis Plugin

Support 'HGET mqtt_user:%u password' for authentication query

### emq_auth_mongo Plugin

Support mongodb Cluster, Replica Set

### Documentation

Add 'Build on Windows' chapter

## Version 2.2-beta.2

_Release Date: 2017-05-20_

### Bugfix and Enhancements

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

## Version 2.2-beta.1

_Release Date: 2017-05-05_

EMQ 2.2-beta.1 is now available. Many new features including Web Hook, Lua Hook and Proxy Protocol have been released in this version.

### MQTT Listeners

Support to configure multiple MQTT TCP/SSL listeners for one EMQ node. For example:

    -------
            -- External TCP 1883 --> |     |
    | EMQ | -- Internal TCP 2883 --> Service
            -- External SSL 8883-->  |     |
    -------

Configure a listener in etc/emq.conf:

    listener.tcp.${name}= 127.0.0.1:2883

    listener.tcp.${name}.acceptors = 16

    listener.tcp.${name}.max_clients = 102400

### Proxy Protocol V1/2

The EMQ cluster is usually deployed behind a Load Balancer, such as HAProxy or NGINX:

    -----
    |   |
    | L | --TCP 1883--> EMQ
          --SSL 8883--> |   |                |
    | B | --TCP 1883--> EMQ
    |   |
    -----

The LB can pass the source IP, port of the TCP connection on to EMQ cluster by Proxy Protocol.

Enable Proxy Protocol support for MQTT Listener:

    ## Proxy Protocol V1/2
    ## listener.tcp.${name}.proxy_protocol = on
    ## listener.tcp.${name}.proxy_protocol_timeout = 3s

### Web Hook Plugin

The Web Hook plugin [ emq-web-hook ](https://github.com/emqtt/emq-web-hook) can trigger a webhook callback when a MQTT client connected to or disconnected from the broker, a MQTT message is published or acked.

### Lua Hook Plugin

The Lua Hook plugin [ emq-lua-hook ](https://github.com/emqtt/emq-lua-hook) make it possible to extend the broker and write business logic with Lua script.

### Improve the Auth/ACL Chain

We improved the Auth/ACL chain design in 2.2 release. The Auth request will be forwarded to next auth module if it is ignored by the current auth module:

                 --------------           -------------           --------------
      Client --> | Redis Auth | -ignore-> | HTTP Auth | -ignore-> | MySQL Auth |
                 --------------           -------------           --------------
                       |                       |                       |
                      \|/                     \|/                     \|/
                 allow | deny            allow | deny            allow | deny

### Support bcrypt password hash

Enable the bcrypt password hash in auth module, for example:

    auth.redis.password_hash = bcrypt

### API Breaking Change

etc/emq.conf: 'mqtt.queue. _' changed to 'mqtt.mqueue._ '

### emq-dashboard

Support 'Unsubscribe' action on WebSocket Page.

## Version 2.1.2

_Release Date: 2017-04-21_

Fix emqttd_ctl sessions list CLI

Newline character in emq.conf causing error;(emqttd#1000)

Fix crash caused by duplicated PUBREC packet (emqttd#1004)

Unload the 'session.created' and 'session.teminated' hooks (emq-plugin-template)

## Version 2.1.1

_Release Date: 2017-04-14_

Localhost:8083/status returns 404 when AWS LB check the health of EMQ (emqttd#984)

Https listener not working in 2.1.0 as in 2.0.7 (emq-dashboard#105)

Fix mqtt-sn Gateway not working (emq-sn#12)

Upgrade emq-sn Plugin (emq-sn#11)

Upgrade emq-coap Plugin (emq-coap#21)

## Version 2.1.0

_Release Date: 2017-04-07_

The stable release of 2.1 version.

Trouble with auth.mysql.acl_query (emq-auth-mysql#38)

Filter the empty fields in ACL table (emq-auth-mysql#39)

## Version 2.1.0-rc.2

_Release Date: 2017-03-31_

Support pbkdf2 hash (emq-auth-mongo#46)

Kickout the conflict WebSocket connection (emqttd#963)

Correct licence in app.src (emqttd#958)

SSL options to connect to pgsql (emq-auth-pgsql#41)

## Version 2.1.0-rc.1

_Release Date: 2017-03-24_

EMQ fails to start if run under a different linux user than that which first ran it (emqttd#842)

Depend on emqtt/pbkdf2 to fix the building errors of Travis CI (emqttd#957)

Depend on goldrush and emqtt/pbkdf2 to resolve the building errors (emqttd#956)

Fix 'rebar command not found' (emq-relx#33)

Compile error in v2.1.0-beta.2 (emq-relx#32)

Support salt with passwords (emq-auth-mongo#11)

Change the default storage_type to 'ram' (emq-retainer#13)

## Version 2.1.0-beta.2

_Release Date: 2017-03-13_

Cannot find AwaitingAck (emqttd#597)

EMQ V2.1 crash when public with QoS = 2 (emqttd#919)

Support pbkdf2 hash (emqttd#940)

Add src/emqttd.app.src to be compatible with rebar3 (emqttd#920)

Add more test cases (emqttd#944)

CRASH REPORT Process \<0.1498.0> with 0 neighbours crashed with reason: {ssl_error,{tls_alert,"certificate unknown"}} in esockd_connection:upgrade (emqttd#915)

auth.redis.password_hash = plain by default (emq-auth-redis#20)

## Version 2.1.0-beta.1

_Release Date: 2017-02-24_

EMQ v2.1.0-beta.1 is now available.

::: warning Warning
EMQ 2.1+ Requires Erlang/OTP R19+ to build.
:::

Since 2.1.0 release, we will tag EMQ versions accoding to the [ Semantic Versioning 2.0.0 ](http://semver.org) principles. And we will release EMQ versions monthly, odd number releases for bugfix and optimization, and even number releases for bugfix and new features.

### Tuning GC

1. All the WebSocket, Client, Session processes will hiberante and GC after a period of idle time.
2. Add 'mqtt.conn.force_gc_count' configuration to force the Client, Session processes to GC when high message throughput.
3. Tune the 'fullsweep_after' option of WebSocket, Client, Session processes.

### Hooks API

Hooks module now support to register the same function with different tags.

### Bugfix

emqttd#916: Add 'mqtt_msg_from()' type

emq-auth-http#15: ACL endpoint isnt called

## Version 2.1-beta

## Version 2.1-beta

_Release Date: 2017-02-18_

EMQ v2.1-beta is now available. We improved the design of Session/Inflight and use one timer to redeliver the inflight QoS1/2 messages, and improved the GC mechanism of MQTT connection process to reduce CPU usage at the high rate of messages.

### Per Client, Session Statistics

Support Per Client, Session Statistics. Enable by configuration in etc/emq.conf:

    mqtt.client.enable_stats = 60s

    mqtt.session.enable_stats = 60s

### Add 'missed' Metrics

The 'missed' metrics will be increased when EMQ broker received PUBACK, PUBREC, PUBREL, PUBCOMP packets from clients, but missing in inflight window:

    packets/puback/missed

    packets/pubrec/missed

    packets/pubrel/missed

    packets/pubcomp/missed

### Integrate Syslog

Output EMQ log to syslog:

    ## Syslog. Enum: on, off
    log.syslog = on

    ##  syslog level. Enum: debug, info, notice, warning, error, critical, alert, emergency
    log.syslog.level = error

### Upgrade QoS

Support to upgrade QoS accoding to the subscription:

    mqtt.session.upgrade_qos = on

### Add 'acl reload' CLI

Reload acl.conf without restarting emqttd service (#885)

### etc/emq.conf Changes

1. Rename mqtt.client_idle_timeout to mqtt.client.idle_timeout
2. Add mqtt.client.enable_stats
3. Add mqtt.session.upgrade_qos
4. Delete mqtt.session.collect_interval
5. Add mqtt.session.enable_stats
6. Rename mqtt.session.expired_after to mqtt.session.expiry_interval

### Merge modules to emq_modules

Merge the emq_mod_presence, emq_mod_subscription, emq_mod_rewrite into emq_modules

Rename emq_mod_retainer to emq_retainer project

### Dashboard Plugin

Overview page: Add 'missed' metrics Client page: Add 'SendMsg', 'RecvMsg' Fields Session page: Add DeliverMsg, EnqueueMsg Fields

### recon Plugin

Change the datatype of 'recon.gc_interval' to duration

### reloader Plugin

Change the datatype of 'reloader.interval' to duration

## Version 2.0.7

_Release Date: 2017-01-20_

The Last Maintenance Release for EMQ 2.0, and support to build RPM/DEB Packages.

Create the emq-package project: [ https://github.com/emqtt/emq-package ](https://github.com/emqtt/emq-package)

emq-auth-http#9: Update the priv/emq_auth_http.schema, cuttlefish:unset() if no super_req/acl_req config exists

emq-auth-mongo#31: cuttlefish:unset() if no ACL/super config exists

emq-dashboard#91: Fix the exception caused by binary payload

emq-relx#21: Improve the binemqttd.cmd batch script for windows platform

emqttd#873: Documentation: installing-from-source

emqttd#870: Documentation: The word in Documents is wrong

emqttd#864: Hook 'client.unsubscribe' need to handle 'stop'

emqttd#856: Support variables in etc/emq.conf: {{ runner_etc_dir }}, {{ runner_etc_dir }}, {{ runner_data_dir }}

## Version 2.0.6

_Release Date: 2017-01-08_

Upgrade the [ esockd ](https://github.com/emqtt/esockd) library to v4.1.1

esockd#41: Fast close the TCP socket if ssl:ssl_accept failed

emq-relx#15: The EMQ 2.0 broker cannot run on Windows.

emq-auth-mongo#31: Mongodb ACL Cannot work?

## Version 2.0.5

_Release Date: 2016-12-24_

emq-auth-http#9: Disable ACL support

emq-auth-mongo#29: Disable ACL support

emq-auth-mongo#30: {datatype, flag}

## Version 2.0.4

_Release Date: 2016-12-16_

emqttd#822: Test cases for SSL connections

emqttd#818: trap_exit to link WebSocket process

emqttd#799: Can't publish via HTTPS

## Version 2.0.3

_Release Date: 2016-12-12_

emqttd#796: Unable to forbidden tcp lisener

emqttd#814: Cannot remove a 'DOWN' node from the cluster

emqttd#813: Change parameters order

emqttd#795: Fix metrics of websocket connections

emq-dashboard#88: Rename the default topic from “/World” to “world”

emq-dashboard#86: Lookup all online clients

emq-dashboard#85: Comment the default listener port

emq-mod-retainer#3: Retained messages get lost after EMQTT broker restart.

## Version 2.0.2

_Release Date: 2016-12-05_

emqttd#787: Stop plugins before the broker stopped, clean routes when a node down

emqttd#790: Unable to start emqttd service if username/password contains special characters

emq-auth-clientid#4: Improve the configuration of emq_auth_clientid.conf to resolve emqttd#790

emq-auth-username#4: Improve the configuration of emq_auth_username.conf to resolve emqttd#790

## Version 2.0.1

_Release Date: 2016-11-30_

emqttd#781: Update README for EMQ 2.0

emq_dashboard#84: Show the Cluster Status of Node

emq_dashboard#79: disc_copies to store mqtt_admin table

emq_auth_clientid: disc_copies to store mqtt_auth_clientid table

emq_auth_username: disc_copies to store mqtt_auth_username table

emq_mod_subscription#3: Remove emq_mod_subscription table and module.subscription.backend config

emq_plugin_template#5: Unregister Auth/ACL modules when the plugin unloaded

## Version 2.0 "West of West Lake"

_Release Date: 2016-11-24_

_Release Name: West of West Lake_

The EMQ Version 2.0, named "West of West Lake", has been released with a lot of improvements and enhancements, and is ready to deploy in production now.

1. First of all, the EMQ broker now supports Shared Subscription and Local Subscription .
2. Supports CoAP(RFC 7252) and MQTT-SN protocol/gateway.
3. Adopt a more user-friendly k = v syntax for the new configuration file.
4. Add more hooks and new plugins, integrate with HTTP, LDAP, Redis, MySQL, PostgreSQL and MongoDB.
5. Cross-platform Builds and Deployment. Run the broker on Linux, Unix, Windows, Raspberry Pi and ARM platform.

### Shared Subscription

Shared Subscription supports Load balancing to distribute MQTT messages between multiple subscribers in the same group:

    ---------
    |       | --Msg1--> Subscriber1
    Publisher--Msg1,Msg2,Msg3-->|  EMQ  | --Msg2--> Subscriber2
    |       | --Msg3--> Subscriber3
    ---------

Create a shared subscription with $queue/ or $share/\<group>/ prefix:

| Prefix           | Examples                             |
| ---------------- | ------------------------------------ |
| $queue/          | mosquitto_sub -t '$queue/topic       |
| $share/\<group>/ | mosquitto_sub -t '$share/group/topic |

### Local Subscription

The Local Subscription will not create global routes on clustered nodes, and only dispatch MQTT messages on local node.

Usage: subscribe a topic with $local/ prefix.

### erlang.mk and relx

The EMQ 2.0 adopts [ erlang.mk ](https://erlang.mk) and [ relx ](https://github.com/erlware/relx) tools to build the whole projects on Linux, Unix and Windows.

### CoAP Support

The EMQ 2.0 supports CoAP(RFC7252) protocol/gateway now, and supports communication between CoAP, MQTT-SN and MQTT clients.

CoAP Protocol Plugin: [ https://github.com/emqtt/emqttd_coap ](https://github.com/emqtt/emqttd_coap)

### MQTT-SN Support

The EMQ 2.0 now supports MQTT-SN protocol/gateway.

MQTT-SN Plugin: [ https://github.com/emqtt/emq_sn ](https://github.com/emqtt/emq_sn)

### New Configuration File

The release integrated with cuttlefish library, and adopted a more user-friendly k = v syntax for the new configuration file:

    ## Node name
    node.name = emqttd@127.0.0.1
    ...
    ## Max ClientId Length Allowed.
    mqtt.max_clientid_len = 1024
    ...

The new configuration files will be preprocessed and translated to an Erlang app.config before the EMQ broker started:

    ----------------------                                          2.0/schema/*.schema      -------------------
    | etc/emq.conf       |                   -----------------              \|/              | data/app.config |
    |       +            | --> mergeconf --> | data/app.conf | -->  cuttlefish generate  --> |                 |
    | etc/plugins/*.conf |                   -----------------                               | data/vm.args    |
    ----------------------                                                                   -------------------

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

We released an official Docker Image for EMQ 2.0. The open source project for Dockerfile: [ https://github.com/emqtt/emq_docker ](https://github.com/emqtt/emq_docker) .

### Full Support for Windows

The EMQ 2.0 fully supports Windows platform. You can run 'emqttd_ctl' command and cluster two nodes on Windows now.

### Bugfix and Enhancements

#764: add mqtt.cache_acl option

#667: Configuring emqttd from environment variables

#722: mqtt/superuser calls two times emqtt_auth_http

#754: "-heart" option for EMQ 2.0

#741: emq_auth_redis cannot use hostname as server address

### Plugins

| Plugin                                                                  | Description                   |
| ----------------------------------------------------------------------- | ----------------------------- |
| [ emq_dashboard ](https://github.com/emqtt/emqttd_dashboard)            | Web Dashboard                 |
| [ emq_auth_clientid ](https://github.com/emqtt/emq_auth_clientid)       | ClientId Auth Plugin          |
| [ emq_auth_username ](https://github.com/emqtt/emq_auth_username)       | Username/Password Auth Plugin |
| [ emq_auth_ldap ](https://github.com/emqtt/emq_auth_ldap)               | LDAP Auth                     |
| [ emq_auth_http ](https://github.com/emqtt/emq_auth_http)               | HTTP Auth/ACL Plugin          |
| [ emq_auth_mysql ](https://github.com/emqtt/emq_auth_mysql)             | MySQL Auth/ACL Plugin         |
| [ emq_auth_pgsql ](https://github.com/emqtt/emq_auth_pgsql)             | PostgreSQL Auth/ACL Plugin    |
| [ emq_auth_redis ](https://github.com/emqtt/emq_auth_redis)             | Redis Auth/ACL Plugin         |
| [ emq_auth_mongo ](https://github.com/emqtt/emq_auth_mongo)             | MongoDB Auth/ACL Plugin       |
| [ emq_mod_presence ](https://github.com/emqtt/emq_mod_presence)         | Presence Module               |
| [ emq_mod_retainer ](https://github.com/emqtt/emq_mod_retainer)         | Retainer Module               |
| [ emq_mod_rewrite ](https://github.com/emqtt/emq_mod_rewrite)           | Topic Rewrite Module          |
| [ emq_mod_subscription ](https://github.com/emqtt/emq_mod_subscription) | Subscription Module           |
| [ emq_coap ](https://github.com/emqtt/emq_coap)                         | CoAP Protocol Plugin          |
| [ emq_sn ](https://github.com/emqtt/emq_sn)                             | MQTT-SN Protocol Plugin       |
| [ emq_stomp ](https://github.com/emqtt/emq_stomp)                       | STOMP Protocol Plugin         |
| [ emq_sockjs ](https://github.com/emqtt/emq_sockjs)                     | STOMP over SockJS Plugin      |
| [ emq_recon ](https://github.com/emqtt/emq_recon)                       | Recon Plugin                  |
| [ emq_reloader ](https://github.com/emqtt/emq_reloader)                 | Reloader Plugin               |
| [ emq_plugin_template ](https://github.com/emqtt/emq_plugin_template)   | Template Plugin               |

## Version 2.0-rc.3

_Release Date: 2016-11-01_

1. Change the three modules(Presence, Retainer, Subscription) to standalone plugins:

| [ emq_mod_retainer ](https://github.com/emqtt/emq_mod_retainer)         | Retained Message Storage                                                      |
| ----------------------------------------------------------------------- | ----------------------------------------------------------------------------- |
| [ emq_mod_presence ](https://github.com/emqtt/emq_mod_presence)         | Publish presence message to $SYS topics when client connected or disconnected |
| [ emq_mod_subscription ](https://github.com/emqtt/emq_mod_subscription) | Subscribe topics automatically when client connected                          |

2. Update the SSL certificates under the etc/certs/ folder.
3. Bugfix: Fixed a typo (#716)
4. Bugfix: emqttd_http can not use emq_auth_http? #739
5. Bugfix: emq_auth_redis cannot use hostname as server address (#741)
6. Upgrade Redis, MySQL, Postgre and MongoDB plugins to support hostname.

## Version 2.0-rc.2

_Release Date: 2016-10-19_

1.  A more user-friendly configuration for the EMQ broker. Integrate with cuttlefish library and adopt K = V syntax:

        node.name = emqttd@127.0.0.1

        ...

        mqtt.listener.tcp = 1883

        ...

2.  Support OS Environments:

        EMQ_NODE_NAME

        EMQ_NODE_COOKIE
        EMQ_MAX_PORTS
        EMQ_TCP_PORT
        EMQ_SSL_PORT
        EMQ_HTTP_PORT
        EMQ_HTTPS_PORT

3.  Refactor all the modules and plugins, and adopt new configuration syntax.


## Version 2.0-rc.1

_Release Date: 2016-10-03_

1.  mqtt/superuser POST called two times in emqtt_auth_http (#696)

2.  Close MQTT TCP connection if authentication failed (#707)

3.  Improve the plugin management. Developer don't need to add plugin's config to rel/sys.config

4.  Add BUILD_DEPS in the plugin's Makefile:

        BUILD_DEPS = emqttd

        dep_emqttd = git https://github.com/emqtt/emqttd emq20

5.  Improve the design of Redis ACL.

## Version 2.0-beta.3

_Release Date: 2016-09-18_

### New Features

Shared Suscriptions (#639, #416):

    mosquitto_sub -t '$queue/topic'
    mosquitto_sub -t '$share/group/topic'

Local Subscriptions that will not create global routes:

    mosquitto_sub -t '$local/topic'

### Bugfix

Error on Loading emqttd_auth_http (#691)

Remove 'emqttd' application from dependencies (emqttd_coap PR#3)

## Version 2.0-beta.2

_Release Date: 2016-09-10_

### CoAP Support

Release an experimental CoAP Gateway: [ https://github.com/emqtt/emqttd_coap ](https://github.com/emqtt/emqttd_coap)

### API Breaking Changes

'\$u', '$c' variables in emqttd.conf and modules/acl.conf changed to '%u', '%c'

Improve the design of mqtt retained message, replace emqttd_retainer with emqttd_mod_retainer.

Add 'session.subscribed', 'session.unsubscribed' hooks, remove 'client.subscribe.after' hook

Tab 'retained_message' -> 'mqtt_retained'

### Bugfix

[2.0 beta1] FORMAT ERROR: "~s PUBLISH to ~s: ~p" (PR #671)

Fixing issues in cluster mode. (PR #681)

Fixing issues with unsubscribe hook (PR #673)

## Version 2.0-beta.1

_Release Date: 2016-08-30_

_Release Name: West of West Lake_

### EMQ - Shortened Project Name

Adopt a shortened project name: EMQ(Erlang/Enterprise/Elastic MQTT Broker), E means Erlang/OTP, Enterprise and Elastic.

### Improve the Release Management

In order to iterate the project fast, we will adopt a new release management strategy since 2.0. There will be two or three 'Preview Release' named beta1, beta2 or beta3, and then one or two 'Release Candidate' named rc1, rc2 before a Major version is production ready.

### Seperate Rel from Application

We split the emqttd 1.x project into two projects since 2.0-beta1 release to resolve the plugins' dependency issue.

A new project named [ emqttd-relx ](https://github.com/emqtt/emqttd-relx) is created and responsible for buiding the emqttd application and the plugins:

    git clone https://github.com/emqtt/emqttd-relx.git

    cd emqttd-relx && make

    cd _rel/emqttd && ./bin/emqttd console

### erlang.mk and relx

The rebar which is used in 1.x release is replaced by [ erlang.mk ](https://erlang.mk) and [ relx ](https://github.com/erlware/relx) tools since 2.0-beta1 release.

You can check the 'Makefile' and 'relx.config' in the release project of the borker: [ emqttd-relx ](https://github.com/emqtt/emqttd-relx) .

### Improve Git Branch Management

| stable     | 1.x Stable Branch       |
| ---------- | ----------------------- |
| master     | 2.x Master Branch       |
| emq10      | 1.x Developement Branch |
| emq20      | 2.x Development Branch  |
| emq30      | 3.x Development Branch  |
| issue#{id} | BugFix Branch           |

### New Config Syntax

Since 2.0-beta1 release the configuration file of the broker and plugins adopt a new syntax like rebar.config and relx.config:

etc/emqttd.conf for example:

    %% Max ClientId Length Allowed.
    {mqtt_max_clientid_len, 512}.

    %% Max Packet Size Allowed, 64K by default.
    {mqtt_max_packet_size, 65536}.

    %% Client Idle Timeout.
    {mqtt_client_idle_timeout, 30}. % Second

### MQTT-SN Protocol Plugin

The MQTT-SN Protocol Plugin [ emqttd_sn ](https://github.com/emqtt/emqttd_sn) has been ready in 2.0-beta1 release. The default UDP port of MQTT-SN is 1884.

Load the plugin:

    ./bin/emqttd_ctl plugins load emqttd_sn

### Improve the PubSub Design

![image](./_static/images/publish.png)

### Improve the Plugin Management

The plugin of EMQ 2.0 broker is a normal erlang application which depends on and extends 'emqttd'. You can create a standalone plugin application project, and add it to [ emqttd-relx ](https://github.com/emqtt/emqttd-relx) Makefile as a DEP.

All the plugins' config files will be copied to emqttd/etc/plugins/ folder when making emqttd brinary packages in [ emqttd-relx ](https://github.com/emqtt/emqttd-relx) project:

    ▾ emqttd/
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

### EMQ 2.0 Documentation

[ https://developer.emqx.io/docs/emq/v2/en/index.html ](https://developer.emqx.io/docs/emq/v2/en/index.html)

## Version 1.1.3

_Release Date: 2016-08-19_

Support './bin/emqttd_ctl users list' CLI (#621)

Cannot publish payloads with a size of the order 64K using WebSockets (#643)

Optimize the procedures that retrieve the Broker version and Borker description in the tick timer (PR#627)

Fix SSL certfile, keyfile config (#651)

## Version 1.1.2

## Version 1.1.2

_Release Date: 2016-06-30_

Upgrade mysql-otp driver to 1.2.0 (#564, #523, #586, #596)

Fix WebSocket Client Leak (PR #612)

java.io.EOFException using paho java client (#551)

Send message from paho java client to javascript client (#552)

Compatible with the Qos0 PUBREL packet (#575)

Empty clientId with non-clean session accepted (#599)

Update docs to fix typos (#601, #607)

## Version 1.1.1

_Release Date: 2016-06-04_

Compatible with the Qos0 PUBREL packet (#575)

phpMqtt Client Compatibility (#572)

java.io.EOFException using paho java client (#551)

## Version 1.1

_Release Date: 2016-06-01_

### Highlights

Upgrade eSockd library to 4.0 and Support IPv6

Support to listen on specific IP Address:

    {mqtt, {"192.168.1.20", 1883}, [
        ...
    ]},

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

Authentication/ACL by HTTP API: [ https://github.com/emqtt/emqttd_auth_http ](https://github.com/emqtt/emqttd_auth_http)

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

## Version 1.0.3

_Release Date: 2016-05-23_

eSockd 3.2

MochiWeb 4.0.1

## Version 1.0.2

_Release Date: 2016-05-04_

Issue#534 - './bin/emqttd_ctl vm' - add 'port/count', 'port/limit' statistics

Issue#535 - emqttd_client should be terminated properly even if exception happened when sending data

PR#519 - The erlang '-name' requires the fully qualified host name

emqttd_reloader plugin - help reload modified modules during development.

## Version 1.0.1

_Release Date: 2016-04-16_

PR#515 - Fix '$queue' pubsub, add 'pubsub_queue' test and update docs

## Version 1.0 (The Seven Mile Journey)

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

[ https://developer.emqx.io/docs ](https://developer.emqx.io/docs)

[ http://docs.emqtt.com/ ](http://docs.emqtt.com/)

### Thanks

Thank Ericsson for the Great Erlang/OTP Platform ( [ http://erlang.org/ ](http://erlang.org/) )!

Contributors on GitHub: @callbay @lsxredrain @hejin1026 @desoulter @turtleDeng @Hades32 @huangdan @phanimahesh @dvliman @Prots @joaohf

Partners: EACG ( [ http://eacg.de/ ](http://eacg.de/) )

Favorite Band: The Seven Mile Journey ( [ http://www.thesevenmilejourney.dk/ ](http://www.thesevenmilejourney.dk/) )

## Version 0.17.1-beta

_Release Date: 2016-03-22_

### Enhancements

Time unit of session 'expired_after' changed to minute. (#479)

### Dashboard

Code Review and improve the design of Dashboard.

## Version 0.17.0-beta

_Release Date: 2016-03-15_

### Highlights

Installation and Configuration Guide released on [ http://docs.emqtt.com ](http://docs.emqtt.com)

Improve and Consolidate the design of Hook, Server, PubSub and Router

Upgrade the [Web Dashboard]( [ https://github.com/emqtt/emqttd_dashboard ](https://github.com/emqtt/emqttd_dashboard) ) to support pagination

Bridge emqttd broker to another emqttd broker & emqttd to mosquitto bridge (#438)

### Enhancements

emqttd_ctl: better error message (#450)

./bin/emqttd_ctl: add 'routes' command:

    routes list             # List all routes
    routes show <Topic>     # Show a route

Add 'backend_subscription' table and support static subscriptions (emqttd_backend)

Add 'retained_message' table and refactor emqttd_retainer module (emqttd_backend)

A New Hook and Callback Design (emqttd_hook)

Add PubSub, Hooks APIs to emqttd module (emqttd)

Move start_listeners/0, stop_listeners/0 APIs to emqttd_app module (emqttd_app)

### Tests

Add 100+ common test cases.

### Plugins

Upgrade Dashboard, Redis, Stomp and Template Plugins

## Version 0.16.0-beta

_Release Date: 2016-02-16_

### Highlights

Licensed under the Apache License, Version 2.0 Now.

Improve the design of cluster, support to join or leave the cluster (#449):

    $ ./bin/emqttd_ctl cluster
    cluster join <Node>                     #Join the cluster
    cluster leave                           #Leave the cluster
    cluster remove <Node>                   #Remove the node from cluster
    cluster status                          #Cluster status

Improve the design of Trie and Route, only the wildcard topics stored in Trie.

Common Test to replace EUnit.

### Enhancements

mqtt_message record: add 'sender' field (#440)

refactor the emqttd, emqttd_time, emqttd_opts, emqttd_node modules.

### Bugfix

noproc error when call to gen_server2:call(false, {add_route,Topic,\<0.685.0>}, infinity) (#446)

### Plugins

Changed the license of all plugins.

## Version 0.15.0-beta

_Release Date: 2016-01-31_

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

## Version 0.14.1-beta

_Release Date: 2015-12-28_

Bugfix: emqttd_ws_client.erl: Unexpected Info: {'EXIT',\<0.27792.18>,{shutdown,destroy}} (#413)

Improve: fix spec errors found by dialyzer

## Version 0.14.0-beta

_Release Date: 2015-12-18_

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

## Version 0.13.1-beta

_Release Date: 2015-11-28_

Bugfix: Plugin pathes error under windows (#387)

Improve: Too many error logs "[error] Session ..... Unexpected EXIT: client_pid=\<0.14137.35>, exit_pid=\<0.30829.22>, reason=nop..." (#383)

Improve: Define QOS0/1/2, Pooler Error (PR#382)

Improve: High CPU load when 400K unstable mobile connections (#377)

BugFix: emqttd_plugin_pgsql - error using same query with latest update plugin (pgsql#5)

## Version 0.13.0-beta

_Release Date: 2015-11-08_

### Highlights

Rate Limiting based on [Token Bucket]( [ https://en.wikipedia.org/wiki/Token_bucket ](https://en.wikipedia.org/wiki/Token_bucket) ) and [Leaky Bucket]( [ https://en.wikipedia.org/wiki/Leaky_bucket#The_Leaky_Bucket_Algorithm_as_a_Meter ](https://en.wikipedia.org/wiki/Leaky_bucket#The_Leaky_Bucket_Algorithm_as_a_Meter) ) Algorithm

Upgrade eSockd and MochiWeb libraries to support Parameterized Connection Module

Improve emqttd_client to support fully asynchronous socket networking

### Enhancements

Protocol Compliant - Session Present Flag (#163)

Compilation fails if repo is cloned with a different name (#348)

emqttd_client: replace gen_tcp:send with port_command (#358)

TCP sndbuf, recbuf, buffer tuning (#359)

emqttd_client.erl to handle 'inet_async', 'inet_reply' properly (#360)

Refator the [client/session management design]( [ https://github.com/emqtt/emqttd/blob/master/doc/design/ClientSession.md ](https://github.com/emqtt/emqttd/blob/master/doc/design/ClientSession.md) )

### Bugfix

Cannot kick transient client out when clientId collision (#357)

Fix the order of emqttd_app:start_server/1 (#367)

emqttd\_ [ session:subscribe/2 ](session:subscribe/2) will crash (#374)

### Benchmark

[benchmark for 0.13.0 release]( [ https://github.com/emqtt/emqttd/wiki/benchmark-for-0.13.0-release ](https://github.com/emqtt/emqttd/wiki/benchmark-for-0.13.0-release) )

3.1G memory and 50+ CPU/core:

    Connections: 250K
    Subscribers: 250K
    Topics:      50K
    Qos1 Messages/Sec In:  4K
    Qos1 Messages/Sec Out: 20K
    Traffic In(bps):  12M+
    Traffic Out(bps): 56M+

## Version 0.12.3-beta

_Release Date: 2015-10-22_

Bugfix: emqttd_sysmon crasher for 'undefined' process_info (#350)

Bugfix: emqttd_client: catch parser exception (#353)

## Version 0.12.2-beta

_Release Date: 2015-10-16_

Bugfix: Retained messages should not be expired if 'broker.retained.expired_after = 0' (#346)

## Version 0.12.1-beta

_Release Date: 2015-10-15_

Highlight: Release for Bugfix and Code Refactor.

Feature: Retained message expiration (#182)

Improve: '$SYS/#' publish will not match '#' or '+/#' (#68)

Improve: Add more metrics and ignore '$SYS/#' publish (#266)

Improve: emqttd_sm should be optimized for clustered nodes may be crashed (#282)

Improve: Refactor emqttd_sysmon and suppress 'monitor' messages (#328)

Task: benchmark for 0.12.0 release (#225)

Benchmark: About 900K concurrent connections established on a 20Core, 32G CentOS server.

## Version 0.12.0-beta

_Release Date: 2015-10-08_

### Highlights

Enhance the **emqttd_ctl** module to allow plugins to register new commands (#256)

Add [emqttd_recon plugin]( [ https://github.com/emqtt/emqttd_recon ](https://github.com/emqtt/emqttd_recon) ) to debug/optimize the broker (#235)

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

Write a new [benchmark tool]( [ https://github.com/emqtt/emqtt_benchmark ](https://github.com/emqtt/emqtt_benchmark) ) to benchmark this release

Hw requirements - 5K users, 25-50 msgs/sec, QoS=1 (#209)

Supported Number of Connections Greatly Reduced When Clients are Subscribing (#324)

## Version 0.11.0-beta

_Release Date: 2015-09-25_

Highlight: Rebar to manage plugin dependencies.

Highlight: [Stomp]( [ https://github.com/emqtt/emqttd_stomp ](https://github.com/emqtt/emqttd_stomp) ) and [SockJS]( [ https://github.com/emqtt/emqttd_sockjs ](https://github.com/emqtt/emqttd_sockjs) ) Plugins!

Improve: add rel/files/emqttd.config.development|production.

Improve: rel/reltool.config.script to release deps of plugin.

Improve: persist mnesia schema on slave nodes.

Improve: use timer:seconds/1 api.

Improve: The binary release will be compiled with R18.1 now.

Bugfix: issue#306 - emqttd_cm should unregister the duplicated client

Bugfix: issue#310 - usage of emqttd_ctl error: 'session list' should be 'sessions list'

Bugfix: issue#311 - './bin/emqttd_ctl sessions list' error

Bugfix: issue#312 - unsubcribe will lead to crash if emqttd_plugin_template plugin loaded

## Version 0.10.4-beta

_Release Date: 2015-09-18_

Optimize session management and upgrade eSockd library to 2.7.1

[Benchmark for 0.10.4 release]( [ https://github.com/emqtt/emqttd/wiki/benchmark-for-0.10.4-release ](https://github.com/emqtt/emqttd/wiki/benchmark-for-0.10.4-release) )

Improve: issue#294 - [error] failed to start connection on 0.0.0.0:1883 - enotconn

Improve: issue#297 - How do I allow user with some pattern to access topic with some pattern?

Bugfix: issue#291 - "./bin/emqttd attach ..." cannot work

Bugfix: issue#284 - Should not use erlang:list_to_atom/1 in emqttd_vm.erl

## Version 0.10.3-beta

_Release Date: 2015-08-30_

Bugfix: issue#271 - add emqttd_ws_client:subscribe/2 function

Bugfix: issue#269 - bin/emqttd Syntax error on ubuntu

Improve: issue#265 - client under unstable mobile network generate a lot of logs

## Version 0.10.2-beta

_Release Date: 2015-08-26_

Improve: issue#257 - After the node name changed, the broker cannot restart for mnesia schema error.

## Version 0.10.1-beta

_Release Date: 2015-08-25_

Bugfix: issue#259 - when clustered the emqttd_dashboard port is close, and the 'emqttd' application cannot stop normally.

Feature: issue#262 - Add ' [ http://host:8083/mqtt/status ](http://host:8083/mqtt/status) ' Page for health check

## Version 0.10.0-beta

_Release Date: 2015-08-20_

[Web Dashboard]( [ https://github.com/emqtt/emqttd_dashboard ](https://github.com/emqtt/emqttd_dashboard) ) and [MySQL]( [ https://github.com/emqtt/emqttd_plugin_mysql ](https://github.com/emqtt/emqttd_plugin_mysql) ), [PostgreSQL]( [ https://github.com/emqtt/emqttd_plugin_pgsql ](https://github.com/emqtt/emqttd_plugin_pgsql) ) Authentication/ACL Plugins!

Highlight: Web Dashboard to monitor Statistics, Metrics, Clients, Sessions and Topics of the broker.

Highlight: JSON/HTTP API to query all clients connected to broker.

Highlight: A new [Plugin Design]( [ https://github.com/emqtt/emqttd/wiki/Plugin%20Design ](https://github.com/emqtt/emqttd/wiki/Plugin%20Design) ) and a [Template project]( [ https://github.com/emqtt/emqttd_plugin_template ](https://github.com/emqtt/emqttd_plugin_template) ) for plugin development.

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

## Version 0.9.3-alpha

_Release Date: 2015-07-25_

Wiki: [Bridge]( [ https://github.com/emqtt/emqttd/wiki/Bridge ](https://github.com/emqtt/emqttd/wiki/Bridge) )

Improve: emqttd_protocol.hrl to define 'QOS_I'

Improve: emqttd_pubsub to add subscribe/2 API

Improve: ./bin/emqttd_ctl to support new bridges command

Bugfix: issue #206 - Cannot bridge two nodes

## Version 0.9.2-alpha

_Release Date: 2015-07-18_

Improve: issue #196 - Add New Hook 'client.subscribe.after'

## Version 0.9.1-alpha

_Release Date: 2015-07-10_

Bugfix: issue #189 - MQTT over WebSocket(SSL) cannot work?

Bugfix: issue #193 - 'client.ack' hook should be renamed to 'message.acked', and called by emqttd_broker:foreach_hooks

## Version 0.9.0-alpha

_Release Date: 2015-07-09_

[Session, Queue, Inflight Window, Hooks, Global MessageId and More Protocol Compliant]( [ https://github.com/emqtt/emqttd/releases/tag/0.9.0-alpha ](https://github.com/emqtt/emqttd/releases/tag/0.9.0-alpha) ) Now!

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

Improve: issue #170 - $SYS Topics should support alarms.

Improve: issue #169 - Add More [Hooks]( [ https://github.com/emqtt/emqttd/wiki/Hooks-Design ](https://github.com/emqtt/emqttd/wiki/Hooks-Design) )

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

## Version 0.8.6-beta

_Release Date: 2015-06-17_

Bugfix: issue #175 - publish Will message when websocket is closed without 'DISCONNECT' packet

## Version 0.8.5-beta

_Release Date: 2015-06-10_

Bugfix: issue #53 - client will receive duplicate messages when overlapping subscription

## Version 0.8.4-beta

_Release Date: 2015-06-08_

Bugfix: issue #165 - duplicated message when publish 'retained' message to persistent client

## Version 0.8.3-beta

_Release Date: 2015-06-05_

Bugfix: issue #158 - should queue:in new message after old one dropped

Bugfix: issue #155 - emqtt_parser.erl: parse_topics/3 should reverse topics

Bugfix: issue #149 - Forget to merge plugins/emqttd_auth_mysql from 'dev' branch to 'master' in 0.8.x release

## Version 0.8.2-alpha

_Release Date: 2015-06-01_

Bugfix: issue #147 - WebSocket client cannot subscribe queue '$Q/queue/${clientId}'

Bugfix: issue #146 - emqttd_auth_ldap: fill(Username, UserDn) is not right

## Version 0.8.1-alpha

_Release Date: 2015-05-28_

Client [Presence]( [ https://github.com/emqtt/emqttd/wiki/Presence ](https://github.com/emqtt/emqttd/wiki/Presence) ) Support and [\$SYS Topics]( [ https://github.com/emqtt/emqttd/wiki/$SYS-Topics ](https://github.com/emqtt/emqttd/wiki/\$SYS-Topics) ) Redesigned!

Bugfix: issue #138 - when client disconnected normally, broker will not publish disconnected $SYS message

Bugfix: fix websocket url in emqttd/priv/www/websocket.html

Improve: etc/emqttd.config to allow websocket connections from any hosts

Improve: rel/reltool.config to exclude unnecessary apps.

## Version 0.8.0-alpha

_Release Date: 2015-05-25_

[Hooks]( [ https://github.com/emqtt/emqttd/wiki/Hooks%20Design ](https://github.com/emqtt/emqttd/wiki/Hooks%20Design) ), Modules and [Plugins]( [ https://github.com/emqtt/emqttd/wiki/Plugin%20Design ](https://github.com/emqtt/emqttd/wiki/Plugin%20Design) ) to extend the broker Now!

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

## Version 0.7.1-alpha

_Release Date: 2015-05-04_

Add doc/design/_ and merge doc/_ to github Wiki

Bugfix: issue #121 - emqttd cluster issuse

Bugfix: issue #123 - emqttd:unload_all_plugins/0 cannot unload any plugin

Bugfix: fix errors found by dialyzer

## Version 0.7.0-alpha

_Release Date: 2015-05-02_

[MQTT over WebSocket(SSL)]( [ https://github.com/emqtt/emqttd/wiki/MQTT-Over-WebSocket ](https://github.com/emqtt/emqttd/wiki/MQTT-Over-WebSocket) ) Now!

[Plugin Achitecture]( [ https://github.com/emqtt/emqttd/wiki/Plugin%20Design ](https://github.com/emqtt/emqttd/wiki/Plugin%20Design) ) based on OTP application

[Trace MQTT Packets or Messages]( [ https://github.com/emqtt/emqttd/wiki/Trace%20Design ](https://github.com/emqtt/emqttd/wiki/Trace%20Design) ) to log files

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

## Version 0.6.2-alpha

_Release Date: 2015-04-24_

Bugfix: critical issue #54, #104, #106 - error when resume session

Improve: add emqttd_cm_sup module, and use 'hash' gproc_pool to register/unregister client ids

Improve: kick old client out when session is duplicated.

Improve: move mnesia dir config from etc/app.config to etc/vm.args

## Version 0.6.1-alpha

_Release Date: 2015-04-20_

Integrate with [gproc library]( [ https://github.com/uwiger/gproc ](https://github.com/uwiger/gproc) ) to support pool

Feature: issues#91 - should use worker_pool to handle some async work?

Feature: issues#95 - Topic filters in ACL rule should support 'eq' tag

Improve: issues#84 - emqttd_pubsub is redesigned again to protect mnesia transaction

Improve: issues#74 - ACL Support and update [ACL Design Wiki]( [ https://github.com/emqtt/emqttd/wiki/ACL-Design ](https://github.com/emqtt/emqttd/wiki/ACL-Design) )

## Version 0.6.0-alpha

_Release Date: 2015-04-17_

ACL Support Now: [ACL-Design Wiki]( [ https://github.com/emqtt/emqttd/wiki/ACL-Design ](https://github.com/emqtt/emqttd/wiki/ACL-Design) )

Authentication with username, clientid Now: [Authentication Wiki]( [ https://github.com/emqtt/emqttd/wiki/Authentication ](https://github.com/emqtt/emqttd/wiki/Authentication) )

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

## Version 0.5.5-beta

_Release Date: 2015-04-09_

Bugfix: issue #75 - careless about function name when emqttd_pubsub handle getstats message.

Bugfix: issue #79 - cannot find topic_subscriber table after cluster with other nodes.

## Version 0.5.4-alpha

_Release Date: 2015-03-22_

Benchmark this release on a ubuntu/14.04 server with 8 cores, 32G memory from QingCloud.com:

    200K Connections,
    30K Messages/Sec,
    20Mbps In/Out Traffic,
    200K Topics,
    200K Subscribers,

    Consumed 7G memory, 40% CPU/core

Benchmark code: [ https://github.com/emqtt/emqttd_benchmark ](https://github.com/emqtt/emqttd_benchmark)

Change: rewrite emqttd_pubsub to handle more concurrent subscribe requests.

Change: ./bin/emqttd_ctl add 'stats', 'metrics' commands.

Bugfix: issue #71, #72

## Version 0.5.3-alpha

_Release Date: 2015-03-19_

Bugfix: issues#72 - emqttd_cm, emqtt_sm ets:match_delete/2 with wrong pattern

## Version 0.5.2-alpha

_Release Date: 2015-03-18_

Change: upgrade esockd to 2.1.0-alpha, do not tune socket buffer for mqtt connection.

## Version 0.5.1-alpha

_Release Date: 2015-03-13_

Change: upgrade esockd to v1.2.0-beta, rename 'acceptor_pool' to 'acceptors'

## Version 0.5.0-alpha

_Release Date: 2015-03-12_

RENAME 'emqtt' to 'emqttd'!

Support [Broker Bridge]( [ https://github.com/emqtt/emqttd/wiki/Bridge-Design ](https://github.com/emqtt/emqttd/wiki/Bridge-Design) ) Now!

Change: rename project from 'emqtt' to 'emqttd'

Change: lager:debug to dump RECV/SENT packets

Feature: emqttd_bridge, emqttd_bridge_sup to support broker bridge

Feature: emqtt_event to publish client connected/disconnected message to $SYS topics

Feature: ./bin/emqttd_ctl add more commands: listeners, broker, bridges, start_bridge, stop_bridge...

Feature: issue#57 - support to configure max packet size

Feature: issue#68 - if sys_interval = 0, emqttd_broker will not publish messages to $SYS/brokers/#

Bugfix: issue#67 - subscribe '#' to receive all messages

Bugfix: issue#64 - emqtt_app start/2: should wait_for_databases

Test: emqttd_topic_tests add more '\_match_test'

## Version 0.4.0-alpha

_Release Date: 2015-03-10_

Support [$SYS Topics of Broker]( [ https://github.com/emqtt/emqttd/wiki/$SYS-Topics-of-Broker ](https://github.com/emqtt/emqttd/wiki/$SYS-Topics-of-Broker) ) Now!

Feature: emqtt_broker to publish version, uptime, datetime to $SYS/brokers/# topics

Feature: emqtt_broker to publish count of clients, sessions, suscribers to $SYS/brokers/# topics

Feature: emqtt_metrics to publish bytes, packets, messages metrics to $SYS/brokers/# topics

Feature: add include/emqtt_systop.hrl

Change: emqtt_cm to count current clients

Change: emqtt_sm to count current sessions

Change: emqtt_pubsub to count current topics and suscribers

Change: emqtt_pubsub to add create/1 API

Change: emqtt_pubsub dispatch/2 to return number of subscribers

Change: emqtt_pubsub to count 'dropped' messages

Change: emqtt_opts to add merge/2 function

Test: add emqtt_serialiser_tests.erl

## Version 0.3.4-beta

_Release Date: 2015-03-08_

Bugfix: emqtt_serialiser.erl cannot serialise UNSUBACK packets

## Version 0.3.3-beta

_Release Date: 2015-03-07_

Bugfix: emqtt_serialiser.erl cannot serialise PINGRESP issue#60

## Version 0.3.2-beta

_Release Date: 2015-03-05_

Improve: merge emqttc serialiser, parser, packet

Add: emqtt_opts to merge socket options

## Version 0.3.1-beta

_Release Date: 2015-03-02_

Feature: SSL Socket Support

Feature: issue#44 HTTP API should add Qos parameter

Bugfix: issue#52 emqtt_session crash

Bugfix: issue#53 sslsocket keepalive error

Upgrade: esockd to v0.2.0

Upgrade: mochiweb to v3.0.0

## Version 0.3.0-beta

_Release Date: 2015-01-19_

Feature: HTTP POST API to support 'qos', 'retain' parameters

Feature: $SYS system topics support

Change: Rewrite emqtt_topic.erl, use '', '#', '+' to replace <<"">>, <<"#">>, <<"+">>

Change: fix emqtt_pubsub.erl to match '#', '+'

Tests: emqtt_topic_tests.erl add more test cases

## Version 0.3.0-alpha

_Release Date: 2015-01-08_

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

## Version 0.2.1-beta

_Release Date: 2015-01-08_

pull request 26: Use binaries for topic paths and fix wildcard topics

emqtt_pubsub.erl: fix wildcard topic match bug caused by binary topic in 0.2.0

Makefile: deps -> get-deps

rebar.config: fix mochiweb git url

tag emqtt release accoding to [Semantic Versioning]( [ http://semver.org/ ](http://semver.org/) )

max clientId length is 1024 now.

## Version 0.2.0

_Release Date: 2014-12-07_

rewrite the project, integrate with esockd, mochiweb

support MQTT 3.1.1

support HTTP to publish message

## Version 0.1.5

_Release Date: 2013-01-05_

Bugfix: remove QOS_1 match when handle PUBREL request

Bugfix: reverse word in emqtt_topic:words/1 function

## Version 0.1.4

_Release Date: 2013-01-04_

Bugfix: fix "mosquitto_sub -q 2 ......" bug

Bugfix: fix keep alive bug

## Version 0.1.3

_Release Date: 2013-01-04_

Feature: Support QOS2 PUBREC, PUBREL, PUBCOMP messages

Bugfix: fix emqtt_frame to encode/decoe PUBREC/PUBREL messages

## Version 0.1.2

_Release Date: 2012-12-27_

Feature: release support like riak

Bugfix: use ?INFO/?ERROR to print log in tcp_listener.erl

## Version 0.1.1

_Release Date: 2012-09-24_

Feature: use rebar to generate release

Feature: support retained messages

Bugfix: send will msg when network error

## Version 0.1.0

_Release Date: 2012-09-21_

The first public release.
