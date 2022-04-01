---
# 编写日期
date: 2021-12-21 09:32:21
# 作者 Github 名称
author: tigercl
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref:
---

# Changes
## Version 4.4.2

*Release Date: 2022-04-01*

### Important changes

- For Docker images, the configuration directory `/opt/emqx/etc` has been removed from the VOLUME list, making it easier for users to rebuild images with changed configurations.
- CentOS 7 Erlang runtime rebuilt on OpenSSL-1.1.1n (previously 1.0), prior to v4.3.13, EMQX will fail to handshake and trigger `malformed_handshake_data` exception when clients use certain cipher suites.
- CentOS 8 Erlang runtime system rebuilt on RockyLinux 8. `centos8` will remain in the package name for backward compatibility.

### Enhancement

- Windows package support for building on Erlang/OTP 24.
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

## Version 4.4.1

*Release Date: 2022-02-18*

NOTE: 4.4.1 is in sync with: 4.3.7.
The compare base of this change set is 4.4.0.

### Important changes

- A cluster-wide total connections calculation bug was fixed in in Enterprise edition 4.4.1. Previously only the individual node's local number of connections were checked against the max number of connections allowed by the license. After this fix, the total number of connections is aggregated cluster-wide every 5 seconds. An extra 10% overrun is allowed to compensate the delays in aggregation.
- The slow subscription feature is improved. It supports counting the time spent in the process of message transmission, and recording and displaying time-consuming clients and topics.
**Users planning to upgrade should be aware of the possibility that this change may cause clients to reach the license limit and not be able to connect.**
- Rules engine supports Lindorm database
- Support client-level message drop metrics
- Optimize online Trace log display on Dashboard, support syntax highlighting

### Minor changes

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

## Version 4.4.0

*Release Date: 2021-12-21*

EMQX Enterprise 4.4.0 mainly includes the following changes:

### Important changes

- Starting from 4.4, EMQX releases are named with Erlang/OTP release in the package name. e.g. `emqx-ee-4.4.0-otp24.1.5-3-centos7-arm64.rpm`

- **For Debian/Ubuntu users**, Debian/Ubuntu package (deb) installed EMQX now now run on systemd. This is to use systemd's supervision functionality to ensure that EMQX service restarts after a crash. The package installation service upgrade from init.d to systemd has been verified, but it is still recommended that you verify and confirm again before deploying to the production environment, at least to ensure that systemd is available in your system

- Rule engine InfluxDB integration adds support for InfluxDB v2 API, rule engine supports InfluxDB 2.0 and InfluxDB Cloud now

- Rule engine adds support for SAP Event Mesh

- Rule engine adds support for MatrixDB

- MongoDB integration supports DNS SRV and TXT Records resolution, which can seamlessly connect with MongoDB Altas

- Supports trace online, users can complete the tracking operation of the client and topic on the Dashboard, and view or download the trace log

- Supports slow subscription statistics, which can be used to find abnormal situations such as message blockage in the production environment in time

- Support dynamic modification of MQTT Keep Alive to adapt to different energy consumption strategies

- Support 4.3 to 4.4 rolling upgrade of clustered nodes. See upgrade document for more dtails.

- TLS for cluster backplane (RPC) connections. See [clustering document](../advanced/cluster.md#using-tls-for-backplane-connections) for details.

### Minor changes

- Dashboard supports viewing the number of active client connections

- Dashboard supports relative paths and custom access paths

- Dashboard remove tab navigation

- Support configuring whether to write integer data to InfluxDB as floating point type

- Supports configuring whether to forward retained messages with empty payload to suit users who are still using MQTT v3.1. The relevant configurable item is `retainer.stop_publish_clear_msg`

- Multi-language hook extension (ExHook) supports dynamic cancellation of subsequent forwarding of client messages

- Rule engine SQL supports the use of single quotes in FROM clause, for example: `SELECT * FROM't/#'`

- Optimize the use and interaction of the built-in access control file module

- Change the default value of the `max_topic_levels` configurable item to 128. Previously, it had no limit (configured to 0), which may be a potential DoS threat

- Improve the error log content when the Proxy Protocol message is received but the `proxy_protocol` configuration is not turned on

- Add additional message attributes to the message reported by the gateway. Messages from gateways such as CoAP, LwM2M, Stomp, ExProto, etc., when converted to EMQX messages, add fields such as protocol name, protocol version, user name, client IP, etc., which can be used for multi-language hook extension (ExHook)

- HTTP client performance improvement

- Add openssl-1.1 to RPM dependency

### Bug fixes

- Fix the issue that the client process becomes unresponsive due to the blockage of RPC calls between nodes

- Fix the issue that the lock management process `ekka_locker` crashes after killing the suspended lock owner

- Fix the issue of garbled data when writing data to RocketMQ asynchronously

- Fix the issue of inaccurate statistics of RocketMQ

- Fix the display error of the Dashboard monitoring page when the number of cluster nodes exceeds seven

- Fix the issue that the rule engine may have a higher failure rate when saving data to MySQL

- Fix the issue that the Clickhouse-based offline messaging feature of the rule engine is unavailable

- Fix the issue that the Max Returned Count option in the MongoDB-based offline message feature of the rule engine cannot be used

- Fix the issue that the Path parameter of WebHook action in rule engine cannot use the rule engine variable

- Fix MongoDB authentication module cannot use Replica Set mode and other issues

- Fix the issue of out-of-sequence message forwarding between clusters. The relevant configurable item is `rpc.tcp_client_num`

- Fix the issue of incorrect calculation of memory usage

- Fix MQTT bridge malfunction when remote host is unreachable (hangs the connection)

- Fix the issue that HTTP headers may be duplicated
