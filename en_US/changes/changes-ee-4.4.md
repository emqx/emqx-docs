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

## Version 4.4.1

*Release Date: 2022-02-18*

NOTE: 4.4.1 is in sync with: 4.3.7.
The compare base of this change set is 4.4.0.

### Important changes

- The slow subscription feature is improved. It supports counting the time spent in the process of message transmission, and recording and displaying time-consuming clients and topics.
- A cluster-wide total connections calculation bug was fixed in in Enterprise edition 4.4.1. Previously only the individual node's local number of connections were checked against the max number of connections allowed by the license. After this fix, the total number of connections is aggregated cluster-wide every 5 seconds. An extra 10% overrun is allowed to compensate the delays in aggregation.
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
