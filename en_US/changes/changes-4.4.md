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

## Version 4.4.1

*Release Date: 2022-02-21*

This patch release only includes a CI change for the Windows package.

## Version 4.4.0

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

## Version 4.4-beta.1

*Release Date: 2021-12-21*

EMQX 4.4-beta.1 is released now, it mainly includes the following changes:

**Important changes:**

- Starting from 4.4, EMQX releases are named with Erlang/OTP release in the package name. e.g. `emqx-4.4.0-otp24.1.5-3-centos7-arm64.rpm`

- **For Debian/Ubuntu users**, Debian/Ubuntu package (deb) installed EMQX now now run on systemd. This is to use systemd's supervision functionality to ensure that EMQX service restarts after a crash. The package installation service upgrade from init.d to systemd has been verified, but it is still recommended that you verify and confirm again before deploying to the production environment, at least to ensure that systemd is available in your system

- MongoDB authentication supports DNS SRV and TXT Records resolution, which can seamlessly connect with MongoDB Altas

- Support dynamic modification of MQTT Keep Alive to adapt to different energy consumption strategies

- Support 4.3 to 4.4 rolling upgrade of clustered nodes. See upgrade document for more dtails.

- TLS for cluster backplane (RPC) connections. See [clustering document](../advanced/cluster.md#using-tls-for-backplane-connections) for details.

**Minor changes:**

- Dashboard supports relative paths and custom access paths

- Supports configuring whether to forward retained messages with empty payload to suit users who are still using MQTT v3.1. The relevant configurable item is `retainer.stop_publish_clear_msg`

- Multi-language hook extension (ExHook) supports dynamic cancellation of subsequent forwarding of client messages

- Rule engine SQL supports the use of single quotes in FROM clause, for example: `SELECT * FROM't/#'`

- Change the default value of the `max_topic_levels` configurable item to 128. Previously, it had no limit (configured to 0), which may be a potential DoS threat

- Improve the error log content when the Proxy Protocol message is received but the `proxy_protocol` configuration is not turned on

- Add additional message attributes to the message reported by the gateway. Messages from gateways such as CoAP, LwM2M, Stomp, ExProto, etc., when converted to EMQX messages, add fields such as protocol name, protocol version, user name, client IP, etc., which can be used for multi-language hook extension (ExHook)

- HTTP client performance improvement

- Add openssl-1.1 to RPM dependency

**Bug fixes:**

- Various RPC timeouts or even indefinite hangs, e.g. client becomes unresponsive due to the a bug in [Erlang/OTP](#https://github.com/erlang/otp/issues/5346) which is triggered under high load

- Fix the issue that the lock management process `ekka_locker` crashes after killing the suspended lock owner

- Fix the issue that the Path parameter of WebHook action in rule engine cannot use the rule engine variable

- Fix MongoDB authentication module cannot use Replica Set mode and other issues

- Fix the issue of out-of-sequence message forwarding between clusters. The relevant configurable item is `rpc.tcp_client_num`

- Fix the issue of incorrect calculation of memory usage

- Fix MQTT bridge malfunction when remote host is unreachable (hangs the connection)

- Fix the issue that HTTP headers may be duplicated
