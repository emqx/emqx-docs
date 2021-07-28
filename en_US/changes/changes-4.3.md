---
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# Changes

## Version 4.3.6

*Release Date: 2021-07-28*

EMQ X 4.3.6 is released now, it mainly includes the following changes:

**Enhancement:**

- Support disable HTTP Pipelining

  Github PR: [emqx#5279](https://github.com/emqx/emqx/pull/5279)

- ACL supports IP address list

  Github PR: [emqx#5328](https://github.com/emqx/emqx/pull/5328)

## Version 4.3.5

*Release Date: 2021-06-28*

EMQ X 4.3.5 is released now, it mainly includes the following changes:

**Bug fixes:**

- Fix the issue that messages may be lost after canceling the subscription when multiple shared subscriptions are established by the same client

  Github PR: [emqx#5098](https://github.com/emqx/emqx/pull/5098)

## Version 4.3.4

*Release Date: 2021-06-23*

EMQ X 4.3.4 is released now, it mainly includes the following changes:

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

## Version 4.3.3

*Release Date: 2021-06-05*

EMQ X 4.3.3 is released now, it mainly includes the following changes:

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

## Version 4.3.2

*Release Date: 2021-05-27*

EMQ X 4.3.2 is released now, it mainly includes the following changes:

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

## Version 4.3.1

*Release Date: 2021-05-14*

EMQ X 4.3.1 is released now, it mainly includes the following changes:

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

## Version 4.3.0

*Release Date: 2021-05-06*

EMQ X 4.3.0 is released now, it mainly includes the following changes:

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

- Protect EMQ X Broker from cross-site WebSocket hijacking attacks
- SSL supports `verify` and `server_name_indication` configuration
- Support the configuration of the maximum length of the certificate chain and the password of the private key file
- Use TLS v1.3 by default, TLS v1.3 configs has no effect if started on OTP 22
- JWT authentication supports JWKS

#### Other

- Added update resource functionality for rule engine
- Rule engine SQL function supports conversion between unix timestamp and rfc3339 format time
- Keep retrying the resources that failed to connect after the EMQ X Broker is started
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
- Certificate upload from dashboard has been made available for open-source edition (previously only for enterprise edition)

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
- EMQ X may fail to start in Docker
- Fixed delayed connection process OOM kill
- The MQTT-SN connection with Clean Session being false did not publish a will message when it was disconnected abnormally

## Version 4.3-rc.5

*Release Date: 2021-04-26*

EMQ X 4.3-rc.5 is released now, it mainly includes the following changes:

### emqx

**Enhancement:**

- Improve wildcard subscription performance

  Github Issue: [emqx#2985](https://github.com/emqx/emqx/issues/2985)
  Github PR: [emqx#4645](https://github.com/emqx/emqx/pull/4645)

- Support single-line log output, and support rfc3339 time format

  Github PR: [emqx#4656](https://github.com/emqx/emqx/pull/4656)

- Support routing table compression, reduce memory usage, enhance subscription performance, publishing performance will be slightly affected, so disable option is provided

  Github PR: [emqx##4628](https://github.com/emqx/emqx/pull/4628)

- Rule engine SQL function supports conversion between unix timestamp and rfc3339 format time

  Github PR: [emqx#4639](https://github.com/emqx/emqx/pull/4639)

**Bug fixes:**

- Fix the issue that EMQ X may fail to start in Docker

  Github PR: [emqx#4670](https://github.com/emqx/emqx/pull/4670), [emqx#4675](https://github.com/emqx/emqx/pull/4675), [emqx#4657](https://github.com/emqx/emqx/pull/4657)

- When the rule engine resource is not initialized successfully, the corresponding rule status is set to unavailable

  Github Issue: [emqx#4642](https://github.com/emqx/emqx/issues/4642)
  Github PR: [emqx#4643](https://github.com/emqx/emqx/pull/4643)

- Fix the issue caused by reporting telemetry data when EMQ X is not fully started

  Github PR: [emqx#4627](https://github.com/emqx/emqx/pull/4627)

- Fix the issue that the HTTPS certificate must be configured to load `emqx-exhook` plugin

  Github PR: [emqx#4678](https://github.com/emqx/emqx/pull/4678)

## Version 4.3-rc.4

*Release Date: 2021-04-16*

EMQ X 4.3-rc.4 is released now, it mainly includes the following changes:

### emqx

**Enhancement:**

- Redis sentinel mode supports SSL connection

  Github PR: [emqx#4553](https://github.com/emqx/emqx/pull/4553)

- WebSocket connection supports obtaining real IP and Port

  Github PR: [emqx#4558](https://github.com/emqx/emqx/pull/4558)

- Support cluster metrics for Prometheus

  Github Issue: [emqx#4548](https://github.com/emqx/emqx/pull/4548)
  Github PR: [emqx#4572](https://github.com/emqx/emqx/pull/4572)

**Bug fixes:**

- Fix the issue of inflight in MQTT bridge

  Github Issue: [emqx#3629](https://github.com/emqx/emqx/issues/3629)
  Github PR: [emqx#4513](https://github.com/emqx/emqx/pull/4513), [emqx#4526](https://github.com/emqx/emqx/pull/4526)
  
- Fix the issue that the multi-language extension hook cannot handle the false value returned

  Github PR: [emqx#4542](https://github.com/emqx/emqx/pull/4542)

- Start modules by default to prevent modules from not working properly after the cluster

  Github PR: [emqx#4547](https://github.com/emqx/emqx/pull/4547)

- Fix the issue that the authentication data cannot be imported

  Github PR: [emqx#4582](https://github.com/emqx/emqx/pull/4582), [emqx#4528](https://github.com/emqx/emqx/pull/4528)

- Fix the issue that the authentication data cannot be imported

  Github PR: [emqx#4563](https://github.com/emqx/emqx/pull/4563)

- Fix the issue that the MQTT-SN gateway will discard DISCONNECT packets in sleep mode

  Github Issue: [emqx#4506](https://github.com/emqx/emqx/issues/4506)
  Github PR: [emqx#4515](https://github.com/emqx/emqx/pull/4515)

- Fix the issue that the LwM2M gateway encodes and decodes numbers into unsigned integers

  Github Issue: [emqx#4499](https://github.com/emqx/emqx/issues/4499)
  Github PR: [emqx#4500](https://github.com/emqx/emqx/pull/4500)

- Fix the issue that some HTTP APIs are unavailable

  Github Issue: [emqx#4472](https://github.com/emqx/emqx/issues/4472)
  Github PR: [emqx#4503](https://github.com/emqx/emqx/pull/4503)

## Version 4.3-rc.3

*Release Date: 2021-03-30*

EMQ X 4.3-rc.3 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Limit the maximum effective size of the flight window to 65535

  Github PR: [emqx#4436](https://github.com/emqx/emqx/pull/4436)

- Fix the issue that the value of the `Keep Alive` field in the Dashboard is not synchronized when `Server Keep Alive` was in effect

  Github PR: [emqx#4444](https://github.com/emqx/emqx/pull/4444)

- Quickly kill the connection process when OOM

  Github PR: [emqx#4451](https://github.com/emqx/emqx/pull/4451)

- Fix the issue that `emqx start` reports timeout but the service has actually started

  Github PR: [emqx#4449](https://github.com/emqx/emqx/pull/4449)

- Fix the issue that the sleep mode of MQTT-SN is unavailable

  Github PR: [emqx#4435](https://github.com/emqx/emqx/pull/4435)

## Version 4.3-rc.2

*Release Date: 2021-03-26*

EMQ X 4.3-rc.2 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue that `emqx` and `emqx_ctl` commands are not available in some cases

  Github PR: [emqx#4430](https://github.com/emqx/emqx/pull/4430)

## Version 4.3-rc.1

*Release Date: 2021-03-23*

EMQ X 4.3-rc.1 is released now, it mainly includes the following changes:

### emqx

**Enhancement:**

- Added observer_cli

  Github PR: [emqx#4323](https://github.com/emqx/emqx/pull/4323)

- Support clearing all ACL cache

  Github PR: [emqx#4361](https://github.com/emqx/emqx/pull/4361)

- SSL supports `verify` and `server_name_indication` configuration

  Github PR: [emqx#4349](https://github.com/emqx/emqx/pull/4349)

**Bug fixes:**

- Fix issues caused by subject rewriting and ACL execution order

  Github Issue: [emqx#4200](https://github.com/emqx/emqx/issues/4200)
  Github PR: [emqx#4331](https://github.com/emqx/emqx/pull/4331)

- Fix MQTT packet receiving count issue

  Github PR: [emqx#4371](https://github.com/emqx/emqx/pull/4371)

- Fix the processing of heartbeat packets

  Github Issue: [emqx#4370](https://github.com/emqx/emqx/issues/4370)
  Github PR: [emqx#4371](https://github.com/emqx/emqx/pull/4371)

- Fix the issue that the default SSL Ciphers contains Ciphers that are not supported by OTP 22, which causes the startup failure after compiling with OTP 22

  Github PR: [emqx#4377](https://github.com/emqx/emqx/pull/4377)

## Version 4.3-beta.1

*Release Date: 2021-03-03*

EMQ X 4.3-beta.1 is released now, it mainly includes the following changes:

### emqx

**Enhancement:**

- Reduce the performance loss when opening the rule engine plugin

  Github PR: [emqx#4160](https://github.com/emqx/emqx/pull/4160)

- Only enable data telemetry in the official version

  Github PR: [emqx#4163](https://github.com/emqx/emqx/pull/4163)

- Support restarting the listener

  Github PR: [emqx#4188](https://github.com/emqx/emqx/pull/4188), [emqx#4190](https://github.com/emqx/emqx/pull/4190)

- Disable the rules while destroying the resources occupied by the action

  Github PR: [emqx#4232](https://github.com/emqx/emqx/pull/4232)

- The starting point is randomly selected when the shared subscription distribution strategy is configured as `round_robin`

  Github PR: [emqx#4232](https://github.com/emqx/emqx/pull/4232)

- Allow to use Base64 encoded client certificate or MD5 value of client certificate as username or Client ID

  Github PR: [emqx#4194](https://github.com/emqx/emqx/pull/4194)

- Keep retrying the resources that failed to connect after the EMQ X Broker is started

  Github PR: [emqx#4125](https://github.com/emqx/emqx/pull/4125)

**Bug fixes:**

- Fix the issue that the long Client ID cannot be tracked

  Github PR: [emqx#4163](https://github.com/emqx/emqx/pull/4163)

- Fix the issue that the query client information may crash

  Github PR: [emqx#4124](https://github.com/emqx/emqx/pull/4124)

## Version 4.3-alpha.1

*Release Date: 2021-01-29*

EMQ X 4.3-alpha.1 is released now, it mainly includes the following changes:

*Features*

- Added update resource logic for rule engine
- Enhance Webhook and HTTP authentication performance
- The underlying implementation of the multi-language extension function is changed from erlport to gRPC
- Protect EMQ X Broker from cross-site WebSocket hijacking attacks
- Project adjusted to umbrella structure
- Solve the problem that the nodes in the cluster environment must be started in the first startup sequence, otherwise you need to wait for the front node to start
- Websocket listener supports selecting supported subprotocols from the subprotocols list
- Support the default authentication method caching_sha2_password of MySQL 8.0
- JWT authentication supports JWKS
- Support the configuration of the maximum length of the certificate chain and the password of the private key file
- Support import and export of Authentication & ACL information in Mnesia
- Shared subscription supports Hash distribution of messages by source topic
- EMQ X Broker in docker now starts in foreground mode

*BUG*

- Fix the issue that ekka_locker's memory may grow infinitely under extreme conditions
- Fix the issue that the `max_inflight_size` configuration item in the MQTT bridge function does not take effect
- Fix the issue that ACL configuration in CoAP connection does not take effect
- Fix the issue that CoAP clients using the same ClientID can access at the same time
- Fix the issue of incorrect calculation of alarm duration
- Fix the issue that the MySQL authentication SSL/TLS connection function is not available
- Fixed the error of indicator statistics in the MQTT bridge function and the problem of multiple unit conversions in the `retry_interval` field
- Fix Redis reconnection failure

*Others*

- Support Erlang/OTP 23
- The new installation package only supports macOS 10.14 and above
