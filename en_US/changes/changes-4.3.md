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

## Version 4.3-rc.3

*Release Date: 2020-03-30*

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

*Release Date: 2020-03-26*

EMQ X 4.3-rc.2 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue that `emqx` and `emqx_ctl` commands are not available in some cases

  Github PR: [emqx#4430](https://github.com/emqx/emqx/pull/4430)

## Version 4.3-rc.1

*Release Date: 2020-03-23*

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

*Release Date: 2020-03-03*

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

*Release Date: 2020-01-29*

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
