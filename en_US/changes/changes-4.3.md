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

## Version 4.3.15

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

## Version 4.3.14

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

## Version 4.3.13

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

## Version 4.3.12

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

## Version 4.3.11

*Release Date: 2021-12-17*

EMQX 4.3.11 is released now, it mainly includes the following changes:

### Enhancement

- Support the configuration of whether to continue to deliver empty retained messages to suit users who are still using the MQTT v3.1 protocol

### Bug fixes

- Fix the issue of incorrect calculation of memory usage
- Fix the issue that the Path option of Webhook Action in rule engine doesn't support the use of ${Variable}
- Fix the issue that the connection failure log will continue to be printed when stopping MQTT Bridge plugin in some cases

## Version 4.3.10

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

## Version 4.3.9

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

## Version 4.3.8

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

> Note: Starting from this version, CentoOS 7 requires the use of openssl 1.1.1. For the installation method of openssl upgrade, please refer to: [FAQ - Incorrect OpenSSL Version](https://docs.emqx.io/en/broker/v4.3/faq/error.html#incorrect-openssl-version)

## Version 4.3.7

*Release Date: 2021-08-09*

EMQX 4.3.7 is released now, it mainly includes the following changes:

**Bug fixes:**

- Fix the issue that the current HTTP KeepAlive behavior may cause some servers to disconnect

  Github PR: [emqx#5395](https://github.com/emqx/emqx/pull/5395)

- Fix the issue that the command line interface cannot print certain characters

  Github PR: [emqx#5411](https://github.com/emqx/emqx/pull/5411)

- Fix the issue of coding error when LwM2M gateway sends integer numbers

  Github PR: [emqx#5425](https://github.com/emqx/emqx/pull/5425)

## Version 4.3.6

*Release Date: 2021-07-28*

EMQX 4.3.6 is released now, it mainly includes the following changes:

**Enhancement:**

- Support disable HTTP Pipelining

  Github PR: [emqx#5279](https://github.com/emqx/emqx/pull/5279)

- ACL supports IP address list

  Github PR: [emqx#5328](https://github.com/emqx/emqx/pull/5328)

## Version 4.3.5

*Release Date: 2021-06-28*

EMQX 4.3.5 is released now, it mainly includes the following changes:

**Bug fixes:**

- Fix the issue that messages may be lost after canceling the subscription when multiple shared subscriptions are established by the same client

  Github PR: [emqx#5098](https://github.com/emqx/emqx/pull/5098)

## Version 4.3.4

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

## Version 4.3.3

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

## Version 4.3.2

*Release Date: 2021-05-27*

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

## Version 4.3.1

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

## Version 4.3.0

*Release Date: 2021-05-06*

EMQX 4.3.0 is released now, it mainly includes the following changes:

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

## Version 4.3-rc.5

*Release Date: 2021-04-26*

EMQX 4.3-rc.5 is released now, it mainly includes the following changes:

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

- Fix the issue that EMQX may fail to start in Docker

  Github PR: [emqx#4670](https://github.com/emqx/emqx/pull/4670), [emqx#4675](https://github.com/emqx/emqx/pull/4675), [emqx#4657](https://github.com/emqx/emqx/pull/4657)

- When the rule engine resource is not initialized successfully, the corresponding rule status is set to unavailable

  Github Issue: [emqx#4642](https://github.com/emqx/emqx/issues/4642)
  Github PR: [emqx#4643](https://github.com/emqx/emqx/pull/4643)

- Fix the issue caused by reporting telemetry data when EMQX is not fully started

  Github PR: [emqx#4627](https://github.com/emqx/emqx/pull/4627)

- Fix the issue that the HTTPS certificate must be configured to load `emqx-exhook` plugin

  Github PR: [emqx#4678](https://github.com/emqx/emqx/pull/4678)

## Version 4.3-rc.4

*Release Date: 2021-04-16*

EMQX 4.3-rc.4 is released now, it mainly includes the following changes:

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

EMQX 4.3-rc.3 is released now, it mainly includes the following changes:

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

EMQX 4.3-rc.2 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue that `emqx` and `emqx_ctl` commands are not available in some cases

  Github PR: [emqx#4430](https://github.com/emqx/emqx/pull/4430)

## Version 4.3-rc.1

*Release Date: 2021-03-23*

EMQX 4.3-rc.1 is released now, it mainly includes the following changes:

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

EMQX 4.3-beta.1 is released now, it mainly includes the following changes:

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

- Keep retrying the resources that failed to connect after the EMQX Broker is started

  Github PR: [emqx#4125](https://github.com/emqx/emqx/pull/4125)

**Bug fixes:**

- Fix the issue that the long Client ID cannot be tracked

  Github PR: [emqx#4163](https://github.com/emqx/emqx/pull/4163)

- Fix the issue that the query client information may crash

  Github PR: [emqx#4124](https://github.com/emqx/emqx/pull/4124)

## Version 4.3-alpha.1

*Release Date: 2021-01-29*

EMQX 4.3-alpha.1 is released now, it mainly includes the following changes:

*Features*

- Added update resource logic for rule engine
- Enhance Webhook and HTTP authentication performance
- The underlying implementation of the multi-language extension function is changed from erlport to gRPC
- Protect EMQX Broker from cross-site WebSocket hijacking attacks
- Project adjusted to umbrella structure
- Solve the problem that the nodes in the cluster environment must be started in the first startup sequence, otherwise you need to wait for the front node to start
- Websocket listener supports selecting supported subprotocols from the subprotocols list
- Support the default authentication method caching_sha2_password of MySQL 8.0
- JWT authentication supports JWKS
- Support the configuration of the maximum length of the certificate chain and the password of the private key file
- Support import and export of Authentication & ACL information in Mnesia
- Shared subscription supports Hash distribution of messages by source topic
- EMQX Broker in docker now starts in foreground mode

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
