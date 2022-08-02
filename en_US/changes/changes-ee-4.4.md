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

## Version 4.4.6

*Release Date: 2022-07-29*

### Enhancement

- Rules engine supports RocketMQ with ACL enabled
- Supports searching and paging of rules in rule engine
- Kafka in Rules Engine now supports SASL/SCRAM authentication and SASL/GSSAPI authentication. Note that the `cyrus-sasl-gssapi` dependency needs to be installed before using SASL/GSSAPI authentication
- Provides CLI `./bin/emqx check_conf` to actively check if the configuration is correct
- Optimize the write performance of TDEngine in the rule engine
- Support for clearing historical alarms on Dashboard
- Optimizing Shared Subscription Performance
- Add `db_name` field to the action of rule engine writing data to TDEngine to improve support for super table

### Bug fixes

- Fix the issue that the action count is wrong when the rule engine writes to TDEngine
- Fix the issue that the process pool size setting does not take effect when the rule engine writes to HStreamDB
- Fix the issue that an error was reported when querying the subscription list after the GB/T 32960 plugin was enabled
- Fix the issue that incompatible configuration items when restoring a 4.2 backup to 4.4 with hot configuration enabled
- Fix the issue that once the old version of EMQX is uninstalled after hot upgrade, EMQX will not be able to start again
- Fix the issue that the keep-alive check for UDP clients in the Multilingual Protocol Extension was incorrect, causing clients not to expire
- Fix the issue that the client information in the Multilingual Protocol Extension was not updated in time
- Fix the issue that the license update does not take effect when running after hot upgrade to e4.4.4 and later versions
- Fix the issue that when the client specified Clean Session as false to reconnect, the shared subscription message in the flight window would be re-dispatched to the old session process
- Fix the issue that the new node did not use the cluster license after joining the cluster
- Fix the issue that the `emqx_lua_hook` plugin cannot cancel the message publishing

## Version 4.4.5

*Release Date: 2022-06-30*

### Enhancement

- Rule engine supports persisting data to HStreamDB
- QoS and Retain flag in rule engine's message republish actions can now use placeholders
- Supports exclusive subscriptions, that is, only one subscriber is allowed for a topic
- Support one-click update of cluster license through CLI
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

## Version 4.4.4

*Release Date: 2022-06-01*

### Enhancement

- Add more time transformation functions to the SQL of rule engine
- Add the `float2str/2` function to the SQL of rule engine to support specifying the output precision of floating point numbers
- Rule engine supports message persistence to Alibaba TableStore
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
- When using CLI for backup and recovery, it is no longer required that the backup file must be located in the `backup` folder of the EMQX data directory
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
- Fix the issue that the subscriber's connection was disconnected due to the wrong user properties type in the message publishing API `api/v4/mqtt/publish`
- Fix DynamoDB driver not adapting to OTP 24, causing it to be unavailable
- Fix the issue that some authentication algorithms were unavailable due to the PostgreSQL driver not adapting to OTP 24
- Fix the issue that the returned results did not match the query conditions when querying subscriptions with multiple conditions
- Fix rule engine resource connection test not working
- Fix multiple Dashboard display issues

## Version 4.4.3

*Release Date: 2022-04-18*

### Enhancement

- Schema registry now supports decoding arbitrary binary payloads to JSON data using gRPC services
- Support for connecting to Pulsar using TLS
- Add `mongo_date` function for SQL in rule engine, which supports saving timestamps as MongoDB Date objects
- Rule engine supports resetting metrics of the specified rule
- Add connection confirmation and authorization completion events to the rule engine
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

## Version 4.4.2

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
