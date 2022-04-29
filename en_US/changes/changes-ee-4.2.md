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

# Release version

## 4.2.11 Release

*Release Date: 2022-04-26*

### Important changes

- A cluster-wide total connections calculation bug was fixed in in Enterprise edition 4.2.11. Previously only the individual node's local number of connections were checked against the max number of connections allowed by the license. After this fix, the total number of connections is aggregated cluster-wide every 5 seconds. An extra 10% overrun is allowed to compensate the delays in aggregation. **Users planning to upgrade should be aware of the possibility that this change may cause clients to reach the license limit and not be able to connect.**

> Note: The WebSocket listener will be restarted when e4.2.0 and e4.2.1 are hot-upgraded to a higher version, so WebSocket connections will be disconnected for a short time, please pay attention when upgrading.

### Enhancement

- MQTT-SN gateway supports initiative to synchronize registered topics after session resumed.
- Improve the relevant metrics during the execution of the rule engine SQL matching
- Improve the error message when rule engine fails to parse payload

### Bug fixes

#### Rule Engine

- Fix the issue that rule engine data persistence to Oracle failed but the success count still increased
- Fix the issue that the alternate action could not be triggered when the action of the rule engine persisting data to Oracle (only synchronous operation) failed to execute
- Fix the issue that enabling system messages would cause rule engine's Kakfa action to crash
- Fix the issue of query resource request timeout when rule engine resource is unavailable
- If a rule with the same ID already exists when creating a rule, , rule engine will now report an error instead of replacing the existing rule

#### Protocol

- Fix the issue that the configuration item `server_keepalive` would be incorrectly applied to MQTT v3.1.1 clients
- Fix the issue that the JT/T 808 location report frame was parsed incorrectly
- Fix the issue that messages that failed to be delivered due to unregistered topics were not retransmitted when topics were successfully registered with the MQTT-SN client

#### REST API & CLI

- Fix the issue that incorrect query results were returned when querying subscriptions using multiple condition
- Fix the issue that the subscription query interface did not return paginated data
- Add the format check for Dashboard User and AppID to avoid User and AppID containing some special characters cannot be deleted
- Fix the issue that the metrics interface does not return authentication metrics such as client.acl.deny by default
- Fix the issue that the LwM2M client list query API returned incorrect data in a cluster environment, which resulted in the inability to access the LwM2M gateway module management page

#### Dashboard

- Fix the issue that the session creation time option was not available when filtering clients
- Fix multiple UI display issues

#### Other

- Fix various issues of hot config, such as the configuration cannot be cleared, the updated configuration is invalid after restarting, etc
- Fix the issue that the MQTT Bridge plugin cannot be started when only the subscription topic is configured but QoS is not configured
- Fix an issue with plugin default startup list, now duplicate plugin startup items in `loaded_plugins` file will be ignored
- Fix the issue that auto subscriptions might subscribe to an empty topic
- Fix the issue that Message ID displayed garbled characters in some logs

## 4.2.10 Version

*Release date: 2022-01-13*

### Enhancement

- The action's metrics in rule engine will no longer be cleared when updating
- Supports configuring whether to forward retained messages with empty payload to suit users who are still using MQTT v3.1. The relevant configurable item is `retainer.stop_publish_clear_msg`
- Optimize the use and interaction of the built-in access control file module
- Change the default value of the `max_topic_levels` configurable item to 128. Previously, it had no limit (configured to 0), which may be a potential DoS threat
- Improve the error log content when the Proxy Protocol message is received but the `proxy_protocol` configuration is not turned on

### Important fixes

- Fix the issue that the rule engine may have a higher failure rate when saving data to MySQL
- Fix the issue of garbled data when writing data to RocketMQ asynchronously
- Fix the issue of inaccurate metrics of RocketMQ
- Fix the issue that the Max Returned Count option in the MongoDB-based offline message feature of the rule engine cannot be used
- Fixed an issue that health checks on resources could block the creation process

### Minor fixes

- Fixed the issue that the Retain Handling subscription option in the proxy subscription module could not be configured to 2
- Fix the issue that the client list obtained by filtering by session creation time is inaccurate
- Fix Erlang VM memory calculation error in Dashboard node details page
- Removed run-time configurable items that have expired, support more run-time configurable items

## 4.2.9 Version

*Release date: 2021-11-17*

### Enhancement

- Improve client kick (forced step-down)

### Important fixes

- Fix the issue that calls between clusters may cause the client process to lose response
- Fix the issue that modules reporting errors after multiple startups and shutdowns
- Fix the issue that lock release may cause the client process to crash in some cases

### Minor fixes

- MongoDB resources allow host names
- Fix some issues of MongoDB authentication module
- Fixed the issue that the Client ID containing "\\" characters could not be searched in a fuzzy manner
- Fix the issue that variable byte integers may be larger than 4 bytes
- Fix the issue that the same module may be added repeatedly
- Fixed the issue that the new configuration actually took effect after modifying the Action-related configuration in Dashboard, but the Dashboard did not refresh the display

## 4.2.8 version

*Release date: 2021-09-29*

- Fix an issue that the rule engine failed to synchronize batch write to SQL Server
- Fix an issue that the rule engine cannot create Oracle resources
- Fix an issue that multi-language protocol analysis cannot be stopped under abnormal conditions
- Fix an issue of failure to create LDAP Auth authentication module
- Fix an issue where custom fields could not be parsed in the JT/T808 gateway parsing location report
- Fix an issue that rule engine offline messages cannot be deleted after being received in some cases
- Fix an issue that resources cannot be released after the rule engine is closed
- Enhanced saving of offline messages to Redis to support clearing residual data
- Fix an issue that the error code returned by the backend is not clear when the wrong data format is entered when searching on the client
- Fix an issue that the client's protocol name is incorrectly displayed after the MQTT-SN client is connected
- Fix an issue that the client process may be stuck, causing some clients to fail to connect
- Fix an issue that the client cannot access after proxy-protocol is turned on.
- Fix an issue where the client page displayed incorrect Socket type after proxy-protocol was turned on
- Fix an issue of "Connection process is not alive" when calling exproto's ConnectionAdapter method across nodes in the cluster
- Fix a bug that caused a zombie on the Kafka client due to network fluctuations
- Webhook supports switching http-pipelining , which is disabled by default
- Added support for ipaddrs in acl.conf
- Optimize an issue of printing a large number of useless logs when the exproto client is disconnected

## 4.2.7 version

*Release date: 2021-06-17*

- Fix an issue where rule engine data is saved to an openTSDB exception that cannot be written  
- Fix A hot configuration issue cannot be performed on dashboard in a special case 
- Fix the problem that the client of the MQTT-SN protocol cleansession-false lost topicid when restoring the session  
- Fix MQTT-SN the client is stuck in an abnormal situation
- Fix an issue where rule engine data is forwarded to a WebServer SSL configuration that does not take effect  
- Fix an issue where module Kafka consumer group SSL configurations do not work  
- Fix rule engine The problem with editing a resource that prevents the list of resources from appearing  
- Enhanced exception handling of failed import lice

## 4.2.6 version

*Release date: 2021-04-28*

- Fix the problem that the module cannot be started after stopping under special circumstances
- Fix the problem of the time format of the alarm list
- Fix the problem that the MQTT-SN client goes offline abnormally and the Will message is not sent
- Fix the problem that PUBLISH and REGACK are out of order when the MQTT-SN client reconnects and cleansession=false
- Fix the problem of partial display errors in Dashboard
- Update Log default output in File

## 4.2.5 version

*Release date: 2021-03-10*

- Fix an issue with the Pulsar consumer group resolving bulk message errors
- Fix an issue that cannot be resolved in the event of an MQTT protocol exception
- Fix an issue where an error was displayed in the exception of the Dashboard subscription list
- Fix a bulk message performance issue that the rule engine handles for a single process

## 4.2.4 version

*Release date: 2021-02-18*

- New rule engine update resource logic
- Added new rule engine, data bridge to kafka supports configuration of cache size
- Fix the situation where the AUTH_HTTP long connection is disconnected when the Keepalive timeout period or the maximum number of requests is reached, causing the request to be lost
- Fix the issue of WebHook SSL certificate configuration
- Fix the problem of AuthRedis reconnection failure
- Fix the issue of checking MQTT Topic format when creating Kafka consumer group
- Optimize the theme statistics page moved to the module management page

## 4.2.3 version

*Release date: 2020-12-25*

- New GT/T32960 protocol access
- New Rule Engine SQL statements support binary data operation functions
- Adjust the rule engine/module interface parameters uniformly
- Optimize the LWM2M access process
- Optimize webHook plug-in performance
- Fix The rule engine redis sentinel mode failed to create a resource

## 4.2.2 version

*Release date: 2020-12-10*

- Optimize AuthHttp performance issues
- Add new rule engine data save to Oracle
- Added rule engine data save to DolphinDB
- Added rule engine data saving to MS SQL server
- Enhanced rule engine data saving support synchronous and asynchronous
- Fix the problem of inaccurate counting in the asynchronous mode of the rule engine
- Added SSL support to configure the depth of the CA certificate
- Fix the abnormal problem in the hot upgrade

## 4.2.1 version

*Release date: 2020-11-16*

- Added Dashboard module page to support management mqtt enhanced authentication
- Added Dashboard module page to support management of lwm2m client
- Added redis resources to support configuring SSL parameters
- Added auth_jwt to support JWKs
- Added alert message when subscriber TCP is busy
- New rule engine, data bridge to kafka, support ACK policy configuration
- Optimize Dashboard monitoring page
- Optimize emqx_exporto performance
- Optimize emqx_exhook performance
- Fix the wrong action type when editing actions in dashboard
- Fix rule engine-resource typo
- Fix the failure of import and export recovery in cluster
- Fix the problem that the rule engine MySQL resource cannot use the domain name
- Fix the problem that the message is too large when the data is bridged to Kafka

## 4.2.0 version

*Release date: 2020-10-13*

- Rule engine Mysql/MongoDB/Cassandra/PGsql resource supports IPV6 and SSL connection
- The rule engine "resources" supports uploading certificates
- Rule engine "action" group
- Fix InfluxDB not supporting underscore characters
- Support dynamic creation and configuration of functional modules
- Support more parameter hot configuration
- Support hot upgrade between minor version numbers
- Remove emqx_auth_username and emqx_auth_clientid plugins
- Refactor emqx_auth_mnesia, compatible with the data import of the old version emqx_auth_username and emqx_auth_clientid
- The emqx main configuration file is split and supports include configuration files
