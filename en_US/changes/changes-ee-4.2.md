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
