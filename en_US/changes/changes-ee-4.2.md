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
- Fixed the situation where the AUTH_HTTP long connection is disconnected when the Keepalive timeout period or the maximum number of requests is reached, causing the request to be lost
- Fix the issue of WebHook SSL certificate configuration
- Fix the problem of AuthRedis reconnection failure
- Fixed the issue of checking MQTT Topic format when creating Kafka consumer group
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
- Fixed InfluxDB not supporting underscore characters
- Support dynamic creation and configuration of functional modules
- Support more parameter hot configuration
- Support hot upgrade between minor version numbers
- Remove emqx_auth_username and emqx_auth_clientid plugins
- Refactor emqx_auth_mnesia, compatible with the data import of the old version emqx_auth_username and emqx_auth_clientid
- The emqx main configuration file is split and supports include configuration files
