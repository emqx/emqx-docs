---
# 编写日期
date: 2020-07-02 18:32:08
# 作者 Github 名称
author: tigercl
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

## Version 5.0-alpha.6

*Release Date: 2021-09-10*

EMQ X 5.0-alpha.6 is released now, it mainly includes the following changes:

**Feature**

- Support global authentication and listener authentication
- Provides APIs to manage the gateway's listener
- LwM2M and CoAP gateways provide client-related APIs
- Dashboard provides authentication and blacklist pages, currently only supports HTTP Server and MySQL authentication, and the remaining authentication will be supported successively
- Fix some bugs

## Version 5.0-alpha.5

*Release Date: 2021-08-27*

EMQ X 5.0-alpha.5 is released now, it mainly includes the following changes:

**Feature**

- Dashboard provides pages such as retained message, delayed publish, topic rewriting, auto subscribe, etc.
- HTTP API services are uniformly provided by port 18083
- Support auto subscribe
- Improved configuration items to make configuration files easier to read
- Optimize cluster calls
- Support to view EMQ X VM information through the command line interface

## Version 5.0-alpha.4

*Release Date: 2021-08-13*

EMQ X 5.0-alpha.4 is released now, it mainly includes the following changes:

**Feature**

- Dashboard provides monitoring, management, system pages, and supports viewing node topology diagrams, etc.
- Support Redis authentication
- Authorization provides HTTP API, supports creation of ACL rules and adjustment sequence
- New gateway, adding support for CoAP
- Hot configuration provides HTTP API, supports updating configuration at runtime

## Version 5.0-alpha.3

*Release Date: 2021-07-30*

EMQ X 5.0-alpha.3 is released now, it mainly includes the following changes:

**Feature**

- Improve event notification feature
- Introduce a new HTTP API development framework, support the use of OpenAPI specifications to define interfaces and generate documents
- Authentication supports MongoDB, improves the update mechanism, and opens HTTP API
- Open HTTP API for metric monitoring (StatsD, Prometheus) and telemetry
- Improvements to the bottom layer of hot configuration

## Version 5.0-alpha.2

*Release Date: 2021-07-17*

EMQ X 5.0-alpha.2 is released now, it mainly includes the following changes:

**Feature**

- Support RLOG to provide better support for large clusters
- Authorization supports MongoDB and HTTP Server
- Functional adjustments for authentication, support for HTTP Server, support for enhanced authentication
- Support flow control for retained messages
- Brand new configuration structure

## Version 5.0-alpha.1

*Release Date: 2021-07-02*

EMQ X 5.0-alpha.1 is released now, it mainly includes the following changes:

> Note: In 5.0-alpha.1, some required but not yet implemented feature codes have been removed, so the current feature set does not represent the final state, we will implement them in the next versions.

**Feature**

- Support MQTT over QUIC
- Fully supports Hocon configuration format, and provides type-safe configuration verification through Hocon Schema
- Support deployment of authentication services, rule engine resources/actions and other features through configuration files
- Brand new authentication feature, support different listeners to use different authentication services, currently supports Mnesia, MySQL, PosgreSQL and JWT
- Brand new authorization feature, extended the syntax of acl.conf and merged with emqx.conf, currently supports MySQL, PostgreSQL and Redis
- Brand new gateway feature, currently supports STOMP
- Support StatsD protocol
