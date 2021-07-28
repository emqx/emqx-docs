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
