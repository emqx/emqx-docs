---
# 编写日期
date: 2020-07-02 18:32:06
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

# 版本发布

## 5.0-alpha.2 版本

*发布日期: 2021-07-17*

EMQ X 5.0-alpha.2 现已发布，主要包含以下改动:

**功能**

- 支持 RLOG，为大型集群提供更好的支持
- 授权支持 MongoDB 与 HTTP Server
- 认证进行功能性调整，支持 HTTP Server，支持增强认证
- 保留消息下发支持流控
- 全新的配置结构

## 5.0-alpha.1 版本

*发布日期: 2021-07-02*

EMQ X 5.0-alpha.1 现已发布，主要包含以下改动:

> 注意：5.0-alpha.1 中移除了一些需要重构但尚未实现的功能代码，因此当前功能集合不代表最终状态，我们会在后续版本中实现它们。

**功能**

- 支持 MQTT over QUIC
- 全面支持 Hocon 配置格式，通过 Hocon Schema 提供类型安全的配置验证
- 支持通过配置文件部署认证服务、规则引擎资源/动作等功能
- 全新的身份验证功能，支持不同监听器使用不同的认证服务，目前已支持 Mnesia, MySQL, PosgreSQL 和 JWT
- 全新的授权功能，对 acl.conf 的语法进行了扩展，并与 emqx.conf 合并，目前已支持 MySQL, PostgreSQL 和 Redis
- 全新的网关功能，目前已支持 STOMP
- 支持 StatsD 协议