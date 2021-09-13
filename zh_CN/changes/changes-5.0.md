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

## 5.0-alpha.6 版本

*发布日期: 2021-09-10*

EMQ X 5.0-alpha.6 现已发布，主要包含以下改动:

**功能**

- 认证功能改进，支持全局认证与监听器认证
- 提供网关监听器管理 API
- LwM2M、CoAP 网关提供客户端操作 API
- Dashboard 提供认证、黑名单页面，当前仅支持 HTTP Server 与 MySQL 认证，剩余认证将陆续支持
- 修复一些问题

## 5.0-alpha.5 版本

*发布日期: 2021-08-27*

EMQ X 5.0-alpha.5 现已发布，主要包含以下改动:

**功能**

- Dashboard 提供保留消息、延迟发布、主题重写、代理订阅等功能页面
- HTTP API 服务统一由 18083 端口提供
- 支持自动订阅功能
- 配置项改进，使配置文件更易读
- 优化集群调用
- 支持通过命令行查看 EMQ X VM 信息

## 5.0-alpha.4 版本

*发布日期: 2021-08-13*

EMQ X 5.0-alpha.4 现已发布，主要包含以下改动:

**功能**

- Dashboard 提供监控、管理、系统页面，支持查看节点拓扑图等
- 支持 Redis 认证
- 授权功能提供 HTTP API，支持创建 ACL 规则与调整顺序
- 全新的网关功能，新增对 CoAP 的支持
- 热配置提供 HTTP API，支持运行时更新配置

## 5.0-alpha.3 版本

*发布日期: 2021-07-30*

EMQ X 5.0-alpha.3 现已发布，主要包含以下改动:

**功能**

- 改进事件通知功能
- 引入全新 HTTP API 开发框架，支持使用 OpenAPI 规范定义接口并生成文档
- 认证支持 MongoDB，改进更新机制，并开放 HTTP API
- 指标监控 (StatsD，Prometheus) 与遥测功能开放 HTTP API
- 热配置底层实现改进

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