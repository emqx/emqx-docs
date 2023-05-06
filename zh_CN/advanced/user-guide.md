# 用户指南

本章将帮助用户快速上手 EMQX，涵盖了从安全性和访问控制、数据管理、消息服务与订阅、集群、系统管理与监控、以及高级特性和扩展等主题。



**安全性和访问控制**：包括与数据安全和访问控制相关的主题。

- [认证鉴权](./auth.md)
- [发布订阅 ACL](./acl.md)
- [黑名单](./blacklist.md)



**消息服务和订阅**：包括所有与消息处理和订阅管理相关的主题。

- [消息桥接](../bridge/bridge.md)
- [共享订阅](./shared-subscriptions.md)
- [排它订阅](./exclusive-subscriptions.md)
- [消息重传](./retransmission.md)



**数据管理**：介绍与数据处理和存储相关的主题。

- [数据存储](../backend/backend.md)
- [数据导入导出](./data-import-and-export.md)



**[分布式集群](./cluster.md)**



**系统管理和监控**：包括常见运维主题，例如通过 $SYS 系统主题进行日志订阅，指标监控、告警、以及速率限制等。

- [$SYS 系统主题](./system-topic.md)
- [指标监控](./metrics-and-stats.md)
- [告警](./alarms.md)
- [速率限制](rate-limit.md)

**高级特性和扩展**：介绍 EMQX 支持的高级特性和扩展功能。

- [WebHook](./webhook.md)
- [钩子](./hooks.md)
- [飞行窗口与消息队列](./inflight-window-and-message-queue.md)