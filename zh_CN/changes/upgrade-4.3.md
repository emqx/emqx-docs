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
ref:
---

# 升级指南

## 升级到 4.3 版本

以下内容仅针对从 4.2 升级到 4.3 的用户。

4.3 中的不兼容改动与行为变更主要包括：

- **以 WebSocket 方式接入 EMQ X 的用户需要注意：** 移除 `verify_protocol_header` 配置项，由新增的 `fail_if_no_subprotocol` 和 `supported_subprotocols` 配置项实现原有功能。
- 共享订阅分发策略配置为 `round_robin` 时的行为变更为随机选择起始订阅者。
- 如果 EMQ X Broker 启动时规则引擎资源暂时不可用，现在 EMQ X Broker 会自动重连，不需要用户手动重连。
- 新安装包不再支持 macOS 10.14 以下版本（不包括 10.14）。
- 多语言扩展功能底层实现方式由 erlport 改为 gRPC，因此无法兼容基于 4.2 开发的 Java、Python 扩展代码。
- HTTP 认证功能的配置项变更，具体请查看 [配置项](../configuration/configuration.md)。
- WebHook 功能的配置项变更，以及规则引擎中的相应字段调整，具体请查看 [配置项](../configuration/configuration.md)。（注：可以通过数据导入导出功能迁移规则引擎中的 Webhook 资源与 Action）
- 4.3 版本不兼容 4.2 的 Mnesia 数据文件，无法启动新版本，可以借助导入导出功能进行升级：
  - 使用导入导出功能，导出最新数据并将导出的数据下载备份
  - 将集群中所有节点停机，删除 `data/mnesia` 目录下的文件以清空数据库
  - 升级到 4.3 版本
  - 启动新版本 EMQ X，使用导入导出功能导入备份的数据，完成数据恢复
  
> 升级过程中请妥善备份相关目录和数据文件。
