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
- HTTP 认证功能的配置项变更，为方便理解，auth_req，super_req 和 acl_req 将统一代称为 REQUEST：
  - 更新了 URL 的配置方式，`auth.http.REQUEST` 更名为 `auth.http.REQUEST.url`。
  - 废弃 `auth.http.header.<Key>` 配置项，4.3 允许为认证请求、超级用户验证请求、ACL 请求配置不同的 Headers，配置方式为 `auth.http.REQUEST.headers.<Key> = <Value>`。
  - 废弃 `auth.http.REQUEST.content_type` 配置项，可以通过 `auth.http.REQUEST.headers.content_type` 来配置。
  - `auth.http.request.timeout` 更名为 `auth.http.timeout`。
  - `auth.http.request.connect_timeout` 更名为 `auth.http.connect_timeout`。
  - 废弃 `auth.http.request.retry_times`，`auth.http.request.retry_interval` 和 `auth.http.request.retry_backoff` 配置项。
  - 新增 `auth.http.pool_size` 和 `auth.http.enable_pipelining` 配置项，允许配置进程池大小和是否启用 HTTP Pipelining。
  - 新增 `auth.http.ssl.verify` 和 `auth.http.ssl.server_name_indication` 配置项以满足不同 HTTPS 服务端的要求。
- WebHook 功能的配置项变更，以及规则引擎中的相应字段调整。（注：可以通过数据导入导出功能迁移规则引擎中的 Webhook 资源与 Action）
  - `web.hook.api.url` 更名为 `web.hook.url`。
  - `web.hook.encode_payload` 更新为 `web.hook.body.encoding_of_payload_field`
  - 新增 `web.hook.ssl.verify` 和 `web.hook.ssl.server_name_indication` 配置项以满足不同 HTTPS 服务端的要求。
  - 新增 `web.hook.pool_size` 和 `web.hook.enable_pipelining` 配置项，允许配置进程池大小和是否启用 HTTP Pipelining。
- 4.3 版本不兼容 4.2 的 Mnesia 数据文件，无法启动新版本，可以借助导入导出功能进行升级：
  - 使用导入导出功能，导出最新数据并将导出的数据下载备份
  - 将集群中所有节点停机，删除 `data/mnesia` 目录下的文件以清空数据库
  - 升级到 4.3 版本
  - 启动新版本 EMQ X，使用导入导出功能导入备份的数据，完成数据恢复
  
> 升级过程中请妥善备份相关目录和数据文件。
