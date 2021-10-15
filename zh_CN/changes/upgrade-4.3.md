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

由于数据库架构和 Broker 间 API 更改，EMQ X 4.3 节点将无法加入 4.2 集群。

推荐按以下步骤完成升级：

1. 从 4.2 任一节点导出数据（见下文）
2. 使用 4.3 的配置创建备用的 4.3 集群
3. 将导出数据导入至 4.3 任一节点，然后重启集群
4. 将流量从 4.2 集群切换至 4.3 集群
5. 关闭 4.2 集群

旧集群中存储在内置数据库（Mneisa）的数据可以通过 [数据库迁移](#数据库迁移) 来迁移至新集群。此外还有一些重要的 [配置变更](#重要的配置变更) 和 [行为变更](#重要的行为变更)，详见下文。

### 数据库迁移

注：详情请参阅 [数据导入导出](../advanced/data-import-and-export.md)。

执行以下命令以导出数据：
```bash
$ emqx_ctl data export
```

这将生成一个名称中带有时间戳的 JSON 文件，该文件包含了旧集群的所有可迁移数据，可用于导入至新集群。

`emqx_auth_mnesia` 插件现在支持基于 `clientid` 和 `username` 的规则。而此前只支持一种类型的过滤器，如 `etc/plugins/emqx_auth_mnesia.conf` 文件中配置的那样。

为了正确导入来自旧版本 `emqx_auth_mnesia` 插件的数据，需要将该参数的值通过命令行参数选项传递给数据导入命令：

```bash
$ emqx_ctl data import <filename> --env '{"auth.mnesia.as":"username"}'
```

或者

```bash
$ emqx_ctl data import <filename> --env '{"auth.mnesia.as":"clientid"}'
```

### 重要的配置变更

- EMQ X 现在默认尝试使用 TLS 1.3，请确保 openssl 是最新的(1.1.1)，否则可能需要更改 SSL 相关配置，例如从 `listener.ssl.external.tls_versions` 的列表中删除 `tlsv1.3`
- 新增 `listener.ws.$zone.check_origin_enable` `listener.ws.$zone.allow_origin_absence` 和 `listener.ws.$zone.check_origins` 配置以获得更好的 WebSocket 安全性。
- 配置项 `listener.ws.$name.verify_protocol_header` 由 `listener.ws.external.fail_if_no_subprotocol` 和 `listener.ws.external.supported_subprotocols` 替代。
- 配置项 `node.heartbeat` 不能被环境变量 `EMQX_NODE__HEARTBEAT` 覆盖，待修复，[Github Issue#5929](https://github.com/emqx/emqx/issues/5929)
- 支持设置 `log.formatter = json` 以 JSON 格式输出日志，但可能需要更多 CPU 资源
- 支持设置 `log.single_line = true` 以阻止单条日志发生换行。
- `rpc.tcp_client_num` 默认配置为 1。大于 1 的值可能会导致集群节点间传递消息时乱序。

#### 重要的插件配置变更

##### `emqx_auth_http`

为了更容易理解，我们将使用关键字 `REQUEST` 来指代 `auth_req`、`super_req` 和 `acl_req`。

- `auth.http.REQUEST` 更名为 `auth.http.REQUEST.url`。
- `auth.http.header.<Key>` 更名为 `auth.http.REQUEST.headers.<Key> = <Value>`。即现在可以为每个 REQUEST 类型配置不同的 HTTP Headers。
- `auth.http.REQUEST.content_type` 更名为 `auth.http.REQUEST.headers.content_type`。
- `auth.http.request.timeout` 更名为 `auth.http.timeout`。
- `auth.http.request.connect_timeout` 更名为 `auth.http.connect_timeout`。
- 废弃 `auth.http.request.retry_times`，`auth.http.request.retry_interval` 和 `auth.http.request.retry_backoff` 配置项。
- 新增 `auth.http.pool_size` 以支持配置进程池大小。
- 新增 `auth.http.enable_pipelining` 以支持开启或关闭 HTTP Pipelining。
- 新增安全相关配置：`auth.http.ssl.verify` 和 `auth.http.ssl.server_name_indication`。

##### `emqx_auth_mongo`

- `auth.mongo.login` 更名为 `auth.mongo.username`
- `auth.mongo.ssl_opts.*` 更名为 `auth.mongo.ssl.*`

##### `emqx_auth_pgsql`

- `auth.pgsql.ssl_opts.*` 更名为 `auth.mongo.pgsql.*`

##### `emqx_auth_redis`

- SSL 配置现在按配置路径中的 `.ssl` 分组。例如`auth.redis.cacertfile` 现在是 `auth.redis.ssl.cacertfile`。

##### `emqx_web_hook`

注：规则引擎中的 Webhook 资源和操作可以通过数据库迁移命令进行迁移。

- `web.hook.api.url` 更名为 `web.hook.url`。
- `web.hook.encode_payload` 更名为 `web.hook.body.encoding_of_payload_field`
- 新增安全相关配置： `web.hook.ssl.verify` 和 `web.hook.ssl.server_name_indication`
- 新增 `web.hook.pool_size` 以支持配置进程池大小。
- 新增 `web.hook.enable_pipelining` 以支持开启或关闭 HTTP Pipelining。

### 重要的行为变更

- 日志时间戳现在是 RFC3339 格式，请确保您的日志索引器已准备好进行此更改。
- 共享订阅分发策略配置为 `round_robin` 时的行为变更为随机选择起始订阅者。
- 多语言扩展功能底层实现方式由 erlport 改为 gRPC，因此无法兼容基于 4.2 开发的 Java、Python 扩展代码。
- 新安装包不再支持 macOS 10.14 以下版本（不包括 10.14）。
