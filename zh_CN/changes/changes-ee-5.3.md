# 版本发布

## e5.3.2

## 增强

- [#11752](https://github.com/emqx/emqx/pull/11752) 将 core-replica 数据库同步的默认 RPC 驱动从 `gen_rpc` 更改为 `rpc`。

  这提升了核心副本数据复制的速度。

- [#11785](https://github.com/emqx/emqx/pull/11785) 允许拥有“查看者”角色的用户更改自己的密码。然而，拥有“查看者”角色的用户没有权限更改其他用户的密码。

- [#11787](https://github.com/emqx/emqx/pull/11787) 提升了 `emqx` 命令的性能。

- [#11790](https://github.com/emqx/emqx/pull/11790) 为配置在 Redis 授权数据源中的 Redis 命令添加了验证功能。此外，此次改进优化了认证和授权过程中 Redis 命令的解析。现在的解析符合 `redis-cli` 兼容性标准，并支持引号参数。

- [#11541](https://github.com/emqx/emqx/pull/11541) 通过引入更多的互动方式，增强了 EMQX 的文件传输能力。现在，客户端可以将文件传输命令发送到 `$file-async/...` 主题，而不是 `$file/...`。命令执行结果将作为消息发送到 `$file-response/{clientId}` 主题。 这一增强在某些情况下简化了文件传输功能的使用，例如，当客户端使用 MQTTv3 或当代理服务器位于 MQTT 桥接之后时。 更多详情请参阅 [EIP-0021](https://github.com/emqx/eip)。

## 修复

- [#11757](https://github.com/emqx/emqx/pull/11757) 修复了下载不存在的追踪文件时返回的错误响应码。现在，响应码会返回 `404` 而不是 `500`。

- [#11762](https://github.com/emqx/emqx/pull/11762) 修复了 EMQX 中 `built_in_database` 授权数据源的一个问题。通过这次修复，现在在销毁授权数据源时，所有访问控制列表（ACL）记录都会被彻底移除。这解决了以前的问题，即数据库中残留的记录可能在重新创建授权数据源时导致问题。

- [#11771](https://github.com/emqx/emqx/pull/11771) 修复了通过 API/Dashboard 进行身份验证管理时 Bcrypt 盐轮次的验证问题。

- [#11780](https://github.com/emqx/emqx/pull/11780) 修复了 `pbkdf2` 密码哈希算法中 `iterations` 字段的验证问题。现在，`iterations` 必须是严格正数。之前，`iterations` 可以被设置为 0，这会导致验证器无法正常工作。

- [#11791](https://github.com/emqx/emqx/pull/11791) 修复了 EMQX CoAP 网关中的一个问题，即心跳没有有效地维持连接的活跃状态。此修复确保心跳机制正确维持 CoAP 网关连接的活跃状态。

- [#11797](https://github.com/emqx/emqx/pull/11797) 修改了管理 `built_in_database` 授权数据源的 HTTP API 行为。如果未将 `built_in_database` 设置为授权数据源，这些 API 现在将返回 `404` 状态码，替换了以前的 `20X` 响应。

- [#11965](https://github.com/emqx/emqx/pull/11965) 优化了 EMQX 服务的终止过程，确保即使在存在不可用的 MongoDB 资源的情况下，也能够实现优雅停止。

- [#11975](https://github.com/emqx/emqx/pull/11975) 此修复解决了由于对端和服务器同时关闭套接字时发生竞争条件导致的冗余错误日志问题。以前，由操作系统和 EMQX 触发的并发套接字关闭事件会导致不必要的错误记录。通过改进事件处理，本次修复消除了不必要的错误信息。

- [#11987](https://github.com/emqx/emqx/pull/11987) 修复了在尝试设置 TCP/SSL 套接字的 `active_n` 选项时连接崩溃的问题。

  在此修复之前，如果在连接过程中尝试设置 `active_n` 选项时套接字已经关闭，会导致 `case_clause` 崩溃。

- [#11731](https://github.com/emqx/emqx/pull/11731) 将文件传输功能配置添加到了热配置模式。

- [#11754](https://github.com/emqx/emqx/pull/11754) 改进了 EMQX 中针对 Postgres 桥接的日志格式化功能。这个改进针对驱动程序返回的错误消息中的 Unicode 字符进行了处理。

## e5.3.1

### 增强

- [#11637](https://github.com/emqx/emqx/pull/11637) 增加了额外的诊断检查，以帮助调试当 Mnesia 因等待表而停滞时出现的问题。更新依赖库：`ekka` 已升级至 0.15.15 版本，`mria` 已升级至 0.6.4 版本。
- [#11581](https://github.com/emqx/emqx/pull/11581) 功能预告：计划在 EMQX v5.4.0 版本中，在数据桥接的基础上新增*连接*与*动作*概念，并逐步迁移现有数据桥接到连接与动作。连接用于管理数据集成与外部系统的连接，动作仅用于配置数据操作方式，连接可以在多个动作之间重复使用，以提供更大的灵活性和更好的可扩展性。目前 Kafka 生产者与 Azure Event Hub 生产者已经完成迁移。
- Dashboard 为规则引擎消息重发布动作提供了 MQTT 5.0 发布属性设置，允许用户更灵活的发布消息。

### 修复

- [#11565](https://github.com/emqx/emqx/pull/11565) 将 jq 库从 v0.3.10 升级至 v0.3.11。在此版本中，jq_port 程序将按需启动，除非 EMQX 中使用 jq 功能，否则不会出现在用户的进程中。此外，空闲的 jq_port 程序将在设定的一段时间后自动终止。注意：大多数运行 NIF 模式下的 EMQX 用户不会受到此更新的影响。

- [#11676](https://github.com/emqx/emqx/pull/11676) 隐藏 DEBUG 级别的日志中的部分敏感信息。

- [#11697](https://github.com/emqx/emqx/pull/11697) 在 EMQX 后端网络 (`gen_rpc`) 中禁用了过时的 TLS 版本和密码套件。增加了对后端网络的 tlsv1.3 支持，并引入了新的配置参数：`EMQX_RPC__TLS_VERSIONS` 和 `EMQX_RPC__CIPHERS`。

  对应的 `gen_rpc` PR: https://github.com/emqx/gen_rpc/pull/36

- [#11734](https://github.com/emqx/emqx/pull/11734) 修复了 IPv6 网络中集群配置的问题。新增了新的配置项 `rpc.listen_address` 和 `rpc.ipv6_only`，以允许 EMQX 集群的 RPC 服务和客户端使用 IPv6。

- [#11747](https://github.com/emqx/emqx/pull/11747) 更新 QUIC 到 msquic 2.2.3 版本。

- [#11796](https://github.com/emqx/emqx/pull/11796) 修复了 RPC schema，以确保客户端和服务器使用相同的传输驱动程序。

- [#11798](https://github.com/emqx/emqx/pull/11798) 修复了在执行 `./bin/emqx data import [FILE]` 后节点无法启动的问题。

  同时增强了 `apikey_key` 和 `apikey_name` 之间的关联以提高一致性和唯一标识性：

  - `apikey_key`：通过 Dashboard 生成 API 密钥时，`apikey_key` 现在会根据提供的易读性较强的 `apikey_name` 创建一个唯一值。
  - `apikey_name`：相反，当使用引导文件生成 API 密钥时，`apikey_name` 将基于关联的 `apikey_key` 生成为唯一值。

- [#11813](https://github.com/emqx/emqx/pull/11813) 修复了 schema，确保 RPC 客户端 SSL 端口与配置的服务器端口一致。此修复还确保了RPC 端口在 Helm 图表中的被正确打开。

- [#11819](https://github.com/emqx/emqx/pull/11819) 升级了 OpenTelemetry 库至 v1.3.1-emqx。该版本修复了在导出的指标中指标时间戳无效的问题。

- [#11861](https://github.com/emqx/emqx/pull/11861) 修复了 remote shell 中打印过多警告信息的问题。

- [#11722](https://github.com/emqx/emqx/pull/11722) 修复了同步请求模式下的 Kafka 生产者桥接在`正在连接`状态下无法缓存消息的问题。

- [#11724](https://github.com/emqx/emqx/pull/11724) 修复了一个与统计指标相关的问题，即消息发送到 Kafka 时，由于内部缓存、即使后来被成功传输，仍然被计为发送失败。

- [#11728](https://github.com/emqx/emqx/pull/11728) 改进了 LDAP 过滤字符串解析器，具体改进如下：
  - 自动转义过滤字符串中的特殊字符。
  - 修复了先前阻止使用 `dn` 作为过滤值的错误。
  
- [#11733](https://github.com/emqx/emqx/pull/11733) 解决了一个不兼容性问题，该问题导致在会话接管或通道驱逐时，如果会话位于运行 EMQX v5.2.x 或更早版本的远程节点上，可能会导致崩溃。

- [#11750](https://github.com/emqx/emqx/pull/11750) 日志不再输出使用 HTTP 服务进行认证和 HTTP 服务数据桥接的请求 Body。

- [#11760](https://github.com/emqx/emqx/pull/11760) 简化了用于 Cassandra 数据桥接健康检查的 CQL 查询，之前该查询在 Cassandra 服务器日志中生成了警告。

- [#11886](https://github.com/emqx/emqx/pull/11886) 修复了一个向后的插件兼容性的问题。

  目前，EMQX 对钩子挂载点名称进行验证，无效的钩子挂载点不能用于注册钩子。但是旧版本的插件模板使用了一些拼写错误的钩子挂载点，实际使用中的插件也可能存在这种问题，为了兼容以前的插件，我们允许使用旧的钩子挂载点来注册钩子，但会发出已被弃用的警告，这些钩子与以前一样不会被调用。

- [#11897](https://github.com/emqx/emqx/pull/11897) 修复了当集群节点几乎在同一时间启动时，节点间配置同步的时候等待循环竞争条件的问题。


## e5.3.0

### 增强

- [#11597](https://github.com/emqx/emqx/pull/11597)  将 Ekka 升级到 0.15.13，包括以下增强：

  - 升级 Mria 到 0.6.2。
  - 可以通过配置设置初始化阶段数据同步批量大小，[Mria PR](https://github.com/emqx/mria/pull/159)。
  - 提升了 mria_membership 进程的健壮性，[Mria PR](https://github.com/emqx/mria/pull/156)。
  - 修复日志消息格式错误。
  - EMQX 配置中添加了 `node.default_bootstrap_batch_size` 选项。 增加此选项的值可以极大地减少复制节点的启动时间，特别是当 EMQX 集群互连网络延迟较高且 EMQX 内置数据库包含大量数据时，例如订阅数较多的情况。

- [#11620](https://github.com/emqx/emqx/pull/11620)  添加一个新的规则引擎 SQL 函数 `bytesize` 以获取字节字符串的大小。例如：`SELECT * FROM "t/#" WHERE bytesize(payload) > 10`。

- [#11642](https://github.com/emqx/emqx/pull/11642) 将 quicer 升级到版本 0.0.200，为启用 OpenSSL3 对 QUIC 传输的支持做准备。

- [#11610](https://github.com/emqx/emqx/pull/11610) 在 Dashboard 中实施了初步基于角色的访问控制。

  在此版本中，有两个预定义的角色：

  - 管理员：此角色可以访问所有资源。
  - 查看者：此角色只能查看资源和数据，对应于 REST API 中的所有 GET 请求。

- [#11631](https://github.com/emqx/emqx/pull/11631) 添加了单点登录（SSO）功能并与 LDAP 集成。
- [#11656](https://github.com/emqx/emqx/pull/11656) 集成了 SAML 2.0 支持以实现单点登录（SSO）。
- [#11599](https://github.com/emqx/emqx/pull/11599) 支持审计日志，会将来自 CLI、REST API 和 Dashboard 的操作记录在独立的日志文件中。

### 修复

- [#11682](https://github.com/emqx/emqx/pull/11682) 修复了在文件日志处理程序上将“轮换大小”设置为`infinity`时日志记录停止的问题。

- [#11567](https://github.com/emqx/emqx/pull/11567) 改进了 EMQX 的优雅关闭（`emqx stop` 命令）：
  
  - 将超时时间从1分钟增加到2分钟。
  - 如果 EMQX 无法在配置的超时时间内优雅地停止，则打印错误消息。
  - 在 EMQX 关闭过程中定期打印状态消息。
  
- [#11584](https://github.com/emqx/emqx/pull/11584) 修复了在 Windows 上当 os_mon 模块不可用时的遥测报告错误。

- [#11605](https://github.com/emqx/emqx/pull/11605) 降低了 CMD_overridden 的日志严重程度，从警告（warning）降至信息（info）。

- [#11622](https://github.com/emqx/emqx/pull/11622) 升级了 RPC 库 `gen_rpc` 从版本 2.8.1 到 3.1.0。

- [#11623](https://github.com/emqx/emqx/pull/11623) 将 `esockd` 库从版本 5.9.6 升级到 5.9.7。此次升级包括以下内容：

  - 对代理协议错误和超时进行了增强。[esockd pr#178](https://github.com/emqx/esockd/pull/178)
  - 将 `ssl_error` 异常的日志级别降低为信息级别。[esockd pr#180](https://github.com/emqx/esockd/pull/180)
  - 将异常 MQTT 数据包解析的日志级别从 `error` 降低为 `info`。
  - 在 `emqx ctl listeners` 命令输出中，当 TLS 握手失败（`ssl_error`）或 MQTT 数据包格式错误（`frame_error`）发生时，会增加 `shutdown_count` 计数器。

- [#11661](https://github.com/emqx/emqx/pull/11661) 修复了文件日志格式类型配置 `log.HANDLER.formatter` 设置为 `json` 时的问题。

  该 bug 在 v5.0.4 中引入，导致日志行不再是有效的 JSON，而是以时间戳字符串和级别名称作为前缀。

- [#11627](https://github.com/emqx/emqx/pull/11627) 修复了 HStreamDB 桥接中的资源清理问题。在此修复之前，HStreamDB 桥接在桥接配置更新期间可能会报告错误，因为 hstreamdb 客户端/生产者没有被正确停止。
