# 版本发布

## v5.3.1

### 增强

- [#11637](https://github.com/emqx/emqx/pull/11637) 增加了额外的诊断检查，以帮助调试当 Mnesia 因等待表而停滞时出现的问题。更新依赖库：`ekka` 已升级至 0.15.15 版本，`mria` 已升级至 0.6.4 版本。

### 修复

- [#11565](https://github.com/emqx/emqx/pull/11565) 将 jq 库从 v0.3.10 升级至 v0.3.11。在此版本中，jq_port 程序将按需启动，除非 EMQX 中使用 jq 功能，否则不会出现在用户的进程中。此外，空闲的 jq_port 程序将在设定的一段时间后自动终止。注意：大多数运行 NIF 模式下的 EMQX 用户不会受到此更新的影响。

- [#11676](https://github.com/emqx/emqx/pull/11676) 隐藏 DEBUG 级别的日志中的部分敏感信息。

- [#11697](https://github.com/emqx/emqx/pull/11697) 在 EMQX 后端网络 (`gen_rpc`) 中禁用了过时的 TLS 版本和密码套件。增加了对后端网络的 tlsv1.3 支持，并引入了新的配置参数：`EMQX_RPC__TLS_VERSIONS` 和 `EMQX_RPC__CIPHERS`。

  对应的 `gen_rpc` PR: https://github.com/emqx/gen_rpc/pull/36

- [#11734](https://github.com/emqx/emqx/pull/11734) 修复了 IPv6 网络中集群配置的问题。新增了新的配置项 `rpc.listen_address` 和 `rpc.ipv6_only`，以允许 EMQX 集群的 RPC 服务和客户端使用 IPv6。

- [#11796](https://github.com/emqx/emqx/pull/11796) 修复了 RPC schema，以确保客户端和服务器使用相同的传输驱动程序。

- [#11798](https://github.com/emqx/emqx/pull/11798) 修复了在执行 `./bin/emqx data import [FILE]` 后节点无法启动的问题。

  同时增强了 `apikey_key` 和 `apikey_name` 之间的关联以提高一致性和唯一标识性：

  - `apikey_key`：通过 Dashboard 生成 API 密钥时，`apikey_key` 现在会根据提供的易读性较强的 `apikey_name` 创建一个唯一值。
  - `apikey_name`：相反，当使用引导文件生成 API 密钥时，`apikey_name` 将基于关联的 `apikey_key` 生成为唯一值。

- [#11813](https://github.com/emqx/emqx/pull/11813) 修复了 schema，确保 RPC 客户端 SSL 端口与配置的服务器端口一致。此修复还确保了RPC 端口在 Helm 图表中的被正确打开。

- [#11819](https://github.com/emqx/emqx/pull/11819) 升级了 OpenTelemetry 库至 v1.3.1-emqx。该版本修复了在导出的指标中指标时间戳无效的问题。

- [#11861](https://github.com/emqx/emqx/pull/11861) 修复了 remote shell 中打印过多警告信息的问题。

- [#11733](https://github.com/emqx/emqx/pull/11733) 解决了一个不兼容性问题，该问题导致在会话接管或通道驱逐时，如果会话位于运行 EMQX v5.2.x 或更早版本的远程节点上，可能会导致崩溃。

- [#11750](https://github.com/emqx/emqx/pull/11750) 日志不再输出使用 HTTP 服务进行认证和 HTTP 服务数据桥接的请求 Body。

- [#11886](https://github.com/emqx/emqx/pull/11886) 修复了一个向后的插件兼容性的问题。目前，EMQX 对钩子点名称进行验证，无效的钩子点不能用于注册钩子。但是旧版本的插件模板使用了一些拼写错误的钩子点，实际使用中的插件也可能存在这种问题，为了兼容以前的插件，我们允许使用旧的钩子点来注册钩子，但会发出它们已被弃用的警告，这些钩子与以前一样不会被调用

- [#11897](https://github.com/emqx/emqx/pull/11897) 修复了当集群节点大约在同一时间启动时，节点间配置同步的时候等待循环竞争条件的问题。

## v5.3.0

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

- [#11682](https://github.com/emqx/emqx/pull/11682) 修复了在文件日志处理程序上将“旋转大小”设置为`infinity`时日志记录停止的问题。

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

- [#11667](https://github.com/emqx/emqx/pull/11667) 禁止 API 密钥访问 `logout` 端点。该端点仅供 Dashboard 使用。