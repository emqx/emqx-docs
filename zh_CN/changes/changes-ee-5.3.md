# 版本发布

## e5.3.1

### 增强

- [#11637](https://github.com/emqx/emqx/pull/11637) 增加了额外的诊断检查，以帮助调试当 Mnesia 因等待表而停滞时出现的问题。Library 更新：`ekka` 已升级至版本0.15.15，`mria` 已升级至版本0.6.4。
- [#11581](https://github.com/emqx/emqx/pull/11581) 功能预告：引入了一个新的 "连接器" 概念，支持桥接设计版本2。
  - 在之前的桥接设计版本1中，每个连接器专属于单个桥接。虽然这种设计优先考虑了错误隔离和性能，但不便于需要将多个桥接设置到同一个服务的用户。例如，将10个桥接设置到单个 Kafka 集群需要为每个桥接重复相同的配置。
  - 改进后的桥接版本2设计提供了更大的灵活性和更好的可扩展性。用户可以选择将连接器共享给多个桥接，或者与版本1设计类似，将其专用于一个桥接。对于大多数数据桥接而言，将连接器共享给过多的桥接可能会导致性能下降，但在管理大量桥接时，可以避免过多连接给外部系统带来负担。对于某些数据桥接，即使连接器被共享，也始终使用专用连接。桥接设计版本2已应用于以下数据桥接：
    - Kafka生产者
    - Azure事件中心生产者
  - 引入连接器增加了设置的灵活性。为了单独管理连接器，引入了新的 API 端点，在`/connectors ` 路径下提供连接器管理功能。版本2的桥接可以通过`/bridges_v2`端点进行管理。
  - 在版本 e5.3.1中的限制：在此版本中，只有 Kafka 和 Azure Event Hub 生产者桥接已升级到 v2设计。v2 桥接功能可以通过配置文件和 HTTP API 访问，但尚不在 Dashboard UI 中提供。

### 修复

- [#11565](https://github.com/emqx/emqx/pull/11565) 将 jq 库从 v0.3.10升级至v0.3.11。在此版本中，jq_port 程序将按需启动，除非 EMQX 中使用 jq 功能，否则不会出现在用户的进程中。此外，空闲的 jq_port 程序将在设定的一段时间后自动终止。注意：大多数运行 NIF 模式下的 EMQX 用户不会受到此更新的影响。

- [#11676](https://github.com/emqx/emqx/pull/11676) 从调试级别的日志中隐藏了一些敏感信息。

- [#11697](https://github.com/emqx/emqx/pull/11697) 在 EMQX 后端网络 (`gen_rpc`) 中禁用了过时的 TLS 版本和密码套件。增加了对后端网络的 tlsv1.3 支持，并引入了新的配置参数：`EMQX_RPC__TLS_VERSIONS` 和 `EMQX_RPC__CIPHERS`。

  对应的 `gen_rpc` PR: https://github.com/emqx/gen_rpc/pull/36

- [#11734](https://github.com/emqx/emqx/pull/11734) 修复了 IPv6 网络中集群配置的问题。新增了新的配置项 `rpc.listen_address` 和 `rpc.ipv6_only`，以允许 EMQX 集群的 RPC 服务器和客户端使用 IPv6。

- [#11747](https://github.com/emqx/emqx/pull/11747) 更新了 QUIC 栈到 msquic 2.2.3 版本。


- [#11796](https://github.com/emqx/emqx/pull/11796) 修复了 RPC schema，以确保客户端和服务器使用相同的传输驱动程序。


- [#11798](https://github.com/emqx/emqx/pull/11798) 修复了在执行 `./bin/emqx data import [FILE]` 后节点无法启动的问题。

  同时增强了 `apikey_key` 和 `apikey_name` 之间的关联以提高一致性和唯一标识性：

  - `apikey_key`：通过 Dashboard 生成 API 密钥时，`apikey_key` 现在会根据提供的易读性较强的 `apikey_name` 创建一个唯一值。
  - `apikey_name`：相反，当使用引导文件生成 API 密钥时，`apikey_name` 将基于关联的 `apikey_key` 生成为唯一值。

- [#11813](https://github.com/emqx/emqx/pull/11813) 修复了 schema，确保 RPC 客户端 SSL 端口与配置的服务器端口一致。此修复还确保了RPC 端口在 Helm 图表中的被正确打开。

- [#11819](https://github.com/emqx/emqx/pull/11819) 升级了 opentelemetry 库至 v1.3.1-emqx。这个 opentelemetry 版本修复了在导出的指标中，指标时间戳无效的问题。

- [#11861](https://github.com/emqx/emqx/pull/11861) 修复了在远程控制台 shell 中打印过多警告信息的问题。


- [#11722](https://github.com/emqx/emqx/pull/11722) 修复了同步请求模式下的 Kafka 生产者桥接在`正在连接`状态下无法缓存消息的问题。
- [#11724](https://github.com/emqx/emqx/pull/11724) 修复了一个与统计指标相关的问题，即消息发送到 Kafka 时，由于内部缓存、即使后来被成功传输，仍然被计为发送失败。
- [#11728](https://github.com/emqx/emqx/pull/11728) 改进了 LDAP 过滤字符串解析器，具体改进如下：
  - 自动转义过滤字符串中的特殊字符。
  - 修复了先前阻止使用 `dn` 作为过滤值的错误。
- [#11733](https://github.com/emqx/emqx/pull/11733) 解决了一个不兼容性问题，该问题导致在会话接管或通道驱逐时，如果会话位于运行 EMQX v5.2.x 或更早版本的远程节点上，可能会导致崩溃。
- [#11750](https://github.com/emqx/emqx/pull/11750) 在使用 HTTP 服务进行认证和 HTTP 服务数据桥接中取消了对 HTTP 请求体的日志记录和追踪。
- [#11760](https://github.com/emqx/emqx/pull/11760) 简化了用于 Cassandra 数据桥接健康检查的 CQL 查询，之前该查询在 Cassandra 服务器日志中生成了警告。 

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

- [#11627](https://github.com/emqx/emqx/pull/11627) 修复了 HStreamDB 桥接中的资源清理问题。在此修复之前，HStreamDB 桥接在桥接配置更新期间可能会报告错误，因为 hstreamdb 客户端/生产者没有被正确停止。
