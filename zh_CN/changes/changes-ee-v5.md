# v5 版本

## 5.4.1

*发布日期: 2024-01-05*

### 增强

- [#12234](https://github.com/emqx/emqx/pull/12234) 解决了 EMQX 5.4.0 之前版本在 `emqx.conf` 中定义的 Open Telemetry 配置的兼容性问题，确保最新 EMQX 发布版本能够平滑兼容旧版配置。
- [#12236](https://github.com/emqx/emqx/pull/12236) MQTT Sink/Source 使用简短的客户端 ID。
- [#12238](https://github.com/emqx/emqx/pull/12238) 增强了与 EMQX 5.3.2 版本中 HTTP Action 功能引入的错误格式配置的兼容性。
- [#12240](https://github.com/emqx/emqx/pull/12240) 修改了 `/file_transfer` REST API，按照原始格式返回配置，避免将时间单位（如 "1h"）转换为秒，确保调用者接收到初始配置的值，此修改与其他 GET API 保持一致的数据格式。
- [#12241](https://github.com/emqx/emqx/pull/12241) 修复了配置额外的 S3 HTTP 请求头导致文件传输中断的问题，确保稳定且不间断的文件传输操作。
- [#12246](https://github.com/emqx/emqx/pull/12246) 停止在 Docker 中默认暴露不再使用的 11883 端口，并从 Helm  Chart 中移除。
- [#12249](https://github.com/emqx/emqx/pull/12249) 修复了 `/configs` API 中尝试修改只读配置值导致响应消息乱码的问题。
- [#12250](https://github.com/emqx/emqx/pull/12250) 解决了 `file_transfer` 配置的 `secret_access_key` 值错误更新为掩码星号 ('*****') 的问题，确保原始密钥值保持不变以保证安全性。
- [#12256](https://github.com/emqx/emqx/pull/12256) 修复了没有密码就无法与 MySQL 资源建立连接的问题。

## 5.4.0

*发布日期: 2023-12-23*

### 增强

- [#11884](https://github.com/emqx/emqx/pull/11884) 对 Prometheus API 及其配置进行了以下改进：

  - 重构了配置部分，将相关设置分组，提高了可读性和可维护性。
  - 引入了 `enable_basic_auth` 配置项，用于 scrape API 端点的基本认证，增强了安全性。
  - 在重构代码的同时保持了向后兼容性，避免了破坏性的变更。

- [#11896](https://github.com/emqx/emqx/pull/11896) 引入了在桥接配置中设置敏感认证字段（如密码、令牌和密钥）的增强功能。此改进允许使用以文件形式存储在文件系统中的秘密信息。这些秘密信息可以通过在配置文件中使用特殊的 `file://` 前缀安全地引用，从而增强了桥接配置中敏感数据处理的安全性。

- [#11921](https://github.com/emqx/emqx/pull/11921) 引入了 Open Telemetry 日志处理进程，该进程允许按照 Open Telemetry 日志数据模型格式化日志事件。此处理程序便于将格式化的日志事件导出到配置的 Open Telemetry 收集器或后端，从而增强了日志管理和集成能力。

- [#11935](https://github.com/emqx/emqx/pull/11935) 默认切换到新的`v2`路由存储模式。新模式提升了订阅和路由性能，尤其是在具有共同通配符前缀的主题过滤器的并发订阅场景中更为显著。但这也会带来轻微的内存使用增加。该模式还消除了对单独索引的需求，从而解决了在以往版本中偶尔遇到的路由状态不一致问题。

  如果集群是从旧版本进行滚动升级，那么集群将继续使用`v1`存储模式，直到发生全集群（非滚动）重启。

  用户仍可以通过将`broker.routing.storage_schema` 配置选项设置为`v1`来选择以前的模式。但是，这也需要完整的非滚动集群重启才能生效。

- [#11984](https://github.com/emqx/emqx/pull/11984)  实现了 Open Telemetry 分布式追踪特性。

- [#12017](https://github.com/emqx/emqx/pull/12017) 实现了一个专用的 REST API，用于配置和用户数据的导入和导出。

- [#12040](https://github.com/emqx/emqx/pull/12040) 升级了 QUIC 协议栈。

- [#12201](https://github.com/emqx/emqx/pull/11994) 添加了对 TCP/SSL/WS/WSS MQTT 监听器配置的热更新支持。这个功能允许您在无需重新启动监听器和断开客户端连接的情况下修改大多数配置参数。然而，目前有一些限制：

  - 对于 TCP/SSL 监听器，仍然需要重新启动监听器并重新连接客户端才能更改以下参数：
    - `bind`
    - `tcp_options.backlog`
  - 对于 WS/WSS（WebSocket）监听器，修改与传输相关的参数（如下所示）将导致监听套接字被重新打开，但已建立的连接将保持不间断。
    - `bind`
    - `tcp_options.*`
    - `ssl_options.*`

- [#11608](https://github.com/emqx/emqx/pull/11608) 客户端认证 LDAP 数据源支持通过绑定操作进行认证，提供了更多灵活性和安全性的用户认证方式。

- [#11766](https://github.com/emqx/emqx/pull/11766) 为 REST API 实现了初步的基于角色的访问控制。在这个版本中，有三个预定义的角色：

  - 管理员：此角色可以访问所有资源。
  - 查看者：此角色只能查看资源和数据，对应于 REST API 中的所有 GET 请求。
  - 发布者：专门为 MQTT 消息发布定制，此角色仅限于访问与消息发布相关的端点。

- [#11773](https://github.com/emqx/emqx/pull/11773) Dashboard 中添加了审计日志管理页面，用户可以使用该页面查看对 EMQX 设备和数据进行的所有更改操作，例如踢出设备、创建/删除规则等。

- [#11778](https://github.com/emqx/emqx/pull/11778) Dashboard 单点登录中的 SAML 协议支持与 Azure Entra ID 进行集成。


- [#11811](https://github.com/emqx/emqx/pull/11811) 优化了 REST API 密钥引导文件的格式，以支持使用角色初始化密钥。

  新的格式为：`api_key:api_secret:role`。

  其中 `role` 是可选的，默认值为 `administrator`。

- [#11852](https://github.com/emqx/emqx/pull/11852) 新增了 GB/T 32960 协议网关，使车辆能够通过 GB/T 32960 车联网协议与 EMQX 连接。

- [#11883](https://github.com/emqx/emqx/pull/11883) 新增了 JT/T808 协议网关，使车辆能够通过 JT/T 808 车联网协议与 EMQX 连接。

- [#11885](https://github.com/emqx/emqx/pull/11885) 新增了 OCPP 网关，使电动车（EV）充电站能够通过 OCPP (Open Charge Point Protocol) 协议访问 EMQX。

- [#11971](https://github.com/emqx/emqx/pull/11971) 将 `/api/v5/load_rebalance/availability_check` 接口设为公共接口，即不再需要进行身份验证。这一变更简化了负载均衡器的设置。

  此外，它还改善了等待健康检查阶段的负载均衡重平衡/疏散过程的流畅性。现在，在此阶段，不会禁止连接到被标记为要疏散的节点。这个调整是因为无法确定负载均衡器是否已将所有这些节点标记为不健康。禁止连接到它们可能会导致多次不成功的重新连接尝试。
  
- [#12013](https://github.com/emqx/emqx/pull/12013) 调整数据桥接设计，将其拆分为连接器与动作（Sink）。连接用于管理数据集成与外部系统的连接，可以在多个动作之间重复使用，动作仅用于配置数据操作方式。这个设计能够提供更大的灵活性和更好的可扩展性，实现更清晰的数据集成配置与管理。

  已调整的数据桥接有包括 PostgreSQL, Timescale 和 Matrix，现在拆分为连接器和动作 API，不过它们仍然与旧的数据桥接 API 兼容。

- [#12016](https://github.com/emqx/emqx/pull/12016) 增强了许可证密钥管理。

  EMQX 现在可以从指定文件加载许可证密钥。通过将 `license.key` 配置设置为文件路径，并使用 `"file://"` 作为前缀来启用此功能。 还添加了通过设置 `license.key = default` 来恢复到默认试用许可证的功能。此选项简化了在需要时返回试用许可证的过程。
  
- [#12129](https://github.com/emqx/emqx/pull/12129) 续期默认的 License，替换了 2023 年 1 月发布的旧 License。与此同时还将 License 规格从 100 并发连接调整为 25 个并发连接。

### 修复

- [#10976](https://github.com/emqx/emqx/pull/10976) 修复共享订阅中的主题过滤器重复处理问题。 在之前的实现中，订阅选项的存储方法没有充分适配共享订阅，这导致在特定的主题和流程下，”订阅-取消订阅” 期间消息路由失败并且节点之间的路由表出现泄漏问题。
- [#12048](https://github.com/emqx/emqx/pull/12048) 修复 COAP 网关忽略订阅选项的错误。
- [#12078](https://github.com/emqx/emqx/pull/12078) 升级了 grpc-erl 到版本 0.6.12。此更新解决了潜在的死锁问题，其中 grpc 客户端延迟启动了依赖的应用程序。
- [#12081](https://github.com/emqx/emqx/pull/12081) 更新了 `gen_rpc` 库到版本 3.3.1。这个新版本包括了一些性能改进：
  - 在某些情况下避免为数据包在发送到网络之前分配额外的内存。
  - 对于本地调用，绕过了网络层。
  - 避免敏感信息打印到日志中 [#12202](https://github.com/emqx/emqx/pull/12202)。
- [#12111](https://github.com/emqx/emqx/pull/12111) 修复了一个问题，该问题导致 API 令牌因为竞态条件在登录后立即不可用。
- [#12121](https://github.com/emqx/emqx/pull/12121) 修复了在不同节点同时更新配置时，集群中的节点偶尔会返回旧视图的问题。
- [#12158](https://github.com/emqx/emqx/pull/12158) 修复规则引擎无法连接到 [Upstash](https://upstash.com/) Redis 的问题。修复前，在与 Redis 服务建立 TCP 连接之后，EMQX 的 Redis 驱动程序使用 [inline commands](https://redis.io/docs/reference/protocol-spec/#inline-commands) 来发送 AUTH 和 SELECT 命令。但 Upstash Redis 服务不支持 inline commands，导致 EMQX 无法连接到 Upstash Redis 服务。 修复后，EMQX 的 Redis 驱动使用 RESP (Redis Serialization Protocol) 来发送 AUTH 和 SELECT 命令。
- [#12176](https://github.com/emqx/emqx/pull/12176) 无论之前是否成功建立连接，始终向 MQTT-SN 客户端发送 "DISCONNECT" 数据包的确认。
- [#12180](https://github.com/emqx/emqx/pull/12180) 修复了 MQTT-SN 网关因 DTLS 相关配置兼容性问题导致监听器无法启动的问题。
- [#12219](https://github.com/emqx/emqx/pull/12219) 修复了从 Dashboard 更新文件传输 S3 配置时密钥反混淆的问题。

## 5.3.2

*发布日期: 2023-12-01*

### 增强

- [#11752](https://github.com/emqx/emqx/pull/11752) 将 core-replica 数据库同步的默认 RPC 驱动从 `gen_rpc` 更改为 `rpc`。

  这提升了核心副本数据复制的速度。

- [#11785](https://github.com/emqx/emqx/pull/11785) 拥有“查看者”角色的用户具有更改自己密码的权限，但无权更改其他用户密码。

- [#11787](https://github.com/emqx/emqx/pull/11787) 提升了 `emqx` 命令的性能。

- [#11790](https://github.com/emqx/emqx/pull/11790) 为 Redis 授权数据源中的 Redis 命令添加了验证功能。此外，此次改进优化了认证和授权过程中 Redis 命令的解析，现在的解析符合 `redis-cli` 兼容性标准，并支持引号参数。

- [#11541](https://github.com/emqx/emqx/pull/11541) 文件传输能力得到了增强。现在，客户端可以使用异步方式，通过 `$file-async/...` 主题进行文件传输，并通过 `$file-response/{clientId}` 主题订阅命令执行结果。这一改进简化了文件传输功能的使用，尤其适用于 MQTT v3.1/v3.1.1 或使用了 MQTT 桥接的客户端。 更多详情请参阅 [EIP-0021](https://github.com/emqx/eip)。

### 修复

- [#11757](https://github.com/emqx/emqx/pull/11757) 修复了下载不存在的追踪文件时返回的错误响应码。现在，响应码会返回 `404` 而不是 `500`。

- [#11762](https://github.com/emqx/emqx/pull/11762) 修复了 EMQX 中 `built_in_database` 授权数据源的一个问题。通过这次修复，现在在删除数据源时，所有 ACL 记录都会被彻底移除。这解决了数据库残留的记录在重新创建授权数据源时仍然存在的问题。

- [#11771](https://github.com/emqx/emqx/pull/11771) 修复了通过 API/Dashboard 进行身份验证管理时 Bcrypt 盐轮次(salt rounds)的验证问题。

- [#11780](https://github.com/emqx/emqx/pull/11780) 修复了 `pbkdf2` 密码哈希算法中 `iterations` 字段的验证问题。现在，`iterations` 必须是严格正数。之前，`iterations` 可以被设置为 0，这会导致验证器无法正常工作。

- [#11791](https://github.com/emqx/emqx/pull/11791) 修复了 EMQX CoAP 网关中的一个问题，即心跳没有有效地维持连接的活跃状态。此修复确保心跳机制正确维持 CoAP 网关连接的活跃状态。

- [#11797](https://github.com/emqx/emqx/pull/11797) 修改了管理 `built_in_database` 授权数据源的 HTTP API 行为。如果未将 `built_in_database` 设置为授权数据源，这些 API 现在将返回 `404` 状态码，替换了以前的 `20X` 响应。

- [#11965](https://github.com/emqx/emqx/pull/11965) 优化了 EMQX 服务的终止过程，确保即使在存在不可用的 MongoDB 资源的情况下，也能够实现优雅停止。

- [#11975](https://github.com/emqx/emqx/pull/11975) 此修复解决了由于对端和服务器同时关闭套接字时发生竞争条件导致的冗余错误日志问题。以前，由操作系统和 EMQX 触发的并发套接字关闭事件会导致不必要的错误记录。通过改进事件处理，本次修复消除了不必要的错误信息。

- [#11987](https://github.com/emqx/emqx/pull/11987) 修复了在尝试设置 TCP/SSL 套接字的 `active_n` 选项时连接崩溃的问题。

  在此修复之前，如果在连接过程中尝试设置 `active_n` 选项时套接字已经关闭，会导致 `case_clause` 崩溃。

- [#11731](https://github.com/emqx/emqx/pull/11731) 为文件传输功能添加了热配置支持。

- [#11754](https://github.com/emqx/emqx/pull/11754) 改进了 Postgres 桥接的日志格式化功能，针对驱动程序返回的错误消息中的 Unicode 字符进行了处理。

## 5.3.1

*发布日期: 2023-11-14*

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


## 5.3.0

*发布日期: 2023-09-29*

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

## 5.2.1

*发布日期: 2023-09-20*

### 增强

- [#11487](https://github.com/emqx/emqx/pull/11487) 将认证功能中，基于 bcrypt 算法的密码加密的计算强度因子 (work factor) 限制在5-10的范围内，因为较高的值会消耗太多 CPU 资源。Bcrypt 库已更新以允许并行哈希计算。
- [#11568](https://github.com/emqx/emqx/pull/11568) 在消息重发布规则动作中，支持设置 MQTT 5.0 发布属性与用户属性。目前配置接口暂未完全集成到 Dashboard，将在后续版本中提供支持。
- [#11612](https://github.com/emqx/emqx/pull/11612) 在节点疏散期间，疏散所有断开连接的会话，而不仅仅是那些以 `clean_start` 设置为 `false` 开始的会话。
- [#11532](https://github.com/emqx/emqx/pull/11532) 改进了解析无效数据包时的错误消息，以提供更清晰的错误提示。

### 修复

- [#11493](https://github.com/emqx/emqx/pull/11493) 修复了REST API 示例文档中关于 `/api/v5/publish` 错误请求响应的描述。之前的文档示例指出错误请求的响应可以在响应体中返回一个列表，但实际情况并非如此。
- [#11499](https://github.com/emqx/emqx/pull/11499) 升级 Erlang/OTP 至 25.3.2-2，此版本从 mnesia_hook 日志消息中排除了敏感数据。
- [#11506](https://github.com/emqx/emqx/pull/11506) 此前尝试下载不存在的跟踪日志文件时，会下载一个空的文件。在实施此修复后，尝试使用 GET 请求 `/api/v5/trace/clientempty/download` 下载不存在的跟踪日志文件时，服务器现在将返回 404 状态码以及以下 JSON 消息：`{"code":"NOT_FOUND","message":"Trace is empty"}`。
- [#11522](https://github.com/emqx/emqx/pull/11522) 在规则引擎的编解码功能中，改进了当 schema 名称超出允许的长度时出现的错误消息。
- [#11531](https://github.com/emqx/emqx/pull/11531) 修复了针对某个特定的客户端 ID，授权缓存清理 CLI 无法正常工作的问题。
- [#11564](https://github.com/emqx/emqx/pull/11564) 修复了集群分区自动恢复功能。实施了对分裂成多个分区的集群的自动恢复。
- [#11568](https://github.com/emqx/emqx/pull/11568) 修复了一个未明确定义的内置规则动作配置，以避免该配置被理解为自定义用户函数。
- [#11394](https://github.com/emqx/emqx/pull/11394) 将 Kafka 生产者客户端 `wolff` 从1.7.6版本升级到1.7.7版本。这个升级修复了一个潜在的竞态条件，可能会导致在有些 Kafka 生产者初始化失败时所有的 Kafka 生产者崩溃。
- [#11401](https://github.com/emqx/emqx/pull/11401) 修复了在 EMQX Dashboard 中对 SQL 语句进行测试时，规则 SQL 函数 `mongo_date` 的行为。规则 SQL 函数 `mongo_date` 现在在测试模式下返回具有格式 `ISODate(*)` 的字符串，其中 * 是 ISO 日期字符串。这个格式与 MongoDB 存储日期的方式保持一致。
- [#11547](https://github.com/emqx/emqx/pull/11547) 修复了几个 emqx_bridge 的问题：
  - 修复了 Cassandra 数据桥接在没有配置用户名/密码时出现连接错误的问题 （当配置为 `authenticator: AllowAllAuthenticator` 时，Cassandra 不需要用户凭据。）
  - 修复了因为空密码而导致 SQL Server 数据桥接连接错误的问题。
  - 将 Oracle 数据桥接中的 `username` 字段设置为必填项。
  - 修复了 IoTDB 数据桥接因未设置基础 URL 的模式（例如 `<host>:<port>`）而导致的错误。
- [#11630](https://github.com/emqx/emqx/pull/11630) 修复了核心节点可能会卡在 `mria_schema:bootstrap/0` 状态，导致新节点加入集群失败。

## 5.2.0

*发布日期: 2023-09-07*

### 增强

- [#10697](https://github.com/emqx/emqx/pull/10697) 此增强功能允许配置 StatefulSet 的 `minReadySeconds`，从而允许在升级或重新启动命令触发的每个 pod 重新启动之间引入时间间隔。

- [#11124](https://github.com/emqx/emqx/pull/11124) 发布了适用于 Amazon Linux 2023 的软件包。

- [#11289](https://github.com/emqx/emqx/pull/11289) 发布了适用于 Debian 12 的软件包。

- [#11290](https://github.com/emqx/emqx/pull/11290) 更新了 `jq` 依赖项至版本 0.3.10，其引用的 `oniguruma` 库更新至版本 6.9.8，修复了一些小的安全问题。

- [#11291](https://github.com/emqx/emqx/pull/11291) 通过 ekka 更新至版本 0.15.6，将 RocksDB 版本更新至 1.8.0-emqx-1。

- [#11390](https://github.com/emqx/emqx/pull/11390) 向 EMQX 配置添加了 `node.broker_pool_size`、`node.generic_pool_size` 和 `node.channel_cleanup_batch_size` 选项。如果集群互连网络延迟较高，调整这些选项可以显著提高性能。

- [#11429](https://github.com/emqx/emqx/pull/11429) 在 MondoDB 连接和桥接中添加了配置检测遗留协议的选项。

- [#11436](https://github.com/emqx/emqx/pull/11436) 添加了新的 REST API `DELETE /banned`，用于清除所有黑名单数据。

- [#11438](https://github.com/emqx/emqx/pull/11438) 将 `mqtt.max_packet_size` 的类型从字符串更改为 byteSize，以更好地表示有效的数字范围。仍然支持字符串以确保向后兼容性。

- [#11469](https://github.com/emqx/emqx/pull/11469) 支持在 Redis 认证中指定用户名。

- [#11496](https://github.com/emqx/emqx/pull/11496) 默认情况下禁用 Erlang VM Prometheus 导出器，以提高性能和安全性。

- [#11497](https://github.com/emqx/emqx/pull/11497) 通过添加新的消息、过载保护、授权、身份验证指标，改进 OpenTelemetry 的命名一致性，增强了指标可观测性。

- [#10647](https://github.com/emqx/emqx/pull/10647) 新增了 [GreptimeDB](https://github.com/GreptimeTeam/greptimedb) 数据集成。

- [#11261](https://github.com/emqx/emqx/pull/11261) 新增了 Amazon Kinesis Data Streams 生产者数据集成。

- [#11329](https://github.com/emqx/emqx/pull/11329) 新增了 Azure Event Hub 生产者数据集成。

- [#11363](https://github.com/emqx/emqx/pull/11363) 为 RabbitMQ 桥接添加了 TLS 连接支持。

- [#11367](https://github.com/emqx/emqx/pull/11367) 从 EMQX 4.4 迁移了 GCP IoT Hub 认证支持。

- [#11386](https://github.com/emqx/emqx/pull/11386) 认证器新增了 LDAP 数据源。

- [#11392](https://github.com/emqx/emqx/pull/11392) 授权管理器新增了 LDAP 数据源。

- [#11402](https://github.com/emqx/emqx/pull/11402)  Kafka 消费者桥接支持使用占位符动态设置 MQTT 主题。

- [#11403](https://github.com/emqx/emqx/pull/11403) 添加了支持定义 GCP PubSub 生产者桥接的消息属性和排序键模板。还更新了我们的 HOCON 库，以修复一个问题，即数组中的对象即使位于不同的行上也会被串联在一起。

- [#11459](https://github.com/emqx/emqx/pull/11459) 添加了配置 Kafka 桥接的健康检查间隔的选项。

- [#11478](https://github.com/emqx/emqx/pull/11478) 添加了对 HStreamDB 桥接的支持（允许 TCP 和 TLS 连接），并适配了 HStreamDB `v0.16.1`。

  在 [PR#11530](https://github.com/emqx/emqx/pull/11530) 中更新了驱动程序至 `0.4.5+v0.16.1`。

- [#11389](https://github.com/emqx/emqx/pull/11389) 通过利用 Mria 0.6.0 中引入的新 API 将多个索引更新操作合并为单个 Mnesia 事务来提高保留消息发布的速度。

- [#11396](https://github.com/emqx/emqx/pull/11396) 为规则引擎运行时引入了主题索引，提高了消息主题与规则 SQL 中的主题过滤器匹配的速度，避免了对规则集的全面扫描，大幅提升了 EMQX 在处理大量规则时的性能。

- [#11399](https://github.com/emqx/emqx/pull/11399) 改进了规则引擎中的占位符语法。发布操作支持使用占位符语法动态填充 payload 变量中的内容。 占位符语法的格式为 `\${key}`。 在此改进之前，`\${key}` 中只能包含字母、数字和下划线。现在，`\${key}` 支持任何 UTF8 字符。

- [#11405](https://github.com/emqx/emqx/pull/11405) 改进了 `date_to_unix_ts` 的错误原因以便理解。

- [#11490](https://github.com/emqx/emqx/pull/11490) 为各种认证后端添加了未定义密码的快速错误处理。这提高了认证过程的一致性和用户友好性。

### 修复

- [#11065](https://github.com/emqx/emqx/pull/11065) 修复了在 EMQX 关闭过程中防止日志记录无关错误消息的问题。
- [#11279](https://github.com/emqx/emqx/pull/11279) 修复了当在 EMQX 启用了 debug/trace 日志记录时客户端无法发送包含大型 payload 消息的问题。
- [#11296](https://github.com/emqx/emqx/pull/11296) 添加了从 EMQX 备份文件中使用 `emqx ctl import` 命令导入附加配置的支持：
  - rule_engine（以前由于错误而未导入）
  - topic_metrics（以前未实现）
  - slow_subs（以前未实现）
- [#11327](https://github.com/emqx/emqx/pull/11327) 更新了 ekka 到版本 0.15.8，mria 到版本 0.15.8，以及 optvar 到 1.0.5。 这修复了偶发的断言失败问题。
- [#11346](https://github.com/emqx/emqx/pull/11346) 更新了 ekka 到版本 0.15.9。 这修复了在获取锁定超时时出现的悬挂的 etcd 锁定问题。
- [#11347](https://github.com/emqx/emqx/pull/11347) 确保 OCSP 请求路径正确进行了 URL 编码。
- [#11352](https://github.com/emqx/emqx/pull/11352) 修复了在 Windows 或其他不支持 RocksDB 的平台上启动时出现的崩溃问题。
- [#11388](https://github.com/emqx/emqx/pull/11388) 增加了 `emqx_router_sup` 重启强度，以提高对在正常情况下发生偶发崩溃的容忍度，而无需关闭整个 EMQX 应用程序。 例如， 如果核心节点正在停止、重新启动或处于不可用状态，从复制节点委派给 `emqx_router_helper` 的 mria 写入/删除调用可能会失败。修改后的重启强度确保系统保持稳定运行。
- [#11424](https://github.com/emqx/emqx/pull/11424) 添加了对 API 中时间戳的最大值的检查，以确保它是有效的 Unix 时间戳。
- [#11445](https://github.com/emqx/emqx/pull/11445) 删除了 Windows 平台上的 os_mon 应用程序监控支持，以防止虚拟机崩溃。 该功能仍然适用于非 Windows 平台。
- [#11454](https://github.com/emqx/emqx/pull/11454) 修复了在调试/跟踪大型 payload 时出现的崩溃问题（在 [#11279](https://github.com/emqx/emqx/pull/11279) 中引入）。
- [#11456](https://github.com/emqx/emqx/pull/11456) 移除了对 CA 证书文件强制要求非空 PEM 的验证，允许 CA 证书文件 PEM 为空。
- [#11466](https://github.com/emqx/emqx/pull/11466) 修复了将 `ssl_options.ciphers` 配置选项设置为空字符串（""）时出现崩溃的问题。
- [#11480](https://github.com/emqx/emqx/pull/11480) 改进了规则引擎中当规则函数接收到错误参数时的错误处理和 SQL 函数测试。
- [#11520](https://github.com/emqx/emqx/pull/11520) 修复了在发送带有非零 `ack_flag` 的 CONNACK 数据包时未增加 `packets_connack_sent` 指标的问题。
- [#11523](https://github.com/emqx/emqx/pull/11523) 更正了在为 `/configs` API 指定无效证书/密钥时出现的令人误解的提示。
- [#11534](https://github.com/emqx/emqx/pull/11534) 修复了当桥接状态不健康时数据桥接统计数据的增量。现在，发送到不健康桥接的消息将被计算为丢弃的消息。
- [#11540](https://github.com/emqx/emqx/pull/11540) 在尝试创建具有无效名称的桥接时，改进了 HTTP 响应。
- [#11548](https://github.com/emqx/emqx/pull/11548) 修复了在整个集群中更新插件顺序的问题。
- [#11366](https://github.com/emqx/emqx/pull/11366) 修复了在使用 EMQX Operator 在 `bootstrapConfig` 中指定一些桥接配置时可能会阻止 pod 启动的问题。
- [#11453](https://github.com/emqx/emqx/pull/11453) 修复了测试 InfluxDB 桥接的连接时可能产生虚假负面结果的问题。
- [#11461](https://github.com/emqx/emqx/pull/11461) 将测试桥接连接的超时更加紧密地与配置的健康检查超时保持一致。
- [#11492](https://github.com/emqx/emqx/pull/11492) 修复了测试 GreptimeDB 桥接的连接时可能产生虚假负面结果的问题。
- [#11508](https://github.com/emqx/emqx/pull/11508) 修复了 Kafka 桥接中将 header 翻译为无效值时的错误处理。
- [#11513](https://github.com/emqx/emqx/pull/11513) 修复了一个错误，该错误导致 Kafka 生产者桥接无法使用正确的模板的来处理 `timestamp` 字段。
- [#11527](https://github.com/emqx/emqx/pull/11527) 修复了与 Kafka header 模板处理相关的问题。该问题发生在占位符解析为键值对数组时（例如：`[{"key": "foo", "value": "bar"}]`）。

## 5.1.1

*发布日期: 2023-07-27*

### 增强

- [#10667](https://github.com/emqx/emqx/pull/10667) 将 MongoDB 连接器和桥接重构为单独的应用程序，以改进代码结构。

- [#11115](https://github.com/emqx/emqx/pull/11115) 添加了信息日志，标示由于存活时间（TTL）过期而丢弃的缓冲消息。

- [#11133](https://github.com/emqx/emqx/pull/11133) 在 `retainer` 的配置中将 `deliver_rate` 重命名为 `delivery_rate`，兼容之前的 `deliver_rate`。

- [#11137](https://github.com/emqx/emqx/pull/11137) 重构了 Dashboard 监听器配置，使用嵌套的 `ssl_options` 字段来进行 SSL 设置。

- [#11138](https://github.com/emqx/emqx/pull/11138)  将k8s `api_server `的默认值从 `http://127.0.0.1:9091 ` 更改为 `https://kubernetes.default.svc:443`。

  - 当 `discovery_strategy=static` 时，`emqx_ctl conf show cluster` 不再显示不相关的配置项。 与`etcd/k8s/dns` 相关的配置信息将不再显示。
  - 从 `emqx_ctl conf show_keys` 中删除了 `zones`（已弃用的配置键）。

- [#11165](https://github.com/emqx/emqx/pull/11165) 从 `swagger.json` 中删除了 `/configs/limiter` API 文档，API 仍然保留。

- [#11166](https://github.com/emqx/emqx/pull/11166) 在规则引擎 SQL 中添加了 3 个随机函数：

  - `random()`: 生成 0 到 1 之间的随机数（0.0 =< X < 1.0）。
  - `uuid_v4()`: 生成随机 UUID（版本4）字符串。
  - `uuid_v4_no_hyphen()`: 生成无连字符的随机 UUID（版本4）字符串。

- [#11180](https://github.com/emqx/emqx/pull/11180) 添加了一个新的配置 API `/configs`（GET/PUT），支持重新加载 HOCON 格式的配置文件。

- [#11226](https://github.com/emqx/emqx/pull/11226) 统一监听器开关配置项为 `enable`，同时兼容之前的`enabled`。

- [#11249](https://github.com/emqx/emqx/pull/11249) 添加了支持通过 REST API 设置 License 连接使用配额的告警阈值。

- [#11251](https://github.com/emqx/emqx/pull/11251) 添加了 `GET /cluster/topology` REST API，用来返回集群拓扑，显示 RLOG 核心节点和复制节点之间的连接。

- [#11253](https://github.com/emqx/emqx/pull/11253) 将 Webhook/HTTP 桥接重构为单独的 Erlang 应用，为将来的使用提供了灵活性，并允许将桥接作为独立应用程序运行。

- [#11079](https://github.com/emqx/emqx/pull/11079) Kafka 桥接，生产者模式下为消息添加了自定义 headers 支持。

- [#11132](https://github.com/emqx/emqx/pull/11132) MQTT 授权检查添加了 QoS 级别和保留消息条件，现在 EMQX 可以验证客户端是否有权限使用指定的 QoS 级别进行发布/订阅，以及有权限发布保留消息。

- [#11207](https://github.com/emqx/emqx/pull/11207) 更新了多个数据桥接的驱动版本，以增强安全性并确保敏感数据不会泄露。包括：

  - TDengine
  - MongoDB
  - MySQL
  - Clickhouse

- [#11241](https://github.com/emqx/emqx/pull/11241) 将 Schema Registry 重构为单独的 Erlang 应用以提供灵活性。

- [#11020](https://github.com/emqx/emqx/pull/11020) 升级了 emqtt 依赖项，以防止调试日志中的敏感数据泄漏。

- [#11135](https://github.com/emqx/emqx/pull/11135) 改进了规则引擎中的时间偏移解析器，并返回统一的错误代码。

- [#11236](https://github.com/emqx/emqx/pull/11236) 改进了默认参数下 `GET /clients` REST API 客户端查询的速度。

### 修复

- [#11004](https://github.com/emqx/emqx/pull/11004) 主题重写不再允许在目标主题中使用通配符。

- [#11026](https://github.com/emqx/emqx/pull/11026) 解决了规则引擎中 `div` 和 `mod` 操作使用的一致性问题。之前，`div` 操作只能作为中缀操作使用，`mod ` 只能通过函数调用使用用。现在，`div ` 和 `mod` 都可以通过函数调用语法和中缀语法使用。

- [#11037](https://github.com/emqx/emqx/pull/11037) 启动 HTTP 连接器时，如果系统无法连接到远程目标系统，EMQX 现在会返回一个描述性错误。

- [#11039](https://github.com/emqx/emqx/pull/11039) 修复了 Redis 连接器的数据库验证问题。之前，负数被接受为有效的数据库。

- [#11074](https://github.com/emqx/emqx/pull/11074) 修复了有关 MQTT-5.0 [MQTT-3.8.3-4] 协议内容的问题。

- [#11077](https://github.com/emqx/emqx/pull/11077) 修复了更新监听器时使用非整数的端口可能发生崩溃的问题。

- [#11094](https://github.com/emqx/emqx/pull/11094) 修复了 Kafka 桥接生产者模式在重新连接时无法报告连接错误的问题。

- [#11103](https://github.com/emqx/emqx/pull/11103) 更新了 `erlcloud` 依赖项。

- [#11106](https://github.com/emqx/emqx/pull/11106) 添加了对桥接资源 `worker_pool_size` 的最大数量的验证。现在最大数量为 1024，以避免因不合理数量的工作进程导致大内存消耗。

- [#11118](https://github.com/emqx/emqx/pull/11118) 确保 REST API 响应中的验证错误信息更加明确。现在，如果有超出范围的错误，将呈现为`{"value": 42, "reason": {"expected": "1..10"}, ...}`，替换了先前使用的 `expected_type`，改为使用 `expected`。

- [#11126](https://github.com/emqx/emqx/pull/11126) 修复了规则下有异步模式的桥接时，规则的失败指标计数问题。

- [#11134](https://github.com/emqx/emqx/pull/11134) 修复了日志中敏感字段大写 `authorization` header 的值不被混淆的问题。

- [#11139](https://github.com/emqx/emqx/pull/11139) 将 Redis 桥接器重构为单独的 Erlang 应用，以改进代码结构。

- [#11145](https://github.com/emqx/emqx/pull/11145) 在 Ekka 和 Mria 中进行了几处修复和改进。

  Ekka:

  - 改进了集群发现日志消息，以一致地描述实际事件。 [Ekka PR](https://github.com/emqx/ekka/pull/204)
  - 删除了弃用的集群自动清理配置参数（已移动到 Mria）。[Ekka PR](https://github.com/emqx/ekka/pull/203)

  Mria:

  - 现在 Ping 只在复制节点上运行。之前，`mria_lb `尝试同时 ping 停止和运行中的复制节点，可能导致超时错误。 [Mria PR](https://github.com/emqx/mria/pull/146)
  - 在复制 `$mria_rlog_sync` 表时使用 `null_copies` 存储。 此修复对 EMQX 目前没有影响，因为 `$mria_rlog_sync`仅在 `mria:sync_transaction/2,3,4` 中使用，而这在 EMQX 中未被使用。 [Mria PR](https://github.com/emqx/mria/pull/144)

- [#11148](https://github.com/emqx/emqx/pull/11148) 修复当一个节点离开集群时，其他节点仍然尝试将配置更新操作同步到其中的问题。

- [#11150](https://github.com/emqx/emqx/pull/11150) 在启动 `emqx_psk` 应用程序时等待 Mria 表，确保即使没有初始化 PSK 文件，PSK 数据也能同步到 replicant 节点。

- [#11151](https://github.com/emqx/emqx/pull/11151) 将 MySQL 桥接重构为单独的 Erlang 应用，以改进代码结构。

- [#11158](https://github.com/emqx/emqx/pull/11158) 在 Mnesia 后端的 retainer 启动时等待 Mria  表，避免加入集群时出现错误。

- [#11162](https://github.com/emqx/emqx/pull/11162) 修复 Webhook 桥接异步模式下，4XX 和 5XX HTTP 状态码被统计为成功指标的问题。

- [#11164](https://github.com/emqx/emqx/pull/11164) 重新引入对嵌套占位符（例如：`${payload.a.b.c}`）的支持，对于 JSON 格式的 Payload，从规则动作与数据桥接中提取数据时无需先调用 `json_decode(payload)`。

- [#11172](https://github.com/emqx/emqx/pull/11172) 修复了特定情况下 `payload` 重复的问题：

  - 使用不带 `as` 子表达式的 `foreach` 语句，并选择所有字段，例如：

    ```
    FOREACH payload.sensors FROM "t/#"
    ```
  
  - 在选择 `payload` 字段和所有字段的情况下，例如：

    ```
    SELECT payload.sensors, * FROM "t/#"
    ```

- [#11174](https://github.com/emqx/emqx/pull/11174) 修复了 MQTT 桥接 Ingress 的 `server` 字段的编码问题。在修复之前，它被编码为 ASCII 字符对应的整数列表。

- [#11184](https://github.com/emqx/emqx/pull/11184) 配置项 `mqtt.max_packet_size` 最大值设置为协议定义 256MB。

- [#11192](https://github.com/emqx/emqx/pull/11192) 修复了在使用 atom 类型生成有效 HOCON 文件时，从 HOCON 文件中删除不必要的 `"`。

- [#11195](https://github.com/emqx/emqx/pull/11195) 修复了 REST API 可以为 Stomp 网关指定客户端创建重复订阅的问题。

- [#11206](https://github.com/emqx/emqx/pull/11206) 连接到 CoAP 网关时，不再要求客户端的 `username` 和 `password` 参数必填。

- [#11208](https://github.com/emqx/emqx/pull/11208) 修复了 LwM2M 客户端异常数据统计的问题。

- [#11211](https://github.com/emqx/emqx/pull/11211) `DELETE` 操作操作不存在的资源时，一致性地返回 `404` 状态码。

- [#11214](https://github.com/emqx/emqx/pull/11214) 修复了节点配置可能在加入集群时无法正确同步的问题。

- [#11229](https://github.com/emqx/emqx/pull/11229) 修复了在通过 `emqx ctl conf load` 更改配置后，插件无法启动/停止的问题。

- [#11237](https://github.com/emqx/emqx/pull/11237)  `/prometheus` API中 `headers` 的默认值应为对象而不是列表。

- [#11250](https://github.com/emqx/emqx/pull/11250) 修复了 WebSocket 数据包中包含多个 MQTT 数据包时其顺序被颠倒的问题。


- [#11271](https://github.com/emqx/emqx/pull/11271) 确保 REST API 与配置中百分比类型的范围为 0% 到 100%。比如 `sysom.os.sysmem_high_watermark=101%` 现在是无效的设置。

- [#11272](https://github.com/emqx/emqx/pull/11272) 修复了日志中的拼写错误，错误地将异常 `PUBREL` 数据包称为 `pubrec`。

- [#11281](https://github.com/emqx/emqx/pull/11281) 恢复对特殊的共享订阅主题前缀 `$queue/` 的支持。

- [#11294](https://github.com/emqx/emqx/pull/11294) 修复了`emqx_ctl cluster join`，`leave `和 `status `命令。

- [#11306](https://github.com/emqx/emqx/pull/11306) 修复了规则动作指标不一致的问题，未计算丢弃的请求。

- [#11309](https://github.com/emqx/emqx/pull/11309) 改进了 EMQX 应用程序的启动顺序。简化了构建脚本，并改进了代码重用。

- [#11322](https://github.com/emqx/emqx/pull/11322) 支持从 EMQX 备份文件（`emqx ctl import `命令）中导入了其他配置：

  - rule_engine（以前由于错误未导入）
  - topic_metrics（以前未实现）
  - slow_subs（以前未实现）。

- [#10645](https://github.com/emqx/emqx/pull/10645) 更改了对 Oracle、PostgreSQL、MySQL 和 Kafka 生产者数据桥接的健康检查，以确保目标表/主题存在。

- [#11107](https://github.com/emqx/emqx/pull/11107) 现在在测试 MongoDB 桥接时返回健康检查失败原因。

- [#11139](https://github.com/emqx/emqx/pull/11139) 将 Redis 桥接重构为单独的 Erlang 应用，以改进代码结构和易维护性。

- [#11151](https://github.com/emqx/emqx/pull/11151) 将 MySQL 桥接重构为单独的 Erlang 应用，以改进代码结构和易维护性。

- [#11163](https://github.com/emqx/emqx/pull/11163) 隐藏 MondoDB 桥接中的 `topology.pool_size`，并将其固定为 1 以避免混淆。

- [#11175](https://github.com/emqx/emqx/pull/11175) 现在，REST API 创建 MySQL 连接时如果使用不存在的主机名，将返回 400 错误而不是 503 错误。

- [#11198](https://github.com/emqx/emqx/pull/11198) 修复了复制节点上全局再平衡状态评估策略。之前，`/api/v5/load_rebalance/global_status` API method 可能在由复制节点处理时返回不完整的结果。

- [#11223](https://github.com/emqx/emqx/pull/11223) InfluxDB 桥接写入配置中，某个字段混用小数和整数时可能会导致 Influx Line Protocol 序列化失败，并且无法写入 InfluxDB 桥接（当小数位为 0 时小数点会被忽略，InfluxDB 误以为其是整数）。

  另请参阅：[InfluxDB v2.7 Line-Protocol](https://docs.influxdata.com/influxdb/v2.7/reference/syntax/line-protocol/#float)。

- [#11225](https://github.com/emqx/emqx/pull/11225) 修复了PostgreSQL/Timescale/MatrixDB 桥接没有验证 `username` 可能为空的问题。

- [#11242](https://github.com/emqx/emqx/pull/11242) 当节点加入集群时重新启动 emqx_ee_schema_registry。因为 emqx_ee_schema_registry 使用 Mria 表，所以节点加入集群需要重新启动此应用程序，以启动相关的 Mria 分片进程，确保在核心/复制节点模式下正确工作。

- [#11266](https://github.com/emqx/emqx/pull/11266) 修复和改进对 TDengine `insert `语法的支持：

  1. 支持在模板中插入多表。

     例如：

     `insert into table_1 values (${ts}, ${val}) into table_2 values (${ts}, ${val})`

  2. 支持在模版中混合前缀/后缀和占位符。

     例如：

     `insert into table_${topic} values (${ts}, '${id}', '${topic}')`

     注意：这是一个破坏性变更。此前，字符类型的占位符会被自动转义加上单引号，而现在需要手动加上单引号。

     例如：

     `insert into table values (${ts}, '${a_string}')`

- [#11307](https://github.com/emqx/emqx/pull/11307) 在 Oracle 桥接中检查表是否存在时返回更友好的错误信息。

- [#11316](https://github.com/emqx/emqx/pull/11316) 修复了在 Oracle 桥接中未考虑 Pool Size 值的问题。

- [#11326](https://github.com/emqx/emqx/pull/11326) 修复了在 Oracle 桥接中返回错误检查的问题。

### [已知问题](https://github.com/emqx/emqx-docs/blob/release-5.1/en_US/changes/known-issues-5.1.1.md)

## 5.1.0

*发布日期: 2023-06-21*

### 增强

-   [#11035](https://github.com/emqx/emqx/pull/11035) 升级 Cassandra 驱动以避免用户名密码在数据桥接日志中泄漏。
-   [#10584](https://github.com/emqx/emqx/pull/10584) 为 SSL 通信增加日志等级配置。
-   [#10678](https://github.com/emqx/emqx/pull/10678) 优化计数器递增调用以避免在递增为0的情况下计数。
-   [#10690](https://github.com/emqx/emqx/pull/10690) 为 Webhook 桥接添加了重试机制，旨在尝试提高吞吐量。
    这个优化让客户端可以在请求失败时进行重试，而不会阻塞缓冲层，从而在高消息传输率的情况下提高吞吐量。
-   [#10702](https://github.com/emqx/emqx/pull/10702) 引入了一个更直观的配置选项 `keepalive_multiplier`，并废弃了旧的 `keepalive_backoff` 配置。在改进之后，EMQX 通过将"客户端请求的 Keepalive 间隔"与 `keepalive_multiplier` 相乘来周期性地检查客户端的 Keepalive 超时状态。
-   [#10698](https://github.com/emqx/emqx/pull/10698) 优化了在运行时访问配置的内存使用。
-   [#10778](https://github.com/emqx/emqx/pull/10778) 重构 Pulsar 生产者桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10813](https://github.com/emqx/emqx/pull/10813) 重构了Kafka 生产者和消费者桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10858](https://github.com/emqx/emqx/pull/10858) 规则引擎 SQL 语言新增了一个实用函数 timezone_to_offset_seconds/1。该函数将时区字符串（例如"+02:00"、"Z"和"local"）转换为相应的偏移秒数。
-   [#10841](https://github.com/emqx/emqx/pull/10841) 为 Kafka 和 Pulsar 生产者桥接添加了参数校验，以确保在选择了 `key_dispatch` 策略时消息键参数不为空。
-   [#10754](https://github.com/emqx/emqx/pull/10754) 对 MQTT 桥接进行了增强，利用连接池和可用的并行性，大大提高了吞吐量。因此，单个 MQTT 桥接现在使用一组 `clientid` 连接到远程代理。
-   [#10782](https://github.com/emqx/emqx/pull/10782) 在保留器（retainer）配置中添加了一个新的 `deliver_rate` 选项，它可以限制保留器中每个会话的最大传递速率。
-   [#10877](https://github.com/emqx/emqx/pull/10877) 升级 RocketMQ 驱动程序以增强处理敏感数据的安全性。
-   [#10598](https://github.com/emqx/emqx/pull/10598) 在 ExProto 中提供了一种 Unary 类型的回调方法，以避免可能的消息乱序问题。
-   [#10895](https://github.com/emqx/emqx/pull/10895) 重构了大部分桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10790](https://github.com/emqx/emqx/pull/10790) 通过优化配置读取机制减少读取配置的开销。
-   [#10892](https://github.com/emqx/emqx/pull/10892) 在创建 Oracle 数据库桥接时要求设置 SID 或服务名称。
-   [#10910](https://github.com/emqx/emqx/pull/10910) 数据桥接资源选项 `auto_restart_interval` 已被弃用，改为使用 `health_check_interval`，而 `request_timeout` 则被重命名为 `request_ttl`。此外，默认的 `request_ttl` 值从 15 秒增加到了 45 秒。
    之前同时存在 `auto_restart_interval` 和 `health_check_interval` 会导致混淆，因为这两个参数都会影响数据桥接在故障下的恢复。这两个参数的配置不一致可能导致消息过期而无法重试。现在，`health_check_interval` 用于控制进行健康检查的间隔，健康检查会将数据桥接转换为 `disconnected` 或 `connecting` 状态，也可以让数据桥接从 `disconnected` 状态中进行恢复。
-   [#10929](https://github.com/emqx/emqx/pull/10929) 升级 Erland/OTP 到 25.3.2-1。
-   [#10909](https://github.com/emqx/emqx/pull/10909) 移除了网关已弃用的 HTTP API。
-   [#10908](https://github.com/emqx/emqx/pull/10908) 重构了 RocketMQ 桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10924](https://github.com/emqx/emqx/pull/10924) 重构了 Influxdb 桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10944](https://github.com/emqx/emqx/pull/10944) 改进了 GCP PubSub 桥接，以避免在重启节点时可能出现消息发送失败的潜在问题。
-   [#10933](https://github.com/emqx/emqx/pull/10933) 支持在 MQTT/TCP 和 MQTT/SSL 监听器中配置 TCP keep-alive。
-   [#10948](https://github.com/emqx/emqx/pull/10948) 在一些 HTTP API 中添加了 live_connections 字段，例如：
    -   `/monitor_current，/monitor_current/nodes/{node}`
    -   `/monitor/nodes/{node}，/monitor`
    -   `/node/{node}，/nodes`
-   [#10941](https://github.com/emqx/emqx/pull/10941) 设置 `prometheus.vm_dist_collector=disabled` 且度量指标 `erlang_vm_statistics_run_queues_length_total` 被重命名为`erlang_vm_statistics_run_queues_length`，提高 Prometheus 指标的收集速度。
-   [#10985](https://github.com/emqx/emqx/pull/10985) 将 `emqx ctl` 命令的名称从 `cluster_call` 更名为 `conf cluster_sync`。旧命令 `cluster_call` 仍然是一个有效的命令，但不包含在帮助信息中。
-   [#10988](https://github.com/emqx/emqx/pull/10988) 改进日志安全性，确保在数据桥接创建失败时敏感数据始终被模糊化。
-   [#10926](https://github.com/emqx/emqx/pull/10926) 允许在监听器的状态标志中使用 `enable` 和 "`enabled`。
    在此更改之前，可以通过在 `enabled` 配置上设置 `true` 或 `false` 来启用/禁用监听器。与系统中其他状态标志的命名略有不同。现在，添加了 `enable` 标志作为监听器的别名。
-   [#10970](https://github.com/emqx/emqx/pull/10970) 已向 Kafka 生产者桥接添加了一个 query_mode 参数。该参数允许您指定在向 Kafka 发送数据时桥接应该使用异步模式还是同步模式。默认为异步模式。
-   [#10676](https://github.com/emqx/emqx/pull/10676) 新增了用于导入/导出配置和用户数据的命令 `emqx ctl export ` 和 `emqx ctl import`， 允许从正在运行的 EMQX 集群中导出配置和内置数据库数据，然后将其导入到相同或另一个正在运行的 EMQX 集群中。
-   [#11003](https://github.com/emqx/emqx/pull/11003) 在 Kafka 数据桥接中添加一个配置 TCP keepalive 的选项。
-   [#10961](https://github.com/emqx/emqx/pull/10961) 通过允许在配置和 HTTP API 中的 `max_connections` 字段中使用无限大（infinity）作为有效值，为网关监听器添加了对无限制最大连接数的支持。
-   [#11019](https://github.com/emqx/emqx/pull/11019) 改进了 JWT 的日志安全性，现在在打印之前将进行模糊化处理。
-   [#11024](https://github.com/emqx/emqx/pull/11024) 添加了一个小的改进，以减少在创建/更新 Pulsar 生产者桥接时看到`connecting` 状态的机会。
-   [#11034](https://github.com/emqx/emqx/pull/11034) 隐藏 "broker" 配置， 并将 `broker.shared_subscription_strategy` 改为 `mqtt.shared_subscription_strategy` 因为它属于 mqtt 的特性。
-   [#11045](https://github.com/emqx/emqx/pull/11045) 监听器认证和分区相关 api 在 `5.1.0`版本中被正式移除。
-   [#11062](https://github.com/emqx/emqx/pull/11062) 将 `log.file.to` 更名为 `log.file.path`。

### 修复

-   [#11018](https://github.com/emqx/emqx/pull/11018) 修复了 Stomp 网关的多个问题，包括：
    -   修复了关于 is_superuser 无法正常工作的问题。
    -   修复了 mountpoint 在消息发送中没有被移除的问题。
    -   消息或订阅请求失败后，Stomp 客户端应该在回复错误信息后立即断开。
-   [#11051](https://github.com/emqx/emqx/pull/11051) 增加了对证书`层级`（监听器 SSL 选项）须为非负整数的验证。
-   [#10563](https://github.com/emqx/emqx/pull/10563) 修复了订阅时 no_local flag 无法正常工作的问题。
-   [#10653](https://github.com/emqx/emqx/pull/10653) 将网关认证的 TLS 证书和密钥保存到数据目录以修复内存泄漏问题。
-   [#10682](https://github.com/emqx/emqx/pull/10682) 修正了在会话创建时为遗嘱消息赋予时间戳这个错误，现在该时间戳为会话断开时间。
-   [#10701](https://github.com/emqx/emqx/pull/10701) Amazon Linux 2 的 EMQX RPM 软件包不支持 TLS v1.3，因为它是使用内置 openssl 1.0 的 Erlang/OTP 构建的。
-   [#10677](https://github.com/emqx/emqx/pull/10677) 修复了规则 API 中的问题：当尝试删除不存在的规则时，会响应 404 HTTP 错误代码。
-   [#10715](https://github.com/emqx/emqx/pull/10715) 支持在客户端连接钩子函数(client.connected hooks)中获取客户端证书。之前，为了减少内存消耗在建立连接后移除了该数据。
-   [#10737](https://github.com/emqx/emqx/pull/10737) 修复了网关的 HTTP API 接口无法处理包含特殊字符的 ClientID 的问题，例如：`!@#$%^&*()_+{}:"<>?/`。
-   [#10809](https://github.com/emqx/emqx/pull/10809) 解决节点关闭或重启时出现的 `** ERROR ** Mnesia post_commit hook failed: error:badarg` 错误信息。Mria pull request: [https://github.com/emqx/mria/pull/142](https://github.com/emqx/mria/pull/142)
-   [#10807](https://github.com/emqx/emqx/pull/10807) 在 debug 级别下，不再输出 license 检查相关的日志，该日志产生过于频繁，可能会干扰日志记录。
-   [#10818](https://github.com/emqx/emqx/pull/10818) 修复了 `emqx_ctl traces` 命令错误，其中 `emqx_mgmt_cli` 模块中的 `traces start `命令在某些过滤器下无法正常工作。
-   [#10600](https://github.com/emqx/emqx/pull/10600) 删除了 emqx_statsd 应用。
-   [#10820](https://github.com/emqx/emqx/pull/10820) 修复了集群更新 license 后，新加入的节点不会应用新 license 而是继续使用旧 license 的问题。
    有时新节点必须使用过时的 license。例如，在 license 过期后使用 emqx-operator 进行部署，并且需要扩展规模。此时，集群的 license 已经通过 API/CLI 更新，但新节点不会使用它。
-   [#10851](https://github.com/emqx/emqx/pull/10851) 在错误的 API 日志中对敏感数据进行了混淆处理。
-   [#10884](https://github.com/emqx/emqx/pull/10884) 修复了在节点加入集群时尝试获取规则信息或指标可能导致崩溃的问题。
-   [#10887](https://github.com/emqx/emqx/pull/10887) 修复了一个潜在问题，即对桥接器的请求可能需要很长时间才能进行重试。
    这只影响低吞吐量的情况，其中缓冲层可能需要很长时间才能检测到连接和驱动程序问题。
-   [#10878](https://github.com/emqx/emqx/pull/10878) 已修复了 RabbitMQ 桥接程序中的一个漏洞，该漏洞可能会将密码暴露到日志文件中。
-   [#10871](https://github.com/emqx/emqx/pull/10871) 修复了一个问题，即在 CoAP 连接断开后，Dashboard 仍显示连接存在，但删除和消息发布请求不生效。
-   [#10880](https://github.com/emqx/emqx/pull/10880) 新增了一个 REST API `POST /clients/kickout/bulk`，用于批量踢出多个客户端。
-   [#10913](https://github.com/emqx/emqx/pull/10913) 修复了某个节点离开集群后，其插件状态 REST API 仍会包含集群节点状态问题。
-   [#10923](https://github.com/emqx/emqx/pull/10923) 修复了通道信息注册中的竞态条件。
    在此修复之前，当系统负载较重时，可能出现客户端已断开连接（或会话已过期），但仍可在 Dashboard 的客户端页面中找到的情况。其中一个可能的原因是期间发生的竞态条件：连接在通道数据注册过程中被中断。
-   [#10930](https://github.com/emqx/emqx/pull/10930) 增加了对持续时间数据类型的 schema 验证，以避免使用无效值。
    在此修复之前，可以在 schema 中使用不合理的值，超出系统限制，从而导致崩溃。
-   [#10952](https://github.com/emqx/emqx/pull/10952) 如果设置了`verify = verify_none`，则禁止在监听器 SSL 选项中启用 `fail_if_no_peer_cert`。
    设置 `fail_if_no_peer_cert = true` 和 `verify = verify_none` 会导致连接错误，因为选项不兼容。此修复在创建或更新监听器时验证选项，以避免这些错误。

    注意：应用此修复后，任何具有 `fail_if_no_peer_cert = true` 和 `verify = verify_none` 的旧监听器配置将无法加载，并且必须手动修复。
-   [#10951](https://github.com/emqx/emqx/pull/10951) 修复了 MQTT-SN 网关中发布消息时挂载点未生效的问题。
-   [#10943](https://github.com/emqx/emqx/pull/10943) 弃用了集群发现的 UDP 组播机制。
    该功能自5.0版本以来一直计划弃用，主要是因为实际生产中缺乏使用。尽管该功能的代码在5.1版本中尚未移除，但文档接口已经被降级处理。
-   [#10902](https://github.com/emqx/emqx/pull/10902) 避免从运行较新版本的节点同步 `cluster.hocon` 文件。
    在集群滚动升级期间，如果旧版本节点由于任何原因需要重新启动，如果它从较新版本的节点复制 `cluster.hocon` 文件，可能会导致启动失败。在此修复后，旧版本节点将不会从较新版本的节点复制 `cluster.hocon` 文件，而是使用自己的 `cluster.hocon` 文件进行启动。
-   [#10967](https://github.com/emqx/emqx/pull/10967) 修复了重平衡 API 中错误消息的格式问题：之前它们可能以不清晰的 Erlang 内部结构转储的形式显示。
    在节点疏散的 CLI 和 API 中添加了 `wait_health_check` 选项。这是一个时间间隔，节点在此期间报告为"不健康状态"，但不会开始实际的疏散操作。我们需要这个选项来允许负载均衡器（如果有）将已疏散的节点从负载均衡中移除，并且不将（重新）连接的客户端转发到已疏散的节点。
-   [#10911](https://github.com/emqx/emqx/pull/10911) 修改了当尝试创建一个名称超过255个字节的桥接时出现的错误消息和日志条目的内容，使其更易于理解。
-   [#10983](https://github.com/emqx/emqx/pull/10983) 修复了一个问题，即当 MQTT 客户端尝试通过配置为仅使用 TLS v1.3 的监听器进行连接时，无法建立TLS连接。
    问题在于 TLS 连接尝试使用与 TLS v1.3 不兼容的选项。
-   [#10977](https://github.com/emqx/emqx/pull/10977) 修复了订阅计数指标更新延迟以及 Stomp 网关中的配置问题。
-   [#10950](https://github.com/emqx/emqx/pull/10950) 修复了在 MQTT-SN 网关中使 `enable_qos` 选项无效的问题。
-   [#10999](https://github.com/emqx/emqx/pull/10999) 更改了 Kafka 字段 "Partition Count Refresh Interval" 和 "Offset Commit Interval" 的 schema 验证，以避免接受超过最大允许值的值。
-   [#10997](https://github.com/emqx/emqx/pull/10997) ClickHouse 桥接存在一个问题，即当 ClickHouse 服务器在发送消息时关闭时，即使请求的 ttl（time to live）设置为无限大，也可能导致消息丢失。通过将由于连接关闭引起的错误视为可恢复错误修复了该问题。
-   [#10994](https://github.com/emqx/emqx/pull/10994) 在 HTTP 连接器中，对 `proxy-authorization headers` 进行了屏蔽处理，以防止将机密信息泄露到日志文件中。
-   [#10996](https://github.com/emqx/emqx/pull/10996) 对于任何未知的 HTTP/API 请求，默认返回404错误，而不是返回 Dashboard 的 index.html 页面。
-   [#11005](https://github.com/emqx/emqx/pull/11005) 修复了在 AuthN HTTP 的跟踪日志中无法正确打印方法字段的问题。
-   [#11006](https://github.com/emqx/emqx/pull/11006) 修复了 QUIC 监听器的默认证书文件路径。
    在此更改之前，默认的证书文件路径以环境变量 `${EMQX_ETC_DIR}` 为前缀，但在 QUIC 监听器中使用之前未进行插值处理。
-   [#10998](https://github.com/emqx/emqx/pull/10998) 不允许为 MongoDB 桥接资源设置 `batch_size` 选项。当前的 MongoDB 连接器不支持批量处理，如果提供了 `batch_size` 配置值，它将被强制设置为1。
-   [#10955](https://github.com/emqx/emqx/pull/10955) 修复了 MQTT-SN 网关中对预定义主题的配置删除无效的问题。
-   [#11025](https://github.com/emqx/emqx/pull/11025) 修复了Pulsar Producer 桥接中可能在竞态条件下引发的 `case_clause` 错误。
-   [#11030](https://github.com/emqx/emqx/pull/11030) 改进了在使用 Listeners HTTP API 时发生验证错误所产生的错误信息。
-   [#11033](https://github.com/emqx/emqx/pull/11033) 在 ExProto 网关中，弃用了 `AuthenticateRequest` 中的 `mountpoint` 字段。
    该字段在 e4.x 版本中引入，但实际上，在 e5.0 版本中，我们已经提供了 `gateway.exproto.mountpoint` 进行配置，因此无需通过 Authenticate 请求来覆盖它。

    此外，将 `subscriptions_max`、`inflight_max` 和 `mqueue_max` 的默认值更新为无限大。
-   [#11040](https://github.com/emqx/emqx/pull/11040) 修复了 Kafka 生产者桥接的健康检查问题，当与 Kafka 代理的连接断开时该问题可能会导致消息丢失。
-   [#11038](https://github.com/emqx/emqx/pull/11038) 修复了 Pulsar 生产者的健康检查问题，当与 Pulsar 代理的连接断开时该问题可能会导致消息丢失。
-   [#11042](https://github.com/emqx/emqx/pull/11042) 修复了当监听器的 max_connections 配置设为字符串时 REST API `GET /listeners` 崩溃的问题。
-   [#11028](https://github.com/emqx/emqx/pull/11028) 禁止在监听器配置中同时使用包括 tlsv1.3 但排除 tlsv1.2 的多个 TLS 版本。
    使用具有这种版本差异的 TLS 配置会导致连接错误。此外，删除和记录与所选 TLS 版本不兼容的 TLS 选项。

    注意：应用此修复后，任何包含上述版本差异的旧监听器配置将无法加载，必须手动修复。
-   [#11031](https://github.com/emqx/emqx/pull/11031) 修复了创建桥接和检查 InfluxDB 桥接时的认证信息验证问题。
-   [#11056](https://github.com/emqx/emqx/pull/11056) 修复了新创建的监听器有时无法正确启动的问题。当您删除一个名为 "default" 的系统默认监听器并添加一个新的同名监听器时，它将无法正确启动。
    -   修复了某些节点上的配置失败可能导致 Dashboard 无法使用的错误。
-   [#11070](https://github.com/emqx/emqx/pull/11070) 修复了 `cluster.autoclean` 配置项无法生效的问题。
-   [#11100](https://github.com/emqx/emqx/pull/11100) 修复了复制节点由于 `mria_lb:core_nodes()` 调用超时而无法连接到核心节点的问题。
    相关的 mria pull request: [https://github.com/emqx/mria/pull/143](https://github.com/emqx/mria/pull/143)
-   [#11092](https://github.com/emqx/emqx/pull/11092) 修复复制节点因超时无法连接到核心节点的问题。

### [已知问题](https://github.com/emqx/emqx-docs/blob/release-5.1/en_US/changes/known-issues-5.1.0.md)

## 5.0.4

*发布日期: 2023-05-26*

### 增强功能

- [#10389](https://github.com/emqx/emqx/pull/10389) 统一 `cluster.core_nodes` 和 `cluster.statics.seeds` 配置格式，同时支持数组 `["emqx1@127.0.0.1", "emqx2@127.0.0.1"]` 或逗号分隔的字符串 `"emqx1@127.0.0.1,emqx2@127.0.0.1"` 两种格式。

- [#10392](https://github.com/emqx/emqx/pull/10392) 新增函数 date_to_unix_ts/3，用于将格式化的日期转换成整数类型的时间戳：

  `date_to_unix_ts(TimeUnit, FormatString, InputDateTimeString)`

- [#10426](https://github.com/emqx/emqx/pull/10426) 优化配置优先级机制，修复重启 EMQX 后 `etc/emqx.conf` 中的配置不生效的问题。有关配置优先级请参考[配置重写规则](https://docs.emqx.com/zh/enterprise/v5.0/configuration/configuration.html#%E9%85%8D%E7%BD%AE%E8%A6%86%E7%9B%96%E8%A7%84%E5%88%99)

- [#10457](https://github.com/emqx/emqx/pull/10457) 移除 StatsD 指标监控功能。

- [#10458](https://github.com/emqx/emqx/pull/10458) 调整插件配置在示例配置文件中的位置，大多数情况下用户都在 Dashboard 上管理插件，不需要手动修改配置，因此我们将插件配置调整到更靠后的位置。

- [#10491](https://github.com/emqx/emqx/pull/10491) 将 `etcd.ssl` 重命名为 `etcd.ssl_options` ，使其与配置文件中其他 SSL 相关配置项保持一致的命名。

- [#10512](https://github.com/emqx/emqx/pull/10512) 改进了数据文件存储格式，现已支持正常存储 Unicode 字符，例如包含 Unicode 字符的规则 SQL `SELECT * FROM "t/1" WHERE clientid = "-测试专用-"`。

- [#10568](https://github.com/emqx/emqx/pull/10568) 为 `emqx ctl listeners` 命令返回结果添加连接关闭计数指标 `shutdown_count`。

- [#10588](https://github.com/emqx/emqx/pull/10588) 将日志跟踪记录的时间精度从秒提高到微秒。例如，从 `2023-05-02T08:43:50+00:00` 到 `2023-05-02T08:43:50.237945+00:00` 。

- [#10623](https://github.com/emqx/emqx/pull/10623) 在 `force_shutdown` 配置中，将 `max_message_queue_len` 重命名为 `max_mailbox_size` ，旧名称作为别名被保留，保证向后兼容性。

- [#10713](https://github.com/emqx/emqx/pull/10713) 为减少歧义与配置复杂度，隐藏 WebHook `resource_option.request_timeout` 配置项，并使用 `http` `request_timeout` 来设置该值。

- [#10075](https://github.com/emqx/emqx/pull/10075) 新增节点再平衡/节点疏散功能，功能设计与使用请参考 [EIP 文档](https://github.com/emqx/eip/blob/main/active/0020-node-rebalance.md)。

- [#10378](https://github.com/emqx/emqx/pull/10378) 新增 Pulsar 数据桥接，目前仅支持单向数据集成即 Pulsar 生产者角色。

- [#10408](https://github.com/emqx/emqx/pull/10408) 规则 SQL 新增三个内置函数，用于写入日期类型的值到 MongoDB 中。

- [#10409](https://github.com/emqx/emqx/pull/10409) [#10337](https://github.com/emqx/emqx/pull/10337) 规则引擎新增 [Protocol Buffer](https://protobuf.dev/)  和 [Apache Avro](https://avro.apache.org/) 格式消息编解码功能。

- [#10425](https://github.com/emqx/emqx/pull/10425) 新增 OpenTSDB 数据桥接。

- [#10498](https://github.com/emqx/emqx/pull/10498) 新增 Oracle 数据桥接。

- [#10560](https://github.com/emqx/emqx/pull/10560) 新增 Apache IoTDB 数据桥接。

- [#10417](https://github.com/emqx/emqx/pull/10417) 通过解除临时引用来提高配置项读取性能。

- [#10430](https://github.com/emqx/emqx/pull/10430) 简化 `retainer` 的配置，将 `flow_control` 标记为不重要的字段。

- [#10511](https://github.com/emqx/emqx/pull/10511) 对资源相关的日志进行脱敏以提升隐私与安全性。

- [#10525](https://github.com/emqx/emqx/pull/10525) 提高 MQTT 数据包处理的性能。

- [#10528](https://github.com/emqx/emqx/pull/10528) 在代码的热路径（hot code path）中减少内存占用。热路径包括在消息处理、连接管理、认证授权等核心功能中被频繁执行的代码。

- [#10591](https://github.com/emqx/emqx/pull/10591) [#10625](https://github.com/emqx/emqx/pull/10625) 改进并简化速率限制器的配置：

  - 降低速率限制器配置的复杂度。

  - 更新 `configs/limiter` API。

  - 减少速率限制器配置内存占用。

- [#10487](https://github.com/emqx/emqx/pull/10487) 优化不设置速率限制(`infinity`)时速率限制器的性能表现，减少了内存和 CPU 的使用量。

- [#10490](https://github.com/emqx/emqx/pull/10490) 移除默认 `1000/s` 的连接速率限制。

- [#10077](https://github.com/emqx/emqx/pull/10077) QUIC TLS 现已支持密码保护证书文件。

### 错误修复

- [#10340](https://github.com/emqx/emqx/pull/10340) 修复通过 `systemd` 停止 EMQX 时可能导致的日志打印崩溃问题。

- [#10369](https://github.com/emqx/emqx/pull/10369) 修复 `/api/v5/monitor_current` API，修复前，当某些 EMQX 节点出现故障时，该 API 请求会返回 500 和以下信息：

  `{"code":"INTERNAL_ERROR","message":"error, badarg, [{erlang,'++',[{error,nodedown},[{node,'emqx@10.42.0.150'}]], ...`

- [#10407](https://github.com/emqx/emqx/pull/10407) 修复告警崩溃问题

  - 通过使用 Mnesia dirty 操作和避免不必要的调用。

  - 使用 'emqx_resource_manager' 来重新激活已经激活的告警，提高'emqx_alarm'性能。

  - 使用新的安全的 'emqx_alarm' API 来激活/停用告警，确保 emqx_resource_manager 不会因为警报超时而崩溃。

  - 当以下条件同时出现时告警系统容易崩溃：

    - 失败的资源数量相对较多，例如，桥接试图激活重复出现的错误的警报。

    - 系统经历了一个非常高的负载。

- [#10420](https://github.com/emqx/emqx/pull/10420) 修复认证和授权 HTTP 路径可能被多次编码的问题：

  - 由于无法假定外部服务器对原始和规范化 URL 的处理方式，因此将尽量避免不必要的 URL 编码，以免导致类似 [#10411](https://github.com/emqx/emqx/issues/10411) 的 bug。

- [#10422](https://github.com/emqx/emqx/pull/10422) 修正单节点集群中，外部插件无法通过环境变量进行配置的问题。

- [#10448](https://github.com/emqx/emqx/pull/10448) 修复由 e5.0.3 引入的速率限制器配置（用 `burst` 代替 `capacity` ）的兼容性问题，修复前，如 `capacity` 设为 `infinity` ，将无法从之前的版本进行升级。修复后，`capacity = infinity` 配置将自动转换为等同的 `burst = 0` 。

- [#10462](https://github.com/emqx/emqx/pull/10462) 废弃已经无效的 `broker.shared_dispatch_ack_enabled` 配置项 。该配置项旨在避免向存在连接断开客户端的共享订阅组中派发回消息。但实际从 e5.0.0 版本开始，消息会被重新派发到组内的其他已经连接的会话中，该配置已经失效。请参考: <https://github.com/emqx/emqx/pull/9104> 。

- [#10463](https://github.com/emqx/emqx/pull/10463) 修复数据桥接 API 的错误处理问题，例如 WebHook 桥接 URL 无效时， API 将返回 '400' 而非 '500'。

- [#10484](https://github.com/emqx/emqx/pull/10484) 修复滚动升级时无法设置配置优先级的问题。例如，在 e5.0.2 中修改了授权的优先级，当通过滚动升级升级到 e5.0.3 时，授权的优先级将被恢复为默认。

- [#10495](https://github.com/emqx/emqx/pull/10495) 恢复了被误删的速率限制器 API `/configs/limiter`。

- [#10500](https://github.com/emqx/emqx/pull/10500) 修复并增强了 Mria 中的一些功能：

  - 通过全局锁来保护 `mria:join/1,2` 函数，避免两个同时试图加入对方的节点之间的冲突 [Mria PR](https://github.com/emqx/mria/pull/137)

  - 提供了新函数 `mria:sync_transaction/4,3,2`，该函数会阻止调用者，直到事务被导入到本地节点（当本地节点是复制者时有效，否则它的行为与`mria:transaction/3,2` 函数完全一样）。

  - 优化  `mria:running_nodes/0`  函数 [Mria PR](https://github.com/emqx/mria/pull/135)

  - 优化单个复制节点调用  `mria:ro_transaction/2`  函数时的性能 [Mria PR](https://github.com/emqx/mria/pull/134).

- [#10518](https://github.com/emqx/emqx/pull/10518) 在 Mria 中增加以下修复和功能：

  - 在 mria_membership 中安全地调用 `mria_rlog:role/1`，以确保 mria_membership gen_server 在 RPC 到另一个节点失败时不会崩溃 [Mria PR](https://github.com/emqx/mria/pull/139)。

  - 在 `?rlog_sync` 表中添加额外的字段，以方便在未来的扩展这一功能 [Mria PR](https://github.com/emqx/mria/pull/138)。

- [#10556](https://github.com/emqx/emqx/pull/10556) 加密`emqx_connector_http`  携带的  `Authorization header`。

- [#10571](https://github.com/emqx/emqx/pull/10571) 尝试通过 API 获取主题统计中不存在的主题时，不会产生无意义的崩溃报告。

- [#10659](https://github.com/emqx/emqx/pull/10659) 修复当 `sysmon.os.mem_check_interval` 被禁用时，EMQX 无法启动的问题。

- [#10717](https://github.com/emqx/emqx/pull/10717) 修复当飞行窗口拥塞时，缓冲层进程 CPU 占用过高的问题。

- [#10724](https://github.com/emqx/emqx/pull/10724) 为 HTTP API 文档所有 API 添加摘要字段以便查找 API（访问地址为 "http://<emqx_host_name\>:18083/api-docs")。

- [#10726](https://github.com/emqx/emqx/pull/10726) 对健康检查间隔和自动重启间隔的范围进行验证，支持 1 ms 到 1 小时的时间范围。

- [#10728](https://github.com/emqx/emqx/pull/10728) 修复规则引擎 SQL `FOREACH - DO`语句执行问题，例如在以下 SQL 语句中：

  `FOREACH   payload.date as date, payload.array as elem DO   date, elem FROM "t/#"  -- {"date": "2023-05-06", "array": ["a"]}`

  修复前，由 `FOREACH` 导出的 `date` 变量无法在 `DO` 子句中访问，导致该 SQL 输出如下： `[{"elem": "a","date": "undefined"}]` 。

  修复后，该 SQL 语句将能正确输出：`[{"elem": "a","date": "2023-05-06"}]`

- [#10742](https://github.com/emqx/emqx/pull/10742) 在保存授权文件前对规则进行正确性检查，避免由于错误规则导致 EMQX 无法启动。

- [#10743](https://github.com/emqx/emqx/pull/10743) 修复节点加入集群后，尝试获取桥接信息或指标时可能导致的崩溃问题。

- [#10755](https://github.com/emqx/emqx/pull/10755) 修复了数据桥接资源更新中的竞争条件。

  在 EMQX 的资源更新中，进行了"删除+创建"的操作。如果桥接器的创建时间过长，可能导致 Dashboard 请求超时。如果在桥接创建完成之前进行更新操作，可能会导致桥接被删除。

  此修复解决了桥接器资源更新中的竞争条件，确保准确识别和添加新数据桥接，并保持运行时和配置文件状态的一致性。

- [#10761](https://github.com/emqx/emqx/pull/10761) 修复了 Dashboard Listener 的 SSL 证书的默认值没能被正确设置的问题，当 `verify_peer` 和 `cacertfile` 使用默认配置时，将导致 HTTPS 无法访问。

- [#10672](https://github.com/emqx/emqx/pull/10672) 修复当监听器中缺少 `ssl_options` 的默认值导致启动失败的问题。修复前， `EMQX_LISTENERS__WSS__DEFAULT__BIND='0.0.0.0:8089' ./bin/emqx console` 类似的命令将导致 EMQX 崩溃。

- [#10738](https://github.com/emqx/emqx/pull/10738) 修复 TDengine 数据桥不支持子表以及自动建表问题。在此之前，子表插入 SQL 语句将执行失败：

  - `insert into ${clientid} using msg TAGS (${clientid}) values (${ts},${msg})`

* [#10746](https://github.com/emqx/emqx/pull/10746)  在规则引擎的测试 API `rule_test` 中增加对事件`$events/delivery_dropped`的支持。
* [#10747](https://github.com/emqx/emqx/pull/10747)  移植 4.4 版本中一些规则引擎的时间格式化函数的修复到当前版本。
* [#10760](https://github.com/emqx/emqx/pull/10760)  修复在节点加入集群是访问数据桥接数据统计页面是出现 "internal error 500" 错误的问题。
* [#10801](https://github.com/emqx/emqx/pull/10801)  避免在 API `/topics/{topic}`  和  `/topics` 中对主题名进行双重百分号解码。
* [#10817](https://github.com/emqx/emqx/pull/10817)  修复对桥接配置项`auto_restart_interval`的值的处理，现在支持设置为`infinity`。

## 5.0.3

*发布日期: 2023-05-08*

### 优化

- [#10128](https://github.com/emqx/emqx/pull/10128) SSL MQTT 监听器增加对 OCSP Stapling 的支持。

- [#10156](https://github.com/emqx/emqx/pull/10156) 调整配置文件覆盖顺序机制。

  对于新安装的 EMQX，emqx.conf 和环境变量中的配置会覆盖 API 传入的配置（即 `cluster.hocon` 中的配置）

  对于从旧版本升级的 EMQX（即 `data` 文件夹中包含 `cluster-override.conf` 文件），保留之前的覆盖规则， 即 `cluster-override.conf` 中的配置会覆盖 `emqx.conf` 和环境变量的配置。

  注意：`data/configs/cluster-override.conf` 已弃用。升级后，建议您在 `emqx.conf` 中重新配置之前被 `cluster-override.conf` 覆盖的配置项，并将 cluster-override.conf 中的配置迁移到 `cluster.hocon` 中。

  升级后，EMQX 将像以前一样继续读取 `local-override.conf` (如果存在的话)，但建议您将配置合并到 `emqx.conf` 中。

- [#10164](https://github.com/emqx/emqx/pull/10164) TLS MQTT 监听器增加对 CRL 检查的支持。

- [#10207](https://github.com/emqx/emqx/pull/10207) 提高 OpenAPI (swagger) 文档的可读性。 在此更改之前，文档中有一些 `Summary` 字段冗长且缺乏翻译，现在使用了 i18n 数据库中更简洁的 `label` 字段。

- [#10210](https://github.com/emqx/emqx/pull/10210) 解决停止/重启 Mria 时 Mnesia callback 可能出现的问题。优化后，当 Mria 被停止前会取消 Mnesia callback 的注册。详情见 [Mria PR](https://github.com/emqx/mria/pull/133)。

- [#10224](https://github.com/emqx/emqx/pull/10224) 在 Helm 图表中增加自定义 `clusterIP` 选项，用户可以将其设置为固定 IP。

- [#10263](https://github.com/emqx/emqx/pull/10263) 添加用于评估 Elixir 表达式的命令 `eval-ex`。

- [#10278](https://github.com/emqx/emqx/pull/10278) 重构所有网关的目录结构。

- [#10206](https://github.com/emqx/emqx/pull/10206) 所有数据桥接支持异步查询模式。

  优化前，如将某项资源（如数据桥接）的查询模式设为 sync（同步模式），缓存将以同步的模式调用底层连接器，即使它支持异步调用。

- [#10306](https://github.com/emqx/emqx/pull/10306) 大多数数据桥接支持 async 查询模式。
  这是 [#10206](https://github.com/emqx/emqx/pull/10206) 的后续优化, 优化前，Cassandra、MongoDB、MySQL、Postgres、Redis、RocketMQ、TDengine 等数据桥接只支持同步查询模式。

- [#10318](https://github.com/emqx/emqx/pull/10318) 规则引擎中的 FROM 语句新增支持由单引号（'）包裹的字符串。

- [#10336](https://github.com/emqx/emqx/pull/10336) 添加 API Endpoint `/rule_engine`，用以管理规则引擎的配置。

- [#10354](https://github.com/emqx/emqx/pull/10354) 优化 `max_heap_size` 配置错误时的报错信息。当发生 `message_queue_too_long` 报错时，会在日志文件中记录当前值和最大值。

- [#10358](https://github.com/emqx/emqx/pull/10358) 隐藏 `flapping_detect/conn_congestion/stats` 配置。弃用 `flapping_detect.enable` 配置项。

- [#10359](https://github.com/emqx/emqx/pull/10359) 通过独立的 RPC 收集针对集群级别的指标，不再隐式收集不被 API 调用的指标。

- [#10373](https://github.com/emqx/emqx/pull/10373) 废弃 `trace.payload_encode` 配置项。可以在通过 HTTP API 创建的日志追踪时使用 `trace.payload_encode = [text, hidden, hex]` 字段替代。

- [#10381](https://github.com/emqx/emqx/pull/10381) 隐藏 `auto_subscribe` 配置项，后续将只能通过 HTTP API 来修改自动订阅规则。

- [#10391](https://github.com/emqx/emqx/pull/10391) 简化配置文件并隐藏大量的配置项，包括 `rewrite`、`topic_metric`、`persistent_session_store`、`overload_protection`、`flapping_detect`、`conn_congestion`、`stats`、`auto_subscribe`、`broker_perf`、`shared_subscription_group`、`slow_subs`、`ssl_options.user_lookup_fun`、 `node` 和 `dashboard` 相关的部分高级配置项，[#10358](https://github.com/emqx/emqx/pull/10358), [#10381](https://github.com/emqx/emqx/pull/10381), [#10385](https://github.com/emqx/emqx/pull/10385)。

- [#10404](https://github.com/emqx/emqx/pull/10404) 将缓冲区工作线程的默认队列模式更改为 `memory_only`。在此优化前，默认队列模式为 `volatile_offload`，当消息速率很高，资源无法满足该需求时，缓冲区性能会由于频繁的磁盘操作而受影响。

- [#10140](https://github.com/emqx/emqx/pull/10140) 新增 Cassandra 数据桥接，目前仅支持 Cassandra 3.x 版本，暂不支持 4.x 版本。

- [#10143](https://github.com/emqx/emqx/pull/10143) 新增 RocketMQ 数据桥接。

- [#10165](https://github.com/emqx/emqx/pull/10165) InfluxDB 数据桥接中的 `write_syntax` 中支持转义特殊字符。优化后，用户可根据 InfluxDB Line Protocol 在字符串中使用经转义的特殊字符。

- [#10211](https://github.com/emqx/emqx/pull/10211) 隐藏 `broker.broker_perf` 配置和相关 API 文档。其中的两个配置项 `route_lock_type` 和 `trie_compaction` 很少使用，而且需要重新启动整个集群才能生效，不必要暴露给用户。更多信息可阅读：<https://gist.github.com/zmstone/01ad5754b9beaeaf3f5b86d14d49a0b7/revisions>。

- [#10294](https://github.com/emqx/emqx/pull/10294) 配置 MongoDB 数据桥接时，现支持通过占位符 `${field}` 语法来引用消息中的字段，从而动态地选择要插入的数据集合。

- [#10363](https://github.com/emqx/emqx/pull/10363) 新增 Microsoft SQL Server 数桥接。

- [#10573](https://github.com/emqx/emqx/pull/10573) 提升了 WebHook 在同步请求模式下的性能表现，以及其他数据桥接在未配置批处理时的性能表现。

### 修复

- [#10145](https://github.com/emqx/emqx/pull/10145) 在针对 `GET /bridges/:id` 的 API 调用响应中，如桥接的状态为断开，且内部健康检查返回错误，添加 `status_reason` 字段说明错误原因。在相应的告警信息中，同样增加 `status_reason` 字段说明断开原因。

- [#10172](https://github.com/emqx/emqx/pull/10172) 修复默认 ACL 规则中不正确的正则表达式，从而允许 dashboard 用户名订阅 `$SYS/#` 主题。

- [#10174](https://github.com/emqx/emqx/pull/10174) 将库 `esockd` 从 5.9.4 升级至 5.9.6。如连接在代理发送代理协议头之前关闭，将不再产生的一条错误级别日志。

- [#10195](https://github.com/emqx/emqx/pull/10195) 对包含 HTML 的 API Schema 添加标签，解决之前会破坏生成文档格式的问题。

- [#10196](https://github.com/emqx/emqx/pull/10196) 针对用于生成在线文档菜单中的模式摘要和描述，使用小写字母。

- [#10209](https://github.com/emqx/emqx/pull/10209) 修复在断开禁用客户端时，此类客户端仍可发布遗嘱消息的错误。

- [#10225](https://github.com/emqx/emqx/pull/10225) 对于名称与已安装的插件开头相同的插件，用户仍可继续安装。例如：如果插件 `emqx_plugin_template_a` 已安装，用户仍可安装名为 `emqx_plugin_template` 的插件。

- [#10226](https://github.com/emqx/emqx/pull/10226) 在 `/bridges` API 验证错误时，返回 `400` 而非 `500`。

- [#10242](https://github.com/emqx/emqx/pull/10242) 修复日志中数据字段名称冲突。修复前，一些调试日志可能会报告错误的 Erlang PID，影响解决会话接管类问题。

- [#10257](https://github.com/emqx/emqx/pull/10257) 修复 LwM2M 网关中 `auto_observe` 无法正常工作的问题。

  修复前，在发送 `OBSERVE` 请求时没有发送 token，导致 LwM2M 网关无法处理客户端请求。

  修复后，LwM2M 网关可以正确观察到客户端携带的资源列表，此外，未知资源将被忽并打印以下警告日志：

  ```
  2023-03-28T18:50:27.771123+08:00 [warning] msg: ignore_observer_resource, mfa: emqx_lwm2m_session:observe_object_list/3, line: 522, peername: 127.0.0.1:56830, clientid: testlwm2mclient, object_id: 31024, reason: no_xml_definition
  ```

- [#10286](https://github.com/emqx/emqx/pull/10286) 优化 EMQX 启动失败时的日志记录行为。当 EMQX 由于配置文件破坏无法启动时，不会再产生过多的日志记录，也不会再产生崩溃转储文件。

- [#10297](https://github.com/emqx/emqx/pull/10297) 通过仅评估 Erlang 表达式实现 `eval` 命令与 v4 版本的向后兼容，该更新同样适用于 Elixir 节点。对于 Elixir 表达式，请使用 `eval-ex` 命令。

- [#10300](https://github.com/emqx/emqx/pull/10300) 针对通过 Elixir 构建的项目，修复无法通过环境变量配置插件的问题。

- [#10315](https://github.com/emqx/emqx/pull/10315) 修复在 `/mqtt/delayed/messages` API 调用中，在检查 `limit` 和 `page` 参数时的崩溃问题。

- [#10317](https://github.com/emqx/emqx/pull/10317) 在经过充分验证前，隐藏监听器级别的认证信息。

- [#10323](https://github.com/emqx/emqx/pull/10323) 出于安全原因，API 示例中 `password` 字段的值被替换为 `******`。

- [#10410](https://github.com/emqx/emqx/pull/10410) 针对由`emqx.conf` 配置的网关，修复配置检查失败问题。
  此问题最早在 v5.0.22 版本中由 [#10278](https://github.com/emqx/emqx/pull/10278)引入，启动时缺少配置检查。

- [#10533](https://github.com/emqx/emqx/pull/10533) 修复可能导致日志中的出现无害的噪音的问题。

  在对数据桥接进行同步调用时，一些迟到的回复可能会发送到不再期待回复的连接进程，导致产生错误日志，如：

  ```
  2023-04-19T18:24:35.350233+00:00 [error] msg: unexpected_info, mfa: emqx_channel:handle_info/2, line: 1278, peername: 172.22.0.1:36384, clientid: caribdis_bench_sub_1137967633_4788, info: {#Ref<0.408802983.1941504010.189402>,{ok,200,[{<<"cache-control">>,<<"max-age=0, ...">>}}
  ```

  这些日志是无害的，但它们可能会泛滥成灾，引起用户不必要的担心。

- [#10449](https://github.com/emqx/emqx/pull/10449) 在通过 HTTP 服务（`authn_http`）创建身份验证时，将进行 `ssl_options` 和 `header` 配置验证。在此修复前，用户通过错误的 ssl 配置也可成功创建身份验证，但该验证整体不生效。

- [#10548](https://github.com/emqx/emqx/pull/10548) 修复了 HTTP 驱动程序在竞争条件下会导致错误而不去重试的问题。
  相关的驱动程序修复：[emqx/ehttpc#45](https://github.com/emqx/ehttpc/pull/45)

- [#10201](https://github.com/emqx/emqx/pull/10201) 在 TDengine 数据桥接中，移除 SQL 模板中冗余的数据库名称。

- [#10270](https://github.com/emqx/emqx/pull/10270) 在创建 ClickHouse 数据桥接时，优化用户点击测试按钮时的错误信息。

- [#10324](https://github.com/emqx/emqx/pull/10324) 针对配置有误的 ClickHouse 数据桥接，当用户尝试通过 Dashboard 重连时，将收到报错。修复前，用户不会收到报错。

- [#10438](https://github.com/emqx/emqx/pull/10438) 修复 DynamoDB 数据桥接中的部分术语使用错误：

  - 将 `database` 修改为 `table`
  - 将 `username` 修改为 `aws_access_key_id`
  - 将 `password` 修改为 `aws_secret_access_key`

## 5.0.2

*发布日期: 2023-04-12*

### 优化

- [#10022](https://github.com/emqx/emqx/pull/10022) 发布 Rocky Linux 9 (兼容 Red Hat Enterprise Linux 9) 以及 macOS 12 Intel 平台的安装包。

- [#10139](https://github.com/emqx/emqx/pull/10139) 在 EMQX Helm Chart 中添加 `extraVolumeMounts`，可以挂载用户自己的文件到 EMQX 实例中，例如在 [#9052](https://github.com/emqx/emqx/issues/9052) 中提到的 ACL 规则文件。

- [#9893](https://github.com/emqx/emqx/pull/9893) 当使用 `clean_start=false` 标志连接时，EMQX 将过滤掉会话中被被黑名单功能禁止的客户端发布的消息。以前，在这种情况下，被黑名单功能禁止的客户端发送的消息仍可能被传递给订阅者。

- [#9986](https://github.com/emqx/emqx/pull/9986) 在 helm charts 中增加 MQTT ingress 并删除过时的 `mgmt` 引用。

- [#9564](https://github.com/emqx/emqx/pull/9564) 数据桥接新增 Kafka Consumer，支持从 Kafka 消费消息并将它们发布到 MQTT 主题。

- [#9881](https://github.com/emqx/emqx/pull/9881) 改进了与 InfluxDB 连接的健康检查相关的错误日志。

- [#9985](https://github.com/emqx/emqx/pull/9985) 添加 ClickHouse 数据桥接。

- [#10123](https://github.com/emqx/emqx/pull/10123) 改进了 `/bridges` API 的性能。避免了当集群节点数量较多时可能出现的请求响应超时。

- [#9998](https://github.com/emqx/emqx/pull/9998) 出于安全原因，在使用 HTTP 服务进行客户端认证时，对错误日志中的请求体进行脱敏处理。

- [#10026](https://github.com/emqx/emqx/pull/10026) 仅在 `/bridges/:id/metrics` API 中返回指标数据。

- [#10052](https://github.com/emqx/emqx/pull/10052) 改进守护进程模式下启动失败后的日志。

### 修复

- [#10013](https://github.com/emqx/emqx/pull/10013) 修复 `/gateways/:name/clients` API 在错误情况下的返回类型结构。

- [#10014](https://github.com/emqx/emqx/pull/10014) 当节点不存在时，`/monitor(_current)/nodes/:node` API 返回 `404` 错误代码而不是 `400` 。

- [#10027](https://github.com/emqx/emqx/pull/10027) 允许在 Docker 中通过 `EMQX_NODE__NAME` 设置节点名称。

- [#10050](https://github.com/emqx/emqx/pull/10050) 当调用 Bridge API 时若资源不能存在则返回 `404` 状态码。

- [#10055](https://github.com/emqx/emqx/pull/10055) 修复配置项 `mqtt.max_awaiting_rel` 设置无效的问题。

- [#10056](https://github.com/emqx/emqx/pull/10056) 修复 `/bridges` API 状态码返回错误。当被删除的 Bridge 存在依赖，或 Bridge 未启用时进行启用、停止、重启等操作时返回 `400` 状态码。

- [#10066](https://github.com/emqx/emqx/pull/10066) 优化 `/briges_probe` 和 `[/node/:node]/bridges/:id/:operation` API 调用的错误消息，使其更易理解。并修正错误状态码为`400`。

- [#10074](https://github.com/emqx/emqx/pull/10074) 增加 `PUT /authorization/sources/:type` 请求参数 `type` 的值与请求体中的实际类型的一致性检查。

- [#10079](https://github.com/emqx/emqx/pull/10079) 修复文档中关于 `shared_subscription_strategy` 的描述错误。

- [#10085](https://github.com/emqx/emqx/pull/10085) 对于 `/authorization/sources/:source[/*]` API 中不存在的资源的所有请求，始终返回 `404`。

- [#10098](https://github.com/emqx/emqx/pull/10098) 修复了当配置 MongoDB 授权时 MongoDB 连接器崩溃的问题。

- [#10100](https://github.com/emqx/emqx/pull/10100) 修复了当客户端使用增强认证时，认证消息发送缓慢或者消息丢失时客户端进程崩溃的问题。

- [#10107](https://github.com/emqx/emqx/pull/10107) 当调用 `bridges API` 时如果 `bridge-id` 不存在则返回 `404` 状态码。

- [#10117](https://github.com/emqx/emqx/pull/10117) 修复当加入的节点没有安装在集群其他节点上的插件时发生的错误。修复后，加入的节点将从其他节点复制所有必要的插件。

- [#10118](https://github.com/emqx/emqx/pull/10118) 修复手动添加 EMQX 副本类型节点到集群的相关问题。

- [#10119](https://github.com/emqx/emqx/pull/10119) 修复了当 `statsd.server` 设置为空字符串时会崩溃的问题。

- [#10124](https://github.com/emqx/emqx/pull/10124) 调整 MongoDB 的默认心跳周期，以降低日志文件记录过多的风险。

- [#10130](https://github.com/emqx/emqx/pull/10130) 修复了通过环境变量设置的值，在 Dashboard 中显示乱码的问题。

- [#10132](https://github.com/emqx/emqx/pull/10132) 修复了 `systemctl stop emqx` 命令无法正确停止 `jq` 和 `os_mon` 应用的问题。

- [#10144](https://github.com/emqx/emqx/pull/10144) 修复了当 emqx 目录只读时， emqx cli 设置 Erlang cookie 失败的问题。

- [#10154](https://github.com/emqx/emqx/pull/10154) 将数据桥接和连接器的 `resume_interval` 参数值设为 `health_check_interval` 和 `request_timeout / 3` 中的较小值，以解决请求超时的问题。

- [#10157](https://github.com/emqx/emqx/pull/10157) 修复在创建新的监听器时默认速率限制不生效的问题。

- [#10237](https://github.com/emqx/emqx/pull/10237) 当调用 `/nodes/:node[/metrics|/stats]` API，若节点不存在则返回 `404` 状态码。

- [#10251](https://github.com/emqx/emqx/pull/10251) 修复了当删除一个使用中的 ingress 类型的桥接时，未提示存在规则依赖的问题。

- [#10313](https://github.com/emqx/emqx/pull/10313) 确保在核心节点或副本节点启动时，仅从核心节点复制 `cluster-override.conf` 文件。

- [#10327](https://github.com/emqx/emqx/pull/10327) 数据桥接出现不可恢复的错误不再计入到 `actions.failed.unknown` 指标中。

- [#10095](https://github.com/emqx/emqx/pull/10095) 修复当 MySQL 连接器处于批处理模式时，会发生客户端在每个批次上不断使用不必要的 `PREPARE` 语句查询服务器，可能会导致服务器资源耗尽的问题。

## 5.0.1

*发布日期: 2023-03-10*

### 增强

- [#10019](https://github.com/emqx/emqx/pull/10019) 为 QUIC 监听器添加更多底层调优选项。
- [#10059](https://github.com/emqx/emqx/pull/10059) 规则引擎 API 在出错时返回用户可读的错误信息而不是原始的异常堆栈信息。
- [#9213](https://github.com/emqx/emqx/pull/9213) 在 Helm Chart 中支持 PodDisruptionBudget。
- [#9949](https://github.com/emqx/emqx/pull/9949) QUIC 支持多流传输与 TLS。
- [#9932](https://github.com/emqx/emqx/pull/9932) 添加 TDengine 数据桥接。
- [#9967](https://github.com/emqx/emqx/pull/9967) 新增 TLS 配置项 `hibernate_after`，通过在闲置一段时间后休眠 TLS 进程以减少其内存占用，默认： 5s 。

### 修复

- [#10009](https://github.com/emqx/emqx/pull/10009) `GET /trace/:name/log` API 添加 `bytes` 参数校验，长度不超过 32 位有符号整数。
- [#10015](https://github.com/emqx/emqx/pull/10015) 执行 CLI 时，如果节点 cookie 配置错误则快速抛出错误。
  在此修复前，即使 cookie 配置错误，EMQX 命令仍然会尝试去 ping EMQX 节点，
  并得到一个 'Node xxx not responding to pings' 的错误。
  修复后，如果发现 cookie 不一致，立即打印不一致的错误信息并退出。
- [#10020](https://github.com/emqx/emqx/pull/10020) 修复使用异步和批量配置的桥接计数不准确的问题。
- [#10021](https://github.com/emqx/emqx/pull/10021) 修正执行`emqx_ctl cluster join`命令时，目标节点未运行时的错误信息。
- [#10032](https://github.com/emqx/emqx/pull/10032) 当集群中某些节点上的资源仍处于 **初始化/连接中** 状态时，调用 `/bridges` API 获取 metrics 时将因为没有 metrics 数据而崩溃，此修复将忽略没有 metrics 数据的资源。
- [#10041](https://github.com/emqx/emqx/pull/10041) 为 InfluxDB 桥接配置项 `write_syntax` 描述文档添加了整数占位符注释说明。
  另外在配置中支持 `timestamp` 使用一个常量。
- [#10042](https://github.com/emqx/emqx/pull/10042) 当 core 节点离开集群时，改进 replicant 节点的行为。
  以前，直到所有 core 节点全部起来前， replicant 节点无法重新平衡与 core 节点的连接。
  该情况会打印以下日志：
  `[error] line: 182, mfa: mria_lb:list_core_nodes/1, msg: mria_lb_core_discovery divergent cluster`。
- [#10054](https://github.com/emqx/emqx/pull/10054) 修复了对于已有的 Data Bridge 进行测试连接时需要手工输入一遍密码的问题。
- [#10058](https://github.com/emqx/emqx/pull/10058) 优化未使用的 QUIC TLS 选项。
  QUIC 监听器只保留以下 TLS 选项:
  - cacertfile
  - certfile
  - keyfile
  - verify
- [#10076](https://github.com/emqx/emqx/pull/10076) 修复 Webhook 桥接的一个异常处理：连接超时错误发生后，发生错误的请求可以被重试。
  在此修复前，连接超时后，被当作不可重试类型的错误处理，导致请求被丢弃。
- [#10078](https://github.com/emqx/emqx/pull/10078) 修复了无效的 QUIC 监听器设置可能导致 segfault 的问题。
- [#10084](https://github.com/emqx/emqx/pull/10084) 修正将运行不同 EMQX 版本的 core 节点加入集群的问题。
- [#10086](https://github.com/emqx/emqx/pull/10086) HTTP 客户端库 `ehttpc` 升级到 0.4.7。
  在升级前，如果 HTTP 客户端，例如 认证，授权，webhook 等配置中使用了 content-type HTTP 头，但是没有配置 body，则可能会发生异常。
  详情见 [ehttpc PR#44](https://github.com/emqx/ehttpc/pull/44)。
- [#9939](https://github.com/emqx/emqx/pull/9939) 允许 `emqx ctl cluster join` 命令在 Mnesia 启动前调用。
  在此修复前， EMQX 的 `replicant` 类型节点无法使用 `manual` 集群发现策略。
- [#9958](https://github.com/emqx/emqx/pull/9958) 修复 `clients` API 在 Client ID 不存在时返回的错误码和错误提示。
- [#9961](https://github.com/emqx/emqx/pull/9961) 在 bin/emqx 脚本中，避免在运行非启动命令时解析 emqx.conf 来获取节点名称和 cookie。
- [#9974](https://github.com/emqx/emqx/pull/9974) Statsd 和 prometheus 使用跟 Dashboard 相同的内存用量数据源。
  在此修复前，内存的总量和用量统计使用了过时的（在容器环境中不准确）的数据源。
- [#9997](https://github.com/emqx/emqx/pull/9997) 修复生成 Swagger API 时没有遵循[标准](https://swagger.io/specification/)将元数据字段 `deprecated` 设置为布尔值的问题。
- [#10007](https://github.com/emqx/emqx/pull/10007) Kafka 桥接的配置参数 `memory_overload_protection` 默认值从 `true` 改成了 `false`。
  尽管内存过载后消息被丢弃会产生日志和计数，如果没有基于这些日志或计数的告警，系统管理员可能无法及时发现消息被丢弃。
  当前更好的选择是：让管理员显式的配置该项，迫使他们理解这个配置的好处以及风险。
- [#10087](https://github.com/emqx/emqx/pull/10087) 往 InfluxDB 中插入数据时，如果时间戳为空（未定义），则使用默认的占位符 `${timestamp}`。
  在此修复前，如果时间戳字段没有设置，InfluxDB 桥接使用了一个错误的时间戳。

## 5.0.0

*发布日期: 2023-02-03*

EMQX 企业版 5.0 是一个全新的版本。在这个版本中，我们采用了全新的集群架构，并对主要功能进行了改进，同时还引入了大量新功能。

### 集群水平扩展性与可靠性

基于开创性的 Core + Replica 集群架构，EMQX 企业版 5.0 获得了更好的集群水平扩展性和可靠性：

- 单个集群支持至多 23 个节点并能够承载超过 1 亿 MQTT 连接，相比 4.x 版本实现了 [10 倍的接入能力提升](https://www.emqx.com/zh/blog/how-emqx-5-0-achieves-100-million-mqtt-connections)。
- 使用 Core-Replicant 模式部署时，由于 Replicant 节点是无状态的，动态伸缩增减 Replicant 节点数量不会影响集群稳定性。
- 大规模部署时节点脑裂风险以及脑裂后对业务产生的影响显著降低，为企业用户提供更加稳定可靠的服务。

目前 [EMQX Kubernetes Operator](https://www.emqx.com/zh/emqx-kubernetes-operator) 已经针对新集群架构进行了适配，结合新集群架构能够将 EMQX 部署为一个弹性的、无状态的 MQTT 服务。

### MQTT over QUIC，下一代物联网传输协议

QUIC 是下一代互联网协议 HTTP/3 的底层传输协议，它的出现能够大大扩充现有物联网的应用场景。

EMQX 企业版 5.0 将 QUIC 作为传输层引入到 MQTT 中，以满足多样化的应用场景并统一接入设备进行管理。

MQTT over QUIC 适用于位置不固定、或是需要频繁断连不适合做长连接的设备。

多项与 TCP/TLS 测试的对比表明，在弱网与多变的网络环境下，QUIC 都能够满足其高质量、稳定的消息通信需求，弥补了现有 TCP/TLS 传输层的不足。车联网、移动数据采集等场景的用户将从中受益。

现在，用户可以创建一个 MQTT over QUIC 监听器并使用 EMQ 提供的 SDK 接入物联网设备，EMQ 也正在以 OASIS 成员身份推动 MQTT over QUIC 的标准化落地。

更多信息请参考： [从零开始上手 MQTT over QUIC：快速体验下一代物联网标准协议](https://www.emqx.com/zh/blog/getting-started-with-mqtt-over-quic-from-scratch)

### 可视化双向数据集成

EMQX 企业版 5.0 数据集成包括规则引擎与数据桥接功能，能够以灵活、低代码的配置方式进行物联网数据的实时处理和与第三方数据系统的集成。

**Flow Editor 可视化编排规则处理数据流**

通过可视化查看 Flows 页面，改善规则数量较多情况下的维护和管理难度。现在，用户可以清晰看到数据筛选、处理与桥接的整个步骤，并实时监控这一链路中每个步骤的状态。

后续版本 EMQX 将允许用户在 Dashboard 上以拖拽的方式自由编排规则和数据桥接，通过可视化界面将物联网硬件数据流轻松连接在一起，使开发者专注于自有业务开发。

**更灵活的双向数据集成**

除了将设备数据桥接至外部系统外，还能从外部数据系统如另一个 MQTT 服务、Kafka 中桥接数据至 EMQX，并经过规则处理后发送到指定客户端。

双向数据集成适用于云端下发场景，在支撑持续大规模消息实时处理与下发，为物联网业务开发提供了更多的可能性。

**数据桥接磁盘缓存**

为数据桥接集成增加了磁盘缓存功能，数据桥接连接异常的情况下能够将期间产生的消息缓存起来，待连接恢复后继续发送，这为数据集成提供了极佳的可靠性，大大提升业务可用性。

**数据集成支持情况**

EMQX 企业版 5.0 中首批已获支持的数据集成包括 Webhook、MQTT、Kafka、InfluxDB、MySQL、Redis、GCP PubSub 以及 MongoDB，更多数据集成将在后续的维护版本（5.0.x 版本）中陆续加入。

### 全面的安全保障

**更灵活的访问控制**

EMQX 企业版 5.0 提供了用户名/密码、LDAP、JWT、PSK 和 X.509 证书等多种身份认证，以及消息发布订阅授权检查功能。

访问控制功能可以通过 Dashboard 进行配置，无需重启操作即可为整个集群启用访问控制安全设置，拥有更灵活、易用的操作体验。

认证授权还具有详细的统计指标，可以分别统计集群以及单个节点上认证器和授权检查器的执行情况，包括以下数据：

- 允许数：允许通过认证或授权检查的次数
- 拒绝数：拒绝通过认证或授权检查的次数
- 不匹配数：没有找到用户凭证或权限列表的次数
- 当前速率：当前执行速度

通过认证授权统计指标，用户可以及时发现如大量的失败认证/授权检查，及时感知安全系统的异常情况。

**过载保护与速率限制**

引入了过载保护功能，新的速率限制系统也提供了精度更高的分层速率控制支持，能够从客户端、监听器以及节点多个层面限制客户端行为确保应用按照用户预期的负载运行。

两者结合可以避免客户端过于繁忙以及过多的请求流量导致服务不稳定或故障。

### 全新的 Dashboard

采用全新 UI 设计风格的 Dashboard，优化了关键数据和指标数据的显示方式与内容，在提升视觉体验的同时，也提供了更全面、强大、易用的内置功能，为用户使用 EMQX 进行更多物联网业务开发提供了便利。

主要改进如下：

- 提供了访问控制管理页面
- 改进的数据集成管理 UI，支持可视化数据集成查看
- 更强大的热配置功能
- 更多诊断工具，例如慢订阅和在线追踪
- 更多数据管理：能够在 Dashboard 上管理保留消息、延迟发布数据

### 更好的扩展性

**新的插件机制** 允许通过插件包的的形式编译、分发、安装插件，当用户需要扩展功能时，可以下载独立的插件安装包，在 Web 界面完成上传即可进行安装，整个过程甚至不需要重启 EMQX 集群。

同时，一个规范的插件将会随身附带使用说明、插件主页地址等信息，普通用户可以依照说明快速将插件用起来，也为插件开发者提供了与用户沟通的渠道。

**更易用的 ExHook/gRPC 扩展** 允许创建多个 ExHook 实例，并为每个实例提供了详细的使用情况统计，同时还可以查看每个 Exhook 实例注册的钩子以及钩子参数，能够更好地感知 Exhook 扩展负载情况。

**更"原生"的多协议接入**

网关以统一的设计语言重新实现，针对每种协议特性提供不同的客户端管理页面、安全认证配置，让用户以更原生的方式接入。

由于每个网关都可以配置自己独立的认证，不同网关设备的认证信息现在可以相互隔离，满足更多场景的需求。
