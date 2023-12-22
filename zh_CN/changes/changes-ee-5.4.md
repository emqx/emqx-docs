# 版本发布

## e5.4.0

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

- [#11608](https://github.com/emqx/emqx/pull/11608) 集成了 LDAP 绑定操作作为新的认证器，提供了更多灵活性和安全性的用户认证方式。

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
