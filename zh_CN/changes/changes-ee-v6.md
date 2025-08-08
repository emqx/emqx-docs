# EMQX 企业版 6

## 6.0.0

### 亮点

- **主要版本发布**：EMQX 企业版 6.0.0 是 EMQX 企业版 6 系列的首个版本，带来了重大架构改进和新功能。

- **增强的 AWS 集成**：在使用 S3 或 S3Tables 数据集成时，支持从 EC2 实例访问实例元数据服务 v2 API。这使得无需手动配置 AWS 凭据即可无缝访问 S3 存储桶，通过 IAM 角色实现更好的安全性。

- **Elixir 支持**：所有软件包现在都通过 Mix 构建系统提供 Elixir 支持，为 Elixir 社区开放 EMQX，并通过 IEx 控制台实现更好的工具支持。

- **新的数据集成**：
  - BigQuery 连接器和动作，用于将数据追加到 Google BigQuery
  - Snowflake 动作的 Snowpipe 流式上传模式（预览功能）
  - S3Tables 动作的 Parquet 格式支持

- **持久化存储优化**：通过新的 RocksDB 配置选项和默认的 ASN1 序列化架构，显著改善了 RAM 使用和存储效率。

- **增强的 LDAP 支持**：LDAP 授权现在支持 JSON 格式的扩展 ACL 规则，LDAP 认证可以直接从 LDAP 获取 ACL 规则并支持客户端缓存。

- **改进的链路跟踪**：为最大跟踪数量（`trace.max_traces`）和跟踪文件大小（`trace.max_file_size`）提供可配置限制，并通过优化实现防止原子泄漏。

- **集群管理**：新的 `cluster.description` 配置选项允许用户在 EMQX Dashboard 中设置和显示自定义集群描述。

### 功能增强

#### 规则引擎

- [#15631](https://github.com/emqx/emqx/pull/15631) 添加了新的 API 端点来列出 AI 提供商的所有可用模型。

- [#15467](https://github.com/emqx/emqx/pull/15467) 为 AI 完成提供商公开传输选项。
  这些选项允许配置连接超时和到 AI 完成提供商的最大连接数。

#### 数据集成

- [#15635](https://github.com/emqx/emqx/pull/15635) 为 RocketMQ 动作添加了新的 `key` 和 `tag` 模板字段，分别设置消息的键和标签。同时为 `strategy` 字段添加了新的 `key_dispatch` 值。

- [#15621](https://github.com/emqx/emqx/pull/15621) 现在，S3Tables 连接器的 `access_key_id` 和 `secret_access_key` 是可选字段。如果省略，将从部署 EMQX 的 EC2 实例的实例元数据服务 v2 API 获取。

- [#15542](https://github.com/emqx/emqx/pull/15542) 将我们的 `erlcoud` 库升级到 `3.8.3.0`。这允许在不指定访问密钥 ID 和秘密访问密钥的情况下设置 S3 连接器，只要运行 EMQX 的 EC2 实例具有读取/写入配置存储桶的正确 IAM 权限。

- [#15418](https://github.com/emqx/emqx/pull/15418) 添加了新的连接器和动作，用于将数据追加到 BigQuery。

- [#15401](https://github.com/emqx/emqx/pull/15401) 为 Snowflake 动作添加了 Snowpipe 流式上传模式支持。注意：这目前是 Snowflake 的预览功能，仅在 AWS 上的所有账户都支持。

- [#15387](https://github.com/emqx/emqx/pull/15387) 改进了 Kinesis Producer 连接器和动作健康检查，以减少调用 `ListStreams` 和 `DescribeStream` API 时的限速发生。现在，我们将每个连接器对这些 API 的调用分别限制为 5/秒和 10/秒。如果连接器或动作在超时前无法调用其健康检查 API，它们将简单地保持当前状态。如果它们收到限流响应（例如：`LimitExceededException`），它们也将保持当前状态。

  引入了新的 `resource_opts.health_check_interval_jitter` 配置，为 `resource_opts.health_check_interval` 添加均匀随机延迟，以便同一连接器下的多个动作很少同时运行其健康检查。

- [#15371](https://github.com/emqx/emqx/pull/15371) 在 `GET /actions_summary` 的返回和 `GET /actions/:id` 中返回的回退动作中添加了 `tags` 字段。

- [#15360](https://github.com/emqx/emqx/pull/15360) 为 S3Tables 动作添加了以 Parquet 格式写入数据文件的支持。

- [#15176](https://github.com/emqx/emqx/pull/15176) 升级了 GreptimeDB 连接器客户端，并支持可选的新参数 `ttl` 来设置自动创建表的默认生存时间。

- [#15585](https://github.com/emqx/emqx/pull/15585) 将我们的 `brod` 客户端更新到版本 4.4.4。这扩展了支持的 Kafka API 范围，特别是由于 `JoinGroups` API `v0`-`v1` 已被弃用。

- [#15628](https://github.com/emqx/emqx/pull/15628) 移除了 HStreamDB 数据集成。

#### 访问控制

- [#15349](https://github.com/emqx/emqx/pull/15349) 优化认证和授权的外部资源管理。以前，EMQX 可能会保持连接到为已禁用的认证或授权提供商配置的资源。

- [#15294](https://github.com/emqx/emqx/pull/15294) 增强 LDAP 认证和授权。
  LDAP 授权现在支持 JSON 格式的扩展 ACL 规则。
  LDAP 认证现在可以从 LDAP 获取 ACL 规则。这些规则被缓存在客户端元数据中，因此授权无需额外的 LDAP 查询即可执行。

#### 智能数据中心

- [#15525](https://github.com/emqx/emqx/pull/15525) 现在，当尝试移除被架构验证或消息转换引用的内部架构时，移除操作将被拒绝。

#### 持久化存储

- [#15463](https://github.com/emqx/emqx/pull/15463) 持久化存储 RAM 使用和存储效率优化。

  1. 为持久化存储添加了以下配置参数：

  - `durable_storage.messages.rocksdb.write_buffer_size`：每个分片的 RocksDB 内存表大小。
  - `durable_storage.messages.rocksdb.cache_size`：每个分片的 RocksDB 块大小。
  - `durable_storage.messages.rocksdb.max_open_files`：限制每个分片 RocksDB 使用的文件描述符数量。
  - `durable_storage.messages.layout.wildcard_thresholds`：允许为 `wildcard_optimized_v2` 存储布局调整通配符阈值

  2. 消息的默认 `serialization_schema` 已更改为 `asn1`。

#### 可观测性

- [#15594](https://github.com/emqx/emqx/pull/15594) 将集群中同时允许存在的最大跟踪数量公开为配置选项 `trace.max_traces`。此限制不适用于通过 `emqx ctl trace` 管理的节点本地跟踪。

  优化了跟踪实现，消除了每个创建的跟踪可能的原子泄漏。

- [#15556](https://github.com/emqx/emqx/pull/15556) 将每个单独跟踪的最大跟踪文件大小限制公开为配置选项 `trace.max_file_size`。

- [#15364](https://github.com/emqx/emqx/pull/15364) 向 OpenTelemetry 集成添加 HTTP 标头配置项，以适应具有 HTTP 认证的收集器。

#### 部署

- [#15484](https://github.com/emqx/emqx/pull/15484) 更改了我们的构建系统，使所有软件包都使用 [Elixir](https://elixir-lang.org/) 的 [Mix](https://hexdocs.pm/elixir/introduction-to-mix.html) 构建，因此所有软件包现在都提供 Elixir 支持。这为 Elixir 社区开放了 EMQX，允许我们在需要时使用 Elixir 依赖项，同时支持使用 [IEx](https://hexdocs.pm/iex/IEx.html) 作为更好的 EMQX 控制台。

- [#15399](https://github.com/emqx/emqx/pull/15399) 现在，`node_dump` 将以 HOCON 格式导出当前系统配置，并对机密信息进行脱敏处理。

#### 集群

- [#15600](https://github.com/emqx/emqx/pull/15600) 添加了新的 `cluster.description` 配置，允许为 EMQX 集群添加描述。此描述可以通过 `PUT /cluster` 更改，并在 `GET /cluster` 响应中查看。

#### 性能

- [#15536](https://github.com/emqx/emqx/pull/15536) 默认禁用 `node.global_gc_interval` 配置。

- [#15539](https://github.com/emqx/emqx/pull/15539) 优化 Erlang VM 参数。

  - 将分布式通道的缓冲区大小增加到 32MB，以避免在密集 Mnesia 操作期间出现 `busy_dist_port` 警报：`+zdbbl 32768`
  - 禁用调度器忙等待以减少操作系统观察到的 CPU 使用率：`+sbwt none +sbwtdcpu none +sbwtdio none`
  - 将调度器绑定类型设置为 `db` 以减少消息延迟：`+stbt db`

- [#15451](https://github.com/emqx/emqx/pull/15451) 为 TCP 监听器引入实验性 `socket` 后端，旨在改善消息处理延迟并减少计算资源使用。这可以通过新的 `tcp_backend` 监听器选项启用。

### 错误修复

#### 数据集成

- [#15647](https://github.com/emqx/emqx/pull/15647) 以前，如果 MongoDB 连接器的用户没有足够的权限在 `foo` 集合中执行 `find` 查询，它会被视为断开连接。现在已修复此问题。

- [#15603](https://github.com/emqx/emqx/pull/15603) 修复了 MQTT 桥接中陈旧连接显示为"已连接"且连接未重新建立的问题。


- [#15522](https://github.com/emqx/emqx/pull/15522) 修复了当未提供 `username` 时 Snowflake 连接器无法正确启动的问题。

- [#15476](https://github.com/emqx/emqx/pull/15476) 当大多数使用聚合模式的动作（Azure Blob Storage、Snowflake、S3Tables）的传送失败时，会打印以下日志：

  ```
  "emqx_connector_aggreg_delivery:format_status/1 crashed"
  ```

  现在已修复此问题，并且将记录更多关于传送过程的信息。

- [#15394](https://github.com/emqx/emqx/pull/15394) 修复了动作指标可能最终处于不一致状态的非常罕见的竞争条件。

- [#15383](https://github.com/emqx/emqx/pull/15383) 修复了当桥接启动失败时 MQTT 桥接中的潜在资源泄漏。以前，当桥接启动失败时，主题索引表没有被正确清理。

#### 规则引擎

- [#15569](https://github.com/emqx/emqx/pull/15569) 修复了当 `direct_dispatch` 模板为空字符串或解析为非布尔值时重新发布规则动作可能失败的问题。现在，如果发生此类情况，将使用默认值 `false`。

#### 核心 MQTT 功能

- [#15518](https://github.com/emqx/emqx/pull/15518) 解决了当大量共享订阅者同时断开连接时可能导致路由表和集群中共享订阅状态累积不一致的竞争条件。

- [#15416](https://github.com/emqx/emqx/pull/15416) 修复了在 WebSocket 连接会话过期期间偶尔出现的警告级别日志事件和崩溃，这是由最近的 WebSocket 性能改进引起的。这些对代理容量没有影响，但会产生如下日志条目：
  * `error: {function_clause,[{gen_tcp,send,[closed,[]],[{file,"gen_tcp.erl"},{line,966}]},{cowboy_websocket_linger,commands,3,[{file,"cowboy_websocket_linger.erl"},{line,665}]},...`
  * `message: {tcp,#Port<0.364>,<<136,130,...>>}, msg: emqx_session_mem_unknown_message`

- [#15396](https://github.com/emqx/emqx/pull/15396) 移除了断开连接客户端共享订阅的冗余清理操作，这些操作在高断开连接量下容易崩溃，导致全局代理状态的潜在不一致。

- [#15361](https://github.com/emqx/emqx/pull/15361) 修复了解析格式错误的 `User-Property` 对时的函数子句错误，其中对长度错误（太短）。

#### 访问控制

- [#15489](https://github.com/emqx/emqx/pull/15489) 修复 OIDC 颁发者架构验证。

  现在支持以前不支持的颁发者 URL：

  - `https://xxxxxxxx:8443/webman/sso/.well-known/openid-configuration`
  - `hostname`

#### 网关

- [#15342](https://github.com/emqx/emqx/pull/15342) 修复了当 clientinfo 覆盖模板包含未定义的数据包字段时 NATS 网关崩溃的问题，现在返回空二进制而不是未定义的原子。

#### 安全

- [#15581](https://github.com/emqx/emqx/pull/15581) 将 OTP 版本从 26.2.5.2 升级到 26.2.5.14

  此升级包括两个与 EMQX 相关的 TLS 相关修复：

  - 修复了证书续签期间由竞争条件导致的 TLS 连接崩溃。
  - 添加了对使用 PSS 参数签名的 RSA 证书的支持。以前 TLS 握手可能会因 `invalid_signature` 而失败。

#### 部署

- [#15580](https://github.com/emqx/emqx/pull/15580) 向 EMQX 企业版 helm 图表添加 emqxLicenseSecretRef 变量，允许用户指定包含 EMQX 许可证密钥的 Kubernetes 机密。这修复了失效的 emqxLicenseSecretName 变量的问题。

- [#15553](https://github.com/emqx/emqx/pull/15553) 修复了如果使用默认值部署图表时除一个节点外所有节点都会崩溃的 helm 图表问题。

#### HTTP 服务器

- [#15547](https://github.com/emqx/emqx/pull/15547) 修复了发送带有大消息体的 HTTP 请求时的错误。