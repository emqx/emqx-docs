# EMQX 企业版 v6 版本

## 6.0.0

*发布日期: 2025-09-*

在升级到 EMQX 6.0.0 之前，请务必查阅不兼容变更和已知问题。

### 功能亮点

EMQX Enterprise 6.0.0 是 EMQX 企业版 6 系列的首个发布版本，带来了重大的架构改进和全新能力。

#### 消息队列

#### 命名空间

#### 持久存储

通过新的 RocksDB 配置选项和默认启用的 ASN1 序列化方案，显著提升了内存使用效率和存储效率。

#### 新增数据集成

- Google BigQuery
- AWS AlloyDB
- CockroachDB
- AWS Redshift

#### 增强的数据集成

- **AWS**:
  - 在使用 S3 或 S3Tables 数据集成时，支持来自 EC2 实例的 Instance Metadata Service v2 API。这使得 EMQX 能够在无需手动配置 AWS 凭证的情况下无缝访问 S3 存储桶，并利用 IAM 角色提升安全性。
  - S3 Tables Action 新增 Parquet 格式支持。
- **RabbitMQ**: 在 RabbitMQ Sink 中支持自定义 Headers 和 Properties 模板，以增强消息路由能力和与 RabbitMQ 的兼容性。
- **Snowflake**: Snowflake Action 新增 Snowpipe Streaming 上传模式（预览功能）。

#### Elixir 支持

所有安装包现在均基于 Mix 构建系统 提供 Elixir 支持，为 Elixir 社区开放 EMQX，并通过 IEx 控制台提供更强大的工具链。

#### 增强的 LDAP 支持

LDAP 授权现在支持基于 JSON 格式的扩展 ACL 规则；LDAP 认证也可直接从 LDAP 获取 ACL 规则，并支持客户端缓存。

#### 改进的追踪功能

新增可配置的追踪数量上限（`trace.max_traces`）和追踪文件大小上限（`trace.max_file_size`），并优化了实现以防止 atom 泄漏。

#### 集群管理

新增 `cluster.description` 配置项，允许用户在 EMQX Dashboard 中设置和显示自定义集群描述。

### 增强

#### 消息队列

- [#15789](https://github.com/emqx/emqx/pull/15789) 实现了消息队列，这是由 `topic/filter` 标识的消息集合。每个队列都有明确的生命周期，并在其生命周期内自动补充与队列主题过滤器匹配的已发布消息。客户端可以通过订阅特殊格式的主题 `$q/topic/filter` 来协同消费队列中的消息。

#### 核心 MQTT 功能

- [#15805](https://github.com/emqx/emqx/pull/15805) 引入了一个专用的工作线程池，用于处理分片广播式消息分发。之前，broker 线程池同时处理订阅管理和消息分发，可能导致调度争用。此更改将广播式分发的工作负载单独划分到一个独立线程池中，以确保更均衡和高效地处理发布/订阅操作。

#### 访问控制

- [#15349](https://github.com/emqx/emqx/pull/15349) 优化了认证和授权的外部资源管理。此前，EMQX 在禁用认证或授权源的情况下，仍可能与配置的资源保持连接。
- [#15294](https://github.com/emqx/emqx/pull/15294) 增强了 LDAP 认证和授权功能。LDAP 授权现在支持使用 JSON 格式的扩展 ACL 规则。LDAP 认证现在可以从 LDAP 获取 ACL 规则。这些规则缓存在客户端的元数据中，因此无需额外的 LDAP 查询即可执行授权。
- [#15730](https://github.com/emqx/emqx/pull/15730) 新增支持根据认证结果覆盖客户端 ID。如果认证后端在成功认证后返回 `clientid_override` 属性，它将替换客户端原有的客户端 ID。
  以下认证后端现在支持 `clientid_override`：
  - HTTP
  - JWT
  - LDAP
  - MongoDB
  - MySQL
  - Postgres
  - Redis
- [#15820](https://github.com/emqx/emqx/pull/15820) 出于更安全的默认配置考量，将配置 `authorization.no_match` 的默认值从允许（`allow`） 更改为拒绝 （`deny`）。

#### 集群

- [#15600](https://github.com/emqx/emqx/pull/15600) 引入了一个新的配置选项 `cluster.description`，允许您为 EMQX 集群添加描述性标签。此描述可以通过 `PUT /cluster` 更新，并通过 `GET /cluster` API 检索。

#### 基于 LLM 的 MQTT 数据处理

- [#15467](https://github.com/emqx/emqx/pull/15467) AI 补全服务提供器现已支持传输层配置选项。用户可配置连接超时和最大连接数，从而在消息吞吐量较高、提供器负载较大时，减少 `checkout_timeout` 错误的发生。
- Flow 设计器支持与 Google Gemini 模型集成。
- [#15631](https://github.com/emqx/emqx/pull/15631) 添加了一个新的 API 端点，用于列出 AI 提供器可用的所有模型。
- [#15467](https://github.com/emqx/emqx/pull/15467) 为 AI 补全服务提供器开放了传输选项。这些选项允许配置连接超时和到 AI 补全服务提供器的最大连接数。
- [#15724](https://github.com/emqx/emqx/pull/15724) 为 AI 补全服务提供器和补全配置文件引入了 `openai_response` 类型，以使用 OpenAI 的 `response` API。

#### 数据集成

- [#15418](https://github.com/emqx/emqx/pull/15418) EMQX 新增与 BigQuery 的数据集成。
- [#15401](https://github.com/emqx/emqx/pull/15401) 在 Snowflake 动作中添加了对 Snowpipe Streaming 上传模式的支持。
  *注意：Snowpipe Streaming 目前是*[*预览功能*](https://docs.snowflake.com/en/release-notes/preview-features)*，仅适用于托管在 AWS 上的 Snowflake 帐户。*
- [#15387](https://github.com/emqx/emqx/pull/15387) 为 Kinesis 生产者连接器和动作的健康检查增加了限速机制，以遵守 AWS API 限额并提升集群行为一致性：
  - 对 `ListStreams` 和 `DescribeStream` 接口的调用分别限制为每个连接器每秒 5 次和 10 次。
  - 集群中的核心节点协调分布式限速器，以确保限速一致。
  - 若健康检查被限速或超时，连接器或动作将保留原状态，而不是被标记为已断开。
  - 新增配置项 `resource_opts.health_check_interval_jitter`，在健康检查间隔基础上引入一个均匀随机延迟，减少同一连接器下多个动作同时发起健康检查的可能性。
- [#15176](https://github.com/emqx/emqx/pull/15176) 升级了 GreptimeDB 连接器客户端，并支持一个可选的新参数 `ttl`，用于为自动创建的表设置默认的生存时间。
- [#15649](https://github.com/emqx/emqx/pull/15649) EMQX 新增与 AWS AlloyDB、CockroachDB 和 AWS Redshift 的数据集成。
- [#15635](https://github.com/emqx/emqx/pull/15635) 在 RocketMQ 动作中添加了新的 `key` 和 `tag` 模板字段，允许自定义消息的键和标签。此外，还为 `Produce Strategy` 字段引入了一个新的 `key_dispatch` 选项。
- [#15621](https://github.com/emqx/emqx/pull/15621) 现在，`access_key_id` 和 `secret_access_key` 是 S3 Tables 连接器的可选字段。如果省略，它们将从部署 EMQX 的 EC2 实例的实例元数据服务 v2 API 中获取。
- [#15628](https://github.com/emqx/emqx/pull/15628) 移除了 HStreamDB 数据集成。
- [#15544](https://github.com/emqx/emqx/pull/15544) 为 Datalayers 集成添加了 Arrow Flight SQL NIF 驱动支持。
- [#15637](https://github.com/emqx/emqx/pull/15637) 为 RabbitMQ 动作添加了消息头和属性的模板支持。
- [#15864](https://github.com/emqx/emqx/pull/15864) 移除了已弃用的“Bridges V1” API 和配置模式。`/bridges/*` 下的所有端点和 `bridges` 根键下的配置条目已不再可用，因为数据集成已完全迁移到“连接器/动作/Source”模型。

#### 智能数据中心

- [#15525](https://github.com/emqx/emqx/pull/15525) 防止删除仍在使用的内部 schema。如果一个 schema 被 schema 验证或消息转换引用，它将不能再被移除，以避免运行时错误和配置不一致。

#### 持久存储

- [#15463](https://github.com/emqx/emqx/pull/15463) 改进了持久存储的 RAM 使用和存储效率。
  - 为持久存储引入了以下配置参数，以改进对 RocksDB 内存使用和存储性能的控制：
    - `durable_storage.messages.rocksdb.write_buffer_size`：每个分片的 RocksDB memtable 大小。
    - `durable_storage.messages.rocksdb.cache_size`：每个分片的 RocksDB 块大小。
    - `durable_storage.messages.rocksdb.max_open_files`：限制每个分片 RocksDB 使用的文件描述符数量。
    - `durable_storage.messages.layout.wildcard_thresholds`：允许为 `wildcard_optimized_v2` 存储布局调整通配符阈值。
  - 此外，存储消息的默认 `serialization_schema` 已更改为 `asn1`。

#### CLI

- [#15399](https://github.com/emqx/emqx/pull/15399) `node_dump` 工具现在导出当前系统配置为 HOCON 格式，并自动对敏感信息（如密码和密钥）进行脱敏处理，以确保安全。

#### 命名空间

- [#15841](https://github.com/emqx/emqx/pull/15841) 优化了命名空间会话数的刷新频率。
  
  - 当某个命名空间的连接数少于 1000 时，会话数将按需刷新；
  - 当连接数大于或等于 1000 时，会话数每 5 秒刷新一次。
  
  在从 6.0 之前版本进行滚动升级期间，由于内部跟踪表结构的变更，命名空间的会话数可能会出现不一致的情况。这属于预期行为：随着客户端逐步重新连接到已升级的节点，会话数将逐步趋于稳定，并在所有节点升级至 6.0 版本后恢复准确。

#### 可观测性

- [#15594](https://github.com/emqx/emqx/pull/15594) 引入了一个新的配置选项 `trace.max_traces`，用于控制集群范围内活动追踪的最大数量。此限制不适用于使用 `emqx ctl trace` 管理的节点本地 Trace。
  同时优化了 Trace 实现，消除了每个 Trace 可能导致的 atom 泄漏问题。
  
- [#15556](https://github.com/emqx/emqx/pull/15556) 引入了一个新的配置选项 `trace.max_file_size`，用于限制单个 Trace 的最大文件大小。

- [#15650](https://github.com/emqx/emqx/pull/15650) 实现了追踪日志自动轮转功能。
  
  当单个追踪日志文件大小超过 `trace.max_file_size` 限制时，EMQX 不再丢弃所有后续事件并向 `stderr` 输出难以理解的警告信息。取而代之的是，会优先丢弃最旧的部分事件，以保留最新的追踪数据。
  
  受此变更影响：
  
  - EMQX 现在为每个活动的追踪任务维护多个日志文件，追踪目录的结构也已相应调整。
  - Trace API 已更新以支持此行为，Log Stream API 也可能返回新的错误，例如在消费者处理过慢时，日志流变为过期状态。
  
- [#15904](https://github.com/emqx/emqx/pull/15904) 支持通过 Trace API 查看和更新追踪配置。

#### 性能

- [#15451](https://github.com/emqx/emqx/pull/15451) 为 TCP 监听器引入了一个实验性的 `socket` 后端，旨在提高消息处理延迟并减少计算资源使用。该功能可以通过新的 `tcp_backend` 监听器选项启用。

#### 构建和工具

- [#15484](https://github.com/emqx/emqx/pull/15484) 将构建系统切换到 [Elixir](https://elixir-lang.org/) 的 [Mix](https://hexdocs.pm/elixir/introduction-to-mix.html)，使所有软件包都包含原生 Elixir 支持。这一变化改进了开发人员工具，允许在需要时与 Elixir 依赖项集成，并能够使用 [IEx](https://hexdocs.pm/iex/IEx.html) shell 作为更强大的 EMQX 控制台。

#### License

- [#15921](https://github.com/emqx/emqx/pull/15921) 引入了 License 告警，用于监控集群范围内的最大 TPS（Transactions Per Second，每秒事务数）。
  - 每个节点的 TPS 计算方式为过去 10 秒内接收和发送的 MQTT 消息数的平均值。
  - 集群总 TPS 每 5 秒聚合一次。
  - 如果观测到的 TPS 超过了 License 限制，将触发告警。
  - 告警会一直保持，直到应用了具有更高 TPS 配额的 License 为止。

#### MQTT over QUIC

- [#15997](https://github.com/emqx/emqx/pull/15997) 添加了通过设置环境变量 `QUICER_SKIP_NIF_LOAD=1` 来禁用 QUIC 协议栈加载的支持。

### 修复

#### 核心 MQTT 功能

- [#15396](https://github.com/emqx/emqx/pull/15396) 移除了已断开客户端的共享订阅中冗余的清理操作。这些操作在高并发断开情况下容易导致崩溃，并可能引发全局路由状态不一致。
- [#15361](https://github.com/emqx/emqx/pull/15361) 修复了在解析格式错误的 `User-Property` 键值对时产生的 `function_clause` 错误，特别是当键值对的长度无效（过短）时。

#### 访问控制

- [#15489](https://github.com/emqx/emqx/pull/15489) 修复了单点登录（SSO）设置中的 OIDC issuer URL 验证。此前，带有端口号的 issuer URL（如
  `https://xxxxxxxx:8443/webman/sso/.well-known/openid-configuration`）会被错误地拒绝并报 `bad_port_number`。
- 现在支持这些 URL。

#### 规则引擎

- [#15569](https://github.com/emqx/emqx/pull/15569) 修复了当 `direct_dispatch` 模板为空或解析为非布尔值时，消息重发布规则动作可能失败的问题。在这些情况下，现在将使用默认值 `false`。

#### 数据集成

- [#15522](https://github.com/emqx/emqx/pull/15522) 修复了 Snowflake 连接器在未提供用户名时无法正常启动的问题。
- [#15476](https://github.com/emqx/emqx/pull/15476) 修复了 `emqx_connector_aggreg_delivery` 中遗漏的回调函数，导致在格式化聚合模式动作（如 Azure Blob Storage、Snowflake、S3 Tables）传输状态时发生崩溃。
   此问题发生在传输失败或调用 `gen_server:format_status/1` 检查传输状态时。现已修复，并增加了更详细的日志信息以便排查。
- [#15394](https://github.com/emqx/emqx/pull/15394) 修复了一个罕见的竞态条件，某些情况下由于收到意外的异步响应，导致动作指标统计出现不一致的问题。
- [#15647](https://github.com/emqx/emqx/pull/15647) 修复了 MongoDB 连接器被错误标记为 `Disconnected` 的问题。此前，如果配置的 MongoDB 账号缺少对某个集合执行 `find` 查询的权限，就会触发该问题。
- [#15603](https://github.com/emqx/emqx/pull/15603) 修复了 MQTT 桥接中的一个问题：过期的连接可能仍显示为 `Connected` 状态，且不会自动重连。
- [#15383](https://github.com/emqx/emqx/pull/15383) 修复了 MQTT 桥接中可能存在的资源泄漏问题。当桥接启动失败时，主题索引表未被正确清理。
- [#15786](https://github.com/emqx/emqx/pull/15786) 修复了探测 RocketMQ 连接器时可能存在的 atom 泄漏。
- [#15806](https://github.com/emqx/emqx/pull/15806) 改进了 Oracle 动作创建时的验证。以前，在极少数情况下，包含无效 SQL 语句的动作可能会被成功添加。
- [#15848](https://github.com/emqx/emqx/pull/15848) 改进了 Oracle 连接器的错误报告。当连接器断开连接时，其状态现在包含更具体的原因，使诊断更容易。

#### 数据智能中心

- [#15839](https://github.com/emqx/emqx/pull/15839) 修复了使用 `map<_, _>` 字段的 Protobuf schema 的编码问题。
  此前，包含 `map<string, string>` 字段的 schema 可能无法编码有效的 payload，导致隐晦的运行时错误。
  示例模式：
  
  ```protobuf
  syntax = "proto3";
  
  message test {
  map<string, string> args = 1;
  }
  ```
  示例规则：
  ```sql
  SELECT
  schema_encode('xxx', json_decode(payload), 'test') as protobuf_test
  FROM
  "t/#"
  ```
  无法编码的示例 payload：
  ```json
  {
  "args": {
  "env": "stag"
  }
  }
  ```
  此前的错误类似于：
  ```
  2025-06-17T06:59:22.725785+00:00 [warning] tag: RULE_SQL_EXEC, clientid: c_emqx, msg: SELECT_clause_exception, reason: {error,{gpb_type_error,{bad_unicode_string,[{value,env},{path,"test.args.key"}]}},[{'$schema_parser_xxx',mk_type_error,3,[{file,"$schema_parser_xxx.erl"},{line,437}]},{'$schema_parser_xxx','-v_map<string,string>/3-lc$^0/1-0-',3,[{file,"$schema_parser_xxx.erl"},{line,429}]},{'$schema_parser_xxx','v_map<string,string>',3,[{file,"$schema_parser_xxx.erl"},{line,429}]},{'$schema_parser_xxx',v_msg_test,3,[{file,"$schema_parser_xxx.erl"},{line,404}]},{'$schema_parser_xxx',encode_msg,3,[{file,"$schema_parser_xxx.erl"},{line,73}]},{emqx_schema_registry_serde,with_serde,2,[{file,"emqx_schema_registry_serde.erl"},{line,212}]}...
  ```

#### 可观测性

- [#15931](https://github.com/emqx/emqx/pull/15931) 修复了与 EMQX 告警系统相关的两个问题：
  
  - 解决了一个在节点启动期间可能出现虚假但无害的错误日志的错误，例如：
    ```
    [error] Generic event handler emqx_alarm_handler crashed ...
    Reason: {aborted,{no_exists,[emqx_activated_alarm,runq_overload]}}
    ```
  - 修复了一个在某些条件下告警激活超时可能导致连接进程崩溃的错误。

#### MQTT over QUIC

- [#15614](https://github.com/emqx/emqx/pull/15614) QUIC 监听器：当启用 TLS 密钥日志记录（`SSLKEYLOGFILE`）时，即使握手失败，EMQX 现在也会转储 TLS 密钥。

#### 集群连接

- [#15894](https://github.com/emqx/emqx/pull/15894) 以前，通过 `GET /cluster/links` 列出所有集群连接时，禁用的连接会以 `inconsistent` 状态返回。现在它们将以 `disconnected` 状态返回。

#### 性能

- [#15696](https://github.com/emqx/emqx/pull/15696) 为 WebSocket (WS) 和 WebSocket Secure (WSS) 监听器添加了连接速率限制支持。
  现在强制执行 `max_conn_rate` 和 `max_conn_burst` 配置选项：超过定义速率的传入连接在接受后会立即关闭，与现有的 TCP 监听器行为一致。
  此外，`max_connections` 的行为也已更新。当超过连接限制时，WS/WSS 监听器现在会在任何 HTTP 握手之前立即关闭连接，导致socket 突然关闭，而不是返回 HTTP 429 响应。
- [#15854](https://github.com/emqx/emqx/pull/15854) 将默认的 `active_n` 值从 `100` 减少到 `10`，以提高 MQTT 客户端的响应能力，特别是在高消息速率和消息 payload 较小的情况下。
  较低的 `active_n` 会在 TCP 层引入更强的背压机制，比默认的 `Receive-Maximum` of `32` 更严格，这在以下情况下有所帮助：
  - 客户端进程被外部授权检查阻塞
  - 数据集成操作延迟了消息处理
  - 系统负载过重或接近资源限制
- [#15981](https://github.com/emqx/emqx/pull/15981) 防止了因 Mnesia 事务阻塞在清理大量审计日志时导致的内存过度增长。这提高了在繁重的审计日志维护操作期间的系统稳定性和内存效率。