# EMQX 企业版 v6.0

## 6.0.0

*发布日期: 2025-09-*

在升级到 EMQX 6.0.0 之前，请务必查阅不兼容变更和已知问题。

### 功能亮点

- **主要版本发布**：EMQX 企业版 6.0.0 是 EMQX 企业版 v6 系列的第一个版本，带来了重大的架构改进和新功能。
- **增强的 AWS 集成**：在使用 S3 或 S3Tables 数据集成时，支持来自 EC2 实例的实例元数据服务 v2 API。这使得无需手动配置 AWS 凭证即可无缝访问 S3 存储桶，利用 IAM 角色提高了安全性。
- **Elixir 支持**：所有软件包现在都通过 Mix 构建系统提供 Elixir 支持，为 Elixir 社区打开了 EMQX 的大门，并通过 IEx 控制台实现了更好的工具支持。
- **新的数据集成**：
  - 用于向 Google BigQuery 追加数据的 BigQuery 连接器和动作
  - Snowflake 动作的 Snowpipe Streaming 上传模式（预览功能）
  - S3Tables 动作支持 Parquet 格式
- **持久存储优化**：通过新的 RocksDB 配置选项和默认的 ASN1 序列化模式，显著提高了 RAM 使用率和存储效率。
- **增强的 LDAP 支持**：LDAP 授权现在支持 JSON 格式的扩展 ACL 规则，LDAP 认证可以直接从 LDAP 获取 ACL 规则，并进行客户端缓存。
- **改进的追踪**：可配置最大追踪数（`trace.max_traces`）和追踪文件大小（`trace.max_file_size`）的限制，并优化了实现以防止原子泄漏。
- **集群管理**：新的 `cluster.description` 配置选项允许用户在 EMQX Dashboard 中设置和显示自定义集群描述。

### 增强

#### 消息队列

- [#15789](https://github.com/emqx/emqx/pull/15789) 实现了消息队列，这是由 `topic/filter` 标识的消息集合。每个队列都有明确的生命周期，并在其生命周期内自动补充与队列主题过滤器匹配的已发布消息。客户端可以通过订阅特殊格式的主题 `$q/topic/filter` 来协同消费队列中的消息。

#### 核心 MQTT 功能

- [#15805](https://github.com/emqx/emqx/pull/15805) 引入了一个专用的工作池来处理分片扇出消息传递。以前，代理池同时处理订阅管理和消息分派，可能导致调度争用。此更改将扇出分派工作负载分离到其自己的池中，以确保更均衡和高效地处理发布/订阅操作。

#### 访问控制

- [#15349](https://github.com/emqx/emqx/pull/15349) 优化了认证和授权的外部资源管理。此前，EMQX 在禁用认证或授权源的情况下，仍可能与配置的资源保持连接。
- [#15294](https://github.com/emqx/emqx/pull/15294) 增强了 LDAP 认证和授权功能。LDAP 授权现在支持使用 JSON 格式的扩展 ACL 规则。LDAP 认证现在可以从 LDAP 获取 ACL 规则。这些规则缓存在客户端的元数据中，因此无需额外的 LDAP 查询即可执行授权。
- [#15730](https://github.com/emqx/emqx/pull/15730) 新增支持根据认证结果覆盖客户端 ID。如果认证后端在成功认证后返回 `clientid_override` 属性，它将替换客户端原有的客户端 ID。
  以下后端现在支持 `clientid_override`：
  - HTTP
  - JWT
  - LDAP
  - MongoDB
  - MySQL
  - Postgres
  - Redis
- [#15820](https://github.com/emqx/emqx/pull/15820) 为了更好的安全默认设置，将配置 `authorization.no_match` 的默认值从 `allow` 更改为 `deny`。

#### 集群

- [#15600](https://github.com/emqx/emqx/pull/15600) 引入了一个新的配置选项 `cluster.description`，允许您为 EMQX 集群添加描述性标签。此描述可以通过 `PUT /cluster` 更新，并通过 `GET /cluster` API 检索。

#### 基于 LLM 的 MQTT 数据处理

- [#15467](https://github.com/emqx/emqx/pull/15467) 为 AI 推理服务提供商开放了传输配置选项。用户现在可以配置连接超时和到 AI 推理服务提供商的最大连接数。这有助于在高消息吞吐量和提供商负载较高时防止 `checkout_timeout` 错误。
- Flow Designer 支持与 [Google Gemini 模型](https://docs.mqttce.com/zh/emqx/v6.0/flow-designer/gemini-node-quick-start.html)集成。
- [#15631](https://github.com/emqx/emqx/pull/15631) 添加了一个新的 API 端点，用于列出 AI 提供商可用的所有模型。
- [#15467](https://github.com/emqx/emqx/pull/15467) 为 AI 推理服务提供商开放了传输选项。这些选项允许配置连接超时和到 AI 推理服务提供商的最大连接数。
- [#15724](https://github.com/emqx/emqx/pull/15724) 为 AI 推理服务提供商和推理配置文件引入了 `openai_response` 类型，以使用 OpenAI 的 `response` API。

#### 数据集成

- [#15418](https://github.com/emqx/emqx/pull/15418) EMQX 支持与 [BigQuery](https://docs.mqttce.com/zh/emqx/v6.0/data-integration/bigquery.html) 的数据集成。
- [#15401](https://github.com/emqx/emqx/pull/15401) 在 [Snowflake 动作](https://docs.mqttce.com/zh/emqx/v6.0/data-integration/snowflake.html#_create-a-rule-with-snowflake-sink)中添加了对 Snowpipe Streaming 上传模式的支持。
  *注意：Snowpipe Streaming 目前是*[*预览功能*](https://docs.snowflake.com/en/release-notes/preview-features)*，仅适用于托管在 AWS 上的 Snowflake 帐户。*
- [#15387](https://github.com/emqx/emqx/pull/15387) 为 Kinesis 生产者连接器和动作的健康检查增加了限速机制，以遵守 AWS API 限额并提升集群行为一致性：
  - 对 `ListStreams` 和 `DescribeStream` 接口的调用分别限制为每个连接器每秒 5 次和 10 次；
  - 集群中的核心节点协调分布式限速器，以确保限速一致。
  - 若健康检查被限速或超时，连接器或动作将保留原状态，而不是被标记为已断开。
  - 新增配置项 `resource_opts.health_check_interval_jitter`，在健康检查间隔基础上引入一个均匀随机延迟，减少同一连接器下多个动作同时发起健康检查的可能性。
- [#15371](https://github.com/emqx/emqx/pull/15371) 为 `GET /actions_summary` 和 `GET /sources_summary` 接口的响应以及 `GET /actions/:id` 接口返回的备选动作添加了 `tags` 字段。
- [#15360](https://github.com/emqx/emqx/pull/15360) 在 S3 Tables 动作中添加了以 Parquet 格式写入数据文件的支持。
- [#15176](https://github.com/emqx/emqx/pull/15176) 升级了 GreptimeDB 连接器客户端，并支持一个可选的新参数 `ttl`，用于为自动创建的表设置默认的生存时间。
- [#15649](https://github.com/emqx/emqx/pull/15649) EMQX 支持与 [AWS AlloyDB](https://docs.mqttce.com/zh/emqx/v6.0/data-integration/alloydb.html)、[CockroachDB](https://docs.mqttce.com/zh/emqx/v6.0/data-integration/cockroachdb.html) 和 [AWS Redshift](https://docs.mqttce.com/zh/emqx/v6.0/data-integration/redshift.html) 的数据集成。
- [#15635](https://github.com/emqx/emqx/pull/15635) 在 [RocketMQ 动作](https://docs.mqttce.com/zh/emqx/v6.0/data-integration/data-bridge-rocketmq.html#_create-a-rule-with-rocketmq-sink-for-message-storage)中添加了新的 `key` 和 `tag` 模板字段，允许自定义消息的键和标签。此外，还为 `Produce Strategy` 字段引入了一个新的 `key_dispatch` 选项。
- [#15621](https://github.com/emqx/emqx/pull/15621) 现在，`access_key_id` 和 `secret_access_key` 是 S3Tables 连接器的可选字段。如果省略，它们将从部署 EMQX 的 EC2 实例的实例元数据服务 v2 API 中获取。
- [#15542](https://github.com/emqx/emqx/pull/15542) 将 `erlcloud` 库升级到 `3.8.3.0`。升级后，如果 EMQX 运行的 EC2 实例具有正确的 IAM 权限来读写配置的 S3 存储桶，就可以在不指定访问密钥 ID 和私有访问密钥的情况下配置 [S3 连接器](https://docs.mqttce.com/zh/emqx/v6.0/data-integration/s3.html#_create-a-connector)。
- [#15583](https://github.com/emqx/emqx/pull/15583) [#15585](https://github.com/emqx/emqx/pull/15585) 将 `brod` 客户端更新到 4.4.4 版本，扩展了对更广泛 Kafka API 的支持。此更新解决了 `JoinGroups` API 版本 `v0` - `v1` 的弃用问题。
- [#15628](https://github.com/emqx/emqx/pull/15628) 移除了 HStreamDB 数据集成。
- [#15544](https://github.com/emqx/emqx/pull/15544) 为 Datalayers 集成添加了 Arrow Flight SQL NIF 驱动支持。
- [#15637](https://github.com/emqx/emqx/pull/15637) 为 RabbitMQ 动作添加了消息头和属性的模板支持。
- [#15864](https://github.com/emqx/emqx/pull/15864) 移除了已弃用的“Bridges V1” API 和配置模式。`/bridges/*` 下的所有端点和 `bridges` 根键下的配置条目已不再可用，因为数据集成已完全迁移到“连接器/动作/源”模型。

#### 智能数据中心

- [#15525](https://github.com/emqx/emqx/pull/15525) 防止删除仍在使用的内部模式。如果一个模式被模式验证或消息转换引用，它将不能再被移除，以避免运行时错误和配置不一致。

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

- [#15841](https://github.com/emqx/emqx/pull/15841) 改进了命名空间会话的会话计数刷新率。
  - 如果命名空间的连接数少于 1000，其会话计数现在按需更新。
  - 对于具有 1000 或更多连接的命名空间，计数每 5 秒更新一次。
  在从 6.0 之前的版本进行滚动升级期间，由于内部跟踪表的更改，会话计数可能会出现不一致。这是预期的：随着客户端重新连接到升级后的节点，会话计数将逐渐稳定，并在所有节点都运行 6.0 或更高版本后变得准确。

#### 可观测性

- [#15594](https://github.com/emqx/emqx/pull/15594) 引入了一个新的配置选项 `trace.max_traces`，用于控制集群范围内活动追踪的最大数量。此限制不适用于使用 `emqx ctl trace` 管理的节点本地追踪。
  此更新还优化了追踪实现，以消除每个创建的追踪可能存在的原子泄漏。
- [#15556](https://github.com/emqx/emqx/pull/15556) 引入了一个新的配置选项 `trace.max_file_size`，用于限制每个单独追踪的最大文件大小。
- [#15650](https://github.com/emqx/emqx/pull/15650) 实现了自动追踪日志轮换。
  当追踪文件大小超过 `trace.max_file_size` 时，EMQX 不再丢弃所有后续事件并向 `stderr` 发出难以理解的警告。相反，部分最旧的事件将被丢弃，而最新的事件将被保留。
  因此，这也意味着：
  * EMQX 现在为每个活动追踪维护多个追踪日志文件。追踪目录的布局也相应更改。
  * 追踪 API 已更新以反映此行为。日志流 API 可能会返回新的错误，例如当流因消费者速度慢而变得陈旧时。
- [#15904](https://github.com/emqx/emqx/pull/15904) 支持通过 Trace API 查看和更新追踪配置。

#### 性能

- [#15536](https://github.com/emqx/emqx/pull/15536) 默认禁用了 `node.global_gc_interval` 配置。该配置在启用时会引发 CPU 波动和消息延迟，而 Erlang 内置 GC 已足够应对大部分场景。禁用后整体性能更稳定。
- [#15539](https://github.com/emqx/emqx/pull/15539) 优化 Erlang VM 参数以提升性能与稳定性：
  - 增大分布式通道缓冲区至 32 MB（`+zdbbl 32768`），避免在高强度 Mnesia 操作中触发 `busy_dist_port` 报警。
  - 禁用调度器忙等待（`+sbwt none +sbwtdcpu none +sbwtdio none`），降低操作系统报告的 CPU 使用率。
  - 设置调度器绑定类型为 db（`+stbt db`），以降低消息延迟。
- [#15451](https://github.com/emqx/emqx/pull/15451) 为 TCP 监听器引入了一个实验性的 `socket` 后端，旨在提高消息处理延迟并减少计算资源使用。该功能可以通过新的 `tcp_backend` 监听器选项启用。

#### 构建和工具

- [#15484](https://github.com/emqx/emqx/pull/15484) 将构建系统切换到 [Elixir](https://elixir-lang.org/) 的 [Mix](https://hexdocs.pm/elixir/introduction-to-mix.html)，使所有软件包都包含原生 Elixir 支持。这一变化改进了开发人员工具，允许在需要时与 Elixir 依赖项集成，并能够使用 [IEx](https://hexdocs.pm/iex/IEx.html) shell 作为更强大的 EMQX 控制台。

#### License

- [#15921](https://github.com/emqx/emqx/pull/15921) 引入了集群范围最大每秒事务数（TPS）的许可证警报。
  - 每个节点计算 TPS 为过去 10 秒内发送和接收的 MQTT 消息的平均数。
  - 总集群 TPS 每 5 秒聚合一次。
  - 如果观察到的 TPS 超过许可证限制，将触发警报。
  - 警报将保持活动状态，直到应用了具有更高 TPS 允许量的许可证。

#### MQTT over QUIC

- [#15997](https://github.com/emqx/emqx/pull/15997) 添加了通过设置环境变量 `QUICER_SKIP_NIF_LOAD=1` 来禁用 QUIC 协议栈加载的支持。

### 修复

#### 核心 MQTT 功能

- [#15396](https://github.com/emqx/emqx/pull/15396) 移除了已断开连接客户端的共享订阅的冗余清理操作。这些操作在高频断开情况下容易导致崩溃，并可能导致全局代理状态不一致。
- [#15361](https://github.com/emqx/emqx/pull/15361) 修复了在解析格式错误的 `User-Property` 键值对时产生的 `function_clause` 错误，特别是当键值对的长度无效（过短）时。
- [#15416](https://github.com/emqx/emqx/pull/15416) 修复了 WebSocket 连接会话过期时偶尔出现的 warning 级别日志和崩溃问题。该问题由近期的 WebSocket 性能优化引入，虽然不会影响 Broker 的容量，但会在日志中产生如下错误信息：
  - `error: {function_clause,[{gen_tcp,send,[closed,[]],[{file,“gen_tcp.erl”},{line,966}]},{cowboy_websocket_linger,commands,3,[{file,“cowboy_websocket_linger.erl”},{line,665}]},...`
  - `message: {tcp,#Port<0.364>,<<136,130,...>>}, msg: emqx_session_mem_unknown_message`

#### 部署

- [#15580](https://github.com/emqx/emqx/pull/15580) 在 EMQX Enterprise Helm Chart 中新增变量 `emqxLicenseSecretRef`，可指定包含 EMQX License Key 的 Kubernetes Secret，使 License 自动生效。
  该变量替代了无效的 `emqxLicenseSecretName`，后者仅创建并挂载 Secret 文件，却未将 License 应用于 EMQX。
- [#15553](https://github.com/emqx/emqx/pull/15553) 修复了 EMQX Helm chart 的一个问题：在使用默认配置部署 EMQX 时，会启动多个副本，并导致除一个节点外其余节点全部崩溃。现在 Helm chart 默认改为单副本，因为集群部署需要商业 License。

#### 安全

- [#15581](https://github.com/emqx/emqx/pull/15581) 将 Erlang/OTP 从 26.2.5.2 升级至 26.2.5.14，包含两个与 TLS 相关的重要修复：
  - 修复了因证书更新过程中的竞争条件导致的 TLS 连接崩溃。
  - 现在可以正常使用 RSASSA-PSS 签名的 RSA 证书。此前，TLS 握手可能因 `bad_certificate / invalid_signature` 错误而失败。

#### 访问控制

- [#15489](https://github.com/emqx/emqx/pull/15489) 修复了单点登录（SSO）设置中的 OIDC issuer URL 验证。以前，包含端口号的 issuer URL（例如 `https://xxxxxxxx:8443/webman/sso/.well-known/openid-configuration`）会被 `bad_port_number` 错误拒绝。现在支持这些 URL。

#### 集群

- [#15518](https://github.com/emqx/emqx/pull/15518) 修复了一个竞争条件，该问题在大量共享订阅者同时断开连接时，可能导致集群中路由表和共享订阅状态持续出现不一致。

#### 规则引擎

- [#15569](https://github.com/emqx/emqx/pull/15569) 修复了当 `direct_dispatch` 模板为空或解析为非布尔值时，Republish 规则动作可能失败的问题。在这些情况下，现在将使用默认值 `false`。

#### 数据集成

- [#15522](https://github.com/emqx/emqx/pull/15522) 修复了如果未提供 `username`，Snowflake 连接器将无法正确启动的问题。
- [#15476](https://github.com/emqx/emqx/pull/15476) 修复了 `emqx_connector_aggreg_delivery` 中一个缺失的回调，该回调在格式化聚合模式动作（例如 Azure Blob Storage、Snowflake、S3 Tables）的交付过程状态时导致崩溃。
  这发生在失败时或使用 `gen_server:format_status/1` 检查交付过程时。问题现已解决，并将记录更详细的交付状态信息。
- [#15394](https://github.com/emqx/emqx/pull/15394) 修复了一个罕见的竞争条件，导致动作指标因意外的异步回复而变得不一致。
- [#15647](https://github.com/emqx/emqx/pull/15647) 修复了如果连接器配置中指定的 MongoDB 帐户缺少对 `foo` 集合执行 `find` 查询的权限，MongoDB 连接器将被标记为 `Disconnected` 的问题。
- [#15603](https://github.com/emqx/emqx/pull/15603) 修复了 MQTT 桥接中的一个问题：过期的连接可能仍显示为 `Connected` 状态，且不会自动重连。
- [#15383](https://github.com/emqx/emqx/pull/15383) 修复了 MQTT 桥接中可能存在的资源泄漏问题。当桥接启动失败时，主题索引表未被正确清理。
- [#15786](https://github.com/emqx/emqx/pull/15786) 修复了探测 RocketMQ 连接器时可能存在的原子泄漏。
- [#15806](https://github.com/emqx/emqx/pull/15806) 改进了 Oracle 动作创建时的验证。以前，在极少数情况下，包含无效 SQL 语句的动作可能会被成功添加。
- [#15848](https://github.com/emqx/emqx/pull/15848) 改进了 Oracle 连接器的错误报告。当连接器断开连接时，其状态现在包含更具体的原因，使诊断更容易。

#### 智能数据中心

- [#15839](https://github.com/emqx/emqx/pull/15839) 修复了使用 `map<_, _>` 字段的 Protobuf 模式的编码问题。
  以前，包含 `map<string, string>` 字段的模式可能无法编码有效的有效负载，导致隐晦的运行时错误。
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
  无法编码的示例有效负载：
  ```json
  {
  "args": {
  "env": "stag"
  }
  }
  ```
  以前的错误类似于：
  ```
  2025-06-17T06:59:22.725785+00:00 [warning] tag: RULE_SQL_EXEC, clientid: c_emqx, msg: SELECT_clause_exception, reason: {error,{gpb_type_error,{bad_unicode_string,[{value,env},{path,"test.args.key"}]}},[{'$schema_parser_xxx',mk_type_error,3,[{file,"$schema_parser_xxx.erl"},{line,437}]},{'$schema_parser_xxx','-v_map<string,string>/3-lc$^0/1-0-',3,[{file,"$schema_parser_xxx.erl"},{line,429}]},{'$schema_parser_xxx','v_map<string,string>',3,[{file,"$schema_parser_xxx.erl"},{line,429}]},{'$schema_parser_xxx',v_msg_test,3,[{file,"$schema_parser_xxx.erl"},{line,404}]},{'$schema_parser_xxx',encode_msg,3,[{file,"$schema_parser_xxx.erl"},{line,73}]},{emqx_schema_registry_serde,with_serde,2,[{file,"emqx_schema_registry_serde.erl"},{line,212}]}...
  ```

#### 可观测性

- [#15931](https://github.com/emqx/emqx/pull/15931) 修复了与 EMQX 警报系统相关的两个问题：
  - 解决了一个在节点启动期间可能出现虚假但无害的错误日志的错误，例如：
    ```
    [error] Generic event handler emqx_alarm_handler crashed ...
    Reason: {aborted,{no_exists,[emqx_activated_alarm,runq_overload]}}
    ```
  - 修复了一个在某些条件下警报激活超时可能导致连接进程崩溃的错误。

#### 网关

- [#15342](https://github.com/emqx/emqx/pull/15342) 修复了 NATS 网关中的崩溃问题，该问题由客户端信息覆盖模板引用了未定义的报文字段引起。系统现在会返回空二进制而非未定义的原子值。

#### MQTT over QUIC

- [#15614](https://github.com/emqx/emqx/pull/15614) QUIC 监听器：当启用 TLS 密钥日志记录（`SSLKEYLOGFILE`）时，即使握手失败，EMQX 现在也会转储 TLS 密钥。

#### 集群连接

- [#15894](https://github.com/emqx/emqx/pull/15894) 以前，通过 `GET /cluster/links` 列出所有集群链接时，禁用的链接会以 `inconsistent` 状态返回。现在它们将以 `disconnected` 状态返回。

#### 性能

- [#15696](https://github.com/emqx/emqx/pull/15696) 为 WebSocket (WS) 和 WebSocket Secure (WSS) 监听器添加了连接速率限制支持。
  现在强制执行 `max_conn_rate` 和 `max_conn_burst` 配置选项：超过定义速率的传入连接在接受后会立即关闭，与现有的 TCP 监听器行为一致。
  此外，`max_connections` 的行为也已更新。当超过连接限制时，WS/WSS 监听器现在会在任何 HTTP 握手之前立即关闭连接，导致套接字突然关闭，而不是返回 HTTP 429 响应。
- [#15854](https://github.com/emqx/emqx/pull/15854) 将默认的 `active_n` 值从 `100` 减少到 `10`，以提高 MQTT 客户端的响应能力，特别是在高消息速率和小负载的情况下。
  较低的 `active_n` 在 TCP 层引入了更多的背压，比默认的 `Receive-Maximum` of `32` 更严格，这在以下情况下有所帮助：
  - 客户端进程被外部授权检查阻塞
  - 数据集成操作延迟了消息处理
  - 系统负载过重或接近资源限制
- [#15981](https://github.com/emqx/emqx/pull/15981) 防止了因 Mnesia 事务阻塞在清理大量审计日志时导致的内存过度增长。这提高了在繁重的审计日志维护操作期间的系统稳定性和内存效率。