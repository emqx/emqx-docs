# 全新功能

本页重点展示当前版本支持的主要新功能，不涵盖 EMQX 提供的全部功能。

## EMQX 6.1.0（最新版本）

### MQTT 消息流

MQTT 消息流为 EMQX 引入了一种可持久化、可回放的消息流模型，在保持 MQTT 发布/订阅语义不变的前提下，扩展了对历史消息存储与回放的能力。

与传统 MQTT 仅面向实时投递、依赖订阅者在线状态不同，消息流会持续捕获与主题过滤器匹配的消息并持久化存储。消费者可以通过带时间戳的订阅主题（`$s/<timestamp>/<topic_filter>`），从任意时间点开始回放历史消息，而无需关心消息最初的发布时间或自身是否在线。

消息流使 MQTT 原生支持历史消息回放、事件追溯以及基于消息的状态恢复，可直接用于时序数据处理、设备状态管理和事件驱动型应用，而无需引入额外的流式系统。

#### 功能亮点

- **持久化消息流**：按主题过滤器持续存储 MQTT 消息，并支持可配置的数据保留策略。
- **基于时间戳的回放**：消费者在订阅时指定回放起点，从对应时间开始读取历史消息。
- **常规消息流与最后值消息流**：既支持完整事件流，也支持在启用最后值语义时，按流键仅保留每个键的最新消息。
- **按键有序性保证**：具有相同流键的消息在存储与投递时保持严格顺序。
- **MQTT 原生扩展**：无需修改现有发布端或客户端，完全兼容现有 MQTT 使用方式。

MQTT 消息流使 EMQX 同时适用于实时消息传递与流式数据处理场景，降低系统架构复杂度，为物联网和事件驱动系统提供更统一的数据基础。

了解更多请参阅：[MQTT 消息流文档](../mqtt-stream/mqtt-stream-concept.md)。

### 消息队列

EMQX 6.0.0 引入了原生**消息队列**功能，将可靠的实时 MQTT 发布/订阅与异步消息队列功能集成在同一平台中，无需依赖任何外部队列服务。

与传统 MQTT 依赖订阅者在线不同，消息队列通过在服务器端缓存消息，实现发布者与订阅者的解耦。匹配特定主题过滤器（Topic Filter）的消息会被持久化存储，并可通过特殊的主题格式 `$q/{topic_filter}` 进行消费。这确保了即使客户端离线或网络不稳定，也能可靠地传递消息。

<img src="./assets/message_queque.png" alt="message_queque" style="zoom:50%;" />

该功能使 MQTT 同时支持实时与延迟消息处理，简化了物联网系统架构，无需再引入 Kafka 或 RabbitMQ 等外部队列系统。它特别适用于对消息持久性、可靠传递和离线缓存有严格要求的场景。

#### 功能亮点

- **融合的消息处理模型**：在单一系统中将轻量级 MQTT 与企业级消息队列相结合。
- **离线消息存储**：即使订阅者断开连接，消息也会被保留。
- **最后值保留**：可选启用“最后值语义”功能，启用后，EMQX 会基于指定的队列键（如设备 ID）保留每个键的最新消息。该特性非常适用于传感器读数等快速变化的数据场景，旧消息可被安全丢弃。
- **灵活的分发策略**：支持随机（Random）、轮询（Round Robin）和最少未完成订阅者（Least Inflight Subscriber）等分发方式，实现高效消息分配。
- **可靠消息投递**：支持持久化存储与 QoS 1 级消息投递，确保数据无丢失。

详细信息请参阅 [Message Queue 文档](../message-queue/message-queue-concept.md)。

### 命名空间角色的多租户支持

EMQX 6.0.0 引入了**命名空间角色**，以增强[多租户](../multi-tenancy/namespace-overview.md)和访问控制能力，特别适用于大规模物联网部署。

通过该功能，用户可被分配至特定命名空间，每个命名空间都是一个隔离的环境，用户仅能管理自己命名空间内的资源，如规则、连接器和动作，而不会访问或影响其他租户。

命名空间角色支持细粒度权限控制（如管理员、查看者），并可通过 Dashboard、API 或 CLI 进行管理，使管理员能够轻松分配职责，同时确保团队、部门或客户之间的安全隔离。

#### 功能亮点

- **安全隔离**：用户仅能访问其所属命名空间内的资源，例如 `ns:team_a::administrator`。
- **精细化访问控制**：命名空间用户可完全管理其命名空间资源，但集群级设置仅可查看，除非被授予全局管理员权限。
- **简化运维**：在创建用户时即可轻松创建并分配命名空间角色。
- **企业级可扩展性**：非常适合为多个租户提供 MQTT 服务的组织，或管理多个业务单元的企业环境。

#### 其他增强

- **改进可观测性**：Dashboard 视图现在会根据命名空间自动过滤，更加聚焦。
- **优化会话统计**：当连接数少于 1,000 时，会话计数实时刷新；否则每 5 秒刷新一次，以提高性能与准确性。

详细操作请参阅[创建具有命名空间角色的用户](../dashboard/system.md#创建具有命名空间角色的用户)。

### 持久化存储优化

EMQX 6.0.0 在持久化存储方面进行了显著优化，提升了性能与可扩展性，更好地支持高吞吐量的物联网工作负载。通过将会话数据与其他消息服务器的元数据解耦，EMQX 显著降低了内存使用量并提高了存储效率，从而在相同硬件条件下支持更多连接。

#### RocksDB 参数优化

新增的配置项允许用户更精细地控制内存与性能：

- `durable_storage.messages.rocksdb.write_buffer_size`：控制每个分片的 RocksDB memtable 大小。
- `durable_storage.messages.rocksdb.cache_size`：设置每个分片的 RocksDB 块大小。
- `durable_storage.messages.rocksdb.max_open_files`：限制每个分片 RocksDB 使用的文件描述符数量。
- `durable_storage.messages.layout.wildcard_thresholds`：允许为 `wildcard_optimized_v2` 存储布局调整通配符阈值。

#### 其他增强

- **高效序列化**：默认消息序列化格式改为 ASN.1，减小存储体积并提升处理速度。
- **更快访问与更低开销**：优化后的存储布局使消息检索更快，同时降低磁盘与内存开销。

这些改进使 EMQX 更加适用于需要稳定性能的大规模持久化 MQTT 负载场景。

### 扩展的数据集成支持

EMQX 6.0.0 进一步增强了数据集成功能，支持与各类主流云服务和高性能数据库生态系统无缝集成，实现实时分析、处理与存储。

#### 新增集成

以下数据集成在 EMQX 6.0.0 中全新支持：

- **[Google BigQuery](../data-integration/bigquery.md)**：将 MQTT 数据流式传输至 BigQuery，实现大规模数据仓储与高级查询，从海量物联网数据中获取洞察。
- **[AWS AlloyDB](../data-integration/alloydb.md)**、**[CockroachDB](../data-integration/cockroachdb.md)** 和 **[AWS Redshift](../data-integration/redshift.md)**：将 MQTT 数据流式发送至这些高性能分布式数据库，用于实时分析与可扩展存储，非常适合企业级物联网分析场景。

#### 增强的集成能力

除了新增集成外，EMQX 6.0.0 还对现有集成进行了性能、可用性与云原生兼容性优化：

- **Snowflake Snowpipe Streaming**：现支持通过 Snowpipe Streaming（Snowflake 的预览版功能）将数据低延迟写入 Snowflake 表，适用于 AWS 托管账户。
- **RocketMQ 动作**：新增 `key` 与 `tag` 模板字段，并提供 `key_dispatch` 消息分发策略，实现更灵活的消息路由与元数据管理。
- **AWS S3 和 AWS S3 Tables 连接器**：`access_key_id` 与 `secret_access_key` 参数现为可选项，EMQX 可自动通过 AWS EC2 Instance Metadata Service v2 获取凭证，实现无缝集成。
- **RabbitMQ Sink**：支持自定义消息 Headers 与 Properties 模板，提升消息在 RabbitMQ 中的消息路由能力和兼容性。

### 其他增强功能

#### 基于 LLM 的高级 MQTT 数据处理

EMQX 6.0.0 在基于大语言模型（LLM）的数据处理方面进一步增强，现已支持 [Google Gemini 模型](../flow-designer/gemini-node-quick-start.md)，并兼容 OpenAI 与 Anthropic Claude。

#### 增强的 LDAP 支持

LDAP 授权现支持 JSON 格式的扩展 ACL 规则，LDAP 认证还可直接从 LDAP 拉取 ACL 规则并支持客户端缓存。

#### 日志追踪功能改进

新增可配置项：
- `trace.max_traces`：限制最大追踪数量；
- `trace.max_file_size`：限制追踪文件大小，当达到最大值后自动轮转新文件，而非停止记录。

#### 集群管理

新增配置项 `cluster.description`，允许用户在 Dashboard 中为集群设置并显示自定义描述。详细步骤请参阅 [EMQX Dashboard -> 管理 -> 集群配置](../dashboard/cluster_settings.md#集群)。

### 更多功能

除了上述功能，EMQX 最近的版本还包含诸多增强和优化。完整详情请参阅：[版本更新日志](../changes/changes-ee-v6.md)。

### 不兼容变更

有关弃用项与不兼容变更的完整信息，请参阅 [EMQX 5.x 与 6.0 不兼容变更](../changes/breaking-changes-6.0.md)。

## EMQX 5.x 系列

有关 EMQX 5.x 的全新功能，请参阅 EMQX 企业版 v5 版本的[全新功能](https://docs.emqx.com/zh/emqx/v5.10/getting-started/new-features.html)。

