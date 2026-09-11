# 全新功能

本页重点展示当前版本支持的主要新功能，不涵盖 EMQX 提供的全部功能。

## EMQX 6.x 系列

### 安全配置方案

EMQX 6.3 引入了节点级安全配置方案，可在兼容早期版本默认行为与采用更严格的安全默认行为之间进行选择。

#### 功能亮点

- **兼容原有行为**：默认的 `legacy` 方案保留早期 EMQX 版本的默认行为。
- **强化安全默认值**：`hardened` 方案针对监听器暴露范围、认证、授权、延迟发布、扩展功能和 Dashboard 访问采用更严格的默认行为。
- **检查集群一致性**：查看每个节点启用的方案，并检测集群内的方案差异。

通过 `EMQX_SECURITY_PROFILE` 在启动时选择方案。集群内所有节点应使用相同方案；更改后需重启节点才能生效。详细信息请参阅[安全配置方案](../guides/access-control/security-profile.md)。

### 功能门控

EMQX 6.3 引入了部署阶段的功能门控，可仅启动部署所需的可选功能。

#### 功能亮点

- **预设或自定义选择**：通过 `EMQX_FEATURES` 使用 `FULL`、`ESSENTIAL` 或明确指定功能列表。
- **自动启用依赖**：EMQX 会自动启用所选功能依赖的其他功能。
- **降低资源占用**：`ESSENTIAL` 仅启动 MQTT Broker 和访问控制等核心能力，不加载可选功能应用。
- **保持部署一致**：功能集在启动时确定，集群内所有节点应使用相同设置。

详细信息请参阅[功能门控](./deploy/feature-gates.md)。

### 可观测性增强

EMQX 6.3 增强了对 MQTT 流量、客户端会话和外部可观测性平台的运行状态监测。

#### 功能亮点

- **主题指标 v2**：支持创建带名称的指标集合，并使用通配符主题过滤器、命名空间隔离、REST API 管理和 Prometheus 导出。
- **会话缓冲区监测**：统计缓冲消息的载荷字节数，在会话超过配置阈值时发出警告，并通过 `emqx ctl session-top` 识别占用最大的会话。
- **Dynatrace 集成**：通过 OAuth2 令牌认证，将 OpenTelemetry 追踪和日志导出到 Dynatrace。

详细信息请参阅[主题指标](../guides/observability/topic-metrics.md)、[生产环境监控最佳实践](../guides/observability/monitoring-best-practices.md)和 [Dynatrace 集成](../guides/observability/opentelemetry/dynatrace.md)。

### 订阅过滤器

EMQX 6.2 为 MQTT 5.0 客户端引入了订阅过滤器。订阅者在主题过滤器的 `?` 后添加过滤条件。发布者无需在主题中包含该条件，只需使用普通主题名称，并通过 MQTT 5.0 User Properties 携带待判断的值。只有消息主题和过滤条件均匹配时，EMQX 才会投递消息。

例如，订阅 `sensor/+/temperature?location=roomA&value>25` 可匹配发布到 `sensor/1/temperature`，且 User Properties 为 `location=roomA` 和 `value=26` 的消息。

#### 功能亮点

- **仅投递符合条件的消息**：先匹配消息主题，再根据 User Properties 判断过滤条件。
- **灵活的过滤表达式**：支持等值和数值比较运算符，并可使用 `&` 组合多个条件。
- **降低网络与客户端负载**：避免向客户端投递不符合订阅者过滤条件的消息。
- **可选启用并保持向后兼容**：通过 `mqtt.subscription_message_filter` 控制该功能。该功能默认关闭；关闭时，EMQX 将 `?` 作为主题过滤器的一部分处理。
- **内置可观测性**：过滤条件不匹配时，EMQX 会触发原因为 `subscription_filter` 的 `delivery.dropped` 事件，并通过 `delivery.dropped.filter` 指标进行统计。

详细信息请参阅[订阅过滤器文档](../develop/subscription-filter/subscription-filter-concept.md)。

### MQTT 消息流

MQTT 消息流为 EMQX 引入了一种可持久化、可回放的消息流模型，在保持 MQTT 发布/订阅语义不变的前提下，扩展了对历史消息存储与回放的能力。

与传统 MQTT 仅面向实时投递、依赖订阅者在线状态不同，消息流会持续捕获与主题过滤器匹配的消息，并将其写入一个命名的持久化消息流中。每个消息流通过唯一的名称进行标识和管理，而不是通过主题过滤器进行标识。

消费者通过订阅 `$stream/<name>` 或 `$stream/<name>/<topic_filter>` 来消费消息流数据。回放起点通过 MQTT 5 的订阅属性 `stream-offset` 指定，可选择从指定时间戳、`earliest` 或 `latest` 位置开始回放历史消息，而无需关心消息最初的发布时间或自身是否在线。

这一设计使 EMQX 原生支持历史消息回放、事件追溯以及基于消息的状态恢复，而无需引入外部流式处理系统。

#### 功能亮点

- **具名持久化消息流**：将 MQTT 消息持久化到显式命名的消息流中，并支持可配置的数据保留策略。
- **基于 Offset 的回放**：消费者通过 `stream-offset` 订阅属性控制回放起点。
- **常规消息流与最后值消息流**：既支持完整事件流，也支持在启用最后值语义时，按流键仅保留每个键的最新消息。
- **按键有序性保证**：具有相同流键的消息在存储与投递时保持严格顺序。
- **MQTT 原生扩展**：无需修改现有发布端或客户端，完全兼容现有 MQTT 使用方式。

MQTT 消息流使 EMQX 同时适用于实时消息传递与流式数据处理场景，降低系统架构复杂度，为物联网和事件驱动系统提供更统一的数据基础。

了解更多请参阅：[MQTT 消息流文档](../develop/mqtt-stream/mqtt-stream-concept.md)。

### 消息队列

EMQX 6.0.0 引入了原生消息队列功能，将可靠的实时 MQTT 发布/订阅与异步消息队列功能集成在同一平台中，无需依赖任何外部队列服务。

与传统 MQTT 依赖订阅者在线不同，消息队列通过在服务器端缓存消息，实现发布者与订阅者的解耦。匹配配置主题过滤器的消息会被持久化存储到一个命名队列中。每个队列通过唯一名称进行标识和管理，而不是通过主题过滤器进行标识。

客户端通过以下格式订阅队列进行消费：

```
$queue/<name>
$queue/<name>/<topic_filter>
```

其中 `<name>` 为队列名称。

> 当启用消息队列功能后，`$queue/` 前缀将被保留用于消息队列订阅，不能再用于共享订阅。

![message_queue_routing_overview](./assets/message_queue_routing_overview.png)

该功能使 MQTT 同时支持实时与延迟消息处理，简化了物联网系统架构，无需再引入 Kafka 或 RabbitMQ 等外部队列系统。它特别适用于对消息持久性、可靠传递和离线缓存有严格要求的场景。

#### 功能亮点

- **具名持久化队列**：消息被存储到显式命名的队列中，并支持可配置的队列行为。
- **发布与订阅解耦**：通过服务器端缓冲实现生产者与消费者解耦。
- **离线消息存储**：即使订阅者断开连接，消息也会被保留。
- **最后值保留**：可选启用“最后值语义”功能，启用后，EMQX 会基于指定的队列键（如设备 ID）保留每个键的最新消息。该特性非常适用于传感器读数等快速变化的数据场景，旧消息可被安全丢弃。
- **灵活的分发策略**：支持随机（Random）、轮询（Round Robin）和最少未完成订阅者（Least Inflight Subscriber）等分发方式，实现高效消息分配。
- **可靠消息投递**：支持持久化存储与 QoS 1 级消息投递，确保数据无丢失。

详细信息请参阅 [Message Queue 文档](../develop/message-queue/message-queue-concept.md)。

### 命名空间角色的多租户支持

EMQX 6.0.0 引入了**命名空间角色**，以增强[多租户](../guides/multi-tenancy/namespace-overview.md)和访问控制能力，特别适用于大规模物联网部署。

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

详细操作请参阅[创建具有命名空间角色的用户](../guides/dashboard/system.md#创建具有命名空间角色的用户)。

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

EMQX 6.x 持续增强数据集成功能，支持将 MQTT 数据接入云服务和数据库生态系统，用于实时分析、处理与存储。

#### 新增集成

EMQX 6.x 系列新增了以下数据集成：

- **[Google Bigtable](../develop/data-integration/bigtable.md)（6.3.0）**：将 MQTT 数据追加写入 Bigtable，用于可扩展、低延迟的物联网运行数据存储与检索。
- **[Google BigQuery](../develop/data-integration/bigquery.md)（6.0.0）**：将 MQTT 数据流式传输至 BigQuery，实现大规模数据仓储与高级查询，从海量物联网数据中获取洞察。
- **[AWS AlloyDB](../develop/data-integration/alloydb.md)（6.0.0）**、**[CockroachDB](../develop/data-integration/cockroachdb.md)（6.0.0）** 和 **[AWS Redshift](../develop/data-integration/redshift.md)（6.0.0）**：将 MQTT 数据流式发送至这些高性能分布式数据库，用于实时分析与可扩展存储，非常适合企业级物联网分析场景。

#### 增强的集成能力

除了新增集成外，EMQX 6.0.0 还对现有集成进行了性能、可用性与云原生兼容性优化：

- **Snowflake Snowpipe Streaming**：现支持通过 Snowpipe Streaming（Snowflake 的预览版功能）将数据低延迟写入 Snowflake 表，适用于 AWS 托管账户。
- **RocketMQ 动作**：新增 `key` 与 `tag` 模板字段，并提供 `key_dispatch` 消息分发策略，实现更灵活的消息路由与元数据管理。
- **AWS S3 和 AWS S3 Tables 连接器**：`access_key_id` 与 `secret_access_key` 参数现为可选项，EMQX 可自动通过 AWS EC2 Instance Metadata Service v2 获取凭证，实现无缝集成。
- **RabbitMQ Sink**：支持自定义消息 Headers 与 Properties 模板，提升消息在 RabbitMQ 中的消息路由能力和兼容性。

### 其他增强功能

#### 基于 LLM 的高级 MQTT 数据处理

EMQX 6.0.0 在基于大语言模型（LLM）的数据处理方面进一步增强，现已支持 [Google Gemini 模型](../develop/flow-designer/gemini-node-quick-start.md)，并兼容 OpenAI 与 Anthropic Claude。

#### 增强的 LDAP 支持

LDAP 授权现支持 JSON 格式的扩展 ACL 规则，LDAP 认证还可直接从 LDAP 拉取 ACL 规则并支持客户端缓存。

#### 日志追踪功能改进

新增可配置项：
- `trace.max_traces`：限制最大追踪数量；
- `trace.max_file_size`：限制追踪文件大小，当达到最大值后自动轮转新文件，而非停止记录。

#### 集群管理

新增配置项 `cluster.description`，允许用户在 Dashboard 中为集群设置并显示自定义描述。详细步骤请参阅 [EMQX Dashboard -> 管理 -> 集群配置](../guides/dashboard/cluster_settings.md#集群)。

### 更多功能

除了上述功能，EMQX 最近的版本还包含诸多增强和优化。完整详情请参阅：[版本更新日志](../release-notes/changes-ee-v6.md)。

### 不兼容变更

有关 EMQX 6.3 引入的不兼容变更，请参阅 [EMQX 6.3 中的不兼容变更](../release-notes/breaking-changes-6.3.md)。有关从 EMQX 5.x 迁移到 6.0 的不兼容变更，请参阅 [从 EMQX 5.x 到 EMQX 6.0 的不兼容变更](../release-notes/breaking-changes-6.0.0.md)。

## EMQX 5.x 系列

有关 EMQX 5.x 的全新功能，请参阅 EMQX 企业版 v5 版本的[全新功能](https://docs.emqx.com/zh/emqx/v5.10/getting-started/new-features.html)。
