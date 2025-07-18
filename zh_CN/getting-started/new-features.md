# 全新功能

本页重点展示当前版本支持的主要新功能，不涵盖 EMQX 提供的全部功能。

## 集群连接

集群连接可以实现地理分散的 EMQX 集群之间安全、高效、透明的消息共享。与传统 MQTT 桥接需转发全部消息并使用主题前缀防止循环相比，集群连接仅传输有订阅需求的消息，节省带宽、降低延迟、提升扩展性。

集群连接的配置与管理操作简便，可通过 EMQX Dashboard、配置文件或 REST API 创建、修改和监控连接，且提供实时状态指示和链路统计信息。

开始使用集群连接，请参阅： [集群连接快速开始](../cluster-linking/quick-start.md)。

<img src="./assets/cluster_linking_feature.png" alt="cluster_linking_feature" style="zoom:80%;" />

## 命名空间

命名空间允许在单一 EMQX 集群内实现逻辑级别的多租户隔离，每个租户可拥有独立的客户端、话题、配额和配置。每个命名空间通过 `tns` 客户属性标识，可从用户名或 TLS SNI 等元数据提取，实现连接级别的租户识别。

命名空间支持两种创建方式：

- **手动创建**：管理员通过 Dashboard 或 REST API 定义。
- **动态创建**：EMQX 根据连接元数据自动生成。

当前版本支持命名空间级别的速率限制配置，用于分配资源并控制使用率。更多细节请参阅：[命名空间](../multi-tenancy/namespace-overview.md)。

## 数据智能中心

[数据智能中心](../data-integration/smart-data-hub.md)提供统一的方案，用于在 MQTT 数据流中进行基于 schema 的消息校验与转换。它通过关键组件简化了结构化、可靠的数据流开发。

### Schema Registry

Schema Registry 现已支持内部格式（如 JSON、Avro 和 Protobuf）以及通过 HTTP 服务接入的外部 Schema。对于未被原生支持的数据格式，EMQX 可通过 `schema_encode` 和 `schema_decode` 函数将 Schema 编解码操作委托给[外部 HTTP 服务](../data-integration/schema-registry-example-external-http.md)。

### Schema 验证

Schema 验证可确保只有符合预定义格式的消息可以被处理或投递。EMQX 支持使用 JSON Schema、Protobuf、Avro 以及规则引擎的 SQL 语法进行格式校验。根据校验结果，用户可配置消息丢弃、断开客户端连接或触发规则引擎等动作。

### 消息转换

消息转换功能允许用户定义消息转换管道，在消息投递或进一步处理前进行解码、修改和重新编码。系统支持嵌套转换、多个编码器/解码器，并可通过 [Variform 表达式](../configuration/configuration.md#variform-表达式)动态赋值字段内容。

## 大语言模型驱动的 MQTT 数据处理

EMQX 5.10.0 在 Flow 设计器中支持[基于 LLM 的 MQTT 数据处理](../flow-designer/llm-based-data-processing.md)。该功能集成了 OpenAI 的 GPT、Anthropic 的 Claude 等模型，能够通过自然语言提示词处理 MQTT 消息。处理节点会通过可复用的补全配置调用 AI 模型，并返回结果用于后续的转发或存储等操作。该功能非常适合构建智能、具备上下文感知能力的低吞吐量工作流。

## 数据集成扩展

EMQX 的最新版本显著增强了数据集成能力。不仅新增多个 Sink 和服务的支持，还引入了备选动作机制，用于提升物联网数据实时处理的可靠性。

新增的数据集成类型包括但不限于：

- **[Amazon S3 Tables](../data-integration/s3-tables.md)**：将 MQTT 数据转换为 Iceberg 表格式，并直接写入 S3 存储。无需传统数据库，同时保留类似 SQL 的查询能力。
- **[Apache Doris](../data-integration/apache-doris.md)**：处理 MQTT 消息并映射为结构化数据，通过 HTTP 或 JDBC 写入 Doris 数据库。支持使用标准 SQL 实时查询物联网数据，并可通过 Grafana 等 BI 工具构建实时仪表盘。
- **[Snowflake](../data-integration/snowflake.md)**：将处理后的数据写入 Snowflake Stage 并加载至 Snowflake 表中。可安全地进行长期归档，并利用其数据仓库与分析能力实现实时或批量分析。

### 备选动作

从 EMQX 5.9.0 起，EMQX 引入了备选动作功能，用于在数据投递失败时自动执行备用方案。当主动作因投递错误、缓冲区溢出或请求超时等原因失败时，系统可自动触发一个或多个备选动作。

该功能有助于最大限度减少数据丢失、提升系统可靠性，并增强可观测性。了解更多详情，请参阅[备选动作](../data-integration/data-bridges.md#备选动作)。

## 安全性增强

EMQX 的多个新版本对访问控制功能进行了显著改进，确保系统在满足企业级安全标准的同时，仍具备良好的灵活性和易用性。这些增强功能有助于保护数据完整性、支持合规要求，并防止复杂物联网环境中的未授权访问。

EMQX 现已支持更多类型的身份认证与访问控制机制，提供更灵活且精细化的访问控制能力。新增支持的功能包括：

- **[认证器调用条件](../access-control/authn/authn.md#认证器调用条件)**：支持基于客户端元数据设置认证器的触发条件。
- **[使用 LDAP 进行密码认证](../access-control/authn/ldap.md)**：通过外部 LDAP 目录验证用户，支持企业级用户管理。
- **[基于 REST API 的 MQTT 5.0 SCRAM 认证](../access-control/authn/scram_restapi.md)**：支持符合 MQTT 5.0 标准的 SCRAM 认证方式，基于 RESTful API 实现。
- **[Kerberos 认证](../access-control/authn/kerberos.md)**：集成 Kerberos 单点登录系统，实现安全的集中式用户认证。
- **[Client-Info 认证](../access-control/authn/cinfo.md)**：基于客户端元信息（如 IP 地址、设备 ID 或用户名）实现灵活的访问控制。

在 EMQX Enterprise 5.9.0 中，还引入了一套更强大的安全功能，以保护您的部署环境：

- **[多因素认证](../multi-factor-authn/multi-factor-authentication.md)**：在用户名密码之外增加验证步骤，提升登录安全性。
- **[账户锁定与解锁](../dashboard/introduction.md#账户锁定与解锁)**：多次登录失败后自动禁用账户，支持手动或定时解锁。
- **[密码过期](../dashboard/introduction.md#密码过期)**：强制执行密码轮换策略，降低长期凭据泄露风险，满足组织安全合规要求。

## OpenTelemetry 集成：指标、日志与追踪

EMQX 现已全面支持 OpenTelemetry，使用户可以更轻松地监控和排查 MQTT 系统中的问题。

**主要功能：**

- **指标监控**：将实时指标数据导出至 OpenTelemetry Collector，并可通过 Prometheus、Grafana 等工具进行可视化。
- **日志采集**：发送结构化日志，包含丰富上下文信息（如 Trace ID），方便调试与故障定位。
- **追踪**：支持在 EMQX 节点间对 MQTT 消息流进行分布式追踪，有助于发现延迟、路由问题或特定节点的性能瓶颈。
- **端到端追踪模式**：可完整追踪消息路径和客户端操作，支持按客户端 ID、主题或 QoS 等维度过滤，用户可自定义采样率与导出频率以平衡系统负载。

借助 OpenTelemetry，您可以通过开放的标准工具全面掌握 EMQX 的性能与消息流动情况。详细信息请参阅 [OpenTelemetry 集成](../observability/opentelemetry/opentelemetry.md)。

## NATS 协议网关

EMQX 5.10.0 引入了原生的 NATS 协议网关，实现了 NATS 与 MQTT 之间的双向消息通信。该功能允许 NATS 客户端直接连接到 EMQX，并通过主题与 subject 的映射与 MQTT 客户端交换消息。

**主要特性：**

- **全面支持 NATS 协议**：支持核心消息类型，如 PUB、SUB、PING 以及请求/响应模式。
- **MQTT 互操作性**：可将 NATS 的 subject 转换为 MQTT 主题，支持通配符和共享订阅。
- **灵活的部署方式**：可通过 Dashboard、REST API 或配置文件启用。
- **多种传输协议支持**：支持 TCP、TLS、WebSocket 和加密 WebSocket（WSS）。
- **身份验证机制**：支持多种认证后端，包括数据库、HTTP、JWT 和 LDAP。

借助该网关，EMQX 能够桥接使用 NATS 的现代云原生环境，拓展混合消息系统的集成场景。详见 [NATS 协议网关](../gateway/nats.md)。

## 更多功能

除了上述功能，EMQX 最近的版本还包含诸多增强和优化。完整详情请参阅：[版本更新日志](../changes/changes-ee-v5.md)。
