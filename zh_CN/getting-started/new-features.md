# 全新功能

本页重点展示当前版本支持的主要新功能，未覆盖 EMQX 提供的全部功能。

## 集群连接

集群连接可以实现地理分散的 EMQX 集群之间安全、高效、透明的消息共享。与传统 MQTT 桥接需转发全部消息并使用主题前缀防止循环相比，集群连接仅传输有订阅需求的消息，节省带宽、降低延迟、提升扩展性。

集群连接的配置与管理操作简便，可通过 EMQX Dashboard、配置文件或 REST API 创建、修改和监控连接，且提供实时状态指示和链路统计信息。

开始使用集群连接，请参阅： [集群连接快速开始](../cluster-linking/quick-start.md)。

## Schema 验证

[Schema 验证](../data-integration/schema-validation.md)功能可确保仅符合预定义格式的消息被处理或投递。EMQX 支持使用 JSON Schema、Protobuf、Avro 及规则引擎 SQL 语法进行格式校验。针对校验失败的消息，用户可以配置自动丢弃、断开客户端连接或触发规则引擎等动作。

## 消息转换

[消息转换](../data-integration/message-transformation.md)允许用户定义消息转换管道，在投递前对消息进行解码、修改和重新编码。系统支持嵌套转换、多个编码器/解码器，并可通过 [Variform 表达式](../configuration/configuration.md#variform-表达式)实现动态字段赋值。

## 数据集成功能扩展

近期版本的 EMQX 显著增强了数据集成功能，新增了多种数据集成类型，包括但不限于：

- **[Snowflake](../data-integration/snowflake.md)**：将处理后的数据写入 Snowflake Stage 并加载至 Snowflake 表中。可安全地进行长期归档，并利用其数据仓库与分析能力实现实时或批量分析。
- **[Azure Blob Storage](../data-integration/azure-blob-storage.md)**：与 Microsoft Azure 的可扩展对象存储服务集成，支持结构化与非结构化数据的持久化存储，适用于大规模 IoT 数据归档，功能类似于 AWS S3。
- **[Datalayers](../data-integration/data-bridge-datalayers.md)**：Datalayers 是面向工业物联网、车联网与能源行业的边云协同分布式数据库平台。通过与 EMQX 的集成，用户可将实时数据写入 Datalayers，实现时序存储、键值缓存及边缘计算分析。

## 安全性增强

EMQX 在近期版本中新增了多种认证与授权机制，提供更灵活、细粒度的访问控制能力。新增支持的功能包括：

- **[使用 LDAP 进行密码认证](../access-control/authn/ldap.md)**：通过外部 LDAP 目录验证用户，支持企业级用户管理。
- **[基于 REST API 的 MQTT 5.0 SCRAM 认证](../access-control/authn/scram_restapi.md)**：支持符合 MQTT 5.0 标准的 SCRAM 认证方式，基于 RESTful API 实现。
- **[Kerberos 认证](../access-control/authn/kerberos.md)**：集成 Kerberos 单点登录系统，实现安全的集中式用户认证。
- **[Client-Info 认证](../access-control/authn/cinfo.md)**：基于客户端元信息（如 IP 地址、设备 ID 或用户名）实现灵活的访问控制。

## OpenTelemetry 集成：指标、日志与追踪

EMQX 现已全面支持 OpenTelemetry，使用户可以更轻松地监控和排查 MQTT 系统中的问题。

**主要功能：**

- **指标监控**：将实时指标数据导出至 OpenTelemetry Collector，并可通过 Prometheus、Grafana 等工具进行可视化。
- **日志采集**：发送结构化日志，包含丰富上下文信息（如 Trace ID），方便调试与故障定位。
- **追踪**：支持在 EMQX 节点间对 MQTT 消息流进行分布式追踪，有助于发现延迟、路由问题或特定节点的性能瓶颈。
- **端到端追踪模式**：可完整追踪消息路径和客户端操作，支持按客户端 ID、主题或 QoS 等维度过滤，用户可自定义采样率与导出频率以平衡系统负载。

借助 OpenTelemetry，您可以通过开放的标准工具全面掌握 EMQX 的性能与消息流动情况。详细信息请参阅 [OpenTelemetry 集成](../observability/opentelemetry/opentelemetry.md)。

## 新增协议网关

EMQX 在 5.2 至 5.8 版本中新增了多种行业协议网关，支持交通、能源与电动汽车等垂直领域的系统接入，帮助用户将行业标准协议的数据快速接入基于 MQTT 的平台。

- **[OCPP 协议网关](../gateway/ocpp.md)**：支持 OCPP 1.6 协议，EMQX 可直接连接电动汽车充电桩，将 OCPP 消息转换为 MQTT 消息，实现统一通信管理。
- **[JT/T 808 协议网关](../gateway/jt808.md)**：支持 JT/T 808 车载终端通信协议，EMQX 可接收来自 GPS 终端与车载设备的二进制消息，并以十六进制编码形式通过 MQTT 发布供下游系统解析。
- **[GB/T 32960 协议网关](../gateway/gbt32960.md)**：基于 GB/T 32960 协议，使 EMQX 能够接收、解码并通过 MQTT 转发来自新能源汽车的结构化诊断与遥测数据。

这些网关简化了行业协议到 MQTT 的接入流程，广泛应用于智能充电、车队监控与新能源汽车数据上报等场景。

## Dashboard 优化

自 5.8 版本起，EMQX Dashboard 引入了更直观、功能更强的管理界面，提升了日常运维与监控体验。

**易用性提升**

- 为规则引擎的动作 与 Source 页面新增分页、搜索与状态筛选功能，方便大规模集成管理。
- 增加“一键重置集群指标”功能，便于快速诊断并观察集群状态变化。

**指标展示增强**

- 对 `/api/v5/monitor` 接口进行优化，使用并发 RPC 拉取全集群指标，避免在大规模部署下出现超时。
- 在首页新增核心指标（如消息速率），方便快速获取关键数据。

**监控工具集成**

- 提供 Webhook 集成功能，用于发送告警事件，便于实现自动化监控与异常响应。参见 [Webhook 告警集成配置](../observability/alarms.md#通过-webhook-集成发送告警事件通知)。

## 更多功能

除了上述功能，EMQX 最近的版本还包含诸多增强和优化。完整详情请参阅：[版本更新日志](https://github.com/emqx/emqx-docs/blob/6f1b5b885bb9a82475d567433a0b477a0ef34d0b/zh_CN/changes/changes-ee-v5.md)。
