# 功能对比

本页详细列出了不同部署模式所支持的具体功能对比。

## 核心 / 企业功能

| 功能                   | 自托管 (企业版)                   | 云服务 (Serverless) | 云服务 (专有版) | 备注和链接                                                   |
| ---------------------- | --------------------------------- | ------------------- | --------------- | ------------------------------------------------------------ |
| **MQTT 5.0 Broker**    | ✅                                 | ✅                   | ✅               | 完整实现 MQTT 5.0 协议                                       |
| **MQTT over QUIC**     | ✅                                 | ✅                   | ✅               | 全球领先支持                                                 |
| **MQTT 扩展**          | ✅                                 | ❌                   | ✅               | [共享订阅](../messaging/mqtt-shared-subscription.md)<br>[排他订阅](../messaging/mqtt-exclusive-subscription.md)<br>[延迟发布](../messaging/mqtt-delayed-publish.md)<br>[自动订阅](../messaging/mqtt-auto-subscription.md)<br>[主题重写](../messaging/mqtt-topic-rewrite.md)<br>更多个性化选项 |
| **多协议网关**         | ✅                                 | ❌                   | ✅               | 更多行业设备接入                                             |
| **多租户**             | ✅                                 | ❌                   | ✅               | 更高的系统灵活性和利用率                                     |
| **集群连接**           | ✅                                 | ❌                   | ✅               | 设备和应用数据的无缝连接                                     |
| **消息队列**           | ✅                                 | ❌                   | ✅               | 数据传输和分析的统一架构                                     |
| **流处理**             | ✅                                 | ❌                   | ✅               | 更高的可靠性和灾难恢复能力<br>（即将发布）                   |
| **数据持久化**         | ✅ 内置 RocksDB 数据库或外部数据库 | N/A                 | N/A             | [提高稳定性和可靠性](../durability/durability_introduction.md) |
| **Schema Registry**    | ✅                                 | ❌                   | ✅               | [编解码](../data-integration/schema-registry.md)保证数据一致性和可兼容性 |
| **消息编解码**         | ✅                                 | ❌                   | ✅               | 灵活的消息格式转换：JSON / Avro / Protobuf / HTTP / gRPC     |
| **消息验证**           | ✅                                 | ❌                   | ✅               | 确保消息的完整性和合法性                                     |
| **规则引擎**           | ✅                                 | ✅                   | ✅               | [基于 SQL 的内置规则引擎](../data-integration/rules.md)      |
| **Flow 设计器**        | ✅                                 | ❌                   | ✅               | [更简便的数据集成编排](../flow-designer/introduction.md)     |
| **文件传输**           | ✅                                 | ❌                   | ✅ 已规划        | 统一平台数据传输                                             |
| **Kafka 集成**         | ✅                                 | ✅                   | ✅               | [将 MQTT 数据传输到 Apache Kafka](../data-integration/data-bridge-kafka.md) |
| **企业级数据集成**     | ✅ 40+                             | ✅                   | ✅ 40+           | [提升业务开发和发布速度](https://www.emqx.com/zh/integrations) |
| **故障排查**           | ✅                                 | ❌                   | ✅               | [日志追踪 (Trace)](../observability/tracer.md)<br>[慢订阅统计](../observability/slow-subscribers-statistics.md) |
| **Cloud-Native & K8s** | ✅                                 | N/A                 | N/A             | [降低系统部署和管理成本](../deploy/kubernetes/kubernetes.md) |
| **边缘计算**           | ✅                                 | ✅                   | ✅               | 降低数据传输延迟和成本<br>[Neuron](https://www.emqx.com/zh/products/neuronex)<br>[NanoMQ](https://www.emqx.com/zh/products/nanomq) |

## 可扩展性与性能

| 指标                   | 自托管 (企业版)                                      | 云服务 (Serverless)         | 云服务 (专有版)                  | 备注与链接                                                   |
| ---------------------- | ---------------------------------------------------- | --------------------------- | -------------------------------- | ------------------------------------------------------------ |
| **可扩展性**           | 最多支持 100 节点集群<br>每集群可支持 1 亿 MQTT 连接 | 自动扩展，最多 1,000 条连接 | 无限制                           | [高度可扩展，EMQX 5.0 达成 1 亿 MQTT 连接](https://www.emqx.com/zh/blog/reaching-100m-mqtt-connections-with-emqx-5-0) |
| **高可用性**           | 核心-副本集群                                        | 无主集群                    | 无主集群                         |                                                              |
| **可靠性**             | 基于 RocksDB 的消息持久化，具备高可用副本            | 会话持久化                  | 会话持久化                       | [基于 RocksDB 实现高可靠、低时延的 MQTT 数据持久化](https://www.emqx.com/zh/blog/mqtt-persistence-based-on-rocksdb) |
| **性能**               | 每秒处理 500 万条以上 MQTT 消息                      | 每秒 1000 条 MQTT 消息      | 每秒 500 万条以上 MQTT 消息      |                                                              |
| **延迟**               | 1~5 毫秒                                             | 1~5 毫秒                    | 1~5 毫秒                         |                                                              |
| **服务等级协议 (SLA)** | N/A                                                  | 99.9% 正常运行时间          | 最高可达 99.99%<br/>正常运行时间 |                                                              |

## 集群架构

| 功能项                       | 自托管 (企业版) | 云服务 (Serverless) | 云服务 (专有版) | 备注与链接                                                   |
| ---------------------------- | --------------- | ------------------- | --------------- | ------------------------------------------------------------ |
| **集群节点数量**             | 100+            | 保密信息            | 保密信息        | 支持大规模集群部署                                           |
| **运行时弹性伸缩和韧性扩展** | ✅               | ❌                   | ✅               | 提升系统稳定性与资源利用率                                   |
| **自动扩展**                 | ✅               | ✅                   | ✅               |                                                              |
| **高一致性**                 | ✅               | ✅                   | ✅               |                                                              |
| **事务处理**                 | ✅               | ✅                   | ✅               | 确保数据操作的原子性与可靠性                                 |
| **网络分区恢复**             | ✅               | ✅                   | ✅               | 集群故障自动修复                                             |
| **节点迁移与集群重平衡**     | ✅               | N/A                 | N/A             | 支持不停机维护集群                                           |
| **自动集群发现**             | ✅               | N/A                 | N/A             | 支持静态节点列表发现、UDP 组播、DNS、etcd、Kubernetes Service 等多种发现方式 |
| **零停机/热升级**            | ✅               | N/A                 | N/A             | 及时修复系统漏洞                                             |
| **热补丁**                   | ✅               | N/A                 | N/A             | 确保系统稳定运行                                             |
| **过载保护**                 | ✅               | N/A                 | N/A             | 提升系统管理效率                                             |
| **多集群管理**               | ✅               | N/A                 | N/A             | 提高系统稳定性                                               |
| **集群指标**                 | ✅               | N/A                 | N/A             |                                                              |

## MQTT 与其他连接协议支持

| MQTT 与连接能力         | 自托管 (企业版) | 云服务 (Serverless) | 云服务 (专有版) | 备注与链接                                            |
| ----------------------- | --------------- | ------------------- | --------------- | ----------------------------------------------------- |
| **MQTT 3.x**            | ✅               | ✅                   | ✅               |                                                       |
| **MQTT 5.0**            | ✅               | ✅                   | ✅               |                                                       |
| **MQTT 保留消息**       | ✅               | ✅                   | ✅               |                                                       |
| **MQTT over TCP**       | ✅               | ❌                   | ✅               |                                                       |
| **MQTT over TLS**       | ✅               | ✅                   | ✅               |                                                       |
| **MQTT over WebSocket** | ✅               | ✅                   | ✅               |                                                       |
| **MQTT over QUIC**      | ✅               | ❌                   | ✅               | EMQX 是现在全球唯一支持 QUIC 协议的 MQTT 消息服务器。 |
| **LB（代理协议）**      | ✅               | ❌                   | ✅               | Proxy protocol v1, v2                                 |
| **LB (Custom)**         | ✅               | ❌                   | ✅               | GmSSL<br>平滑连接迁移                                 |
| **IPv6 支持**           | ✅               | ❌                   | ✅               |                                                       |
| **多协议网关**          | ✅               | ❌                   | ✅               |                                                       |
| **MQTT-SN**             | ✅               | ❌                   | ✅               |                                                       |
| **STOMP**               | ✅               | ❌                   | ❌               |                                                       |
| **CoAP**                | ✅               | ❌                   | ✅               |                                                       |
| **LwM2M**               | ✅               | ❌                   | ❌               |                                                       |
| **ExProto**             | ✅               | ❌                   | ❌               |                                                       |
| **OCPP**                | ✅               | ❌                   | ❌               |                                                       |
| **JT/808**              | ✅               | ❌                   | ✅               |                                                       |
| **GB/T 32960**          | ✅               | ❌                   | ❌               |                                                       |

## 安全

| 安全性                                           | 自托管 (企业版) | 云服务 (Serverless) | 云服务 (专有版) | 备注与链接                                                   |
| ------------------------------------------------ | --------------- | ------------------- | --------------- | ------------------------------------------------------------ |
| **TLS/SSL**                                      | ✅               | ✅                   | ✅               | 保护数据传输安全：TLS 1.1, 1.2, 1.3                          |
| **QUIC**                                         | ✅               | ❌                   | ✅               | 提升弱网及移动网络数据传输效率                               |
| **OCSP Stapling**                                | ✅               | ❌                   | ✅               | 提供更灵活的安全实践                                         |
| **连接抖动**                                     | ✅               | ✅                   | ✅ 规划中        | 检测并拦截频繁上下线的连接                                   |
| **审计日志**                                     | ✅               | ✅                   | ✅               | 支持重要操作的审计追踪                                       |
| **Dashboard 单点登录（SSO）**                    | ✅               | ✅                   | ✅               | [安全简便的认证流程](../dashboard/sso.md) |
| **Dashboard/REST API 基于角色的访问控制 (RBAC)** | ✅               | ✅                   | ✅               | 最小化权限以确保系统安全                                     |

## 认证与授权

| 认证与授权            | 自托管 (企业版) | 云服务 (Serverless)     | 云服务 (专有版) | 备注与链接                                                   |
| --------------------- | --------------- | ----------------------- | --------------- | ------------------------------------------------------------ |
| **用户名/密码**       | ✅               | ✅                       | ✅               | [密码认证](../access-control/authn/pwoverview.md) |
| **JWT**               | ✅               | ❌                       | ✅               | [JWT 认证](../access-control/authn/jwt.md) |
| **MQTT 5.0 增强认证** | ✅               | N/A                     | N/A             | [MQTT 5.0 增强认证](../access-control/authn/scram.md) |
| **LDAP**              | ✅               | ❌                       | ✅ 已规划        |                                                              |
| **PSK 验证**          | ✅               | ❌                       | ✅               | [启用 PSK 认证](../network/psk-authentication.md#enable-psk-authentication) |
| **X.509 证书**        | ✅               | ✅（由 EMQX Cloud 管理） | ✅               |                                                              |
| **细粒度访问控制**    | ✅               | ✅                       | ✅               |                                                              |
| **认证数据源**        | ✅               | ❌                       | ✅               |                                                              |
| **ACL 数据源**        | ✅               | ❌                       | ✅               |                                                              |

## 数据集成

随着 EMQX 的演进，其支持的数据集成范围不断扩展，包括但不限于下表所列内容。

| 数据集成                     | 自托管 (企业版) | 云服务 (Serverless) | 云服务 (专有版) |
| ---------------------------- | --------------- | ------------------- | --------------- |
| **MQTT 服务**                | ✅               | ❌                   | ✅               |
| **Webhook / HTTP Server**    | ✅               | ✅                   | ✅               |
| **Aliyun Tablestore**        | ✅               | ✅                   | ✅               |
| **Apache Kafka / Confluent** | ✅               | ✅                   | ✅               |
| **Apache IoTDB**             | ✅               | ❌                   | ✅               |
| **Apache Pulsar**            | ✅               | ❌                   | ✅               |
| **AWS Kinesis**              | ✅               | ❌                   | ✅               |
| **AWS S3**                   | ✅               | ❌                   | ✅               |
| **Azure Event Hubs**         | ✅               | ❌                   | ✅               |
| **Azure Blob Storage**       | ✅               | ❌                   | ✅               |
| **Cassandra**                | ✅               | ❌                   | ✅               |
| **ClickHouse**               | ✅               | ❌                   | ✅               |
| **Couchbase**                | ✅               | ❌                   | ✅               |
| **DynamoDB**                 | ✅               | ❌                   | ✅               |
| **Elasticsearch**            | ✅               | ❌                   | ✅               |
| **GCP PubSub**               | ✅               | ❌                   | ✅               |
| **GreptimeDB**               | ✅               | ❌                   | ✅               |
| **HStreamDB**                | ✅               | ❌                   | ✅               |
| **InfluxDB**                 | ✅               | ❌                   | ✅               |
| **Microsoft SQL Server**     | ✅               | ❌                   | ✅               |
| **MongoDB**                  | ✅               | ❌                   | ✅               |
| **MySQL**                    | ✅               | ❌                   | ✅               |
| **OpenTSDB**                 | ✅               | ❌                   | ✅               |
| **Oracle Database**          | ✅               | ❌                   | ✅               |
| **PostgreSQL**               | ✅               | ❌                   | ✅               |
| **RabbitMQ**                 | ✅               | ❌                   | ✅               |
| **Redis**                    | ✅               | ❌                   | ✅               |
| **RocketMQ**                 | ✅               | ❌                   | ✅               |
| **Snowflake**                | ✅               | ❌                   | ✅               |
| **Syskeeper**                | ✅               | ❌                   | ✅               |
| **TDengine**                 | ✅               | ❌                   | ✅               |
| **TimeScaleDB**              | ✅               | ❌                   | ✅               |

## 规则引擎

| 规则引擎              | 自托管 (企业版) | 云服务 (Serverless) | 云服务 (专有版) | 备注与链接                                                   |
| --------------------- | --------------- | ------------------- | --------------- | ------------------------------------------------------------ |
| **编解码**            | ✅               | ❌                   | ✅               | 确保消息格式一致性                                           |
| **JSON 编解码**       | ✅               | ❌                   | ✅               |                                                              |
| **Avro 编解码**       | ✅               | ❌                   | ✅               |                                                              |
| **ProtoBuf 编解码**   | ✅               | ❌                   | ✅               |                                                              |
| **Sparkplug B Codec** | ✅               | ❌                   | ✅               |                                                              |
| **JSON Schema 验证**  | ✅               | ❌                   | ✅               |                                                              |
| **Avro 消息验证**     | ✅               | ❌                   | ✅               |                                                              |
| **ProtoBuf 消息验证** | ✅               | ❌                   | ✅               |                                                              |
| **内置 SQL 函数**     | ✅               | ❌                   | ✅               | [丰富的内置 SQL 函数，支持自定义扩展](../data-integration/rule-sql-builtin-functions.md) |
| **jq 函数**           | ✅               | ❌                   | ✅               | 高效的 JSON 数据处理                                         |
| **客户端事件处理**    | ✅               | ❌                   | ✅               | [客户端事件](../data-integration/rule-sql-events-and-fields.md#客户端事件)，事件驱动的业务开发 |
| **Schema 验证**       | ✅               | ❌                   | ✅               |                                                              |
| **消息转换**          | ✅               | ❌                   | ✅               |                                                              |

## 可扩展性

| 可扩展性         | 自托管 (企业版) | 云服务 (Serverless) | 云服务 (专有版) | 备注与链接                                                   |
| ---------------- | --------------- | ------------------- | --------------- | ------------------------------------------------------------ |
| **钩子**         | ✅               | N/A                 | N/A             | [钩子](../extensions/hooks.md) |
| **插件**         | ✅               | N/A                 | N/A             | [插件](../extensions/plugins.md) |
| **插件热加载**   | ✅               | N/A                 | N/A             |                                                              |
| **插件热配置**   | ✅               | N/A                 | N/A             |                                                              |
| **网关**         | ✅               | N/A                 | N/A             |                                                              |
| **ExHooks/gRPC** | ✅               | N/A                 | N/A             |                                                              |

## 可操作性

| 可操作性       | 自托管 (企业版) | 云服务 (Serverless) | 云服务 (专有版) | 备注与链接                                                   |
| -------------- | --------------- | ------------------- | --------------- | ------------------------------------------------------------ |
| **Dashboard**  | ✅               | ✅                   | ✅               | EMQX Dashboard 具备丰富的功能。可通过 Dashboard 进行配置的热更新。 |
| **功能配置**   | ✅ HOCON         | N/A                 | N/A             | 简洁明了的 HOCON 格式。                                      |
| **HTTP API**   | ✅               | ✅                   | ✅               |                                                              |
| **CLI**        | ✅               | ✅                   | ✅               |                                                              |
| **配置热升级** | ✅               | N/A                 | N/A             |                                                              |
| **运行审计**   | ✅               | ✅                   | ✅               |                                                              |

## 可观测性

| 可观测性          | 自托管 (企业版) | 云服务 (Serverless) | 云服务 (专有版) | 备注与链接                          |
| ----------------- | --------------- | ------------------- | --------------- | ----------------------------------- |
| **Dashboard**     | ✅               | ✅                   | ✅               | 通过优雅的 Dashboard 实时监控集群。 |
| **单节点指标**    | ✅               | ✅                   | ✅               |                                     |
| **Grafana**       | ✅               | ❌                   | ✅               |                                     |
| **Prometheus**    | ✅               | ❌                   | ✅               |                                     |
| **Datadog**       | ✅               | ❌                   | ✅               |                                     |
| **OpenTelemetry** | ✅               | ❌                   | ✅               |                                     |
| **集群指标**      | ✅               | N/A                 | N/A             |                                     |
| **告警**          | ✅               | ✅                   | ✅               |                                     |
| **慢订阅监控**    | ✅               | ❌                   | ✅               |                                     |
| **主题监控**      | ✅               | ❌                   | ✅               |                                     |
| **客户端监控**    | ✅               | ✅                   | ✅               |                                     |
| **日志追踪**      | ✅               | ✅                   | ✅               |                                     |

## 云原生与 K8S

| 云原生与 K8S            | 自托管 (企业版) | 云服务 (Serverless) | 云服务 (专有版) | 备注与链接                                                   |
| ----------------------- | --------------- | ------------------- | --------------- | ------------------------------------------------------------ |
| **Docker**              | ✅               | N/A                 | N/A             | [Docker Hub](https://hub.docker.com/r/emqx/emqx-enterprise) |
| **Kubernetes Operator** | ✅               | N/A                 | N/A             | [EMQX Kubernetes Operator](https://www.emqx.com/zh/emqx-kubernetes-operator) |

## 云平台支持

| 云平台                | 自托管 (企业版) | 云服务 (Serverless) | 云服务 (专有版) | 备注和链接                                                   |
| --------------------- | --------------- | ------------------- | --------------- | ------------------------------------------------------------ |
| **AWS Marketplace**   | ✅               | ✅                   | ✅               | [AWS Marketplace: EMQX Platform (Pay as you go)](https://aws.amazon.com/marketplace/pp/prodview-g6zejrbcad6mu) |
| **Azure Marketplace** | ✅               | ✅                   | ✅               | [Azure Marketplace: EMQX Platform](https://marketplace.microsoft.com/en-us/product/saas/emqtechnologiesincorporated1678779968155.emqx_cloud?tab=Overview) |
| **GCP Marketplace**   | ✅               | ✅                   | ✅               | [GCP Marketplace](https://console.cloud.google.com/marketplace/product/emq-public-380104/emqx-cloudpay-as-you-go) |
| **阿里云**            | ✅               | ✅                   | ✅               |                                                              |
| **华为云**            | ✅               | ✅ 规划中            | ✅               |                                                              |
| **腾讯云**            | ✅               | ✅ 规划中            | ✅               |                                                              |

## MQTT 开发工具与 SDKs

| MQTT 开发工具 & SDKs  | 自托管 (企业版) | 云服务 (Serverless) | 云服务 (专有版) | 备注和链接                                                   |
| --------------------- | --------------- | ------------------- | --------------- | ------------------------------------------------------------ |
| **MQTTX 桌面版**      | ✅               | ✅                   | ✅               | MQTTX - 学习 MQTT 最好的工具。<br>[MQTTX: 你的全功能 MQTT 客户端工具](https://mqttx.app/zh) |
| **MQTTX 命令行版**    | ✅               | ✅                   | ✅               | [MQTTX CLI: 强大易用的 MQTT 5.0 命令行工具](https://mqttx.app/zh/cli) |
| **MQTTX Web 版**      | ✅               | ✅                   | ✅               | 功能丰富并且简单易用<br>[MQTTX Web: 易用的 MQTT 5.0 Websocket 客户端工具](https://mqttx.app/zh/web) |
| **MQTT 基准测试工具** | ✅               | ✅                   | ✅               | [GitHub - emqx/emqtt-bench: 用 Erlang 编写的轻量级 MQTT 基准测试工具。](https://github.com/emqx/emqtt-bench) |
| **MQTT & JMeter**     | ✅ JMeter 插件   | ✅                   | ✅               | [GitHub - emqx/mqtt-jmeter: MQTT JMeter 插件](https://github.com/emqx/mqtt-jmeter) |
| **MQTT SDK for C**    | ✅ NanoSDK       | ✅                   | ✅               | [GitHub - nanomq/NanoSDK: NanoSDK - 支持 QUIC 的 MQTT 5.0 兼容 SDK，采用 NNG 风格。](https://github.com/nanomq/NanoSDK) |
| **MQTT Erlang SDK**   | ✅               | ✅                   | ✅               | [GitHub - emqx/emqtt: Erlang MQTT 5.0 客户端](https://github.com/emqx/emqtt) |
| **MQTT iOS SDK**      | ✅               | ✅                   | ✅               | [GitHub - emqx/CocoaMQTT：为 iOS 和 macOS 编写的 MQTT 5.0 客户端库，使用 Swift 语言。](https://github.com/emqx/CocoaMQTT)<br />[GitHub - emqx/swift-mqtt：支持 TCP 和 QUIC 协议的 MQTT 客户端](https://github.com/emqx/swift-mqtt) |
| **MQTT QUIC 客户端**  | ✅               | ✅                   | ✅               | [GitHub - emqx/quic: 用于 Erlang 和 Elixir 的 QUIC 协议。](https://github.com/emqx/quic) |

## 客户支持服务

| 客户支持     | 自托管 (企业版)    | 云服务 (Serverless) | 云服务 (专有版)    | 备注和链接 |
| ------------ | ------------------ | ------------------- | ------------------ | ---------- |
| **技术支持** | 5×8、7×24 全球支持 | 5×8 全球支持        | 5×8、7×24 全球支持 | 社区和论坛 |
| **架构咨询** | ✅                  | ❌                   | ✅                  |            |
| **项目集成** | ✅                  | ❌                   | ❌                  |            |
| **定制开发** | ✅                  | ❌                   | ❌                  |            |
