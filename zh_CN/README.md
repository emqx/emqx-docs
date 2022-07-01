# 产品概览 (5.0 文档正在建设中)

**EMQX** 是一款基于 Erlang/OTP 平台开发的，大规模可弹性伸缩的云原生分布式物联网 MQTT 消息服务器，提供高效可靠连接海量物联网设备，高性能实时处理消息与事件流数据，助力构建关键业务的物联网平台与应用。

Erlang/OTP 是出色的软实时 (Soft-Realtime)、低延时 (Low-Latency)、分布式 (Distributed)的语言平台。

MQTT 是轻量的 (Lightweight)、发布订阅模式 (PubSub) 的物联网消息协议。

## 设计目标

1. **连接任何设备**：通过开放标准物联网协议 MQTT、CoAP 和 LwM2M 连接任何设备。支持来自开源社区的所有 MQTT 客户端，如 Eclipse Paho 或定制的 MQTT 库。
2. **支持任意规模**：单服务器节点支持 200 万并发连接，通过集群扩展，可以毫不费力地支持 1 亿并发连接。
3. **通信安全**：通过 TLS/SSL 和多样化的认证机制确保与 MQTT 服务器进行安全通信，包括用户名/密码、JWT、PSK 和 X.509 证书。

4. **实时数据处理**： 通过内置数据集成提供的丰富 SQL 查询进行低代码事件处理，以数百万条/秒的速度实时处理设备与云端之间双向移动的 MQTT 消息数据。
5. **轻松管理与监控**：通过 CLI、HTTP API 和一个优雅的 Dashboard 轻松管理 EMQX 集群。支持使用 Datadog、StatsD、Prometheus 和 Granfana 进行监控和报警。
6. **灵活扩展与定制**：通过网关与插件对 EMQX 集群进行扩展与定制，快速实现与云服务、企业系统的数据集成以及专有的物联网协议。
7. **Run Anywhere**：采用基于 Kubernetes 的云原生架构。可运行在私有云、混合云和公有云（如华为云、腾讯云、阿里云和 AWS）。

## 功能概览

以下是 EMQX 主要功能列表。

### 连接

- 完整支持 MQTT v3.1、v3.1.1 and v5.0 协议规范
  - QoS 0, QoS 1, QoS 2 消息支持
  - 持久会话和离线消息支持
  - 保留消息（Retained Message）支持
  - 遗嘱消息（Will Message）支持
  - 共享订阅支持
  - `$SYS/` 系统主题支持
- MQTT 支持 4 种传输协议
  - TCP
  - TLS
  - WebSocket
  - QUIC（实验性）
- HTTP 消息发布接口
- 网关
  - CoAP
  - LwM2M
  - MQTT-SN
  - Stomp
  - GB/T 32960（企业版）
  - JT/T 808（企业版）

更多的扩展支持：

- 延迟发布
- 代理订阅
- 主题重写

### 安全

- 基于用户名/密码的身份认证，支持使用内置数据库、Redis、MySQL、PostgreSQL、MongoDB 作为数据源，也支持使用 HTTP Server 提供认证服务。
- 基于 JWT 的身份认证与权限控制，支持 JWKs
- MQTT 5.0 增强认证
- PSK 身份验证
- 基于 Client ID, IP 地址，用户名的访问控制，支持使用内置数据库、Redis、MySQL、PostgreSQL、MongoDB 作为数据源，也支持使用 HTTP Server 提供授权服务
- 客户端黑名单支持

### 可伸缩性

- 多节点集群 (Cluster)
- 支持手动, mcast, dns, etcd, k8s 集群发现方式集群
- 多服务器节点桥接 (Bridge)

### 数据集成


- 内置数据集成，通过 SQL 语法实时提取、过滤、丰富和转换消息或内部事件并将其传输到外部数据平台
- 通过 MQTT 与其他 Broker 或物联网平台进行双向数据桥接（如 EMQX Cloud，AWS IoT Core，Azure IoT Hub）
- 通过 WebHook 与其他应用集成
{% emqxee %}
- 企业版: TODO
{% endemqxee %}

### 可靠性

- 过载保护
- 消息速率限制
- 连接速率限制

### 可观测性

- 客户端在线状态查询
- 集群状态与指标查询
- Prometheus/StatsD 集成
- 自动网络分区恢复
- 在线日志追踪(Log Trace)
- Erlang 运行时追踪工具

### 可扩展性

- 插件
- 钩子
- gRPC 钩子扩展
- gRPC 协议扩展
