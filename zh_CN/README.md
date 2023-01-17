# 产品概览

[EMQX](https://www.emqx.com/zh/products/emqx) 是一款大规模可弹性伸缩的云原生分布式物联网 [MQTT](https://mqtt.org/) 消息服务器。

作为全球最具扩展性的 MQTT 消息服务器，EMQX 提供了高效可靠海量物联网设备连接，能够高性能实时移动与处理消息和事件流数据，帮助您快速构建关键业务的物联网平台与应用。

## 产品优势

- **[开放源码](https://www.emqx.io/zh)**：基于 Apache 2.0 许可证完全开源，自 2013 年起 200+ 开源版本迭代。
- **[MQTT 5.0](https://www.emqx.com/zh/blog/introduction-to-mqtt-5)**：100% 支持 MQTT 5.0 和 3.x 协议标准，更好的伸缩性、安全性和可靠性。
- **[海量连接](https://www.emqx.com/zh/blog/reaching-100m-mqtt-connections-with-emqx-5-0)**：单节点支持 500 万 MQTT 设备连接，集群可扩展至 1 亿并发 MQTT 连接。
- **高性能**：单节点支持每秒实时接收、移动、处理与分发数百万条的 MQTT 消息。
- **低时延**：基于 Erlang/OTP 软实时的运行时系统设计，消息分发与投递时延低于 1 毫秒。
- **高可用**：采用 Masterless 的大规模分布式集群架构，实现系统高可用和水平扩展。

## 功能概览

以下是 EMQX 不完全功能列表。

### 连接

- 完整支持 MQTT v3.1、v3.1.1 和 v5.0 协议规范
  - [QoS 0、QoS 1、QoS 2 消息支持](./mqtt/mqtt-qos.md)
  - [持久会话](./mqtt/mqtt-session-and-message-expiry.md#mqtt-会话d)和离线消息支持
  - [保留消息（Retained Message）支持](./mqtt/mqtt-retained-messages.md)
  - [遗嘱消息（Will Message）支持](./mqtt/mqtt-last-will-and-testament.md)
  - [共享订阅支持](./mqtt/mqtt-shared-subscription.md)
  - [`$SYS/` 系统主题支持](./mqtt/mqtt-system-topics.md)
- MQTT 支持 4 种传输协议
  - TCP
  - [TLS](./network/emqx-mqtt-tls)
  - [WebSocket](./messaging/mqtt-publish-and-subscribe.md)
  - [QUIC（实验性）](./mqtt-over-quic/introduction.md)
- HTTP 消息发布接口
- 网关
  - [CoAP](./gateway/coap.md)
  - LwM2M
  - [MQTT-SN](./gateway/mqttsn.md)
  - [Stomp](./gateway/stomp.md)
  - GB/T 32960（企业版）
  - JT/T 808（企业版）

更多 MQTT 扩展支持：

- [延迟发布](./mqtt/mqtt-delayed-publish.md)
- 代理订阅
- [主题重写](./mqtt/mqtt-topic-rewrite.md)

### 安全

- 基于用户名/密码的身份认证，支持使用[内置数据库](./access-control/authn/mnesia.md)、[Redis](./access-control/authn/redis.md)、[MySQL](./access-control/authn/mysql.md)、[PostgreSQL](./access-control/authn/postgresql.md)、[MongoDB](./access-control/authn/mongodb.md) 作为数据源，也支持使用 [HTTP Server](./access-control/authn/http.md) 提供认证服务
- 基于 [JWT](./access-control/authn/jwt.md) 的身份认证与权限控制，支持 JWKs
- [MQTT 5.0 增强认证](./access-control/authn/scram.md)
- PSK 身份验证
- 基于 Client ID、IP 地址，用户名的访问控制，支持使用[内置数据库](./access-control/authz/mnesia.md)、[Redis](./access-control/authz/redis.md)、[MySQL](./access-control/authz/mysql.md)、[PostgreSQL](./access-control/authz/postgresql.md)、[MongoDB](./access-control/authz/mongodb.md)作为数据源，也支持使用  [HTTP Server](./access-control/authz/http.md)  提供授权服务
- [客户端黑名单支持](./access-control/blacklist.md)

### 可伸缩性

- [多节点集群 (Cluster)](./deploy/cluster/introduction.md)
- 支持[手动](./deploy/cluster/manual.md)、[自动（dns、etcd、k8s）](./deploy/cluster/auto.md)等发现方式集群
- 多服务器节点桥接 (Bridge)

### 数据集成

- [SQL 语法数据集成](./data-integration/rules.md)，实时提取、过滤、丰富和转换 MQTT 消息或内部事件为用户所需格式，并将其发送到外部数据平台
- 通过 MQTT 与其他 Broker 或物联网平台进行双向数据桥接（如 [EMQX Cloud](https://www.emqx.com/zh/cloud)，AWS IoT Core，Azure IoT Hub）
- 通过 WebHook 与其他应用集成

### 可靠性

- 过载保护
- [消息速率限制](./rate-limit/rate-limit.md)
- [连接速率限制]((./rate-limit/rate-limit.md))

### 可观测性

- 客户端在线状态查询
- 集群状态与指标查询
- [Prometheus](./observability/prometheus.md)/[StatsD](./observability/statsd.md) 集成
- 自动网络分区恢复
- [在线日志追踪(Log Trace)](./observability/tracer.md)
- Erlang 运行时追踪工具

### 可扩展性

- [插件](./extensions/plugins.md)
- [钩子](./extensions/hooks.md)
- [gRPC 钩子扩展](./extensions/exhook.md)
- gRPC 协议扩展
