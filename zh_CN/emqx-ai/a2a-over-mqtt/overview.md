# A2A over MQTT

A2A over MQTT 是一种与 Broker 无关的传输规范，将 [Agent-to-Agent（A2A）协议](https://a2a-protocol.org/latest/)引入 MQTT。它定义了标准化的主题规范、MQTT v5 属性映射及 Agent 发现机制，使基于不同框架构建的 AI Agent 能够通过 MQTT Broker 完成注册、发现与协作，无需彼此之间建立点对点集成。

EMQX 通过内置的 **A2A Registry** 功能实现 A2A over MQTT。该功能负责收录发布到发现主题的 Agent Card，跟踪 Agent 的在线状态，并为运维人员提供管理界面。

::: warning 与大多数监听器挂载点不兼容
对于通过配置了[挂载点](../../configuration/listener.md#挂载点mountpoint)的监听器连接的 Agent，A2A Registry 不生效，除非挂载点恰好为一个主题层级（例如 `acme/`）——EMQX 会将其解析为 `$a2a` 主题的命名空间前缀。使用其他形式的挂载点时，挂载后的主题不再符合 `$a2a/v1/` 主题规范：Agent Card 会作为普通保留消息存储，不会被收录、校验或附加在线状态标注，且不会向客户端报告任何错误。
:::

## 为什么选择 MQTT 传输 A2A

标准 A2A 协议以 HTTP 作为传输层。这在云端环境中运行良好，但在分布式、IoT 或边缘部署场景下存在局限：Agent 可能运行在资源受限的设备上、处于 NAT 之后，或所处环境不适合维持持久 HTTP 连接。

MQTT 可以直接解决上述问题：

- **轻量高效**：协议开销极小，适合低带宽和不稳定网络环境。
- **Broker 中介路由**：Agent 无需知道彼此的地址，由 Broker 负责消息路由。
- **保留消息实现发现**：Agent 将 Agent Card 作为保留消息发布，任何订阅方无需专属注册服务即可立即获取。
- **原生在线状态检测**：MQTT 遗嘱消息（LWT）在 Agent 意外断开时自动发出下线通知。
- **统一鉴权**：EMQX 现有的认证与授权机制对所有 Agent 流量统一生效。

## 主要特性

- **Agent 注册与发现**：通过标准化 `$a2a/v1/discovery/` 主题上的保留消息实现。
- **Dashboard UI**：可交互地查看、注册和管理 Agent Card。
- **CLI 管理**：通过 `emqx ctl a2a-registry` 命令进行管理。
- **Schema 校验**：可选地在注册时对 A2A Agent Card 进行 Schema 合规校验。
- **Broker 托管在线状态**：EMQX 在转发消息时附加 `a2a-status` MQTT 用户属性，反映 Agent 的连接状态。
- **MQTT v5 请求/响应**：标准化的 `Response Topic` 和 `Correlation Data` 约定，用于 Agent 之间的任务委派。

## 延伸阅读

- [A2A over MQTT 工作原理](./architecture.md)：组件说明、主题模型与交互模式
- [管理 Agent](./manage-agents.md)：启用 Registry、注册 Agent，以及使用 Dashboard、CLI 和 MQTT 接口
- [A2A over MQTT 规范](https://www.emqx.com/mqtt-for-ai/a2a-over-mqtt/specification/0.1/basic/mqtt_transport.html)：完整的规范传输层定义
