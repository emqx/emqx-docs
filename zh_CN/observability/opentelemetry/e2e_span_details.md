# 端到端追踪 Span 详情

EMQX 提供基于 OpenTelemetry 标准的端到端追踪能力。这允许您监控 EMQX 集群内 MQTT 消息和客户端活动的全生命周期。本文档详细介绍了 EMQX 生成的各种 Span，以便深入了解 Broker 的内部工作原理。

## 客户端生命周期 Span

这些 Span 追踪 MQTT 客户端的主要生命周期事件。

- **`client.connect`**：这是一个追踪客户端连接过程的根 Span。它在客户端向 Broker 发起连接时开始，在连接建立或被拒绝时结束。

- **`client.disconnect`**：此 Span 追踪客户端断开连接的过程。它在客户端发送 `DISCONNECT` 报文或因其他原因（例如，网络错误、Keep Alive 超时）关闭连接时开始。

- **`client.subscribe`**：此 Span 追踪客户端的订阅请求。它涵盖了 Broker 接收 `SUBSCRIBE` 报文、处理订阅和发送 `SUBACK` 报文的整个过程。

- **`client.unsubscribe`**：此 Span 追踪客户端的取消订阅请求。它涵盖了从接收 `UNSUBSCRIBE` 报文到发送 `UNSUBACK` 报文的过程。

## 认证和授权 Span

这些 Span 提供了对客户端执行的安全相关检查的可见性。

- **`client.authn`**：追踪客户端的认证过程。此 Span 是 `client.connect` Span 的子 Span。

- **`client.authn_backend`**：追踪认证期间特定的后端调用（例如，查询数据库、调用 HTTP 服务）。这是 `client.authn` 的子 Span，有助于识别认证后端的性能瓶颈。

- **`client.authz`**：追踪客户端的授权过程，该过程在发布或订阅操作时发生。

- **`client.authz_backend`**：追踪授权期间特定的后端调用。这是 `client.authz` 的子 Span。

## 消息生命周期 Span

这些 Span 追踪 MQTT 消息在 Broker 中的传递过程。

### 消息入口 (客户端 -> Broker)

- **`client.publish`**：这是一个根 Span，追踪客户端发布到 Broker 的消息。它在 Broker 收到 `PUBLISH` 报文时开始。

- **`message.route`**：作为 `client.publish` 的子 Span，此 Span 追踪消息在 Broker 内部的路由过程，以查找匹配的订阅者。

- **`message.forward`**：如果消息需要传递到集群中另一个节点上的订阅者，此 Span 会追踪消息转发到该节点的过程。它是 `message.route` 的子 Span。

- **`message.handle_forward`**：在接收节点上，此 Span 追踪对已转发消息的处理。

### 消息出口 (Broker -> 客户端)

- **`broker.publish`**：此 Span 追踪 Broker 准备向订阅者发布消息的过程。它是 `message.route` 或 `message.handle_forward` 的子 Span。

### QoS 确认 Span

这些 Span 追踪 QoS 1 和 QoS 2 的确认流程。

#### Broker -> 发布者

- **`broker.puback`**：追踪 Broker向发布者发送 `PUBACK` 的过程 (QoS 1)。

- **`broker.pubrec`**：追踪 Broker向发布者发送 `PUBREC` 的过程 (QoS 2)。

- **`broker.pubcomp`**：追踪 Broker向发布者发送 `PUBCOMP` 的过程 (QoS 2)，从而完成来自发布者一方的 QoS 2 流程。

#### 发布者 -> Broker

- **`client.pubrel`**：追踪 Broker 从发布者处接收 `PUBREL` 的过程 (QoS 2)。

#### Broker -> 订阅者

- **`broker.pubrel`**：追踪 Broker 向订阅者发送 `PUBREL` 的过程 (QoS 2)。

#### 订阅者 -> Broker

- **`client.puback`**：追踪 Broker 从订阅者处接收 `PUBACK` 的过程 (QoS 1)。

- **`client.pubrec`**：追踪 Broker 从订阅者处接收 `PUBREC` 的过程 (QoS 2)。

- **`client.pubcomp`**：追踪 Broker 从订阅者处接收 `PUBCOMP` 的过程 (QoS 2)，从而完成来自订阅者一方的 QoS 2 流程。

## 规则引擎 Span

这些 Span 追踪规则引擎的执行过程。

- **`broker.rule_engine.apply`**：追踪规则引擎处理消息以查看其是否匹配任何规则的过程。这是 `message.route` Span 的子 Span。

- **`broker.rule_engine.action`**：如果规则匹配，则追踪规则引擎内特定动作的执行。这是 `broker.rule_engine.apply` 的子 Span。

## Broker 内部 Span

这些 Span 追踪 Broker 的内部操作。

- **`broker.disconnect`**：追踪 Broker 主动断开客户端连接的情况（例如，由于管理操作）。

- **`broker.subscribe`**：追踪由 Broker 自身发起的内部订阅过程（例如，由于管理操作）。

- **`broker.unsubscribe`**：追踪内部取消订阅的过程。

## 追踪采样和过滤

EMQX 的 OpenTelemetry 集成包含一个灵活的采样器，允许您控制生成哪些追踪。这有助于管理追踪数据的量，并专注于特定的客户端、主题或事件类型。采样决策基于以下层级结构：

1.  **追踪上下文来源 (`follow_traceparent`)**: 第一个决策是确定从何处获取初始追踪上下文。这由布尔开关 `follow_traceparent` 控制。
    -   如果为 `true`（默认值），EMQX 将尝试从传入的请求中提取追踪上下文（例如，从 MQTT 报文的 `traceparent` 用户属性中）。这允许您将源自上游已插桩应用程序的追踪链接起来。

    -   如果为 `false`，EMQX 将忽略任何传入的追踪上下文，并始终启动一个新的追踪。

2.  **远程采样决策**: 如果 `follow_traceparent` 为 `true` 并且传入的请求包含一个已被标记为“已采样”的追踪上下文，EMQX 将尊重此上游决策并对该追踪进行采样，除非被其他规则覆盖。

3.  **白名单规则**: 如果追踪未被远程父级采样，您可以定义特定规则来强制对某些客户端或主题进行采样。这是确保您追踪所关心活动的最直接方法。
    -   **按 ClientID**: 如果客户端的 ID 在 ClientID 白名单中，其所有根级别的活动（连接、订阅、发布等）都将被追踪。
    -   **按 Topic**: 如果消息发布到的主题与主题白名单中的过滤器匹配，则该消息的追踪将被采样。
        > **注意：** 此规则适用于追踪的开始（例如 `client.publish` span）。它不适用于负责向订阅者传递消息的 `broker.publish` span。

4.  **基于比例的采样**: 如果没有匹配的白名单规则，则决策将回退到基于比例的采样，由 `sample_ratio` 配置控制。
    -   `sample_ratio`：您可以配置此比例（从 `0.0` 到 `1.0`）来控制捕获的追踪百分比。值为 `1.0` 意味着将捕获 100% 的追踪，而 `0.0` 意味着不会捕获任何追踪（除非它们匹配白名单规则）。

5.  **事件类型开关**: 即使追踪被基于比例的采样器选中，也只有在通过其配置开关启用了相应的事件类型时，才会生成该追踪。这些开关充当各类 Span 的全局开关。可用的开关有：
    -   `client_connect_disconnect`：一个布尔开关，用于启用或禁用客户端连接和断开连接事件的追踪。

    -   `client_subscribe_unsubscribe`：一个布尔开关，用于启用或禁用客户端订阅和取消订阅事件的追踪。

    -   `client_messaging`：一个布尔开关，用于启用或禁用客户端消息发布事件的追踪。

    -   `trace_rule_engine`：一个布尔开关，用于启用或禁用规则引擎的追踪。

6.  **消息追踪级别**: 对于与 QoS 确认相关的 Span（例如 `PUBACK`、`PUBREC`），您可以使用 `msg_trace_level` 开关根据 QoS 级别控制它们的创建。
    -   `msg_trace_level`：此设置可以配置为特定的 QoS 级别（0、1 或 2），以根据原始消息的 QoS 控制创建哪些确认 Span。例如，如果 `msg_trace_level` 设置为 `1`，将为 QoS 1 消息创建 `PUBACK` Span。对于 QoS 2 消息，此设置将生成 `PUBREC` Span，但不会生成 `PUBREL` 或 `PUBCOMP` Span。这有助于减少高 QoS 消息流的追踪详细程度。
