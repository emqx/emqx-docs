# 订阅过滤器

EMQX 6.2 引入的订阅过滤器（Subscription Filter）功能在 MQTT 5.0 发布/订阅模型的基础上，增加了订阅层面的内容过滤能力。客户端可以只接收同时满足主题过滤器和过滤表达式的消息，从而减少不必要的消息投递，降低网络流量和资源消耗。

本页面对 EMQX 中的订阅过滤器功能进行完整介绍，涵盖设计动机、核心概念、过滤表达式语法、行为语义及实际应用场景。

## 什么是订阅过滤器？

订阅过滤器是附加在 MQTT 订阅上的可选过滤条件。当已发布的消息匹配订阅的主题过滤器时，EMQX 会在转发前根据消息的 MQTT 5.0 用户属性（User Properties）对过滤表达式进行求值。只有同时满足主题过滤器和过滤表达式的消息，才会被投递给订阅者。

标准 MQTT 消息路由将所有匹配主题的消息转发给订阅者：

```
Publisher --> Topic -- (Filter) --> Subscription --> Subscriber
```

订阅过滤器在消息路由路径中引入了第二级过滤：

```
Publisher --> Topic -- (Filter) --> Subscription -- (Filter) --> Subscriber
```

这一双层过滤机制同时支持基于主题和基于内容的过滤，使客户端能够精确指定希望接收的消息。

## 为什么使用订阅过滤器？

标准 MQTT 5.0 订阅仅根据主题匹配来路由消息，所有发布到匹配主题的消息都会被投递给每个订阅者，与消息内容无关。在以下场景中，这种方式存在局限性：

- 订阅者只关注来自特定区域、设备组或类别的消息。
- 高频主题承载着不同消费者需要独立拆分的混合数据。
- 将所有消息下发给客户端会造成不必要的网络流量和处理负担。

订阅过滤器通过允许在订阅时声明精确的、基于内容的投递规则来解决上述问题，无需修改发布者、调整主题结构或为每个数据维度创建独立主题。

## 核心概念

- **主题过滤器（Topic Filter）**：订阅中的标准 MQTT 主题过滤器部分（`?` 之前的内容），决定哪些消息进入路由阶段。

- **过滤表达式（Filter Expression）**：基于内容的过滤条件（`?` 之后的内容），对通过主题过滤器的每条消息的 MQTT 5.0 用户属性进行求值。

- **用户属性（User Properties）**：附加在 MQTT 5.0 消息上的键值对元数据。发布者可通过用户属性提供额外上下文信息（如 `location`、`device_type`、`region` 等），供订阅者进行过滤。

- **双阶段投递（Two-Stage Delivery）**：在消息投递给订阅者之前，依次对主题过滤器和过滤表达式进行求值的组合过程。

- **无过滤订阅（Unfiltered Subscription）**：不含 `?` 分隔符的订阅，与标准 MQTT 订阅行为一致，所有匹配主题的消息均会被投递，不受内容限制。

## 订阅过滤器的工作原理

订阅过滤器以 MQTT 5.0 **用户属性**作为过滤依据。当客户端发布消息时，可在消息的用户属性头部包含键值对。EMQX 对每个过滤表达式与这些键值对进行匹配，只有表达式满足时才投递消息。

订阅过滤器默认禁用。如需启用该功能，请参阅[订阅过滤器快速入门](./subscription-filter-get-started.md)。

### 过滤语法

订阅过滤器通过 `?` 作为分隔符附加在主题过滤器之后：

```
<topic-filter>?<filter-expression>
```

| 组成部分 | 说明 |
|---|---|
| `<topic-filter>` | 标准 MQTT 主题过滤器，例如 `sensor/+/temperature` 或 `home/#` |
| `?` | 分隔主题过滤器与过滤表达式的分隔符 |
| `<filter-expression>` | 针对消息用户属性进行求值的键值过滤条件 |

### 过滤表达式格式

过滤表达式支持相等匹配和比较运算符，多个条件通过 `&` 组合（逻辑与）：

```
key1=value1&key2>value2
```

| 元素 | 说明 |
|---|---|
| `key` | 已发布消息中用户属性的键名 |
| `=` | 相等匹配（键的值必须等于指定字符串） |
| `>` | 数值比较（键的值必须大于指定数字） |
| `&` | 组合多个条件，所有条件均为真时消息才会被投递 |

过滤表达式**区分大小写**。若消息的用户属性中不存在指定的键，该消息将被过滤掉。

::: tip

订阅过滤器仅适用于 MQTT 5.0 客户端。当该功能启用时，使用含 `?` 主题字符串进行订阅的 MQTT 3.1.1 客户端，其 `?` 及后续内容将被视为主题过滤器的字面字符。

:::

## 行为语义

- 只有当主题过滤器匹配**且**过滤表达式求值为真时，EMQX 才会将消息投递给订阅者。
- 若过滤表达式引用的键在消息用户属性中不存在，该消息**不会**被投递给该订阅者。
- 每个订阅的过滤表达式独立求值，一条消息是否投递给某个订阅者，不影响其是否被投递给同主题上的其他订阅者。
- 不含 `?` 分隔符的订阅与标准 MQTT 订阅行为完全一致。
- 过滤表达式在服务端求值，客户端无需承担任何过滤逻辑。

## 过滤表达式示例

以下示例展示了常见的订阅模式：

| 订阅字符串 | 含义 |
|---|---|
| `sensor/+/temperature?location=roomA` | 接收用户属性中包含 `location=roomA` 的温度消息。 |
| `sensor/+/temperature?value>25` | 接收用户属性中 `value` 大于 25 的温度消息。 |
| `sensor/+/temperature?location=roomA&unit=celsius` | 接收同时满足 `location=roomA` 和 `unit=celsius` 的温度消息。 |
| `home/lights/#` | 标准订阅，投递所有匹配主题的消息。 |

### 发布者

发布者向 `sensor/1/temperature` 发布一条消息，用户属性如下：

```json
{
  "location": "roomA",
  "unit": "celsius"
}
```

### 订阅者

| 订阅 | 是否投递？ | 原因 |
|---|---|---|
| `sensor/+/temperature?location=roomA` | 是 | `location=roomA` 匹配 |
| `sensor/+/temperature?location=roomB` | 否 | `location` 值不匹配 |
| `sensor/+/temperature?location=roomA&unit=celsius` | 是 | 两个条件均匹配 |
| `sensor/+/temperature?location=roomA&unit=fahrenheit` | 否 | `unit` 值不匹配 |
| `sensor/+/temperature` | 是 | 无过滤表达式，标准订阅 |

## 授权注意事项

启用[授权](../access-control/authz/authz.md)后，EMQX 根据配置的规则校验订阅主题。用于授权的主题为**基础主题过滤器**（`?` 分隔符之前的部分），过滤表达式在授权求值前会被剥离。

例如，订阅 `sensor/+/temperature?location=roomA` 的客户端，授权时使用的主题为 `sensor/+/temperature`。请确保授权规则中已覆盖与订阅过滤器配合使用的基础主题模式。

## 相关功能

订阅过滤器与 EMQX 中其他消息功能相辅相成：

- [共享订阅](../messaging/mqtt-shared-subscription.md)：在订阅者组之间分发消息以实现负载均衡，不支持基于内容的过滤。
- [保留消息](../messaging/mqtt-retained-message.md)：为新订阅者存储每个主题的最后一条消息，保留消息的投递不受订阅过滤表达式影响。
- [主题重写](../messaging/mqtt-topic-rewrite.md)：在路由前对主题字符串进行重写，主题重写规则在订阅过滤器求值之前执行。
- [通配符订阅](../messaging/mqtt-wildcard-subscription.md)：使用 `+` 和 `#` 通配符匹配多个主题，通配符主题过滤器可与订阅过滤器组合使用。
- [消息队列](../message-queue/message-queue-concept.md)：提供持久化的异步消息队列能力，支持持久存储和可配置的分发策略。<!-- Needs to be verified-->

## 下一步

了解订阅过滤器的概念后，可通过以下内容进行实践：

- [订阅过滤器快速开始](./subscription-filter-get-started.md)：启用该功能，并按照分步指南使用 MQTTX CLI 端对端验证过滤行为。
