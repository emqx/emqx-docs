# MQTT 配置

[MQTT](https://mqtt.org/) 是一种用于连接物联网（IoT）设备的轻量级发布/订阅消息传输协议。EMQX 支持 MQTT 3.1、3.1.1 和 5.0。

本页介绍如何配置 EMQX 的 MQTT 协议行为，包括报文校验和限制、订阅、延迟发布、Keep Alive 处理以及会话。

## 基本 MQTT 配置

本节将介绍决定 MQTT 协议行为的配置设置，包括数据包大小、客户端 ID 长度、主题级别、服务质量（QoS）、主题别名和保留等。

::: tip

您也可以在 EMQX Dashboard 中找到对应的配置项（**管理** -> **MQTT 配置** -> **通用**）。一旦您通过 Dashboard 配置了这些项，您的设置将覆盖 `emqx.conf` 中的相同配置项。

:::

**示例代码：**

```bash
mqtt {
  max_packet_size = 1MB
  max_clientid_len = 65535
  max_topic_levels = 128
  max_qos_allowed = 2
  max_topic_alias = 65535
  retain_available = true
  strict_mode = true
}  
```

其中，

| **配置项**         | Dashboard UI       | **描述**                                                     | **默认值** | **可选值**      |
| ------------------ | ------------------ | ------------------------------------------------------------ | ---------- | --------------- |
| `max_packet_size`  | 最大报文大小       | MQTT 报文用于在 MQTT 客户端和 EMQX 之间发送消息。<br /><br />此设置允许的最大 MQTT 报文大小。 | `1MB`      |                 |
| `max_clientid_len` | 最大客户端 ID 长度 | 此设置 MQTT 客户端 ID 的最大长度。<br /><br />它可以帮助防止客户端使用过长的客户端 ID 导致问题。 | `65535`    | `23` - `65535`  |
| `max_topic_levels` | 最大主题层级       | MQTT 主题用于组织和分类消息。<br /><br />此设置允许 MQTT 主题中的最大级别数量。 | `128`      | `1` - `65535`   |
| `max_qos_allowed`  | 最大 QoS           | QoS 等级决定了消息的可靠性和传递保证等级。<br /><br />此设置允许 MQTT 消息的最大服务质量（QoS）等级。 |            |                 |
| `max_topic_alias`  | 最大主题别名数     | 主题别名是通过使用较短的别名代替完整主题名称来减少 MQTT 数据包大小的一种方式。<br /><br />此设置允许在 MQTT 会话中使用的最大主题别名数量。 | `65535`    | `1` - `65535`   |
| `retain_available` | 启用保留消息       | 保留消息用于存储发布到主题的最后一条消息，以便新订阅该主题的客户端可以接收到最新的消息。<br /><br />此设置是否启用 MQTT 中的保留消息功能。 | `true`     | `true`, `false` |
| `strict_mode`      | 严格模式           | 设置是否对传入的 MQTT 报文执行额外的协议合规性校验。未通过这些校验的报文会导致客户端连接关闭。 | `true` | `true`, `false` |

### MQTT 报文严格校验

从 EMQX 6.3.0 开始，默认启用 MQTT 报文严格校验。设置 `strict_mode = true` 后，EMQX 会拒绝格式错误的 MQTT 报文，包括存在以下问题的报文：

- MQTT 固定报文头中的标志位组合无效。
- MQTT 3.1.1 CONNECT 报文设置了 Password Flag，但未设置 Username Flag。
- 客户端 ID、主题名称、用户名、密码、Will Topic 或 MQTT 5.0 字符串属性等字段包含无效的 UTF-8 字符串，包括空字符和其他协议禁止的控制字符。
- MQTT 协议要求 Packet Identifier 非零，但报文中的值为零。

检测到格式错误的报文后，EMQX 会关闭客户端连接，并记录一条 `info` 级别的 `frame_parse_error` 日志，其中包含具体原因。对于 MQTT 5.0 客户端，EMQX 还会在可能的情况下发送原因码为 `0x81`（Malformed Packet）的 CONNACK 或 DISCONNECT 报文。对于 MQTT 3.1 和 MQTT 3.1.1 客户端，EMQX 会直接断开连接，不返回格式错误原因码。

如果现有客户端不符合这些 MQTT 协议要求，可以通过以下配置暂时关闭仅在严格模式下执行的协议合规性校验：

```bash
mqtt.strict_mode = false
```

如需仅对特定旧客户端关闭严格模式校验，可以配置一个 Zone，并将其关联到专用监听器：

```bash
zones.legacy_clients {
  mqtt.strict_mode = false
}

listeners.tcp.legacy {
  bind = "0.0.0.0:1884"
  zone = legacy_clients
}
```

通过其他监听器连接的客户端仍使用严格校验。有关 Zone 的更多信息，请参见 [Zone 覆盖](./configuration.md#zone-覆盖)。

## 订阅设置

在 EMQX 中，订阅指的是客户端在 EMQX 上订阅主题的过程。当客户端订阅一个主题时，它表示希望接收发布到该主题的消息。

本节介绍如何配置共享订阅、通配符订阅和排它订阅。

::: tip

您也可以在 EMQX Dashboard 中找到对应的配置项（**管理** -> **MQTT 配置** -> **通用**）。一旦您通过 Dashboard 配置了这些项，您的设置将覆盖 `emqx.conf` 中的相同配置项。

:::

**示例代码：** <!--待审核代码-->

```bash
mqtt {
	wildcard_subscription = true
  exclusive_subscription = false
  shared_subscription = true
  shared_subscription_strategy  =  round_robin
}
```

其中，

| **配置项**                     | Dashboard UI   | **描述**                                                     | **默认值**    | 可选值                                                       |
| ------------------------------ | -------------- | ------------------------------------------------------------ | ------------- | ------------------------------------------------------------ |
| `wildcard_subscription`        | 允许通配符订阅 | 通配符订阅允许 MQTT 客户端使用单个订阅通过通配符如 `+` 和 `#` 订阅多个主题。<br /><br />此设置是否启用通配符订阅。 | `true`        | `true`, `false`                                              |
| `exclusive_subscription`       | 允许排它订阅   | 排它订阅允许一次只有一个 MQTT 客户端可以订阅一个主题。<br /><br />此设置是否启用排它订阅。 | `true`        | `true`, `false`                                              |
| `shared_subscription`          | 允许共享订阅   | 共享订阅允许多个 MQTT 客户端共享对主题的订阅。<br /><br />此设置是否在 MQTT 中启用共享订阅。 | `true`        | `true`, `false`                                              |
| `shared_subscription_strategy` | 共享订阅策略   | 此设置定义了在共享订阅的 MQTT 客户端之间分发消息的策略。<br /><br />仅当 `shared_subscription` 设置为 `true` 时需要。 | `round_robin` | - `random` (将消息随机分发给选定的订阅者)<br /><br />- `round_robin` (以轮询方式选择订阅者)<br /><br />- `sticky` (总是使用最后选定的订阅者进行分发，直到订阅者断开连接。)<br /><br />- `hash` (根据 `clientIds` 的哈希选择订阅者) |

## 延迟发布设置

本节介绍如何启用延迟发布以及如何设置允许的最大延迟消息数量。延迟发布功能允许客户端将消息延迟一定时间后发布到主题。这个功能对于需要在特定时间或满足某个条件时发布消息的场景非常有用。

**示例代码：**

```bash
delay {
  delayed_publish_enabled = true
  max_delayed_messages = 0
}
```

其中，

- `delayed_publish_enabled` 设置是否在 EMQX 中启用延迟发布功能；默认值：`true`，可选值：`true`, `false`。
- `max_delayed_messages` 设置允许的最大延迟消息数量；默认值：`0`。

## Keep Alive 设置

Keep Alive 是一个两字节整数，表示以秒为单位的时间间隔。它是一种机制，确保即使没有数据传输，MQTT 客户端和 EMQX 之间的连接仍然保持活动。当 MQTT 客户端创建和 EMQX 的连接时，在连接请求协议包的 Keep Alive 变量头字段中设置非零值就可以在通信双方之间启用 Keep Alive 机制。有关 Keep Alive 工作原理的详细信息，请参见 [MQTT 协议 Keep Alive 详解](https://www.emqx.com/zh/blog/mqtt-keep-alive)。

根据 MQTT 5.0 协议，对于启用了 Keep Alive 的客户端，如果服务器在 Keep Alive 时长的 1.5 倍时间内没有收到来自客户端的 MQTT 控制报文，它必须关闭与客户端的网络连接。因此，EMQX 引入了一个配置项 `keepalive_multiplier`，用来周期性地检查客户端的 Keep Alive 超时状态。`keepalive_multiplier` 的默认值是 `1.5`：

```bash
keepalive_multiplier = 1.5
```

超时计算公式如下：

$$
\text{Keep Alive} \times \text{keepalive\_multiplier}
$$

### 动态 Keep Alive 调整

在车联网（T-Box）和移动物联网等场景中，MQTT 客户端需要在**活跃状态**（高频通信）和**休眠状态**（低功耗保活）之间切换。固定的 keepalive 值无法同时满足两种需求：

- 较短的 keepalive 能在活跃期快速检测到断线，但在车辆停驻或设备空闲时会产生过多心跳流量，加速耗电。
- 较长的 keepalive 能减少休眠期的流量，但在活跃期会延迟断线检测。

EMQX 支持通过 `$SETOPTS/` 系统主题对每个客户端动态调整 keepalive。客户端可向对应主题发布消息来更新自身的 keepalive 容忍时长，有权限的后端服务也可批量更新多个客户端——均无需断开连接或重新协商 MQTT 连接。调整仅作用于内存中的活跃会话，不会持久化。

::: warning 与监听器挂载点不兼容
对于通过配置了[挂载点](./listener.md#挂载点mountpoint)的监听器连接的客户端，动态 Keep Alive 调整不生效。EMQX 会先应用挂载点，再匹配 `$SETOPTS/` 前缀，因此该更新会作为普通消息路由到挂载后的字面主题，且不会向客户端报告任何错误。
:::

#### 单客户端更新：`$SETOPTS/mqtt/keepalive`

客户端向该主题发布消息，即可更新自身在 Broker 侧的 keepalive 超时。EMQX 自动从发布者的会话中获取客户端 ID，无需在有效载荷中显式指定。

**有效载荷格式：** 以字符串表示的非负整数，单位为秒。

```text
300
```

**有效范围：** `0`–`65535` 秒。`0` 表示禁用该会话的 keepalive 检查；超过 `65535` 的值将被截断为 `65535`。若客户端所在 zone 配置了 `mqtt.server_keepalive`，实际生效值为两者的最小值。

**使用场景示例：** 车辆进入停驻状态后，T-Box 客户端向 `$SETOPTS/mqtt/keepalive` 发布 `300`。EMQX 将该客户端在 Broker 侧的 keepalive 容忍时长延长至 300 秒（在默认 `1.5×` 乘数下，实际空闲超时为 450 秒），保持 MQTT 连接持续可用以接收远程指令。需要注意的是，此操作仅调整 Broker 侧的超时容忍度，客户端实际发送 PINGREQ 的间隔不会自动变化。如需降低心跳流量，客户端还需自行调长其本地的 keepalive 间隔。

#### 批量更新：`$SETOPTS/mqtt/keepalive-bulk`

后端服务向该主题发布消息，可在单条消息中批量更新多个客户端的 keepalive。

**有效载荷格式：** JSON 数组，每个元素包含以下字段：

| 字段 | 类型 | 是否必填 | 说明 |
|---|---|---|---|
| `clientid` | String | 是 | 目标 MQTT 客户端标识符 |
| `keepalive` | Integer | 是 | 新的 keepalive 间隔，单位为秒（0–65535） |

```json
[
  { "clientid": "tbox-001", "keepalive": 300 },
  { "clientid": "tbox-002", "keepalive": 60 }
]
```

批量更新为异步处理，并支持集群感知：EMQX 会定位各目标客户端所在节点，通过节点间 RPC 完成更新。若内部队列中待处理的批量请求超过 10 条，后续请求将被丢弃并记录警告日志。

#### 访问控制

两个主题有意设计为独立路径，以支持精细化的权限控制：

- 允许已认证的客户端向 `$SETOPTS/mqtt/keepalive` 发布消息，使每台设备可以自行调整 keepalive。
- 将 `$SETOPTS/mqtt/keepalive-bulk` 的发布权限限制为可信的后端服务。

:::tip
不建议对不可信客户端开放 `$SETOPTS/mqtt/keepalive` 的发布权限。将 keepalive 设为 `0` 会完全禁用该会话的 keepalive 检查，设置过大的值则可能导致僵尸连接长时间保留，消耗 Broker 资源。
:::

发布到上述任一主题的消息均会被 EMQX 在路由前拦截消费，不会投递给任何订阅者。

## 会话设置

本节介绍如何配置会话。在 MQTT 中，会话指的是客户端与消息服务器之间的连接。如在 EMQX 中，当客户端连接到 EMQX 时，它建立了一个会话，允许它订阅主题并接收消息，以及向 EMQX 发布消息。

::: tip

您也可以在 EMQX Dashboard 中找到对应的配置项（**管理** -> **MQTT 配置**）。一旦您通过 Dashboard 配置了这些项，您的设置将覆盖 `emqx.conf` 中的相同配置项。

:::

**示例代码：**

```bash
mqtt {
    max_subscriptions = infinity
    upgrade_qos = false
    max_inflight = 32
    retry_interval = 30s
    max_awaiting_rel = 100
    await_rel_timeout = 300s
    session_expiry_interval = 2h
    max_session_expiry_interval = infinity
    max_mqueue_len = 1000
    mqueue_priorities = disabled
    mqueue_default_priority = lowest
    mqueue_store_qos0 = true
    force_shutdown {
      max_mailbox_size = 1000
      max_heap_size = 32MB
    }
    force_gc {
      count  =  16000
      bytes  =  16MB
    }
  }
```

其中，

| **配置项**                        | Dashboard UI         | **描述**                                                     | **默认值**                                                   | **可选值**                          |
| --------------------------------- | -------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ----------------------------------- |
| `max_subscriptions`               | 最大订阅数量         | 此设置允许客户端拥有的最大订阅数。                           | `infinity`                                                   | `1` - `infinity`                    |
| `upgrade_qos`                     | 升级 QoS             | 此设置是否允许客户端在消息发布后升级消息的 QoS (服务质量) 等级。 | `false` (禁用)                                               | `true`, `false`                     |
| `max_inflight`                    | 最大飞行窗口         | 此设置允许同时在途（即已发送但尚未确认）的 QoS 1 和 QoS 2 消息的最大数量。 | `32`                                                         | `1` - `65535`                       |
| `retry_interval`                  | 消息重试间隔         | 此设置客户端应该以多久的间隔重试发送 QoS 1 或 QoS 2 消息。   | `30s`<br />单位: 秒                                          | --                                  |
| `max_awaiting_rel`                | 最大待发 PUBREL 数量 | 此设置每个会话中挂起的 QoS 2 消息数量，直到收到 `PUBREL` 或超时。达到此限制后，新的 QoS 2 `PUBLISH` 请求将被拒绝，并返回错误码 `147(0x93)`。<br />在 MQTT 中，`PUBREL` 是 QoS 2 消息流中用于确保消息交付的控制包。 | `100`                                                        | `1` - `infinity`                    |
| `await_rel_timeout`               | 最大 PUBREL 等待时长 | 此设置等待接收到 QoS 2 消息的 `PUBREL` 的时间。达到此限制后，EMQX 将释放包 ID 并生成警告级别日志。<br />注意：无论是否收到 `PUBREL`，EMQX 都会转发收到的 QoS 2 消息。 | `300s`<br />单位: 秒                                         | --                                  |
| `session_expiry_interval`         | 会话过期间隔         | 此设置客户端断开连接后 EMQX 保留会话的时长。适用于以 `Clean Session = false` 连接的 MQTT 3.1 和 3.1.1 客户端。MQTT 5.0 客户端通过 CONNECT 报文的 `Session-Expiry-Interval` 属性自行指定该值，参见 `max_session_expiry_interval`。<br />使用默认的内存会话存储时，已断开连接的会话会在整个过期间隔内驻留在内存中。参见表格后的警告。 | `2h`                                                         | --                                  |
| `max_session_expiry_interval`     | 最大会话过期间隔     | 此设置限制 MQTT 5.0 客户端通过 CONNECT 和 DISCONNECT 报文的 `Session-Expiry-Interval` 属性所能请求的最大会话过期间隔。当客户端在连接时请求的值超过此限制时，EMQX 会将其截断为该限制值，并在 CONNACK 的 `Session-Expiry-Interval` 属性中返回截断后的值（MQTT 5.0 规范 3.2.2.3.2 节）。DISCONNECT 报文中超过此限制的值同样会被截断为该限制值。对 MQTT 3.1 和 3.1.1 客户端无效，其会话过期间隔由 `session_expiry_interval` 决定。<br />自 EMQX 6.3.0 起提供。 | `infinity`（不限制）                                         | 时长<br />或<br />`infinity`        |
| `max_mqueue_len`                  | 最大消息队列长度     | 设置内存会话使用的消息队列长度限制。当客户端离线但会话仍保留、飞行窗口已满或连接的发送队列拥塞时，消息会进入该队列。当某个主题优先级的消息队列达到此限制时，EMQX 会优先淘汰该优先级中最早入队的 QoS 0 消息。 | `1000`                                                       | `0` - `infinity`                    |
| `mqueue_priorities`               | 主题优先级           | 此设置主题优先级，此处的配置将覆盖 `mqueue_default_priority` 定义的优先级。 | `disabled` <br />会话使用 `mqueue_default_priority` 设置的优先级。 | `disabled`<br />或<br />`1` - `255` |
| `mqueue_default_priority`         | 默认主题优先级       | 此设置默认主题优先级。                                       | `lowest`                                                     | `highest`， `lowest`                |
| `mqueue_store_qos0`               | 存储 QoS 0 消息      | 设置客户端离线但会话仍保留、内存会话连接的发送队列拥塞或飞行窗口已满时，EMQX 是否将 QoS 0 消息存入会话消息队列。如果禁用该配置项，EMQX 会丢弃客户端离线或发送队列拥塞时到达的 QoS 0 消息。如果仅飞行窗口已满，EMQX 仍会立即投递 QoS 0 消息。 | `true`                                                       | `true`, `false`                     |
| `force_shutdown`                  | 强制关闭             | 此设置是否启用强制关闭功能，当邮箱队列长度（`max_mailbox_size`）或堆内存（`max_heap_size`）超过设定值时强制关闭客户端进程。 | `true`                                                       | `true`, `false`                     |
| `force_shutdown.max_mailbox_size` | 最大邮箱大小         | 此设置触发强制关闭的最大邮箱队列长度。                       | `1000`                                                       | `1` - `infinity`                    |
| `force_shutdown.max_heap_size`    | 最大堆内存           | 此设置触发强制关闭的最大堆大小。                             | `32 MB`                                                      | --                                  |
| `force_gc`                        | --                   | 此设置是否启用强制垃圾回收，如果达到指定的消息数量（`count`）或接收字节（`bytes`）： | `true`                                                       | `true`, `false`                     |
| `force_gc.count`                  | --                   | 此设置将触发强制垃圾回收的接收消息数量。                     | `16000`                                                      | `0` - `infinity`                    |
| `force_gc.bytes`                  | --                   | 此设置将触发强制垃圾回收的接收字节数量。                     | `16 MB`<br />单位: `MB`                                      | --                                  |

::: warning 已断开连接会话的内存开销

使用默认的内存会话存储时，过期间隔大于零的会话不会在客户端断开连接时被移除。EMQX 会将会话、订阅和消息队列保留在内存中，直到客户端重新连接或过期间隔结束。如果客户端未在会话过期前重新连接，节点上已断开连接的会话数量大约等于客户端断开连接的速率与过期间隔的乘积。

客户端断开连接后仍保留会话是 MQTT 持久会话的预期行为。请为您的工作负载分配足够的内存，或使用[会话持久化](../durability/durability_introduction.md)，它将会话状态保存在磁盘上。

:::

::: tip

EMQX 提供了更多配置项以更好地满足定制化需求。详情请参见 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)。

:::
