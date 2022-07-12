# 共享订阅

共享订阅是在多个订阅者之间实现负载均衡的订阅方式：

```txt
                                                   [subscriber1] got msg1
             msg1, msg2, msg3                    /
[publisher]  ---------------->  "$share/g/topic"  -- [subscriber2] got msg2
                                                 \
                                                   [subscriber3] got msg3
```

上图中，共享 3 个 subscriber 用共享订阅的方式订阅了同一个主题 `$share/g/topic`，其中`topic` 是它们订阅的真实主题名，而  `$share/g/` 是共享订阅前缀。EMQX 支持两种格式的共享订阅前缀：

| 示例            | 前缀        | 真实主题名 |
| --------------- | ----------- | ---------- |
| $queue/t/1      | $queue/     | t/1        |
| $share/abc/t/1 | $share/abc | t/1        |

## 带群组的共享订阅

以 `$share/<group-name>` 为前缀的共享订阅是带群组的共享订阅：

group-name 可以为任意字符串，属于同一个群组内部的订阅者将以负载均衡接收消息，但 EMQX 会向不同群组广播消息。

例如，假设订阅者 s1，s2，s3 属于群组 g1，订阅者 s4，s5 属于群组 g2。那么当 EMQX 向这个主题发布消息 msg1 的时候：

- EMQX 会向两个群组 g1 和 g2 同时发送 msg1

- s1，s2，s3 中只有一个会收到 msg1
- s4，s5 中只有一个会收到 msg1

```txt
                                       [s1]
           msg1                      /
[emqx]  ------>  "$share/g1/topic"    - [s2] got msg1
         |                           \
         |                             [s3]
         | msg1
          ---->  "$share/g2/topic"   --  [s4]
                                     \
                                      [s5] got msg1
```

## 不带群组的共享订阅

以 `$queue/` 为前缀的共享订阅是不带群组的共享订阅。它是 `$share` 订阅的一种特例，相当与所有订阅者都在一个订阅组里面：

```txt
                                       [s1] got msg1
        msg1,msg2,msg3               /
[emqx]  --------------->  "$queue/topic" - [s2] got msg2
                                     \
                                       [s3] got msg3
```

## 负载均衡策略与派发 ACK 配置

MQTT 协议规范中，没有明确的规范平衡策略。EMQX 在配置的帮助下支持一些不同的平衡策略。
关于调度策略的更多信息，请参考配置文件。

平衡策略可以在全局或每组中指定。

- 全局策略可以在 `broker.shared_subscription_strategy` 配置中设置。
- 配置 `broker.shared_subscription_group.$group_name.strategy` 为每组策略。

```txt
# etc/emqx.conf

# 均衡策略
broker.shared_subscription_strategy = random

# 当设备离线，或者消息等级为 QoS1、QoS2，因各种各样原因设备没有回复 ACK 确认，消息会被重新派发至群组内其他的设备。
broker.shared_dispatch_ack_enabled = false
```

|  均衡策略    |             描述             |
| :---------- | :--------------------------- |
| random      | 在所有订阅者中随机选择       |
| round_robin | 按照订阅顺序                 |
| sticky      | 一直发往上次选取的订阅者     |
| hash        | 按照发布者 ClientID 的哈希值 |

::: tip
无论是单客户端订阅还是共享订阅都要注意客户端性能与消息接收速率，否则会引发消息堆积、客户端崩溃等错误。
:::

### 关于消息丢失的讨论

EMQX 向订阅者的会话发送消息。

当开启会话持久化功能（clean_session=false）时，订阅者可以在重新连接后立即恢复数据流而不丢失消息。

这与 "负载平衡" 有点矛盾，因为通常当共享订阅使用时，如果一个设备离线，组内的其他设备就会接手接管数据流。但是，如果设备离线足够长的时间，会话消息缓冲区最终会溢出，并导致消息丢失。

由于上述原因，持久化会话对于共享订阅业务来说并不常见，但是用户仍然可以使用这种方式。

配置 `broker.shared_dispatch_ack_enabled` 的引入是为了在持久化会话的情况下改善负载共享的逻辑。当设置为 `true` 时，EMQX 派发消息时会检查设备的在线状态，如果设备时历险的，将会把消息分配给组内的其他成员。

- 一旦消息被派发到一个订阅者的会话，消息将留在会话中缓冲区，但不会立即重新派发。
- 当会话终止时（Session 的生命周期结束），会话中的待发信息会被重新分配给组内的其他成员。会话终止时，会话中的待发信息会被重新分配给组内的其他成员。
- 当所有成员都离线时，消息会按照配置的策略来派发。
- 当共享订阅组中所有的会话都已经超期被销毁，消息就会被丢弃。
