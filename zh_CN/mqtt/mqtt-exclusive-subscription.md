# 排它订阅

排它订阅允许对主题进行互斥订阅，一个主题同一时刻仅被允许存在一个订阅者，在当前订阅者未取消订阅前，其他订阅者都将无法订阅对应主题。

排它订阅的前缀和示例：

| 示例            | 前缀        | 真实主题名 |
| --------------- | ----------- | ---------- |
| $exclusive/t/1      | $exclusive/     | t/1        |

当某个客户端 **A** 订阅 `$exclusive/t/1` 后，其他客户端再订阅 `$exclusive/t/1` 时都会失败，直到 **A** 取消了对 `$exclusive/t/1` 的订阅为止。

**注意**: 排它订阅必须使用 `$exclusive/` 前缀，在上面的示例中，其他客户端依然可以通过 `t/1` 成功进行订阅。 

## 订阅失败错误码

| 错误码            | 原因        | 
| --------------- | ----------- | 
| 0x8F     | 使用了 `$exclusive/`，但并未开启排它订阅  | 
| 0x97 | 已经有客户端订阅了该主题 |


## 配置设置

排它订阅默认未开启，可在 `etc/emqx.conf` 中配置：

|               配置项                |      类型       | 默认值 |                 描述                 |
| ----------------------------------- | --------------- | ------ | ------------------------------------ |
| mqtt.exclusive_subscription  | boolean          | false   | 排它订阅的默认开关  |
| zone.external.exclusive_subscription | boolean | 未设置 | 排它订阅在 external zone 上的开关   |
| zone.internal.exclusive_subscription | boolean | 未设置 | 排它订阅在 internal zone 上的开关 |

在 zone 上如果没有设置 `exclusive_subscription` 的值，EMQX 将会使用 `mqtt.exclusive_subscription` 来判断功能是否开启。