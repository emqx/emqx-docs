# 发布订阅模式

发布订阅模式区别于传统的客户端-服务器模式，它使发送消息的客户端（发布者）与接收消息的客户端（订阅者）分离，发布者与订阅者不需要建立直接联系。

MQTT 协议使用发布订阅模式，并且根据主题而不是消息内容来路由消息，每个消息都包含一个主题，代理无需解析用户数据，这为实现一个通用的、与业务无关的 MQTT 代理提供了可能。

MQTT 主题 (Topic) 类似 URL 路径，例如:

```bash
chat/room/1

sensor/10/temperature

sensor/+/temperature

$SYS/broker/metrics/packets/received

$SYS/broker/metrics/#
```

主题 (Topic) 通过'/'分割层级，支持'+', '\#'通配符:

```bash
'+': 表示通配一个层级，例如 a/+，匹配 a/x, a/y

'#': 表示通配多个层级，例如 a/#，匹配 a/x, a/b/c/d
```

订阅者与发布者之间通过主题路由消息进行通信，例如采用 MQTTX CLI 命令行发布订阅消息:

```bash
mqttx pub -t a/b/+ -q 1

mqttx sub -t a/b/c -m hello -q 1
```

订阅者可以订阅含通配符主题，但发布者不允许向含通配符主题发布消息。

::: tip

更多有关 MQTT 发布订阅模式与 MQTT 主题内容请参考：

- [MQTT 发布订阅模式介绍](https://www.emqx.com/zh/blog/mqtt-5-introduction-to-publish-subscribe-model)
- [MQTT 主题](https://www.emqx.com/zh/blog/advanced-features-of-mqtt-topics)

:::
