# 消息桥接

EMQ X 企业版桥接转发 MQTT 消息到 Kafka、RabbitMQ、Pulsar、RocketMQ、MQTT Broker 或其他 EMQ X 节点。

桥接是一种连接多个 EMQ X 或者其他 MQTT 消息中间件的方式。不同于集群，工作在桥接模式下的节点之间不会复制主题树和路由表。桥接模式所做的是：

按照规则把消息转发至桥接节点；
从桥接节点订阅主题，并在收到消息后在本节点/集群中转发该消息。

```bash
              ---------                     ---------                     ---------
Publisher --> | Node1 | --Bridge Forward--> | Node2 | --Bridge Forward--> | Node3 | --> Subscriber
              ---------                     ---------                     ---------
```

工作在桥接模式下和工作在集群模式下有不同的应用场景，桥接可以完成一些单纯使用集群无法实现的功能：

- 跨 VPC 部署。由于桥接不需要复制主题树和路由表，对于网络稳定性和延迟的要求相对于集群更低，桥接模式下不同的节点可以部署在不同的 VPC 上，客户端可以选择物理上比较近的节点连接，提高整个应用的覆盖能力。
- 支持异构节点。由于桥接的本质是对消息的转发和订阅，所以理论上凡是支持 MQTT 协议的消息中间件都可以被桥接到 EMQ X，甚至一些使用其他协议的消息服务，如果有协议适配器，也可以通过桥接转发消息过去。
- 提高单个应用的服务上限。由于内部的系统开销，单个的 EMQ X 有节点数上限。如果将多个集群桥接起来，按照业务需求设计桥接规则，可以将应用的服务上限再提高一个等级。
在具体应用中，一个桥接的发起节点可以被近似的看作一个远程节点的客户端。

## 桥接插件列表

| 存储插件                 | 配置文件                      | 说明               |
| -------------------- | ------------------------- | ---------------- |
| emqx_bridge_mqtt   | emqx_bridge_mqtt.conf   | MQTT Broker 消息转发 |
| emqx_bridge_kafka  | emqx_bridge_kafka.conf  | Kafka 消息队列       |
| emqx_bridge_rabbit | emqx_bridge_rabbit.conf | RabbitMQ 消息队列    |
| emqx_bridge_pulsar | emqx_bridge_pulsar.conf | Pulsar 消息队列      |
| emqx_bridge_rocket | emqx_bridge_rocket.conf | RocketMQ 消息队列    |

{% emqxce %}

::: danger
EMQ X Broker 中仅适用以下操作：

- MQTT 桥接
- RPC 桥接

其余均是 EMQ X Enterprise 专属功能，推荐使用[规则引擎](../rule/rule-engine.md) 以实现更灵活的桥接功能。
:::

{% endemqxce %}

{% emqxee %}

::: tip
推荐使用
[规则引擎](../rule/rule-engine.md)
以实现更灵活的桥接功能。
:::

{% endemqxee %}
