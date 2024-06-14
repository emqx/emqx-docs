# 常见功能问题解答

{% emqxce %}

## EMQX 开源版支持转储数据吗？

开源版不支持数据转储，此能力仅在企业版中提供。

{% endemqxce %}

## EMQX 支持将 MQTT 消息保存到数据库吗？

支持。你可以通过 **EMQX 企业版** 的 [数据集成](../data-integration/data-bridges.md) 功能实现消息的持久化，EMQX 支持多种关系型、非关系型以及时序数据库，你可以按需选择。

## EMQX 支持将 MQTT 消息转发到 Kafka 等消息队列吗？

支持。你可以通过 **EMQX 企业版** 的 [数据集成](../data-integration/data-bridges.md) 功能将消息转发给 Kafka、RabbitMQ 等消息队列。

## EMQX 支持将 MQTT 消息转发到其他 MQTT 服务吗？

支持。你可以通过 EMQX 的 [MQTT 桥接](../data-integration/data-bridge-mqtt.md) 功能将消息转发给其他 MQTT 服务，包括部署在 AWS 和 Azure 等公有云上的 IoT Hub，以及 EMQX、Mosquitto 这类标准的 MQTT Broker。

## 什么是共享订阅？

共享订阅是 MQTT 5.0 引入的全新特性，它允许客户端以负载均衡的方式消费消息。我们可以将客户端划分为多个订阅组，消息仍然会被转发给所有订阅组，但一个订阅组内的客户端将以随机、轮询等策略交替接收消息。由于共享订阅的所有处理逻辑都在服务端完成，而客户端只需订阅 `$share/group/{topic filter}` 即可，所以 MQTT 3.1.1 的客户端也可以以相同方式使用共享订阅。

共享订阅在数据采集这类消息生产者远多于消费者的场景中非常有用，更多内容请参考 [共享订阅](../messaging/mqtt-shared-subscription.md)。

## 什么是系统主题？

EMQX 会周期性地发布自身运行状态、MQTT 报文收发计数和客户端上下线事件等消息到 `$SYS/` 开头的系统主题，客户端可以订阅系统主题来获取相关的信息。

系统主题的完整介绍可参考 [此处](../observability/mqtt-system-topics.md)。

## EMQX 支持客户端以共享订阅的方式订阅系统主题吗？

支持。某些系统消息的发布频率可能较高，例如客户端上下线事件，所以采用共享订阅对于客户端来说是非常必要的。订阅示例： `$share/group1/$SYS/brokers/+/clients/+/connected`。

## EMQX 支持接入自定义协议吗？

EMQX 提供了一个 ExProto 网关，支持用户使用其熟悉的编程语言（如 Java、Python、Go 等）开发 gRPC 服务，用于解析设备使用的自定义协议，并完成设备连接、身份验证和消息传输等功能。

详情可参考 [ExProto 协议网关](../gateway/exproto.md)。

## EMQX 支持限制客户端可以发布或订阅的主题吗？

支持。EMQX 中的授权管理机制可以实现对客户端权限的精细管理。

EMQX 默认从 `acl.conf` 文件中查询 ACL 规则，用户也可以配置数据库作为 ACL 规则的数据源，例如 EMQX 的内置数据库、MySQL、Redis 等。

通常我们推荐在 `acl.conf` 中添加针对多个客户端生效的规则，例如仅允许同一网段内的客户端订阅系统主题；在数据库中添加针对单个客户端生效的规则，例如允许客户端 client1 订阅主题 example。

授权的完整介绍可参考 [此处](../access-control/authz/authz.md)。

## EMQX 支持流量控制吗？

支持。EMQX 支持连接速率和消息流入速率控制，从入口处避免系统过载，完整介绍可参考 [此处](../rate-limit/rate-limit.md)。

## EMQX 支持集群自动发现吗？有哪些实现方式？

除了手动创建集群，EMQX 也支持 DNS、etcd 等多种节点发现策略以实现自动集群，详情可参考 [创建与管理集群](../deploy/cluster/create-cluster.md)。

## EMQX 支持用户在服务端侧主动断开 MQTT 连接吗？

{% emqxce %}

支持。EMQX 提供了 [命令行接口](../admin/cli.md#clients) `emqx ctl clients kick <Client ID>` 与 [HTTP API](https://docs.emqx.com/cn/emqx/v@CE_MINOR_VERSION@/admin/api-docs.html) `DELETE /clients/{clientid}`，允许用户手动踢除 MQTT 连接，用户也可以在 Dashboard 的客户端列表页完成此操作。

{% endemqxce %}

{% emqxee %}

支持。EMQX 提供了 [命令行接口](../admin/cli.md#clients) `emqx ctl clients kick <Client ID>` 与 [HTTP API](https://docs.emqx.com/cn/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html) `DELETE /clients/{clientid}`，允许用户手动踢除 MQTT 连接，用户也可以在 Dashboard 的客户端列表页完成此操作。

{% endemqxee %}

## 我想监听设备的上下线事件，该怎么操作？

EMQX 提供了三种监听设备上下线事件的方式：

- 使用 [WebHook](../data-integration/data-bridge-webhook.md) 功能将上下线事件消息转发给外部 HTTP 服务。
- 使用 MQTT 客户端订阅 [系统主题](../observability/mqtt-system-topics.md) 获取上下线事件通知。
- 使用 [规则引擎](../data-integration/rules.md) 监听 `client.connected` 和 `client.disconnected` 事件，配合 [数据集成](../data-integration/data-bridges.md) 功能将事件消息写入到指定数据库。（仅限企业版）