# MQTT

MQTT 数据桥接是一种连接多个 EMQX 集群或其他 MQTT 服务的方式。本页介绍了 EMQX 中 MQTT 数据桥接的工作原理，并提供了在EMQX Dashboard 或使用配置文件创建 MQTT 数据桥接的快速入门教程。

## 桥接模式

EMQX 支持在两种主要模式下工作的 MQTT 数据桥接：入口和出口。本节将详细介绍每种模式的工作原理。同时，还介绍了在这两种模式中使用的连接池。

### 入口模式

在入口模式下，本地的 EMQX 从桥接的远程 MQTT 服务器订阅主题，并在当前集群内分发接收到的消息。下面是**入口**方向的消息服务流程：

<img src="./assets/bridge_mqtt_igress.png" alt="MQTT 数据桥接 igress 示意图" style="zoom:67%;" />

MQTT 数据桥接可以单独使用，也可以与规则结合使用，以实现更强大、更灵活的数据处理能力。在 **入口**方向上，数据桥接可以作为规则的数据源。MQTT 数据桥接与规则配合工作的消息流程如下：

<img src="./assets/bridge_igress_rule_link.png" alt="bridge_igress_rule_link" style="zoom:67%;" />

### 出口模式

在出口模式下，本地的 EMQX 根据规则设置，将当前集群中的消息转发给桥接的远程 MQTT 服务器。下面是**出口**方向上的消息服务流程：

<img src="./assets/bridge_mqtt_egerss.png" alt="MQTT 数据桥接 egress 示意图" style="zoom:67%;" />

在**出口**方向下，可以将规则处理结果作为消息，转发到远程 MQTT 服务器的指定主题下：

<img src="./assets/bridge_egress_rule.png" alt="bridge_egress_rule" style="zoom:67%;" />

### 连接池

EMQX 允许多个客户端同时连接到桥接的 MQTT 服务器。在创建桥接时您可以设置一个 MQTT 客户端连接池，并配置连接池大小以表明连接池中的客户端连接数。在 MQTT 数据桥接中启用连接池，可以充分利用服务器资源，以实现更大的消息吞吐和更好的并发性能。这对于处理高负载、高并发的场景非常重要。

由于 MQTT 协议要求连接到一个 MQTT 服务器的客户端必须具有唯一的客户端 ID，因此连接池中的每个客户端都被分配了一个唯一的客户端 ID。为了使客户端 ID 可预测，EMQX 根据以下模式自动生成客户端 ID：

```bash
[${ClientIDPrefix}:]${BridgeName}:${Mode}:${NodeName}:${N}
```

| 片段                | 描述                                                   |
| ------------------- | ------------------------------------------------------ |
| `${ClientIDPrefix}` | 配置的客户端 ID 前缀。如果未设置，则省略整个第一片段。 |
| `${BridgeName}`     | 桥接的名称，由用户提供。                               |
| `${Mode}`           | `ingress` 或 `egress`。                                |
| `${NodeName}`       | 运行 MQTT 客户端的节点名称。                           |
| `${N}`              | 从 `1` 到配置的 MQTT 客户端连接池的大小的数字。        |

#### 在入口模式下使用连接池

尽管连接池适用于入口和出口模式，但在入口模式下使用连接池需要考虑一些注意事项。当您拥有多个节点的 EMQX [集群](../deploy/cluster/introduction.md)并配置了一个入口 MQTT 桥接以从远程 MQTT 服务器订阅非共享主题时，如果连接池中的所有客户端都订阅相同的主题，它们将从远程 MQTT 服务器接收到重复的消息，这将给服务器带来压力。在这种情况下，强烈建议使用[共享订阅](../messaging/mqtt-shared-subscription.md)作为一种安全措施。例如，您可以将远程 MQTT 服务器的主题配置为 `$share/name1/topic1` 或者在使用主题过滤器时配置为 `$share/name2/topic2/#`。在非共享订阅情况下，MQTT 客户端连接池将缩减为一个客户端，这意味着只有一个客户端会启动。

## 快速开始

下面将以 EMQX 的[在线 MQTT 服务器](https://www.emqx.com/zh/mqtt/public-mqtt5-broker)作为桥接服务器，指导您如何配置连接与桥接。

:::tip 前置准备

- 了解[规则](./rules.md)。
- 了解[数据桥接](./data-bridges.md)。

:::

### 功能清单

- [异步请求模式](./data-bridges.md#异步请求模式)
- [缓存队列](./data-bridges.md#缓存队列)

### 通过 Dashboard 配置

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。

2. 点击页面右上角的**创建**。

3. 在**数据桥接类型**中选择 **MQTT**，点击**下一步**。

4. 输入数据桥接**名称**，要求是大小写英文字母或数字组合，例如  `my_mqtt_bridge`。

5. 进行连接相关配置。**MQTT 服务地址**设为 `broker.emqx.io:1883`，由于该服务器不需要认证，因此**用户名**、**密码**留空即可。该区域的其他字段可保留默认设置，也可根据实际场景设置。

7. 通过**入口配置**或**出口配置**设定桥接规则。
   
   :::tip
   入口配置与出口配置应至少配置一个，您可打开下方的开关进行相关配置。
   :::
   
   - **入口配置**（可选）：配置桥接规则，将远程 MQTT 服务上的消息转发到本地；我们希望订阅 `remote/topic/ingress ` 下的消息，并将收到的信息转发至 `local/topic/ingress` 主题，因此将进行如下配置：
   
      - **远程 MQTT 服务**：订阅主题以获取消息。
         - **主题**：在集群工作模式下，须通过共享订阅来避免消息重复，因此填入 `$share/g/remote/topic/ingress`。
         - QoS：选择 `0`。
   
      - **本地 MQTT 服务**：将订阅得到的消息发布到指定主题中，也可以留空，通过配置规则处理并使用[消息重发布](./rules.md#消息重发布)动作转发。
         - **主题**：填入 `local/topic/ingress`。
         - **QoS**：选择 `0`，或 `${qos}` （跟随消息 QoS）。
         - **Retain**：通过勾选确认是否以保留消息方式发布消息。
         - **消息模版**：转发的消息 Payload 模板，支持使用 `${field}` 语法提取数据，支持的字段如下：

        | 字段名称                      | 描述                                                                |
        | ----------------------------- | ------------------------------------------------------------------- |
        | topic                         | 来源消息主题                                                        |
        | server                        | 桥接连接的服务器地址                                                |
        | retain                        | 是否保留消息，值为 false                                            |
        | qos                           | 消息服务质量                                                        |
        | pub_props                     | MQTT 5.0 消息属性对象，包含用户属性对、用户属性和其他属性           |
        | pub_props.User-Property-Pairs | 用户属性对数组，每个包含键值对，例如 `{"key":"foo", "value":"bar"}` |
        | pub_props.User-Property       | 用户属性对象，包含键值对，例如 `{"foo":"bar"}`                      |
        | pub_props.*                   | 其他包含的消息属性键值对，例如 `Content-Type: JSON`                 |
        | payload                       | 消息内容                                                            |
        | message_received_at           | 消息接收时间戳，单位为毫秒                                          |
        | id                            | 消息 ID                                                             |
        | dup                           | 是否为重复消息                                                      |

        例如，当消息模板留空时将发布以下内容消息：
        ```json
        {
          "topic": "f/1",
          "server": "broker.emqx.io:1883",
          "retain": false,
          "qos": 0,
          "pub_props": {
              "User-Property-Pairs": [
                  {
                      "value": "bar",
                      "key": "foo"
                  }
              ],
              "User-Property": {
                  "foo": "bar"
              },
              "Message-Expiry-Interval": 3600,
              "Content-Type": "JSON"
          },
          "payload": "Hello MQTTX CLI",
          "message_received_at": 1699603701552,
          "id": "000609C7D2E3D556F445000010E4000C",
          "dup": false
        }
        ```

      - **连接池大小**：指定本地 MQTT 服务的客户端连接池的大小。在这个例子中，您可以设置为`8`。只要远程 MQTT 服务的主题使用共享订阅，这样的设置不会影响性能。
   
   - **出口配置**（可选）：将本地指定 MQTT 主题下的消息发布到远程 MQTT 服务，可以理解为入口配置的反向数据流。我们希望将 `local/topic/egress` 主题下的消息转发到远程 MQTT 服务 `remote/topic/egress` 主题中，因此将进行如下配置：
   
      - **本地 MQTT 服务**：指定待转发的消息主题。
        - **主题**：填入 `local/topic/egress` 
   
      - **远程 MQTT 服务**：指定远程服务器的目标主题。
        - **主题**：填入 `remote/topic/egress`。
        - **QoS**：选择 `0`，或 `${qos}` （跟随消息 QoS）。
        - **Retain**：通过勾选确认是否以保留消息方式发布消息。
        - **消息模版**：转发的消息 Payload 模板，支持使用 `${field}` 语法提取数据。
      - **连接池大小**：指定本地 MQTT 服务器的客户端连接池的大小。在这个例子中，您可以设置为`8`。
   
8. 其他配置（可选），根据情况配置同步/异步模式，队列与批量等参数。

9. 点击**创建**按钮完成数据桥接创建。

### 通过配置文件配置

EMQX 同时也支持使用配置文件创建 MQTT 数据桥接，对应的配置示例如下：

```bash
bridges.mqtt.my_mqtt_bridge {
  enable = true
  server = "broker.emqx.io:1883"
  username = "emqx_u"
  password = "public"
  proto_ver = "v4"
  clean_start = true
  keepalive = "60s"

  reconnect_interval = "10s"
  egress {
    local {topic = "local/topic/egress"}
    remote {
      payload = "${payload}"
      qos = 1
      retain = true
      topic = "remote/topic/egress"
    }
  }
  ingress {
    local {
      topic = "$share/g/remote/topic/ingress"
      qos = 1
      payload = "${payload}"
    }
    remote {qos = 1, topic = "local/topic/ingress"}
  }
}
```

