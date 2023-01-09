- - [阿里 ACK 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/aliyun-ack-deployment.html)
  - [华为 CCE 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/cce-deployment.html)
  - [AWS EKS 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/aws-eks-deployment.html)
  - [腾讯云 TKE 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/tencent-tke-deployment.html)
  - [Azure AKS 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/azure-deployment.html)
- 任务
- FAQ 常见问题解答

# MQTT 特性验证

EMQX 支持完整的 MQTT 消息特性包括标准协议中的遗嘱消息、保留消息、共享订阅，这些特性满足了物联网开发中各类应用场景的需求，大大简化了开发难度和使用流程。

本章节将提供以上每种特性的使用步骤及注意事项，并通过 MQTT X CLI 进行验证，带领大家熟悉 MQTT 消息特性并掌握基本使用方式。

:::tip 先决条件

- 熟悉并了解 [发布订阅操作](./mqtt-publish-and-subscribe.md)。
:::

## 遗嘱消息

使用步骤：

1. 在服务器端启用遗嘱消息功能（EMQX 默认已启用）。
2. 客户端连接时设置遗嘱消息主题与 Payload。
3. 使用另一个客户端订阅遗嘱消息主题，准备接收遗嘱消息。
4. 客户端断开连接或关闭时，服务器将发布预先设置的遗嘱消息，并转发到订阅了对应主题的其他在线客户端。

使用 MQTT X CLI 验证：

1. 客户端连接时设置遗嘱消息主题为 `t/1`，Payload 为 `A will message from MQTTX CLI`：

    ```bash
    $ mqttx conn -h 'localhost' -p 1883 --will-topic 't/1' --will-message 'A will message from MQTTX CLI'
    Connected
    ```

2. 使用另一个客户端订阅遗嘱消息主题 `t/1` 以接收遗嘱消息：

    ```bash
    mqttx sub -t 't/1' -h 'localhost' -p 1883 -v
    ```

1. 断开 1 中的客户端，2 中的客户端将接收到遗嘱消息：

    ```bash
    topic:  t/1
    payload:  A will message from MQTTX CLI
    ```

## 保留消息

使用步骤：

1. 在服务器端启用保留消息功能（EMQX 默认已启用）。
2. 发布消息时设置 `retain = true`，该条消息将首先发送给当前订阅者，并在对应主题上设置为保留消息。
3. 任意客户端订阅**同一主题**时，消息将发送给订阅者。
4. 任意客户端发布一条空的**保留消息**消息到**同一主题**时，将清空主题上的保留消息。

使用 MQTT X CLI 验证：

1. 客户端发布消息时设置主题为 `t/1`，Payload 为 `A retained message from MQTTX CLI`，设置 `retain = true`：

    ```bash
    mqttx pub -t 't/1' -m 'A retained message from MQTTX CLI' --retain true -h 'localhost' -p 1883
    ```

2. 其他客户端重新订阅 `t/1` 主题时将接收到保留消息，重复多次该步骤，保留消息一直存在：

    ```bash
    $ mqttx sub -t 't/1' -h 'localhost' -p 1883 -v
    topic:  t/1
    payload:  A retained message from MQTTX CLI
    ```

3. 使用空消息清空保留消息：

    ```bash
    mqttx pub -t 't/1' -m '' --retain true -h 'localhost' -p 1883
    ```

4. 重复第 2 步，无法接收到保留消息，表明保留消息已被清空。

## 共享订阅

使用步骤：

1. 在服务器端启用共享订阅（EMQX 默认已启用）。
2. 多个订阅者订阅相同的主题（原始主题），订阅时需要加上共享订阅分组前缀 `$share/{group}`（共享订阅主题）：
   1. 如原始主题为 `t/1`，则共享订阅主题为 `$share/my_group/t/1`（`my_group` 为自定义的组名）。
   2. 如果原始祖逖为 `/t/1`，则共享订阅主题为 `$share/my_group//t/1`。
3. 使用任意客户端向原始主题依次发布多条消息，消息按照预设的共享订阅策略转发到多个订阅者，同一个分组下每个订阅者每次只能收到一条消息。

使用 MQTT X CLI 验证：

1. 4 个订阅者分为 2 组共享订阅 `t/1` 主题：

    ```bash
    # A、B 客户端订阅的主题为 `$share/my_group1/t/1`
    mqttx sub -t '$share/my_group1/t/1' -h 'localhost' -p 1883

    ## C、D 客户端订阅的主题为 `$share/my_group2/t/1`
    mqttx sub -t '$share/my_group2/t/1' -h 'localhost' -p 1883
    ```

2. 使用新客户端向原始主题 `t/1` 发布 Payload 分别为 `1` `2` `3` `4` 的 4 条消息：

    ```bash
    mqttx pub -t 't/1' -m '1' -h 'localhost' -p 1883
    mqttx pub -t 't/1' -m '2' -h 'localhost' -p 1883
    mqttx pub -t 't/1' -m '3' -h 'localhost' -p 1883
    mqttx pub -t 't/1' -m '4' -h 'localhost' -p 1883
    ```

3. 观察 A、B 分组与 C、D 分组每个客户端消息接收情况，可以得到以下现象：
   1. 消息会同时出现在 A、B 分组与 C、D 分组中。
   2. 同一分组中，同一条消息不会重复出现。
