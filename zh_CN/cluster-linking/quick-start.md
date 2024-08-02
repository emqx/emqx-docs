# 集群连接快速开始

本页面提供了在两个远程 EMQX 集群之间设置集群连接的快速开始指南。

## 前提条件

确保您在两个不同的区域中拥有计算资源，每个区域都托管一个 EMQX 集群。对于本示例，您可以使用 `us-east` 和 `eu-west` 区域，分别命名为 `cluster-us-east` 和 `cluster-eu-west`。

### 要求

- EMQX 版本 5.8.0 或更高版本
- 唯一的集群名称
- 集群之间的网络通信

集群连接要求每个集群的 MQTT 监听器在另一个集群的网络中可访问。建议将这些 MQTT 监听器置于负载均衡器之后，以实现均匀的流量分配。为了安全，如果使用公共互联网，确保集群之间的通信使用 [TLS](./configuration.md) 和严格的 [TLS 或 MQTT 客户端认证](../access-control/authn/authn.md) 进行保护。

## 设置第一个集群（cluster-us-east）

使用以下配置片段在 `cluster-us-east` 集群的配置文件中设置第一个集群：

```bash
# 集群连接配置
cluster {
  # 本集群名称
  name = "cluster-us-east"
  links = [
    {
      # 第二个集群名称
      name = "cluster-eu-west"
      # 第二个集群的 MQTT 监听器端点
      server = "emqx.us-east.myinfra.net:11883"
      clientid = "clink-us-east"
      topics = ["#"]
    }
  ]
}

# 集群连接专用监听器
listeners {
  tcp.clink {
    bind = 11883
  }
}
```

此配置指定以下内容：

- 远程集群名称为 `cluster-eu-west`。
- 该集群可以通过 `emqx.us-east.myinfra.net:11883` 进行访问。
- 集群连接 MQTT 连接的客户端 ID 前缀为 `clink-us-east`。
- 所有消息（匹配 `#` 通配符主题）将被转发到本地集群。
- 启用了集群连接专用监听器，端口为 11883。

## 设置第二个集群（cluster-eu-west）

使用以下配置片段在 `cluster-eu-west` 集群的配置文件中设置第二个集群：

```bash
# Cluster Linking configuration
cluster {
  name = "cluster-eu-west"
  links = [
    {
      name = "cluster-us-east"
      server = "emqx.eu-west.myinfra.net:11883"
      clientid = "clink-eu-west"
      topics = ["#"]
    }
  ]
}

# Dedicated listener for Cluster Linking connections
listeners {
  tcp.clink {
    bind = 11883
  }
}
```

这个配置与第一个集群的配置是对称的。在两个配置都完成后，一旦集群启动运行，两个集群之间就会建立一个对称的、双向的集群连接（Cluster Link）。您还可以创建**非对称**连接，这将在后面介绍。

## 验证集群连接

要确认连接到不同集群的客户端现在可以使用标准的 MQTT 机制进行通信，可以使用 [MQTTX CLI](https://mqttx.app/zh/cli) 工具从一个集群发布消息，并从另一个集群订阅消息。

1. 在 `cluster-us-east` 上启动一个订阅者：

   ```bash
   mqttx sub -h emqx.us-east.myinfra.net --topic linked/# --qos 1 --verbose
   [6/4/2024] [3:53:32 PM] › …  连接中...
   [6/4/2024] [3:53:32 PM] › ✔  已连接
   [6/4/2024] [3:53:32 PM] › …  订阅 linked/#...
   [6/4/2024] [3:53:32 PM] › ✔  已订阅 linked/#
   ```

2. 从 `cluster-eu-west` 发布消息：

   ```bash
   mqttx pub -h emqx.eu-west.myinfra.net --topic linked/42 --message "Hello from the other side!"
   [6/4/2024] [3:53:35 PM] › …  连接中...
   [6/4/2024] [3:53:35 PM] › ✔  已连接
   [6/4/2024] [3:53:35 PM] › …  正在发布消息...
   [6/4/2024] [3:53:35 PM] › ✔  已发布消息
   ```

3. 可以看到订阅者接收到消息：

   ```bash
   [6/4/2024] [3:53:35 PM] › 主题: linked/42
   内容: Hello from the other side!
   ```

4. 反方向重复该过程：

   - 在 `cluster-eu-west` 上启动一个订阅者：

     ```bash
     mqttx sub -h emqx.eu-west.myinfra.net --topic linked/# --qos 1 --verbose
     [6/4/2024] [3:54:12 PM] › …  连接中...
     [6/4/2024] [3:54:12 PM] › ✔  已连接
     [6/4/2024] [3:54:12 PM] › …  订阅 linked/#...
     [6/4/2024] [3:54:12 PM] › ✔  已订阅 linked/#
     ```

   - 从 `cluster-us-east` 发布消息：

     ```bash
     mqttx pub -h emqx.us-east.myinfra.net --topic linked/1 --message "Hello from US!"
     [6/4/2024] [3:54:15 PM] › …  连接中...
     [6/4/2024] [3:54:15 PM] › ✔  已连接
     [6/4/2024] [3:54:15 PM] › …  正在发布消息...
     [6/4/2024] [3:54:15 PM] › ✔  已发布消息
     ```

   - 可以看到到订阅者接收到消息：

     ```bash
     [6/4/2024] [3:54:15 PM] › 主题: linked/1
     内容: Hello from US!
     ```

很好！ 集群连接工作正常。

::: tip

集群连接涉及在集群之间传播订阅信息，这是一个异步过程。通常只需几毫秒，但如果你在订阅后立即发布消息，可能会导致消息传递有轻微的延迟。

:::

## 设置非对称连接

要创建一个非对称连接，需要稍微修改 `cluster-eu-west` 的配置：

```bash
cluster {
  name = "cluster-eu-west"
  links = [
    {
      name = "cluster-us-east"
      server = "emqx.eu-west.myinfra.net:11883"
      clientid = "clink-eu-west"
      topics = []
    }
  ]
}
```

这个配置与之前的几乎相同，除了 `topics` 字段为空。这意味着 `cluster-eu-west` 现在不对 `cluster-us-east`的任何消息感兴趣。这使得集群连接变得非对称，这对于在集群之间进行单向消息转发非常有用。

如果您重复上述的消息发布和订阅步骤，你会注意到从 `cluster-us-east` 发布的消息不会被 `cluster-eu-west`上的订阅者接收到。
