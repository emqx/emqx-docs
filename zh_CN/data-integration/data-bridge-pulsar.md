# Apache Pulsar

[Apache Pulsar](https://pulsar.apache.org/) 是一款流行的开源分布式事件流平台。EMQX 与 Apache Pulsar 的集成为用户在高吞吐量环境中提供可靠的数据传输和处理能力。

将数据流式传输至 Apache Pulsar 需要以生产者角色创建数据桥接（将消息发送至 Pulsar）。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

::: tip 前置准备

- 了解[规则](./rules.md)。
- 了解[数据桥接](./data-bridges.md)。

:::

## 功能清单

- [异步请求模式](./data-bridges.md#异步请求模式)
- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)

## 快速开始

本节将带您创建一个 Pulsar 服务器以及 Pulsar 主题，然后在 EMQX 创建 Pulsar 生产者的数据桥接，之后再创建一条规则来将数据转发至 Pulsar，以验证该数据桥接是否正常工作。

本教程假定 EMQX 与 Pulsar 均在本地运行，如您在远程运行 EMQX 及 Pulsar，请根据实际情况调整相应配置。

### 安装 Pulsar 服务器

在 Docker 中运行 Pulsar。

```bash
docker run --rm -it -p 6650:6650 --name pulsar apachepulsar/pulsar:2.11.0 bin/pulsar standalone -nfw -nss
```

具体的操作步骤可参阅 [Quick Start section in Pulsar Documentation](https://pulsar.apache.org/docs/2.11.x/getting-started-home/)。

### 创建 Pulsar 主题

在 EMQX 中创建数据桥接之前需要先创建相关的 Pulsar 主题。在 Pulsar 的 `public` 租户、`default` 命名空间下创建名为 `my-topic` 的主题，并指定 1 个分区。使用以下命令创建主题 `my-topic`：

```bash
docker exec -it pulsar bin/pulsar-admin topics create-partitioned-topic persistent://public/default/my-topic -p 1
```

### 创建 Pulsar 数据桥接

本节将演示如何通过 Dashboard 创建一个 Pulsar 生产者数据桥接。

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。
2. 点击页面右上角的**创建**。
3. 在**数据桥接类型**中选择 **Pulsar**，点击**下一步**。
4. 在**桥接角色**中选择**生产者**。 

   - 填写必填信息（标星号字段）。
   - 输入数据桥接名称，要求是大小写英文字母和数字的组合。

   - 输入 Pulsar 连接信息，**主机列表**填写 `pulsar://localhost:6650`，其他参数根据实际情况填写。

   - **源 MQTT 主题**：选择要为其建立桥接的 MQTT 主题，此处填写 `t/#` 表示将匹配此主题的 MQTT 消息转发至 Pulsar。您也可以选择将此项留空，通过[新建规则](#创建-pulsar-生产者数据转发规则)指定发往 Pulsar 的数据。

   - **Pulsar 主题名称**：填写 Pulsar 中预先创建好的主题 `persistent://public/default/my-topic`，此处暂不支持使用变量。

   - **消息键**：Pulsar 消息键，此处填写字符串或者包含占位符（ ${var}）的字符串。

   - **消息值**：Pulsar 消息值，此处填写字符串或者包含占位符（ ${var}）的字符串。

   - 高级配置（可选）：根据情况配置**最大批量字节数**、**压缩**、**分区选择策略**等参数。
5. 点击**创建**前，您可点击**测试连接**按钮确保能连接到 Pulsar 服务器。
6. 点击**创建**，将提示是否使用该数据桥接创建规则。点击**创建规则**，详细步骤参阅[创建 Pulsar 生产者数据转发规则](#创建-pulsar-生产者数据转发规则)。


::: tip

创建关联的规则可以通过规则进一步处理 Pulsar 消息，然后再发送到 MQTT 客户端。有关创建规则的更多信息，请参阅[规则引擎](./rules.md)。

:::

至此，您已经完成数据桥接的创建，在 Dashboard 的数据桥接页面，可以看到 Pulsar 数据桥接的状态为**已连接**。

### 创建 Pulsar 生产者数据转发规则

至此您已经完成数据桥接创建流程，接下来将继续创建一条规则来指定需要写入的数据：

1. 转到 Dashboard **数据集成** -> **规则页面**。

2. 点击页面右上角的创建。

3. 输入规则 ID，例如  `my_rule`。

4. 在 SQL 编辑器中输入规则，例如我们希望将 `t/#` 主题的 MQTT 消息存储至 Pulsar，可通过如下规则实现：

   注意：如果要自定义 SQL 语句，请确保 `SELECT` 字段包含数据桥接中所需的所有字段。


```sql
SELECT
  *
FROM
  "t/#"
```

5. 点击**添加动作**按钮，在下拉框中选择**使用数据桥接转发**选项，选择先前创建好的 Pulsar 数据桥接。

6. 点击**添加**按钮确认添加动作。

7. 点击最下方**创建**按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 Pulsar 进行存储。

### 测试桥接和规则

 使用 MQTTX 向 `t/1` 主题发布消息：

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Pulsar" }'
```

查看数据桥接运行统计，命中、发送成功次数应当 +1。

通过 Pulsar 命令查看 `persistent://public/default/my-topic` 主题是否写入消息：

   ```bash
docker exec -it pulsar bin/pulsar-client consume -n 0 -s mysubscriptionid -p Earliest persistent://public/default/my-topic
   ```

