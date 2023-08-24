# Azure Event Hubs

[Azure Event Hub](https://azure.microsoft.com/en-us/products/event-hubs) 是一个用于数据摄取的实时托管事件流平台。EMQX 与 Azure Event Hub 的集成为用户在高吞吐量情况下提供了可靠的数据传输和处理能力。目前，EMQX 支持使用 SASL/PLAIN 身份验证、通过与 Kafka 协议兼容的 Apache Kafka 终端点进行 Azure Event Hub 集成。

{% emqxce %}
::: tip

EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。

:::
{% endemqxce %}

::: tip 前置准备

- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

:::

## 功能清单

- [异步请求模式](./data-bridges.md)
- [批量模式](./data-bridges.md)
- [缓存队列](./data-bridges.md)

## 快速开始

本节介绍了如何将数据通过数据桥接流入或流出 Azure Event Hubs，涵盖了诸如如何设置 Azure Event Hub、如何创建桥接器和转发数据到桥接器的规则，以及如何测试数据桥接器和规则等主题。

### 设置 Azure Event Hubs

为了使用 Azure Event Hub 数据集成，必须在 Azure 账户中设置命名空间和事件中心。以下链接指向官方文档，详细介绍了如何进行设置。

- [什么是适用于 Apache Kafka 的 Azure 事件中心](https://learn.microsoft.com/zh-cn/azure/event-hubs/azure-event-hubs-kafka-overview)
- [快速入门：使用 Azure 门户创建事件中心](https://learn.microsoft.com/zh-cn/azure/event-hubs/event-hubs-create)
- [快速入门：使用 Azure 事件中心和 Apache Kafka 流式传输数据](https://learn.microsoft.com/zh-cn/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - 遵循“连接字符串”说明，这是 EMQX 用于连接的方式。
- [获取事件中心连接字符串](https://learn.microsoft.com/zh-cn/azure/event-hubs/event-hubs-get-connection-string)

### 创建 Azure Event Hubs 数据桥接

本节演示如何通过 Dashboard 创建 Azure Event Hubs 生产者数据桥接。

1. 进入 EMQX Dashboard，点击**集成** -> **数据桥接**。

2. 点击页面右上角的**创建**。

3. 在**创建数据桥接**页面，点击选择 **Azure Event Hubs**，然后点击**下一步**。

4. 为数据桥接输入一个名称。名称应为大小写字母和数字的组合。

5. 配置连接信息。

   - **引导主机**：输入命名空间的主机名。默认端口为 `9093`。其他字段按实际情况设置。
   - **连接字符串**：输入命名空间的连接字符串。可以在命名空间共享访问策略的“连接字符串 - 主键”中找到。有关详细信息，请参阅 [获取事件中心连接字符串](https://learn.microsoft.com/zh-cn/azure/event-hubs/event-hubs-get-connection-string)。
   - **启用 TLS**：连接到 Azure Event Hub 时默认启用 TLS。有关 TLS 连接选项的详细信息，请参阅 [外部资源访问的 TLS](../network/overview.md#启用-tls-加密访问外部资源)。

6. 配置数据桥接信息。

   - **事件中心名称**：输入要使用的事件中心的名称。注意：此处不支持变量。
   - **Azure Event Hub 头部**：
   - **Azure Event Hub 头部值编码模式**：
   - **额外的 Azure Event Hub 头部信息**：
   - **消息键**：事件中心消息键。在此处插入一个字符串，可以是纯字符串或包含占位符（${var}）的字符串。
   - **消息值**：事件中心消息值。在此处插入一个字符串，可以是纯字符串或包含占位符（${var}）的字符串。
   - **消息时间戳**：指定要使用的时间戳类型。

7. 高级设置（可选）：根据业务需求设置 **最大批次字节数**、**所需确认** 和 **分区策略**等。

8. 在点击**创建**之前，您可以点击**测试连接**测试桥接是否能够连接到 Azure Event Hub 服务器。

9. 点击**创建**，系统将提示您创建关联规则。

   对于 Azure Event Hub 生产者数据桥接，点击**创建规则**创建关联规则。有关详细操作步骤，请参阅 [为 Azure Event Hub 生产者数据桥接创建规则](#创建-azure-event-hubs-生产者数据转发规则)。

   ::: tip

   创建规则允许对与规则匹配的 Azure Event Hub 消息进行进一步的转换和过滤，然后将其转发到其他规则操作，如不同的桥接。有关创建规则的更多信息，请参阅[规则](./rules.md)。

   :::

现在，Azure Event Hubs 数据桥接应该在数据桥接列表（**集成** -> **数据桥接**）中显示，**资源状态**为 **已连接**。

### 创建 Azure Event Hubs 生产者数据转发规则

至此您已经完成数据桥接创建流程，接下来将继续创建一条规则来指定需要写入的数据：

1. 转到 Dashboard **数据集成** -> **规则页面**。

2. 点击页面右上角的创建。

3. 输入规则 ID，例如  `my_rule`。

4. 在 SQL 编辑器中输入规则，例如我们希望将 `t/#` 主题的 MQTT 消息存储至 Azure Event Hub，可通过如下规则实现：

   注意：如果要自定义 SQL 语句，请确保 `SELECT` 字段包含数据桥接中所需的所有字段。


```sql
SELECT
  *
FROM
  "t/#"
```

5. 点击**添加动作**按钮，在下拉框中选择**使用数据桥接转发**选项，选择先前创建好的 Azure Event Hubs 数据桥接。

6. 点击**添加**按钮确认添加动作。

7. 点击最下方**创建**按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 Azure Event Hub 进行存储。

### 测试数据桥接和规则

使用 MQTTX 向 `t/1` 主题发布消息：

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

查看数据桥接运行统计，命中、发送成功次数应当 +1。

在 Azure 门户仪表板中检查是否将消息写入配置的事件中心。
