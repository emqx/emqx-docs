# Apache Pulsar

[Apache Pulsar](https://pulsar.apache.org/) 是一款流行的开源分布式事件流平台，专为处理实时数据流在应用程序和系统之间的传输而设计。Apache Pulsar 具有更高的可伸缩性，并提供了更快的吞吐量和更低的延迟。在物联网应用中，设备生成的数据通常通过轻量级的 MQTT 协议进行传输，用户可以轻松地将 MQTT 数据传入 Apache Pulsar，并与其他数据系统连接，实现对物联网设备生成的数据进行实时处理、存储和分析。

本页详细介绍了 EMQX 与 Apache Pulsar 的数据集成并提供了实用的规则和数据桥接创建指导。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

## 工作原理

Apache Pulsar 数据桥接是 EMQX 的开箱即用功能，旨在弥合基于 MQTT 的物联网数据与 Pulsar 强大数据处理能力之间的差距。借助内置的规则引擎组件，数据流传输和处理过程在两个平台之间更加简化。这意味着您可以轻松地将 MQTT 数据传输到 Pulsar，并利用 Pulsar 的强大功能进行数据处理，而无需额外的开发工作，使得物联网数据的管理和利用变得更加高效和方便。

EMQX 通过规则引擎与数据桥接将 MQTT 数据转发至 Apache Pulsar，其完整流程如下：

1. **物联网设备发布消息**：设备通过特定的主题发布遥测和状态数据，EMQX 接收到消息后将在规则引擎中进行比对。
3. **规则引擎处理消息**：通过内置的规则引擎，可以根据主题匹配处理特定来源的 MQTT 消息。规则引擎会匹配对应的规则，并对消息进行处理，例如转换数据格式、过滤掉特定信息或使用上下文信息丰富消息。
4. **桥接到 Apache Pulsar**：规则触发将消息转发到 Pulsar 的操作，允许轻松配置数据到 Pulsar 消息的键（Key）和值（Value），以及 MQTT 主题到 Pulsar 主题的映射关系，以便更好地组织和标识数据，方便后续的数据处理和分析。

MQTT 消息数据写入到 Apache Pulsar 后，您可以进行灵活的应用开发，例如：

1. 实时数据同步：编写 Pulsar 消费者应用程序来订阅并处理这些消息，根据业务需求，将 MQTT 数据与其他数据源进行关联、聚合或转换，实现实时的数据同步和整合。

2. 跨平台事件触发：接收到特定的 MQTT 消息时，可以使用 Pulsar 的规则引擎组件触发相应的操作或事件，实现跨系统和应用的事件驱动功能。

3. 实时监控和警报：在 Pulsar 中实时分析 MQTT 数据流，检测异常或特定的事件模式，并基于这些情况触发警报通知或执行相应的操作。

4. 数据聚合和分析：将来自多个 MQTT 主题的分散数据集中到一个统一的数据流中，并利用 Pulsar 的计算功能进行实时的聚合、计算和分析，以获得更全面的数据洞察。

## 桥接准备

本节将带您创建一个 Pulsar 服务器以及对应的 Pulsar 主题，以便后续的数据桥接和规则创建。此处假定 EMQX 与 Pulsar 均在本地运行，如您在远程运行 EMQX 及 Pulsar，请根据实际情况调整相应配置。

### 先决条件

- 了解[规则](./rules.md)。
- 了解[数据桥接](./data-bridges.md)。

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

## 创建 Pulsar 数据桥接

本节将演示如何通过 Dashboard 创建一个 Pulsar 生产者数据桥接，并验证数据桥接是否正常工作。

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
