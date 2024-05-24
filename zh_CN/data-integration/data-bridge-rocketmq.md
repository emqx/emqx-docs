# 将 MQTT 数据传输到 RocketMQ

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

通过 [RocketMQ](https://rocketmq.apache.org/) 数据集成可以将 MQTT 消息和客户端事件转发到 RocketMQ 中。例如，可以通过事件触发转发消息到 RocketMQ 中，从而实现对诸如设备在线状态、上下线历史等的记录。

本页详细介绍了 EMQX 与 RocketMQ 的数据集成并提供了实用的规则和 Sink 创建指导。

::: tip 注意

此数据集成在使用阿里云托管的 RockstMQ 服务时不支持批量模式。

:::

## 工作原理

RocketMQ 数据集成是 EMQX 中的一个开箱即用功能，它结合了 EMQX 的设备接入以及实时数据捕获和传输能力与 RocketMQ 强大的消息队列处理能力。通过内置的[规则引擎](./rules.md)组件，该集成简化了将数据从 EMQX 引入到 RocketMQ 进行存储和管理的过程，无需复杂编码。

下图展示了 EMQX 与 RocketMQ 之间数据集成的典型架构:

![EMQX-RocketMQ 集成](./assets/emqx-integration-rocketmq.png)

将 MQTT 数据引入 RocketMQ 的过程如下：

1. **消息发布和接收**：工业物联网设备通过 MQTT 协议成功连接到 EMQX，并向 EMQX 发布实时 MQTT 数据。EMQX 收到这些消息后，将启动其规则引擎中的匹配过程。
2. **消息数据处理**：当消息到达时，它会经过规则引擎，然后由 EMQX 中定义的规则处理。这些规则基于预定义的标准，确定哪些消息需要路由到 RocketMQ。如果任何规则指定了有效载荷转换，那么将应用这些转换，例如转换数据格式、过滤特定信息或用额外的上下文丰富有效载荷。
3. **数据传入到 RocketMQ**：一旦规则处理了消息，它就会触发一个动作，将消息转发到 RocketMQ。处理后的数据将无缝写入 RocketMQ。
4. **数据存储和利用**：现在数据存储在 RocketMQ 中，企业可以利用其查询能力应用于各种用例。例如，在金融行业，RocketMQ 可以用作可靠的高性能消息队列来存储和管理来自支付终端、交易系统的数据，并将消息连接到数据分析和监管平台，实现风险管理、欺诈检测和预防、监管合规等要求。

## 特性与优势

RocketMQ 数据集成为您的业务带来了以下功能和优势：

- **可靠的物联网数据消息传递**：EMQX 能够可靠地批处理并发送 MQTT 消息到 RocketMQ，实现物联网设备与 RocketMQ 及应用系统的集成。
- **MQTT 消息转换**：使用规则引擎，EMQX 可以过滤和转换 MQTT 消息。消息在发送到 RocketMQ 之前，可以进行数据提取、过滤、丰富和转换。
- **云原生弹性扩展**：EMQX 与 RocketMQ 都是基于云原生构建的应用，提供了友好的 K8s 支持以及云原生生态集成，能够无限弹性扩缩以适应业务的快速发展。
- **灵活的主题映射**：RocketMQ 数据集成支持将 MQTT 主题灵活映射到 RocketMQ 主题，允许轻松配置 RocketMQ 消息中的键（Key）和值（Value）。
- **高吞吐量场景下的处理能力**：RocketMQ 数据集成支持同步和异步写入模式，允许根据不同场景灵活平衡延迟和吞吐量。

## 准备工作

本节介绍了在 EMQX 中创建 RocketMQ 数据集成之前需要做的准备工作，包括如何设置 RocketMQ 服务器。

### 前置准备

- 了解 EMQX Sink [规则](./rules.md)。
- 了解[数据集成](./data-bridges.md)。

### 安装 RocketMQ

1. 准备一份 docker-compose 文件 `rocketmq.yaml` 来部署 RocketMQ。

```yaml
version: '3.9'

services:
  mqnamesrv:
    image: apache/rocketmq:4.9.4
    container_name: rocketmq_namesrv
    ports:
      - 9876:9876
    volumes:
      - ./rocketmq/logs:/opt/logs
      - ./rocketmq/store:/opt/store
    command: ./mqnamesrv

  mqbroker:
    image: apache/rocketmq:4.9.4
    container_name: rocketmq_broker
    ports:
      - 10909:10909
      - 10911:10911
    volumes:
      - ./rocketmq/logs:/opt/logs
      - ./rocketmq/store:/opt/store
      - ./rocketmq/conf/broker.conf:/etc/rocketmq/broker.conf
    environment:
        NAMESRV_ADDR: "rocketmq_namesrv:9876"
        JAVA_OPTS: " -Duser.home=/opt"
        JAVA_OPT_EXT: "-server -Xms1024m -Xmx1024m -Xmn1024m"
    command: ./mqbroker -c /etc/rocketmq/broker.conf
    depends_on:
      - mqnamesrv
```

2. 准备运行 RocketMQ 所需的文件夹和配置文件。

```bash
mkdir rocketmq
mkdir rocketmq/logs
mkdir rocketmq/store
mkdir rocketmq/conf
```

3. 将下面的内容存入到 `rocketmq/conf/broker.conf` 文件中。

```bash
brokerClusterName=DefaultCluster
brokerName=broker-a
brokerId=0

brokerIP1=这里需要填写你的真实 IP 地址

defaultTopicQueueNums=4
autoCreateTopicEnable=true
autoCreateSubscriptionGroup=true

listenPort=10911
deleteWhen=04

fileReservedTime=120
mapedFileSizeCommitLog=1073741824
mapedFileSizeConsumeQueue=300000
diskMaxUsedSpaceRatio=100
maxMessageSize=65536

brokerRole=ASYNC_MASTER

flushDiskType=ASYNC_FLUSH

```

4. 启动 RocketMQ。

```bash
docker-compose -f rocketmq.yaml up
```

5. 启动一个 RocketMQ 的消费者。

```bash
docker run --rm -e NAMESRV_ADDR=host.docker.internal:9876 apache/rocketmq:4.9.4 ./tools.sh org.apache.rocketmq.example.quickstart.Consumer
```

::: tip 注意

如果是在 Linux 中，需要将 `host.docker.internal` 替换成您的真实 IP 地址。

:::

## 创建连接器

在创建 RocketMQ Sink 之前，您需要创建一个 RocketMQ 连接器，以便 EMQX 与 RocketMQ 服务建立连接。以下示例假定您在本地机器上同时运行 EMQX 和 RocketMQ。如果您在远程运行 RocketMQ 和 EMQX，请相应地调整设置。

1. 转到 Dashboard **集成** -> **连接器** 页面。点击页面右上角的**创建**。
2. 在连接器类型中选择 **RocketMQ**，点击**下一步**。
3. 在 **配置** 步骤，配置以下信息：

   - **连接器名称**：应为大写和小写字母及数字的组合，例如：`my_rocketmq`。
   - **服务器列表**：输入 `127.0.0.1:9876`。
   - **命名空间**：此处留空。如果您的 RocketMQ 服务配置了命名空间，则必须填写此项。对于阿里云的 RocketMQ 服务来说，命名空间就是实例 ID。
   - **Accesskey**、**Secretkey**与**安全令牌**：此处留空，根据您的 RocketMQ 实际配置填写。
4. 高级配置（可选）：详细请参考 [Sink 的特性](./data-bridges.md#sink-的特性)。
5. 在点击**创建**之前，您可以点击**测试连接**来测试连接器是否能连接到 RocketMQ 服务器。
6. 点击**创建**按钮完成连接器创建。
7. 在弹出的**创建成功**对话框中您可以点击**创建规则**，继续创建规则以指定需要写入 RocketMQ 的数据和需要记录的客户端事件。您也可以按照[创建消息存储 Sink 规则](#创建消息存储-sink-规则)和[创建事件记录 Sink 规则](#创建事件记录-sink-规则)章节的步骤来创建规则。

## 创建消息存储 Sink 规则

本节演示了如何在 Dashboard 中创建一条规则，用于处理来自源 MQTT 主题 `t/#` 的消息，并通过配置的 Sink 将处理后的数据转发到 RocketMQ 的主题 `TopicTest`。

1. 转到 Dashboard **集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 输入规则 ID `my_rule`。如需实现对指定主题消息的转发，例如将 `t/#` 主题的 MQTT 消息转发至 RocketMQ，在 **SQL 编辑器**中输入以下 SQL 语法：

   注意：如果您希望制定自己的 SQL 语法，需要确保规则选出的字段（SELECT 部分）包含所有 SQL 模板中用到的变量。

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

   ::: tip

   如果您初次使用 SQL，可以点击 **SQL 示例**和**启用调试**来学习和测试规则 SQL 的结果。

   :::

4. 点击右侧的**添加动作**按钮，为规则在被触发的情况下指定一个动作。通过这个动作，EMQX 会将经规则处理的数据发送到 RocketMQ。

5. 在**动作类型**下拉框中选择 `RocketMQ`，保持**动作**下拉框为默认的`创建动作`选项，您也可以选择一个之前已经创建好的 RocketMQ Sink。此处我们创建一个全新的 Sink 并添加到规则中。

6. 输入 Sink 名称，名称应为大/小写字母和数字的组合。

7. 从**连接器**下拉框中选择刚刚创建的 `my_rocketmq`。您也可以通过点击下拉框旁边的按钮创建一个新的连接器。有关配置参数，请参见[创建连接器](#创建连接器)。

7. **RocketMQ 主题**：输入 `TopicTest`。

8. **消息模版**设置为默认值，即为空；模版为空时将会将整个消息转发给 RocketMQ，实际值为 JSON 模版数据。

9. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细内容请参考 [Sink 的特性](./data-bridges.md#sink-的特性)中的配置参数章节。

10. 在完成创建之前，您可以点击**测试连接**来测试 Sink 可以连接到 RockeMQ 服务器。

11. 点击**添加**按钮完成 Sink 创建，新建的 Sink 将被添加到**动作输出**列表中。

12. 回到创建规则页面，对配置的信息进行确认，点击**创建**。一条规则应该出现在规则列表中。

现在您已成功创建了通过 RocketMQ Sink 将数据转发到 RocketMQ 的规则，同时在**规则**页面的**动作(Sink)** 标签页看到新建的 RocketMQ Sink。

您还可以点击 **集成** -> **Flow 设计器**可以查看拓扑，通过拓扑可以直观的看到，主题 `t/#` 下的消息在经过规则 `my_rule` 解析后被发送到 RocketMQ 中。

## 创建事件记录 Sink 规则

本节展示如何创建用于记录客户端上/下线状态的规则，并通过配置的 Sink 将事件记录数据转发到 RocketMQ 主题 `TopicTest` 中。

::: tip

为了演示方便，上下线消息也设置为复用 `TopicTest` 主题。

:::

创建规则的步骤与[创建消息存储 Sink 规则](#创建消息存储-sink-规则)中的步骤相似，不同之处在于 SQL 规则语法和 SQL 模板。

客户端上/下线状态记录的 SQL 规则语法如下所示：

```sql
SELECT
  *
FROM
  "$events/client_connected", "$events/client_disconnected"
```

## 测试规则


使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello RocketMQ" }'
```

分别查看两个 Sink 运行统计，命中、发送成功次数均 +1。触发上下线事件，命中及发送成功次数会 +2。

查看数据是否被转发到了 `TopicTest` 主题。

在 RocketMQ 的消费者窗口，我们将看到下面的输出:
```bash
ConsumeMessageThread_please_rename_unique_group_name_4_1 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=581, queueOffset=0, sysFlag=0, bornTimestamp=1679037578889, bornHost=/172.26.83.106:43920, storeTimestamp=1679037578891, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000060E, commitLogOffset=1550, bodyCRC=7414108, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_2 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=511, queueOffset=1, sysFlag=0, bornTimestamp=1679037580174, bornHost=/172.26.83.106:43920, storeTimestamp=1679037580176, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F0000000000000E61, commitLogOffset=3681, bodyCRC=1604860416, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_3 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=458, queueOffset=2, sysFlag=0, bornTimestamp=1679037584933, bornHost=/172.26.83.106:43920, storeTimestamp=1679037584934, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000166E, commitLogOffset=5742, bodyCRC=383397630, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
```
