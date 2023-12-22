# 将 MQTT 数据传输到 RocketMQ

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

通过 [RocketMQ](https://rocketmq.apache.org/) Sink 可以将 MQTT 消息和客户端事件转发到 RocketMQ 中。例如，可以通过事件触发转发消息到 RocketMQ 中，从而实现对诸如设备在线状态、上下线历史等的记录。

本页详细介绍了 EMQX 与 RocketMQ 的数据集成并提供了实用的规则和 Sink 创建指导。

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
- **灵活的主题映射**：RocketMQ 数据桥支持将 MQTT 主题灵活映射到 RocketMQ 主题，允许轻松配置 RocketMQ 消息中的键（Key）和值（Value）。
- **高吞吐量场景下的处理能力**：RocketMQ 数据桥支持同步和异步写入模式，允许根据不同场景灵活平衡延迟和吞吐量。

## 准备工作

本节介绍了在 EMQX 中创建 RocketMQ Sink 之前需要做的准备工作，包括如何设置 RocketMQ 服务器。

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

本节我们将创建一个 (.+) 连接器来实现对客户端发布消息的转发。以下步骤假定您在本地机器上同时运行 EMQX 和 RocketMQ。如果您在远程运行 RocketMQ 和 EMQX，请相应地调整设置。

1. 转到 Dashboard **集成** -> **连接器**页面。

2. 点击页面右上角的**创建**。

3. 在连接器类型中选择 **RocketMQ**，点击**下一步**。

4. 输入连接器名称，要求是大小写英文字母和数字组合。

5. 输入 RocketMQ 连接信息，**服务器地址**填写 `127.0.0.1:9876`，**RocketMQ 主题**填写 `TopicTest`，其他使用默认值即可。

6. **模版**设置为默认值，即为空；模版为空时将会将整个消息转发给 RocketMQ，实际值为 JSON 模版数据。

7. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考[ Sink 简介](./data-bridges.md)中的配置参数章节。

8. 在完成创建之前，您可以点击**测试连接**来测试桥接可以连接到 RockeMQ 服务器。

9. 点击**创建**按钮完成 Sink 创建。

   在弹出的**创建成功**对话框中您可以点击**创建规则**，继续创建规则以指定需要写入 RocketMQ 的数据。您也可以按照[创建 RocketMQ Sink 规则](#创建-rocketmq- Sink 规则)章节的步骤来创建规则。

## 创建 RocketMQ Sink 规则

至此您已经完成 Sink 创建，接下来将继续创建一条规则来指定需要写入的数据。您需要为消息转发和设备上下线记录创建两条不同的规则。

1. 转到 Dashboard **集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 输入规则 ID `my_rule`，在 **SQL 编辑器**中根据业务实现需要输入规则：


   - 如需实现对指定主题消息的转发，例如将 `t/#` 主题的 MQTT 消息转发至 RocketMQ，输入以下 SQL 语法：

     注意：如果您希望制定自己的 SQL 语法，需要确保规则选出的字段（SELECT 部分）包含所有 SQL 模板中用到的变量。

     ```sql
     SELECT 
       *
     FROM
       "t/#"
     ```

   - 如需实现设备上下线记录，输入以下 SQL 语法：

     ```sql
     SELECT
       *
     FROM 
       "$events/client_connected", "$events/client_disconnected"
     ```
     
     ::: tip
     
     为了演示方便，上下线消息也设置为复用 `TopicTest` 主题。
     
     :::

4. 点击**添加动作**，从**动作类型**下拉列表中选择 RocketMQ，从**动作**下拉框中选择刚刚创建的连接器，点击**添加**按钮将其添加到规则中。。

6. 点击最下方**创建**按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **集成** -> **Flow 设计器** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果转发至 RocketMQ。

## 测试桥接和规则


使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello RocketMQ" }'
```

分别查看两个 Sink 运行统计，命中、发送成功次数均 +1。

查看数据是否被转发到了 `TopicTest` 主题。

在 RocketMQ 的消费者窗口，我们将看到下面的输出:
```bash
ConsumeMessageThread_please_rename_unique_group_name_4_1 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=581, queueOffset=0, sysFlag=0, bornTimestamp=1679037578889, bornHost=/172.26.83.106:43920, storeTimestamp=1679037578891, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000060E, commitLogOffset=1550, bodyCRC=7414108, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_2 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=511, queueOffset=1, sysFlag=0, bornTimestamp=1679037580174, bornHost=/172.26.83.106:43920, storeTimestamp=1679037580176, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F0000000000000E61, commitLogOffset=3681, bodyCRC=1604860416, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_3 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=458, queueOffset=2, sysFlag=0, bornTimestamp=1679037584933, bornHost=/172.26.83.106:43920, storeTimestamp=1679037584934, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000166E, commitLogOffset=5742, bodyCRC=383397630, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
```
