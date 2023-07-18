# RocketMQ

通过 RocketMQ 数据桥接可以将 MQTT 消息和客户端事件转发到 RocketMQ 中。例如，可以通过事件触发转发消息到 RocketMQ 中，从而实现对诸如设备在线状态、上下线历史等的记录。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

## 前置条件

- 了解 EMQX 数据桥接[规则](./rules.md)。
- 了解[数据桥接](./data-bridges.md)。

## 特性

- [连接池](./data-bridges.md#连接池)
- [异步请求模式](./data-bridges.md#异步请求模式)
- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)
- [SQL 预处理](./data-bridges.md#sql-预处理)

## 快速开始教程

本节介绍如何配置 RocketMQ 数据桥接，包括如何设置 RocketMQ 服务器、创建数据桥接和规则以将数据转发到 RocketMQ、以及如何测试数据桥接和规则。

本教程假定您在本地机器上同时运行 EMQX 和 RocketMQ。如果您在远程运行 RocketMQ 和 EMQX，请相应地调整设置。

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

### 创建 RocketMQ 数据桥接

本节我们将创建一个 RocketMQ 数据桥接来实现对客户端发布消息的转发。

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。

2. 点击页面右上角的**创建**。

3. 在数据桥接类型中选择 **RocketMQ**，点击**下一步**。

4. 输入数据桥接名称，要求是大小写英文字母和数字组合。

5. 输入 RocketMQ 连接信息，**服务器地址**填写 `127.0.0.1:9876`，**RocketMQ 主题**填写 `TopicTest`，其他使用默认值即可。

6. **模版**设置为默认值，即为空；模版为空时将会将整个消息转发给 RocketMQ，实际值为 JSON 模版数据。

7. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考[数据桥接简介](./data-bridges.md)中的配置参数章节。

8. 在完成创建之前，您可以点击**测试连接**来测试桥接可以连接到 RockeMQ 服务器。

9. 点击**创建**按钮完成数据桥接创建。

   在弹出的**创建成功**对话框中您可以点击**创建规则**，继续创建规则以指定需要写入 RocketMQ 的数据。您也可以按照[创建 RocketMQ 数据桥接规则](#创建-rocketmq-数据桥接规则)章节的步骤来创建规则。

### 创建 RocketMQ 数据桥接规则

至此您已经完成数据桥接创建，接下来将继续创建一条规则来指定需要写入的数据。您需要为消息转发和设备上下线记录创建两条不同的规则。

1. 转到 Dashboard **数据集成** -> **规则**页面。

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

4. 点击**添加动作**，在动作下拉框中选择**使用数据桥接转发**选项，选择先前创建好的 RocketMQ 数据桥接。点击**添加**。

6. 点击最下方**创建**按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果转发至 RocketMQ。

### 测试桥接和规则


使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello RocketMQ" }'
```

分别查看两个数据桥接运行统计，命中、发送成功次数均 +1。

查看数据是否被转发到了 `TopicTest` 主题。

在 RocketMQ 的消费者窗口，我们将看到下面的输出:
```bash
ConsumeMessageThread_please_rename_unique_group_name_4_1 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=581, queueOffset=0, sysFlag=0, bornTimestamp=1679037578889, bornHost=/172.26.83.106:43920, storeTimestamp=1679037578891, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000060E, commitLogOffset=1550, bodyCRC=7414108, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_2 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=511, queueOffset=1, sysFlag=0, bornTimestamp=1679037580174, bornHost=/172.26.83.106:43920, storeTimestamp=1679037580176, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F0000000000000E61, commitLogOffset=3681, bodyCRC=1604860416, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_3 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=458, queueOffset=2, sysFlag=0, bornTimestamp=1679037584933, bornHost=/172.26.83.106:43920, storeTimestamp=1679037584934, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000166E, commitLogOffset=5742, bodyCRC=383397630, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
```
