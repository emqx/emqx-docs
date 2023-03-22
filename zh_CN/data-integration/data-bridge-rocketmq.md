# RocketMQ

通过 RocketMQ 数据桥接可以将客户端消息和事件转发到 RocketMQ 中，例如，可以通过事件触发转发消息到 RocketMQ 中，从而实现对诸如设备在线状态、上下线历史等的记录。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

## 先决条件

- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

## 特性

- [连接池](./data-bridges.md#连接池)
- [异步请求模式](./data-bridges.md#异步请求模式)
- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)
- [SQL 预处理](./data-bridges.md#SQL-预处理)

## 快速开始

### 安装 RocketMQ

我们需要一份 docker compose 文件来部署 RocketMQ 

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

然后，准备运行 RocketMQ 所需的文件夹和配置文件

```bash
mkdir rocketmq
mkdir rocketmq/logs
mkdir rocketmq/store
mkdir rocketmq/conf
```
将下面的内容存入到 `rocketmq/conf/broker/conf` 中去

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

启动 RocketMQ

```bash
docker-compose -f rocketmq.yaml up
```

启动一个 RocketMQ 的消费者
```bash
docker run --rm -e NAMESRV_ADDR=host.docker.internal:9876 apache/rocketmq:4.9.4 ./tools.sh org.apache.rocketmq.example.quickstart.Consumer
```

*注意*: 如果是在 Linux 中，需要将 `host.docker.internal` 换成你的真实 IP 地址。

### 连接到 RocketMQ

接下来将会创建一个 RocketMQ 数据桥接和两条规则，来实现消息转发与事件记录：

#### 消息转发

本节我们将创建一个 RocketMQ 数据桥接来实现对客户端发布消息的转发。

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。
2. 点击页面右上角的创建。
3. 在数据桥接类型中选择 RocketMQ，点击下一步。
4. 输入数据桥接名称，要求是大小写英文字母或数字组合。
5. 输入 RocketMQ 连接信息，**Server** 地址填写 **127.0.0.1:9876**，**Topic** 填写 `TopicTest`，其他使用默认值即可。
6. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考[配置参数](#配置参数)。
7. 点击创建按钮完成数据桥接创建。

至此您已经完成数据桥接创建，接下来将继续创建一条规则来指定需要写入的数据：

1. 转到 Dashboard **数据集成** -> **规则**页面。
2. 点击页面右上角的创建。
3. 输入规则 ID `my_rule`，在 SQL 编辑器中输入规则，此处选择将 `t/#` 主题的 MQTT 消息转发至 RocketMQ，请确认规则选出的字段（SELECT 部分）包含所有 SQL 模板中用到的变量，此处规则 SQL 如下：

  ```sql
  SELECT 
    *
  FROM
    "t/#"
  ```

4. 添加动作，在动作下拉框中选择 使用数据桥接转发 选项，选择先前创建好的 RocketMQ 数据桥接。
5. 点击最下方创建按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果转发至 RocketMQ。

#### 上下线记录

在上面一个章节，我们实现了对指定主题消息的转发，本节我们将演示如何通过 RocketMQ 实现对设备上下线的记录。

注意：除 SQL 模板与规则外，其他操作步骤与[消息转发](#消息转发)章节完全相同。

这里为了演示方便，上下线消息也设置为复用 `TopicTest` 主题。

规则 SQL 如下：

```sql
SELECT
  *
FROM 
  "$events/client_connected", "$events/client_disconnected"
```

### 测试

使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello RocketMQ" }'
```

分别查看两个数据桥接运行统计，命中、发送成功次数均 +1。

在 RocketMQ 的消费者窗口，我们将看到下面的输出:
```bash
ConsumeMessageThread_please_rename_unique_group_name_4_1 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=581, queueOffset=0, sysFlag=0, bornTimestamp=1679037578889, bornHost=/172.26.83.106:43920, storeTimestamp=1679037578891, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000060E, commitLogOffset=1550, bodyCRC=7414108, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_2 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=511, queueOffset=1, sysFlag=0, bornTimestamp=1679037580174, bornHost=/172.26.83.106:43920, storeTimestamp=1679037580176, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F0000000000000E61, commitLogOffset=3681, bodyCRC=1604860416, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_3 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=458, queueOffset=2, sysFlag=0, bornTimestamp=1679037584933, bornHost=/172.26.83.106:43920, storeTimestamp=1679037584934, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000166E, commitLogOffset=5742, bodyCRC=383397630, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
```
