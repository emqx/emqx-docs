# Amazon Kinesis

EMQX 支持与 [Amazon Kinesis Data Streams](https://aws.amazon.com/kinesis/data-streams/) 无缝集成，从而能进一步与其他 AWS 服务集成，实时提取、处理和分析 MQTT 数据。

{% emqxce %}
::: tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

:::tip 前置准备

- 了解[规则](./rules.md)。
- 了解[数据桥接](./data-bridges.md)。

:::

## 功能清单

- [连接池](./data-bridges.md#连接池)
- [批量模式](./data-bridges.md)
- [缓存队列](./data-bridges.md)

## 快速开始教程

本节介绍如何配置 Amaxon Kinesis 数据桥接，包括如何设置 Kinesis 服务、创建数据桥接和转发数据到 Kinesis Data Stream 的规则以及测试数据桥接和规则等主题。

### 在 Amazon Kinesis Data Streams 中创建数据流

按照以下步骤通过 AWS 管理控制台创建数据流（详细信息请参阅[本教程](https://docs.aws.amazon.com/zh_cn/streams/latest/dev/how-do-i-create-a-stream.html)）。

1. 登录 AWS 管理控制台并打开 [Kinesis 控制台](https://console.aws.amazon.com/kinesis)。
2. 在导航栏中，展开区域选择器并选择一个区域。
3. 选择**创建数据流**。
4. 在**创建 Kinesis 流**页面，为您的数据流输入名称，然后选择**按需**容量模式。

### 在本地模拟 Amazon Kinesis Data Streams

为了便于开发和测试，您可以通过 [LocalStack](https://localstack.cloud/) 在本地模拟 Amazon Kinesis Data Streams 服务。有了 LocalStack，您可以在本地机器上运行 AWS 应用，无需连接到远程云提供商。

1. 安装 LocalStack 并使用 Docker Image 运行：

   ```bash
   # To start the LocalStack docker image locally
   docker run --name localstack -p '4566:4566' -e 'KINESIS_LATENCY=0' -d localstack/localstack:2.1
   
   # Access the container
   docker exec -it localstack bash
   ```

2. 创建一个只有一个分片的流，名称设为 **my_stream**：

   ```bash
   awslocal kinesis create-stream --stream-name "my_stream" --shard-count 1
   ```

### 创建 Amazon Kinesis 数据桥接

1. 转到 EMQX Dashboard，点击**集成**->**数据桥接**。

2. 点击页面右上角的**创建**。

3. 在**创建数据桥接**页面，点击选择 **Amazon Kinesis**，然后点击**下一步**。

4. 为数据桥接输入一个名称。名称应为大写/小写字母和数字的组合。

5. 输入 Amazon Kinesis Data Streams 服务的连接信息：

   - **AWS 访问密钥 ID**：输入[访问密钥 ID](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html)。如果使用 LocalStack，可输入任何值。
   - **AWS 秘密访问密钥**：输入[密钥](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html)。如果使用 LocalStack，可输入任何值。
   - **Amazon Kinesis 端点**：输入 Kinesis 服务的[终端节点](https://docs.aws.amazon.com/zh_cn/general/latest/gr/ak.html)。如果使用 [LocalStack](#在本地模拟-amazon-kinesis-data-streams)，输入`http://localhost:4566`。
   - **AWS Kinesis 流**：输入您[在 Amazon Kinesis Data Streams 中创建数据流](#在-amazon-kinesis-data-streams-中创建数据流)中创建的数据流名称。
   - **分区键**：输入将与发送到此数据流的记录关联的分区键。允许使用 `${variable_name}` 形式的占位符（查看下一步以了解占位符示例）。

6. 在 **Payload Template** 字段中，将其留空或定义模板。

   - 如果留空，它将使用 JSON 格式编码 MQTT 消息中的所有可见输入，例如 clientid、topic、payload 等。
   - 如果使用定义的模板，`${variable_name}` 形式的占位符将使用 MQTT 上下文中的相应值进行填充。例如，如果 MQTT 消息主题是 `my/topic`，`${topic}` 将被替换为 `my/topic`。

7. 高级配置（可选），根据情况配置队列与批量等参数，详细请参考[数据桥接简介](./data-bridges.md)中的配置参数。

8. 在点击**创建**之前，您可以点击**测试连接性**以测试桥接。

9. 点击**创建**按钮完成数据桥接创建。

   在弹出的**创建成功**对话框中您可以点击**创建规则**，继续创建规则以指定需要写入 Amazon Kinesis 的数据。您也可以按照[创建 Amazon Kinesis 数据桥接规则](#创建-amazon-kinesis-数据桥接规则)中的步骤来创建规则。

### 创建 Amazon Kinesis 数据桥接规则

接下来您可以创建一条规则以指定需要写入 Amazon Kinesis 的数据。

1. 在 EMQX Dashboard 左侧导航栏中点击**集成** -> **规则 **.

2. 点击页面右上角的**创建**。

3. Input `my_rule` as the rule ID.

4. 输入规则 ID `my_rule`，在 **SQL 编辑器**中输入规则。例如将 `t/#` 主题的 MQTT 消息存储至 Amazon Kinesis Data Streams，需输入以下 SQL 语法：

   注意：如果您希望制定自己的 SQL 语法，需要确保规则选出的字段（`SELECT` 部分）包含所有 SQL 模板中用到的变量。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. 点击**添加动作**，在动作下拉框中选择**使用数据桥接转发**选项，选择先前创建好的 Amazon Kinesis 数据桥接。点击**添加**。

6. 点击最下方**创建**按钮完成规则创建。

至此您已经完成整个 Amazon Kinesis 数据桥接创建过程，可以前往 **集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 Amazon Kinesis 存储。

### 测试桥接和规则

1. 使用 MQTTX 向 `t/my_topic` 主题发布一条消息：

   ```bash
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

2. 查看 Amazon Kinesis 的数据桥接中的运行统计，命中、发送成功次数均 +1。

3. 转到 [Amazon Kinesis 数据查看器](https://docs.aws.amazon.com/zh_cn/streams/latest/dev/data-viewer.html)。您应该可以看到数据流指定分片内的数据记录。

#### 使用 LocalStack 查看数据

如果您使用 LocalStack，通过以下步骤查看接收到的数据。

1. 在发送数据到桥接之前，使用以下命令获取 *ShardIterator*：

   ```bash
   awslocal kinesis get-shard-iterator --stream-name my_stream --shard-id shardId-000000000000 --shard-iterator-type LATEST
   {
   "ShardIterator": "AAAAAAAAAAG3YjBK9sp0uSIFGTPIYBI17bJ1RsqX4uJmRllBAZmFRnjq1kPLrgcyn7RVigmH+WsGciWpImxjXYLJhmqI2QO/DrlLfp6d1IyJFixg1s+MhtKoM6IOH0Tb2CPW9NwPYoT809x03n1zL8HbkXg7hpZjWXPmsEvkXjn4UCBf5dBerq7NLKS3RtAmOiXVN6skPpk="
   }
   ```

2. 使用 MQTTX 向 `t/my_topic` 主题发布一条消息：

   ```
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

3. 查看数据记录并解码接收到的数据：

   ```bash
   awslocal kinesis get-records --shard-iterator="AAAAAAAAAAG3YjBK9sp0uSIFGTPIYBI17bJ1RsqX4uJmRllBAZmFRnjq1kPLrgcyn7RVigmH+WsGciWpImxjXYLJhmqI2QO/DrlLfp6d1IyJFixg1s+MhtKoM6IOH0Tb2CPW9NwPYoT809x03n1zL8HbkXg7hpZjWXPmsEvkXjn4UCBf5dBerq7NLKS3RtAmOiXVN6skPpk="
   {
       "Records": [
           {
               "SequenceNumber": "49642650476690467334495639799144299020426020544120356866",
               "ApproximateArrivalTimestamp": 1689389148.261,
               "Data": "eyAibXNnIjogImhlbGxvIEFtYXpvbiBLaW5lc2lzIiB9",
               "PartitionKey": "key",
               "EncryptionType": "NONE"
           }
       ],
       "NextShardIterator": "AAAAAAAAAAFj5M3+6XUECflJAlkoSNHV/LBciTYY9If2z1iP+egC/PtdVI2t1HCf3L0S6efAxb01UtvI+3ZSh6BO02+L0BxP5ssB6ONBPfFgqvUIjbfu0GOmzUaPiHTqS8nNjoBtqk0fkYFDOiATdCCnMSqZDVqvARng5oiObgigmxq8InciH+xry2vce1dF9+RRFkKLBc0=",
       "MillisBehindLatest": 0
   }
   
   echo 'eyAibXNnIjogImhlbGxvIEFtYXpvbiBLaW5lc2lzIiB9' | base64 -d
   { "msg": "hello Amazon Kinesis" }
   ```