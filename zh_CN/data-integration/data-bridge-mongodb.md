# 将 MQTT 数据写入到 MongoDB

::: tip

MongoDB 数据集成是 EMQX 企业版功能。

:::

[MongoDB](https://www.mongodb.com/)，作为领先的 NoSQL 数据库，以其在模式设计的灵活性、可扩展性以及存储大量结构化和半结构化数据的能力而闻名。通过将 EMQX 与 MongoDB 的数据集成，用户可以高效地将 MQTT 消息和客户端事件直接导入 MongoDB。这有助于在 MongoDB 中进行长期序列数据存储和高级查询。集成确保了单向流动，即 EMQX 的 MQTT 消息被写入 MongoDB 数据库。这种强大的组合是寻求有效管理其物联网数据的企业的坚实基础。

本页提供了 EMQX 与 MongoDB 数据集成的全面介绍，并提供了创建和验证数据集成的实用指导。

## 工作原理

MongoDB 数据集成是 EMQX 中的开箱即用功能，旨在弥合基于 MQTT 的物联网数据与 MongoDB 强大的数据存储能力之间的差距。借助内置的[规则引擎](./rules.md)组件，集成简化了将数据从 EMQX 导入 MongoDB 进行存储和管理的过程，无需复杂的编码。

下图展示了 EMQX 与 MongoDB 之间数据集成的典型架构。

<img src="./assets/mongdb_bridge_architecture.png" alt="mongdb_bridge_architecture" style="zoom:67%;" />

将 MQTT 数据写入 MongoDB 的过程如下：

1. **消息发布和接收**：无论是连接车辆、工业物联网系统还是能源管理平台的一部分，物联网设备都通过 MQTT 协议成功连接到 EMQX，并向特定主题发布 MQTT 消息。当 EMQX 收到这些消息时，它启动其规则引擎中的匹配过程。
2. **消息数据处理**：消息到达后，经过规则引擎处理，然后由 EMQX 中定义的规则处理。基于预定义标准的规则确定哪些消息需要路由到 MongoDB。如果任何规则指定了有效载荷转换，则应用这些转换，例如转换数据格式、过滤特定信息或使用额外上下文丰富有效载荷。
3. **数据导入 MongoDB**：一旦规则引擎识别出一个消息用于 MongoDB 存储，它就会触发一个动作，将消息转发到 MongoDB。处理过的数据将被无缝写入 MongoDB 数据库的集合中。
4. **数据存储和利用**：随着数据现在存储在 MongoDB 中，企业可以利用其查询能力应用于各种用例。例如，在连接车辆领域，存储的数据可以通知车队管理系统关于车辆健康状况，根据实时指标优化路线规划或跟踪资产。同样，在工业物联网环境中，数据可能用于监控机械健康，预测维护或优化生产计划。

通过使用这种集成系统，像电力和能源等行业的企业可以持续监控电网健康状况，预测需求或在发生之前识别潜在停电。从实时和历史数据中获得的价值不仅确保了运营效率，还可以带来显著的成本节约和增强客户体验。

## 特性与优势

EMQX 与 MongoDB 的数据集成提供了一系列功能和优势，以确保有效的数据处理和存储：

- **简化物联网数据管理**

  您可以在一个地方摄取、存储、处理和分析您的物联网数据，消除了复杂集成和繁琐数据迁移的需求。告别数据孤岛，迎接物联网数据的统一视图。

- **实时数据处理**

  EMQX 专为处理实时数据流而构建，确保从源系统到 MongoDB 的高效可靠数据传输。它使组织能够实时捕获和分析数据，非常适合需要立即洞察和行动的用例。

- **灵活的 MongoDB 连接选项**

  无论您是操作单个 MongoDB 实例还是利用副本集的强大性能，数据集成都提供原生支持以连接这两种配置，为企业提供根据其基础设施需求进行调整的灵活性。

- **高性能和可扩展性**

  EMQX 的分布式架构和 MongoDB 的列式存储格式实现随着数据量增加的无缝扩展。这确保了即使在大数据集下也能保持一致的性能和响应能力。随着您的物联网部署增长，您的数据存储能力也可以轻松扩展。

- **灵活的数据转换**

  EMQX 提供了强大的基于 SQL 的规则引擎，允许组织在将数据存储到 MongoDB 之前对数据进行预处理。它支持各种数据转换机制，如过滤、路由、聚合和丰富，使组织能够根据其需求塑造数据。

- **NoSQL**

  MongoDB 的无模式架构确保了多样化的 MQTT 消息结构可以轻松存储，无需固定模式，适应物联网数据的动态特性。

- **可靠的数据存储**

  一旦 EMQX 规则引擎处理并路由消息，它将存储在 MongoDB 中，平台已证明的可靠性确保了数据的完整性和持续可用性。

- **运营指标和高级分析**

  从诸如总消息计数、出口流量速率等指标中获得洞察。这些指标与 MongoDB 强大的查询能力相结合，可用于监控、分析和优化数据流，赋予用户从物联网数据中获得宝贵洞察的能力，实现预测分析、异常检测等。

- **支持最新的 MongoDB 版本**

  数据集成兼容并支持 MongoDB 的最新版本，确保用户受益于数据库平台提供的最新功能、优化和安全更新。

- **成本效益**

  EMQX 和 MongoDB 都有开源解决方案，与专有解决方案相比，它们更具成本效益。这有助于降低物联网项目的总体拥有成本，并提高投资回报率。

MongoDB 数据集成加强了您的物联网基础设施，确保您的设备生成的大量数据不仅被存储，而且还为将来的查询和分析做好了准备。它带来的简便设置和运营卓越性能可以大大提高您的物联网系统的效率和可靠性。

## 准备工作

本节描述了您在开始在 EMQX Dashboard 中创建 MongoDB 数据集成之前需要完成的准备工作。

### 前置准备

- 了解 EMQX 数据集成[规则](./rules.md)
- 了解[数据集成](./data-bridges.md)
- 了解 [MongoDB](https://www.mongodb.com/)

### 安装 MongoDB 并创建数据库

通过 Docker 安装并启动 MongoDB：

```bash
# 启动一个 MongoDB 容器并设置密码为 public
docker run -d --name mongodb -p 27017:27017 mongo

# 进入容器
docker exec -it mongodb bash

# 在容器中连接到 MongoDB 服务器（4.x 版本请使用命令 `mongo`）
mongosh

# 创建用户
use admin
db.createUser({ user: "admin", pwd: "public", roles: [ { role: "root", db: "admin" } ] })

# 创建名为 emqx_data 的数据库
use emqx_data

# 创建名为 emqx_messages 的集合
db.createCollection('emqx_messages')
```

## 创建连接器

本节演示如何创建一个连接器，将 MongoDB Sink 连接到 MongoDB 服务器。

以下步骤假设您在本地机器上同时运行 EMQX 和 MongoDB。如果您的 MongDB 部署在其他地方，请相应调整设置。

1. 进入 Dashboard，点击**集成** -> **连接器**。
2. 点击页面右上角的**创建**。
3. 在**创建连接器**页面中，选择 **MongoDB**，然后点击 **下一步**。
4. 为连接器输入一个名称。名称应是大写/小写字母和数字的组合，例如 `my_mongodb`。
5. 配置 MongoDB 服务器的连接信息。填写必填字段（带星号的）。
   - **部署模式**: 根据您实际的部署模式选择要连接的 MongoDB 部署类型。在此演示中，您可以选择 `single`。
     - `single`：单个独立的 MongoDB 实例。
     - `rs`：副本集，一组维护相同数据集的 `mongod` 进程。
     - `sharded`：MongoDB 的分片集群。
   - **服务器地址**：输入 `127.0.0.1:27017`，或如果 MongoDB 服务器运行在远程，则输入实际的 URL。
   - **数据库名字**：输入 `emqx_data`。
   - **写模式**：保持默认值 `unsafe`。
   - **用户名**：输入 `admin`。
   - **密码**：输入 `public`。
   - **认证源**：填写与用户证书关联的数据库名称。
   - **使用旧协议**：可选择是否应使用 MongoDB 的旧版通信协议（MongoDB 在 3.6 版本中引入了新的线协议，旧协议保留用于向后兼容。），可以设置为 `true`、 `false` 或 `auto`。在 `auto` 模式（默认选项）下，EMQX 将根据检测到的 MongoDB 版本自动决定使用哪种协议。
   - **Srv 记录**：默认禁用。启用后，允许 EMQX 使用 DNS SRV 记录来发现它应该连接的 MongoDB 主机，这使得连接到副本集或分片集群更加容易，无需在连接字符串中指定每个主机。
   - 如果您想建立加密连接，请点击**启用 TLS** 切换开关。有关 TLS 连接的更多信息，有关 TLS 连接选项的详细信息，请参阅[启用 TLS 加密访问外部资源](../network/overview.md#启用-tls-加密访问外部资源)。
6. 高级设置（可选）：详情参见[高级配置](#高级配置)。
7. 在点击**创建**之前，您可以点击**测试连接**来测试连接器是否能连接到 MongoDB 服务器。
8. 点击底部的**创建**按钮完成连接器的创建。在弹出的对话框中，您可以点击**返回连接器列表**或点击**创建规则**继续创建规则和 Sink 以指定转发到 MongoDB 的数据。具体步骤请参见[创建规则和 MongoDB Sink](#创建规则和-mongodb-sink)。

## 创建 MongoDB Sink 规则

本节演示了如何在 EMQX 中创建规则，用于处理来自源 MQTT 主题 `t/#` 的消息，并通过配置的 Sink 将处理后的结果发送到 MongoDB。

1. 转到 Dashboard **集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 输入规则 ID `my_rule`，在 SQL 编辑器中输入规则，此处选择将 `t/#` 主题的 MQTT 消息存储至 MongoDB，请确规则选择出来的字段（SELECT 部分）包含所有 SQL 模板中用到的变量。此处规则 SQL 如下：

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   您也可以使用以下 SQL 将 `timestamp` 保存为日期类型数据、将 JSON 格式的 `payload` 保存为 JSON 字符串：

   ```sql
   SELECT
     *,
     mongo_date(timestamp) as timestamp,
     json_encode(payload) as payload
   FROM
     "t/#"
   ```

   ::: tip

   如果您初次使用 SQL，可以点击 **SQL 示例** 和**启用调试**来学习和测试规则 SQL 的结果。

   :::

4. 点击右侧的**添加动作**按钮，为规则在被触发的情况下指定一个动作。在**动作类型**下拉框中选择 `MongoDB`，保持**动作**下拉框为默认的`创建动作`选项，您也可以选择一个之前已经创建好的 MongoDB Sink。此处我们创建一个全新的 Sink 并添加到规则中。

5. 输入 Sink 名称，名称应为大/小写字母和数字的组合。

6. 从**连接器**下拉框中选择刚刚创建的 `my_mongodb`。您也可以通过点击下拉框旁边的按钮创建一个新的连接器。有关配置参数，请参见[创建连接器](#创建连接器)。

7. 在**集合** **(Collection)** 中填写存储数据的集合，支持通过占位符 `${var_name}` 动态设置，本示例中填入 `emqx_messages`。

8. 配置 **有效载荷模板**，将 `clientid`、`topic`、`qos`、`timestamp`、`payload` 字段存储到 MongoDB 中，该模板将通过 MongoDB insert 命令执行，对应模板如下：

   ```json
   {
     "clientid": "${clientid}",
     "topic": "${topic}",
     "qos": ${qos},
     "timestamp": ${timestamp},
     "payload": ${payload}
   }
   ```

    ::: tip

    配置有效载荷模板时需注意以下几点：

     - 所有的**键**需要使用双引号 `"` 包裹；

     - 不支持自动推导**值**的数据类型：
       - 字符类型的字段需要使用 `"` 包裹，否则将报错；
       - 数值类型字段不需要包裹，否则将被识别为字符类型；
       - 时间戳、日期和时间类型，如不做特殊处理，则将被识别为数值或字符类型，如希望以日期和时间类型存储，需要在规则 SQL 中使用 `mongo_date` 函数对字段进行处理，参考 [时间与日期函数](./rule-sql-builtin-functions.md#时间与日期函数)。

     - 允许嵌套对象，当 **值** 为 JSON 对象时：
       - 模板中禁止使用双引号嵌套该值，否则将导致执行错误；

       - 对象将按自身结构嵌套存储；

       - 如需将对象存储为 JSON 字符，可以在规则 SQL 中使用 `json_encode` 函数转换，模板中对应的**值**仍然禁止使用双引号包裹。

      :::

9. 高级配置（可选），详细请参考[高级配置](#高级配置)。

10. 在完成创建之前，您可以点击**测试连接**来测试 Sink 可以连接到 MongoDB 服务器。

11. 点击**创建**按钮完成 Sink 创建，新建的 Sink 将被添加到**动作输出**列表中。

12. 回到创建规则页面，对配置的信息进行确认，点击**创建**。一条规则应该出现在规则列表中。

现在您已成功创建了通过 MongoDB Sink 将数据转发到 MongoDB 的规则，同时在**规则**页面的**动作(Sink)** 标签页看到新建的 MongoDB Sink。

您还可以点击**集成** -> **Flow 设计器**可以查看拓扑，通过拓扑可以直观的看到，主题 `t/#` 下的消息在经过规则 `my_rule` 解析后被发送到 MongoDB 中。


### 测试规则和 Sink

使用 MQTTX 向 `t/1` 主题发布消息：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello MongoDB" }'
```

查看 Sink 运行统计，命中、发送成功次数均 +1。

查看数据是否已经写入`emqx_messages` 集合中：

```bash
> db.emqx_messages.find().pretty()
{
    "_id" : ObjectId("63db7059df489d01ed000009"),
    "clientid" : "emqx_c",
    "payload" : {
      "msg" : "hello MongoDB"
    },
    "qos" : 0,
    "timestamp" : NumberLong("1675325529070"),
    "topic" : "t/1"
}
```

使用第二种规则 SQL 时，对应数据内容如下：

```bash
> db.emqx_messages.find().pretty()
{
    "_id" : ObjectId("63db7535df489d01ed000013"),
    "clientid" : "emqx_c",
    "payload" : "{ \"msg\": \"hello MongoDB\" }",
    "qos" : 0,
    "timestamp" : ISODate("2023-02-02T08:33:36.715Z"),
    "topic" : "t/1"
}
```

## 高级配置

本节介绍了 EMQX MongoDB 连接器和 Sink 的部分高级配置选项。配置连接器和 Sink 时，展开**高级设置**可以调整以下参数以满足您的特定需求。

| **字段**           | **描述**                                                     | **推荐值** |
| ------------------ | ------------------------------------------------------------ | ---------- |
| **连接超时**       | EMQX 尝试建立与 MongoDB 的连接，在超时前等待的时间长度。     | 30秒       |
| **套接字操作超时** | EMQX 在套接字连接上与 MongoDB 尝试发送或接收数据，在超时前等待的时间长度。 | 30秒       |
| **最大溢出**       | 当所有现有工作线程被占用时，可以创建的额外工作线程的数量。在工作负载激增时，此设置至关重要，以允许更多并发连接到 MongoDB。 | 0          |
| **等待队列超时**   | 工作器在等待连接到 MongoDB 变得可用时，可以保持空闲的最大持续时间。 | 10秒       |
| **心跳期**         | 定义驱动程序检查 MongoDB 部署状态的间隔。这指定了连续检查之间的时间，有效控制这些心跳信号的频率，以确保 MongoDB 的运行状态。 | 200秒      |
| **最小心跳周期**   | 设置心跳之间允许的最短时间间隔，确保驱动程序不会过于频繁地检查 MongoDB 状态。这对于避免不必要的负载和确保 EMQX 与 MongoDB 之间的高效通信至关重要。 | 200秒      |

## 更多信息

查看以下链接以了解更多信息：

**博客**：

[MQTT and MongoDB: Crafting Seamless Synergy for IoT Data Mangement](https://www.emqx.com/en/blog/mqtt-and-mongodb-crafting-seamless-synergy-for-iot-data-mangement)

**报告**：

[MQTT 性能基准测试：EMQX-MongoDB 集成](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-mongodb-integration)

**视频**：

https://www.youtube.com/watch?v=c2M-rlkkT5o

