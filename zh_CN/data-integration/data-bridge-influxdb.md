# InfluxDB

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

InfluxDB 是一个用于存储和分析时间序列数据的数据库，其强大的数据吞吐能力以及稳定的性能表现使其非常适合物联网领域。EMQX 目前支持通过数据桥接的方式连接不同版本的 InfluxDB Cloud、InfluxDB OSS 以及 InfluxDB Enterprise。

## 工作原理

InfluxDB 数据集成在 EMQX 中是一个开箱即用的功能，它结合了 EMQX 的设备接入、消息传输能力与 InfluxDB 的数据存储和分析能力，通过简单的配置即可实现 MQTT 数据的无缝集成。

下图展示了储能场景中 EMQX 和 InfluxDB 数据集成的典型架构。

![MQTT to InfluxDB](./assets/mqtt-to-influxdb.jpg)

EMQX 和 InfluxDB 提供了一个可扩展的物联网平台，用于高效地实时收集和分析能耗数据。在此架构中，EMQX 作为物联网平台，负责设备接入、消息传输、数据路由等功能，InfluxDB 作为数据存储和分析平台，负责数据存储、数据分析等功能。

EMQX 通过规则引擎与数据桥接将设备数据转发至 InfluxDB，InfluxDB 通过 SQL 语句对数据进行分析，生成报表、图表等数据分析结果，通过 InfluxDB 的可视化工具展示给用户。其工作流程如下：

1. **物联网设备发布消息**：储能设备使用 MQTT 协议定期发布能耗数据，这些数据包括电量、输入输出功率信息。
2. **消息数据接收**：作为 MQTT 服务器，EMQX 从储能设备接收这些 MQTT 消息。
3. **规则引擎处理消息**：通过内置的规则引擎，可以根据主题匹配处理特定来源的消息。当消息到达时，它会通过规则引擎，规则引擎会匹配对应的规则，并对消息数据进行处理，例如转换数据格式、过滤掉特定信息或使用上下文信息丰富消息。
4. **写入到 InfluxDB**：规则引擎中定义的规则触发将消息写入到 InfluxDB 的操作。InfluxDB 数据桥接提供了 SQL 模板，能够灵活地定义写入的数据格式，将消息中的特定字段写入到 InfluxDB 的对应的表和列中。

储能数据写入到 InfluxDB 后，您可以灵活的使用 SQL 语句对数据进行分析，例如：

- 连接到可视化工具，例如 Grafana，根据数据生成图表，展示储能数据。
- 连接业务系统，进行储能设备状态监控与告警。

## 特性与优势

InfluxDB 数据集成具有以下特性与优势：

- **高效的数据处理能力**：EMQX 能够处理海量物联网设备连接与消息吞吐，InfluxDB 在数据写入、存储和查询方面具有出色的性能表现，能够满足物联网场景下的数据处理需求，不会导致系统不堪重负。
- **消息转换**：消息可以写入 InfluxDB 之前，通过 EMQX 规则中进行丰富的处理和转换。
- **可扩展性**：EMQX 与 InfluxDB 都具备集群扩展能力，能够随着业务的发展，利用灵活地进行集群水平扩展，满足业务的发展需求。
- **丰富的查询能力**：InfluxDB 提供包括优化的函数、运算符和索引技术，可实现对时间戳数据的高效查询和分析，准确地从 IoT 时间序列数据中提取有价值的见解。
- **高效存储**：InfluxDB 使用高压缩比的编码方式，可以大幅降低存储成本。也可以自定义不同数据的存储时间,避免不必要的数据占用存储空间。

## 桥接准备

本节介绍了在 EMQX 中创建 InfluxDB 数据桥接之前需要做的准备工作。

:::tip 前置准备

- 了解 [InfluxDB 行协议](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)，InfluxDB 数据桥接使用行协议进行数据写入。
- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

:::

### 安装 InfluxDB

1. 通过 Docker 安装并启动 InfluxDB，详细步骤请参考 [Install InfluxDB](https://docs.influxdata.com/influxdb/v2.5/install/)。

```bash
# 启动一个 InfluxDB 容器
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. 访问 [http://localhost:8086](http://localhost:8086) 打开 InfluxDB UI，设置用户名、密码、组织名称、Bucket 名称。

3. 前往 InfluxDB UI **Load Data** -> **API Token**，按照 [Create All-Access tokens](https://docs.influxdata.com/influxdb/v2.5/install/#create-all-access-tokens) 指引创建 Token。

## 创建规则和 InfluxDB 数据桥接

本节演示了如何在 EMQX 中创建一条规则，用于处理来自源 MQTT 主题 `t/#` 的消息，并通过配置的数据桥接将处理后的结果发送到 InfluxDB。

1. 点击 Dashboard 左侧导航菜单中的**数据集成** -> **规则**。

2. 在规则页面点击右上角的**创建**按钮。

3. 输入规则 ID `my_rule`。

4. 在 SQL 编辑器中输入规则，例如将 `t/#` 主题的 MQTT 消息存储至 InfluxDB，可以输入以下 SQL 语句：

   ::: tip 注意

   如果您希望指定自己的 SQL 规则，必须确保规则选择出来的字段（SELECT 部分）包含之后在数据桥接中指定的 InfluxDB 数据写入格式中包含的所有变量。

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   如果您初次使用 SQL，可以点击 **SQL 示例** 和**启用调试**来学习和测试规则 SQL 的结果。

   :::

5. 点击右侧的**添加动作**按钮，为规则在被触发的情况下指定一个动作。在**动作**下拉框中选择`使用数据桥接转发`，该动作会将经规则处理的数据转发到 InfluxDB。

6. 点击**数据桥接**下拉框右侧的**+**按钮创建数据桥接。在**数据桥接类型**下拉框中选择 `InfluxDB`。

7. 在**创建数据桥接**页面配置以下信息：

   - 输入数据桥接名称，要求是大小写英文字母和数字的组合。
   - 根据情况选择 InfluxDB 版本，默认为 V2。
   - 输入 InfluxDB 连接信息：
     - 服务器地址填写 `127.0.0.1:8086`。如果是 InfluxDB Cloud 需要指定端口为 443，即填入 `{url}:443` 并点击**启用 TLS** 以启用 TSL 连接。
     - 按照[安装 InfluxDB](#安装-influxdb) 中的设定完成 **Token**、**组织**及 **Bucket** 设置。注：如选择 InfluxDB v1 版本，请完成**数据库**、**用户名**及**密码**的设定。
     - 设定**时间精度**，默认为毫秒。

   - 设置是否启用TLS。

   - 定义解析数据， 指定数据格式与内容，使其能被解析并写入到 InfluxDB 中，可选项为 `JSON` 或 `Line Protocol`。

     - 对于 JSON 格式，需设置数据的 **Measurement**，**Fields**，**Timestamp** 与 **Tags**，键值均支持变量或占位符，可按照[行协议](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)进行设置。

     - 对于 Line Protocol 格式，请通过一段语句指定数据点的 Measurement、Fields、Timestamp 与 Tags，键值均支持变量或占位符，可按照[行协议](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)进行设置。

       :::tip

       如希望输入带符号的整型值，请在占位符后添加 `i` 作为类型标识，例如 `${payload.int}i`。参见 [InfluxDB 1.8 写入整型值](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)。

       对于 InfluxDB 2.x 中支持的无符号整型值，请在占位符后添加 `u` 作为类型标识，例如 `${payload.uint}u`。参见 [InfluxDB 2.6 无符号整型](https://docs.influxdata.com/influxdb/v2.6/reference/syntax/line-protocol/#uinteger)。
       :::

8. 展开**高级设置**，根据需要配置高级设置选项（可选），详细请参考[高级设置](#高级设置)。

9. 点击**添加**完成数据桥接配置和创建。您将回到**添加动作**页面，在**数据桥接**下拉框中选择您创建好的 InfluxDB 数据桥接，点击**添加**以完成动作添加。

10. 回到创建规则页面，对配置的信息进行确认，点击**创建**。一条规则应该出现在规则列表中，**状态**为**已连接**。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 InfluxDB 进行存储。

## 测试规则与数据桥接

使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

分别查看两个数据桥接运行统计，命中、发送成功次数均 +1。

前往 InfluxDB UI Data Explorer 查看数据是否已经写入 InfluxDB 中。

## 高级设置

本节将深入介绍可用于 InfluxDB 数据桥接的高级配置选项。在 Dashboard 中配置数据桥接时，您可以根据您的特定需求展开**高级设置**，调整以下参数。

| 字段名称             | 描述 | 默认值 |
| -------------------- | ---- | ------ |
| **启动超时时间**     |      |        |
| **缓存池大小**       |      |        |
| **请求超期**         |      |        |
| **健康检查间隔**     |      |        |
| **缓存队列最大长度** |      |        |
| **最大批量请求大小** |      |        |
| **请求模式**         |      |        |
| **请求飞行队列窗口** |      |        |

## 更多内容

您可以通过以下链接查看更多关于 InfluxDB 集成的内容：

**博客**：

[EMQX+InfluxDB+Grafana 构建物联网可视化平台](https://www.emqx.com/zh/blog/build-emqx-influxdb-grafana-iot-data-visualization-solution-in-one-hour)

[EMQX 规则引擎系列（三）存储消息到 InfluxDB 时序数据库](https://www.emqx.com/zh/blog/emqx-rule-engine-series-store-messages-to-influxdb-time-series-database)

