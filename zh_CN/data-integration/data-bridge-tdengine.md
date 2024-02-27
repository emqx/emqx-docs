# 将 MQTT 数据写入到 TDengine

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

[TDengine](https://tdengine.com/) 是一款专为物联网、工业互联网等场景设计并优化的大数据平台，其核心模块是高性能、集群开源、云原生、极简的时序数据库。EMQX 支持与 TDengine 集成，能够实现大量设备和数据采集器的海量数据传输、存储、分析和分发，对业务运行状态进行实时监测、预警，提供实时的商业洞察。

本页详细介绍了 EMQX 与 TDengine 的数据集成并提供了实用的规则和 Sink 创建指导。

## 工作原理

TDengine 数据集成是 EMQX 的开箱即用功能，通过内置的[规则引擎](./rules.md)组件和 Sink 将设备数据转发到 TDengine。通过 TDengine Sink ，MQTT 消息和客户端事件可以存储在 TDengine 中。此外，数据更新或在 TDengine 中的删除操作可以由事件触发，从而实现对设备在线状态和历史上下线事件的记录。该集成简化了从 EMQX 到 TDengine 的数据摄取过程，无需复杂编码。

下图展示了 EMQX 和 TDengine 数据集成在工业物联网中的典型架构:

![EMQX-TDengine 集成](./assets/emqx-integration-tdengine.png)

以工业能耗管理场景为例，工作流程如下：

1. **消息发布和接收**：工业设备通过 MQTT 协议成功连接到 EMQX，并定期使用 MQTT 协议发布能耗数据。这些数据包括生产线标识符和能耗值。当 EMQX 接收到这些消息时，它将在其规则引擎中启动匹配过程。
2. **规则引擎处理消息**：内置的规则引擎根据主题匹配处理来自特定来源的消息。当消息到达时，它通过规则引擎进行匹配，规则引擎将处理消息数据。这可能包括转换数据格式、过滤特定信息或用上下文信息丰富消息。
3. **数据写入到 TDengine**：规则引擎中定义的规则触发动作将消息写入 TDengine。TDengine 数据桥提供 SQL 模板，允许灵活定义数据格式，将特定消息字段写入 TDengine 中相应的表和列。

将能耗数据写入 TDengine 后，您可以使用标准 SQL 和强大的时间序列扩展实时分析您的数据，无缝集成众多第三方批分析、实时分析、报表工具、AI/ML 工具、可视化工具。例如：

- 连接到如 Grafana 等可视化工具以生成图表并显示能耗数据。
- 连接到 ERP 或 Power BI 等应用系统进行生产分析和生产计划调整。
- 连接到业务系统以进行实时能源使用分析，促进以数据驱动的能源管理。

## 特性与优势

TDengine 数据集成为您的业务带来了以下功能和优势：

- **高性能海量物联网数据**：EMQX 可以高效处理大量物联网设备连接和消息吞吐量，TDengine 充分利用了时序数据特点，在数据写入、存储、查询方面表现优异，满足物联网场景下的数据处理需求，不会对系统造成过大压力。
- **消息转换**：消息可以在 EMQX 规则中进行丰富的处理和转换，然后写入 TDengine。
- **集群和可扩展性**：EMQX 和 TDengine 支持集群能力并基于云原生构建，能充分利用云平台的存储、计算、网络资源的弹性能力，随着业务增长灵活地水平扩展以满足不断扩大的需求。
- **高级查询能力**：TDengine 为时戳数据的高效查询和分析提供了优化的功能、操作符和索引技术，使得能够从物联网时间序列数据中提取精确的洞察。

## 准备工作

本节介绍了在 EMQX 中创建 TDengine 数据集成之前需要做的准备工作，包括如何安装 TDengine 服务器并创建数据表。

### 前置准备

- 了解[规则](./rules.md)。
- 了解[数据集成](./data-bridges.md)。

### 安装 TDengine

通过 Docker 安装并启动 TDengine：

```bash
# 启动一个 TDengine 容器
docker run --name TDengine -p 6041:6041 tdengine/tdengine

# 进入容器
docker exec -it TDengine bash

# 在容器中连接到 TDengine 服务器
taos

# 创建并选择数据库

CREATE DATABASE mqtt;

use mqtt;
```

我们将在 TDengine 中创建两张表：

数据表 `t_mqtt_msg`，用于存储每条消息的发布者客户端 ID、主题、Payload 以及发布时间：

```sql
CREATE TABLE t_mqtt_msg (
  ts timestamp,
  msgid NCHAR(64),
  mqtt_topic NCHAR(255),
  qos TINYINT,
  payload BINARY(1024),
  arrived timestamp
);
```

数据表 `emqx_client_events`，用于存储上下线的客户端 ID、事件类型以及事件发生时间：

```sql
CREATE TABLE emqx_client_events (
  ts timestamp,
  clientid VARCHAR(255),
  event VARCHAR(255)
);
```

## 创建连接器

在创建 TDengine Sink 之前，您需要创建一个 TDengine 连接器，以便 EMQX 与 TDengine 服务建立连接。以下示例假定您在本地机器上同时运行 EMQX 和 TDengine。如果您在远程运行 TDengine 和 EMQX，请相应地调整设置。

1. 转到 Dashboard **集成** -> **连接器** 页面。点击页面右上角的**创建**。
2. 在连接器类型中选择 **TDengine**，点击**下一步**。
3. 在 **配置** 步骤，配置以下信息：

   - **连接器名称**：应为大写和小写字母及数字的组合，例如：`my_opentsdb`。
   - **主机列表**：填写 `127.0.0.1:6041`。
   - **数据库**：填写 `mqtt`。
   - **用户名**：填写 `root`。
   - **密码**：填写 `taosdata`。
4. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考 [Sink 的特性](./data-bridges.md#sink-的特性)。
5. 点击**创建**按钮完成连接器创建。
6. 在弹出的**创建成功**对话框中您可以点击**创建规则**，继续创建规则以指定需要写入 TDengine 的数据。您也可以按照[创建消息存储 Sink 规则](#创建消息存储-sink-规则)章节的步骤来创建规则。

## 创建消息存储 Sink 规则

我们将创建两个 TDengine Sink 分别完成消息存储与事件记录。本节将创建第一个 TDengine Sink 来实现对客户端发布消息的存储。

1. 转到 Dashboard **集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 输入规则 ID `my_rule`，在 SQL 编辑器中输入规则，此处选择将 `t/#` 主题的 MQTT 消息存储至 TDengine，请确认规则选出的字段（SELECT 部分）包含所有 SQL 模板中用到的变量，此处规则 SQL 如下：

   ```sql
   SELECT
     *,
     now_timestamp('millisecond')  as ts
   FROM
     "t/#"
   ```

   ::: tip

   如果您初次使用 SQL，可以点击 **SQL 示例**和**启用调试**来学习和测试规则 SQL 的结果。

   :::

4. 点击右侧的**添加动作**按钮，为规则在被触发的情况下指定一个动作。在**动作类型**下拉框中选择 `TDengine`，保持**动作**下拉框为默认的`创建动作`选项，您也可以选择一个之前已经创建好的 TDengine Sink。此处我们创建一个全新的 Sink 并添加到规则中。

5. 输入 Sink 名称，名称应为大/小写字母和数字的组合。

6. 从**连接器**下拉框中选择刚刚创建的 `my_tdengine`。您也可以通过点击下拉框旁边的按钮创建一个新的连接器。有关配置参数，请参见[创建连接器](#创建连接器)。

7. 配置 SQL 模板，可使用如下 SQL 完成数据插入，并支持通过 CSV 文件批量设置，详细请参考[批量设置](#批量设置)。

   ::: tip

   在 EMQX 5.1.1 中引入了一个重大变更。在 EMQX 5.1.1 之前，字符类型的占位符会被自动转义加上单引号，而现在需要手动加上单引号。

   :::

   ```sql
   INSERT INTO t_mqtt_msg(ts, msgid, mqtt_topic, qos, payload, arrived)
       VALUES (${ts}, '${id}', '${topic}', ${qos}, '${payload}', ${timestamp})
   ```

8. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考 [Sink 的特性](./data-bridges.md#sink-的特性)。

9. 在完成 Sink 创建之前，您可以使用**测试连接**来测试当前 Sink 到 TDengine 的连接是否成功。

10. 点击**创建**按钮完成 Sink 创建，新建的 Sink 将被添加到**动作输出**列表中。

11. 回到创建规则页面，对配置的信息进行确认，点击**创建**。一条规则应该出现在规则列表中。

现在您已成功创建了通过 TDengine Sink 将数据转发到 TDengine 的规则，同时在**规则**页面的**动作(Sink)** 标签页看到新建的 TDengine Sink。

您还可以点击 **集成** -> **Flow 设计器**可以查看拓扑，通过拓扑可以直观的看到，主题 `t/#` 下的消息在经过规则 `my_rule` 解析后被发送到 TDengine 中。

### 批量设置

在 TDengine 中，一条数据可能包含数百个数据点，这使得编写 SQL 语句变得具有挑战性。为了解决这个问题，EMQX 提供了批量设置 SQL 的功能。

当编辑 SQL 模板时，您可以使用批量设置功能，从 CSV 文件中导入要进行插入操作的字段。

1. 点击 **SQL 模板**下方的**批量设置**按钮，打开**导入批量设置**弹窗。

2. 根据指引，先下载批量设置模板文件，然后在模板文件中填入 Fields 键值对，默认的模板文件内容如下：

   | Field      | Value             | Char Value | Remarks (Optional) |
   | ---------- | ----------------- | ---------- | ------------------ |
   | ts         | now               | FALSE      | Example Remark     |
   | msgid      | ${id}             | TRUE       |                    |
   | mqtt_topic | ${topic}          | TRUE       |                    |
   | qos        | ${qos}            | FALSE      |                    |
   | temp       | ${payload.temp}   | FALSE      |                    |
   | hum        | ${payload.hum}    | FALSE      |                    |
   | status     | ${payload.status} | FALSE      |                    |

     - **Field**: 字段键，支持常量或 ${var} 格式的占位符。
     - **Value**: 字段值，支持常量或 ${var} 格式的占位符。虽然 SQL 中要求字符类型需要通过引号包裹，但在模板文件中无需包裹引号，而是通过 `Char Value` 列来指定字段是否为字符类型。
     - **Char Value**: 用于指定字段是否为字符类型，以便在导入生成 SQL 时为字段添加引号。如果字段是字符类型，则填写 `TRUE` 或 `1`，否则填写 `FALSE` 或 `0`。
     - **Remarks**: 仅用于 CSV 文件内字段的备注，无法导入到 EMQX 中。

     注意，批量设置 CSV 文件中数据不能超过 2048 行。

3. 将填好的模板文件保存并上传到**导入批量设置**弹窗中，点击**导入**完成批量设置。
4. 导入完成后，您可以在 **SQL 模板** 中对 SQL 进行进一步的调整，例如设置表名称，美化 SQL 等。

## 创建上下线记录 Sink 规则

本节将演示为通过 TDengine 实现对设备上下线的记录的 Sink 创建一条规则。

注意：除规则 SQL 和 Sink 的 SQL 模板设置不同外，其他操作步骤与[创建消息存储 Sink 规则](#创建消息存储-sink-规则)章节完全相同。

规则 SQL 如下：

```sql
    SELECT
      *,
      now_timestamp('millisecond')  as ts
    FROM
      "$events/client_connected", "$events/client_disconnected"
```

Sink 的 SQL 模板如下，请注意字段不应当包含引号，SQL 末尾不要带 `;`:

```sql
     INSERT INTO emqx_client_events(ts, clientid, event) VALUES (
           ${ts},
           '${clientid}',
           '${event}'
         )
```

## 测试规则

使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello TDengine" }'
```

分别查看两个 Sink 运行统计，命中、发送成功次数均 +1，上下线记录 Sink 次数 +2。

查看数据是否已经写入表中。

`t_mqtt_msg` 表：

```bash
taos> select * from t_mqtt_msg;
           ts            |             msgid              |           mqtt_topic           | qos  |            payload             |         arrived         |
==============================================================================================================================================================
 2023-02-13 06:10:53.787 | 0005F48EB5A83865F440000014F... | t/1                            |    0 | { "msg": "hello TDengine" }    | 2023-02-13 06:10:53.787 |
Query OK, 1 row(s) in set (0.002968s)
```

`emqx_client_events` 表：

```bash
taos> select * from emqx_client_events;
           ts            |            clientid            |             event              |
============================================================================================
 2023-02-13 06:10:53.777 | emqx_c                         | client.connected               |
 2023-02-13 06:10:53.791 | emqx_c                         | client.disconnected            |
Query OK, 2 row(s) in set (0.002327s)
```
