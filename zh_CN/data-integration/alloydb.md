# 将 MQTT 数据写入到 AlloyDB

[AlloyDB for PostgreSQL](https://cloud.google.com/products/alloydb?hl=zh-CN) 是 Google Cloud 提供的全托管、兼容 PostgreSQL 的数据库服务，专为高要求的企业级工作负载而设计。EMQX 支持与 AlloyDB 的无缝集成，可实现来自物联网设备的 MQTT 数据的实时采集与存储。借助 EMQX 高效的消息路由能力，以及 AlloyDB 通过混合事务/分析处理（HTAP）引擎提供的高吞吐事务处理能力与实时分析能力，您可以构建一个强大的数据管道，用于捕获设备状态、记录事件并进行深入分析。

本页面将全面介绍 EMQX 与 AlloyDB 的数据集成，并提供创建与验证数据集成的实用操作指南。

## 工作原理

EMQX 中的 AlloyDB 数据集成是一项开箱即用功能，可将基于 MQTT 的物联网数据流直接写入 AlloyDB 这一高性能、兼容 PostgreSQL 的数据库中。借助内置的[规则引擎](./rules.md)组件，该集成简化了从 EMQX 向 AlloyDB 采集数据并进行存储与分析的过程，无需编写复杂代码。通过 AlloyDB Sink 可以将 MQTT 消息和客户端事件存储到 AlloyDB 中，也可以通过事件触发对 AlloyDB 中数据的更新或删除操作，从而实现对诸如设备在线状态、上下线历史等的记录。

下图展示了 EMQX 与 AlloyDB 数据集成的典型架构：

![EMQX-PostgeSQL集成](./assets/alloydb_architecture.png)

将 MQTT 数据写入 AlloyDB 的流程如下：

1. **物联网设备连接至 EMQX**：设备通过 MQTT 协议成功连接后，会触发上线事件。事件包含设备 ID、源 IP 地址及其他属性信息。
2. **消息发布与接收**：设备向特定主题发布遥测数据与状态数据。EMQX 接收到消息后，会在规则引擎中启动匹配过程。
3. **规则引擎处理消息**：EMQX 的规则引擎会根据主题或消息内容，将事件与消息匹配到预定义规则。处理过程可包括数据转换（例如将 JSON 转换为 SQL 可用格式）、过滤，以及在入库前进行上下文信息补充。
4. **写入 AlloyDB**：匹配成功的规则会触发针对 AlloyDB 的 SQL 执行。通过 SQL 模板，用户可以将处理后的数据字段映射到 AlloyDB 的表和列中。由于 AlloyDB 支持并行查询执行以及内置列式存储引擎的优化，数据可被快速写入，并可立即用于分析查询。

事件与消息数据写入 AlloyDB 后，可以：

- 连接 Grafana 等可视化工具生成数据图表并实时展示数据变化；
- 将 AlloyDB 与设备管理系统或分析模型集成，实现设备健康监测、异常检测与告警触发；
- 利用 AlloyDB 的 HTAP 能力，在处理实时设备遥测数据的同时，对实时数据执行复杂分析（聚合、关联、时序查询）。

## 特性与优势

与 AlloyDB 的数据集成可以为您的业务带来以下功能和优势：

- **灵活的事件处理**：借助 EMQX 规则引擎，AlloyDB 可以低延迟地存储和处理设备生命周期事件（连接、断开、状态变化）。结合 AlloyDB 的并行查询执行和独立扩展能力，可以实时分析事件数据，检测设备故障、异常或使用趋势。
- **消息转换**：通过 EMQX 规则，消息在写入 AlloyDB 之前可以进行广泛的处理和转换，使存储和使用更加方便。
- **基于 SQL 模板的灵活数据操作**：通过 EMQX 的 SQL 模板映射，可以将结构化 IoT 数据插入或更新到 AlloyDB 的表和列中。AlloyDB 的 PostgreSQL 兼容性支持标准 SQL、JSONB 存储和索引，同时 AI 驱动的索引功能会根据工作负载的变化自动优化查询性能。
- **业务流程集成**：AlloyDB 的 PostgreSQL 生态系统兼容性允许与 ERP、CRM、GIS 以及定制业务系统直接集成，无论是在 Google Cloud 上还是在本地部署。结合 EMQX，可以在无需复杂数据管道的情况下，实现事件驱动的自动化和业务流程编排。
- **高级地理空间功能**：通过 PostgreSQL 扩展（如 PostGIS），AlloyDB 支持地理空间数据的存储、索引和查询，实现地理围栏、路线跟踪和位置分析。结合 EMQX 可靠的 MQTT 数据接入，可以构建车队跟踪、资产监控等实时 IoT-GIS 解决方案。
- **内置指标与监控**：EMQX 为每个 AlloyDB Sink 提供运行时指标，而 AlloyDB 可与 Cloud Monitoring 集成，对查询性能、存储利用率和副本健康状况进行监控，实现端到端可观测性。

## 准备工作

本节介绍在开始创建 AlloyDB 集成之前需要完成的准备工作，包括如何创建 AlloyDB 实例、数据库以及数据表。

### 前置准备

- 了解 EMQX 数据集成[规则](./rules.md)
- 了解[数据集成](./data-bridges.md)

### 在 AlloyDB 中创建数据库和数据表

在 EMQX 中创建 AlloyDB 连接器之前，请确保已准备好可用的 AlloyDB 实例，并在其中创建用于存储 IoT 数据的数据库和数据表。

按照 [AlloyDB 快速入门指南](https://cloud.google.com/alloydb/docs/quickstart/create-and-connect)的步骤操作：

1. 创建一个 AlloyDB 实例。

   - 在设置过程中，定义数据库用户账号信息，本示例使用以下配置：
     - **用户名**：`emqx_user`（需具备连接、插入、更新和查询数据的权限）
     - **密码**：`你的密码`
   - 可以在实例创建时设置该用户，也可以在之后通过 SQL、Google Cloud 控制台或 `gcloud` CLI 创建。

2. 在实例中创建数据库。本示例中数据库名称为 `emqx_data`。

3. 使用 PostgreSQL 兼容客户端（如 `psql`）通过上述账号信息连接到数据库。

4. 在 `emqx_data` 数据库中创建两个表，用于存储 MQTT 消息和客户端事件数据。

   - 使用以下 SQL 创建 `t_mqtt_msg` 表，用于存储带有客户端 ID、主题、QoS、载荷以及到达时间等元数据的 MQTT 消息：

     ```sql
     CREATE TABLE t_mqtt_msg (
       id SERIAL primary key,
       msgid character varying(64),
       sender character varying(64),
       topic character varying(255),
       qos integer,
       retain integer,
       payload text,
       arrived timestamp without time zone
     );
     ```

   - 使用以下 SQL 创建 `emqx_client_events` 表，用于存储客户端生命周期事件（如连接和断开）及其时间戳：

     ```sql
     CREATE TABLE emqx_client_events (
       id SERIAL primary key,
       clientid VARCHAR(255),
       event VARCHAR(255),
       created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
     );
     ```

## 创建 AlloyDB 连接器

在添加 AlloyDB Sink 之前，需要先在 EMQX 中创建 AlloyDB 连接器。连接器定义了 EMQX 如何连接到 Google Cloud 中的 AlloyDB 实例。

1. 在 EMQX Dashboard 中，进入**集成** -> **连接器**页面。
2. 点击页面右上角的**创建**。
3. 在**创建连接器**页面中，选择  **AlloyDB**，点击**下一步**。
4. 输入连接器名称。名称必须以字母或数字开头，可以包含字母、数字、连字符（-）或下划线（_），例如：`my_alloydb`。
5. 输入连接信息：
   - **服务器地址**：Google Cloud 中 AlloyDB 实例的主机名或 IP 地址。
   - **数据库名字**：EMQX 将写入数据的 AlloyDB 目标数据库名称。本示例为 `emqx_data`。
   - **用户名**：用于身份验证和标识的 AlloyDB 数据库用户名。本示例为 `emqx_user`。
   - **密码**：`emqx_user` 对应的密码。
   - **启用 TLS**：如需建立加密连接，切换该开关。更多 TLS 连接信息请参见[启用 TLS 访问外部资源](../network/overview.md#启用-tls-加密访问外部资源)。
6. **高级设置**（可选）：可配置连接池大小、空闲超时、请求超时等其他连接属性。
7. 点击**测试连接**，验证 EMQX 是否能够使用所提供的配置成功连接到 AlloyDB 实例。
8. 点击**创建**保存连接器。
9. 创建完成后，可以选择：

   - 点击**返回连接器列表**查看所有连接器，或
   - 点击**创建规则**立即创建使用该连接器将数据转发到 AlloyDB 的规则。

   详细示例请参见：

   - [创建 AlloyDB 消息存储规则](#创建-alloydb-消息存储规则)
   - [创建 AlloyDB 事件记录规则](#创建-alloydb-事件记录规则)

## 创建 AlloyDB 消息存储规则

本节演示如何在 Dashboard 中创建一条规则，用于处理来自源 MQTT 主题 `t/#` 的消息，并通过已配置的 Sink 将处理后的数据保存到 AlloyDB 表 `t_mqtt_msg` 中。

1. 在 Dashboard 中，进入**集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 在 SQL 编辑器中输入规则 ID `my_rule` 和规则 SQL。此处选择将主题为 `t/#` 的 MQTT 消息存储到 AlloyDB，请确保规则 SQL（`SELECT` 部分）中选择的字段包含 SQL 模板中使用的所有变量。示例 SQL 如下：

   ```sql
   SELECT
    *
   FROM
    "t/#"
   ```

   ::: tip

   如果是初学者，可以点击 **SQL 示例** 和**启用调试**来学习和测试 SQL 规则。

   :::

4. 点击 **+ 添加动作**按钮，定义规则触发时要执行的动作。通过该动作，EMQX 会将规则处理后的数据发送到 AlloyDB。

5. 从**动作类型**下拉列表中选择 AlloyDB，保持动作下拉框为默认的`创建动作`选项，或者从列表中选择一个之前已创建的 AlloyDB 动作。本示例将新建一个 Sink 并将其添加到规则中。

6. 在表单中输入 Sink 的名称与描述。

7. 在**连接器**下拉框中选择刚刚创建的 `my_alloydb`  连接器。您也可以点击下拉框旁的按钮新建连接器。连接器配置方法参见[创建 AlloyDB 连接器](#创建-alloydb-连接器)。

8. 配置 SQL 模板，使用如下 SQL 完成数据插入。

   注意，这是一个[预处理 SQL](./data-bridges.md#sql-预处理)，字段不应当包含引号，SQL 末尾不要带分号 `;`。

   ```sql
   INSERT INTO t_mqtt_msg(msgid, sender, topic, qos, payload, arrived) VALUES(
     ${id},
     ${clientid},
     ${topic},
     ${qos},
     ${payload},
     TO_TIMESTAMP((${timestamp} :: bigint)/1000)
   )
   ```

9. **备选动作（可选）**：如果您希望在消息投递失败时提升系统的可靠性，可以为 Sink 配置一个或多个备选动作。当 Sink 无法成功处理消息时，这些备选动作将被触发。更多信息请参见：[备选动作](./data-bridges.md#备选动作)。

10. **高级配置（可选）**：根据情况配置同步/异步模式，队列与批量等参数。详细内容请参考 [Sink 的特性](./data-bridges.md#sink-的特性)中的配置参数章节。

11. 在点击**创建**之前，可以先点击**测试连接**以验证 Sink 是否可以连接到 AlloyDB 实例。

12. 点击**创建**完成 Sink 配置。此时会在**动作输出**中新增一个 Sink。

13. 在**创建规则**页面中检查配置信息后，点击**保存**生成规则。

现在您已成功创建了规则，你可以点击**集成** -> **规则**页面看到新建的规则，同时在**动作 (Sink)**  标签页看到新建的 AlloyDB Sink。

您也可以点击**集成** -> **Flow 设计器**查看拓扑，通过拓扑可以直观的看到，主题 `t/#` 下的消息在经过规则 `my_rule` 解析后被写入到 AlloyDB 中。

## 创建 AlloyDB 事件记录规则

本节展示如何创建用于记录客户端上/下线状态的规则，并通过配置的 Sink 将事件记录数据写入到 AlloyDB 的数据表 `emqx_client_events` 中。

注意：除 SQL 模板与规则外，其他操作步骤与[创建 AlloyDB 消息存储规则](#创建-alloydb-消息存储规则)章节完全相同。

SQL 模板如下，请注意字段不应当包含引号，SQL 末尾不要带分号 `;`:

```sql
INSERT INTO emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  TO_TIMESTAMP((${timestamp} :: bigint)/1000)
)
```

规则 SQL 如下：

```sql
SELECT
  *
FROM
  "$events/client_connected", "$events/client_disconnected"
```

## 测试规则

使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello AlloyDB" }'
```

分别查看两个 Sink 运行统计，消息存储 Sink 的命中、发送成功次数均 +1。触发上下线事件，命中及发送成功次数会 +2。

查看数据是否已经写入表中，`t_mqtt_msg` 表：

```bash
emqx_data=# select * from t_mqtt_msg;
 id |              msgid               | sender | topic | qos | retain |            payload
        |       arrived
----+----------------------------------+--------+-------+-----+--------+-------------------------------+---------------------
  1 | 0005F298A0F0AEE2F443000012DC0002 | emqx_c | t/1   |   0 |        | { "msg": "hello AlloyDB" } | 2023-01-19 07:10:32
(1 row)
```

`emqx_client_events` 表：

```bash
emqx_data=# select * from emqx_client_events;
 id | clientid |        event        |     created_at
----+----------+---------------------+---------------------
  3 | emqx_c   | client.connected    | 2023-01-19 07:10:32
  4 | emqx_c   | client.disconnected | 2023-01-19 07:10:32
(2 rows)
```