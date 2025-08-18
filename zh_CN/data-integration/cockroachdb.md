# 将 MQTT 数据写入到 CockroachDB

[CockroachDB](https://www.cockroachlabs.com/product/overview/) 是一种分布式、兼容 PostgreSQL 的数据库，既可以作为全托管云服务（CockroachDB Cloud）提供，也可以自建部署。它专为需要高可靠性、水平扩展性和完整 SQL 兼容性的全球化应用而设计。EMQX 可与 CockroachDB 无缝集成，实现物联网设备 MQTT 数据的实时采集与存储。两者结合能够在全球部署中实现快速、可靠的数据接入，借助基于 Raft 的复制机制确保数据一致性，并支持低延迟的读取，以满足运维和分析的需求。

本页面将全面介绍 EMQX 与 CockroachDB 的数据集成，并提供创建与验证数据集成的实用操作指南。

## 工作原理

EMQX 中的 CockroachDB 数据集成是一项开箱即用功能，可以将基于 MQTT 的物联网数据流直接写入 CockroachDB 这一分布式、兼容 PostgreSQL 的数据库中。借助 EMQX 内置的[规则引擎](./rules.md)，用户无需编写复杂的自定义代码，即可将数据直接采集到 CockroachDB 中，实现全球一致性存储与实时查询。

CockroachDB 采用无共享（shared-nothing）的分布式架构，利用基于 Raft 的一致性协议自动在多个节点和地域间复制数据，即使在发生故障时也能保持强一致性。这确保了 IoT 数据始终安全、同步且高可用。

下图展示了 EMQX 与 CockroachDB 数据集成的典型架构：

![CockroachDB 集成](./assets/cockroachdb_architecture.png)

将 MQTT 数据写入 CockroachDB 的流程如下：

1. **物联网设备连接到 EMQX**：设备通过 MQTT 协议成功连接后，会触发上线事件。事件包含设备 ID、源 IP 地址以及其他属性信息。
2. **消息发布与接收**：设备向特定主题发布遥测和状态数据。EMQX 接收到消息后，会在规则引擎中启动匹配过程。
3. **规则引擎处理消息**：EMQX 的规则引擎会根据主题或消息内容，将事件与消息匹配到预定义规则。处理过程可能包括数据转换（例如将 JSON 转换为 SQL 可用格式）、过滤，以及在入库前补充上下文信息。
4. **写入 CockroachDB**：匹配成功的规则会触发针对 CockroachDB 的 SQL 执行。通过 SQL 模板，用户可以将处理后的数据字段映射到 CockroachDB 的表和列中。CockroachDB 的分布式 SQL 执行与向量化查询引擎确保高吞吐写入，同时支持低延迟分析查询。数据还可以按地理区域进行分区，以优化多区域部署的性能。

当事件与消息数据写入 CockroachDB 后，您可以：

- 将 CockroachDB 连接至 Grafana 等工具，生成仪表盘和图表，实时展示 IoT 指标；
- 与设备管理平台或 AI/ML 模型集成，实现设备健康监测、异常检测和告警触发；
- 利用 CockroachDB 的分布式查询引擎，在处理实时设备遥测数据的同时，对实时数据执行复杂分析（聚合、关联、时序分析）。

## 特性与优势

与 CockroachDB 的数据集成可以为您的业务带来以下特性与优势：

- **灵活的事件处理**：借助 EMQX 的规则引擎，CockroachDB 可以以低延迟存储和处理设备生命周期事件（连接、断开、状态变化）。结合 CockroachDB 的分布式执行和自动负载均衡，事件数据始终保持高可用，并可实时分析以检测故障、异常或趋势。
- **消息转换**：通过 EMQX 的规则，消息在写入 CockroachDB 之前即可进行充分的处理和转换，使得存储的数据从一开始就适合分析。这种预处理可以降低查询复杂度并优化后续使用。
- **基于 SQL 模板的灵活数据操作**：借助 EMQX 的 SQL 模板映射，可以将结构化 IoT 数据插入或更新到 CockroachDB 的表和列中。CockroachDB 兼容 PostgreSQL，支持标准 SQL、JSONB 存储与索引，并通过其向量化执行引擎提升分析性能，同时支持跟随者读取（Follower Reads），实现区域本地的低延迟访问。
- **业务流程集成**：CockroachDB 的 PostgreSQL 兼容性允许其与 ERP、CRM、GIS 以及其他业务系统集成。结合 EMQX，可以实现事件驱动的自动化和跨系统编排，而无需构建复杂的 ETL 数据管道。
- **高级地理空间能力**：通过 PostgreSQL 扩展（如 PostGIS），CockroachDB 支持地理空间数据的存储、索引与查询。结合 EMQX 可靠的 IoT 数据接入，可以实现地理围栏、基于位置的告警、路线跟踪和实时资产监控。
- **内置指标与监控**：EMQX 为每个 CockroachDB Sink 提供运行时指标（消息数量、成功/失败率、吞吐量），而 CockroachDB 提供内置可观测性工具，并可与 Prometheus 和 Grafana 集成，实现详细的性能与健康监控。

## 准备工作

本节介绍在开始创建 CockroachDB 数据集成之前需要完成的准备工作，包括如何部署 CockroachDB 集群以及如何创建数据库和数据表。

### 前置准备

- 了解 EMQX 数据集成[规则](./rules.md)
- 了解[数据集成](./data-bridges.md)

### 在 CockroachDB 中创建数据库和数据表

在 EMQX 中创建 CockroachDB 连接器之前，需确保已有 CockroachDB 集群在运行，并且已创建用于存储 IoT 数据的数据库和数据表。

1. 创建一个 CockroachDB 集群。

   - 对于 CockroachDB Cloud，请参考 [CockroachDB Cloud 文档](https://www.cockroachlabs.com/docs/cockroachcloud)来创建集群。
   - 对于自托管部署，请参考[安装指南](https://www.cockroachlabs.com/docs/stable/install-cockroachdb-linux.html)。

2. 为 EMQX 创建一个专用 SQL 用户，参考 [CockroachDB 用户管理指南](https://www.cockroachlabs.com/docs/cockroachcloud/managing-access#manage-sql-users-on-a-cluster)。
    在本示例中，SQL 用户命名为 `emqx_user`，稍后将在配置 CockroachDB 连接器时使用。该用户需具备以下权限：

   - 连接目标数据库
   - 创建数据表
   - 读写 EMQX 数据表

3. 按照[创建数据库指南](https://www.cockroachlabs.com/docs/cockroachcloud/managing-access#manage-sql-users-on-a-cluster)创建一个数据库。在本示例中，数据库名称为 `emqx_data`。

4. 连接到 `emqx_data` 数据库，并创建两个数据表用于存储 MQTT 消息和客户端事件数据。 可参考[创建数据表指南](https://www.cockroachlabs.com/docs/v25.3/schema-design-table#create-a-table)。

   - 使用以下 SQL 创建 `t_mqtt_msg` 表，用于存储包含客户端 ID、主题、QoS、消息负载及到达时间等元数据的 MQTT 消息：

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

## 创建 CockroachDB 连接器

在添加 CockroachDB Sink 之前，需要先在 EMQX 中创建 CockroachDB 连接器。连接器定义了 EMQX 如何连接到 CockroachDB 集群，无论是自托管部署还是在 CockroachDB Cloud 中部署。

1. 在 EMQX Dashboard 中，进入**集成** -> **连接器**页面。

2. 点击页面右上角的**创建**。

3. 在**创建连接器**页面中，选择  **CockroachDB**，点击**下一步**。

4. 输入连接器名称：必须以字母或数字开头，可以包含字母、数字、连字符（-）或下划线（_），例如：`my_cockroachdb`。

5. 输入连接信息：

   - **服务器地址**：CockroachDB 集群的主机名或 IP 地址。
     - **CockroachDB Cloud**：使用 CockroachDB Cloud 控制台中提供的连接字符串里的 host 值（例如：`free-tier.gcp-us-central1.cockroachlabs.cloud`）。
     - **自托管**：使用 CockroachDB 实际运行的地址（本地示例为 `127.0.0.1`，或服务器的公网/私网 IP）。
   - **数据库名字**：EMQX 将写入数据的 CockroachDB 目标数据库名称。本示例为 `emqx_data`。
   - **用户名**：CockroachDB 中用于认证和标识的 SQL 用户名。本示例为 `emqx_user`。
   - **密码**：`emqx_user` 对应的密码。
   - **启用 TLS**：如需建立加密连接，切换该开关。更多 TLS 连接信息请参见[启用 TLS 访问外部资源](../network/overview.md#启用-tls-加密访问外部资源)。

6. **高级设置**（可选）：可配置连接池大小、空闲超时、请求超时等其他连接属性。

7. 点击**测试连接**，验证 EMQX 是否能够使用所提供的配置成功连接到 CockroachDB 集群。

8. 点击**创建**保存连接器。

9. 创建完成后，可以选择：

   - 点击**返回连接器列表**查看所有连接器，或
   - 点击**创建规则**立即创建使用该连接器将数据转发到 CockroachDB 的规则。

   详细示例请参见：

   - [创建 CockroachDB 消息存储规则](#创建-cockroachdb-消息存储规则)
   - [创建 CockroachDB 事件记录规则](#创建-cockroachdb-事件记录规则)

## 创建 CockroachDB 消息存储规则

本节演示如何在 Dashboard 中创建一条规则，用于处理来自源 MQTT 主题 `t/#` 的消息，并通过已配置的 Sink 将处理后的数据保存到 CockroachDB 表 `t_mqtt_msg` 中。

1. 在 Dashboard 中，进入**集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 在 SQL 编辑器中输入规则 ID `my_rule` 和规则 SQL。此处选择将主题为 `t/#` 的 MQTT 消息存储到 CockroachDB，请确保规则 SQL（`SELECT` 部分）中选择的字段包含 SQL 模板中使用的所有变量。示例 SQL 如下：

   ```sql
   SELECT
    *
   FROM
    "t/#"
   ```

   ::: tip

   如果是初学者，可以点击 **SQL 示例** 和**启用调试**来学习和测试 SQL 规则。

   :::

4. 点击 **+ 添加动作**按钮，定义规则触发时要执行的动作。通过该动作，EMQX 会将规则处理后的数据发送到 CockroachDB。

5. 从**动作类型**下拉列表中选择 CockroachDB，保持动作下拉框为默认的`创建动作`选项，或者从列表中选择一个之前已创建的 CockroachDB 动作。本示例将新建一个 Sink 并将其添加到规则中。

6. 在表单中输入 Sink 的名称与描述。

7. 在**连接器**下拉框中选择刚刚创建的 `my_cockroachdb`  连接器。您也可以点击下拉框旁的按钮新建连接器。连接器配置方法参见[创建 CockroachDB 连接器](#创建-cockroachdb-连接器)。

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

11. 在点击**创建**之前，可以先点击**测试连接**以验证 Sink 是否可以连接到 CockroachDB。

12. 点击**创建**完成 Sink 配置。此时会在**动作输出**中新增一个 Sink。

13. 在**创建规则**页面中检查配置信息后，点击**保存**生成规则。

现在您已成功创建了规则，可以点击**集成** -> **规则**页面看到新建的规则，同时在**动作 (Sink)**  标签页看到新建的 CockroachDB Sink。

您也可以点击**集成** -> **Flow 设计器**查看拓扑，直观地看到主题 `t/#` 下的消息在经过规则 `my_rule` 解析后被写入到 CockroachDB 中。

## 创建 CockroachDB 事件记录规则

本节展示如何创建用于记录客户端上/下线状态的规则，并通过配置的 Sink 将事件记录数据写入到 CockroachDB 的数据表 `emqx_client_events` 中。

注意：除 SQL 模板与规则外，其他操作步骤与[创建 CockroachDB 消息存储规则](#创建-cockroachdb-消息存储规则)章节完全相同。

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
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello CockroachDB" }'
```

分别查看两个 Sink 运行统计，消息存储 Sink 应显示 1 条新接收的消息和 1 条新写入的消息。事件记录 Sink 应显示 2 条新事件记录。

查看数据是否已经写入表中，`t_mqtt_msg` 表：

```bash
emqx_data=# select * from t_mqtt_msg;
 id |              msgid               | sender | topic | qos | retain |            payload
        |       arrived
----+----------------------------------+--------+-------+-----+--------+-------------------------------+---------------------
  1 | 0005F298A0F0AEE2F443000012DC0002 | emqx_c | t/1   |   0 |        | { "msg": "hello CockroachDB" } | 2023-01-19 07:10:32
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

