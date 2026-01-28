# 将 MQTT 数据写入到 Apache IoTDB

[Apache IoTDB](https://iotdb.apache.org/) 是一款高性能、可扩展的时序数据库，专为处理来自各类 IoT 设备和系统的大规模时序数据而设计。

EMQX 提供与 Apache IoTDB 的无缝数据集成能力，使 EMQX 接收的实时 MQTT 消息能够通过 [REST API V2](https://iotdb.apache.org/UserGuide/latest/API/RestServiceV2.html) 转发并写入 IoTDB。该集成采用单向数据流，将 MQTT 数据写入 IoTDB，用于高效的时序数据存储与分析。

本页面介绍如何将 EMQX 与 Apache IoTDB 进行集成，并提供创建和验证数据集成的实用指南。

## 工作原理

Apache IoTDB 数据集成是 EMQX 的内置功能，可在无需额外编码的情况下，将基于 MQTT 的时序数据写入 Apache IoTDB。该集成通过 EMQX 内置的[规则引擎](./rules.md)，简化了数据的过滤、转换和转发过程，实现高效的 IoTDB 存储与查询。

下图展示了 EMQX 与 IoTDB 之间的典型数据集成架构。<!-- 这张图片需要修改为特定于IoTDB的-->

<img src="./assets/IoTDB_bridge_architecture.png" alt="IoTDB_bridge_architecture" style="zoom:67%;" />

数据集成的工作流程如下：

1. **消息发布与接收**：设备通过 MQTT 协议连接到 EMQX，并发布包含遥测数据、状态信息或事件数据的消息。规则引擎会对接收到的消息进行匹配。
1. **基于规则的消息处理**：匹配到规则的消息会被选中进行处理，并可按需执行字段过滤、数据格式转换或数据增强等操作。
1. **数据缓冲**：当 IoTDB 临时不可用时，EMQX 会将消息缓存在内存中以提高可靠性；必要时，缓冲数据可写入磁盘以避免内存压力。但当数据集成或 EMQX 节点重启时，缓冲数据不会被保留。
1. **写入 IoTDB**：对于匹配规则的消息，EMQX 会触发 IoTDB Sink，将处理后的数据作为时序数据写入 IoTDB。
1. **数据存储与使用**：数据写入 IoTDB 后，可用于设备监控、资产跟踪、预测性维护和业务运行优化等下游分析场景。

## 特性与优势

IoTDB 数据集成为高效的时序数据处理与存储提供了以下关键能力：

- **免代码 IoT 数据管道**

  通过内置规则和 Sink，在 EMQX 与 Apache IoTDB 之间构建完整的 MQTT 到时序数据管道，无需编写自定义代码或引入外部服务。

- **灵活的 MQTT 到 IoTDB 数据模型映射**

  同时支持树模型和表模型，可根据设备建模方式和查询需求，将 MQTT 数据写入 IoTDB 的合适结构中。

- **采集与存储解耦**

  EMQX 负责承载突发的高频 MQTT 流量，IoTDB 专注于可靠的时序数据存储，从而提升系统稳定性与整体韧性。

- **面向生产环境的可扩展性**

  随设备数量和数据规模的增长实现水平扩展，适用于大规模 IoT、IIoT 及能源等场景。

- **即用型时序数据分析能力**

  写入 IoTDB 的数据可直接用于查询、聚合与分析，并可与大数据引擎集成，实现高级分析与长期洞察。

## 准备工作

本节描述了在 EMQX Dashboard 中创建 Apache IoTDB 数据集成之前您必须完成的准备工作。

### 前置准备

- 了解EMQX数据集成[规则](./rules.md)
- 了解[数据集成](./data-bridges.md)

### 启动 Apache IoTDB 服务器

本节将介绍如何通过 [Docker](https://www.docker.com/) 启动 Apache IoTDB 服务器。 确保在您的 IoTDB 配置中具备该字段： `enable_rest_service=true`。

在 REST 接口开启的情况下运行下面的命令启动 Apache IoTDB 服务器：

```bash
docker run -d --name iotdb-service \
              --hostname iotdb-service \
              -p 6667:6667 \
              -p 18080:18080 \
              -e enable_rest_service=true \
              -e cn_internal_address=iotdb-service \
              -e cn_target_config_node_list=iotdb-service:10710 \
              -e cn_internal_port=10710 \
              -e cn_consensus_port=10720 \
              -e dn_rpc_address=iotdb-service \
              -e dn_internal_address=iotdb-service \
              -e dn_target_config_node_list=iotdb-service:10710 \
              -e dn_mpp_data_exchange_port=10740 \
              -e dn_schema_region_consensus_port=10750 \
              -e dn_data_region_consensus_port=10760 \
              -e dn_rpc_port=6667 \
              apache/iotdb:2.0.5-standalone
```

有关如何通过 Docker 运行 IoTDB 的更多信息，请参阅： [IoTDB in Docker on Docker Hub](https://hub.docker.com/r/apache/iotdb)。

### 创建数据库

IoTDB 支持两种数据建模方式：树模型和表模型。  在创建数据库前，请先确认后续在连接器和 Sink 中使用的 **SQL 方言**（树模型或表模型），并根据所选模型创建对应的数据库。

- 对于**树模型**，只需创建数据库即可。
- 对于**表模型**，在创建数据库后，还需要创建表用于数据写入。

具体步骤请参考 IoTDB 用户手册：

- [树模型创建数据库](https://iotdb.apache.org/zh/UserGuide/latest/Basic-Concept/Operate-Metadata_apache.html#_1-1-%E5%88%9B%E5%BB%BA%E6%95%B0%E6%8D%AE%E5%BA%93)
- [表模型创建数据库](https://iotdb.apache.org/zh/UserGuide/latest-Table/Basic-Concept/Database-Management_apache.html#_1-1-%E5%88%9B%E5%BB%BA%E6%95%B0%E6%8D%AE%E5%BA%93)
- [表模型创建表](https://iotdb.apache.org/zh/UserGuide/latest-Table/Basic-Concept/Table-Management_apache.html#_1-1-%E5%88%9B%E5%BB%BA%E8%A1%A8)

## 创建 IoTDB 连接器

要创建 Apache IoTDB 数据集成，需要先创建一个连接器，用于将 Apache IoTDB Sink 连接到 Apache IoTDB 服务器。以下步骤假定 EMQX 和 Apache IoTDB 均在本地运行。如果您在远程运行 Apache IoTDB 和 EMQX，请根据实际情况调整相应配置。

EMQX 支持通过 REST API 或 Thrift 协议与 IoTDB 通信。

1. 登录 EMQX Dashboard，进入**集成**->**连接器**。

2. 在页面右上角点击**创建**。

3. 在**创建连接器**页面中，选择 **Apache IoTDB**。

4. 配置连接器：

   - **连接器名称**：输入连接器的唯一名称。可由大小写字母或数字组成，例如 `my_iotdb`。
   - **描述**：（可选）填写连接器的简要说明。
   - **驱动**：选择用于连接 IoTDB 的协议。
     - `REST API`：在 **IoTDB REST 服务基础 URL** 中输入 IoTDB REST 服务地址，例如 `http://localhost:18080`。
     - `Thrift 协议`：在**服务器地址**中输入 IoTDB Thrift 服务器地址，例如：`localhost:6667`。
   - **SQL 方言**：选择 EMQX 向 IoTDB 写入设备数据时使用的数据模型。
     - `树模型`：将数据写入为分层的时序路径，适用于基于路径的设备和测点管理。
     - `表模型`：将数据写入关系表中，适用于按设备类型或设备类别管理数据。
   - **数据库名字**：当 **SQL 方言**选择表模型时，需要配置所连接到的数据库名称。
   - **用户名**和**密码**：输入 EMQX 用于认证 Apache IoTDB 服务器的凭据。
   - **IoTDB 版本**：选择 Apache IoTDB 的部署版本。此处，选择 `v2.0.x`。
   - **启用 TLS**：启用该选项以建立与 Apache IoTDB 服务器之间的加密连接。更多信息请参见[启用 TLS 加密访问外部资源](../network/overview.md#启用-tls-加密访问外部资源)。
   - 如需进行可选调优配置，请参见**高级设置**，详见[高级设置](#高级设置)。

5. （可选）点击**测试连接**，验证连接器是否能够成功连接到 Apache IoTDB 服务器。

6. 点击**创建**完成连接器创建。

   在弹出的对话框中，可以选择**返回连接器列表**，或点击**创建规则**继续配置规则和 Apache IoTDB Sink。详细步骤请参见[创建 Apache IoTDB Sink 规则](#创建-apache-iotdb-sink-规则)。

## 创建 Apache IoTDB Sink 规则

本节介绍如何在 EMQX 中创建一条规则，用于处理来自 MQTT 源主题 `root/#` 的消息，并通过已配置的 Apache IoTDB Sink 将处理后的结果写入 Apache IoTDB 中存储时序数据。

### 使用自定义 SQL 创建规则

1. 登录 EMQX Dashboard，点击**集成**->**规则**。

2. 在页面右上角点击**创建**。

3. 输入规则 ID，例如 `my_rule`。

4. 在 **SQL 编辑器**中输入以下语句，用于转发匹配主题 `root/#` 的 MQTT 消息：

   ```sql
   SELECT
     *
   FROM
     "root/#"
   ```

   ::: tip

   如果你是初学者，可以点击 **SQL 示例**和**启用调试**来学习和测试 SQL 规则。

   :::

5. 为规则添加一个 Apache IoTDB Sink，用于将处理后的数据写入 IoTDB。详细步骤请参见[添加 Apache IoTDB Sink](#add-an-apache-iotdb-sink)。
6. 在**创建规则**页面中，确认配置信息后点击**保存**创建规则。

规则创建完成后，将显示在**规则**列表中。点击**动作（Sink）**页签可查看该规则关联的 IoTDB Sink。

你也可以进入**集成**->**Flow 设计器**查看拓扑图，其中将展示来自主题 `root/#` 的消息如何经由 `my_rule` 规则处理并写入 IoTDB。

### 添加 Apache IoTDB Sink

1. 点击右侧的**添加动作**按钮，定义当规则匹配时触发的动作。该动作会将处理后的数据转发至 IoTDB。

2. 在**动作类型**下拉框中选择 `Apache IoTDB`，并保持**动作**为默认的**创建动作**。你也可以选择已有的 IoTDB Sink。本示例假设创建一个新的 Sink。

3. 输入 Sink 的名称和描述。

4. 在**连接器**下拉框中，选择刚刚创建的连接器 `my_iotdb`。如果没有可用连接器，可点击右侧按钮进行创建，详见[创建 IoTDB 连接器](#创建-iotdb-连接器)。

5. 配置 Sink 的以下信息：

   - **SQL 方言**：选择 Apache IoTDB Sink 向 IoTDB 写入数据的方式。该选项必须与连接器中配置的 SQL 方言保持一致，否则数据将无法写入。

     - `树模型`：以时序路径的形式向 IoTDB 写入数据。每条 Sink 记录会写入一个设备路径，测点作为该设备下的独立时间序列。当选择此模型，您可以填写**设备 ID** 字段。
     - `表模型`：将数据写入 IoTDB 的关系表中。每条 Sink 记录对应表中的一行数据，字段映射为表中的列。当选择此模型，您必须填写**表**字段。
     
   - **设备 ID**（可选）：指定用于写入时序数据的设备名称。

     ::: tip

     如果留空，设备 ID 仍然可以在发布的消息中指定或在规则中配置。例如，如果您发布一个包含 `device_id` 字段的 JSON 编码消息，该字段的值将确定输出设备 ID。要使用规则引擎提取这些信息，您可以使用类似以下的 SQL：

     ```sql
     SELECT
      payload,
      `my_device` as payload.device_id
     ```

     但是，此字段中配置的固定设备ID优先于前面提到的任何方法。

     :::

   - **表**：指定写入数据的 IoTDB 表名。

   - **对齐时间序列**：默认禁用。启用后，一组对齐的时序数据的时间戳列将在 IoTDB 中仅存储一次，而不是在该组内的每个单独时序数据中重复存储。有关更多信息，请参见[对齐时序数据](https://iotdb.apache.org/UserGuide/V1.1.x/Data-Concept/Data-Model-and-Terminology.html#aligned-timeseries)。

6. 为 Sink 配置**写入数据**以指定从 MQTT 消息生成 IoTDB 数据的方式。

   您可以在**写入数据**中定义一个模板，包括所需的每行的上下文信息。当提供此模板时，系统将通过应用它到MQTT 消息来生成 IoTDB 数据。写入数据的模版支持通过 CSV 文件批量设置，详细说明请参考[批量设置](#批量设置)。

   例如，使用以下模板：

   ::: tip 注意

   **列类别**列仅适用于 SQL 方言为表类型的情况。

   :::

   | 列类别 | 时间戳 | 字段        | 数据类型 | 值       |
   | ------ | ------ | ----------- | -------- | -------- |
   | field  |        | index       | INT32    | ${index} |
   |        |        | temperature | FLOAT    | ${temp}  |

   每列支持占位符语法以用变量填充。如果省略时间戳，它将自动填充为当前系统时间（毫秒）。然后，您的 MQTT 消息将如下所示：

   ```json
   {
   "index": "42",
   "temp": "32.67"
   }
   ```

7. **备选动作（可选）**：如果您希望在消息投递失败时提升系统的可靠性，可以为 Sink 配置一个或多个备选动作。当 Sink 无法成功处理消息时，这些备选动作将被触发。更多信息请参见：[备选动作](./data-bridges.md#备选动作)。

8. **高级设置**（可选）：详细请参考[高级设置](#高级设置)。

9. （可选）点击**测试连接**，验证 Sink 是否能够成功连接到 Apache IoTDB 服务器。

### 批量设置

在 Apache IoTDB 中，可能需要同时写入数百条数据，在 Dashboard 上进行配置是具有挑战性的工作。为了解决这个问题，EMQX 提供了批量设置数据写入的功能。

当配置**写入数据**时，您可以使用批量设置功能，从 CSV 文件中导入要进行插入操作的字段。

1. 点击**写入数据**表格的**批量设置**按钮，打开**导入批量设置**弹窗。

2. 根据指引，先下载批量设置模板文件，然后在模板文件中填入数据写入配置，默认的模板文件内容如下：

   ::: tip 注意

   以下为 **SQL 方言**设置为`表模型`时的默认模板。  当 **SQL 方言**设置为`树模型`时，不包含 **Column Category** 列。

   :::

   | Column Category | Timestamp | Measurement | Data Type | Value             | Remarks (Optional)                                           |
   | --------------- | --------- | ----------- | --------- | ----------------- | ------------------------------------------------------------ |
   | tag             | now       | clientid    | text      | ${clientid}       |                                                              |
   | field           | now       | temp        | float     | ${payload.temp}   | 字段、值、数据类型是必填选项，数据类型可选的值为 boolean、 int32、 int64、 float、 double、 text |
   | attribute       | now       | hum         | text      | ${payload.hum}    |                                                              |
   | attribute       | now       | status      | text      | ${payload.status} |                                                              |

   - **Column Category**: 列的数据模型，可选值：tag、field、attribute。tag 必须是字符串，推荐选择 field 或者 attribute。
   - **Timestamp**: 支持使用 ${var} 格式的占位符，要求是时间戳格式。也可以使用以下特殊字符插入系统时间：
     - now: 当前毫秒级时间戳
     - now_ms: 当前毫秒级时间戳
     - now_us: 当前微秒级时间戳
     - now_ns: 当前纳秒级时间戳
   - **Measurement**: 字段名。
   - **Data Type**: 数据类型，可选值包括 boolean、 int32、 int64、 float、 double、 text。
   - **Value**: 写入的数据值，支持常量或 ${var} 格式的占位符，需要与数据类型匹配。
   - **Remarks**: 仅用于 CSV 文件内字段的备注，无法导入到 EMQX 中。


   注意，仅支持 1M 以内的 CSV 格式文件，文件中数据不能超过 2000 行。

3. 将填好的模板文件保存并上传到**导入批量设置**弹窗中，点击**导入**完成批量设置。
4. 导入完成后，您可以在**写入数据**表格中对数据进行进一步的调整。

## 测试规则

您可通过 EMQX Dashboard 内置的 WebSocket 客户端进行规则和 Sink 的验证。

1. 在 Dashboard 页面，点击左侧导航目录中的 **问题分析** -> **WebSocket 客户端**。

2. 填写当前 EMQX 的连接信息。

   - 如果 EMQX 在本地运行，可直接使用默认配置。
   - 如果您修改过 EMQX 的默认配置，如修改过访问规则的配置，则需要输入用户名和密码。

3. 点击**连接**，建立该 WebSocket 客户端与 EMQX 的连接。

4. 前往发布区域，在消息 payload 中设置设备 ID 并发布消息：

   - **主题**：`root/sg27`

     ::: tip

      如果主题不以 `root` 开头，系统将自动为其添加前缀。例如，如果您将消息发布到 `test/sg27`，生成的设备名称将为 `root.test.sg27`。请确保您的规则和主题已正确配置，以便将来自该主题的消息转发到 Sink。

     :::

   - **Payload**:

     ```json
     {
       "value": "37.6"
       "device_id": "root.sg27"
     }
     ```

      ::: tip

      `Write Data` 的模版为：

     ```
     now, "temp", float, "${payload.value}"
     ```

      :::

   - **QoS**: `2`

7. 点击**发布**完成消息的发送。

   如果 Sink 和规则创建成功，消息应该已被转发至 Apache IoTDB 服务器里指定的时序数据表中。

8. 您可以使用 IoTDB 的命令行查看。如果服务器在 docker 中运行，可以使用下面的命令连接服务器：

   ```shell
    $ docker exec -ti iotdb-service /iotdb/sbin/start-cli.sh -h iotdb-service
   ```

9. 在控制台中继续输入：

   ```sql
   IoTDB> select * from root.sg27
   ```

   您将能看到以下返回结果：

   ```
   +------------------------+--------------+
   |                    Time|root.sg27.temp|
   +------------------------+--------------+
   |2023-05-05T14:26:44.743Z|          37.6|
   +------------------------+--------------+
   ```

## 高级设置

本节描述了一些高级配置选项，可以优化您的连接器和 Sink 性能，并根据您的特定场景定制操作。创建连接器和 Sink 时，您可以展开**高级设置**并根据您的业务需求配置以下设置。

| 字段                  | 描述                                                         | 推荐值   |
| --------------------- | ------------------------------------------------------------ | -------- |
| HTTP 流水线           | 指定可以连续不间断地向服务器发送的 HTTP 请求数量，无需等待个别响应。此选项采用正整数值，表示将被管道化的最大 HTTP 请求数量。<br />当设置为 `1` 时，表示传统的请求-响应模型，其中每个 HTTP 请求将被发送，然后客户端将等待服务器响应，再发送下一个请求。较高的值可以通过允许批量发送多个请求，减少往返时间，从而更有效地利用网络资源。 | `100`    |
| 连接池类型            | 定义用于管理和分配 EMQX 与 Apache IoTDB 之间连接器中连接的算法策略。<br />当设置为 `random` 时，将从可用连接池中随机选择与 Apache IoTDB 服务器的连接。此选项提供了简单平衡的分配。<br />当设置为 `hash` 时，使用哈希算法将请求一致地映射到连接池中的连接。此类型通常用于需要更确定性请求分配的场景，例如基于客户端标识符或主题名称的负载平衡。<br />**注意**：选择适当的池类型取决于您的具体用例和您希望实现的分布特性。 | `random` |
| 连接池大小            | 指定在与 Apache IoTDB 服务接口时可以在连接池中维护的并发连接数量。此选项有助于通过限制或增加EMQX 与 Apache IoTDB 之间的活动连接数量来管理应用程序的可扩展性和性能。<br />**注意**：设置适当的连接池大小取决于系统资源、网络延迟和您的应用程序的特定工作负载等多种因素。太大的池大小可能导致资源耗尽，而太小的大小可能限制吞吐量。 | `8`      |
| 连接超时              | 指定 EMQX 在尝试与 Apache IoTDB HTTP 服务器建立连接时的最大等待时间，以秒为单位。<br />**注意**：仔细选择超时设置对于平衡系统性能和资源利用至关重要。建议在不同的网络条件下测试系统，以找到适合您特定用例的最佳超时值。 | `15`     |
| HTTP 请求最大重试次数 | 指定如果 HTTP 请求在 EMQX 与 Apache IoTDB 通信过程中未能成功完成时的最大重试次数。 | `2`      |
| 启动超时时间          | 确定连接器在响应资源创建请求之前等待自动启动的资源达到健康状态的最大时间间隔，以秒为单位。此设置有助于确保集成在验证连接资源（如 Apache IoTDB 中的数据库实例）已完全运行并准备处理数据交易之前不会继续操作。 | `5`      |
| 缓存池大小            | 指定将分配用于管理 EMQX 与 Apache IoTDB 之间出站类型桥接的数据流的缓冲工作进程数量。这些工作进程负责在将数据发送到目标服务之前临时存储和处理数据。此设置特别适用于优化性能和确保出站（出口）场景中的平稳数据传输。对于仅处理入站（入口）数据流的桥接，此选项可以设置为“0”，因为它不适用。 | `18`     |
| 请求超期              | “请求 TTL”（生存时间）配置设置指定了一旦请求进入缓冲区后，请求被认为有效的最大持续时间，以秒为单位。此计时器从请求被缓冲的那一刻开始计时。如果请求在缓冲区中停留的时间超过此 TTL 设置，或者如果发送了请求但没有及时从 Apache IoTDB 收到响应或确认，则认为请求已过期。 | `45`     |
| 健康检查间隔          | 指定连接器执行对 Apache IoTDB 连接的自动健康检查的时间间隔，以秒为单位。 | `15`     |
| 缓存队列最大长度      | 指定 Apache IoTDB 数据集成中每个缓冲工作进程可以缓冲的最大字节数。缓冲工作进程临时存储数据，然后发送到 IoTDB，充当中介以更有效地处理数据流。根据您的系统性能和数据传输需求调整此值。 | `265`    |
| 请求模式              | 允许您选择 `asynchronous` 或 `synchronous` 查询模式，以根据不同需求优化消息传输。在异步模式下，写入 IoTDB 不会阻塞 MQTT 消息发布过程。然而，这可能导致客户端在消息到达 IoTDB 之前就收到消息。 | `Async`  |
| 请求飞行队列窗口      | “在途查询”指的是已经启动但尚未收到响应或确认的查询。此设置控制在连接器与 Apache IoTDB 通信时，可以同时存在的最大在途查询数量。<br />当 `query_mode` 设置为 `async`（异步）时，“在途窗口”参数具有特殊重要性。如果对来自同一 MQTT 客户端的消息以严格顺序处理至关重要，则应将此值设置为1。 | `100`    |

## 更多信息

EMQX 提供了许多关于与 Apache IoTDB 数据集成的学习资源。查看以下链接以了解更多信息：

**博客：**

[如何将 MQTT 与时序数据库高效应用于物联网场景](https://www.emqx.com/zh/blog/time-series-database-for-iot-the-missing-piece)
