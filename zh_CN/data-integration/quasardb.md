# 将 MQTT 数据写入到 QuasarDB

[QuasarDB](https://www.quasardb.net/) 是一款高性能的列式时序数据库，专为存储和查询大规模时间戳数据而设计。EMQX 支持与 QuasarDB 集成，使您能够将 MQTT 消息和客户端事件保存到 QuasarDB，以便构建物联网遥测数据管理和分析的数据管道。

本页详细介绍了 EMQX 与 QuasarDB 的数据集成，并提供了实用的连接器和规则创建指导。

## 工作原理

QuasarDB 数据集成是 EMQX 的开箱即用功能，结合了 EMQX 的设备接入、消息传输能力与 QuasarDB 高性能的时序存储能力。通过内置的[规则引擎](./rules.md)组件和 Sink，您可以将 MQTT 消息和客户端事件存储到 QuasarDB 中。该集成简化了从 EMQX 到 QuasarDB 的数据摄取过程，无需复杂的编码。

下图展示了 EMQX 和 QuasarDB 之间数据集成的典型架构：

<!-- TODO: add architecture diagram -->

将 MQTT 数据摄取到 QuasarDB 的工作流程如下：

1. **消息发布和接收**：物联网设备通过 MQTT 协议成功连接到 EMQX，并将实时 MQTT 数据发布到 EMQX。当 EMQX 接收到这些消息时，它将在其规则引擎中启动匹配过程。
2. **消息数据处理**：当消息到达时，它会通过规则引擎进行处理，然后由 EMQX 中定义的规则处理。规则根据预定义的标准确定哪些消息需要路由到 QuasarDB。如果任何规则指定了载荷转换，那些转换将被应用，例如转换数据格式、过滤出特定信息，或用额外的上下文丰富载荷。
3. **数据写入到 QuasarDB**：规则触发将消息写入 QuasarDB 的操作。借助 SQL 模板，用户可以从规则处理结果中提取数据来构造 INSERT 语句并发送到 QuasarDB 执行，从而将消息的特定字段写入对应的数据表中。
4. **数据存储和利用**：数据现存储在 QuasarDB 中，企业可以利用其时序查询能力进行分析、监控和运营。

## 特性与优势

与 QuasarDB 的数据集成提供了一系列特性和优势：

- **实时数据流**：EMQX 专为处理实时数据流而构建，确保了从源系统到 QuasarDB 的数据传输的高效性和可靠性，非常适合需要立即洞察和行动的用例。
- **高性能时序存储**：QuasarDB 的列式引擎针对时序工作负载进行了优化，提供快速的写入吞吐量，并能对大量时间戳数据进行高效的范围查询。
- **数据转换的灵活性**：EMQX 提供了强大的基于 SQL 的规则引擎，允许组织在将数据存储到 QuasarDB 之前进行预处理，支持过滤、路由、聚合和丰富等多种数据转换机制。
- **批量写入支持**：QuasarDB Sink 支持批量写入，减少网络往返次数，提升整体写入吞吐量。

## 准备工作

本节介绍了在 EMQX 中创建 QuasarDB 数据集成之前需要做的准备工作，包括如何安装并配置 ODBC 驱动程序以及安装 QuasarDB。

### 前置准备

- 了解[规则](./rules.md)。
- 了解[数据集成](./data-bridges.md)。

### 安装并配置 ODBC 驱动程序

QuasarDB 连接器通过 ODBC 连接数据库。在创建连接器之前，您需要在运行 EMQX 的主机上安装并配置 QuasarDB ODBC 驱动程序。

完整安装说明请参考 [QuasarDB ODBC 文档](https://doc.quasar.ai/master/user-guide/integration/odbc.html)。以下步骤展示了在基于 Debian 的系统上使用驱动程序 3.14.1 版本的典型配置流程。

1. 下载并安装 QuasarDB C API 包和 ODBC 驱动程序：

   ```bash
   curl -fsSL -O https://download.quasar.ai/quasardb/3.14/3.14.1/api/c/qdb-api_3.14.1.deb
   curl -fsSL -O https://download.quasar.ai/quasardb/3.14/3.14.1/api/odbc/qdb-3.14.1-linux-64bit-odbc-driver.tar.gz
   apt-get install -yqq ./qdb-api_3.14.1.deb
   tar -C /tmp/qdb_odbc_driver -xf qdb-3.14.1-linux-64bit-odbc-driver.tar.gz
   ```

2. 在 `/etc/odbcinst.ini` 中注册驱动程序：

   ```ini
   [qdb_odbc_driver]
   Description=Quasardb ODBC Driver
   Driver=/tmp/qdb_odbc_driver/lib/libqdb_odbc_driver.so
   Setup=/tmp/qdb_odbc_driver/lib/libqdb_odbc_driver.so
   ```

3. 在 `/etc/odbc.ini` 中创建数据源名称（DSN）条目：

   ```ini
   [qdb]
   Driver = qdb_odbc_driver
   Description = QuasarDB ODBC Data Source
   #URI = qdb://172.100.239.30:2836
   #UID = user_name
   #PWD = user_key
   #KEY = cluster_public_key
   ```

此处配置的 DSN 名称（如 `qdb`）即为创建连接器时在**数据源名称**字段中填写的值。

### 安装并连接到 QuasarDB

本节介绍如何使用 Docker 启动 QuasarDB 实例。

1. 拉取并启动 QuasarDB Docker 镜像：

   ```bash
   docker run -d --name qdb \
     -p 2836:2836 \
     bureau14/qdb:3.14.1
   ```

   ::: tip

   QuasarDB 要求使用 **IP 地址**而非主机名进行连接。请在 URI 中使用 `127.0.0.1` 或实际主机 IP，不支持基于主机名的连接。

   :::

2. 使用 QuasarDB Shell 验证实例是否正常运行：

   ```bash
   docker run -it --rm bureau14/qdbsh --cluster qdb://127.0.0.1:2836
   ```

如需启用用户认证或集群密钥认证，请参考 [QuasarDB 安全文档](https://doc.quasar.ai/)。

### 创建数据表

在 QuasarDB 中创建用于接收写入数据的表。以下示例创建一张存储温湿度数据的表：

```sql
CREATE TABLE temp_hum (temp DOUBLE, hum DOUBLE);
```

::: tip

QuasarDB 数据表始终包含一个隐式的 `$timestamp` 索引列，创建表时无需声明，但可以在 INSERT 语句中引用它。

:::

## 创建连接器

本节介绍如何创建连接器，将 EMQX 连接到 QuasarDB。

1. 进入 EMQX Dashboard，点击**集成** -> **连接器**。

2. 点击页面右上角的**创建**。

3. 在**创建连接器**页面，选择 **QuasarDB**，然后点击**下一步**。

4. 输入连接器名称，必须为大小写字母和数字的组合，例如 `my_quasardb`。

5. 填写连接信息：

   - **URI**：输入 QuasarDB 集群的 URI，需使用 IP 地址，例如 `qdb://127.0.0.1:2836`。默认端口为 `2836`。
   - **数据源名称**：输入在 `/etc/odbc.ini` 中定义的 DSN 名称，例如 `qdb`。
   - **用户名**：输入用户名（如有）。
   - **密码**：输入用户密钥（如有）。
   - **集群公钥**：输入集群公钥（如有）。
6. 高级设置（可选）：详情请参考[高级设置](#高级设置)。

7. 点击**创建**之前，可先点击**测试连接**，验证 EMQX 是否能成功连接到 QuasarDB。

8. 点击**创建**按钮完成连接器创建。页面将弹出**创建成功**对话框，询问是否立即创建规则。点击**创建规则**可直接进入规则创建页面且连接器已预先选中；点击**返回连接器列表**则返回列表，稍后再创建规则。

## 创建 QuasarDB Sink 规则

本节介绍如何在 Dashboard 中创建一条规则，处理来自 MQTT 主题 `t/#` 的消息，并通过配置的 Sink 将处理后的数据保存到 QuasarDB 表 `temp_hum` 中。

1. 如果您在上一步点击了**创建规则**，**添加动作**面板将自动打开，且**动作类型**已设置为 `QuasarDB`、连接器已预先选中。可直接跳至第 5 步。

   否则，请进入 EMQX Dashboard，点击**集成** -> **规则**，点击右上角的**创建**，然后点击 **+ 添加动作**。

2. 在左侧 **SQL 编辑器**中输入规则 ID 及以下 SQL，匹配主题 `t/#` 下的消息：

   注意：如需自定义 SQL 语法，请确保 `SELECT` 部分包含了 Sink 所需的所有字段。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   如果您是初学者，可点击 **SQL 示例**和**启用调试**来学习和测试 SQL 规则。

   :::

3. 在右侧**添加动作**面板中，从**动作类型**下拉列表选择 `QuasarDB`，**动作**下拉框保持默认的`创建动作`。

4. 在**连接器**下拉框中选择刚创建的 `my_quasardb` 连接器。也可以点击下拉框旁的按钮创建新连接器，配置参数请参考[创建连接器](#创建连接器)。

5. 输入 Sink 的名称和可选描述。

6. 配置 **SQL 模板**，定义数据写入 QuasarDB 的方式。

   ::: tip 注意

   SQL 模板仅接受 **INSERT** 语句，不支持 UPDATE、DELETE 等其他语句类型。

   :::

   SQL 模板支持占位符变量，如 `${clientid}`。QuasarDB 使用 `$timestamp` 作为隐式时间戳索引列，可使用 `now()` 插入当前服务器时间。

   ::: tip 注意

   QuasarDB ODBC 驱动程序不支持预处理语句。在 SQL 模板中，所有解析为 `STRING` 或 `BLOB` 类型的值都必须手动用单引号（`'`）包裹。

   :::

   ```sql
   insert into temp_hum($timestamp, temp, hum)
   values (now(), ${.temp}, ${.hum})
   ```

7. **备选动作（可选）**：定义一个或多个备选动作，以提高消息投递失败时的可靠性。详情请参考[备选动作](./data-bridges.md#备选动作)。

8. **高级设置（可选）**：详情请参考 [Sink 高级设置](#sink-高级设置)。

9. 点击**创建**之前，可先点击**测试连接**，验证 Sink 能否连接到 QuasarDB。

10. 点击**创建**按钮完成 Sink 配置，新 Sink 将添加到**动作输出**中。

11. 返回**创建规则**页面，确认配置信息无误后，点击**保存**生成规则。

至此，您已成功创建规则。您可以在**集成** -> **规则**页面看到新创建的规则，点击**动作（Sink）**标签可查看新的 QuasarDB Sink。

您还可以点击**集成** -> **流程设计器**查看拓扑图，确认主题 `t/#` 下的消息经规则 `my_rule` 处理后被转发并保存到 QuasarDB。

## 测试规则

使用 MQTTX 向主题 `t/1` 发送一条消息以触发规则：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "temp": "27.5", "hum": "41.8" }'
```

查看 QuasarDB Sink 的运行统计，应有 1 条新的匹配消息和 1 条新的发出消息。在 QuasarDB 中查询 `temp_hum` 表，验证数据是否已成功写入。

## 高级设置

本节介绍 QuasarDB 连接器和 Sink 的高级配置选项。在 Dashboard 中进行配置时，您可以展开**高级设置**，根据具体需求调整以下参数。

### 连接器高级设置

| 字段名称 | 描述 | 默认值 |
| --- | --- | --- |
| 连接池大小 | 连接池中维护的并发连接数。连接池过大可能导致资源耗尽，过小则可能限制吞吐量。 | `8` |
| 连接超时 | 与 QuasarDB 建立连接时的最大等待时长。 | `5` 秒 |
| 启动超时时间 | 连接器等待自动启动的资源进入健康状态的最长时间，超时后才响应资源创建请求。 | `5` 秒 |
| 健康检查间隔 | 连接器对 QuasarDB 连接执行自动健康检查的时间间隔。 | `15` 秒 |
| 健康检查超时 | 每次健康检查的最大允许时长。 | `60` 秒 |

### Sink 高级设置

| 字段名称 | 描述 | 默认值 |
| --- | --- | --- |
| 缓存池大小 | 负责处理 EMQX 与 QuasarDB 之间数据流的缓冲工作进程数量。高负载场景下可适当调大以提升吞吐量。 | `16` |
| 请求超期 | 请求在缓冲区中的最长有效时长。超过此时间仍未发送，或已发送但未收到 QuasarDB 确认的请求，将被丢弃。 | `45` 秒 |
| 健康检查间隔 | Sink 对 QuasarDB 连接执行自动健康检查的时间间隔。 | `15` 秒 |
| 健康检查间隔抖动 | 在健康检查间隔上叠加的随机延迟，用于避免多个节点同时触发健康检查。多个 Action 或 Source 共享同一连接器时尤为有用。 | `0` 毫秒 |
| 健康检查超时 | Sink 每次健康检查的最大允许时长。 | `60` 秒 |
| 缓存队列最大长度 | 每个缓冲工作进程可缓冲的最大字节数。负载存在突发峰值时，可适当调大此值。 | `256` MB |
| 最大批量请求大小 | 单次向 QuasarDB 写入的最大记录数。设置为 `1` 时禁用批量写入，改为逐条发送。 | `100` |
| 请求模式 | `异步`模式下，写入 QuasarDB 不阻塞 MQTT 消息发布，吞吐量更高，但可能造成乱序；`同步`模式下，每次写入等待确认后再继续。 | `异步` |
| 请求飞行队列窗口 | 同时允许的最大在途请求数。**请求模式**为 `异步` 时，如需保证同一 MQTT 客户端的消息严格有序，应将此值设为 `1`。 | `100` |
