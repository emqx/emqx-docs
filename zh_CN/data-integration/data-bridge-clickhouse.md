# 将 MQTT 数据写入到 ClickHouse

[ClickHouse](https://clickhouse.com/) 是一个高性能、列式存储的分布式数据库管理系统，专门用于处理大规模数据。它具有出色的查询性能、灵活的数据模型和可扩展的分布式架构，适用于多种数据分析场景。
EMQX 将 MQTT 消息和客户端事件存储到 ClickHouse。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

::: tip 前置准备

- 了解 EMQX 数据集成[规则](./rules.md)

- 了解[数据集成](./data-bridges.md)

- 了解 UNIX 终端及命令

:::

## 功能清单

- [连接池](./data-bridges.md#连接池)
- [异步请求模式](./data-bridges.md)
- [批量模式](./data-bridges.md)
- [缓存队列](./data-bridges.md)

## 快速开始

本节将带您创建一个 ClickHouse 服务器，然后在 EMQX 创建 ClickHouse 的 Sink ，之后再通过创建一条规则来将数据转发至 ClickHouse，以验证该 Sink 是否正常工作。

:::tip

本教程假定 EMQX 与 ClickHouse 均在本地运行，如您在远程运行 EMQX 及 ClickHouse，请根据实际情况调整相应配置。

:::

### 启动 ClickHouse 服务器

本节将介绍如何通过 [Docker](https://www.docker.com/) 启动 ClickHouse 服务器。

1. 创建一个 `init.sql` 文件并包含以下初始化 SQL 命令：

   ```bash
   cat >init.sql <<SQL_INIT
   CREATE DATABASE IF NOT EXISTS mqtt_data;
   CREATE TABLE IF NOT EXISTS mqtt_data.messages (
      data String,
      arrived TIMESTAMP
   ) ENGINE = MergeTree()
   ORDER BY arrived;
   SQL_INIT
   ```

2. 运行以下命令通过 Docker 启动 ClickHouse 服务器，其中定义了数据库的名称、端口号、用户名和密码，方便后续通过 EMQX 创建 Sink 时配置连接信息。同时我们还将 `init.sql` 文件映射为容器内的一个文件。

   ```bash
   docker run \
   --rm \
   -e CLICKHOUSE_DB=mqtt_data \
   -e CLICKHOUSE_USER=emqx \
   -e CLICKHOUSE_DEFAULT_ACCESS_MANAGEMENT=1 \
   -e CLICKHOUSE_PASSWORD=public \
   -p 18123:8123 \
   -p 19000:9000 \
   --ulimit nofile=262144:262144 \
   -v ./init.sql:/docker-entrypoint-initdb.d/init.sql \
   clickhouse/clickhouse-server
   ```

有关如何通过 Docker 运行 ClickHouse 服务器的更多信息，可阅读 [Docker - ClickHouse Server](https://hub.docker.com/r/clickhouse/clickhouse-server)。

## 创建连接器和 Sink

本节将通过 Dashboard 演示如何创建到 ClickHouse 的连接器和 Sink。

1. 登陆 EMQX Dashboard，点击左侧目录菜单中的**数据集成** -> **连接器**。

2. 点击页面右上角的**创建**。

3. 在**连接器类型**中选择 **ClickHouse**，点击**下一步**。

4. 输入连接器名称，名称应为大/小写字母和数字的组合。

5. 输入连接信息：

   - **服务器 URL**： `http://127.0.0.1:18123`
   - **数据库名称**：`mqtt_data`
   - **用户名**：`emqx`
   - **密码**：`public`

6. **分隔符**（可选）：用于区分多个输入项，本教程中可保留默认的 `,` 。注意：您只需在启用[批量模式](./data-bridges.md)、且使用其他 [ClickHouse 数据格式](https://clickhouse.com/docs/en/sql-reference/statements/insert-into)时才需更改设置。
7. 在 SQL 模版中输入以下命令（您可通过[规则引擎](./rules.md)确保输入 SQL 语句中的字符串能被正确转义，以防 SQL 注入攻击）：

   ```sql
   INSERT INTO messages(data, arrived) VALUES ('${data}', ${timestamp})
   ```

   其中，`${data}` 和 `${timestamp}` 分别代表消息内容和时间戳，即稍后将在[规则](#创建数据转发规则)时进行配置的消息转发内容。EMQX 在进行消息转发前会按照设定将其替换为相应内容。

8. 高级功能（可选）：具体可阅读[ Sink 的简介部分](./data-bridges.md)。

9. 点击**创建**前，您可点击**测试连接**按钮确保能连接到 ClickHouse 服务器。

10. 最后点击**创建**按钮完成连接器和 Sink 的创建。

在 Dashboard 的连接器页面，可以看到 ClickHouse 的状态为**已连接**。接下来，我们将继续创建一条规则来指定需要转发至 ClickHouse 的数据。

## 创建数据转发规则

1. 转到 Dashboard **集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 输入规则 ID，例如 `my_rule` 。

4. 在 SQL 编辑器中输入规则，例如我们希望将 `t/#` 主题的 MQTT 消息转发至 ClickHouse，可通过如下规则 SQL 实现：

   ```sql
   SELECT
     payload as data,
     now_timestamp() as timestamp
   FROM
     "t/#"
   ```

5. 点击**添加动作**按钮，在下拉框中选择 `ClickHouse`，在**动作**下拉框中选择之前创建好的 ClickHouse 连接器 。

6. 点击**添加**按钮确认添加动作。
7. 点击最下方**创建**按钮完成规则创建。

至此您已经完成 Sink 和转发规则的创建，您可以点击**集成** -> **规则**页面看到新建的规则，同时在**动作(Sink)** 标签页看到新建的 ClickHouse Sink。您还可以前往 **数据集成** -> **Flow 设计器** 页面查看拓扑图，可看到 `t/#` 主题的消息已被存储至 ClickHouse。

## 测试规则

您可通过 EMQX Dashboard 内置的 WebSocket 客户端进行规则和 Sink 的验证。在 Dashboard 页面，点击左侧导航目录中的 **问题分析** -> **WebSocket 客户端**。

按照以下步骤建立 WebSocket 客户端并通过该客户端向主题 `t/test` 发送一条消息：

1. 填写当前 EMQX 的连接信息。由于本教程中的 EMQX 在本地运行，因此除您修改过配置外（如修改过访问规则的配置，需要输入用户名和密码），可直接使用默认配置。
2. 点击**连接**，建立该 MQTT 客户端与 EMQX 的连接。
3. 前往发布区域，并进行如下配置：
   - 主题：`t/test`
   - Payload：`Hello World ClickHouse from EMQX`
   - QoS：2
4. 点击**发布**完成消息的发送。

此时，该消息应该已被转发至 ClickHouse 服务器的 `mqtt_data` 数据库中，并插入 `messages` 表中。你可在命令行中输入如下命令进行确认：

```bash
curl -u emqx:public -X POST -d "SELECT * FROM mqtt_data.messages" http://localhost:18123
```

如消息被正确转发，将能看到类似的返回结果：

```bash
Hello World Clickhouse from EMQX        1679932005
```

## 高级配置

本节深入探讨了 EMQX ClickHouse 数据结成的高级配置选项。在 Dashboard 中配置 ClickHouse 连接器时，导航至**高级设置**以根据您的特定需求调整以下参数。

| **字段**                | **描述**                                                     | **推荐值** |
| ----------------------- | ------------------------------------------------------------ | ---------- |
| **批量值分隔符**        | 用于区分多个输入项。在本示例中，您可以保持默认值 ","。仅当您为连接器启用[批处理](./data-bridges.md)并且使用 [ClickHouse 的 FORMAT 语法](https://clickhouse.com/docs/en/sql-reference/statements/insert-into)指定其他格式时，才需要更改此设置。 | `,`        |
| **连接池大小**          | 指定与 ClickHouse 服务交互时连接池中可以维护的并发连接数量。此选项通过限制或增加 EMQX 与 ClickHouse 之间的活动连接数量，帮助管理应用的可伸缩性和性能。<br/>**注意**：设置合适的连接池大小取决于多种因素，如系统资源、网络延迟和您应用的特定工作负载。过大的池大小可能导致资源耗尽，而过小的大小可能限制吞吐量。 | `8`        |
| **Clickhouse 超时时间** | 指定连接器尝试与 ClickHouse 服务器建立连接时的最大等待时间（以秒为单位）。<br/>**注意**：谨慎选择超时设置对于平衡系统性能和资源利用至关重要。建议在各种网络条件下测试系统，以找到适合您特定用例的最佳超时值。 | `15`       |
| **启动超时时间**        | 确定连接器在响应资源创建请求之前等待自动启动资源达到健康状态的最大时间间隔（以秒为单位）。此设置有助于确保连接器在验证连接的资源 - 如 ClickHouse 中的数据库实例 - 已完全运行并准备处理数据事务之前，不会继续进行操作。 | `5`        |
| **缓冲池大小**          | 指定将为管理 EMQX 和 ClichHouse 之间的 egress （出口）模式数据集成的数据流分配的缓冲工作进程数量。这些工作进程负责在数据发送到目标服务之前临时存储和处理数据。此设置特别适用于优化性能和确保出口场景中的平稳数据传输。对于仅处理 ingress（入口）数据流的数据集成，此选项可以设置为 "0"，因为它不适用。 | `16`       |
| **请求 TTL**            | “请求 TTL”（生存时间）配置设置指定请求进入缓冲后被视为有效的最大持续时间（以秒为单位）。此计时器从请求被缓冲的那一刻开始计时。如果请求在缓冲区中停留的时间超过此 TTL 设置，或者被发送但没有从 ClickHouse 收到及时的响应或确认，请求将被视为已过期。 | `45`       |
| **健康检查间隔**        | 指定连接器对 ClickHouse 连接执行自动健康检查的时间间隔（以秒为单位）。 | `15`       |
| **最大缓冲队列大小**    | 指定 ClickHouse 连接器中每个缓冲工作进程可以缓冲的最大字节数。缓冲工作进程在数据发送到 ClickHouse 之前临时存储数据，作为中介以更有效地处理数据流。根据您的系统性能和数据传输需求调整该值。 | `256`      |
| **最大批处理大小**      | 指定从 EMQX 到 ClickHouse 的单次传输操作中可以传输的数据批次的最大大小。通过调整大小，您可以微调 EMQX 和 ClickHouse 之间数据传输的效率和性能。<br />如果“最大批处理大小”设置为“1”，则数据记录将单独发送，而不会分组到批次中。 | `1`        |
| **查询模式**            | 允许您选择 `asynchronous` 或 `synchronous` 查询模式，根据不同需求优化消息传输。在异步模式下，写入 ClickHouse 不会阻塞 MQTT 消息发布过程。然而，这可能导致客户端在消息到达 ClickHouse 之前就接收到消息。 | `Async`    |
| **Inflight 窗口**       | “in-flight 查询”是指已启动但尚未收到响应或确认的查询。此设置控制连接器与 ClickHouse 通信时可以同时存在的最大 in-flight 查询数量。<br/>当 **查询模式** 设置为 `async`（异步）时，“Inflight 窗口”参数获得特殊重要性。如果对于来自同一 MQTT 客户端的消息按严格顺序处理至关重要，您应将此值设置为 1。 | `100`      |

## 更多信息

点击以下链接了解更多：

**博客**：

- [EMQX + ClickHouse 实现物联网数据接入与分析](https://www.emqx.com/zh/blog/emqx-and-clickhouse-for-iot-data-access-and-analysis)
- [MQTT 与 ClickHouse 集成：激发实时物联网数据分析的潜能](https://www.emqx.com/zh/blog/mqtt-to-clickhouse-integration)
