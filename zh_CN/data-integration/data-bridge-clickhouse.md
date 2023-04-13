# ClickHouse 

ClickHouse 是一个高性能、列式存储的分布式数据库管理系统，专门用于处理大规模数据。它具有出色的查询性能、灵活的数据模型和可扩展的分布式架构，适用于多种数据分析场景。
EMQX 将客户端消息和事件存储到 [ClickHouse](https://clickhouse.com/)。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用 (opens new window)](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

::: tip 前置准备

- 了解 EMQX 数据集成[规则](./rules.md)

- 了解[数据桥接](./data-bridges.md)

- 了解 UNIX 终端及命令

:::

## 功能清单

- [连接池](./data-bridges.md)
- [异步请求模式](./data-bridges.md)
- [批量模式](./data-bridges.md)
- [缓存队列](./data-bridges.md)

## 快速开始

本节将带您创建一个 ClickHouse 服务器，然后在 EMQX 创建 ClickHouse 的数据桥接，之后再通过创建一条规则来将数据转发至 ClickHouse，以验证该数据桥接是否正常工作。

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
       arrived UnixTimestamp
   ) ENGINE = MergeTree();
   SQL_INIT
   ```

2. 运行以下命令通过 Docker 启动 ClickHouse 服务器，其中定义了数据库的名称、端口号、用户名和密码，方便后续通过 EMQX 创建数据桥接时配置连接信息。同时我们还将 `init.sql` 文件映射为容器内的一个文件。 

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

### 连接到 ClickHouse

本节将通过 Dashboard 演示如何创建到 ClickHouse 的数据桥接。

1. 登陆 EMQX Dashboard，点击左侧目录菜单中的**数据集成** -> **数据桥接**。

2. 点击页面右上角的**创建**。

3. 在**数据桥接类型**中选择 **ClickHouse**，点击**下一步**。

4. 输入数据桥接名称，名称应为大/小写字母和数字的组合。

5. 输入连接信息：
   
   * **服务器URL**： **http://127.0.0.1:18123**
   * **数据库名称**：**mqtt_data**
   * **用户名**：**emqx**
   * **密码**：**public**
   
7. **分隔符**（可选）：用于区分多个输入项，本教程中可保留默认的  `,`  。注意：您只需在启用[批量模式](./data-bridges.md)、且使用其他 [ClickHouse 数据格式](https://clickhouse.com/docs/en/sql-reference/statements/insert-into)时才需更改设置。
   
7. 在 SQL 模版中输入以下命令（您可通过[规则引擎](./rules.md)确保输入 SQL 语句中的字符串能被正确转义，以防 SQL 注入攻击）：
   
   ```sql
   INSERT INTO messages(data, arrived) VALUES ('${data}', ${timestamp})
   ```
   其中，`${data}` 和 `${timestamp}` 分别代表消息内容和时间戳，即稍后将在[规则](#创建数据转发规则)时进行配置的消息转发内容。EMQX 在进行消息转发前会按照设定将其替换为相应内容。
   
8. 高级功能（可选）：根据情况配置同步/异步模式、批量模式，具体可阅读[数据桥接的简介部分](./data-bridges.md)。

9. 点击**创建**前，您可点击**测试连接**按钮确保能连接到 ClickHouse 服务器。

10. 最后点击**创建**按钮完成数据桥接创建。

至此，您已经完成数据桥接的创建，在 Dashboard 的数据桥接页面，可以看到 ClickHouse 数据桥接的状态为**已连接**。接下来，我们将继续创建一条规则来指定需要转发至 ClickHouse 的数据。

### 创建数据转发规则

1. 转到 Dashboard **数据集成** -> **规则**页面。

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

5. 点击**添加动作**按钮，在下拉框中选择**使用数据桥接转发**，选择之前创建好的 ClickHouse 数据桥接。

7. 点击**添加**按钮确认添加动作。
8. 点击最下方**创建**按钮完成规则创建。

至此我们已经完成数据桥接和转发规则的创建，您可前往 **数据集成** -> **Flows** 页面查看拓扑图，可看到 `t/#` 主题的消息已被转发至 ClickHouse。

### 测试规则与数据桥接

您可通过 EMQX Dashboard 内置的 WebSocket 客户端进行规则和数据桥接的验证。在 Dashboard 页面，点击左侧导航目录中的 **问题分析** -> **WebSocket 客户端**。

按照以下步骤建立 WebSocket 客户端并通过该客户端向主题  `t/test` 发送一条消息：

1. 填写当前 EMQX 的连接信息。由于本教程中的 EMQX 在本地运行，因此除您修改过配置外（如修改过访问规则的配置，需要输入用户名和密码），可直接使用默认配置。
2. 点击**连接**，建立该 MQTT 客户端与 EMQX 的连接。
3. 前往发布区域，并进行如下配置：
   * 主题：`t/test`
   * Payload：`Hello World ClickHouse from EMQX`
   * QoS：2
4. 点击**发布**完成消息的发送。

此时，该消息应该已被转发至 ClickHouse 服务器的 `mqtt_data` 数据库中，并插入 `messages` 表中。你可在命令行中输入如下命令进行确认：

```bash
curl -u emqx:public -X POST -d "SELECT * FROM mqtt_data.messages" http://localhost:18123
```

如消息被正确转发，将能看到类似的返回结果：

```
Hello World Clickhouse from EMQX        1679932005
```
