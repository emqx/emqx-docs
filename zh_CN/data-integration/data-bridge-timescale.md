# Timescale

[Timescale](https://www.timescale.com/) (TimescaleDB) 是一个专门用于存储和分析时间序列数据的数据库，它的出色数据吞吐能力和可靠的性能表现使其成为物联网领域的理想选择，EMQX 与 Timescale 集成，能够将 MQTT 数据无缝的集成到 Timescale 记性存储，并借助其丰富的数据分析能力，为物联网应用提供了高效、可扩展的数据存储和分析解决方案。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

## 如何工作

Timescale 数据集成时 EMQX 中开箱即用的功能，旨在结合 EMQX 的设备接入，消息传输能力与 Timescale 的数据存储、分析能力，通过简单的配置即可实现 MQTT 数据的无缝集成。

下图展示了工业物联网中 EMQX 和 Timescale 数据集成的典型架构。

![MQTT to Timescale](./assets/mqtt-to-timescaledb.jpg)

EMQX 和 Timescale 提供了一个可扩展的物联网平台，用于高效地实时收集和分析能耗数据。在此架构中，EMQX 作为物联网平台，负责设备接入、消息传输、数据路由等功能，Timescale 作为数据存储和分析平台，负责数据存储、数据分析等功能。

EMQX 通过规则引擎与数据桥接将设备数据转发至 Timescale，Timescale 通过 SQL 语句对数据进行分析，生成报表、图表等数据分析结果，通过 Timescale 的可视化工具展示给用户。其工作流程如下：

1. 物联网设备发布消息：工业设备使用 MQTT 协议定期发布能耗数据，这些数据包括产线标识、能耗值信息。
2. 消息数据处理：作为 MQTT 服务器，EMQX 从工业设备接收这些 MQTT 消息。
3. 规则引擎处理消息：通过内置的规则引擎，可以根据主题匹配处理特定来源的消息。当消息到达时，它会通过规则引擎，规则引擎会匹配对应的规则，并对消息数据进行处理，例如转换数据格式、过滤掉特定信息或使用上下文信息丰富消息。
4. 写入到 Timescale：规则引擎中定义的规则触发将消息写入到 Timescale 的操作。Timescale 数据桥接提供了 SQL 模板，能够灵活地定义写入的数据格式，将消息中的特定字段写入到 Timescale 的对应的表和列中。

能耗数据写入到 Timescale 后，您可以灵活的使用 SQL 语句对数据进行分析，例如：

- 连接到可视化工具，例如 Grafana，根据数据生成图表，展示能耗数据。
- 连接到应用系统，例如 ERP，进行生产分析与生产计划调整。
- 连接到业务系统，实时分析能源使用情况，便于数据驱动节能管理。

## Timescale 数据桥接优势

在 EMQX 中使用 Timescale 数据桥接能够为您的业务带来以下功能与优势：

1. **高效的数据处理能力**：EMQX 能够处理海量物联网设备连接与消息吞吐，TimescaleDB 在数据写入、存储和查询方面具有出色的性能表现，能够满足物联网场景下的数据处理需求，不会导致系统不堪重负。
2. **消息转换**：消息可以写入 Timescale 之前，通过 EMQX 规则中进行丰富的处理和转换。
3. **高效存储和可扩展性**：EMQX 与 Timescale 都具备集群扩展能力，能够随着业务的发展，利用灵活地进行集群水平扩展，满足业务的发展需求。
4.**丰富的查询能力**：Timescale 提供包括优化的函数、运算符和索引技术，可实现对时间戳数据的高效查询和分析，准确地从 IoT 时间序列数据中提取有价值的见解。


## 前置准备

::: tip 前置准备

- 了解[规则](./rules.md)。
- 了解[数据桥接](./data-bridges.md)。

:::

## 特性

- [连接池](./data-bridges.md#连接池)
- [异步请求模式](./data-bridges.md#异步请求模式)
- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)
- [SQL 预处理](./data-bridges.md#SQL-预处理)

## 快速开始教程

本节介绍了如何安装 Timescale 并创建数据表、创建规则和数据桥接以将数据转发至 Timescale以及测试规则和桥接。

以下步骤假设您在本地机器上同时运行 EMQX 和 Timescale（若私有部署）。 如果您有远程运行的 EMQX 和 Timescale，请相应地调整设置。

### 安装 Timescale 并创建数据表

EMQX 支持与私有部署的 TimescaleDB 或与云上的 Timescale Service 集成。您可以使用 Timescale Service 云服务或者 Docker 部署一个 TimescaleDB 实例。

:::: tabs 
::: tab Timescale Service

1. 如果您没有 Timescale 账户，参考[创建一个 Timescale 账户](https://docs.timescale.com/getting-started/latest/services/#create-your-timescale-account)。
  
2. 登录到 Timescale portal 并[创建 Timescale service](https://docs.timescale.com/getting-started/latest/services/#create-your-first-service), 注意保存服务的密码。

3. 在 Overview 页面获取连接信息, EMQX 所需的字段包括 **Database name**, **Host**, **Port** and **Username**。

4. 使用 `psql client` [连接到 service](https://docs.timescale.com/getting-started/latest/services/#connect-to-your-service)。

   ```bash
   # 使用 servie URL 连接
   psql "postgres://tsdbadmin@xxxxx.xxxxx.tsdb.cloud.timescale.com:32541/tsdb?sslmode=require"
   # 输入使用步骤 2 中的密码
   Password for user tsdbadmin:
   ```

5. 创建 `sensor_data` 表用于存储客户端发布的消息：

   ```sql
   CREATE TABLE sensor_data (
       time        TIMESTAMPTZ       NOT NULL,
       location    TEXT              NOT NULL,
       temperature DOUBLE PRECISION  NULL,
       humidity    DOUBLE PRECISION  NULL
   );
   
   SELECT create_hypertable('sensor_data', 'time');
   ```

完成后, 您可以在 service 详情中的 **Explorer** 页签中查看  `sensor_data` 表的信息：

![Timescale Explorer table](./assets/timescale-explorer-table.png)

:::

::: tab TimescaleDB Docker

1. 如果没有 Docker 环境请[安装 Docker](https://docs.docker.com/install/)。

2. 启动 TimescaleDB 容器，并通过环境变量设置数据库密码。

   ```bash
   docker run -d --name timescaledb \
       -p 5432:5432 \
       -e POSTGRES_PASSWORD=public \
       timescale/timescaledb:latest-pg13
   ```

3. 创建数据库。

   ```bash
   docker exec -it timescaledb psql -U postgres
   
   ## create tsdb database
   > CREATE database tsdb;
   
   > \c tsdb;
   ```

4. 创建 `sensor_data` 表用于存储客户端发布的消息。

   ```sql
   CREATE TABLE sensor_data (
       time        TIMESTAMPTZ       NOT NULL,
       location    TEXT              NOT NULL,
       temperature DOUBLE PRECISION  NULL,
       humidity    DOUBLE PRECISION  NULL
   );
   
   SELECT create_hypertable('sensor_data', 'time');
   ```

:::
::::

### 创建规则与数据桥接

1. 转到 Dashboard **数据集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 输入规则 ID `my_rule`，在 SQL 编辑器中输入规则，此处选择将 `t/#` 主题的 MQTT 消息存储至 TimescaleDB，规则 SQL 如下：

   ```sql
   SELECT
     payload.temp as temp,
     payload.humidity as humidity,
     payload.location as location
   FROM
       "t/#"
   ```

4. 添加动作，在动作下拉框中选择**使用数据桥接转发**选项，点击在数据桥接下拉框旁边的 **+** 按钮创建数据桥接，进入**创建数据桥接**弹出页面。

5. 在数据桥接类型中选择 TimescaleDB，输入数据桥接名称，要求是大小写英文字母和数字的组合。

6. 根据你的部署方式输入对应的 TimescaleDB 连接信息，如果使用的是 Docker，**服务器地址**填写 `127.0.0.1:5432`，**数据库**填写 `tsdb`，**用户名**为 `postgres`，**密码**为 `public`。

7. 配置 SQL 模板，使用如下 SQL 完成数据插入。

   ::: tip

   此处为[预处理 SQL](./data-bridges.md#sql-预处理)，字段不应当包含引号，SQL 末尾不要带分号 `;`。

   :::

   ```sql
    INSERT INTO
    sensor_data (time, location, temperature, humidity)
    VALUES
      (NOW(), ${location}, ${temp}, ${humidity})
   ```

8. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数。

9. 点击**添加**按钮完成数据桥接创建，此时会自动返回到**添加动作**页面，点击**添加**按钮将数据桥接添加到规则动作中。

10. 点击**创建**按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 TimescaleDB 进行存储。


### 测试数据桥接与规则

使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{"temp":24,"humidity":30,"location":"hangzhou"}'
```

分别查看数据桥接运行统计，命中、发送成功次数均 +1。

查看数据是否已经写入`sensor_data` 表中：

```bash
tsdb=# select * from sensor_data;
             time              | location | temperature | humidity 
-------------------------------+----------+-------------+----------
 2023-07-10 08:28:48.813988+00 | hangzhou |          24 |       30
 2023-07-10 08:28:57.737768+00 | hangzhou |          24 |       30
 2023-07-10 08:28:58.599537+00 | hangzhou |          24 |       30
(3 rows)
```
