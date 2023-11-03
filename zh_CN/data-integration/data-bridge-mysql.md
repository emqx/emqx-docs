# MySQL

[MySQL](https://www.mysql.com/) 是一个广泛使用关系数据库，具备高度的可靠性和稳定性，能够快速安装和配置使用。MySQL 数据桥接能够将 MQTT 消息高效地存储至 MySQL 数据库中，同时也支持通过事件触发实时更新或删除 MySQL 中的数据。借助 MySQl 桥接，用户能够轻松实现消息存储、设备在线状态更新以及设备行为记录等功能，实现灵活的物联网数据存储与设备管理功能。

本页详细介绍了 EMQX 与 MySQL 的数据集成并提供了实用的规则和数据桥接创建指导。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

## 工作原理

MySQL 数据桥接是 EMQX 中开箱即用的功能，通过简单的配置即可实现复杂的业务开发。在一个典型的物联网应用中，EMQX 作为物联网平台，负责接入设备，进行消息传输，MySQL 作为数据存储平台，负责设备状态与元数据的存储，以及消息数据数据存储和数据分析等功能。

<!-- TODO 下图展示了物联网应用中 EMQX 和 MySQL 数据集成的典型架构。 -->

EMQX 通过规则引擎与数据桥接将设备事件和数据转发至 MySQL，应用读取 MySQL 中数据即可感知设备状态，获取设备上下线记录，以及分析设备数据。其工作流程如下：

1. **物联网设备连接到 EMQX**：设备连接成功后将触发上线事件，事件包含设备 ID、来源 IP 地址以及其他属性等信息。
2. **物联网设备发布消息**：设备通过特定的主题发布遥测和状态数据，消息将触发规则引擎。
3. **规则引擎处理消息**：通过内置的规则引擎，可以根据主题匹配处理特定来源的消息和事件。规则引擎会匹配对应的规则，并对消息和事件进行处理，例如转换数据格式、过滤掉特定信息或使用上下文信息丰富消息。
4. **写入到 MySQL**：规则触发将消息写入到 MySQL 的操作。借助 SQL 模板，用户可以从规则处理结果中提取数据构造 SQL 发送给 MySQL 执行，实现将消息特定字段写入或更新到数据库对应表和列中。

事件和消息数据写入到 MySQL 后，您可以连接到 MySQL 读取数据，进行灵活的应用开发，例如：

1. 连接到可视化工具，例如 Grafana，根据数据生成图表，展示数据变化。
2. 连接到设备管理系统，查看设备列表与状态，并检测设备异常行为，及时排除潜在的问题。

## 桥接准备

本节介绍了在 EMQX 中创建 MySQL 数据桥接之前需要做的准备工作，包括安装 MySQL 和创建数据表。

### 前置准备

- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

### 安装 MySQL

通过 Docker 安装并启动 MySQL：

```bash
# 启动一个 MySQL 容器并设置密码为 public
docker run --name mysql -p 3306:3306 -e MYSQL_ROOT_PASSWORD=public -d mysql

# 进入容器
docker exec -it mysql bash

# 在容器中连接到 MySQL 服务器，需要输入预设的密码
mysql -u root -p

# 创建并选择数据库
CREATE DATABASE emqx_data CHARACTER SET utf8mb4;
use emqx_data;
```

我们需要在 MySQL 中创建两张表：

数据表 `emqx_messages` 存储每条消息的发布者客户端 ID、主题、Payload 以及发布时间：

  ```sql
CREATE TABLE emqx_messages (
  id INT AUTO_INCREMENT PRIMARY KEY,
  clientid VARCHAR(255),
  topic VARCHAR(255),
  payload BLOB,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
  ```

数据表 `emqx_client_events` 存储上下线的客户端 ID、事件类型以及事件发生时间：

```sql
CREATE TABLE emqx_client_events (
  id INT AUTO_INCREMENT PRIMARY KEY,
  clientid VARCHAR(255),
  event VARCHAR(255),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

## 创建 MySQL 数据桥接

需要创建两个 MySQL 数据桥接分别完成消息存储与事件记录：

### 消息存储

本节我们将创建第一个 MySQL 数据桥接来实现对客户端发布消息的存储。

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。
2. 点击页面右上角的创建。
3. 在数据桥接类型中选择 MySQL，点击下一步。
4. 输入数据桥接名称，要求是大小写英文字母或数字组合。
5. 输入 MySQL 连接信息，主机列表填写 **127.0.0.1:3306**，数据库填写 `emqx_data`，用户名为 `root`，密码为 `public`。
6. 配置 SQL 模板，使用如下 SQL 完成数据插入，此处为[预处理 SQL](./data-bridges.md#sql-预处理)，字段不应当包含引号，SQL 末尾不要带分号 `;`:

  ```sql
  INSERT INTO emqx_messages(clientid, topic, payload, created_at) VALUES(
    ${clientid},
    ${topic},
    ${payload},
    FROM_UNIXTIME(${timestamp}/1000)
  )
  ```

  请在 MySQL 中使用以下 SQL 创建存储消息的高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考[配置参数](#配置参数)。

8. 点击**创建**按钮完成数据桥接创建。

至此您已经完成数据桥接创建，接下来将继续创建一条规则来指定需要写入的数据。

#### 创建数据转发规则

1. 转到 Dashboard **数据集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 输入规则 ID `my_rule`，在 SQL 编辑器中输入规则，此处选择将 `t/#` 主题的 MQTT 消息存储至 MySQL，请确规则选择出来的字段（SELECT 部分）包含所有 SQL 模板中用到的变量，此处规则 SQL 如下：

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

4. 添加动作，在动作下拉框中选择 **使用数据桥接转发** 选项，选择先前创建好的 MySQL 数据桥接。
5. 点击最下方**创建**按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 MySQL 存储。

### 上下线记录

在上面一个章节，我们实现了对指定主题消息的存储，本节我们将演示如何通过 MySQL 实现对设备上下线的记录。

注意：除 SQL 模板与规则外，其他操作步骤与[消息存储](#消息存储)章节完全相同。

数据桥接的 SQL 模板如下，请注意字段不应当包含引号，SQL 末尾不要带分号 `;`:

```sql
INSERT INTO emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  FROM_UNIXTIME(${timestamp}/1000)
)
```

**规则 SQL**

规则 SQL 如下：

```sql
SELECT
  *
FROM 
  "$events/client_connected", "$events/client_disconnected"
```

### 测试数据桥接与规则

使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello MySQL" }'
```

分别查看两个数据桥接运行统计，命中、发送成功次数均 +1。

查看数据是否已经写入表中，`emqx_messages` 表：

```bash
mysql> select * from emqx_messages;
+----+----------+-------+--------------------------+---------------------+
| id | clientid | topic | payload                  | created_at          |
+----+----------+-------+--------------------------+---------------------+
|  1 | emqx_c   | t/1   | { "msg": "hello MySQL" } | 2022-12-09 08:44:07 |
+----+----------+-------+--------------------------+---------------------+
1 row in set (0.01 sec)
```

`emqx_client_events` 表：

```bash
mysql> select * from emqx_client_events;
+----+----------+---------------------+---------------------+
| id | clientid | event               | created_at          |
+----+----------+---------------------+---------------------+
|  1 | emqx_c   | client.connected    | 2022-12-09 08:44:07 |
|  2 | emqx_c   | client.disconnected | 2022-12-09 08:44:07 |
+----+----------+---------------------+---------------------+
2 rows in set (0.00 sec)
```

## 更多内容

您可以通过以下链接查看更多关于 MySQL 集成的内容：

- [MQTT Performance Benchmark Testing: EMQX-MySQL Integration](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-mysql-integration)

