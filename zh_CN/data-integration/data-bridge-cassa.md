# Cassandra

<!-- 提供一段简介，描述支数据桥接的基本工作方式、关键特性和价值，如果有局限性也应当在此处说明（如必须说明的版本限制、当前未解决的问题）。 -->

Cassandra 是一种流行的开源分布式 NoSQL 数据库管理系统。
EMQX 与 Apache Cassandra 的集成提供了将消息和事件存储到 Cassandra 数据库的能力。

当前实现中：
- 仅支持 Cassandra v3.x，不兼容 v4.x。
- 仅支持同步方式存储数据。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

## 前置条件

<!-- 根据情况编写，包含必须的前置知识点、软件版本要求、需要预先创建/初始化的操作。 -->
- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

<!-- 列举功能或性能方面的亮点，如支持批处理、支持异步模式、双向数据桥接，链接到对应的功能介绍章节。 -->

## 特性

- [连接池](./data-bridges.md#连接池)
- [SQL 预处理](./data-bridges.md#sql-预处理)

<!--  Configuration parameters TODO 链接到配置手册对应配置章节。 -->

## 快速开始教程
<!-- 从安装测试所需步骤，如果有不同的用法增加章节介绍。 -->

本节介绍如何配置 Cassandra 数据桥接，包括如何设置 Cassandra 服务器、创建数据桥接和规则以将数据转发到 Cassandra、以及如何测试数据桥接和规则。

本教程假定您在本地机器上同时运行 EMQX 和 Cassandra。如果您在远程运行 Cassandra 和 EMQX，请相应地调整设置。

### 安装 Cassandra

使用 docker 启动一个简单的 Cassandra 服务：

```bash
docker run --name cassa --rm -p 9042:9042 cassandra:3.11.14
```

### 创建 Keyspace 和 Table

在创建到 Cassandra 的数据桥接之前，您必须先创建 Keyspace 和 Table。

使用 Docker 命令行创建名为 `mqtt` 的 Keyspace：

```bash
docker exec -it cassa cqlsh "-e CREATE KEYSPACE mqtt WITH REPLICATION = {'class': 'SimpleStrategy', 'replication_factor': 1}"
```

使用 Docker 命令行创建名为 `mqtt_msg` 的 Table：
```bash
docker exec -it cassa cqlsh "-e \
    CREATE TABLE mqtt.mqtt_msg( \
        msgid text, \
        topic text, \
        qos int,    \
        payload text, \
        arrived timestamp, \
        PRIMARY KEY(msgid, topic));"
```

### 创建 Cassandra 数据桥接

本节我们将创建一个 Cassandra 数据桥接来存储所有 `t/#` 主题上的消息。

1. 转到 Dashboard **数据集成** -> **数据桥接** 页面。

2. 点击页面右上角的**创建**。

3. 在数据桥接类型中选择 **Cassandra**，点击**下一步**。

4. 输入数据桥接名称，要求是大小写英文字母和数字组合。

5. 输入 Cassandra 连接信息，**Servers** 地址填写 `127.0.0.1:9042`，**Keyspace** 填写 `mqtt`，其他使用默认值即可。

6. 配置 CQL 模版，将字段 `topic`, `id`, `clientid`, `qos`, `palyload`, `timestamp`, 和 `flags.retain` 存储到 Cassandra 数据库中。该模板将通过 Cassandra 查询语言执行，对应模板如下：

   ```sql
   insert into mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic},  ${qos}, ${payload}, ${timestamp})
   ```

7. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考[数据桥接简介](./data-bridges.md)。

8. 点击**创建**按钮完成数据桥接创建。

   在弹出的**创建成功**对话框中您可以点击**创建规则**，继续创建规则以指定需要写入 Cassandra 的数据。您也可以按照[创建 Cassandra 数据桥接规则](#创建 Cassandra 数据桥接规则)章节的步骤来创建规则。

### 创建 Cassandra 数据桥接规则

至此已经完成数据桥接创建，接下来将继续创建一条规则来指定需要写入的数据。

1. 转到 Dashboard **数据集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 输入规则 ID `my_rule`，在 **SQL 编辑器**中输入规则，例如选择将 `t/#` 主题的 MQTT 消息转发至 Cassandra，规则 SQL 如下：

   注意：如果您希望制定自己的 SQL 语法，需要确保规则选出的字段（SELECT 部分）包含所有 SQL 模板中用到的变量。

  ```sql
  SELECT 
    *
  FROM
    "t/#"
  ```

4. 点击**添加动作**，在下拉框中选择**使用数据桥接转发**选项，选择先前创建好的 Cassandra 数据桥接。
6. 点击最下方**创建**按钮完成规则创建。

至此已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果转发至 Cassandra。

### 测试桥接和规则

使用 MQTTX 向 `t/1` 主题发布消息：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Cassandra" }'
```

查看数据桥接运行统计，命中、发送成功次数应当 +1。

通过 Cassandra 命令查看消息是否已经写入 `mqtt_msg` 表中：

```bash
docker exec -it cassa cqlsh "-e SELECT * FROM mqtt.mqtt_msg;"
```
