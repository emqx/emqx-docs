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

## 先决条件

<!-- 根据情况编写，包含必须的前置知识点、软件版本要求、需要预先创建/初始化的操作。 -->
- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

<!-- 列举功能或性能方面的亮点，如支持批处理、支持异步模式、双向数据桥接，链接到对应的功能介绍章节。 -->

## 特性

- [连接池](./data-bridges.md#连接池)
- [SQL 预处理](./data-bridges.md#SQL-预处理)

<!--  Configuration parameters TODO 链接到配置手册对应配置章节。 -->

## 快速开始
<!-- 从安装测试所需步骤，如果有不同的用法增加章节介绍。 -->

### 安装 Cassandra

我们使用 docker 启动一个简单的 Cassandra 服务：

```bash
docker run --name cassa --rm -p 9042:9042 cassandra:3.11.14
```

### 创建 Keyspace 和 Table

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

:::tip
在创建到 Cassandra 的数据桥接之前，必须创建这些 Keyspace 和 Table。
:::

### 创建到 Cassandra 的数据桥接

本节我们将创建一个 Cassandra 数据桥接来存储所有 `t/#` 主题上的消息。

1. 转到 Dashboard **数据集成** -> **数据桥接** 页面。
2. 点击页面右上角的**创建**。
3. 在数据桥接类型中选择 **Cassandra**，点击下一步。
4. 输入数据桥接名称，要求是大小写英文字母或数字组合。
5. 输入 Cassandra 连接信息，**Servers** 地址填写 `127.0.0.1:9042`，**Keyspace** 填写 `mqtt`，其他使用默认值即可。
6. 点击 **创建** 按钮完成数据桥接创建。

至此已经完成数据桥接创建，接下来将继续创建一条规则来指定需要写入的数据：

1. 转到 Dashboard **数据集成** -> **规则**页面。
2. 点击页面右上角的**创建**。
3. 输入规则 ID `my_rule`，在 SQL 编辑器中输入规则，此处选择将 `t/#` 主题的 MQTT 消息转发至 Cassandra，请确规则选择出来的字段（SELECT 部分）包含所有 SQL 模板中用到的变量，此处规则 SQL 如下：

  ```sql
  SELECT 
    *
  FROM
    "t/#"
  ```

4. 在 **动作** 下拉框中选择 **使用数据桥接转发** 选项，选择先前创建好的 Cassandra 数据桥接。
6. 点击最下方**创建**按钮完成规则创建。

至此已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果转发至 Cassandra。

### 测试

使用 MQTTX 向 `t/1` 主题发布消息：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Cassandra" }'
```

查看数据桥接运行统计，命中、发送成功次数应当 +1。

通过 Cassandra 命令查看消息是否已经写入 `mqtt_msg` 表中：

```bash
docker exec -it cassa cqlsh "-e SELECT * FROM mqtt.mqtt_msg;"
```
