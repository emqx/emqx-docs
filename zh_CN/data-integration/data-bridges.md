# 数据集成简介

数据集成通过规则引擎和数据桥接，将物联网数据无缝传输到不同的数据系统。数据桥接是用来对接 EMQX 和外部数据系统的通道，比如 MySQL、MongoDB 等数据库，Kafka，RabbitMQ 等消息中间件或 HTTP 服务等。通过数据集成，用户可以实时地将消息从 EMQX 发送到外部数据系统。如果使用双向数据桥接，用户还可以从外部数据系统拉取数据并发送到 EMQX 的某个主题。

::: tip

目前，EMQX 仅支持与 Kafka 和 Pulsar 进行双向数据桥接。

:::

本页面提供了数据集成的总体介绍，包括工作原理、支持的外部数据系统、主要特性以及数据集成的运行。

## 工作原理

EMQX 数据集成是一个开箱即用的功能，在可视化界面上简单配置，无需编写代码即可将物联网数据传输到外部数据系统。作为一个 MQTT 消息平台，EMQX 通过 MQTT 协议接收来自物联网设备的数据。借助内置的规则引擎，接收到的数据会通过用户配置的规则进行处理。处理完成后，规则将通过配置的数据桥接，将处理后的数据发送到外部数据系统。您可以在 EMQX Dashboard 上轻松创建规则、并添加数据桥接。

### 内置规则引擎

强大的规则引擎是基于 MQTT 的数据处理和分发的核心组件。在物联网数据被传输到外部数据系统之前，数据将经过规则引擎，按照规则进行实时数据提取、过滤、丰富和格式转换。当规则添加了定义数据处理方式的动作时，EMQX 将输出处理结果到配置的数据桥接。

您可以在[规则引擎](./rules.md)章节中找到有关规则引擎工作原理的详细信息。

### 基于 SQL 的数据处理

来自各种物联网设备和系统的数据源具有各种数据类型和格式。为了获取所需的数据，规则中使用基于 SQL 的语法进行数据提取、过滤、丰富和转换。有关基于 SQL 的规则的详细信息，请参阅 [SQL 语法与示列](./rule-sql-syntax.md)。

## 支持的集成

{% emqxce %}
EMQX 开源版中支持以下两种数据集成：

- [MQTT](./data-bridge-mqtt.md)
- [Webhook](./webhook.md)/[HTTPServer](./data-bridge-webhook.md)

下列数据系统仅在 EMQX 企业版中支持：

**消息队列**

- [Amazon Kinesis](./data-bridge-kinesis.md)
- [Apache Kafka/Confluent](./data-bridge-kafka.md)
- [HStreamDB](./data-bridge-hstreamdb.md)
- [Pulsar](./data-bridge-pulsar.md)
- [RabbitMQ](./data-bridge-rabbitmq.md)
- [RocketMQ](./data-bridge-rocketmq.md)

**SQL**

- [Cassandra](./data-bridge-cassa.md)
- [Microsoft SQL Server](./data-bridge-sqlserver.md)
- [MySQL](./data-bridge-mysql.md)
- [Oracle](./data-bridge-oracle.md)
- [PostgreSQL](./data-bridge-pgsql.md)

**NoSQL**

- [ClickHouse](./data-bridge-clickhouse.md)
- [DynamoDB](./data-bridge-dynamo.md)
- [Greptime](./data-bridge-greptimedb.md)
- [MongoDB](./data-bridge-mongodb.md)
- [Redis](./data-bridge-redis.md)
- [TDengine](./data-bridge-tdengine.md)

**时序数据库**

- [Apache IoTDB](./data-bridge-iotdb.md)
- [InfluxDB](./data-bridge-influxdb.md)
- [OpenTSDB](./data-bridge-opents.md)
- [TimescalesDB](./data-bridge-timescale.md)

**企业系统**

- [Azure EventHub](./data-bridge-azure-event-hub.md)
- [Google Cloud](./data-bridge-gcp-pubsub.md)

{% endemqxce %}

{% emqxee %}

EMQX 支持与以下数据系统的集成：

**外部服务**

- [MQTT](./data-bridge-mqtt.md)
- [Webhook](./webhook.md)/[HTTPServer](./data-bridge-webhook.md)

**消息队列**

- [Amazon Kinesis](./data-bridge-kinesis.md)
- [Apache Kafka/Confluent](./data-bridge-kafka.md)
- [HStreamDB](./data-bridge-hstreamdb.md)
- [Pulsar](./data-bridge-pulsar.md)
- [RabbitMQ](./data-bridge-rabbitmq.md)
- [RocketMQ](./data-bridge-rocketmq.md)

**SQL**

- [Cassandra](./data-bridge-cassa.md)
- [Microsoft SQL Server](./data-bridge-sqlserver.md)
- [MySQL](./data-bridge-mysql.md)
- [Oracle](./data-bridge-oracle.md)
- [PostgreSQL](./data-bridge-pgsql.md)

**NoSQL**

- [ClickHouse](./data-bridge-clickhouse.md)
- [DynamoDB](./data-bridge-dynamo.md)
- [Greptime](./data-bridge-greptimedb.md)
- [MongoDB](./data-bridge-mongodb.md)
- [Redis](./data-bridge-redis.md)
- [TDengine](./data-bridge-tdengine.md)

**时序数据库**

- [Apache IoTDB](./data-bridge-iotdb.md)
- [InfluxDB](./data-bridge-influxdb.md)
- [OpenTSDB](./data-bridge-opents.md)
- [TimescalesDB](./data-bridge-timescale.md)

**企业系统**

- [Azure EventHub](./data-bridge-azure-event-hub.md)
- [Google Cloud](./data-bridge-gcp-pubsub.md)

{% endemqxee %}

## 数据桥接特性

数据桥接借助以下特性以增强易用性、进一步提高数据集成的性能和可靠性，并非所有数据桥接都完全实现了这些特性，具体支持情况请参照各自的说明文档。

### 连接池

连接池是一组可重用的连接对象。通过连接池，用户无需在为每个请求重新创建连接，有助于降低资源消耗，提高连接效率和并发能力。

EMQX 会为每个需要创建数据桥的节点创建一个单独的连接池。例如，对一个包含 3 个 EMQX 节点的集群，如果将每个数据桥的连接池大小设置为 8，那么 EMQX 将创建 3 x 8 = 24 个连接。注意：请确保构建的连接池数量不要超过资源的连接限制。

### 异步请求模式

异步请求模式可以防止消息的发布服务由于 I/O 压力而受影响。但在开启异步请求模式后，由于历史消息会在数据桥接处排队 ，客户端新发送消息的时间序列将受到影响。

为了提高数据处理效率，EMQX 默认开启异步请求模式。如果您对消息的时间序列有严格要求，请禁用异步请求模式。

**示例代码**

```bash
bridges.mysql.foo {
  server = "localhost"
  database = "emqx"
  enable = true
  ...
  resource_opts {
    query_mode = "sync"
    ...
  }
}
```

::: tip

为保证消息的时序性，请同时将 `max_inflight` 设置为 1。

:::

### 批量模式

批量模式可以将多条数据同时写入外部数据集成中，启用批量后 EMQX 将暂存每次请求的数据（单条），达到一定时间或累积一定数据条数（两者均可自行配置）后将暂存的整批数据写入到目标数据系统。

**优点：**

- 提高写入效率：相对于单条消息的写入方式，批量模式下，数据库系统在正式处理消息前，一般会先对其进行缓存或预处理等优化操作，提高写入效率。


- 减少网络延迟：批量写入可以减少网络传输次数，进而减少网络延迟。

**问题：**

- 数据写入时延较长：数据在达到设置的时间或条数之后才会被写入，时延较长。注意：您可以通过下方参数对设置时间或条数进行调整。
- 有一定延迟：在达到设置的时间或累积数据条数之前数据不会立即写入，可通过参数进行调整。


#### 批量模式配置

```bash
bridges.mysql.foo {
  server = "localhost"
  database = "emqx"
  enable = true
  ...
  resource_opts {
    batch_size = 100
    batch_time = "20ms"
    ...
  }
}
```

### 缓存队列


缓存队列为数据桥接提供了一定的容错性，建议启用该选项以提高数据安全性。

每个资源连接（此处并非 MQTT 连接）缓存队列长度（按容量大小），超出长度按照 FIFO 的原则丢弃数据。

#### 缓存文件位置

对于 Kafka Bridge，磁盘缓存文件位于 `data/kafka` 下，其他数据桥接磁盘缓存文件位于 `data/resource_worker` 下。

实际使用中可以根据情况将 `data` 目录挂载至高性能磁盘以提高吞吐能力。

#### 缓存队列配置

```bash
bridges.mysql.foo {
  server = "localhost"
  database = "emqx"
  enable = true
  ...
  resource_opts {
    max_queue_bytes = "100MB"
    query_mode = "async"
    ...
  }
}
```

### SQL 预处理

在诸如 MySQL、PostgreSQL 等 SQL 数据库中，SQL 模板会进行预处理执行，无需显式的指定字段变量。

直接执行 SQL 时，必须通过单引号显式设置 topic 与 payload 为字符类型，qos 为 int 类型：

```sql
INSERT INTO msg(topic, qos, payload) VALUES('${topic}', ${qos}, '${payload}');
```

但在支持 SQL 预处理的数据桥接中，SQL 模板**必须**使用不带引号的预处理语句：

```sql
INSERT INTO msg(topic, qos, payload) VALUES(${topic}, ${qos}, ${payload});
```

除了自动推导字段类型外，SQL 预处理技术还能避免 SQL 注入以提高安全性。

## 数据集成运行

您可以在 Dashboard 上查看数据桥接的运行状态和数据集成统计信息，以了解桥接和集成是否正常运行。

### 数据桥接状态

数据桥接可以具有以下状态：

- `connecting`：在进行任何健康检查之前的初始状态，桥接仍在尝试连接到外部数据系统。
- `connected`：桥接成功连接并正常运行。如果健康检查失败，桥接可能会转换为 `connecting` 或 `disconnected` 状态，具体取决于故障的严重程度。
- `disconnected`：桥接未通过健康检查，处于不健康状态。根据其配置，它可能会定期尝试自动重新连接。
- `stopped`：桥接已被手动禁用。
- `inconsistent`：集群节点之间对桥接状态存在分歧。

### 数据集成指标

EMQX 提供以下数据集成的运行统计指标：

- 命中（counter）
- 发送成功（counter）
- 发送失败（counter）
- 已丢弃（counter）
- 延迟回复（counter）
- 进行中（gauge）
- 排队中（gauge）

![data-bridge-metrics](./assets/data-bridge-metrics.png)

### 命中

`命中` 统计了无论桥接的状态如何，都被路由到桥接的请求/消息的数量。每条消息最终由其他指标计算，因此 `命中` 的计算公式为：`命中 = 成功发送 + 发送失败 + 进行中 + 排队中 + 延迟回复 + 已丢弃`。

### 发送成功

`发送成功` 统计了成功被外部数据系统接收的消息数量。`重新尝试发送成功` 是 `发送成功` 的子计数，用于跟踪至少重试一次的消息数量。因此，`重新尝试发送成功 ≤ 发送成功`。

### 发送失败

`发送失败` 统计了未能被外部数据系统接收的消息数量。`重新尝试发送失败` 是 `发送失败` 的子计数，用于跟踪至少重试一次的消息数量。因此，`重新尝试发送失败 ≤ 发送失败`。

### 已丢弃

`已丢弃` 统计了未经任何发送尝试而被丢弃的消息数量。它包含了几个更具体的子类别，每个子类别都表示丢弃的不同原因。`已丢弃` 的计算公式为：`已丢弃 = 过期 + 队列已满 + 资源已停止 + 未找到资源`。

- `过期`：在排队等待发送之前，消息的生存时间（TTL）已经到期。
- `队列已满`：达到了最大队列大小，为防止内存溢出而丢弃消息。
- `资源已停止`：在桥接已停止的情况下，仍然尝试发送消息。
- `未找到资源`：在桥接不再存在时尝试发送消息。这种情况非常罕见，通常是由于在移除桥接时出现竞争条件。

### 延迟回复

当尝试发送消息时，在消息的生存时间（TTL）过期后仍然收到底层驱动程序的响应时，`延迟回复` 会递增。

::: tip

请注意，`延迟回复` 不表示消息是否成功发送或发送失败，它是一种未知状态。它既可能成功插入外部数据系统，也可能插入失败，甚至在尝试建立与数据系统的连接时连接超时。 

:::

### 进行中

`进行中` 是度量当前在缓冲层中正在等待来自外部数据系统的响应的消息数。

### 排队中

`排队中` 是度量已经被缓冲层接收但尚未发送到外部数据系统的消息数。

