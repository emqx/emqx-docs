# 数据桥接简介

数据桥接是用来对接 EMQX 和外部数据系统的通道。外部数据系统可以是 MySQL、MongoDB 等数据库，
也可以是 Kafka，RabbitMQ 等消息中间件，或者是 HTTP 服务器等。

通过数据桥接，用户可以实时地将消息从 EMQX 发送到外部数据系统，或者从外部数据系统拉取数据并发送到 EMQX 的某个主题。

{% emqxce %}
::: tip
EMQX 开源版中仅支持 MQTT 桥接 和 Webhook，企业版支持的数据桥接请详见：[企业数据集成](https://www.emqx.com/zh/integrations)
:::
{% endemqxce %}

## 数据桥接状态与相关指标

数据桥接提供了运行统计指标，包括：
<!-- TODO 由于调整过 Data Bridge 结构，先前的指标设计过时了重新设计指标后补充文档 -->
<!-- - 命中：
- 发送成功：
- 发送失败
- 已发送未确认
- 已丢弃
- 已缓存
- 已重试
- 当前速率 -->

## 功能特性

数据桥接借助以下特性以增强易用性、进一步提高数据集成的性能和可靠性，并非所有数据桥接都完全实现了这些特性，具体支持情况请参照各自的说明文档。

### 连接池

连接池是指一组可重复使用的连接对象，通过提高连接效率、避免每次请求重新创建连接，连接池可以减少资源消耗能够支持大量并发请求。

EMQX 单个数据桥接在每个节点独立使用一个连接池，举个例子，当您使用 3 个 EMQX 节点集群并设置某个数据集成的连接池大小为 8，整个集群将建立 3x8 = 24 个连接，请留意您所用资源对连接数的限制。

### 异步请求模式

数据桥接数据操作处理模式，启用异步请求模式能够避免 I/O 操作阻塞消息发布流程，相应的消息时效性也会发生变化：客户端可能已经发出多条消息，但数据桥接还在排队处理先前的消息。

数据桥接默认启用了异步请求模式以提高数据处理效率，如果您的应用对消息时序有严格要求，请关闭此选项。

#### 异步模式配置

```bash
bridges.mysql.foo {
  server = "localhost"
  database = "emqx"
  enable = true
  ...
  resource_opts {
    # sync | async
    query_mode = "async"
    ...
  }
}
```
  
### 批量模式

批量模式可以将多条数据同时写入外部数据集成中，启用批量后 EMQX 将暂存每次请求的数据（单条），达到一定时间或累积一定数据条数（两者均可自行配置）后将暂存的整批数据写入到目标数据系统。

优点：

- 提高写入效率：通常数据库系统会对批量写入进行优化，比如利用缓存、预编译语句等技术来提高写入效率。因此，批量写入比单条写入更快。
- 减少网络延迟：批量写入可以减少网络传输次数，进而减少网络延迟。

缺点：

- 数据错误可能难以排查：批量写入过程中出现了单条数据错误，该条或整批数据将被丢弃难以排查错误问题。
- 有一定延迟：在达到设置的时间或累积数据条数之前数据不会立即写入，可通过参数进行调整。

数据桥接默认启用了批量模式，可根据情况关闭此特性。

#### 批量模式配置

```bash
bridges.mysql.foo {
  server = "localhost"
  database = "emqx"
  enable = true
  ...
  resource_opts {
    enable_batch = true
    batch_size = 100
    batch_time = "20ms"
    ...
  }
}
```

### 缓冲队列

<!-- TODO 发版前再次确认技术实现有无变动 -->

缓冲队列为数据桥接提供了一定的容错性，建议启用该选项以提高数据安全性，可选配置如下：

- 是否启用缓冲队列。
- 部分数据桥接可配置缓存存储介质，可选内存/磁盘/内存-磁盘混合。
- 每个资源连接（此处并非 MQTT 连接）缓冲队列长度（按容量大小），超出长度按照 FIFO 的原则丢弃数据。

#### 缓存存储位置

对于 Kafka Bridge，磁盘缓存文件位于 `data/kafka` 下，其他数据桥接磁盘缓存文件位于 `data/resource_worker` 下。

实际使用中可以根据情况将 `data` 目录挂载至高性能磁盘以提高吞吐能力。

#### 缓冲队列配置

```bash
bridges.mysql.foo {
  server = "localhost"
  database = "emqx"
  enable = true
  ...
  resource_opts {
    enable_queue = true
    max_queue_bytes = "100MB"
    query_mode = "async"
    ...
  }
}
```

### SQL 预处理

[SQL 预处理](https://dev.mysql.com/doc/refman/8.0/en/sql-prepared-statements.html)（Prepared Statements）是一种使用预编译语句执行 SQL 的方法。除了方便维护外，预处理语句还能避免 SQL 注入以提高安全性。

如果某个数据桥接支持 SQL 预处理，则在其 SQL 模板中无需显式的指定字段类型，反之则必须在 SQL 模板中明确数据类型。

比如要将以下数据插入数据库：

```json
{
  "topic": "t/1",
  "qos": 0,
  "payload": "Hello EMQX"
}
```

- topic：消息主题，字符。
- payload：消息内容，字符。
- qos：消息 QoS，整数。

在不支持 SQL 预处理的数据桥接中，必须通过单引号显式设置字段类型，如下所示：

```sql
INSERT INTO msg(topic, qos, payload) VALUES('${topic}', ${qos}, '${payload}');
```

在支持 SQL 预处理的数据桥接中，**必须**使用不带引号的预处理语句：

```sql
INSERT INTO msg(topic, qos, payload) VALUES(${topic}, ${qos}, ${payload});
```
