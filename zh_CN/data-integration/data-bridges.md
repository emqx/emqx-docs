# 数据桥接简介

数据桥接是用来对接 EMQX 和外部数据系统的通道，比如 MySQL、MongoDB 等数据库， 或 Kafka，RabbitMQ 等消息中间件，或 HTTP 服务器等。

通过数据桥接，用户可以实时地将消息从 EMQX 发送到外部数据系统，或者从外部数据系统拉取数据并发送到 EMQX 的某个主题。

{% emqxce %}
::: tip
EMQX 开源版中仅支持 MQTT 桥接 和 Webhook，企业版支持的数据桥接请详见：[企业数据集成](https://www.emqx.com/zh/integrations)
:::
{% endemqxce %}

## 数据桥接指标

数据桥接提供了运行统计指标，包括：
<!-- TODO 由于调整过 Data Bridge 结构，先前的指标设计过时了重新设计指标后补充文档 -->

- 命中

- 发送成功
- 发送失败
- 已发送未确认
- 已丢弃
- 已缓存
- 已重试
- 当前速率
- 延迟回复

## 数据桥接特性

数据桥接借助以下特性以增强易用性、进一步提高数据集成的性能和可靠性，并非所有数据桥接都完全实现了这些特性，具体支持情况请参照各自的说明文档。

### 连接池

连接池是一组可重用的连接对象。通过连接池，用户无需在为每个请求重新创建连接，有助于降低资源消耗，提高连接效率和并发能力。

EMQX 会为每个需要创建数据桥的节点创建一个单独的连接池。例如，对一个包含 3 个 EMQX 节点的集群，如果将每个数据桥的连接池大小设置为 8，那么 EMQX 将创建 3 x 8 = 24 个连接。注意：请确保构建的连接池数量不要超过资源的连接限制。

### 异步请求模式

#### 异步模式配置

```bash
bridges.mysql.foo {
  server = "localhost"
  database = "emqx"
  enable = true
  ...
  resource_opts {
    query_mode = "async"
    ...
  }
}
```

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
    enable_batch = true
    batch_size = 100
    batch_time = "20ms"
    ...
  }
}
```

### 缓存队列


缓存队列为数据桥接提供了一定的容错性，建议启用该选项以提高数据安全性，可选配置如下：

- 是否启用缓存队列。
- 部分数据桥接可配置缓存存储介质，可选内存/磁盘/内存-磁盘混合。
- 每个资源连接（此处并非 MQTT 连接）缓存队列长度（按容量大小），超出长度按照 FIFO 的原则丢弃数据。

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
    enable_queue = true
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
