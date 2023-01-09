# Introduction to Data Bridges

Data bridge is a channel used to connect EMQX and external data systems, for example, databases like MySQL, MongoDB, message brokers like Apache Kafka and RabbitMQ, or even an HTTP server.

Through data bridges, users can send messages from EMQX to the external data system in real-time, or pull data from the external data system and send it to a topic in EMQX.

{% emqxce %}
::: tip
Only data bridges to MQTT and Webhook are supported in the EMQX open-source version. For the data systems supported in the EMQX enterprise version, you may refer to the [Data integration page on EMQX website](https://www.emqx.com/en/integrations). 
:::
{% endemqxce %}

<!-- TODO sync zh -->

## Data bridge execution statistics

EMQX will provide the running statistics of data bridges in the following categories: 
<!-- TODO 由于调整过 Data Bridge 结构，先前的指标设计过时了重新设计指标后补充文档 -->
<!-- - Matched

- Sent Successfully
- Sent Failed
- Sent Inflight
- Dropped
- Queuing
- Retried
- Rate -->

## Features supported

You can further improve the performance and reliability of data integration with the following easy-to-use data bridge features. Note: Depending on the data system you are connecting to, the features supported may differ. You may refer to the document about different data systems for feature support.

### Connection pool

Connection pool is a set of reusable connection objects. With connection pooling, users no longer need to re-create connections for each request, which can help to significantly reduce resource consumption, improve connection efficiency, and achieves better support for high concurrent requests.

EMQX will create a separate connection pool for each node with data bridge to be created. For example, let's say you use a cluster with 3 EMQX nodes and set the connection pool size for each data bridge to 8, then EMQX will create 3 x 8 = 24 connection pools. Note: Please ensure the number of connection pools to build should not exceed the connection limit of your resources.

### Async mode

Async mode is the data processing mode of the data bridge. By enabling the Async mode, we can prevent the message publishing services from being blocked by the I/O pressure. Note: The time series of the message publishing might be affected, as the data bridge may be still processing the queued messages while the client has already sent several new messages.

To improve  the data processing efficiency, EMQX has enabled the Async mode by default. You can use the following commands to disable the Async mode if your application has strict requirements on the time series. 

#### Configuration

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

### Batch mode

In batch mode, multiple pieces of data will be simultaneously written into the external data integrations. After enabling the Batching mode, EMQX will temporarily store the data of each request and put the data in batches into the target data system after reaching a specified batch time or size. You can configure the batch time and batch size as needed. 

**Advantages**

- Improve writing efficiency: Compared with the single-message writing mode, the database system will usually use techniques like caching or preprocessing to optimize the batch files before further operations, so the writing efficiency can be improved. 
- Reduce network latency: The batch mode can reduce the times of net transmissions, thereby reducing network latency.

**Disadvantages**

- Hard to troubleshoot data errors: If data error occurs during batch writing, the data with error or even the entire batch of data will be discarded, so it is difficult to do troubleshooting.
- Long data writing delay: Data will be written into the target data systems only after reaching the specified batch time or batch size, so the delay is prolonged. You can configure the batch time or size to control the delay. 

The Batch mode is enabled by default, you can disable it as needed. 

#### Configuration

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

### Buffer queue

When external resources are unavailable, for example, due to network fluctuations or service downtime, the buffer queue feature can help to save the message generated during this period as memory or disk cache and then resume the messaging after the service is restored.

It is recommended to enable this feature to improve the fault tolerance capability of the data bridge. The configuration items include: 

- Whether to enable Buffer Queue;
- For data bridges connecting certain data systems, you can set the cache medium as memory, disk, or memory-disk.
- For each resource connection (not MQTT connection), you can specify the cache queue size based on the storage size. If the cached size exceeds the limit, data will be discarded following the First In First Out (FIFO) rule. 

#### Configuration

For data bridges connecting to Kafka, the disk cache file is saved under `data/kafka`, for other data systems, the disk cache file is saved under `data/resource_worker`.

In actual use, it is recommended to mount the `data` folder in a high-performance disk to improve the throughput capacity. 

#### Configuration

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

### Prepared statement

[Prepared statement](https://dev.mysql.com/doc/refman/8.0/en/sql-prepared-statements.html) provides a way to run SQL with prepared statements. It simplifies the operation and maintenance and also helps to avoid SQL injection and improve security. 

For data bridges supporting prepared statements, you need not explicitly specify the field variables; otherwise, you will need to explicitly specify the field variables. 

For example, you will insert the following data into the database:

```json
{
  "topic": "t/1",
  "qos": 0,
  "payload": "Hello EMQX"
}
```

- topic: message topic, string
- payload: message body, string
- qos: message QoS, integer

For data bridges not supporting prepared statements, the fields should be enclosed in quotation marks, as shown below: 

```sql
INSERT INTO msg(topic, qos, payload) VALUES('${topic}', ${qos}, '${payload}');
```

But for data bridges supporting prepared statements, the fields in the SQL template should **NOT** be enclosed in quotation marks, as shown below:

```sql
INSERT INTO msg(topic, qos, payload) VALUES(${topic}, ${qos}, ${payload});
```

