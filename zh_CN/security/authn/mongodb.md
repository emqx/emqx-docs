# 使用 MongoDB 的密码认证

该认证器实现了密码验证算法，并使用 MongoDB 数据库作为凭证存储。

## 存储架构

MongoDB 密码认证器支持将凭据存储为 MongoDB 文档。用户提供集合名称和过滤器模板以选择相关文档。

该文档应包含具有 `password_hash`、`salt` 和 `is_superuser` 值的字段。 字段名称是可配置的。`password_hash` 的值是必需的，其他值是可选的。没有 `salt` 将被解释为空盐（`salt = ""`）；`is_superuser` 缺失将按照默认值 `false` 处理。

为用户名 `user123`、密码 `secret`、盐值 `salt` 和超级用户标识为 true 的用户添加文档的示例：

```js
> db.mqtt_user.insertOne(
  {
      "username": "user123",
      "s": "salt",
      "is": true,
      "ph": "bede90386d450cea8b77b822f8887065e4e5abf132c2f9dccfcc7fbd4cba5e35"
  }
);
{
  acknowledged: true,
  insertedId: ObjectId("6290aa4959fbb6cf748c0148")
}
```

对应的配置参数为：

```
password_hash_algorithm {
    name = sha256
    salt_position = prefix
}

collection = "mqtt_user"
filter { username = "${username}" }

password_hash_field = "ph"
salt_field = "s"
is_superuser_field = "is"
```

::: warning
当系统中有大量用户时，请确保选择器使用的集合已优化并使用有效的索引。 否则连接 MQTT 客户端会对数据库和 EMQX 本身产生过多的负载。
:::

## 配置

MongoDB 密码认证器由 `mechanism = password_based` 和 `backend = mongodb` 标识。

此认证器支持 MongoDB 的 3 种部署类型：

- Standalone MongoDB server:

  ```
  {
    mechanism = password_based
    backend = mongodb
    enable = true

    password_hash_algorithm {
      name = sha256
      salt_position = suffix
    }

    collection = "mqtt_user"
    filter { username = "${username}" }

    mongo_type = single
    server = "127.0.0.1:27017"

    database = "mqtt"
    username = "emqx"
    password = "secret"
  }
  ```
- MongoDB [ReplicaSet](https://www.mongodb.com/docs/manual/reference/replica-configuration/):

  ```
  {
    mechanism = password_based
    backend = mongodb
    enable = true
  
    password_hash_algorithm {
      name = sha256
      salt_position = suffix
    }
  
    collection = "mqtt_user"
    filter { username = "${username}" }
  
    mongo_type = rs
    servers = "10.123.12.10:27017,10.123.12.11:27017,10.123.12.12:27017"
    replica_set_name = "rs0"
  
    database = "mqtt"
    username = "emqx"
    password = "secret"
  }
  ```
- MongoDB [Sharded Cluster](https://www.mongodb.com/docs/manual/sharding/):

  ```
  {
    mechanism = password_based
    backend = mongodb
    enable = true
  
    password_hash_algorithm {
      name = sha256
      salt_position = suffix
    }
  
    collection = "mqtt_user"
    filter { username = "${username}" }
  
    mongo_type = sharded
    servers = "10.123.12.10:27017,10.123.12.11:27017,10.123.12.12:27017"
  
    database = "mqtt"
    username = "emqx"
    password = "secret"
  }
  ```

### 通用配置选项

#### `password_hash_algorithm`

标准 [密码散列选项](./authn.md#密码散列)。

#### `filter`

A map interpreted as MongoDB selector for credential lookup.
Supports [placeholders](./authn.md#认证占位符).

#### `database`

必选的字符串类型配置，用于指定 MongoDB 的数据库名称。

#### `username`

可选的字符串类型配置，用于指定 MongoDB 用户。

#### `password`

可选的字符串类型配置，用于指定 MongoDB 用户密码。

#### `pool_size`

可选的整型配置，用于定义从 EMQX 节点到 MongoDB 服务器的并发连接数。默认值为 8。

#### `ssl`

标准 [SSL 选项](../ssl.md) 。

#### `srv_record`

可选的布尔类型配置，默认值为 `false`。 如果设置为 `true`，EMQX 将尝试从 DNS 记录中获取 MongoDB 主机、Replica Set 名称、Auth Source 等信息。参见 [DNS 种子列表连接格式](https://www.mongodb.com/docs/manual/reference/connection-string/#dns-seed-list-connection-format)。

#### `topology`

可选的 map 类型配置，包含了一些细粒度 MongoDB 驱动程序设置：

- `pool_size` — 整数类型，内部连接池的初始大小。
- `max_overflow` — 整数类型，当内部池中的所有工作进程都忙时，能够溢出创建的工作进程的数量。
- `overflow_ttl` — 时长，溢出的工作进程在终止之前留在内部池中的毫秒数。
- `overflow_check_period` — 时长，工作进程的 `overflow_ttl` 的检查周期（以毫秒为单位）。
- `local_threshold_ms` — 以毫秒为单位的时长，只能选择 RTT 适合从较低 RTT 到较低 RTT + `local_threshold_ms` 的窗口中的辅助节点来处理用户的请求。
- `connect_timeout_ms` — 以毫秒为单位的时长，建立 TCP 连接的超时时间。
- `server_selection_timeout_ms` — 以毫秒为单位的时长，选择适当服务器的最长时间。
- `wait_queue_timeout_ms` — 以毫秒为单位的时长，等待工作进程在内部池中可用的最长时间。
- `heartbeat_frequency_ms` — 以毫秒为单位的时长，Topology 重新扫描之间的默认延迟。
- `min_heartbeat_frequency_ms` — 以毫秒为单位的时长，Topology 重新扫描之间的最小延迟。

### Standalone MongoDB 选项

#### `server`

必选的字符串类型配置，用于连接或被用作 seed 的 MongoDB 服务器地址。

#### `w_mode`

写入模式，`unsafe`（默认）或 `safe`。 安全模式在序列中的每次写入后都会发出 `getLastError` 请求。如果回复说它失败，那么序列的其余部分将被中止。不安全模式会在不确认的情况下发出每次写入，因此如果写入失败，您将不会知道它，并且将执行剩余的操作。这是不安全的，但速度更快，因为没有往返延迟。

### MongoDB ReplicaSet 选项

#### `servers`

必选的字符串类型配置，用逗号分隔，指定用于连接或被用作 seeds 的 MongoDB 服务器地址列表。

#### `w_mode`

写模式，与 [Standalone MongoDB](#standalone-mongodb-options) 相同。

#### `r_mode`

读取模式，`master`（默认）或 `slave_ok`。`master` 意味着序列中的每个查询都必须从主服务器读取新数据。 如果连接的服务器不是主服务器，则第一次读取将失败，其余操作将中止。`slave_ok` 表示允许每个查询从从服务器读取陈旧数据（来自主服务器的新数据也可以）。

#### `replica_set_name`

必选的字符串类型配置，用于指定 Replica Set 名称。但是当 `srv_record` 配置为 `true` 时，即从 DNS 记录中查询这些配置信息，可以不设置此字段。

### MongoDB Cluster 选项

#### `servers`

必选的字符串类型配置，用逗号分隔，指定用于连接或被用作 seeds 的 MongoDB 服务器地址列表。

#### `w_mode`

写模式，与 [Standalone MongoDB](#standalone-mongodb-options) 相同。
