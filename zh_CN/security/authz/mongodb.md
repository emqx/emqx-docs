# MongoDB

EMQX 支持从 MongoDB 中读取于定义的一系列规则来对客户端的发布和订阅等操作进行授权。

## 存储方式

MongoDB authorizer 可以将 ACL 规则存储到 MongoDB 中。
管理员可以指定一个 collection 和 filter 模版来对客户端进行查找和匹配。

每个 MongoDB 的记录应该包含 `permission`、`action` 和 `topics` 这些字段。
* `permission`： 可以是 `deny` 或 `allow`，用于指定授权的结果。
* `action`： 可以是 `publish`， `subscribe`， 或 `all`，用于指定该规则适用于哪些客户端请求。
* `topics`： 用于指定该规则适用的发布或订阅主题，或订阅的主题过滤器。比如是一个字符串数组，支持[占位符](./authz.md#主题占位符).

下面举一个使用的例子，让用户名为 `user123` 的客户端有权订阅主题 `data/user123/#`：

```js
> db.mqtt_acl.insertOne(
  {
      "username": "user123",
      "permission": "allow",
      "action": "publish",
      "topics": ["data/user123/#"]
  }
);
{
  acknowledged: true,
  insertedId: ObjectId("62b4a1a0e693ae0233bc3e98")
}
```

相应的链接配置参数如下：

```
collection = "mqtt_acl"
filter { username = "${username}" }

```

::: warning
当需要存储的客户端数量很大时，有必要为这些数据创建索引。否则授权信息的查询会导致 MongoDB 和 EMQX 的系统压力增加。
:::

## 配置

MongoDB Authorizer 必需有 `type = mongodb`。

有三种不同的连接模式：

* Standalone：
  ```
  {
    type = mongodb
    enable = true

    collection = "mqtt_user"
    filter { username = "${username}" }

    mongo_type = single
    server = "127.0.0.1:27017"

    database = "mqtt"
    username = "emqx"
    password = "secret"
  }
  ```

*  [ReplicaSet](https://www.mongodb.com/docs/manual/reference/replica-configuration/)：

```
{
  type = mongodb
  enable = true

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

*  [Sharded Cluster](https://www.mongodb.com/docs/manual/sharding/)：

```
{
  type = mongodb
  enable = true

  collection = "mqtt_user"
  filter { username = "${username}" }

  mongo_type = sharded
  servers = "10.123.12.10:27017,10.123.12.11:27017,10.123.12.12:27017"

  database = "mqtt"
  username = "emqx"
  password = "secret"
}
```

### 详细的配置参数

#### `collection`

必填字段，指定 MongoDB 中用于存储授权信息的 Collection。

#### `filter`

用于指定 MongoDB 查询时使用哪些字段进行过滤。
这些字段名称没有限制，值可以使用以下这些[占位符](./authz.md#authorizer-配置中的占位符):
* `${clientid}` — 客户端 ID
* `${username}` — 客户端登录时使用的用户名
* `${peerhost}` — 客户端的源 IP 地址

#### `database`

指定在哪个 MongoDB 数据库中查询授权数据。

#### `username`

指定访问 MongoDB 数据库时使用的用户名。

#### `password`

指定访问 MongodDB 数据库时使用的密码。

#### `pool_size`

指定 MongoDB 的连接池的大小。默认值是 8。

#### `ssl`

[SSL](../ssl.md)客户端参数。

#### `srv_record`

可选的布尔类型参数。默认是 `false` 。该配置设置成 `true` 时，EMQX 会从 DNS 记录中查找指定的 MongoDB 地址和端口。

详情请参考[DNS Seed List Connection Format](https://www.mongodb.com/docs/manual/reference/connection-string/#dns-seed-list-connection-format).

#### `topology`

指定 MongDB 连接参数的更多字段。

* `pool_size` — 连接池大小。
* `max_overflow` — 当连接池中所有的连接都忙时，可以额外创建的连接数上限。
* `overflow_ttl` — 额外创建的连接允许存活的时间。
* `overflow_check_period` — 时间间隔（毫秒），用于指定间隔多久做一次 `overflow_ttl` 的检查。
* `connect_timeout_ms` — 时间间隔（毫秒），用于指定连接初始化最长的等待时间。
* `server_selection_timeout_ms` — 时间间隔（毫秒），用于指定发现一个MongoDB服务器的时间。
<!--
TODO
* `local_threshold_ms` — ms duration, secondaries only which RTTs fit in the window from lower RTT to lower RTT + `local_threshold_ms` could be selected for handling user's requests.
* `wait_queue_timeout_ms` — ms duration, max time for waiting for a worker to be available in the internal pool.
* `heartbeat_frequency_ms` — ms duration, default delay between Topology rescans.
* `min_heartbeat_frequency_ms` — ms duration, the minimum delay between Topology rescans.
-->

### MongoDB 单机模式

#### `server`

MongoDB 服务器的 FQDN 或者 IP 地址和端口号


#### `w_mode`

该配置在授权中用不到。

### MongoDB ReplicaSet模式

#### `servers`

必选的字符串类型配置，用逗号分隔，指定用于连接或被用作 seeds 的 MongoDB 服务器地址列表。

#### `w_mode`

该配置在授权中用不到。

#### `r_mode`

指定读模式，默认为 `master`。
设置为 `master` 时，所有的读操作都会返回最新的数据。如果连接的服务器不是主节点，那么读取将会失败。
设置为 `slave_ok` 时，如果连接到的 MongoDB 是一个备节点，则可能会读到过期的数据。

#### `replica_set_name`

MongoDB 的 Replica set 名称，此参数为必填项。但在 `srv_record` 设置为 `true` 时，可能会被发现的信息覆写。

### MongoDB Cluster 模式

#### `servers`

必填字段，用于指定可用于连接的所有 MongoDB 服务器的 FQDN 和端口号，使用逗号分隔。

#### `w_mode`

该配置在授权中用不到。
