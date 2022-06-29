# MongoDB

This authorizer implements ACL checks through matching pub/sub requests against lists of rules stored in the
MmongoDB database.

## Storage Schema

MongoDB authenticator supports storing ACL rules as MongoDB documents. A user provides the collection name and a
filter template for selecting the relevant documents.

The documents should contain `permission`, `action`, and `topics` fields.
* `permission` value specifies the applied action if the rule matches. Should be one of `deny` or `allow`.
* `action` value specifies the request for which the rule is relevant. Should be one of `publish`, `subscribe`, or `all`.
* `topics` value specifies the topic filters for topics relevant to the rule. Should be a list of strings that support wildcards and [topic placeholders](./authz.md#topic-placeholders).

Example of adding an ACL rule for a user `user123` that allows publishing to topics `data/user123/#`:

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

The corresponding config parameters are:
```
collection = "mqtt_acl"
filter { username = "${username}" }

```

::: warning
When there are a significant number of users in the system make sure that the collections used by the selector are optimized
and that effective indexes are used. Otherwise ACL lookup will produce excessive load on the database
and on the EMQX broker itself.
:::

## Configuration

The MySQL authorizer is identified by type `mongodb`.

The authenticator supports connecting to MongoDB running in three different modes:
* Standalone MongoDB server:
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
*  MongoDB [ReplicaSet](https://www.mongodb.com/docs/manual/reference/replica-configuration/):
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
*  MongoDB [Sharded Cluster](https://www.mongodb.com/docs/manual/sharding/):
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

### Common Configuration Options

#### `collection`

Required string value with the name of MongoDB collection where ACL rules are stored.

#### `filter`

A map interpreted as MongoDB selector for ACL rule lookup.
Supports [placeholders](./authz.md#authentication-placeholders):
* `${clientid}` — clientid of the client.
* `${username}` — username of the client.
* `${peerhost}` — client IP address.

#### `database`

Required string value with MongoDB database name to use.

#### `username`

Optional string value with MongoDB user.

#### `password`

Optional string value with MongoDB user password.

#### `pool_size`

Optional integer value defining the number of concurrent connections from an EMQX node to a MongoDB server.
The default value is 8.

#### `ssl`

Standard [SSL options](../ssl.md) for [secure connecting to MongoDB](https://dev.mysql.com/doc/refman/en/using-encrypted-connections.html).

#### `srv_record`

Optional boolean value, the default value is `false`. If set to `true`, EMQX will try to
fetch information about MongoDB hosts, `replica_set_name` (for `rs` type), and `auth_source` from
DNS records of the specified server(s). See [DNS Seed List Connection Format](https://www.mongodb.com/docs/manual/reference/connection-string/#dns-seed-list-connection-format).

#### `topology`

An optional map of some fine-grained MongoDB driver settings.

* `pool_size` — integer value, the initial size of the internal connection pool.
* `max_overflow` — integer value, number of overflow workers be created, when all workers from the internal pool are busy.
* `overflow_ttl` — duration, number of milliseconds for overflow workers to stay in the internal pool before terminating.
* `overflow_check_period` — duration, `overflow_ttl` check period for workers (in milliseconds).
* `local_threshold_ms` — ms duration, secondaries only which RTTs fit in the window from lower RTT to lower RTT + `local_threshold_ms` could be selected for handling user's requests.
* `connect_timeout_ms` — ms duration, timeout for establishing TCP connections.
* `server_selection_timeout_ms` — ms duration, max time appropriate server should be selected by.
* `wait_queue_timeout_ms` — ms duration, max time for waiting for a worker to be available in the internal pool.
* `heartbeat_frequency_ms` — ms duration, default delay between Topology rescans.
* `min_heartbeat_frequency_ms` — ms duration, the minimum delay between Topology rescans.

### Standalone MongoDB Options

#### `server`

MongoDB server address to connect or to us as a seed, required.

#### `w_mode`

Write mode, `unsafe` (default) or `safe`. The safe mode makes a `getLastError` request after every write in the sequence. If the reply says it failed then the rest of the sequence is aborted. Alternatively, unsafe mode issues every write without confirmation, so if a write fails you won't know about it and the remaining operations will be executed. This is unsafe but faster because there is no round-trip delay.

### MongoDB ReplicaSet Options

#### `servers`

MongoDB server addresses to connect or to us as seeds, required.

#### `w_mode`

Write mode, the same as for [Standalone MongoDB](#wmode).

#### `r_mode`

Read mode, `master` (default) or `slave_ok`. `master` means that every query in a sequence must read only fresh data (from a master/primary server). If the connected server is not a master then the first read will fail, and the remaining operations will be aborted. `slave_ok` means every query is allowed to read stale data from a slave/secondary (fresh data from a master is fine too).

#### `replica_set_name`

Replica set name to use, required. Can be overwritten with seeds if `srv_record` is set to `true`.

### MongoDB Cluster Options

#### `servers`

MongoDB server addresses to connect or to us as seeds, required.

#### `w_mode`

Write mode, the same as for [Standalone MongoDB](#wmode).
