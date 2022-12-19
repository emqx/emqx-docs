# Redis

This authorizer implements authorization checks through matching pub/sub requests against lists of rules stored in the
Redis database.

The user should provide a templated Redis command that returns a key-value list with topic filters as keys and actions(`publish`, `subscribe`, or `all`) as values.

For example, rules can be stored as Redis [hashes](https://redis.io/docs/manual/data-types/#hashes):

```
>redis-cli
127.0.0.1:6379> HSET users:someuser foo/# subscribe
(integer) 1
127.0.0.1:6379> HSET users:someuser bar/baz publish
(integer) 1
```

The corresponding config parameters are:
```
cmd = "HGET users:${username}"
```

Fetched rules are used as permissive ones, i.e., a request is accepted if topic filter and action match.

## Configuration

The Redis authorizer is identified by type `redis`.

EMQX supports working with three kinds of Redis installation.

* Standalone Redis.
  ```
  {
      type = redis
      enable = true

      redis_type = single
      server = "127.0.0.1:6379"

      cmd = "HGETALL mqtt_user:${username}"
      database => 1
      password = public
      server = "127.0.0.1:6379"

  }
  ```
* [Redis Sentinel](https://redis.io/docs/manual/sentinel/).
  ```
  {
      type = redis
      enable = true

      redis_type = sentinel
      servers = "10.123.13.11:6379,10.123.13.12:6379"
      sentinel = "mymaster"

      cmd = "HGETALL mqtt_user:${username}"
      database => 1
      password = public

  }
  ```
* [Redis Cluster](https://redis.io/docs/manual/scaling/).
  ```
  {
      type = redis
      enable = true

      redis_type = cluster
      servers = "10.123.13.11:6379,10.123.13.12:6379"

      cmd = "HGETALL mqtt_user:${username}"
      database => 1
      password = public
  }
  ```

### Common configuration parameters

#### `redis_type`

One of `single`, `cluster`, or `sentinel`, required. Defines Redis installation type:
standalone Redis, [Redis Cluster](https://redis.io/docs/manual/scaling/), or
[Redis Sentinel](https://redis.io/docs/manual/sentinel/) respectively.

#### `cmd`

Required string value with the command used for fetching authorization rules. The following placeolders are supported for `cmd` value:
* `${clientid}` — Client ID of the client.
* `${username}` — username of the client.
* `${peerhost}` — client IP address.
* `${cert_subject}` — subject of client's TLS certificate, valid only for TLS connections.
* `${cert_common_name}` common name of client's TLS certificate, valid only for TLS connections.

[Topic placeholders](./authz.md#topic-placeholders) are allowed in topic filters.

#### `database`

Redis database index to use, required.

#### `password`

Password used for Redis [authentication](https://redis.io/docs/manual/security/#authentication), optional.

#### `auto_reconnect`

Optional boolean value. The default value is `true`. Specifies whether to automatically reconnect to
Redis on client failure.

#### `pool_size`

Optional integer value defining the number of concurrent connections from an EMQX node to Redis.
The default value is 8.

#### `ssl`

Standard [SSL options](../ssl.md) for [secure connecting to Redis](https://redis.io/docs/manual/security/encryption/).

### Standalone Redis options (`redis_type = single`).

#### `server`

Required `host:port` string value, the address of the Redis server.

### Redis Cluster options (`redis_type = cluster`).

#### `servers`

Required string value with comma-separated list of Redis Cluster endpoints: `host1:port1,host2:port2,...`.

### Redis Sentinel options (`redis_type = sentinel`).

#### `servers`

Required string value with comma-separated list of Redis Sentinel endpoints: `host1:port1,host2:port2,...`.

#### `sentinel`

Required string value with [master name](https://redis.io/docs/manual/sentinel/#configuring-sentinel) to use from Sentinel configuration.
