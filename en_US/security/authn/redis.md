# Redis

This authenticator implements password verification algorithm and uses Redis database as credential storage.

## Configuration

EMQX supports working with three kinds of Redis installation.

* Standalone Redis.
  ```hocon
  {
    mechanism = password_based
    backend = redis
    enable = true

    redis_type = single
    server = "127.0.0.1:6379"

    password_hash_algorithm {
        name = sha256
        salt_position = suffix
    }

    cmd = "HMGET mqtt_user:${username} password_hash salt is_superuser"
    database = 1
    password = "public"
    auto_reconnect = true
  }
  ```
* [Redis Sentinel](https://redis.io/docs/manual/sentinel/).
  ```hocon
  {
    mechanism = password_based
    backend = redis
    enable = true

    redis_type = sentinel
    servers = "10.123.13.11:6379,10.123.13.12:6379"
    sentinel = "mymaster"

    password_hash_algorithm {
        name = sha256
        salt_position = suffix
    }

    cmd = "HMGET mqtt_user:${username} password_hash salt is_superuser"
    database = 1
    password = "public"
    auto_reconnect = true
  }
  ```
* [Redis Cluster](https://redis.io/docs/manual/scaling/).
  ```hocon
  {
    mechanism = password_based
    backend = redis
    enable = true

    redis_type = cluster
    servers = "10.123.13.11:6379,10.123.13.12:6379"

    password_hash_algorithm {
        name = sha256
        salt_position = suffix
    }

    cmd = "HMGET mqtt_user:${username} password_hash salt is_superuser"
    database = 1
    password = "public"
    auto_reconnect = true
  }
  ```

### Common configuration parameters

#### `redis_type`

One of `single`, `cluster`, or `sentinel`, required. Defines Redis installation type:
standalone Redis, [Redis Cluster](https://redis.io/docs/manual/scaling/), or
[Redis Sentinel](https://redis.io/docs/manual/sentinel/) respectively.

#### `password_hash_algorithm`

Standard [password hashing options](./authn.md#password-hashing).

#### `cmd`

Requied string value with command used for fetching credetials. Supported command formats are:
* `HMGET KEY_TEMPLATE ...Fields...` where possible fields are `password_hash`, `salt`, `is_superuser`. `password_hash` is
required to be present;
* `HGET KEY_TEMPLATE password_hash`.

`KEY_TEMPLATE` supports [placeholders](./authn.md#authentication-placeholders).

#### `database`

Redis database index to use, required.

#### `password`

Password used for Redis [authentication](https://redis.io/docs/manual/security/#authentication), optional.

#### `auto_reconnect`

Optional boolean value. The default value is `true`. Specifies whether to automatically reconnect to
Redis on client failure.

#### `pool_size`

Optional integer value defining number of concurrent connections from an EMQX node to Redis.
The default value is 8.

#### `ssl`

Standard [SSL options](./ssl.md) for [secure connecting to Redis](https://redis.io/docs/manual/security/encryption/).

### Standalone Redis options (`redis_type = single`).

#### `server`

Required `host:port` string value, the address of Redis server.

### Redis Cluster options (`redis_type = cluster`).

#### `servers`

Required string value with comma-separated list of Redis Cluster endpoints: `host1:port1,host2:port2,...`.

### Redis Sentinel options (`redis_type = sentinel`).

#### `servers`

Required string value with comma-separated list of Redis Sentinel endpoints: `host1:port1,host2:port2,...`.

#### `sentinel`

Required string value with [master name](https://redis.io/docs/manual/sentinel/#configuring-sentinel) to use from Sentinel configuration.

