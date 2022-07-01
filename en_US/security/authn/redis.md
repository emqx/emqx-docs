# Password Authentication Using Redis

This authenticator implements the password verification algorithm and uses Redis database as credential storage.

## Storage schema

Redis authentication works with credentials stored as Redis [hashes](https://redis.io/docs/manual/data-types/#hashes) with predefined field names: `password_hash`, `salt`, `is_superuser`. `password_hash` field is required, other fields are optional. The absence of `salt` field is interpreted as empty salt (`salt = ""`); the absence of `is_superuser` is interpreted as its false value.

Example of adding a user with username `user123`, password `secret`, prefixed salt `salt` and is_superuser `true`:

```
>redis-cli
127.0.0.1:6379> HSET mqtt:user123 is_superuser 1 salt salt password_hash ac63a624e7074776d677dd61a003b8c803eb11db004d0ec6ae032a5d7c9c5caf
(integer) 3
```

The corresponding config params are:

```
password_hash_algorithm {
    name = sha256
    salt_position = prefix
}

cmd = "HMGET mqtt:${username} password_hash salt is_superuser"
```

::: tip
The name `password_hash` conveys our preference for storing hashed passwords. But given that Redis doesn't have a MySQL-like as syntax, we keep `password` compatible.

So, we can also configure `cmd` as `HMGET mqtt:${username} password salt is_superuser`.
:::

## Configuration

Redis authentication is identified with `mechanism = password_based` and `backend = redis`.

EMQX supports working with three kinds of Redis installation.

- Standalone Redis.

  ```
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

- [Redis Sentinel](https://redis.io/docs/manual/sentinel/).

  ```
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

- [Redis Cluster](https://redis.io/docs/manual/scaling/).

  ```
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

Required string value with the command used for fetching credentials. Supported command formats are:

- `HMGET KEY_TEMPLATE ...Fields...` where possible fields are `password_hash`, `salt`, `is_superuser`. `password_hash` is
required to be present;
- `HGET KEY_TEMPLATE password_hash`.

`KEY_TEMPLATE` supports [placeholders](./authn.md#authentication-placeholders).

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
