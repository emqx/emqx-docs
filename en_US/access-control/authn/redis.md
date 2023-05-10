# Integrate with Redis

EMQX supports integrating with Redis for password authentication. EMQX Redis authenticator currently supports connecting to running in three different modes, which are Single, [Redis Sentinel](https://redis.io/docs/manual/sentinel/) and [Redis Cluster](https://redis.io/docs/manual/scaling/). This section gives detailed instructions on the data schema supported and on how to configure with EMQX Dashboard and configuration file. 

::: tip Prerequisite:

- Knowledge about [basic EMQX authentication concepts](../authn/authn.md)

:::

## Data Schema and Query Statement

Redis authentication works with credentials stored as [Redis hashes](https://redis.io/docs/manual/data-types/#hashes) with predefined field names: 

- `password_hash`: required; password (in plain text or hashed) stored in the database; 
- `salt`: optional; `salt = ""` or just remove this field to indicate no salt value will be added; 
-  `is_superuser`: optional; flag if the current client is a superuser; default: `false`.

For example, if we want to add a document for a superuser (`is_superuser`: `true`) with username `user123`, password `secret`, prefixed salt `salt`, and password hash `sha256`, the query statement should be:

```bash
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
The name `password_hash` conveys our preference for storing hashed passwords. But given that Redis doesn't have a MySQL-like `as` syntax, EMQX 5.0 has kept the `password` field (in EMQX 4.x) compatible.

So, we can also configure `cmd` as `HMGET mqtt:${username} password salt is_superuser`.
:::

## Configure with Dashboard

You can use EMQX Dashboard to configure how to use Redis for password authentication. 

On [EMQX Dashboard](http://127.0.0.1:18083/#/authentication), click **Access Control** -> **Authentication** on the left navigation tree to enter the **Authentication** page. Click **Create** at the top right corner, then click to select **Password-Based** as **Mechanism**, and **Redis** as **Backend**, this will lead us to the **Configuration** tab, as shown below. 

<img src="./assets/authn-redis.png" alt="Authentication with redis" style="zoom:67%;" />

Follow the instruction below on how to configure:

**Connect**: Fill in the information needed to connect Redis.

- **Redis Mode**: Select how Redis is deployed, including **Single**, **Sentinel** and **Cluster**. 
- **Server(s)**: Specify the Redis server address that EMQX is to connect, if **Redis Mode** is set to **Sentinel** or **Cluster**, you will need to input all Redis servers (separated with a `,`) that EMQX is to connect.
- **Sentinel Name**: Specify the name to use; type: strings; only needed if you set **Redis Mode** to **Sentinel**.
- **Database**: Redis database name; Data type: strings.
- **Password** (optional): Specify Redis user password. 

**TLS Configuration**: Turn on the toggle switch if you want to enable TLS. 

**Connection Configuration**: Set the concurrent connections.

- **Pool size** (optional): Input an integer value to define the number of concurrent connections from an EMQX node to a Redis server. Default: **8**. 

**Authentication configuration**: Fill in the authentication-related settings:

- **Password Hash**: Select the Hash function for storing the password in the database, for example, plain, md5, sha, bcrypt, pbkdf2. 
  - If **plain**, **md5**, **sha**, **sha256** or **sha512** are selected, we also need to configure:
    - **Salt Position**: Specify the way (**suffix**, **prefix**, or **disable**) to add salt (random data) to the password. You can keep the default value unless you are migrating user credentials from external storage into EMQX built-in database. Note: If **plain** is selected, the **Salt Position** should be **disable**. 
  - If **bcrypt** is selected, you also need to configure:
    - **Salt Rounds**: Specify the calculation times of Hush function (2^Salt Rounds). Default value: **10**; Value range **4~31**. You are recommended to use a higher value for better protection. Note: Increasing the cost factor by 1 doubles the necessary time. 
  - If **pkbdf2** is selected, we also need to configure:
    - **Pseudorandom Function**: Specify the Hush functions to generate the key, such as sha256. 
    - **Iteration Count**: Specify the iteration times; Default: 4096
    - **Derived Key Length** (optional): Specify the length of the generated password. You can leave this field blank, then the key length will be determined by the pseudorandom function you selected. 
- **CMD**: Redis query command. 

Now we can click **Create** to finish the settings. 

## Configure with Configuration Items

You can configure the EMQX Redis authenticator with EMQX configuration items. <!--For detailed operation steps, see  [authn-redis:standalone](../../configuration/configuration-manual.md#authn-redis:standalone), [authn-redis:sentinel](../../configuration/configuration-manual.md#authn-redis:sentinel), and  [authn-redis:cluster](../../configuration/configuration-manual.md#authn-redis:cluster).-->

Redis authentication is identified with `mechanism = password_based` and `backend = redis`.

EMQX supports working with three kinds of Redis installation.

:::: tabs type:card

::: tab Standalone Redis.

```bash
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

:::

::: tab Redis Sentinel 

```bash
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

:::

::: tab Redis Cluster 

```bash
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
  password = "public"
  auto_reconnect = true
}
```

:::

::::
