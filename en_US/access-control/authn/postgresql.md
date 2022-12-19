# Password Authentication Using PostgreSQL

This authenticator implements the password verification algorithm and uses PostgreSQL database as credential storage.

## Storage schema

PostgreSQL authenticator supports almost any storage schema. It is up to the user to decide how to store credentials and access them: use one or multiple tables, views, etc.

The user should only provide a templated query that selects credentials as a single row containing `password_hash`, `salt`, and `is_superuser` columns. `password_hash` column is required, other columns are optional. The absence of `salt` column is interpreted as empty salt (`salt = ""`); the absence of `is_superuser` is interpreted as its false value.

Example table structure for storing credentials:

```sql
CREATE TABLE mqtt_user (
    id serial PRIMARY KEY,
    username text NOT NULL UNIQUE,
    password_hash  text NOT NULL,
    salt text NOT NULL,
    is_superuser boolean DEFAULT false,
    created timestamp with time zone DEFAULT NOW()
);
```

::: warning
The above example has an implicit `UNIQUE` index created.
When using a different schema, it is important to make sure an index is created on the column(s) used in your queries.
:::

In this table, MQTT users are identified by `username`.

Example of adding a user with username `user123`, password `secret`, prefixed salt `salt`, and is_superuser `true`:

```sql
postgres=# INSERT INTO mqtt_user(username, password_hash, salt, is_superuser) VALUES ('user123', 'bede90386d450cea8b77b822f8887065e4e5abf132c2f9dccfcc7fbd4cba5e35', 'salt', true);
INSERT 0 1
```

The corresponding config parameters are:

```
password_hash_algorithm {
    name = sha256
    salt_position = prefix
}

query = "SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} LIMIT 1"
```

::: warning
When there are a significant number of users in the system make sure that the tables used by the query are optimized and that effective indexes are used. Otherwise connecting MQTT clients will produce excessive load on the database and on the EMQX itself.
:::

## Configuration

PostgreSQL authentication is identified with `mechanism = password_based` and `backend = postgresql`.

Sample configuration:

```
{
  mechanism = password_based
  backend = postgresql
  enable = true

  password_hash_algorithm {
    name = sha256
    salt_position = suffix
  }

  database = mqtt
  username = postgres
  password = public
  server = "127.0.0.1:5432"
  query = "SELECT password_hash, salt, is_superuser FROM users where username = ${username} LIMIT 1"
}
```

### `password_hash_algorithm`

Standard [password hashing options](./authn.md#password-hashing).

### `query`

Required string value with PostgreSQL query template for fetching credentials. Supports [placeholders](./authn.md#authentication-placeholders).

For security reasons, placeholder values are not interpolated directly, but through PostgreSQL placeholders.
I.e. a query
```sql
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} AND peerhost = ${peerhost} LIMIT 1
```
is first translated into
```sql
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = $1 AND peerhost = $2 LIMIT 1
```
prepared statement and then executed with `${username}` and `${peerhost}` values.

### `server`

Required string value with PostgreSQL server address (`host:port`).

### `database`

Required string value with PostgreSQL database name to use.

### `username`

Optional string value with PostgreSQL user.

### `password`

Optional string value with PostgreSQL user password.

#### `auto_reconnect`

Optional boolean value. The default value is `true`. Specifies whether to automatically reconnect to
PostgreSQL on client failure.

### `pool_size`

Optional integer value defining the number of concurrent connections from an EMQX node to a PostgreSQL server.
The default value is 8.

### `ssl`

Standard [SSL options](../ssl.md) for [secure connecting to PostgreSQL](https://www.postgresql.org/docs/current/ssl-tcp.html).
