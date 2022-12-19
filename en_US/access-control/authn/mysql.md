# Password Authentication Using MySQL

This authenticator implements the password verification algorithm and uses MySQL database as credential storage.

## Storage schema

MySQL authenticator supports almost any storage schema. It is up to the user to decide how to store credentials
and access them: using one or multiple tables, views, etc.

The user should only provide a templated query that selects credentials as a single row containing `password_hash`, `salt`, and `is_superuser` columns. `password_hash` column is required, other columns are optional. The absence of `salt` column is interpreted as empty salt (`salt = ""`); the absence of `is_superuser` is interpreted as its false value.

Example table structure for storing credentials:

```sql
CREATE TABLE `mqtt_user` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(100) DEFAULT NULL,
  `password_hash` varchar(100) DEFAULT NULL,
  `salt` varchar(35) DEFAULT NULL,
  `is_superuser` tinyint(1) DEFAULT 0,
  `created` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `mqtt_username` (`username`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
```

::: warning
The above example has an implicit `UNIQUE` index created.
When using a different schema, it is important to make sure an index is created on the column(s) used in your queries.
:::

In this table, MQTT users are identified by `username`.

Example of adding a user with username `user123`, password `secret`, prefixed salt `salt`, and is_superuser `true`:

```
mysql> INSERT INTO mqtt_user(username, password_hash, salt, is_superuser) VALUES ('user123', 'bede90386d450cea8b77b822f8887065e4e5abf132c2f9dccfcc7fbd4cba5e35', 'salt', 1);
Query OK, 1 row affected (0,01 sec)
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
When there are a significant number of users in the system make sure that the tables used by the query are optimized and that effective indexes are used. Otherwise connecting MQTT clients will produce excessive load on the database
and on the EMQX itself.
:::

## Configuration

MySQL authentication is identified with `mechanism = password_based` and `backend = mysql`.

Sample configuration:

```
{
  mechanism = password_based
  backend = mysql
  enable = true

  password_hash_algorithm {
    name = sha256
    salt_position = suffix
  }

  database = mqtt
  username = root
  password = public
  server = "127.0.0.1:3306"
  query = "SELECT password_hash, salt, is_superuser FROM users where username = ${username} LIMIT 1"
}
```

### `password_hash_algorithm`

Standard [password hashing options](./authn.md#password-hashing).

### `query`

Required string value with MySQL query template for fetching credentials. Supports [placeholders](./authn.md#authentication-placeholders).

For security reasons, placeholder values are not interpolated directly, but through MySQL placeholders.

I.e. a query
```sql
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} AND peerhost = ${peerhost} LIMIT 1
```
is first translated into
```sql
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ? AND peerhost = ? LIMIT 1
```
prepared statement and then executed with `${username}` and `${peerhost}` values.

### `server`

Required string value with MySQL server address (`host:port`).

### `database`

Required string value with MySQL database name to use.

### `username`

Optional string value with MySQL user.

### `password`

Optional string value with MySQL user password.

#### `auto_reconnect`

Optional boolean value. The default value is `true`. Specifies whether to automatically reconnect to MySQL on client failure.

### `pool_size`

Optional integer value defining the number of concurrent connections from an EMQX node to a MySQL server.
The default value is 8.

### `ssl`

Standard [SSL options](../ssl.md) for [secure connecting to MySQL](https://dev.mysql.com/doc/refman/en/using-encrypted-connections.html).
