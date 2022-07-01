# MySQL

This authorizer implements authorization checks through matching pub/sub requests against lists of rules stored in the
MySQL database.

MySQL authorizer supports almost any storage schema. It is up to the user to decide how to store acl rules
and access them: using one or multiple tables, views, etc.

The user should only provide a templated query that selects acl rules as rows containing
`permission`, `action`, and `topic` columns.
* `permission` value specifies the applied action if the rule matches. Should be one of `deny` or `allow`.
* `action` value specifies the request for which the rule is relevant. Should be one of `publish`, `subscribe`, or `all`.
* `topic` value specifies the topic filter for topics relevant to the rule. Should be a string that supports wildcards and
[topic placeholders](./authz.md#topic-placeholders).

Example table structure for storing credentials:

```sql
CREATE TABLE `mqtt_acl` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(100) NOT NULL,
  `permission` varchar(5) NOT NULL,
  `action` varchar(9) NOT NULL,
  `topic` varchar(100) NOT NULL,
  INDEX username_idx(username),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
```

Example of adding an authorization rule for a user `user123` that allows publishing to topics `data/user123/#`:
```
mysql> INSERT INTO mqtt_acl(username, permission, action, topic) VALUES ('user123', 'allow', 'publish', 'data/user123/#');
Query OK, 1 row affected (0,01 sec)
```

The corresponding config parameters are:
```
query = "SELECT permission, action, topic FROM mqtt_acl WHERE username = ${username}"
```

## Configuration

The MySQL authorizer is identified by type `mysql`.

Sample configuration:

```
{
  type = mysql
  enable = true

  database = "mqtt"
  username = "root"
  password = "public"
  server = "127.0.0.1:3306"
  query = "SELECT permission, action, topic FROM acl WHERE username = ${username}"
}
```

### `query`

Required string value with MySQL query template for fetching authorization rules. Supports [placeholders](./authz.md#authorization-placeholders):
* `${clientid}` — Client ID of the client.
* `${username}` — username of the client.
* `${peerhost}` — client IP address.
* `${cert_subject}` — subject of client's TLS certificate, valid only for TLS connections.
* `${cert_common_name}` common name of client's TLS certificate, valid only for TLS connections.

For security reasons, placeholder values are not interpolated directly, but through MySQL placeholders.
I.e. a query
```sql
SELECT permission, action, topic FROM acl WHERE username = ${username}
```
is first translated into
```sql
SELECT permission, action, topic FROM acl WHERE username = ?
```
prepared statement and then executed with `${username}` value.

### `server`

Required string value with MySQL server address (`host:port`).

### `database`

Required string value with MySQL database name to use.

### `username`

Optional string value with MySQL user.

### `password`

Optional string value with MySQL user password.

#### `auto_reconnect`

Optional boolean value. The default value is `true`. Specifies whether to automatically reconnect to
MySQL on client failure.

### `pool_size`

Optional integer value defining the number of concurrent connections from an EMQX node to a MySQL server.
The default value is 8.

### `ssl`

Standard [SSL options](../ssl.md) for [secure connecting to MySQL](https://dev.mysql.com/doc/refman/en/using-encrypted-connections.html).
