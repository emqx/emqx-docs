# PostgreSQL

This authorizer implements authorization checks through matching pub/sub requests against lists of rules stored in the
PostgreSQL database.

PostgreSQL authorizer supports almost any storage schema. It is up to the user to decide how to store acl rules
and access them: using one or multiple tables, views, etc.

The user should only provide a templated query that selects acl rules as rows containing
`permission`, `action`, and `topic` columns.
* `permission` value specifies the applied action if the rule matches. Should be one of `deny` or `allow`.
* `action` value specifies the request for which the rule is relevant. Should be one of `publish`, `subscribe`, or `all`.
* `topic` value specifies the topic filter for topics relevant to the rule. Should be a string that supports wildcards and
[topic placeholders](./authz.md#topic-placeholders).

Example table structure for storing credentials:

```sql
CREATE TABLE mqtt_acl(
  id serial PRIMARY KEY,
  username text NOT NULL,
  permission text NOT NULL,
  action text NOT NULL,
  topic text NOT NULL
);
CREATE INDEX mqtt_acl_username_idx ON mqtt_acl(username);
```

Example of adding an authorization rule for a user `user123` that allows publishing to topics `data/user123/#`:
```
postgres=# INSERT INTO mqtt_acl(username, permission, action, topic) VALUES ('user123', 'allow', 'publish', 'data/user123/#');
INSERT 0 1
```

The corresponding config parameters are:
```
query = "SELECT permission, action, topic FROM mqtt_acl WHERE username = ${username}"
```

## Configuration

The PostgreSQL authorizer is identified by type `postgresql`.

Sample configuration:

```
{
  type = postgresql
  enable = true

  database = "mqtt"
  username = "postgres"
  password = "public"
  server = "127.0.0.1:5432"
  query = "SELECT permission, action, topic FROM acl WHERE username = ${username}"
}
```

### `query`

Required string value with PostgreSQL query template for fetching authorization rules. Supports [placeholders](./authz.md#authorization-placeholders):
* `${clientid}` — Client ID of the client.
* `${username}` — username of the client.
* `${peerhost}` — client IP address.
* `${cert_subject}` — subject of client's TLS certificate, valid only for TLS connections.
* `${cert_common_name}` common name of client's TLS certificate, valid only for TLS connections.

For security reasons, placeholder values are not interpolated directly, but through PostgreSQL placeholders.
I.e. a query
```sql
SELECT permission, action, topic FROM acl WHERE username = ${username}
```
is first translated into
```sql
SELECT permission, action, topic FROM acl WHERE username = $1
```
prepared statement and then executed with `${username}` value.

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
