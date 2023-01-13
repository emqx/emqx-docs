# PostgreSQL

PostgreSQL Authorizer 支持客户端的授权规则存储在 PostgreSQL 数据库中。

PostgreSQL Authorizer 支持几乎任何存储模式。由用户决定如何存储 访问它们：使用一个或多个表、视图等。

用户应该只提供一个模板化查询，该查询选择匹配的规则，并返回规则中的 `permission`、`action` 和 `topic` 列，该规则定义了当前用户对于规则中声明的主题具有什么样的操作权限，例如：允许发布、拒绝订阅等。

- `permission` 用于指定操作权限，可选值有 `allow` 和 `deny`。
- `action` 用于指定当前规则适用于哪些操作，可选值有 `publish`、`subscribe` 和 `all`。
- `topic` 用于指定当前规则适用的主题，可以使用主题过滤器和 [主题占位符](./authz.md#主题占位符)。

用于存储权限规则的示例表结构：

```sql
CREATE TYPE ACTION AS ENUM('publish','subscribe','all');
CREATE TYPE PERMISSION AS ENUM('allow','deny');

CREATE TABLE mqtt_acl (
  id SERIAL PRIMARY KEY,
  ipaddress CHARACTER VARYING(60) NOT NULL DEFAULT '',
  username CHARACTER VARYING(255) NOT NULL DEFAULT '',
  clientid CHARACTER VARYING(255) NOT NULL DEFAULT '',
  action ACTION,
  permission PERMISSION,
  topic CHARACTER VARYING(255) NOT NULL
);

CREATE INDEX mqtt_acl_username_idx ON mqtt_acl(username);
```

为用户 `user123` 添加允许发布到主题 `data/user123/#` 的权限规则的示例：

```
postgres=# INSERT INTO mqtt_acl(username, permission, action, topic, ipaddress) VALUES ('user123', 'allow', 'publish', 'data/user123/#', '127.0.0.1');
INSERT 0 1
```

对应的配置参数为：

```
query = "SELECT permission, action, topic, ipaddress FROM mqtt_acl WHERE username = ${username} and ipaddress = ${peerhost}"
```

## 配置

PostgreSQL authorizer 由 `type=postgresql` 标识。

示例：

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

必选的字符串类型配置，用于指定 MySQL 数据库中权限规则的查询模板，支持 [占位符](./authz.md#authorizer-配置中的占位符)。

出于安全原因，占位符值不是直接插入的，而是通过 PostgreSQL 占位符插入的。

例如，以下查询语句：

```sql
SELECT permission, action, topic FROM acl WHERE username = ${username}
```

将首先被转换为以下 Prepared statement：

```sql
SELECT permission, action, topic FROM acl WHERE username = $1
```

然后使用 `${username}` 执行查询。

### `server`

必选的字符串类型配置，用于指定 PostgreSQL 服务器地址，格式为 `host:port`。

### `database`

必选的字符串类型配置，用于指定 PostgreSQL 数据库名称。

### `username`

可选的字符串类型配置，用于指定 PostgreSQL 用户名。

### `password`

可选的字符串类型配置，用于指定 PostgreSQL 用户密码。

#### `auto_reconnect`

可选的布尔类型配置，用于指定 EMQX 是否会在连接断开时自动重新连接到 PostgreSQL，默认值为 `true`。


### `pool_size`

可选的整型配置，用于指定 EMQX 节点到 PostgreSQL 服务器的并发连接数，默认值为 8。

### `ssl`

用于 [安全连接到 PostgreSQL](https://www.postgresql.org/docs/current/ssl-tcp.html) 的标准 [SSL 选项](../ssl.md)。
