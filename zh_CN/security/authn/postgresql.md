# 使用 PostgreSQL 的密码认证

该认证器实现了密码验证算法，并使用 PostgreSQL 数据库作为凭证存储。

## 存储架构

PostgreSQL 认证器支持几乎任何存储模式。由用户决定如何存储凭据和访问它们：使用一个或多个表、视图等。

用户只应提供一个模板化查询，该查询将凭证选择为包含 `password_hash`、`salt` 和 `is_superuser` 列的单行。`password_hash` 列是必需的，其他列是可选的。`salt` 列的缺失被解释为空盐（`salt = ""`）；`is_superuser` 的缺失将被设置为默认值 `false`。

用于存储凭据的示例表结构：

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
上面的示例创建了一个隐式的 `UNIQUE` 索引。
当使用不同的模式时，确保在查询中使用的列上创建索引很重要。
:::

在这个表中，MQTT 用户由“用户名”标识。

添加用户名为 `user123`、密码为 `secret`、盐值为 `salt`和超级用户标识为 `true` 的用户示例：

```sql
postgres=# INSERT INTO mqtt_user(username, password_hash, salt, is_superuser) VALUES ('user123', 'bede90386d450cea8b77b822f8887065e4e5abf132c2f9dccfcc7fbd4cba5e35', 'salt', true);
INSERT 0 1
```

对应的配置参数为：

```
password_hash_algorithm {
    name = sha256
    salt_position = prefix
}

query = "SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} LIMIT 1"
```

::: warning
当系统中有大量用户时，请确保查询使用的表已经过优化，并且使用了有效的索引。 否则连接 MQTT 客户端会对数据库和 EMQX 本身产生过多的负载。
:::

## 配置

PostgreSQL 密码认证器由 `mechanism = password_based` 和 `backend = postgresql` 标识。

配置示例：

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

标准的 [密码散列选项](./authn.md#密码散列)。

### `query`

必选的字符串类型配置。用于指定 PostgreSQL 查询语句模板。支持 [占位符](./authn.md#认证占位符)。

出于安全原因，占位符值不是直接插入的，而是通过 PostgreSQL 占位符插入的。

例如，以下查询语句：

```sql
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} AND peerhost = ${peerhost} LIMIT 1
```

将首先被转换为以下 Prepared statement：

```sql
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = $1 AND peerhost = $2 LIMIT 1
```

然后使用 `${username}` 和 `${peerhost}` 执行查询。

### `server`

必选的字符串类型配置，格式为 `host:port`，用于指定 PostgreSQL 服务器地址。

### `database`

必选的字符串类型配置，用于指定 PostgreSQL 数据库名称。

### `username`

可选的字符串类型配置，用于指定 PostgreSQL 用户。

### `password`

可选的字符串类型配置，用户指定 PostgreSQL 用户密码。

#### `auto_reconnect`

可选的布尔类型字段。指定连接中断时 EMQX 是否自动重新连接到 PostgreSQL。默认值为 `true`。

### `pool_size`

可选的整型字段。指定从 EMQX 节点到 PostgreSQL 的并发连接数。默认值为 8。

### `ssl`

用于 [安全连接到 PostgreSQL](https://www.postgresql.org/docs/current/ssl-tcp.html) 的标准 [SSL 选项](../ssl.md)。
