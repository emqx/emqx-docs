# 使用 MySQL 的密码认证

该认证器实现了密码验证算法，并使用 MySQL 数据库作为凭证存储。

## 存储架构

MySQL 认证器支持几乎任何存储模式。由用户决定如何存储凭据并访问它们：使用一个或多个表、视图等。

用户只应提供一个模板化查询，该查询将凭证选择为包含 `password_hash`、`salt` 和 `is_superuser` 列的单行。 `password_hash` 列是必需的，其他列是可选的。`salt` 列的缺失被解释为空盐（`salt = ""`）； `is_superuser` 缺失会被设置为默认值 `false`。

用于存储凭据的示例表结构：

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
上面的示例创建了一个隐式的 `UNIQUE` 索引。
当使用不同的模式时，确保在查询中使用的列上创建索引很重要。
:::

在此表中，MQTT 用户由“用户名”标识。

添加用户名为 `user123`、密码为 `secret`、盐值为 `salt` 和超级用户标志为 `true` 的用户示例：

```
mysql> INSERT INTO mqtt_user(username, password_hash, salt, is_superuser) VALUES ('user123', 'bede90386d450cea8b77b822f8887065e4e5abf132c2f9dccfcc7fbd4cba5e35', 'salt', 1);
Query OK, 1 row affected (0,01 sec)
```

对应的配置参数为：

```
password_hash_algorithm {
    name = sha256
    salt_position = prefix
}

query = "SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} LIMIT 1"
```

注意，当系统中有大量用户时，请确保查询使用的表已优化并使用有效的索引。否则连接 MQTT 客户端会对数据库和 EMQX 本身产生过多的负载。

## 配置

使用 MySQL 的密码认证器由 `mechanism = password_based` 和 `backend = mysql` 标识。

配置示例：

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

标准的 [密码散列选项](./authn.md#密码散列)。

### `query`

用于查询身份凭据的 MySQL 查询语句模板，这是一个必填项，支持使用 [占位符](./authn.md#认证占位符)。

出于安全原因，占位符值不是直接插入的，而是通过 MySQL 占位符插入的。

例如，以下查询语句：

```sql
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} AND peerhost = ${peerhost} LIMIT 1
```

将首先被转换为以下 Prepared statement：

```sql
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ? AND peerhost = ? LIMIT 1
```

然后使用 `${username}` 和 `${peerhost}` 执行查询。

### `server`

MySQL 服务器地址 (`host:port`) ，必填项。

### `database`

MySQL 数据库名称，必填项。

### `username`

MySQL 用户，可选。
### `password`

MySQL 用户密码，可选。

#### `auto_reconnect`

可选的布尔类型字段。指定连接中断时 EMQX 是否自动重新连接到 MySQL。默认值为 `true`。

### `pool_size`

可选的整型字段。指定从 EMQX 节点到 MySQL 的并发连接数。默认值为 8。

### `ssl`

用于 [安全连接至 MySQL](https://dev.mysql.com/doc/refman/en/using-encrypted-connections.html) 的标准 [SSL 选项](../ssl.md)。
