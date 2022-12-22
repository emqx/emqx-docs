# 使用 MySQL 的密码认证

::: tip
先决条件：

- 了解 [EMQX 认证基本概念](../authn/authn.md)
:::

该认证器实现了密码认证，并使用 MySQL 数据库作为数据源。

## 表结构与查询语句

MySQL 认证器可以支持任何表结构，甚至是多个表联合查询、或从视图中查询。用户需要提供一个查询 SQL 模板，且确保查询结果包含以下字段：

- `password_hash`: 必需，数据库中的明文或散列密码字段
- `salt`: 可选，为空或不存在时视为空盐（`salt = ""`）
- `is_superuser`: 可选，标记当前客户端是否为超级用户，默认为 `false`

示例表结构：

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

::: tip
上面的示例创建了一个隐式的 `UNIQUE` 索引，当系统中有大量用户时，请确保查询使用的表已优化并使用有效的索引，以提升大量连接时的数据查找速度并降低 EMQX 负载。
:::

在此表中使用 `username` 作为查找条件。

添加用户名为 `emqx_u`、密码为 `public`、盐值为 `slat_foo123`、散列方式为 `sha256` 且超级用户标志为 `true` 的用户示例：

```bash
mysql> INSERT INTO mqtt_user(username, password_hash, salt, is_superuser) VALUES ('emqx_u', SHA2(concat('public', 'slat_foo123'), 256), 'slat_foo123', 1);
Query OK, 1 row affected (0,01 sec)
```

对应的查询语句和密码散列方法配置参数为：

```bash
query = "SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} LIMIT 1"

password_hash_algorithm {
    name = sha256
    salt_position = suffix
}
```

## 配置项

详细配置请参考 [authn-mysql:authentication](../../admin/cfg.md#authn-mysql:authentication)。

配置示例：

```hocon
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

### password_hash_algorithm

[密码散列选项](./authn.md#密码散列)。

### query

用于查询身份凭据的 MySQL 查询语句模板，这是一个必填项，支持[占位符](./authn.md#认证占位符)。

出于安全原因占位符值不会直接拼接 SQL，而是通过 MySQL 预处理插入，能够有效预防 SQL 注入。

例如，以下查询语句：

```sql
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} AND peerhost = ${peerhost} LIMIT 1
```

将首先被转换为以下 Prepared statement：

```sql
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ? AND peerhost = ? LIMIT 1
```

然后使用 `${username}` 和 `${peerhost}` 执行查询。

### server

MySQL 服务器地址 (`host:port`) ，必填项。

### database

MySQL 数据库名称，必填项。

### username

MySQL 用户，可选。

### password

MySQL 密码，可选。

#### auto_reconnect

可选的布尔类型字段。指定连接中断时 EMQX 是否自动重新连接到 MySQL。默认值为 true。

### pool_size

可选的整型字段。指定从 EMQX 节点到 MySQL 的并发连接数。默认值为 8。

### ssl

用于 [安全连接至 MySQL](https://dev.mysql.com/doc/refman/en/using-encrypted-connections.html) 的标准 SSL 选项。
