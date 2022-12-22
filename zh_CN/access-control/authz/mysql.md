# MySQL

::: tip
先决条件：

- 了解 [EMQX 授权基本概念](./authz.md)
:::

MySQL Authorizer 支持客户端的权限列表存储在 MySQL 数据库中。

## 表结构与查询语句

MySQL Authorizer 可以支持任何表结构，甚至是多个表联合查询、或从视图中查询。用户需要提供一个查询 SQL 模板，且确保查询结果包含以下字段：

- `permission`: 用于指定操作权限，可选值有 `allow` 和 `deny`
- `action`: 用于指定当前规则适用于哪些操作，可选值有 `publish`、`subscribe` 和 `all`
- `topic`: 用于指定当前规则适用的主题，可以使用主题过滤器和[主题占位符](./authz.md#主题占位符)

示例表结构：

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

::: tip
上面的示例创建了一个索引，当系统中有大量权限数据时，请确保查询使用的表已优化并使用有效的索引，以提升大量连接时的数据查找速度并降低 EMQX 负载。
:::

在此表中使用 `username` 作为查找条件。

添加用户名为 `emqx_u`、禁止发布到 `t/1` 主题的规则示例：

```bash
mysql> INSERT INTO mqtt_acl(username, permission, action, topic) VALUES ('emqx_u', 'deny', 'publish', 't/1');
Query OK, 1 row affected (0,01 sec)
```

对应的配置参数为：

```bash
query = "SELECT permission, action, topic FROM mqtt_acl WHERE username = ${username}"
```

## 配置项

详细配置请参考 [authz:mysql](../../admin/cfg.md#authz:mysql)。

MySQL authorizer 由 `type=mysql` 标识。

配置示例：

```hocon
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

### query

必选的字符串类型配置，用于查询当前客户端具有的权限列表，支持[占位符](./authz.md#数据查询占位符)。

出于安全原因占位符值不会直接拼接 SQL，而是通过 MySQL 预处理插入，能够有效预防 SQL 注入。

例如，以下查询语句：

```sql
SELECT permission, action, topic FROM acl WHERE username = ${username}
```

将首先被转换为以下 Prepared statement：

```sql
SELECT permission, action, topic FROM acl WHERE username = ?
```

然后使用 `${username}` 执行查询。

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

用于 [安全连接到 MySQL](https://dev.mysql.com/doc/refman/en/using-encrypted-connections.html) 的标准 SSL 选项]。
