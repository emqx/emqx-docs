# MySQL

MySQL Authorizer 支持客户端的授权规则存储在 MySQL 数据库中。

MySQL Authorizer 支持几乎任何存储模式。由用户决定如何存储 访问它们：使用一个或多个表、视图等。

用户应该只提供一个模板化查询，该查询选择匹配的规则，并返回规则中的 `permission`、`action` 和 `topic` 列，该规则定义了当前用户对于规则中声明的主题具有什么样的操作权限，例如：允许发布、拒绝订阅等。

- `permission` 用于指定操作权限，可选值有 `allow` 和 `deny`。
- `action` 用于指定当前规则适用于哪些操作，可选值有 `publish`、`subscribe` 和 `all`。
- `topic` 用于指定当前规则适用的主题，可以使用主题过滤器和 [主题占位符](./authz.md#主题占位符)。

用于存储权限规则的示例表结构：

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

为用户 `user123` 添加允许发布到主题 `data/user123/#` 的权限规则的示例：

```
mysql> INSERT INTO mqtt_acl(username, permission, action, topic) VALUES ('user123', 'allow', 'publish', 'data/user123/#');
Query OK, 1 row affected (0,01 sec)
```

对应的配置参数为：

```
query = "SELECT permission, action, topic FROM mqtt_acl WHERE username = ${username}"
```

## 配置

MySQL authorizer 由 `type=mysql` 标识。

示例：

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

必选的字符串类型配置，用于指定 MySQL 数据库中权限规则的查询模板，支持 [占位符](./authz.md#authorizer-配置中的占位符)。

出于安全原因，占位符值不是直接插入的，而是通过 MySQL 占位符插入的。

例如，以下查询语句：

```sql
SELECT permission, action, topic FROM acl WHERE username = ${username}
```

将首先被转换为以下 Prepared statement：

```sql
SELECT permission, action, topic FROM acl WHERE username = ?
```

然后使用 `${username}` 执行查询。

### `server`

必选的字符串类型配置，用于指定 MySQL 服务器地址，格式为 `host:port`。

### `database`

必选的字符串类型配置，用于指定 MySQL 数据库名称。

### `username`

可选的字符串类型配置，用于指定 MySQL 用户名。

### `password`

可选的字符串类型配置，用于指定 MySQL 用户密码。

#### `auto_reconnect`

可选的布尔类型配置，用于指定 EMQX 是否会在连接断开时自动重新连接到 MySQL，默认值为 `true`。

### `pool_size`

可选的整型配置，用于指定 EMQX 节点到 MySQL 服务器的并发连接数，默认值为 8。

### `ssl`

用于 [安全连接到 MySQL](https://dev.mysql.com/doc/refman/en/using-encrypted-connections.html) 的标准 [SSL 选项](../ssl.md)。
