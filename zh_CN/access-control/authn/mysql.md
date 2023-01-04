# 使用 MySQL 的密码认证

作为密码认证方式的一种，EMQX 支持通过集成 MySQL 进行密码认证。

::: tip
前置准备：

- 熟悉 [EMQX 认证基本概念](../authn/authn.md)
:::



## 表结构与查询语句

EMQX MySQL 认证器支持所有表结构，包括多表联合查询、或从视图中查询。用户需要提供一个查询 SQL 模板，并确保查询结果包含以下字段：

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
以上示例创建了一个隐式的 `UNIQUE` 索引。当系统中存在大量用户时，建议预先优化查询使用的集合并为之建立有效的索引，以提升数据查询的响应速度并降低 EMQX 负载。
:::

例如我们希望增加一位用户名为 `emqx_u`、密码为 `public`、盐值为 `slat_foo123`、散列方式为 `sha256` ，且超级用户标志为 `true` 的用户，即可通过如下代码实现：

代码示例：

```bash
mysql> INSERT INTO mqtt_user(username, password_hash, salt, is_superuser) VALUES ('emqx_u', SHA2(concat('public', 'slat_foo123'), 256), 'slat_foo123', 1);
Query OK, 1 row affected (0,01 sec)
```

同时使用 `username` 作为查找条件，对应的查询语句和密码散列方法配置参数为：

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



### 通过 Dashboard 配置

在 [EMQX Dashboard](http://127.0.0.1:18083/#/authentication)页面，点击左侧导航栏的**访问控制** -> **认证**，在随即打开的**认证**页面，单击**创建**，依次选择**认证方式**为 `Password-Based`，**数据源**为 `MySQL`，进入**配置参数**页签：

![image-20230104134554003](./assets/authn-mysql.png)

您可按照如下说明完成相关配置：

**连接：**在此部分完成到 MySQL 数据库的连接设置。

​	**服务**：填入 MySQL 服务器地址 (`host:port`) ，必填项。

​	**数据库**：

​	用户名：

​	密码：

TLS 配置：

连接配置：

​	Pool size：

​	自动重连：

​	查询超时：

认证配置：

​	密码加密方式：

​	加盐方式

### 通过配置文件配置

1. 路径

2. 示例代码

3. 字段解释

   

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
