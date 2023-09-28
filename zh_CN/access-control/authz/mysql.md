# 基于 MySQL 进行授权

MySQL Authorizer 支持客户端的权限列表存储在 MySQL 数据库中。

::: tip 前置准备

熟悉 [EMQX 授权基本概念](./authz.md)
:::

## 表结构与查询语句

MySQL Authorizer 可以支持任何表结构，甚至是多个表联合查询、或从视图中查询。用户需要提供一个查询 SQL 模板，且确保查询结果包含以下字段：

- `permission`: 用于指定操作权限，可选值有 `allow` 和 `deny`。
- `action`: 用于指定当前规则适用于哪些操作，可选值有 `publish`、`subscribe` 和 `all`。
- `topic`: 用于指定当前规则适用的主题，可以使用主题过滤器和[主题占位符](./authz.md#主题占位符)。
- `qos`: (可选)用于指定规则适用的消息 QoS，可选值为 `0`、`1`、`2`，也可以用 `,` 分隔的字符串指定多个 QoS，例如 `0,1`。默认为全部 QoS。
- `retain`: （可选）用于指定当前规则是否支持发布保留消息，可选值有 `0`、`1`，默认允许保留消息。

## 示例表结构

在数据库中创建如下表结构：

```sql
CREATE TABLE `mqtt_acl` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(100) NOT NULL,
  `permission` varchar(5) NOT NULL,
  `action` varchar(9) NOT NULL,
  `topic` varchar(100) NOT NULL,
  `qos` tinyint(1),
  `retain` tinyint(1),
  INDEX username_idx(username),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
```

::: tip
当系统中有大量权限数据时，请确保查询使用的表已优化并使用有效的索引，以提升大量连接时的数据查找速度、并降低 EMQX 负载。
:::

## 查询语句

在 EMQX 中配置以下查询参数，使用 `mqtt_acl` 表并以 `username` 作为查找条件，查询出权限数据。

```bash
SELECT 
  permission, action, topic, qos, retain 
FROM mqtt_acl 
  WHERE username = ${username}
```

## 权限测试

MySQL Authorizer 添加成功后完成后，向 MySQL 中添加权限数据，并使用 MQTTX CLI 连接到 EMQX 进行测试。

1. 用户名为 `emqx_u`、禁止发布到 `t/1` 主题的规则示例：

```bash
INSERT INTO mqtt_acl(username, permission, action, topic) VALUES ('emqx_u', 'deny', 'publish', 't/1');
```

使用以下命令进行发布测试，测试结果表明没有发布权限：

```bash
$ mqttx pub -u emqx_u -t t/1 -q 1 -m '{ "msg": "Can I publish it?" }'
[2023-9-20] [18:43:38] › …  Connecting...
[2023-9-20] [18:43:39] › ✔  Connected
[2023-9-20] [18:43:39] › …  Message publishing...
[2023-9-20] [18:43:39] › ⚠  Error: Publish error: Not authorized
```

2. 用户名为 `emqx_u`、禁止发布保留消息到 `t/2` 主题的规则示例：

```bash
INSERT INTO mqtt_acl(username, permission, action, topic, retain) VALUES ('emqx_u', 'deny', 'publish', 't/2', 1);
```

使用以下命令进行发布测试，测试结果表明仅保留消息没有发布权限：

```bash
# 可以发布成功
$ mqttx pub -u emqx_u -t t/2 -q 1 -m '{ "msg": "Can I publish it?" }'
[2023-9-20] [18:47:10] › …  Connecting...
[2023-9-20] [18:47:10] › ✔  Connected
[2023-9-20] [18:47:10] › …  Message publishing...
[2023-9-20] [18:47:10] › ✔  Message published

# -r 参数指定消息为保留消息时，没有发布权限
$ mqttx pub -u emqx_u -t t/2 -q 1 -r -m '{ "msg": "Can I publish it?" }'
[2023-9-20] [18:46:00] › …  Connecting...
[2023-9-20] [18:46:00] › ✔  Connected
[2023-9-20] [18:46:00] › …  Message publishing...
[2023-9-20] [18:46:00] › ⚠  Error: Publish error: Not authorized
````

3. 用户名为 `emqx_u`、禁止以 QoS1 订阅 `t/3` 主题的规则示例：

```bash
INSERT INTO mqtt_acl(username, permission, action, topic, qos) VALUES ('emqx_u', 'deny', 'subscribe', 't/3', 1);
```

使用以下命令进行发布测试，测试结果表明仅保留消息没有发布权限：

```bash
# 指定 QoS0 时可以订阅成功
$ mqttx sub -u emqx_u -t t/3 -q 0
[2023-9-20] [18:49:00] › …  Connecting...
[2023-9-20] [18:49:00] › ✔  Connected
[2023-9-20] [18:49:00] › …  Subscribing to t/3...
[2023-9-20] [18:49:00] › ✔  Subscribed to t/3

# 指定 QoS1 时无法订阅主题
$ mqttx sub -u emqx_u -t t/3 -q 1
[2023-9-20] [18:49:45] › …  Connecting...
[2023-9-20] [18:49:45] › ✔  Connected
[2023-9-20] [18:49:45] › …  Subscribing to t/3...
[2023-9-20] [18:49:45] › ✔  Subscribed to t/3
[2023-9-20] [18:49:45] › ✖  Subscription negated to t/3 with code 135
```

## 配置项

详细配置请参考 [authz:mysql](../../configuration/configuration-manual.html#authz:mysql)。

MySQL authorizer 由 `type=mysql` 标识。

配置示例：

```hcl
{
  type = mysql
  enable = true

  database = "mqtt"
  username = "root"
  password = "public"
  server = "127.0.0.1:3306"
  query = "SELECT permission, action, topic FROM mqtt_acl WHERE username = ${username}"
}
```

### query

必选的字符串类型配置，用于查询当前客户端具有的权限列表，支持[占位符](./authz.md#数据查询占位符)。

出于安全原因占位符值不会直接拼接 SQL，而是通过 MySQL 预处理插入，能够有效预防 SQL 注入。

例如，以下查询语句：

```sql
SELECT permission, action, topic FROM mqtt_acl WHERE username = ${username}
```

将首先被转换为以下 Prepared statement：

```sql
SELECT permission, action, topic FROM mqtt_acl WHERE username = ?
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
