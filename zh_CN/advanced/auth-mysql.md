---
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---

# MySQL 认证

MySQL 认证使用外部 MySQL 数据库作为认证数据源，可以存储大量数据，同时方便与外部设备管理系统集成。

插件：

```bash
emqx_auth_mysql
```

::: tip 
emqx_auth_mysql 插件同时包含 ACL 功能，可通过注释禁用。
:::


要启用 MySQL 认证，需要在 `etc/plugins/emqx_auth_mysql.conf` 中配置以下内容：

## MySQL 连接信息

配置 MySQL 服务相关的连接地址，用户名密码和数据库：

```bash
## 服务器地址
auth.mysql.server = 127.0.0.1:3306

## 连接池大小
auth.mysql.pool = 8

auth.mysql.username = emqx

auth.mysql.password = public

auth.mysql.database = mqtt

auth.mysql.query_timeout = 5s
```


## 默认表结构

MySQL 认证默认配置下需要确保数据库中有下表：

```sql
CREATE TABLE `mqtt_user` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(100) DEFAULT NULL,
  `password` varchar(100) DEFAULT NULL,
  `salt` varchar(35) DEFAULT NULL,
  `is_superuser` tinyint(1) DEFAULT 0,
  `created` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `mqtt_username` (`username`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
```



默认配置下示例数据如下：

```sql
INSERT INTO `mqtt_user` ( `username`, `password`, `salt`)
VALUES
	('emqx', 'efa1f375d76194fa51a3556a97e641e61685f914d446979da50a551a4333ffd7', NULL);
```

启用 MySQL 认证后，你可以通过用户名： emqx，密码：public 连接。



::: tip 
这是默认配置使用的表结构，熟悉该插件的使用后你可以使用任何满足条件的数据表进行认证。
:::



## 加盐规则与哈希方法

MySQL 认证支持配置[加盐规则与哈希方法](./auth.md#加盐规则与哈希方法)：

```bash
# etc/plugins/emqx_auth_mysql.conf

auth.mysql.password_hash = sha256
```


## 认证 SQL（auth_query）

进行身份认证时，EMQ X 将使用当前客户端信息填充并执行用户配置的认证 SQL，查询出该客户端在数据库中的认证数据。

```bash
# etc/plugins/emqx_auth_mysql.conf

auth.mysql.auth_query = select password from mqtt_user where username = '%u' limit 1
```



你可以在认证 SQL 中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名
- %c：Client ID
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效



你可以根据业务需要调整认证 SQL，如添加多个查询条件、使用数据库预处理函数，以实现更多业务相关的功能。但是任何情况下认证 SQL 需要满足以下条件：

1. 查询结果中必须包含 password 字段，EMQ X 使用该字段与客户端密码比对
2. 如果启用了加盐配置，查询结果中必须包含 salt 字段，EMQ X 使用该字段作为 salt（盐）值
3. 查询结果只能有一条，多条结果时只取第一条作为有效数据

::: tip 
可以在 SQL 中使用 AS 语法为字段重命名指定 password，或者将 salt 值设为固定值。
:::


## 特殊说明

MySQL 8.0 及以后版本使用了 `caching_sha2_password` 作为默认身份验证插件，受限于客户端驱动你必须将其更改为 `mysql_native_password` 插件：

```sql
ALTER USER 'your_username'@'your_host' IDENTIFIED WITH mysql_native_password BY 'your_password';
```
