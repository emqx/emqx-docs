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

# MySQL ACL

MySQL ACL 使用外部 MySQL 数据库存储 ACL 规则，可以存储大量数据、动态管理 ACL，方便与外部设备管理系统集成

插件：

```bash
emqx_auth_mysql
```

::: tip 
emqx_auth_mysql 插件同时包含认证功能，可通过注释禁用。
:::


## MySQL 连接信息

MySQL 基础连接信息，需要保证集群内所有节点均能访问。

```bash
# etc/plugins/emqx_auth_mysql.conf

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

MySQL 认证插件默认配置下需要确保数据库中有以下两张数据表，用于存储认证规则信息：

### 认证/超级用户表

```sql
CREATE TABLE `mqtt_user` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(100) DEFAULT NULL,
  `password` varchar(100) DEFAULT NULL,
  `salt` varchar(35) DEFAULT NULL,
  `is_superuser` (1) DEFAULT 0,
  `created` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `mqtt_username` (`username`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
```

示例数据：

```sql
-- 客户端信息
INSERT INTO `mqtt_user` ( `username`, `password`, `salt`, `is_superuser`)
VALUES
	('emqx', 'efa1f375d76194fa51a3556a97e641e61685f914d446979da50a551a4333ffd7', NULL, 0);
```

### ACL 规则表

```sql
CREATE TABLE `mqtt_acl` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `allow` int(1) DEFAULT 1 COMMENT '0: deny, 1: allow',
  `ipaddr` varchar(60) DEFAULT NULL COMMENT 'IpAddress',
  `username` varchar(100) DEFAULT NULL COMMENT 'Username',
  `clientid` varchar(100) DEFAULT NULL COMMENT 'ClientId',
  `access` int(2) NOT NULL COMMENT '1: subscribe, 2: publish, 3: pubsub',
  `topic` varchar(100) NOT NULL DEFAULT '' COMMENT 'Topic Filter',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
```

规则表字段说明：

- allow：禁止（0），允许（1）
- ipaddr：设置 IP 地址
- username：连接客户端的用户名，此处的值如果设置为 `$all`  表示该规则适用于所有的用户
- clientid：连接客户端的 Client ID
- access：允许的操作：订阅（1），发布（2），订阅发布都可以（3）
- topic：控制的主题，可以使用通配符，并且可以在主题中加入占位符来匹配客户端信息，例如 `t/%c` 则在匹配时主题将会替换为当前客户端的 Client ID
  - %u：用户名
  - %c：Client ID
  


默认配置下示例数据：

```sql
-- 所有用户不可以订阅系统主题
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (0, NULL, '$all', NULL, 1, '$SYS/#');

-- 允许 10.59.1.100 上的客户端订阅系统主题
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (1, '10.59.1.100', NULL, NULL, 1, '$SYS/#');

-- 禁止客户端订阅 /smarthome/+/temperature 主题
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (0, NULL, NULL, NULL, 1, '/smarthome/+/temperature');

-- 允许客户端订阅包含自身 Client ID 的 /smarthome/${clientid}/temperature 主题
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (1, NULL, NULL, NULL, 1, '/smarthome/%c/temperature');
```

启用 MySQL ACL 后并以用户名 emqx 成功连接后，客户端应当数据具有相应的主题权限。


::: tip 
这是默认配置使用的表结构，熟悉该插件的使用后你可以使用任何满足条件的数据表进行 ACL 规则存储。
:::



## 超级用户 SQL（super_query）

进行 ACL 鉴权时，EMQ X 将使用当前客户端信息填充并执行用户配置的超级用户 SQL，查询客户端是否为超级用户。客户端为超级用户时将跳过 ACL SQL。

```bash
# etc/plugins/emqx_auth_mysql.conf

auth.mysql.super_query = select is_superuser from mqtt_user where username = '%u' limit 1
```

你可以在 SQL 中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名
- %c：Client ID
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效


你可以根据业务需要调整超级用户 SQL，如添加多个查询条件、使用数据库预处理函数，以实现更多业务相关的功能。但是任何情况下超级用户 SQL 需要满足以下条件：

1. 查询结果中必须包含 is_superuser 字段，is_superuser 应该显式的为 true
2. 查询结果只能有一条，多条结果时只取第一条作为有效数据

::: tip 
如果不需要超级用户功能，注释并禁用该选项能有效提高效率
:::


## ACL SQL（acl_query）

进行 ACL 鉴权时，EMQ X 将使用当前客户端信息填充并执行用户配置的超级用户 SQL，如果没有启用超级用户 SQL 或客户端不是超级用户，则使用 ACL SQL 查询出该客户端在数据库中的 ACL 规则。

```bash
# etc/plugins/emqx_auth_mysql.conf

auth.mysql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'
```

你可以在 ACL SQL 中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名
- %c：Client ID
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效


你可以根据业务需要调整 ACL SQL，如添加多个查询条件、使用数据库预处理函数，以实现更多业务相关的功能。但是任何情况下 ACL SQL 需要满足以下条件：

1. 查询结果中必须包含 allow、access、topic、clientid、username、ipaddr 字段，如果字段不想参与比对则使用 `$all` 字符串或者数据库 `NULL` 值
2. 查询结果可以有多条，多条结果时按照从上到下的顺序进行匹配

::: tip 
可以在 SQL 中调整查询条件、指定排序方式实现更高效率的查询。
:::


## 特殊说明

MySQL 8.0 及以后版本使用了 `caching_sha2_password` 作为默认身份验证插件，受限于客户端驱动你必须将其更改为 `mysql_native_password` 插件：

```sql
ALTER USER 'your_username'@'your_host' IDENTIFIED WITH mysql_native_password BY 'your_password';
```
