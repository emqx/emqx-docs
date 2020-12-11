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

# PostgreSQL ACL

PostgreSQL ACL 使用外部 PostgreSQL 数据库存储 ACL 规则，可以存储大量数据、动态管理 ACL，方便与外部设备管理系统集成。

插件：

```bash
emqx_auth_pgsql
```

::: tip 
emqx_auth_pgsql 插件同时包含认证功能，可通过注释禁用。
:::


## PostgreSQL 连接信息

PostgreSQL 基础连接信息，需要保证集群内所有节点均能访问。

```bash
# etc/plugins/emqx_auth_pgsql.conf

## 服务器地址
auth.pgsql.server = 127.0.0.1:5432

## 连接池大小
auth.pgsql.pool = 8

auth.pgsql.username = root

auth.pgsql.password = public

auth.pgsql.database = mqtt

auth.pgsql.encoding = utf8

## TLS 配置
## auth.pgsql.ssl = false
## auth.pgsql.ssl_opts.keyfile =
## auth.pgsql.ssl_opts.certfile =
```


## 默认表结构

PostgreSQL 认证插件默认配置下需要确保数据库中有以下两张数据表，用于存储认证规则信息：

### 认证/超级用户表

```sql
CREATE TABLE mqtt_user (
  id SERIAL primary key,
  is_superuser boolean,
  username character varying(100),
  password character varying(100),
  salt character varying(40)
)
```

示例数据：

```sql
-- 客户端信息
INSERT INTO mqtt_user (username, password, salt, is_superuser)
VALUES
	('emqx', 'efa1f375d76194fa51a3556a97e641e61685f914d446979da50a551a4333ffd7', NULL, false);
```

### ACL 规则表

```sql
CREATE TABLE mqtt_acl (
  id SERIAL primary key,
  allow integer,
  ipaddr character varying(60),
  username character varying(100),
  clientid character varying(100),
  access  integer,
  topic character varying(100)
)
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

启用 PostgreSQL ACL 后并以用户名 emqx 成功连接后，客户端应当数据具有相应的主题权限。


::: tip 
这是默认配置使用的表结构，熟悉该插件的使用后你可以使用任何满足条件的数据表进行 ACL 规则存储。
:::



## 超级用户 SQL（super_query）

进行 ACL 鉴权时，EMQ X 将使用当前客户端信息填充并执行用户配置的超级用户 SQL，查询客户端是否为超级用户。客户端为超级用户时将跳过 ACL SQL。

```bash
# etc/plugins/emqx_auth_pgsql.conf

auth.pgsql.super_query = select is_superuser from mqtt_user where username = '%u' limit 1
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
# etc/plugins/emqx_auth_pgsql.conf

auth.pgsql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'
```

你可以在 ACL SQL 中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %a：客户端地址
- %u：用户名
- %c：Client ID


你可以根据业务需要调整 ACL SQL，如添加多个查询条件、使用数据库预处理函数，以实现更多业务相关的功能。但是任何情况下 ACL SQL 需要满足以下条件：

1. 查询结果中必须包含 allow、access、topic、clientid、username、ipaddr 字段，如果字段不想参与比对则使用 `$all` 字符串或者数据库 `NULL` 值
2. 查询结果可以有多条，多条结果时按照从上到下的顺序进行匹配

::: tip 
可以在 SQL 中调整查询条件、指定排序方式实现更高效率的查询。
:::

