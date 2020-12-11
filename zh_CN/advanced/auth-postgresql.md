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

# PostgreSQL 认证

PostgreSQL 认证使用外部 PostgreSQL 数据库作为认证数据源，可以存储大量数据，同时方便与外部设备管理系统集成。

插件：

```bash
emqx_auth_pgsql
```

::: tip 
emqx_auth_pgsql 插件同时包含 ACL 功能，可通过注释禁用。
:::



要启用 PostgreSQL 认证，需要在 `etc/plugins/emqx_auth_pgsql.conf` 中配置以下内容：

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

PostgreSQL 认证默认配置下需要确保数据库中有下表：

```sql
CREATE TABLE mqtt_user (
  id SERIAL primary key,
  is_superuser boolean,
  username character varying(100),
  password character varying(100),
  salt character varying(40)
)
```



默认配置下示例数据如下：

```sql
INSERT INTO mqtt_user (username, password, salt, is_superuser)
VALUES
	('emqx', 'efa1f375d76194fa51a3556a97e641e61685f914d446979da50a551a4333ffd7', NULL, false);
```

启用 PostgreSQL 认证后，你可以通过用户名： emqx，密码：public 连接。



::: tip 
这是默认配置使用的表结构，熟悉该插件的使用后你可以使用任何满足条件的数据表进行认证。
:::



## 加盐规则与哈希方法

PostgreSQL 认证支持配置[加盐规则与哈希方法](./auth.md#加盐规则与哈希方法)：

```bash
# etc/plugins/emqx_auth_pgsql.conf

auth.pgsql.password_hash = sha256
```



## 认证 SQL（auth_query）

进行身份认证时，EMQ X 将使用当前客户端信息填充并执行用户配置的认证 SQL，查询出该客户端在数据库中的认证数据。

```bash
# etc/plugins/emqx_auth_pgsql.conf

auth.pgsql.auth_query = select password from mqtt_user where username = '%u' limit 1
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

