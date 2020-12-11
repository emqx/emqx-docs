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
ref: undefined
---

# PostgreSQL

PostgreSQL authentication uses an external PostgreSQL database as the authentication data source, which can store a large amount of data and facilitate integration with external device management systems.

Plugin:

```bash
emqx_auth_pgsql
```

::: tip 
The emqx_auth_pgsql also includes ACL feature, which can be disabled via comments
:::



To enable PostgreSQL authentication, you need to configure the following in `etc/plugins/emqx_auth_pgsql.conf` 

## PostgreSQL connection information

For PostgreSQL basic connection information, it needs to ensure that all nodes in the cluster can access.

```bash
# etc/plugins/emqx_auth_pgsql.conf

## Server address
auth.pgsql.server = 127.0.0.1:5432

## Connection pool size
auth.pgsql.pool = 8

auth.pgsql.username = root

auth.pgsql.password = public

auth.pgsql.database = mqtt

auth.pgsql.encoding = utf8

## TLS configuration
## auth.pgsql.ssl = false
## auth.pgsql.ssl_opts.keyfile =
## auth.pgsql.ssl_opts.certfile =
```



## Default table structure

In the default configuration of PostgreSQL authentication, you need to ensure that the following table is in the database:

```sql
CREATE TABLE mqtt_user (
  id SERIAL primary key,
  is_superuser boolean,
  username character varying(100),
  password character varying(100),
  salt character varying(40)
)
```



The sample data in the default configuration is as follows:

```sql
INSERT INTO mqtt_user (username, password, salt, is_superuser)
VALUES
	('emqx', 'efa1f375d76194fa51a3556a97e641e61685f914d446979da50a551a4333ffd7', NULL, false);
```

After PostgreSQL authentication is enabled, you can connect with username: emqx, password: public.



::: tip 
This is the table structure used by default configuration. After being familiar with the use of the plugin, you can use any data table that meets the conditions for authentication
:::



## Salting rules and hash methods

PostgreSQL authentication support to configure [Salting rules and hash methods](./auth.md#password-salting-rules-and-hash-methods)：

```bash
# etc/plugins/emqx_auth_pgsql.conf

auth.pgsql.password_hash = sha256
```



## auth_query

During authentication, EMQ X Broker will use the current client information to populate and execute the user-configured authentication SQL to query the client's authentication data in the database.

```bash
# etc/plugins/emqx_auth_pgsql.conf

auth.pgsql.auth_query = select password from mqtt_user where username = '%u' limit 1
```



You can use the following placeholders in the SQL authentication, and EMQ X Broker will be automatically populated with client information when executed:

- %u：Username
- %c：Client ID
- %C：TLS certificate common name (the domain name or subdomain name of the certificate), valid only for TLS connections
- %d：TLS certificate subject, valid only for TLS connections



You can adjust the authentication SQL according to business to achieve more business-related functions, such as adding multiple query conditions and using database preprocessing functions. However, in any case, the authentication  must meet the following conditions:

1. The query result must include the password field, which is used by EMQ X Broker to compare with the client password
2. If the salting configuration is enabled, the query result must include the salt field, which is used by EMQ X Broker as the salt value
3. There can only be one query result. When there are multiple results, only the first one is taken as valid data.

::: tip 
You can use AS syntax in SQL to specify passwords for field renaming, or set the salt value to a fixed value.
:::

