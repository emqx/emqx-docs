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

# PostgreSQL ACL

An external PostgreSQL database is used to store ACL rules for PostgreSQL ACL, which can store a large amount of data and dynamically manage ACLs for easy integration with external device management systems.

Plugin:

```bash
emqx_auth_pgsql
```

::: tip 
The emqx_auth_mysql plugin also includes authentication feature, which can be disabled via comments.
:::


## PostgreSQL Connection information

PostgreSQL basic connection information needs to be accessible to all nodes in the cluster.

```bash
# etc/plugins/emqx_auth_pgsql.conf

## server address
auth.pgsql.server = 127.0.0.1:5432

## Connection pool size
auth.pgsql.pool = 8

auth.pgsql.username = root

auth.pgsql.password = public

auth.pgsql.database = mqtt

auth.pgsql.encoding = utf8

## TLS Configuration
## auth.pgsql.ssl = false
## auth.pgsql.ssl_opts.keyfile =
## auth.pgsql.ssl_opts.certfile =
```


## Default table structure

Under the default configuration of the PostgreSQL authentication plugin, you need to ensure that the database has the following two data tables for storing authentication rule information:

### Authentication / Superuser Table

```sql
CREATE TABLE mqtt_user (
  id SERIAL PRIMARY KEY,
  username CHARACTER VARYING(100),
  password CHARACTER VARYING(100),
  salt CHARACTER VARYING(40),
  is_superuser BOOLEAN,
  UNIQUE (username)
)
```

Sample data:

```sql
-- Client information
INSERT INTO mqtt_user (username, password, salt, is_superuser)
VALUES
	('emqx', 'efa1f375d76194fa51a3556a97e641e61685f914d446979da50a551a4333ffd7', NULL, false);
```

### ACL rule table

```sql
CREATE TABLE mqtt_acl (
  id SERIAL PRIMARY KEY,
  allow INTEGER,
  ipaddr CHARACTER VARYING(60),
  username CHARACTER VARYING(100),
  clientid CHARACTER VARYING(100),
  access  INTEGER,
  topic CHARACTER VARYING(100)
);
CREATE INDEX ipaddr ON mqtt_acl (ipaddr);
CREATE INDEX username ON mqtt_acl (username);
CREATE INDEX clientid ON mqtt_acl (clientid);
```

Rule table field description:

- allow: Deny（0），Allow（1）
- ipaddr: Set IP address
- username: User name for connecting to the client. If the value is set to `$ all`, the rule applies to all users.
- clientid: Client ID of the connected client
- access: Allowed operations: subscribe (1), publish (2), both subscribe and publish (3)
- topic: Topics to be controlled, which can use wildcards, and placeholders can be added to the topic to match client information. For example, the topic will be replaced with the client ID of the current client when matching `t/%c`
  - %u：Username
  - %c：Client ID



Sample data in the default configuration:

```sql
-- All users cannot subscribe to system topics
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (0, NULL, '$all', NULL, 1, '$SYS/#');

-- Allow clients on 10.59.1.100 to subscribe to system topics
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (1, '10.59.1.100', NULL, NULL, 1, '$SYS/#');

-- Deny client to subscribe to the topic of /smarthome/+/temperature
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (0, NULL, '$all', NULL, 1, '/smarthome/+/temperature');

-- Allow clients to subscribe to the topic of /smarthome/${clientid}/temperature with their own Client ID
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (1, NULL, '$all', NULL, 1, '/smarthome/%c/temperature');
```

After enabling PostgreSQL ACL and successfully connecting with the username emqx, the client should have permissions on the topics it wants to subscribe to/publish.

::: tip 
This is the table structure used by default configuration. After being familiar with the use of this plugin, you can use any data table that meets the conditions for ACL rule storage.
:::



## Superuser  SQL（super_query）

When performing ACL authentication, EMQX Broker will use the current client information to execute the user-configured superuser SQL to query whether the client is a superuser. ACL SQL is skipped when the client is superuser.

```bash
# etc/plugins/emqx_auth_pgsql.conf

auth.pgsql.super_query = select is_superuser from mqtt_user where username = '%u' limit 1
```

You can use the following placeholders in SQL and EMQX Broker will automatically populate with client information when executed:

- %u：Username
- %c：Client ID
- %C：TLS certificate common name (the domain name or subdomain name of the certificate), valid only for TLS connections
- %d：TLS certificate subject, valid only for TLS connections

You can adjust the super user SQL according to business to achieve more business-related functions, such as adding multiple query conditions and using database preprocessing functions. However, in any case, the superuser SQL needs to meet the following conditions:

1. The query result must include the is_superuser field, which should be explicitly true
2. There can be only one query result. When there are multiple results, only the first one is taken as valid data.

::: tip 
If superuser functionality is not needed, it can be more efficient when commenting and disabling this option 
:::


## ACL SQL（acl_query）

When performing ACL authentication, EMQX Broker will use the current client information to populate and execute the user-configured superuser SQL. If superuser SQL is not enabled or the client is not a superuser, ACL SQL is used to query the client's ACL rules in the database.

```bash
# etc/plugins/emqx_auth_pgsql.conf

auth.pgsql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'
```

You can use the following placeholders in ACL SQL and EMQX Broker will automatically populate with client information when executed:

- %a：Client address
- %u：Username
- %c：Client ID


You can adjust the ACL SQL according to business to achieve more business-related functions, such as adding multiple query conditions and using database preprocessing functions. However, in any case, the ACL SQL needs to meet the following conditions:

1. The query result must include the fields of allow, access, topic, clientid, username, ipaddr. If the fields is not involved in the comparison, the `$ all` string or the database` NULL` value should be used.
2. There can be multiple query results. When multiple results are matched, they are matched from top to bottom.

::: tip 
You can adjust query conditions and specify sorting methods in SQL to achieve more efficient queries.
:::

