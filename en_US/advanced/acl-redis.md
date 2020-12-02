---
# 标题
title: Redis ACL
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

# Redis ACL

An external Redis database is used to store ACL rules for Redis ACL, which can store a large amount of data and dynamically manage ACLs for easy integration with external device management systems.

Plugin:

```bash
emqx_auth_redis
```

::: tip 
The emqx_auth_mysql plugin also includes authentication feature, which can be disabled via comments.
:::


## Redis connection information

Redis basic connection information needs to be accessible to all nodes in the cluster.

```bash
# etc/plugins/emqx_auth_redis.conf

## Server address
auth.redis.server = 127.0.0.1:6379

## Connection pool size
auth.redis.pool = 8

auth.redis.database = 0

auth.redis.password = 
```


## Default data structure

Under the default configuration of the Redis  authentication plugin, a hash table is used to store authentication data, and `mqtt_user:` is used as the Redis key prefix. The data structure is as follows:

### Authentication / Superuser

```bash
redis> hgetall mqtt_user:emqx
  password public
  salt wivwiv
```

Under the default configuration, Sample data is as follows:

```bash
HMSET mqtt_user:emqx password public salt wivwiv
```

### ACL rule table

```bash
## Format
HSET mqtt_acl:[username clientid] [topic] [access]

## Structure
redis> hgetall mqtt_acl:emqx
  testtopic/1 1
```

A rule of Redis ACL defines publish, subscribe, or publish/subscribe information. All lists in the rule are **allow** lists.

Rule field description:

- ipaddr: Set IP address
- username: User name for connecting to the client. If the value is set to `$ all`, the rule applies to all users.
- clientid: Client ID of the connected client
- access: Allowed operations: subscribe (1), publish (2), both subscribe and publish (3)
- topic: Topics to be controlled, which can use wildcards, and placeholders can be added to the topic to match client information. For example, the topic will be replaced with the client ID of the current client when matching `t/%c`
  - %u：Username
  - %c：Client ID
  

Under the default configuration, Sample data is as follows:

```bash
HSET mqtt_acl:emqx # 1
HSET mqtt_acl:testtopic/2 2
```

After enabling Redis ACL and successfully connecting with the username emqx, the client should have permissions on the topics it wants to subscribe/publish.



## Super user query command（super cmd）

When performing ACL authentication, EMQ X Broker will use the current client information to execute the user-configured superuser query command to query whether the client is a superuser. ACL query command is skipped when the client is superuser.

```bash
# etc/plugins/emqx_auth_redis.conf

auth.redis.super_cmd = HGET mqtt_user:%u is_superuser
```

You can use the following placeholders in query command and EMQ X Broker will automatically populate with client information when executed:

- %u：Username
- %c：Client ID
- %C：TLS certificate common name (the domain name or subdomain name of the certificate), valid only for TLS connections
- %d：TLS certificate subject, valid only for TLS connections

You can adjust the super user query command according to business to achieve more business-related functions, such as adding multiple query conditions and using database preprocessing functions. However, in any case, the superuser query command needs to meet the following conditions:

1. The first data in the query results must be the is_superuser data

::: tip 
If superuser functionality is not needed, it can be more efficient when commenting and disabling this option .
:::


## ACL query command（acl cmd）

When performing ACL authentication, EMQ X Broker will use the current client information to populate and execute the user-configured superuser SQL. If superuser SQL is not enabled or the client is not a superuser, ACL SQL is used to query the client's ACL rules in the database.

```bash
# etc/plugins/emqx_auth_redis.conf

auth.redis.acl_cmd = HGETALL mqtt_acl:%u
```

You can use the following placeholders in ACL SQL and EMQ X Broker will automatically populate with client information when executed:

- %u：Username
- %c：Client ID

You can adjust the ACL query command according to business requirement. However, in any case, the ACL query command  needs to meet the following conditions:

1. Topic is used as key and access is used as value in hash

::: danger 
The above data structures   need to be used strictly for Redis ACL rules.All rules added in Redis ACL are `allow` rules, which can be used with `acl_nomatch = deny` in` etc / emqx.conf`. :::