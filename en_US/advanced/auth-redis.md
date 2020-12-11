---
# 标题
title: Redis authentication
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

# Redis

Redis authentication uses an external Redis database as the authentication data source, which can store a large amount of data and facilitate integration with external device management systems.

Plugin:

```bash
emqx_auth_redis
```

::: tip 
The emqx_auth_redis lso includes ACL feature, which can be disabled via comments
:::




To enable Redis authentication, you need to configure the following in `etc/plugins/emqx_auth_redis.conf` ：

## Redis connection information

For Redis basic connection information, it needs to ensure that all nodes in the cluster can access.

```bash
# etc/plugins/emqx_auth_redis.conf

## Server address
auth.redis.server = 127.0.0.1:6379

## Connection pool size
auth.redis.pool = 8

auth.redis.database = 0

auth.redis.password = 
```

## Default table structure

A hash table is used to store authentication data by default for Redis authentication, and `mqtt_user:` is used as the Redis key prefix. The data structure is as follows:

```bash
redis> hgetall mqtt_user:emqx
  password public
  salt wivwiv
```

The sample data in the default configuration is as follows:

```bash
HMSET mqtt_user:emqx password public salt wivwiv
```

After Redis  authentication is enabled, you can connect with username: emqx, password: public.

::: tip 
This is the data structure used by default configuration. After being familiar with the use of the plugin, you can use any data structure that meets the conditions for authentication
:::


## Salting rules and hash methods

Redis authentication supports the configuration of [salting rules and hash methods](./auth.md#password-salting-rules-and-hash-methods), and plaintext passwords are stored without processing by default:

```bash
# etc/plugins/emqx_auth_redis.conf

auth.redis.password_hash = plain
```


## auth query cmd

During authentication, EMQ X Broker will use the current client information to populate and execute the user-configured authentication query command to query the client's authentication data in the Redis.

```bash
# etc/plugins/emqx_auth_redis.conf

auth.redis.auth_cmd = HMGET mqtt_user:%u password
```

You can use the following placeholders in the command, and EMQ X Broker will be automatically populated with client information when executed:

- %u：Username
- %c：Client ID
- %C：TLS certificate common name (the domain name or subdomain name of the certificate), valid only for TLS connections
- %d：TLS certificate subject, valid only for TLS connections

You can adjust the authentication query command according to your business needs and use any  [Redis supported command](http://redisdoc.com/index.html). However, in any case, the authentication query command must meet the following conditions:

1. The first data in the query result must be password. EMQ X Broker will use this field to compare with the client password.
2. If the salting configuration is enabled, the second data in the query result must be the salt field. EMQ X Broker will use this field as the salt value.



