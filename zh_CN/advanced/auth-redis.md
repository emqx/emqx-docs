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

# Redis 认证

Redis 认证使用外部 Redis 数据库作为认证数据源，可以存储大量数据，同时方便与外部设备管理系统集成。

插件：

```bash
emqx_auth_redis
```

::: tip 
emqx_auth_redis 插件同时包含 ACL 功能，可通过注释禁用。
:::




要启用 Redis 认证，需要在 `etc/plugins/emqx_auth_redis.conf` 中配置以下内容：

## Redis 连接信息

Redis 基础连接信息，需要保证集群内所有节点均能访问。

```bash
# etc/plugins/emqx_auth_redis.conf

## 服务器地址
auth.redis.server = 127.0.0.1:6379

## 连接池大小
auth.redis.pool = 8

auth.redis.database = 0

auth.redis.password = 
```

## 默认数据结构

Redis 认证默认配置下使用哈希表存储认证数据，使用 `mqtt_user:` 作为 Redis 键前缀，数据结构如下：

```bash
redis> hgetall mqtt_user:emqx
  password public
  salt wivwiv
```

默认配置下示例数据如下：

```bash
HMSET mqtt_user:emqx password public salt wivwiv
```

启用 Redis 认证后，你可以通过用户名： emqx，密码：public 连接。


::: tip 
这是默认配置使用的数据结构，熟悉该插件的使用后你可以使用任何满足条件的数据结构进行认证。
:::


## 加盐规则与哈希方法

Redis 认证支持配置[加盐规则与哈希方法](./auth.md#加盐规则与哈希方法)，默认存储明文密码不做处理：

```bash
# etc/plugins/emqx_auth_redis.conf

auth.redis.password_hash = plain
```


## 认证查询命令（auth query cmd）

进行身份认证时，EMQ X 将使用当前客户端信息填充并执行用户配置的认证查询命令，查询出该客户端在 Redis 中的认证数据。

```bash
# etc/plugins/emqx_auth_redis.conf

auth.redis.auth_cmd = HMGET mqtt_user:%u password
```

你可以在命令中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名
- %c：Client ID
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效


你可以根据业务需要调整认证查询命令，使用任意 [Redis 支持的命令](http://redisdoc.com/index.html)，但是任何情况下认证查询命令需要满足以下条件：

1. 查询结果中第一个数据必须为 password，EMQ X 使用该字段与客户端密码比对
2. 如果启用了加盐配置，查询结果中第二个数据必须是 salt 字段，EMQ X 使用该字段作为 salt（盐）值



