---
mqtt_user# 标题
title: Mnesia 认证
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

# Mnesia 认证

Mnesia 认证使用 EMQ X 内置 Mnesia 数据库存储客户端 Client ID/Username 与密码，支持通过 HTTP API 管理认证数据。

Mnesia 认证不依赖外部数据源，使用上足够简单轻量。

插件：

```bash
emqx_auth_mnesia
```

## 认证规则

Mnesia 认证默认基于 CONNECT 报文中的 Username 和密码进行认证, 可在 `etc/plugins/emqx_auth_mnesia.conf` 中更改为使用 CONNECT 报文的的 Client ID 与密码认证：

```bash
## Auth as username or auth as clientid.
##
## Value: username | clientid
auth.mnesia.as = username
```

## 哈希方法

Mnesia 认证默认使用 sha256 进行密码哈希加密，可在 `etc/plugins/emqx_auth_mnesia.conf` 中更改：

```bash
# etc/plugins/emqx_auth_mnesia.conf

## Value: plain | md5 | sha | sha256 
auth.mnesia.password_hash = sha256
```

配置[哈希方法](./auth.md#加盐规则与哈希方法)后，新增的预设认证数据与通过 HTTP API 添加的认证数据将以哈希密文存储在 EMQ X 内置数据库中。

## 预设认证数据

可以通过配置文件预设认证数据，编辑配置文件：`etc/plugins/emqx_auth_mnesia.conf`

```bash
# etc/plugins/emqx_auth_mnesia.conf

## 第一组认证数据
auth.mnesia.1.login = admin
auth.mnesia.1.password = public
auth.mnesia.1.is_superuser = true

## 第二组认证数据
auth.mnesia.2.login = client
auth.mnesia.2.password = public
auth.mnesia.2.is_superuser = false
```

认证数据中的 `login` 会根据 `auth.mnesia.as` 的值去读取客户端的 Username 或 Client ID.

插件启动时将读取预设认证数据并加载到 EMQ X 内置数据库中，节点上的认证数据会在此阶段同步至集群中。

<!-- TODO 补充加载规则 -->

{% hint style="danger" %} 

预设认证数据在配置文件中使用了明文密码，出于安全性与可维护性考虑应当避免使用该功能。

预设认证数据无法通过 API 修改、删除，请慎用。
{% endhint %}

## 使用 HTTP API 管理认证数据

#### 添加认证数据

API 定义：

```bash
# Request
POST api/v4/mqtt_user
{
    "login": "emqx_c",
    "password": "emqx_p",
    "is_superuser": false
}

# Response
{
    "data": {
        "emqx_c": "ok"
    },
    "code": 0
}
```

使用 POST 请求添加 login 为 `emqx_c`，password 为 `emqx_p`，非超级用户的认证信息，返回信息中 `code = 0` 即为成功。

#### 批量添加认证数据

API 定义：

```bash
# Request
POST api/v4/mqtt_user
[
    {
    "login": "emqx_c_1",
    "password": "emqx_p",
    "is_superuser": false
    },
    {
        "login": "emqx_c_2",
        "password": "emqx_p",
        "is_superuser": false
    }
]

# Response
{
    "data": {
        "emqx_c_2": "ok",
        "emqx_c_1": "ok"
    },
    "code": 0
}
```

#### 查看已经添加的认证数据

API 定义：

```bash
# Request
GET api/v4/mqtt_user

# Response
{
  "meta": {
    "page": 1,
    "limit": 10,
    "count": 1
  },
  "data": [
    {
      "password": "ceb5e917f7930ae8f0dc3ceb496a428f7e644736eebca36a2b8f6bbac756171a",
      "login": "emqx_c",
      "is_superuser": false
    }
  ],
  "code": 0
}
```

#### 更改已添加的认证数据

API 定义：

```bash
# Request
PUT api/v4/mqtt_user/${login}
{
    "password": "emqx_new_p",
    "is_superuser": false
}

# Response
{
    "code": 0
}
```

#### 查看指定的认证数据

注意此处返回的密码是使用配置文件指定哈希方式加密后的密码：

API 定义：

```bash
# Request
GET api/v4/mqtt_user/${login}

# Response
{
    "data": {
        "password": "3b20ff0218af39d01252844ccaac8ce0160f969ad00c601e23f6e57cd26fad4e",
        "login": "emqx_c",
        "is_superuser": false
    },
    "code": 0
}
```

#### 删除认证数据

删除指定认证数据：

API 定义：

```bash
# Request
DELETE api/v4/mqtt_user/${login}

# Response
{
    "code": 0
}
```
