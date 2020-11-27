---
# 标题
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

# Mnesia  Authentication 

Mnesia authentication uses the built-in Mnesia database of EMQ X to store client Client ID/Username and password, and supports management of authentication data through HTTP API.

Mnesia authentication does not depend on external data sources, and it is simple and lightweight to use.

Plugin:

```bash
emqx_auth_mnesia
```

## Authentication rules

Mnesia authentication uses the username and password based on CONNECT packet for authentication by default, and can be changed to use the Client ID and password of CONNECT packet for authentication in  `etc/plugins/emqx_auth_mnesia.conf` :

```bash
## Auth as username or auth as clientid.
##
## Value: username | clientid
auth.mnesia.as = username
```

## Hash method

Mnesia authentication uses sha256 for password hash encryption by default, which can be changed in `etc/plugins/emqx_auth_mnesia.conf`:

```bash
# etc/plugins/emqx_auth_mnesia.conf

## Value: plain | md5 | sha | sha256 
auth.mnesia.password_hash = sha256
```

After configuring [Hash Method](./auth.md#加盐规则与哈希方法), the newly added preset authentication data and authentication data added through the HTTP API will be stored in the EMQ X built-in database in the format of hash ciphertext.



## Preset authentication data

You can preset authentication data through the configuration file and edit the configuration file: `etc/plugins/emqx_auth_mnesia.conf`

```bash
# etc/plugins/emqx_auth_mnesia.conf

## The first group of authentication data
auth.mnesia.1.login = admin
auth.mnesia.1.password = public
auth.mnesia.1.is_superuser = true

## The second group of authentication data
auth.mnesia.2.login = client
auth.mnesia.2.password = public
auth.mnesia.2.is_superuser = false
```

In the authentication data, `login` will read the client's Username or Client ID based on the value of `auth.mnesia.as`.

When the plugin starts, it will read the preset authentication data and load it into the EMQ X built-in database, and the authentication data on the node will be synchronized to the cluster at this stage.

<!-- TODO 补充加载规则 -->

::: danger 

The preset authentication data uses a clear text password in the configuration file. For security and maintainability, this function should be avoided.

The preset authentication data cannot be modified or deleted through the API, please use it with caution.
:::

## Use the HTTP API to manage authentication data

#### Add authentication data

API definition:

```bash
# Request
POST api/v4/auth_user
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

Use POST request to add login as `emqx_c`, password as `emqx_p`, which is an non-super user authentication information, and  “code = 0` in the returned message means success.

#### Add authentication data in batch

API definition:

```bash
# Request
POST api/v4/auth_user
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

#### Check the added authentication data

API definition:

```bash
# Request
GET api/v4/auth_user

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

#### Change the added authentication data

API definition:

```bash
# Request
PUT api/v4/auth_user/${login}
{
    "password": "emqx_new_p",
    "is_superuser": false
}

# Response
{
    "code": 0
}
```

#### Check the specified authentication data

Note that the password returned here is the password encrypted using the hash method specified in the configuration file:

API definition:

```bash
# Request
GET api/v4/auth_user/${login}

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

#### Delete the authentication data

Delete the specified authentication data:

API definition:

```bash
# Request
DELETE api/v4/auth_user/${login}

# Response
{
    "code": 0
}
```
