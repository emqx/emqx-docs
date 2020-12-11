---
# 标题
title: Username authentication
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

# Username

Username authentication uses a configuration file to preset client user names and passwords, and supports authentication data management via HTTP API.

Username authentication does not rely on external data sources, and is simple and lightweight to use.

Plugin:

```bash
emqx_auth_username
```



## Hash method

For username authentication, sha256 is used for password hash encryption by default, which can be changed in `etc/plugins/emqx_auth_username.conf`:

```bash
# etc/plugins/emqx_auth_username.conf

## Value: plain | md5 | sha | sha256 
auth.user.password_hash = sha256
```

After configuring the [hash method](./auth.md#password-salting-rules-and-hash-methods), the new preset authentication data and authentication data added through the HTTP API will be stored in the EMQ X Broker built-in database in hash cipher text. 


## Preset authentication data

You can preset authentication data through configuration files and edit configuration files：`etc/plugins/emqx_auth_username.conf`

```bash
# etc/plugins/emqx_auth_username.conf

## The first set of authentication data
auth.user.1.username = admin
auth.user.1.password = public

## The second set of authentication data
auth.user.2.username = wivwiv
auth.user.2.password = public
```

When the plugin is started, the preset authentication data is read and loaded into the EMQ X Broker built-in database, and the authentication data on the nodes will be synchronized to the cluster at this stage.

<!-- TODO 补充加载规则 -->

::: danger 

The preset authentication data uses a clear text password in the configuration file. This function should be disabled for security and maintainability considerations.

The preset authentication data cannot be modified or deleted through the API, please use it with caution.

:::



## Manage authentication data using HTTP API

#### Add authentication data

API definition:

```bash
# Request
POST api/v4/auth_username
{
    "username": "emqx_u",
    "password": "emqx_p"
}

# Response
{
    "code": 0
}
```

A POST request is used to add authentication information with username of `emqx_u` and password of ` emqx_p`, and it will succeed if  `code = 0` is returned.



#### Check the added authentication data

API definition:

```bash
# Request
GET api/v4/auth_username

# Response
{
    "code": 0,
    "data": ["emqx_u"]
}
```



#### Change the password of the specified user name

After specifying the user name and passing the new password to change it, it needs to use the new password when connecting again:

API definition:

```bash
# Request
PUT api/v4/auth_username/${username}
{
    "password": "emqx_new_p"
}

# Response
{
    "code": 0
}
```



#### Check the specified username information

Specify the user name and check the related user name and password information. Note that the password returned here is the password encrypted with the hash method specified by the configuration file:

API definition:

```bash
# Request
GET api/v4/auth_username/${username}

# Response
{
    "code": 0,
    "data": {
        "username": "emqx_u",
        "password": "091dc8753347e7dc5d348508fe6323735eecdb84fa800548870158117af8a0c0"
    }
}
```




#### Delete authentication data

Delete the specified authentication data:

API definition:

```bash
# Request
DELETE api/v4/auth_username/${username}

# Response
{
    "code": 0
}
```


<!-- TODO: 引用 HTTP API -->
