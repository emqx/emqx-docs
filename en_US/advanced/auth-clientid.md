---
# 标题
title: Client ID 认证
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

# Client ID

Client ID authentication uses a configuration file to preset the client Client ID and password, and supports authentication data management through the HTTP API.

Client ID authentication does not rely on external data sources, which is simple and lightweight to use.

Plugin:

```bash
emqx_auth_clientid
```



## Hash method

Client ID authentication uses sha256 for password hash encryption by default, which can be changed in `etc/plugins/emqx_auth_clientid.conf`:

```bash
# etc/plugins/emqx_auth_clientid.conf

## Value: plain | md5 | sha | sha256 
auth.client.password_hash = sha256
```

After configuring the [hash method](./auth.md#password-salting-rules-and-hash-methods), the new preset authentication data and authentication data added through the HTTP API will be stored in the built-in database of EMQ X Broker in hash cipher text.



## Preset authentication data

The configuration file can be used to preset the authentication data and can be edit in:`etc/plugins/emqx_auth_clientid.conf`

```bash
# etc/plugins/emqx_auth_clientid.conf

## The first group of authentication data
auth.client.1.clientid = admin_client
auth.client.1.password = public

## The second group of authentication data
auth.clientid.2.clientid = wivwiv_client
auth.clientid.2.password = public
```

When the plugin is started, the preset authentication data is read and loaded into the built-in database of EMQ X Broker, and the authentication data on the nodes will be synchronized to the cluster at this stage.

<!-- TODO 补充加载规则 -->

::: danger 

The preset authentication data uses a clear text password in the configuration file, which should be avoided for security and maintainability considerations.

The preset authentication data cannot be modified or deleted through the API, please use it with caution.

:::



## Manage authentication data with HTTP API

#### Add authentication data

API definition:

```bash
# Request
POST api/v4/auth_clientid
{
    "clientid": "emqx_c",
    "password": "emqx_p"
}

# Response
{
    "code": 0
}
```

The POST request is used to add authentication information with clientid of `emqx_c` and password of ` emqx_p`, and  it will succeed if `code = 0` is returned.



#### Check the added authentication data

API definition:

```bash
# Request
GET api/v4/auth_clientid

# Response
{
    "code": 0,
    "data": ["emqx_c"]
}
```



#### Change the password of the specified Client ID

After specifying the Client ID and passing the new password to change it, it need to use the new password when connecting again:

API definition:

```bash
# Request
PUT api/v4/auth_clientid/${clientid}
{
    "password": "emqx_new_p"
}

# Response
{
    "code": 0
}
```



#### Check the specified Client ID information

Specify the Client ID and check the related Client ID and password information. Note that the password returned here is the password encrypted using the hash method specified by the configuration file:

API definition:

```bash
# Request
GET api/v4/auth_clientid/${clientid}

# Response
{
    "code": 0,
    "data": {
        "clientid": "emqx_c",
        "password": "091dc8753347e7dc5d348508fe6323735eecdb84fa800548870158117af8a0c0"
    }
}
```




#### Delete authentication data

Delete the specified Client ID:

API definition:

```bash
# Request
DELETE api/v4/auth_clientid/${clientid}

# Response
{
    "code": 0
}
```
