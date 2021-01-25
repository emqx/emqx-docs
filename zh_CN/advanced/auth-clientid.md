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

# Client ID 认证

Client ID 认证使用配置文件预设客户端Client ID 与密码，支持通过 HTTP API 管理认证数据。

Client ID 认证不依赖外部数据源，使用上足够简单轻量。

插件：

```bash
emqx_auth_clientid
```



## 哈希方法

Client ID 认证默认使用 sha256 进行密码哈希加密，可在 `etc/plugins/emqx_auth_clientid.conf` 中更改：

```bash
# etc/plugins/emqx_auth_clientid.conf

## Value: plain | md5 | sha | sha256 
auth.client.password_hash = sha256
```

配置[哈希方法](./auth.md#加盐规则与哈希方法)后，新增的预设认证数据与通过 HTTP API 添加的认证数据将以哈希密文存储在 EMQ X 内置数据库中。



## 预设认证数据

可以通过配置文件预设认证数据，编辑配置文件：`etc/plugins/emqx_auth_clientid.conf`

```bash
# etc/plugins/emqx_auth_clientid.conf

## 第一组认证数据
auth.client.1.clientid = admin_client
auth.client.1.password = public

## 第二组认证数据
auth.clientid.2.clientid = wivwiv_client
auth.clientid.2.password = public
```

插件启动时将读取预设认证数据并加载到 EMQ X 内置数据库中，节点上的认证数据会在此阶段同步至集群中。

<!-- TODO 补充加载规则 -->

::: danger 

预设认证数据在配置文件中使用了明文密码，出于安全性与可维护性考虑应当避免使用该功能。

预设认证数据无法通过 API 修改、删除，请慎用。
:::



## 使用 HTTP API 管理认证数据

### 添加认证数据

API 定义：

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

使用 POST 请求添加 clientid 为 `emqx_c` password 为 `emqx_p` 的认证信息，返回信息中 `code = 0` 即为成功。



### 查看已经添加的认证数据

API 定义：

```bash
# Request
GET api/v4/auth_clientid

# Response
{
    "code": 0,
    "data": ["emqx_c"]
}
```



### 更改指定 Client ID 的密码

指定 Client ID，传递新密码进行更改，再次连接时需要使用新密码进行连接：

API 定义：

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



### 查看指定 Client ID 信息

指定 Client ID，查看相关 Client ID、密码信息，注意此处返回的密码是使用配置文件指定哈希方式加密后的密码：

API 定义：

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




### 删除认证数据

删除指定 Client ID：

API 定义：

```bash
# Request
DELETE api/v4/auth_clientid/${clientid}

# Response
{
    "code": 0
}
```
