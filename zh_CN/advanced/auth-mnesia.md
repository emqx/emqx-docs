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

# Mnesia 认证

Mnesia 认证使用 EMQ X 内置 Mnesia 数据库存储客户端 Client ID/Username 与密码，支持通过 HTTP API 管理认证数据。

Mnesia 认证不依赖外部数据源，使用上足够简单轻量。

插件：

```bash
emqx_auth_mnesia
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

预设认证数据格式兼容 `emqx_auth_clientid` 与 `emqx_auth_username` 插件的配置格式

```bash
# etc/plugins/emqx_auth_mnesia.conf

## clientid 认证数据
auth.client.1.clientid = admin_client
auth.client.1.password = public

## username 认证数据
auth.user.2.username = admin
auth.user.2.password = public
```

插件启动时将读取预设认证数据并加载到 EMQ X 内置数据库中，节点上的认证数据会在此阶段同步至集群中。

<!-- TODO 补充加载规则 -->

::: danger 

预设认证数据在配置文件中使用了明文密码，出于安全性与可维护性考虑应当避免使用该功能。
:::

## 使用 HTTP API 管理认证数据

### 添加认证数据

+ Clientid

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

+ Username

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


### 批量添加认证数据

+ Clientid

  ```bash
  # Request
  POST api/v4/auth_clientid
  [
      {
      		"clientid": "emqx_c_1",
      		"password": "emqx_p"
      },
      {
          "clientid": "emqx_c_2",
          "password": "emqx_p"
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

+ Username

  ```bash
  # Request
  POST api/v4/auth_username
  [
      {
      		"username": "emqx_u_1",
      		"password": "emqx_p"
      },
      {
          "username": "emqx_u_2",
          "password": "emqx_p"
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


### 查看已经添加的认证数据

+ Clientid

  ```bash
  # Request
  GET api/v4/auth_clientid
  
  # Response
  {
    "meta": {
      "page": 1,
      "limit": 10,
      "count": 1
    },
    "data": [
    			"clinetid": "emqx_c",
    			"clinetid": "emqx_c_1",
    			"clinetid": "emqx_c_2"
    		],
    "code": 0
  }
  ```

+ Username

  ```bash
  # Request
  GET api/v4/auth_username
  
  # Response
  {
    "meta": {
      "page": 1,
      "limit": 10,
      "count": 1
    },
    "data": [
    			"username": "emqx_u",
    			"username": "emqx_u_1",
    			"username": "emqx_u_2"
    		],
    "code": 0
  }
  ```

### 更改已添加的认证数据

+ Clientid

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

+ Username

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

### 查看指定的认证数据

注意此处返回的密码是使用配置文件指定哈希方式加密后的密码：

+ Clientid

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

+ Username

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


### 删除指定的 Clientid 认证数据

+ Clinetid

  ```bash
  # Request
  DELETE api/v4/auth_clientid/${clientid}
  
  # Response
  {
      "code": 0
  }
  ```

+ Username

  ```bash
  # Request
  DELETE api/v4/auth_username/${username}
  
  # Response
  {
      "code": 0
  }
  ```