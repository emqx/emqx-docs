---
# 标题
title: 认证
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

# 认证

身份认证是大多数应用的重要组成部分，MQTT 协议支持用户名和密码实现客户端的身份认证，启用身份认证能有效阻止非法客户端的连接。

本章节介绍了 EMQ X 支持的认证方式以及对应插件的配置方法。



## 认证方式

EMQ X 支持使用内置数据源（文件、内置数据库）、JWT、外部主流数据库和自定义 HTTP API 进行身份认证。

认证功能是通过插件实现的，每个插件对应一种认证方式，使用前需要启用相应的插件。插件通过检查每个终端接入的 username/clientid 和 password 是否与指定数据源的信息一致来实现对终端的身份认证。

EMQ X 支持的认证方式：


**内置数据源**

* [用户名认证](./auth-username.md)
* [Cliend ID 认证](./auth-clientid.md)

使用配置文件与 EMQ X 内置数据库提供认证数据源，通过 REST API 进行管理，足够简单轻量。



**外部数据库**

* [LDAP 认证](./auth-ldap.md)
* [MySQL 认证](./auth-mysql.md)
* [PostgreSQL 认证](./auth-postgresql.md)
* [Redis 认证](./auth-redis.md)
* [MongoDB 认证](./auth-mongodb.md)

外部数据库可以存储大量数据，同时方便与外部设备管理系统集成。



**其他**

* [HTTP 认证](./auth-http.md)
* [JWT 认证](./auth-jwt.md)

JWT 认证可以批量签发认证信息，HTTP 认证能够实现复杂的后端逻辑。



{% hint style="info" %} 

部分认证插件同时具有 ACL 功能，认证部分具体使用方式以插件页面为准。

{% endhint %}


## 认证结果

任何一种认证方式最终都会返回三个结果中的一种：

- 认证成功：经过比对客户端认证成功
- 认证失败：经过比对客户端认证失败，数据源中密码与当前密码不一致
- 忽略认证（ignore）：当前认证方式中未查找到认证数据，无法显式判断结果是成功还是失败，交由认证链下一认证方式或匿名认证来判断


## 匿名认证

EMQ  X 默认配置中启用了匿名认证，任何客户端都能接入 EMQ X。没有启用认证插件或认证插件没有显式允许/拒绝（ignore）连接请求时，EMQ X 将根据匿名认证启用情况决定是否允许客户端连接。

配置匿名认证开关：

```bash
# etc/emqx.conf

## Value: true | false
allow_anonymous = true
```

{% hint style="danger" %} 

生产环境中请禁用匿名认证。

{% endhint %}



## 认证链

当同时启用多个认证方式时，EMQ X 将按照插件开启先后顺序进行链式认证：
- 一旦认证成功，终止认证链并允许客户端接入
- 一旦认证失败，终止认证连并禁止客户端接入
- 直到最后一个认证方式仍未通过，根据**匿名认证**配置判定
  - 匿名认证开启时，允许客户端接入
  - 匿名认证关闭时，禁止客户端接入



![_images/guide_2.png](_assets/guide_2.png)

<!-- replace -->

{% hint style="info" %} 

同时只启用一个认证插件可以提高客户端身份认证效率。

{% endhint %}


