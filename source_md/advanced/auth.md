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

认证（认证鉴权）指的是当一个客户端连接到 MQTT 服务器的时候，通过服务器端的配置来控制客户端连接服务器的权限。EMQ X 的认证支持包括多个层面，分别有 MQTT 传输层，应用层和 EMQ X 本身以插件的方式来支持各种增强的认证方式。

- 在传输层上，TLS 可以保证使用客户端证书的客户端到服务器的身份验证，并确保服务器向客户端验证服务器证书
- 在应用层上，MQTT 协议本身在 CONNECT 报文中指定用户名和密码。客户端可以在连接到 MQTT 服务器时发送用户名和密码进行认证，有效阻止非法客户端的连接
- EMQ X 层面上，以插件形式支持配置文件、HTTP API、JWT、LDAP 及各类数据库如 MongoDB、MySQL、PostgreSQL、Redis 等多种认证。


## 认证与认证链

EMQ X 默认开启匿名认证，即允许任意客户端登录，具体配置在 `etc/emqx.conf` 中：

```bash
## Allow Anonymous authentication
mqtt.allow_anonymous = true
```

EMQ X 认证相关插件名称以 `emqx_auth` 开头。当启用认证插件之前，请在配置文件 `etc/emqx.conf` 中把允许匿名认证的方式给去掉:``mqtt.allow_anonymous = false``。当共同启用多个认证插件时，EMQ X 将按照插件开启先后顺序进行链式认证，一旦认证成功就终止认证链并允许客户端接入，最后一个认证源仍未通过时将终止客户端连接，认证链的认证过程示意图如下所示。

