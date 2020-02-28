---
# 标题
title: JWT 认证
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

# JWT 认证

[JWT](https://jwt.io/) 认证基于 Token 的鉴权机制，不依赖服务端保留客户端的认证信息或者会话信息，在持有密钥的情况下可以批量签发认证信息，是最简便的认证方式。

插件：

```bash
emqx_auth_jwt
```

## 认证原理

客户端使用 Token 作为用户名或密码（取决于插件配置），发起连接时 EMQ X 使用配置中的密钥、证书进行解密，如果能成功解密则认证成功，否则认证失败。

默认配置下启用 JWT 认证后，你可以通过任意用户名，以下密码进行连接：

```bash
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7ImF1dGhvciI6IndpdndpdiIsInNpdGUiOiJodHRwczovL3dpdndpdi5jb20ifSwiZXhwIjoxNTgyMjU1MzYwNjQyMDAwMCwiaWF0IjoxNTgyMjU1MzYwfQ.FdyAx2fYahm6h3g47m88ttyINzptzKy_speimyUcma4
```


## 配置项

要启用 JWT 认证，需要在 `etc/plugins/emqx_auth_jwt.conf` 中配置以下内容：

```bash
# etc/plugins/emqx_auth_jwt.conf

## 密钥
auth.jwt.secret = emqxsecret

## 客户端携带 Token 的方式
## Value: username | password
auth.jwt.from = password


## 高级选项
## 公钥文件，证书作为签发密钥时使用
auth.jwt.pubkey = etc/certs/jwt_public_key.pem

## Value: on | off
auth.jwt.verify_claims = off

## auth.jwt.verify_claims.$name = expected

## Variables:
##  - %u: username
##  - %c: clientid
# auth.jwt.verify_claims.username = %u
```
<!-- TODO: verify_claims 的作用 -->

{% hint style="danger" %} 
JWT 本身包含了认证信息，一旦泄露，任何人都可以获得该令牌的所有权限，使用 JWT 时建议启用 TLS 加密传输。
JWT 使用过程中无法在过期前废止某个 Token，请妥善设置有效时长并保管好密钥等加密信息。
{% endhint %}