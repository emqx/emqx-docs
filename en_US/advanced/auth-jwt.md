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
ref: undefined
---

# JWT

[JWT](https://jwt.io/) is a Token-based authentication mechanism. It does not rely on the server to retain client authentication information or session information. It can issue authentication information in batches while holding keys, which is an easiest authentication method.

Plugin:

```bash
emqx_auth_jwt
```

## Authentication principle

The client uses Token as the user name or password (depending on the plugin configuration). When initiating the connection, EMQ X Broker uses the key and certificate in the configuration to decrypt. If it can be successfully decrypted, the authentication successes, otherwise the authentication fails.

After JWT authentication is enabled by default, you can connect with the following password and any username:

```bash
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7ImF1dGhvciI6IndpdndpdiIsInNpdGUiOiJodHRwczovL3dpdndpdi5jb20ifSwiZXhwIjoxNTgyMjU1MzYwNjQyMDAwMCwiaWF0IjoxNTgyMjU1MzYwfQ.FdyAx2fYahm6h3g47m88ttyINzptzKy_speimyUcma4
```


## Configuration item

If you want to use JWT Auth you need open `etc/plugins/emqx_auth_jwt.conf` and edit as：

To enable JWT authentication, the following needs to be configured in  `etc/plugins/emqx_auth_jwt.conf`:

```bash
# etc/plugins/emqx_auth_jwt.conf

## Key
auth.jwt.secret = emqxsecret

## The way the client carries the token
## Value: username | password
auth.jwt.from = password


## Advanced options
## Public key file, certificate is used when signing the key
auth.jwt.pubkey = etc/certs/jwt_public_key.pem

## Value: on | off
auth.jwt.verify_claims = off

## auth.jwt.verify_claims.$name = expected

## Variables:
### - %u: username
### - %c: clientid
# auth.jwt.verify_claims.username = %u
```
<!-- TODO: verify_claims 的作用 -->

::: danger 
JWT contains authentication information by itself. Once leaked, anyone can get all the permissions of the token. It is recommended to enable TLS encrypted transmission when using JWT.

During the use of JWT, a token cannot be invalidated before it expires. Please properly set the validity time and keep the encryption information well.
:::