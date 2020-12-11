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

# JWT 认证

[JWT](https://jwt.io/) 认证基于 Token 的鉴权机制，不依赖服务端保留客户端的认证信息或者会话信息，在持有密钥的情况下可以批量签发认证信息，是最简便的认证方式。

插件：

```bash
emqx_auth_jwt
```

## 认证原理

客户端使用 Token 作为用户名或密码（取决于插件配置），发起连接时 EMQ X 使用配置中的密钥、证书进行解密，如果能成功解密则认证成功，否则认证失败。

默认配置下启用 JWT 认证后，你可以通过任意用户名+以下密码进行连接：

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
## auth.jwt.pubkey = etc/certs/jwt_public_key.pem

## Value: on | off
auth.jwt.verify_claims = off

## auth.jwt.verify_claims.$name = expected

## Variables:
### - %u: username
### - %c: clientid
# auth.jwt.verify_claims.username = %u
```

### auth.jwt.from

客户端携带 JWT 的位置，用于配置客户端 JWT 字符串携带位置，可选 username 与 password。

### auth.jwt.verify_claims

如果你启用了 `auth.jwt.verify_claims` 选项，认证插件在验证 JWT 有效性之后还会进一步验证 Payload 中的数据有效性。

```bash
auth.jwt.verify_claims = on
```

JWT 的 Payload 应当是 JSON 结构的数据，`auth.jwt.verify_claims.$name` 中 `name` 即为需要验证的 Payload 数据 key 值，假设你的 Payload 为：

```json
{
  "username": "emqx_client_username"
}
```

你可以使用如下配置，当客户端携带此 Token 时，将验证客户端 `username` **是否等于** `emqx_client_username`：

```bash
## Variables:
### - %u: username
### - %c: clientid
auth.jwt.verify_claims.username = %u
```

支持使用固定值或当前客户端信息进行验证：
- %u：当前客户端 username
- %c：当前客户端 client id



::: danger 
JWT 本身包含了认证信息，一旦泄露，任何人都可以获得该令牌的所有权限，使用 JWT 时建议启用 TLS 加密传输。
JWT 使用过程中无法在过期前废止某个 Token，请妥善设置有效时长并保管好密钥等加密信息。
:::