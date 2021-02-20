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
##
## 使用 HMAC 算法校验 Token 的密钥
auth.jwt.secret = emqxsecret

## RSA 或 ECDSA 公钥文件
##
## 使用 RSA 或 ECDSA 算法校验 Token 的公钥
#auth.jwt.pubkey = etc/certs/jwt_public_key.pem

## JWKs 的服务器地址
##
## EMQ X 会从 JWKs 服务器获取密钥列表，并用于验证 Token
##
## JWKs 规范见: http://self-issued.info/docs/draft-ietf-jose-json-web-key.html
#auth.jwt.jwks = https://127.0.0.1:8080/jwks

## JWKs 密钥刷新时间
##
#auth.jwt.jwks.refresh_interval = 5m

## 客户端携带 Token 的方式
##
## Value: username | password
auth.jwt.from = password

## 是否校验 JWT 携带的字段
##
## Value: on | off
auth.jwt.verify_claims = off

## 字段校验列表
##
## 配置格式：auth.jwt.verify_claims.$name = $expected
##   $name 为需要校验的 JWT 的 payload 中的字段名称
##   $expected 为期待值
##
## $expected 可用的占位符为
##   - %u: username
##   - %c: clientid
##
## 例如：校验 JWT 的 payload 中的 username 是否与
## 客户端(MQTT协议)中携带的 username 一致
#auth.jwt.verify_claims.username = %u
```

### auth.jwt.from

客户端携带 JWT 的位置，用于配置客户端 JWT 字符串携带位置，可选 username 与 password。

### auth.jwt.verify_claims

如果你启用了 `auth.jwt.verify_claims` 选项，认证插件在验证 JWT 有效性之后还会进一步验证 Payload 中的数据有效性。

假设你的 Payload 为：

```json
{
  "username": "emqx_client_username"
}
```

你可以使用如下配置，当客户端携带此 Token 时，将验证客户端 `username` **是否等于** `emqx_client_username`：

```bash
## Variables:
##   - %u: username
##   - %c: clientid
auth.jwt.verify_claims.username = %u
```

支持使用固定值或当前客户端信息进行验证：
- %u：当前客户端 username
- %c：当前客户端 client id


## 密钥配置和算法支持

JWT 认证支持以三种方式配置密钥，这三种方式分别对应三种类型的算法支持：

- `auth.jwt.secret`：对称加密的方式，验证 JWT 的 Token 字段。它支持的算法有：
    - HS256 - HMAC，使用 SHA-256 哈希算法。
    - HS384 - HMAC，使用 SHA-384 哈希算法。
    - HS512 - HMAC，使用 SHA-512 哈希算法。

- `auth.jwt.pubkey`：使用非对称加密的方式，验证 JWT 的 Token 字段。它支持的算法有：
    - RS256 - RSA，使用 SHA-256 哈希算法。
    - RS384 - RSA，使用 SHA-384 哈希算法。
    - RS512 - RSA，使用 SHA-512 哈希算法。
    - ES256 - ECDSA，使用 P-256 曲线。
    - ES384 - ECDSA，使用 P-384 曲线。
    - ES512 - ECDSA，使用 P-512 曲线。

- `auth.jwt.jwks`：配置为 [JWKs](http://self-issued.info/docs/draft-ietf-jose-json-web-key.html) 服务器地址，从 JWKs 服务器中获取可用的密钥列表。


该三类密钥允许同时配置。EMQ X 在验证 Token 时会按 `auth.jwt.secret`，`auth.jwt.pubkey`，`auth.jwt.jwks` 顺序检查。


::: danger 
JWT 本身包含了认证信息，一旦泄露，任何人都可以获得该令牌的所有权限，使用 JWT 时建议启用 TLS 加密传输。
JWT 使用过程中无法在过期前废止某个 Token，请妥善设置有效时长并保管好密钥等加密信息。
:::
