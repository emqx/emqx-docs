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

The client uses Token as the user name or password (depending on the plugin configuration). When initiating the connection, EMQX Broker uses the key and certificate in the configuration to decrypt. If it can be successfully decrypted, the authentication successes, otherwise the authentication fails.

After JWT authentication is enabled by default, you can connect with the following password and any username:

```bash
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7ImF1dGhvciI6IndpdndpdiIsInNpdGUiOiJodHRwczovL3dpdndpdi5jb20ifSwiZXhwIjoxNTgyMjU1MzYwNjQyMDAwMCwiaWF0IjoxNTgyMjU1MzYwfQ.FdyAx2fYahm6h3g47m88ttyINzptzKy_speimyUcma4
```

## JWT Authorization

JWT authentication plugin can extract ACL rules from authentication tokens. These ACL rules will be later used
to authorize client's actions. See [JWT ACL](./acl-jwt.md).

## Configuration item

If you want to use JWT Auth you need open `etc/plugins/emqx_auth_jwt.conf` and edit as：

To enable JWT authentication, the following needs to be configured in `etc/plugins/emqx_auth_jwt.conf`:

```bash
# etc/plugins/emqx_auth_jwt.conf

## Secret Key
##
## The key to verify the JWT Token using HMAC algorithm
auth.jwt.secret = emqxsecret

## RSA or ECDSA public key file
##
## The public key file to verify the JWT Token using RSA or ECDSA algorithm
#auth.jwt.pubkey = etc/certs/jwt_public_key.pem

## JWKs server address
##
## EMQX will get the key list from JWKs server and use it to verify the Token
##
## About the JWKs, see: http://self-issued.info/docs/draft-ietf-jose-json-web-key.html
#auth.jwt.jwks = https://127.0.0.1:8080/jwks

## JWKs refresh interval
##
#auth.jwt.jwks.refresh_interval = 5m

## The way the client carries the token
## Value: username | password
auth.jwt.from = password

## Enable to verify claims fields
##
## Value: on | off
auth.jwt.verify_claims = off

## The checklist of claims to validate
##
## Configuration format: auth.jwt.verify_claims.$name = $expected
##   - $name: the name of the field in the JWT payload to be verified
##   - $expected: the expected value
##
## The available placeholders for $expected:
##   - %u: username
##   - %c: clientid
##
## For example, to verify that the username in the JWT payload is the same
## as the client (MQTT protocol) username
#auth.jwt.verify_claims.username = %u
```
### auth.jwt.from

The field where the client carries the JWT Token, used to configure where the client carries the JWT string, optional fields are username and password.

### auth.jwt.verify_claims

If you enable the `auth.jwt.verify_claims` option, EMQX will verify the validity of the data in the Payload after verifying the validity of the JWT.

suppose your Payload is:

```json
{
  "username": "emqx_client_username"
}
```

You can use the following configuration to verify that client `username` is **equal to** `emqx_client_username` when the client carries this Token.

```properties
## Variables:
## - %u: username
## - %c: clientid
auth.jwt.verify_claims.username = %u
```

Support for verification using fixed values or current client information:
- %u: current client username
- %c: current client client id

## Key and Algorithm support

JWT authentication supports three ways to configure keys, which correspond to three types of algorithm support.

- `auth.jwt.secret`: a symmetric encryption method that validates the JWT Token field. It supports the following algorithms:
    - HS256 - HMAC, using the SHA-256 hash algorithm.
    - HS384 - HMAC, using the SHA-384 hash algorithm.
    - HS512 - HMAC, using the SHA-512 hash algorithm.

- `auth.jwt.pubkey`: authenticates the JWT Token field using asymmetric encryption. It supports the following algorithms.
    - RS256 - RSA, using the SHA-256 hash algorithm.
    - RS384 - RSA, using the SHA-384 hash algorithm.
    - RS512 - RSA, using the SHA-512 hash algorithm.
    - ES256 - ECDSA, using the P-256 curve.
    - ES384 - ECDSA, using the P-384 curve.
    - ES512 - ECDSA, using the P-512 curve.

- `auth.jwt.jwks`: configured as [JWKs](http://self-issued.info/docs/draft-ietf-jose-json-web-key.html) server address to get the list of available keys from the JWKs server.

::: tip
To get a JWT be successfully verified with a key from JWKs server, both key and JWT header must have the same key id.
:::

That is, JWKs server keys must contain `kid` field, like:
```json
{
    "keys": [
        {
            "kid": "testid@somedomain.io",

            "kty": "EC",
            "x": "m2qqiRuO04JawMAnlzXWAUlqlrpI84h6She9Aud7K_c",
            "y": "YC6VjPOT2fIk0256ZsF5McApzJqMlFnyZFXkWrTpXRs",
            "crv": "P-256",
            "alg": "ES256"
        }
        ...
    ]
}
```

JWT signed with the key, must have the key's id in its header, e.g.:
```json
{
  "alg": "ES256",
  "kid": "testid@somedomain.io",
  "typ": "JWT"
}
```

The three types of keys are allowed to be configured simultaneously. EMQX checks the Token in the order of `auth.jwt.secret`, `auth.jwt.pubkey`, `auth.jwt.jwks`.

::: tip
JWT contains authentication information by itself. Once leaked, anyone can get all the permissions of the token. It is recommended to enable TLS encrypted transmission when using JWT.

During the use of JWT, a token cannot be invalidated before it expires. Please properly set the validity time and keep the encryption information well.
:::
