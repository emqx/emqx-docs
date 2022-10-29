# JWT 认证

[JWT](https://JWT.io/) 认证是基于 Token 的鉴权机制，不依赖服务端保留客户端的认证信息或者会话信息，在持有密钥的情况下可以批量签发认证信息，是最简便的认证方式。

## 创建模块

打开 [EMQX Dashboard](http://127.0.0.1:18083/#/modules)，点击左侧的“模块”选项卡，选择“添加模块”：

![Modules](./assets/auth_jwt1.png)

然后选择“认证鉴权”下的“JWT 认证”：

![Modules JWT Selected](./assets/auth_jwt2.png)

JWT 认证提供了以下配置项：

1. 认证来源：客户端连接时存放 JWT 的字段，目前支持选择 username 或 password。
2. 密钥：签发 JWT 时使用的密钥。这里将用于验证 EMQX 收到的 JWT 是否合法，适用于 HMAC 算法签发的 JWT。
3. 公钥文件：将用于验证 EMQX 收到的 JWT 是否合法，适用于 RSA 或 ECDSA 算法签发的 JWT。
4. JWKS 服务器地址：EMQX 将从 JWKS 服务器定期查询最新的公钥列表，并用于验证收到的 JWT 是否合法，适用于 RSA 或 ECDSA 算法签发的 JWT。
5. 验证声明字段：是否需要验证 JWT Payload 中的声明与“声明字段列表”一致。
6. 声明字段列表：用于验证 JWT Payload 中的声明是否合法。最常见的用法是，添加一个键为 `username` 值为 `%u` 的键值对，`%u` 作为占位符将在运行时被替换为客户端实际连接时使用的 Username，替换后的值将被用于与 JWT Payload 的同键声明的值比较，以起到 JWT 与 Username 一一对应的效果。声明字段列表中目前支持以下两种占位符：
   1. `%u`：将在运行时被替换为客户端连接时使用的 Username。
   2. `%c`：将在运行时被替换为客户端连接时使用的 Client ID。

> 注意：EMQX 会按照 `Secret`、`Pubkey` 和 `JWKs Addr` 的固定顺序验证 JWT。没有配置的字段将被忽略。

![JWT Module Settings](./assets/auth_jwt3.png)

配置完成后点击“添加”按钮即可成功添加 JWT 认证模块。

![Modules JWT Added](./assets/auth_jwt4.png)

## 认证原理

客户端使用用户名或密码字段携带 JWT（取决于模块配置），发起连接时 EMQX 使用配置中的密钥、证书进行解密，如果能成功解密则认证成功，否则认证失败。

默认配置下启用 JWT 认证后，你可以通过任意用户名+以下密码进行连接：

```bash
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7ImF1dGhvciI6IndpdndpdiIsInNpdGUiOiJodHRwczovL3dpdndpdi5jb20ifSwiZXhwIjoxNTgyMjU1MzYwNjQyMDAwMCwiaWF0IjoxNTgyMjU1MzYwfQ.FdyAx2fYahm6h3g47m88ttyINzptzKy_speimyUcma4
```

::: tip

上述JWT Token仅做测试使用，可根据自己的业务需求用相关工具生成。此处提供一个在线生成工具：https://www.jsonwebtoken.io/。

:::

## JWT 中的 acl 字段

模块配置参数中的 "ACL Claim Name" 字段可以用于指定从那个 JWT 字段中提取 ACL 信息。
如果一个 JWT 中不含该字段，则不会对该客户端进行 ACL 检查（其他插件或模块的 ACL 不受影响）。

## 数据结构

```js
{
    # ... payload claims ...
    "acl": {
        "sub": [
            "some/topic/for/sub/1",
            "some/topic/for/sub/2"
        ],
        "pub": [
            "some/topics/for/pub/1",
            "some/topics/for/pub/2"
        ],
        "all": [
            "some/topics/for/pubsub/1",
            "some/topics/for/pubsub/2"
        ]
    }
}
```

`pub`、`sub` 和 `all` 用于指定该客户端允许发布和订阅的主题白名单。

可以在主题白名单中使用以下占位符:

- %u：用户名
- %c：客户端 ID

例如:
```js
{
    # ... payload claims ...
    "acl": {
        "pub": [
            "some/stats/%c"
        ]
    }
}
```

## ACL 过期

JWT ACL 检查时如果发现这个 Token 已经超期 (`exp` 字段指定)，那么所有的操作都将被禁止。
客户端必需获取一个新的有效 JWT 在进行重连。

如果想要让一个 JWT 永不超期，可以在 JWT 中不提供 `exp` 字段即可。

::: warning
1. 使用长期的 JWT 是不安全的。
2. ACL 缓存开启的情况下，JWT 超期之后一段时间可能还会继续有效，直到缓存失效。
:::
