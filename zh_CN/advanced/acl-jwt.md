---
# 编写日期
date: 2022-05-18 13:00:00
# 作者 Github 名称
author: savonarola
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref: undefined
---

# JWT ACL

JWT ACL 在身份验证期间使用来自客户端提供的 JWT 的 ACL 规则。 为了使 JWT 保持合理的小，使用 JWT ACL 的客户端不应该有很多 ACL 规则。

插件：

```bash
emqx_auth_jwt
```

::: 提示
emqx_auth_jwt 授权特性与认证特性紧密耦合。
:::

## ACL信息存储在声明中

要通过JWT启用授权，应该指定用于搜索ACL规则的声明名称。

```bash
# etc/plugins/emqx_auth_jwt.conf

## Server address
auth.jwt.acl_clame_name = acl

```

如果客户端在其 JWT 中没有指定的声明，则不会应用 JWT 授权。

## 数据结构

ACL规则的数据结构如下：

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

`pub`、`sub` 和 `all` 罗列出用作相应操作的白名单。

您可以在主题白名单中使用以下占位符:
- %u：Username
- %c：Client ID

例如：
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

EMQX Broker 在检查 ACL 之前会自动填充使用占位符的主题。

## ACL 过期

JWT ACL 引擎禁止JWT 声明中 `exp` 指定的截止日期之后的所有操作，所以
过期的JWT客户端必须使用新的JWT进行重连。

为了使 ACL 规则永远有效，客户端可能根本不提供 `exp` 声明。

::: 警告！！
使用长期的JWT是不安全的。
:::
