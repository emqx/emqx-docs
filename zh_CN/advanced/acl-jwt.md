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

JWT ACL 使用登录认证时来自客户端提供的 JWT 内的规则列表。为了使 JWT 字符串足够下，建议使用 JWT ACL 的客户端应该定义少量的 ACL 规则表。

插件：

```bash
emqx_auth_jwt
```

::: 提示
emqx_auth_jwt 授权特性与认证特性紧密耦合。
:::

## ACL 信息存储在声明中

要启用 JWT ACL，首先应该指定用于搜索 ACL 规则列表的字段名称。

```bash
# etc/plugins/emqx_auth_jwt.conf

## Server address
auth.jwt.acl_claim_name = acl

```

如果客户端在其 JWT 中没有指定 ACL 的字段，则不会应用 JWT ACL。

## 数据结构

ACL 规则的数据结构如下：

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

`pub`、`sub` 和 `all` 表示对于发布/订阅具有权限的主题列表。

可以在主题白名单中使用以下占位符:
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

客户端登录的 JWT 字段中，可以通过携带 `exp` 字段来拒绝该客户端所有的发布/订阅请求。
所以，过期的 JWT 客户端必须使用新的 JWT 进行重连。

为了使 ACL 规则永远有效，客户端可以不提供 `exp` 字段来表示这是一个永久有效的 ACL 规则。

::: 警告！！
1. 使用长期的 JWT 是不安全的。
2. 开启 ACL 缓存后，可能会导致过期时间检查无效。
:::
