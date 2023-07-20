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

如果一个 JWT 中不含该字段，则不会对该客户端进行 ACL 检查（其他插件或模块的 ACL 不受影响）。

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

## ACL 过期

JWT ACL 检查时如果发现这个 Token 已经超期 (`exp` 字段指定)，那么所有的操作都将被禁止。
客户端必需获取一个新的有效 JWT 在进行重连。

如果想要让一个 JWT 永不超期，可以在 JWT 中不提供 `exp` 字段即可。

:::
1. 使用长期的 JWT 是不安全的。
2. ACL 缓存开启的情况下，JWT 超期之后一段时间可能还会继续有效，直到缓存失效。
:::
