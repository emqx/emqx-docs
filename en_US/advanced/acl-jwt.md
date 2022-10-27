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

JWT ACL uses ACL rules from JWTs provided by a client during authentication. To keep JWTs reasonably small, clients using JWT ACL are not supposed to have many ACL rules.

Plugin:

```bash
emqx_auth_jwt
```

::: tip
The emqx_auth_jwt authorization features are tightly coupled with authentication features.
:::

## ACL information stored in claims

To enable authorization via JWT one should specify claim name for searching ACL rules.

```bash
# etc/plugins/emqx_auth_jwt.conf

## Server address
auth.jwt.acl_claim_name = acl

```

If the provided claim is not found in the JWT, no ACL check will be applied for this client, unless there
are other ACL plugins or modules enabled.


## Data structure

The data structure of ACL rules is the following:

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

`pub`, `sub` and `all` lists serve as whitelists for the corresponding operations.

You can use the following placeholders in topic whitelists:
- %u: Username
- %c: Client ID

For example:
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

EMQX Broker will automatically interpolate topic names before checking ACL.

## ACL expiration

JWT ACL engine will prohibit all operations after the deadline specified in `exp` JWT claim, so
a client with an expired JWT has to reconnect with a fresh JWT.

To make ACL rules valid forever, a client may not provide `exp` claim at all.

::: warning
1. Using long-living JWTs is not considered secure.
2. When ACL cache is enabled, the ACL rule's expiration is either when the cache or JWT expires, whichever is the later.
:::
