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

认证插件：

```bash
emqx_auth_jwt
```

{% hint style="danger" %} 
JWT 一旦签发只能自动过期，请妥善保管密钥，密钥泄露后有很大的安全影响。
{% endhint %}



