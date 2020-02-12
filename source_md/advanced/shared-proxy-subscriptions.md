---
# 标题
title: 代理订阅
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

# 代理订阅

EMQ X 的代理订阅功能可以在当客户端连接时, 自动订阅主题.

启用 EMQ X 的代理订阅需要配置 `etc/emqx.conf` 文件.

```
## Subscription Module

## Enable Subscription Module.
##
## Value: on | off
module.subscription = on

## Subscribe the Topics automatically when client connected.
## Qos of the subscription: 0 | 1 | 2

module.subscription.1.topic = $client/%c
module.subscription.1.qos = 1

module.subscription.2.topic = $user/%u
module.subscription.2.qos = 2
```

上面的配置决定了当客户端链接的时候, 会自动帮客户端订阅 Qos 为 1 的 `$client/<client_id>` 主题和 Qos 为 2 的 `$user/<username>` 主题, 配置项中的 `%c` 代表来自客户端连接的 `client_id`, `%u` 代表来自客户端连接的 `username`, `$client` 和 `$user` 均为文本字符串.

举个例子:

当一个 `clientid = testclient`, `username=tester` 的客户端连接 EMQ X 的时候, 代理订阅功能会主动帮客户端订阅 `$client/estclient`、`$user/tester` 这两个主题.