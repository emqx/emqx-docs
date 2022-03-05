---
# 编写日期
date: 2020-09-12 09:15:26
# 作者 Github 名称
author: hjianbo
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---

# 多语言支持

从 4.1 开始，EMQX 提供了专门的多语言支持。它允许用户使用 Python, Java 等编程语言来处理 EMQX 的 [钩子(Hooks)](./hooks.md) ，或解析私有的 TCP/UDP 协议，使得它们接入到 EMQX 系统。

注：在 4.1 到 4.2 版本中，多语言的功能使用 [erlport](https://github.com/emqx/erlport) 作为底层的通信的支持，在 4.3 以后升级到了 [gRPC](https://grpc.io) 。这两个版本的对外接口并不兼容，旧版本的设计请查看对应版本的文档。

注： 4.3 中，开放支持了所有的 EMQX 的钩子。


## 钩子扩展

多语言的 **钩子扩展** 由 **emqx-exhook** 插件进行支持。它允许用户使用其它编程语言处理 EMQX 的 [钩子(Hooks)](hooks.md)。例如：

- 校验某客户端的登录权限。
- 校验某客户端 PUB/SUB 的操作权限。
- 处理消息类事件，并消息桥接、转发或存储到其它的系统。

见：[多语言 - 钩子扩展](lang-exhook.md)。

## 协议接入

多语言的 **协议接入** 处理由 **emqx-exproto**  插件进行支持。它允许用户使用其它的编程语言实现私有的，自定义的协议接入。例如：

- 可接收基于 TCP/UDP/TLS/DTLS 及 PSK 的终端连接。
- 允许 Python, Java 代码侧订阅主题，或发布消息到 EMQX 系统。

见：[多语言 - 协议接入](lang-exproto.md)。

## 历史性遗留

在 EMQX 4.1 之前，仅包含对 Lua 的支持，它由 `emqx-lua-hook`  实现。该插件仅支持对系统钩子的处理，不支持协议接入的处理。

见：[多语言 - Lua](lang-lua.md)。
