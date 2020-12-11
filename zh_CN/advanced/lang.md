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

从 4.1 开始，EMQ X 提供了专门的多语言支持。它允许用户使用 Python, Java 等编程语言来处理 EMQ X 的 [钩子(Hooks)](./hooks.md) 事件，或解析私有的 TCP/UDP 协议，使得这类设备可以接入到 EMQ X 系统。

## 钩子扩展

多语言的 **钩子扩展** 由 [emqx-extension-hook](https://github.com/emqx/emqx-extension-hook) 插件进行支持。该插件在 4.1.0 中首次引入。它允许用户使用其它编程语言处理 EMQ X 的 [钩子(Hooks)](hooks.md)。例如：

- 校验某客户端的登录权限。
- 校验某客户端 PUB/SUB 的操作权限。
- 处理消息类事件，并消息桥接、转发或存储到其它的系统。

见：[多语言 - 钩子扩展](lang-exhook.md)。

## 协议接入

多语言的 **协议接入** 处理由 [emqx-exproto](https://github.com/emqx/emqx-exproto) 插件进行支持。该插件在 4.2.0 中首次引入。它允许用户使用其它的编程语言实现私有的，自定义的协议接入。例如：

- 可接收基于 TCP/UDP/TLS/DTLS 及 PSK 的终端连接。
- 允许 Python, Java 代码侧订阅主题，或发布消息到 EMQ X 系统。

见：[多语言 - 协议接入](lang-exproto.md)。

## 历史性遗留

在 EMQ X 4.1 之前，仅包函对 Lua 的支持，它由 [emqx-lun-hook](https://github.com/emqx/emqx-lua-hook) 实现。该插件仅支持对系统钩子的处理，不支持协议接入的处理。

见：[多语言 - Lua](lang-lua.md)。

