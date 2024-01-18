---
# 标题
title: EMQX CLI
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

# Basic command

EMQX Broker provides the `emqx` command line tool, which is convenient for users to start, stop, and enter the console of EMQX Broker.

+   `emqx start`

    Start EMQX Broker；

+   `emqx stop`

    Stop EMQX Broker；

+   `emqx restart`

    Restart EMQX Broker；

+   `emqx console`

    Start EMQX Broker with console；

+   `emqx foreground`

    Start EMQX Broker with console. Unlike `emqx console` , `emqx foreground` does not support entering Erlang commands;

+   `emqx ping`

    Ping EMQX Broker。

The above commands are commonly used by users. In addition, the `emqx` command has some [other options](../advanced/cli.md) for the convenience of developers, and ordinary users do not need to care about that.