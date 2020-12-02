---
# 标题
title: EMQ X CLI
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

EMQ X Broker provides the `emqx` command line tool, which is convenient for users to start, stop, and enter the console of EMQ X Broker.

+   `emqx start`

    Start EMQ X Broker；

+   `emqx stop`

    Stop EMQ X Broker；

+   `emqx restart`

    Restart EMQ X Broker；

+   `emqx console`

    Start EMQ X Broker with console；

+   `emqx foreground`

    Start EMQ X Broker with console. Unlike `emqx console` , `emqx foreground` does not support entering Erlang commands;

+   `emqx ping`

    Ping EMQ X Broker。

The above commands are commonly used by users. In addition, the `emqx` command has some [other options](../advanced/cli.md) for the convenience of developers, and ordinary users do not need to care about that.