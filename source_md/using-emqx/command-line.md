---
# 标题
title: 基本命令
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

# 基本命令

使用二进制包安装 EMQ X 与使用 ZIP 压缩包安装 EMQ X 在使用 `emqx` 命令时有所不同，使用二进制包安装时 `emqx` 命令会软连接到 `/usr/bin` 目录下，所以可以直接使用，使用 ZIP 压缩包安装的 EMQ X 需要在使用时指定 `emqx` 的目录，默认在 EMQ X 解压缩后的根目录下，下文以使用二进制包安装 EMQ X 为例:

+   `emqx start`

    后台启动 EMQ X 服务；

+   `emqx stop`

    关闭 EMQ X 服务；

+   `emqx restart`

    重启 EMQ X 服务；

+   `emqx console`

    使用控制台启动 EMQ X 服务；

+   `emqx foreground`

    使用控制台启动 EMQ X 服务，与 `emqx console` 不同，`emqx foreground` 不支持输入 Erlang 命令；

+   `emqx ping`

    Ping EMQ X 服务。

以上命令为用户常用命令， 此外 `emqx` 命令还有一些其他选项为方便开发者使用， 普通用户无需关心。