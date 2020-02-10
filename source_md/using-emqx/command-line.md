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

使用二进制包安装 EMQ X 与使用 ZIP 压缩包安装 EMQ X 在使用 `emqx` 命令时有所不同, 使用二进制包安装时 `emqx` 命令会软连接到 `/usr/bin` 目录下, 所以可以直接使用, 使用 ZIP 压缩包安装的 EMQ X 需要在使用时指定 `emqx` 的目录, 默认在 EMQ X 解压缩后的根目录下, 下文以使用二进制包安装 EMQ X 为例:

+ `emqx start` : 启动 EMQ X
+ `emqx stop` : 关闭 EMQ X
+ `emqx restart` : 重启 EMQ X
+ `emqx console` : 使用控制台启动 EMQ X