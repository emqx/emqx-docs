---
# 标题
title: 目录结构
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

# 目录结构

使用二进制包安装 EMQ X 与使用 ZIP 压缩包安装 EMQ X 的目录结构有所不同, 具体如下:

#### 使用 ZIP 压缩包安装 EMQ X

+ 可执行目录: `./bin`
+ 数据文件: `./data`
+ Erlang虚拟机文件: `./erts-*`
+ 配置文件目录: `./etc`
+ 依赖项目录: `./lib`
+ 日志文件: `./log`

#### 二进制包安装 EMQ X 

+ 可执行目录: `/usr/lib/emqx/bin`
+ 数据文件: `/var/lib/emqx/data`
+ Erlang虚拟机文件: `/usr/lib/emqx/erts-*`
+ 配置文件目录: `/etc/emqx/etc`
+ 依赖项目录: `/usr/lib/emqx/lib`        
+ 日志文件: `/var/log/emqx`