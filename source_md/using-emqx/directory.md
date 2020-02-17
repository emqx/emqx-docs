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

使用二进制包安装 EMQ X 与使用 ZIP 压缩包安装 EMQ X 的目录路径有所不同，具体如下:

#### 使用 ZIP 压缩包安装 EMQ X

+ 可执行文件目录: `./bin`；
+ 数据文件: `./data`；
+ Erlang虚拟机文件: `./erts-*`；
+ 配置文件目录: `./etc`；
+ 依赖项目录: `./lib`；
+ 日志文件: `./log`；
+ 启动相关的脚本、schema 文件:: `./releases`。

#### 二进制包安装 EMQ X 

+ 可执行文件目录: `/usr/lib/emqx/bin`；
+ 数据文件: `/var/lib/emqx/data`；
+ Erlang虚拟机文件: `/usr/lib/emqx/erts-*`；
+ 配置文件目录: `/etc/emqx/etc`；
+ 依赖项目录: `/usr/lib/emqx/lib`；       
+ 日志文件: `/var/log/emqx`；
+ 启动相关的脚本、schema 文件: `/usr/lib/emqx/releases`。

## 目录结构
以上目录中，用户经常接触与使用的是 `bin`、`etc`、`data`、`log` 目录。

#### `bin` 目录

+   `emqx`、`emqx.cmd`

    EMQ X 服务器的可执行文件，具体使用可以查看 [基本命令](using-emqx/command-line.md)。

+   `emqx_ctl`、`emqx_ctl.cmd`

    EMQ X 管理命令的可执行文件，具体使用可以查看  [管理命令 CLI](advanced/cli.md) 章节。

#### `etc` 目录

EMQ X 服务器通过 `etc` 目录下配置文件进行设置，主要配置文件包括:

| 配置文件           | 说明                      |
| -------------- | ------------------------- |
| emqx.conf      | EMQ X 消息服务器配置文件  |
| acl.conf       | EMQ X 默认ACL规则配置文件 |
| plugins/*.conf | EMQ X 各类插件配置文件    |
| certs          | EMQ X 证书文件           |

EMQ X 具体的配置内容可以查看 [配置项](configuration/index.md) 章节。

#### `data` 目录

EMQ X 服务器通过 `data` 目录储存运行数据，主要的文件包括:

+   `config`

    EMQ X 服务器读取 `etc/emqx.conf` 和 `etc/plugins/*.conf` 中的配置后，转换为 Erlang 原生配置文件格式，并在运行时读取其中的配置。

+   `loaded_plugins`

    `loaded_plugins` 文件记录了 EMQ X 服务器启动时默认启动的插件，可以通过编辑此文件配置 EMQ X 服务器插件的默认启动。

    ```
    $ cat loaded_plugins
    {emqx_management,true}.
    {emqx_recon,true}.
    {emqx_retainer,true}.
    {emqx_dashboard,true}.
    {emqx_rule_engine,true}.
    {emqx_bridge_mqtt,false}.
    {emqx_delayed_publish,true}.
    ```

+   `mnesia`

    Mnesia 数据库是 Erlang 内置的一个分布式 DBMS，可以直接存储 Erlang 的各种数据结构。

    `mnesia` 目录储存了所有 EMQ X 储存到 Mnesia 数据库中的数据，是一个十分重要的目录，`mnesia` 目录的删除将导致 EMQ X 丢掉所有的业务数据。

    `emqx_ctl mnesia` 命令可以查询 EMQ X 运行时 Mnesia 数据库系统状态，具体请查看 [管理命令 CLI](advanced/cli.md) 章节。


#### `log` 目录

+   `emqx.log.*`

    EMQ X 服务器运行时产生的日志文件的默认储存位置，可以通过编译 `etc/emqx.conf` 文件来完成对日志目录、日志文件名、日志文件最大容量、日志文件的最大文件数量等一系列配置，具体内容可以查看 [配置项](configuration/index.md) 章节。

    EMQ X 服务器在运行时可以通过 `emqx_ctl log` 命令和 `emqx_ctl trace` 命令来实现追踪来自某个客户端，或者发布到某个主题的全部消息，具体请查看 [管理命令 CLI](advanced/cli.md) 章节。

+   `crash.dump`

    EMQ X 服务器崩溃时转储日志文件位置，可以通过 `etc/emqx.conf` 修改配置，具体内容可以查看 [配置项](configuration/index.md) 章节。

+  `erlang.log*`

    Erlang 运行时产生的日志文件。
