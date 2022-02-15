---
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
ref:
---

# 目录结构

不同安装方式得到的 EMQ X 其目录结构会有所不同，具体如下:

| 描述                        | 使用 ZIP 压缩包安装                 | 使用二进制包安装                  |
| --------------------------- | -------------------------------- | ----------------------------- |
| 可执行文件目录              | `./bin`                          | `/usr/lib/emqx/bin`           |
| 数据文件                    | `./data`                         | `/var/lib/emqx/data`          |
| Erlang 虚拟机文件           | `./erts-*`                       | `/usr/lib/emqx/erts-*`        |
| 配置文件目录                | `./etc`                          | `/etc/emqx/etc`               |
| 依赖项目录                  | `./lib`                          | `/usr/lib/emqx/lib`           |
| 日志文件                    | `./log`                          | `/var/log/emqx`               |
| 启动相关的脚本、schema 文件 | `./releases`                     | `/usr/lib/emqx/releases`      |


以上目录中，用户经常接触与使用的是 `bin`、`etc`、`data`、`log` 目录。

## bin 目录

**emqx、emqx.cmd**

EMQ X 的可执行文件，具体使用可以查看 [基本命令](./command-line.md)。

**emqx_ctl、emqx_ctl.cmd**

EMQ X 管理命令的可执行文件，具体使用可以查看  [管理命令 CLI](../advanced/cli.md)。

## etc 目录

EMQ X 通过 `etc` 目录下配置文件进行设置，主要配置文件包括:

| 配置文件           | 说明                      |
| -------------- | ------------------------- |
| emqx.conf      | EMQ X 配置文件  |
| acl.conf       | EMQ X 默认 ACL 规则配置文件 |
| plugins/*.conf | EMQ X 各类插件配置文件    |
| certs          | EMQ X SSL 证书文件       |
| emqx.lic      | License 文件{% emqxce %}仅限 EMQ X Enterprise{% endemqxce %}   |

EMQ X 具体的配置内容可以查看 [配置项](../configuration/configuration.md)。

## data 目录

EMQ X 将运行数据存储在 `data` 目录下，主要的文件包括:

**configs/app.*.config**

EMQ X 读取 `etc/emqx.conf` 和 `etc/plugins/*.conf` 中的配置后，转换为 Erlang 原生配置文件格式，并在运行时读取其中的配置。

**loaded_plugins**

`loaded_plugins` 文件记录了 EMQ X 默认启动的插件列表，可以修改此文件以增删默认启动的插件。`loaded_plugins` 中启动项格式为 `{<Plugin Name>, <Enabled>}.`，`<Enabled>` 字段为布尔类型，EMQ X 会在启动时根据 `<Enabled>` 的值判断是否需要启动该插件。关于插件的更多内容，请查看 [插件](../advanced/plugins.md)。

{% emqxce %}

```bash
$ cat loaded_plugins
{emqx_management,true}.
{emqx_recon,true}.
{emqx_retainer,true}.
{emqx_dashboard,true}.
{emqx_rule_engine,true}.
{emqx_bridge_mqtt,false}.
```

{% endemqxce %}

{% emqxee %}

```bash
$ cat loaded_plugins
{emqx_management, true}.
{emqx_dashboard, true}.
{emqx_schema_registry, true}.
{emqx_rule_engine, true}.
```

{% endemqxee %}

**mnesia**

Mnesia 数据库是 Erlang 内置的一个分布式 DBMS，可以直接存储 Erlang 的各种数据结构。

EMQ X 使用 Mnesia 数据库存储自身运行数据，例如告警记录、规则引擎已创建的资源和规则、Dashbaord 用户信息等数据，这些数据都将被存储在 `mnesia` 目录下，因此一旦删除该目录，将导致 EMQ X 丢失所有业务数据。

可以通过 `emqx_ctl mnesia` 命令查询 EMQ X 中 Mnesia 数据库的系统信息，具体请查看 [管理命令 CLI](../advanced/cli.md)。


## log 目录

**emqx.log.***

EMQ X 运行时产生的日志文件，具体请查看 [日志与追踪](./log.md)。

**crash.dump**

EMQ X 的崩溃转储文件，可以通过 `etc/emqx.conf` 修改配置，具体内容可以查看 [配置项](../configuration/configuration.md)。

**erlang.log.***

以 `emqx start` 方式后台启动 EMQ X 时，控制台日志的副本文件。
    