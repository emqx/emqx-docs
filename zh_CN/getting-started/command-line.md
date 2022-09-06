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

# 基本命令

EMQX 提供了 `emqx` 命令行工具，方便用户对 EMQX 进行启动、关闭、进入控制台等操作。

+   `emqx start`

    后台启动 EMQX Broker；

+   `emqx stop`

    关闭 EMQX Broker；

+   `emqx restart`

    重启 EMQX Broker；

+   `emqx console`

    使用控制台启动 EMQX Broker；

+   `emqx foreground`

    使用控制台启动 EMQX Broker，与 `emqx console` 不同，`emqx foreground` 不支持输入 Erlang 命令；

+   `emqx ping`

    Ping EMQX Broker, 检查当前节点是否通信正常；

+   `emqx check_conf`

    检查配置文件格式是否正常，如果你修改了配置文件，推荐在启动前先执行此命令，来检查配置文件的格式是否符合要求。


以上命令为用户常用命令，此外 `emqx` 命令还有一些[其他选项](../advanced/cli.md)为方便开发者使用，普通用户无需关心。