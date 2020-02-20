---
# 标题
title: 代理订阅
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

# 代理订阅

EMQ X Broker 的代理订阅功能可以在客户端连接时，帮助客户端订阅用户配置的主题。

#### 开启代理订阅功能

EMQ X Broker 代理订阅功能默认是关闭的，打开代理订阅功能需要配置 `etc/emqx.conf` 文件，在 `etc/emqx.conf` 文件中查找 `module.subscription` 的配置项，并把它的值设置为 `on`，开启代理订阅功能。

```
module.subscription = on
```
#### 配置代理订阅规则

当然，仅仅开启并不意味代理订阅已经工作，你还需要配置相应的规则，EMQ X Broker 的代理订阅规则支持用户自行配置，用户可以自行添加多条代理订阅规则，每条代理订阅规则都需要指定 Topic 和 QoS，规则的数量没有限制，代理订阅规则的格式如下：

```
module.subscription.<number>.topic = <topic>
module.subscription.<number>.qos = <qos>
```

举个例子：

在 `etc/emqx.conf` 文件中手动添加代理订阅规则：

```
module.subscription.1.topic = $client/%c
module.subscription.1.qos = 1

module.subscription.2.topic = $user/%u
module.subscription.2.qos = 2
```

在代理订阅的配置中，可以使用 `%c` 和 `%u` 两个变量，EMQ X Broker 会把它们解析为来自客户端的 `client_id` 和 `username`

上面的配置决定了当客户端链接的时候，会自动帮客户端订阅 Qos 为 1 的 `$client/<client_id>` 主题和 Qos 为 2 的 `$user/<username>` 主题，配置项中的 `%c` 代表来自客户端连接的 `client_id`，`%u` 代表来自客户端连接的 `username`，`$client` 和 `$user` 均为文本字符串。

当一个 `clientid = testclient`，`username = tester` 的客户端连接 EMQ X Broker 的时候，代理订阅功能会主动帮客户端订阅 `$client/estclient`、`$user/tester` 这两个主题。