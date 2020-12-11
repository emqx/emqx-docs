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
ref: undefined
---

# Proxy subscriptions

The proxy subscription function of EMQ X Broker allows the client to automatically establish the user's preset subscription relationship without sending additional SUBSCRIBE packets when the connection is established.

::: tip
You can use the Http API to subscribe and unsubscribe to connected devices, see [topic subscription](./http-api.md#endpoint-subscribe) and  [topic unscription](./http-api.md#endpoint-do-unsubscribe)
:::


## Built-in proxy subscription

Through the built-in proxy subscription module, you can specify proxy subscription rules through the configuration file to achieve proxy subscription, which is suitable for regular and static proxy subscription requirements.

### Enable proxy subscription function

The proxy subscription function is disabled by default. To enable this function, you need to modify the `module.subscription` configuration item in the `etc/emqx.conf` file. The default `off` means disabled. If you want to enable it, please change it to ` on`.

```bash
module.subscription = off
```

### Configure proxy subscription rules

Of course, just enabling does not mean that the proxy subscription has already worked. You need to configure the corresponding rules. The proxy subscription rules of EMQ X Broker support users to configure by themselves. The user can add multiple proxy subscription rules and each rule should specify the Topic and QoS. There is no limit for the number of rules. The format of proxy subscription rules is as follows:

```bash
module.subscription.<number>.topic = <topic>
module.subscription.<number>.qos = <qos>
```

When configuring the topic of proxy subscription, EMQ X Broker provides two placeholders of  `%c` and `%u` for users to use, and EMQ X Broker will replace the `%c` and `%u` in the configuration with the client's `Client ID` and `Username` respectively when performing proxy subscription. It should be noted that `%c` and `%u` must occupy an entire topic level.

For example, the following proxy subscription rules are added in the  `etc/emqx.conf`  file:

```bash
module.subscription.1.topic = client/%c
module.subscription.1.qos = 1

module.subscription.2.topic = user/%u
module.subscription.2.qos = 2
```

When a client connects to EMQ X Broker, if the client's `Client ID` is ` testclient` and `Username` is ` tester`, according to the configuration rules above, the proxy subscription function will actively help the client subscribe to the topics of `client/testclient`(QoS is 1) and `user/tester`( QoS is 2)


## Dynamic proxy subscription

The EMQ X Enterprise version supports dynamic proxy subscription. The external database is used to set the topic list and the list is read when the device is connected to realize the proxy subscription.

