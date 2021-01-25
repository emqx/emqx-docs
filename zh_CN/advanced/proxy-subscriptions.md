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

# 代理订阅

EMQ X 的代理订阅功能使得客户端在连接建立时，不需要发送额外的 SUBSCRIBE 报文，便能自动建立用户预设的订阅关系。

## 开启代理订阅功能

代理订阅功能由 `emqx_mod_subscription` 内置模块提供，此功能默认关闭，支持在 EMQ X Broker 运行期间动态启停，请参见 [内置模块](./internal-modules.md)。

## 配置代理订阅规则

代理订阅功能开启之后还需要配置相应的规则，EMQ X 的代理订阅规则支持用户自行配置，用户可以自行添加多条代理订阅规则，代理订阅规则支持用户配置 Topic 和订阅选项（QoS、No Local、Retain As Published、Retain Handling），其中 Topic 为必填项，订阅选项如果不配置的话 EMQ X 会应用默认的配置。

代理订阅规则的格式如下：

```bash
## 代理订阅的主题
module.subscription.<number>.topic = <topic>

## 代理订阅的订阅选项：QoS
## 可选值: 0、1、2
## 默认值：1
module.subscription.<number>.qos = <qos>

## 代理订阅的订阅选项：No Local
## 可选值: 0、1
## 默认值：0
module.subscription.<number>.nl = <nl>

## 代理订阅的订阅选项：Retain As Published
## 可选值: 0、1
## 默认值：0
module.subscription.<number>.rap = <rap>

## 代理订阅的订阅选项：Retain Handling
## 可选值: 0、1、2
## 默认值：0
module.subscription.<number>.rh = <rh>
```

需要注意的是，订阅选项中的 No Local、Retain As Published、Retain Handling 仅支持 MQTT V5 协议，当客户端以 MQTT V3 或 MQTT V3.1.1 连接时，代理订阅配置中仅有 Topic 与 QoS 的配置生效。

在配置代理订阅的主题时，EMQ X 提供了 `%c` 和 `%u` 两个占位符供用户使用，EMQ X 会在执行代理订阅时将配置中的 `%c` 和 `%u` 分别替换为客户端的 `Client ID` 和 `Username`，需要注意的是，`%c` 和 `%u` 必须占用一整个主题层级。

例如，在 `etc/emqx.conf` 文件中添加以下代理订阅规则：

```bash
module.subscription.1.topic = client/%c

module.subscription.2.topic = user/%u
module.subscription.2.qos = 2
module.subscription.2.nl  = 1
module.subscription.2.rap = 1
module.subscription.2.rh  = 1
```

配置 A、B 两个客户端，客户端 A 的 `Client ID` 为 `testclientA`，`Username` 为 `testerA`，客户端 B 的 `Client ID` 为 `testclientB`，`Username` 为 `testerB`。

A 客户端使用 MQTT V3.1.1 协议连接 EMQ X，根据上文的配置规则，代理订阅功能会主动帮客户端订阅 QoS 为 1 的 `client/testclientA` 和 QoS 为 2 的 `user/testerA` 这两个主题，因为连接协议为 MQTT V3.1.1，所以配置中的 No Local、Retain As Published、Retain Handling 不生效。

B 客户端使用 MQTT V5 协议连接 EMQ X，根据上文的配置规则，代理订阅功能会主动帮客户端订阅 `client/testclientB` 和 `user/testerB` 这两个主题，其中 `client/testclientB` 的订阅选项为 Qos = 1，No Local、Retain As Published、Retain Handling 均为 0；`user/testerB` 的订阅选项为 Qos = 2、No Local = 1、Retain As Published = 1 、Retain Handling = 1。

## 动态代理订阅

EMQ X Enterprise 版本中支持动态代理订阅，即通过外部数据库设置主题列表在设备连接时读取列表实现代理订阅。

