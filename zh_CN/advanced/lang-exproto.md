---
# 编写日期
date: 2020-09-12 09:15:26
# 作者 Github 名称
author: hjianbo
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---

# 多语言 - 协议接入

多语言的 **协议接入** 处理由 [emqx-exproto](https://github.com/emqx/emqx-exproto) 插件进行支持。该插件在 4.2.0 中首次引入。它允许用户使用其它的编程语言实现私有的，自定义的协议接入。例如：

- 可接收基于 TCP/UDP/TLS/DTLS 及 PSK 的终端连接。
- 允许 Python, Java 代码侧订阅主题，或发布消息到 EMQ X 系统。

## 设计

多语言系统的通信及过程调用的逻辑参见：[多语言 - 钩子扩展#设计](lang-exhook.md#design)。

### 回调与函数接口

与 `emqx-extension-hook` 不同的是，`emqx-exproto` 提供的是完全不同的回调和函数接口以实现协议接入的处理：

![ExProto - Arch](./assets/exproto-arch.jpg)

`emqx-exproto` 会提供函数接口供 Python/Java 侧调用；也设计了回调函数去通知 Python/Java 侧 `emqx-exproto` 有新的事件产生。

我们按层级将它分为：

#### 连接层

该部分主要 维持 Socket 的生命周期和数据的收发：

- 调用 `init` 回调。用于通知外部模块**已新建立了一个连接**。
- 调用 `terminated` 回调。用于通知外部模块连接**已关闭**。
- 调用 `received` 回调。用于通知外部模块**该连接新收到的数据包**。
- 提供 `send` 接口。供外部模块调用，**用于发送数据包**。
- 提供 `close` 接口。供外部模块调用，**用于主动关闭连接**。

#### 协议/会话层

该部分主要提供 PUB/SUB 接口，以实现与 EMQ X Broker 系统的消息互通。包括：

- 提供 `register` 接口。供外部模块调用，用于向集群注册客户端。
- 提供 `publish` 接口。供外部模块调用，用于发布消息 EMQ X Broker 中。
- 提供 `subscribe` 接口。供外部模块调用，用于订阅某主题，以实现从 EMQ X Broker 中接收某些下行消息。
- 提供 `unsubscribe` 接口。供外部模块调用，用于取消订阅某主题。
- 调用 `deliver` 回调。用于接收下行消息（在订阅主题成功后，如果主题上有消息，便会回调该方法）

注：函数，及参数的类型定义等，可参考 SDK 的实现。

### SDK

目前对于 `emqx-exproto` 提供的 SDK 有：

- Python: https://github.com/emqx/emqx-exproto-python-sdk
- Java: https://github.com/emqx/emqx-exproto-java-sdk

注：SDK 版本与 EMQ X 的第二位版本号进行兼容。例如，在 EMQ X v4.1.4 中，应该使用 v4.1.x 的 SDK


SDK 的概念参考：[多语言 - 钩子扩展#SDK](lang-exhook.md#sdk)

## 快速上手

### Python

参考：[emqx-exproto-python-sdk - Get Started](https://github.com/emqx/emqx-exproto-python-sdk#get-started)

### Java

参考：[emqx-exproto-java-sdk - Get Started](https://github.com/emqx/emqx-exproto-java-sdk#get-started)
