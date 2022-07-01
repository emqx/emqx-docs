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

**多语言 - 协议接入** 由 **emqx-exproto** 插件提供，它允许其他编程语言（例如：Python，Java 等）直接处理字节数据报文实现自定义协议的解析，并提供 Pub/Sub 接口实现与系统的消息交换。

该功能给 EMQX 带来的扩展性十分的强大，它能以用户熟悉的编程语言处理任何的私有协议，并享受由 EMQX 系统带来的极高并发连接的优点。

## 特性

- 极强的扩展能力。使用 gRPC 作为 RPC 通信框架，支持各个主流编程语言
- 完全的异步 IO。连接层以完全的异步非阻塞式 I/O 的方式实现
- 连接层透明。完全的支持 TCP\TLS UDP\DTLS 类型的连接管理，并对上层提供统一个 API
- 连接管理能力。例如，最大连接数，连接和吞吐的速率限制，IP 黑名单等

## 架构

![Extension-Protocol Arch](./assets/exproto-arch.jpg)

该功能提供的接口包括：

1. **连接层：** 该部分主要**维持 Socket 的生命周期，和数据的收发**。它的功能要求包括：

    - 监听某个端口。当有新的 TCP/UDP 连接到达后，启动一个连接进程，来维持连接的状态。
    - 调用 `OnSocketCreated` 回调。用于通知外部模块**已新建立了一个连接**。
    - 调用 `OnSocketClosed` 回调。用于通知外部模块连接**已关闭**。
    - 调用 `OnReceivedBytes` 回调。用于通知外部模块**该连接新收到的数据包**。
    - 提供 `Send` 接口。供外部模块调用，**用于发送数据包**。
    - 提供 `Close` 接口。供外部模块调用，**用于主动关闭连接**。

2. **协议/会话层：** 该部分主要**提供 PUB/SUB 接口**，以实现与 EMQX Broker 系统的消息互通。包括：

    - 提供 `Authenticate` 接口。供外部模块调用，用于向集群注册客户端。
    - 提供 `StartTimer` 接口。供外部模块调用，用于为该连接进程启动心跳等定时器。
    - 提供 `Publish` 接口。供外部模块调用，用于发布消息 EMQX Broker 中。
    - 提供 `Subscribe` 接口。供外部模块调用，用于订阅某主题，以实现从 EMQX Broker 中接收某些下行消息。
    - 提供 `Unsubscribe` 接口。供外部模块调用，用于取消订阅某主题。
    - 调用 `OnTimerTimeout` 回调。用于处理定时器超时的事件。
    - 调用 `OnReceivedMessages` 回调。用于接收下行消息（在订阅主题成功后，如果主题上有消息，便会回调该方法）


## 接口设计

从 gRPC 的角度上看，ExProto 会作为客户端向 `ConnectionHandler` 服务发送回调请求。同时，它也会作为服务端向外部模块提供 `ConnectionAdapter` 服务，以提供 各类接口的调用。如图：

![Extension Protocol gRPC Arch](../modules/assets/exproto-grpc-arch.jpg)

详细的设计参见：[exproto.proto](https://github.com/emqx/emqx/blob/v4.3-beta.1/apps/emqx_exproto/priv/protos/exproto.proto)

例如，其中接口的定义有：

```protobuff
syntax = "proto3";

package emqx.exproto.v1;

// The Broker side service. It provides a set of APIs to
// handle a protocol access
service ConnectionAdapter {

  // -- socket layer

  rpc Send(SendBytesRequest) returns (CodeResponse) {};

  rpc Close(CloseSocketRequest) returns (CodeResponse) {};

  // -- protocol layer

  rpc Authenticate(AuthenticateRequest) returns (CodeResponse) {};

  rpc StartTimer(TimerRequest) returns (CodeResponse) {};

  // -- pub/sub layer

  rpc Publish(PublishRequest) returns (CodeResponse) {};

  rpc Subscribe(SubscribeRequest) returns (CodeResponse) {};

  rpc Unsubscribe(UnsubscribeRequest) returns (CodeResponse) {};
}

service ConnectionHandler {

  // -- socket layer

  rpc OnSocketCreated(SocketCreatedRequest) returns (EmptySuccess) {};

  rpc OnSocketClosed(SocketClosedRequest) returns (EmptySuccess) {};

  rpc OnReceivedBytes(ReceivedBytesRequest) returns (EmptySuccess) {};

  // -- pub/sub layer

  rpc OnTimerTimeout(TimerTimeoutRequest) returns (EmptySuccess) {};

  rpc OnReceivedMessages(ReceivedMessagesRequest) returns (EmptySuccess) {};
}
```


## 开发指南

在使用该功能之前，用户需要开发和部署一个 gRPC 的服务，并实现 `exproto.proto` 定义的接口。

其步骤如下：

1. 拷贝出当前版本的 `lib/emqx_exproto-<x.y.z>/priv/protos/exproto.proto` 文件。
2. 使用对应编程语言的 gRPC 框架，生成 `exproto.proto` 的 gRPC 服务端的代码。
3. 实现 exproto.proto 当中 `ConnectionHandler` 服务的接口。

开发完成后，需将该服务部署到与 EMQX 能够通信的服务器上，并保证端口的开放。



其中各个语言的 gRPC 框架可参考：[grpc-ecosystem/awesome-grpc](https://github.com/grpc-ecosystem/awesome-grpc)

我们也提供了常见编程语言的示例程序：[emqx-extension-examples](https://github.com/emqx/emqx-extension-examples)
