---
# 编写日期
date: 2021-03-09 09:15:26
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

# Extension Hook

The **Extension Hook** is supported by the **emqx-exhook** plugin. It allows users to process EMQX's [Hooks](hooks.md) using other programming languages.

In this way, other programming languages can handle emqx events for the purpose of customizing and extending emqx. For example, users can use other programming languages to implement:

- Authorization for client access
- ACL authentication for publishing/subscribing
- Persistence and bridging for messages
- Process client connected/disconnected events


## Design

The **emqx-exhook** plugin uses [gRPC](https://www.grpc.io) as the communication framework for RPC.

The architecture is as illustrated below:

```
  EMQX
+========================+                 +========+==========+
|    ExHook              |                 |        |          |
|   +----------------+   |      gRPC       | gRPC   |  User's  |
|   |   gRPC Client  | ------------------> | Server |  Codes   |
|   +----------------+   |    (HTTP/2)     |        |          |
|                        |                 |        |          |
+========================+                 +========+==========+
```

It indicates that EMQX acts as a gRPC client, sending hook events from EMQX to the user's gRPC server.

Consistent with EMQX's native hooks, it also supports a chained approach to calculating and returning.

![chain_of_responsiblity](./assets/chain_of_responsiblity.png)


## APIs

As the event handler, i.e. user implemented server side of gRPC. It can define the list of hooks that need to be mounted, and the implement callback functions for how to go about handling each event when it arrives.

These interfaces are defined as a gRPC service called `HookProvider`:

```protobuf
syntax = "proto3";

package emqx.exhook.v1;

service HookProvider {

  rpc OnProviderLoaded(ProviderLoadedRequest) returns (LoadedResponse) {};

  rpc OnProviderUnloaded(ProviderUnloadedRequest) returns (EmptySuccess) {};

  rpc OnClientConnect(ClientConnectRequest) returns (EmptySuccess) {};

  rpc OnClientConnack(ClientConnackRequest) returns (EmptySuccess) {};

  rpc OnClientConnected(ClientConnectedRequest) returns (EmptySuccess) {};

  rpc OnClientDisconnected(ClientDisconnectedRequest) returns (EmptySuccess) {};

  rpc OnClientAuthenticate(ClientAuthenticateRequest) returns (ValuedResponse) {};

  rpc OnClientCheckAcl(ClientCheckAclRequest) returns (ValuedResponse) {};

  rpc OnClientSubscribe(ClientSubscribeRequest) returns (EmptySuccess) {};

  rpc OnClientUnsubscribe(ClientUnsubscribeRequest) returns (EmptySuccess) {};

  rpc OnSessionCreated(SessionCreatedRequest) returns (EmptySuccess) {};

  rpc OnSessionSubscribed(SessionSubscribedRequest) returns (EmptySuccess) {};

  rpc OnSessionUnsubscribed(SessionUnsubscribedRequest) returns (EmptySuccess) {};

  rpc OnSessionResumed(SessionResumedRequest) returns (EmptySuccess) {};

  rpc OnSessionDiscarded(SessionDiscardedRequest) returns (EmptySuccess) {};

  rpc OnSessionTakeovered(SessionTakeoveredRequest) returns (EmptySuccess) {};

  rpc OnSessionTerminated(SessionTerminatedRequest) returns (EmptySuccess) {};

  rpc OnMessagePublish(MessagePublishRequest) returns (ValuedResponse) {};

  rpc OnMessageDelivered(MessageDeliveredRequest) returns (EmptySuccess) {};

  rpc OnMessageDropped(MessageDroppedRequest) returns (EmptySuccess) {};

  rpc OnMessageAcked(MessageAckedRequest) returns (EmptySuccess) {};
}
```

The HookProvider part:

- `OnProviderLoaded`: Defines how the HookProvider is loaded and return the list of hooks that need to be mounted. Only the hooks in this list will be called back to the HookProivder service.
- `OnProviderUnloaded`: Defines how the HookProvider is unloaded, only for notification.

The hook events part:

- The methods prefixed with `OnClient`, `OnSession`, and `OnMessage` correspond to the methods in [hooks](hooks.md). They have the same call timing and a similar argument list.
- Only `OnClientAuthenticate`, `OnClientCheckAcl`, `OnMessagePublish` are allowed to carry the return values to EMQX, other callbacks are not supported.
- Specifically, for message type hooks: `message.publish`, `message.delivered`, `message.acked`, `message.dropped`, it is possible to carry a list of topic filters for these hooks when returning a list of hooks.
  Then when a message event is triggered, only subject-specific messages that can match any filter in the subject filter list will be sent to the user's gRPC server.
  In this way, the gRPC server can process messages under only the topics it cares about, to avoid consuming redundant gRPC requests.

For details of the interface and parameter data structures refer to: [exhook.proto](https://github.com/emqx/emqx/blob/v4.3-beta.1/apps/emqx_exhook/priv/protos/exhook.proto)

::: tip
It should be noted that the hooks and the topic filter list configured by the message type hook hooks are confirmed only once when the HookProvider is loaded. And the subsequent gRPC requests will be based on the configured when loading. <br />
If the list of hooks to be mounted has changed, or the list of topic filters concerned by the message type hook has changed, it needs to be reloaded. That is, the ExHook plugin/module needs to be restarted in EMQX.
:::

## Developing Guide

The user needs to implement the gRPC service of `HookProvider` to receive callback events from EMQX.

The main development steps are as following:

1. Copy the `lib/emqx_exhook-<x.y.z>/priv/protos/exhook.proto` file to your project.
2. Generate the code for the gRPC server side of `exhook.proto` using the gRPC framework for the corresponding programming language.
3. Implement the interfaces defined in exhook.proto on demand

Once the development is complete, the service needs to be deployed to a server that can communicate with EMQX and ensure that the ports are open.

Then modify the server configuration in `etc/plugins/emqx_exhook.conf`, for example:

```
exhook.server.default.url = http://127.0.0.1:9000
```

Start the `emqx_exhook` plugin and observe the output.

One of the gRPC frameworks for each language can be found at: [grpc-ecosystem/awesome-grpc](https://github.com/grpc-ecosystem/awesome-grpc)

We also provide sample programs for some common programming languages: [emqx-extension-examples](https://github.com/emqx/emqx-extension-examples)
