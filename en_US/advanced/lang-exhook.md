# gRPC Hook Extension

The **Extension Hook** is supported by the **emqx-exhook** plugin. It allows users to process EMQX [Hooks](hooks.md) using other programming languages.

In this way, other programming languages can handle emqx events for the purpose of customizing and extending emqx. For example, users can use other programming languages to implement:

- Authentication for client connecting
- Authorization for publishing/subscribing
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

Consistent with EMQX native hooks, it also supports a chained approach to calculating and returning.

[Callback Functions Chain](./hooks.md#callback-functions-chain)


## APIs

As the event handler, i.e. user implemented server side of gRPC. It can define the list of hooks that need to be mounted, and the implement callback functions for how to go about handling each event when it arrives.

These interfaces are defined as a gRPC service called `HookProvider`:

```
syntax = "proto3";

package emqx.exhook.v2;

service HookProvider {

  rpc OnProviderLoaded(ProviderLoadedRequest) returns (LoadedResponse) {};

  rpc OnProviderUnloaded(ProviderUnloadedRequest) returns (EmptySuccess) {};

  rpc OnClientConnect(ClientConnectRequest) returns (EmptySuccess) {};

  rpc OnClientConnack(ClientConnackRequest) returns (EmptySuccess) {};

  rpc OnClientConnected(ClientConnectedRequest) returns (EmptySuccess) {};

  rpc OnClientDisconnected(ClientDisconnectedRequest) returns (EmptySuccess) {};

  rpc OnClientAuthenticate(ClientAuthenticateRequest) returns (ValuedResponse) {};

  rpc OnClientAuthorize(ClientAuthorizeRequest) returns (ValuedResponse) {};

  rpc OnClientSubscribe(ClientSubscribeRequest) returns (EmptySuccess) {};

  rpc OnClientUnsubscribe(ClientUnsubscribeRequest) returns (EmptySuccess) {};

  rpc OnSessionCreated(SessionCreatedRequest) returns (EmptySuccess) {};

  rpc OnSessionSubscribed(SessionSubscribedRequest) returns (EmptySuccess) {};

  rpc OnSessionUnsubscribed(SessionUnsubscribedRequest) returns (EmptySuccess) {};

  rpc OnSessionResumed(SessionResumedRequest) returns (EmptySuccess) {};

  rpc OnSessionDiscarded(SessionDiscardedRequest) returns (EmptySuccess) {};

  rpc OnSessionTakenover(SessionTakenoverRequest) returns (EmptySuccess) {};

  rpc OnSessionTerminated(SessionTerminatedRequest) returns (EmptySuccess) {};

  rpc OnMessagePublish(MessagePublishRequest) returns (ValuedResponse) {};

  rpc OnMessageDelivered(MessageDeliveredRequest) returns (EmptySuccess) {};

  rpc OnMessageDropped(MessageDroppedRequest) returns (EmptySuccess) {};

  rpc OnMessageAcked(MessageAckedRequest) returns (EmptySuccess) {};
}
```

The HookProvider part:

- `OnProviderLoaded`: Defines how the HookProvider will be loaded. This method returns the list of hooks that need to be mounted. Only the hooks in this list will be called back to the user's HookProvider service.
- `OnProviderUnloaded`: Notify the user that the HookProvider has been uninstalled from emqx

The hook events part:

- The methods prefixed with `OnClient`, `OnSession`, and `OnMessage` correspond to the methods in [hooks](hooks.md). They have the same call timing and a similar argument list.
- Only `OnClientAuthenticate`, `OnClientCheckAcl`, `OnMessagePublish` are allowed to carry the return values to EMQX, other callbacks are not supported.

For details of the interface and parameter data structures refer to: [exhook.proto](https://github.com/emqx/emqx/blob/master/apps/emqx_exhook/priv/protos/exhook.proto)

## Developing Guide

The user needs to implement the gRPC service of `HookProvider` to receive callback events from EMQX.

The main development steps are as following:

1. Copy the `lib/emqx_exhook-<x.y.z>/priv/protos/exhook.proto` file to your project.
2. Generate the code for the gRPC server side of `exhook.proto` using the gRPC framework for the corresponding programming language.
3. Implement the interfaces defined in exhook.proto on demand

Once the development is complete, the service needs to be deployed to a server that can communicate with EMQX and ensure that the ports are open.

You can use the [EMQX Dashboard](http://127.0.0.1:18083/#/exhook) to manage and monitor the ExHook service

One of the gRPC frameworks for each language can be found at: [grpc-ecosystem/awesome-grpc](https://github.com/grpc-ecosystem/awesome-grpc)

We also provide sample programs for some common programming languages: [emqx-extension-examples](https://github.com/emqx/emqx-extension-examples)
