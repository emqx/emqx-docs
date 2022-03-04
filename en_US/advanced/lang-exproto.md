# Extension Protocol

**Extension Protocol** is provided by the **emqx-exproto** plugin, which allows other programming languages (e.g. Python, Java, etc.) to process bytes directly for parsing private protocols and provides a Pub/Sub interface for message exchange with EMQX.

This feature gives EMQX the power of scalability to handle any private protocol in a user-friendly programming language and enjoy the benefits of extremely high concurrent connections brought by the EMQX.


## Features

- Extremely scalable. Supports all major programming languages using gRPC as the RPC communication framework
- Fully asynchronous IO. The connection layer is implemented as fully asynchronous non-blocking I/O
- Transparent connection layer. Full support for TCP/TLS UDP/DTLS type connection management, and provides a unified API for the upper layers
- Connection management capabilities. For example, maximum number of connections, connection and throughput rate limits, IP blacklisting, etc.


## Design

![Extension-Protocol Arch](./assets/exproto-arch.jpg)

The interfaces provided by emqx-exproto:

1. **Connection Layer:** This layer mainly **maintains the life cycle of the socket, and the sending/receiving of data**. Included:
    - Listens on a port. When a new TCP/UDP connection arrives, a connection process is started to maintain the state of the connection.
    - Call the `OnSocketCreated` callback. Used to notify the user's server that **a new connection has been established**.
    - Call `OnSocketClosed` callback. Used to notify the user's server that a connection **has been closed**.
    - Call `OnReceivedBytes` callback. Used to notify the user's server that **the connection received new packets**.
    - Provides the `Send` interface. Called by user's server to **send packets**.
    - Provides the `Close` interface. Called by user's server. **For actively closing the connection**.

2. **Protocol/Session Layer:** This layer primarily **provides the PUB/SUB interface** for message interoperability with the EMQX. Includes:
    - Provides the `Authenticate` interface. Called by user's server to register clients into EMQX.
    - Provides the `StartTimer` interface. Called by user's server to start a timer such as a heartbeat for the connected process.
    - Provides the `Publish` interface. Called by user's server to publish messages to the EMQX.
    - Provides the `Subscribe` interface. Called by user's server to subscribe to a topic to receive certain downlink messages from the EMQX.
    - Provides the `Unsubscribe` interface. Called by user's server to unsubscribe from a topic.
    - Calls the `OnTimerTimeout` callback. Used to handle timer timeout events
    - Call the `OnReceivedMessages` callback. Used to receive downlink messages (After a successful subscription to a topic, this method will be called back if there are messages on the topic)


## APIs

From a gRPC perspective, ExProto acts as a client to send callback requests to the `ConnectionHandler` service. It also acts as a server side to provide the `ConnectionAdapter` service to user's server to provide calls to various interfaces. As shown in the figure.

![Extension Protocol gRPC Arch](../modules/assets/exproto-grpc-arch.jpg)

The services defination see: [exproto.proto](https://github.com/emqx/emqx/blob/v4.3-beta.1/apps/emqx_exproto/priv/protos/exproto.proto)

For examples:

```protobuff
syntax = "proto3";

package emqx.exproto.v1;

// The Broker side serivce. It provides a set of APIs to
// handle a protcol access
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


## Developing Guide

The user needs to implement the gRPC service of `ConnectionHandler` to receive callback events from EMQX.

The main development steps are as following:

1. Copy the `lib/emqx_exproto-<x.y.z>/priv/protos/exproto.proto` file to your project.
2. Generate the code for the gRPC server and client side of `exproto.proto` using the gRPC framework for the corresponding programming language.
3. Implement the interfaces defined in exhook.proto on demand

Once the development is complete, the service needs to be deployed to a server that can communicate with EMQX and ensure that the ports are open.

Then modify the server configuration in `etc/plugins/emqx_exproto.conf`, for example:

```

## The ConnectionAdapter services listen on
exproto.server.http.port = 9100

## The ExProto listen on for accepting Client connection
exproto.listener.protoname = tcp://0.0.0.0:7993

## The ConnectionHandler callback address
exproto.listener.protoname.connection_handler_url = http://127.0.0.1:9001
```

Start the `emqx_exproto` plugin and observe the output.

One of the gRPC frameworks for each language can be found at: [grpc-ecosystem/awesome-grpc](https://github.com/grpc-ecosystem/awesome-grpc)

We also provide sample programs for some common programming languages: [emqx-extension-examples](https://github.com/emqx/emqx-extension-examples)
