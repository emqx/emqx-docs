# Introduction

EMQX 5.0 supports extensions in the form of plugins, multilingual gRPC hooks and protocol extensions to facilitate your development workflow, and you can choose the extension method according to your needs.

## Plugins

Through EMQX's plugin development interface, users can access the core process to customize business logic, such as access control, message routing, or message storage. In addition, our protocol extensions help our users to manage client access through a unified [Gateway](../gateway/gateway.md) framework.

Developing plugins requires an understanding of EMQX and Erlang.

## Multilingual gRPC hooks and protocol extension

EMQX offers a multilingual extension combining [general-purpose Remote Procedure Calls (gRPC)](https://grpc.io/) multilingual hooks and protocol extension.

Through multilingual extensions, users can write services in Python or Java and integrate them with EMQX through the gRPC channel, enabling features similar to plugins, such as Hooks processing or private TCP/UDP protocol parsing.

