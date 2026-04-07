# Extensions

EMQX supports several extension mechanisms beyond native plugins. This section focuses on external extension interfaces, especially hook integrations and protocol adaptation paths that let you extend EMQX without embedding Erlang code directly into the broker.

## Hooks and Protocol Extensions

EMQX provides extension interfaces for hook processing and protocol adaptation. These interfaces are suitable when you want to intercept broker events, connect external services, or adapt private protocols while keeping the integration boundary explicit.

For MQTT and non-MQTT device access, EMQX also provides a unified [Gateway](../gateway/gateway.md) framework for protocol access management.

## Multilingual gRPC Extensions

EMQX offers multilingual extension capabilities through [gRPC](https://grpc.io/), allowing you to implement services in languages such as Python or Java and integrate them with EMQX over RPC.

This approach is useful when you want functionality similar to plugins, such as hook processing or custom TCP/UDP protocol parsing, but prefer to keep the implementation outside the broker runtime.
