# Plugin Catalog

This catalog collects plugin-specific documentation for custom EMQX plugins maintained outside the built-in product documentation.

EMQX plugins are typically built when standard product features do not fully meet a requirement, or when a domain-specific problem is better solved as an extension rather than as a built-in capability.

Some plugins remain specialized, while others may later be promoted into standard EMQX features if they prove useful across a broader range of real-world use cases.

The plugins listed on this page are maintained as part of the [`emqx.git` monorepo](https://github.com/emqx/emqx/tree/master/plugins).

## Message Persistence

[Offline Messages](./plugin-catalog/emqx-offline-messages.md)

This plugin persists MQTT messages to MySQL or Redis so that subscribers can retrieve them after coming back online, beyond what standard MQTT session persistence provides.

## Operations

[Hot Upgrade (Relup)](./plugin-catalog/emqx-relup.md)

This plugin applies `.relup` code-change instructions to a running EMQX node, allowing operators to roll out patched releases without restarting the VM.
