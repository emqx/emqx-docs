# A2A over MQTT

A2A over MQTT is a broker-neutral transport profile that brings the [Agent-to-Agent (A2A) protocol](https://a2a-protocol.org/latest/) to MQTT. It defines standardized topic conventions, MQTT v5 property mappings, and discovery behavior so that AI agents built on different frameworks can register, discover, and collaborate through an MQTT broker without requiring any point-to-point integration between them.

EMQX implements A2A over MQTT through the **A2A Registry**, a built-in feature that indexes Agent Cards published to discovery topics, tracks agent liveness, and provides management interfaces for operators.

::: warning Incompatible with Most Listener Mountpoints
The A2A Registry does not work for agents connected through a listener with a [mountpoint](../../configuration/listener.md#mountpoint) configured. The only exception is a mountpoint with exactly one topic level, such as `acme/`, which EMQX parses as a namespace prefix on `$a2a` topics. With any other mountpoint, the mounted topics no longer match the `$a2a/v1/` conventions: Agent Cards are stored as ordinary retained messages without being indexed, validated, or annotated with liveness status. No error is reported to the client.
:::

## Why MQTT for A2A

The standard A2A protocol uses HTTP as its transport. This works well in cloud environments, but becomes limiting in distributed, IoT, or edge deployments where agents may run on constrained devices, behind NAT, or in environments where persistent HTTP connections are impractical.

MQTT addresses these constraints directly:

- **Lightweight and efficient**: Minimal protocol overhead, well-suited for low-bandwidth and unreliable networks.
- **Broker-mediated routing**: Agents do not need to know each other's addresses; the broker handles routing.
- **Retained messages for discovery**: Agents publish their Agent Card as a retained message, making it immediately available to any subscriber without a dedicated registry service.
- **Native presence detection**: MQTT Last Will and Testament (LWT) provides automatic offline signaling when an agent disconnects unexpectedly.
- **Centralized auth**: EMQX's existing authentication and authorization apply uniformly to all agent traffic.

## Key Features

- **Agent registration and discovery** via retained messages on standardized `$a2a/v1/discovery/` topics.
- **Dashboard UI** for viewing, registering, and managing Agent Cards interactively.
- **CLI management** via `emqx ctl a2a-registry` commands.
- **Schema validation**: optional enforcement of A2A Agent Card schema on registration.
- **Broker-managed liveness**: EMQX attaches `a2a-status` MQTT User Properties reflecting agent connection state.
- **MQTT v5 request/reply**: standardized `Response Topic` and `Correlation Data` conventions for task delegation between agents.

## Learn More

- [How A2A over MQTT Works](./architecture.md): components, topic model, and interaction patterns
- [Manage Agents](./manage-agents.md): enable the registry, register agents, and use the Dashboard, CLI, and MQTT interfaces
- [A2A over MQTT Specification](https://www.emqx.com/mqtt-for-ai/a2a-over-mqtt/specification/0.1/basic/mqtt_transport.html): full normative transport profile
