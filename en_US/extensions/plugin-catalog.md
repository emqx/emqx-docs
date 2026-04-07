# Plugin Catalog

This catalog collects plugin-specific documentation for custom EMQX plugins maintained outside the built-in product documentation.

EMQX plugins are typically built when standard product features do not fully meet a requirement, or when a domain-specific problem is better solved as an extension rather than as a built-in capability.

Some plugins remain specialized, while others may later be promoted into standard EMQX features if they prove useful across a broader range of real-world use cases.

The plugins listed on this page are maintained as part of the [`emqx.git` monorepo](https://github.com/emqx/emqx/tree/master/plugins).

## Data Integration

[MQTT Bridge with Disk Queue](./plugin-catalog/emqx-bridge-mqtt-dq.md)

This plugin forwards MQTT messages from EMQX to a remote MQTT broker and uses a disk-backed queue to improve resilience during downstream outages or intermittent connectivity failures.

## Data Governance

[UNS Governance](./plugin-catalog/emqx-unsgov.md)

This plugin enforces Unified Namespace topic structure and optional payload validation rules, helping you standardize topic hierarchies and data contracts at publish time.

## Access Control

[Per-username Session Quota](./plugin-catalog/emqx-username-quota.md)

This plugin enforces cluster-wide concurrent session limits per username, with support for quota overrides, snapshot-based inspection APIs, and administrative session control.
