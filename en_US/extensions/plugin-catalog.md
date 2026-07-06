# Plugin Catalog

This catalog collects plugin-specific documentation for custom EMQX plugins maintained outside the built-in product documentation.

EMQX plugins are typically built when standard product features do not fully meet a requirement, or when a domain-specific problem is better solved as an extension rather than as a built-in capability.

Some plugins remain specialized, while others may later be promoted into standard EMQX features if they prove useful across a broader range of real-world use cases.

The plugins listed on this page are maintained as part of the [`emqx.git` monorepo](https://github.com/emqx/emqx/tree/master/plugins).

## Operations

[Hot Upgrade (Relup)](./plugin-catalog/emqx-relup.md)

This plugin applies `.relup` code-change instructions to a running EMQX node, allowing operators to roll out patched releases without restarting the VM.

[Backup Sync](./plugin-catalog/emqx-backup-sync.md)

This plugin periodically synchronizes selected backup data from a primary EMQX cluster to a secondary cluster using the Data Backup APIs, keeping the secondary in step for disaster recovery.

## Connection Management

[Per-username Session Quota](./plugin-catalog/emqx-username-quota.md)

This plugin enforces a per-username session quota across the cluster, rejecting authentication with `quota_exceeded` once a username reaches its configured limit.
