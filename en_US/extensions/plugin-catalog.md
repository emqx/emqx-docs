# Plugin Catalog

This catalog collects plugin-specific documentation for custom EMQX plugins maintained outside the built-in product documentation.

EMQX plugins are typically built when standard product features do not fully meet a requirement, or when a domain-specific problem is better solved as an extension rather than as a built-in capability.

Some plugins remain specialized, while others may later be promoted into standard EMQX features if they prove useful across a broader range of real-world use cases.

The plugins listed on this page are maintained as part of the [`emqx.git` monorepo](https://github.com/emqx/emqx/tree/master/plugins).

## Security

[EMQX ACME Plugin](./plugin-catalog/6.2/emqx-acme.md)

This plugin automatically issues and renews TLS certificates for EMQX SSL listeners through ACME-compatible certificate authorities such as Let's Encrypt.

## Operations

[Hot Upgrade (Relup)](./plugin-catalog/6.2/emqx-relup.md)

This plugin applies `.relup` code-change instructions to a running EMQX node, allowing operators to roll out patched releases without restarting the VM.

[Backup Sync](./plugin-catalog/6.2/emqx-backup-sync.md)

This plugin periodically synchronizes selected backup data from a primary EMQX cluster to a secondary cluster using the Data Backup APIs, keeping the secondary in step for disaster recovery.

## Data Integration

[MQTT Bridge with Disk Queue](./plugin-catalog/6.2/emqx-bridge-mqtt-dq.md)

This plugin forwards local MQTT messages to another MQTT broker, buffering them on disk for better resilience across network interruptions.

[Mapping Tables](./plugin-catalog/6.2/emqx-maptabs.md)

This plugin provides named mapping tables for Rule SQL, allowing rules to replace long `CASE WHEN` expressions with table lookups.

## Message Persistence

[Offline Messages](./plugin-catalog/6.2/emqx-offline-messages.md)

This plugin persists MQTT messages to MySQL or Redis so that subscribers can retrieve them after they reconnect, beyond what standard MQTT session persistence provides.

## Messaging

[Sync Request](./plugin-catalog/6.2/emqx-sync-request.md)

This plugin lets an HTTP caller publish one MQTT request through the EMQX REST API and wait synchronously for the first matching MQTT response.

## Connection Management

[Per-username Session Quota](./plugin-catalog/6.2/emqx-username-quota.md)

This plugin enforces a per-username session quota across the cluster, rejecting authentication with `quota_exceeded` once a username reaches its configured limit.

## Namespace Governance

[UNS Governance](./plugin-catalog/6.2/emqx-unsgov.md)

This plugin enforces a Unified Namespace (UNS) topic structure and can validate the payloads of messages published to topics governed by UNS Governance.
