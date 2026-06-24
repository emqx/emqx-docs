# EMQX Enterprise Rolling Upgrade

When in a cluster, EMQX nodes can be upgraded one at a time without any downtime. This process is referred to as a rolling upgrade. To achieve smooth client session migration, you can use the Cluster Rebalancing feature of the EMQX Enterprise edition to evacuate clients from a node before upgrading it. Find more information about Cluster Rebalancing [here](../../guides/cluster/rebalancing.md).

**The Upgrade Process for a Cluster Node**

1. Evacuate clients from the node using cluster rebalancing. (optional)
2. Stop the old version node.
3. [Backup](../../guides/backup-restore.md) the config files and data directory of the nodes.
4. Install a new version of EMQX.
5. Start the new version node.

**Upgrading Core and Replicant Nodes**

In a core/replicant cluster, upgrade nodes in alternating groups to maintain cluster stability:

- Begin with one core node.
- Then upgrade a proportionate subset of replicant nodes (for example, roughly one-third if there are three core nodes).
- Continue alternating between core and replicant nodes until all nodes are upgraded.

This approach ensures replicants always have compatible cores to connect to during the rollout.

:::tip Note
Do not perform cluster-wide config changes during a rolling upgrade. Configuration changes made from Dashboard, HTTP API, or CLI are applied to all nodes in the cluster. Making configuration changes during a rolling upgrade may cause nodes to become out of sync.
:::

## Supported Rolling Upgrade Paths Since 5.0

Below are the matrices of supported rolling upgrade paths since 5.0.
Tables are split for readability; the late-v5 versions (5.8 – 5.10) appear in both.

- Version numbers ending with `?` (e.g. `6.3?`) are future releases.
- ✅: Supported, or planned to support.
- ⚠️: Supported, but with limitations.
- ❌: Not supported.

See release notes for detailed information.

### Within v5 (5.0 – 5.10)

| From\To | 5.1  | 5.2  | 5.3  | 5.4  | 5.5  | 5.6  | 5.7  | 5.8  | 5.9   | 5.10  |
|---------|------|------|------|------|------|------|------|------|-------|-------|
| 5.0     | ✅   | ✅   | ✅   | ✅   | ✅   | ✅   | ✅   | ✅   | ⚠️[1]  | ❌[2] |
| 5.1     | ✅   | ✅   | ✅   | ✅   | ✅   | ✅   | ✅   | ✅   | ✅    | ❌[2] |
| 5.2     |      | ✅   | ✅   | ✅   | ✅   | ✅   | ✅   | ✅   | ✅    | ❌[2] |
| 5.3     |      |      | ✅   | ✅   | ✅   | ✅   | ✅   | ✅   | ✅    | ❌[2] |
| 5.4     |      |      |      | ✅   | ✅   | ⚠️    | ✅   | ✅   | ✅    | ✅    |
| 5.5     |      |      |      |      | ✅   | ⚠️    | ✅   | ✅   | ✅    | ✅    |
| 5.6     |      |      |      |      |      | ✅   | ✅   | ✅   | ✅    | ✅    |
| 5.7     |      |      |      |      |      |      | ✅   | ✅   | ✅    | ✅    |
| 5.8     |      |      |      |      |      |      |      | ✅   | ⚠️[3]  | ⚠️[3]  |
| 5.9     |      |      |      |      |      |      |      |      | ✅    | ✅    |
| 5.10    |      |      |      |      |      |      |      |      |       | ✅    |

- [1] Old limiter configs should be deleted from the config files (`etc/emqx.conf` and `data/configs/cluster-override.conf`) before upgrade.
- [2] Pre-5.4 routing table will be deleted. Upgrade to 5.9 first, then perform a full-cluster restart (not rolling) before upgrade to 5.10 or later.
- [3] Support for OpenTelemetry header configuration was introduced in 5.8.7, which was released after 5.9.0 and 5.10.0. 5.8 versions running 5.8.7 or later require a rolling upgrade to version 5.9.1 or 5.10.1. Alternatively, remove the header configuration for OpenTelemetry integration during the upgrade.

### Into v6 (5.8 – 6.3?)

| From\To | 5.8  | 5.9   | 5.10  | 6.0   | 6.1   | 6.2  | 6.3?  |
|---------|------|-------|-------|-------|-------|------|-------|
| 5.8     | ✅   | ⚠️[3]  | ⚠️[3]  | ⚠️[4]  | ⚠️[4]  | ⚠️[4] | ⚠️[4]  |
| 5.9     |      | ✅    | ✅    | ⚠️[4]  | ⚠️[4]  | ⚠️[4] | ⚠️[4]  |
| 5.10    |      |       | ✅    | ⚠️[4]  | ⚠️[4]  | ⚠️[4] | ⚠️[4]  |
| 6.0     |      |       |       | ✅    | ✅    | ✅   | ✅    |
| 6.1     |      |       |       |       | ✅    | ✅   | ✅    |
| 6.2     |      |       |       |       |       | ✅   | ✅    |
| 6.3?    |      |       |       |       |       |      | ✅    |

- [4] Durable session states will be lost after upgrading from v5 to v6. After clients reconnect, the sessions created in the new nodes will appear to be clean.

## RPM and DEB

When using RPM or DEB packages, you can upgrade EMQX by simply installing the newer version package.

## Docker

When using Docker, you can upgrade EMQX by simply pulling the newer version image and restarting the container.

## Upgrade from Open Source to Enterprise Edition

If you are running an Open Source version of EMQX and would like to upgrade to the Enterprise Edition,
the process is the same as upgrading to a newer version of the Open Source Edition.

There is no difference in installation and upgrade between the Open Source and Enterprise Editions of EMQX. The only notice is that you need to manually [configure your License](./license.md) for the Enterprise edition nodes after each upgrade.
You cannot apply the License key to the whole cluster before all nodes are upgraded.

For example, add the following line to `etc/base.hocon` (`etc/emqx.conf` if upgrade target version is before `e5.8.5`):
```
license.key = "your license"
```

:::tip Note
If a License configuration is added to `emqx.conf`, any runtime changes made from the Dashboard, HTTP API, or CLI will be lost after the node is restarted. This is because `emqx.conf` and environment variables have the highest priority when loading configurations during startup.
:::
