# EMQX Enterprise Rolling Upgrade

In a clustered deployment, EMQX nodes can be upgraded one at a time without incurring any downtime. This process is referred to as a rolling upgrade. To achieve smooth client session migration, you can use the Cluster Rebalancing feature of the EMQX Enterprise to evacuate clients from a node before upgrading it. Find more information about Cluster Rebalancing [here](../../operate/cluster/rebalancing.md).

## Important Notice for Upgrades to EMQX 5.9 or Later

Starting from version 5.9.0, EMQX Enterprise is released under the Business Source License (BSL) 1.1, replacing the previous model that separated Open Source and Enterprise editions.

::: tip

For detailed information about the licensing changes, see the [EMQX Licensing FAQ](https://www.emqx.com/en/content/license-faq).

:::

While the technical steps for upgrading EMQX (like replacing binaries) are similar to previous version upgrades, version 5.9.0 introduces important licensing changes, especially for clustered deployments. If you are upgrading from an Open Source version earlier than 5.9 or upgrading a single-node deployment to clustering deployments, please note the following critical changes:

1. **New Licensing Model**: EMQX 5.9.0+ includes a default EMQX Community License. This default license enables all features but restricts deployment to a single node.
2. **Clustering Requirement**: Clustering was supported in the previous Open Source edition. Under the new model, clustering is not permitted by default in EMQX 5.9.0+. If you are upgrading a clustered Open Source deployment and wish to retain clustering capabilities, you must [obtain a Commercial License](./license.md#apply-for-a-license).
3. **License Configuration Requirement**: This Commercial License must be [configured on each node](./license.md#update-and-configure-license-settings) before you start any node with EMQX 5.9.0+ in a cluster during the upgrade process. If the license is missing or misconfigured, the node will not function correctly in the cluster.

::: tip

If a License configuration is added to `emqx.conf`, any runtime changes made from the Dashboard, HTTP API, or CLI will be lost after the node is restarted. This is because `emqx.conf` and environment variables have the highest priority when loading configurations during startup.

:::

## How to Perform a Rolling Upgrade

To upgrade each node in the cluster without downtime, follow these steps:

1. Evacuate clients from the node using cluster rebalancing. (optional)
2. Stop the old version node.
3. [Back up](../../operate/backup-restore.md) the config files and data directory of the nodes.
4. Install a new version of EMQX.
5. Start the new version node.

**Upgrading Core and Replicant Nodes**

In a core/replicant cluster, upgrade nodes in alternating groups to maintain cluster stability:

- Begin with one core node.
- Then upgrade a proportionate subset of replicant nodes (for example, roughly one-third if there are three core nodes).
- Continue alternating between core and replicant nodes until all nodes are upgraded.

This approach ensures replicants always have compatible cores to connect to during the rollout.

:::tip Note
Do not perform cluster-wide config changes during a rolling upgrade. Configuration changes made from the Dashboard, HTTP API, or CLI are applied to all nodes in the cluster. Making configuration changes during a rolling upgrade may cause nodes to become out of sync.
:::

## Upgrade with RPM and DEB Packages

When using RPM or DEB packages, you can upgrade EMQX by simply installing the newer version package.

## Upgrade with Docker

When using Docker, you can upgrade EMQX by simply pulling the newer version image and restarting the container.

## Upgrade from Open Source to Enterprise Edition

If you are running an Open Source version of EMQX and would like to upgrade to the Enterprise Edition,
the process is the same as upgrading to a newer version of the Open Source Edition.

There is no difference in installation and upgrade between the Open Source and Enterprise Editions of EMQX.
The only thing special is that you need to manually [configure your License](./license.md) for the Enterprise edition nodes after each upgrade.
You cannot apply the License key to the whole cluster before all nodes are upgraded.

For example, add the following line to `etc/base.hocon` (`etc/emqx.conf` if upgrade target version is before `e5.8.5`):
```
license.key = "your license"
```

:::tip Note
If a License configuration is added to `emqx.conf`, any runtime changes made from the Dashboard, HTTP API, or CLI will be lost after the node is restarted.
This is because `emqx.conf` and environment variables have the highest priority when loading configurations during startup.
:::
