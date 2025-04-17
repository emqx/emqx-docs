# EMQX Enterprise Rolling Upgrade

When in a cluster, EMQX nodes can be upgraded one at a time without any downtime. This process is referred to as a rolling upgrade. To achieve smooth client session migration, you can use the Cluster Rebalancing feature of the EMQX Enterprise edition to evacuate clients from a node before upgrading it. Find more information about Cluster Rebalancing [here](../deploy/cluster/rebalancing.md).

The rolling upgrade process for each node in the cluster is as follows:

1. Evacuate clients from the node using cluster rebalancing. (optional)
2. Stop the old version node.
3. [Backup](../operations/backup-restore.md) the config files and data directory of the nodes.
4. Install a new version of EMQX.
5. Start the new version node.

:::tip Note
Do not perform cluster-wide config changes during a rolling upgrade. Configuration changes made from Dashboard, HTTP API, or CLI are applied to all nodes in the cluster. Making configuration changes during a rolling upgrade may cause nodes to become out of sync.
:::

## RPM and DEB

When using RPM or DEB packages, you can upgrade EMQX by simply installing the newer version package.

## Docker

When using Docker, you can upgrade EMQX by simply pulling the newer version image and restarting the container.

## Upgrade from Open Source (<5.9) to Enterprise Edition version 5.9+

Starting from version 5.9.0, EMQX Enterprise is released under the Business Source License (BSL) 1.1, replacing the previous separate Open Source and Enterprise editions.

While the technical steps for upgrading EMQX (like replacing binaries) are similar to previous version upgrades, the licensing model introduced in 5.9.0 is different and requires attention, particularly if you are running a cluster:

1. **New Licensing Model**: EMQX 5.9.0+ includes a default BSL 1.1 license grant (sometimes referred to as a single-node Community License). This default license enables all features but restricts deployment to a single node only.
1. **Clustering Requirement**: The previous Open Source edition allowed clustering. Because the default 5.9.0+ license does not permit clustering, if you are upgrading a clustered Open Source deployment and wish to maintain clustering capabilities, you must [obtain a Commercial License](./license.md#apply-for-a-license).
1. **Upgrade Process**: It is crucial that this Commercial License is [configured on each node](./license.md#update-and-configure-license-settings) before you start that node with the 5.9.0+ version during the upgrade process. Failing to do so will prevent the node from operating correctly within the cluster.
