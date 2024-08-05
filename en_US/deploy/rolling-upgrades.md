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

## Upgrade from Open Source to Enterprise Edition

If you are running an Open Source version of EMQX and would like to upgrade to the Enterprise Edition,
the process is the same as upgrading to a newer version of the Open Source Edition.

There is no difference in installation and upgrade between the Open Source and Enterprise Editions of EMQX. The only notice is that you need to manually [configure your License](./license.md) in `emqx.conf` for the Enterprise edition nodes after each upgrade. You cannot apply the License key to the whole cluster before all nodes are upgraded.

For example, add the following line to `emqx.conf`:
```
license.key = "your license"
```

:::tip Note
If a License configuration is added to `emqx.conf`, any runtime changes made from the Dashboard, HTTP API, or CLI will be lost after the node is restarted. This is because `emqx.conf` and environment variables have the highest priority when loading configurations during startup.

:::
