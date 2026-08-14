# EMQX Enterprise Rolling Upgrade

In a clustered deployment, EMQX nodes can be upgraded one at a time without incurring any downtime. This process is referred to as a rolling upgrade. To achieve smooth client session migration, you can use the Cluster Rebalancing feature of the EMQX Enterprise to evacuate clients from a node before upgrading it. Find more information about Cluster Rebalancing [here](../../guides/cluster/rebalancing.md).

## Important Licensing Notice for Upgrades to EMQX 5.9 or Later
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

## Supported Rolling Upgrade Paths Since 5.0

Below are the matrices of supported rolling upgrade paths since 5.0.
Tables are split for readability; the late-v5 versions (5.8 – 5.10) appear in both.

- Version numbers ending with `?` (e.g. `6.3?`) are future releases.
- ✅: Supported, or planned to support.
- ⚠️: Supported, but with limitations.
- ❌: Not supported.

See release notes for detailed information.

### Within v5 (5.0 – 5.10)

| From\To | 5.1  | 5.2  | 5.3  | 5.4  | 5.5  | 5.6  | 5.7  | 5.8  | 5.9  | 5.10 |
| ------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
| 5.0     | ✅    | ✅    | ✅    | ✅    | ✅    | ✅    | ✅    | ✅    | ⚠️[1] | ❌[2] |
| 5.1     | ✅    | ✅    | ✅    | ✅    | ✅    | ✅    | ✅    | ✅    | ✅    | ❌[2] |
| 5.2     |      | ✅    | ✅    | ✅    | ✅    | ✅    | ✅    | ✅    | ✅    | ❌[2] |
| 5.3     |      |      | ✅    | ✅    | ✅    | ✅    | ✅    | ✅    | ✅    | ❌[2] |
| 5.4     |      |      |      | ✅    | ✅    | ⚠️    | ✅    | ✅    | ✅    | ✅    |
| 5.5     |      |      |      |      | ✅    | ⚠️    | ✅    | ✅    | ✅    | ✅    |
| 5.6     |      |      |      |      |      | ✅    | ✅    | ✅    | ✅    | ✅    |
| 5.7     |      |      |      |      |      |      | ✅    | ✅    | ✅    | ✅    |
| 5.8     |      |      |      |      |      |      |      | ✅    | ⚠️[3] | ⚠️[3] |
| 5.9     |      |      |      |      |      |      |      |      | ✅    | ✅    |
| 5.10    |      |      |      |      |      |      |      |      |      | ✅    |

- [1] Old limiter configs should be deleted from the config files (`etc/emqx.conf` and `data/configs/cluster-override.conf`) before the upgrade.
- [2] The pre-5.4 routing table will be deleted. Upgrade to 5.9 first, then perform a full-cluster restart (not rolling) before upgrading to 5.10 or later.
- [3] Support for OpenTelemetry header configuration was introduced in 5.8.7, which was released after 5.9.0 and 5.10.0. 5.8 versions running 5.8.7 or later require a rolling upgrade to version 5.9.1 or 5.10.1. Alternatively, remove the header configuration for OpenTelemetry integration during the upgrade.

### Into v6 (5.8 – 6.3?)

| From\To | 5.8  | 5.9  | 5.10 | 6.0  | 6.1  | 6.2  | 6.3? |
| ------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
| 5.8     | ✅    | ⚠️[3] | ⚠️[3] | ⚠️[4] | ⚠️[4] | ⚠️[4] | ⚠️[4] |
| 5.9     |      | ✅    | ✅    | ⚠️[4] | ⚠️[4] | ⚠️[4] | ⚠️[4] |
| 5.10    |      |      | ✅    | ⚠️[4] | ⚠️[4] | ⚠️[4] | ⚠️[4] |
| 6.0     |      |      |      | ✅    | ✅    | ✅    | ✅    |
| 6.1     |      |      |      |      | ✅    | ✅    | ✅    |
| 6.2     |      |      |      |      |      | ✅    | ✅    |
| 6.3?    |      |      |      |      |      |      | ✅    |

- [4] Durable session states will be lost after upgrading from v5 to v6. After clients reconnect, the sessions created in the new nodes will appear to be clean.

## Rolling Upgrade Considerations for EMQX 5.10 or Later

Starting from EMQX 5.10.0, only the _v2_ routing storage schema is supported. The legacy _v1_ schema, which was the default in versions before 5.4.0, is no longer compatible. As a result, clusters still using the *v1* schema, particularly those that have been incrementally upgraded from earlier versions, cannot perform rolling upgrades to 5.10.0 or later.

::: tip Important

If your cluster is still using the *v1* routing schema, a full cluster restart is required to complete the upgrade.

:::

### Determine the Current Routing Schema

Before proceeding, check which routing schema your EMQX cluster uses by running the following command:
```
$ emqx eval 'emqx_router:get_schema_vsn()'
```

If the output is `v2`, you can proceed with a regular rolling upgrade.

If the output is `v1`, you must perform a full cluster restart following the steps below.

### Upgrade Procedure for _v1_ Routing Schema

If your cluster uses the *v1* schema, follow these steps to upgrade to EMQX 5.10.0 or later:

1. Stop **all** nodes in the cluster.
2. Remove the `broker.routing.storage_schema` option from any configuration file where it is defined.
3. Upgrade all nodes to version 5.10.0 or newer.
4. Start core nodes first.
5. Start replicant nodes.

## General Procedure for Performing a Rolling Upgrade

To upgrade each node in the cluster without downtime, follow these steps:

1. Evacuate clients from the node using cluster rebalancing. (optional)
2. Stop the old version node.
3. [Back up](../../guides/backup-restore.md) the config files and data directory of the nodes.
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

## Rolling Upgrade Using RPM and DEB Packages


When using RPM or DEB packages, you can upgrade EMQX by simply installing the newer version package.

## Rolling Upgrade Using Docker

When using Docker, you can upgrade EMQX by simply pulling the newer version image and restarting the container.

## Rolling Upgrade from Open Source Edition to Enterprise Edition

If you are running EMQX Open Source Edition and want to upgrade to EMQX Enterprise Edition, the process is the same as upgrading to a newer version of the Open Source Edition.

There is no difference in installation and upgrade procedures between EMQX Open Source Edition and EMQX Enterprise Edition. The only additional step is that you need to manually [configure your License](./license.md) for the Enterprise Edition nodes after each upgrade. You cannot apply the License key to the whole cluster before all nodes are upgraded.

For example, add the following line to `etc/base.hocon` (`etc/emqx.conf` if upgrade target version is before `e5.8.5`):
```
license.key = "your license"
```

:::tip Note
If a License configuration is added to `emqx.conf`, any runtime changes made from the Dashboard, HTTP API, or CLI will be lost after the node is restarted.
This is because `emqx.conf` and environment variables have the highest priority when loading configurations during startup.
:::
