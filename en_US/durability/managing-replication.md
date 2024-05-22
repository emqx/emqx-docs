# Manage Replication

This guide provides instructions on configuring data replication and ensuring high availability for EMQX durable storage. This guide consists of instructions for two scenarios: setting up a new EMQX cluster with durable storage and upgrading an existing cluster to enable durable storage.

## Initial Cluster Setup

During the initial setup of the cluster, several configuration parameters influence how durable storage is established and data replication starts. These parameters cannot be changed in the runtime, and modifying them will not take any effect once the durable storage is initialized.

### Replication Factor

The replication factor, controlled with `durable_storage.messages.replication_factor` configuration parameter, determines the number of replicas each shard should have across the cluster. The default value is `3`.

Setting the replication factor to an odd number is advisable as it influences the quorum size required for successful write operations. A higher replication factor results in more copies of data distributed across the cluster, thereby enhancing high availability. However, it also increases storage and network overhead due to additional communication needed to achieve consensus.

::: tip

In smaller clusters, the replication factor is not strictly enforced. For instance, in a two-node cluster, the effective replication factor is `2`, as each shard is replicated on both nodes, eliminating the need for further replication. EMQX allocates replicas of each shard to different nodes in the cluster to ensure redundancy.

:::

### Number of Shards

The builtin durable storage is split into shards, which are replicated independently from each other.
A higher number of shards allows for more parallel publishing and consuming of MQTT messages from the durable storage. However, each shard consumes system resources, such as file descriptors, and increases the volume of metadata stored per session.

The `durable_storage.messages.n_shards` parameter controls the number of shards, which remains fixed once the durable storage is initialized.

### Number of Sites

The `durable_storage.messages.n_sites` configuration parameter determines the minimum number of sites that must be online for the durable storage to initialize and start accepting writes. Once this minimum is met, the durable storage begins allocating shards to the available sites in a balanced manner.

The default value is `1`, meaning each node may initially consider itself the sole site responsible for data storage. This setup is optimized for single-node EMQX clusters. When the cluster forms, one node's view will eventually dominate, causing other nodes to abandon their stored data.

In multi-node clusters, it is recommended to set the number of sites to the initial cluster size to prevent such conflicts. Note that once the durable storage is initialized, this parameter cannot be changed.

## Change Existing Cluster

Existing clusters may require reconfiguration due to changes in capacity, durability, or client traffic, or the need to decommission old nodes and replace them with new ones. This can be achieved by adding new sites to the set of sites with durable storage replication or removing sites no longer required.

You can use the `emqx ctl` CLI can be used with the `ds` subcommand to view the current shard allocation:

```shell
$ emqx ctl ds info
SITES:
...
SHARDS:
...
```

### Add Sites

When a new node joins the cluster, it is assigned a *Site ID* and can be included in the durable storage. Some shard replica responsibilities will be transferred to the new site, which will then start replicating the data.
```shell
$ emqx ctl ds join messages <Site ID>
ok
```

Depending on the cluster's data volume, joining a new site may take some time. While this process does not compromise the availability of durable storage, it may temporarily affect cluster performance due to the background data transfer between sites.

Changes to the set of durable storage sites are durably stored, ensuring that node restarts or network partitions do not affect the outcome. The cluster will eventually achieve the desired state consistently.

### Remove Sites

Removing a site involves transferring shard replica responsibilities away from the site being removed. Similar to adding a site, this process can take time and resources.
```shell
$ emqx ctl ds leave messages <Site ID>
ok
```

Removing a site can cause the effective replication factor to drop below the configured value. For example, if the replication factor is `3` and one site in a 3-node cluster is removed, the replication factor will effectively drop to `2`. To avoid this risk when permanently replacing a site, it is recommended to add a new site before decommissioning the old one or perform both operations simultaneously.

### Assign Sites

A series of changes to the set of sites holding durable storage replicas can be performed in a single operation.
```shell
$ emqx ctl ds set_replicas messages <Site ID 1> <Site ID 2> ...
```

This approach minimizes the volume of data transferred between sites, while ensuring that the replication factor is maintained if possible.
