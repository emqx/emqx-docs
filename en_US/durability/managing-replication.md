# Manage Data Replicas

::: tip Note

Managing Replication is an EMQX Enterprise feature.

:::

For an EMQX cluster, durable storage achieves high availability through multiple data replicas. If a node crashes, clients can immediately connect to a new node and recover data from the replicas on other nodes. This guide provides instructions on configuring data replication and ensuring high availability for durable storage. This guide consists of instructions for two scenarios: setting up a new EMQX cluster with durable storage and upgrading an existing cluster to enable durable storage.

## Initial Cluster Setup

During the initial setup of the cluster, several configuration parameters influence how durable storage is established and data replication starts. These parameters cannot be changed in the runtime, and modifying them will not take any effect once the durable storage is initialized.

### Replication Factor

The replication factor, controlled with `durable_storage.messages.replication_factor` configuration parameter, determines the number of replicas each shard should have across the cluster. The default value is `3`.

Setting the replication factor to an odd number is advisable as it influences the quorum size required for successful write operations. A higher replication factor results in more copies of data distributed across the cluster, thereby enhancing high availability. However, it also increases storage and network overhead due to additional communication needed to achieve consensus.

::: tip

In smaller clusters, the replication factor is not strictly enforced. For instance, in a two-node cluster, the effective replication factor is `2`, as each shard is replicated on both nodes, eliminating the need for further replication. EMQX allocates replicas of each shard to different nodes in the cluster to ensure redundancy.

:::

### Number of Shards

The built-in durable storages are split into shards, which are replicated independently from each other.
A higher number of shards allows for more parallel publishing and consuming of MQTT messages. However, each shard consumes system resources, such as file descriptors, and increases the volume of metadata stored per session.

The `durable_storage.messages.n_shards` parameter controls the number of shards, which remains fixed once the durable storage is initialized.

### Number of Sites

The `durable_storage.messages.n_sites` configuration parameter determines the minimum number of sites that must be online for the durable storage to initialize and start accepting writes. Once this minimum is met, the durable storage begins allocating shards to the available sites in a balanced manner.

The default value is `1`, meaning each node may initially consider itself the sole site responsible for data storage. This setup is optimized for single-node EMQX clusters. When the cluster forms, one node's view will eventually dominate, causing other nodes to abandon their stored data.

In multi-node clusters, it is recommended to set the number of sites to the initial cluster size to prevent such conflicts. Note that once the durable storage is initialized, this parameter cannot be changed.

## Change Existing Cluster

Existing clusters may require reconfiguration due to changes in capacity, durability, client traffic, or the need to decommission old nodes and replace them with new ones. This can be achieved by adding new sites to the set of sites with durable storage replications or removing sites no longer required.

You can use the `emqx ctl` CLI with the `ds` subcommand to view the current shard allocation:

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

Changes to the replica set are durably stored, ensuring that node restarts or network partitions do not affect the outcome. The cluster will eventually achieve the desired state consistently.

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

## Recover from Disasters

When things go extremely wrong it's important to know how to recover efficiently. This section provides guidance on how to recover from common disaster scenarios.

### Complete Loss of a Node

Probably the most common disaster scenario is losing a node completely, due to a unrecoverable hardware failure, disk corruption or plain human mistake.

1. Once a node is completely lost, availability is partially compromised. Hence, it's probably a good idea to first restore desired availability, by moving the lost node's shards to other sites.
    
    Usual `leave` command should be enough to achieve this. It works even if the node is not reachable. However, in this case transitions may take longer time to complete.
    ```shell
    $ emqx ctl ds leave messages 5C6028D6CE9459C7 # Here, 5C6028D6CE9459C7 is the lost node's Site ID
    ```

2. Watch the cluster status, transitions should eventually complete.

    ```shell
    $ emqx ctl ds info
    <...>

    SITES:
    D8894F95DC86DFDB    'emqx@n1.local'        up
    5C6028D6CE9459C7    'emqx@n2.local'        (x) down
    <...>

    REPLICA TRANSITIONS:
    Shard                         Transitions
    messages/0                    -5C6028D6CE9459C7 +D8894F95DC86DFDB
    <...>
    ```

3. Once there are no more transitions, it's time to tell the cluster that the lost node is not coming back.

    ```shell
    $ emqx ctl ds forget messages 5C6028D6CE9459C7
    ```

    It's very important to perform this step if the plan is to replace the lost node with a new one, preserving the original node name. Otherwise, the cluster will have the same node name known under two different Site IDs, which will cause a lot of confusion down the road.
