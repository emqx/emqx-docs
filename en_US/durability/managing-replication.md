# Replication Management

This document describes how to setup data replication and high availability for the EMQX durable storage.
It is relevant when setting up a new EMQX cluster with session persistence enabled, or upgrading an existing cluster to enable session persistence.

## Initial Cluster Setup

There are a few configuration parameters that influence how the cluster initially sets up durable storage and starts replicating data. These parameters can't be changed in the runtime, and modifying them will not take any effect once the durable storage is initialized.

### Replication Factor

The replication factor, controlled with `durable_storage.messages.replication_factor` configuration parameter, determines the number of replicas each shard should have across the cluster. The default value is 3. As a rule of thumb, the replication factor should be set to an odd number. This is because the replication factor is used to determine the quorum size needed to consider a write operation successful. The larger the replication factor, the more copies of the data are stored across the cluster, thus improving high availability.
However, this comes at the expense of increased storage and network overhead, because more communication is needed to achieve consensus.

Note that the replication factor is not strictly enforced in smaller clusters. For example, in a two-node cluster, the real replication factor is effectively 2, as each shard is replicated on both nodes, and there is no point in replicating it further. In general, EMQX allocates the replicas of each shard to different sites in the cluster, so that no two replicas are stored on the same node.

### Number of Shards

The builtin durable storage is split into shards, which are replicated independently from each other.
Larger number of shards makes publishing and consuming MQTT messages from the durable storage more parallel.
However, each shard requires system resources, such as file descriptors.
Additionally, it increases the volume of metadata stored per session.

This number is controlled by the `durable_storage.messages.n_shards` parameter.
The number of shards can't be changed once the durable storage in the EMQX cluster has been initialized.

### Number of Sites

The number of sites, as controlled by the `durable_storage.messages.n_sites` configuration parameter, determines the _minimum_ number of sites that must be online in the cluster for the durable storage to even initialize and start accepting writes. Once the minimum number of sites is online, the durable storage will start allocating shards to the available sites in a fair manner.

The default value is 1, which in actuality means that (depending on clustering strategy) each node may initially consider itself an only site responsible for storing the data, and initialize the durable storage accordingly.
This default has been optimized for the base case of a single-node EMQX cluster. In this case once the cluster is formed, a single view of the cluster will eventually take precedence, which will cause the other nodes to abandon the data they have stored so far.
Therefore, in a multi-node cluster it's recommended to set the number of sites to an initial cluster size to avoid such situations.

Once the durable storage is initialized, changes to this parameter will not take any effect.

## Changing Existing Cluster

At some point an existing cluster may need to be reconfigured. Changes to required capacity, durability, or client traffic, decommissioning old nodes and replacing them with new ones are most common reasons for reconfiguration. This can be achieved by joining new sites to the set of sites the durable storage is replicated to, or by removing the sites that are no longer needed.

One can always look up the current allocation of shards through `emqx ctl` CLI. The `ds` subcommand stands for "durable storage".
```shell
$ emqx ctl ds info
SITES:
...
SHARDS:
...
```

### Adding Sites

As soon as the new node is part of the cluster, it's assigned _Site ID_ and can be made a part of the durable storage.
```shell
$ emqx ctl ds join emqx_durable_storage <Site ID>
ok
```

Responsibility over some of the shard replicas will be transferred to this new site, and it will start replicating the data.

Depending on the amount of data stored in the cluster, the process of joining a new site may take some time. This does not compromise availability of the durable storage, but may affect the performance of the cluster, because the data must be transferred between the sites in the background.

Any changes to the set of durable storage sites are durably stored as well, so that node restarts or network partitions do not affect the outcome. Cluster will eventually reach the desired state in a consistent manner.

### Removing Sites

Removing a site implies transferring the responsibility over the shard replicas away from the site being removed.
```shell
$ emqx ctl ds leave emqx_durable_storage <Site ID>
ok
```

This process is similar to adding a site, and may take time and resources as well.

However, removing a site may cause the effective replication factor to drop below the configured value. For example, if the replication factor is 3 and one of the sites in a 3-node cluster is removed, the replication factor will effectively drop to 2. This could be risky in a situations when the goal is to permanently replace a site, so it's recommended to add the new site first, and only then decommission the old one. Or, alternatively, perform both operations simultaneously.

### Assigning Sites

Series of changes to the set of sites holding the durable storage replicas (as in the aforementioned example) can be performed in a single operation.
```shell
$ emqx ctl ds set_replicas emqx_durable_storage <Site ID 1> <Site ID 2> ...
```

This will help to minimize the volume of data transferred between the sites, while at the same time ensuring that the replication factor is maintained if possible.
